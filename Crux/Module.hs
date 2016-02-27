{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

module Crux.Module where

import Control.Exception (tryJust)
import System.IO.Error (isDoesNotExistError)
import qualified Crux.AST              as AST
import qualified Crux.Error            as Error
import qualified Crux.Lex              as Lex
import qualified Crux.MutableHashTable as HashTable
import qualified Crux.Parse            as Parse
import           Crux.Prelude
import qualified Crux.Tokens as Tokens
import qualified Crux.Typecheck        as Typecheck
import Crux.Typecheck.Monad
import qualified Data.Aeson            as JSON
import qualified Data.ByteString       as BS
import qualified Data.HashMap.Strict   as HashMap
import qualified Data.Text             as Text
import qualified Data.Text.Encoding    as TE
import qualified Data.Text.IO          as TextIO
import           System.Directory      (doesFileExist, getCurrentDirectory)
import           System.Environment    (getExecutablePath)
import qualified System.FilePath       as FP
import Crux.Module.Types as AST
import Data.Yaml

type ModuleLoader = AST.ModuleName -> IO (Either Error.Error AST.ParsedModule)

findCompilerConfig :: IO (Maybe FilePath)
findCompilerConfig = do
    cc <- getExecutablePath >>= loop
    case cc of
        Just d -> return (Just d)
        Nothing ->
            getCurrentDirectory >>= loop

  where
    loop c = do
        let configPath = FP.combine c "cxconfig.yaml"
        exists <- doesFileExist configPath
        if | exists ->
                return $ Just configPath
           | c == FP.takeDirectory c ->
                return Nothing
           | otherwise ->
                loop $ FP.takeDirectory c

data CompilerConfig = CompilerConfig
    { baseLibraryPath :: FilePath
    }

instance JSON.FromJSON CompilerConfig where
    parseJSON (JSON.Object o) = CompilerConfig <$> o JSON..: "baseLibraryPath"
    parseJSON _ = fail "must be object"

loadCompilerConfig :: IO CompilerConfig
loadCompilerConfig = do
    configPath <- findCompilerConfig >>= \case
        Nothing -> fail "Failed to find compiler's cxconfig.yaml"
        Just c -> return c

    config <- decodeFileEither configPath >>= \case
        Left err -> fail $ "Failed to parse cxconfig.yaml:\n" ++ prettyPrintParseException err
        Right c -> return c

    return config { baseLibraryPath = FP.combine (FP.takeDirectory configPath) (FP.takeDirectory $ baseLibraryPath config) }

loadBuiltinSource :: IO Text
loadBuiltinSource = do
    config <- loadCompilerConfig
    TextIO.readFile $ baseLibraryPath config FP.</> "builtin.cx"

defaultModuleLoader :: ModuleLoader
defaultModuleLoader name = do
    if name == "builtin" then do
        builtinSource <- loadBuiltinSource
        parseModuleFromSource "builtin" "<Builtin>" builtinSource
    else
        return $ Left $ Error.UnknownModule name

loadBuiltin :: IO (Either Error.Error AST.LoadedModule)
loadBuiltin = loadBuiltinSource >>= loadBuiltinFromSource

loadBuiltinFromSource :: Text -> IO (Either Error.Error AST.LoadedModule)
loadBuiltinFromSource = loadModuleFromSource' id [] "builtin" "builtin"

parseModuleFromSource :: AST.ModuleName -> FilePath -> Text -> IO (Either Error.Error AST.ParsedModule)
parseModuleFromSource moduleName filename source = do
    case Lex.lexSource filename source of
        Left err ->
            return $ Left $ Error.LexError err
        Right tokens -> do
            case Parse.parse moduleName filename tokens of
                Left err ->
                    return $ Left $ Error.ParseError err
                Right mod' ->
                    return $ Right mod'

parseModuleFromFile :: AST.ModuleName -> FilePath -> IO (Either Error.Error AST.ParsedModule)
parseModuleFromFile moduleName filename = runEitherT $ do
    source <- EitherT $ tryJust (\e -> if isDoesNotExistError e then Just $ Error.ModuleNotFound moduleName else Nothing) $ BS.readFile filename
    EitherT $ parseModuleFromSource moduleName filename $ TE.decodeUtf8 source

loadModuleFromSource'
    :: (AST.ParsedModule -> AST.ParsedModule)
    -> HashMap AST.ModuleName AST.LoadedModule
    -> AST.ModuleName
    -> FilePath
    -> Text
    -> IO (Either Error.Error AST.LoadedModule)
loadModuleFromSource' adjust loadedModules moduleName filename source = runEitherT $ do
    mod' <- EitherT $ parseModuleFromSource moduleName filename source
    bridgeEitherTC $ Typecheck.run loadedModules (adjust mod') moduleName

loadModuleFromSource :: AST.ModuleName -> FilePath -> Text -> IO (Either Error.Error AST.LoadedModule)
loadModuleFromSource moduleName filename source = runEitherT $ do
    builtin <- EitherT $ loadBuiltin
    let lm = [("builtin", builtin)]
    EitherT $ loadModuleFromSource' addBuiltin lm moduleName filename source

getModuleName :: AST.Import -> AST.ModuleName
getModuleName (AST.UnqualifiedImport mn) = mn
getModuleName (AST.QualifiedImport mn _) = mn

importsOf :: AST.Module a b -> [(Tokens.Pos, AST.ModuleName)]
importsOf m = fmap (fmap getModuleName) $ AST.mImports m

addBuiltin :: AST.Module a b -> AST.Module a b
addBuiltin m = m { AST.mImports = (Tokens.Pos 0 0 0, AST.UnqualifiedImport "builtin") : AST.mImports m }

type ProgramLoadResult a = Either (AST.ModuleName, Error.Error) a

loadModule ::
       ModuleLoader
    -> IORef (HashMap AST.ModuleName AST.LoadedModule)
    -> AST.ModuleName
    -> Bool
    -> IO (ProgramLoadResult AST.LoadedModule)
loadModule loader loadedModules moduleName shouldAddBuiltin = runEitherT $ do
    lift (HashTable.lookup moduleName loadedModules) >>= \case
        Just m ->
            return m
        Nothing -> do
            parsedModuleResult <- lift $ loader moduleName
            parsedModule <- case parsedModuleResult of
                Left err -> left (moduleName, err)
                Right m
                    | shouldAddBuiltin -> return $ addBuiltin m
                    | otherwise -> return m

            for_ (importsOf parsedModule) $ \(_, referencedModule) -> do
                EitherT $ loadModule loader loadedModules referencedModule shouldAddBuiltin

            lm <- lift $ readIORef loadedModules
            (lift $ bridgeTC $ Typecheck.run lm parsedModule moduleName) >>= \case
                Left typeError -> do
                    left (moduleName, typeError)
                Right loadedModule -> do
                    lift $ HashTable.insert moduleName loadedModule loadedModules
                    return loadedModule

loadProgram :: ModuleLoader -> AST.ModuleName -> IO (ProgramLoadResult AST.Program)
loadProgram loader main = runEitherT $ do
    loadedModules <- lift $ newIORef []

    _ <- EitherT $ loadModule loader loadedModules "builtin" False
    mainModule <- EitherT $ loadModule loader loadedModules main True

    otherModules <- lift $ readIORef loadedModules
    return AST.Program
        { AST.pMainModule = mainModule
        , AST.pOtherModules = otherModules
        }

moduleNameToPath :: AST.ModuleName -> FilePath
moduleNameToPath (AST.ModuleName prefix m) =
    let toPathSegment (AST.ModuleSegment t) = Text.unpack t in
    FP.combine
        (FP.joinPath $ map toPathSegment prefix)
        (toPathSegment m <> ".cx")

newFSModuleLoader :: CompilerConfig -> FilePath -> FilePath -> ModuleLoader
newFSModuleLoader config root mainModulePath moduleName = do
    e1 <- doesFileExist $ (baseLibraryPath config) FP.</> moduleNameToPath moduleName
    let path = if e1 then baseLibraryPath config else root
    if moduleName == "main" then do
        parseModuleFromFile moduleName mainModulePath
    else do
        parseModuleFromFile moduleName $ FP.combine path $ moduleNameToPath moduleName

newMemoryLoader :: HashMap.HashMap AST.ModuleName Text -> ModuleLoader
newMemoryLoader sources moduleName = do
    case HashMap.lookup moduleName sources of
        Just source -> parseModuleFromSource
            moduleName
            ("<" ++ Text.unpack (AST.printModuleName moduleName) ++ ">")
            source
        Nothing ->
            return $ Left $ Error.UnknownModule moduleName

loadProgramFromDirectoryAndModule :: FilePath -> Text -> IO (ProgramLoadResult AST.Program)
loadProgramFromDirectoryAndModule sourceDir mainModule = do
    loadProgramFromFile $ FP.combine sourceDir (Text.unpack mainModule ++ ".cx")

loadProgramFromFile :: FilePath -> IO (ProgramLoadResult AST.Program)
loadProgramFromFile path = do
    config <- loadCompilerConfig
    let (dirname, basename) = FP.splitFileName path
    let loader = newFSModuleLoader config dirname path
    case FP.splitExtension basename of
        (_, ".cx") -> return ()
        _ -> fail "Please load .cx file"

    loadProgram loader "main"

loadProgramFromSource :: Text -> IO (ProgramLoadResult AST.Program)
loadProgramFromSource mainModuleSource = do
    builtinSource <- loadBuiltinSource
    let loader = newMemoryLoader $ HashMap.fromList
            [ ("builtin", builtinSource)
            , ("main", mainModuleSource)
            ]
    loadProgram loader "main"

loadProgramFromSources :: HashMap.HashMap AST.ModuleName Text -> IO (ProgramLoadResult AST.Program)
loadProgramFromSources sources = do
    builtin <- loadBuiltinSource
    let loader = newMemoryLoader (HashMap.insert "builtin" builtin sources)
    loadProgram loader "main"
