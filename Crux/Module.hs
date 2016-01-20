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
import qualified Crux.Typecheck        as Typecheck
import qualified Data.Aeson            as JSON
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as BSL
import qualified Data.HashMap.Strict   as HashMap
import qualified Data.Text             as Text
import qualified Data.Text.Encoding    as TE
import qualified Data.Text.IO          as TextIO
import           System.Directory      (doesFileExist, getCurrentDirectory)
import           System.Environment    (getExecutablePath)
import qualified System.FilePath       as FP

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
        let configPath = FP.combine c "cxconfig.json"
        exists <- doesFileExist configPath
        if | exists ->
                return $ Just configPath
           | c == FP.takeDirectory c ->
                return Nothing
           | otherwise ->
                loop $ FP.takeDirectory c

data CompilerConfig = CompilerConfig
    { preludePath :: FilePath
    }

instance JSON.FromJSON CompilerConfig where
    parseJSON (JSON.Object o) = CompilerConfig <$> o JSON..: "preludePath"
    parseJSON _ = fail "must be object"

loadCompilerConfig :: IO CompilerConfig
loadCompilerConfig = do
    configPath <- findCompilerConfig >>= \case
        Nothing -> fail "Failed to find compiler's cxconfig.json"
        Just c -> return c

    configContents <- BSL.readFile configPath
    config <- case JSON.decode configContents of
        Nothing -> fail "Failed to parse cxconfig.json"
        Just c -> return c

    return config { preludePath = FP.combine (FP.takeDirectory configPath) (FP.takeDirectory $ preludePath config) }

loadPreludeSource :: IO Text
loadPreludeSource = do
    config <- loadCompilerConfig
    TextIO.readFile $ preludePath config FP.</> "prelude.cx"

defaultModuleLoader :: ModuleLoader
defaultModuleLoader name = do
    if name == "prelude" then do
        preludeSource <- loadPreludeSource
        parseModuleFromSource "prelude" "<Prelude>" preludeSource
    else
        return $ Left $ Error.UnknownModule name

loadPrelude :: IO (Either Error.Error AST.LoadedModule)
loadPrelude = loadPreludeSource >>= loadPreludeFromSource

loadPreludeFromSource :: Text -> IO (Either Error.Error AST.LoadedModule)
loadPreludeFromSource = loadModuleFromSource' id [] "prelude" "prelude"

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
    EitherT $ Typecheck.run loadedModules $ adjust mod'

loadModuleFromSource :: AST.ModuleName -> FilePath -> Text -> IO (Either Error.Error AST.LoadedModule)
loadModuleFromSource moduleName filename source = runEitherT $ do
    prelude <- EitherT $ loadPrelude
    let lm = [("prelude", prelude)]
    EitherT $ loadModuleFromSource' addPrelude lm moduleName filename source

getModuleName :: AST.Import -> AST.ModuleName
getModuleName (AST.UnqualifiedImport mn) = mn
getModuleName (AST.QualifiedImport mn _) = mn

importsOf :: AST.Module a b -> [AST.ModuleName]
importsOf m = map getModuleName $ AST.mImports m

addPrelude :: AST.Module a b -> AST.Module a b
addPrelude m = m { AST.mImports = AST.UnqualifiedImport "prelude" : AST.mImports m }

type ProgramLoadResult a = Either (AST.ModuleName, Error.Error) a

loadModule ::
       ModuleLoader
    -> IORef (HashMap AST.ModuleName AST.LoadedModule)
    -> AST.ModuleName
    -> Bool
    -> IO (ProgramLoadResult AST.LoadedModule)
loadModule loader loadedModules moduleName shouldAddPrelude = runEitherT $ do
    lift (HashTable.lookup moduleName loadedModules) >>= \case
        Just m ->
            return m
        Nothing -> do
            parsedModuleResult <- lift $ loader moduleName
            parsedModule <- case parsedModuleResult of
                Left err -> left (moduleName, err)
                Right m
                    | shouldAddPrelude -> return $ addPrelude m
                    | otherwise -> return m

            forM_ (importsOf parsedModule) $ \referencedModule -> do
                EitherT $ loadModule loader loadedModules referencedModule shouldAddPrelude

            lm <- lift $ readIORef loadedModules
            (lift $ Typecheck.run lm parsedModule) >>= \case
                Left typeError -> do
                    left (moduleName, typeError)
                Right loadedModule -> do
                    lift $ HashTable.insert moduleName loadedModule loadedModules
                    return loadedModule

loadProgram :: ModuleLoader -> AST.ModuleName -> IO (ProgramLoadResult AST.Program)
loadProgram loader main = runEitherT $ do
    loadedModules <- lift $ newIORef []

    _ <- EitherT $ loadModule loader loadedModules "prelude" False
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
    e1 <- doesFileExist $ (preludePath config) FP.</> moduleNameToPath moduleName
    let path = if e1 then preludePath config else root
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
    preludeSource <- loadPreludeSource
    let loader = newMemoryLoader $ HashMap.fromList
            [ ("prelude", preludeSource)
            , ("main", mainModuleSource)
            ]
    loadProgram loader "main"

loadProgramFromSources :: HashMap.HashMap AST.ModuleName Text -> IO (ProgramLoadResult AST.Program)
loadProgramFromSources sources = do
    prelude <- loadPreludeSource
    let loader = newMemoryLoader (HashMap.insert "prelude" prelude sources)
    loadProgram loader "main"
