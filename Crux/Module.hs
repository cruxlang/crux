{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

module Crux.Module where

import           Control.Exception     (ErrorCall (..))
import qualified Crux.AST              as AST
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
import           System.Directory      (doesFileExist)
import           System.Environment    (getExecutablePath)
import qualified System.FilePath       as FP

type ModuleLoader = AST.ModuleName -> IO (Either String AST.ParsedModule)

findCompilerConfig :: IO (Maybe FilePath)
findCompilerConfig = do
    exePath <- getExecutablePath
    loop exePath
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
    TextIO.readFile $ preludePath config FP.</> "Prelude.cx"

defaultModuleLoader :: ModuleLoader
defaultModuleLoader name = do
    if name == "Prelude" then do
        preludeSource <- loadPreludeSource
        parseModuleFromSource "Prelude" "<Prelude>" preludeSource
    else
        return $ Left $ "unknown module: " <> (Text.unpack $ AST.printModuleName name)

loadPrelude :: IO AST.LoadedModule
loadPrelude = do
    loadPreludeSource >>= loadPreludeFromSource

loadPreludeFromSource :: Text -> IO AST.LoadedModule
loadPreludeFromSource preludeSource =
    loadModuleFromSource' id [] "Prelude" "Prelude" preludeSource >>= \case
        Left err -> do
            throwIO $ ErrorCall $ "Failed to load Prelude: " ++ show err
        Right m -> return m

parseModuleFromSource :: AST.ModuleName -> FilePath -> Text -> IO (Either String AST.ParsedModule)
parseModuleFromSource moduleName filename source = do
    let l = Lex.lexSource filename source
    case l of
        Left err ->
            return $ Left $ "Lex error: " <> show err
        Right l' -> do
            p <- Parse.parse moduleName filename l'
            case p of
                Left err ->
                    return $ Left $ "Parse error: " <> show err
                Right mod' ->
                    return $ Right mod'

parseModuleFromFile :: AST.ModuleName -> FilePath -> IO (Either String AST.ParsedModule)
parseModuleFromFile moduleName filename = do
    source <- BS.readFile filename
    parseModuleFromSource moduleName filename $ TE.decodeUtf8 source

loadModuleFromSource' :: (AST.ParsedModule -> AST.ParsedModule) -> HashMap AST.ModuleName AST.LoadedModule -> AST.ModuleName -> FilePath -> Text -> IO (Either String AST.LoadedModule)
loadModuleFromSource' adjust loadedModules moduleName filename source = do
    parseModuleFromSource moduleName filename source >>= \case
        Left err ->
            return $ Left err
        Right mod' -> do
            fmap Right $ Typecheck.run loadedModules $ adjust mod'

loadModuleFromSource :: AST.ModuleName -> FilePath -> Text -> IO (Either String AST.LoadedModule)
loadModuleFromSource moduleName filename source = do
    prelude <- loadPrelude
    let lm = [("Prelude", prelude)]
    loadModuleFromSource' addPrelude lm moduleName filename source

importsOf :: AST.Module a b -> [AST.ModuleName]
importsOf m =
    map (\(AST.UnqualifiedImport i) -> i) (AST.mImports m)

addPrelude :: AST.Module a b -> AST.Module a b
addPrelude m = m { AST.mImports = AST.UnqualifiedImport "Prelude" : AST.mImports m }

loadModule ::
       ModuleLoader
    -> IORef (HashMap AST.ModuleName AST.LoadedModule)
    -> AST.ModuleName
    -> Bool
    -> IO AST.LoadedModule
loadModule loader loadedModules moduleName shouldAddPrelude = do
    HashTable.lookup moduleName loadedModules >>= \case
        Just m ->
            return m
        Nothing -> do
            parsedModuleResult <- loader moduleName
            parsedModule <- case parsedModuleResult of
                Left err -> fail $ "Error loading module: " <> err
                Right m
                    | shouldAddPrelude -> return $ addPrelude m
                    | otherwise -> return m

            forM_ (importsOf parsedModule) $ \referencedModule ->
                loadModule loader loadedModules referencedModule shouldAddPrelude

            lm <- readIORef loadedModules
            loadedModule <- Typecheck.run lm parsedModule
            HashTable.insert moduleName loadedModule loadedModules
            return loadedModule

loadProgram :: ModuleLoader -> AST.ModuleName -> IO AST.Program
loadProgram loader main = do
    loadedModules <- newIORef []

    _ <- loadModule loader loadedModules "Prelude" False
    mainModule <- loadModule loader loadedModules main True

    otherModules <- readIORef loadedModules
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
    let path = if e1 then preludePath config else mainModulePath
    if moduleName == "Main"
        then parseModuleFromFile moduleName path
        else parseModuleFromFile moduleName $ FP.combine path $ moduleNameToPath moduleName

newMemoryLoader :: HashMap.HashMap AST.ModuleName Text -> ModuleLoader
newMemoryLoader sources moduleName = do
    case HashMap.lookup moduleName sources of
        Just source -> parseModuleFromSource
            moduleName
            ("<" ++ Text.unpack (AST.printModuleName moduleName) ++ ">")
            source
        Nothing ->
            return $ Left $ "Unknown module: " ++ show moduleName

loadProgramFromFile :: FilePath -> IO AST.Program
loadProgramFromFile path = do
    config <- loadCompilerConfig
    let (dirname, basename) = FP.splitFileName path
    let loader = newFSModuleLoader config dirname path
    case FP.splitExtension basename of
        (_, ".cx") -> return ()
        _ -> fail "Please load .cx file"

    loadProgram loader "Main"

loadProgramFromSource :: Text -> IO AST.Program
loadProgramFromSource mainModuleSource = do
    preludeSource <- loadPreludeSource
    let loader = newMemoryLoader $ HashMap.fromList
            [ ("Prelude", preludeSource)
            , ("Main", mainModuleSource)
            ]
    loadProgram loader "Main"

loadProgramFromSources :: HashMap.HashMap AST.ModuleName Text -> IO AST.Program
loadProgramFromSources sources = do
    prelude <- loadPreludeSource
    let loader = newMemoryLoader (HashMap.insert "Prelude" prelude sources)
    loadProgram loader "Main"
