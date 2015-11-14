{-# LANGUAGE OverloadedLists #-}

module Crux.Module where

import Crux.Prelude
import Control.Exception (throwIO, ErrorCall(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Crux.AST as AST
import qualified Crux.Lex as Lex
import qualified Crux.Parse as Parse
import qualified Crux.Typecheck as Typecheck
import qualified Crux.MutableHashTable as HashTable
import qualified System.FilePath as FP
import qualified Data.Aeson as JSON
import System.Directory (doesFileExist)
import System.Environment (getExecutablePath)
import qualified Data.Text.IO as TextIO

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

loadPreludeSource :: IO Text
loadPreludeSource = do
    configPath <- findCompilerConfig >>= \case
        Nothing -> fail "Failed to find compiler's cxconfig.json"
        Just c -> return c

    configContents <- BSL.readFile configPath
    config <- case JSON.decode configContents of
        Nothing -> fail "Failed to parse cxconfig.json"
        Just c -> return c

    TextIO.readFile $ FP.combine (FP.takeDirectory configPath) (preludePath config)

defaultModuleLoader :: ModuleLoader
defaultModuleLoader name = do
    if name == "Prelude" then do
        preludeSource <- loadPreludeSource
        parseModuleFromSource "Prelude" preludeSource
    else
        return $ Left $ "unknown module: " <> (Text.unpack $ AST.printModuleName name)

loadPrelude :: IO AST.LoadedModule
loadPrelude = do
    loadPreludeSource >>= loadPreludeFromSource

loadPreludeFromSource :: Text -> IO AST.LoadedModule
loadPreludeFromSource preludeSource =
    loadModuleFromSource' id [] "Prelude" preludeSource >>= \case
        Left err -> do
            throwIO $ ErrorCall $ "Failed to load Prelude: " ++ show err
        Right m -> return m

parseModuleFromSource :: FilePath -> Text -> IO (Either String AST.ParsedModule)
parseModuleFromSource filename source = do
    let l = Lex.lexSource filename source
    case l of
        Left err ->
            return $ Left $ "Lex error: " <> show err
        Right l' -> do
            p <- Parse.parse filename l'
            case p of
                Left err ->
                    return $ Left $ "Parse error: " <> show err
                Right mod' ->
                    return $ Right mod'

parseModuleFromFile :: FilePath -> IO (Either String AST.ParsedModule)
parseModuleFromFile filename = do
    source <- BS.readFile filename
    parseModuleFromSource filename $ TE.decodeUtf8 source

loadModuleFromSource' :: (AST.ParsedModule -> AST.ParsedModule) -> HashMap AST.ModuleName AST.LoadedModule -> FilePath -> Text -> IO (Either String AST.LoadedModule)
loadModuleFromSource' adjust loadedModules filename source = do
    parseModuleFromSource filename source >>= \case
        Left err ->
            return $ Left err
        Right mod' -> do
            fmap Right $ Typecheck.run loadedModules $ adjust mod'

loadModuleFromSource :: FilePath -> Text -> IO (Either String AST.LoadedModule)
loadModuleFromSource filename source = do
    prelude <- loadPrelude
    let lm = [("Prelude", prelude)]
    loadModuleFromSource' addPrelude lm filename source

importsOf :: AST.Module a b -> [AST.ModuleName]
importsOf m = do
    map (\(AST.UnqualifiedImport i) -> i) $ AST.mImports m

addPrelude :: AST.Module a b -> AST.Module a b
addPrelude m = m { AST.mImports = AST.UnqualifiedImport "Prelude" : AST.mImports m }

loadModule :: ModuleLoader -> IORef (HashMap AST.ModuleName AST.LoadedModule) -> AST.ModuleName -> IO AST.LoadedModule
loadModule loader loadedModules moduleName = do
    HashTable.lookup moduleName loadedModules >>= \case
        Just m ->
            return m
        Nothing -> do
            parsedModuleResult <- loader moduleName
            parsedModule <- case parsedModuleResult of
                Left err -> fail $ "Error loading module: " <> err
                Right m -> return $ addPrelude m
            forM_ (importsOf parsedModule) $ \referencedModule ->
                loadModule loader loadedModules referencedModule
            lm <- readIORef loadedModules
            loadedModule <- Typecheck.run lm parsedModule
            HashTable.insert moduleName loadedModule loadedModules
            return loadedModule

loadProgram :: ModuleLoader -> AST.ModuleName -> IO AST.Program
loadProgram loader main = do
    prelude <- loadPrelude
    loadedModules <- newIORef [("Prelude", prelude)]

    mainModule <- loadModule loader loadedModules main

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

newFSModuleLoader :: FilePath -> ModuleLoader
newFSModuleLoader root moduleName = do
    parseModuleFromFile $ FP.combine root $ moduleNameToPath moduleName

newMemoryLoader :: FilePath -> Text -> ModuleLoader
newMemoryLoader fp source moduleName = do
    if moduleName == "Main" then
        parseModuleFromSource fp source
    else
        return $ Left $ "Unknown module: " ++ show moduleName

loadProgramFromFile :: FilePath -> IO AST.Program
loadProgramFromFile path = do
    let (dirname, basename) = FP.splitFileName path
    let loader = newFSModuleLoader dirname
    rootModuleName <- case FP.splitExtension basename of
        (rootModuleName, ".cx") -> return $ fromString rootModuleName
        _ -> fail "Please load .cx file"

    loadProgram loader rootModuleName

loadProgramFromSource :: FilePath -> Text -> IO AST.Program
loadProgramFromSource mainModuleName mainModuleSource = do
    let loader = newMemoryLoader mainModuleName mainModuleSource
    loadProgram loader "Main"
