{-# LANGUAGE OverloadedStrings, OverloadedLists, QuasiQuotes #-}

module Crux.Module where

import Crux.Prelude
import Control.Exception (throwIO, ErrorCall(..))
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import Text.RawString.QQ (r)
import qualified Crux.AST as AST
import qualified Crux.Lex as Lex
import qualified Crux.Parse as Parse
import qualified Crux.Typecheck as Typecheck
import qualified Crux.MutableHashTable as HashTable
import qualified System.FilePath as FP

preludeSource :: Text
preludeSource = Text.pack $ [r|
data jsffi Boolean {
    True = true;
    False = false;
};
|]

type LoadedModules = HashMap AST.ModuleName

type ModuleLoader = AST.ModuleName -> IO (Either String AST.ParsedModule)

data Program = Program
    { pMainModule :: AST.LoadedModule
    , pOtherModules :: HashMap AST.ModuleName AST.LoadedModule
    }

defaultModuleLoader :: ModuleLoader
defaultModuleLoader name = do
    if name == "Prelude" then
        parseModuleFromSource "Prelude" preludeSource
    else
        return $ Left $ "unknown module: " <> Text.unpack name

loadPrelude :: IO AST.LoadedModule
loadPrelude = do
    rv <- loadModuleFromSource' [] "Prelude" preludeSource
    case rv of
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

loadModuleFromSource' :: HashMap AST.ModuleName AST.LoadedModule -> FilePath -> Text -> IO (Either String AST.LoadedModule)
loadModuleFromSource' loadedModules filename source = do
    p <- parseModuleFromSource filename source
    case p of
        Left err ->
            return $ Left err
        Right mod' -> do
            fmap Right $ Typecheck.run loadedModules mod'

loadModuleFromSource :: FilePath -> Text -> IO (Either String AST.LoadedModule)
loadModuleFromSource filename source = do
    prelude <- loadPrelude
    loadModuleFromSource' [("Prelude", prelude)] filename source

loadModuleFromFile :: FilePath -> IO (Either String AST.LoadedModule)
loadModuleFromFile filename = do
    source <- BS.readFile filename
    loadModuleFromSource filename $ TE.decodeUtf8 source

importsOf :: AST.Module a b -> [AST.ModuleName]
importsOf m = do
    map (\(AST.UnqualifiedImport i) -> i) $ AST.mImports m

loadModule :: ModuleLoader -> IORef (HashMap AST.ModuleName AST.LoadedModule) -> AST.ModuleName -> IO AST.LoadedModule
loadModule loader loadedModules moduleName = do
    loadedAlready <- HashTable.lookup moduleName loadedModules
    case loadedAlready of
        Just m ->
            return m
        Nothing -> do
            parsedModuleResult <- loader moduleName
            parsedModule <- case parsedModuleResult of
                Left err -> fail $ "Error loading module: " <> err
                Right m -> return m
            forM_ (importsOf parsedModule) $ \referencedModule ->
                loadModule loader loadedModules referencedModule
            lm <- readIORef loadedModules
            loadedModule <- Typecheck.run lm parsedModule
            HashTable.insert moduleName loadedModule loadedModules
            return loadedModule

loadProgram :: ModuleLoader -> AST.ModuleName -> IO Program
loadProgram loader main = do
    prelude <- loadPrelude
    loadedModules <- newIORef [("Prelude", prelude)]

    mainModule <- loadModule loader loadedModules main

    otherModules <- readIORef loadedModules
    return Program
        { pMainModule = mainModule
        , pOtherModules = otherModules
        }

newFSModuleLoader :: FilePath -> ModuleLoader
newFSModuleLoader root moduleName = do
    parseModuleFromFile $ FP.combine root $ Text.unpack moduleName <> ".crux"

loadProgramFromFile :: FilePath -> IO Program
loadProgramFromFile path = do
    let (dirname, basename) = FP.splitFileName path
    let loader = newFSModuleLoader dirname
    rootModuleName <- case FP.splitExtension basename of
        (rootModuleName, ".crux") -> return $ Text.pack rootModuleName
        _ -> fail "Please load .crux file"

    loadProgram loader rootModuleName
