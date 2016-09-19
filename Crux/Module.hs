{-# LANGUAGE OverloadedStrings #-}

module Crux.Module
    ( importsOf
    , loadModuleFromSource
    , loadProgramFromSource
    , loadProgramFromSources
    , loadProgramFromFile
    , loadProgramFromDirectoryAndModule
    , loadRTSSource

      -- largely for cruxjs
    , pathToModuleName
    , newMemoryLoader
    , loadProgram
    ) where

import Control.Exception (tryJust)
import qualified Crux.AST as AST
import qualified Crux.Error as Error
import qualified Crux.Lex as Lex
import Crux.ModuleName
import Crux.Module.Types as AST
import qualified Crux.HashTable as HashTable
import qualified Crux.Parse as Parse
import Crux.Prelude
import Crux.Pos
import qualified Crux.Typecheck as Typecheck
import Crux.Typecheck.Monad
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import Data.Yaml
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Environment (getExecutablePath)
import qualified System.FilePath as FP
import System.IO.Error (isDoesNotExistError)
import qualified Text.Parsec as P
import qualified Text.Parsec.Error as P

-- Given an import source location and a module name, return a parsed module or an error.
type ModuleLoader = Pos -> ModuleName -> IO (Either Error.Error AST.ParsedModule)

newChainedModuleLoader :: [ModuleLoader] -> ModuleLoader
newChainedModuleLoader = newChainedModuleLoader' []

newChainedModuleLoader' :: [FilePath] -> [ModuleLoader] -> ModuleLoader
newChainedModuleLoader' triedPaths [] pos moduleName = do
    return $ Left $ Error.ModuleNotFound pos moduleName triedPaths
newChainedModuleLoader' triedPaths (loader:rest) pos moduleName = do
    loader pos moduleName >>= \case
        Left (Error.ModuleNotFound _ _ triedPaths') -> newChainedModuleLoader' (triedPaths <> triedPaths') rest pos moduleName
        Left e -> return $ Left e
        Right m -> return $ Right m

moduleNameToPath :: ModuleName -> FilePath
moduleNameToPath (ModuleName prefix m) =
    let toPathSegment (ModuleSegment t) = Text.unpack t in
    FP.combine
        (FP.joinPath $ map toPathSegment prefix)
        (toPathSegment m <> ".cx")

newFSModuleLoader :: FilePath -> ModuleLoader
newFSModuleLoader includePath pos moduleName = do
    let path = includePath FP.</> moduleNameToPath moduleName
    parseModuleFromFile pos moduleName path

newBaseLoader :: IO ModuleLoader
newBaseLoader = do
    config <- loadCompilerConfig
    return $ newFSModuleLoader $ baseLibraryPath config

newProjectModuleLoader :: CompilerConfig -> FilePath -> FilePath -> ModuleLoader
newProjectModuleLoader config root mainModulePath =
    let baseLoader = newFSModuleLoader $ baseLibraryPath config
        projectLoader = newFSModuleLoader root
        mainLoader pos moduleName =
            if moduleName == "main"
            then parseModuleFromFile pos moduleName mainModulePath
            else return $ Left $ Error.ModuleNotFound pos moduleName []
    in newChainedModuleLoader [mainLoader, baseLoader, projectLoader]

newMemoryLoader :: HashMap.HashMap ModuleName Text -> ModuleLoader
newMemoryLoader sources pos moduleName = do
    case HashMap.lookup moduleName sources of
        Just source -> parseModuleFromSource
            ("<" ++ Text.unpack (printModuleName moduleName) ++ ">")
            source
        Nothing ->
            return $ Left $ Error.ModuleNotFound pos moduleName [Text.unpack $ "<memory: " <> printModuleName moduleName <> ">"]

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
    , rtsPath         :: FilePath
    }

instance JSON.FromJSON CompilerConfig where
    parseJSON (JSON.Object o) = do
        baseLibraryPath <- o JSON..: "baseLibraryPath"
        rtsPath <- o JSON..: "rtsPath"
        return $ CompilerConfig{..}
    parseJSON _ = fail "must be object"

loadCompilerConfig :: IO CompilerConfig
loadCompilerConfig = do
    configPath <- findCompilerConfig >>= \case
        Nothing -> fail "Failed to find compiler's cxconfig.yaml"
        Just c -> return c

    config <- decodeFileEither configPath >>= \case
        Left err -> fail $ "Failed to parse cxconfig.yaml:\n" ++ prettyPrintParseException err
        Right c -> return c

    return config
        { baseLibraryPath = FP.combine (FP.takeDirectory configPath) (FP.takeDirectory $ baseLibraryPath config)
        , rtsPath = FP.combine (FP.takeDirectory configPath) (FP.takeDirectory $ rtsPath config)
        }

loadRTSSource :: IO Text
loadRTSSource = do
    config <- loadCompilerConfig
    bytes <- BS.readFile $ FP.combine (rtsPath config) "rts.js"
    return $ TE.decodeUtf8 bytes

posFromSourcePos :: P.SourcePos -> Pos
posFromSourcePos sourcePos = Pos
    { posFileName = P.sourceName sourcePos
    , posLine = P.sourceLine sourcePos
    , posColumn = P.sourceColumn sourcePos
    }

errorFromParseError :: (Pos -> String -> e) -> P.ParseError -> e
errorFromParseError ctor parseError = ctor pos message
  where
    pos = posFromSourcePos $ P.errorPos parseError
    message = stringify (P.errorMessages parseError)
    stringify = P.showErrorMessages
        "or" "unknown parse error"
        "expecting" "unexpected" "end of input"

parseModuleFromSource :: FilePath -> Text -> IO (Either Error.Error AST.ParsedModule)
parseModuleFromSource filename source = do
    case Lex.lexSource filename source of
        Left err -> do
            return $ Left $ errorFromParseError Error.LexError err
        Right tokens -> do
            case Parse.parse filename tokens of
                Left err ->
                    return $ Left $ errorFromParseError Error.ParseError err
                Right mod' ->
                    return $ Right mod'

parseModuleFromFile :: Pos -> ModuleName -> FilePath -> IO (Either Error.Error AST.ParsedModule)
parseModuleFromFile pos moduleName filename = runEitherT $ do
    source <- EitherT $ do
        tryJust (\e -> if isDoesNotExistError e then Just $ Error.ModuleNotFound pos moduleName [filename] else Nothing) $ BS.readFile filename
    EitherT $ parseModuleFromSource filename $ TE.decodeUtf8 source

loadModuleFromSource :: Text -> IO (ProgramLoadResult AST.LoadedModule)
loadModuleFromSource source = runEitherT $ do
    program <- EitherT $ loadProgramFromSource source
    return $ pMainModule program

getModuleName :: AST.Import -> ModuleName
getModuleName (AST.Import mn _) = mn

importsOf :: AST.Module a b c -> [(Pos, ModuleName)]
importsOf m = fmap (fmap getModuleName) $ AST.mImports m

addBuiltin :: AST.Module a b c -> AST.Module a b c
addBuiltin m = m { AST.mImports = (dummyPos, AST.Import "builtin" AST.UnqualifiedImport) : AST.mImports m }

type ProgramLoadResult a = Either Error.Error a

hasNoBuiltinPragma :: AST.Module a b c -> Bool
hasNoBuiltinPragma AST.Module{..} = AST.PNoBuiltin `elem` mPragmas

loadModule ::
       ModuleLoader
    -> IORef (HashMap ModuleName AST.LoadedModule)
    -> IORef (HashSet ModuleName)
    -> Pos
    -> ModuleName
    -> IO (ProgramLoadResult AST.LoadedModule)
loadModule loader loadedModules loadingModules importPos moduleName = runEitherT $ do
    HashTable.lookup moduleName loadedModules >>= \case
        Just m ->
            return m
        Nothing -> do
            loadingModuleSet <- readIORef loadingModules
            when (HashSet.member moduleName loadingModuleSet) $ do
                left $ Error.CircularImport moduleName
            writeIORef loadingModules $ HashSet.insert moduleName loadingModuleSet

            parsedModuleResult <- EitherT $ loader importPos moduleName
            let parsedModule =
                    if hasNoBuiltinPragma parsedModuleResult then
                        parsedModuleResult
                    else
                        addBuiltin parsedModuleResult

            for_ (importsOf parsedModule) $ \(pos, referencedModule) -> do
                EitherT $ loadModule loader loadedModules loadingModules pos referencedModule

            lm <- readIORef loadedModules
            loadedModule <- EitherT $ bridgeTC $ Typecheck.run lm parsedModule moduleName
            HashTable.insert moduleName loadedModule loadedModules
            return loadedModule

loadProgram :: ModuleLoader -> ModuleName -> IO (ProgramLoadResult AST.Program)
loadProgram loader main = runEitherT $ do
    loadingModules <- newIORef mempty
    loadedModules <- newIORef mempty

    let syntaxPos = Pos
            { posFileName = "<syntax>"
            , posLine = 0
            , posColumn = 0
            }
    let loadSyntaxDependency n = void $ EitherT $ loadModule loader loadedModules loadingModules syntaxPos n

    -- any module that uses a unit literal or unit type ident depends on 'void' being loaded
    loadSyntaxDependency "types"
    -- any module that uses tuples depends on 'tuple'
    loadSyntaxDependency "tuple"
    -- any module that uses == or != depends on 'cmp'
    loadSyntaxDependency "cmp"
    -- any module that uses a string literal depends on 'string' being loaded
    loadSyntaxDependency "string"
    -- any module that uses a number literal depends on 'number' being loaded
    loadSyntaxDependency "number"

    mainModule <- EitherT $ loadModule loader loadedModules loadingModules syntaxPos main

    otherModules <- readIORef loadedModules
    return AST.Program
        { AST.pMainModule = mainModule
        , AST.pOtherModules = otherModules
        }

loadProgramFromDirectoryAndModule :: FilePath -> Text -> IO (ProgramLoadResult AST.Program)
loadProgramFromDirectoryAndModule sourceDir mainModule = do
    loadProgramFromFile $ FP.combine sourceDir (Text.unpack mainModule ++ ".cx")

pathToModuleName :: FilePath -> ModuleName
pathToModuleName path =
    case FP.splitPath path of
        [] -> error "pathToModuleName called on empty path"
        segments ->
            let prefix = fmap FP.dropTrailingPathSeparator $ init segments
                base = last segments
            in case FP.splitExtension base of
                (base', ".cx") -> ModuleName (fmap fromString prefix) (fromString base')
                _ -> error "Please load .cx file"

loadProgramFromFile :: FilePath -> IO (ProgramLoadResult AST.Program)
loadProgramFromFile path = do
    config <- loadCompilerConfig
    let (dirname, _basename) = FP.splitFileName path
    let loader = newProjectModuleLoader config dirname path
    loadProgram loader "main"

loadProgramFromSource :: Text -> IO (ProgramLoadResult AST.Program)
loadProgramFromSource mainModuleSource = do
    loadProgramFromSources $ HashMap.fromList [ ("main", mainModuleSource) ]

loadProgramFromSources :: HashMap.HashMap ModuleName Text -> IO (ProgramLoadResult AST.Program)
loadProgramFromSources sources = do
    base <- newBaseLoader
    let mem = newMemoryLoader sources
    let loader = newChainedModuleLoader [mem, base]
    loadProgram loader "main"
