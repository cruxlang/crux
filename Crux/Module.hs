{-# LANGUAGE OverloadedStrings #-}

module Crux.Module
    ( importsOf
    , loadModuleFromSource
    , loadProgramFromSource
    , loadProgramFromSources
    , loadProgramFromFile
    , loadProgramFromDirectoryAndModule
    , loadRtsSource
    , loadJsRtsSource
    , loadLuaRtsSource

    , CompilerConfig(..)
    , loadCompilerConfig

      -- largely for cruxjs
    , MainModuleMode(..)
    , pathToModuleName
    , newMemoryLoader
    , loadProgram
    ) where

import qualified Crux.AST as AST
import Crux.Error (Error(..), ErrorType(..))
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
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import Data.Char (isSpace)
import qualified Data.Yaml as Yaml
import System.Directory (getCurrentDirectory)
import System.Environment (getExecutablePath)
import qualified System.FilePath as FP
import qualified Text.Parsec as P
import qualified Text.Parsec.Error as P
import Crux.TrackIO

-- Given an import source location and a module name, return a parsed module or an error.
type ModuleLoader = Pos -> ModuleName -> TrackIO (Either Error (FilePath, AST.ParsedModule))

newChainedModuleLoader :: [ModuleLoader] -> ModuleLoader
newChainedModuleLoader = newChainedModuleLoader' []

newChainedModuleLoader' :: [FilePath] -> [ModuleLoader] -> ModuleLoader
newChainedModuleLoader' triedPaths [] pos moduleName = do
    return $ Left $ Error pos $ ModuleNotFound moduleName triedPaths
newChainedModuleLoader' triedPaths (loader:rest) pos moduleName = do
    loader pos moduleName >>= \case
        Left (Error _ (ModuleNotFound _ triedPaths')) -> do
            newChainedModuleLoader' (triedPaths <> triedPaths') rest pos moduleName
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

newBaseLoader :: CompilerConfig -> ModuleLoader
newBaseLoader config = newFSModuleLoader $ ccBaseLibraryPath config

newProjectModuleLoader :: CompilerConfig -> FilePath -> FilePath -> ModuleLoader
newProjectModuleLoader config root mainModulePath =
    let baseLoader = newFSModuleLoader $ ccBaseLibraryPath config
        projectLoader = newFSModuleLoader root
        mainLoader pos moduleName =
            if moduleName == "main" then do
                parseModuleFromFile pos moduleName mainModulePath >>= \case
                    Left (Error errorPos (ModuleNotFound _ _)) -> return $ Left $ Error errorPos $ MainModuleNotFound mainModulePath
                    Left err -> return $ Left err
                    Right rv -> return $ Right rv
            else do
                return $ Left $ Error pos $ ModuleNotFound moduleName []
    in newChainedModuleLoader [mainLoader, baseLoader, projectLoader]

newMemoryLoader :: HashMap.HashMap ModuleName Text -> ModuleLoader
newMemoryLoader sources pos moduleName = do
    case HashMap.lookup moduleName sources of
        Just source -> runExceptT $ do
            mod' <- ExceptT $ parseModuleFromSource
                ("<" ++ Text.unpack (printModuleName moduleName) ++ ">")
                source
            return ("<memory:" <> show moduleName <> ">", mod')
        Nothing ->
            return $ Left $ Error pos $ ModuleNotFound moduleName [Text.unpack $ "<memory: " <> printModuleName moduleName <> ">"]

findCompilerConfig :: FilePath -> TrackIO (Maybe (FilePath, ByteString))
findCompilerConfig filename = do
    -- First, search for a cxconfig relative to the executable path.
    -- If that doesn't work, search for a cxconfig relative to the current directory.
    -- Importantly, this has to work in the playground.
    exePath <- liftIO getExecutablePath
    loop exePath >>= \case
        Just success -> do
            return $ Just success
        Nothing -> do
            cwd <- liftIO getCurrentDirectory
            loop cwd

  where
    loop current = do
        let configPath = FP.combine current filename
        readTrackedFile configPath >>= \case
            Left _err -> do
                let parent = FP.takeDirectory current
                if parent == current then
                    return Nothing
                else
                    loop parent
            Right bytes -> do
                return $ Just (configPath, bytes)

data CompilerConfig = CompilerConfig
    { ccBaseLibraryPath :: !FilePath
    , ccRTSPath         :: !FilePath
    , ccTemplatePath    :: !FilePath
    }

instance JSON.FromJSON CompilerConfig where
    parseJSON (JSON.Object o) = do
        ccBaseLibraryPath <- o JSON..: "baseLibraryPath"
        ccRTSPath <- o JSON..: "rtsPath"
        ccTemplatePath <- o JSON..: "templatePath"
        return $ CompilerConfig{..}
    parseJSON _ = fail "must be object"

loadCompilerConfig :: FilePath -> TrackIO CompilerConfig
loadCompilerConfig filename = do
    (configPath, configBytes) <- findCompilerConfig filename >>= \case
        Nothing -> fail $ "Failed to find compiler's " <> filename
        Just c -> return c

    config <- case Yaml.decodeEither' configBytes of
        Left err -> fail $ "Failed to parse cxconfig.yaml:\n" ++ show err
        Right c -> return c

    return config
        { ccBaseLibraryPath = FP.combine (FP.takeDirectory configPath) (FP.takeDirectory $ ccBaseLibraryPath config)
        , ccRTSPath = FP.combine (FP.takeDirectory configPath) (FP.takeDirectory $ ccRTSPath config)
        , ccTemplatePath = FP.combine (FP.takeDirectory configPath) (FP.takeDirectory $ ccTemplatePath config)
        }

loadRtsSource :: FilePath -> FilePath -> TrackIO Text
loadRtsSource configName sourceName = do
    config <- loadCompilerConfig configName

    readTrackedTextFile (FP.combine (ccRTSPath config) sourceName) >>= \case
        Left _err -> fail $ "Failed to read " <> sourceName <> " file"
        Right src -> return src

loadJsRtsSource :: TrackIO Text
loadJsRtsSource = loadRtsSource "cxconfig.yaml" "rts.js"

loadLuaRtsSource :: TrackIO Text
loadLuaRtsSource = loadRtsSource "cxconfig.lua.yaml" "rts.lua"

posFromSourcePos :: P.SourcePos -> Pos
posFromSourcePos sourcePos = Pos $ PosRec
    { posFileName = P.sourceName sourcePos
    , posLine = P.sourceLine sourcePos
    , posColumn = P.sourceColumn sourcePos
    }

errorFromParseError :: (String -> ErrorType) -> P.ParseError -> Error
errorFromParseError ctor parseError = Error pos $ ctor message
  where
    pos = posFromSourcePos $ P.errorPos parseError
    message = dropWhile isSpace $ stringify (P.errorMessages parseError)
    stringify = P.showErrorMessages
        "or" "unknown parse error"
        "expecting" "unexpected" "end of input"

parseModuleFromSource :: FilePath -> Text -> TrackIO (Either Error AST.ParsedModule)
parseModuleFromSource filename source = do
    case Lex.lexSource filename source of
        Left err -> do
            return $ Left $ errorFromParseError LexError err
        Right tokens -> do
            case Parse.parse filename tokens of
                Left err ->
                    return $ Left $ errorFromParseError ParseError err
                Right mod' ->
                    return $ Right mod'

parseModuleFromFile :: Pos -> ModuleName -> FilePath -> TrackIO (Either Error (FilePath, AST.ParsedModule))
parseModuleFromFile pos moduleName filename = runExceptT $ do
    source <- ExceptT $ do
        --liftIO $ putStrLn $ "reading " <> show filename
        readTrackedTextFile filename >>= \case
            Left _err -> do
                -- TODO: limit to isDoesNotExistError like the old code
                return $ Left $ Error pos $ ModuleNotFound moduleName [filename]
            Right source -> do
                return $ Right source
    mod' <- ExceptT $ do
        parseModuleFromSource filename source
    return (filename, mod')

loadModuleFromSource :: Text -> TrackIO (ProgramLoadResult AST.LoadedModule)
loadModuleFromSource source = runExceptT $ do
    program <- ExceptT $ loadProgramFromSource source
    return $ pMainModule program

getModuleName :: AST.Import -> ModuleName
getModuleName (AST.Import mn _) = mn

importsOf :: AST.Module a b c -> [(Pos, ModuleName)]
importsOf m = fmap (fmap getModuleName) $ AST.mImports m

addBuiltin :: AST.Module a b c -> AST.Module a b c
addBuiltin m = m { AST.mImports = (dummyPos, AST.Import "builtin" AST.UnqualifiedImport) : AST.mImports m }

type ProgramLoadResult a = Either Error a

hasNoBuiltinPragma :: AST.Module a b c -> Bool
hasNoBuiltinPragma AST.Module{..} = AST.PNoBuiltin `elem` mPragmas

loadModule ::
       ModuleLoader
    -> (AST.ParsedModule -> AST.ParsedModule)
    -> IORef (HashMap ModuleName AST.LoadedModule)
    -> IORef (HashSet ModuleName)
    -> Pos
    -> ModuleName
    -> TrackIO (ProgramLoadResult AST.LoadedModule)
loadModule loader transformer loadedModules loadingModules importPos moduleName = runExceptT $ do
    HashTable.lookup moduleName loadedModules >>= \case
        Just m ->
            return m
        Nothing -> do
            loadingModuleSet <- readIORef loadingModules
            when (HashSet.member moduleName loadingModuleSet) $ do
                throwError $ Error importPos $ CircularImport moduleName
            writeIORef loadingModules $ HashSet.insert moduleName loadingModuleSet

            (filePath, parsedModuleResult) <- ExceptT $ loader importPos moduleName
            let parsedModule = transformer $
                    if hasNoBuiltinPragma parsedModuleResult then
                        parsedModuleResult
                    else
                        addBuiltin parsedModuleResult

            for_ (importsOf parsedModule) $ \(pos, referencedModule) -> do
                ExceptT $ loadModule loader id loadedModules loadingModules pos referencedModule

            lm <- readIORef loadedModules
            loadedModule <- ExceptT $ liftIO $ bridgeTC filePath $ Typecheck.run lm parsedModule moduleName
            HashTable.insert moduleName loadedModule loadedModules
            return loadedModule

addMainCall :: FilePath -> AST.ParsedModule -> AST.ParsedModule
addMainCall filename AST.Module{..} = AST.Module
    { mPragmas = mPragmas
    , mImports = mImports
    , mDecls = mDecls ++ [mainCallDecl]
    }
  where
    mainCallDecl = AST.Declaration AST.NoExport sourcePos mainCallDeclType
    mainCallDeclType = AST.DLet sourcePos AST.Immutable AST.PWildcard [] Nothing mainCall
    mainCall = AST.EApp sourcePos (AST.EIdentifier sourcePos (AST.UnqualifiedReference "main")) []
    -- TODO: add a Pos variant to represent this special case
    sourcePos = GeneratedMainCall filename

data MainModuleMode
    = AddMainCall
    | NoTransformation

loadProgram :: MainModuleMode -> ModuleLoader -> FilePath -> ModuleName -> TrackIO (ProgramLoadResult AST.Program)
loadProgram mode loader filename main = runExceptT $ do
    loadingModules <- newIORef mempty
    loadedModules <- newIORef mempty

    let syntaxPos = SyntaxDependency filename
    let loadSyntaxDependency n = void $ ExceptT $ loadModule loader id loadedModules loadingModules syntaxPos n

    -- TODO: it would be better to form implicit dependencies from modules that
    -- use the language features below

    -- any module that uses a unit literal or unit type ident depends on 'void' being loaded
    loadSyntaxDependency "types"
    -- any module that uses tuples depends on 'tuple'
    loadSyntaxDependency "tuple"
    -- any module that uses a comparison operator depends on 'cmp'
    loadSyntaxDependency "cmp"
    -- any module that uses a string literal depends on 'string' being loaded
    loadSyntaxDependency "string"
    -- any module that uses an integer literal depends on 'integer' being loaded
    loadSyntaxDependency "integer"
    -- any module that uses an integer literal depends on 'literal' being loaded
    loadSyntaxDependency "literal"
    -- any module that uses a number literal depends on 'number' being loaded
    loadSyntaxDependency "number"
    -- any module that uses a negation operator depends on 'operator'
    loadSyntaxDependency "operator"

    let transformer = case mode of
            AddMainCall -> addMainCall filename
            NoTransformation -> id
    mainModule <- ExceptT $ loadModule loader transformer loadedModules loadingModules syntaxPos main

    otherModules <- readIORef loadedModules
    return AST.Program
        { AST.pMainModule = mainModule
        , AST.pOtherModules = otherModules
        }

loadProgramFromDirectoryAndModule :: MainModuleMode -> FilePath -> Text -> TrackIO (ProgramLoadResult AST.Program)
loadProgramFromDirectoryAndModule mode sourceDir mainModule = do
    loadProgramFromFile mode $ FP.combine sourceDir (Text.unpack mainModule ++ ".cx")

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

loadProgramFromFile :: MainModuleMode -> FilePath -> TrackIO (ProgramLoadResult AST.Program)
loadProgramFromFile mode path = do
    config <- loadCompilerConfig "cxconfig.yaml"
    let (dirname, _basename) = FP.splitFileName path
    let loader = newProjectModuleLoader config dirname path
    loadProgram mode loader path "main"

loadProgramFromSource :: Text -> TrackIO (ProgramLoadResult AST.Program)
loadProgramFromSource mainModuleSource = do
    loadProgramFromSources $ HashMap.fromList [ ("main", mainModuleSource) ]

loadProgramFromSources :: HashMap.HashMap ModuleName Text -> TrackIO (ProgramLoadResult AST.Program)
loadProgramFromSources sources = do
    config <- loadCompilerConfig "cxconfig.yaml"
    let base = newBaseLoader config
    let mem = newMemoryLoader sources
    let loader = newChainedModuleLoader [mem, base]
    loadProgram NoTransformation loader "<source>" "main"
