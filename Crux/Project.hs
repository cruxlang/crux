{-# LANGUAGE ScopedTypeVariables, TupleSections #-}

module Crux.Project
    ( runJS
    , runJSWithResult
    , ProjectOptions(..)
    , TargetLanguage(..)
    , buildProject
    , buildProjectAndRunTests
    , createProjectTemplate
    ) where

import qualified Crux.Error as Error
import qualified Crux.Gen as Gen
import qualified Crux.JSBackend as JSBackend
import qualified Crux.LuaBackend as LuaBackend
import Crux.Module
import Crux.Prelude
import Crux.TrackIO
import Data.Map.Strict (Map)
import Data.Char (isSpace)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Yaml
import System.Exit (exitWith, ExitCode (..))
import qualified System.Exit as Exit
import qualified System.FilePath as FP
import System.Directory
import System.IO
import System.Process (readProcessWithExitCode)
import Text.Mustache

data TargetLanguage
    = TargetJS
    | TargetLua

instance FromJSON TargetLanguage where
    parseJSON (String s)
        | s == "js"  = pure TargetJS
        | s == "lua" = pure TargetLua
    parseJSON _ = fail "Allowed target languages are js and lua"

data TargetConfig = TargetConfig
    { tcSourceDir  :: String
    , tcMainModule :: Text
    }

data ProjectConfig = ProjectConfig
    { pcName :: Text
    , pcTargetDir :: FilePath
    , pcLibrary :: Maybe TargetConfig
    , pcPrograms :: Map Text TargetConfig
    , pcTests :: Map Text TargetConfig
    , pcTargetLanguage :: TargetLanguage
    }

instance FromJSON TargetConfig where
    parseJSON (Object o) = do
        TargetConfig <$> o .: "src-dir" <*> o .: "main-module"
    parseJSON _ = fail "Target must be an object"
instance FromJSON ProjectConfig where
    parseJSON (Object o) = do
        targetOptions <- o .:? "target-options" .!= mempty
        projectInfo <- o .: "project"
        pcName <- projectInfo .: "name"
        pcTargetDir <- targetOptions .:? "output-dir" .!= "build"
        pcLibrary <- o .:? "library"
        pcPrograms <- o .:? "programs" .!= mempty
        pcTests <- o .:? "tests" .!= mempty
        pcTargetLanguage <- o .:? "target-language" .!= TargetJS
        return ProjectConfig{..}
    parseJSON _ = fail "Config must be an object"

-- Prints stdout and stderr, returns Left exitCode on failure, Right () on success.
runJSWithResult :: String -> IO (Either Int ())
runJSWithResult js = do
    readProcessWithExitCode "node" [] js >>= \case
        (ExitSuccess, stdoutBody, stderrBody) -> do
            -- TODO: just inherit stdout and stderr
            hPutStr stderr stderrBody
            hPutStr stdout stdoutBody
            return $ Right ()
        (ExitFailure code, _, stderrBody) -> do
            hPutStr stderr stderrBody
            return $ Left code

runJS :: String -> IO ()
runJS js = runJSWithResult js >>= \case
    Left code -> do
        fail $ "Process failed with code: " ++ show code ++ "\n"
    Right () -> do
        return ()

loadProjectBuild :: TrackIO ProjectConfig
loadProjectBuild = do
    -- TODO: actually track this
    liftIO (decodeFileEither "crux.yaml") >>= \case
        Left err -> fail $ show err
        Right x -> return x

ensureDirectoryExists :: FilePath -> TrackIO ()
ensureDirectoryExists path = do
    liftIO $ createDirectoryIfMissing True path

buildTarget :: Text -> Text -> BuildMode -> ProjectConfig -> TargetConfig -> TrackIO (Either () ())
buildTarget rtsSource targetName buildMode ProjectConfig{..} TargetConfig{..} = do
    ensureDirectoryExists pcTargetDir
    let targetPath = FP.combine pcTargetDir $ Text.unpack targetName ++ ".js"

    -- liftIO $ putStrLn $ "buildTarget: " <> show tcSourceDir <> " " <> show tcMainModule
    let mainModuleMode = case buildMode of
            BMProgram -> AddMainCall
            BMLibrary -> NoTransformation
    loadProgramFromDirectoryAndModule mainModuleMode tcSourceDir tcMainModule >>= \case
        Left err -> liftIO $ do
            Error.printError stderr err
            return $ Left ()
        Right program -> liftIO $ do
            program' <- Gen.generateProgram program
            -- TODO: should we track outputs in TrackIO?
            case pcTargetLanguage of
                TargetJS -> TextIO.writeFile targetPath $ JSBackend.generateJS rtsSource program'
                TargetLua -> TextIO.writeFile targetPath $ LuaBackend.generateLua rtsSource program'
            putStrLn $ "Built " ++ targetPath
            return $ Right ()

data ProjectOptions = ProjectOptions
    { poWatch :: Bool
    }

trackerFromOptions :: ProjectOptions -> TrackIO a -> IO a
trackerFromOptions ProjectOptions{..} =
    if poWatch then loopWithTrackedIO else runUntrackedIO

data BuildMode = BMProgram | BMLibrary

enumerateTargets :: ProjectConfig -> [(Text, BuildMode, TargetConfig)]
enumerateTargets ProjectConfig{..} =
    libraryTarget <> fmap (\(x, y) -> (x, BMProgram, y)) (Map.assocs pcPrograms <> Map.assocs pcTests)
  where
    libraryTarget = case pcLibrary of
        Nothing -> []
        Just tc -> [(pcName, BMLibrary, tc)]

loadProjectRtsSource :: ProjectConfig -> TrackIO Text
loadProjectRtsSource ProjectConfig{ pcTargetLanguage } = case pcTargetLanguage of
    TargetLua -> loadLuaRtsSource
    TargetJS -> loadJsRtsSource

buildProject :: ProjectOptions -> IO ()
buildProject options = do
    let tracker = trackerFromOptions options
    result <- tracker $ runExceptT $ do
        config <- lift $ loadProjectBuild
        rtsSource <- lift $ loadProjectRtsSource config
        for_ (enumerateTargets config) $ \(targetName, buildMode, targetConfig) -> do
            ExceptT $ buildTarget rtsSource targetName buildMode config targetConfig
    case result of
        Left () -> Exit.exitWith $ Exit.ExitFailure 1
        Right () -> return ()

buildProjectAndRunTests :: ProjectOptions -> IO ()
buildProjectAndRunTests options = do
    let tracker = trackerFromOptions options
    result <- tracker $ runExceptT $ do
        config <- lift $ loadProjectBuild
        rtsSource <- lift $ loadProjectRtsSource config
        for_ (Map.assocs $ pcTests config) $ \(_targetName, TargetConfig{..}) -> do
            (lift $ loadProgramFromDirectoryAndModule AddMainCall tcSourceDir tcMainModule) >>= \case
                Left err -> do
                    liftIO $ Error.printError stderr err
                    throwError ()
                Right program -> liftIO $ do
                    program' <- Gen.generateProgram program
                    let source = JSBackend.generateJS rtsSource program'
                    (runJSWithResult $ Text.unpack source) >>= \case
                        Left _code -> return $ Left ()
                        Right () -> return $ Right ()
    case result of
        Left () -> Exit.exitWith $ Exit.ExitFailure 1
        Right () -> return ()

data ProjectTemplate = ProjectTemplate
    { ptName :: !String
    , ptDescription :: !String
    , ptLicense :: !String
    , ptAuthor :: !String
    , ptTargetName :: !String
    , ptTestName :: !String
    }

instance ToMustache ProjectTemplate where
    toMustache ProjectTemplate{..} = Text.Mustache.object
        [ "name" ~> ptName
        , "description" ~> ptDescription
        , "license" ~> ptLicense
        , "author" ~> ptAuthor
        , "targetName" ~> ptTargetName
        , "testName" ~> ptTestName
        ]

readInputString :: String -> String -> IO String
readInputString prompt def = do
    putStr $ prompt <> (if def == "" then ": " else " (" <> def <> "): ")
    hFlush stdout
    line <- getLine
    return $ if line == "" then def else line

strip :: String -> String
strip = lstrip . rstrip
lstrip :: String -> String
lstrip = dropWhile isSpace
rstrip :: String -> String
rstrip = reverse . lstrip . reverse

softReadCommand :: String -> [String] -> IO String
softReadCommand command args = do
    readProcessWithExitCode command args "" >>= \case
        (ExitSuccess, stdout', _) -> return stdout'
        (ExitFailure _, _, _) -> return ""

createProjectTemplate :: IO ()
createProjectTemplate = do
    -- check for existing project file
    alreadyExists <- doesFileExist "crux.yaml"
    if alreadyExists then do
        hPutStrLn stderr "crux.yaml already exists -- refusing to overwrite"
        exitWith $ ExitFailure 1
    else do
        return ()

    currentDirectory <- getCurrentDirectory
    let defaultProjectName = FP.takeBaseName currentDirectory
    defaultAuthor <- strip <$> softReadCommand "git" ["config", "user.name"]

    -- TODO: fold email into the username
    -- defaultEmail <- readCommand "git config user.email"

    putStrLn "Welcome to Crux!"
    putStrLn ""
    putStrLn "This script will guide you through creating a Crux project."
    putStrLn ""
    ptName <- readInputString "Project name" defaultProjectName
    ptDescription <- readInputString "One-line project description" ""
    ptLicense <- readInputString "License" "ISC"
    ptAuthor <- readInputString "Author" defaultAuthor
    ptTargetName <- readInputString "Target name" defaultProjectName
    ptTestName <- readInputString "Test name" "test"

    let projectTemplate = ProjectTemplate{..}

    -- read crux.yaml.template

    compilerConfig <- runUntrackedIO $ loadCompilerConfig "cxconfig.yaml"

    Right template <- automaticCompile [ccTemplatePath compilerConfig] "crux.yaml.template"
    let resultText = substitute template projectTemplate

    -- write resultText to crux.yaml

    TextIO.writeFile "crux.yaml" resultText

    putStrLn "\ncrux.yaml has been created!"
    putStrLn ""
    putStrLn "Try running `crux build` or `crux test`"
    putStrLn ""
    putStrLn "Both commands accept a --watch option for automatically rerunning when files change."

    -- TODO:
    -- mkdir src
    -- mkdir tests
