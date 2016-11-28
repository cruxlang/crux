{-# LANGUAGE ScopedTypeVariables #-}

module Crux.Project
    ( runJS
    , runJSWithResult
    , ProjectOptions(..)
    , buildProject
    , buildProjectAndRunTests
    ) where

import qualified Crux.Error as Error
import qualified Crux.Gen as Gen
import qualified Crux.JSBackend as JSBackend
import Crux.Module
import Crux.Prelude
import Crux.TrackIO
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Yaml
import System.Exit (ExitCode (..))
import qualified System.Exit as Exit
import System.FilePath (combine)
import System.IO
import System.Process (readProcessWithExitCode)

data TargetConfig = TargetConfig
    { tcSourceDir  :: String
    , tcMainModule :: Text
    }

data ProjectConfig = ProjectConfig
    { pcTargets :: Map Text TargetConfig
    , pcTests   :: Map Text TargetConfig
    }

instance FromJSON TargetConfig where
    parseJSON (Object o) = do
        TargetConfig <$> o .: "src-dir" <*> o .: "main-module"
    parseJSON _ = fail "Target must be an object"
instance FromJSON ProjectConfig where
    parseJSON (Object o) = do
        pcTargets <- o .:? "targets" .!= mempty
        pcTests <- o .:? "tests" .!= mempty
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

buildTarget :: Text -> Text -> TargetConfig -> TrackIO ()
buildTarget rtsSource targetName TargetConfig{..} = do
    let targetPath = combine "build" $ Text.unpack targetName ++ ".js"

    loadProgramFromDirectoryAndModule tcSourceDir tcMainModule >>= \case
        Left err -> liftIO $ do
            Error.printError stderr err
            Exit.exitWith $ Exit.ExitFailure 1
        Right program -> liftIO $ do
            program' <- Gen.generateProgram program
            -- TODO: should we track outputs in TrackIO?
            TextIO.writeFile targetPath $ JSBackend.generateJS rtsSource program'
            putStrLn $ "Built " ++ targetPath

data ProjectOptions = ProjectOptions
    { poWatch :: Bool
    }

trackerFromOptions :: ProjectOptions -> TrackIO a -> IO a
trackerFromOptions ProjectOptions{..} =
    if poWatch then loopWithTrackedIO else runUntrackedIO

buildProject :: ProjectOptions -> IO ()
buildProject options = do
    let tracker = trackerFromOptions options
    tracker $ do
        rtsSource <- loadRTSSource
        config <- loadProjectBuild
        for_ (Map.assocs $ pcTargets config) $ \(targetName, targetConfig) -> do
            buildTarget rtsSource targetName targetConfig
        for_ (Map.assocs $ pcTests config) $ \(targetName, targetConfig) -> do
            buildTarget rtsSource targetName targetConfig

buildProjectAndRunTests :: ProjectOptions -> IO ()
buildProjectAndRunTests options = do
    let tracker = trackerFromOptions options
    result <- tracker $ runEitherT $ do
        rtsSource <- lift $ loadRTSSource
        config <- lift $ loadProjectBuild
        for_ (Map.assocs $ pcTests config) $ \(_targetName, TargetConfig{..}) -> do
            (lift $ loadProgramFromDirectoryAndModule tcSourceDir tcMainModule) >>= \case
                Left err -> do
                    liftIO $ Error.printError stderr err
                    left ()
                Right program -> liftIO $ do
                    program' <- Gen.generateProgram program
                    let source = JSBackend.generateJS rtsSource program'
                    (runJSWithResult $ Text.unpack source) >>= \case
                        Left _code -> return $ Left ()
                        Right () -> return $ Right ()
    case result of
        Left () -> Exit.exitWith $ Exit.ExitFailure 1
        Right () -> return ()
