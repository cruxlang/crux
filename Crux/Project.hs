module Crux.Project
    ( runJS
    , buildProject
    , buildProjectAndRunTests
    ) where

import qualified Crux.AST as AST
import qualified Crux.Error as Error
import qualified Crux.Gen as Gen
import qualified Crux.JSBackend as JSBackend
import Crux.Module
import Crux.Prelude
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

runJS :: String -> IO ()
runJS js = do
    readProcessWithExitCode "node" [] js >>= \case
        (ExitSuccess, stdoutBody, stderrBody) -> do
            -- TODO: just inherit stdout and stderr
            hPutStr stderr stderrBody
            hPutStr stdout stdoutBody
        (ExitFailure code, _, stderrBody) -> do
            fail $ "Process failed with code: " ++ show code ++ "\n" ++ stderrBody

loadProjectBuild :: IO ProjectConfig
loadProjectBuild = do
    decodeFileEither "crux.yaml" >>= \case
        Left err -> fail $ show err
        Right x -> return x

buildTarget :: Text -> Text -> TargetConfig -> IO ()
buildTarget rtsSource targetName TargetConfig{..} = do
    let targetPath = combine "build" $ Text.unpack targetName ++ ".js"

    loadProgramFromDirectoryAndModule tcSourceDir tcMainModule >>= \case
        Left (moduleName, err) -> do
            message <- Error.renderError' err
            Exit.die $ "project build failed\nin module: " ++ Text.unpack (AST.printModuleName moduleName) ++ "\n" ++ message
        Right program -> do
            program' <- Gen.generateProgram program
            TextIO.writeFile targetPath $ JSBackend.generateJS rtsSource program'
            putStrLn $ "Built " ++ targetPath

buildProject :: IO ()
buildProject = do
    rtsSource <- loadRTSSource
    config <- loadProjectBuild
    for_ (Map.assocs $ pcTargets config) $ \(targetName, targetConfig) -> do
        buildTarget rtsSource targetName targetConfig
    for_ (Map.assocs $ pcTests config) $ \(targetName, targetConfig) -> do
        buildTarget rtsSource targetName targetConfig

buildProjectAndRunTests :: IO ()
buildProjectAndRunTests = do
    rtsSource <- loadRTSSource
    config <- loadProjectBuild
    for_ (Map.assocs $ pcTests config) $ \(_targetName, TargetConfig{..}) -> do
        loadProgramFromDirectoryAndModule tcSourceDir tcMainModule >>= \case
            Left (moduleName, err) -> do
                message <- Error.renderError' err
                Exit.die $ "test build failed\nin module: " ++ Text.unpack (AST.printModuleName moduleName) ++ "\n" ++ message
            Right program -> do
                program' <- Gen.generateProgram program
                let source = JSBackend.generateJS rtsSource program'
                runJS $ Text.unpack source
