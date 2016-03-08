module Crux.Project (buildProject) where

import Crux.Prelude
import Data.Yaml
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.FilePath (combine)
import qualified Data.Text.IO as TextIO
import qualified Data.Text as Text
import Crux.Module
import qualified Crux.AST as AST
import qualified Crux.Gen as Gen
import qualified Crux.JSBackend as JSBackend
import qualified Crux.Error as Error
import qualified System.Exit as Exit

data TargetConfig = TargetConfig
    { tcSourceDir :: String
    , tcMainModule :: Text
    }

data ProjectConfig = ProjectConfig
    { pcTargets :: Map Text TargetConfig
    }

instance FromJSON TargetConfig where
    parseJSON (Object o) = do
        TargetConfig <$> o .: "src-dir" <*> o .: "main-module"
    parseJSON _ = fail "Target must be an object"
instance FromJSON ProjectConfig where
    parseJSON (Object o) = do
        ProjectConfig <$> o .: "targets"
    parseJSON _ = fail "Config must be an object"

loadProjectBuild :: IO ProjectConfig
loadProjectBuild = do
    decodeFileEither "crux.yaml" >>= \case
        Left err -> fail $ show err
        Right x -> return x

buildTarget :: Text -> TargetConfig -> IO ()
buildTarget targetName TargetConfig{..} = do
    let targetPath = combine "build" $ Text.unpack targetName ++ ".js"

    loadProgramFromDirectoryAndModule tcSourceDir tcMainModule >>= \case
        Left (moduleName, err) -> do
            message <- Error.renderError' err
            Exit.die $ "project build failed\nin module: " ++ Text.unpack (AST.printModuleName moduleName) ++ "\n" ++ message
        Right program -> do
            program' <- Gen.generateProgram program
            TextIO.writeFile targetPath $ JSBackend.generateJS program'
            putStrLn $ "Built " ++ targetPath

buildProject :: IO ()
buildProject = do
    config <- loadProjectBuild
    for_ (Map.assocs $ pcTargets config) $ \(targetName, targetConfig) -> do
        buildTarget targetName targetConfig
