module Crux.Main (main) where

import qualified Crux.Error as Error
import qualified Crux.Gen as Gen
import qualified Crux.JSBackend as JS
import Crux.Module (loadProgramFromFile, loadRTSSource)
import Crux.Prelude
import Crux.Project (buildProject, buildProjectAndRunTests, ProjectOptions(..), runJS)
import qualified Data.Text as Text
import Options.Applicative
import Options.Applicative.Types (readerAsk)
import System.Exit (ExitCode (..), exitWith)
import System.IO
import Data.List (isSuffixOf)
import Crux.TrackIO

data Command
    = VersionCommand
    | CompileCommand FilePath
    | RunCommand FilePath
    | BuildCommand ProjectOptions
    | TestCommand ProjectOptions

compileCommand :: Parser Command
compileCommand = CompileCommand <$> argument str mempty

projectOptions :: Parser ProjectOptions
projectOptions = ProjectOptions <$> flag False True (long "watch" <> short 'w' <> help "watch filesystem for changes")

runCommand :: Parser Command
runCommand = RunCommand <$> argument str mempty

parseVersion :: Parser Command
parseVersion = flag' VersionCommand (long "version" <> hidden)

parseCommand :: Parser Command
parseCommand = subparser $ mconcat
    [ command "build" $ (BuildCommand <$> projectOptions) `withInfo` "build project"
    , command "compile" $ compileCommand `withInfo` "compile single file"
    , command "run" $ runCommand `withInfo` "run program file"
    , command "test" $ (TestCommand <$> projectOptions) `withInfo` "test project"
    , command "version" $ pure VersionCommand `withInfo` "print compiler version"
    ]

checkExtension :: ReadM Command
checkExtension = do
    fn <- readerAsk
    if ".cx" `isSuffixOf` fn then
        return $ RunCommand fn
    else
        readerError "implicit run requires .cx extension"

parseSingleScript :: Parser Command
parseSingleScript = argument checkExtension mempty

parseOptions :: Parser Command
parseOptions = parseVersion <|> parseCommand <|> parseSingleScript

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

compileToJS :: FilePath -> IO Text
compileToJS fn = do
    runTrackIO' $ do
        rtsSource <- loadRTSSource
        loadProgramFromFile fn >>= \case
            Left err -> liftIO $ do
                Error.printError stderr err
                exitWith $ ExitFailure 1
            Right program -> liftIO $ do
                program'' <- Gen.generateProgram program
                return $ JS.generateJS rtsSource program''

main :: IO ()
main = do
    cmd <- execParser $ info (helper <*> parseOptions) mempty
    case cmd of
        VersionCommand -> do
            putStrLn "Crux version ???"
        BuildCommand options -> do
            buildProject options
        TestCommand options -> do
            buildProjectAndRunTests options
        CompileCommand fn -> do
            js <- compileToJS fn
            putStr $ Text.unpack js
        RunCommand fn -> do
            js <- compileToJS fn
            runJS $ Text.unpack js
