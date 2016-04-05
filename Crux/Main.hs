module Crux.Main (main) where

import qualified Crux.Error          as Error
import qualified Crux.Gen            as Gen
import qualified Crux.JSBackend      as JS
import           Crux.Module         (loadProgramFromFile, loadRTSSource)
import           Crux.Prelude
import           Crux.Project        (buildProject)
import qualified Data.Text           as Text
import qualified Options.Applicative as Opt
import           System.Exit         (ExitCode (..), exitWith)
import           System.IO
import           System.Process      (readProcessWithExitCode)

data Options = Options
     { files :: [String]
     }

optionsParser :: Opt.Parser Options
optionsParser = Options
    <$> Opt.some (Opt.strArgument (Opt.metavar "SOURCE"))

parseOptions :: IO Options
parseOptions = Opt.execParser $
    Opt.info (Opt.helper <*> optionsParser) (
        Opt.fullDesc
        <> Opt.progDesc "Compile Crux into JavaScript"
        <> Opt.header "Crux - the best AltJS" )

help :: IO ()
help = putStrLn "Pass a single filename as an argument"

{-
failed :: String -> IO a
failed message = do
    hPutStr stderr message
    exitWith $ ExitFailure 1
-}

runJS :: String -> IO ()
runJS js = do
    readProcessWithExitCode "node" [] js >>= \case
        (ExitSuccess, stdoutBody, stderrBody) -> do
            -- TODO: just inherit stdout and stderr
            hPutStr stderr stderrBody
            hPutStr stdout stdoutBody
        (ExitFailure code, _, stderrBody) -> do
            fail $ "Process failed with code: " ++ show code ++ "\n" ++ stderrBody

main :: IO ()
main = do
    options <- parseOptions
    case files options of
        [] -> help
        ["build"] -> do
            buildProject
        ["run", fn] -> do
            rtsSource <- loadRTSSource
            loadProgramFromFile fn >>= \case
                Left (moduleName, err) -> do
                    Error.renderError moduleName err >>= hPutStrLn stderr
                    exitWith $ ExitFailure 1
                Right program -> do
                    program'' <- Gen.generateProgram program
                    let js = JS.generateJS rtsSource program''
                    runJS $ Text.unpack js
        [fn] -> do
            rtsSource <- loadRTSSource
            loadProgramFromFile fn >>= \case
                Left (moduleName, err) -> do
                    Error.renderError moduleName err >>= hPutStrLn stderr
                    exitWith $ ExitFailure 1
                Right program -> do
                    program'' <- Gen.generateProgram program
                    putStr $ Text.unpack $ JS.generateJS rtsSource program''
                    exitWith ExitSuccess
        _ -> help
