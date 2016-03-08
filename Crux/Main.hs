module Crux.Main (main) where

import qualified Crux.Error as Error
import qualified Crux.JSBackend     as JS
import qualified Crux.Gen            as Gen
import           Crux.Module         (loadProgramFromFile)
import           Crux.Prelude
import Crux.Project (buildProject)
import qualified Data.Text           as Text
import qualified Options.Applicative as Opt
import           System.Exit         (ExitCode (..), exitWith)
--import           Text.Show.Pretty    (ppShow)
import System.IO

data Options = Options
     { ast   :: Bool
     , files :: [String]
     }

optionsParser :: Opt.Parser Options
optionsParser = Options
    <$> Opt.switch (Opt.long "ast" <> Opt.help "output AST only")
    <*> Opt.some (Opt.strArgument (Opt.metavar "SOURCE"))

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



main :: IO ()
main = do
    options <- parseOptions
    case files options of
        [] -> help
        (_:_:_) -> help
        ["build"] -> do
            buildProject
        [fn] -> do
            loadProgramFromFile fn >>= \case
                Left (moduleName, err) -> do
                    Error.renderError moduleName err >>= hPutStrLn stderr
                    exitWith $ ExitFailure 1
                Right program -> do
                    if ast options then do
                        fail "bitroooottttt"
                        --putStrLn $ ppShow program
                    else do
                        program'' <- Gen.generateProgram program
                        putStr $ Text.unpack $ JS.generateJS program''
                    -- putStrLn $ ppShow (concatMap generateDecl typetree')
                     --T.writeFile outfile $ JSTree.renderDocument doc
                    exitWith ExitSuccess
