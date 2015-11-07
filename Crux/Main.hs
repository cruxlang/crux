
module Crux.Main (main) where

import Crux.Prelude
import qualified Data.Text as Text
--import           Crux.AST (Program(..))
import           Crux.Module (loadProgramFromFile)
import           Text.Show.Pretty   (ppShow)
import           System.Exit        (ExitCode (..), exitWith)
import qualified Crux.Gen as Gen
import qualified Crux.Backend.JS as JS
--import qualified System.FilePath    as F
--import System.IO (stderr, hPutStr)
import qualified Options.Applicative as Opt

data Options = Options
     { ast :: Bool
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
        [fn] -> do
            program <- loadProgramFromFile fn
            --let outfile = F.replaceExtension fn "js"
            if ast options then
                putStrLn $ ppShow program
            else do
                program'' <- Gen.generateProgram program
                putStr $ Text.unpack $ JS.generateJS program''
            -- putStrLn $ ppShow (concatMap generateDecl typetree')
             --T.writeFile outfile $ JSTree.renderDocument doc
            exitWith ExitSuccess
