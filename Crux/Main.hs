
module Crux.Main where

import           Control.Applicative
import           Data.Monoid
import           Control.Monad      (forM)
import           Crux.JSGen
import qualified Crux.JSTree        as JSTree
import           Crux.Lex
import           Crux.Parse
import qualified Crux.Typecheck     as Typecheck
import qualified Data.Text.IO       as T
import           System.Environment (getArgs)
import           Text.Show.Pretty   (ppShow)
import           System.Exit        (ExitCode (..), exitWith)
import qualified System.FilePath    as F
import System.IO (stderr, hPutStr)
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
        <> Opt.header "crux - the best AltJS" )

help :: IO ()
help = putStrLn "Pass a single filename as an argument"

failed :: String -> IO a
failed message = do
    hPutStr stderr message
    exitWith $ ExitFailure 1

main :: IO ()
main = do
    options <- parseOptions
    case files options of
        [] -> help
        (_:_:_) -> help
        [fn] -> do
            let outfile = F.replaceExtension fn "js"
            l <- Crux.Lex.lex fn
            case l of
                Left err -> failed $ "Lex error: " ++ show err
                Right l' -> do
                    p <- Crux.Parse.parse fn l'
                    case p of
                        Left err -> failed $ "Parse error: " ++ show err
                        Right p' -> do
                            -- putStrLn $ ppShow p'
                            typetree <- Typecheck.run p'
                            typetree' <- Typecheck.flattenProgram typetree
                            putStrLn $ ppShow typetree'
                            -- putStrLn $ ppShow (concatMap generateDecl typetree')
                            doc <- generateDocument typetree'
                            T.writeFile outfile $ JSTree.renderDocument doc
                            exitWith ExitSuccess
