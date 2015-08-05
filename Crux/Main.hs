
module Crux.Main (main) where

import           Control.Applicative
import           Data.Monoid
import qualified Data.Text as Text
import           Crux.AST (mDecls)
import           Crux.JSGen
import qualified Crux.JSTree        as JSTree
import           Crux.Lex
import           Crux.Parse hiding (module')
import qualified Crux.Typecheck     as Typecheck
import           Text.Show.Pretty   (ppShow)
import           System.Exit        (ExitCode (..), exitWith)
--import qualified System.FilePath    as F
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
            --let outfile = F.replaceExtension fn "js"
            l <- Crux.Lex.lex fn
            case l of
                Left err -> failed $ "Lex error: " ++ show err
                Right l' -> do
                    p <- Crux.Parse.parse fn l'
                    case p of
                        Left err -> failed $ "Parse error: " ++ show err
                        Right module' -> do
                            -- putStrLn $ ppShow p'
                            typetree <- Typecheck.run $ mDecls module'
                            typetree' <- Typecheck.flattenProgram typetree
                            if ast options then
                                putStrLn $ ppShow typetree'
                            else do
                                doc <- generateDocument typetree'
                                putStr $ Text.unpack $ JSTree.renderDocument doc
                            -- putStrLn $ ppShow (concatMap generateDecl typetree')
                             --T.writeFile outfile $ JSTree.renderDocument doc
                            exitWith ExitSuccess
