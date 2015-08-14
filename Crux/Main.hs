
module Crux.Main (main) where

import           Data.Monoid
import qualified Data.Text as Text
import           Crux.JSGen
import qualified Crux.JSTree        as JSTree
import           Crux.Module (loadModuleFromFile)
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
            mod' <- loadModuleFromFile fn
            --let outfile = F.replaceExtension fn "js"
            case mod' of
                Left err -> do
                    failed $ show err
                Right module' -> do
                    if ast options then
                        putStrLn $ ppShow module'
                    else do
                        doc <- generateDocument module'
                        putStr $ Text.unpack $ JSTree.renderDocument doc
                    -- putStrLn $ ppShow (concatMap generateDecl typetree')
                     --T.writeFile outfile $ JSTree.renderDocument doc
                    exitWith ExitSuccess
