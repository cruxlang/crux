
module Crux.Main where

import           Control.Monad      (forM)
import           Crux.JSGen
import qualified Crux.JSTree        as JSTree
import           Crux.Lex
import           Crux.Parse
import qualified Crux.Typecheck     as Typecheck
import qualified Data.Text.IO       as T
import           System.Environment (getArgs)
-- import           Text.Show.Pretty   (ppShow)
import           System.Exit        (ExitCode (..), exitWith)
import qualified System.FilePath    as F

help :: IO ()
help = putStrLn "Pass a single filename as an argument"

failed message = do
    putStrLn message
    exitWith $ ExitFailure 1

main :: IO ()
main = do
    args <- getArgs

    case args of
        [] -> help
        (_:_:_) -> help
        [fn] -> do
            let outfile = F.replaceExtension fn "js"
            l <- Crux.Lex.lex fn
            case l of
                Left err -> putStrLn $ "Lex error: " ++ show err
                Right l' -> do
                    p <- Crux.Parse.parse fn l'
                    case p of
                        Left err -> putStrLn $ "Parse error: " ++ show err
                        Right p' -> do
                            typetree <- Typecheck.run p'
                            typetree' <- forM typetree Typecheck.flattenDecl
                            T.writeFile outfile $ JSTree.renderDocument (concatMap generateDecl typetree')
                            exitWith ExitSuccess
