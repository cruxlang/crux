
module Main where

import System.Environment (getArgs)

import Crux.Lex
import Crux.Parse
-- import Crux.Gen

--import Control.Monad (void)
import Control.Monad.Error (runErrorT)
--import Control.Monad.Error

help :: IO ()
help = putStrLn "Pass a single filename as an argument"

main :: IO ()
main = do
    args <- getArgs

    case args of
        [] -> help
        (_:_:_) -> help
        [fn] -> do
            l <- Crux.Lex.lex fn
            case l of
                Left err -> putStrLn $ "Lex error: " ++ show err
                Right l' -> do
                    let p = Crux.Parse.parse fn l'
                    case p of
                        Left err -> putStrLn $ "Parse error: " ++ show err
                        Right p' -> do
                            putStrLn "Parse OK"
                            print p'
