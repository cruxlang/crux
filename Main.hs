
module Main where

import System.Environment (getArgs)

import Crux.Lex
import Crux.Parse

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fn] -> do
            l <- Crux.Lex.lex fn
            print l

            case l of
                Left err -> putStrLn $ "Lex error: " ++ show err
                Right l' -> do
                    let p = Crux.Parse.parse fn l'
                    print p

        _ -> return ()
