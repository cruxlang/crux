
module Crux.Main where

import           Crux.Lex
import           Crux.Parse
import           Crux.AST
import qualified Crux.Typecheck     as Typecheck
import           System.Environment (getArgs)

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
                    putStrLn "Lex OK"
                    -- print l'
                    p <- Crux.Parse.parse fn l'
                    case p of
                        Left err -> putStrLn $ "Parse error: " ++ show err
                        Right p' -> do
                            putStrLn "Parse OK"
                            -- print p'
                            typetree <- Typecheck.run (EBlock () p')
                            typetree' <- Typecheck.flatten typetree
                            print typetree'
