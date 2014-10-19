
module Main where

import System.Environment (getArgs)

import Crux.Lex
import Crux.Parse
import Crux.Gen

import LLVM.General.Context
import LLVM.General.Module
--import Control.Monad (void)
import Control.Monad.Except (runExceptT)
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
                            m <- withContext $ \context ->
                                runExceptT $ withModuleFromAST context (Crux.Gen.gen "main" [p']) $ \newModule -> do
                                    llstr <- moduleLLVMAssembly newModule
                                    putStrLn llstr
                                    return newModule
                            case m of
                                Left err -> putStrLn $ "Codegen error: " ++ err
                                Right _ -> return ()
