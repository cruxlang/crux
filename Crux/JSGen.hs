{-# LANGUAGE OverloadedStrings #-}

module Crux.JSGen where

import           Control.Monad (forM_, when)
import           Crux.AST
import           Data.Monoid   ((<>))
import qualified Data.Text.IO  as T

generateDecl :: Declaration t -> IO ()
generateDecl decl = case decl of
    DData _ _ -> return ()
    DLet _ name expr -> do
        T.putStr $ "var " <> name <> " = "
        generate expr
        T.putStr ";"

generateList :: [Expression t] -> IO ()
generateList exprs =
    when (not $ null exprs) $ do
        forM_ (init exprs) $ \ex -> do
            generate ex
            T.putStr ";"
        T.putStr "return "
        generate (last exprs)
        T.putStr ";"

generate :: Expression t -> IO ()
generate expr = case expr of
    EBlock _ exprs -> do
        T.putStr "(function () {"
        generateList exprs
        T.putStr "})()"

    ELet _ name ex -> do
        T.putStr $ "var " <> name <> " = "
        generate ex
        T.putStr ";"

    EFun _ [arg] body -> do
        T.putStr $ "function (" <> arg <> ") {"
        generateList body
        T.putStr "}"

    EFun _ _ _ -> error "Multi-function arguments shouldn't appear here yet"

    EApp _ lhs rhs -> do
        generate lhs
        T.putStr "("
        generate rhs
        T.putStr ")"

    EPrint _ ex -> do
        T.putStr "console.log("
        generate ex
        T.putStr ")"

    ELiteral _ (LInteger i) ->
        putStr $ show i

    ELiteral _ (LString s) ->
        putStr $ show s

    EIdentifier _ i ->
        T.putStr i

    ESemi _ lhs rhs -> do
        generate lhs
        T.putStr ";"
        generate rhs
