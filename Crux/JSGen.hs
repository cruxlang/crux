{-# LANGUAGE OverloadedStrings #-}

module Crux.JSGen where

import           Control.Monad (forM_, when)
import           Crux.AST
import qualified Crux.JSTree   as JS
import           Data.Monoid   ((<>))
import qualified Data.Text.IO  as T

generateDecl' :: Declaration t -> [JS.Statement]
generateDecl' decl = case decl of
    DData _name variants ->
        map (\v -> JS.SVar v (JS.ELiteral $ JS.LString v)) variants
    DLet _ name (EFun _ [param] body) ->
        [JS.SFunction (Just name) (Just param) (generateBlock body)]
    DLet _ name expr ->
        [JS.SVar name (generateExpr expr)]

generateBlock :: [Expression t] -> [JS.Statement]
generateBlock exprs = concatMap generateStatementExpr exprs

generateStatementExpr :: Expression t -> [JS.Statement]
generateStatementExpr expr = case expr of
    EBlock _ subExprs ->
        concatMap generateStatementExpr subExprs
    ELet _ name (EFun _ [param] body) ->
        [JS.SFunction (Just name) (Just param) (generateBlock body)]
    ELet _ name e ->
        [JS.SVar name (generateExpr e)]
    EFun _ [param] body ->
        [JS.SFunction Nothing (Just param) (generateBlock body)]
    EFun _ _ _ ->
        error "Andy to fill this in"
    EApp {} ->
        [JS.SExpression $ generateExpr expr]
    EPrint _ ex ->
        [JS.SExpression $
            JS.EApplication
                (JS.EIdentifier "console.log")
                (Just $ generateExpr ex)
        ]
    ELiteral {} ->
        [JS.SExpression $ generateExpr expr]
    EIdentifier {} ->
        [JS.SExpression $ generateExpr expr]
    ESemi _ lhs rhs ->
        [JS.SExpression $ JS.ESemi (generateExpr lhs) (generateExpr rhs)]

generateExpr :: Expression t -> JS.Expression
generateExpr expr = case expr of
    EApp _ lhs rhs ->
        JS.EApplication (generateExpr lhs) (Just $ generateExpr rhs)
    ELiteral _ (LString s)  ->
        JS.ELiteral (JS.LString s)
    ELiteral _ (LInteger i) ->
        JS.ELiteral (JS.LInteger i)
    EIdentifier _ s ->
        JS.EIdentifier s
    EFun _ [param] body ->
        JS.EFunction (Just param) (generateBlock body)
        -- ESemi translates to JS comma operator
    _ ->
        JS.EApplication
            (JS.EFunction Nothing $ generateBlock [expr])
            Nothing

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
