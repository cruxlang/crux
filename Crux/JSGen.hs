{-# LANGUAGE OverloadedStrings #-}

module Crux.JSGen where

import           Crux.AST
import qualified Crux.JSTree   as JS

generateDecl :: Declaration t -> [JS.Statement]
generateDecl decl = case decl of
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
    ESemi _ lhs rhs ->
        JS.EComma (generateExpr lhs) (generateExpr rhs)
    _ ->
        JS.EApplication
            (JS.EFunction Nothing $ generateBlock [expr])
            Nothing
