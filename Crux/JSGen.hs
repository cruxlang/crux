{-# LANGUAGE OverloadedStrings #-}

module Crux.JSGen where

import           Crux.AST
import qualified Crux.JSTree   as JS
import qualified Data.Text as T

mkArgName i = T.pack ('a':show i)

blah arity count body
    | count == arity - 1 =
        JS.SReturn $ JS.EFunction (Just $ mkArgName count) body
    | otherwise =
        JS.SReturn $ JS.EFunction (Just $ mkArgName count) [blah arity (count + 1) body]

mkCurriedFunction name numArgs body
    | 1 == numArgs = JS.SFunction (Just name) ["a0"] body
    | otherwise = JS.SFunction (Just name) ["a0"] [blah numArgs 1 body]

generateVariant :: Name -> [Name] -> JS.Statement
generateVariant vname vdata = case vdata of
    [] ->
        JS.SVar vname (JS.EArray [JS.ELiteral $ JS.LString vname])
    _ ->
        mkCurriedFunction vname (length vdata) $
            let argNames = [T.pack ('a':show i) | i <- [0..(length vdata) - 1]]
            -- in JS.SFunction (Just vname) argNames
            in [ JS.SReturn $ JS.EArray $
                  [JS.ELiteral $ JS.LString vname] ++ (map JS.EIdentifier argNames)
                ]

generateDecl :: Declaration t -> [JS.Statement]
generateDecl decl = case decl of
    DData _name variants ->
        map (\(Variant vname vdata) -> generateVariant vname vdata) variants
    DLet _ name (EFun _ [param] body) ->
        [JS.SFunction (Just name) [param] (generateBlock body)]
    DLet _ name expr ->
        [JS.SVar name (generateExpr expr)]

generateBlock :: [Expression t] -> [JS.Statement]
generateBlock exprs = concatMap generateStatementExpr exprs

generateStatementExpr :: Expression t -> [JS.Statement]
generateStatementExpr expr = case expr of
    EBlock _ subExprs ->
        concatMap generateStatementExpr subExprs
    ELet _ name (EFun _ [param] body) ->
        [JS.SFunction (Just name) [param] (generateBlock body)]
    ELet _ name e ->
        [JS.SVar name (generateExpr e)]
    EFun _ [param] body ->
        [JS.SFunction Nothing [param] (generateBlock body)]
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
    EToString _ ex ->
        [JS.SExpression $
            JS.EBinOp "+" (JS.ELiteral (JS.LString "" :: JS.Literal)) (generateExpr ex)
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
