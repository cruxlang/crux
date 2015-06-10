{-# LANGUAGE OverloadedStrings #-}

module Crux.JSGen where

import           Crux.AST
import Data.Monoid ((<>))
import Data.List (foldl', foldr)
import qualified Crux.JSTree   as JS
import qualified Data.Text as T

data Env = Env
    {
    }

mkArgName :: Int -> T.Text
mkArgName i = T.pack ('a':show i)

blah :: Int -> Int -> [JS.Statement] -> JS.Statement
blah arity count body
    | count == arity - 1 =
        JS.SReturn $ JS.EFunction (Just $ mkArgName count) body
    | otherwise =
        JS.SReturn $ JS.EFunction (Just $ mkArgName count) [blah arity (count + 1) body]

mkCurriedFunction :: Name -> Int -> [JS.Statement] -> JS.Statement
mkCurriedFunction name numArgs body
    | 1 == numArgs = JS.SFunction (Just name) ["a0"] body
    | otherwise = JS.SFunction (Just name) ["a0"] [blah numArgs 1 body]

generateVariant :: Name -> [Name] -> JS.Statement
generateVariant variantName vdata = case vdata of
    [] ->
        JS.SVar variantName (JS.EArray [JS.ELiteral $ JS.LString variantName])
    _ ->
        mkCurriedFunction variantName (length vdata) $
            let argNames = [T.pack ('a':show i) | i <- [0..(length vdata) - 1]]
            in [ JS.SReturn $ JS.EArray $
                  [JS.ELiteral $ JS.LString variantName] ++ (map JS.EIdentifier argNames)
                ]

generateDecl :: Env -> Declaration t -> [JS.Statement]
generateDecl env decl = case decl of
    DData _name variants ->
        map (\(Variant variantName vdata) -> generateVariant variantName vdata) variants
    DLet _ name (EFun _ [param] body) ->
        [JS.SFunction (Just name) [param] (generateBlock env body)]
    DLet _ name expr ->
        [JS.SVar name (generateExpr env expr)]

generateBlock :: Env -> [Expression t] -> [JS.Statement]
generateBlock env exprs = concatMap (generateStatementExpr env) exprs

-- | Generate an expression which produces the boolean "true" if the variable "matchVar"
-- matches the pattern "patt"
generateMatchCond :: JS.Expression -> Pattern2 -> JS.Expression
generateMatchCond matchVar patt = case patt of
    PPlaceholder _ ->
        JS.ELiteral JS.LTrue
    PConstructor name subpatterns ->
        let testIt = JS.EBinOp "=="
                (JS.ELiteral $ JS.LString name)
                (JS.ESubscript matchVar (JS.ELiteral (JS.LInteger 0)))
            buildTestCascade acc (index, subpattern) = case subpattern of
                PPlaceholder _ -> acc
                _ -> JS.EBinOp "&&"
                    acc
                    (generateMatchCond (JS.ESubscript matchVar (JS.ELiteral (JS.LInteger index))) subpattern)
        in case subpatterns of
            [] -> testIt
            _ -> JS.EBinOp "&&" testIt
                (foldl' buildTestCascade (JS.ELiteral JS.LTrue) (zip [1..] subpatterns))

generateMatchVars matchVar patt = case patt of
    PPlaceholder "_" -> []
    PPlaceholder pname ->
        [ JS.SVar pname matchVar ]
    PConstructor _ subpatterns ->
        concat
            [ generateMatchVars (JS.ESubscript matchVar (JS.ELiteral $ JS.LInteger index)) patt
            | (index, patt) <- zip [1..] subpatterns
            ]

generateStatementExpr :: Env -> Expression t -> [JS.Statement]
generateStatementExpr env expr = case expr of
    EBlock _ subExprs ->
        concatMap (generateStatementExpr env) subExprs
    ELet _ name (EFun _ [param] body) ->
        [JS.SFunction (Just name) [param] (generateBlock env body)]
    ELet _ name e ->
        [JS.SVar name (generateExpr env e)]
    EFun _ [param] body ->
        [JS.SFunction Nothing [param] (generateBlock env body)]
    EFun _ _ _ ->
        error "Andy to fill this in"
    EApp {} ->
        [JS.SExpression $ generateExpr env expr]
    EMatch _ matchExpr cases ->
        let matchVar = "$$match" -- todo compute something unique
            matchVarIdent = JS.EIdentifier matchVar

            ifBody (Case patt caseExpr) =
                generateMatchVars matchVarIdent patt <> generateStatementExpr env caseExpr

            generateIfCascade :: Maybe JS.Statement -> Case a -> Maybe JS.Statement
            generateIfCascade acc case_@(Case patt _) =
                Just $ JS.SIf (generateMatchCond matchVarIdent patt) (JS.SBlock $ ifBody case_) acc

        in case foldl' generateIfCascade Nothing (reverse cases) of
            Nothing -> [JS.SVar matchVar (generateExpr env matchExpr)]
            Just if_ -> [JS.SVar matchVar (generateExpr env matchExpr), if_]
    EPrint _ ex ->
        [JS.SExpression $
            JS.EApplication
                (JS.EIdentifier "console.log")
                (Just $ generateExpr env ex)
        ]
    EToString _ ex ->
        [JS.SExpression $
            JS.EBinOp "+" (JS.ELiteral (JS.LString "" :: JS.Literal)) (generateExpr env ex)
        ]
    ELiteral {} ->
        [JS.SExpression $ generateExpr env expr]
    EIdentifier {} ->
        [JS.SExpression $ generateExpr env expr]
    ESemi _ lhs rhs ->
        [JS.SExpression $ JS.ESemi (generateExpr env lhs) (generateExpr env rhs)]

generateExpr :: Env -> Expression t -> JS.Expression
generateExpr env expr = case expr of
    EFun _ [param] body ->
        JS.EFunction (Just param) (generateBlock env body)
    EApp _ lhs rhs ->
        JS.EApplication (generateExpr env lhs) (Just $ generateExpr env rhs)
    ELiteral _ (LString s)  ->
        JS.ELiteral (JS.LString s)
    ELiteral _ (LInteger i) ->
        JS.ELiteral (JS.LInteger i)
    EIdentifier _ s ->
        JS.EIdentifier s
    ESemi _ lhs rhs ->
        JS.EComma (generateExpr env lhs) (generateExpr env rhs)
    _ ->
        JS.EApplication
            (JS.EFunction Nothing $ generateBlock env [expr])
            Nothing

generateDocument :: [Declaration t] -> IO [JS.Statement]
generateDocument decls = do
    let env = Env{}
    return $ concatMap (generateDecl env) decls
