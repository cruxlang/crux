{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Crux.JSGen where

import           Crux.AST
import qualified Crux.JSTree           as JS
import qualified Crux.MutableHashTable as HashTable
import           Data.Foldable         (foldlM)
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as HashMap
import           Data.IORef            (IORef)
import qualified Data.IORef            as IORef
import           Data.List             (foldl')
import           Data.Monoid           ((<>))
import           Data.Text             (Text)
import qualified Data.Text             as T

data Env = Env
    { eNames :: IORef (HashMap Name JS.Name)
    }

mkChildEnv :: Env -> IO Env
mkChildEnv env = do
    eNames' <- HashTable.clone (eNames env)
    return env{eNames=eNames'}

mkName :: Env -> Text -> IO Text
mkName Env{..} prefix =
    let insert number = do
            let n = prefix <> "_" <> (T.pack $ show number)
            r <- HashTable.lookup n eNames
            case r of
                Just _ -> insert (1 + number)
                Nothing -> do
                    HashTable.insert n n eNames
                    return n
    in do
        r <- HashTable.lookup prefix eNames
        case r of
            Nothing -> do
                HashTable.insert prefix prefix eNames
                return prefix
            Just _ ->
                insert (0 :: Int)

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

generateDecl :: Env -> Declaration t -> IO [JS.Statement]
generateDecl env decl = case decl of
    DData _name variants ->
        return $ map (\(Variant variantName vdata) -> generateVariant variantName vdata) variants
    DLet _ name (EFun _ [param] body) -> do
        body' <- generateBlock env body
        return [JS.SFunction (Just name) [param] body']
    DLet _ name expr -> do
        expr' <- generateExpr env expr
        return [JS.SVar name expr']

generateBlock :: Env -> [Expression t] -> IO [JS.Statement]
generateBlock env exprs = fmap concat $ mapM (generateStatementExpr env) exprs

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

generateMatchVars :: JS.Expression -> Pattern2 -> [JS.Statement]
generateMatchVars matchVar patt = case patt of
    PPlaceholder "_" -> []
    PPlaceholder pname ->
        [ JS.SVar pname matchVar ]
    PConstructor _ subpatterns ->
        concat
            [ generateMatchVars (JS.ESubscript matchVar (JS.ELiteral $ JS.LInteger index)) subPattern
            | (index, subPattern) <- zip [1..] subpatterns
            ]

generateStatementExpr :: Env -> Expression t -> IO [JS.Statement]
generateStatementExpr env expr = case expr of
    EBlock _ subExprs -> do
        fmap concat $ mapM (generateStatementExpr env) subExprs

    ELet _ name (EFun _ [param] body) -> do
        body' <- generateBlock env body
        return [JS.SFunction (Just name) [param] body']
    ELet _ name e -> do
        e' <- generateExpr env e
        return [JS.SVar name e']
    EFun _ [param] body -> do
        body' <- generateBlock env body
        return [JS.SFunction Nothing [param] body']
    EFun _ _ _ ->
        error "Andy to fill this in"
    EApp {} -> do
        expr' <- generateExpr env expr
        return [JS.SExpression expr']
    EMatch _ matchExpr cases -> do
        matchVar <- mkName env "match"
        let matchVarIdent = JS.EIdentifier matchVar

            ifBody (Case patt caseExpr) = do
                caseExpr' <- generateStatementExpr env caseExpr
                return $ generateMatchVars matchVarIdent patt <> caseExpr'

            generateIfCascade :: Maybe JS.Statement -> Case a -> IO (Maybe JS.Statement)
            generateIfCascade acc case_@(Case patt _) = do
                print acc
                ifBody' <- ifBody case_
                return $ Just $ JS.SIf (generateMatchCond matchVarIdent patt) (JS.SBlock ifBody') acc

        if_' <- foldlM generateIfCascade Nothing (reverse cases)
        matchExpr' <- generateExpr env matchExpr
        return $ case if_' of
            Nothing ->
                [JS.SVar matchVar matchExpr']
            Just if_ ->
                [JS.SVar matchVar matchExpr', if_]

    EPrint _ ex -> do
        ex' <- generateExpr env ex
        return [JS.SExpression $
            JS.EApplication
                (JS.EIdentifier "console.log")
                (Just ex')
            ]
    EToString _ ex -> do
        ex' <- generateExpr env ex

        return [JS.SExpression $
            JS.EBinOp "+" (JS.ELiteral (JS.LString "" :: JS.Literal)) ex'
            ]
    ELiteral {} -> do
        ex' <- generateExpr env expr
        return [JS.SExpression ex']
    EIdentifier {} -> do
        ex' <- generateExpr env expr
        return [JS.SExpression ex']
    ESemi _ lhs rhs -> do
        l <- generateExpr env lhs
        r <- generateExpr env rhs
        return [JS.SExpression $ JS.ESemi l r]

generateExpr :: Env -> Expression t -> IO JS.Expression
generateExpr env expr = case expr of
    EFun _ [param] body -> do
        body' <- generateBlock env body
        return $ JS.EFunction (Just param) body'
    EApp _ lhs rhs -> do
        l <- generateExpr env lhs
        r <- generateExpr env rhs
        return $ JS.EApplication l (Just r)
    ELiteral _ (LString s)  ->
        return $ JS.ELiteral (JS.LString s)
    ELiteral _ (LInteger i) ->
        return $ JS.ELiteral (JS.LInteger i)
    EIdentifier _ s ->
        return $ JS.EIdentifier s
    ESemi _ lhs rhs -> do
        l <- generateExpr env lhs
        r <- generateExpr env rhs
        return $ JS.EComma l r
    _ -> do
        block' <- generateBlock env [expr]
        return $ JS.EApplication
            (JS.EFunction Nothing block')
            Nothing

generateDocument :: [Declaration t] -> IO [JS.Statement]
generateDocument decls = do
    eNames <- IORef.newIORef HashMap.empty
    let env = Env{..}
    d <- mapM (generateDecl env) decls
    return $ concat d
