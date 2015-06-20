{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Crux.JSGen where

import Debug.Trace (trace)
import           Control.Monad         (forM)
import           Control.Monad.Trans   (lift)
import qualified Control.Monad.Writer  as Writer
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

type JSWrite a = Writer.WriterT [JS.Statement] IO a

-- When rendering an expression, what should we do with the result?
data ExprDestination
    = DReturn
    | DDiscard
    | DAssign JS.Name

mkChildEnv :: Env -> IO Env
mkChildEnv env = do
    eNames' <- HashTable.clone (eNames env)
    return env{eNames=eNames'}

mkName :: Env -> Text -> JSWrite Text
mkName Env{..} prefix =
    let insert number = do
            let n = prefix <> "_" <> (T.pack $ show number)
            r <- lift $ HashTable.lookup n eNames
            case r of
                Just _ -> insert (1 + number)
                Nothing -> do
                    lift $ HashTable.insert n n eNames
                    return n
    in do
        r <- lift $ HashTable.lookup prefix eNames
        case r of
            Nothing -> do
                lift $ HashTable.insert prefix prefix eNames
                return prefix
            Just _ ->
                insert (0 :: Int)

mkArgName :: Int -> T.Text
mkArgName i = T.pack ('a':show i)

blah :: Int -> Int -> [JS.Statement] -> JS.Statement
blah arity count body
    | count == arity - 1 =
        JS.SReturn $ Just $ JS.EFunction (Just $ mkArgName count) body
    | otherwise =
        JS.SReturn $ Just $ JS.EFunction (Just $ mkArgName count) [blah arity (count + 1) body]

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
            in [ JS.SReturn $ Just $ JS.EArray $
                  [JS.ELiteral $ JS.LString variantName] ++ (map JS.EIdentifier argNames)
                ]

generateDecl :: Env -> Declaration t -> IO [JS.Statement]
generateDecl env decl = case decl of
    DData _name variants ->
        return $ map (\(Variant variantName vdata) -> generateVariant variantName vdata) variants
    DLet _ name (EFun funData [param] body) -> do
        body' <- generateBlock env (DAssign name) funData body
        return [JS.SFunction (Just name) [param] body']
    DLet _ name expr -> do
        (expr', written) <- Writer.runWriterT $ generateExpr env expr
        return $ written ++ [JS.SVar name expr']

generateBlock :: Env -> ExprDestination -> t -> [Expression t] -> IO [JS.Statement]
generateBlock env dest blockData exprs = do
    (e', written) <- Writer.runWriterT $ generateStatementExpr env dest (EBlock blockData exprs)
    return (written ++ e')

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

generateStatementExpr :: Env -> ExprDestination -> Expression t -> JSWrite [JS.Statement]
generateStatementExpr env dest expr = case expr of
    EBlock _ [] -> case dest of
        DReturn -> return [JS.SReturn Nothing]
        _ -> return []
    EBlock _ [subExpr] ->
        generateStatementExpr env dest subExpr
    EBlock _ subExprs -> do
        let sei = init subExprs
            sel = last subExprs
        sei' <- fmap concat $ mapM (generateStatementExpr env DDiscard) sei
        sel' <- generateStatementExpr env dest sel
        return (sei' ++ sel')

    ELet letData name (EFun _ [param] body) -> do
        body' <- generateStatementExpr env DReturn (EBlock letData body)
        return [JS.SFunction (Just name) [param] body']
    ELet _ name e -> do
        e' <- generateExpr env e
        return [JS.SVar name e']
    EFun funData [param] body -> do
        body' <- lift $ generateBlock env dest funData body
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
                caseExpr' <- generateStatementExpr env dest caseExpr
                return $ generateMatchVars matchVarIdent patt <> caseExpr'

            generateIfCascade :: Maybe JS.Statement -> Case a -> JSWrite (Maybe JS.Statement)
            generateIfCascade acc case_@(Case patt _) = do
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

        return [emitWriteDestination dest $
              JS.EBinOp "+" (JS.ELiteral (JS.LString "")) ex'
            ]
    ELiteral {} -> do
        ex' <- generateExpr env expr
        return [emitWriteDestination dest ex']
    EIdentifier {} -> do
        ex' <- generateExpr env expr
        return [emitWriteDestination dest ex']
    ESemi _ lhs rhs -> do
        l <- generateExpr env lhs
        r <- generateExpr env rhs
        return [JS.SExpression l, emitWriteDestination dest r]

emitWriteDestination :: ExprDestination -> JS.Expression -> JS.Statement
emitWriteDestination dest expr = case dest of
    DDiscard -> JS.SExpression expr
    DReturn -> JS.SReturn $ Just expr
    DAssign var -> JS.SAssign (JS.EIdentifier var) expr

generateExpr :: Env -> Expression t -> JSWrite JS.Expression
generateExpr env expr = case expr of
    EFun funData [param] body -> do
        body' <- lift $ generateBlock env DReturn funData body
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
    EPrint _ ex -> do
        ex' <- generateExpr env ex
        return $ JS.EApplication
            (JS.EIdentifier "console.log")
            (Just ex')
    _ -> do
        tempName <- mkName env "temp"
        s <- generateStatementExpr env (DAssign tempName) expr
        Writer.tell s
        return $ JS.EIdentifier tempName

generateDocument :: [Declaration t] -> IO [JS.Statement]
generateDocument decls = do
    eNames <- IORef.newIORef HashMap.empty
    let env = Env{..}
    d <- mapM (generateDecl env) decls
    return $ concat d
