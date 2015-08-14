{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Crux.JSGen where

import           Control.Monad.Trans   (lift)
import qualified Control.Monad.Writer  as Writer
import           Crux.AST
import           Crux.JSGen.Types      (Env (..), JSWrite)
import qualified Crux.JSTree           as JS
import qualified Crux.MutableHashTable as HashTable
import           Crux.Prelude
import           Data.Foldable         (foldlM)
import qualified Data.HashMap.Strict   as HashMap
import qualified Data.IORef            as IORef
import qualified Data.Text             as T

-- When rendering an expression, what should we do with the result?
data ExprDestination
    = DReturn
    | DDiscard
    | DAssign JS.Name
    deriving (Show, Eq)

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

generateVariant :: Name -> [TypeIdent] -> JS.Statement
generateVariant variantName vdata = case vdata of
    [] ->
        JS.SVar variantName (Just $ JS.EArray [JS.ELiteral $ JS.LString variantName])
    _ ->
        let argNames = [T.pack ('a':show i) | i <- [0..(length vdata) - 1]]
        in JS.SFunction variantName argNames $
            [ JS.SReturn $ Just $ JS.EArray $
              [JS.ELiteral $ JS.LString variantName] ++ (map JS.EIdentifier argNames)
            ]

generateDecl :: Show t => Env -> Declaration t -> IO [JS.Statement]
generateDecl env decl = case decl of
    DData _name _ variants ->
        return $ map (\(Variant variantName vdata) -> generateVariant variantName vdata) variants
    DFun (FunDef _ name params body) -> do
        -- putStrLn $ show name ++ ": " ++ show body
        body' <- generateBlock env DReturn body
        return [JS.SFunction name params body']
    DLet _ name _ expr -> do
        (expr', written) <- Writer.runWriterT $ generateExpr env expr
        return $ written ++ [JS.SVar name $ Just expr']


generateBlock' :: Env -> ExprDestination -> Expression t -> JSWrite [JS.Statement]
generateBlock' env dest expr = do
    generateStatementExpr env dest expr

generateBlock :: Env -> ExprDestination -> Expression t -> IO [JS.Statement]
generateBlock env dest exprs = do
    (e', written) <- Writer.runWriterT $
        generateBlock' env dest exprs
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
        [ JS.SVar pname $ Just matchVar ]
    PConstructor _ subpatterns ->
        concat
            [ generateMatchVars (JS.ESubscript matchVar (JS.ELiteral $ JS.LInteger index)) subPattern
            | (index, subPattern) <- zip [1..] subpatterns
            ]

generateStatementExpr :: Env -> ExprDestination -> Expression t -> JSWrite [JS.Statement]
generateStatementExpr env dest expr = case expr of
    ELet _ name _ (EFun _ params body) -> do
        body' <- generateBlock' env DReturn body
        return [JS.SFunction name params body']
    ELet _ name _ e -> do
        e' <- generateExpr env e
        return [JS.SVar name $ Just e']
    EFun {} -> do
        ex' <- generateExpr env expr
        return [emitWriteDestination dest ex']
    ERecordLiteral {} -> do
        expr' <- generateExpr env expr
        return [JS.SExpression expr']
    ELookup {} -> do
        expr' <- generateExpr env expr
        return [JS.SExpression expr']
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
                [JS.SVar matchVar $ Just matchExpr']
            Just if_ ->
                [JS.SVar matchVar $ Just matchExpr', if_]

    ESemi _ lhs rhs -> do
        l <- generateExpr env lhs
        r <- generateExpr env rhs
        return [JS.SExpression l, emitWriteDestination dest r]
    ELiteral {} -> do
        ex' <- generateExpr env expr
        return [emitWriteDestination dest ex']
    EIdentifier {} -> do
        ex' <- generateExpr env expr
        return [emitWriteDestination dest ex']
    EBinIntrinsic {} -> do
        ex' <- generateExpr env expr
        return [emitWriteDestination dest ex']
    EIntrinsic _ _ -> do
        ex' <- generateExpr env expr
        return [emitWriteDestination dest ex']
    EIfThenElse {} -> do
        ex' <- generateExpr env expr
        return [emitWriteDestination dest ex']
    EReturn _ rv -> do
        ex' <- generateExpr env rv
        return [JS.SReturn $ Just ex']

emitWriteDestination :: ExprDestination -> JS.Expression -> JS.Statement
emitWriteDestination dest expr = case dest of
    DDiscard -> JS.SExpression expr
    DReturn -> JS.SReturn $ Just expr
    DAssign var -> JS.SAssign (JS.EIdentifier var) expr

data Computation = Computation (Maybe Name)

generateExpr :: Env -> Expression t -> JSWrite JS.Expression
generateExpr env expr = case expr of
    EFun _ params body -> do
        body' <- lift $ generateBlock env DReturn body
        return $ JS.EFunction params body'
    ERecordLiteral _ fields -> do
        fields' <- forM (HashMap.toList fields) $ \(key, fieldExpr) -> do
            fieldExpr' <- generateExpr env fieldExpr
            return (key, fieldExpr')
        return $ JS.EObject $ HashMap.fromList fields'
    ELookup _ subExpr propName -> do
        subExpr' <- generateExpr env subExpr
        return $ JS.ELookup subExpr' propName
    EApp _ lhs rhs -> do
        l <- generateExpr env lhs
        r <- mapM (generateExpr env) rhs
        return $ JS.EApplication l r
    ELiteral _ lit ->
        return $ JS.ELiteral $ case lit of
            LString s -> JS.LString s
            LInteger i -> JS.LInteger i
            LUnit -> JS.LUndefined
    EIdentifier _ s ->
        return $ JS.EIdentifier s
    ESemi _ lhs rhs -> do
        l <- generateExpr env lhs
        r <- generateExpr env rhs
        return $ JS.EComma l r
    EBinIntrinsic _ op lhs rhs -> do
        let sym = case op of
                BIPlus     -> "+"
                BIMinus    -> "-"
                BIMultiply -> "*"
                BIDivide   -> "/"
        l <- generateExpr env lhs
        r <- generateExpr env rhs
        return $ JS.EBinOp sym l r
    EIntrinsic _ intrin -> case intrin of
        IUnsafeJs txt -> do
            return $ JS.ERaw txt
        IUnsafeCoerce arg -> do
            generateExpr env arg
        IPrint args -> do
            exprs <- mapM (generateExpr env) args
            return $ JS.EApplication
                (JS.EIdentifier "console.log")
                exprs
        IToString arg -> do
            arg' <- generateExpr env arg
            return $ JS.EBinOp "+" (JS.ELiteral (JS.LString "")) arg'
    EIfThenElse _ condition ifTrue ifFalse -> do
        condition' <- generateExpr env condition
        ifTrue' <- generateExpr env ifTrue
        ifFalse' <- generateExpr env ifFalse
        return $ JS.ETernary condition' ifTrue' ifFalse'
    _ -> do
        tempName <- mkName env "temp"
        s <- generateStatementExpr env (DAssign tempName) expr
        Writer.tell [JS.SVar tempName Nothing]
        Writer.tell s
        return $ JS.EIdentifier tempName

generateDocument :: Show t => Module t -> IO [JS.Statement]
generateDocument Module{..} = do
    eNames <- IORef.newIORef HashMap.empty
    let env = Env{..}

    let prelude =
            [ JS.SVar "True" (Just $ JS.EIdentifier "true")
            , JS.SVar "False" (Just $ JS.EIdentifier "false")
            ]

    d <- mapM (generateDecl env) $ mDecls
    return $ prelude ++ (concat d)

generateDocumentWithoutPrelude :: Show t => Module t -> IO [JS.Statement]
generateDocumentWithoutPrelude Module{..} = do
    eNames <- IORef.newIORef HashMap.empty
    let env = Env{..}

    d <- mapM (generateDecl env) mDecls
    return $ concat d
