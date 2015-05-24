{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Crux.Typecheck where

import           Control.Monad         (forM, forM_, when)
import           Crux.AST
import qualified Crux.MutableHashTable as HashTable
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as HashMap
import           Data.IORef            (IORef, readIORef, writeIORef)
import qualified Data.IORef            as IORef
import           Data.Text             (Text)
import           Prelude               hiding (String)

data Type
    = Number
    | String
    | Unit
    deriving (Eq, Show)

data VarLink a
    = Unbound
    | Link a
    deriving (Show, Eq)

data TypeVar
    = TVar Int (IORef (VarLink TypeVar))
    | TQuant Int
    | TFun [TypeVar] TypeVar
    | TType Type

data ImmutableTypeVar
    = IVar Int (VarLink ImmutableTypeVar)
    | IQuant Int
    | IFun [ImmutableTypeVar] ImmutableTypeVar
    | IType Type
    deriving (Show, Eq)

data Env = Env
    { eNextTypeIndex :: IORef Int
    , eBindings      :: IORef (HashMap Text TypeVar)
    , eIsTopLevel    :: Bool
    }

newEnv :: IO Env
newEnv = do
    eNextTypeIndex <- IORef.newIORef 0
    eBindings <- IORef.newIORef HashMap.empty
    let eIsTopLevel = True
    return Env {..}

childEnv :: Env -> IO Env
childEnv env = do
    bindings' <- HashTable.clone (eBindings env)
    return env{eBindings=bindings', eIsTopLevel=False}

freshType :: Env -> IO TypeVar
freshType Env{eNextTypeIndex} = do
    IORef.modifyIORef' eNextTypeIndex (+1)
    index <- IORef.readIORef eNextTypeIndex
    link <- IORef.newIORef Unbound
    return $ TVar index link

check :: Env -> Expression a -> IO (Expression TypeVar)
check env expr = case expr of
    EBlock _ exprs -> do
        bindings' <- HashTable.clone (eBindings env)
        let env' = env{eBindings=bindings'}
        case exprs of
            [] -> do
                return $ EBlock (TType Unit) []
            _ -> do
                exprs' <- forM exprs (check env')
                return $ EBlock (edata $ last exprs') exprs'
    EFun _ params exprs -> do
        bindings' <- HashTable.clone (eBindings env)
        paramTypes <- forM params $ \param -> do
            paramType <- freshType env
            HashTable.insert param paramType bindings'
            return paramType

        case exprs of
            [] -> do
                return $ EFun (TFun paramTypes (TType Unit)) params []
            _ -> do
                let env' = env{eBindings=bindings', eIsTopLevel=False}
                exprs' <- forM exprs (check env')
                return $ EFun (TFun paramTypes (edata $ last exprs')) params exprs'

    EApp _ lhs rhs -> do
        lhs' <- check env lhs
        rhs' <- check env rhs
        result <- freshType env
        unify (edata lhs') (TFun [edata rhs'] result)
        return $ EApp result lhs' rhs'

    ELet _ name expr' -> do
        ty <- freshType env
        HashTable.insert name ty (eBindings env)
        expr'' <- check env expr'
        unify ty (edata expr'')
        when (eIsTopLevel env) $ do
            quantify ty
        return $ ELet ty name expr''

    EPrint _ expr' -> do
        expr'' <- check env expr'
        return $ EPrint (TType Unit) expr''
    ELiteral _ (LInteger i) -> do
        return $ ELiteral (TType Number) (LInteger i)
    ELiteral _ (LString s) -> do
        return $ ELiteral (TType String) (LString s)
    EIdentifier _ txt -> do
        result <- HashTable.lookup txt (eBindings env)
        case result of
            Nothing ->
                error $ "Unbound symbol " ++ show txt
            Just tyref -> do
                tyref' <- instantiate env tyref
                return $ EIdentifier tyref' txt
    ESemi _ lhs rhs -> do
        lhs' <- check env lhs
        rhs' <- check env rhs
        return $ ESemi (edata rhs') lhs' rhs'

quantify :: TypeVar -> IO ()
quantify ty = do
    case ty of
        TVar i tv -> do
            tv' <- readIORef tv
            case tv' of
                Unbound -> do
                    writeIORef tv (Link $ TQuant i)
                Link t' ->
                    quantify t'
        TFun [param] ret -> do
            quantify param
            quantify ret
        _ ->
            return ()

instantiate :: Env -> TypeVar -> IO TypeVar
instantiate env t =
    let go ty subst = case ty of
            TQuant name -> do
                case lookup name (subst :: [(Int, TypeVar)]) of
                    Just v -> return (v, subst)
                    Nothing -> do
                        tv <- freshType env
                        return (tv, (name, tv):subst)
            TVar _ tv -> do
                vl <- readIORef tv
                case vl of
                    Link tv' -> go tv' subst
                    Unbound -> return (ty, subst)
            TFun [param] ret -> do
                (ty1, subst') <- go param subst
                (ty2, subst'') <- go ret subst'
                return (TFun [ty1] ty2, subst'')
            _ -> return (ty, subst)
    in fmap fst (go t [])

flatten :: Expression (TypeVar) -> IO (Expression ImmutableTypeVar)
flatten expr =
    let flattenTypeVar tv = case tv of
            TVar i ior -> do
                t <- IORef.readIORef ior
                case t of
                    Unbound ->
                        return $ IVar i Unbound
                    Link tv' -> do
                        flattenTypeVar tv'
            TQuant i ->
                return $ IQuant i
            TFun args body -> do
                args' <- forM args flattenTypeVar
                body' <- flattenTypeVar body
                return $ IFun args' body'
            TType t ->
                return $ IType t

    in case expr of
        EBlock td exprs -> do
            td' <- flattenTypeVar td
            exprs' <- forM exprs flatten
            return $ EBlock td' exprs'
        EFun td params exprs -> do
            td' <- flattenTypeVar td
            exprs' <- forM exprs flatten
            return $ EFun td' params exprs'
        EApp td lhs rhs -> do
            td' <- flattenTypeVar td
            lhs' <- flatten lhs
            rhs' <- flatten rhs
            return $ EApp td' lhs' rhs'
        ELet td name expr' -> do
            td' <- flattenTypeVar td
            expr'' <- flatten expr'
            return $ ELet td' name expr''
        EPrint td expr' -> do
            td' <- flattenTypeVar td
            expr'' <- flatten expr'
            return $ EPrint td' expr''
        ELiteral td lit -> do
            td' <- flattenTypeVar td
            return $ ELiteral td' lit
        EIdentifier td i -> do
            td' <- flattenTypeVar td
            return $ EIdentifier td' i
        ESemi td lhs rhs -> do
            td' <- flattenTypeVar td
            lhs' <- flatten lhs
            rhs' <- flatten rhs
            return $ ESemi td' lhs' rhs'

unify :: TypeVar -> TypeVar -> IO ()
unify a b = case (a, b) of
        (TVar _ ar, _) -> do
            a' <- readIORef ar
            case a' of
                Unbound -> do
                    -- occurs ...
                    writeIORef ar (Link b)
                Link a'' ->
                    unify a'' b

        (_, TVar {}) -> do
            unify b a

        (TType aType, TType bType)
            | aType == bType ->
                return ()
            | otherwise -> do
                error "unification failure"

        (TFun aa ar, TFun ba br) -> do
            forM_ (zip aa ba) (uncurry unify)
            unify ar br

        (TFun {}, TType {}) ->
            error "Unification failure"
        (TType {}, TFun {}) ->
            error "Unification failure"

        (TQuant {}, _) ->
            error "Internal error: QVar made it to unify"
        (_, TQuant {}) ->
            error "Internal error: QVar made it to unify"

run :: Expression a -> IO (Expression TypeVar)
run expr = do
    env <- newEnv
    check env expr
