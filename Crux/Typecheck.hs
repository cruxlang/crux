{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Crux.Typecheck where

import           Control.Monad.Reader
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
    | TFun [TypeVar] TypeVar
    | TType Type

data ImmutableTypeVar
    = IVar Int (VarLink ImmutableTypeVar)
    | IFun [ImmutableTypeVar] ImmutableTypeVar
    | IType Type
    deriving (Show, Eq)

data Env = Env
    { eNextTypeIndex :: IORef Int
    , eBindings      :: IORef (HashMap Text TypeVar)
    }

newEnv :: IO Env
newEnv = do
    eNextTypeIndex <- IORef.newIORef 0
    eBindings <- IORef.newIORef HashMap.empty
    return Env {..}

type CheckT = ReaderT Env IO

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

                let env' = env{eBindings=bindings'}

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
                return $ EIdentifier tyref txt
    ESemi _ lhs rhs -> do
        lhs' <- check env lhs
        rhs' <- check env rhs
        return $ ESemi (edata rhs') lhs' rhs'

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

run :: Expression a -> IO (Expression TypeVar)
run expr = do
    env <- newEnv
    check env expr
