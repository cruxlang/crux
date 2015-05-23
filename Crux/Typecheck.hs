{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Crux.Typecheck where

import           Control.Monad.Reader
import           Crux.AST
import qualified Crux.MutableHashTable as HashTable
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as HashMap
import           Data.IORef            (IORef)
import qualified Data.IORef            as IORef
import           Data.Text             (Text)
import           Prelude               hiding (String)

data Type
    = Number
    | String
    | Unit
    deriving (Eq, Show)

data TypeVar
    = TVar Int
    | TType Type
    deriving (Eq, Show)

data TypeData = TypeData
    { tdType :: IORef TypeVar
    }

instance Show TypeData where
    show _ = "TD"

data Env = Env
    { eNextTypeIndex :: IORef Int
    , eBindings      :: IORef (HashMap Text TypeData)
    }

newEnv :: IO Env
newEnv = do
    eNextTypeIndex <- IORef.newIORef 0
    eBindings <- IORef.newIORef HashMap.empty
    return Env {..}

type CheckT = ReaderT Env IO

freshType :: Env -> IO TypeData
freshType Env{eNextTypeIndex} = do
    liftIO $ IORef.modifyIORef' eNextTypeIndex (+1)
    index <- liftIO $ IORef.readIORef eNextTypeIndex
    fmap TypeData $ IORef.newIORef (TVar index)

check :: Env -> Expression a -> IO (Expression TypeData)
check env expr = case expr of
    EBlock _ exprs -> do
        bindings' <- HashTable.clone (eBindings env)
        let env' = env{eBindings=bindings'}
        case exprs of
            [] -> do
                tdType <- IORef.newIORef $ TType Unit
                return $ EBlock TypeData{..} []
            _ -> do
                exprs' <- forM exprs (check env')
                return $ EBlock (edata $ last exprs') exprs'

    ELet _ name expr' -> do
        ty <- freshType env
        HashTable.insert name ty (eBindings env)
        expr'' <- check env expr'
        unify ty (edata expr'')
        return $ ELet ty name expr''
    EPrint _ expr' -> do
        expr'' <- check env expr'
        tdType <- IORef.newIORef $ TType Unit
        return $ EPrint TypeData{..} expr''
    ELiteral _ (LInteger i) -> do
        tdType <- IORef.newIORef $ TType Number
        return $ ELiteral TypeData{..} (LInteger i)
    ELiteral _ (LString s) -> do
        tdType <- IORef.newIORef $ TType String
        return $ ELiteral TypeData{..} (LString s)
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

flatten :: Expression TypeData -> IO (Expression TypeVar)
flatten expr = case expr of
    EBlock td exprs -> do
        td' <- IORef.readIORef (tdType td)
        exprs' <- forM exprs flatten
        return $ EBlock td' exprs'
    ELet td name expr' -> do
        td' <- IORef.readIORef (tdType td)
        expr'' <- flatten expr'
        return $ ELet td' name expr''
    EPrint td expr' -> do
        td' <- IORef.readIORef (tdType td)
        expr'' <- flatten expr'
        return $ EPrint td' expr''
    ELiteral td lit -> do
        td' <- IORef.readIORef (tdType td)
        return $ ELiteral td' lit
    EIdentifier td i -> do
        td' <- IORef.readIORef (tdType td)
        return $ EIdentifier td' i
    ESemi td lhs rhs -> do
        td' <- IORef.readIORef (tdType td)
        lhs' <- flatten lhs
        rhs' <- flatten rhs
        return $ ESemi td' lhs' rhs'

unify :: TypeData -> TypeData -> IO ()
unify a b = do
    a' <- IORef.readIORef $ tdType a
    b' <- IORef.readIORef $ tdType b
    case (a', b') of
        (TVar _, TType _) ->
            IORef.writeIORef (tdType a) b'
        (TType _, TVar _) ->
            IORef.writeIORef (tdType b) a'
        (TVar _, TVar _) ->
            IORef.writeIORef (tdType b) a'

        (TType aType, TType bType)
            | aType == bType ->
                return ()
            | otherwise -> do
                error "unification failure"

run :: Expression a -> IO (Expression TypeData)
run expr = do
    env <- newEnv
    check env expr
