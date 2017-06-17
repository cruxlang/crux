module Crux.Typecheck.Inst
    ( instantiateDataTypeDef
    , instantiate
    , instantiate'
    , instantiateAll
    ) where

import Crux.Prelude
import Crux.TypeVar
import Crux.Typecheck.TypeAlloc
import Crux.Typecheck.Types
import qualified Crux.HashTable as HashTable

-- TODO: move Env and the substitution table into a reader monad

instantiateDataTypeDef' ::
    MonadIO m =>
    IORef (HashMap Int TypeVar) ->
    Env ->
    TDataTypeDef TypeVar ->
    m (TDataTypeDef TypeVar)
instantiateDataTypeDef' subst env def = do
    for def $ instantiate' subst env

instantiateDataTypeDef :: MonadIO m => Env -> TDataTypeDef TypeVar -> m (TDataTypeDef TypeVar)
instantiateDataTypeDef env def = do
    subst <- HashTable.new
    instantiateDataTypeDef' subst env def

instantiateField :: MonadIO m => IORef (HashMap Int TypeVar) -> Env -> RecordField TypeVar -> m (RecordField TypeVar)
instantiateField subst env RecordField{..} = do
    trTyVar' <- instantiate' subst env trTyVar
    let mut' = case trMut of
            RQuantified -> RFree
            _ -> trMut
    return RecordField
        { trName
        , trMut=mut'
        , trOptional
        , trTyVar=trTyVar'
        }

instantiateConstraints :: MonadIO m => IORef (HashMap Int TypeVar) -> Env -> ConstraintSet -> m ConstraintSet
instantiateConstraints subst env (ConstraintSet recordConstraint traits) = do
    record' <- for recordConstraint $ \record -> do
        fields' <- for (rcFields record) $ \field@RecordField{trTyVar} -> do
            tyVar' <- instantiate' subst env trTyVar
            return field{trTyVar=tyVar'}
        fieldType' <- for (rcFieldType record) $ instantiate' subst env
        return RecordConstraint
                { rcFields = fields'
                , rcFieldType = fieldType'
                }
    return $ ConstraintSet record' traits

instantiate' :: MonadIO m => IORef (HashMap Int TypeVar) -> Env -> TypeVar -> m TypeVar
instantiate' subst env ty = case ty of
    TypeVar ref -> do
        readIORef ref >>= \case
            TUnbound _strength _level _constraints _number -> do
                -- We instantiate unbound type variables when recursively
                -- referencing a function whose parameter and return types are
                -- not yet quantified.
                return ty
            TBound tv' -> do
                instantiate' subst env tv'
    TQuant _ constraints name -> do
        HashTable.lookup name subst >>= \case
            Just v ->
                return v
            Nothing -> do
                -- TODO: propagate source
                constraints' <- instantiateConstraints subst env constraints
                tv <- freshTypeConstrained env constraints'
                HashTable.insert name tv subst
                return tv
    TFun param ret -> do
        ty1 <- for param $ instantiate' subst env
        ty2 <- instantiate' subst env ret
        return $ TFun ty1 ty2
    TDataType def -> do
        typeVars' <- for (tuParameters def) $ instantiate' subst env
        return $ TDataType def{ tuParameters = typeVars' }
    TRecord fields -> do
        fields' <- for fields $ instantiateField subst env
        return $ TRecord fields'
    TTypeFun args rv -> do
        args' <- for args $ instantiate' subst env
        rv' <- instantiate' subst env rv
        return $ TTypeFun args' rv'

instantiate :: MonadIO m => Env -> TypeVar -> m TypeVar
instantiate env t = do
    subst <- HashTable.new
    instantiate' subst env t

instantiateAll :: (MonadIO m, Traversable c) => Env -> c TypeVar -> m (c TypeVar)
instantiateAll env container = do
    subst <- HashTable.new
    for container $ instantiate' subst env
