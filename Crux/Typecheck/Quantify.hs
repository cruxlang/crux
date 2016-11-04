module Crux.Typecheck.Quantify (quantify) where

import Crux.Prelude
import Crux.TypeVar

quantifyConstraintSet :: MonadIO m => ConstraintSet -> m ()
quantifyConstraintSet (ConstraintSet recordConstraint _traits) = do
    for_ recordConstraint $ \RecordConstraint{..} -> do
        for_ rcFields $ \RecordField{..} -> do
            -- TODO: quantify mutability.  there's almost certainly a bug
            -- but I don't know how to write a test for it yet.
            quantify trTyVar
        for_ rcFieldType $ quantify

quantify :: MonadIO m => TypeVar -> m ()
quantify ty = case ty of
    TypeVar ref -> readIORef ref >>= \case
        TUnbound Strong _ constraints i -> do
            quantifyConstraintSet constraints
            writeIORef ref $ TBound $ TQuant Instantiation constraints i
        TUnbound Weak _ constraints _ -> do
            quantifyConstraintSet constraints
            return ()
        TBound t -> do
            quantify t
    TQuant _source _constraints _tn -> do
        -- Constraints should already be quantified here.
        return ()
    TFun param ret -> do
        for_ param quantify
        quantify ret
    TDataType def ->
        for_ (tuParameters def) quantify
    TRecord fields -> do
        for_ fields $ \RecordField{..} -> do
            quantify trTyVar
    TTypeFun args rv -> do
        for_ args quantify
        quantify rv
