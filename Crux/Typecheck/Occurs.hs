module Crux.Typecheck.Occurs (occurs) where

import Crux.Pos (Pos)
import Crux.TypeVar
import Crux.Typecheck.Monad
import Crux.Prelude
import Crux.Error

occursConstraintSet :: Pos -> Int -> ConstraintSet -> TC ()
occursConstraintSet pos tvn (ConstraintSet record _traits) = do
    for_ record $ \RecordConstraint{..} -> do
        for_ rcFields $ \field -> do
            occurs pos tvn $ trTyVar field
        for_ rcFieldType $ occurs pos tvn

occurs :: Pos -> Int -> TypeVar -> TC ()
occurs pos tvn = \case
    TypeVar ref -> readIORef ref >>= \case
        TUnbound _ _ _ q | tvn == q -> do
            failTypeError pos OccursCheckFailed
        TUnbound _strength _level constraints _tn -> do
            occursConstraintSet pos tvn constraints
            return ()
        TBound next -> do
            occurs pos tvn next
    TFun arg ret -> do
        for_ arg $ occurs pos tvn
        occurs pos tvn ret
    TDataType def -> do
        for_ (tuParameters def) $ occurs pos tvn
    TRecord fields -> do
        for_ fields $ \RecordField{..} ->
            occurs pos tvn trTyVar
    TQuant _source constraints _tn ->
        occursConstraintSet pos tvn constraints
    TTypeFun args rv -> do
        for_ args $ occurs pos tvn
        occurs pos tvn rv
