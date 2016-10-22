module Crux.Typecheck.Occurs (occurs) where

import Crux.Pos (Pos)
import Crux.TypeVar
import Crux.Typecheck.Monad
import Crux.Prelude
import Crux.Error

occurs :: Pos -> Int -> TypeVar -> TC ()
occurs pos tvn = \case
    TypeVar ref -> readIORef ref >>= \case
        TUnbound _ _ _ q | tvn == q -> do
            failTypeError pos OccursCheckFailed
        TUnbound {} -> do
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
    TQuant {} ->
        return ()
    TTypeFun args rv -> do
        for_ args $ occurs pos tvn
        occurs pos tvn rv
