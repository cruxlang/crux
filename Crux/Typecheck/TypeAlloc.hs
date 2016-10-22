module Crux.Typecheck.TypeAlloc
    ( freshTypeIndex
    , freshType
    , freshTypeConstrained
    , freshWeakQVar
    ) where

import Crux.Prelude
import Crux.Typecheck.Types
import Crux.TypeVar

freshTypeIndex :: MonadIO m => Env -> m Int
freshTypeIndex Env{eNextTypeIndex} = do
    modifyIORef' eNextTypeIndex (+1)
    readIORef eNextTypeIndex

freshType :: MonadIO m => Env -> m TypeVar
freshType env = freshTypeConstrained env emptyConstraintSet

freshTypeConstrained :: MonadIO m => Env -> ConstraintSet -> m TypeVar
freshTypeConstrained env constraints = do
    index <- freshTypeIndex env
    newTypeVar $ TUnbound Strong (eLevel env) constraints index

freshWeakQVar :: MonadIO m => Env -> m TypeVar
freshWeakQVar env = do
    index <- freshTypeIndex env
    newTypeVar $ TUnbound Weak (eLevel env) emptyConstraintSet index
