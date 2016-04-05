module Crux.MutableHashTable where

import           Crux.Prelude
import qualified Data.HashMap.Strict as HashMap

new :: MonadIO m => m (IORef (HashMap key value))
new = newIORef HashMap.empty

read :: MonadIO m => IORef (HashMap key value) -> m (HashMap key value)
read = readIORef

insert :: (Hashable key, Eq key, MonadIO m) => key -> value -> IORef (HashMap key value) -> m ()
insert k v hm = do
    modifyIORef hm $ HashMap.insert k v

lookup :: (Hashable key, Eq key, MonadIO m) => key -> IORef (HashMap key value) -> m (Maybe value)
lookup k hm = do
    h <- readIORef hm
    return $ HashMap.lookup k h

clone :: MonadIO m => IORef (HashMap key value) -> m (IORef (HashMap key value))
clone hm = do
    h <- readIORef hm
    newIORef h

-- Create a new mutable hash table containing the union of the two provided mutable hash tables
merge :: (Eq key, Hashable key, MonadIO m) => IORef (HashMap key value) -> IORef (HashMap key value) -> m (IORef (HashMap key value))
merge a b = do
    a' <- readIORef a
    b' <- readIORef b

    newIORef $ HashMap.union a' b'

-- Create a new mutable hash table containing the union of the provided mutable and immutable hash tables.
mergeImmutable :: (Eq key, Hashable key, MonadIO m) => IORef (HashMap key value) -> HashMap key value -> m (IORef (HashMap key value))
mergeImmutable a b = do
    a' <- readIORef a
    newIORef $ HashMap.union a' b
