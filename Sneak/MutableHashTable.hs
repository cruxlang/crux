module Sneak.MutableHashTable where

import Sneak.Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IORef          as IORef

new :: IO (IORef (HashMap key value))
new = IORef.newIORef HashMap.empty

read :: IORef (HashMap key value) -> IO (HashMap key value)
read = IORef.readIORef

insert :: (Hashable key, Eq key) => key -> value -> IORef (HashMap key value) -> IO ()
insert k v hm = do
    IORef.modifyIORef hm $ HashMap.insert k v

lookup :: (Hashable key, Eq key) => key -> IORef (HashMap key value) -> IO (Maybe value)
lookup k hm = do
    h <- IORef.readIORef hm
    return $ HashMap.lookup k h

clone :: IORef (HashMap key value) -> IO (IORef (HashMap key value))
clone hm = do
    h <- IORef.readIORef hm
    IORef.newIORef h

-- Create a new mutable hash table containing the union of the two provided mutable hash tables
merge :: (Eq key, Hashable key) => IORef (HashMap key value) -> IORef (HashMap key value) -> IO (IORef (HashMap key value))
merge a b = do
    a' <- IORef.readIORef a
    b' <- IORef.readIORef b

    IORef.newIORef $ HashMap.union a' b'

-- Create a new mutable hash table containing the union of the provided mutable and immutable hash tables.
mergeImmutable :: (Eq key, Hashable key) => IORef (HashMap key value) -> HashMap key value -> IO (IORef (HashMap key value))
mergeImmutable a b = do
    a' <- IORef.readIORef a
    IORef.newIORef $ HashMap.union a' b
