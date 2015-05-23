module Crux.MutableHashTable where

import           Data.Hashable       (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.IORef          (IORef)
import qualified Data.IORef          as IORef

new :: IO (IORef (HashMap key value))
new = IORef.newIORef HashMap.empty

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
