module Crux.IORef
    ( IORef
    , newIORef
    , readIORef
    , writeIORef
    , modifyIORef
    , modifyIORef'
    ) where

import Data.IORef (IORef)
import qualified Data.IORef as IORef
import Control.Monad.IO.Class

newIORef a = liftIO $ IORef.newIORef a
readIORef a = liftIO $ IORef.readIORef a
writeIORef a b = liftIO $ IORef.writeIORef a b
modifyIORef a b = liftIO $ IORef.modifyIORef a b
modifyIORef' a b = liftIO $ IORef.modifyIORef' a b
