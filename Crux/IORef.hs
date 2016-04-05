module Crux.IORef
    ( IORef
    , newIORef
    , readIORef
    , writeIORef
    , modifyIORef
    , modifyIORef'
    ) where

import           Control.Monad.IO.Class
import           Data.IORef             (IORef)
import qualified Data.IORef             as IORef

newIORef :: MonadIO m => a -> m (IORef a)
newIORef a = liftIO $ IORef.newIORef a

readIORef :: MonadIO m => IORef a -> m a
readIORef a = liftIO $ IORef.readIORef a

writeIORef :: MonadIO m => IORef a -> a -> m ()
writeIORef a b = liftIO $ IORef.writeIORef a b

modifyIORef :: MonadIO m => IORef a -> (a -> a) -> m ()
modifyIORef a b = liftIO $ IORef.modifyIORef a b

modifyIORef' :: MonadIO m => IORef a -> (a -> a) -> m ()
modifyIORef' a b = liftIO $ IORef.modifyIORef' a b
