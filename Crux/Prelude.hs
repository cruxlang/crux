module Crux.Prelude
    ( module Data.Foldable
    , module Data.Text
    , module Data.IORef
    , module Data.Monoid
    , module Control.Monad
    , module Control.Monad.IO.Class
    , module Control.Applicative
    , module Data.HashMap.Strict
    ) where

import Data.Foldable (foldl', foldlM)
import Data.Text (Text)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef, modifyIORef')
import Data.Monoid ((<>), mempty)
import Control.Monad (forM, forM_, when, foldM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Applicative ((<$>), (<*>))
import Data.HashMap.Strict (HashMap)
