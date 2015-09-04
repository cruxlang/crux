module Sneak.Prelude
    ( module Data.Foldable
    , module Data.Text
    , module Data.IORef
    , module Data.Monoid
    , module Debug.Trace
    , module Control.Monad
    , module Control.Monad.IO.Class
    , module Control.Applicative
    , module Data.HashMap.Strict
    , module Data.Hashable
    , module GHC.Generics
    , module Data.String
    ) where

import Data.Foldable (foldl', foldlM)
import Data.Text (Text)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef, modifyIORef')
import Data.Monoid (Monoid(..), mconcat, (<>), mempty)
import Debug.Trace (trace, traceM, traceShow, traceShowM)
import Control.Monad (forM, forM_, when, foldM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Applicative ((<$>), (<*>), (<*), (*>), pure)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Data.String (IsString(..))
