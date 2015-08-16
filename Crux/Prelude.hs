module Crux.Prelude
    ( module Data.Foldable
    , module Data.Text
    , module Data.IORef
    , module Data.Monoid
    , module Control.Monad
    , module Control.Monad.IO.Class
    , module Control.Applicative
    ) where

import Data.Foldable (foldl')
import Data.Text (Text)
import Data.IORef (IORef)
import Data.Monoid ((<>), mempty)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Applicative ((<$>), (<*>))
