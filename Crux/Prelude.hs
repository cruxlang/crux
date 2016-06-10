module Crux.Prelude
    ( module Control.Applicative
    , module Control.Monad
    , module Control.Monad.IO.Class
    , module Control.Monad.Trans.Class
    , module Control.Monad.Trans.Maybe
    , module Control.Monad.Trans.Either
    , module Data.Foldable
    , module Data.Hashable
    , module Data.HashMap.Strict
    , module Data.HashSet
    , module Data.Maybe
    , module Crux.IORef
    , module Data.Monoid
    , module Data.String
    , module Data.Text
    , module Data.Typeable
    , module Debug.Trace
    , module GHC.Generics
    , module Data.Traversable
    , intercalate
    ) where

import Control.Applicative (pure, (*>), (<$>), (<*), (<*>))
import Control.Monad (foldM, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (EitherT (..), left)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Crux.IORef (IORef, modifyIORef, modifyIORef', newIORef, readIORef,
                   writeIORef)
import Data.Foldable (foldl', foldlM, for_, traverse_, asum)
import Data.Hashable (Hashable (..))
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid (Monoid (..), mconcat, mempty, (<>))
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Traversable (for, traverse)
import Data.Typeable (Typeable)
import Debug.Trace (trace, traceM, traceShow, traceShowM)
import GHC.Generics (Generic)

intercalate :: (Show m, Monoid m) => m -> [m] -> m
intercalate sep els =
    mconcat (h els)
    -- case els of
    --     [] -> mempty
    --     [x] -> x
    --     (x:y:xs) -> x <> sep <> intercalate sep (y:xs)
  where
    h l = case l of
        [] -> []
        [x] -> [x]
        (x:rest) -> x:sep:h rest
