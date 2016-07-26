module Crux.SymbolTable
    ( SymbolTable
    , InsertPolicy(..)
    , new
    , clone
    , readAll
    , lookup
    , insert
    ) where

import Prelude hiding (lookup)
import Crux.Prelude
import qualified Data.HashMap.Strict as HashMap
import Crux.Typecheck.Monad (TC, Warning(..), recordWarning, failError)
import Crux.Error (Error(DuplicateSymbol))
import Crux.Pos (Pos)

type Name = Text
newtype SymbolTable v = SymbolTable (IORef (HashMap Name v))

data InsertPolicy = WarnOnShadow | DisallowDuplicates

new :: MonadIO m => m (SymbolTable value)
new = SymbolTable <$> newIORef HashMap.empty

clone :: MonadIO m => SymbolTable value -> m (SymbolTable value)
clone (SymbolTable ref) = do
    h <- readIORef ref
    SymbolTable <$> newIORef h

readAll :: MonadIO m => SymbolTable value -> m (HashMap Name value)
readAll (SymbolTable ref) = readIORef ref

insert :: SymbolTable value -> Pos -> InsertPolicy -> Name -> value -> TC ()
insert (SymbolTable ref) pos policy key value = do
    hm <- readIORef ref
    case HashMap.lookup key hm of
        Nothing ->
            writeIORef ref $ HashMap.insert key value hm
        Just _ -> case policy of
            WarnOnShadow -> do
                -- TODO: a real warning
                recordWarning Warning
                writeIORef ref $ HashMap.insert key value hm
            DisallowDuplicates -> do
                failError $ DuplicateSymbol pos key

lookup :: MonadIO m => SymbolTable value -> Name -> m (Maybe value)
lookup (SymbolTable ref) key = do
    hm <- readIORef ref
    return $ HashMap.lookup key hm

{-
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
-}
