{-# LANGUAGE DeriveFunctor #-}

module Crux.Typecheck.Types
    ( ValueReference(..)
    , TypeReference(..)
    , PatternBinding(..)
    , Env(..)
    ) where

import Crux.TypeVar
    ( TypeVar(..)
    , TUserTypeDef(..)
    )
import Crux.Module.Types (LoadedModule)
import Crux.AST
    ( LetMutability
    , ModuleName
    , ResolvedReference
    , TypeIdent
    )
import           Crux.Prelude

-- TODO: newtype this somewhere and import it
type Name = Text

type HashTable k v = IORef (HashMap k v)

data ValueReference
    = ValueReference ResolvedReference LetMutability TypeVar
    | ModuleReference ModuleName

data TypeReference
    = TypeBinding TypeVar
    | TypeAlias Name [Name] TypeIdent
    deriving (Eq)
instance Show TypeReference where
    show (TypeBinding _tv) = "TypeBinding <typevar>"
    show (TypeAlias name params typeIdent) = "TypeAlias " ++ show name ++ " " ++ show params ++ " " ++ show typeIdent

-- same structure as TUserType constructor
data PatternBinding = PatternBinding
    (TUserTypeDef TypeVar) -- type of value being pattern matched
    [TypeVar] -- type parameters to type

data Env = Env
    { eThisModule :: ModuleName
    , eLoadedModules :: HashMap ModuleName LoadedModule
    , eNextTypeIndex :: IORef Int
    , eValueBindings :: HashTable Name ValueReference
    , eTypeBindings :: HashTable Name TypeReference
    , ePatternBindings :: HashTable Name PatternBinding
    , eReturnType :: Maybe TypeVar -- Nothing if top-level expression
    , eInLoop :: !Bool
    }
