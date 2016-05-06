module Crux.Typecheck.Types
    ( ValueReference(..)
    , TypeReference(..)
    , PatternTag(..)
    , PatternReference(..)
    , ExceptionReference(..)
    , Env(..)
    , TypeLevel(..)
    ) where

import Crux.AST (ModuleName, Mutability, ResolvedReference)
import Crux.Module.Types (LoadedModule, PatternTag (..))
import Crux.Prelude
import Crux.TypeVar (TUserTypeDef (..), TypeLevel (..), TypeVar (..))

-- TODO: newtype this somewhere and import it
type Name = Text

type HashTable k v = IORef (HashMap k v)

data ValueReference
    = ValueReference ResolvedReference Mutability TypeVar
    | ModuleReference ModuleName

data TypeReference = TypeReference TypeVar

data ExceptionReference = ExceptionReference ResolvedReference TypeVar

data PatternReference = PatternReference (TUserTypeDef TypeVar) PatternTag

data Env = Env
    { eThisModule        :: ModuleName
    , eLoadedModules     :: HashMap ModuleName LoadedModule
    , eNextTypeIndex     :: IORef Int
    , eValueBindings     :: HashTable Name ValueReference
    , eTypeBindings      :: HashTable Name TypeReference
    , ePatternBindings   :: HashTable Name PatternReference
    , eExceptionBindings :: HashTable Name ExceptionReference
    , eReturnType        :: Maybe TypeVar -- Nothing if top-level expression
    , eInLoop            :: !Bool
    , eLevel             :: !TypeLevel
    }
