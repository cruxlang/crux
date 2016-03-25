module Crux.Typecheck.Types
    ( ValueReference(..)
    , TypeReference(..)
    , PatternReference(..)
    , ExceptionReference(..)
    , Env(..)
    , TypeLevel(..)
    ) where

import Crux.TypeVar
    ( TypeVar(..)
    , TUserTypeDef(..)
    , TypeLevel(..)
    )
import Crux.Module.Types (LoadedModule)
import Crux.AST
    ( Mutability
    , ModuleName
    , ResolvedReference
    )
import Crux.Prelude

-- TODO: newtype this somewhere and import it
type Name = Text

type HashTable k v = IORef (HashMap k v)

data ValueReference
    = ValueReference ResolvedReference Mutability TypeVar
    | ModuleReference ModuleName

data TypeReference = TypeReference TypeVar

data ExceptionReference = ExceptionReference ResolvedReference TypeVar

-- same structure as TUserType constructor
data PatternReference = PatternReference
    (TUserTypeDef TypeVar) -- type of value being pattern matched
    [TypeVar] -- type parameters to type

data Env = Env
    { eThisModule :: ModuleName
    , eLoadedModules :: HashMap ModuleName LoadedModule
    , eNextTypeIndex :: IORef Int
    , eValueBindings :: HashTable Name ValueReference
    , eTypeBindings :: HashTable Name TypeReference
    , ePatternBindings :: HashTable Name PatternReference
    , eExceptionBindings :: HashTable Name ExceptionReference
    , eReturnType :: Maybe TypeVar -- Nothing if top-level expression
    , eInLoop :: !Bool
    , eLevel :: !TypeLevel
    }
