module Crux.Typecheck.Types
    ( ValueReference(..)
    , TypeReference(..)
    , PatternTag(..)
    , PatternReference(..)
    , TraitNumber
    , ExceptionReference(..)
    , Env(..)
    , TypeLevel(..)
    ) where

import Crux.AST (ModuleName, Mutability, ResolvedReference)
import Crux.Module.Types (LoadedModule, PatternTag (..), PatternReference (..))
import Crux.Prelude
import Crux.TypeVar (TDataTypeIdentity, TraitNumber, TraitDesc, TypeLevel(..), TypeVar(..))
import Crux.SymbolTable (SymbolTable)
import Crux.HashTable (HashTable)

data ValueReference
    = ValueReference ResolvedReference Mutability TypeVar
    | ModuleReference ModuleName

data TypeReference = TypeReference TypeVar

data ExceptionReference = ExceptionReference ResolvedReference TypeVar

data Env = Env
    { eThisModule         :: ModuleName
    , eLoadedModules      :: HashMap ModuleName LoadedModule
    , eNextTypeIndex      :: IORef Int
    , eNextTraitIndex     :: IORef Int
    , eValueBindings      :: SymbolTable ValueReference
    , eTypeBindings       :: SymbolTable TypeReference
    , ePatternBindings    :: SymbolTable PatternReference
    , eTraitBindings      :: SymbolTable (ResolvedReference, TraitNumber, TraitDesc)
    , eExceptionBindings  :: SymbolTable ExceptionReference
    , eKnownInstances     :: HashTable (TraitNumber, TDataTypeIdentity) ModuleName

    , eReturnType         :: Maybe TypeVar -- Nothing if top-level expression
    , eInLoop             :: !Bool
    , eLevel              :: !TypeLevel

    , eExportedValues     :: SymbolTable (ResolvedReference, Mutability, TypeVar)
    , eExportedTypes      :: SymbolTable TypeVar
    , eExportedPatterns   :: SymbolTable PatternReference
    , eExportedTraits     :: SymbolTable (ResolvedReference, TraitNumber, TraitDesc)
    , eExportedExceptions :: SymbolTable TypeVar
    }
