module Crux.Typecheck.Types
    ( ValueReference(..)
    , TypeReference(..)
    , PatternTag(..)
    , PatternReference(..)
    , TraitIdentity
    , ExceptionReference(..)
    , Env(..)
    , TypeLevel(..)
    ) where

import Crux.ModuleName (ModuleName)
import Crux.AST (Mutability, ResolvedReference)
import Crux.Module.Types (LoadedModule, PatternTag (..), PatternReference (..))
import Crux.Prelude
import Crux.TypeVar (TDataTypeIdentity, TraitIdentity, TraitDesc, TypeLevel(..), TypeVar(..))
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
    , eValueBindings      :: SymbolTable ValueReference
    , eTypeBindings       :: SymbolTable TypeReference
    , ePatternBindings    :: SymbolTable PatternReference
    , eTraitBindings      :: SymbolTable (ResolvedReference, TraitIdentity, TraitDesc)
    , eExceptionBindings  :: SymbolTable ExceptionReference
    -- TODO: make this a set?
    -- TODO: we don't actually care about the instance's module name
    , eKnownInstances     :: HashTable (TraitIdentity, TDataTypeIdentity) ModuleName

    , eReturnType         :: Maybe TypeVar -- Nothing if top-level expression
    , eInLoop             :: !Bool
    , eLevel              :: !TypeLevel

    , eExportedValues     :: SymbolTable (ResolvedReference, Mutability, TypeVar)
    , eExportedTypes      :: SymbolTable TypeVar
    , eExportedPatterns   :: SymbolTable PatternReference
    , eExportedTraits     :: SymbolTable (ResolvedReference, TraitIdentity, TraitDesc)
    , eExportedExceptions :: SymbolTable TypeVar
    }
