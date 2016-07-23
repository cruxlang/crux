module Crux.Typecheck.Types
    ( ValueReference(..)
    , PatternTag(..)
    , PatternReference(..)
    , TraitIdentity
    , Env(..)
    , TypeLevel(..)
    ) where

import Crux.ModuleName (ModuleName)
import Crux.AST (Mutability, ResolvedReference)
import Crux.Module.Types (LoadedModule, PatternTag(..), PatternReference(..), InstanceDesc(..))
import Crux.Prelude
import Crux.TypeVar (TDataTypeIdentity, TraitIdentity, TraitDesc, TypeLevel(..), TypeVar(..))
import Crux.SymbolTable (SymbolTable)
import Crux.HashTable (HashTable)

data ValueReference
    = ValueReference ResolvedReference Mutability TypeVar
    | ModuleReference ModuleName

data Env = Env
    { eThisModule         :: ModuleName
    , eLoadedModules      :: HashMap ModuleName LoadedModule
    , eNextTypeIndex      :: IORef Int

    , eValueBindings      :: SymbolTable ValueReference
    , eTypeBindings       :: SymbolTable TypeVar
    , ePatternBindings    :: SymbolTable PatternReference
    , eTraitBindings      :: SymbolTable (ResolvedReference, TraitIdentity, TraitDesc)
    , eExceptionBindings  :: SymbolTable (ResolvedReference, TypeVar)

    , eKnownInstances     :: HashTable (TraitIdentity, TDataTypeIdentity) InstanceDesc

    , eReturnType         :: Maybe TypeVar -- Nothing if top-level expression
    , eInLoop             :: !Bool
    , eLevel              :: !TypeLevel

    , eExportedValues     :: SymbolTable (ResolvedReference, Mutability, TypeVar)
    , eExportedTypes      :: SymbolTable TypeVar
    , eExportedPatterns   :: SymbolTable PatternReference
    , eExportedTraits     :: SymbolTable (ResolvedReference, TraitIdentity, TraitDesc)
    , eExportedExceptions :: SymbolTable (ResolvedReference, TypeVar)
    }
