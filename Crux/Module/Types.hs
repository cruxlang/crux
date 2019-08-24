module Crux.Module.Types
    ( PatternTag(..)
    , PatternReference(..)
    , InstanceDesc(..)
    , LoadedModule(..)
    , Program(..)
    ) where

import Crux.ModuleName (ModuleName)
import Crux.AST (Module, Mutability, ResolvedReference)
import qualified Crux.JSTree as JSTree
import Crux.Prelude
import Crux.TypeVar (TypeVar, TDataTypeDef, TraitIdentity, TraitDesc, TraitImplIdentity)

type Name = Text

data PatternTag
    = TagBoxedVariant Name
    | TagNamedVariant Name
    | TagLiteral JSTree.Literal
    | TagNullish
    | TagNonNullish PatternTag
    deriving (Eq, Show)

data PatternReference = PatternReference (TDataTypeDef TypeVar) PatternTag
    deriving (Eq)

data InstanceDesc = InstanceDesc
    { idModuleName :: ModuleName
    , idTypeVar :: TypeVar -- quantified TypeVar of the type providing the impl
    , idFieldFunctionType :: Maybe TypeVar -- for records, the type of the field transformer function
    }
    deriving (Eq)

data LoadedModule = LoadedModule
    { lmModule :: Module ResolvedReference PatternTag TypeVar
    , lmExportedValues :: [(Name, (ResolvedReference, Mutability, TypeVar))]
    , lmExportedTypes :: [(Name, TypeVar)]
    , lmExportedPatterns :: [(Name, PatternReference)]
    , lmExportedTraits :: [(Name, (ResolvedReference, TraitIdentity, TraitDesc))]
    , lmExportedTypeFamilies :: [(Name, ResolvedReference)]
    , lmExportedExceptions :: [(Name, (ResolvedReference, TypeVar))]
    , lmKnownInstances :: HashMap (TraitIdentity, TraitImplIdentity) InstanceDesc
    } deriving (Eq)

data Program = Program
    { pMainModule   :: LoadedModule
    , pOtherModules :: HashMap ModuleName LoadedModule
    }
    deriving (Eq)
