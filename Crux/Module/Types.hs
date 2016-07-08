module Crux.Module.Types
    ( PatternTag(..)
    , PatternReference(..)
    , LoadedModule(..)
    , Program(..)
    ) where

import Crux.ModuleName (ModuleName)
import Crux.AST (Module, Mutability, ResolvedReference)
import qualified Crux.JSTree as JSTree
import Crux.Prelude
import Crux.TypeVar (TypeVar, TDataTypeDef, TraitNumber, TraitDesc)

type Name = Text

data PatternTag
    = TagVariant Name
    | TagLiteral JSTree.Literal
    deriving (Eq, Show)

data PatternReference = PatternReference (TDataTypeDef TypeVar) PatternTag
    deriving (Eq)

data LoadedModule = LoadedModule
    { lmModule :: Module ResolvedReference PatternTag TypeVar
    , lmExportedValues :: [(Name, (ResolvedReference, Mutability, TypeVar))]
    , lmExportedTypes :: [(Name, TypeVar)]
    , lmExportedPatterns :: [(Name, PatternReference)]
    , lmExportedTraits :: [(Name, (ResolvedReference, TraitNumber, TraitDesc))]
    , lmExportedExceptions :: [(Name, TypeVar)]
    } deriving (Eq)

--type CheckedModule = Module ResolvedReference PatternTag TypeVar

data Program = Program
    { pMainModule   :: LoadedModule
    , pOtherModules :: HashMap ModuleName LoadedModule
    }
    deriving (Eq)
