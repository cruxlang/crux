module Crux.Module.Types
    ( PatternTag(..)
    , LoadedModule
    , Program(..)
    ) where

import Crux.AST (Module, ModuleName, ResolvedReference)
import Crux.Prelude
import Crux.TypeVar (TypeVar)
import qualified Crux.JSTree as JSTree

type Name = Text

data PatternTag
    = TagVariant Name
    | TagLiteral JSTree.Literal
    deriving (Eq, Show)

type LoadedModule = Module ResolvedReference PatternTag TypeVar

data Program = Program
    { pMainModule   :: LoadedModule
    , pOtherModules :: HashMap ModuleName LoadedModule
    }
    deriving (Eq)
