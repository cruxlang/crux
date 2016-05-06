module Crux.Module.Types
    ( PatternTag(..)
    , LoadedModule
    , Program(..)
    ) where

import Crux.AST (Module, ModuleName, ResolvedReference)
import qualified Crux.JSTree as JSTree
import Crux.Prelude
import Crux.TypeVar (TypeVar)

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
