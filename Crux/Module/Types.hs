module Crux.Module.Types (LoadedModule, Program(..)) where

import Crux.AST (Module, ModuleName, ResolvedReference)
import Crux.Prelude
import Crux.TypeVar (ImmutableTypeVar)

type LoadedModule = Module ResolvedReference ImmutableTypeVar

data Program = Program
    { pMainModule   :: LoadedModule
    , pOtherModules :: HashMap ModuleName LoadedModule
    }
    deriving (Show, Eq)
