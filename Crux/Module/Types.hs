module Crux.Module.Types (LoadedModule, Program(..)) where

import           Crux.AST     (Module, ModuleName, ResolvedReference)
import           Crux.Prelude
import           Crux.TypeVar (TypeVar)

type LoadedModule = Module ResolvedReference () TypeVar

data Program = Program
    { pMainModule   :: LoadedModule
    , pOtherModules :: HashMap ModuleName LoadedModule
    }
    deriving (Eq)
