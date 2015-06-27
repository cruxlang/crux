module Crux.JSGen.Types where

import qualified Control.Monad.Writer  as Writer
import qualified Crux.JSTree           as JS
import           Crux.AST
import           Data.HashMap.Strict   (HashMap)
import           Data.IORef            (IORef)

data Env = Env
    { eNames :: IORef (HashMap Name JS.Name)
    }

type JSWrite a = Writer.WriterT [JS.Statement] IO a
