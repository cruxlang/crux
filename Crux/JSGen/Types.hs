{-# LANGUAGE Rank2Types #-}

module Crux.JSGen.Types where

import Crux.Prelude
import qualified Control.Monad.Writer  as Writer
import qualified Crux.JSTree           as JS
import           Crux.AST

data Env = Env
    { eNames :: IORef (HashMap Name JS.Name)
    }

type JSWrite a = Writer.WriterT [JS.Statement] IO a

data GenVTable = GenVTable
    { vGenerateExpr :: forall i t. Env -> Expression i t -> JSWrite JS.Expression
    }
