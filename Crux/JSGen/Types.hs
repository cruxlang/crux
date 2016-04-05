{-# LANGUAGE Rank2Types #-}

module Crux.JSGen.Types where

import qualified Control.Monad.Writer as Writer
import           Crux.AST
import qualified Crux.JSTree          as JS
import           Crux.Prelude

data Env = Env
    { eNames :: IORef (HashMap Name JS.Name)
    }

type JSWrite a = Writer.WriterT [JS.Statement] IO a

data GenVTable = GenVTable
    { vGenerateExpr :: forall i t. Env -> Expression i t -> JSWrite JS.Expression
    }
