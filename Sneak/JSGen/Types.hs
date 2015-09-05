{-# LANGUAGE Rank2Types #-}

module Sneak.JSGen.Types where

import Sneak.Prelude
import qualified Control.Monad.Writer  as Writer
import qualified Sneak.JSTree           as JS
import           Sneak.AST

data Env = Env
    { eNames :: IORef (HashMap Name JS.Name)
    }

type JSWrite a = Writer.WriterT [JS.Statement] IO a

data GenVTable = GenVTable
    { vGenerateExpr :: forall i t. Env -> Expression i t -> JSWrite JS.Expression
    }
