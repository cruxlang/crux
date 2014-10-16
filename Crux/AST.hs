
module Crux.AST where

import Data.Text (Text)

data Literal
    = LInteger Integer
    | LString Text
      deriving (Show, Eq)

data Expression
    = EBlock [Expression]
    | EPrint Literal
      deriving (Show, Eq)
