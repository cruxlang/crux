
module Crux.AST where

import Data.Text (Text)

type Name = Text -- Temporary
type Pattern = Name -- Temporary

data Literal
    = LInteger Integer
    | LString Text
      deriving (Show, Eq)

data Expression
    = EBlock [Expression]
    | ELet Pattern Expression
    | EPrint Expression
    | ELiteral Literal
    | ESemi Expression Expression
      deriving (Show, Eq)
