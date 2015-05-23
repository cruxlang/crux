
module Crux.AST where

import Data.Text (Text)

type Name = Text -- Temporary
type Pattern = Name -- Temporary

data Literal
    = LInteger Integer
    | LString Text
      deriving (Show, Eq)

data Expression edata
    = EBlock edata [Expression edata]
    | ELet edata Pattern (Expression edata)
    | EPrint edata (Expression edata)
    | ELiteral edata Literal
    | ESemi edata (Expression edata) (Expression edata)
      deriving (Show, Eq)

edata :: Expression edata -> edata
edata expr = case expr of
    EBlock ed _ -> ed
    ELet ed _ _ -> ed
    EPrint ed _ -> ed
    ELiteral ed _ -> ed
    ESemi ed _ _ -> ed
