module Crux.JSTree where

import Data.Text (Text)

type Name = Text

data Statement
    = SBlock [Statement]
    | SVar Name Expression
    | SFunction (Maybe Name) (Maybe Name) [Statement] -- function name(arg_name) { statements }
    | SExpression Expression
    | SReturn Expression
    deriving (Show, Eq)

data Literal
    = LInteger Integer
    | LString Text
    | LTrue
    | LFalse
    | LNull
    | LUndefined
    deriving (Show, Eq)

data Expression
    = EApplication Expression (Maybe Expression)
    | EFunction (Maybe Name) [Statement] -- function(arg_name) { statements }
    | ELiteral Literal
    | EIdentifier Name
    | ESemi Expression Expression
    deriving (Show, Eq)
