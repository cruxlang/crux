
module Crux.Tokens where

import Data.Text

data Token
    = TInteger Integer
    | TString Text
    | TIdentifier Text
    | TOpenBrace
    | TCloseBrace
    | TOpenParen
    | TCloseParen
    | TSemicolon
    | TEqual
    | TLet
      deriving (Show, Eq)
