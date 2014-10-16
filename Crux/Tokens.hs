
module Crux.Tokens where

import Data.Text

data Token
    = TInteger Integer
    | TString Text
    | TIdentifier Text
    | TOpenBrace
    | TCloseBrace
    | TSemicolon
      deriving (Show, Eq)
