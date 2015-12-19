{-# LANGUAGE RecordWildCards #-}
module Crux.Tokens where

import Crux.Prelude

-- TODO: we should record the start and end positions of tokens
data Pos = Pos
    -- The start column of the line containing this token. Zero-based.
    { posLineStart :: Int
    -- This token's line number. One-based.
    , posLine :: Int
    -- This token's column number. One-based.
    , posCol :: Int
    }
    deriving (Eq)

instance Show Pos where
    show Pos{..} = "<" <> show posLineStart <> ">" <> show posLine <> ":" <> show posCol

data TokenType
    = TInteger Integer
    | TString Text
    | TUpperIdentifier Text
    | TLowerIdentifier Text
    -- symbols
    | TOpenBrace
    | TCloseBrace
    | TOpenParen
    | TCloseParen
    | TOpenBracket
    | TCloseBracket
    | TSemicolon
    | TColon
    | TComma
    | TEqual
    | TDot
    | TRightArrow
    | TFatRightArrow
    | TEllipsis
    | TPlus
    | TMinus
    | TMultiply
    | TDivide
    | TLess
    | TGreater
    | TLessEqual
    | TGreaterEqual
    | TDoubleEqual
    | TNotEqual
    | TAndAnd
    | TOrOr
    | TWildcard -- _
    -- Keywords
    | TImport
    | TExport
    | TFun
    | TLet
    | TData
    | TDeclare
    | TJSFFI
    | TType
    | TMatch
    | TIf
    | TThen
    | TElse
    | TWhile
    | TFor
    | TIn
    | TDo
    | TReturn
    | TConst
    | TMutable
    deriving (Show, Eq)

data Token tdata = Token tdata TokenType
      deriving (Show)

tokenData :: Token tdata -> tdata
tokenData (Token tdata _) = tdata
