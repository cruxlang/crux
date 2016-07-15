{-# LANGUAGE RecordWildCards #-}
module Crux.Tokens where

import Crux.Prelude

-- TODO: we should record the start and end positions of tokens
-- All of these numbers are one-based, because that's how editors standardized.
-- (Except emacs.  Emacs users will have to deal with column numbers being off
-- by one.)
data Pos = Pos
    -- The start column of the line containing this token.
    { posLineStart :: Int
    -- This token's line number.
    , posLine      :: Int
    -- This token's column number.
    , posCol       :: Int
    }
    deriving (Eq)

instance Show Pos where
    show Pos{..} = "Pos " ++ show posLineStart ++ " " ++ show posLine ++ " " ++ show posCol

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
    | TQuestionMark
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
    | TAs
    | TPragma
    | TImport
    | TExport
    | TFun
    | TLet
    | TData
    | TDeclare
    | TException
    | TThrow
    | TTry
    | TCatch
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
    | TTrait
    | TImpl
    deriving (Show, Eq)

data Token tdata = Token tdata TokenType
      deriving (Show)

tokenData :: Token tdata -> tdata
tokenData (Token tdata _) = tdata
