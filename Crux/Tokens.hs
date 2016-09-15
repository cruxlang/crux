module Crux.Tokens where

import Crux.Prelude

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
