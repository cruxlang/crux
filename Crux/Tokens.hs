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
    | TColonColon
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
    | TMutable
    | TTrait
    | TImpl
    | TTypeFamily
    | TTypeImpl
    deriving (Eq)

data Token tdata = Token tdata TokenType

instance Show (Token tdata) where
    show (Token _ ttype) = show ttype

-- TODO: merge this into a standard set of lists?
showTokenType :: TokenType -> String
showTokenType = \case
    TInteger i -> "integer " ++ show i
    TString s -> "string " ++ show s
    TUpperIdentifier i -> "identifier " ++ show i
    TLowerIdentifier i -> "identifier " ++ show i
    TOpenBrace -> "{"
    TCloseBrace -> "}"
    TOpenParen -> "("
    TCloseParen -> ")"
    TOpenBracket -> "["
    TCloseBracket -> "]"
    TQuestionMark -> "?"
    TSemicolon -> ";"
    TColonColon -> "::"
    TColon -> ":"
    TComma -> ","
    TEqual -> "="
    TDot -> "."
    TRightArrow -> "->"
    TFatRightArrow -> "=>"
    TEllipsis -> "..."
    TPlus -> "+"
    TMinus -> "-"
    TMultiply -> "*"
    TDivide -> "/"
    TLess -> "<"
    TGreater -> ">"
    TLessEqual -> "<="
    TGreaterEqual -> ">="
    TDoubleEqual -> "=="
    TNotEqual -> "!="
    TAndAnd -> "&&"
    TOrOr -> "||"
    TWildcard -> "_"
    TAs -> "as"
    TPragma -> "pragma"
    TImport -> "import"
    TExport -> "export"
    TFun -> "fun"
    TLet -> "let"
    TData -> "data"
    TDeclare -> "declare"
    TException -> "exception"
    TThrow -> "throw"
    TTry -> "try"
    TCatch -> "catch"
    TJSFFI -> "jsffi"
    TType -> "type"
    TMatch -> "match"
    TIf -> "if"
    TThen -> "then"
    TElse -> "else"
    TWhile -> "while"
    TFor -> "for"
    TIn -> "in"
    TDo -> "do"
    TReturn -> "return"
    TMutable -> "mutable"
    TTrait -> "trait"
    TImpl -> "impl"
    TTypeFamily -> "typefamily"
    TTypeImpl -> "typeimpl"

instance Show TokenType where
    show = showTokenType

tokenData :: Token tdata -> tdata
tokenData (Token tdata _) = tdata
