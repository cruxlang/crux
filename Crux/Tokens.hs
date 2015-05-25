
module Crux.Tokens where

import Data.Text

data Pos = Pos
    { posLine :: Int
    , posCol :: Int
    }
    deriving (Eq, Show)

data Token tdata
    = TInteger tdata Integer
    | TString tdata Text
    | TIdentifier tdata Text
    | TOpenBrace tdata
    | TCloseBrace tdata
    | TOpenParen tdata
    | TCloseParen tdata
    | TSemicolon tdata
    | TEqual tdata
    | TFun tdata
    | TLet tdata
      deriving (Show)

instance Eq (Token a) where
    a == b = case (a, b) of
        (TInteger _ lhs,    TInteger _ rhs)    -> lhs == rhs
        (TString _ lhs,     TString _ rhs)     -> lhs == rhs
        (TIdentifier _ lhs, TIdentifier _ rhs) -> lhs == rhs
        (TOpenBrace _,      TOpenBrace _)      -> True
        (TCloseBrace _,     TCloseBrace _)     -> True
        (TOpenParen _,      TOpenParen _)      -> True
        (TCloseParen _,     TCloseParen _)     -> True
        (TSemicolon _,      TSemicolon _)      -> True
        (TEqual _,          TEqual _)          -> True
        (TFun _,            TFun _)            -> True
        (TLet _,            TLet _)            -> True
        _ -> False

tokenData :: Token tdata -> tdata
tokenData tok = case tok of
    TInteger tdata _ -> tdata
    TString tdata _ -> tdata
    TIdentifier tdata _ -> tdata
    TOpenBrace tdata -> tdata
    TCloseBrace tdata -> tdata
    TOpenParen tdata -> tdata
    TCloseParen tdata -> tdata
    TSemicolon tdata -> tdata
    TEqual tdata -> tdata
    TFun tdata -> tdata
    TLet tdata -> tdata
