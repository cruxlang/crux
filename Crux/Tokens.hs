{-# LANGUAGE RecordWildCards #-}
module Crux.Tokens where

import Data.Text

data Pos = Pos
    { posLine :: Int
    , posCol :: Int
    }
    deriving (Eq)

instance Show Pos where
    show Pos{..} = (show posLine) ++ ":" ++ (show posCol)

data Token tdata
    = TInteger tdata Integer
    | TString tdata Text
    | TIdentifier tdata Text
    -- symbols
    | TOpenBrace tdata
    | TCloseBrace tdata
    | TOpenParen tdata
    | TCloseParen tdata
    | TSemicolon tdata
    | TColon tdata
    | TComma tdata
    | TEqual tdata
    | TDot tdata
    | TRightArrow tdata
    | TFatRightArrow tdata
    | TPlus tdata
    | TMinus tdata
    | TMultiply tdata
    | TDivide tdata
    -- Keywords
    | TFun tdata
    | TLet tdata
    | TData tdata
    | TType tdata
    | TMatch tdata
    | TIf tdata
    | TThen tdata
    | TElse tdata
    | TReturn tdata
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
        (TColon _,          TColon _)          -> True
        (TComma _,          TComma _)          -> True
        (TDot _,            TDot _)            -> True
        (TRightArrow _,     TRightArrow _)     -> True
        (TFatRightArrow _,  TFatRightArrow _)  -> True
        (TEqual _,          TEqual _)          -> True
        (TPlus _,           TPlus _)           -> True
        (TMinus _,          TMinus _)          -> True
        (TMultiply _,       TMultiply _)       -> True
        (TDivide _,         TDivide _)         -> True
        (TFun _,            TFun _)            -> True
        (TLet _,            TLet _)            -> True
        (TData _,           TData _)           -> True
        (TType _,           TType _)           -> True
        (TMatch _,          TMatch _)          -> True
        (TIf _,             TIf _)             -> True
        (TThen _,           TThen _)           -> True
        (TElse _,           TElse _)           -> True
        (TReturn _,         TReturn _)         -> True
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
    TColon tdata -> tdata
    TComma tdata -> tdata
    TDot tdata -> tdata
    TEqual tdata -> tdata
    TPlus tdata -> tdata
    TMinus tdata -> tdata
    TMultiply tdata -> tdata
    TDivide tdata -> tdata
    TRightArrow tdata -> tdata
    TFatRightArrow tdata -> tdata
    TFun tdata -> tdata
    TLet tdata -> tdata
    TData tdata -> tdata
    TType tdata -> tdata
    TMatch tdata -> tdata
    TIf tdata -> tdata
    TThen tdata -> tdata
    TElse tdata -> tdata
    TReturn tdata -> tdata
