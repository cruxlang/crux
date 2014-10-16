{-# LANGUAGE OverloadedStrings #-}
module Crux.Parse where

import Crux.Tokens
import Crux.AST
import qualified Text.Parsec as P
import Control.Monad.Identity (Identity)

token :: P.ParsecT [Token] u Identity Token
token = P.tokenPrim showTok nextPos testTok
  where
    showTok = show
    nextPos pos _ _ = pos
    testTok = Just

printStatement :: P.ParsecT [Token] u Identity Expression
printStatement = do
    TIdentifier i <- token
    if i == "print" then do
        expr <- token
        case expr of
            TString value -> return $ EPrint $ LString value
            TInteger value -> return $ EPrint $ LInteger value
            _ -> fail "Expected int or string"
    else fail "Expected 'print'"

document = printStatement

parse fileName tokens = P.runParser document () fileName tokens
