{-# LANGUAGE OverloadedStrings #-}
module Crux.Parse where

import Crux.Tokens
import Crux.AST
import qualified Text.Parsec as P
import Control.Monad.Identity (Identity)
import Data.Text (Text)

type Parser = P.ParsecT [Token] () Identity

token :: Parser Token
token = P.tokenPrim showTok nextPos testTok
  where
    showTok = show
    nextPos pos _ _ = pos
    testTok = Just

identifier :: Text -> Parser Token
identifier name = P.tokenPrim showTok nextPos testTok
  where
    showTok = show
    nextPos pos _ _ = pos
    testTok tok = case tok of
        TIdentifier t | t == name -> Just tok
        _ -> Nothing

printStatement :: Parser Expression
printStatement = do
    identifier "print"
    expr <- token
    case expr of
        TString value -> return $ EPrint $ LString value
        TInteger value -> return $ EPrint $ LInteger value
        _ -> fail "Expected int or string"

document :: Parser Expression
document = printStatement

parse :: P.SourceName -> [Token] -> Either P.ParseError Expression
parse fileName tokens = P.runParser document () fileName tokens
