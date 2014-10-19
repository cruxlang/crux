{-# LANGUAGE OverloadedStrings #-}
module Crux.Parse where

import Crux.Tokens
import Crux.AST
import qualified Text.Parsec as P
import Control.Monad.Identity (Identity)
import Data.Text (Text)

type Parser = P.ParsecT [Token] () Identity

anyToken :: Parser Token
anyToken = P.tokenPrim showTok nextPos testTok
  where
    showTok = show
    nextPos pos _ _ = pos
    testTok = Just

token :: Token -> Parser Token
token expected = P.tokenPrim showTok nextPos testTok
  where
    showTok = show
    nextPos pos _ _ = pos
    testTok tok
        | expected == tok = Just tok
        | otherwise = Nothing

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
    expr <- anyToken
    case expr of
        TString value -> return $ EPrint $ LString value
        TInteger value -> return $ EPrint $ LInteger value
        _ -> fail "Expected int or string"

statement = do
    s <- printStatement
    token TSemicolon
    return s

document :: Parser [Expression]
document = P.many1 statement

parse :: P.SourceName -> [Token] -> Either P.ParseError [Expression]
parse fileName tokens = P.runParser document () fileName tokens
