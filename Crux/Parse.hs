{-# LANGUAGE OverloadedStrings #-}
module Crux.Parse where

import Crux.Tokens
import Crux.AST
import qualified Text.Parsec as P
import Data.List (foldl1')
import Control.Applicative ((<|>))
import Control.Monad.Identity (Identity)
import Control.Monad.Trans (liftIO)
import Data.Text (Text)

type Parser = P.ParsecT [Token] () IO

getToken predicate = P.tokenPrim showTok nextPos predicate
  where
    showTok = show
    nextPos pos _ _ = pos

anyToken :: Parser Token
anyToken = getToken Just

token :: Token -> Parser Token
token expected = getToken testTok
  where
    testTok tok
        | expected == tok = Just tok
        | otherwise = Nothing

identifier :: Text -> Parser Token
identifier name = getToken testTok
  where
    testTok tok = case tok of
        TIdentifier t | t == name -> Just tok
        _ -> Nothing


anyIdentifier = getToken testTok
  where
    testTok tok = case tok of
        TIdentifier t -> Just t
        _ -> Nothing

peekAndShow msg = do
    peeked <- P.lookAhead $ P.anyToken
    liftIO $ print (msg, peeked)
    return ()

printExpression :: Parser Expression
printExpression = do
    P.try $ identifier "print"
    expr <- noSemiExpression
    return $ EPrint expr

literalExpression = P.tokenPrim showTok nextPos testTok
  where
    showTok = show
    nextPos pos _ _ = pos
    testTok tok = case tok of
        TInteger i -> Just $ ELiteral $ LInteger i
        TString s -> Just $ ELiteral $ LString s
        _ -> Nothing

letExpression :: Parser Expression
letExpression = do
    P.try $ token TLet
    name <- anyIdentifier
    token TEqual
    expr <- noSemiExpression
    return (ELet name expr)

semiExpression = do
    e <- noSemiExpression
    token TSemicolon
    e2 <- noSemiExpression
    return $ ESemi e e2

parenExpression = do
    token $ TOpenParen
    e <- P.try semiExpression <|> noSemiExpression
    token $ TCloseParen
    return e

noSemiExpression =
    P.try letExpression
    <|> P.try printExpression
    <|> P.try parenExpression
    <|> P.try literalExpression

expression = do
    s <- noSemiExpression
    token TSemicolon
    return s

document :: Parser [Expression]
document = P.many1 expression

parse :: P.SourceName -> [Token] -> IO (Either P.ParseError [Expression])
parse fileName tokens = P.runParserT document () fileName tokens
