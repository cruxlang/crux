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

anyIdentifier = P.tokenPrim showTok nextPos testTok
  where
    showTok = show
    nextPos pos _ _ = pos
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

-- literalExpression = do
--     tok <- anyToken
--
--     t <- case tok of
--         TInteger i -> return $ ELiteral $ LInteger i
--         TString s ->  return $ ELiteral $ LString s
--         _ -> fail ""
--
--     return t

letExpression :: Parser Expression
letExpression = do
    P.try $ token TLet
    name <- anyIdentifier
    token TEqual
    expr <- noSemiExpression
    return (ELet name expr)

parenExpression = do
    token $ TOpenParen
    e <- noSemiExpression
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
