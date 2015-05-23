{-# LANGUAGE OverloadedStrings #-}
module Crux.Parse where

import Crux.Tokens
import Crux.AST
import qualified Text.Parsec as P
import Control.Applicative ((<|>))
import Control.Monad.Trans (liftIO)
import Data.Text (Text)

type Parser = P.ParsecT [Token] () IO
type ParseData = ()
type ParseExpression = Expression ParseData

getToken :: (P.Stream s m t, Show t) => (t -> Maybe a) -> P.ParsecT s u m a
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


anyIdentifier :: Parser Text
anyIdentifier = getToken testTok
  where
    testTok tok = case tok of
        TIdentifier t -> Just t
        _ -> Nothing

peekAndShow :: Show msg => msg -> Parser ()
peekAndShow msg = do
    peeked <- P.lookAhead $ P.anyToken
    liftIO $ print (msg, peeked)
    return ()

printExpression :: Parser ParseExpression
printExpression = do
    _ <- P.try $ identifier "print"
    expr <- noSemiExpression
    return $ EPrint () expr

literalExpression :: Parser ParseExpression
literalExpression = P.tokenPrim showTok nextPos testTok
  where
    showTok = show
    nextPos pos _ _ = pos
    testTok tok = case tok of
        TInteger i -> Just $ ELiteral () $ LInteger i
        TString s -> Just $ ELiteral () $ LString s
        _ -> Nothing

identifierExpression :: Parser ParseExpression
identifierExpression = getToken testTok
  where
    testTok (TIdentifier txt) = Just $ EIdentifier () txt
    testTok _ = Nothing

letExpression :: Parser ParseExpression
letExpression = do
    _ <- P.try $ token TLet
    name <- anyIdentifier
    _ <- token TEqual
    expr <- noSemiExpression
    return (ELet () name expr)

semiExpression :: Parser ParseExpression
semiExpression = do
    e <- noSemiExpression
    _ <- token TSemicolon
    e2 <- noSemiExpression
    return $ ESemi () e e2

parenExpression :: Parser ParseExpression
parenExpression = do
    _ <- token $ TOpenParen
    e <- P.try semiExpression <|> noSemiExpression
    _ <- token $ TCloseParen
    return e

noSemiExpression :: Parser ParseExpression
noSemiExpression =
    P.try letExpression
    <|> P.try printExpression
    <|> P.try parenExpression
    <|> P.try literalExpression
    <|> identifierExpression

expression :: Parser ParseExpression
expression = do
    s <- noSemiExpression
    _ <- token TSemicolon
    return s

document :: Parser [ParseExpression]
document = do
    doc <- P.many1 expression
    P.eof
    return doc

parse :: P.SourceName -> [Token] -> IO (Either P.ParseError [ParseExpression])
parse fileName tokens = P.runParserT document () fileName tokens
