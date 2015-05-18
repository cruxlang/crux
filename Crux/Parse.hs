{-# LANGUAGE OverloadedStrings #-}
module Crux.Parse where

import Crux.Tokens
import Crux.AST
import qualified Text.Parsec as P
import Control.Applicative ((<|>))
import Control.Monad.Trans (liftIO)
import Data.Text (Text)

type Parser = P.ParsecT [Token] () IO

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

printExpression :: Parser Expression
printExpression = do
    _ <- P.try $ identifier "print"
    expr <- noSemiExpression
    return $ EPrint expr

literalExpression :: Parser Expression
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
    _ <- P.try $ token TLet
    name <- anyIdentifier
    _ <- token TEqual
    expr <- noSemiExpression
    return (ELet name expr)

semiExpression :: Parser Expression
semiExpression = do
    e <- noSemiExpression
    _ <- token TSemicolon
    e2 <- noSemiExpression
    return $ ESemi e e2

parenExpression :: Parser Expression
parenExpression = do
    _ <- token $ TOpenParen
    e <- P.try semiExpression <|> noSemiExpression
    _ <- token $ TCloseParen
    return e

noSemiExpression :: Parser Expression
noSemiExpression =
    P.try letExpression
    <|> P.try printExpression
    <|> P.try parenExpression
    <|> P.try literalExpression

expression :: Parser Expression
expression = do
    s <- noSemiExpression
    _ <- token TSemicolon
    return s

document :: Parser [Expression]
document = P.many1 expression

parse :: P.SourceName -> [Token] -> IO (Either P.ParseError [Expression])
parse fileName tokens = P.runParserT document () fileName tokens
