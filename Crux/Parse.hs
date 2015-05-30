{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Crux.Parse where

import Crux.Tokens
import Crux.AST
import qualified Text.Parsec as P
import Control.Applicative ((<|>), (<*))
import Control.Monad.Trans (liftIO)
import Data.Text (Text)

type Parser = P.ParsecT [Token Pos] () IO
type ParseData = ()
type ParseExpression = Expression ParseData
type ParseDeclaration = Declaration ParseData

getToken :: P.Stream s m (Token Pos)
         => (Token Pos -> Maybe a) -> P.ParsecT s u m a
getToken predicate = P.tokenPrim showTok nextPos predicate
  where
    showTok = show
    nextPos pos t _ =
        let Pos{..} = tokenData t
        in P.setSourceLine (P.setSourceColumn pos posCol) posLine

anyToken :: Parser (Token Pos)
anyToken = getToken Just

token :: (Pos -> Token Pos) -> Parser (Token Pos)
token expected = getToken testTok
  where
    testTok tok
        | (expected (Pos 0 0)) == tok = Just tok
        | otherwise = Nothing

identifier :: Text -> Parser (Token Pos)
identifier name = getToken testTok
  where
    testTok tok = case tok of
        TIdentifier _ t | t == name -> Just tok
        _ -> Nothing

anyIdentifier :: Parser Text
anyIdentifier = getToken testTok
  where
    testTok tok = case tok of
        TIdentifier _ t -> Just t
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
        TInteger _ i -> Just $ ELiteral () $ LInteger i
        TString _ s -> Just $ ELiteral () $ LString s
        _ -> Nothing

identifierExpression :: Parser ParseExpression
identifierExpression = getToken testTok
  where
    testTok (TIdentifier _ txt) = Just $ EIdentifier () txt
    testTok _ = Nothing

functionExpression :: Parser ParseExpression
functionExpression = do
    _ <- P.try $ token TFun
    args <- P.many anyIdentifier
    _ <- token TOpenBrace
    body <- P.many expression
    _ <- token TCloseBrace
    return $ EFun () args body

applicationExpression :: Parser ParseExpression
applicationExpression = do
    lhs <- identifierExpression <|> literalExpression
    rhs <- P.optionMaybe (P.try applicationExpression)
    case rhs of
        Just rhs' -> return $ EApp () lhs rhs'
        Nothing -> return lhs

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
    <|> P.try functionExpression
    <|> applicationExpression

expression :: Parser ParseExpression
expression = do
    s <- noSemiExpression
    _ <- token TSemicolon
    return s

letDeclaration :: Parser ParseDeclaration
letDeclaration = do
    ELet ed name expr <- letExpression
    return $ DLet ed name expr

dataDeclaration :: Parser ParseDeclaration
dataDeclaration = do
    _ <- P.try $ token TData

    name <- anyIdentifier
    -- type vars go here
    _ <- token TOpenBrace
    variants <- P.many $ anyIdentifier <* token TSemicolon
    _ <- token TCloseBrace
    return $ DData name variants

declaration :: Parser ParseDeclaration
declaration =
    dataDeclaration <|>
    letDeclaration

document :: Parser [ParseDeclaration]
document = do
    doc <- P.many1 $ declaration <* token TSemicolon
    P.eof
    return doc

parse :: P.SourceName -> [Token Pos] -> IO (Either P.ParseError [ParseDeclaration])
parse fileName tokens = P.runParserT document () fileName tokens
