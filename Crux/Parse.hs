{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections #-}

module Crux.Parse where

import           Control.Applicative ((<*), (<|>))
import           Control.Monad.Trans (liftIO)
import           Crux.AST as AST
import           Crux.Tokens as Tokens
import Data.Char (isUpper)
import           Data.List           (foldl')
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Text.Parsec         as P

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

toStringExpression :: Parser ParseExpression
toStringExpression = do
    _ <- P.try $ identifier "toString"
    expr <- noSemiExpression
    return $ EToString () expr

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
    _ <- P.try $ token Tokens.TFun
    (first:args) <- P.many1 anyIdentifier
    _ <- token TOpenBrace
    body <- P.many expression
    _ <- token TCloseBrace

    let curryTheFunction firstArg [] = EFun () firstArg body
        curryTheFunction firstArg (next:rest) = EFun () firstArg [curryTheFunction next rest]

    return $ curryTheFunction first args

pattern :: Parser Pattern2
pattern =
    let parenPattern = do
            _ <- P.try $ token TOpenParen
            pat <- pattern
            _ <- token TCloseParen
            return pat

    in parenPattern <|> noParenPattern

noParenPattern :: Parser Pattern2
noParenPattern = do
    txt <- anyIdentifier
    if isUpper (T.head txt)
        then do
            patternArgs <- P.many pattern
            return $ PConstructor txt patternArgs
        else
            return $ PPlaceholder txt

matchExpression :: Parser ParseExpression
matchExpression = do
    _ <- P.try (token TMatch)
    expr <- noSemiExpression
    _ <- token TOpenBrace
    cases <- P.many $ do
        pat <- pattern
        _ <- token TFatRightArrow
        ex <- noSemiExpression
        _ <- token TSemicolon
        return $ Case pat ex

    _ <- token TCloseBrace
    return $ EMatch () expr cases

basicExpression :: Parser ParseExpression
basicExpression = identifierExpression <|> literalExpression <|> parenExpression

infixExpression :: Parser BinIntrinsic -> Parser ParseExpression -> Parser ParseExpression
infixExpression operator term = do
    first <- term
    rest <- P.many $ do
        t <- operator
        fmap (t,) term

    let foldIt acc [] = acc
        foldIt acc ((binopType, next):terms) =
            foldIt (AST.EBinIntrinsic () binopType acc next) terms

    return $ foldIt first rest

multiplyExpression :: Parser ParseExpression
multiplyExpression = do
    let op = (token TMultiply >> return BIMultiply) <|> (token TDivide >> return BIDivide)
    infixExpression op basicExpression

addExpression :: Parser ParseExpression
addExpression = do
    let op = (token TPlus >> return BIPlus) <|> (token TMinus >> return BIMinus)
    infixExpression op multiplyExpression

applicationExpression :: Parser ParseExpression
applicationExpression = do
    terms <- P.many1 addExpression
    case terms of
        [] -> error "This should be very impossible"
        [x] -> return x
        (x:xs) -> return $ foldl' (EApp ()) x xs

letExpression :: Parser ParseExpression
letExpression = do
    _ <- P.try $ token TLet
    mt <- P.optionMaybe $ token TRec
    let rec = case mt of
            Nothing -> NoRec
            _ -> Rec
    name <- anyIdentifier
    _ <- token TEqual
    expr <- noSemiExpression
    return (ELet () rec name expr)

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
    <|> matchExpression
    <|> P.try printExpression
    <|> P.try toStringExpression
    <|> P.try functionExpression
    <|> applicationExpression

expression :: Parser ParseExpression
expression = do
    s <- noSemiExpression
    _ <- token TSemicolon
    return s

letDeclaration :: Parser ParseDeclaration
letDeclaration = do
    ELet ed rec name expr <- letExpression
    return $ DLet ed rec name expr

dataDeclaration :: Parser ParseDeclaration
dataDeclaration = do
    _ <- P.try $ token TData

    name <- anyIdentifier
    -- type vars go here

    _ <- token TOpenBrace
    variants <- P.many $ do
        ctorname <- anyIdentifier
        ctordata <- P.many anyIdentifier
        _ <- token TSemicolon
        return (Variant ctorname ctordata)
    _ <- token TCloseBrace
    return $ DData name [] variants

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
