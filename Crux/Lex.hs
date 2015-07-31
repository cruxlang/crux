{-# LANGUAGE OverloadedStrings #-}

module Crux.Lex where

import Data.Char
import Crux.Tokens
import qualified Text.Parsec as P
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as DTE
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Identity (Identity)
import Text.Parsec ((<|>))

pos :: P.ParsecT Text u Identity Pos
pos = do
    p <- P.getPosition
    return $ Pos (P.sourceLine p) (P.sourceColumn p)

integerLiteral :: P.ParsecT Text u Identity (Token Pos)
integerLiteral = do
    p <- pos
    digits <- P.many1 P.digit
    return $ TInteger p $ read digits

stringLiteral :: P.ParsecT Text u Identity (Token Pos)
stringLiteral = do
    p <- pos
    _ <- P.char '"'
    chars <- P.many $ P.satisfy (/= '"')
    _ <- P.char '"'
    return $ TString p $ T.pack chars

parseIdentifier :: P.ParsecT Text u Identity (Token Pos)
parseIdentifier = do
    p <- pos
    let isIdentifierStart '_' = True
        isIdentifierStart x = isAlpha x

        isIdentifierChar x = isIdentifierStart x || isNumber x

    first <- P.satisfy isIdentifierStart
    rest <- P.many $ P.satisfy isIdentifierChar
    return $ TIdentifier p $ T.pack (first:rest)

token :: P.ParsecT Text u Identity (Token Pos)
token =
    P.try keyword
    <|> P.try integerLiteral
    <|> P.try stringLiteral
    <|> P.try parseIdentifier
    <|> P.try symbol

keyword :: P.ParsecT Text u Identity (Token Pos)
keyword = P.try $ do
    TIdentifier p i <- parseIdentifier
    case i of
        "let" -> return $ TLet p
        "rec" -> return $ TRec p
        "fun" -> return $ TFun p
        "data" -> return $ TData p
        "match" -> return $ TMatch p
        "if" -> return $ TIf p
        "then" -> return $ TThen p
        "else" -> return $ TElse p
        "return" -> return $ TReturn p
        _ -> fail ""

symbol :: P.ParsecT Text u Identity (Token Pos)
symbol = sym2 '=' '>' TFatRightArrow
     <|> sym ';' TSemicolon
     <|> sym ':' TColon
     <|> sym '.' TDot
     <|> sym ',' TComma
     <|> sym '=' TEqual
     <|> sym '(' TOpenParen
     <|> sym ')' TCloseParen
     <|> sym '{' TOpenBrace
     <|> sym '}' TCloseBrace
     <|> sym '+' TPlus
     <|> sym '-' TMinus
     <|> sym '*' TMultiply
     <|> sym '/' TDivide
  where
    sym ch tok = do
        p <- pos
        _ <- P.char ch
        return (tok p)
    sym2 c1 c2 tok = P.try $ do
        p <- pos
        _ <- P.char c1
        _ <- P.char c2
        return (tok p)

whitespace :: P.ParsecT Text u Identity ()
whitespace = P.spaces

document :: P.ParsecT Text u Identity [Token Pos]
document = do
    whitespace
    r <- P.many1 $ P.try (whitespace >> token)
    whitespace
    P.eof
    return r

lexSource :: FilePath -> Text -> Either P.ParseError [Token Pos]
lexSource fileName text =
    P.runParser document () fileName text

lex :: FilePath -> IO (Either P.ParseError [Token Pos])
lex fileName = do
    bytes <- BS.readFile fileName
    return $ lexSource fileName (DTE.decodeUtf8 bytes)
