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

integerLiteral :: P.ParsecT Text u Identity Token
integerLiteral = do
    digits <- P.many1 P.digit
    return $ TInteger $ read digits

stringLiteral :: P.ParsecT Text u Identity Token
stringLiteral = do
    _ <- P.char '"'
    chars <- P.many $ P.satisfy (/= '"')
    _ <- P.char '"'
    return $ TString $ T.pack chars

parseIdentifier :: P.ParsecT Text u Identity Token
parseIdentifier = do
    first <- P.satisfy isAlpha
    rest <- P.many P.alphaNum
    return $ TIdentifier $ T.pack (first:rest)

token :: P.ParsecT Text u Identity Token
token =
    P.try keyword
    <|> P.try integerLiteral
    <|> P.try stringLiteral
    <|> P.try parseIdentifier
    <|> P.try symbol

keyword :: P.ParsecT Text u Identity Token
keyword = P.try $ do
    TIdentifier i <- parseIdentifier
    case i of
        "let" -> return TLet
        "fun" -> return TFun
        _ -> fail ""

symbol :: P.ParsecT Text u Identity Token
symbol = sym ';' TSemicolon
     <|> sym '=' TEqual
     <|> sym '(' TOpenParen
     <|> sym ')' TCloseParen
     <|> sym '{' TOpenBrace
     <|> sym '}' TCloseBrace
  where
    sym ch tok = P.char ch >> return tok
    sym2 c1 c2 tok = P.char c1 >> P.char c2 >> return tok

whitespace :: P.ParsecT Text u Identity ()
whitespace = P.spaces

document :: P.ParsecT Text u Identity [Token]
document = do
    whitespace
    r <- P.many1 $ P.try (whitespace >> token)
    whitespace
    P.eof
    return r

lexSource :: FilePath -> Text -> Either P.ParseError [Token]
lexSource fileName text =
    P.runParser document () fileName text

lex :: FilePath -> IO (Either P.ParseError [Token])
lex fileName = do
    bytes <- BS.readFile fileName
    return $ lexSource fileName (DTE.decodeUtf8 bytes)
