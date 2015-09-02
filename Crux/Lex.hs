{-# LANGUAGE OverloadedStrings #-}

module Crux.Lex where

import Control.Monad (void)
import Data.Char
import Crux.Tokens
import qualified Text.Parsec as P
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Identity (Identity)
import Text.Parsec ((<|>))

type Parser u a = P.ParsecT Text u Identity a

pos :: Parser u Pos
pos = do
    p <- P.getPosition
    return $ Pos (P.sourceLine p) (P.sourceColumn p)

integerLiteral :: Parser u (Token Pos)
integerLiteral = do
    p <- pos
    digits <- P.many1 P.digit
    return $ Token p $ TInteger $ read digits

stringLiteral :: Parser u (Token Pos)
stringLiteral = do
    p <- pos
    _ <- P.char '"'
    chars <- P.many $ P.satisfy (/= '"')
    _ <- P.char '"'
    return $ Token p $ TString $ T.pack chars

parseIdentifier :: Parser u (Token Pos)
parseIdentifier = do
    p <- pos
    let isIdentifierStart '_' = True
        isIdentifierStart x = isAlpha x

        isIdentifierChar x = isIdentifierStart x || isNumber x

    first <- P.satisfy isIdentifierStart
    rest <- P.many $ P.satisfy isIdentifierChar
    return $ Token p $ TIdentifier $ T.pack (first:rest)

token :: Parser u (Token Pos)
token =
    P.try keyword
    <|> P.try integerLiteral
    <|> P.try stringLiteral
    <|> P.try parseIdentifier
    <|> P.try symbol

keyword :: Parser u (Token Pos)
keyword = P.try $ do
    Token p (TIdentifier i) <- parseIdentifier
    fmap (Token p) $ case i of
        "import" -> return TImport
        "export" -> return TExport
        "let" -> return TLet
        "fun" -> return TFun
        "data" -> return TData
        "jsffi" -> return TJSFFI
        "type" -> return TType
        "match" -> return TMatch
        "if" -> return TIf
        "then" -> return TThen
        "else" -> return TElse
        "while" -> return TWhile
        "do" -> return TDo
        "return" -> return TReturn
        "const" -> return TConst
        "mutable" -> return TMutable
        _ -> fail ""

symbol :: Parser u (Token Pos)
symbol = sym2 '=' '>' TFatRightArrow
     <|> sym2 '-' '>' TRightArrow
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
        return (Token p tok)
    sym2 c1 c2 tok = P.try $ do
        p <- pos
        _ <- P.char c1
        _ <- P.char c2
        return (Token p tok)

whitespace :: Parser u ()
whitespace = P.spaces

lineComment :: Parser u ()
lineComment = do
    void $ P.try $ P.char '/' >> P.char '/'
    void $ P.manyTill P.anyChar (P.eof <|> void (P.char '\n'))

blockComment :: Parser u ()
blockComment = do
    void $ P.try $ P.char '/' >> P.char '*'
    void $ P.manyTill P.anyChar (P.char '*' >> P.char '/')

comments :: Parser u ()
comments =
    whitespace >> void (P.many $ (lineComment <|> blockComment) >> whitespace)

document :: Parser u [Token Pos]
document = do
    whitespace
    r <- P.many $ P.try (comments >> token)
    whitespace
    P.eof
    return r

lexSource :: FilePath -> Text -> Either P.ParseError [Token Pos]
lexSource fileName text =
    P.runParser document () fileName text
