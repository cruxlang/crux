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

stringChar :: Parser u String
stringChar = do
    let hexChar = do
            c <- P.satisfy isHexDigit
            return $ digitToInt c
    let hex2 a b = toEnum $
            0x10 * a +
            b
    let hex4 a b c d = toEnum $
            0x1000 * a +
            0x100 * b +
            0x10 * c +
            d
    let hex8 a b c d e f g h = toEnum $
            0x10000000 * a +
            0x1000000 * b +
            0x100000 * c +
            0x10000 * d +
            0x1000 * e +
            0x100 * f +
            0x10 * g +
            h
    let escapes =
            [ P.char '0' *> return '\0'
            , P.char '\\' *> return '\\'
            , P.char '"' *> return '"'
            , P.char '?' *> return '?'
            , P.char '\'' *> return '\''
            , P.char 'a' >> return '\a'
            , P.char 'b' >> return '\b'
            , P.char 'f' >> return '\f'
            , P.char 'r' *> return '\r'
            , P.char 'n' *> return '\n'
            , P.char 't' *> return '\t'
            , P.char 'v' *> return '\v'
            , P.char 'x' *> (hex2 <$> hexChar <*> hexChar)
            , P.char 'u' *> (hex4 <$> hexChar <*> hexChar <*> hexChar <*> hexChar)
            , P.char 'U' *> (hex8 <$> hexChar <*> hexChar <*> hexChar <*> hexChar <*> hexChar <*> hexChar <*> hexChar <*> hexChar)
            ]
    let escape = P.char '\\' *> P.choice escapes
    let normal = P.satisfy (`notElem` ['\"', '\n', '\r'])

    P.many $ escape <|> normal

stringLiteral :: Parser u (Token Pos)
stringLiteral = do
    p <- pos
    _ <- P.try $ P.char '"'
    chars <- stringChar
    _ <- P.char '"'
    return $ Token p $ TString $ T.pack chars

parseAnyIdentifier :: (Char -> Bool) -> (Text -> TokenType) -> Parser u (Token Pos)
parseAnyIdentifier isStartChar f = do
    p <- pos
    let isIdentifierChar x = isAlpha x || isNumber x || x == '_'
    first <- P.satisfy isStartChar
    rest <- P.many $ P.satisfy isIdentifierChar
    return $ Token p $ f $ T.pack (first : rest)

orUnderscore :: (Char -> Bool) -> Char -> Bool
orUnderscore f x = f x || x == '_'

parseUpperIdentifier :: Parser u (Token Pos)
parseUpperIdentifier = parseAnyIdentifier isUpper TUpperIdentifier

parseLowerIdentifier :: Parser u (Token Pos)
parseLowerIdentifier = parseAnyIdentifier (orUnderscore isLower) TLowerIdentifier

parseIdentifier :: Parser u (Token Pos)
parseIdentifier = parseUpperIdentifier <|> parseLowerIdentifier

{-
parseIdentifier :: Parser u (Token Pos)
parseIdentifier = do
    p <- pos
    let isIdentifierStart '_' = True
        isIdentifierStart x = isAlpha x

        isIdentifierChar x = isIdentifierStart x || isNumber x

    first <- P.satisfy isIdentifierStart
    rest <- P.many $ P.satisfy isIdentifierChar
    return $ Token p $ TIdentifier $ T.pack (first:rest)
-}

token :: Parser u (Token Pos)
token =
    P.try keyword
    <|> P.try integerLiteral
    <|> stringLiteral
    <|> P.try parseIdentifier
    <|> P.try symbol

keyword :: Parser u (Token Pos)
keyword = P.try $ do
    Token p (TLowerIdentifier i) <- parseLowerIdentifier
    fmap (Token p) $ case i of
        "_" -> return TWildcard
        "import" -> return TImport
        "export" -> return TExport
        "let" -> return TLet
        "fun" -> return TFun
        "data" -> return TData
        "declare" -> return TDeclare
        "jsffi" -> return TJSFFI
        "type" -> return TType
        "match" -> return TMatch
        "if" -> return TIf
        "then" -> return TThen
        "else" -> return TElse
        "while" -> return TWhile
        "for" -> return TFor
        "in" -> return TIn
        "do" -> return TDo
        "return" -> return TReturn
        "const" -> return TConst
        "mutable" -> return TMutable
        _ -> fail ""

symbol :: Parser u (Token Pos)
symbol = sym3 '.' '.' '.' TEllipsis
     <|> sym2 '=' '>' TFatRightArrow
     <|> sym2 '-' '>' TRightArrow
     <|> sym2 '=' '=' TDoubleEqual
     <|> sym2 '!' '=' TNotEqual
     <|> sym2 '<' '=' TLessEqual
     <|> sym2 '>' '=' TGreaterEqual
     <|> sym ';' TSemicolon
     <|> sym ':' TColon
     <|> sym '.' TDot
     <|> sym ',' TComma
     <|> sym '=' TEqual
     <|> sym '(' TOpenParen
     <|> sym ')' TCloseParen
     <|> sym '{' TOpenBrace
     <|> sym '}' TCloseBrace
     <|> sym '[' TOpenBracket
     <|> sym ']' TCloseBracket
     <|> sym '+' TPlus
     <|> sym '-' TMinus
     <|> sym '*' TMultiply
     <|> sym '/' TDivide
     <|> sym '<' TLess
     <|> sym '>' TGreater
     <|> sym2 '&' '&' TAndAnd
     <|> sym2 '|' '|' TOrOr
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
    sym3 c1 c2 c3 tok = P.try $ do
        p <- pos
        _ <- P.char c1
        _ <- P.char c2
        _ <- P.char c3
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
    void $ P.manyTill (blockComment <|> void P.anyChar) $ P.char '*' >> P.char '/'

comments :: Parser u ()
comments =
    whitespace >> void (P.many $ (lineComment <|> blockComment) >> whitespace)

document :: Parser u [Token Pos]
document = do
    comments
    r <- P.many $ P.try (comments >> token)
    comments
    P.eof
    return r

lexSource :: FilePath -> Text -> Either P.ParseError [Token Pos]
lexSource fileName text =
    P.runParser document () fileName text
