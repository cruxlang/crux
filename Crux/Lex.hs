{-# LANGUAGE OverloadedStrings #-}

module Crux.Lex where

import Control.Monad.Identity (Identity)
import Crux.Prelude
import Crux.Tokens
import Crux.Pos
import Data.Char
import qualified Data.Text as T
import Text.Parsec ((<|>))
import qualified Text.Parsec as P

type ParserState = (Int, Int) -- current line, start column
type Parser a = P.ParsecT Text ParserState Identity a
type LexToken = Token ParsePos

pos :: Parser ParsePos
pos = do
    p <- P.getPosition
    let sourceLine = P.sourceLine p
    let sourceColumn = P.sourceColumn p

    -- assumes we run pos for every token
    (currentLine, lineStart) <- P.getState
    newLineStart <- if currentLine == sourceLine then do
        return lineStart
    else do
        P.putState (sourceLine, sourceColumn)
        return sourceColumn

    return $ ParsePos
        { ppLineStart = newLineStart
        , ppLine = sourceLine
        , ppColumn = sourceColumn
        }

integerLiteral :: Parser LexToken
integerLiteral = do
    p <- pos
    digits <- P.many1 P.digit
    return $ Token p $ TInteger $ read digits

stringChar :: Parser String
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

stringLiteral :: Parser LexToken
stringLiteral = do
    p <- pos
    _ <- P.char '"'
    chars <- stringChar
    _ <- P.char '"'
    return $ Token p $ TString $ T.pack chars

parseAnyIdentifier :: (Char -> Bool) -> (Text -> TokenType) -> Parser LexToken
parseAnyIdentifier isStartChar f = do
    p <- pos
    let isIdentifierChar x = isAlpha x || isNumber x || x == '_'
    first <- P.satisfy isStartChar
    rest <- P.many $ P.satisfy isIdentifierChar
    return $ Token p $ f $ T.pack (first : rest)

orUnderscore :: (Char -> Bool) -> Char -> Bool
orUnderscore f x = f x || x == '_'

parseUpperIdentifier :: Parser LexToken
parseUpperIdentifier = parseAnyIdentifier isUpper TUpperIdentifier

parseLowerIdentifier :: Parser LexToken
parseLowerIdentifier = parseAnyIdentifier (orUnderscore isLower) TLowerIdentifier

parseIdentifier :: Parser LexToken
parseIdentifier = parseUpperIdentifier <|> parseLowerIdentifier

{-
parseIdentifier :: Parser LexToken
parseIdentifier = do
    p <- pos
    let isIdentifierStart '_' = True
        isIdentifierStart x = isAlpha x

        isIdentifierChar x = isIdentifierStart x || isNumber x

    first <- P.satisfy isIdentifierStart
    rest <- P.many $ P.satisfy isIdentifierChar
    return $ Token p $ TIdentifier $ T.pack (first:rest)
-}

token :: Parser LexToken
token =
    keyword
    <|> integerLiteral
    <|> stringLiteral
    <|> parseIdentifier
    <|> symbol

keyword :: Parser LexToken
keyword = P.try $ do
    Token p (TLowerIdentifier i) <- parseLowerIdentifier
    fmap (Token p) $ case i of
        "_" -> return TWildcard
        "as" -> return TAs
        "pragma" -> return TPragma
        "import" -> return TImport
        "export" -> return TExport
        "let" -> return TLet
        "fun" -> return TFun
        "data" -> return TData
        "declare" -> return TDeclare
        "exception" -> return TException
        "jsffi" -> return TJSFFI
        "throw" -> return TThrow
        "try" -> return TTry
        "catch" -> return TCatch
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
        "trait" -> return TTrait
        "impl" -> return TImpl
        _ -> fail ""

symbol :: Parser LexToken
symbol = sym3 '.' '.' '.' TEllipsis
     <|> sym2 '=' '>' TFatRightArrow
     <|> sym2 '-' '>' TRightArrow
     <|> sym2 '=' '=' TDoubleEqual
     <|> sym2 '!' '=' TNotEqual
     <|> sym2 '<' '=' TLessEqual
     <|> sym2 '>' '=' TGreaterEqual
     <|> sym2 '&' '&' TAndAnd
     <|> sym2 '|' '|' TOrOr
     <|> sym '?' TQuestionMark
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

whitespace :: Parser ()
whitespace = P.spaces

lineComment :: Parser ()
lineComment = do
    void $ P.try $ P.char '/' >> P.char '/'
    void $ P.manyTill P.anyChar (P.eof <|> void (P.char '\n'))

blockComment :: Parser ()
blockComment = do
    void $ P.try $ P.char '/' >> P.char '*'
    void $ P.manyTill (blockComment <|> void P.anyChar) $ P.char '*' >> P.char '/'

comments :: Parser ()
comments = do
    whitespace
    _ <- P.many $ do
        lineComment <|> blockComment
        whitespace
    return ()

document :: Parser [LexToken]
document = do
    comments
    r <- P.many $ token <* comments
    P.eof
    return r

lexSource :: FilePath -> Text -> Either P.ParseError [LexToken]
lexSource fileName text =
    P.runParser document (-1, -1) fileName text
