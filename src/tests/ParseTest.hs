{-# LANGUAGE OverloadedStrings #-}

module ParseTest where

import           Crux.Parse
import           Crux.Tokens
import           Crux.AST
import           Crux.Lex
import           Crux.Driver
import qualified Data.Text as T
import           Test.HUnit
import qualified Text.Parsec as P

assertParseOk parser source expected = do
    case Crux.Lex.lexSource "<>" source of
        Left err ->
            assertFailure $ "Lexer error: " ++ show err
        Right tokens -> do
            result <- P.runParserT parser () "<>" tokens
            assertEqual (T.unpack source) (Right expected) result

testLiterals = do
    assertParseOk literalExpression "5"
        (ELiteral (LInteger 5))
    assertParseOk literalExpression "\"Hooper\""
        (ELiteral (LString "Hooper"))

testParens = do
    assertParseOk noSemiExpression "(5)"
        (ELiteral (LInteger 5))

testLet = do
    assertParseOk letExpression "let a = \"Hello\""
        (ELet "a" (ELiteral (LString "Hello")))

testLet2 = do
    assertParseOk letExpression "let a = (5)"
        (ELet "a" (ELiteral (LInteger 5)))

tests = TestList
    [ TestLabel "testLiterals" $ TestCase testLiterals
    , TestLabel "testLet" $ TestCase testLet
    , TestLabel "testLet2" $ TestCase testLet2
    , TestLabel "testParens" $ TestCase testParens
    ]
