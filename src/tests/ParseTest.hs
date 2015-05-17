{-# LANGUAGE OverloadedStrings #-}

module ParseTest where

import Test.HUnit
import qualified Text.Parsec as P
import Crux.Parse
import Crux.Tokens
import Crux.AST

assertParseOk parser tokens expected = do
    r <- P.runParserT parser () "" tokens
    assertEqual "" (Right expected) r

testLiterals = do
    assertParseOk literalExpression [TInteger 5] (ELiteral (LInteger 5))
    assertParseOk literalExpression [TString "Hooper"] (ELiteral (LString "Hooper"))

testParens = do
    assertParseOk noSemiExpression [TOpenParen, TInteger 5, TCloseParen] (ELiteral (LInteger 5))

testLet = do
    assertParseOk letExpression [TLet, TIdentifier "a", TEqual, TString "Hello"]
        (ELet "a" (ELiteral (LString "Hello")))

testLet2 = do
    assertParseOk letExpression [TLet, TIdentifier "a", TEqual, TOpenParen, TInteger 5, TCloseParen]
        (ELet "a" (ELiteral (LInteger 5)))

tests = TestList
    [ TestLabel "testLiterals" $ TestCase testLiterals
    , TestLabel "testLet" $ TestCase testLet
    , TestLabel "testLet2" $ TestCase testLet2
    , TestLabel "testParens" $ TestCase testParens
    ]
