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

testLet = do
    assertParseOk letExpression [TLet, TIdentifier "a", TEqual, TString "Hello"]
        (ELet "a" (ELiteral (LString "Hello")))

tests = TestList $ map TestCase
    [ testLiterals
    , testLet
    ]
