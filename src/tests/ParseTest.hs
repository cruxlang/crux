{-# LANGUAGE OverloadedStrings #-}

module ParseTest where

import           Crux.Parse
import           Crux.AST
import           Crux.Lex
import qualified Data.Text as T
import           Test.HUnit
import qualified Text.Parsec as P

assertParseOk :: (Show a, Eq a) => Parser a -> T.Text -> a -> IO ()
assertParseOk parser source expected = do
    case Crux.Lex.lexSource "<>" source of
        Left err ->
            assertFailure $ "Lexer error: " ++ show err
        Right tokens -> do
            Right result <- P.runParserT parser () "<>" tokens
            assertEqual (T.unpack source) expected result

testLiterals :: IO ()
testLiterals = do
    assertParseOk literalExpression "5"
        (ELiteral (LInteger 5))
    assertParseOk literalExpression "\"Hooper\""
        (ELiteral (LString "Hooper"))

testParens :: IO ()
testParens = do
    assertParseOk noSemiExpression "(5)"
        (ELiteral (LInteger 5))

testLet :: IO ()
testLet = do
    assertParseOk letExpression "let a = \"Hello\""
        (ELet "a" (ELiteral (LString "Hello")))

testLet2 :: IO ()
testLet2 = do
    assertParseOk letExpression "let a = (5)"
        (ELet "a" (ELiteral (LInteger 5)))

tests :: Test
tests = TestList
    [ TestLabel "testLiterals" $ TestCase testLiterals
    , TestLabel "testLet" $ TestCase testLet
    , TestLabel "testLet2" $ TestCase testLet2
    , TestLabel "testParens" $ TestCase testParens
    ]
