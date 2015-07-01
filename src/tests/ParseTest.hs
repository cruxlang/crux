{-# LANGUAGE OverloadedStrings #-}

module ParseTest where

import           Crux.Parse
import           Crux.AST
import           Crux.Lex
import qualified Data.Text as T
import           Test.HUnit
import qualified Text.Parsec as P
{-
assertParseOk :: (Show a, Eq a) => Parser a -> T.Text -> a -> IO ()
assertParseOk parser source expected = do
    case Crux.Lex.lexSource "<>" source of
        Left err ->
            assertFailure $ "Lexer error: " ++ show err
        Right tokens -> do
            res <- P.runParserT parser () "<>" tokens
            case res of
                Right result -> assertEqual (T.unpack source) expected result
                Left err -> assertFailure ("Parse failed: " ++ show err)

testLiterals :: IO ()
testLiterals = do
    assertParseOk literalExpression "5"
        (ELiteral () (LInteger 5))
    assertParseOk literalExpression "\"Hooper\""
        (ELiteral () (LString "Hooper"))

testParens :: IO ()
testParens = do
    assertParseOk noSemiExpression "(5)"
        (ELiteral () (LInteger 5))

testLet :: IO ()
testLet = do
    assertParseOk letExpression "let a = \"Hello\""
        (ELet () NoRec "a" (ELiteral () (LString "Hello")))

testLet2 :: IO ()
testLet2 = do
    assertParseOk letExpression "let a = (5)"
        (ELet () NoRec "a" (ELiteral () (LInteger 5)))

testPattern :: IO ()
testPattern = do
    assertParseOk pattern "Cons a (Cons b Nil)"
        (PConstructor "Cons" [PPlaceholder "a", PConstructor "Cons" [PPlaceholder "b", PConstructor "Nil" []]])

testMatch :: IO ()
testMatch = do
    assertParseOk matchExpression "match hoot { Nil => hodor ; Cons a b => hoober ; }"
        (EMatch () (EIdentifier () "hoot")
            [ Case (PConstructor "Nil" []) (EIdentifier () "hodor")
            , Case (PConstructor "Cons" [PPlaceholder "a",PPlaceholder "b"]) (EIdentifier () "hoober")
            ])

testPlus :: IO ()
testPlus = do
    assertParseOk noSemiExpression "5 + 5"
        (EBinIntrinsic () BIPlus (ELiteral () $ LInteger 5) (ELiteral () $ LInteger 5))

testTimes = do
    assertParseOk multiplyExpression "8 * 8"
        (EBinIntrinsic () BIMultiply (ELiteral () $ LInteger 8) (ELiteral () $ LInteger 8))

testPolymorphicData = do
    assertParseOk dataDeclaration "data Maybe a { Some a; None; };"
        (DData "Maybe" ["a"] [Variant {vname = "Some", vparameters = ["a"]},Variant {vname = "None", vparameters = []}])

tests :: Test
tests = TestList
    [ TestLabel "testLiterals" $ TestCase testLiterals
    , TestLabel "testLet" $ TestCase testLet
    , TestLabel "testLet2" $ TestCase testLet2
    , TestLabel "testParens" $ TestCase testParens
    , TestLabel "testPattern" $ TestCase testPattern
    , TestLabel "testMatch" $ TestCase testMatch
    , TestLabel "testPlus" $ TestCase testPlus
    , TestLabel "testTimes" $ TestCase testTimes
    , TestLabel "testPolymorphicData" $ TestCase testPolymorphicData
    ]
    -}
tests = TestList []
