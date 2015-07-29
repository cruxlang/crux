{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module ParseTest (tests) where

import           Crux.Parse
import           Crux.AST
import           Crux.Lex
import qualified Data.Text as T
import           Test.HUnit
import           Test.Framework
import           Test.Framework.TH
import           Test.Framework.Providers.HUnit
import qualified Text.Parsec as P

discardData expr = fmap (const ()) expr

-- assertParseOk :: (Show a, Eq a) => Parser (Expression a) -> T.Text -> (Expression a) -> IO ()
assertParseOk parser source expected f = do
    case Crux.Lex.lexSource "<>" source of
        Left err ->
            assertFailure $ "Lexer error: " ++ show err
        Right tokens -> do
            res <- P.runParserT parser () "<>" tokens
            case res of
                Right result -> assertEqual (T.unpack source) expected (f result)
                Left err -> assertFailure ("Parse failed: " ++ show err)

assertExprParses parser source expected = assertParseOk parser source expected discardData

case_literals = do
    assertExprParses literalExpression "5"
        (ELiteral () (LInteger 5))
    assertExprParses literalExpression "\"Hooper\""
        (ELiteral () (LString "Hooper"))

case_parens = do
    assertExprParses noSemiExpression "(5)"
        (ELiteral () (LInteger 5))

case_application = do
    assertExprParses expression "foo();"
        (EApp () (EIdentifier () "foo") [])

case_application_with_args = do
    assertExprParses expression "foo(bar, baz);"
        (EApp () (EIdentifier () "foo") [EIdentifier () "bar", EIdentifier () "baz"])

case_application_association = do
    assertExprParses expression "1 + length(list);"
        (EBinIntrinsic () BIPlus (ELiteral () (LInteger 1)) (EApp () (EIdentifier () "length") [EIdentifier () "list"]))

case_let = do
    assertExprParses letExpression "let a = \"Hello\""
        (ELet () NoRec "a" (ELiteral () (LString "Hello")))

case_let2 = do
    assertExprParses letExpression "let a = (5)"
        (ELet () NoRec "a" (ELiteral () (LInteger 5)))

case_pattern = do
    assertParseOk pattern "Cons a (Cons b Nil)"
        (PConstructor "Cons" [PPlaceholder "a", PConstructor "Cons" [PPlaceholder "b", PConstructor "Nil" []]]) id

case_match = do
    assertExprParses matchExpression "match hoot { Nil => hodor ; Cons a b => hoober ; }"
        (EMatch () (EIdentifier () "hoot")
            [ Case (PConstructor "Nil" []) (EIdentifier () "hodor")
            , Case (PConstructor "Cons" [PPlaceholder "a",PPlaceholder "b"]) (EIdentifier () "hoober")
            ])

case_plus = do
    assertExprParses noSemiExpression "5 + 5"
        (EBinIntrinsic () BIPlus (ELiteral () $ LInteger 5) (ELiteral () $ LInteger 5))

case_times = do
    assertExprParses multiplyExpression "8 * 8"
        (EBinIntrinsic () BIMultiply (ELiteral () $ LInteger 8) (ELiteral () $ LInteger 8))

case_polymorphic_data = do
    assertExprParses dataDeclaration "data Maybe a { Some a; None; };"
        (DData "Maybe" ["a"] [Variant {vname = "Some", vparameters = [TypeIdent "a" []]},Variant {vname = "None", vparameters = []}])

tests = $(testGroupGenerator)
