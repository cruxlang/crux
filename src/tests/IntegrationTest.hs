{-# LANGUAGE OverloadedStrings #-}

module IntegrationTest (run, tests) where

import           Control.Monad  (forM)
import qualified Crux.JSGen     as JSGen
import qualified Crux.JSTree    as JSTree
import           Crux.Lex
import           Crux.Parse
import qualified Crux.Typecheck as Typecheck
import           Data.Text      (Text)
import qualified Data.Text      as T
import qualified Data.Text      as T
import qualified Data.Text.IO   as T
import           System.Process (readProcess)
import           Test.HUnit
-- import           Text.Show.Pretty   (ppShow)
import           Data.Monoid    ((<>))

run :: Text -> IO (Either String Text)
run src = do
    let fn = "<string>"
    let l = Crux.Lex.lexSource fn src
    case l of
        Left err ->
            return $ Left $ "Lex error: " <> show err
        Right l' -> do
            p <- Crux.Parse.parse fn l'
            case p of
                Left err -> return $ Left $ "Parse error: " ++ show err
                Right p' -> do
                    typetree <- Typecheck.run p'
                    typetree' <- forM typetree Typecheck.flattenDecl
                    js <- JSGen.generateDocument typetree'
                    T.writeFile "temp.js" $ JSTree.renderDocument js

                    fmap (Right . T.pack) $ readProcess "node" ["temp.js"] ""

testHelloWorld :: IO ()
testHelloWorld = do
    result <- run $ T.unlines
        [ "let _ = print \"Hello, World!\";"
        ]
    assertEqual "" (Right "Hello, World!\n") result

testInteger = do
    result <- run $ T.unlines
        [ "let x = 1;"
        , "let y = x;"
        , "let _ = print (toString y);"
        ]
    assertEqual "" (Right "1\n") result

testDataTypes = do
    result <- run $ T.unlines
        [ "data IntList {"
        , "    Element Number IntList;"
        , "    Nil;"
        , "};"
        , "let mylist = Element 1 (Element 2 Nil);"
        , "let _ = print mylist;"
        ]

    assertEqual "" (Right "[ 'Element', 1, [ 'Element', 2, [ 'Nil' ] ] ]\n") result

test_pattern_matches_can_be_expressions_that_yield_values = do
    result <- run $ T.unlines
        [ "data IntList {"
        , "    Element Number IntList;"
        , "    Nil;"
        , "};"
        , ""
        , "let list = Element 1 (Element 2 Nil);"
        , "let len = match list {"
        , "    Element num Nil => 1;"
        , "    Element numOne (Element numTwo Nil) => 2;"
        , "    Nil => 0;"
        , "};"
        , "let _ = print len;"
        ]
    assertEqual "" (Right "2\n") result

test_arithmetic = do
    result <- run $ T.unlines
        [ "let hypot_squared = fun x { fun y { x * x + y * y; }; };"
        , "let _ = print (square 4 3);"
        ]
    assertEqual "" (Right "25\n") result

tests :: Test
tests = TestList
    [ TestLabel "testHelloWorld" $ TestCase testHelloWorld
    , TestLabel "testInteger" $ TestCase testInteger
    , TestLabel "testDataTypes" $ TestCase testDataTypes
    , TestLabel "test_pattern_matches_can_be_expressions_that_yield_values" $ TestCase test_pattern_matches_can_be_expressions_that_yield_values
    , TestLabel "test_arithmetic" $ TestCase test_arithmetic
    ]
