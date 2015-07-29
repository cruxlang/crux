{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module IntegrationTest (run, tests) where

import           Control.Monad  (forM)
import Control.Exception (catch, SomeException)
import qualified Crux.JSGen     as JSGen
import qualified Crux.JSTree    as JSTree
import           Crux.Lex
import           Crux.Parse
import qualified Crux.Typecheck as Typecheck
import           Data.Text      (Text)
import qualified Data.Text      as T
import qualified Data.Text.IO   as T
import           System.Process (readProcess)
import           TestJesus
-- import           Text.Show.Pretty   (ppShow)
import           Data.Monoid    ((<>))

run :: Text -> IO (Either String Text)
run src = do
    catch (run' src) $ \e -> do
        let _ = e :: SomeException
        return $ Left (show e)

run' :: Text -> IO (Either String Text)
run' src = do
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

case_hello_world = do
    result <- run $ T.unlines
        [ "let _ = print \"Hello, World!\";"
        ]
    assertEqual "" (Right "Hello, World!\n") result

case_integer = do
    result <- run $ T.unlines
        [ "let x = 1;"
        , "let y = x;"
        , "let _ = print (toString y);"
        ]
    assertEqual "" (Right "1\n") result

case_data_types = do
    result <- run $ T.unlines
        [ "data IntList {"
        , "    Element Number IntList;"
        , "    Nil;"
        , "};"
        , "let mylist = Element(1, Element(2, Nil));"
        , "let _ = print(mylist);"
        ]

    assertEqual "" (Right "[ 'Element', 1, [ 'Element', 2, [ 'Nil' ] ] ]\n") result

case_pattern_matches_can_be_expressions_that_yield_values = do
    result <- run $ T.unlines
        [ "data IntList {"
        , "    Element Number IntList;"
        , "    Nil;"
        , "};"
        , ""
        , "let list = Element(1, Element(2, Nil));"
        , "let len = match list {"
        , "    Element num Nil => 1;"
        , "    Element numOne (Element numTwo Nil) => 2;"
        , "    Nil => 0;"
        , "};"
        , "let _ = print(len);"
        ]
    assertEqual "" (Right "2\n") result

case_arithmetic = do
    result <- run $ T.unlines
        [ "let hypot_squared = fun (x, y) { x * x + y * y; };"
        , "let _ = print(hypot_squared(4, 3));"
        ]
    assertEqual "" (Right "25\n") result

case_let_is_not_recursive_by_default = do
    result <- run $ T.unlines [ "let foo = fun (x) { foo(x); };" ]
    assertEqual "" (Left "Unbound symbol (1:21,\"foo\")") result

case_recursive = do
    result <- run $ T.unlines
        [ "data IntList { Cons Number IntList; Nil; };"
        , "let rec len = fun (l) {"
        , "    match l {"
        , "        Nil => 0;"
        , "        Cons num tail => 1 + len(tail);"
        , "    };"
        , "};"
        , "let _ = print(len(Cons(5, Nil)));"
        ]
    assertEqual "" (Right "1\n") result

case_recursive_data = do
    result <- run $ T.unlines
        [ "data List a {"
        , "    Cons a (List a);"
        , "    Nil;"
        , "};"
        , ""
        , "let s = Cons(5, Cons(6, Cons(7, Nil)));"
        , ""
        , "let rec len = fun (list) {"
        , "    match list {"
        , "        Nil => 0;"
        , "        Cons x tail => 1 + len(tail);"
        , "    };"
        , "};"
        , ""
        , "let _ = print(len(s));"
        ]
    assertEqual "" (Right "3\n") result

case_row_polymorphic_records = do
    result <- run $ T.unlines
        [ "fun manhattan(p) { p.x + p.y; };"
        , ""
        , "let zero = { x: 0, y: 0 };"
        , "let myhouse = {x: 33, y: 44, z:8};"
        , ""
        , "fun main() {"
        , "    print(manhattan(zero));"
        , "    print(manhattan(myhouse));"
        , "};"
        , ""
        , "let _ = main();"
        ]

    assertEqual "" (Right "0\n77\n") result

tests = $(testGroupGenerator)
