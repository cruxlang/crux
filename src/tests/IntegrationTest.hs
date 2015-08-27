{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module IntegrationTest
    -- (run, tests)
    where

import           Control.Monad  (forM)
import Control.Exception (catch, SomeException)
import qualified Crux.AST       as AST
import qualified Crux.JSTree    as JSTree
import qualified Crux.Backend.JS as JS
import qualified Crux.Gen       as Gen
import           Crux.Lex
import           Crux.Parse
import qualified Crux.Module
import qualified Crux.Typecheck as Typecheck
import           Data.Text      (Text)
import qualified Data.Text      as T
import qualified Data.Text.IO   as T
import           System.Process (readProcess)
import           TestJesus
import           Data.Monoid    ((<>))
import           System.IO (hFlush)
import           System.IO.Temp (withSystemTempFile)

run :: Text -> IO (Either String Text)
run src = do
    catch (run' src) $ \e -> do
        let _ = e :: SomeException
        return $ Left (show e)

run' :: Text -> IO (Either String Text)
run' src = do
    p <- Crux.Module.loadModuleFromSource "<string>" src
    case p of
        Left err -> do
            return $ Left err
        Right m -> do
            m' <- Gen.generateModule m
            let js = JS.generateJS m'
            withSystemTempFile "crux.js" $ \path handle -> do
                T.hPutStr handle $ js
                hFlush handle
                fmap (Right . T.pack) $ readProcess "node" [path] ""

case_hello_world = do
    result <- run $ T.unlines
        [ "let _ = print(\"Hello, World!\");"
        ]
    assertEqual "" (Right "Hello, World!\n") result

case_integer = do
    result <- run $ T.unlines
        [ "let x = 1;"
        , "let y = x;"
        , "let _ = print(toString(y));"
        ]
    assertEqual "" (Right "1\n") result

case_data_types = do
    result <- run $ T.unlines
        [ "data IntList {"
        , "    Element Number IntList,"
        , "    Nil"
        , "};"
        , "let mylist = Element(1, Element(2, Nil));"
        , "let _ = print(mylist);"
        ]

    assertEqual "" (Right "[ 'Element', 1, [ 'Element', 2, [ 'Nil' ] ] ]\n") result

case_pattern_matches_can_be_expressions_that_yield_values = do
    result <- run $ T.unlines
        [ "data IntList {"
        , "    Element Number IntList,"
        , "    Nil,"
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
    assertEqual "" result $ Left "FATAL: Unbound symbol (1:21,\"foo\")"

case_recursive = do
    result <- run $ T.unlines
        [ "data IntList { Cons Number IntList, Nil };"
        , "fun len(l) {"
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
        , "    Cons a (List a),"
        , "    Nil"
        , "};"
        , ""
        , "let s = Cons(5, Cons(6, Cons(7, Nil)));"
        , ""
        , "fun len(list) {"
        , "    match list {"
        , "        Nil => 0;"
        , "        Cons x tail => 1 + len(tail);"
        , "    };"
        , "};"
        , ""
        , "let _ = print(len(s));"
        ]
    assertEqual "" (Right "3\n") result

case_occurs_on_fun = do
    result <- run $ T.unlines
        [ "fun bad() { bad; };"
        ]

    assertEqual "" (Left "Occurs check failed") result

case_occurs_on_sum = do
    result <- run $ T.unlines
        [ "data List a { Cons a (List a), Nil };"
        , "fun bad(a) { Cons(a, a); };"
        ]

    assertEqual "" (Left "Occurs check failed") result

case_occurs_on_record = do
    result <- run $ T.unlines
        [ "fun bad(p) { { field: bad(p) }; };"
        ]

    assertEqual "" (Left "Occurs check failed") result

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

case_unsafe_js_intrinsic = do
    result <- run $ T.unlines
        [ "let c = _unsafe_js(\"console\");"
        , "let _ = c.log(\"hoop\");"
        ]
    assertEqual "" (Right "hoop\n") result

case_incorrect_unsafe_js = do
    result <- run $ T.unlines
        [ "let bad = _unsafe_js;"
        ]
    assertEqual "" (Left "Intrinsic _unsafe_js is not a value") result

case_unsafe_coerce = do
    result <- run $ T.unlines
        [ "let message = \"ohai\";"
        , "let coerced = _unsafe_coerce(message);"
        , "let _ = print(5 + coerced);"
        ]

    assertEqual "" (Right "5ohai\n") result

case_annotation_is_checked = do
    result <- run $ T.unlines
        [ "let i : Number = \"hody\";"
        ]

    assertEqual "" (Left "Unification error:  String and Number") result

case_record_annotation_is_checked = do
    result <- run $ T.unlines
        [ "let c : {log:(String) -> Unit} = _unsafe_js(\"console\");"
        , "fun main() {"
        , "    c.log(\"Hoop\");"
        , "};"
        , "let _ = main();"
        ]

    assertEqual "" (Right "Hoop\n") result

case_record_annotation_is_checked2 = do
    result <- run $ T.unlines
        [ "let c : {} = _unsafe_js(\"console\");"
        , "fun main() {"
        , "    c.log(\"Hoop\");"
        , "};"
        , "let _ = main();"
        ]

    assertEqual "" (Left "Unification error: Field 'log' not found in quantified record {} and {log: (TVar 4 Unbound 4),f...}") result

case_type_alias = do
    result <- run $ T.unlines
        [ "type Hoot = Number;"
        , "type Boast = Number;"
        , "let a : Hoot = 55;"
        , "let b : Boast = 4;"
        , "let _ = print(a + b);"
        ]
    assertEqual "" (Right "59\n") result

case_parameterized_type_alias = do
    result <- run $ T.unlines
        [ "data List a { Nil, Cons a (List a) };"
        , "type Bogo a = List a;"
        , "let hoop : Bogo Number = Cons(5, Nil);"
        ]
    assertEqual "" (Right "") result

case_if_then = do
    result <- run $ T.unlines
        [ "let _ = if True then print(\"True!\");"
        ]

    assertEqual "" (Right "True!\n") result

case_if_then_else = do
    result <- run $ T.unlines
        [ "let _ = if False then print(\"This should not run\")"
        , "        else print(\"Falso!\");"
        ]

    assertEqual "" (Right "Falso!\n") result

case_if_then_else_2 = do
    result <- run $ T.unlines
        [ "let _ = if False then if True then print(\"True!\")"
        , "        else print(\"This should not run\");"
        ]

    assertEqual "" (Right "") result

case_line_comments = do
    result <- run $ T.unlines
        [ "// A list is either Nil, the empty case, or"
        , "// it is Cons an element and another list."
        , "data List a { Nil, Cons a (List a), };"
        , ""
        , "/* TODO: Decide on an optimal name for this type alias"
        , " type Bogo = List; */"
        , "type Bogo a = List a;"
        , ""
        , "let hoop : Bogo Number = Cons(5, Nil);"
        ]
    assertEqual "" (Right "") result

case_let_mutable = do
    result <- run $ T.unlines
        [ "fun main() {"
        , "    let mutable x = 2;"
        , "    x = x + 1;"
        , "    print(x);"
        , "};"
        , "let _ = main();"
        ]

    assertEqual "" (Right "3\n") result

case_cannot_assign_to_immutable_binding = do
    result <- run $ T.unlines
        [ "fun main() {"
        , "    let x = 2;"
        , "    x = x + 1;"
        , "    print(x);"
        , "};"
        , "let _ = main();"
        ]

    assertEqual "" (Left "Not an lvar: EIdentifier (IType Number) (Local \"x\")") result

case_assign_to_mutable_record_field = do
    result <- run $ T.unlines
        [ "fun main() {"
        , "    let a : {x:Number} = {x:44};"
        , "    a.x = 22;"
        , "    print(a);"
        , "};"
        , "let _ = main();"
        ]

    assertEqual "" (Right "{ x: 22 }\n") result

case_cannot_assign_to_immutable_record_field = do
    result <- run $ T.unlines
        [ "fun main() {"
        , "    let a : {const x: Number} = {x:44};"
        , "    a.x = 22;"
        , "    print(a);"
        , "};"
        , "let _ = main();"
        ]

    assertEqual ""
        (Left "Not an lvar: ELookup (IType Number) (EIdentifier (IRecord (RecordType RecordClose [TypeRow {trName = \"x\", trMut = RImmutable, trTyVar = IType Number}])) (Local \"a\")) \"x\"")
        result

case_mutable_record_field_requirement_is_inferred = do
    result <- run $ T.unlines
        [ "fun swap(p) {"
        , "    let t = p.x;"
        , "    p.x = p.y;"
        , "    p.y = t;"
        , "};"
        , "fun main() {"
        , "    let a : {const x: Number, const y: Number} = {x:44, y:0};"
        , "    swap(a);"
        , "};"
        , "let _ = main();"
        ]

    assertEqual ""
        (Left "Could not unify mutability of record field \"x\": Record field mutability does not match")
        result

case_inferred_record_field_accepts_either_mutable_or_immutable_fields = do
    result <- run $ T.unlines
        [ "fun manhattan(p) {"
        , "    p.x + p.y;"
        , "};"
        , ""
        , "fun main() {"
        , "    let a : {const x:Number, const y:Number} = {x:44, y:0};"
        , "    print(manhattan(a));"
        , ""
        , "    let b : {mutable x:Number, mutable y:Number} = {x:0, y:0};"
        , "    print(manhattan(b));"
        , "};"
        , "let _ = main();"
        ]

    assertEqual "" (Right "44\n0\n") result

case_jsffi_data_type_names_and_values_can_be_used = do
    result <- run $ T.unlines
        [ "data jsffi Method {"
        , "    Get=\"GET\","
        , "    Post=\"POST\","
        , "};"
        , "let result : Method = Get;"
        , "let _ = print(result);"
        ]

    assertEqual "" (Right "GET\n") result

case_record_self_unification = do
    result <- run $ T.unlines
        [ "let r = {};"
        , "fun main(o) {"
        , "    if False then o else r;"
        , "};"
        , "let _ = main(r);"
        ]

    assertEqual "" (Right "") result

case_return_unifies_with_anything = do
    result <- run $ T.unlines
        [ "fun a() {"
        , "    let p ="
        , "        if True"
        , "            then return \"hody\""
        , "            else 22;"
        , "    toString(p);"
        , "};"
        ]

    assertEqual "" (Right "") result

tests = $(testGroupGenerator)
