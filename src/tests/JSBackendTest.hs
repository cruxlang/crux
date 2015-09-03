{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module JSBackendTest (tests) where

import Control.Monad (forM)
import Control.Exception (try)
import GHC.Exception (ErrorCall(..))
import Data.Monoid ((<>))
import Data.Text (Text)
import TestJesus
import qualified Sneak.AST as AST
import qualified Sneak.Lex
import qualified Sneak.Parse
import qualified Sneak.Module
import qualified Sneak.Typecheck as Typecheck
import qualified Sneak.Gen as Gen
import qualified Sneak.Backend.JS as JS

genDoc' :: Text -> IO (Either String Text)
genDoc' src = do
    let fn = "<string>"
    mod' <- Sneak.Module.loadModuleFromSource "<string>" src
    case mod' of
        Left err ->
            return $ Left err
        Right m -> do
            modul <- Gen.generateModule m
            return $ Right $ JS.generateJSWithoutPrelude modul

genDoc :: Text -> IO Text
genDoc src = do
    rv <- genDoc' src
    case rv of
        Left err -> error err
        Right stmts -> return stmts

case_direct_prints = do
    doc <- genDoc "let _ = print(10);"
    assertEqual "single print expression"
        "var _ = (function (){\nvar $0 = console.log(10);\nreturn $0;\n}\n)();\n"
        doc

case_return_from_function = do
    doc <- genDoc "fun f() { return 1; };"
    assertEqual "statements"
        "function f(){\nreturn 1;\n}\n"
        doc

case_export_function = do
    doc <- genDoc "export fun f() { 1; };"
    assertEqual "statements"
        "function f(){\nreturn 1;\n}\n(exports).f = f;\n"
        doc

case_return_from_branch = do
    result <- genDoc "fun f() { if True then return 1 else return 2; };"
    assertEqual "statements"
        "function f(){\nvar $0;\nif(True){\nreturn 1;\n}\nelse {\nreturn 2;\n}\nreturn $0;\n}\n"
        result

case_branch_with_value = do
    result <- genDoc "let x = if True then 1 else 2;"
    assertEqual "statements"
        "var x = (function (){\nvar $0;\nif(True){\n$0 = 1;\n}\nelse {\n$0 = 2;\n}\nreturn $0;\n}\n)();\n"
        result

case_jsffi_data = do
    result <- genDoc "data jsffi JST { A = undefined, B = null, C = true, D = false, E = 10, F = \"hi\", };"
    assertEqual "statements"
        "var A = (void 0);\nvar B = null;\nvar C = true;\nvar D = false;\nvar E = 10;\nvar F = \"hi\";\n"
        result

tests = $(testGroupGenerator)
