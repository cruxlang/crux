{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module JSBackendTest (tests) where

import Control.Monad (forM)
import Control.Exception (try)
import GHC.Exception (ErrorCall(..))
import Data.Monoid ((<>))
import Data.Text (Text)
import TestJesus
import qualified Crux.AST as AST
import qualified Crux.Lex
import qualified Crux.Parse
import qualified Crux.Module
import qualified Crux.Typecheck as Typecheck
import qualified Crux.Gen as Gen
import qualified Crux.Backend.JS as JS

genDoc' :: Text -> IO (Either String Text)
genDoc' src = do
    let fn = "<string>"
    mod' <- Crux.Module.loadModuleFromSource "<string>" src
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
        "var $0 = console.log(10);\nvar _ = $0;\n"
        doc

case_return_from_function = do
    doc <- genDoc "fun f() { return 1; };"
    assertEqual "statements"
        "var f = (function (){\nreturn 1;\n}\n);\n"
        doc

case_return_from_branch = do
    result <- genDoc "fun f() { if True then return 1 else return 2; };"
    assertEqual "statements"
        "var f = (function (){\nvar $0;\nif(True){\nreturn 1;\n}\nelse {\nreturn 2;\n}\nreturn $0;\n}\n);\n"
        result

case_branch_with_value = do
    result <- genDoc "let x = if True then 1 else 2;"
    assertEqual "statements"
        "var $0;\nif(True){\n$0 = 1;\n}\nelse {\n$0 = 2;\n}\nvar x = $0;\n"
        result

tests = $(testGroupGenerator)
