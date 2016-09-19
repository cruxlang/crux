{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module JSBackendTest (htf_thisModulesTests) where

import qualified Crux.Error as Error
import qualified Crux.Gen as Gen
import qualified Crux.JSBackend as JS
import qualified Crux.JSTree as JSTree
import qualified Crux.Module
import qualified Crux.Module.Types as MT
import Data.Text (Text)
import Test.Framework

genDoc' :: Text -> IO (Either Error.Error Text)
genDoc' src = do
    Crux.Module.loadModuleFromSource src >>= \case
        Left err ->
            return $ Left err
        Right mod' -> do
            let moduleName = "JSBackendTest"
            modul <- Gen.generateModule moduleName $ MT.lmModule mod'
            return $ Right $ JSTree.renderDocument $ JS.generateModule moduleName modul

genDoc :: Text -> IO Text
genDoc src = do
    rv <- genDoc' src
    case rv of
        Left err -> error =<< Error.renderError err
        Right stmts -> return stmts

test_direct_prints = do
    doc <- genDoc "let _ = print(10)"
    assertEqual
        "var $0 = $builtin_print(10);\n"
        doc

test_return_from_function = do
    doc <- genDoc "fun f() { return 1 }"
    assertEqual
        "function f() {\n  return 1;\n}\n"
        doc

test_export_function = do
    doc <- genDoc "export fun f() { 1 }"
    assertEqual
        "function f() {\n  return 1;\n}\n"
        doc

test_return_from_branch = do
    result <- genDoc "fun f() { if True then return 1 else return 2 }"
    assertEqual
        "function f() {\n  var $0;\n  if ($types_True) {\n    return 1;\n  }\n  else {\n    return 2;\n  }\n  return $0;\n}\n"
        result

test_branch_with_value = do
    result <- genDoc "let x = if True then 1 else 2"
    assertEqual
        "var $0;\nif ($types_True) {\n  $0 = 1;\n}\nelse {\n  $0 = 2;\n}\nvar x = $0;\n"
        result

test_jsffi_data = do
    result <- genDoc "data jsffi JST { A = undefined, B = null, C = true, D = false, E = 10, F = \"hi\", }"
    assertEqual
        "var A = (void 0);\nvar B = null;\nvar C = true;\nvar D = false;\nvar E = 10;\nvar F = \"hi\";\n"
        result
