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
            return $ Right $ JS.generateJS modul

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
{-
xcase_return_at_top_level_is_error = do
    result <- try $! genDoc "let _ = return 1;"
    assertEqual "exception matches" (Left $ ErrorCall "Cannot return outside of functions") $ result

xcase_return_from_function = do
    doc <- genDoc "fun f() { return 1; };"
    assertEqual "statements"
        [ Gen.LetBinding "f" $ Gen.FunctionLiteral []
            [ Gen.Return $ Gen.Literal $ AST.LInteger 1
            ]
        ]
        doc

xcase_return_from_branch = do
    result <- genDoc "fun f() { if True then return 1 else return 2; };"
    assertEqual "statements"
        [ Gen.LetBinding "f" $ Gen.FunctionLiteral []
            [ Gen.EmptyLet (Gen.Temporary 0)
            , Gen.If (Gen.Reference $ Gen.Binding "True")
                [ Gen.Return $ Gen.Literal $ AST.LInteger 1
                ]
                [ Gen.Return $ Gen.Literal $ AST.LInteger 2
                ]
            ]
        ]
        result

xcase_branch_with_value = do
    result <- genDoc "let x = if True then 1 else 2;"
    assertEqual "statements"
        [ Gen.EmptyLet (Gen.Temporary 0)
        , Gen.If (Gen.Reference $ Gen.Binding "True")
            [ Gen.Assign (Gen.Temporary 0) $ Gen.Literal $ AST.LInteger 1
            ]
            [ Gen.Assign (Gen.Temporary 0) $ Gen.Literal $ AST.LInteger 2
            ]
        , Gen.LetBinding "x" $ Gen.Reference $ Gen.Temporary 0
        ]
        result
-}

tests = $(testGroupGenerator)
