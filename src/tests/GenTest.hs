{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module GenTest (tests) where

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

genDoc' :: Text -> IO (Either String Gen.Module)
genDoc' src = do
    let fn = "<string>"
    mod' <- Crux.Module.loadModuleFromSource "<string>" src
    case mod' of
        Left err ->
            return $ Left err
        Right m -> do
            fmap Right $ Gen.generateModule m

genDoc :: Text -> IO Gen.Module
genDoc src = do
    rv <- genDoc' src
    case rv of
        Left err -> error err
        Right stmts -> return stmts

case_direct_prints = do
    doc <- genDoc "let _ = print(10);"
    assertEqual "single print expression"
        [ Gen.Declaration AST.NoExport $ Gen.DLet "_"
            [ Gen.Intrinsic (Gen.Temporary 0) $ AST.IPrint [Gen.Literal $ AST.LInteger 10]
            , Gen.Return $ Gen.Reference (Gen.Temporary 0)
            ]
        ]
        doc

case_return_at_top_level_is_error = do
    result <- try $! genDoc "let _ = return 1;"
    assertEqual "exception matches" (Left $ ErrorCall "Cannot return outside of functions") $ result

case_return_from_function = do
    doc <- genDoc "fun f() { return 1; };"
    assertEqual "statements"
        [ Gen.Declaration AST.NoExport $ Gen.DFun "f" [] $
            [ Gen.Return $ Gen.Literal $ AST.LInteger 1
            ]
        ]
        doc

case_return_from_branch = do
    result <- genDoc "fun f() { if True then return 1 else return 2; };"
    assertEqual "statements"
        [ Gen.Declaration AST.NoExport $ Gen.DFun "f" []
            [ Gen.EmptyLet $ Gen.Temporary 0
            , Gen.If (Gen.Reference $ Gen.Binding "True")
                [ Gen.Return $ Gen.Literal $ AST.LInteger 1
                ]
                [ Gen.Return $ Gen.Literal $ AST.LInteger 2
                ]
            , Gen.Return $ Gen.Reference $ Gen.Temporary 0
            ]
        ]
        result

case_branch_with_value = do
    result <- genDoc "let x = if True then 1 else 2;"
    assertEqual "statements"
        [ Gen.Declaration AST.NoExport $ Gen.DLet "x"
            [ Gen.EmptyLet (Gen.Temporary 0)
            , Gen.If (Gen.Reference $ Gen.Binding "True")
                [ Gen.Assign (Gen.Temporary 0) $ Gen.Literal $ AST.LInteger 1
                ]
                [ Gen.Assign (Gen.Temporary 0) $ Gen.Literal $ AST.LInteger 2
                ]
            , Gen.Return $ Gen.Reference $ Gen.Temporary 0
            ]
        ]
        result

tests = $(testGroupGenerator)
