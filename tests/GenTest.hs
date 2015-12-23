{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

module GenTest (htf_thisModulesTests) where

import Control.Exception (try)
import GHC.Exception (ErrorCall(..))
import Data.Text (Text)
import Test.Framework
import qualified Crux.AST as AST
import qualified Crux.Module
import qualified Crux.Gen as Gen

genDoc' :: Text -> IO (Either String Gen.Module)
genDoc' src = do
    mod' <- Crux.Module.loadModuleFromSource "Main" "<string>" src
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

test_return_at_top_level_is_error = do
    result <- try $! genDoc "let _ = return 1"
    assertEqual (Left $ ErrorCall "Cannot return outside of functions") $ result

test_return_from_function = do
    doc <- genDoc "fun f() { return 1 }"
    assertEqual
        [ Gen.Declaration AST.NoExport $ Gen.DFun "f" [] $
            [ Gen.Return $ Gen.Literal $ AST.LInteger 1
            ]
        ]
        doc

test_return_from_branch = do
    result <- genDoc "fun f() { if True then return 1 else return 2 }"
    assertEqual
        [ Gen.Declaration AST.NoExport $ Gen.DFun "f" []
            [ Gen.EmptyTemporary 0
            , Gen.If (Gen.ResolvedBinding $ AST.OtherModule "Prelude" "True")
                [ Gen.Return $ Gen.Literal $ AST.LInteger 1
                ]
                [ Gen.Return $ Gen.Literal $ AST.LInteger 2
                ]
            , Gen.Return $ Gen.Temporary 0
            ]
        ]
        result

test_branch_with_value = do
    result <- genDoc "let x = if True then 1 else 2"
    assertEqual
        [ Gen.Declaration AST.NoExport $ Gen.DLet (AST.PBinding "x")
            [ Gen.EmptyTemporary 0
            , Gen.If (Gen.ResolvedBinding $ AST.OtherModule "Prelude" "True")
                [ Gen.Assign (Gen.ExistingTemporary 0) $ Gen.Literal $ AST.LInteger 1
                ]
                [ Gen.Assign (Gen.ExistingTemporary 0) $ Gen.Literal $ AST.LInteger 2
                ]
            , Gen.Assign (Gen.NewLocalBinding "x") (Gen.Temporary 0)
            ]
        ]
        result

test_method_call = do
    result <- genDoc "let hoop = _unsafe_js(\"we-can-put-anything-here\")\nlet _ = hoop.woop()"
    assertEqual
        [ Gen.Declaration AST.NoExport (Gen.DLet (AST.PBinding "hoop") [Gen.Intrinsic (Gen.NewTemporary 0) (AST.IUnsafeJs "we-can-put-anything-here")
            , Gen.Assign (Gen.NewLocalBinding "hoop") (Gen.Temporary 0)])
        , Gen.Declaration AST.NoExport (Gen.DLet AST.PWildcard [Gen.MethodCall (Gen.NewTemporary 1) (Gen.ResolvedBinding $ AST.ThisModule "hoop") "woop" []])
        ]
        result
