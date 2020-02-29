{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module GenTest (htf_thisModulesTests) where

import Control.Exception (try)
import qualified Crux.AST as AST
import qualified Crux.Error as Error
import qualified Crux.Gen as Gen
import qualified Crux.Module
import qualified Crux.Module.Types
import Data.Text (Text)
import GHC.Exception (ErrorCall (..))
import Test.Framework
import Crux.TrackIO

genDoc' :: Text -> IO (Either Error.Error Gen.Module)
genDoc' src = do
    (runUntrackedIO $ Crux.Module.loadModuleFromSource src) >>= \case
        Left err ->
            return $ Left err
        Right m -> do
            fmap Right $ Gen.generateModule "GenTest" $ Crux.Module.Types.lmModule m

genDoc :: Text -> IO Gen.Module
genDoc src = do
    rv <- genDoc' src
    case rv of
        Left err -> error =<< Error.renderError err
        Right stmts -> return stmts

test_return_at_top_level_is_error = do
    result <- try $! genDoc "let _ = return ()"
    case result of
        Left (ErrorCall message) ->
            assertEqual ("Cannot return outside of functions") message
        _ ->
            assertFailure "Expected compile error"
    -- assertEqual (Left $ ErrorCall "Cannot return outside of functions") $ result

test_return_from_function = do
    doc <- genDoc "fun f() { return () }"
    assertEqual
        [ Gen.Declaration AST.NoExport $ Gen.DFun "f" [] $
            [ Gen.Return $ Gen.Literal $ AST.LUnit
            ]
        ]
        doc

test_return_from_branch = do
    result <- genDoc "fun f() { if True then return \"a\" else return \"b\" }"
    assertEqual
        [ Gen.Declaration AST.NoExport $ Gen.DFun "f" []
            [ Gen.EmptyTemporary 0
            , Gen.If (Gen.ResolvedBinding $ (AST.FromModule "types", "True"))
                [ Gen.Return $ Gen.Literal $ AST.LString "a"
                ]
                [ Gen.Return $ Gen.Literal $ AST.LString "b"
                ]
            , Gen.Return $ Gen.Temporary 0
            ]
        ]
        result

test_branch_with_value = do
    result <- genDoc "let x = if True then \"1\" else \"2\""
    assertEqual
        [ Gen.Declaration AST.NoExport $ Gen.DLet (AST.PBinding "x")
            [ Gen.EmptyTemporary 0
            , Gen.If (Gen.ResolvedBinding $ (AST.FromModule "types", "True"))
                [ Gen.Assign (Gen.ExistingTemporary 0) $ Gen.Literal $ AST.LString "1"
                ]
                [ Gen.Assign (Gen.ExistingTemporary 0) $ Gen.Literal $ AST.LString "2"
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
        , Gen.Declaration AST.NoExport (Gen.DLet AST.PWildcard [Gen.MethodCall (Gen.NewTemporary 1) (Gen.ResolvedBinding $ (AST.FromModule "main", "hoop")) "woop" []])
        ]
        result
