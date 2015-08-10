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

genDoc' :: Text -> IO (Either String [Gen.Computation])
genDoc' src = do
    let fn = "<string>"
    mod' <- Crux.Module.loadModuleFromSource "<string>" src
    case mod' of
        Left err ->
            return $ Left err
        Right m -> do
            fmap Right $ Gen.generateModule m

genDoc :: Text -> IO [Gen.Computation]
genDoc src = do
    rv <- genDoc' src
    case rv of
        Left err -> error err
        Right stmts -> return stmts

case_direct_prints = do
    doc <- genDoc "let _ = print(10);"
    assertEqual "single print expression"
        [ Gen.Computation "temp_0" $ Gen.Literal $ AST.LInteger 10
        , Gen.Computation "temp_1" $ Gen.Intrinsic (AST.IPrint ["temp_0"])
        , Gen.Computation "_" $ Gen.LetBinding "_" "temp_1"
        ]
        doc

case_return_at_top_level_is_error = do
    result <- try $! genDoc "let _ = return 1;"
    assertEqual "exception matches" (Left $ ErrorCall "Cannot return outside of functions") $ result

case_return_from_function = do
    doc <- genDoc "fun f() { return 1; };"
    assertEqual "statements"
        [ Gen.Computation "f" $ Gen.Function []
            [ Gen.Computation "temp_0" $ Gen.Literal $ AST.LInteger 1
            , Gen.Computation "temp_1" $ Gen.Return $ "temp_0"
            ]
        ]
        doc

case_return_from_branch = do
    result <- genDoc "fun f() { if True then return 1 else return 2; };"
    assertEqual "statements"
        [ Gen.Computation "f" $ Gen.Function []
            [ Gen.Computation "temp_4" (Gen.If "True"
                [ Gen.Computation "temp_0" (Gen.Literal (AST.LInteger 1))
                , Gen.Computation "temp_1" (Gen.Return "temp_0")
                ]
                [ Gen.Computation "temp_2" (Gen.Literal (AST.LInteger 2))
                , Gen.Computation "temp_3" (Gen.Return "temp_2")
                ]
            )]
        ]
        result

tests = $(testGroupGenerator)
