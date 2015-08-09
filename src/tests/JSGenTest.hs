{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module JSGenTest (tests) where

import Control.Monad (forM)
import Control.Exception (try)
import GHC.Exception (ErrorCall(..))
import Data.Monoid ((<>))
import Data.Text (Text)
import TestJesus
import qualified Crux.Lex
import qualified Crux.Parse
import Crux.JSGen (generateDocumentWithoutPrelude)
import qualified Crux.JSTree as JS
import qualified Crux.Typecheck as Typecheck
import qualified Crux.AST as AST
import qualified Crux.Module as Module

genDoc' :: Text -> IO (Either String [JS.Statement])
genDoc' src = do
    mod' <- Module.loadModuleFromSource "<string>" src
    case mod' of
        Left err ->
            return $ Left err
        Right mod -> do
            fmap Right $ generateDocumentWithoutPrelude $ mod

genDoc :: Text -> IO [JS.Statement]
genDoc src = do
    rv <- genDoc' src
    case rv of
        Left err -> error err
        Right stmts -> return stmts

case_direct_prints = do
    doc <- genDoc "let _ = print(10);"
    assertEqual "single print expression" doc
        [ JS.SVar "_" $ Just $ JS.EApplication (JS.EIdentifier "console.log") [JS.ELiteral (JS.LInteger 10)]
        ]

case_return_at_top_level_is_error = do
    result <- try $! genDoc "let _ = return 1;"
    assertEqual "exception matches" (Left $ ErrorCall "Cannot return outside of functions") $ result

case_if_on_non_boolean_is_error = do
    result <- try $! genDoc "let _ = if 1 then 2 else 3;"
    assertEqual "blah" (Left $ ErrorCall "Unification error:  TType Boolean and TType Number") $ result

case_return_from_function = do
    result <- genDoc "fun f() { return 1; };"
    assertEqual "statements" [JS.SFunction "f" [] [JS.SReturn (Just (JS.ELiteral (JS.LInteger 1)))]] result

{-
xcase_return_from_branch = do
    result <- genDoc "fun f() { if True then return 1 else return 2; };"
    assertEqual "statements"
        [ JS.SFunction "f" []
            [ JS.SVar
            ]
        ]
        result
-}

tests = $(testGroupGenerator)
