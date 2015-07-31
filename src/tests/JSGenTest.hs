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

genDoc' :: Text -> IO (Either String [JS.Statement])
genDoc' src = do
    let fn = "<string>"
    let l = Crux.Lex.lexSource fn src
    case l of
        Left err ->
            return $ Left $ "Lex error: " <> show err
        Right l' -> do
            p <- Crux.Parse.parse fn l'
            case p of
                Left err ->
                    return $ Left $ "Parse error: " <> show err
                Right p' -> do
                    typetree <- Typecheck.run p'
                    typetree' <- forM typetree Typecheck.flattenDecl
                    fmap Right $ generateDocumentWithoutPrelude typetree'

genDoc :: Text -> IO [JS.Statement]
genDoc src = do
    rv <- genDoc' src
    case rv of
        Left err -> error err
        Right stmts -> return stmts

case_direct_prints = do
    doc <- genDoc "let _ = print 10;"
    assertEqual "single print expression" doc
        [ JS.SVar "_" $ Just $ JS.EApplication (JS.EIdentifier "console.log") [JS.ELiteral (JS.LInteger 10)]
        ]

case_return_at_top_level_is_error = do
    result <- try $! genDoc "let _ = return 1;"
    assertEqual "exception matches" (Left $ ErrorCall "Cannot return outside of functions") $ result

tests = $(testGroupGenerator)
