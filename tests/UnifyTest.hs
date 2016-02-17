{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UnifyTest (htf_thisModulesTests) where

import           Control.Exception    (try)
import           Crux.Typecheck.Env   (newEnv)
import           Crux.Typecheck.Types
import           Crux.Typecheck.Unify
import qualified Data.HashMap.Strict  as HashMap
import           Test.Framework
import Crux.IORef
import Crux.TypeVar

test_quantified_with_number = do
    lhs <- newTypeVar $ TPrimitive $ Number
    rhs <- newTypeVar $ TQuant 10
    try (unify lhs rhs) >>= \(a :: Either (TypeError ()) ()) ->
        case a of
            (Left UnificationError {}) -> return ()
            _ -> assertFailure ("BLAH " ++ show a)

test_function_taking_record = do
    env <- newEnv "main" HashMap.empty Nothing

    numTy <- newTypeVar $ TPrimitive $ Number
    rect <- newIORef $ RRecord $ RecordType (RecordQuantified (RowVariable 1)) [TypeRow "x" RImmutable numTy]
    argType <- newTypeVar $ TRecord $ rect
    retType <- newTypeVar $ TBound argType
    funType <- newTypeVar $ TFun [argType] retType

    funTypei <- instantiate env funType
    argTypei <- readTypeVar funTypei >>= \(TFun [a] _) ->
        return a

    rect2 <- newIORef $ RRecord $ RecordType (RecordClose) [TypeRow "x" RImmutable numTy]
    recordLiteralType <- newTypeVar $ TRecord rect2
    unify argTypei recordLiteralType

    s <- renderTypeVarIO funTypei
    assertEqual "({x: Number}) -> {x: Number}" s
