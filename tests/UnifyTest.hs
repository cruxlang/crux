{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UnifyTest (htf_thisModulesTests) where

import           Control.Exception    (try)
import           Crux.Typecheck.Env   (newEnv)
import           Crux.Typecheck.Unify
import qualified Data.HashMap.Strict  as HashMap
import           Test.Framework
import Crux.IORef
import Crux.TypeVar
import Crux.Error

test_quantified_with_number = do
    let lhs = TPrimitive $ Number
    let rhs = TQuant 10
    try (unify lhs rhs) >>= \(a :: Either (TypeError ()) ()) ->
        case a of
            (Left UnificationError {}) -> return ()
            _ -> assertFailure ("BLAH " ++ show a)

test_function_taking_record = do
    env <- newEnv "main" HashMap.empty Nothing

    let numTy = TPrimitive Number
    rect <- newIORef $ RRecord $ RecordType (RecordQuantified (RowVariable 1)) [TypeRow "x" RImmutable numTy]
    let argType = TRecord $ rect
    bref <- newIORef $ TBound argType
    let retType = TypeVar bref
    let funType = TFun [argType] retType

    funTypei <- instantiate env funType
    argTypei <- followTypeVar funTypei >>= \(TFun [a] _) ->
        return a

    rect2 <- newIORef $ RRecord $ RecordType (RecordClose) [TypeRow "x" RImmutable numTy]
    let recordLiteralType = TRecord rect2
    unify argTypei recordLiteralType

    s <- renderTypeVarIO funTypei
    assertEqual "({x: Number}) -> {x: Number}" s
