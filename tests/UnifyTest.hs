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
    argType <- newTypeVar $ TRecord (RecordType (RecordQuantified (RowVariable 1)) [TypeRow "x" RImmutable numTy])
    retType <- newTypeVar $ TBound argType
    funType <- newTypeVar $ TFun [argType] retType

    funTypei <- instantiate env funType
    argTypei <- readTypeVar funTypei >>= \(TFun [a] _) ->
        return a

    recordLiteralType <- newTypeVar $ TRecord (RecordType (RecordClose) [TypeRow "x" RImmutable numTy])
    unify argTypei recordLiteralType

    s <- renderTypeVarIO funTypei
    assertEqual "({x: Number}) -> {x: Number}" s
