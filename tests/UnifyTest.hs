{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UnifyTest (htf_thisModulesTests) where

import           Control.Exception    (try)
import           Crux.AST
import           Crux.Typecheck.Env   (newEnv)
import           Crux.Typecheck.Types
import           Crux.Typecheck.Unify
import qualified Data.HashMap.Strict  as HashMap
import           Data.IORef
import           Test.Framework

test_quantified_with_number = do
    lhs <- newIORef $ TPrimitive $ Number
    rhs <- newIORef $ TQuant 10
    try (unify lhs rhs) >>= \(a :: Either (TypeError ()) ()) ->
        case a of
            (Left UnificationError {}) -> return ()
            _ -> assertFailure ("BLAH " ++ show a)

test_function_taking_record = do
    env <- newEnv HashMap.empty Nothing

    numTy <- newIORef $ TPrimitive $ Number
    argType <- newIORef $ TRecord (RecordType (RecordQuantified (RowVariable 1)) [TypeRow "x" RImmutable numTy])
    retType <- newIORef $ TBound argType
    funType <- newIORef $ TFun [argType] retType

    funTypei <- instantiate env funType
    argTypei <- readIORef funTypei >>= \(TFun [a] _) ->
        return a

    recordLiteralType <- newIORef $ TRecord (RecordType (RecordClose) [TypeRow "x" RImmutable numTy])
    unify argTypei recordLiteralType

    s <- renderTypeVarIO funTypei
    assertEqual "({x: Number}) -> {x: Number}" s
