{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, ScopedTypeVariables #-}

module UnifyTest (htf_thisModulesTests) where

import Crux.IORef
import Crux.Pos (dummyPos)
import Crux.Typecheck.Env (newEnv)
import Crux.Typecheck.Monad
import Crux.Typecheck.Unify
import Crux.TypeVar
import qualified Data.HashMap.Strict as HashMap
import Test.Framework

numTy = TDataType $ TDataTypeDef
            { tuName = "Number"
            , tuModuleName = "number"
            , tuParameters = []
            , tuVariants = []
            }

test_function_taking_record = do
    env <- newEnv "unifytest" HashMap.empty Nothing

    rect <- newIORef $ RRecord $ RecordType (RecordQuantified 1 Nothing) [TypeRow "x" RImmutable numTy]
    let argType = TObject $ rect
    bref <- newIORef $ TBound argType
    let retType = TypeVar bref
    let funType = TFun [argType] retType

    funTypei <- instantiate env funType
    argTypei <- followTypeVar funTypei >>= \(TFun [a] _) ->
        return a

    rect2 <- newIORef $ RRecord $ RecordType (RecordClose) [TypeRow "x" RImmutable numTy]
    let recordLiteralType = TObject rect2
    (Right ()) <- bridgeTC $ unify env dummyPos argTypei recordLiteralType

    s <- renderTypeVarIO funTypei
    assertEqual "({x: Number}) => {x: Number}" s

test_free_records_unify_by_merging_fields = do
    let rowA = TypeRow { trName = "a", trMut = RImmutable, trTyVar = numTy }
    let rowB = TypeRow { trName = "b", trMut = RImmutable, trTyVar = numTy }

    env <- newEnv "unifytest" HashMap.empty Nothing
    lhsObject <- newIORef $ RRecord $ RecordType (RecordFree 0 Nothing) [rowA]
    rhsObject <- newIORef $ RRecord $ RecordType (RecordFree 1 Nothing) [rowB]
    let lhs = TObject $ lhsObject
    let rhs = TObject $ rhsObject
    Right () <- bridgeTC $ unify env dummyPos lhs rhs
    lhs' <- readIORef lhsObject
    RRecord rhsRecord <- readIORef rhsObject

    assertEqual (RecordType (RecordFree 0 Nothing) [rowA, rowB]) rhsRecord
    assertEqualNoShow (RBound rhsObject) lhs'