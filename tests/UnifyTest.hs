{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE LambdaCase, OverloadedStrings, ScopedTypeVariables #-}

module UnifyTest (htf_thisModulesTests) where

import Crux.IORef
import Crux.Pos (dummyPos)
import Crux.Typecheck.Env (newEnv)
import Crux.Typecheck.Monad
import Crux.Typecheck.TypeAlloc
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

test_free_records_unify_by_merging_fields = do
    let rowA = RecordField { trName = "a", trMut = RImmutable, trTyVar = numTy }
    let rowB = RecordField { trName = "b", trMut = RImmutable, trTyVar = numTy }

    env <- newEnv "unifytest" HashMap.empty Nothing
    lhsRecord <- freshTypeConstrained env $ ConstraintSet (Just $ RecordConstraint [rowA] Nothing) mempty
    rhsRecord <- freshTypeConstrained env $ ConstraintSet (Just $ RecordConstraint [rowB] Nothing) mempty
    Right () <- bridgeTC "unifytest" $ unify env dummyPos lhsRecord rhsRecord

    let (TypeVar lhs) = lhsRecord
    let (TypeVar rhs) = rhsRecord

    (TBound lhs') <- readIORef lhs
    (TUnbound _strength _level mergedConstraints _tn) <- readIORef rhs

    assertEqual (ConstraintSet (Just $ RecordConstraint [rowA, rowB] Nothing) mempty) mergedConstraints
    assertEqual rhsRecord lhs'

