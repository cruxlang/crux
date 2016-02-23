{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, LambdaCase #-}

module UnifyTest (htf_thisModulesTests) where

import           Crux.Typecheck.Env   (newEnv)
import           Crux.Typecheck.Unify
import qualified Data.HashMap.Strict  as HashMap
import           Test.Framework
import Crux.IORef
import Crux.TypeVar
import Crux.Error
import Crux.AST (Pos(..))
import Crux.Typecheck.Monad

test_quantified_with_number = do
    let lhs = TPrimitive $ Number
    let rhs = TQuant 10
    bridgeTC (unify (Pos 0 0 0) lhs rhs) >>= \a -> case a of
        Left (TypeError UnificationError{}) -> return ()
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
    (Right ()) <- bridgeTC $ unify (Pos 0 0 0) argTypei recordLiteralType

    s <- renderTypeVarIO funTypei
    assertEqual "({x: Number}) -> {x: Number}" s
