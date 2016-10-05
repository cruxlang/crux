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

test_function_taking_record = do
    env <- newEnv "main" HashMap.empty Nothing

    let numTy = TDataType $ TDataTypeDef
            { tuName = "Number"
            , tuModuleName = "number"
            , tuParameters = []
            , tuVariants = []
            }
    rect <- newIORef $ RRecord $ RecordType (RecordQuantified (RowVariable 1) Nothing) [TypeRow "x" RImmutable numTy]
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
