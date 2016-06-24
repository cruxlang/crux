{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UnifyTest (htf_thisModulesTests) where

import           Crux.AST             (Pos (..))
import           Crux.IORef
import           Crux.Typecheck.Env   (newEnv)
import           Crux.Typecheck.Monad
import           Crux.Typecheck.Unify
import           Crux.TypeVar
import qualified Data.HashMap.Strict  as HashMap
import           Test.Framework

test_function_taking_record = do
    env <- newEnv "main" HashMap.empty Nothing

    let numTy = TDataType $ TUserTypeDef
            { tuName = "Number"
            , tuModuleName = "number"
            , tuParameters = []
            , tuVariants = []
            }
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
