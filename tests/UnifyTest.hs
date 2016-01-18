{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module UnifyTest (htf_thisModulesTests) where

import Test.Framework
import Control.Exception (try)
import Data.IORef
import Crux.AST
import Crux.Typecheck.Types
import Crux.Typecheck.Unify

test_quantified_with_number = do
    lhs <- newIORef $ TPrimitive $ Number
    rhs <- newIORef $ TQuant 10
    try (unify lhs rhs) >>= \(a :: Either (UnificationError ()) ()) ->
        case a of
            (Left UnificationError {}) -> return ()
            _ -> assertFailure ("BLAH " ++ show a)
