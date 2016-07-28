{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import System.Environment (getArgs)
import Test.Framework

import {-@ HTF_TESTS @-} GenTest
import {-@ HTF_TESTS @-} JSBackendTest
import {-@ HTF_TESTS @-} ParseTest
import {-@ HTF_TESTS @-} TypecheckTest
import {-@ HTF_TESTS @-} UnifyTest

main :: IO ()
main = do
    args <- getArgs
    htfMainWithArgs ("--colors=true" : "--fail-fast" : args) htf_importedTests
