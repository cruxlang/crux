{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import System.Environment (getArgs)

import {-@ HTF_TESTS @-} ParseTest
import {-@ HTF_TESTS @-} UnifyTest
import {-@ HTF_TESTS @-} GenTest
import {-@ HTF_TESTS @-} JSBackendTest

main :: IO ()
main = do
    args <- getArgs
    htfMainWithArgs ("--colors=true" : "--fail-fast" : args) htf_importedTests
