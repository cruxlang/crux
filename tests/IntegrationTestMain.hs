{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import System.Environment (getArgs)
import Test.Framework

import {-@ HTF_TESTS @-} IntegrationTest

main :: IO ()
main = do
    args <- getArgs
    htfMainWithArgs ("--colors=true" : "--fail-fast" : args) htf_importedTests
