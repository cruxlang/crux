
module Main where

import qualified ParseTest
import Test.HUnit

main = runTestTT $ TestList
    [ ParseTest.tests
    ]
