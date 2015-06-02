
module Main where

import qualified ParseTest
import qualified IntegrationTest
import Test.HUnit
import System.Exit (exitWith, ExitCode (..))

main :: IO Counts
main = do
    testCounts <- runTestTT $ TestList
        [ ParseTest.tests
        , IntegrationTest.tests
        ]
    case failures testCounts of
        0 -> exitWith ExitSuccess
        _ -> exitWith $ ExitFailure 1
