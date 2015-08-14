module Main (main) where

import qualified ParseTest
import qualified IntegrationTest
import qualified GenTest
import qualified JSBackendTest

import Test.Framework
import Test.HUnit
import System.Exit (exitWith, ExitCode (..))

main = defaultMain
        [ ParseTest.tests
        , IntegrationTest.tests
        , GenTest.tests
        , JSBackendTest.tests
        ]
