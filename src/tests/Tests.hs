module Main (main) where

import qualified ParseTest
import qualified IntegrationTest
import qualified JSGenTest
import qualified GenTest

import Test.Framework
import Test.HUnit
import System.Exit (exitWith, ExitCode (..))

main = defaultMain
        [ ParseTest.tests
        , IntegrationTest.tests
        , JSGenTest.tests
        , GenTest.tests
        ]
