module Main (main) where

import qualified ParseTest
import qualified IntegrationTest
import Test.Framework
import System.Exit (exitWith, ExitCode (..))

main = defaultMain
        [ ParseTest.tests
        , IntegrationTest.tests
        ]
