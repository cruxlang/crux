{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

module ModuleTest (htf_thisModulesTests) where

import Crux.ModuleName
import Crux.Module
import Test.Framework

test_slashes_are_separators = do
    assertEqual
        (ModuleName ["foo"] "bar")
        (pathToModuleName "foo/bar.cx")
