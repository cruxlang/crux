{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

module TypecheckTest (htf_thisModulesTests) where

import Test.Framework
import Data.List (sort)
import Crux.AST
import Crux.Typecheck.Types
import qualified Crux.Typecheck.Env as Env
import qualified Data.HashMap.Strict as HashMap
import Crux.Prelude

test_qualified_import_of_module_with_types_leaves_type_bindings_empty = do
    let aModuleName = ModuleName [] (ModuleSegment "A")
    let aModule = Module
            { mImports = []
            , mDecls =
                [ Declaration Export undefined (DData "DT" aModuleName [] [])
                ]
            }
    let thisModule = Module
            { mImports = [QualifiedImport aModuleName "A"]
            , mDecls = []
            }
    let loadedModules = HashMap.fromList
            [ ("A", aModule) ]
    (Right env) <- Env.buildTypeEnvironment loadedModules thisModule

    types <- readIORef (eTypeBindings env)
    assertEqual ["Number", "String"] $ sort $ HashMap.keys types
