{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

module TypecheckTest (htf_thisModulesTests) where

import           Crux.AST
import           Crux.Prelude
import qualified Crux.Typecheck.Env   as Env
import           Crux.Typecheck.Monad
import           Crux.Typecheck.Types
import qualified Data.HashMap.Strict  as HashMap
import           Data.List            (sort)
import           Test.Framework

test_qualified_import_of_module_with_types_leaves_type_bindings_empty = do
    let aModuleName = ModuleName [] (ModuleSegment "A")
    let aModule = Module
            { mPragmas = []
            , mImports = []
            , mDecls =
                [ Declaration Export undefined (DData undefined "DT" aModuleName [] [])
                ]
            }
    let thisModule = Module
            { mPragmas = []
            , mImports = [(Pos 0 0 0, QualifiedImport aModuleName (Just "A"))]
            , mDecls = []
            }
    let loadedModules = HashMap.fromList
            [ ("A", aModule) ]
    (Right env) <- bridgeTC $ Env.buildTypeEnvironment "main" loadedModules thisModule

    types <- readIORef (eTypeBindings env)
    assertEqual ["Number", "String"] $ sort $ HashMap.keys types
