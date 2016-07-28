{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}

module TypecheckTest (htf_thisModulesTests) where

import Crux.AST
import Crux.Module.Types
import Crux.ModuleName
import Crux.Pos (Pos(..))
import qualified Crux.SymbolTable as SymbolTable
import qualified Crux.Typecheck.Env as Env
import Crux.Typecheck.Monad
import Crux.Typecheck.Types
import qualified Data.HashMap.Strict as HashMap
import Test.Framework

test_qualified_import_of_module_with_types_leaves_type_bindings_empty = do
    let aModuleName = ModuleName [] (ModuleSegment "A")
    let aModule = Module
            { mPragmas = []
            , mImports = []
            , mDecls =
                [ Declaration Export undefined (DData undefined "DT" [] [])
                ]
            }
    let thisModule = Module
            { mPragmas = []
            , mImports = [(Pos 0 0 0, QualifiedImport aModuleName (Just "A"))]
            , mDecls = []
            }
    let loadedModules = HashMap.fromList
            [ ("A", LoadedModule
                    { lmModule = aModule
                    , lmExportedValues = []
                    , lmExportedTypes = []
                    , lmExportedPatterns = []
                    , lmExportedTraits = []
                    , lmExportedExceptions = []
                    , lmKnownInstances = mempty }) ]
    (Right env) <- bridgeTC $ Env.buildTypeEnvironment "main" loadedModules thisModule

    types <- SymbolTable.readAll (eTypeBindings env)
    assertEqual [] $ HashMap.keys types
