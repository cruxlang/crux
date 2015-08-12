{-# LANGUAGE OverloadedStrings #-}

module Crux.Backend.JS where

import Crux.Prelude
import           Crux.AST
import qualified Crux.Gen as Gen
import qualified Crux.JSTree as JSTree
import qualified Data.Text as Text

renderOutput :: Gen.Output -> JSTree.Name
renderOutput (Gen.Binding name) = name
renderOutput (Gen.Temporary i) = Text.pack $ "$" <> show i

renderValue :: Gen.Value -> JSTree.Expression
renderValue value = case value of
    Gen.Reference output -> JSTree.EIdentifier $ renderOutput output
    Gen.Literal lit -> case lit of
        LInteger i -> JSTree.ELiteral $ JSTree.LInteger i
        LString s -> JSTree.ELiteral $ JSTree.LString s
        _ -> error $ "unknown literal: " <> show lit
    Gen.FunctionLiteral args body -> JSTree.EFunction args $
        map renderInstruction body

renderInstruction :: Gen.Instruction -> JSTree.Statement
renderInstruction instr = case instr of
    Gen.EmptyLet name -> JSTree.SVar (renderOutput name) Nothing
    Gen.LetBinding name value -> JSTree.SVar name $ Just $ renderValue value
    Gen.Assign output value -> JSTree.SAssign (JSTree.EIdentifier $ renderOutput output) (renderValue value)
    Gen.Intrinsic output intrin ->
        JSTree.SVar (renderOutput output) $ Just $ case intrin of
            IUnsafeJs txt ->
                JSTree.ERaw txt
            {-
            IUnsafeCoerce arg -> do
                generateExpr env arg
            -}
            IPrint args -> do
                JSTree.EApplication
                    (JSTree.EIdentifier "console.log")
                    (map renderValue args)
            {-
            IToString arg -> do
                arg' <- generateExpr env arg
                return $ JS.EBinOp "+" (JS.ELiteral (JS.LString "")) arg'
            -}
    Gen.Call output fn args -> JSTree.SVar (renderOutput output) $ Just $ JSTree.EApplication (renderValue fn) $ map renderValue args
    Gen.Lookup output value name -> JSTree.SVar (renderOutput output) $ Just $ JSTree.ELookup (renderValue value) name
    Gen.Return value -> JSTree.SReturn $ Just $ renderValue value
    Gen.If cond ifTrue ifFalse ->
        JSTree.SIf
            (renderValue cond)
            (JSTree.SBlock $ map renderInstruction ifTrue)
            (Just $ JSTree.SBlock $ map renderInstruction ifFalse)
    i -> error $ "Unknown instruction: " <> show i

generateJS :: Gen.Module -> Text
generateJS modul = do
    let statements = map renderInstruction modul
    JSTree.renderDocument statements
