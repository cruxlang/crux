{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

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
        LUnit -> JSTree.ELiteral $ JSTree.LUndefined
    Gen.FunctionLiteral args body -> JSTree.EFunction args $
        map renderInstruction body
    Gen.RecordLiteral props -> JSTree.EObject $ fmap renderValue props

renderInstruction :: Gen.Instruction -> JSTree.Statement
renderInstruction instr = case instr of
    Gen.EmptyLet name -> JSTree.SVar (renderOutput name) Nothing
    Gen.LetBinding name value -> JSTree.SVar name $ Just $ renderValue value
    Gen.Assign output value -> JSTree.SAssign (JSTree.EIdentifier $ renderOutput output) (renderValue value)
    Gen.BinIntrinsic output op lhs rhs ->
        let sym = case op of
                BIPlus     -> "+"
                BIMinus    -> "-"
                BIMultiply -> "*"
                BIDivide   -> "/"
        in JSTree.SVar (renderOutput output) $ Just $ JSTree.EBinOp sym (renderValue lhs) (renderValue rhs)
    Gen.Intrinsic output intrin ->
        JSTree.SVar (renderOutput output) $ Just $ case intrin of
            IUnsafeJs txt ->
                JSTree.ERaw txt
            IUnsafeCoerce arg -> do
                renderValue arg
            IPrint args -> do
                JSTree.EApplication
                    (JSTree.EIdentifier "console.log")
                    (map renderValue args)
            IToString arg -> do
                JSTree.EBinOp "+" (JSTree.ELiteral (JSTree.LString "")) $ renderValue arg
    Gen.Call output fn args -> JSTree.SVar (renderOutput output) $ Just $ JSTree.EApplication (renderValue fn) $ map renderValue args
    Gen.MethodCall output this methodName args ->
        JSTree.SVar (renderOutput output) $
            Just $ JSTree.EApplication
                (JSTree.ELookup (renderValue this) methodName)
                (map renderValue args)
    Gen.Lookup output value name -> JSTree.SVar (renderOutput output) $ Just $ JSTree.ELookup (renderValue value) name
    Gen.Return value -> JSTree.SReturn $ Just $ renderValue value
    Gen.Match value cases ->
        let value' = renderValue value

            genIfElse (pattern, body) um =
                JSTree.SIf
                    (generateMatchCond value' pattern)
                    (JSTree.SBlock $ generateMatchVars value' pattern <> map renderInstruction body)
                    (Just um)

            -- TODO: throw "unreachable"
            unmatched = JSTree.SBlock []
        in foldr genIfElse unmatched cases

    Gen.If cond ifTrue ifFalse ->
        JSTree.SIf
            (renderValue cond)
            (JSTree.SBlock $ map renderInstruction ifTrue)
            (Just $ JSTree.SBlock $ map renderInstruction ifFalse)

-- | Generate an expression which produces the boolean "true" if the variable "matchVar"
-- matches the pattern "patt"
generateMatchCond :: JSTree.Expression -> Pattern -> JSTree.Expression
generateMatchCond matchVar patt = case patt of
    PPlaceholder _ ->
        JSTree.ELiteral JSTree.LTrue
    PConstructor name subpatterns ->
        let testIt = JSTree.EBinOp "=="
                (JSTree.ELiteral $ JSTree.LString name)
                (JSTree.ESubscript matchVar (JSTree.ELiteral (JSTree.LInteger 0)))
            buildTestCascade acc (index, subpattern) = case subpattern of
                PPlaceholder _ -> acc
                _ -> JSTree.EBinOp "&&"
                    acc
                    (generateMatchCond (JSTree.ESubscript matchVar (JSTree.ELiteral (JSTree.LInteger index))) subpattern)
        in case subpatterns of
            [] -> testIt
            _ -> JSTree.EBinOp "&&" testIt
                (foldl' buildTestCascade (JSTree.ELiteral JSTree.LTrue) (zip [1..] subpatterns))

generateMatchVars :: JSTree.Expression -> Pattern -> [JSTree.Statement]
generateMatchVars matchVar patt = case patt of
    -- TODO: ignore _ let bindings in the IR or sugar
    PPlaceholder "_" -> []
    PPlaceholder pname ->
        [ JSTree.SVar pname $ Just matchVar ]
    PConstructor _ subpatterns ->
        concat
            [ generateMatchVars (JSTree.ESubscript matchVar (JSTree.ELiteral $ JSTree.LInteger index)) subPattern
            | (index, subPattern) <- zip [1..] subpatterns
            ]

renderVariant :: Variant -> JSTree.Statement
renderVariant Variant{..} = case vparameters of
    [] ->
        JSTree.SVar vname (Just $ JSTree.EArray [JSTree.ELiteral $ JSTree.LString vname])
    _ ->
        let argNames = [Text.pack ('a':show i) | i <- [0..(length vparameters) - 1]]
        in JSTree.SFunction vname argNames $
            [ JSTree.SReturn $ Just $ JSTree.EArray $
              [JSTree.ELiteral $ JSTree.LString vname] ++ (map JSTree.EIdentifier argNames)
            ]

renderDeclaration :: Gen.Declaration -> [JSTree.Statement]
renderDeclaration (Gen.Declaration export decl) = case decl of
    Gen.DData _name variants ->
        map renderVariant variants
        -- TODO: export data. and type??
    Gen.DFun name params body ->
        let func = JSTree.SFunction name params $ map renderInstruction body in
        func : case export of
            Export ->
                [ JSTree.SAssign (JSTree.ELookup (JSTree.EIdentifier "exports") name) $ JSTree.EIdentifier name
                ]
            NoExport -> []

    Gen.DLet name defn ->
        [JSTree.SVar name $ Just $ JSTree.iife $ map renderInstruction defn]

wrapInModule :: [JSTree.Statement] -> JSTree.Statement
wrapInModule body = JSTree.SExpression $ JSTree.iife body

generateJS :: Gen.Module -> Text
generateJS modul = do
    -- hack
    let prelude =
            [ JSTree.SVar "True" (Just $ JSTree.EIdentifier "true")
            , JSTree.SVar "False" (Just $ JSTree.EIdentifier "false")
            ]
    let statements = concat $ map renderDeclaration modul
    JSTree.renderDocument [wrapInModule $ prelude <> statements]

generateJSWithoutPrelude :: Gen.Module -> Text
generateJSWithoutPrelude modul = do
    let statements = concat $ map renderDeclaration modul
    JSTree.renderDocument $ statements
