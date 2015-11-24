{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Crux.Backend.JS where

import Crux.Prelude
import           Crux.AST
import qualified Crux.Gen as Gen
import qualified Crux.JSTree as JSTree
import qualified Data.Text as Text

renderModuleName :: ModuleName -> Text
renderModuleName (ModuleName prefix name) = mconcat $ map (("$" <>) . unModuleSegment) $ prefix ++ [name]

renderTemporary :: Int -> Text
renderTemporary = Text.pack . ("$" <>). show

renderOutput :: Gen.Output -> JSTree.Expression -> JSTree.Statement
renderOutput output input = case output of
    Gen.NewLocalBinding name -> JSTree.SVar name $ Just input
    Gen.ExistingLocalBinding name -> JSTree.SAssign (JSTree.EIdentifier name) input
    Gen.NewTemporary i -> JSTree.SVar (renderTemporary i) $ Just input
    Gen.ExistingTemporary i -> JSTree.SAssign (JSTree.EIdentifier $ renderTemporary i) input
    Gen.OutputProperty v name -> JSTree.SAssign (JSTree.ELookup (renderValue v) name) input

renderResolvedReference :: ResolvedReference -> JSTree.Expression
renderResolvedReference = JSTree.EIdentifier . \case
    Local n -> n
    ThisModule n -> n
    OtherModule mn n -> renderModuleName mn <> "." <> n
    Builtin n -> n

renderValue :: Gen.Value -> JSTree.Expression
renderValue value = case value of
    Gen.LocalBinding name -> JSTree.EIdentifier name
    Gen.Temporary i -> JSTree.EIdentifier $ renderTemporary i
    Gen.ResolvedBinding n -> renderResolvedReference n
    Gen.Property v n -> JSTree.ELookup (renderValue v) n
    Gen.Literal lit -> case lit of
        LInteger i -> JSTree.ELiteral $ JSTree.LInteger i
        LString s -> JSTree.ELiteral $ JSTree.LString s
        LUnit -> JSTree.ELiteral $ JSTree.LUndefined
    Gen.FunctionLiteral args body -> JSTree.EFunction args $
        map renderInstruction body
    Gen.ArrayLiteral elements -> JSTree.EArray $ fmap renderValue elements
    Gen.RecordLiteral props -> JSTree.EObject $ fmap renderValue props

renderInstruction :: Gen.Instruction -> JSTree.Statement
renderInstruction instr = case instr of
    Gen.EmptyLocalBinding name -> JSTree.SVar name Nothing
    Gen.EmptyTemporary i -> JSTree.SVar (renderTemporary i) Nothing
    Gen.Assign output value -> renderOutput output $ renderValue value
    Gen.BinIntrinsic output op lhs rhs ->
        let sym = case op of
                BIPlus     -> "+"
                BIMinus    -> "-"
                BIMultiply -> "*"
                BIDivide   -> "/"
                BIGreater  -> ">"
                BILess     -> "<"
                BIGreaterEqual -> ">="
                BILessEqual -> "<="
                BIEqual    -> "==="
                BINotEqual -> "!=="
        in renderOutput output $ JSTree.EBinOp sym (renderValue lhs) (renderValue rhs)
    Gen.Intrinsic output intrin ->
        renderOutput output $ case intrin of
            IUnsafeJs txt ->
                JSTree.ERaw txt
            IUnsafeCoerce arg -> do
                renderValue arg
            INot arg ->
                JSTree.EPrefixOp "!" (renderValue arg)
    Gen.Call output fn args -> renderOutput output $ JSTree.EApplication (renderValue fn) $ map renderValue args
    Gen.MethodCall output this methodName args ->
        renderOutput output $ JSTree.EApplication
            (JSTree.ELookup (renderValue this) methodName)
            (map renderValue args)
    Gen.Lookup output value name -> renderOutput output $ JSTree.ELookup (renderValue value) name
    Gen.Index output arr idx -> renderOutput output $ JSTree.EIndex (renderValue arr) (renderValue idx)
    Gen.Return value -> JSTree.SReturn $ Just $ renderValue value
    Gen.Break -> JSTree.SBreak
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

    Gen.Loop body ->
        JSTree.SWhile
            (JSTree.ELiteral JSTree.LTrue)
            (JSTree.SBlock $ map renderInstruction body)

-- | Generate an expression which produces the boolean "true" if the variable "matchVar"
-- matches the pattern "patt"
generateMatchCond :: JSTree.Expression -> Pattern -> JSTree.Expression
generateMatchCond matchVar patt = case patt of
    PPlaceholder _ ->
        JSTree.ELiteral JSTree.LTrue
    PConstructor name subpatterns ->
        let testIt = JSTree.EBinOp "=="
                (JSTree.ELiteral $ JSTree.LString name)
                (JSTree.EIndex matchVar (JSTree.ELiteral (JSTree.LInteger 0)))
            buildTestCascade acc (index, subpattern) = case subpattern of
                PPlaceholder _ -> acc
                _ -> JSTree.EBinOp "&&"
                    acc
                    (generateMatchCond (JSTree.EIndex matchVar (JSTree.ELiteral (JSTree.LInteger index))) subpattern)
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
            [ generateMatchVars (JSTree.EIndex matchVar (JSTree.ELiteral $ JSTree.LInteger index)) subPattern
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

renderJSVariant :: JSVariant -> JSTree.Statement
renderJSVariant (JSVariant name value) =
    JSTree.SVar name $ Just $ JSTree.ELiteral value

renderExports :: ExportFlag -> [Name] -> [JSTree.Statement]
renderExports NoExport _ = []
renderExports Export names =
    map (\n -> JSTree.SAssign (JSTree.ELookup (JSTree.EIdentifier "exports") n) $ JSTree.EIdentifier n) names

renderDeclaration :: Gen.Declaration -> [JSTree.Statement]
renderDeclaration (Gen.Declaration export decl) = case decl of
    Gen.DData _name variants ->
        let renderedVariants = map renderVariant variants in
        let exports = renderExports export $ map (\(Variant n _) -> n) variants in
        renderedVariants ++ exports
    Gen.DJSData _name variants ->
        let renderedVariants = map renderJSVariant variants in
        let exports = renderExports export $ map (\(JSVariant n _) -> n) variants in
        renderedVariants ++ exports
    Gen.DFun name params body ->
        let func = JSTree.SFunction name params $ map renderInstruction body in
        func : renderExports export [name]
    Gen.DLet name defn ->
        let zz = JSTree.SVar name $ Just $ JSTree.iife $ map renderInstruction defn in
        zz : renderExports export [name]

wrapInModule :: [JSTree.Statement] -> JSTree.Statement
wrapInModule body = JSTree.SExpression $ JSTree.iife body

generateModule :: Gen.Module -> [JSTree.Statement]
generateModule decls = concat $ map renderDeclaration decls

generateJS :: Gen.Program -> Text
generateJS modules =
    let allStatements = (flip map) modules $ \(moduleName, decls) ->
            let body = generateModule decls
                intro = [JSTree.SVar "exports" $ Just $ JSTree.EObject mempty]
                outro = [JSTree.SReturn $ Just $ JSTree.EIdentifier "exports"]
            in JSTree.SVar (renderModuleName moduleName) $ Just $ JSTree.iife $ intro ++ body ++ outro

    in JSTree.renderDocument [wrapInModule allStatements]
