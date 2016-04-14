{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Crux.JSBackend where

import           Crux.AST
import qualified Crux.Gen     as Gen
import qualified Crux.JSTree  as JSTree
import           Crux.Prelude
import qualified Data.Text    as Text

renderModuleName :: ModuleName -> Text
renderModuleName (ModuleName prefix name) = mconcat $ map (("$" <>) . unModuleSegment) $ prefix ++ [name]

renderTemporary :: Int -> Text
renderTemporary = Text.pack . ("$" <>). show

renderArgument :: Pattern -> Name
renderArgument = \case
    PWildcard -> "$_"
    PBinding n -> n

-- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar
jsKeywords :: [Text]
jsKeywords =
    [ "break"
    , "case"
    , "class"
    , "catch"
    , "const"
    , "continue"
    , "debugger"
    , "default"
    , "delete"
    , "do"
    , "else"
    , "export"
    , "extends"
    , "finally"
    , "for"
    , "function"
    , "if"
    , "import"
    , "in"
    , "instanceof"
    , "new"
    , "return"
    , "super"
    , "switch"
    , "this"
    , "throw"
    , "try"
    , "typeof"
    , "var"
    , "void"
    , "while"
    , "with"
    , "yield"
    -- reserved
    , "enum"
    , "implements"
    , "interface"
    , "public"
    , "private"
    , "protected"
    , "package"
    , "static"
    , "let"
    , "await"
    -- literals
    , "undefined"
    , "null"
    , "true"
    , "false"
    ]

renderJSName :: Text -> Text
renderJSName n = if elem n jsKeywords
    then n <> "$"
    else n

renderOutput :: Gen.Output -> JSTree.Expression -> JSTree.Statement
renderOutput output input = case output of
    Gen.NewLocalBinding name -> JSTree.SVar (renderJSName name) $ Just input
    Gen.ExistingLocalBinding name -> JSTree.SAssign (JSTree.EIdentifier $ renderJSName name) input
    Gen.NewTemporary i -> JSTree.SVar (renderTemporary i) $ Just input
    Gen.ExistingTemporary i -> JSTree.SAssign (JSTree.EIdentifier $ renderTemporary i) input
    Gen.OutputProperty v name -> JSTree.SAssign (JSTree.ELookup (renderValue v) name) input

renderResolvedReference' :: ResolvedReference -> Text
renderResolvedReference' = \case
    Ambient n -> renderJSName n
    Local n -> renderJSName n
    ThisModule n -> renderJSName n
    OtherModule mn n -> renderModuleName mn <> "_" <> n
    Builtin n -> n

renderResolvedReference :: ResolvedReference -> JSTree.Expression
renderResolvedReference = JSTree.EIdentifier . renderResolvedReference'

renderValue :: Gen.Value -> JSTree.Expression
renderValue value = case value of
    Gen.LocalBinding name -> JSTree.EIdentifier $ renderJSName name
    Gen.Temporary i -> JSTree.EIdentifier $ renderTemporary i
    Gen.ResolvedBinding n -> renderResolvedReference n
    Gen.Property v n -> JSTree.ELookup (renderValue v) n
    Gen.Literal lit -> case lit of
        LInteger i -> JSTree.ELiteral $ JSTree.LInteger i
        LString s -> JSTree.ELiteral $ JSTree.LString s
        LUnit -> JSTree.ELiteral $ JSTree.LUndefined
    Gen.FunctionLiteral args body -> JSTree.EFunction
        (map renderArgument args)
        (map renderInstruction body)
    Gen.ArrayLiteral elements -> JSTree.EArray $ fmap renderValue elements
    Gen.RecordLiteral props -> JSTree.EObject $ fmap renderValue props
    Gen.TagCheck value pattern -> generateMatchCond (renderValue value) pattern

renderInstruction :: Gen.Instruction -> JSTree.Statement
renderInstruction instr = case instr of
    Gen.EmptyLocalBinding name -> JSTree.SVar (renderJSName name) Nothing
    Gen.EmptyTemporary i -> JSTree.SVar (renderTemporary i) Nothing

    Gen.BindPattern input pattern ->
        let value' = renderValue input in
        JSTree.SBlock $ generateMatchVars value' pattern

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
                BAnd -> "&&"
                BOr -> "||"
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

    Gen.If cond ifTrue ifFalse ->
        JSTree.SIf
            (renderValue cond)
            (JSTree.SBlock $ map renderInstruction ifTrue)
            (Just $ JSTree.SBlock $ map renderInstruction ifFalse)

    Gen.Loop body ->
        JSTree.SWhile
            (JSTree.ELiteral JSTree.LTrue)
            (JSTree.SBlock $ map renderInstruction body)

    Gen.Throw exceptionName body ->
        JSTree.SExpression $ JSTree.EApplication
            (JSTree.ELookup (JSTree.EIdentifier $ renderResolvedReference' exceptionName <> "$") "throw")
            [renderValue body, JSTree.ENew (JSTree.EIdentifier "Error") Nothing]

    Gen.TryCatch tryInstrs exceptionName exceptionBinding catchInstrs ->
        let jsarg = renderArgument exceptionBinding in
        let jsident = JSTree.EIdentifier jsarg in
        let excident = JSTree.EIdentifier $ renderResolvedReference' exceptionName <> "$" in
        -- TODO: instanceof may not be right if we want customized tag checks later
        let check = JSTree.EApplication (JSTree.ELookup excident "check") [jsident] in
        let assign = JSTree.SAssign jsident (JSTree.ELookup jsident "message") in
        let guard = JSTree.SIf check assign (Just $ JSTree.SThrow jsident) in
        JSTree.STryCatch
            (map renderInstruction tryInstrs)
            jsarg
            (guard : map renderInstruction catchInstrs)

-- | Generate an expression which produces the boolean "true" if the variable "matchVar"
-- matches the pattern "patt"
generateMatchCond :: JSTree.Expression -> Pattern -> JSTree.Expression
generateMatchCond matchVar patt = case patt of
    PWildcard ->
        JSTree.ELiteral JSTree.LTrue
    PBinding _ ->
        JSTree.ELiteral JSTree.LTrue
    PConstructor ref subpatterns ->
        let name = getUnresolvedReferenceLeaf ref in
        let testIt = JSTree.EBinOp "=="
                (JSTree.ELiteral $ JSTree.LString name)
                (JSTree.EIndex matchVar (JSTree.ELiteral (JSTree.LInteger 0)))
            buildTestCascade acc (index, subpattern) = case subpattern of
                PWildcard -> acc
                PBinding _ -> acc
                _ -> JSTree.EBinOp "&&"
                    acc
                    (generateMatchCond (JSTree.EIndex matchVar (JSTree.ELiteral (JSTree.LInteger index))) subpattern)
        in case subpatterns of
            [] -> testIt
            _ -> JSTree.EBinOp "&&" testIt
                (foldl' buildTestCascade (JSTree.ELiteral JSTree.LTrue) (zip [1..] subpatterns))

generateMatchVars :: JSTree.Expression -> Pattern -> [JSTree.Statement]
generateMatchVars matchVar = \case
    PWildcard -> []
    PBinding name ->
        [ JSTree.SVar (renderJSName name) $ Just matchVar ]
    PConstructor _ subpatterns ->
        concat
            [ generateMatchVars (JSTree.EIndex matchVar (JSTree.ELiteral $ JSTree.LInteger index)) subPattern
            | (index, subPattern) <- zip [1..] subpatterns
            ]

renderVariant :: Variant () -> JSTree.Statement
renderVariant (Variant () vname vparameters) = case vparameters of
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

renderDeclaration :: Gen.Declaration -> [JSTree.Statement]
renderDeclaration (Gen.Declaration _export decl) = case decl of
    Gen.DData _name variants -> map renderVariant variants
    Gen.DJSData _name variants ->
        map renderJSVariant variants
    Gen.DFun name args body ->
        [JSTree.SFunction (renderJSName name) (map renderArgument args) $ map renderInstruction body]
    Gen.DLet pat defn ->
        case pat of
            PWildcard ->
                map renderInstruction defn
            PBinding _name ->
                map renderInstruction defn
    Gen.DException name ->
        [JSTree.SVar (name <> "$") $ Just $ JSTree.EApplication (JSTree.EIdentifier "_rts_new_exception") [JSTree.ELiteral $ JSTree.LString name]]

getExportedValues :: Gen.Declaration -> [Name]
getExportedValues (Gen.Declaration NoExport _) = []
getExportedValues (Gen.Declaration Export decl) = case decl of
    Gen.DData _name variants -> fmap (\(Variant () n _) -> n) variants
    Gen.DJSData _name variants -> fmap (\(JSVariant n _) -> n) variants
    Gen.DFun name _args _body -> [name]
    Gen.DLet pat _defn -> case pat of
        PWildcard -> []
        PBinding name -> [name]
    Gen.DException name -> [name <> "$"]

wrapInModule :: [JSTree.Statement] -> JSTree.Statement
wrapInModule body = JSTree.SExpression $ JSTree.iife body

generateModule :: Gen.Module -> [JSTree.Statement]
generateModule decls = concat $ map renderDeclaration decls

renderExportName :: ModuleName -> Text -> Text
renderExportName mn n = renderModuleName mn <> "_" <> n

generateJS :: Text -> Gen.Program -> Text
generateJS rtsSource modules =
    let allStatements = (flip map) modules $ \(moduleName, decls) ->
            let exportedValueNames = mconcat $ fmap getExportedValues decls
                declareExports = [JSTree.SVar (renderExportName moduleName n) Nothing | n <- exportedValueNames]
                body = generateModule decls
                setExports = [JSTree.SAssign (JSTree.EIdentifier $ renderExportName moduleName n) (JSTree.EIdentifier $ renderJSName n) | n <- exportedValueNames]
            in declareExports ++ [JSTree.SExpression $ JSTree.iife $ body ++ setExports]

    -- TODO: introduce an SRaw
    in JSTree.renderDocument [wrapInModule $ (JSTree.SExpression $ JSTree.ERaw rtsSource) : mconcat allStatements]
