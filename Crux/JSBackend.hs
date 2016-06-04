{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Crux.JSBackend where

import Crux.AST
import qualified Crux.Gen as Gen
import qualified Crux.JSTree as JSTree
import Crux.Prelude
import qualified Data.Text as Text

renderModuleName :: ModuleName -> Text
renderModuleName (ModuleName prefix name) = mconcat $ map (("$" <>) . unModuleSegment) $ prefix ++ [name]

renderTemporary :: Int -> Text
renderTemporary = Text.pack . ("$" <>). show

renderArgument :: Pattern tagtype -> Name
renderArgument = \case
    PWildcard -> "$_"
    PBinding n -> n
    PConstructor _ref _tag _subpatterns -> error "TODO"
    --    getUnresolvedReferenceLeaf ref <> "(" <> intercalate "," (map renderArgument subpatterns) <> ")"

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

renderOutput :: ModuleName -> Gen.Output -> JSTree.Expression -> JSTree.Statement
renderOutput moduleName output input = case output of
    Gen.NewLocalBinding name -> JSTree.SVar (renderJSName name) $ Just input
    Gen.ExistingLocalBinding name -> JSTree.SAssign (JSTree.EIdentifier $ renderJSName name) input
    Gen.NewTemporary i -> JSTree.SVar (renderTemporary i) $ Just input
    Gen.ExistingTemporary i -> JSTree.SAssign (JSTree.EIdentifier $ renderTemporary i) input
    Gen.OutputProperty v name -> JSTree.SAssign (JSTree.ELookup (renderValue moduleName v) name) input

renderResolvedReference' :: ModuleName -> ResolvedReference -> Text
renderResolvedReference' moduleName (reftype, name) = case reftype of
    Ambient -> renderJSName name
    Local -> renderJSName name
    FromModule mn
        | mn == moduleName -> renderJSName name
        | otherwise -> renderModuleName mn <> "_" <> name

renderResolvedReference :: ModuleName -> ResolvedReference -> JSTree.Expression
renderResolvedReference moduleName = JSTree.EIdentifier . renderResolvedReference' moduleName

renderValue :: ModuleName -> Gen.Value -> JSTree.Expression
renderValue moduleName val = case val of
    Gen.LocalBinding name -> JSTree.EIdentifier $ renderJSName name
    Gen.Temporary i -> JSTree.EIdentifier $ renderTemporary i
    Gen.ResolvedBinding n -> renderResolvedReference moduleName n
    Gen.Property v n -> JSTree.ELookup (renderValue moduleName v) n
    Gen.Literal lit -> case lit of
        LInteger i -> JSTree.ELiteral $ JSTree.LInteger i
        LString s -> JSTree.ELiteral $ JSTree.LString s
        LUnit -> JSTree.ELiteral $ JSTree.LUndefined
    Gen.FunctionLiteral args body -> JSTree.EFunction
        (map renderArgument args)
        (map (renderInstruction moduleName) body)
    Gen.ArrayLiteral elements -> JSTree.EArray $ fmap (renderValue moduleName) elements
    Gen.RecordLiteral props -> JSTree.EObject $ fmap (renderValue moduleName) props
    Gen.TagCheck value tag -> generateMatchCond (renderValue moduleName value) tag

renderInstruction :: ModuleName -> Gen.Instruction -> JSTree.Statement
renderInstruction moduleName instr = case instr of
    Gen.EmptyLocalBinding name -> JSTree.SVar (renderJSName name) Nothing
    Gen.EmptyTemporary i -> JSTree.SVar (renderTemporary i) Nothing

    Gen.BindPattern input pattern ->
        let value' = renderValue moduleName input in
        JSTree.SBlock $ generateMatchVars value' pattern

    Gen.Assign output value -> renderOutput moduleName output $ renderValue moduleName value
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
        in renderOutput moduleName output $ JSTree.EBinOp sym (renderValue moduleName lhs) (renderValue moduleName rhs)
    Gen.Intrinsic output intrin ->
        renderOutput moduleName output $ case intrin of
            IUnsafeJs txt ->
                JSTree.ERaw txt
            IUnsafeCoerce arg -> do
                renderValue moduleName arg
            INot arg ->
                JSTree.EPrefixOp "!" (renderValue moduleName arg)
    Gen.Call output fn args -> renderOutput moduleName output $ JSTree.EApplication (renderValue moduleName fn) $ map (renderValue moduleName) args
    Gen.MethodCall output this methodName args ->
        renderOutput moduleName output $ JSTree.EApplication
            (JSTree.ELookup (renderValue moduleName this) methodName)
            (map (renderValue moduleName) args)
    Gen.Lookup output value name -> renderOutput moduleName output $ JSTree.ELookup (renderValue moduleName value) name
    Gen.Index output arr idx -> renderOutput moduleName output $ JSTree.EIndex (renderValue moduleName arr) (renderValue moduleName idx)
    Gen.Return value -> JSTree.SReturn $ Just $ renderValue moduleName value
    Gen.Break -> JSTree.SBreak

    Gen.If cond ifTrue ifFalse ->
        JSTree.SIf
            (renderValue moduleName cond)
            (JSTree.SBlock $ map (renderInstruction moduleName) ifTrue)
            (Just $ JSTree.SBlock $ map (renderInstruction moduleName) ifFalse)

    Gen.Loop body ->
        JSTree.SWhile
            (JSTree.ELiteral JSTree.LTrue)
            (JSTree.SBlock $ map (renderInstruction moduleName) body)

    Gen.Throw exceptionName body ->
        JSTree.SExpression $ JSTree.EApplication
            (JSTree.ELookup (JSTree.EIdentifier $ renderResolvedReference' moduleName exceptionName <> "$") "throw")
            [renderValue moduleName body, JSTree.ENew (JSTree.EIdentifier "Error") Nothing]

    Gen.TryCatch tryInstrs exceptionName exceptionBinding catchInstrs ->
        let jsarg = renderArgument exceptionBinding in
        let jsident = JSTree.EIdentifier jsarg in
        let excident = JSTree.EIdentifier $ renderResolvedReference' moduleName exceptionName <> "$" in
        -- TODO: instanceof may not be right if we want customized tag checks later
        let check = JSTree.EApplication (JSTree.ELookup excident "check") [jsident] in
        let assign = JSTree.SAssign jsident (JSTree.ELookup jsident "message") in
        let guard = JSTree.SIf check assign (Just $ JSTree.SThrow jsident) in
        JSTree.STryCatch
            (map (renderInstruction moduleName) tryInstrs)
            jsarg
            (guard : map (renderInstruction moduleName) catchInstrs)

-- | Generate an expression which produces the boolean "true" if the variable "matchVar"
-- matches the pattern "patt"
generateMatchCond :: JSTree.Expression -> Gen.Tag -> JSTree.Expression
generateMatchCond matchVar = \case
    Gen.TagVariant name subtags -> do
        let testIt = JSTree.EBinOp "==="
                (JSTree.ELiteral $ JSTree.LString name)
                (JSTree.EIndex matchVar (JSTree.ELiteral (JSTree.LInteger 0)))
        let buildTestCascade acc (index, subtag) =
                let cond = generateMatchCond (JSTree.EIndex matchVar (JSTree.ELiteral (JSTree.LInteger (index + 1)))) subtag
                in JSTree.EBinOp "&&" acc cond
        foldl' buildTestCascade testIt subtags
    Gen.TagLiteral literal -> do
        JSTree.EBinOp "===" (JSTree.ELiteral literal) matchVar

generateMatchVars :: JSTree.Expression -> Pattern tagtype -> [JSTree.Statement]
generateMatchVars matchVar = \case
    PWildcard -> []
    PBinding name ->
        [ JSTree.SVar (renderJSName name) $ Just matchVar ]
    PConstructor _ _tag subpatterns ->
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

renderDeclaration :: ModuleName -> Gen.Declaration -> [JSTree.Statement]
renderDeclaration moduleName (Gen.Declaration _export decl) = case decl of
    Gen.DData _name variants -> map renderVariant variants
    Gen.DJSData _name variants ->
        map renderJSVariant variants
    Gen.DFun name args body ->
        [JSTree.SFunction (renderJSName name) (map renderArgument args) $ map (renderInstruction moduleName) body]
    Gen.DLet pat defn ->
        case pat of
            PWildcard ->
                map (renderInstruction moduleName) defn
            PConstructor {} ->
                error "Gen: Top-level pattern bindings are not supported"
            PBinding _name ->
                map (renderInstruction moduleName) defn
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
        PConstructor {} ->
            error "Gen: Top-level pattern bindings are not supported"
    Gen.DException name -> [name <> "$"]

wrapInModule :: [JSTree.Statement] -> JSTree.Statement
wrapInModule body = JSTree.SExpression $ JSTree.iife body

generateModule :: ModuleName -> Gen.Module -> [JSTree.Statement]
generateModule moduleName decls = concat $ map (renderDeclaration moduleName) decls

renderExportName :: ModuleName -> Text -> Text
renderExportName mn n = renderModuleName mn <> "_" <> n

generateJS :: Text -> Gen.Program -> Text
generateJS rtsSource modules =
    let allStatements = (flip map) modules $ \(moduleName, decls) ->
            let exportedValueNames = mconcat $ fmap getExportedValues decls
                declareExports = [JSTree.SVar (renderExportName moduleName n) Nothing | n <- exportedValueNames]
                body = generateModule moduleName decls
                setExports = [JSTree.SAssign (JSTree.EIdentifier $ renderExportName moduleName n) (JSTree.EIdentifier $ renderJSName n) | n <- exportedValueNames]
            in declareExports ++ [JSTree.SExpression $ JSTree.iife $ body ++ setExports]

    -- TODO: introduce an SRaw
    in JSTree.renderDocument [wrapInModule $ (JSTree.SExpression $ JSTree.ERaw rtsSource) : mconcat allStatements]
