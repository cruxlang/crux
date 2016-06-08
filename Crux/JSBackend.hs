{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Crux.JSBackend where

import Crux.AST
import qualified Crux.Gen as Gen
import qualified Crux.JSTree as JSTree
import Crux.Prelude
import qualified Data.Text as Text
import Data.Functor.Identity (Identity, runIdentity)

type JSGen = Identity

renderModuleName :: ModuleName -> Name
renderModuleName (ModuleName prefix name) = mconcat $ map (("$" <>) . unModuleSegment) $ prefix ++ [name]

renderTemporary :: Int -> Text
renderTemporary = Text.pack . ("$" <>). show

renderArgument :: Pattern tagtype -> JSGen Name
renderArgument = \case
    PWildcard -> return $ "$_"
    PBinding n -> return $ n
    PConstructor _ref _tag _subpatterns -> fail "TODO"
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

renderOutput :: ModuleName -> Gen.Output -> JSTree.Expression -> JSGen JSTree.Statement
renderOutput moduleName output input = case output of
    Gen.NewLocalBinding name -> return $ JSTree.SVar (renderJSName name) $ Just input
    Gen.ExistingLocalBinding name -> return $ JSTree.SAssign (JSTree.EIdentifier $ renderJSName name) input
    Gen.NewTemporary i -> return $ JSTree.SVar (renderTemporary i) $ Just input
    Gen.ExistingTemporary i -> return $ JSTree.SAssign (JSTree.EIdentifier $ renderTemporary i) input
    Gen.OutputProperty v name -> do
        v' <- renderValue moduleName v
        return $ JSTree.SAssign (JSTree.ELookup v' name) input

renderResolvedReference' :: ModuleName -> ResolvedReference -> Text
renderResolvedReference' moduleName (reftype, name) = case reftype of
    Ambient -> renderJSName name
    Local -> renderJSName name
    FromModule mn
        | mn == moduleName -> renderJSName name
        | otherwise -> renderModuleName mn <> "_" <> name

renderResolvedReference :: ModuleName -> ResolvedReference -> JSTree.Expression
renderResolvedReference moduleName = JSTree.EIdentifier . renderResolvedReference' moduleName

renderValue :: ModuleName -> Gen.Value -> JSGen JSTree.Expression
renderValue moduleName val = case val of
    Gen.LocalBinding name -> return $ JSTree.EIdentifier $ renderJSName name
    Gen.Temporary i -> return $ JSTree.EIdentifier $ renderTemporary i
    Gen.ResolvedBinding n -> return $ renderResolvedReference moduleName n
    Gen.Property v n -> do
        v' <- renderValue moduleName v
        return $ JSTree.ELookup v' n
    Gen.Literal lit -> return $ case lit of
        LInteger i -> JSTree.ELiteral $ JSTree.LInteger i
        LString s -> JSTree.ELiteral $ JSTree.LString s
        LUnit -> JSTree.ELiteral $ JSTree.LUndefined
    Gen.FunctionLiteral args body -> do
        args' <- for args renderArgument
        body' <- for body $ renderInstruction moduleName
        return $ JSTree.EFunction args' body'
    Gen.ArrayLiteral elements -> do
        elements' <- for elements $ renderValue moduleName
        return $ JSTree.EArray elements'
    Gen.RecordLiteral props -> do
        props' <- for props $ renderValue moduleName
        return $ JSTree.EObject props'
    Gen.TagCheck value tag -> do
        value' <- renderValue moduleName value
        return $ generateMatchCond value' tag

renderInstruction :: ModuleName -> Gen.Instruction -> JSGen JSTree.Statement
renderInstruction moduleName = \case
    Gen.EmptyLocalBinding name -> return $ JSTree.SVar (renderJSName name) Nothing
    Gen.EmptyTemporary i -> return $ JSTree.SVar (renderTemporary i) Nothing

    Gen.BindPattern input pattern -> do
        value' <- renderValue moduleName input
        return $ JSTree.SBlock $ generateMatchVars value' pattern

    Gen.Assign output value -> do
        value' <- renderValue moduleName value
        renderOutput moduleName output value'
    Gen.BinIntrinsic output op lhs rhs -> do
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
        lhs' <- renderValue moduleName lhs
        rhs' <- renderValue moduleName rhs
        renderOutput moduleName output $ JSTree.EBinOp sym lhs' rhs'
    Gen.Intrinsic output intrin -> do
        renderOutput moduleName output =<< case intrin of
            IUnsafeJs txt ->
                return $ JSTree.ERaw txt
            IUnsafeCoerce arg -> do
                renderValue moduleName arg
            INot arg -> do
                arg' <- renderValue moduleName arg
                return $ JSTree.EPrefixOp "!" arg'
    Gen.Call output fn args -> do
        fn' <- renderValue moduleName fn
        args' <- for args $ renderValue moduleName
        renderOutput moduleName output $ JSTree.EApplication fn' args'
    Gen.MethodCall output this methodName args -> do
        this' <- renderValue moduleName this
        args' <- for args $ renderValue moduleName
        renderOutput moduleName output $ JSTree.EApplication
            (JSTree.ELookup this' methodName)
            args'
    Gen.Lookup output value name -> do
        value' <- renderValue moduleName value
        renderOutput moduleName output $ JSTree.ELookup value' name
    Gen.Index output arr idx -> do
        arr' <- renderValue moduleName arr
        idx' <- renderValue moduleName idx
        renderOutput moduleName output $ JSTree.EIndex arr' idx'
    Gen.Return value -> do
        value' <- renderValue moduleName value
        return $ JSTree.SReturn $ Just $ value'
    Gen.Break -> return JSTree.SBreak

    Gen.If cond ifTrue ifFalse -> do
        cond' <- renderValue moduleName cond
        ifTrue' <- for ifTrue $ renderInstruction moduleName
        ifFalse' <- for ifFalse $ renderInstruction moduleName
        return $ JSTree.SIf
            (cond')
            (JSTree.SBlock $ ifTrue')
            (Just $ JSTree.SBlock $ ifFalse')

    Gen.Loop body -> do
        body' <- for body $ renderInstruction moduleName
        return $ JSTree.SWhile
            (JSTree.ELiteral JSTree.LTrue)
            (JSTree.SBlock body')

    Gen.Throw exceptionName body -> do
        body' <- renderValue moduleName body
        return $ JSTree.SExpression $ JSTree.EApplication
            (JSTree.ELookup (JSTree.EIdentifier $ renderResolvedReference' moduleName exceptionName <> "$") "throw")
            [body', JSTree.ENew (JSTree.EIdentifier "Error") Nothing]

    Gen.TryCatch tryInstrs exceptionName exceptionBinding catchInstrs -> do
        jsarg <- renderArgument exceptionBinding
        let jsident = JSTree.EIdentifier jsarg
        let excident = JSTree.EIdentifier $ renderResolvedReference' moduleName exceptionName <> "$"
        -- TODO: instanceof may not be right if we want customized tag checks later
        let check = JSTree.EApplication (JSTree.ELookup excident "check") [jsident]
        let assign = JSTree.SAssign jsident (JSTree.ELookup jsident "message")
        let guard = JSTree.SIf check assign (Just $ JSTree.SThrow jsident)
        tryInstrs' <- for tryInstrs $ renderInstruction moduleName
        catchInstrs' <- for catchInstrs $ renderInstruction moduleName
        return $ JSTree.STryCatch
            tryInstrs'
            jsarg
            (guard : catchInstrs')

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

renderDeclaration :: ModuleName -> Gen.Declaration -> JSGen [JSTree.Statement]
renderDeclaration moduleName (Gen.Declaration _export decl) = case decl of
    Gen.DData _name variants ->
        return $ map renderVariant variants
    Gen.DJSData _name variants ->
        return $ map renderJSVariant variants
    Gen.DFun name args body -> do
        args' <- for args renderArgument
        body' <- for body $ renderInstruction moduleName
        return [JSTree.SFunction (renderJSName name) args' body']
    Gen.DLet pat defn -> case pat of
        PWildcard -> do
            for defn $ renderInstruction moduleName
        PConstructor {} -> do
            fail "Gen: Top-level pattern bindings are not supported"
        PBinding _name -> do
            for defn $ renderInstruction moduleName
    Gen.DException name ->
        return $ [JSTree.SVar (name <> "$") $ Just $ JSTree.EApplication (JSTree.EIdentifier "_rts_new_exception") [JSTree.ELiteral $ JSTree.LString name]]

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

generateModule' :: ModuleName -> Gen.Module -> JSGen [JSTree.Statement]
generateModule' moduleName decls = concat <$> for decls (renderDeclaration moduleName)

generateModule :: ModuleName -> Gen.Module -> [JSTree.Statement]
generateModule moduleName decls = runIdentity $ generateModule' moduleName decls

renderExportName :: ModuleName -> Text -> Text
renderExportName mn n = renderModuleName mn <> "_" <> n

generateJS' :: Text -> Gen.Program -> JSGen Text
generateJS' rtsSource modules = do
    allStatements <- for modules $ \(moduleName, decls) -> do
        let exportedValueNames = mconcat $ fmap getExportedValues decls
        let declareExports = [JSTree.SVar (renderExportName moduleName n) Nothing | n <- exportedValueNames]
        body <- generateModule' moduleName decls
        let setExports = [JSTree.SAssign (JSTree.EIdentifier $ renderExportName moduleName n) (JSTree.EIdentifier $ renderJSName n) | n <- exportedValueNames]
        return $ declareExports ++ [JSTree.SExpression $ JSTree.iife $ body ++ setExports]

    -- TODO: introduce an SRaw
    return $ JSTree.renderDocument
        [wrapInModule $ (JSTree.SExpression $ JSTree.ERaw rtsSource) : mconcat allStatements]

generateJS :: Text -> Gen.Program -> Text
generateJS rtsSource modules = runIdentity $ generateJS' rtsSource modules
