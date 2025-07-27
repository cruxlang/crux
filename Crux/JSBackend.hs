{-# LANGUAGE OverloadedStrings #-}

module Crux.JSBackend where

import Crux.AST
import qualified Crux.Module.Types as AST
import Crux.ModuleName
import qualified Crux.Gen as Gen
import qualified Crux.JSTree as JSTree
import Crux.Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.ST
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)

type JSGen s = ReaderT (ModuleName, STRef s Int) (ST s)

getModuleName :: JSGen s ModuleName
getModuleName = fst <$> ask

getUniqueCounter :: JSGen s Int
getUniqueCounter = do
    counter <- snd <$> ask
    r <- lift $ readSTRef counter
    lift $ writeSTRef counter (r + 1)
    return r

getUniqueName :: JSGen s Name
getUniqueName = do
    c <- getUniqueCounter
    return $ "$_" <> Text.pack (show c)

renderModuleName :: ModuleName -> Name
renderModuleName (ModuleName prefix name) = mconcat $ map (("$" <>) . unModuleSegment) $ prefix ++ [name]

renderTemporary :: Int -> Text
renderTemporary = Text.pack . ("$" <>). show

renderArgument :: Pattern tagtype -> JSGen s (Name, [JSTree.Statement])
renderArgument = \case
    PWildcard -> do
        n <- getUniqueName
        return (n, [])
    PBinding n -> do
        return (n, [])
    PConstructor _ref _tag subpatterns -> do
        -- we've already proven this pattern is exhaustive and thus don't need
        -- to check the tag.  all we need to do is extract each subpattern.
        n <- getUniqueName
        let matchVar = JSTree.EIdentifier n
        prefixes <- for (zip [(1 :: Integer)..] subpatterns) $ \(index, subpattern) -> do
            let stmts = generateMatchVars (JSTree.EIndex matchVar (JSTree.ELiteral $ JSTree.LInteger index)) subpattern
            return stmts
        return (n, mconcat prefixes)
    PTuple _ -> do
        error "Tuples should be rewritten by this point"

buildTestCascade :: JSTree.Expression -> JSTree.Expression -> (Integer, Gen.Tag) -> JSTree.Expression
buildTestCascade matchVar acc (index, subtag) =
    let cond = generateMatchCond (JSTree.EIndex matchVar (JSTree.ELiteral (JSTree.LInteger (index + 1)))) subtag
    in JSTree.EBinOp "&&" acc cond

-- | Generate an expression which produces the boolean "true" if the variable "matchVar"
-- matches the pattern "patt"
generateMatchCond :: JSTree.Expression -> Gen.Tag -> JSTree.Expression
generateMatchCond matchVar = \case
    Gen.TagBoxedVariant name subtags -> do
        let testIt = JSTree.EBinOp "==="
                (JSTree.ELiteral $ JSTree.LString name)
                (JSTree.EIndex matchVar (JSTree.ELiteral (JSTree.LInteger 0)))
        foldl' (buildTestCascade matchVar) testIt subtags
    Gen.TagLiteral literal -> do
        JSTree.EBinOp "===" (JSTree.ELiteral literal) matchVar
    Gen.TagNonNullish subtags -> do
        -- Neither null nor undefined
        let testIt = JSTree.EBinOp "!="
                (JSTree.ELiteral JSTree.LNull)
                matchVar
        foldl' (buildTestCascade matchVar) testIt subtags
    Gen.TagNullish -> do
        -- Either null or undefined
        JSTree.EBinOp "==" (JSTree.ELiteral JSTree.LNull) matchVar

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
    PTuple _ -> error "Tuple should be rewritten by this point"

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

renderOutput :: Gen.Output -> JSTree.Expression -> JSGen s JSTree.Statement
renderOutput output input = case output of
    Gen.NewLocalBinding name -> return $ JSTree.SVar (renderJSName name) $ Just input
    Gen.ExistingLocalBinding name -> return $ JSTree.SAssign (JSTree.EIdentifier $ renderJSName name) input
    Gen.NewTemporary i -> return $ JSTree.SVar (renderTemporary i) $ Just input
    Gen.ExistingTemporary i -> return $ JSTree.SAssign (JSTree.EIdentifier $ renderTemporary i) input
    Gen.OutputProperty v name -> do
        v' <- renderValue v
        return $ JSTree.SAssign (JSTree.ELookup v' name) input

renderResolvedReference' :: ResolvedReference -> JSGen s Text
renderResolvedReference' (reftype, name) = do
    moduleName <- getModuleName
    return $ case reftype of
        Ambient -> renderJSName name
        Local -> renderJSName name
        FromModule mn
            | mn == moduleName -> renderJSName name
            | otherwise -> renderModuleName mn <> "_" <> name

renderResolvedReference :: ResolvedReference -> JSGen s JSTree.Expression
renderResolvedReference r = JSTree.EIdentifier <$> renderResolvedReference' r

renderValue :: Gen.Value -> JSGen s JSTree.Expression
renderValue val = case val of
    Gen.LocalBinding name -> return $ JSTree.EIdentifier $ renderJSName name
    Gen.Temporary i -> return $ JSTree.EIdentifier $ renderTemporary i
    Gen.ResolvedBinding n -> renderResolvedReference n
    Gen.Property v n -> do
        v' <- renderValue v
        return $ JSTree.ELookup v' n
    Gen.Literal lit -> return $ case lit of
        LInteger i -> JSTree.ELiteral $ JSTree.LInteger i
        LString s -> JSTree.ELiteral $ JSTree.LString s
        LUnit -> JSTree.ELiteral $ JSTree.LUndefined
    Gen.FunctionLiteral args body -> do
        args' <- for args renderArgument
        let argNames = fmap fst args'
        let argStatements = fmap snd args'
        body' <- for body renderInstruction
        return $ JSTree.EFunction argNames $ mconcat argStatements ++ body'
    Gen.ArrayLiteral elements -> do
        elements' <- for elements renderValue
        return $ JSTree.EArray elements'
    Gen.RecordLiteral props -> do
        props' <- for props renderValue
        return $ JSTree.EObject props'
    Gen.TagCheck value tag -> do
        value' <- renderValue value
        return $ generateMatchCond value' tag

renderInstruction :: Gen.Instruction -> JSGen s JSTree.Statement
renderInstruction = \case
    Gen.EmptyLocalBinding name -> return $ JSTree.SVar (renderJSName name) Nothing
    Gen.EmptyTemporary i -> return $ JSTree.SVar (renderTemporary i) Nothing

    Gen.BindPattern input pattern -> do
        value' <- renderValue input
        return $ JSTree.SBlock $ generateMatchVars value' pattern

    Gen.Assign output value -> do
        value' <- renderValue value
        renderOutput output value'
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
        lhs' <- renderValue lhs
        rhs' <- renderValue rhs
        renderOutput output $ JSTree.EBinOp sym lhs' rhs'
    Gen.Intrinsic output intrin -> do
        renderOutput output =<< case intrin of
            IUnsafeJs txt ->
                return $ JSTree.ERaw txt
            IUnsafeCoerce arg -> do
                renderValue arg
            INot arg -> do
                arg' <- renderValue arg
                return $ JSTree.EPrefixOp "!" arg'
    Gen.Call output fn args -> do
        fn' <- renderValue fn
        args' <- for args $ renderValue
        renderOutput output $ JSTree.EApplication fn' args'
    Gen.MethodCall output this methodName args -> do
        this' <- renderValue this
        args' <- for args renderValue
        renderOutput output $ JSTree.EApplication
            (JSTree.ELookup this' methodName)
            args'
    Gen.Lookup output value name -> do
        value' <- renderValue value
        renderOutput output $ JSTree.ELookup value' name
    Gen.Index output arr idx -> do
        arr' <- renderValue arr
        idx' <- renderValue idx
        renderOutput output $ JSTree.EIndex arr' idx'
    Gen.Return value -> do
        value' <- renderValue value
        return $ JSTree.SReturn $ Just $ value'
    Gen.Break -> return JSTree.SBreak

    Gen.If cond ifTrue ifFalse -> do
        cond' <- renderValue cond
        ifTrue' <- for ifTrue renderInstruction
        ifFalse' <- for ifFalse renderInstruction
        return $ JSTree.SIf
            (cond')
            (JSTree.SBlock $ ifTrue')
            (Just $ JSTree.SBlock $ ifFalse')

    Gen.Loop body -> do
        body' <- for body renderInstruction
        return $ JSTree.SWhile
            (JSTree.ELiteral JSTree.LTrue)
            (JSTree.SBlock body')

    Gen.Throw exceptionName body -> do
        body' <- renderValue body
        exceptionName' <- renderResolvedReference' exceptionName
        return $ JSTree.SExpression $ JSTree.EApplication
            (JSTree.ELookup (JSTree.EIdentifier $ exceptionName' <> "$") "throw")
            [body', JSTree.ENew (JSTree.EIdentifier "Error") Nothing]

    Gen.TryCatch tryInstrs catchBinding catchInstrs -> do
        case catchBinding of
            CruxException exceptionName exceptionBinding -> do
                (jsargName, jsargPrefix) <- renderArgument exceptionBinding
                let jsident = JSTree.EIdentifier jsargName
                exceptionName' <- renderResolvedReference' exceptionName
                let excident = JSTree.EIdentifier $ exceptionName' <> "$"
                -- TODO: instanceof may not be right if we want customized tag checks later
                let check = JSTree.EApplication (JSTree.ELookup excident "check") [jsident]
                let assign = JSTree.SAssign jsident (JSTree.ELookup jsident "message")
                let guard = JSTree.SIf check assign (Just $ JSTree.SThrow jsident)
                tryInstrs' <- for tryInstrs renderInstruction
                catchInstrs' <- for catchInstrs renderInstruction
                return $ JSTree.STryCatch
                    tryInstrs'
                    jsargName
                    (jsargPrefix ++ guard : catchInstrs')
            WildcardException -> do
                uniqueName <- getUniqueName
                tryInstrs' <- for tryInstrs renderInstruction
                catchInstrs' <- for catchInstrs renderInstruction
                return $ JSTree.STryCatch
                    tryInstrs'
                    uniqueName
                    catchInstrs'

renderVariant :: Variant AST.PatternTag () -> JSTree.Statement
renderVariant (Variant tag () vname vparameters) = renderTag tag
    where renderTag = \case
            AST.TagBoxedVariant name -> case vparameters of
                [] ->
                    JSTree.SVar vname (Just $ JSTree.EArray [JSTree.ELiteral $ JSTree.LString name])
                _ ->
                    let argNames = [Text.pack ('a':show i) | i <- [0..(length vparameters) - 1]]
                    in JSTree.SFunction vname argNames $
                        [ JSTree.SReturn $ Just $ JSTree.EArray $
                        [JSTree.ELiteral $ JSTree.LString name] ++ (map JSTree.EIdentifier argNames)
                        ]
            AST.TagNamedVariant name -> JSTree.SVar vname $ Just $ JSTree.ELiteral $ JSTree.LString name
            AST.TagLiteral literal -> JSTree.SVar vname $ Just $ JSTree.ELiteral literal
            AST.TagNullish -> JSTree.SVar vname $ Just $ JSTree.ELiteral $ JSTree.LNull
            AST.TagNonNullish realDefinition -> renderTag realDefinition

renderJSVariant :: JSVariant -> JSTree.Statement
renderJSVariant (JSVariant name value) =
    JSTree.SVar name $ Just $ JSTree.ELiteral value

renderDeclaration :: Gen.Declaration -> JSGen s [JSTree.Statement]
renderDeclaration (Gen.Declaration _export decl) = case decl of
    Gen.DData _name variants ->
        return $ map renderVariant variants
    Gen.DJSData _name variants ->
        return $ map renderJSVariant variants
    Gen.DFun name args body -> do
        args' <- for args renderArgument
        let argNames = fmap fst args'
        let argPrefixes = fmap snd args'
        body' <- for body renderInstruction
        return [JSTree.SFunction (renderJSName name) argNames $ mconcat argPrefixes ++ body']
    Gen.DLet pat defn -> case pat of
        PWildcard -> do
            for defn renderInstruction
        PConstructor {} -> do
            error "Gen: Top-level pattern bindings are not supported"
        PBinding _name -> do
            for defn renderInstruction
        PTuple _ -> do
            error "Gen: Tuple should be rewritten by this point"
    Gen.DException name ->
        return $ [JSTree.SVar (name <> "$") $ Just $ JSTree.EApplication (JSTree.EIdentifier "_rts_new_exception") [JSTree.ELiteral $ JSTree.LString name]]

renderDeclaration (Gen.TraitDefinition traitName defaultDecls) = do
    jsDecls <- for (HashMap.toList defaultDecls) $ \(name, (value, instructions)) -> do
        value' <- renderValue value
        instructions' <- for instructions renderInstruction
        return ((name, value'), instructions')
    let obj = JSTree.EObject $ HashMap.fromList (fmap fst jsDecls)
    let prefix = mconcat $ fmap snd jsDecls

    return $ prefix <> [JSTree.SAssign (JSTree.EIdentifier traitName) obj]

renderDeclaration (Gen.TraitInstance defaultDictName instanceName defns contextParameters) = do
    let proto = ("__proto__", JSTree.EIdentifier defaultDictName)

    defns' <- for (HashMap.toList defns) $ \(name, (value, instructions)) -> do
        value' <- renderValue value
        instructions' <- for instructions renderInstruction
        return ((name, value'), instructions')
    let obj = JSTree.EObject $ HashMap.fromList $ proto : (fmap fst defns')
    let prefix = mconcat $ fmap snd defns'
    case contextParameters of
        [] -> do
            return $ prefix <> [JSTree.SAssign (JSTree.EIdentifier instanceName) obj]
        _ -> do
            let body = prefix <> [JSTree.SReturn $ Just obj]
            return [JSTree.SAssign (JSTree.EIdentifier instanceName) $ JSTree.EFunction contextParameters body]
renderDeclaration (Gen.RecordFieldMap fieldMapName fieldValue) = do
    fieldValue' <- renderValue fieldValue
    return [JSTree.SAssign (JSTree.EIdentifier fieldMapName) fieldValue']

data ExportType = QualifiedExport | UnqualifiedExport

getExportedValues :: Gen.Declaration -> [(ExportType, Name)]
getExportedValues = \case
    Gen.Declaration NoExport _ -> []
    Gen.Declaration Export decl -> case decl of
        Gen.DData _name variants -> fmap (\(Variant _ () n _) -> (QualifiedExport, n)) variants
        Gen.DJSData _name variants -> fmap (\(JSVariant n _) -> (QualifiedExport, n)) variants
        Gen.DFun name _args _body -> [(QualifiedExport, name)]
        Gen.DLet pat _defn -> case pat of
            PWildcard -> []
            PBinding name -> [(QualifiedExport, name)]
            PConstructor {} ->
                error "Gen: Top-level pattern bindings are not supported"
            PTuple _ ->
                error "Gen: Tuples should be rewritten by this point"
        Gen.DException name -> [(QualifiedExport, name <> "$")]
    Gen.TraitDefinition name _table -> [(UnqualifiedExport, name)]
    Gen.TraitInstance _ instanceName _ _ -> [(UnqualifiedExport, instanceName)]
    Gen.RecordFieldMap fieldMapName _ -> [(UnqualifiedExport, fieldMapName)]

wrapInModule :: [JSTree.Statement] -> JSTree.Statement
wrapInModule body =
    let prefix = (JSTree.SExpression $ JSTree.ELiteral $ JSTree.LString "use strict")
    in JSTree.SExpression $ JSTree.iife $ prefix : body

generateModule' :: Gen.Module -> JSGen s [JSTree.Statement]
generateModule' decls = concat <$> for decls renderDeclaration

generateModule :: ModuleName -> Gen.Module -> [JSTree.Statement]
generateModule moduleName decls = runST $ do
    counter <- newSTRef 0
    runReaderT (generateModule' decls) (moduleName, counter)

renderExportName :: ModuleName -> (ExportType, Text) -> Text
renderExportName mn (QualifiedExport, n) = renderModuleName mn <> "_" <> n
renderExportName _ (UnqualifiedExport, n) = n

generateJS :: Text -> Gen.Program -> Text
generateJS rtsSource modules = runST $ do
    counter <- newSTRef 0
    globalExports <- newSTRef []
    allStatements <- for modules $ \(moduleName, decls) -> do
        let exportedValueNames = mconcat $ fmap getExportedValues decls
        -- The last generated module is main, so remember its exports.
        writeSTRef globalExports [(n, renderExportName moduleName n) | n <- exportedValueNames]
        let declareExports = [JSTree.SVar (renderExportName moduleName n) Nothing | n <- exportedValueNames]
        body <- runReaderT (generateModule' decls) (moduleName, counter)
        let setExports = [JSTree.SAssign (JSTree.EIdentifier $ renderExportName moduleName n) (JSTree.EIdentifier $ renderJSName $ snd n) | n <- exportedValueNames]
        return $ declareExports ++ [JSTree.SExpression $ JSTree.iife $ body ++ setExports]

    -- TODO: introduce an SRaw
    let prefix = JSTree.SExpression $ JSTree.ERaw rtsSource
    globalExports' <- readSTRef globalExports
    let suffix = [
            (JSTree.SAssign (JSTree.ELookup (JSTree.EIdentifier "_rts_exports") exportName) (JSTree.EIdentifier renderedExportName))
            | ((_exportType, exportName), renderedExportName) <- globalExports' ]

    return $ JSTree.renderDocument
        [wrapInModule $ (prefix : mconcat allStatements) <> suffix]
