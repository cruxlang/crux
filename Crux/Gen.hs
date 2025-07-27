{-# LANGUAGE OverloadedStrings, RecordWildCards, TupleSections #-}

module Crux.Gen
    ( Value(..)
    , Output(..)
    , Tag(..)
    , Instruction(..)
    , DeclarationType(..)
    , Declaration(..)
    , Module
    , Program
    , generateModule
    , generateProgram
    ) where

import Control.Monad.Writer.Lazy (WriterT, runWriterT, tell)
import qualified Crux.AST as AST
import qualified Crux.JSTree as JSTree
import Crux.Module (importsOf)
import Crux.ModuleName
import Crux.TypeVar
import qualified Crux.Module.Types as AST
import Crux.Prelude
import Data.Graph (graphFromEdges, topSort)
import Data.Text (pack)
import qualified Data.HashMap.Strict as HashMap

{-
Instructions can:
- introduce a new temporary
- write to an existing temporary
- introduce a new named binding
- write to an existing named binding
- write to an object property

We'll call those Outputs.

Instructions can read from:
- temporaries
- named local bindings
- named import bindings
- object properties
- literals of various types

We'll call these Values.
-}

-- zero-based index of payload that needs to be matched
type SubtagList = [(Integer, Tag)]

data Tag
    = TagBoxedVariant Name SubtagList
    | TagLiteral JSTree.Literal
    -- null or undefined
    | TagNullish
    -- neither null nor undefined
    | TagNonNullish SubtagList
    deriving (Show, Eq)

type Name = Text
data Output
    = NewLocalBinding Name
    | ExistingLocalBinding Name
    | NewTemporary Int
    | ExistingTemporary Int
    | OutputProperty Value Name
    deriving (Show, Eq)
data Value
    = LocalBinding Name
    | Temporary Int
    | ResolvedBinding AST.ResolvedReference
    | Property Value Name
    | Literal AST.Literal
    | FunctionLiteral [AST.Pattern AST.PatternTag] [Instruction]
    | ArrayLiteral [Value]
    | RecordLiteral (HashMap Name Value)
    -- This is kind of a hack. We at least know that tag checks cannot have side effects.
    -- Returns a boolean value.
    | TagCheck Value Tag
    deriving (Show, Eq)
type Input = Value

data Instruction
    -- binding
    = EmptyLocalBinding Name
    | EmptyTemporary Int
    | BindPattern Input (AST.Pattern AST.PatternTag)

    -- operations
    | Assign Output Input
    | BinIntrinsic Output (AST.BinIntrinsic) Input Input
    | Intrinsic Output (AST.Intrinsic Input)
    | Call Output Input [Input]
    | MethodCall Output Input Name [Input]
    | Lookup Output Input Name
    | Index Output Input Input

    -- control flow
    | Return Input
    | If Input [Instruction] [Instruction]
    | Loop [Instruction]
    | Break
    | Throw AST.ResolvedReference Input
    | TryCatch [Instruction] (AST.CatchBinding AST.ResolvedReference AST.PatternTag) [Instruction]
    deriving (Show, Eq)

-- TODO: make this into a record
data Env = Env
    { eModuleName :: !ModuleName
    , eCounter :: IORef Int
    }

data DeclarationType
    = DData Name [AST.Variant AST.PatternTag ()]
    | DJSData Name [AST.JSVariant]
    | DFun Name [AST.Pattern AST.PatternTag] [Instruction]
    | DLet (AST.Pattern AST.PatternTag) [Instruction]
    | DException Name
    deriving (Show, Eq)

data Declaration
    = Declaration AST.ExportFlag DeclarationType
    | TraitDefinition Name (HashMap Name (Value, [Instruction]))
    | TraitInstance Name Name (HashMap Name (Value, [Instruction])) [Name] -- defaultsDictName, dictName, bindings, contextArgs
    | RecordFieldMap Name Value
    deriving (Show, Eq)

type Program = [(ModuleName, Module)] -- topologically sorted
type Module = [Declaration]

type ASTExpr = AST.Expression AST.ResolvedReference AST.PatternTag TypeVar

newTempOutput :: MonadIO m => Env -> m Int
newTempOutput Env{..} = do
    value <- readIORef eCounter
    writeIORef eCounter (value + 1)
    return value

-- TODO: rename to GenDecl
type DeclarationWriter a = WriterT [Declaration] IO a

writeDeclaration :: Declaration -> DeclarationWriter ()
writeDeclaration d = tell [d]

-- TODO: rename to GenInst
type InstructionWriter a = WriterT [Instruction] IO a

-- TODO: add some kind of invariant error mechanism to Gen

writeInstruction :: Instruction -> InstructionWriter ()
writeInstruction i = tell [i]

newInstruction :: Env -> (Output -> Instruction) -> InstructionWriter Value
newInstruction env instr = do
    t <- newTempOutput env
    writeInstruction $ instr $ NewTemporary t
    return $ Temporary t

both :: Maybe a -> Maybe b -> Maybe (a, b)
both (Just x) (Just y) = Just (x, y)
both _ _ = Nothing

tagFromPattern :: AST.Pattern AST.PatternTag -> Maybe Tag
tagFromPattern = \case
    AST.PWildcard -> Nothing
    AST.PBinding _ -> Nothing
    AST.PConstructor _ref tag subpatterns ->
        let subtags = catMaybes $ (flip map) (zip [0..] subpatterns) $ \(i, sp) -> case tagFromPattern sp of
                Just st -> Just (i, st)
                Nothing -> Nothing
        in case tag of
            AST.TagBoxedVariant name -> do
                Just $ TagBoxedVariant name subtags
            AST.TagNamedVariant name -> do
                Just $ TagLiteral $ JSTree.LString name
            AST.TagLiteral literal ->
                Just $ TagLiteral literal
            AST.TagNullish ->
                Just $ TagNullish
            AST.TagNonNullish _ ->
                Just $ TagNonNullish subtags
    AST.PTuple _ -> fail "Should never get here"

generate :: Env -> ASTExpr -> InstructionWriter (Maybe Value)
generate env = \case
    AST.ELet _ _mut pat _ _ v -> do
        v' <- generate env v
        for v' $ \v'' -> do
            writeInstruction $ BindPattern v'' pat
            return $ Literal AST.LUnit

    AST.EFun _ AST.FunctionDecl{..} -> do
        body' <- subBlockWithReturn env fdBody
        return $ Just $ FunctionLiteral (map fst fdParams) body'

    AST.ELookup _ value propertyName -> do
        v <- generate env value
        for v $ \v' -> do
            newInstruction env $ \output -> Lookup output v' propertyName

    AST.EApp _ (AST.ELookup _ this methodName) args -> do
        this' <- generate env this
        args' <- runMaybeT $ for args $ MaybeT . generate env
        for (both this' args') $ \(this'', args'') -> do
            newInstruction env $ \output -> MethodCall output this'' methodName args''

    AST.EMethodApp {} -> do
        fail "ICE: this should never happen.  EMethodApp should be desugared into EApp by now."

    AST.ETypeLookup {} ->
        fail "ICE: this should never happen.  ETypeLookup should be resolved to EIdentifier by now"

    AST.EAs _ expr _ -> do
        generate env expr

    AST.EApp _ fn args -> do
        fn' <- generate env fn
        args' <- runMaybeT $ for args $ MaybeT . generate env
        for (both fn' args') $ \(fn'', args'') -> do
            newInstruction env $ \output -> Call output fn'' $ {-dict_params ++-} args''

    AST.EAssign _ target rhs -> do
        output <- case target of
            AST.ELookup _ lhs propName -> do
                lhs' <- generate env lhs
                for lhs' $ \lhs'' -> do
                    return $ OutputProperty lhs'' propName
            AST.EIdentifier _ (reftype, n) -> case reftype of
                AST.Ambient -> return $ Just $ ExistingLocalBinding n
                AST.Local -> return $ Just $ ExistingLocalBinding n
                AST.FromModule mn
                    | mn == eModuleName env -> return $ Just $ ExistingLocalBinding n
                    | otherwise -> fail "cannot assign to imported names"
            _ -> fail "Unsupported assignment target"

        rhs' <- generate env rhs
        for (both output rhs') $ \(output', rhs'') -> do
            writeInstruction $ Assign output' rhs''
            return $ Literal AST.LUnit

    AST.ELiteral _ lit -> do
        return $ Just $ Literal lit

    AST.EArrayLiteral _ _ elements -> do
        elements' <- runMaybeT $ for elements $ MaybeT . generate env
        return $ fmap ArrayLiteral elements'

    AST.ETupleLiteral{} -> do
        fail "Tuples should be eliminated during typechecking"

    AST.ERecordLiteral _ props -> do
        props' <- runMaybeT $ for props $ \(_, expr) -> do
            MaybeT $ generate env expr
        return $ fmap RecordLiteral props'

    AST.EIdentifier _ name -> do
        return $ Just $ ResolvedBinding name

    AST.EUnIntrinsic _ _ _ -> do
        fail "Unary intrinsics should be desugared during typechecking"

    AST.EBinIntrinsic _ bi lhs rhs -> do
        lhs' <- generate env lhs
        rhs' <- generate env rhs
        for (both lhs' rhs') $ \(lhs'', rhs'') -> do
            newInstruction env $ \output -> BinIntrinsic output bi lhs'' rhs''

    AST.EIntrinsic _ iid -> do
        iid' <- runMaybeT $ traverse (MaybeT . generate env) iid
        for iid' $ \iid'' -> do
            newInstruction env $ \output -> Intrinsic output iid''

    AST.ESemi _ lhs rhs -> do
        _ <- generate env lhs
        generate env rhs

    AST.EMatch _ value cases -> do
        value' <- generate env value
        for value' $ \value'' -> do
            output <- newTempOutput env

            writeInstruction $ EmptyTemporary output
            cases' <- for cases $ \(AST.Case pat expr') -> do
                expr'' <- subBlockWithOutput env (ExistingTemporary output) expr'
                return (pat, expr'')

            -- TODO: Here, we must carry type information forward:
            -- Matching on jsffi constructors requires different logic.
            let genIfElse (pattern, bodyInstructions) um =
                    case tagFromPattern pattern of
                        Just tag -> [
                            If
                                (TagCheck value'' tag)
                                (BindPattern value'' pattern : bodyInstructions)
                                um]
                        -- irrefutable
                        Nothing -> BindPattern value'' pattern : bodyInstructions

            -- TODO: throw "unreachable"
            traverse_ writeInstruction $ foldr genIfElse [] cases'
            return $ Temporary output

    AST.EIfThenElse _ cond ifTrue ifFalse -> do
        cond' <- generate env cond
        output <- newTempOutput env
        writeInstruction $ EmptyTemporary output
        ifTrue' <- subBlockWithOutput env (ExistingTemporary output) ifTrue
        ifFalse' <- subBlockWithOutput env (ExistingTemporary output) ifFalse
        for cond' $ \cond'' -> do
            writeInstruction $ If cond'' ifTrue' ifFalse'
            return $ Temporary output

    AST.EWhile _ cond body -> do
        let boolType = AST.edata cond
        let unitType = AST.edata body
        body' <- subBlock env $
            AST.ESemi unitType
                (AST.EIfThenElse unitType
                    (AST.EIntrinsic boolType $ AST.INot $ cond)
                    (AST.EBreak unitType)
                    (AST.ELiteral unitType $ AST.LUnit))
                body
        writeInstruction $ Loop body'
        return $ Just $ Literal AST.LUnit

    AST.EFor _ pat over body -> do
        over' <- generate env over
        for over' $ \over'' -> do
            cmpVar <- newTempOutput env
            loopVar <- newTempOutput env
            patVar <- newTempOutput env

            writeInstruction $ Assign (NewTemporary loopVar) $ Literal $ AST.LInteger 0
            lengthVar <- newInstruction env $ \output -> Lookup output over'' "length"

            body' <- subBlock env body
            let loopHead =
                    [ BinIntrinsic (NewTemporary cmpVar) AST.BILess (Temporary loopVar) lengthVar
                    , If (Temporary cmpVar) [] [Break]
                    , Index (NewTemporary patVar) over'' $ Temporary loopVar
                    , BindPattern (Temporary patVar) pat
                    ]
            let loopTail =
                    [ BinIntrinsic (ExistingTemporary loopVar) AST.BIPlus (Temporary loopVar) $ Literal $ AST.LInteger 1
                    ]
            let body'' = loopHead ++ body' ++ loopTail

            writeInstruction $ Loop body''
            return $ Literal AST.LUnit

    AST.EReturn _ rv -> do
        rv' <- generate env rv
        for_ rv' $ \rv'' -> do
            writeInstruction $ Return rv''
        return Nothing

    AST.EBreak _ -> do
        writeInstruction $ Break
        return Nothing

    AST.EThrow _ exceptionName value -> do
        value' <- generate env value
        for_ value' $ \value'' -> do
            writeInstruction $ Throw exceptionName value''
        return Nothing

    AST.ETryCatch _ tryBody catchBinding catchBody -> do
        output <- newTempOutput env
        writeInstruction $ EmptyTemporary output
        tryBody' <- subBlockWithOutput env (ExistingTemporary output) tryBody
        catchBody' <- subBlockWithOutput env (ExistingTemporary output) catchBody
        writeInstruction $ TryCatch tryBody' catchBinding catchBody'
        return $ Just $ Temporary output

    AST.EInstancePlaceholder tv _pos p -> do
        fail $ "Placeholders should not make it to code generation: " ++ show tv ++ " -- " ++ show p
    AST.EInstanceDict _ traitIdentity dti -> do
        return $ Just $ LocalBinding $ instanceDictName traitIdentity dti
    AST.EInstanceArgument _ name -> do
        return $ Just $ LocalBinding name
    AST.EInstanceFieldMap _ traitIdentity -> do
        return $ Just $ LocalBinding $ instanceFieldMapName traitIdentity

subBlock :: MonadIO m => Env -> ASTExpr -> m [Instruction]
subBlock env expr = do
    (_output, instructions) <- liftIO $ runWriterT $ generate env expr
    return instructions

subBlock' :: MonadIO m => Env -> ASTExpr -> m (Maybe Value, [Instruction])
subBlock' env expr = liftIO $ runWriterT $ generate env expr

subBlockWithReturn :: MonadIO m => Env -> ASTExpr -> m [Instruction]
subBlockWithReturn env expr = do
    (output, instrs) <- liftIO $ runWriterT $ generate env expr
    return $ case output of
        Just output' -> instrs ++ [Return output']
        Nothing -> instrs

subBlockWithOutput :: MonadIO m => Env -> Output -> ASTExpr -> m [Instruction]
subBlockWithOutput env output expr = do
    (output', instrs) <- liftIO $ runWriterT $ generate env expr
    return $ case output' of
        Just output'' -> instrs ++ [Assign output output'']
        Nothing -> instrs

-- this should really be calculated in the js backend
renderModuleName :: ModuleName -> Name
renderModuleName (ModuleName prefix name) = mconcat $ map (("$" <>) . unModuleSegment) $ prefix ++ [name]

defaultsDictName :: AST.ResolvedReference -> Text
defaultsDictName (AST.FromModule traitModule, traitName) =
    "$$default$" <> traitName <> "$$" <> renderModuleName traitModule
defaultsDictName _ =
    -- This is pretty bad.
    error "ICE: defaultsDictName should only be passed a FromModule reference"

instanceDictName :: TraitIdentity -> TraitImplIdentity -> Text
instanceDictName (TraitIdentity traitModule traitName) = \case
    DataIdentity typeName typeModule ->
        "$$data$" <> typeName <> "$" <> traitName <> "$$" <> renderModuleName typeModule <> "$$" <> renderModuleName traitModule
    FunctionIdentity arity ->
        "$$function$" <> (Data.Text.pack $ show arity)
    RecordIdentity ->
        "$$record$" <> traitName <> "$" <> renderModuleName traitModule

instanceFieldMapName :: TraitIdentity -> Text
instanceFieldMapName (TraitIdentity traitModule traitName) =
    "$$fieldMap$" <> traitName <> "$" <> renderModuleName traitModule

traitDictName :: (MonadIO m, MonadFail m) => AST.ResolvedReference -> TypeVar -> m Name
traitDictName traitName' typeVar = do
    let (AST.FromModule traitModule, traitName) = traitName'
    dti <- getDTI typeVar
    return $ instanceDictName (TraitIdentity traitModule traitName) dti
  where
    getDTI :: (MonadIO m, MonadFail m) => TypeVar -> m TraitImplIdentity
    getDTI tv = followTypeVar tv >>= \case
        TypeVar {} -> do
            fail "Unexpected traitDictName got unbound typevar"
        TDataType def -> do
            return $ dataTypeIdentity def
        TRecord fields -> do
            fail $ "TODO: compute dictname from rows: " <> show (fmap trName fields)
        tv2 -> do
            s <- renderTypeVarIO tv2
            fail $ "Unexpected traitDictName " ++ s

generateDecl :: Env -> AST.Declaration AST.ResolvedReference AST.PatternTag TypeVar -> DeclarationWriter ()
generateDecl env (AST.Declaration export _pos decl) = case decl of
    AST.DExportImport _ _ -> do
        return ()

    AST.DDeclare _ _ _ _ -> do
        -- declarations are not reflected into the IR
        return ()
    AST.DData _ name _ variants -> do
        writeDeclaration $ Declaration export $ DData name $ fmap (fmap $ const ()) variants
    AST.DJSData _ name variants -> do
        writeDeclaration $ Declaration export $ DJSData name variants

    AST.DFun _ name _ funDecl -> do -- name params _retAnn body -> do
        let AST.FunctionDecl{..} = funDecl
        body' <- subBlockWithReturn env fdBody
        writeDeclaration $ Declaration export $ DFun name (map fst fdParams) body'
    AST.DLet _ _mut pat _ _ defn -> do
        defn' <- case pat of
            AST.PWildcard -> subBlock env defn
            AST.PBinding name -> subBlockWithOutput env (NewLocalBinding name) defn
            AST.PConstructor {} -> error "Gen: Top-level pattern matches are not yet supported"
            AST.PTuple _ -> error "Should never get here"
        writeDeclaration $ Declaration export $ DLet pat defn'
    AST.DTypeAlias {} -> do
        -- type aliases are not reflected into the IR
        return ()

    AST.DTrait _tv traitName decls -> do
        let ddn = defaultsDictName (AST.FromModule (eModuleName env), traitName)

        let hasDefaults =
                [ (name, typeVar, typeIdent, expr)
                | (name, typeVar, typeIdent, Just expr) <- decls
                ]

        defaultDecls <- fmap HashMap.fromList $ for hasDefaults $ \(name, _tv, _typeIdent, expr) -> do
            (maybeValue, instructions) <- subBlock' env expr
            case maybeValue of
                Just value -> return (name, (value, instructions))
                Nothing -> error "ICE: What is this :("

        writeDeclaration $ TraitDefinition ddn defaultDecls

        for_ decls $ \(name, _declType, _typeIdent, _maybeExpr) -> do
            let body =
                    [ Return (Property (LocalBinding "dict") name) ]
            writeDeclaration $ Declaration export $ DFun name [AST.PBinding "dict"] body
        return ()

    AST.DImpl typeVar traitRef implType contextArgs decls -> do
        let (AST.FromModule traitModule, traitName) = traitRef
        let traitIdentity = TraitIdentity traitModule traitName
        let ddn = defaultsDictName traitRef

        -- TODO: refactor the duplication out of this
        case implType of
            AST.ImplTypeNominal AST.ImplNominal{..} -> do
                instanceName <- traitDictName traitRef typeVar
                decls' <- for decls $ \(name, expr) -> do
                    -- TODO: find some better way to guarantee that we never
                    -- define an instance value to be flow control
                    (Just value, instructions) <- subBlock' env expr
                    return (name, (value, instructions))
                writeDeclaration $ TraitInstance ddn instanceName (HashMap.fromList decls') contextArgs
            AST.ImplTypeFunction arity -> do
                let instanceName = instanceDictName traitIdentity $ FunctionIdentity arity
                decls' <- for decls $ \(name, expr) -> do
                    -- TODO: find some better way to guarantee that we never
                    -- define an instance value to be flow control
                    (Just value, instructions) <- subBlock' env expr
                    return (name, (value, instructions))
                writeDeclaration $ TraitInstance ddn instanceName (HashMap.fromList decls') contextArgs
            AST.ImplTypeRecord fieldFunction -> do
                -- TODO: find some better way to guarantee that we never
                -- define an instance value to be be flow control
                (Just value, _instructions) <- subBlock' env fieldFunction

                writeDeclaration $ RecordFieldMap (instanceFieldMapName traitIdentity) value

                let instanceName = instanceDictName traitIdentity RecordIdentity
                decls' <- for decls $ \(name, expr) -> do
                    -- TODO: find some better way to guarantee that we never
                    -- define an instance value to be flow control
                    (Just value', instructions) <- subBlock' env expr
                    return (name, (value', instructions))
                writeDeclaration $ TraitInstance ddn instanceName (HashMap.fromList decls') ("fieldMap" : contextArgs)

    AST.DException _ name _ -> do
        writeDeclaration $ Declaration export $ DException name

generateModule :: ModuleName -> AST.Module AST.ResolvedReference AST.PatternTag TypeVar -> IO Module
generateModule moduleName AST.Module{..} = do
    counter <- newIORef 0
    fmap snd $ runWriterT $ do
        for_ mDecls $
            generateDecl Env { eModuleName=moduleName, eCounter=counter }

generateProgram :: AST.Program -> IO Program
generateProgram AST.Program{..} = do
    let allModules = HashMap.toList pOtherModules

    let nodes = [(m, mn, fmap snd $ importsOf $ AST.lmModule m) | (mn, m) <- allModules]
    let (graph, getModule, _) = graphFromEdges nodes

    -- topologically sort
    let sortedModules = map getModule $ reverse $ topSort graph

    for sortedModules $ \(mod', moduleName, _) ->
        fmap (moduleName,) $ generateModule moduleName $ AST.lmModule mod'
