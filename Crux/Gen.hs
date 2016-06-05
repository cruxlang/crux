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
import qualified Crux.Module.Types as AST
import Crux.Prelude
import Data.Graph (graphFromEdges, topSort)
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

data Tag
    = TagVariant Name [(Integer, Tag)] -- zero-based index of payload that needs to be matched
    | TagLiteral JSTree.Literal
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
    | TryCatch [Instruction] AST.ResolvedReference (AST.Pattern AST.PatternTag) [Instruction]
    deriving (Show, Eq)

type Env = IORef Int

data DeclarationType
    = DData Name [AST.Variant ()]
    | DJSData Name [AST.JSVariant]
    | DFun Name [AST.Pattern AST.PatternTag] [Instruction]
    | DLet (AST.Pattern AST.PatternTag) [Instruction]
    | DException Name
    deriving (Show, Eq)

data Declaration = Declaration AST.ExportFlag DeclarationType
    deriving (Show, Eq)

type Program = [(AST.ModuleName, Module)] -- topologically sorted
type Module = [Declaration]

newTempOutput :: Env -> IO Int
newTempOutput env = do
    value <- readIORef env
    writeIORef env (value + 1)
    return value

type DeclarationWriter a = WriterT [Declaration] IO a

writeDeclaration :: Declaration -> DeclarationWriter ()
writeDeclaration d = tell [d]

type InstructionWriter a = WriterT [Instruction] IO a

writeInstruction :: Instruction -> InstructionWriter ()
writeInstruction i = tell [i]

newInstruction :: Env -> (Output -> Instruction) -> InstructionWriter Value
newInstruction env instr = do
    t <- lift $ newTempOutput env
    writeInstruction $ instr $ NewTemporary t
    return $ Temporary t

both :: Maybe a -> Maybe b -> Maybe (a, b)
both (Just x) (Just y) = Just (x, y)
both _ _ = Nothing

tagFromPattern :: AST.Pattern AST.PatternTag -> Maybe Tag
tagFromPattern = \case
    AST.PWildcard -> Nothing
    AST.PBinding _ -> Nothing
    AST.PConstructor _ref tag subpatterns -> case tag of
        AST.TagVariant name -> do
            let subtags = fmap tagFromPattern subpatterns
            let subtags' = (flip map) (zip [0..] subtags) $ \(i, st) -> case st of
                    Just st' -> Just (i, st')
                    Nothing -> Nothing
            Just $ TagVariant name $ catMaybes subtags'
        AST.TagLiteral literal ->
            Just $ TagLiteral literal

generate :: Env -> AST.Expression AST.ResolvedReference AST.PatternTag t -> InstructionWriter (Maybe Value)
generate env expr = case expr of
    AST.ELet _ _mut pat _ v -> do
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

    AST.EMethodApp _ _ _ _ -> do
        fail "ICE: this should never happen.  EMethodApp should be desugared into EApp by now."

    AST.EApp _ fn args -> do
        fn' <- generate env fn
        args' <- runMaybeT $ for args $ MaybeT . generate env
        for (both fn' args') $ \(fn'', args'') -> do
            newInstruction env $ \output -> Call output fn'' args''

    AST.EAssign _ target rhs -> do
        output <- case target of
            AST.ELookup _ lhs propName -> do
                lhs' <- generate env lhs
                for lhs' $ \lhs'' -> do
                    return $ OutputProperty lhs'' propName
            AST.EIdentifier _ name -> case name of
                AST.Ambient n -> return $ Just $ ExistingLocalBinding n
                AST.Local n -> return $ Just $ ExistingLocalBinding n
                AST.ThisModule n -> return $ Just $ ExistingLocalBinding n
                AST.OtherModule _ _ -> fail "cannot assign to imported names"
                AST.Builtin _ -> fail "cannot assign to builtin names"
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

    AST.ERecordLiteral _ props -> do
        props' <- runMaybeT $ for props $ MaybeT . generate env
        return $ fmap RecordLiteral props'

    AST.EIdentifier _ name -> do
        return $ Just $ ResolvedBinding name

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
            output <- lift $ newTempOutput env

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
        output <- lift $ newTempOutput env
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

    AST.EFor _ name over body -> do
        over' <- generate env over
        for over' $ \over'' -> do
            cmpVar <- lift $ newTempOutput env
            loopVar <- lift $ newTempOutput env

            writeInstruction $ Assign (NewTemporary loopVar) $ Literal $ AST.LInteger 0
            lengthVar <- newInstruction env $ \output -> Lookup output over'' "length"

            body' <- subBlock env body
            let loopHead =
                    [ BinIntrinsic (NewTemporary cmpVar) AST.BILess (Temporary loopVar) lengthVar
                    , If (Temporary cmpVar) [] [Break]
                    , Index (NewLocalBinding name) over'' $ Temporary loopVar
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

    AST.ETryCatch _ tryBody exceptionName binding catchBody -> do
        output <- lift $ newTempOutput env
        writeInstruction $ EmptyTemporary output
        tryBody' <- subBlockWithOutput env (ExistingTemporary output) tryBody
        catchBody' <- subBlockWithOutput env (ExistingTemporary output) catchBody
        writeInstruction $ TryCatch tryBody' exceptionName binding catchBody'
        return $ Just $ Temporary output

subBlock :: MonadIO m => Env -> AST.Expression AST.ResolvedReference AST.PatternTag t -> m [Instruction]
subBlock env expr = do
    (_output, instructions) <- liftIO $ runWriterT $ generate env expr
    return instructions

subBlockWithReturn :: MonadIO m => Env -> AST.Expression AST.ResolvedReference AST.PatternTag t -> m [Instruction]
subBlockWithReturn env expr = do
    (output, instrs) <- liftIO $ runWriterT $ generate env expr
    return $ case output of
        Just output' -> instrs ++ [Return output']
        Nothing -> instrs

subBlockWithOutput :: MonadIO m => Env -> Output -> AST.Expression AST.ResolvedReference AST.PatternTag t -> m [Instruction]
subBlockWithOutput env output expr = do
    (output', instrs) <- liftIO $ runWriterT $ generate env expr
    return $ case output' of
        Just output'' -> instrs ++ [Assign output output'']
        Nothing -> instrs

generateDecl :: Env -> AST.Declaration AST.ResolvedReference AST.PatternTag t -> DeclarationWriter ()
generateDecl env (AST.Declaration export _pos decl) = do
    case decl of
        AST.DDeclare _ _ _ -> do
            -- declarations are not reflected into the IR
            return ()
        AST.DData _ name _ _ variants -> do
            writeDeclaration $ Declaration export $ DData name $ fmap (fmap $ const ()) variants
        AST.DJSData _ name _ variants -> do
            writeDeclaration $ Declaration export $ DJSData name variants
        AST.DFun _ name funDecl -> do -- name params _retAnn body -> do
            let AST.FunctionDecl{..} = funDecl
            body' <- subBlockWithReturn env fdBody
            writeDeclaration $ Declaration export $ DFun name (map fst fdParams) body'
        AST.DLet _ _mut pat _ defn -> do
            defn' <- case pat of
                AST.PWildcard -> subBlock env defn
                AST.PBinding name -> subBlockWithOutput env (NewLocalBinding name) defn
                AST.PConstructor {} -> error "Gen: Top-level pattern matches are not yet supported"
            writeDeclaration $ Declaration export $ DLet pat defn'
        AST.DTypeAlias {} -> do
            -- type aliases are not reflected into the IR
            return ()
        AST.DException _ name _ -> do
            writeDeclaration $ Declaration export $ DException name

generateModule :: AST.Module AST.ResolvedReference AST.PatternTag t -> IO Module
generateModule AST.Module{..} = do
    env <- newIORef 0
    decls <- fmap snd $ runWriterT $ do
        for_ mDecls $ generateDecl env
    return decls

generateProgram :: AST.Program -> IO Program
generateProgram AST.Program{..} = do
    let allModules = HashMap.toList pOtherModules

    let nodes = [(m, mn, fmap snd $ importsOf $ AST.lmModule m) | (mn, m) <- allModules]
    let (graph, getModule, _) = graphFromEdges nodes

    -- topologically sort
    let sortedModules = map getModule $ reverse $ topSort graph

    for sortedModules $ \(mod', moduleName, _) ->
        fmap (moduleName,) $ generateModule $ AST.lmModule mod'
