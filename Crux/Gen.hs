{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Crux.Gen
    ( Value(..)
    , Output(..)
    , Instruction(..)
    , DeclarationType(..)
    , Declaration(..)
    , Module
    , generateModule
    ) where

import Crux.Prelude
import qualified Crux.AST as AST
import Control.Monad.Writer.Lazy (WriterT, runWriterT, tell)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

type Name = Text
data Output = Binding Name | Temporary Int
    deriving (Show, Eq)
data Value
    = Reference Output
    | Literal AST.Literal
    | FunctionLiteral [Name] [Instruction]
    | RecordLiteral (HashMap Name Value)
    deriving (Show, Eq)
type Input = Value

data Instruction
    -- binding
    = EmptyLet Output
    | LetBinding Name Input

    -- operations
    | Assign Output Input
    | BinIntrinsic Output (AST.BinIntrinsic) Input Input
    | Intrinsic Output (AST.Intrinsic Input)
    | Call Output Input [Input]
    | MethodCall Output Input Name [Input]
    | Lookup Output Input Name

    -- control flow
    | Return Input
    | Match Input [(AST.Pattern, [Instruction])]
    | If Input [Instruction] [Instruction]
    deriving (Show, Eq)

type Env = IORef Int

data DeclarationType
    = DData Name [AST.Variant]
    | DJSData Name [AST.JSVariant]
    | DFun Name [Name] [Instruction]
    | DLet Name [Instruction]
    deriving (Show, Eq)

data Declaration = Declaration AST.ExportFlag DeclarationType
    deriving (Show, Eq)

type Module = [Declaration]

newTempOutput :: Env -> IO Output
newTempOutput env = do
    value <- readIORef env
    writeIORef env (value + 1)
    return $ Temporary value

type DeclarationWriter a = WriterT [Declaration] IO a

writeDeclaration :: Declaration -> DeclarationWriter ()
writeDeclaration d = tell [d]

type InstructionWriter a = WriterT [Instruction] IO a

writeInstruction :: Instruction -> InstructionWriter ()
writeInstruction i = tell [i]

newInstruction :: Env -> (Output -> Instruction) -> InstructionWriter (Maybe Value)
newInstruction env instr = do
    output <- lift $ newTempOutput env
    writeInstruction $ instr output
    return $ Just $ Reference output

generate :: Show t => Env -> AST.Expression t -> InstructionWriter (Maybe Value)
generate env expr = case expr of
    AST.ELet _ name _ v -> do
        v' <- generate env v
        case v' of
            Just v'' -> do
                writeInstruction $ LetBinding name v''
                return $ Just $ Literal AST.LUnit
            Nothing -> return Nothing
    AST.EFun _ params body -> do
        body' <- subBlockWithReturn env body
        return $ Just $ FunctionLiteral params body'
    AST.ERecordLiteral _ props -> do
        props' <- runMaybeT $ mapM (MaybeT . generate env) props
        case props' of
            Just props'' -> do
                return $ Just $ RecordLiteral props''
            Nothing -> do
                return Nothing

    AST.ELookup _ value propertyName -> do
        v <- generate env value
        case v of
            Just v' -> do
                newInstruction env $ \output -> Lookup output v' propertyName
            Nothing -> do
                return Nothing

    AST.EApp _ (AST.ELookup _ this methodName) args -> do
        this' <- generate env this
        args' <- runMaybeT $ mapM (MaybeT . generate env) args
        case (this', args') of
            (Just this'', Just args'') -> do
                newInstruction env $ \output -> MethodCall output this'' methodName args''
            _ ->
                return Nothing

    AST.EApp _ fn args -> do
        fn' <- generate env fn
        args' <- runMaybeT $ mapM (MaybeT . generate env) args
        case (fn', args') of
            (Just fn'', Just args'') -> do
                newInstruction env $ \output -> Call output fn'' args''
            _ -> do
                return Nothing

    AST.ELiteral _ lit -> do
        return $ Just $ Literal lit

    AST.EIdentifier _ name -> do
        return $ Just $ Reference $ Binding name

    AST.EBinIntrinsic _ bi lhs rhs -> do
        lhs' <- generate env lhs
        rhs' <- generate env rhs
        case (lhs', rhs') of
            (Just lhs'', Just rhs'') -> do
                newInstruction env $ \output -> BinIntrinsic output bi lhs'' rhs''
            _ -> do
                return Nothing
    AST.EIntrinsic _ iid -> do
        iid' <- runMaybeT $ AST.mapIntrinsicInputs (MaybeT . generate env) iid
        case iid' of
            Just iid'' -> do
                newInstruction env $ \output -> Intrinsic output iid''
            Nothing -> do
                return Nothing

    AST.ESemi _ lhs rhs -> do
        _ <- generate env lhs
        generate env rhs

    AST.EMatch _ value cases -> do
        value' <- generate env value
        case value' of
            Just value'' -> do
                output <- lift $ newTempOutput env
                writeInstruction $ EmptyLet output
                cases' <- forM cases $ \(AST.Case pat expr') -> do
                    expr'' <- subBlockWithOutput env output expr'
                    return (pat, expr'')
                writeInstruction $ Match value'' cases'
                return $ Just $ Reference output
            Nothing -> do
                return Nothing

    AST.EIfThenElse _ cond ifTrue ifFalse -> do
        cond' <- generate env cond
        output <- lift $ newTempOutput env
        writeInstruction $ EmptyLet output
        ifTrue' <- subBlockWithOutput env output ifTrue
        ifFalse' <- subBlockWithOutput env output ifFalse
        case cond' of
            Just cond'' -> do
                writeInstruction $ If cond'' ifTrue' ifFalse'
                return $ Just $ Reference output
            Nothing -> do
                return Nothing

    AST.EReturn _ rv -> do
        rv' <- generate env rv
        case rv' of
            Just rv'' -> do
                writeInstruction $ Return rv''
                return Nothing
            Nothing -> do
                return Nothing

subBlockWithReturn :: (MonadIO m, Show t) => Env -> AST.Expression t -> m [Instruction]
subBlockWithReturn env expr = do
    (output, instrs) <- liftIO $ runWriterT $ generate env expr
    return $ case output of
        Just output' -> instrs ++ [Return output']
        Nothing -> instrs

subBlockWithOutput :: (MonadIO m, Show t) => Env -> Output -> AST.Expression t -> m [Instruction]
subBlockWithOutput env output expr = do
    (output', instrs) <- liftIO $ runWriterT $ generate env expr
    return $ case output' of
        Just output'' -> instrs ++ [Assign output output'']
        Nothing -> instrs

generateDecl :: Show t => Env -> AST.Declaration t -> DeclarationWriter ()
generateDecl env (AST.Declaration export decl) = do
    case decl of
        AST.DData name _ variants -> do
            writeDeclaration $ Declaration export $ DData name variants
        AST.DJSData name variants -> do
            writeDeclaration $ Declaration export $ DJSData name variants
        AST.DFun (AST.FunDef _ name params body) -> do
            body' <- subBlockWithReturn env body
            writeDeclaration $ Declaration export $ DFun name params body'
        AST.DLet _ name _ defn -> do
            -- error if output has no return value
            defn' <- subBlockWithReturn env defn
            writeDeclaration $ Declaration export $ DLet name defn'
        AST.DType {} ->
            -- type aliases are not reflected into the IR
            return ()

generateModule :: Show t => AST.Module t -> IO Module
generateModule AST.Module{..} = do
    env <- newIORef 0
    fmap snd $ runWriterT $ do
        forM_ mDecls $ generateDecl env
