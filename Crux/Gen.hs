{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Crux.Gen
    ( Value(..)
    , Output(..)
    , Instruction(..)
    , Module
    , generateModule
    ) where

import Crux.Prelude
import Data.IORef (newIORef, readIORef, writeIORef)
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
    deriving (Show, Eq)
type Input = Value

data Instruction
    -- binding
    = EmptyLet Output
    | LetBinding Name Input

    -- operations
    | Assign Output Input
    | Intrinsic Output (AST.Intrinsic Input)
    | Call Output Input [Input]

    -- control flow
    | Return Input
    | If Input [Instruction] [Instruction]
    deriving (Show, Eq)

type Env = IORef Int

type Module = [Instruction]

newTempOutput :: Env -> IO Output
newTempOutput env = do
    value <- readIORef env
    writeIORef env (value + 1)
    return $ Temporary value

type GenWriter a = WriterT [Instruction] IO a

writeInstruction :: Instruction -> GenWriter ()
writeInstruction i = tell [i]

newInstruction :: Env -> (Output -> Instruction) -> GenWriter (Maybe Value)
newInstruction env instr = do
    output <- lift $ newTempOutput env
    writeInstruction $ instr output
    return $ Just $ Reference output

generate :: Show t => Env -> AST.Expression t -> GenWriter (Maybe Value)
generate env expr = case expr of
    AST.EApp _ fn args -> do
        fn' <- generate env fn
        args' <- runMaybeT $ mapM (MaybeT . generate env) args
        case (,) <$> fn' <*> args' of
            Just (fn'', args'') -> do
                newInstruction env $ \output -> Call output fn'' args''
            Nothing -> do
                return Nothing

    AST.ELiteral _ lit -> do
        return $ Just $ Literal lit

    AST.EIdentifier _ name -> do
        return $ Just $ Reference $ Binding name

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

    AST.EReturn _ rv -> do
        rv' <- generate env rv
        case rv' of
            Just rv'' -> do
                writeInstruction $ Return rv''
                return Nothing
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

    _ -> do
        error $ "Unexpected expression: " <> show expr

subBlock :: Show t => Env -> AST.Expression t -> GenWriter [Instruction]
subBlock env expr = do
    fmap snd $ lift $ runWriterT $ generate env expr

subBlockWithOutput :: Show t => Env -> Output -> AST.Expression t -> GenWriter [Instruction]
subBlockWithOutput env output expr = do
    (output', instrs) <- lift $ runWriterT $ generate env expr
    return $ case output' of
        Just output'' -> instrs ++ [Assign output output'']
        Nothing -> instrs

generateDecl :: Show t => Env -> AST.Declaration t -> GenWriter ()
generateDecl env decl = do
    case decl of
        AST.DLet _ name _ defn -> do
            -- error if output has no return value
            (Just output) <- generate env defn
            writeInstruction $ LetBinding name output
            return ()
        AST.DFun (AST.FunDef _ name params body) -> do
            body' <- subBlock env body
            writeInstruction $ LetBinding name $ FunctionLiteral params body'
            return ()

generateModule :: Show t => AST.Module t -> IO Module
generateModule AST.Module{..} = do
    env <- newIORef 0
    fmap snd $ runWriterT $ do
        forM_ mDecls $ generateDecl env
