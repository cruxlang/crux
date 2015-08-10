{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Crux.Gen
    ( Instruction(..)
    , generateModule
    ) where

import Crux.Prelude
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Text as Text
import qualified Crux.AST as AST
import Control.Monad.Writer.Lazy (WriterT, runWriterT, tell)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

type Name = Text
type Output = Text
type Input = Name

data Instruction
    -- binding
    = LetBinding Output Name
    -- values
    | FunctionLiteral Output [Name] [Instruction]
    | Literal Output AST.Literal

    -- operations
    | Intrinsic Output (AST.Intrinsic Input)
    | Call Output Input [Input]

    -- control flow
    | Return Input
    | If Input [Instruction] [Instruction]
    deriving (Show, Eq)

type Env = IORef Int

type Module = [Instruction]

newTempOutput :: Env -> IO Name
newTempOutput env = do
    value <- readIORef env
    writeIORef env (value + 1)
    return $ "temp_" <> Text.pack (show value)

type GenWriter a = WriterT [Instruction] IO a

writeInstruction :: Instruction -> GenWriter ()
writeInstruction i = tell [i]

newInstruction :: Env -> (Output -> Instruction) -> GenWriter (Maybe Output)
newInstruction env instr = do
    output <- lift $ newTempOutput env
    writeInstruction $ instr output
    return $ Just output

generate :: Show t => Env -> AST.Expression t -> GenWriter (Maybe Output)
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
        newInstruction env $ \output -> Literal output lit

    AST.EIdentifier _ name -> do
        return $ Just name

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
        ifTrue' <- subBlock env ifTrue
        ifFalse' <- subBlock env ifFalse
        case cond' of
            Just cond'' -> do
                writeInstruction $ If cond'' ifTrue' ifFalse'
                return Nothing
            Nothing -> do
                return Nothing

    _ -> do
        error $ "Unexpected expression: " <> show expr

subBlock :: Show t => Env -> AST.Expression t -> GenWriter [Instruction]
subBlock env expr = do
    fmap snd $ lift $ runWriterT $ generate env expr

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
            writeInstruction $ FunctionLiteral name params body'
            return ()

generateModule :: Show t => AST.Module t -> IO Module
generateModule AST.Module{..} = do
    env <- newIORef 0
    fmap snd $ runWriterT $ do
        forM_ mDecls $ generateDecl env
