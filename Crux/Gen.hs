{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Crux.Gen where

import Crux.Prelude
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Text as Text
import qualified Crux.AST as AST
import Control.Monad.Writer.Lazy (WriterT, runWriterT, tell)
import Control.Monad.Trans.Class (lift)

type Name = Text
type Output = Text
type Input = Name

data Step
    -- values
    = Reference Input
    | Function [Name] [Computation]
    | Literal AST.Literal

    -- operations
    | Intrinsic (AST.Intrinsic Input)
    | Apply Input [Input]
    | Print Input

    -- control flow
    | Return Input
    | If Input [Computation] [Computation]
    deriving (Show, Eq)

data Computation = Computation Output Step
    deriving (Show, Eq)

type Env = IORef Int

type Module = [Computation]

newOutput :: Env -> IO Name
newOutput env = do
    value <- readIORef env
    writeIORef env (value + 1)
    return $ "temp_" <> Text.pack (show value)

type GenWriter a = WriterT [Computation] IO a

writeComputation :: Name -> Step -> GenWriter Output
writeComputation output step = do
    tell [Computation output step]
    return output

newComputation :: Env -> Step -> GenWriter Output
newComputation env step = do
    output <- lift $ newOutput env
    writeComputation output step

generate' :: Show t => Env -> AST.Expression t -> GenWriter Output
generate' env expr = case expr of
    AST.EApp _ fn args -> do
        fn' <- generate' env fn
        args' <- mapM (generate' env) args
        newComputation env $ Apply fn' args'

    AST.ELiteral _ lit -> do
        newComputation env $ Literal lit

    AST.EIdentifier _ name -> do
        return name

    AST.EIntrinsic _ iid -> do
        iid' <- AST.mapIntrinsicInputs (generate' env) iid
        newComputation env $ Intrinsic iid'

    AST.ESemi _ lhs rhs -> do
        _ <- generate' env lhs
        generate' env rhs

    AST.EReturn _ rv -> do
        rv' <- generate' env rv
        newComputation env $ Return rv'

    AST.EIfThenElse _ cond ifTrue ifFalse -> do
        cond' <- generate' env cond
        ifTrue' <- subBlock env ifTrue
        ifFalse' <- subBlock env ifFalse
        newComputation env $ If cond' ifTrue' ifFalse'

    _ -> do
        error $ "Unexpected expression: " <> show expr

subBlock :: Show t => Env -> AST.Expression t -> GenWriter [Computation]
subBlock env expr = do
    fmap snd $ lift $ runWriterT $ generate' env expr

generate :: Show t => AST.Expression t -> IO [Computation]
generate expr = do
    env <- newIORef 0
    fmap snd $ runWriterT $ generate' env expr

generateDecl :: Show t => Env -> AST.Declaration t -> GenWriter ()
generateDecl env decl = do
    case decl of
        AST.DLet _ name _ defn -> do
            output <- generate' env defn
            writeComputation name $ Reference output
            return ()
        AST.DFun (AST.FunDef _ name params body) -> do
            body' <- subBlock env body
            writeComputation name $ Function params body'
            return ()

generateModule :: Show t => AST.Module t -> IO Module
generateModule AST.Module{..} = do
    env <- newIORef 0
    fmap snd $ runWriterT $ do
        forM_ mDecls $ generateDecl env
