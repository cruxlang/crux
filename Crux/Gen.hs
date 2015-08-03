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
    = Function [Name] [Computation]
    | Literal AST.Literal

    -- operations
    | Apply Input [Input]
    | Print Input

    -- control flow
    | Return Input
    | If Input [Computation] [Computation]

data Computation = Computation Output Step

type Env = IORef Int

type Module = [Computation]

newOutput :: Env -> IO Name
newOutput env = do
    value <- readIORef env
    writeIORef env (value + 1)
    return $ "temp_" <> Text.pack (show value)

writeComputation :: Name -> Step -> WriterT [Computation] IO Output
writeComputation output step = do
    tell [Computation output step]
    return output

generate' :: Env -> AST.Expression t -> WriterT [Computation] IO Output
generate' env expr = case expr of
    AST.EApp _ fn args -> do
        fn' <- generate' env fn
        args' <- mapM (generate' env) args
        output <- lift $ newOutput env
        writeComputation output $ Apply fn' args'
    {-
        return $ JS.ELiteral $ case lit of
            LString s -> JS.LString s
            LInteger i -> JS.LInteger i
            LUnit -> JS.LUndefined
    -}
    AST.ESemi _ lhs rhs -> do
        _ <- generate' env lhs
        generate' env rhs

generate :: AST.Expression t -> IO [Computation]
generate expr = do
    env <- newIORef 0
    fmap snd $ runWriterT $ generate' env expr

generateModule :: [AST.Declaration t] -> IO Module
generateModule decls = do
    env <- newIORef 0
    fmap snd $ runWriterT $ do
        forM_ decls $ \decl ->
            return ((), [])
