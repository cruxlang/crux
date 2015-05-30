{-# LANGUAGE OverloadedStrings #-}

module Crux.JSTree where

import           Control.Monad (forM_)
import           Data.Monoid   ((<>))
import           Data.Text     (Text)
import           Data.Text.IO  (putStr, putStrLn)
import           Prelude       hiding (putStr, putStrLn)
import qualified Prelude

type Name = Text

data Statement
    = SBlock [Statement]
    | SVar Name Expression
    | SFunction (Maybe Name) (Maybe Name) [Statement] -- function name(arg_name) { statements }
    | SExpression Expression
    | SReturn Expression
    deriving (Show, Eq)

data Literal
    = LInteger Integer
    | LString Text
    | LTrue
    | LFalse
    | LNull
    | LUndefined
    deriving (Show, Eq)

data Expression
    = EApplication Expression (Maybe Expression)
    | EFunction (Maybe Name) [Statement] -- function(arg_name) { statements }
    | ELiteral Literal
    | EIdentifier Name
    | ESemi Expression Expression
    | EComma Expression Expression
    deriving (Show, Eq)

renderFunction maybeName maybeArg body = do
    putStr $ "function "
    case maybeName of
        Just name -> putStr name
        Nothing -> return ()
    putStr "("
    case maybeArg of
        Just arg -> putStr arg
        Nothing -> return ()
    putStrLn "){"
    case body of
        [] -> return ()
        _ -> do
            forM_ (init body) render
            putStr "return "
            render (last body)
    putStrLn "}"

render stmt = case stmt of
    SBlock s -> do
        putStr "{"
        forM_ s render
        putStrLn "}"

    SVar name expr -> do
        putStr $ "var " <> name <> " = "
        renderExpr expr
        putStrLn ";"

    SFunction Nothing _ _ ->
        return ()
    SFunction (Just name) maybeArg body -> do
        renderFunction (Just name) maybeArg body
    SExpression expr -> do
        renderExpr expr
        putStrLn ";"
    SReturn expr -> do
        putStr "return "
        renderExpr expr
        putStrLn ";"

renderExpr expr = case expr of
    EApplication lhs maybeRhs -> do
        renderExpr lhs
        putStr "("
        case maybeRhs of
            Just rhs -> renderExpr rhs
            Nothing -> return ()
        putStr ")"
    EFunction maybeArg body -> do
        putStr "("
        renderFunction Nothing maybeArg body
        putStr ")"
    ELiteral lit -> case lit of
        LInteger i -> Prelude.putStr $ show i
        LString i -> Prelude.putStr $ show i
        LTrue -> putStr "true"
        LFalse -> putStr "false"
        LNull -> putStr "null"
        LUndefined -> putStr "(void 0)"
    EIdentifier n -> putStr n
    ESemi lhs rhs -> do
        renderExpr lhs
        putStrLn ";"
        renderExpr rhs
    EComma lhs rhs -> do
        renderExpr lhs
        putStrLn ","
        renderExpr rhs
