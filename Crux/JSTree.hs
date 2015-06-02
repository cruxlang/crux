{-# LANGUAGE OverloadedStrings #-}

module Crux.JSTree where

import           Data.Monoid            ((<>), mconcat, mempty)
import           Data.Text              (Text)
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy.Builder as B

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
    | EBinOp Text Expression Expression -- lhs <op> rhs
    | ELiteral Literal
    | EIdentifier Name
    | ESemi Expression Expression
    | EComma Expression Expression
    deriving (Show, Eq)

renderFunction :: Maybe Text -> Maybe Text -> [Statement] -> Builder
renderFunction maybeName maybeArg body =
    let renderedBody = case body of
            [] -> mempty
            _ -> mconcat (map render (init body))
                    <> B.fromText "return " <> render (last body)
    in B.fromText "function "
        <> (maybe mempty B.fromText maybeName)
        <> B.fromText "("
        <> (maybe mempty B.fromText maybeArg)
        <> B.fromText "){\n"
        <> renderedBody
        <> B.fromText "}\n"

render :: Statement -> Builder
render stmt = case stmt of
    SBlock s ->
        B.fromText "{\n"
        <> mconcat (map render s)
        <> B.fromText "}\n"

    SVar name expr ->
        B.fromText "var "
            <> B.fromText name
            <> B.fromText " = "
            <> renderExpr expr
            <> B.fromText ";\n"

    SFunction maybeName maybeArg body ->
        renderFunction maybeName maybeArg body
    SExpression expr ->
        renderExpr expr
            <> B.fromText  ";\n"
    SReturn expr -> do
        B.fromText "return "
            <> renderExpr expr
            <> B.fromText ";\n"

renderExpr :: Expression -> Builder
renderExpr expr = case expr of
    EApplication lhs maybeRhs ->
        renderExpr lhs
            <> B.fromText "("
            <> (maybe mempty renderExpr maybeRhs)
            <> B.fromText ")"
    EFunction maybeArg body ->
        B.fromText "("
            <> renderFunction Nothing maybeArg body
            <> B.fromText ")"
    EBinOp op lhs rhs ->
        B.fromText "("
            <> renderExpr lhs
            <> B.fromText op
            <> renderExpr rhs
            <> B.fromText ")"
    ELiteral lit -> case lit of
        LInteger i -> B.fromString $ show i
        LString i -> B.fromString $ show i
        LTrue -> B.fromText "true"
        LFalse -> B.fromText "false"
        LNull -> B.fromText "null"
        LUndefined -> B.fromText "(void 0)"
    EIdentifier n -> B.fromText n
    ESemi lhs rhs ->
        renderExpr lhs
            <> B.fromText ";\n"
            <> renderExpr rhs
    EComma lhs rhs -> do
        B.fromText "("
            <> renderExpr lhs
            <> B.fromText ",\n"
            <> renderExpr rhs
            <> B.fromText ")"

renderDocument :: [Statement] -> Text
renderDocument statements = T.toStrict $ B.toLazyText $ mconcat (map render statements)
