{-# LANGUAGE OverloadedStrings #-}

module Crux.JSTree where

import Crux.Prelude
import qualified Data.HashMap.Strict    as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Lazy         as TL
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B

type Name = Text

data Statement
    = SBlock [Statement]
    | SVar Name (Maybe Expression)
    | SFunction Name [Name] [Statement] -- function name(arg, arg, arg, ...) { statements }
    | SExpression Expression
    | SReturn (Maybe Expression)
    | SBreak
    | SIf Expression Statement (Maybe Statement) -- if (e1) { s1 } else { s2 }
    | SWhile Expression Statement
    | SAssign Expression Expression
    | SThrow Expression
    | STryCatch [Statement] Name [Statement]
    -- | STryFinally [Statement] [Statement]
    -- | STryCatchFinally ...
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
    = EApplication Expression [Expression]
    | EFunction [Name] [Statement] -- function(args) { statements }
    | EObject (HashMap Name Expression)
    | EPrefixOp Text Expression
    | EBinOp Text Expression Expression -- lhs <op> rhs
    | ELookup Expression Name
    | EIndex Expression Expression
    | ELiteral Literal
    | EIdentifier Name
    | EArray [Expression]
    | ESemi Expression Expression
    | EComma Expression Expression
    | ETernary Expression Expression Expression
    | ERaw Text
    deriving (Show, Eq)

intercalate :: (Show m, Monoid m) => m -> [m] -> m
intercalate sep els = case els of
    [] -> mempty
    [x] -> x
    (x:y:xs) -> x <> sep <> intercalate sep (y:xs)

renderFunction :: Maybe Text -> [Text] -> [Statement] -> Builder
renderFunction maybeName maybeArg body =
    "function "
        <> (maybe mempty B.fromText maybeName)
        <> "("
        <> intercalate "," (map B.fromText maybeArg)
        <> "){\n"
        <> mconcat (map render body)
        <> "}\n"

render :: Statement -> Builder
render stmt = case stmt of
    SBlock s ->
        "{\n"
        <> mconcat (map render s)
        <> "}\n"

    SVar name maybeExpr ->
        "var "
            <> B.fromText name
            <> maybe mempty (\expr -> " = " <> renderExpr expr) maybeExpr
            <> ";\n"

    SAssign lhs rhs ->
        renderExpr lhs
            <> " = "
            <> renderExpr rhs
            <> ";\n"

    SFunction name maybeArg body ->
        renderFunction (Just name) maybeArg body
    SExpression expr ->
        renderExpr expr
            <> B.fromText ";\n"
    SReturn expr ->
        "return "
            <> maybe mempty renderExpr expr
            <> ";\n"
    SBreak ->
        "break;\n"
    SIf expr thenStmt elseStatement ->
        "if("
        <> renderExpr expr
        <> ")"
        <> render thenStmt
        <> (case elseStatement of
            Just elseStmt -> "else " <> render elseStmt
            Nothing -> mempty)
    SWhile expr body ->
        "while("
        <> renderExpr expr
        <> ")"
        <> render body
    SThrow expr ->
        "throw " <> renderExpr expr <> ";"
    STryCatch tryBody exceptionName catchBody ->
        "try{\n"
        <> mconcat (map render tryBody)
        <> "}\n"
        <> "catch(" <> B.fromText exceptionName <> "){\n"
        <> mconcat (map render catchBody)
        <> "}\n"

-- TODO: render nonprintable characters in a human-readable way
renderChar :: Char -> Builder
renderChar '"' = "\\\""
renderChar '\n' = "\\n"
renderChar '\r' = "\\r"
renderChar '\\' = "\\\\"
renderChar c = B.fromString [c]

renderString :: String -> Builder
renderString s = "\"" <> mconcat (map renderChar s) <> "\""

renderExpr :: Expression -> Builder
renderExpr expr = case expr of
    EApplication lhs maybeRhs ->
        renderExpr lhs
            <> "("
            <> (intercalate (", ") $ map renderExpr maybeRhs)
            <> ")"
    EFunction args body ->
        "("
            <> renderFunction Nothing args body
            <> ")"
    EObject fields ->
        let renderKeyValuePair (key, value) = B.fromText key <> ":" <> renderExpr value
        in "{"
        <> (intercalate (", ") $ map renderKeyValuePair (HashMap.toList fields))
        <> "}"
    EPrefixOp op arg ->
        "("
        <> B.fromText op
        <> renderExpr arg
        <> ")"
    EBinOp op lhs rhs ->
        "("
            <> renderExpr lhs
            <> B.fromText op
            <> renderExpr rhs
            <> ")"
    ELookup lhs propName ->
        "("
        <> renderExpr lhs
        <> ")."
        <> B.fromText propName
    EIndex lhs rhs ->
        renderExpr lhs <> "[" <> renderExpr rhs <> "]"
    ELiteral lit -> case lit of
        LInteger i -> B.fromString $ show i
        LString i -> renderString $ Text.unpack i
        LTrue -> "true"
        LFalse -> "false"
        LNull -> "null"
        LUndefined -> "(void 0)"
    EIdentifier n -> B.fromText n
    EArray els ->
        "["
        <> intercalate "," (map renderExpr els)
        <> "]"
    ESemi lhs rhs ->
        renderExpr lhs
            <> ";\n"
            <> renderExpr rhs
    EComma lhs rhs -> do
        "("
            <> renderExpr lhs
            <> ",\n"
            <> renderExpr rhs
            <> ")"
    ETernary condition ifTrue ifFalse ->
        renderExpr condition <> "?" <> renderExpr ifTrue <> ":" <> renderExpr ifFalse
    ERaw txt ->
        B.fromText txt

renderDocument :: [Statement] -> Text
renderDocument statements = TL.toStrict $ B.toLazyText $ mconcat (map render statements)

iife :: [Statement] -> Expression
iife body = EApplication (EFunction [] body) []
