{-# LANGUAGE OverloadedStrings #-}

module Crux.JSTree where

import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HashMap
import           Data.Monoid            (Monoid, mconcat, mempty, (<>))
import           Data.Text              (Text)
import qualified Data.Text.Lazy         as TL
import           Data.Text.Lazy.Builder as B

type Name = Text

data Statement
    = SBlock [Statement]
    | SVar Name (Maybe Expression)
    | SFunction Name [Name] [Statement] -- function name(arg, arg, arg, ...) { statements }
    | SExpression Expression
    | SReturn (Maybe Expression)
    | SIf Expression Statement (Maybe Statement) -- if (e1) { s1 } else { s2 }
    | SAssign Expression Expression
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
    | EBinOp Text Expression Expression -- lhs <op> rhs
    | ESubscript Expression Expression
    | ELookup Expression Name
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
    let renderedBody = case body of
            [] -> mempty
            _ ->
                let inits = map render (init body)
                    lastOne = last body
                    last' = case lastOne of
                        SExpression _ -> B.fromText "return " <> render lastOne
                        _ -> render lastOne
                in mconcat inits <> last'
    in B.fromText "function "
        <> (maybe mempty B.fromText maybeName)
        <> B.fromText "("
        <> intercalate (B.fromText ",") (map B.fromText maybeArg)
        <> B.fromText "){\n"
        <> renderedBody
        <> B.fromText "}\n"

render :: Statement -> Builder
render stmt = case stmt of
    SBlock s ->
        B.fromText "{\n"
        <> mconcat (map render s)
        <> B.fromText "}\n"

    SVar name maybeExpr ->
        B.fromText "var "
            <> B.fromText name
            <> maybe mempty (\expr -> B.fromText " = " <> renderExpr expr) maybeExpr
            <> B.fromText ";\n"

    SAssign lhs rhs ->
        renderExpr lhs
            <> B.fromText " = "
            <> renderExpr rhs
            <> B.fromText ";\n"

    SFunction name maybeArg body ->
        renderFunction (Just name) maybeArg body
    SExpression expr ->
        renderExpr expr
            <> B.fromText  ";\n"
    SReturn expr ->
        B.fromText "return "
            <> maybe mempty renderExpr expr
            <> B.fromText ";\n"
    SIf expr thenStmt elseStatement ->
        B.fromText "if("
        <> renderExpr expr
        <> B.fromText ")"
        <> render thenStmt
        <> (case elseStatement of
            Just elseStmt -> B.fromText "else " <> render elseStmt
            Nothing -> mempty)

renderExpr :: Expression -> Builder
renderExpr expr = case expr of
    EApplication lhs maybeRhs ->
        renderExpr lhs
            <> B.fromText "("
            <> (intercalate (B.fromText ", ") $ map renderExpr maybeRhs)
            <> B.fromText ")"
    EFunction args body ->
        B.fromText "("
            <> renderFunction Nothing args body
            <> B.fromText ")"
    EObject fields ->
        let renderKeyValuePair (key, value) = B.fromText key <> B.fromText ":" <> renderExpr value
        in B.fromText "{"
        <> (intercalate (B.fromText ", ") $ map renderKeyValuePair (HashMap.toList fields))
        <> B.fromText "}"
    EBinOp op lhs rhs ->
        B.fromText "("
            <> renderExpr lhs
            <> B.fromText op
            <> renderExpr rhs
            <> B.fromText ")"
    ESubscript lhs rhs ->
        renderExpr lhs
        <> B.fromText "["
        <> renderExpr rhs
        <> B.fromText "]"
    ELookup lhs propName ->
        B.fromText "("
        <> renderExpr lhs
        <> B.fromText ")"
        <> B.fromText "."
        <> B.fromText propName
    ELiteral lit -> case lit of
        LInteger i -> B.fromString $ show i
        LString i -> B.fromString $ show i
        LTrue -> B.fromText "true"
        LFalse -> B.fromText "false"
        LNull -> B.fromText "null"
        LUndefined -> B.fromText "(void 0)"
    EIdentifier n -> B.fromText n
    EArray els ->
        B.fromText "["
        <> intercalate "," (map renderExpr els)
        <> B.fromText "]"
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
    ETernary condition ifTrue ifFalse ->
        renderExpr condition <> "?" <> renderExpr ifTrue <> ":" <> renderExpr ifFalse
    ERaw txt ->
        B.fromText txt

renderDocument :: [Statement] -> Text
renderDocument statements = TL.toStrict $ B.toLazyText $ mconcat (map render statements)
