{-# LANGUAGE OverloadedStrings #-}

module Crux.JSTree where

import           Data.Maybe             (maybeToList)
import           Data.Monoid            (Monoid, mconcat, mempty, (<>))
import           Data.Text              (Text)
import qualified Data.Text.Lazy         as TL
import           Data.Text.Lazy.Builder as B

type Name = Text

data Statement
    = SBlock [Statement]
    | SVar Name Expression
    | SFunction (Maybe Name) [Name] [Statement] -- function name(arg, arg, arg, ...) { statements }
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
    = EApplication Expression (Maybe Expression)
    | EFunction (Maybe Name) [Statement] -- function(arg_name) { statements }
    | EBinOp Text Expression Expression -- lhs <op> rhs
    | ESubscript Expression Expression
    | ELiteral Literal
    | EIdentifier Name
    | EArray [Expression]
    | ESemi Expression Expression
    | EComma Expression Expression
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

    SVar name expr ->
        B.fromText "var "
            <> B.fromText name
            <> B.fromText " = "
            <> renderExpr expr
            <> B.fromText ";\n"

    SAssign lhs rhs ->
        renderExpr lhs
            <> B.fromText " = "
            <> renderExpr rhs
            <> B.fromText ";\n"

    SFunction maybeName maybeArg body ->
        renderFunction maybeName maybeArg body
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
            <> (maybe mempty renderExpr maybeRhs)
            <> B.fromText ")"
    EFunction maybeArg body ->
        B.fromText "("
            <> renderFunction Nothing (maybeToList maybeArg) body
            <> B.fromText ")"
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

renderDocument :: [Statement] -> Text
renderDocument statements = TL.toStrict $ B.toLazyText $ mconcat (map render statements)
