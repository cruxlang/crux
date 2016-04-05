{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Crux.JSTree where

import Crux.Prelude
import qualified Data.HashMap.Strict    as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Lazy         as TL
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import Control.Monad.State.Class (get, modify)
import Control.Monad.Writer.Class (tell)
import Control.Monad.Trans.State.Strict (State, evalState)
import Control.Monad.Trans.Writer.Strict (WriterT, execWriterT)

type JSWriter = WriterT Builder (State Int)

class StringType a where
    toBuilder :: a -> Builder

instance StringType Builder where
    toBuilder = id
instance StringType Text where
    toBuilder = B.fromText
instance StringType [Char] where
    toBuilder = B.fromString

write :: StringType a => a -> JSWriter ()
write s = do
    tell $ toBuilder s

writeS :: String -> JSWriter ()
writeS = write

beginLine :: StringType a => a -> JSWriter ()
beginLine s = do
    level <- get
    write $ Text.replicate level " "
    write s

beginLineS :: String -> JSWriter ()
beginLineS = beginLine

indent :: JSWriter () -> JSWriter ()
indent inner = do
    modify $ \level -> level + 2
    inner
    modify $ \level -> level - 2

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
    | ENew Expression [Expression]
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

renderFunction :: Maybe Text -> [Text] -> [Statement] -> JSWriter ()
renderFunction maybeName args body = do
    writeS "function"
    for_ maybeName $ \name -> do
        writeS " "
        write name
    writeS "("
    write $ intercalate ", " args
    writeS ") {\n"
    indent $ for_ body renderStatement
    beginLineS "}"

renderStatement :: Statement -> JSWriter ()
renderStatement = \case
    SBlock s -> do
        beginLineS "{\n"
        indent $ for_ s renderStatement
        beginLineS "}\n"

    SVar name maybeExpr -> do
        beginLineS "var "
        write name
        for_ maybeExpr $ \expr -> do
            writeS " = "
            renderExpr expr
        writeS ";\n"

    SAssign lhs rhs -> do
        beginLineS ""
        renderExpr lhs
        writeS " = "
        renderExpr rhs
        writeS ";\n"

    SFunction name maybeArg body -> do
        beginLineS ""
        renderFunction (Just name) maybeArg body
        writeS "\n"
    SExpression expr -> do
        beginLineS ""
        renderExpr expr
        writeS ";\n"
    SReturn expr -> do
        beginLineS "return "
        for_ expr renderExpr
        writeS ";\n"
    SBreak ->
        beginLineS "break;\n"
    SIf expr thenStmt elseStatement -> do
        beginLineS "if ("
        renderExpr expr
        case thenStmt of
            SBlock body -> do
                writeS ") {\n"
                indent $ for_ body renderStatement
                beginLineS "}\n"
            _ -> do
                writeS ")\n"
                indent $ renderStatement thenStmt
        for_ elseStatement $ \case
            SBlock body -> do
                beginLineS "else {\n"
                indent $ for_ body renderStatement
                beginLineS "}\n"
            es -> do
                beginLineS "else\n"
                indent $ renderStatement es
    SWhile expr body -> do
        beginLineS "while ("
        renderExpr expr
        writeS ")\n"
        renderStatement body
    SThrow expr -> do
        beginLineS "throw "
        renderExpr expr
        writeS ";\n"
    STryCatch tryBody exceptionName catchBody -> do
        beginLineS "try {\n"
        indent $ for_ tryBody renderStatement
        beginLineS "} catch ("
        write exceptionName
        writeS ") {\n"
        indent $ for_ catchBody renderStatement
        beginLineS "}\n"

-- TODO: render nonprintable characters in a human-readable way
renderChar :: Char -> Builder
renderChar '"' = "\\\""
renderChar '\n' = "\\n"
renderChar '\r' = "\\r"
renderChar '\\' = "\\\\"
renderChar c = B.fromString [c]

renderString :: String -> Builder
renderString s = "\"" <> mconcat (map renderChar s) <> "\""

commaSeparated :: [JSWriter ()] -> JSWriter ()
commaSeparated [] = return ()
commaSeparated [x] = x
commaSeparated (x:xs) = do
    x
    writeS ", "
    commaSeparated xs

renderExpr :: Expression -> JSWriter ()
renderExpr = \case
    EApplication lhs args -> do
        renderExpr lhs
        writeS "("
        commaSeparated $ map renderExpr args
        writeS ")"
    ENew ctor args -> do
        writeS "new "
        renderExpr ctor
        writeS "("
        commaSeparated $ map renderExpr args
        writeS ")"
    EFunction args body -> do
        writeS "("
        renderFunction Nothing args body
        writeS ")"
    EObject fields -> do
        writeS "{"
        commaSeparated $ (flip map) (HashMap.toList fields) $ \(key, value) -> do
            write key
            writeS ":"
            renderExpr value
        writeS "}"
    EPrefixOp op arg -> do
        writeS "("
        write op
        renderExpr arg
        writeS ")"
    EBinOp op lhs rhs -> do
        writeS "("
        renderExpr lhs
        write op
        renderExpr rhs
        writeS ")"
    ELookup lhs propName -> do
        writeS "("
        renderExpr lhs
        writeS ")."
        write propName
    EIndex lhs rhs -> do
        renderExpr lhs
        writeS "["
        renderExpr rhs
        writeS "]"
    ELiteral lit -> case lit of
        LInteger i -> write $ show i
        LString i -> write $ renderString $ Text.unpack i
        LTrue -> writeS "true"
        LFalse -> writeS "false"
        LNull -> writeS "null"
        LUndefined -> writeS "(void 0)"
    EIdentifier n -> write n
    EArray els -> do
        writeS "["
        commaSeparated $ map renderExpr els
        writeS "]"
    ESemi lhs rhs -> do
        renderExpr lhs
        writeS ";\n"
        renderExpr rhs
    EComma lhs rhs -> do
        writeS "("
        renderExpr lhs
        writeS ",\n"
        renderExpr rhs
        writeS ")"
    ETernary condition ifTrue ifFalse -> do
        renderExpr condition
        writeS "?"
        renderExpr ifTrue
        writeS ":"
        renderExpr ifFalse
    ERaw txt -> write txt

{-
type State s = StateT s Identity
newtype WriterT w m a
execWriterT :: Monad m => WriterT w m a -> m w
evalState :: State s a -> s -> a
WriterT Builder (State Int) a -> (State Int) Builder a
execWriterT Builder (State Int) (theshit) :: State
-}

renderDocument :: [Statement] -> Text
renderDocument statements = output
  where
    output = TL.toStrict $ B.toLazyText builder
    builder :: B.Builder
    builder = evalState (execWriterT $ for_ statements renderStatement) 0

iife :: [Statement] -> Expression
iife body = EApplication (EFunction [] body) []
