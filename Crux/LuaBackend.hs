
module Crux.LuaBackend where

import Crux.Prelude
import qualified Data.Text.Lazy as TL
import qualified Crux.Gen as Gen
import qualified Crux.JSTree as JS
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text as Text
import Data.Text.Lazy.Builder (Builder)
import qualified Data.HashMap.Strict as HashMap
import Control.Monad.Trans.State.Strict (State, evalState)
import Control.Monad.Trans.Writer.Strict (WriterT, execWriterT)
import Crux.JSTree (Statement (..), Expression (..), Literal(..), JSWriter, writeS, write, beginLineS, indent)

renderFunction :: Maybe Text -> [Text] -> [Statement] -> JSWriter ()
renderFunction maybeName args body = do
    writeS "function"
    for_ maybeName $ \name -> do
        writeS " "
        write name
    
    writeS "("
    write $ intercalate "," args
    write (")\n" :: String)
    indent $ for_ body renderStatement
    beginLineS "end"

renderStatement :: Statement -> JSWriter ()
renderStatement = \case
    SBlock s -> do
        indent $ for_ s renderStatement
        beginLineS "end\n"

    SVar name maybeExpr -> do
        beginLineS "local "
        write name
        for_ maybeExpr $ \expr -> do
            writeS " = "
            renderExpr expr
        writeS "\n"

    SAssign lhs rhs -> do
        beginLineS ""
        renderExpr lhs
        writeS " = "
        renderExpr rhs
        writeS "\n"

    SFunction name maybeArg body -> do
        beginLineS ""
        renderFunction (Just name) maybeArg body
        writeS "\n"
    SExpression expr -> do
        beginLineS ""
        renderExpr expr
        writeS "\n"
    SReturn expr -> do
        beginLineS "return "
        for_ expr renderExpr
        writeS "\n"
    SBreak ->
        beginLineS "break\n"
    SIf expr thenStmt elseStatement -> do
        beginLineS "if "
        renderExpr expr
        case thenStmt of
            SBlock body -> do
                writeS "then\n"
                indent $ for_ body renderStatement
                beginLineS "end\n"
            _ -> do
                writeS "then\n"
                indent $ renderStatement thenStmt
                writeS "end\n"
        for_ elseStatement $ \case
            SBlock body -> do
                beginLineS "else\n"
                indent $ for_ body renderStatement
                beginLineS "end\n"
            es -> do
                beginLineS "else\n"
                indent $ renderStatement es
                beginLineS "end\n"
    SWhile expr body -> do
        beginLineS "while"
        renderExpr expr
        writeS "do\n"
        renderStatement body
        beginLineS "end\n"
    -- SThrow expr -> do
    --     beginLineS "throw "
    --     renderExpr expr
    --     writeS ";\n"
    -- STryCatch tryBody exceptionName catchBody -> do
    --     beginLineS "try {\n"
    --     indent $ for_ tryBody renderStatement
    --     beginLineS "} catch ("
    --     write exceptionName
    --     writeS ") {\n"
    --     indent $ for_ catchBody renderStatement
    --     beginLineS "}\n"

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
        renderExpr ctor
        writeS ".new("
        for_ args $ \args' ->
            commaSeparated $ map renderExpr args'
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
        LNull -> writeS "nil"
        LUndefined -> writeS "nil"
    EIdentifier n -> write n
    EArray els -> do
        writeS "["
        commaSeparated $ map renderExpr els
        writeS "]"
    ESemi lhs rhs -> do
        renderExpr lhs
        writeS "\n"
        renderExpr rhs
    -- EComma lhs rhs -> do
    --     writeS "("
    --     renderExpr lhs
    --     writeS ",\n"
    --     renderExpr rhs
    --     writeS ")"
    -- ETernary condition ifTrue ifFalse -> do
    --     renderExpr condition
    --     writeS "?"
    --     renderExpr ifTrue
    --     writeS ":"
    --     renderExpr ifFalse
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

-- iife :: [Statement] -> Expression
-- iife body = EApplication (EFunction [] body) []
