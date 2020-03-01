
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
import Crux.JSTree (Statement (..), Expression (..), Literal(..), JSWriter, writeS, write, beginLineS, indent, iife)
import Control.Monad.ST (runST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import qualified Crux.JSBackend as JSBackend
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Debug.Trace (trace)

renderFunction :: Maybe Text -> [Text] -> [Statement] -> JSWriter ()
renderFunction maybeName args body = do
    writeS "function"
    for_ maybeName $ \name -> do
        writeS " "
        write $ mangle name
    
    writeS "("
    write $ intercalate "," (map mangle args)
    write (")\n" :: String)
    indent $ for_ body renderStatement
    beginLineS "end"

luaKeywords :: [Text]
luaKeywords =
    [ "break"
    , "case"
    , "continue"
    , "do"
    , "else"
    , "end"
    , "for"
    , "function"
    , "if"
    , "in"
    , "local"
    , "repeat"
    , "return"
    , "then"
    , "until"
    , "while"
    , "yield"
    -- literals
    , "undefined"
    , "null"
    , "not"
    , "true"
    , "false"
    ]

renderStatement :: Statement -> JSWriter ()
renderStatement = \case
    SBlock s -> do
        beginLineS "do\n"
        indent $ for_ s renderStatement
        beginLineS "end\n"

    SVar name maybeExpr -> do
        beginLineS "local "
        trace (show ("howdy"::String, name, mangle name)) $ return ()
        write $ mangle name
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
        writeS "\n"
    SReturn expr -> do
        beginLineS "return "
        for_ expr renderExpr
        writeS ";\n"
    SBreak ->
        beginLineS "break;\n"
    SIf expr thenStmt elseStatement -> do
        beginLineS "if "
        renderExpr expr
        case thenStmt of
            SBlock body -> do
                writeS " then\n"
                indent $ for_ body renderStatement
            _ -> do
                writeS " then\n"
                indent $ renderStatement thenStmt
        case elseStatement of
            Nothing -> beginLineS "end\n"
            Just (SBlock body) -> do
                beginLineS "else\n"
                indent $ for_ body renderStatement
                beginLineS "end\n"
            Just es -> do
                beginLineS "else\n"
                indent $ renderStatement es
                beginLineS "end\n"
    SWhile expr body -> do
        beginLineS "while "
        renderExpr expr
        writeS " do\n"
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

renderLuaName :: Text -> Text
renderLuaName n = if elem n luaKeywords
    then n <> "__"
    else n

renderLuaOperator :: Text -> Text
renderLuaOperator op
    | op == "===" = "=="
    | op == "!==" = "~="
    | op == "==" = "=="
    | op == "!=" = "~="
    | op == "||" = "or"
    | op == "&&" = "and"
    | op == "!" = "not"
    | otherwise = op

mangle :: Text -> Text
mangle name = renderLuaName $ Text.replace "$" "_" $ Text.replace "_" "__" name

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
            write $ mangle key
            writeS "="
            renderExpr value
        writeS "}"
    EPrefixOp op arg -> do
        writeS "("
        write $ renderLuaOperator op
        renderExpr arg
        writeS ")"
    EBinOp op lhs rhs -> do
        writeS "("
        renderExpr lhs
        write $ renderLuaOperator op
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
    EIdentifier n -> write $ mangle n
    EArray els -> do
        writeS "{"
        commaSeparated $ map renderExpr els
        writeS "}"
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

iife :: [Statement] -> Expression
iife body = EApplication (EFunction [] body) []

generateLua :: Text -> Gen.Program -> Text
generateLua rtsSource modules = runST $ do
    counter <- newSTRef 0
    globalExports <- newSTRef []
    allStatements <- for modules $ \(moduleName, decls) -> do
        let exportedValueNames = mconcat $ fmap JSBackend.getExportedValues decls
        -- The last generated module is main, so remember its exports.
        writeSTRef globalExports [(n, JSBackend.renderExportName moduleName n) | n <- exportedValueNames]
        let declareExports = [JS.SVar (JSBackend.renderExportName moduleName n) Nothing | n <- exportedValueNames]
        body <- runReaderT (JSBackend.generateModule' decls) (moduleName, counter)
        let setExports = [JS.SAssign (JS.EIdentifier $ JSBackend.renderExportName moduleName n) (JS.EIdentifier $ renderLuaName $ snd n) | n <- exportedValueNames]
        return $ declareExports ++ [JS.SBlock (body ++ setExports)]

    -- TODO: introduce an SRaw
    let prefix = JS.SExpression $ JS.ERaw ""
    globalExports' <- readSTRef globalExports
    let suffix = [
            (JS.SAssign (JS.ELookup (JS.EIdentifier "_rts_exports") (mangle exportName)) (JS.EIdentifier renderedExportName))
            | ((_exportType, exportName), renderedExportName) <- globalExports' ]

    return $ renderDocument
        [SBlock $ (prefix : mconcat allStatements) <> suffix]
