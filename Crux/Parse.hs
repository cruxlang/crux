{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections, LambdaCase #-}

module Crux.Parse where

import           Control.Applicative ((<|>))
import           Crux.AST            as AST
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (MonadReader, ask, local)
import qualified Crux.JSTree         as JSTree
import           Crux.Prelude
import           Crux.Text           (isCapitalized)
import           Crux.Tokens         as Tokens
import qualified Data.HashMap.Strict as HashMap
import qualified Text.Parsec         as P

type Parser = P.ParsecT [Token Pos] () (ReaderT (ModuleName, IndentReq) IO)
type ParseData = Pos
type ParseExpression = Expression UnresolvedReference ParseData
type ParseDeclaration = DeclarationType UnresolvedReference ParseData

data IndentReq
    = AnyIndent
    | LeftMost
    | IndentPast (Token Pos)
    | CloseSymbol (Token Pos)
    deriving (Show)

data IndentMatch
    = IndentOK
    | UnexpectedIndent String
    | UnexpectedDedent String

indentationPredicate :: Pos -> IndentReq -> IndentMatch
indentationPredicate p = \case
    AnyIndent -> IndentOK
    LeftMost -> if 0 == posLineStart p
        then IndentOK
        else UnexpectedIndent $ "Expected LeftMost but got " ++ show (posLineStart p)
    IndentPast t -> let tPos = tokenData t in
        if posLine tPos == posLine p || posLineStart p > posLineStart tPos
            then IndentOK
            else UnexpectedDedent $ "Expected indentation past " ++ show t ++ " p = " ++ show p
    CloseSymbol t -> let tPos = tokenData t in
        if posLine tPos == posLine p || posLineStart p >= posLineStart tPos
            then IndentOK
            else UnexpectedDedent $ "Expected indentation at or past " ++ show t ++ " p = " ++ show p

readModuleName :: MonadReader (a, b) f => f a
readModuleName = fmap fst ask

readIndentReq :: MonadReader (a, b) f => f b
readIndentReq = fmap snd ask

-- Temporarily adjust the indentation policy for a parser
withIndentation :: MonadReader (a, b) m => b -> m r -> m r
withIndentation i = local $ \(mn, _) -> (mn, i)

enclosed :: Parser (Token Pos) -> Parser (Token Pos) -> Parser a -> Parser a
enclosed open close body = do
    openToken <- open
    rv <- body
    withIndentation (CloseSymbol openToken) $ do
        void $ close
    return rv

parenthesized :: Parser a -> Parser a
parenthesized = enclosed (token TOpenParen) (token TCloseParen)

braced :: Parser a -> Parser a
braced = enclosed (token TOpenBrace) (token TCloseBrace)

bracketed :: Parser a -> Parser a
bracketed = enclosed (token TOpenBracket) (token TCloseBracket)

getToken :: P.Stream s m (Token Pos)
         => (Token Pos -> Maybe a) -> P.ParsecT s u m a
getToken predicate = P.tokenPrim show nextPos predicate
  where
    nextPos pos t _ =
        -- This appears to be incorrect logic per the docs, but our errors all
        -- have correct line numbers...
        let Pos{..} = tokenData t
        in P.setSourceLine (P.setSourceColumn pos posCol) posLine

tokenBy :: (TokenType -> Maybe a) -> Parser (a, Token Pos)
tokenBy predicate = do
    indentReq <- readIndentReq
    let testTok tok@(Token pos ttype) = case predicate ttype of
          Just r -> Just (r, tok, indentationPredicate pos indentReq)
          Nothing -> Nothing
    (r, tok, indentMatch) <- getToken testTok
    case indentMatch of
        IndentOK -> return (r, tok)
        UnexpectedIndent msg -> fail $ "Unexpected indent: " ++ msg
        UnexpectedDedent msg -> fail $ "Unexpected dedent: " ++ msg

token :: TokenType -> Parser (Token Pos)
token expected = do
    ((), t) <- tokenBy $ \t ->
        if expected == t then Just () else Nothing
    return t

identifier :: Text -> Parser (Token Pos)
identifier name = fmap snd $ tokenBy $ \case
    TUpperIdentifier t | t == name -> Just ()
    TLowerIdentifier t | t == name -> Just ()
    _ -> Nothing

lowerIdentifier :: Parser (Pos, Text)
lowerIdentifier = do
    (t, Token pos _) <- tokenBy $ \case
        TLowerIdentifier t -> Just t
        _ -> Nothing
    return (pos, t)

upperIdentifier :: Parser (Pos, Text)
upperIdentifier = do
    (t, Token pos _) <- tokenBy $ \case
        TUpperIdentifier t -> Just t
        _ -> Nothing
    return (pos, t)

anyIdentifierWithPos :: Parser (Pos, Text)
anyIdentifierWithPos = do
    lowerIdentifier <|> upperIdentifier

anyIdentifier :: Parser Text
anyIdentifier = do
    (_, txt) <- anyIdentifierWithPos
    return txt

ifThenElseExpression :: Parser ParseExpression
ifThenElseExpression = do
    pr <- token TIf
    condition <- noSemiExpression
    -- TODO: do we need to enforce indentation here?
    let exprIf = do
            _ <- token TThen
            ifTrue <- noSemiExpression
            _ <- token TElse
            ifFalse <- noSemiExpression
            return $ EIfThenElse (tokenData pr) condition ifTrue ifFalse
    let blockIf = do
            ifTrue <- blockExpression
            ifFalse <- P.option (ELiteral (tokenData pr) LUnit) $ do
                _ <- token TElse
                blockExpression <|> ifThenElseExpression
            return $ EIfThenElse (tokenData pr) condition ifTrue ifFalse

    exprIf <|> blockIf

whileExpression :: Parser ParseExpression
whileExpression = do
    pr <- token TWhile
    c <- noSemiExpression
    body <- blockExpression
    return $ EWhile (tokenData pr) c body

forExpression :: Parser ParseExpression
forExpression = do
    pr <- token TFor
    pat <- anyIdentifier
    _ <- token TIn
    iter <- noSemiExpression
    body <- blockExpression
    return $ EFor (tokenData pr) pat iter body

returnExpression :: Parser ParseExpression
returnExpression = do
    pr <- token TReturn
    rv <- noSemiExpression
    return $ EReturn (tokenData pr) rv

unitLiteralExpression :: Parser ParseExpression
unitLiteralExpression = do
    o <- P.try $ token TOpenParen <* token TCloseParen
    return $ ELiteral (tokenData o) LUnit

arrayLiteralExpression :: Parser ParseExpression
arrayLiteralExpression = do
    t <- token TOpenBracket
    exprs <- delimited noSemiExpression (token TComma)
    _ <- token TCloseBracket
    return $ EArrayLiteral (tokenData t) exprs

recordLiteralExpression :: Parser ParseExpression
recordLiteralExpression = do
    t <- token TOpenBrace
    let keyValuePair = do
            name <- anyIdentifier
            _ <- token TColon
            expr <- noSemiExpression
            return (name, expr)
    pairs <- P.sepBy keyValuePair $ token TComma
    _ <- token TCloseBrace

    return $ ERecordLiteral (tokenData t) (HashMap.fromList pairs)

integerLiteralExpression :: Parser ParseExpression
integerLiteralExpression = do
    (i, Token pos _) <- tokenBy $ \case
        TInteger i -> Just i
        _ -> Nothing
    return $ ELiteral pos $ LInteger i

stringLiteralExpression :: Parser ParseExpression
stringLiteralExpression = do
    (s, Token pos _) <- tokenBy $ \case
        TString s -> Just s
        _ -> Nothing
    return $ ELiteral pos $ LString s

literalExpression :: Parser ParseExpression
literalExpression =
    unitLiteralExpression <|>
    arrayLiteralExpression <|>
    recordLiteralExpression <|>
    functionExpression <|>
    -- TODO: fix tokenBy, make this primitiveLiteralExpression
    integerLiteralExpression <|>
    stringLiteralExpression

parseInt :: Parser Integer
parseInt = do
    ELiteral _ (LInteger i) <- integerLiteralExpression
    return i

parseString :: Parser Text
parseString = do
    ELiteral _ (LString s) <- stringLiteralExpression
    return s

identifierExpression :: Parser ParseExpression
identifierExpression = do
    (pos, txt) <- anyIdentifierWithPos
    return $ EIdentifier pos $ UnknownReference txt

functionExpression :: Parser ParseExpression
functionExpression = do
    tfun <- token Tokens.TFun
    args <- parenthesized $ P.sepBy funArgument (token TComma)
    returnAnn <- P.optionMaybe $ do
        _ <- token TColon
        typeIdent
    body <- blockExpression
    return $ EFun (tokenData tfun) args returnAnn body

pattern :: Parser RefutablePattern
pattern = do
    let parenPattern = parenthesized pattern
    parenPattern <|> noParenPattern

noParenPattern :: Parser RefutablePattern
noParenPattern = do
    txt <- anyIdentifier
    if isCapitalized txt then do
        -- TODO: port this to use parenthesized <|> ...
        op <- P.optionMaybe $ token TOpenParen
        case op of
          Just _ -> do
            params <- delimited pattern (token TComma)
            _ <- token TCloseParen
            return $ RPConstructor txt params
          Nothing -> do
            return $ RPConstructor txt []
    else do
        return $ RPIrrefutable $ PBinding txt

matchExpression :: Parser ParseExpression
matchExpression = do
    tmatch <- token TMatch
    expr <- noSemiExpression
    cases <- braced $ P.many $ do
        pat <- pattern
        _ <- token TFatRightArrow
        ex <- noSemiExpression
        _ <- token TSemicolon
        return $ Case pat ex
    return $ EMatch (tokenData tmatch) expr cases

basicExpression :: Parser ParseExpression
basicExpression =
    identifierExpression
    <|> literalExpression
    <|> parenExpression

applicationExpression :: Parser ParseExpression
applicationExpression = do
    let app lhs = do
            argList <- parenthesized $ P.sepBy noSemiExpression (token TComma)
            return $ EApp (edata lhs) lhs argList
        methodApp lhs = do
            _ <- token TRightArrow
            methodName <- anyIdentifier
            argList <- parenthesized $ P.sepBy noSemiExpression (token TComma)
            return $ EMethodApp (edata lhs) lhs methodName argList
        prop lhs = do
            _ <- token TDot
            propName <- anyIdentifier
            return $ ELookup (edata lhs) lhs propName

    let go lhs = do
            mlhs' <- P.optionMaybe $ P.choice
                [ app lhs
                , methodApp lhs
                , prop lhs
                ]
            case mlhs' of
                Nothing -> return lhs
                Just lhs' -> go lhs'

    lhs <- basicExpression
    go lhs

infixExpression :: Parser BinIntrinsic -> Parser ParseExpression -> Parser ParseExpression
infixExpression operator term = do
    first <- term
    rest <- P.many $ do
        t <- operator
        fmap (t,) term

    let foldIt acc [] = acc
        foldIt acc ((binopType, next):terms) =
            foldIt (AST.EBinIntrinsic (edata next) binopType acc next) terms

    return $ foldIt first rest

multiplyExpression :: Parser ParseExpression
multiplyExpression = do
    let op =
            (token TMultiply >> return BIMultiply)
            <|> (token TDivide >> return BIDivide)
    infixExpression op applicationExpression

addExpression :: Parser ParseExpression
addExpression = do
    let op =
            (token TPlus >> return BIPlus)
            <|> (token TMinus >> return BIMinus)
    infixExpression op multiplyExpression

relationExpression :: Parser ParseExpression
relationExpression = do
    let op =
            (token TDoubleEqual >> return BIEqual)
            <|> (token TNotEqual >> return BINotEqual)
            <|> (token TLess >> return BILess)
            <|> (token TGreater >> return BIGreater)
            <|> (token TLessEqual >> return BILessEqual)
            <|> (token TGreaterEqual >> return BIGreaterEqual)
    infixExpression op addExpression

booleanExpression :: Parser ParseExpression
booleanExpression = do
    let op = (token TAndAnd >> return BAnd)
            <|> (token TOrOr >> return BOr)
    infixExpression op relationExpression

assignExpression :: Parser ParseExpression
assignExpression = do
    lhs <- P.try (applicationExpression <* token TEqual)
    rhs <- noSemiExpression
    return $ EAssign (edata lhs) lhs rhs

irrefutablePattern :: Parser Pattern
irrefutablePattern = do
    _ <- token TWildcard
    return PWildcard

letExpression :: Parser ParseExpression
letExpression = do
    tlet <- token TLet
    withIndentation (IndentPast tlet) $ do
        mut <- P.option LImmutable (token TMutable >> return LMutable)
        pat <- irrefutablePattern <|> fmap (PBinding . snd) lowerIdentifier
        typeAnn <- P.optionMaybe $ do
            _ <- token TColon
            typeIdent
        _ <- token TEqual
        expr <- noSemiExpression
        return $ ELet (tokenData tlet) mut pat typeAnn expr

parenExpression :: Parser ParseExpression
parenExpression = parenthesized $ do
    first <- noSemiExpression
    rest <- P.many (token TSemicolon >> noSemiExpression)
    return $ foldl' (\a b -> ESemi (edata a) a b) first rest

noSemiExpression :: Parser ParseExpression
noSemiExpression =
    letExpression
    <|> matchExpression
    <|> ifThenElseExpression
    <|> whileExpression
    <|> forExpression
    <|> returnExpression
    <|> assignExpression
    <|> booleanExpression

-- Parse elements separated by a delimiter.  Differs from P.many in that a dangling delimiter is permitted.
delimited :: Parser a -> Parser b -> Parser [a]
delimited parseElement delim = do
    let go acc = do
            d <- P.optionMaybe delim
            case d of
                Nothing -> return acc
                Just _ -> do
                    next <- P.optionMaybe parseElement
                    case next of
                        Nothing -> return acc
                        Just n -> go (n:acc)

    first <- P.optionMaybe parseElement
    case first of
        Nothing -> return []
        Just f -> fmap reverse $ go [f]

recordTypeIdent :: Parser TypeIdent
recordTypeIdent = do
    let propTypePair = do
            mut <- P.optionMaybe (
                (token TMutable *> pure LMutable) <|>
                (token TConst *> pure LImmutable))
            name <- anyIdentifier
            _ <- token TColon
            ty <- typeIdent
            return (name, mut, ty)

    props <- braced $ do
        props <- delimited propTypePair (token TComma)
        _ <- P.optional $ token TComma
        return props
    return $ RecordIdent props

functionTypeIdent :: Parser TypeIdent
functionTypeIdent = do
    argTypes <- parenthesized $ P.sepBy typeIdent (token TComma)
    _ <- token TRightArrow
    retType <- typeIdent
    return $ FunctionIdent argTypes retType

typeIdent :: Parser TypeIdent
typeIdent =
    let justOne = do
            name <- anyIdentifier
            return $ TypeIdent name []
        sumIdent = do
            name <- anyIdentifier
            params <- P.many (parenthesized sumIdent <|> justOne)
            return $ TypeIdent name params
    in functionTypeIdent <|> sumIdent <|> recordTypeIdent

typeName :: Parser Text
typeName = do
    name <- anyIdentifier
    when (not $ isCapitalized name) $ P.parserFail $ "Expected type name, but got " <> (show name)
    return name

typeVariableName :: Parser Text
typeVariableName = do
    name <- anyIdentifier
    when (isCapitalized name) $ P.parserFail $ "Expected type variable name but got " <> (show name)
    return name

letDeclaration :: Parser ParseDeclaration
letDeclaration = do
    ELet ed mut name typeAnn expr <- letExpression
    return $ DLet ed mut name typeAnn expr

-- typeident = list of types
-- paren (list of things)

singleTypeIdent :: Parser TypeIdent
singleTypeIdent = do
    name <- anyIdentifier
    return $ TypeIdent name []

typeIdent' :: Parser TypeIdent
typeIdent' = parenthesized dataDeclTypeIdent <|> singleTypeIdent

declareDeclaration :: Parser ParseDeclaration
declareDeclaration = do
    declareToken <- token TDeclare
    withIndentation (IndentPast declareToken) $ do
        name <- anyIdentifier
        _ <- token TColon
        ti <- typeIdent
        return $ DDeclare name ti

-- TODO: there is wrongness here -- ((Maybe) (Int)) should parse as Maybe Int
dataDeclTypeIdent :: Parser TypeIdent
dataDeclTypeIdent = do
    name <- anyIdentifier
    params <- P.many typeIdent'
    return $ TypeIdent name params

variantDefinition :: Parser Variant
variantDefinition = do
    ctorname <- anyIdentifier

    -- TODO: port this to parenthesized <|>
    op <- P.optionMaybe $ token TOpenParen
    ctordata <- case op of
        Just _ -> do
            cd <- delimited dataDeclTypeIdent (token TComma)
            _ <- token TCloseParen
            return cd
        Nothing -> do
            return []

    return $ Variant ctorname ctordata

cruxDataDeclaration :: Parser ParseDeclaration
cruxDataDeclaration = do
    name <- typeName
    typeVars <- P.many typeVariableName

    _ <- token TOpenBrace
    variants <- delimited variantDefinition (token TComma)
    _ <- token TCloseBrace
    moduleName <- readModuleName
    return $ DData name moduleName typeVars variants

jsValue :: Parser JSTree.Literal
jsValue =
    (fmap (const JSTree.LUndefined) $ identifier "undefined") <|>
    (fmap (const JSTree.LNull) $ identifier "null") <|>
    (fmap (const JSTree.LTrue) $ identifier "true") <|>
    (fmap (const JSTree.LFalse) $ identifier "false") <|>
    (fmap JSTree.LInteger $ parseInt) <|>
    (fmap JSTree.LString $ parseString)

jsVariantDefinition :: Parser JSVariant
jsVariantDefinition = do
    ctorname <- anyIdentifier
    _ <- token TEqual
    ctorvalue <- jsValue
    return $ JSVariant ctorname ctorvalue

jsDataDeclaration :: Parser ParseDeclaration
jsDataDeclaration = do
    _ <- token TJSFFI
    name <- typeName
    _ <- token TOpenBrace
    variants <- delimited jsVariantDefinition (token TComma)
    _ <- token TCloseBrace
    moduleName <- readModuleName
    return $ DJSData name moduleName variants

dataDeclaration :: Parser ParseDeclaration
dataDeclaration = do
    _ <- token TData
    withIndentation AnyIndent $
        jsDataDeclaration <|> cruxDataDeclaration

typeDeclaration :: Parser ParseDeclaration
typeDeclaration = do
    typeToken <- token TType
    withIndentation (IndentPast typeToken) $ do
        name <- typeName
        vars <- P.many typeVariableName
        _ <- token TEqual
        ty <- typeIdent
        return $ DType $ TypeAlias name vars ty

funArgument :: Parser (Name, Maybe TypeIdent)
funArgument = do
    n <- anyIdentifier
    ann <- P.optionMaybe $ do
        _ <- token TColon
        typeIdent
    return (n, ann)

blockExpression :: Parser ParseExpression
blockExpression = do
    br <- token TOpenBrace
    body <- P.many $ noSemiExpression <* token TSemicolon
    withIndentation AnyIndent $ do
        void $ token TCloseBrace
    return $ case body of
        [] -> ELiteral (tokenData br) LUnit
        _ -> foldl1 (ESemi (tokenData br)) body

funDeclaration :: Parser ParseDeclaration
funDeclaration = do
    tfun <- token Tokens.TFun
    withIndentation AnyIndent $ do
        name <- anyIdentifier
        params <- parenthesized $ P.sepBy funArgument (token TComma)
        returnAnn <- P.optionMaybe $ do
            _ <- token TColon
            typeIdent

        body <- blockExpression
        return $ DFun $ FunDef (tokenData tfun) name params returnAnn body

declaration :: Parser (Declaration UnresolvedReference ParseData)
declaration = do
    export <- P.optionMaybe $ token Tokens.TExport
    let exportFlag = case export of
            Just _ -> Export
            Nothing -> NoExport

    declType <- declareDeclaration
            <|> dataDeclaration
            <|> typeDeclaration
            <|> funDeclaration
            <|> letDeclaration
    return $ Declaration exportFlag declType

importDecl :: Parser Import
importDecl = do
    mname <- P.sepBy1 anyIdentifier (token TDot)
    _ <- parenthesized $ token TEllipsis
    let prefix = fmap ModuleSegment $ init mname
    let name = ModuleSegment $ last mname
    return $ UnqualifiedImport $ ModuleName prefix name

imports :: Parser [Import]
imports = do
    _ <- token TImport
    withIndentation AnyIndent $ do
        _ <- token TOpenBrace
        imp <- delimited importDecl (token TComma)
        _ <- token TCloseBrace
        return imp

parseModule :: Parser ParsedModule
parseModule = do
    imp <- P.option [] imports
    doc <- P.many $ declaration
    P.eof
    return Module
        { mImports = imp
        , mDecls = doc
        }

runParser :: Parser a -> ModuleName -> P.SourceName -> [Token Pos] -> IO (Either P.ParseError a)
runParser parser moduleName sourceName tokens = do
    (flip runReaderT) (moduleName, LeftMost) $ do
        P.runParserT parser () sourceName tokens

parse :: ModuleName -> P.SourceName -> [Token Pos] -> IO (Either P.ParseError ParsedModule)
parse = runParser parseModule
