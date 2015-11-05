{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}

module Crux.Parse where

import           Crux.Prelude
import           Control.Applicative ((<|>))
import           Crux.AST            as AST
import qualified Crux.JSTree         as JSTree
import           Crux.Text           (isCapitalized)
import           Crux.Tokens         as Tokens
import qualified Data.HashMap.Strict as HashMap
import qualified Text.Parsec         as P

type Parser = P.ParsecT [Token Pos] () IO
type ParseData = Pos
type ParseExpression = Expression Name ParseData
type ParseDeclaration = DeclarationType Name ParseData

getToken :: P.Stream s m (Token Pos)
         => (Token Pos -> Maybe a) -> P.ParsecT s u m a
getToken predicate = P.tokenPrim showTok nextPos predicate
  where
    showTok = show
    nextPos pos t _ =
        let Pos{..} = tokenData t
        in P.setSourceLine (P.setSourceColumn pos posCol) posLine

anyToken :: Parser (Token Pos)
anyToken = getToken Just

token :: TokenType -> Parser (Token Pos)
token expected = getToken testTok
  where
    testTok tok@(Token _ ttype)
        | expected == ttype = Just tok
        | otherwise = Nothing

identifier :: Text -> Parser (Token Pos)
identifier name = getToken testTok
  where
    testTok tok@(Token _ ttype) = case ttype of
        TIdentifier t | t == name -> Just tok
        _ -> Nothing

anyIdentifier :: Parser Text
anyIdentifier = getToken testTok
  where
    testTok (Token _ ttype) = case ttype of
        TIdentifier t -> Just t
        _ -> Nothing

peekAndShow :: Show msg => msg -> Parser ()
peekAndShow msg = do
    peeked <- P.lookAhead $ P.anyToken
    liftIO $ print (msg, peeked)
    return ()

ifThenElseExpression :: Parser ParseExpression
ifThenElseExpression = do
    pr <- P.try $ token TIf
    condition <- noSemiExpression
    _ <- token TThen
    ifTrue <- noSemiExpression
    ifFalse <- P.option (ELiteral (tokenData pr) LUnit)
        (P.try (token TElse) >> noSemiExpression)
    return $ EIfThenElse (tokenData pr) condition ifTrue ifFalse

whileExpression :: Parser ParseExpression
whileExpression = do
    pr <- P.try $ token TWhile
    c <- noSemiExpression
    _ <- token TOpenBrace
    bodyExprs <- P.many expression
    _ <- token TCloseBrace

    let body = case bodyExprs of
            [] -> ELiteral (tokenData pr) LUnit
            _ ->  foldl1 (ESemi (tokenData pr)) bodyExprs

    return $ EWhile (tokenData pr) c body

returnExpression :: Parser ParseExpression
returnExpression = do
    pr <- P.try $ token TReturn
    rv <- noSemiExpression
    return $ EReturn (tokenData pr) rv

unitLiteralExpression :: Parser ParseExpression
unitLiteralExpression = do
    o <- token TOpenParen
    _ <- token TCloseParen
    return $ ELiteral (tokenData o) LUnit

literalExpression :: Parser ParseExpression
literalExpression = (P.try unitLiteralExpression) <|> P.tokenPrim showTok nextPos testTok
  where
    showTok = show
    nextPos pos _ _ = pos
    testTok (Token pos ttype) = case ttype of
        TInteger i -> Just $ ELiteral pos $ LInteger i
        TString s -> Just $ ELiteral pos $ LString s
        _ -> Nothing

parseInt :: Parser Integer
parseInt = P.tokenPrim show (\pos _ _ -> pos) test
  where
    test (Token _ ttype) = case ttype of
        TInteger i -> Just i
        _ -> Nothing

parseString :: Parser Text
parseString = P.tokenPrim show (\pos _ _ -> pos) test
  where
    test (Token _ ttype) = case ttype of
        TString s -> Just s
        _ -> Nothing

identifierExpression :: Parser ParseExpression
identifierExpression = getToken testTok
  where
    testTok (Token pos (TIdentifier txt)) = Just $ EIdentifier pos txt
    testTok _ = Nothing

functionExpression :: Parser ParseExpression
functionExpression = do
    tfun <- P.try $ token Tokens.TFun
    args <- parenthesized $ P.sepBy funArgument (token TComma)
    returnAnn <- P.optionMaybe $ do
        _ <- token TColon
        typeIdent
    _ <- token TOpenBrace
    bodyExprs <- P.many expression
    _ <- token TCloseBrace

    let body = case bodyExprs of
            [] -> ELiteral (tokenData tfun) LUnit
            _ -> foldl1 (ESemi (tokenData tfun)) bodyExprs

    return $ EFun (tokenData tfun) args returnAnn body

pattern :: Parser Pattern
pattern = do
    let parenPattern = do
            _ <- P.try $ token TOpenParen
            pat <- pattern
            _ <- token TCloseParen
            return pat

    parenPattern <|> noParenPattern

noParenPattern :: Parser Pattern
noParenPattern = do
    txt <- anyIdentifier
    if isCapitalized txt then do
        op <- P.optionMaybe $ token TOpenParen
        case op of
          Just _ -> do
            params <- delimited pattern (token TComma)
            _ <- token TCloseParen
            return $ PConstructor txt params
          Nothing -> do
            return $ PConstructor txt []
    else do
        return $ PPlaceholder txt

matchExpression :: Parser ParseExpression
matchExpression = do
    tmatch <- P.try (token TMatch)
    expr <- noSemiExpression
    _ <- token TOpenBrace
    cases <- P.many $ do
        pat <- pattern
        _ <- token TFatRightArrow
        ex <- noSemiExpression
        _ <- token TSemicolon
        return $ Case pat ex

    _ <- token TCloseBrace
    return $ EMatch (tokenData tmatch) expr cases

basicExpression :: Parser ParseExpression
basicExpression = identifierExpression <|> literalExpression <|> parenExpression

applicationExpression :: Parser ParseExpression
applicationExpression = do
    let app lhs = do
            argList <- parenthesized $ P.sepBy noSemiExpression (token TComma)
            return $ EApp (edata lhs) lhs argList
        prop lhs = do
            _ <- token TDot
            propName <- anyIdentifier
            return $ ELookup (edata lhs) lhs propName

    let go lhs = do
            mlhs' <- P.optionMaybe (app lhs <|> prop lhs)
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
    let op = (token TMultiply >> return BIMultiply) <|> (token TDivide >> return BIDivide)
    infixExpression op applicationExpression

addExpression :: Parser ParseExpression
addExpression = do
    let op = (token TPlus >> return BIPlus) <|> (token TMinus >> return BIMinus)
    infixExpression op multiplyExpression

relationExpression :: Parser ParseExpression
relationExpression = do
    let op =
            (token TLess >> return BILess)
            <|> (token TGreater >> return BIGreater)
            <|> (token TLessEqual >> return BILessEqual)
            <|> (token TGreaterEqual >> return BIGreaterEqual)
    infixExpression op addExpression

assignExpression :: Parser ParseExpression
assignExpression = do
    lhs <- P.try (applicationExpression <* token TEqual)
    rhs <- noSemiExpression
    return $ EAssign (edata lhs) lhs rhs

letExpression :: Parser ParseExpression
letExpression = do
    tlet <- P.try $ token TLet
    mut <- P.option LImmutable (token TMutable >> return LMutable)
    name <- anyIdentifier
    typeAnn <- P.optionMaybe $ do
        _ <- P.try $ token TColon
        typeIdent
    _ <- token TEqual
    expr <- noSemiExpression
    return $ ELet (tokenData tlet) mut name typeAnn expr

semiExpression :: Parser ParseExpression
semiExpression = do
    e <- noSemiExpression
    e2 <- P.optionMaybe (token TSemicolon *> semiExpression)
    case e2 of
        Nothing ->
            return e
        Just e2' ->
            return (ESemi (edata e) e e2')

parenExpression :: Parser ParseExpression
parenExpression = parenthesized $ P.try semiExpression

recordLiteralExpression :: Parser ParseExpression
recordLiteralExpression = do
    t <- token $ TOpenBrace
    let keyValuePair = do
            name <- anyIdentifier
            _ <- token $ TColon
            expr <- noSemiExpression
            return (name, expr)
    pairs <- P.sepBy keyValuePair $ token $ TComma
    _ <- token $ TCloseBrace

    return $ ERecordLiteral (tokenData t) (HashMap.fromList pairs)

noSemiExpression :: Parser ParseExpression
noSemiExpression =
    P.try letExpression
    <|> matchExpression
    <|> ifThenElseExpression
    <|> whileExpression
    <|> P.try returnExpression
    <|> P.try functionExpression
    <|> P.try recordLiteralExpression
    <|> assignExpression
    <|> relationExpression

expression :: Parser ParseExpression
expression = do
    s <- noSemiExpression
    _ <- token TSemicolon
    return s

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
            mut <- P.optionMaybe ((token TMutable *> pure LMutable) <|> (token TConst *> pure LImmutable))
            name <- anyIdentifier
            _ <- token TColon
            ty <- typeIdent
            return (name, mut, ty)

    _ <- P.try $ token TOpenBrace
    props <- delimited propTypePair (token TComma)
    _ <- P.optional $ token TComma
    _ <- P.try $ token TCloseBrace
    return $ RecordIdent props

functionTypeIdent :: Parser TypeIdent
functionTypeIdent = P.try $ do
    argTypes <- parenthesized $ P.sepBy typeIdent (token TComma)
    _ <- token TRightArrow
    retType <- typeIdent
    return $ FunctionIdent argTypes retType

parenthesized :: Parser a -> Parser a
parenthesized = P.between (token TOpenParen) (token TCloseParen)

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
    _ <- token TSemicolon
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
    _ <- P.try $ token TDeclare
    name <- anyIdentifier
    _ <- token TColon
    ti <- typeIdent
    _ <- token TSemicolon
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
    return $ DData name typeVars variants

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
    _ <- P.try $ token TJSFFI
    name <- typeName
    _ <- token TOpenBrace
    variants <- delimited jsVariantDefinition (token TComma)
    _ <- token TCloseBrace
    return $ DJSData name variants

dataDeclaration :: Parser ParseDeclaration
dataDeclaration = do
    _ <- P.try $ token TData
    jsDataDeclaration <|> cruxDataDeclaration

typeDeclaration :: Parser ParseDeclaration
typeDeclaration = do
    _ <- P.try $ token TType
    name <- typeName
    vars <- P.many typeVariableName
    _ <- token TEqual
    ty <- typeIdent
    _ <- token TSemicolon
    return $ DType $ TypeAlias name vars ty

funArgument :: Parser (Name, Maybe TypeIdent)
funArgument = do
    n <- anyIdentifier
    ann <- P.optionMaybe $ do
        _ <- token TColon
        typeIdent
    return (n, ann)

funDeclaration :: Parser ParseDeclaration
funDeclaration = do
    tfun <- P.try $ token Tokens.TFun
    name <- anyIdentifier
    params <- parenthesized $ P.sepBy funArgument (token TComma)
    returnAnn <- P.optionMaybe $ do
        _ <- token TColon
        typeIdent
    _ <- token TOpenBrace
    bodyExprs <- P.many expression
    _ <- token TCloseBrace

    let body = case bodyExprs of
            [] -> ELiteral (tokenData tfun) LUnit
            _ -> foldl1 (ESemi (tokenData tfun)) bodyExprs

    return $ DFun $ FunDef (tokenData tfun) name params returnAnn body

declaration :: Parser (Declaration Name ParseData)
declaration = do
    export <- P.optionMaybe $ token Tokens.TExport
    let exportFlag = case export of
            Just _ -> Export
            Nothing -> NoExport

    declType <- declareDeclaration <|> dataDeclaration <|> typeDeclaration <|> funDeclaration <|> letDeclaration
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

parse :: P.SourceName -> [Token Pos] -> IO (Either P.ParseError ParsedModule)
parse fileName tokens = P.runParserT parseModule () fileName tokens
