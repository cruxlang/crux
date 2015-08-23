{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}

module Crux.Parse where

import Crux.Prelude
import           Control.Applicative ((<|>))
import           Crux.AST            as AST
import           Crux.Text           (isCapitalized)
import           Crux.Tokens         as Tokens
import qualified Data.HashMap.Strict as HashMap
import qualified Text.Parsec         as P
import qualified Crux.JSTree         as JSTree

type Parser = P.ParsecT [Token Pos] () IO
type ParseData = Pos
type ParseExpression = Expression ParseData
type ParseDeclaration = DeclarationType ParseData
type ParseModule = Module ModuleName ParseData

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
    _ <- token TOpenParen
    args <- P.sepBy anyIdentifier (token TComma)
    _ <- token TCloseParen
    _ <- token TOpenBrace
    bodyExprs <- P.many expression
    _ <- token TCloseBrace

    let body = case bodyExprs of
            [] -> ELiteral (tokenData tfun) LUnit
            _ -> foldl1 (ESemi (tokenData tfun)) bodyExprs

    return $ EFun (tokenData tfun) args body

pattern :: Parser Pattern
pattern =
    let parenPattern = do
            _ <- P.try $ token TOpenParen
            pat <- pattern
            _ <- token TCloseParen
            return pat

    in parenPattern <|> noParenPattern

noParenPattern :: Parser Pattern
noParenPattern = do
    txt <- anyIdentifier
    if isCapitalized txt
        then do
            patternArgs <- P.many pattern
            return $ PConstructor txt patternArgs
        else
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

lookupExpression :: Parser ParseExpression
lookupExpression = do
    lhs <- basicExpression
    rhs <- P.many $ do
        _ <- token TDot
        anyIdentifier

    case rhs of
        [] -> return lhs
        _ -> return $ foldl' (\acc name -> ELookup (edata lhs) acc name) lhs rhs

applicationExpression :: Parser ParseExpression
applicationExpression = do
    lhs <- lookupExpression

    argList <- P.optionMaybe $ do
        _ <- token TOpenParen
        args <- P.sepBy noSemiExpression (token TComma)
        _ <- token TCloseParen
        return args

    case argList of
        Nothing -> return lhs
        Just args -> return $ EApp (edata lhs) lhs args

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

letExpression :: Parser ParseExpression
letExpression = do
    tlet <- P.try $ token TLet
    name <- anyIdentifier
    typeAnn <- P.optionMaybe $ do
        _ <- P.try $ token TColon
        typeIdent
    _ <- token TEqual
    expr <- noSemiExpression
    return (ELet (tokenData tlet) name typeAnn expr)

semiExpression :: Parser ParseExpression
semiExpression = do
    e <- noSemiExpression
    _ <- token TSemicolon
    e2 <- noSemiExpression
    return $ ESemi (edata e) e e2

parenExpression :: Parser ParseExpression
parenExpression = do
    _ <- token $ TOpenParen
    e <- P.try semiExpression <|> noSemiExpression
    _ <- token $ TCloseParen
    return e

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
    <|> P.try ifThenElseExpression
    <|> P.try returnExpression
    <|> P.try functionExpression
    <|> P.try recordLiteralExpression
    <|> addExpression

expression :: Parser ParseExpression
expression = do
    s <- noSemiExpression
    _ <- token TSemicolon
    return s

recordTypeIdent :: Parser TypeIdent
recordTypeIdent =
    let propTypePair = do
            name <- anyIdentifier
            _ <- token TColon
            ty <- typeIdent
            return (name, ty)
    in do
        _ <- P.try $ token TOpenBrace
        props <- P.sepBy propTypePair (token TComma)
        _ <- P.optional $ token TComma
        _ <- P.try $ token TCloseBrace
        return $ RecordIdent props

functionTypeIdent :: Parser TypeIdent
functionTypeIdent = P.try $ do
    _ <- token TOpenParen
    argTypes <- P.sepBy typeIdent (token TComma)
    _ <- token TCloseParen
    _ <- token TRightArrow
    retType <- typeIdent
    return $ FunctionIdent argTypes retType

dataDeclTypeIdent :: Parser TypeIdent
dataDeclTypeIdent =
    let parenthesized =
            P.between (P.try $ token TOpenParen) (token TCloseParen) sumIdent
        justOne = do
            name <- anyIdentifier
            return $ TypeIdent name []
        sumIdent = do
            name <- anyIdentifier
            params <- P.many (parenthesized <|> justOne)
            return $ TypeIdent name params
    in recordTypeIdent <|> parenthesized <|> justOne

typeIdent :: Parser TypeIdent
typeIdent =
    let parenthesized = do
            _ <- P.try $ token TOpenParen
            r <- sumIdent
            _ <- token TCloseParen
            return r
        justOne = do
            name <- anyIdentifier
            return $ TypeIdent name []
        sumIdent = do
            name <- anyIdentifier
            params <- P.many (parenthesized <|> justOne)
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
    ELet ed name typeAnn expr <- letExpression
    return $ DLet ed name typeAnn expr

variantDefinition :: Parser Variant
variantDefinition = do
    ctorname <- anyIdentifier
    ctordata <- P.many dataDeclTypeIdent
    _ <- token TSemicolon
    return $ Variant ctorname ctordata

cruxDataDeclaration :: Parser ParseDeclaration
cruxDataDeclaration = do
    name <- typeName
    typeVars <- P.many typeVariableName

    _ <- token TOpenBrace
    variants <- P.many variantDefinition
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
    _ <- token TSemicolon
    return $ JSVariant ctorname ctorvalue

jsDataDeclaration :: Parser ParseDeclaration
jsDataDeclaration = do
    _ <- P.try $ token TJSFFI
    name <- typeName
    _ <- token TOpenBrace
    variants <- P.many jsVariantDefinition
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
    return $ DType $ TypeAlias name vars ty

funDeclaration :: Parser ParseDeclaration
funDeclaration = do
    tfun <- P.try $ token Tokens.TFun
    name <- anyIdentifier
    _ <- token TOpenParen
    params <- P.sepBy anyIdentifier (token TComma)
    _ <- token TCloseParen
    _ <- token TOpenBrace
    bodyExprs <- P.many expression
    _ <- token TCloseBrace

    let body = case bodyExprs of
            [] -> ELiteral (tokenData tfun) LUnit
            _ -> foldl1 (ESemi (tokenData tfun)) bodyExprs

    return $ DFun $ FunDef (tokenData tfun) name params body

declaration :: Parser (Declaration ParseData)
declaration = do
    export <- P.optionMaybe $ token Tokens.TExport
    let exportFlag = case export of
            Just _ -> Export
            Nothing -> NoExport

    declType <- dataDeclaration <|> typeDeclaration <|> funDeclaration <|> letDeclaration
    return $ Declaration exportFlag declType

parseModule :: Parser ParseModule
parseModule = do
    doc <- P.many1 $ declaration <* token TSemicolon
    P.eof
    return Module
        { mImports = []
        , mDecls = doc
        }

parse :: P.SourceName -> [Token Pos] -> IO (Either P.ParseError ParseModule)
parse fileName tokens = P.runParserT parseModule () fileName tokens
