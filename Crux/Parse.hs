{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections, LambdaCase #-}

module Crux.Parse where

import           Control.Applicative ((<|>))
import           Crux.AST            as AST
import Control.Monad.Trans.Reader (Reader, runReader)
import Control.Monad.Reader.Class (MonadReader, ask, local)
import qualified Crux.JSTree         as JSTree
import           Crux.Prelude
import           Crux.Text           (isCapitalized)
import Crux.Tokens         as Tokens
import qualified Data.HashMap.Strict as HashMap
import qualified Text.Parsec         as P

type Parser = P.ParsecT [Token Pos] () (Reader (ModuleName, IndentReq))
type ParseData = Pos
type ParseExpression = Expression UnresolvedReference ParseData
type ParseDeclaration = DeclarationType UnresolvedReference ParseData

data IndentReq
    = IRLeftMost
    -- TODO: we can kill this if the appropriate parsers always returned the initial token
    | IRAtColumn Pos
    | IRDeeper (Token Pos)
    | IRAtOrDeeper (Token Pos)
    deriving (Show)

data IndentMatch
    = IndentOK
    | UnexpectedIndent String
    | UnexpectedDedent String

indentationPredicate :: Pos -> IndentReq -> IndentMatch
indentationPredicate p = \case
    IRLeftMost -> if 1 == posLineStart p
        then IndentOK
        else UnexpectedIndent $ "Expected LeftMost but got " ++ show (posLineStart p)
    IRAtColumn indent -> if posLine p == posLine indent || posLineStart p == posLineStart indent
        then IndentOK
        else if posCol p > posLineStart indent
            then UnexpectedIndent $ "Expected indentation at " ++ show indent ++ " but it was " ++ show p
            else UnexpectedDedent $ "Expected indentation at " ++ show indent ++ " but it was " ++ show p
    IRDeeper t -> let tPos = tokenData t in
        if posLine tPos == posLine p || posCol p > posLineStart tPos
            then IndentOK
            else UnexpectedDedent $ "Expected indentation past " ++ show t ++ " p = " ++ show p
    IRAtOrDeeper t -> let tPos = tokenData t in
        if posLine tPos == posLine p || posCol p >= posLineStart tPos
            then IndentOK
            else UnexpectedDedent $ "Expected indentation at or past " ++ show t ++ " p = " ++ show p

readModuleName :: MonadReader (a, b) f => f a
readModuleName = fmap fst ask

readIndentReq :: MonadReader (a, b) f => f b
readIndentReq = fmap snd ask

-- Temporarily adjust the indentation policy for a parser
withIndentation :: MonadReader (a, b) m => b -> m r -> m r
withIndentation i = local $ \(mn, _) -> (mn, i)

enclosed :: Parser (Token Pos) -> Parser b -> Parser a -> Parser (Pos, a)
enclosed open close body = do
    openToken <- open
    rv <- body
    withIndentation (IRAtOrDeeper openToken) $ do
        void $ close
    return (tokenData openToken, rv)

parenthesized' :: Parser a -> Parser (Pos, a)
parenthesized' = enclosed (token TOpenParen) (token TCloseParen)

parenthesized :: Parser a -> Parser a
parenthesized p = fmap snd $ parenthesized' p

braced' :: Parser a -> Parser (Pos, a)
braced' = enclosed (token TOpenBrace) (token TCloseBrace)

braced :: Parser a -> Parser a
braced p = fmap snd $ braced' p

bracketed' :: Parser a -> Parser (Pos, a)
bracketed' = enclosed (token TOpenBracket) (token TCloseBracket)

bracketed :: Parser a -> Parser a
bracketed p = fmap snd $ bracketed' p

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
    condition <- withIndentation (IRDeeper pr) noSemiExpression
    -- TODO: do we need to enforce indentation here?
    let exprIf = do
            thenToken <- token TThen
            ifTrue <- withIndentation (IRDeeper thenToken) noSemiExpression
            elseToken <- token TElse
            ifFalse <- withIndentation (IRDeeper elseToken) noSemiExpression
            return $ EIfThenElse (tokenData pr) condition ifTrue ifFalse
    let blockIf = do
            ifTrue <- blockExpression
            ifFalse <- P.option (ELiteral (tokenData pr) LUnit) $ do
                _ <- token TElse
                blockExpression <|> ifThenElseExpression
            return $ EIfThenElse (tokenData pr) condition ifTrue ifFalse

    withIndentation (IRAtOrDeeper pr) $
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
    let arrParser = do
            bracketed' $ commaDelimited noSemiExpression
        mutableArray = do
            mutableToken <- token TMutable
            (_, elts) <- arrParser
            return $ EArrayLiteral (tokenData mutableToken) Mutable elts
        immutableArray = do
            (pos, exprs) <- arrParser
            return $ EArrayLiteral pos Immutable exprs
    mutableArray <|> immutableArray

recordLiteralExpression :: Parser ParseExpression
recordLiteralExpression = do
    (pos, pairs) <- braced' $ commaDelimited $ do
        name <- anyIdentifier
        _ <- token TColon
        expr <- noSemiExpression
        return (name, expr)

    return $ ERecordLiteral pos (HashMap.fromList pairs)

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
    return $ EIdentifier pos $ UnqualifiedReference txt

functionExpression :: Parser ParseExpression
functionExpression = do
    tfun <- token Tokens.TFun
    args <- parenthesized $ commaDelimited funArgument
    returnAnn <- P.optionMaybe $ do
        _ <- token TColon
        typeIdent
    body <- blockExpression
    return $ EFun (tokenData tfun) args returnAnn body

pattern :: Parser RefutablePattern
pattern = parenthesized pattern <|> noParenPattern

constructorPattern :: Parser RefutablePattern
constructorPattern = do
    importName <- P.optionMaybe $ P.try $ anyIdentifier <* token TDot
    (_, patternName) <- upperIdentifier
    args <- P.option [] $ parenthesized $ commaDelimited pattern
    return $ RPConstructor importName patternName args

noParenPattern :: Parser RefutablePattern
noParenPattern = constructorPattern <|> (RPIrrefutable <$> irrefutablePattern)

irrefutablePattern :: Parser Pattern
irrefutablePattern = do
    let wildcardPattern = token TWildcard *> return PWildcard
    let bindingPattern = PBinding <$> anyIdentifier
    wildcardPattern <|> bindingPattern

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
            argList <- parenthesized $ commaDelimited noSemiExpression
            return $ EApp (edata lhs) lhs argList
        methodApp lhs = do
            _ <- token TRightArrow
            methodName <- anyIdentifier
            argList <- parenthesized $ commaDelimited noSemiExpression
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

letExpression :: Parser ParseExpression
letExpression = do
    tlet <- token TLet
    withIndentation (IRDeeper tlet) $ do
        mut <- P.option Immutable (token TMutable >> return Mutable)
        pat <- irrefutablePattern <|> fmap (PBinding . snd) lowerIdentifier
        typeAnn <- P.optionMaybe $ do
            _ <- token TColon
            typeIdent
        _ <- token TEqual
        expr <- noSemiExpression
        return $ ELet (tokenData tlet) mut pat typeAnn expr

semiExpression :: Parser ParseExpression
semiExpression = do
    first <- noSemiExpression
    rest <- P.many (token TSemicolon >> noSemiExpression)
    return $ foldl' (\a b -> ESemi (edata a) a b) first rest

parenExpression :: Parser ParseExpression
parenExpression = parenthesized semiExpression

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
delimited :: Parser b -> Parser a -> Parser [a]
delimited delim parseElement = do
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

commaDelimited :: Parser a -> Parser [a]
commaDelimited = delimited $ token TComma

recordTypeIdent :: Parser TypeIdent
recordTypeIdent = do
    props <- braced $ commaDelimited $ do
        mut <- P.optionMaybe (
            (token TMutable *> pure Mutable) <|>
            (token TConst *> pure Immutable))
        name <- anyIdentifier
        _ <- token TColon
        ty <- typeIdent
        return (name, mut, ty)
    return $ RecordIdent props

functionTypeIdent :: Parser TypeIdent
functionTypeIdent = do
    argTypes <- P.try $ do
        argTypes <- parenthesized $ commaDelimited typeIdent
        _ <- token TRightArrow
        return argTypes
    retType <- typeIdent
    return $ FunctionIdent argTypes retType

typeIdentifier :: Parser TypeIdentifier
typeIdentifier = do
    first <- anyIdentifier
    let unq = return $ UnqualifiedType first
    let qua = do
            _ <- token TDot
            second <- anyIdentifier
            return $ QualifiedType first second
    qua <|> unq

sumIdent :: Parser TypeIdent
sumIdent = do
    let justOne = do
            ti <- typeIdentifier
            return $ TypeIdent ti []
    name <- typeIdentifier
    params <- P.many (parenthesized sumIdent <|> justOne)
    return $ TypeIdent name params

unitTypeIdent :: Parser TypeIdent
unitTypeIdent = do
    _ <- P.try $ token TOpenParen <* token TCloseParen
    return UnitTypeIdent

typeIdent :: Parser TypeIdent
typeIdent = functionTypeIdent <|> unitTypeIdent <|> sumIdent <|> recordTypeIdent

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

declareDeclaration :: Parser ParseDeclaration
declareDeclaration = do
    declareToken <- token TDeclare
    withIndentation (IRDeeper declareToken) $ do
        name <- anyIdentifier
        _ <- token TColon
        ti <- typeIdent
        return $ DDeclare (tokenData declareToken) name ti

variantDefinition :: Parser (Variant Pos)
variantDefinition = do
    (pos, ctorname) <- anyIdentifierWithPos

    let withArgs = parenthesized $ commaDelimited typeIdent
    ctordata <- withArgs <|> return []

    return $ Variant pos ctorname ctordata

cruxDataDeclaration :: Pos -> Parser ParseDeclaration
cruxDataDeclaration pos = do
    name <- typeName
    typeVars <- P.many typeVariableName

    variants <- braced $ commaDelimited variantDefinition
    moduleName <- readModuleName
    return $ DData pos name moduleName typeVars variants

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

jsDataDeclaration :: Pos -> Parser ParseDeclaration
jsDataDeclaration pos = do
    _ <- token TJSFFI
    name <- typeName
    variants <- braced $ commaDelimited jsVariantDefinition
    moduleName <- readModuleName
    return $ DJSData pos name moduleName variants

dataDeclaration :: Parser ParseDeclaration
dataDeclaration = do
    dataToken <- token TData
    let pos = tokenData dataToken
    withIndentation (IRDeeper dataToken) $
        jsDataDeclaration pos <|> cruxDataDeclaration pos

typeDeclaration :: Parser ParseDeclaration
typeDeclaration = do
    typeToken <- token TType
    withIndentation (IRDeeper typeToken) $ do
        name <- typeName
        vars <- P.many typeVariableName
        _ <- token TEqual
        ty <- typeIdent
        return $ DTypeAlias (tokenData typeToken) name vars ty

funArgument :: Parser (Pattern, Maybe TypeIdent)
funArgument = do
    n <- irrefutablePattern
    ann <- P.optionMaybe $ do
        _ <- token TColon
        typeIdent
    return (n, ann)

blockExpression :: Parser ParseExpression
blockExpression = do
    -- TODO: use braced, which needs to return the start pos
    br <- token TOpenBrace
    body <- withIndentation (IRDeeper br) $ P.optionMaybe semiExpression >>= \case
        Nothing -> return []
        Just first -> withIndentation (IRAtColumn $ edata first) $ do
            rest <- P.many semiExpression
            return $ first : rest
    withIndentation (IRAtOrDeeper br) $ do
        void $ token TCloseBrace
    return $ case body of
        [] -> ELiteral (tokenData br) LUnit
        -- TODO: I think using the open brace's position for the position
        -- of all ESemi is wrong
        _ -> foldl1 (ESemi (tokenData br)) body

funDeclaration :: Parser ParseDeclaration
funDeclaration = do
    tfun <- token Tokens.TFun
    withIndentation (IRDeeper tfun) $ do
        name <- anyIdentifier
        params <- parenthesized $ commaDelimited funArgument
        returnAnn <- P.optionMaybe $ do
            _ <- token TColon
            typeIdent

        body <- blockExpression
        return $ DFun (tokenData tfun) name params returnAnn body

declaration :: Parser (Declaration UnresolvedReference ParseData)
declaration = do
    pos <- tokenData <$> P.lookAhead P.anyToken

    export <- P.optionMaybe $ token Tokens.TExport
    let exportFlag = case export of
            Just _ -> Export
            Nothing -> NoExport

    declType <- declareDeclaration
            <|> dataDeclaration
            <|> typeDeclaration
            <|> funDeclaration
            <|> letDeclaration
    return $ Declaration exportFlag pos declType

importDecl :: Parser (Pos, Import)
importDecl = do
    segments' <- P.sepBy1 anyIdentifierWithPos (token TDot)
    let ((pos, _):_) = segments'
    let segments = fmap snd segments'
    let prefix = fmap ModuleSegment $ init segments
    let base = ModuleSegment $ last segments
    let moduleName = ModuleName prefix base
    (P.optionMaybe $ parenthesized $ token TEllipsis) >>= \case
        Nothing -> do
            return (pos, QualifiedImport moduleName $ unModuleSegment base)
        Just _ -> do
            return (pos, UnqualifiedImport moduleName)

pragma :: Parser Pragma
pragma = do
    _ <- identifier "NoBuiltin"
    return PNoBuiltin

pragmas :: Parser [Pragma]
pragmas = do
    pragmaToken <- token TPragma
    withIndentation (IRDeeper pragmaToken) $ do
        braced $ commaDelimited pragma

imports :: Parser [(Pos, Import)]
imports = do
    importToken <- token TImport
    withIndentation (IRDeeper importToken) $ do
        braced $ commaDelimited importDecl

parseModule :: Parser ParsedModule
parseModule = do
    prags <- P.option [] pragmas
    imp <- P.option [] imports
    doc <- P.many $ declaration
    P.eof
    return Module
        { mPragmas = prags
        , mImports = imp
        , mDecls = doc
        }

runParser :: Parser a -> ModuleName -> P.SourceName -> [Token Pos] -> Either P.ParseError a
runParser parser moduleName sourceName tokens = do
    let parseResult = P.runParserT parser () sourceName tokens
    runReader parseResult (moduleName, IRLeftMost)

parse :: ModuleName -> P.SourceName -> [Token Pos] -> Either P.ParseError ParsedModule
parse = runParser parseModule
