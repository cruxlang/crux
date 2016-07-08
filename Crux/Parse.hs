{-# LANGUAGE FlexibleContexts, LambdaCase, MultiParamTypeClasses,
             OverloadedStrings, RecordWildCards, TupleSections #-}

module Crux.Parse where

import Control.Applicative ((<|>))
import Control.Monad.Reader.Class (MonadReader, ask, local)
import Control.Monad.Trans.Reader (Reader, runReader)
import Crux.AST as AST
import Crux.ModuleName
import qualified Crux.JSTree as JSTree
import Crux.Prelude
import Crux.Text (isCapitalized)
import Crux.Tokens as Tokens
import qualified Data.HashMap.Strict as HashMap
import qualified Text.Parsec as P

type Parser = P.ParsecT [Token Pos] () (Reader IndentReq)
type ParseData = Pos
type ParseExpression = Expression UnresolvedReference () ParseData
type ParseDeclaration = DeclarationType UnresolvedReference () ParseData

data IndentReq
    = IRLeftMost
    -- TODO: we can kill this if the appropriate parsers always returned the initial token
    | IRAtColumn Pos
    | IRNextLineAtColumn Pos
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
    IRAtColumn indent -> if posLine p /= posLine indent && posLineStart p == posLineStart indent
        then IndentOK
        else if posCol p > posLineStart indent
            then UnexpectedIndent $ "Expected indentation at " ++ show indent ++ " but it was " ++ show p
            else UnexpectedDedent $ "Expected indentation at " ++ show indent ++ " but it was " ++ show p
    IRNextLineAtColumn indent -> if posLine p > posLine indent && posLineStart p == posLineStart indent
        then IndentOK
        else UnexpectedIndent $ "Must be on subsequent line but at same column"
    IRDeeper t -> let tPos = tokenData t in
        if posLine tPos == posLine p || posCol p > posLineStart tPos
            then IndentOK
            else UnexpectedDedent $ "Expected indentation past " ++ show t ++ " p = " ++ show p
    IRAtOrDeeper t -> let tPos = tokenData t in
        if posLine tPos == posLine p || posCol p >= posLineStart tPos
            then IndentOK
            else UnexpectedDedent $ "Expected indentation at or past " ++ show t ++ " p = " ++ show p

readIndentReq :: MonadReader a f => f a
readIndentReq = ask

-- Temporarily adjust the indentation policy for a parser
withIndentation :: MonadReader a m => a -> m r -> m r
withIndentation i = local $ \_ -> i

enclosed :: Parser (Token Pos) -> Parser b -> Parser a -> Parser (Pos, a)
enclosed open close body = do
    openToken <- open
    rv <- withIndentation (IRDeeper openToken) $ do
        body
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
          Just r ->
            case indentationPredicate pos indentReq of
                IndentOK -> return (r, tok)
                _ -> Nothing -- TODO: this is really bad, we lose all error message information here
          Nothing -> Nothing
    getToken testTok

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

throwExpression :: Parser ParseExpression
throwExpression = do
    throwToken <- token TThrow
    withIndentation (IRDeeper throwToken) $ do
        exceptionName <- unresolvedReference
        expr <- noSemiExpression
        return $ EThrow (tokenData throwToken) exceptionName expr

tryCatchExpression :: Parser ParseExpression
tryCatchExpression = do
    tryToken <- token TTry
    tryBody <- withIndentation (IRDeeper tryToken) $ blockExpression
    catchToken <- withIndentation (IRAtOrDeeper tryToken) $ token TCatch
    (exceptionName, bindingName, catchBody) <- withIndentation (IRDeeper catchToken) $ do
        exceptionName <- unresolvedReference
        bindingName <- pattern IrrefutableContext
        catchBody <- blockExpression
        return (exceptionName, bindingName, catchBody)
    return $ ETryCatch (tokenData tryToken) tryBody exceptionName bindingName catchBody

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
    rv <- P.optionMaybe noSemiExpression >>= \case
        Just a -> return a
        _      -> return $ ELiteral (tokenData pr) LUnit
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
    maybeForall <- P.optionMaybe forallQualifier
    tfun <- token Tokens.TFun

    let (fdForall, pos) = fromMaybe ([], tokenData tfun) maybeForall

    fdParams <- parenthesized $ commaDelimited funArgument
    fdReturnAnnot <- P.optionMaybe $ do
        _ <- token TColon
        typeIdent
    fdBody <- blockExpression
    return $ EFun pos FunctionDecl{..}

wildcardPattern :: Parser (Pattern ())
wildcardPattern = token TWildcard *> return PWildcard

data PatternContext = RefutableContext | IrrefutableContext

-- lower - PBinding
-- UPPER - PBinding if irrefutable else PConstructor
-- UPPER(...) - PConstructor
-- mod.UPPER - PConstructor
-- mod.UPPER(...) - PConstructor
pattern :: PatternContext -> Parser (Pattern ())
pattern ctx = parenthesized (pattern ctx) <|> wildcardPattern <|> do
    let lowerBinding = lowerIdentifier >>= return . PBinding . snd
    let parseConstructor = do
            (_, constructorName) <- upperIdentifier
            -- TODO: if parenthesized, this should require at least one arg
            -- commaDelimited1?
            args <- P.optionMaybe $ parenthesized $ commaDelimited $ pattern ctx
            return (constructorName, args)

    (P.optionMaybe $ P.try $ anyIdentifier <* token TDot) >>= \case
        Nothing -> do
            lowerBinding <|> do
                (constructorName, args) <- parseConstructor
                case (ctx, args) of
                    (RefutableContext, Nothing) -> do
                        return $ PConstructor (UnqualifiedReference constructorName) () []
                    (IrrefutableContext, Nothing) -> do
                        return $ PBinding constructorName
                    (_, Just a) -> do
                        return $ PConstructor (UnqualifiedReference constructorName) () a
        Just importName -> do
            (constructorName, args') <- parseConstructor
            let args = case args' of
                    Nothing -> []
                    Just i -> i
            let ref = QualifiedReference importName constructorName
            return $ PConstructor ref () args

matchExpression :: Parser ParseExpression
matchExpression = do
    tmatch <- token TMatch
    expr <- noSemiExpression
    cases <- braced $ P.many $ do
        pat <- pattern RefutableContext
        arrowToken <- token TFatRightArrow
        ex <- withIndentation (IRDeeper arrowToken) $ do
            blockExpression <|> noSemiExpression
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
        pat <- pattern IrrefutableContext
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
    <|> throwExpression
    <|> tryCatchExpression
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

recordField :: Parser (Text, Maybe Mutability, TypeIdent)
recordField = do
    mut <- P.optionMaybe (
        (token TMutable *> pure Mutable) <|>
        (token TConst *> pure Immutable))
    name <- anyIdentifier
    _ <- token TColon
    ty <- typeIdent
    return (name, mut, ty)

nonEmptyRecord :: Parser TypeIdent
nonEmptyRecord = do
    firstProp <- P.try $
        token TOpenBrace *> recordField

    areMore <- P.optionMaybe $ token TComma
    props <- (firstProp:) <$> case areMore of
        Nothing ->
            return []
        Just _ -> do
            commaDelimited recordField

    _ <- token TCloseBrace

    return $ RecordIdent props

recordTypeIdent :: Parser TypeIdent
recordTypeIdent =
    RecordIdent <$> braced (commaDelimited recordField)

functionTypeIdent :: Parser TypeIdent
functionTypeIdent = do
    _ <- token TFun
    argTypes <- parenthesized $ commaDelimited typeIdent
    rightArrowToken <- token TRightArrow
    retType <- withIndentation (IRDeeper rightArrowToken) $ typeIdent
    return $ FunctionIdent argTypes retType

unresolvedReference :: Parser UnresolvedReference
unresolvedReference = do
    first <- anyIdentifier
    let unq = return $ UnqualifiedReference first
    let qua = do
            _ <- token TDot
            second <- anyIdentifier
            return $ QualifiedReference first second
    qua <|> unq

sumIdent :: Parser TypeIdent -> Parser TypeIdent
sumIdent paramParser = do
    name <- unresolvedReference
    params <- P.many paramParser
    return $ TypeIdent name params

unitTypeIdent :: Parser TypeIdent
unitTypeIdent = do
    _ <- P.try $ token TOpenParen <* token TCloseParen
    return UnitTypeIdent

arrayTypeIdent :: Parser TypeIdent
arrayTypeIdent = do
    mutability <- P.option Immutable (token TMutable *> return Mutable)
    ti <- bracketed typeIdent
    return $ ArrayIdent mutability ti

optionTypeIdent :: Parser TypeIdent -> Parser TypeIdent
optionTypeIdent innerTypeIdent = do
    _ <- token TQuestionMark
    ti <- innerTypeIdent
    return $ OptionIdent ti


typeIdent :: Parser TypeIdent
typeIdent = asum
    [ arrayTypeIdent
    , optionTypeIdent noSpaceTypeIdent
    , functionTypeIdent
    , recordTypeIdent
    , sumIdent noSpaceTypeIdent
    , unitTypeIdent
    , parenthesized typeIdent
    ]

noSpaceTypeIdent :: Parser TypeIdent
noSpaceTypeIdent = asum
    [ arrayTypeIdent
    , optionTypeIdent noSpaceTypeIdent
    , recordTypeIdent
    , sumIdent (fail "")
    , unitTypeIdent
    , parenthesized typeIdent
    ]

returnTypeIdent :: Parser TypeIdent
returnTypeIdent = asum
    [ arrayTypeIdent
    , optionTypeIdent noSpaceTypeIdent
    , functionTypeIdent
    , nonEmptyRecord
    , sumIdent (arrayTypeIdent <|> optionTypeIdent (fail "") <|> nonEmptyRecord <|> sumIdent (fail "") <|> unitTypeIdent <|> parenthesized typeIdent)
    , unitTypeIdent
    , parenthesized typeIdent
    ]

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

    let shorthand = do
            args <- parenthesized $ commaDelimited typeIdent
            return $ [Variant pos name args]
    let expanded = do
            braced $ commaDelimited variantDefinition

    variants <- shorthand <|> expanded
    return $ DData pos name typeVars variants

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
    return $ DJSData pos name variants

dataDeclaration :: Parser ParseDeclaration
dataDeclaration = do
    dataToken <- token TData
    let pos = tokenData dataToken
    withIndentation (IRDeeper dataToken) $
        jsDataDeclaration pos <|> cruxDataDeclaration pos

aliasDeclaration :: Parser ParseDeclaration
aliasDeclaration = do
    typeToken <- token TType
    withIndentation (IRDeeper typeToken) $ do
        name <- typeName
        vars <- P.many typeVariableName
        _ <- token TEqual
        ty <- typeIdent
        return $ DTypeAlias (tokenData typeToken) name vars ty

funArgument :: Parser (Pattern (), Maybe TypeIdent)
funArgument = do
    n <- pattern IrrefutableContext
    ann <- P.optionMaybe $ do
        _ <- token TColon
        typeIdent
    return (n, ann)

bracedLines :: Parser (Pos, a) -> Parser (Pos, [a])
bracedLines lineParser = do
    br <- token TOpenBrace
    firstLine <- withIndentation (IRDeeper br) $ P.optionMaybe lineParser
    body <- case firstLine of
      Nothing -> return []
      Just (linePos, first) -> do
          loop linePos [first]
        where loop previousPos acc = do
                parseResult <- withIndentation (IRNextLineAtColumn previousPos) $ do
                  P.optionMaybe lineParser
                case parseResult of
                  Nothing -> do
                    return acc
                  Just (thisPos, thisLine) -> do
                    -- a snoc would make this linear rather than quadratic
                    loop thisPos (acc <> [thisLine])
    withIndentation (IRAtOrDeeper br) $ do
        void $ token TCloseBrace
    return (tokenData br, body)

blockExpression :: Parser ParseExpression
blockExpression = do
    (brPos, body) <- bracedLines $ do
        expr <- semiExpression
        return (edata expr, expr)
    return $ case body of
        [] -> ELiteral brPos LUnit
        -- TODO: I think using the open brace's position for the position
        -- of all ESemi is wrong
        _ -> foldl1 (ESemi brPos) body

forallQualifier :: Parser ([Name], Pos)
forallQualifier = do
    tforall <- token Tokens.TForall
    (, tokenData tforall) <$> (braced $ commaDelimited typeVarName)
  where
    typeVarName = anyIdentifier

funDeclaration :: Parser ParseDeclaration
funDeclaration = do
    maybeForall <- P.optionMaybe forallQualifier
    tfun <- token Tokens.TFun

    let (fdForall, pos) = fromMaybe ([], tokenData tfun) maybeForall

    withIndentation (IRDeeper tfun) $ do
        name <- anyIdentifier
        fdParams <- parenthesized $ commaDelimited funArgument
        fdReturnAnnot <- P.optionMaybe $ do
            _ <- token TColon
            returnTypeIdent

        fdBody <- blockExpression
        return $ DFun pos name FunctionDecl{..}

traitDeclaration :: Parser ParseDeclaration
traitDeclaration = do
    ttrait <- token Tokens.TTrait
    name <- anyIdentifier
    typeVar <- anyIdentifier
    (_, decls) <- bracedLines $ do
        (pos, mname) <- anyIdentifierWithPos
        tcolon <- token Tokens.TColon
        mident <- withIndentation (IRDeeper tcolon) typeIdent
        return (pos, (mname, pos, mident))

    return $ DTrait (tokenData ttrait) name typeVar decls

implDeclaration :: Parser ParseDeclaration
implDeclaration = do
    timpl <- token Tokens.TImpl
    traitName <- unresolvedReference
    typeIdent_ <- returnTypeIdent
    (_, decls) <- bracedLines $ do
        -- TODO: merge with function decl parsing?
        (pos, funName) <- anyIdentifierWithPos
        args <- parenthesized $ commaDelimited funArgument
        returnAnnot <- P.optionMaybe $ do
            _ <- token TColon
            returnTypeIdent

        body <- blockExpression
        return (pos, (funName, pos, args, returnAnnot, body))

    return $ DImpl (tokenData timpl) traitName typeIdent_ decls

exceptionDeclaration :: Parser ParseDeclaration
exceptionDeclaration = do
    texc <- token Tokens.TException
    withIndentation (IRDeeper texc) $ do
        name <- anyIdentifier
        ti <- typeIdent
        return $ DException (tokenData texc) name ti

exportImportDeclaration :: Parser ParseDeclaration
exportImportDeclaration = do
    importToken <- token Tokens.TImport
    withIndentation (IRDeeper importToken) $ do
        name <- anyIdentifier
        return $ DExportImport (tokenData importToken) name

declaration :: Parser (Declaration UnresolvedReference () ParseData)
declaration = do
    pos <- tokenData <$> P.lookAhead P.anyToken

    export <- P.optionMaybe $ token Tokens.TExport
    let exportFlag = case export of
            Just _ -> Export
            Nothing -> NoExport

    let extra :: [Parser ParseDeclaration]
        extra = if exportFlag == Export
            then [exportImportDeclaration]
            else []
    declType <- asum $
        [ declareDeclaration
        , dataDeclaration
        , aliasDeclaration
        , funDeclaration
        , letDeclaration
        , traitDeclaration
        , implDeclaration
        , exceptionDeclaration
        ] ++ extra
    return $ Declaration exportFlag pos declType

pragma :: Parser (Pos, Pragma)
pragma = do
    p <- identifier "NoBuiltin"
    return (tokenData p, PNoBuiltin)

pragmas :: Parser [Pragma]
pragmas = do
    pragmaToken <- token TPragma
    withIndentation (IRDeeper pragmaToken) $ do
        snd <$> bracedLines pragma

importDecl :: Parser (Pos, Import)
importDecl = do
    segments' <- P.sepBy1 anyIdentifierWithPos (token TDot)
    let ((pos, _):_) = segments'
    let segments = fmap snd segments'
    let prefix = fmap ModuleSegment $ init segments
    let base = ModuleSegment $ last segments
    let moduleName = ModuleName prefix base
    let defaultAlias = unModuleSegment base
    alias <- P.option (Just defaultAlias) (token TAs *> ((Just <$> anyIdentifier) <|> (token TWildcard *> return Nothing)))
    (P.optionMaybe $ parenthesized $ token TEllipsis) >>= \case
        Nothing -> do
            return (pos, QualifiedImport moduleName alias)
        Just _ -> do
            return (pos, UnqualifiedImport moduleName)

imports :: Parser [(Pos, Import)]
imports = do
    importToken <- token TImport
    withIndentation (IRDeeper importToken) $ do
        (_, importDecls) <- bracedLines $ do
            (pos, i) <- importDecl
            return (pos, (pos, i))
        return importDecls

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

runParser :: Parser a -> P.SourceName -> [Token Pos] -> Either P.ParseError a
runParser parser sourceName tokens = do
    let parseResult = P.runParserT parser () sourceName tokens
    runReader parseResult IRLeftMost

parse :: P.SourceName -> [Token Pos] -> Either P.ParseError ParsedModule
parse = runParser parseModule
