{-# LANGUAGE FlexibleContexts, LambdaCase, MultiParamTypeClasses,
             OverloadedStrings, RecordWildCards, TupleSections #-}

module Crux.Parse
    ( parse

    -- for unit tests
    , Parser
    , runParser
    , PatternContext(..)
    , typeIdent
    , pattern
    , literalExpression
    , multiplyExpression
    , noSemiExpression
    , matchExpression
    , letExpression
    , funDeclaration
    , letDeclaration
    , dataDeclaration
    ) where

import Control.Applicative ((<|>))
import Control.Monad.Reader.Class (MonadReader, ask, local)
import Control.Monad.Trans.Reader (Reader, runReader)
import Crux.AST as AST
import Crux.ModuleName
import qualified Crux.JSTree as JSTree
import Crux.Pos (Pos(..), ParsePos(..), PosRec(..))
import Crux.Prelude
import Crux.Text (isCapitalized)
import Crux.Tokens as Tokens
import qualified Data.HashMap.Strict as HashMap
import qualified Text.Parsec as P

type PosInflater = ParsePos -> Pos
type Parser = P.ParsecT [ParseToken] PosInflater (Reader IndentReq)
type ParseData = ParsePos -- TODO: benchmark whether the fmap at the end is faster than inflating at each node
type ParseExpression = Expression UnresolvedReference () ParseData
type ParsePattern = Pattern ()
type ParseDeclaration = DeclarationType UnresolvedReference () ParseData
type ParseToken = Token ParsePos

data IndentReq
    = IRLeftMost
    -- TODO: we can kill this if the appropriate parsers always returned the initial token
    | IRAtColumn ParsePos
    | IRNextLineAtColumn ParsePos
    | IRDeeper ParseToken
    | IRAtOrDeeper ParseToken
    deriving (Show)

data IndentMatch
    = IndentOK
    | UnexpectedIndent String
    | UnexpectedDedent String

indentationPredicate :: ParsePos -> IndentReq -> IndentMatch
indentationPredicate p = \case
    IRLeftMost -> if 1 == ppLineStart p
        then IndentOK
        else UnexpectedIndent $ "Expected LeftMost but got " ++ show (ppLineStart p)
    IRAtColumn indent -> if ppLine p /= ppLine indent && ppLineStart p == ppLineStart indent
        then IndentOK
        else if ppColumn p > ppLineStart indent
            then UnexpectedIndent $ "Expected indentation at " ++ show indent ++ " but it was " ++ show p
            else UnexpectedDedent $ "Expected indentation at " ++ show indent ++ " but it was " ++ show p
    IRNextLineAtColumn indent -> if ppLine p > ppLine indent && ppLineStart p == ppLineStart indent
        then IndentOK
        else UnexpectedIndent $ "Must be on subsequent line but at same column"
    IRDeeper t -> let tPos = tokenData t in
        if ppLine tPos == ppLine p || ppColumn p > ppLineStart tPos
            then IndentOK
            else UnexpectedDedent $ "Expected indentation past " ++ show t ++ " p = " ++ show p
    IRAtOrDeeper t -> let tPos = tokenData t in
        if ppLine tPos == ppLine p || ppColumn p >= ppLineStart tPos
            then IndentOK
            else UnexpectedDedent $ "Expected indentation at or past " ++ show t ++ " p = " ++ show p

readIndentReq :: MonadReader a f => f a
readIndentReq = ask

-- Temporarily adjust the indentation policy for a parser
withIndentation :: MonadReader a m => a -> m r -> m r
withIndentation i = local $ \_ -> i

inflatePos :: ParsePos -> Parser Pos
inflatePos p = do
    -- I put the inflater in the state slot because it was convenient
    -- and avoided allocating a tuple in the ReaderT, but it should never
    -- be modified.
    inflater <- P.getState
    return $ inflater p

enclosed :: Parser ParseToken -> Parser b -> Parser a -> Parser (ParsePos, a)
enclosed open close body = do
    openToken <- open
    rv <- withIndentation (IRDeeper openToken) $ do
        body
    withIndentation (IRAtOrDeeper openToken) $ do
        void $ close
    return (tokenData openToken, rv)

parenthesized' :: Parser a -> Parser (ParsePos, a)
parenthesized' = enclosed (token TOpenParen) (token TCloseParen)

parenthesized :: Parser a -> Parser a
parenthesized p = fmap snd $ parenthesized' p

braced' :: Parser a -> Parser (ParsePos, a)
braced' = enclosed (token TOpenBrace) (token TCloseBrace)

braced :: Parser a -> Parser a
braced p = fmap snd $ braced' p

bracketed' :: Parser a -> Parser (ParsePos, a)
bracketed' = enclosed (token TOpenBracket) (token TCloseBracket)

bracketed :: Parser a -> Parser a
bracketed p = fmap snd $ bracketed' p

angleBracketed :: Parser a -> Parser a
angleBracketed p = fmap snd $ angleBracketed' p

angleBracketed' :: Parser a -> Parser (ParsePos, a)
angleBracketed' = enclosed (token TLess) (token TGreater)

getToken :: P.Stream s m ParseToken
         => (ParseToken -> Maybe a) -> P.ParsecT s u m a
getToken predicate = P.tokenPrim show nextPos predicate
  where
    nextPos pos t _ =
        -- This appears to be incorrect logic per the docs, but our errors all
        -- have correct line numbers...
        let ParsePos{..} = tokenData t
        in P.setSourceLine (P.setSourceColumn pos ppColumn) ppLine

tokenBy :: (TokenType -> Maybe a) -> Parser (a, ParseToken)
tokenBy predicate = do
    indentReq <- readIndentReq
    let testTok tok@(Token pos ttype) = case predicate ttype of
          Just r ->
            case indentationPredicate pos indentReq of
                IndentOK -> return (r, tok)
                _ -> Nothing -- TODO: this is really bad, we lose all error message information here
          Nothing -> Nothing
    getToken testTok

token :: TokenType -> Parser ParseToken
token expected = do
    ((), t) <- tokenBy $ \t ->
        if expected == t then Just () else Nothing
    return t

identifier :: Text -> Parser ParseToken
identifier name = fmap snd $ tokenBy $ \case
    TUpperIdentifier t | t == name -> Just ()
    TLowerIdentifier t | t == name -> Just ()
    _ -> Nothing

lowerIdentifier :: Parser (Text, ParsePos)
lowerIdentifier = do
    (t, Token pos _) <- tokenBy $ \case
        TLowerIdentifier t -> Just t
        _ -> Nothing
    return (t, pos)

upperIdentifier :: Parser (Text, ParsePos)
upperIdentifier = do
    (t, Token pos _) <- tokenBy $ \case
        TUpperIdentifier t -> Just t
        _ -> Nothing
    return (t, pos)

anyIdentifierWithPos :: Parser (Text, ParsePos)
anyIdentifierWithPos = do
    lowerIdentifier <|> upperIdentifier

anyIdentifier :: Parser Text
anyIdentifier = fst <$> anyIdentifierWithPos

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

lambdaTail :: ParsePos -> [ParsePattern] -> Parser ParseExpression
lambdaTail pos params = do
    body <- blockExpression <|> noSemiExpression
    return $ EFun pos $ FunctionDecl
        { fdParams = fmap (\x -> (x, Nothing)) params
        , fdReturnAnnot = Nothing
        , fdBody = body
        }

identifierExpression :: Parser ParseExpression
identifierExpression = do
    (txt, pos) <- anyIdentifierWithPos

    let lambdaForm = do
            _ <- token TFatRightArrow
            lambdaTail pos [PBinding txt]

    let ident = EIdentifier pos $ UnqualifiedReference txt
    lambdaForm <|> return ident

functionGuts :: ParsePos -> Parser ParseExpression
functionGuts pos = do
    fdParams <- parenthesized $ commaDelimited funArgument
    fdReturnAnnot <- P.optionMaybe $ do
        _ <- token TColon
        typeIdent
    fdBody <- blockExpression
    return $ EFun pos FunctionDecl{..}

functionExpression :: Parser ParseExpression
functionExpression = do
    tfun <- token Tokens.TFun
    let pos = tokenData tfun

    functionGuts pos

wildcardPattern :: Parser ParsePattern
wildcardPattern = token TWildcard *> return PWildcard

data PatternContext = RefutableContext | IrrefutableContext

parenPattern :: PatternContext -> Parser ParsePattern
parenPattern ctx = do
    (parenthesized $ commaDelimited $ pattern ctx) >>= \case
        [x] -> return x
        elements -> return $ PTuple elements

-- lower - PBinding
-- UPPER - PBinding if irrefutable else PConstructor
-- UPPER(...) - PConstructor
-- mod.UPPER - PConstructor
-- mod.UPPER(...) - PConstructor
pattern :: PatternContext -> Parser ParsePattern
pattern ctx = parenPattern ctx <|> wildcardPattern <|> do
    let lowerBinding = (PBinding . fst) <$> lowerIdentifier
    let parseConstructor = do
            (constructorName, _) <- upperIdentifier
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

{-
-- Sometimes we don't know what it is yet.
-- We can resolve a PatExpr into a concrete
-- Expression or Pattern.  The complexity here is largely
-- to support ES6 lambda syntax.
data PatExpr
    -- = PWildcard
    = PEUnit
    -- | PEIdentifier Text
    -- | PETuple [PattyOrExpr]
    | PEExpr ParseExpression
    -- | PEPattern ParsePattern
-}

-- TODO: replace this with PatExpr resolution
asPattern :: ParseExpression -> Parser (Pattern ())
asPattern = \case
    EIdentifier _ (UnqualifiedReference n) -> return $ PBinding n
    ELiteral _ LUnit -> return $ PTuple []
    -- TODO: better error message
    _ -> fail "Not a valid pattern"

parenExpression :: Parser ParseExpression
parenExpression = do
    -- TODO: think about commas vs. semicolons
    (pos, elements) <- (parenthesized' $ commaDelimited semiExpression)
    (P.optionMaybe $ token TFatRightArrow) >>= \case
        Nothing -> return $ case elements of
            -- we know it's an expr, parse it as such
            [] -> ELiteral pos LUnit
            [x] -> x
            _ -> ETupleLiteral pos elements
        Just _ -> do
            -- attempt as lambda form
            patterns <- for elements asPattern
            lambdaTail pos patterns

wildcardLambda :: Parser ParseExpression
wildcardLambda = do
    twildcard <- token TWildcard
    _ <- token TFatRightArrow
    body <- noSemiExpression
    return $ EFun (tokenData twildcard) $ FunctionDecl
        { fdParams = [(PWildcard, Nothing)]
        , fdReturnAnnot = Nothing
        , fdBody = body
        }

basicExpression :: Parser ParseExpression
basicExpression =
    identifierExpression
    <|> literalExpression
    <|> parenExpression
    <|> wildcardLambda

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
        forall <- P.option [] $ explicitTypeVariableList
        typeAnn <- P.optionMaybe $ do
            _ <- token TColon
            typeIdent
        _ <- token TEqual
        expr <- noSemiExpression
        return $ ELet (tokenData tlet) mut pat forall typeAnn expr

semiExpression :: Parser ParseExpression
semiExpression = do
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

plusDelimited :: Parser a -> Parser [a]
plusDelimited = delimited $ token TPlus

recordField :: Parser (Text, Maybe Mutability, TypeIdent)
recordField = do
    mut <- P.optionMaybe (
        (token TMutable *> pure Mutable) <|>
        (token TConst *> pure Immutable))
    name <- anyIdentifier
    _ <- token TColon
    ty <- typeIdent
    return (name, mut, ty)

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

sumIdent :: Parser TypeIdent
sumIdent = do
    name <- unresolvedReference
    params <- P.option [] $ angleBracketed $ commaDelimited typeIdent

    let it = TypeIdent name params
    let funner = do
            _ <- token TFatRightArrow
            rv <- typeIdent
            return $ FunctionIdent [it] rv
    funner <|> return it

arrayTypeIdent :: Parser TypeIdent
arrayTypeIdent = do
    mutability <- P.option Immutable (token TMutable *> return Mutable)
    ti <- bracketed typeIdent
    return $ ArrayIdent mutability ti

optionTypeIdent :: Parser TypeIdent
optionTypeIdent = do
    _ <- token TQuestionMark
    ti <- typeIdent
    return $ OptionIdent ti

parenTypeIdent :: Parser TypeIdent
parenTypeIdent = do
    elements <- parenthesized $ commaDelimited typeIdent
    (P.optionMaybe $ token TFatRightArrow) >>= \case
        Nothing -> return $ case elements of
            [x] -> x
            _ -> TupleTypeIdent elements
        Just _arrowToken -> do
            resultType <- typeIdent
            return $ FunctionIdent elements resultType

typeIdent :: Parser TypeIdent
typeIdent = asum
    [ arrayTypeIdent
    , optionTypeIdent
    , functionTypeIdent
    , recordTypeIdent
    , sumIdent       -- also unary functions
    , parenTypeIdent -- also nullary and binary+ functions
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
    ELet ed mut name forall typeAnn expr <- letExpression
    return $ DLet ed mut name forall typeAnn expr

declareDeclaration :: Parser ParseDeclaration
declareDeclaration = do
    declareToken <- token TDeclare
    withIndentation (IRDeeper declareToken) $ do
        name <- anyIdentifier
        forall <- P.option [] explicitTypeVariableList
        _ <- token TColon
        ti <- typeIdent
        return $ DDeclare (tokenData declareToken) name forall ti

variantDefinition :: Parser (Variant ParsePos)
variantDefinition = do
    (ctorname, pos) <- anyIdentifierWithPos

    let withArgs = parenthesized $ commaDelimited typeIdent
    ctordata <- withArgs <|> return []

    return $ Variant pos ctorname ctordata

cruxDataDeclaration :: ParsePos -> Parser ParseDeclaration
cruxDataDeclaration pos = do
    name <- typeName
    typeVars <- P.option [] explicitTypeVariableList

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

jsDataDeclaration :: ParsePos -> Parser ParseDeclaration
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
        vars <- P.option [] $ angleBracketed $ commaDelimited typeVariableName
        _ <- token TEqual
        ty <- typeIdent
        return $ DTypeAlias (tokenData typeToken) name vars ty

funArgument :: Parser (ParsePattern, Maybe TypeIdent)
funArgument = do
    n <- pattern IrrefutableContext
    ann <- P.optionMaybe $ do
        _ <- token TColon
        typeIdent
    return (n, ann)

bracedLines :: Parser (ParsePos, a) -> Parser (ParsePos, [a])
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

typeVarIdent :: Parser TypeVarIdent
typeVarIdent = do
    (name, pos) <- anyIdentifierWithPos
    traits <- P.option [] $ token TColon >> plusDelimited unresolvedReference
    pos' <- inflatePos pos
    return $ TypeVarIdent name pos' traits

explicitTypeVariableList :: Parser [TypeVarIdent]
explicitTypeVariableList = do
    angleBracketed $ commaDelimited typeVarIdent

funDeclaration :: Parser ParseDeclaration
funDeclaration = do
    tfun <- token Tokens.TFun

    let pos = tokenData tfun

    withIndentation (IRDeeper tfun) $ do
        name <- anyIdentifier
        forall <- P.option [] explicitTypeVariableList
        fdParams <- parenthesized $ commaDelimited funArgument
        fdReturnAnnot <- P.optionMaybe $ do
            _ <- token TColon
            typeIdent

        fdBody <- blockExpression
        return $ DFun pos name forall FunctionDecl{..}

traitDeclaration :: Parser ParseDeclaration
traitDeclaration = do
    ttrait <- token Tokens.TTrait
    name <- anyIdentifier
    (_, decls) <- bracedLines $ do
        (mname, pos) <- anyIdentifierWithPos
        let trad = do
                tcolon <- token Tokens.TColon
                withIndentation (IRDeeper tcolon) typeIdent
        let sugar = do
                -- TODO: indentation rules here are probably wrongc
                params <- parenthesized $ commaDelimited typeIdent
                _ <- token Tokens.TColon
                ret <- typeIdent
                return $ FunctionIdent params ret
        mident <- sugar <|> trad
        return (pos, (mname, pos, mident))

    return $ DTrait (tokenData ttrait) name decls

implDeclaration :: Parser ParseDeclaration
implDeclaration = do
    timpl <- token Tokens.TImpl
    traitName <- unresolvedReference
    typeName' <- unresolvedReference
    forall <- P.option [] explicitTypeVariableList
    (_, decls) <- bracedLines $ do
        -- TODO: indentation rules here probably aren't right
        (elementName, pos) <- anyIdentifierWithPos
        let trad = do
                tequal <- token TEqual
                withIndentation (IRDeeper tequal) noSemiExpression
        let sugar = functionGuts pos

        expr <- sugar <|> trad
        return (pos, (elementName, expr))

    return $ DImpl (tokenData timpl) traitName typeName' forall decls []

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
    pos' <- inflatePos pos
    return $ Declaration exportFlag pos' declType

pragma :: Parser (ParsePos, Pragma)
pragma = do
    p <- identifier "NoBuiltin"
    return (tokenData p, PNoBuiltin)

pragmas :: Parser [Pragma]
pragmas = do
    pragmaToken <- token TPragma
    withIndentation (IRDeeper pragmaToken) $ do
        snd <$> bracedLines pragma

importDecl :: Parser (ParsePos, Import)
importDecl = do
    importToken <- token TImport

    segments' <- P.sepBy1 anyIdentifierWithPos (token TDot)
    let segments = fmap fst segments'
    let prefix = init segments
    let base = last segments

    let prefix' = fmap ModuleSegment prefix
    let base' = ModuleSegment base
    let moduleName = ModuleName prefix' base'

    let unconditionalImport = do
            _ <- token TEllipsis
            return $ UnqualifiedImport
    
    let selectiveImport = do
            names <- commaDelimited anyIdentifier
            return $ SelectiveImport names

    let unqualifiedImport = do
            parenthesized $ unconditionalImport <|> selectiveImport

    let qualifiedImport = do
            alias <- P.option (Just base) $ do
                _ <- token TAs
                (Just <$> anyIdentifier) <|> (token TWildcard >> return Nothing)
            return $ QualifiedImport alias

    importType <- unqualifiedImport <|> qualifiedImport
    return (tokenData importToken, Import moduleName importType)

imports :: Parser [(ParsePos, Import)]
imports = P.many importDecl

parseModule :: Parser ParsedModule
parseModule = do
    inflater <- P.getState

    prags <- P.option [] pragmas
    imp <- P.option [] imports
    doc <- P.many $ declaration
    P.eof
    return Module
        { mPragmas = prags
        , mImports = fmap (\(a, b) -> (inflater a, b)) imp
        , mDecls = fmap (fmap inflater) doc
        }

runParser :: Parser a -> P.SourceName -> [ParseToken] -> Either P.ParseError a
runParser parser sourceName tokens = do
    let inflater ParsePos{..} = Pos $ PosRec
            { posFileName = sourceName
            , posLine = ppLine
            , posColumn = ppColumn
            }
    let parseResult = P.runParserT parser inflater sourceName tokens
    runReader parseResult IRLeftMost

parse :: P.SourceName -> [ParseToken] -> Either P.ParseError ParsedModule
parse = runParser parseModule
