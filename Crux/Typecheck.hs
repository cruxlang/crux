{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Crux.Typecheck where

import           Control.Monad         (when)
import           Crux.AST
import           Crux.Intrinsic        (Intrinsic (..))
import qualified Crux.Intrinsic        as Intrinsic
import qualified Crux.MutableHashTable as HashTable
import           Crux.Text             (isCapitalized)
import           Crux.Tokens           (Pos (..))
import           Data.Foldable         (forM_)
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as HashMap
import           Data.IORef            (IORef, readIORef, writeIORef)
import qualified Data.IORef            as IORef
import           Data.List             (foldl', intercalate, nub, sort)
import           Data.Monoid           ((<>))
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           Data.Traversable      (forM)
import           Prelude               hiding (String)
import           Text.Printf           (printf)

data Env = Env
    { eNextTypeIndex :: IORef Int
    , eBindings      :: IORef (HashMap Text TypeVar)
    , eTypeBindings  :: HashMap Text TypeVar
    , eReturnType    :: Maybe TypeVar -- Nothing if top-level expression
    }

showTypeVarIO :: TypeVar -> IO [Char]
showTypeVarIO tvar = case tvar of
    TVar i o -> do
        o' <- readIORef o
        os <- case o' of
            Unbound -> return "Unbound"
            Link x -> showTypeVarIO x
        return $ "(TVar " ++ show i ++ " " ++ os ++ ")"
    TQuant i ->
        return $ "TQuant " ++ show i
    TFun arg ret -> do
        as <- mapM showTypeVarIO arg
        rs <- showTypeVarIO ret
        return $ "TFun " ++ show as  ++ " -> " ++ rs
    TUserType def tvars -> do
        tvs <- mapM showTypeVarIO tvars
        return $ (show $ tuName def) ++ " " ++ (intercalate " " tvs)
    TRecord open rows -> do
        open' <- readIORef open
        rows' <- readIORef rows
        let rowNames = map fst rows'
        rowTypes <- mapM (showTypeVarIO . snd) rows'
        let showRow (name, ty) = Text.unpack name <> ": " <> ty
        let dotdotdot = case open' of
                RecordOpen -> ["..."]
                RecordClose -> []
        return $ "{" <> (intercalate "," (map showRow (zip rowNames rowTypes) <> dotdotdot)) <> "}"
    TType ty ->
        return $ "TType " ++ show ty

newEnv :: Maybe TypeVar -> IO Env
newEnv eReturnType = do
    eNextTypeIndex <- IORef.newIORef 0
    eBindings <- IORef.newIORef HashMap.empty
    let eTypeBindings = HashMap.empty
    return Env {..}

childEnv :: Env -> IO Env
childEnv env = do
    bindings' <- HashTable.clone (eBindings env)
    return env{eBindings=bindings'}

freshType :: Env -> IO TypeVar
freshType Env{eNextTypeIndex} = do
    IORef.modifyIORef' eNextTypeIndex (+1)
    index <- IORef.readIORef eNextTypeIndex
    link <- IORef.newIORef Unbound
    return $ TVar index link

typeFromConstructor :: Env -> Name -> Maybe (TypeVar, TVariant TypeVar)
typeFromConstructor env cname =
    let fold acc ty = case (acc, ty) of
            (Just a, _) -> Just a
            (Nothing, ut@(TUserType def _)) ->
                let TUserTypeDef { tuVariants = variants } = def
                in case [v | v@(TVariant vname _) <- variants, vname == cname] of
                    [v] -> Just (ut, v)
                    [] -> Nothing
                    _ -> error "This should never happen: Type has multiple variants with the same constructor name"
            _ -> Nothing
    in foldl' fold Nothing (HashMap.elems $ eTypeBindings env)

-- | Build up an environment for a case of a match block.
-- exprType is the type of the expression.  We unify this with the constructor of the pattern
buildPatternEnv :: TypeVar -> Env -> Pattern2 -> IO ()
buildPatternEnv exprType env patt = case patt of
    PPlaceholder pname -> do
        HashTable.insert pname exprType (eBindings env)

    PConstructor cname cargs -> do
        case typeFromConstructor env cname of
            Just (TUserType def tyVars, _) -> do
                subst <- HashTable.new
                (ty', variants) <- instantiateUserType subst env def tyVars
                unify exprType ty'

                let findVariant [] = error "findVariant: This should never happen"
                    findVariant (v:rest)
                        | tvName v == cname = Just v
                        | otherwise = findVariant rest

                let Just variant = findVariant variants
                let TVariant{tvParameters} = variant

                when (length tvParameters /= length cargs) $
                    error $ printf "Pattern should specify %i args but got %i" (length tvParameters) (length cargs)

                forM_ (zip cargs tvParameters) $ \(arg, vp) -> do
                    buildPatternEnv vp env arg
            _ -> error $ printf "Unbound constructor %s" (show cname)

check :: Env -> Expression Pos -> IO (Expression TypeVar)
check env expr = case expr of
    EFun _ params body -> do
        bindings' <- HashTable.clone (eBindings env)
        paramTypes <- forM params $ \p -> do
            pt <- freshType env
            HashTable.insert p pt bindings'
            return pt

        returnType <- freshType env

        let env' = env{eBindings=bindings', eReturnType=Just returnType}
        body' <- check env' body
        unify returnType $ edata body'
        return $ EFun (TFun paramTypes returnType) params body'

    EApp _ (EIdentifier _ "_unsafe_js") [ELiteral _ (LString txt)] -> do
        t <- freshType env
        return $ EIntrinsic t (IUnsafeJs txt)
    EApp _ (EIdentifier _ "_unsafe_js") _ ->
        error "_unsafe_js takes just one string literal"

    EApp _ (EIdentifier _ "_unsafe_coerce") [subExpr] -> do
        t <- freshType env
        subExpr' <- check env subExpr
        return $ EIntrinsic t (IUnsafeCoerce subExpr')
    EApp _ (EIdentifier _ "_unsafe_coerce") _ ->
        error "_unsafe_coerce takes just one argument"

    EApp _ (EIdentifier _ "print") args -> do
        args' <- mapM (check env) args
        return $ EIntrinsic (TType Unit) (IPrint args')

    EApp _ (EIdentifier _ "toString") [arg] -> do
        arg' <- check env arg
        return $ EIntrinsic (TType String) (IToString arg')

    EApp _ (EIdentifier _ "toString") _ ->
        error "toString takes just one argument"

    EApp _ lhs rhs -> do
        lhs' <- check env lhs
        rhs' <- mapM (check env) rhs
        result <- freshType env
        unify (edata lhs') (TFun (map edata rhs') result)
        return $ EApp result lhs' rhs'

    EIntrinsic {} -> do
        error "Unexpected: EIntrinsic encountered during typechecking"

    ERecordLiteral _ fields -> do
        fields' <- forM (HashMap.toList fields) $ \(name, fieldExpr) -> do
            ty <- freshType env
            fieldExpr' <- check env fieldExpr
            unify ty (edata fieldExpr')
            return (name, fieldExpr')

        let fieldTypes = map (\(name, ex) -> (name, edata ex)) fields'

        open <- IORef.newIORef RecordClose
        props <- IORef.newIORef fieldTypes

        let recordTy = TRecord open props
        return $ ERecordLiteral recordTy (HashMap.fromList fields')

    ELookup _ lhs propName -> do
        lhs' <- check env lhs
        ty <- freshType env
        open <- IORef.newIORef RecordOpen
        props <- IORef.newIORef [(propName, ty)]
        let recTy = TRecord open props
        unify (edata lhs') recTy
        return $ ELookup ty lhs' propName

    EMatch _ matchExpr cases -> do
        resultType <- freshType env

        matchExpr' <- check env matchExpr

        cases' <- forM cases $ \(Case patt caseExpr) -> do
            env' <- childEnv env
            buildPatternEnv (edata matchExpr') env' patt
            caseExpr' <- check env' caseExpr
            unify resultType (edata caseExpr')
            return $ Case patt caseExpr'

        return $ EMatch resultType matchExpr' cases'

    ELet _ name maybeAnnot expr' -> do
        ty <- freshType env
        expr'' <- check env expr'
        HashTable.insert name ty (eBindings env)
        unify ty (edata expr'')
        forM_ maybeAnnot $ \annotation -> do
            let annotTy = resolveTypeIdent env [] annotation
            unify ty annotTy
        return $ ELet (TType Unit) name maybeAnnot expr''

    ELiteral _ lit -> do
        let litType = case lit of
                LInteger _ -> TType Number
                LString _ -> TType String
                LUnit -> TType Unit
        return $ ELiteral litType lit
    EIdentifier _ "print" ->
        error "Intrinsic print is not a value"
    EIdentifier _ "toString" ->
        error "Intrinsic toString is not a value"
    EIdentifier _ "_unsafe_js" ->
        error "Intrinsic _unsafe_js is not a value"
    EIdentifier _ "_unsafe_coerce" ->
        error "Intrinsic _unsafe_coerce is not a value"
    EIdentifier pos txt -> do
        result <- HashTable.lookup txt (eBindings env)
        case result of
            Nothing -> do
                error $ "Unbound symbol " ++ show (pos, txt)
            Just tyref -> do
                tyref' <- instantiate env tyref
                return $ EIdentifier tyref' txt
    ESemi _ lhs rhs -> do
        lhs' <- check env lhs
        rhs' <- check env rhs
        return $ ESemi (edata rhs') lhs' rhs'

    -- TEMP: For now, all binary intrinsics are Number -> Number -> Number
    EBinIntrinsic _ bi lhs rhs -> do
        lhs' <- check env lhs
        unify (TType Number) (edata lhs')

        rhs' <- check env rhs
        unify (TType Number) (edata rhs')

        return $ EBinIntrinsic (TType Number) bi lhs' rhs'

    EIfThenElse _ condition ifTrue ifFalse -> do
        condition' <- check env condition
        unify (TType Boolean) (edata condition')
        ifTrue' <- check env ifTrue
        ifFalse' <- check env ifFalse

        unify (edata ifTrue') (edata ifFalse')

        return $ EIfThenElse (edata ifTrue') condition' ifTrue' ifFalse'

    EReturn _ rv -> do
        rv' <- check env rv
        case eReturnType env of
            Nothing ->
                error "Cannot return outside of functions"
            Just rt -> do
                unify rt $ edata rv'
                return $ EReturn (edata rv') rv'

quantify :: TypeVar -> IO ()
quantify ty = do
    case ty of
        TVar i tv -> do
            tv' <- readIORef tv
            case tv' of
                Unbound -> do
                    writeIORef tv (Link $ TQuant i)
                Link t' ->
                    quantify t'
        TFun param ret -> do
            mapM_ quantify param
            quantify ret
        TRecord open rows -> do
            writeIORef open RecordOpen
            rows' <- readIORef rows
            forM_ rows' $ \(_key, val) -> do
                quantify val
        _ ->
            return ()

instantiateUserType :: IORef (HashMap Int TypeVar) -> Env -> TUserTypeDef TypeVar -> [TypeVar] -> IO (TypeVar, [TVariant TypeVar])
instantiateUserType subst env def tyVars = do
    typeVars' <- mapM (instantiate' subst env) tyVars
    let userType = TUserType def typeVars'
    variants <- forM (tuVariants def) $ \variant -> do
        paramTypes <- forM (tvParameters variant) $ \param -> do
            instantiate' subst env param
        return variant{tvParameters=paramTypes}
    return (userType, variants)

instantiate' :: IORef (HashMap Int TypeVar) -> Crux.Typecheck.Env -> TypeVar -> IO TypeVar
instantiate' subst env ty = case ty of
    TQuant name -> do
        mv <- HashTable.lookup name subst
        case mv of
            Just v ->
                return v
            Nothing -> do
                tv <- freshType env
                HashTable.insert name tv subst
                return tv
    TVar _ tv -> do
        vl <- readIORef tv
        case vl of
            Link tv' -> instantiate' subst env tv'
            Unbound  -> return ty
    TFun param ret -> do
        ty1 <- mapM (instantiate' subst env) param
        ty2 <- instantiate' subst env ret
        return $ TFun ty1 ty2
    TUserType def tyVars -> do
        typeVars' <- mapM (instantiate' subst env) tyVars
        return $ TUserType def typeVars'
    _ -> return ty

instantiate :: Env -> TypeVar -> IO TypeVar
instantiate env t = do
    subst <- HashTable.new
    instantiate' subst env t

flattenTypeDef :: TUserTypeDef TypeVar -> IO (TUserTypeDef ImmutableTypeVar)
flattenTypeDef TUserTypeDef{..} = do
    parameters' <- mapM flattenTypeVar tuParameters
    -- Huge hack: don't try to flatten variants because they can be recursive
    let variants' = []
    let td = TUserTypeDef {tuName, tuParameters=parameters', tuVariants=variants'}
    return td

flattenTypeVar :: TypeVar -> IO ImmutableTypeVar
flattenTypeVar tv = case tv of
    TVar i ior -> do
        t <- IORef.readIORef ior
        case t of
            Unbound ->
                return $ IVar i Unbound
            Link tv' -> do
                flattenTypeVar tv'
    TQuant i ->
        return $ IQuant i
    TFun arg body -> do
        arg' <- mapM flattenTypeVar arg
        body' <- flattenTypeVar body
        return $ IFun arg' body'
    TUserType def tvars -> do
        tvars' <- mapM flattenTypeVar tvars
        def' <- flattenTypeDef def
        return $ IUserType def' tvars'
    TRecord open rows -> do
        rows' <- readIORef rows
        let flattenRow (name, ty) = do
                ty' <- flattenTypeVar ty
                return (name, ty')
        rows'' <- mapM flattenRow rows'
        open' <- readIORef open
        return $ IRecord open' rows''
    TType t ->
        return $ IType t

flattenIntrinsic :: IntrinsicId TypeVar -> IO (IntrinsicId ImmutableTypeVar)
flattenIntrinsic = mapIntrinsicInputs flatten

flatten :: Expression TypeVar -> IO (Expression ImmutableTypeVar)
flatten expr = case expr of
    EFun td params body -> do
        td' <- flattenTypeVar td
        body' <- flatten body
        return $ EFun td' params body'
    EApp td lhs rhs -> do
        td' <- flattenTypeVar td
        lhs' <- flatten lhs
        rhs' <- mapM flatten rhs
        return $ EApp td' lhs' rhs'
    EIntrinsic td intrin -> do
        td' <- flattenTypeVar td
        intrin' <- flattenIntrinsic intrin
        return $ EIntrinsic td' intrin'
    ERecordLiteral td fields -> do
        td' <- flattenTypeVar td
        fields' <- forM (HashMap.toList fields) $ \(name, fieldExpr) -> do
            fieldExpr' <- flatten fieldExpr
            return (name, fieldExpr')

        return $ ERecordLiteral td' (HashMap.fromList fields')
    ELookup td lhs rhs -> do
        td' <- flattenTypeVar td
        lhs' <- flatten lhs
        return $ ELookup td' lhs' rhs
    EMatch td matchExpr cases -> do
        td' <- flattenTypeVar td
        expr' <- flatten matchExpr
        cases' <- forM cases $ \(Case pattern subExpr) ->
            fmap (Case pattern) (flatten subExpr)
        return $ EMatch td' expr' cases'
    ELet td name typeAnn expr' -> do
        td' <- flattenTypeVar td
        expr'' <- flatten expr'
        return $ ELet td' name typeAnn expr''
    ELiteral td lit -> do
        td' <- flattenTypeVar td
        return $ ELiteral td' lit
    EIdentifier td i -> do
        td' <- flattenTypeVar td
        return $ EIdentifier td' i
    ESemi td lhs rhs -> do
        td' <- flattenTypeVar td
        lhs' <- flatten lhs
        rhs' <- flatten rhs
        return $ ESemi td' lhs' rhs'
    EBinIntrinsic td name lhs rhs -> do
        td' <- flattenTypeVar td
        lhs' <- flatten lhs
        rhs' <- flatten rhs
        return $ EBinIntrinsic td' name lhs' rhs'
    EIfThenElse td condition ifTrue ifFalse -> do
        td' <- flattenTypeVar td
        condition' <- flatten condition
        ifTrue' <- flatten ifTrue
        ifFalse' <- flatten ifFalse
        return $ EIfThenElse td' condition' ifTrue' ifFalse'
    EReturn td rv -> do
        td' <- flattenTypeVar td
        rv' <- flatten rv
        return $ EReturn td' rv'

flattenDecl :: Declaration TypeVar -> IO (Declaration ImmutableTypeVar)
flattenDecl decl = case decl of
    DData name typeVars variants ->
        return $ DData name typeVars variants
    DLet ty name typeAnn expr -> do
        ty' <- flattenTypeVar ty
        expr' <- flatten expr
        return $ DLet ty' name typeAnn expr'
    DFun (FunDef ty name params body) -> do
        ty' <- flattenTypeVar ty
        body' <- flatten body
        return $ DFun $ FunDef ty' name params body'

flattenModule :: Module TypeVar -> IO (Module ImmutableTypeVar)
flattenModule Module{..} = do
    decls <- mapM flattenDecl mDecls
    return $ Module{mDecls=decls}

unificationError :: [Char] -> TypeVar -> TypeVar -> IO a
unificationError message a b = do
    sa <- showTypeVarIO a
    sb <- showTypeVarIO b
    error $ "Unification error: " ++ message ++ " " ++ sa ++ " and " ++ sb

unify :: TypeVar -> TypeVar -> IO ()
unify a b = case (a, b) of
    (TVar aid _, TVar bid _)
        | aid == bid ->
            return ()
    (TVar _ ar, _) -> do
        a' <- readIORef ar
        case a' of
            Unbound -> do
                occurs ar b
                writeIORef ar (Link b)
            Link a'' ->
                unify a'' b

    (_, TVar {}) -> do
        unify b a

    (TType aType, TType bType)
        | aType == bType ->
            return ()
        | otherwise -> do
            unificationError "" a b

    (TUserType ad atv, TUserType bd btv)
        | tuName ad == tuName bd -> do
            mapM_ (uncurry unify) (zip atv btv)
        | otherwise -> do
            unificationError "" a b

    (TRecord aOpen aRows, TRecord bOpen bRows) -> do
        aOpen' <- readIORef aOpen
        aRows' <- readIORef aRows
        bOpen' <- readIORef bOpen
        bRows' <- readIORef bRows

        let aFields = sort $ map fst aRows'
        let bFields = sort $ map fst bRows'

        case (aOpen', bOpen') of
            (RecordClose, RecordClose) -> do
                when (aFields /= bFields) $ do
                    unificationError "Record fields do not match" a b

                forM_ aFields $ \propName ->
                    let Just at = lookup propName aRows'
                        Just bt = lookup propName bRows'
                    in unify at bt

            (RecordClose, RecordOpen) -> do
                forM_ aRows' $ \(field, aTy) -> do
                    case lookup field bRows' of
                        Nothing -> unificationError "Required record field missing" a b
                        Just bTy ->
                            unify aTy bTy

                writeIORef bOpen RecordClose

            (RecordOpen, RecordClose) ->
                unify b a

            (RecordOpen, RecordOpen) -> do
                let allKeys = nub $ sort (aFields ++ bFields)
                newFields <- forM allKeys $ \key -> do
                    case (lookup key aRows', lookup key bRows') of
                        (Just t, Nothing) ->
                            return (key, t)
                        (Nothing, Just t) ->
                            return (key, t)
                        (Just t1, Just t2) -> do
                            unify t1 t2
                            return (key, t1)
                        (Nothing, Nothing) ->
                            error "Unifying rows: This should super never happen"

                writeIORef aRows newFields
                writeIORef bRows newFields

    (TFun aa ar, TFun ba br) -> do
        when (length aa /= length ba) $
            unificationError "" a b

        mapM_ (uncurry unify) (zip aa ba)
        unify ar br

    (TFun {}, TType {}) ->
        unificationError "" a b
    (TType {}, TFun {}) ->
        unificationError "" a b

    -- These should never happen: Quantified type variables should be instantiated before we get here.
    (TQuant {}, _) -> do
        lt <- showTypeVarIO a
        rt <- showTypeVarIO b
        error $ printf "Internal error: QVar made it to unify %s and %s" lt rt
    (_, TQuant {}) -> do
        lt <- showTypeVarIO a
        rt <- showTypeVarIO b
        error $ printf "Internal error: QVar made it to unify %s and %s" lt rt

    _ ->
        unificationError "" a b

occurs :: IORef (VarLink TypeVar) -> TypeVar -> IO ()
occurs tvr ty = case ty of
    TVar _ ty'
        | tvr == ty' -> error "Occurs check"
        | otherwise -> do
            ty'' <- readIORef ty'
            case ty'' of
                Link ty''' -> occurs tvr ty'''
                _ -> return ()
    TFun arg ret -> do
        mapM_ (occurs tvr) arg
        occurs tvr ret
    _ ->
        return ()

checkDecl :: Env -> Declaration Pos -> IO (Declaration TypeVar)
checkDecl env decl = case decl of
    DData name typeVars variants ->
        -- TODO: Verify that all types referred to by variants exist, or are typeVars
        return $ DData name typeVars variants
    DFun (FunDef pos name args body) -> do
        ty <- freshType env
        HashTable.insert name ty (eBindings env)
        let expr = EFun pos args body
        expr'@(EFun _ _ body') <- check env expr
        unify (edata expr') ty
        quantify ty
        return $ DFun $ FunDef (edata expr') name args body'
    DLet _ name maybeAnnot expr -> do
        expr' <- check env expr
        let ty = edata expr'
        HashTable.insert name ty (eBindings env)
        quantify ty
        forM_ maybeAnnot $ \annotation -> do
            let annotTy = resolveTypeIdent env [] annotation
            unify ty annotTy
        return $ DLet (edata expr') name maybeAnnot expr'

buildTypeEnvironment :: [Declaration a] -> IO Env
buildTypeEnvironment decls = do
    e <- newEnv Nothing
    typeEnv <- IORef.newIORef $ HashMap.fromList
        [ ("Number", TType Number)
        , ("Unit", TType Unit)
        , ("String", TType String)
        , ("Boolean", TType Boolean)
        ]

    HashTable.insert "True" (TType Boolean) (eBindings e)
    HashTable.insert "False" (TType Boolean) (eBindings e)

    -- qvarsRef :: IORef HashMap (Name, [(TypeVariable, TypeVar)])
    qvarsRef <- HashTable.new

    -- First, populate the type environment.  Variant parameter types are all initially free.
    forM_ decls $ \decl -> case decl of
        DData name typeVarNames variants -> do
            typeVars <- forM typeVarNames $ const $ do
                ft <- freshType e
                quantify ft
                return ft
            let typeVarTable = zip typeVarNames typeVars
            HashTable.insert name typeVarTable qvarsRef

            variants' <- forM variants $ \Variant{..} -> do
                tvParameters <- forM vparameters $ const $ freshType e
                let tvName = vname
                return TVariant{..}

            let typeDef = TUserTypeDef
                    { tuName = name
                    , tuParameters = typeVars
                    , tuVariants = variants'
                    }
            let userType = TUserType typeDef typeVars
            IORef.modifyIORef' typeEnv (HashMap.insert name userType)
        _ -> return ()

    qvars <- IORef.readIORef qvarsRef
    te <- IORef.readIORef typeEnv
    let env = e{eTypeBindings=te}

    -- Second, unify parameter types
    forM_ decls $ \decl -> case decl of
        DData name _typeVarNames variants -> do
            Just (TUserType typeDef _) <- HashTable.lookup name typeEnv
            let TUserTypeDef { tuVariants = tvariants } = typeDef
            forM_ (zip variants tvariants) $ \(v, tv) -> do
                forM_ (zip (vparameters v) (tvParameters tv)) $ \(typeIdent, typeVar) -> do
                    let Just qv = HashMap.lookup name qvars
                    let t = resolveTypeIdent env qv typeIdent
                    unify typeVar t
        _ -> return ()

    let computeVariantType ty qvarNames _name argTypeIdents = case argTypeIdents of
            [] -> ty
            _ ->
                let resolvedArgTypes = map (resolveTypeIdent env qvarNames) argTypeIdents
                in TFun resolvedArgTypes ty

    forM_ (HashMap.toList Intrinsic.intrinsics) $ \(name, intrin) -> do
        let Intrinsic{..} = intrin
        HashTable.insert name iType (eBindings env)

    -- Note to self: Here we need to match the names of the types of each variant up with concrete types, but also
    -- with the TypeVars created in the type environment.
    forM_ decls $ \decl -> case decl of
        DData name _ variants -> do
            let Just userType = HashMap.lookup name te
            let Just qvarTable = HashMap.lookup name qvars
            forM_ variants $ \(Variant vname vdata) -> do
                let ctorType = computeVariantType userType qvarTable vname vdata
                HashTable.insert vname ctorType (eBindings env)
        _ -> return ()

    return env{eTypeBindings=te}

resolveTypeIdent :: Env -> [(TypeName, TypeVar)] -> TypeIdent -> TypeVar
resolveTypeIdent env qvarTable typeIdent =
    go typeIdent
  where
    te = eTypeBindings env
    go (TypeIdent typeName typeParameters) =
        if isCapitalized typeName
            then case HashMap.lookup typeName te of
                Nothing -> error $ printf "Constructor uses nonexistent type %s" (show typeName)
                Just t@(TType {}) ->
                    if [] /= typeParameters
                        then error "Primitive types don't take type parameters"
                        else t
                Just (TUserType def _) ->
                    if length qvarTable /= length typeParameters
                        then
                            error $ printf "Type %s takes %i type parameters.  %i given" (show $ tuName def) (length qvarTable) (length typeParameters)
                        else
                            TUserType def (map go typeParameters)
                Just t ->
                    t
            else case lookup typeName qvarTable of
                Nothing -> error $ printf "Constructor uses nonexistent type variable %s" (show typeName)
                Just t -> t

run :: Module Pos -> IO (Module TypeVar)
run Module{..} = do
    env <- buildTypeEnvironment mDecls
    decls <- forM mDecls (checkDecl env)
    return Module{mDecls=decls}
