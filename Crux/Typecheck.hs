{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Crux.Typecheck where

import           Control.Monad         (when)
import           Crux.AST
import           Crux.Intrinsic        (Intrinsic (..))
import qualified Crux.Intrinsic        as Intrinsic
import qualified Crux.MutableHashTable as HashTable
import           Crux.Text             (isCapitalized)
import           Crux.Tokens           (Pos (..))
import           Data.Foldable         (foldlM)
import           Data.Foldable         (forM_)
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as HashMap
import           Data.IORef            (IORef, readIORef, writeIORef)
import qualified Data.IORef            as IORef
import           Data.List             (intercalate, nub, sort)
import           Data.Maybe            (catMaybes)
import           Data.Monoid           ((<>))
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           Data.Traversable      (forM)
import           Prelude               hiding (String)
import           Text.Printf           (printf)

copyIORef :: IORef a -> IO (IORef a)
copyIORef ior = do
    v <- IORef.readIORef ior
    IORef.newIORef v

data Env = Env
    { eNextTypeIndex :: IORef Int
    , eBindings      :: IORef (HashMap Text TypeVar)
    , eTypeBindings  :: HashMap Text TypeVar
    , eReturnType    :: Maybe TypeVar -- Nothing if top-level expression
    }

showTypeVarIO :: TypeVar -> IO [Char]
showTypeVarIO tvar = do
  tvar' <- readIORef tvar
  case tvar' of
    TVar i o' -> do
        os <- case o' of
            Unbound j -> return $ "Unbound " ++ show j
            Link x -> showTypeVarIO x
        return $ "(TVar " ++ show i ++ " " ++ os ++ ")"
    TQuant i ->
        return $ "TQuant " ++ show i
    TFun arg ret -> do
        as <- mapM showTypeVarIO arg
        rs <- showTypeVarIO ret
        return $ "TFun (" ++ intercalate "," as ++ ") -> " ++ rs
    TUserType def tvars -> do
        tvs <- mapM showTypeVarIO tvars
        return $ (Text.unpack $ tuName def) ++ " " ++ (intercalate " " tvs)
    TRecord open' rows' -> do
        let rowNames = map fst rows'
        rowTypes <- mapM (showTypeVarIO . snd) rows'
        let showRow (name, ty) = Text.unpack name <> ": " <> ty
        let dotdotdot = case open' of
                RecordFree -> ["f..."]
                RecordQuantified -> ["q..."]
                RecordClose -> []
        return $ "{" <> (intercalate "," (map showRow (zip rowNames rowTypes) <> dotdotdot)) <> "}"
    TType ty ->
        return $ show ty

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
    IORef.newIORef $ TVar index (Unbound index)

typeFromConstructor :: Env -> Name -> IO (Maybe (TypeVar, TVariant TypeVar))
typeFromConstructor env cname = do
    let fold acc ty = case (acc, ty) of
            (Just a, _) -> return $ Just a
            (Nothing, ut) -> do
                ut' <- IORef.readIORef ut
                case ut' of
                    TUserType def _ -> do
                        let TUserTypeDef { tuVariants = variants } = def
                        case [v | v@(TVariant vname _) <- variants, vname == cname] of
                            [v] -> return $ Just (ut, v)
                            [] -> return Nothing
                            _ -> error "This should never happen: Type has multiple variants with the same constructor name"
                    _ ->
                        return Nothing
    foldlM fold Nothing (HashMap.elems $ eTypeBindings env)

-- | Build up an environment for a case of a match block.
-- exprType is the type of the expression.  We unify this with the constructor of the pattern
buildPatternEnv :: TypeVar -> Env -> Pattern -> IO ()
buildPatternEnv exprType env patt = case patt of
    PPlaceholder pname -> do
        HashTable.insert pname exprType (eBindings env)

    PConstructor cname cargs -> do
        ctor <- typeFromConstructor env cname
        case ctor of
            Just (ctorTy, _) -> do
                t <- readIORef ctorTy
                case t of
                    TUserType def tyVars -> do
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
                    _ -> error "buildPatternEnv: Pattern for non-sum type??  This should never happen."
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
        ty <- IORef.newIORef $ TFun paramTypes returnType
        return $ EFun ty params body'

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
        ty <- IORef.newIORef $ TType Unit
        return $ EIntrinsic ty (IPrint args')

    EApp _ (EIdentifier _ "toString") [arg] -> do
        arg' <- check env arg
        ty <- IORef.newIORef $ TType String
        return $ EIntrinsic ty (IToString arg')

    EApp _ (EIdentifier _ "toString") _ ->
        error "toString takes just one argument"

    EApp _ lhs rhs -> do
        lhs' <- check env lhs
        rhs' <- mapM (check env) rhs
        result <- freshType env
        ty <- IORef.newIORef $ TFun (map edata rhs') result
        unify (edata lhs') ty
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

        recordTy <- IORef.newIORef $ TRecord RecordClose fieldTypes
        return $ ERecordLiteral recordTy (HashMap.fromList fields')

    ELookup _ lhs propName -> do
        lhs' <- check env lhs
        ty <- freshType env
        recTy <- IORef.newIORef $ TRecord RecordFree [(propName, ty)]
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
            annotTy <- resolveTypeIdent env [] annotation
            unify ty annotTy

        unitTy <- IORef.newIORef $ TType Unit
        return $ ELet unitTy name maybeAnnot expr''

    ELiteral _ lit -> do
        litType <- IORef.newIORef $ case lit of
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
        numTy <- IORef.newIORef $ TType Number

        lhs' <- check env lhs
        unify numTy (edata lhs')

        rhs' <- check env rhs
        unify numTy (edata rhs')

        return $ EBinIntrinsic numTy bi lhs' rhs'

    EIfThenElse _ condition ifTrue ifFalse -> do
        condition' <- check env condition
        boolTy <- IORef.newIORef $ TType Boolean
        unify boolTy (edata condition')
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
    ty' <- readIORef ty
    case ty' of
        TVar i tv' -> do
            case tv' of
                Unbound j -> do
                    qTy <- IORef.newIORef $ TQuant j
                    writeIORef ty (TVar i $ Link qTy)
                Link t' ->
                    quantify t'
        TFun param ret -> do
            mapM_ quantify param
            quantify ret
        TRecord open rows' -> do
            forM_ rows' $ \(_key, val) -> do
                quantify val
            let open' = case open of
                    RecordFree -> RecordQuantified
                    _ -> open
            writeIORef ty $ TRecord open' rows'
        _ ->
            return ()

instantiateUserType :: IORef (HashMap Int TypeVar) -> Env -> TUserTypeDef TypeVar -> [TypeVar] -> IO (TypeVar, [TVariant TypeVar])
instantiateUserType subst env def tyVars = do
    typeVars' <- mapM (instantiate' subst env) tyVars
    let typeVars'' = if and (map fst typeVars')
            then map snd typeVars'
            else tyVars
    userType <- IORef.newIORef $ TUserType def typeVars''
    variants <- forM (tuVariants def) $ \variant -> do
        paramTypes <- forM (tvParameters variant) $ \param -> do
            instantiate' subst env param
        return variant{tvParameters=map snd paramTypes}
    return (userType, variants)

instantiate' :: IORef (HashMap Int TypeVar) -> Crux.Typecheck.Env -> TypeVar -> IO (Bool, TypeVar)
instantiate' subst env ty = do
    ty' <- readIORef ty
    case ty' of
        TQuant name -> do
            mv <- HashTable.lookup name subst
            case mv of
                Just v ->
                    return (True, v)
                Nothing -> do
                    tv <- freshType env
                    HashTable.insert name tv subst
                    return (True, tv)
        TVar _ vl -> do
            case vl of
                Link tv'  -> instantiate' subst env tv'
                Unbound _ -> return (False, ty)
        TFun param ret -> do
            ty1 <- mapM (instantiate' subst env) param
            (b2, ty2) <- instantiate' subst env ret
            let b = b2 || and (map fst ty1)
            tfun <- IORef.newIORef $ TFun (map snd ty1) ty2
            return (b, tfun)
        TUserType def tyVars -> do
            typeVars' <- mapM (instantiate' subst env) tyVars
            tut <- IORef.newIORef $ TUserType def (map snd typeVars')
            return (and (map fst typeVars'), tut)
        TRecord open rows -> do
            rows' <- forM rows $ \(name, rowTy) -> do
                (b, rowTy') <- instantiate' subst env rowTy
                return (b, (name, rowTy'))

            let (changedOpen, open') = case open of
                    RecordQuantified -> (True, RecordFree)
                    _ -> (False, open)
            tr <- IORef.newIORef $ TRecord open' (map snd rows')
            return (changedOpen || or (map fst rows'), tr)
        TType {} -> return (False, ty)

instantiate :: Env -> TypeVar -> IO TypeVar
instantiate env t = do
    subst <- HashTable.new
    (didAnything, t') <- instantiate' subst env t
    return $ if didAnything
        then t'
        else t

flattenTypeDef :: TUserTypeDef TypeVar -> IO (TUserTypeDef ImmutableTypeVar)
flattenTypeDef TUserTypeDef{..} = do
    parameters' <- mapM flattenTypeVar tuParameters
    -- Huge hack: don't try to flatten variants because they can be recursive
    let variants' = []
    let td = TUserTypeDef {tuName, tuParameters=parameters', tuVariants=variants'}
    return td

flattenTypeVar :: TypeVar -> IO ImmutableTypeVar
flattenTypeVar tv = do
    tv' <- IORef.readIORef tv
    case tv' of
        TVar i t -> do
            case t of
                Unbound j ->
                    return $ IVar i (Unbound j)
                Link tv'' -> do
                    flattenTypeVar tv''
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
        TRecord open' rows' -> do
            let flattenRow (name, ty) = do
                    ty' <- flattenTypeVar ty
                    return (name, ty')
            rows'' <- mapM flattenRow rows'
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
unify av bv = do
  a <- readIORef av
  b <- readIORef bv

  case (a, b) of
    (TVar aid _, TVar bid _)
        | aid == bid ->
            return ()
    (TVar i a', _) -> do
        case a' of
            Unbound _ -> do
                occurs a' bv
                writeIORef av (TVar i $ Link bv)
            Link a'' ->
                unify a'' bv

    (_, TVar {}) -> do
        unify bv av

    (TType aType, TType bType)
        | aType == bType ->
            return ()
        | otherwise -> do
            unificationError "" av bv

    (TUserType ad atv, TUserType bd btv)
        | tuName ad == tuName bd -> do
            mapM_ (uncurry unify) (zip atv btv)
        | otherwise -> do
            unificationError "" av bv

    (TRecord aOpen' aRows', TRecord bOpen' bRows') -> do
        let aFields = sort $ map fst aRows'
        let bFields = sort $ map fst bRows'

        case (aOpen', bOpen') of
            (RecordClose, RecordClose) -> do
                when (aFields /= bFields) $ do
                    unificationError "Record fields do not match" av bv

                forM_ aFields $ \propName ->
                    let Just at = lookup propName aRows'
                        Just bt = lookup propName bRows'
                    in unify at bt

            (RecordClose, RecordFree) -> do
                -- We can infer that rhs must have all the properties of lhs and no more.  Further, the intersection must unify.
                forM_ aRows' $ \(field, aTy) -> do
                    case lookup field bRows' of
                        Nothing ->
                            return ()
                        Just bTy ->
                            unify aTy bTy

                let i = 888 -- hack.  Probably doesn't matter.
                writeIORef bv (TVar i $ Link av)

            (RecordFree, RecordClose) ->
                unify bv av

            (RecordFree, RecordFree) -> do
                -- fields of lhs and rhs must unify.  Result is free record containing the union of fields in lhs and rhs
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

                writeIORef av $ TRecord RecordFree newFields

                let i = 888 -- hack
                writeIORef bv (TVar i $ Link av)

            (RecordQuantified, RecordFree) -> do
                -- Fields of rhs must appear in lhs.  Result is quantified record having fields of lhs
                let allKeys = nub $ sort (aFields ++ bFields)
                newFields <- fmap catMaybes $ forM allKeys $ \key -> do
                    case (lookup key aRows', lookup key bRows') of
                        (Just t1, Just t2) -> do
                            unify t1 t2
                            return $ Just (key, t1)
                        (Just t1, Nothing) ->
                            return $ Just (key, t1)
                        (Nothing, Just _) -> do
                            as <- showTypeVarIO av
                            unificationError ("Field '" ++ Text.unpack key ++ "' not found in quantified record " ++ as) av bv
                        _ ->
                            error "Unifying rows (q): This should super never happen"

                writeIORef av $ TRecord RecordQuantified newFields

                let i = 888 -- hack
                writeIORef bv (TVar i $ Link av)

            (RecordQuantified, RecordQuantified) -> do
                -- Fields of rhs must overlap exactly with lhs.  Result is quantified record having fields of lhs
                let allKeys = nub $ sort (aFields ++ bFields)
                newFields <- fmap catMaybes $ forM allKeys $ \key -> do
                    case (lookup key aRows', lookup key bRows') of
                        (Just t1, Just t2) -> do
                            unify t1 t2
                            return $ Just (key, t1)
                        (Just _, Nothing) -> do
                            bs <- showTypeVarIO bv
                            unificationError ("Field '" ++ Text.unpack key ++ "' not found in quantified record " ++ bs) av bv
                        (Nothing, Just _) -> do
                            as <- showTypeVarIO av
                            unificationError ("Field '" ++ Text.unpack key ++ "' not found in quantified record " ++ as) av bv
                        _ ->
                            error "Unifying rows (q): This should super never happen"

                writeIORef av $ TRecord RecordQuantified newFields

                let i = 888 -- hack
                writeIORef bv (TVar i $ Link av)

            (RecordFree, RecordQuantified) ->
                unify bv av

            (RecordClose, RecordQuantified) ->
                unificationError "" av bv
            (RecordQuantified, RecordClose) ->
                unificationError "" av bv

    (TFun aa ar, TFun ba br) -> do
        when (length aa /= length ba) $
            unificationError "" av bv

        mapM_ (uncurry unify) (zip aa ba)
        unify ar br

    (TFun {}, TType {}) ->
        unificationError "" av bv
    (TType {}, TFun {}) ->
        unificationError "" av bv

    -- These should never happen: Quantified type variables should be instantiated before we get here.
    (TQuant {}, _) -> do
        lt <- showTypeVarIO av
        rt <- showTypeVarIO bv
        error $ printf "Internal error: QVar made it to unify %s and %s" lt rt
    (_, TQuant {}) -> do
        lt <- showTypeVarIO av
        rt <- showTypeVarIO bv
        error $ printf "Internal error: QVar made it to unify %s and %s" lt rt

    _ ->
        unificationError "" av bv

occurs :: VarLink TypeVar -> TypeVar -> IO ()
occurs tvr ty = do
    tyy <- readIORef ty
    case tyy of
        TVar _ ty''
            | tvr == ty'' -> do
                error $ "Occurs check failed"
            | otherwise -> do
                case ty'' of
                    Link ty''' -> occurs tvr ty'''
                    _ -> return ()
        TFun arg ret -> do
            mapM_ (occurs tvr) arg
            occurs tvr ret
        TUserType _ tvars -> do
            mapM_ (occurs tvr) tvars
        TRecord _ rows -> do
            forM_ rows $ \(_, rowTy) ->
                occurs tvr rowTy
        TType {} ->
            return ()
        TQuant {} ->
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
        forM_ maybeAnnot $ \annotation -> do
            annotTy <- resolveTypeIdent env [] annotation
            unify ty annotTy
        quantify ty
        return $ DLet (edata expr') name maybeAnnot expr'

buildTypeEnvironment :: [Declaration a] -> IO Env
buildTypeEnvironment decls = do
    numTy <- IORef.newIORef $ TType Number
    unitTy <- IORef.newIORef $ TType Unit
    strTy <- IORef.newIORef $ TType String
    boolTy <- IORef.newIORef $ TType Boolean

    e <- newEnv Nothing
    typeEnv <- IORef.newIORef $ HashMap.fromList
        [ ("Number", numTy)
        , ("Unit", unitTy)
        , ("String", strTy)
        , ("Boolean", boolTy)
        ]


    HashTable.insert "True" boolTy (eBindings e)
    HashTable.insert "False" boolTy (eBindings e)

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
            userType <- IORef.newIORef $ TUserType typeDef typeVars
            IORef.modifyIORef' typeEnv (HashMap.insert name userType)
        _ -> return ()

    qvars <- IORef.readIORef qvarsRef
    te <- IORef.readIORef typeEnv
    let env = e{eTypeBindings=te}

    -- Second, unify parameter types
    forM_ decls $ \decl -> case decl of
        DData name _typeVarNames variants -> do
            Just ty <- HashTable.lookup name typeEnv
            TUserType typeDef _ <- readIORef ty

            let TUserTypeDef { tuVariants = tvariants } = typeDef
            forM_ (zip variants tvariants) $ \(v, tv) -> do
                forM_ (zip (vparameters v) (tvParameters tv)) $ \(typeIdent, typeVar) -> do
                    let Just qv = HashMap.lookup name qvars
                    t <- resolveTypeIdent env qv typeIdent
                    unify typeVar t
        _ -> return ()

    let computeVariantType ty qvarNames _name argTypeIdents = case argTypeIdents of
            [] ->
                return ty
            _ -> do
                resolvedArgTypes <- mapM (resolveTypeIdent env qvarNames) argTypeIdents
                IORef.newIORef $ TFun resolvedArgTypes ty

    intrinsics <- Intrinsic.intrinsics

    forM_ (HashMap.toList intrinsics) $ \(name, intrin) -> do
        let Intrinsic{..} = intrin
        HashTable.insert name iType (eBindings env)

    -- Note to self: Here we need to match the names of the types of each variant up with concrete types, but also
    -- with the TypeVars created in the type environment.
    forM_ decls $ \decl -> case decl of
        DData name _ variants -> do
            let Just userType = HashMap.lookup name te
            let Just qvarTable = HashMap.lookup name qvars
            forM_ variants $ \(Variant vname vdata) -> do
                ctorType <- computeVariantType userType qvarTable vname vdata
                HashTable.insert vname ctorType (eBindings env)
        _ -> return ()

    return env{eTypeBindings=te}

resolveTypeIdent :: Env -> [(TypeName, TypeVar)] -> TypeIdent -> IO TypeVar
resolveTypeIdent env qvarTable typeIdent =
    go typeIdent
  where
    te = eTypeBindings env
    go (TypeIdent typeName typeParameters) =
        if isCapitalized typeName
            then case HashMap.lookup typeName te of
                Nothing -> error $ printf "Constructor uses nonexistent type %s" (show typeName)
                Just ty -> do
                    ty' <- readIORef ty
                    case ty' of
                        TType {} ->
                            if [] /= typeParameters
                                then error "Primitive types don't take type parameters"
                                else return ty
                        TUserType def _ ->
                            if length qvarTable /= length typeParameters
                                then
                                    error $ printf "Type %s takes %i type parameters.  %i given" (show $ tuName def) (length qvarTable) (length typeParameters)
                                else do
                                    params <- mapM go typeParameters
                                    IORef.newIORef $ TUserType def params
                        _ ->
                            return ty
            else case lookup typeName qvarTable of
                Nothing ->
                    error $ printf "Constructor uses nonexistent type variable %s" (show typeName)
                Just t ->
                    return t

    go (RecordIdent rows) = do
        rows' <- forM rows $ \(rowName, rowTy) ->
            fmap (rowName,) (go rowTy)
        IORef.newIORef $ TRecord RecordClose rows'

    go (FunctionIdent argTypes retType) = do
        argTypes' <- mapM go argTypes
        retType' <- go retType
        IORef.newIORef $ TFun argTypes' retType'

run :: Module Pos -> IO (Module TypeVar)
run Module{..} = do
    env <- buildTypeEnvironment mDecls
    decls <- forM mDecls (checkDecl env)
    return Module{mDecls=decls}
