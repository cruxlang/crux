{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Crux.Typecheck where

import           Control.Monad         (forM, forM_, when, foldM)
import           Crux.AST
import           Crux.Intrinsic        (Intrinsic (..))
import qualified Crux.Intrinsic        as Intrinsic
import qualified Crux.MutableHashTable as HashTable
import           Crux.Tokens           (Pos (..))
import Crux.Text (isCapitalized)
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as HashMap
import           Data.IORef            (IORef, readIORef, writeIORef)
import qualified Data.IORef            as IORef
import           Data.List             (foldl', intercalate)
import           Data.Text             (Text)
import           Prelude               hiding (String)
import           Text.Printf           (printf)

data Env = Env
    { eNextTypeIndex :: IORef Int
    , eBindings      :: IORef (HashMap Text TypeVar)
    , eTypeBindings  :: HashMap Text TypeVar
    , eIsTopLevel    :: Bool
    }

showTypeVarIO :: TypeVar -> IO [Char]
showTypeVarIO tvar = case tvar of
    TVar i o -> do
        o' <- readIORef o
        os <- case o' of
            Unbound -> return "Unbound"
            Link x -> showTypeVarIO x
        return $ "TVar " ++ show i ++ " " ++ os
    TQuant i ->
        return $ "TQuant " ++ show i
    TFun arg ret -> do
        as <- showTypeVarIO arg
        rs <- showTypeVarIO ret
        return $ "TFun " ++ show as  ++ " -> " ++ rs
    TUserType name tvars _ -> do
        tvs <- mapM showTypeVarIO tvars
        return $ (show name) ++ " " ++ (intercalate " " tvs)
    TType ty ->
        return $ "TType " ++ show ty

newEnv :: IO Env
newEnv = do
    eNextTypeIndex <- IORef.newIORef 0
    eBindings <- IORef.newIORef HashMap.empty
    let eTypeBindings = HashMap.empty
    let eIsTopLevel = True
    return Env {..}

childEnv :: Env -> IO Env
childEnv env = do
    bindings' <- HashTable.clone (eBindings env)
    return env{eBindings=bindings', eIsTopLevel=False}

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
            (Nothing, ut@(TUserType _ _ variants)) ->
                case [v | v@(TVariant vname _) <- variants, vname == cname] of
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
            Just (ty@(TUserType {}), _) -> do
                ty' <- instantiate env ty
                unify exprType ty'

                let findVariant [] = error "findVariant: This should never happen"
                    findVariant (v:rest)
                        | tvName v == cname = Just v
                        | otherwise = findVariant rest

                let TUserType _ _ variants = ty'
                let Just variant = findVariant variants
                let TVariant{tvParameters} = variant


                when (length tvParameters /= length cargs) $
                    error $ printf "Pattern should specify %i args but got %i" (length tvParameters) (length cargs)

                st <- showTypeVarIO ty'
                print $ ("pconstr", st)
                -- AHA Here we need the unified type variables.  Right now we're getting the quantified ones still.
                pp <- mapM showTypeVarIO tvParameters
                print ("pp", pp)

                -- Problem here: The TUserType constructor only holds type *names* of variants.  Need to expand that to include the TypeVar

                forM_ (zip cargs tvParameters) $ \(arg, vp) -> do
                    buildPatternEnv vp env arg
            _ -> error $ printf "Unbound constructor %s" (show cname)

check :: Env -> Expression Pos -> IO (Expression TypeVar)
check env expr = case expr of
    EBlock _ exprs -> do
        bindings' <- HashTable.clone (eBindings env)
        let env' = env{eBindings=bindings'}
        case exprs of
            [] -> do
                return $ EBlock (TType Unit) []
            _ -> do
                exprs' <- forM exprs (check env')
                return $ EBlock (edata $ last exprs') exprs'
    EFun _ param exprs -> do
        bindings' <- HashTable.clone (eBindings env)
        paramType <- freshType env
        HashTable.insert param paramType bindings'

        case exprs of
            [] -> do
                return $ EFun (TFun paramType (TType Unit)) param []
            _ -> do
                let env' = env{eBindings=bindings', eIsTopLevel=False}
                exprs' <- forM exprs (check env')
                return $ EFun (TFun paramType (edata $ last exprs')) param exprs'

    EApp _ lhs rhs -> do
        lhs' <- check env lhs
        rhs' <- check env rhs
        result <- freshType env
        unify (edata lhs') (TFun (edata rhs') result)
        return $ EApp result lhs' rhs'

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

    ELet _ Rec name expr' -> do
        ty <- freshType env
        HashTable.insert name ty (eBindings env)
        expr'' <- check env expr'
        unify ty (edata expr'')
        when (eIsTopLevel env) $ do
            quantify ty
        return $ ELet ty Rec name expr''

    ELet _ NoRec name expr' -> do
        ty <- freshType env
        expr'' <- check env expr'
        HashTable.insert name ty (eBindings env)
        unify ty (edata expr'')
        when (eIsTopLevel env) $ do
            quantify ty
        return $ ELet ty NoRec name expr''

    EPrint _ expr' -> do
        expr'' <- check env expr'
        return $ EPrint (TType Unit) expr''
    EToString _ expr' -> do
        expr'' <- check env expr'
        return $ EToString (TType String) expr''
    ELiteral _ (LInteger i) -> do
        return $ ELiteral (TType Number) (LInteger i)
    ELiteral _ (LString s) -> do
        return $ ELiteral (TType String) (LString s)
    ELiteral _ LUnit -> do
        return $ ELiteral (TType Unit) LUnit
    EIdentifier pos txt -> do
        result <- HashTable.lookup txt (eBindings env)
        case result of
            Nothing -> do
                eb <- readIORef $ eBindings env

                error $ "Unbound symbol " ++ show (pos, txt, HashMap.keys eb)
            Just tyref -> do
                tyref' <- instantiate env tyref
                ts <- showTypeVarIO tyref'
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
            quantify param
            quantify ret
        _ ->
            return ()

instantiate :: Env -> TypeVar -> IO TypeVar
instantiate env t = do
    subst <- HashTable.new
    let go ty = case ty of
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
                    Link tv' -> go tv'
                    Unbound  -> return ty
            TFun param ret -> do
                ty1 <- go param
                ty2 <- go ret
                return $ TFun ty1 ty2
            TUserType name tyVars variants -> do
                typeVars' <- mapM go tyVars
                variants' <- forM variants $ \TVariant{..} -> do
                    parameters' <- mapM go tvParameters
                    return $ TVariant {tvName, tvParameters=parameters'}
                return $ TUserType name typeVars' variants'
            _ -> return ty
    go t

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
        arg' <- flattenTypeVar arg
        body' <- flattenTypeVar body
        return $ IFun arg' body'
    TUserType name tvars variants -> do
        tvars' <- mapM flattenTypeVar tvars
        variants' <- forM variants $ \TVariant{..} -> do
            tvParameters' <- mapM flattenTypeVar tvParameters
            return $ TVariant { tvName, tvParameters=tvParameters'}
        return $ IUserType name tvars' variants'
    TType t ->
        return $ IType t

flatten :: Expression TypeVar -> IO (Expression ImmutableTypeVar)
flatten expr = case expr of
    EBlock td exprs -> do
        td' <- flattenTypeVar td
        exprs' <- forM exprs flatten
        return $ EBlock td' exprs'
    EFun td params exprs -> do
        td' <- flattenTypeVar td
        exprs' <- forM exprs flatten
        return $ EFun td' params exprs'
    EApp td lhs rhs -> do
        td' <- flattenTypeVar td
        lhs' <- flatten lhs
        rhs' <- flatten rhs
        return $ EApp td' lhs' rhs'
    EMatch td matchExpr cases -> do
        td' <- flattenTypeVar td
        expr' <- flatten matchExpr
        cases' <- forM cases $ \(Case pattern subExpr) ->
            fmap (Case pattern) (flatten subExpr)
        return $ EMatch td' expr' cases'
    ELet td rec name expr' -> do
        td' <- flattenTypeVar td
        expr'' <- flatten expr'
        return $ ELet td' rec name expr''
    EPrint td expr' -> do
        td' <- flattenTypeVar td
        expr'' <- flatten expr'
        return $ EPrint td' expr''
    EToString td expr' -> do
        td' <- flattenTypeVar td
        expr'' <- flatten expr'
        return $ EToString td' expr''
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

flattenDecl :: Declaration TypeVar -> IO (Declaration ImmutableTypeVar)
flattenDecl decl = case decl of
    DData name typeVars variants ->
        return $ DData name typeVars variants
    DLet ty rec name expr -> do
        ty' <- flattenTypeVar ty
        expr' <- flatten expr
        return $ DLet ty' rec name expr'

unify :: TypeVar -> TypeVar -> IO ()
unify a b = case (a, b) of
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
            error ("unification failure: " ++ (show (aType, bType)))

    (TUserType an atv _av, TUserType bn btv _bv)
        | an == bn -> do
            mapM_ (uncurry unify) (zip atv btv)
        | otherwise -> do
            error ("Unification failure: " ++ (show (an, bn)))

    (TFun aa ar, TFun ba br) -> do
        unify aa ba
        unify ar br

    (TFun {}, TType {}) -> do
        lt <- showTypeVarIO a
        rt <- showTypeVarIO b
        error $ "Unification failure: " ++ (show (lt, rt))
    (TType {}, TFun {}) -> do
        lt <- showTypeVarIO a
        rt <- showTypeVarIO b
        error $ "Unification failure: " ++ (show (lt, rt))

    -- These should never happen: Quantified type variables should be instantiated before we get here.
    (TQuant {}, _) -> do
        lt <- showTypeVarIO a
        rt <- showTypeVarIO b
        error $ printf "Internal error: QVar made it to unify %s and %s" lt rt
    (_, TQuant {}) -> do
        lt <- showTypeVarIO a
        rt <- showTypeVarIO b
        error $ printf "Internal error: QVar made it to unify %s and %s" lt rt

    _ -> do
        as <- showTypeVarIO a
        bs <- showTypeVarIO b
        error $ "Unification failure: " ++ (show (as, bs))

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
        occurs tvr arg
        occurs tvr ret
    _ ->
        return ()

checkDecl :: Env -> Declaration Pos -> IO (Declaration TypeVar)
checkDecl env decl = case decl of
    DData name typeVars variants ->
        -- TODO: Verify that all types referred to by variants exist, or are typeVars
        return $ DData name typeVars variants
    DLet _ Rec name expr -> do
        ty <- freshType env
        HashTable.insert name ty (eBindings env)
        expr' <- check env expr
        unify (edata expr') ty
        return $ DLet (edata expr') Rec name expr'
    DLet _ NoRec name expr -> do
        expr' <- check env expr
        let ty = edata expr'
        HashTable.insert name ty (eBindings env)
        return $ DLet (edata expr') NoRec name expr'

buildTypeEnvironment :: [Declaration a] -> IO Env
buildTypeEnvironment decls = do
    env <- newEnv
    typeEnv <- IORef.newIORef $ HashMap.fromList
        [ ("Number", TType Number)
        , ("Unit", TType Unit)
        , ("String", TType String)
        ]

    -- qvarsRef :: IORef HashMap (Name, [(TypeVariable, TypeVar)])
    qvarsRef <- HashTable.new

    -- First, populate the type environment.  Variant parameter types are all initially free.
    forM_ decls $ \decl -> case decl of
        DData name typeVarNames variants -> do
            typeVars <- forM typeVarNames $ const $ do
                ft <- freshType env
                quantify ft
                return ft
            let typeVarTable = zip typeVarNames typeVars
            HashTable.insert name typeVarTable qvarsRef

            variants' <- forM variants $ \Variant{..} -> do
                tvParameters <- forM vparameters $ const $ freshType env
                let tvName = vname
                return TVariant{..}

            let userType = TUserType name typeVars variants'
            IORef.modifyIORef' typeEnv (HashMap.insert name userType)
        _ -> return ()

    qvars <- IORef.readIORef qvarsRef
    te <- IORef.readIORef typeEnv

    -- Second, unify parameter types
    forM_ decls $ \decl -> case decl of
        DData name _typeVarNames variants -> do
            Just (TUserType _ _tvars tvariants) <- HashTable.lookup name typeEnv
            let Just qvarTable = HashMap.lookup name qvars
            forM_ (zip variants tvariants) $ \(v, tv) -> do
                forM_ (zip (vparameters v) (tvParameters tv)) $ \(typeName, typeVar) -> do
                    if isCapitalized typeName
                        then case HashMap.lookup typeName te of
                            Nothing -> error $ printf "Constructor uses nonexistent type %s" (show typeName)
                            Just t -> unify typeVar t
                        else case lookup typeName qvarTable of
                            Nothing -> error $ printf "Constructor uses nonexistent type variable %s" (show typeName)
                            Just t -> unify typeVar t
        _ -> return ()

    let computeVariantType ty qvarNames name argTypeNames = case argTypeNames of
            [] -> ty
            (x:xs) -> case HashMap.lookup x te of
                Just t ->
                    TFun t (computeVariantType ty qvarNames name xs)
                Nothing ->
                    case lookup x qvarNames of
                        Just t -> TFun t (computeVariantType ty qvarNames name xs)
                        Nothing -> error $ "Constructor " ++ (show name) ++ " variant uses undefined type " ++ (show x)

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
                ct <- showTypeVarIO ctorType
                print (vname, ct)
                HashTable.insert vname ctorType (eBindings env)
        _ -> return ()

    return env{eTypeBindings=te}

run :: [Declaration Pos] -> IO [Declaration TypeVar]
run decls = do
    env <- buildTypeEnvironment decls
    forM decls (checkDecl env)
