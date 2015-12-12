{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Crux.Typecheck where

import           Control.Exception     (ErrorCall (..))
import Control.Monad (void)
import           Crux.AST
import           Crux.Intrinsic        (Intrinsic (..))
import qualified Crux.Intrinsic        as Intrinsic
import qualified Crux.MutableHashTable as HashTable
import           Crux.Prelude
import           Crux.Text             (isCapitalized)
import           Crux.Tokens           (Pos (..))
import           Crux.Typecheck.Types
import           Crux.Typecheck.Unify
import qualified Data.HashMap.Strict   as HashMap
import qualified Data.Text             as Text
import           Prelude               hiding (String)
import           Text.Printf           (printf)

copyIORef :: IORef a -> IO (IORef a)
copyIORef ior = do
    v <- readIORef ior
    newIORef v

newEnv :: HashMap ModuleName LoadedModule -> Maybe TypeVar -> IO Env
newEnv eLoadedModules eReturnType = do
    eNextTypeIndex <- newIORef 0
    eBindings <- newIORef HashMap.empty
    eTypeBindings <- newIORef HashMap.empty
    let eTypeAliases = HashMap.empty
        eInLoop = False
    return Env {..}

childEnv :: Env -> IO Env
childEnv env = do
    bindings'    <- HashTable.clone (eBindings env)
    typeBindings <- HashTable.clone (eTypeBindings env)
    return env{eBindings=bindings', eTypeBindings=typeBindings}

typeFromConstructor :: Env -> Name -> IO (Maybe (TypeVar, TVariant TypeVar))
typeFromConstructor env cname = do
    let fold acc ty = case (acc, ty) of
            (Just a, _) -> return $ Just a
            (Nothing, ut) -> do
                ut' <- readIORef ut
                case ut' of
                    TUserType def _ -> do
                        let TUserTypeDef { tuVariants = variants } = def
                        case [v | v@(TVariant vname _) <- variants, vname == cname] of
                            [v] -> return $ Just (ut, v)
                            [] -> return Nothing
                            _ -> error "This should never happen: Type has multiple variants with the same constructor name"
                    _ ->
                        return Nothing
    typeBindings <- readIORef $ eTypeBindings env
    foldlM fold Nothing (fmap snd $ HashMap.elems typeBindings)

-- | Build up an environment for a case of a match block.
-- exprType is the type of the expression.  We unify this with the constructor of the pattern
buildPatternEnv :: TypeVar -> Env -> RefutablePattern -> IO ()
buildPatternEnv exprType env patt = case patt of
    RPIrrefutable PWildcard -> do
        return ()

    RPIrrefutable (PBinding pname) -> do
        HashTable.insert pname (ThisModule pname, LImmutable, exprType) (eBindings env)

    RPConstructor cname cargs -> do
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

walkMutableTypeVar :: TypeVar -> IO TypeVar
walkMutableTypeVar tyvar = do
    tyvar' <- readIORef tyvar
    case tyvar' of
        TBound tv -> do
            walkMutableTypeVar tv
        _ ->
            return tyvar

isLValue :: Env -> Expression ResolvedReference TypeVar -> IO Bool
isLValue env expr = case expr of
    EIdentifier _ name -> do
        l <- HashTable.lookup (resolvedReferenceName name) (eBindings env)
        return $ case l of
            Just (_, LMutable, _) -> True
            _ -> False
    ELookup _ lhs propName -> do
        lty <- walkMutableTypeVar (edata lhs)
        lty' <- readIORef lty
        case lty' of
            TRecord (RecordType recordEData rows) -> do
                case lookupTypeRow propName rows of
                    Just (RMutable, _) ->
                        return True
                    Just (RQuantified, _) -> do
                        return False
                    Just (RImmutable, _) -> do
                        return False
                    Just (RFree, rowTy) -> do
                        -- Update this record field to be a mutable field
                        let newRow = TypeRow{trName=propName, trMut=RMutable, trTyVar=rowTy}
                        let newFields = newRow:[tr | tr <- rows, trName tr /= propName]

                        writeIORef lty (TRecord $ RecordType recordEData newFields)

                        return True
                    Nothing -> do
                        -- This should  be impossible because type inference should have either failed, or
                        -- caused this record type to include the field by now.
                        error "Internal compiler error: calling isLValue on a nonexistent property"
            _ ->
                error "Internal compiler error: calling isLValue on a property lookup of a non-record type"
    _ -> return False

resolveType :: Pos -> Env -> UnresolvedReference -> IO TypeVar
resolveType pos env (UnknownReference name) = do
    res <- HashTable.lookup name (eTypeBindings env)
    case res of
        Just (_, t) -> return t
        Nothing -> do
            tb <- readIORef $ eTypeBindings env
            throwIO $ ErrorCall $ "FATAL: Environment does not contain a " ++ show name ++ " type at: " ++ show pos ++ " " ++ (show $ HashMap.keys tb)
resolveType _pos _env (KnownReference _moduleName _name) = do
    fail "TODO: resolveType implementation for KnownReference"

resolveName :: Env -> UnresolvedReference -> IO (ResolvedReference, TypeVar)
resolveName env ref@(UnknownReference name) = do
    result <- HashTable.lookup name (eBindings env)
    case result of
        Just (rr, _, t) -> return (rr, t)
        Nothing -> throwIO $ UnboundSymbol () ref
resolveName env (KnownReference moduleName name)
    | Just modul <- HashMap.lookup moduleName (eLoadedModules env)
    , Just func <- findExportedValue modul name = do
        funTy <- unfreezeTypeVar $ typeForFunDef func
        return (OtherModule moduleName name, funTy)
    | otherwise = do
        fail $ printf "No exported %s in module %s" (show name) (Text.unpack $ printModuleName moduleName)

findExportedValue :: Module a b -> Text -> Maybe (FunDef a b)
findExportedValue modul name = do
    let go decls = case decls of
            [] -> Nothing
            ((Declaration Export declType):rest)
                -- | DDeclare n _ <- declType, n == name ->
                --     return $ Just decl
                -- | DLet _ _ (PBinding n) _ _ <- declType, n == name ->
                --     return $ Just decl
                | DFun fd@(FunDef _ n _ _ _) <- declType, n == name ->
                    Just fd
                | otherwise ->
                    go rest
            ((Declaration _ _): rest) ->
                go rest

    go $ mDecls modul


withPositionInformation :: forall a. Expression UnresolvedReference Pos -> IO a -> IO a
withPositionInformation expr a = catch a handle
  where
    handle :: UnificationError () -> IO a
    handle (UnificationError () message at bt) =
        throwIO (UnificationError (edata expr) message at bt :: UnificationError Pos)
    handle (RecordMutabilityUnificationError () s m) =
        throwIO (RecordMutabilityUnificationError (edata expr) s m)
    handle (UnboundSymbol () message) =
        throwIO (UnboundSymbol (edata expr) message)
    handle (OccursCheckFailed ()) =
        throwIO (OccursCheckFailed (edata expr))
    handle (IntrinsicError () message) =
        throwIO (IntrinsicError (edata expr) message)
    handle (NotAnLVar () s) =
        throwIO (NotAnLVar (edata expr) s)
    handle (TdnrLhsTypeUnknown () s) =
        throwIO (TdnrLhsTypeUnknown (edata expr) s)

resolveArrayType :: Pos -> Env -> IO (TypeVar, TypeVar)
resolveArrayType pos env = do
    elementType <- freshType env
    arrayType <- resolveType pos env (UnknownReference "Array")
    readIORef arrayType >>= \case
        TUserType td [_elementType] -> do
            newArrayType <- newIORef $ TUserType td [elementType]
            return (newArrayType, elementType)
        _ -> fail "Unexpected Array type"

resolveBooleanType :: Pos -> Env -> IO TypeVar
resolveBooleanType pos env = do
    resolveType pos env (UnknownReference "Boolean")

-- TODO: rename to checkNew or some other function that conveys "typecheck, but
-- I don't know or care what type you will be." and port all uses of check to it.
check :: Env -> Expression UnresolvedReference Pos -> IO (Expression ResolvedReference TypeVar)
check env expr = do
    newType <- freshType env
    checkExpecting newType env expr

checkExpecting :: TypeVar -> Env -> Expression UnresolvedReference Pos -> IO (Expression ResolvedReference TypeVar)
checkExpecting expectedType env expr = do
    e <- check' expectedType env expr
    unify (edata e) expectedType
    return e

check' :: TypeVar -> Env -> Expression UnresolvedReference Pos -> IO (Expression ResolvedReference TypeVar)
check' expectedType env expr = withPositionInformation expr $ case expr of
    EFun _ params retAnn body -> do
        bindings' <- HashTable.clone (eBindings env)

        -- If we know the expected function type, then use its type variables
        -- rather than make new ones.
        -- TODO: use followTypeVar instead of readIORef
        (paramTypes, returnType) <- readIORef expectedType >>= \case
            TFun paramTypes returnType -> do
                return (paramTypes, returnType)
            _ -> do
                paramTypes <- forM params $ \_ -> do
                    freshType env
                returnType <- freshType env
                return (paramTypes, returnType)

        forM_ (zip params paramTypes) $ \((p, _), pt) -> do
            HashTable.insert p (Local p, LImmutable, pt) bindings'

        let env' = env
                { eBindings=bindings'
                , eReturnType=Just returnType
                , eInLoop=False
                }
        body' <- check env' body

        forM_ (zip params paramTypes) $ \((_, pAnn), pt) -> do
            forM_ pAnn $ \ann -> do
                annTy <- resolveTypeIdent env NewTypesAreQuantified ann
                unify pt annTy

        forM_ retAnn $ \ann -> do
            annTy <- resolveTypeIdent env NewTypesAreQuantified ann
            unify returnType annTy

        unify returnType $ edata body'
        ty <- newIORef $ TFun paramTypes returnType
        return $ EFun ty params retAnn body'

    EApp _ (EIdentifier _ (UnknownReference "_unsafe_js")) [ELiteral _ (LString txt)] -> do
        t <- freshType env
        return $ EIntrinsic t (IUnsafeJs txt)
    EApp _ (EIdentifier _ (UnknownReference "_unsafe_js")) _ ->
        error "_unsafe_js takes just one string literal"

    EApp _ (EIdentifier _ (UnknownReference "_unsafe_coerce")) [subExpr] -> do
        t <- freshType env
        subExpr' <- check env subExpr
        return $ EIntrinsic t (IUnsafeCoerce subExpr')
    EApp _ (EIdentifier _ (UnknownReference "_unsafe_coerce")) _ ->
        error "_unsafe_coerce takes just one argument"

    EApp _ fn args -> do
        fn' <- check env fn
        -- TODO: use followTypeVar instead of readIORef
        readIORef (edata fn') >>= \case
            -- in the case that the type of the function is known, we propogate
            -- the known argument types into the environment so tdnr works
            TFun argTypes resultType -> do
                args' <- forM (zip argTypes args) $ \(argType, arg) -> do
                    checkExpecting argType env arg
                return $ EApp resultType fn' args'
            _ -> do
                args' <- mapM (check env) args
                result <- freshType env
                ty <- newIORef $ TFun (map edata args') result
                unify (edata fn') ty
                return $ EApp result fn' args'

    EIntrinsic {} -> do
        error "Unexpected: EIntrinsic encountered during typechecking"

    ELookup _ lhs propName -> do
        lhs' <- check env lhs
        ty <- freshType env
        recTy <- newIORef $ TRecord $ RecordType RecordFree [TypeRow{trName=propName, trMut=RFree, trTyVar=ty}]
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

    ELet _ mut pat maybeAnnot expr' -> do
        ty <- freshType env
        expr'' <- check env expr'
        case pat of
            PWildcard -> do
                return ()
            PBinding name -> do
                HashTable.insert name (Local name, mut, ty) (eBindings env)
        unify ty (edata expr'')
        forM_ maybeAnnot $ \annotation -> do
            annotTy <- resolveTypeIdent env NewTypesAreQuantified annotation
            unify ty annotTy

        unitTy <- newIORef $ TPrimitive Unit
        return $ ELet unitTy mut pat maybeAnnot expr''

    EAssign _ lhs rhs -> do
        lhs' <- check env lhs
        rhs' <- check env rhs

        unify (edata lhs') (edata rhs')

        islvalue <- isLValue env lhs'
        when (not islvalue) $ do
            lhs'' <- freeze lhs'
            throwIO $ NotAnLVar () (show lhs'')

        unitType <- newIORef $ TPrimitive Unit

        return $ EAssign unitType lhs' rhs'

    ELiteral _ lit -> do
        litType <- newIORef $ case lit of
                LInteger _ -> TPrimitive Number
                LString _ -> TPrimitive String
                LUnit -> TPrimitive Unit
        return $ ELiteral litType lit

    EArrayLiteral _ elements -> do
        (arrayType, elementType) <- resolveArrayType (edata expr) env
        elements' <- forM elements $ \element -> do
            elementExpr <- check env element
            unify elementType (edata elementExpr)
            return elementExpr
        return $ EArrayLiteral arrayType elements'

    ERecordLiteral _ fields -> do
        fields' <- forM (HashMap.toList fields) $ \(name, fieldExpr) -> do
            ty <- freshType env
            fieldExpr' <- check env fieldExpr
            unify ty (edata fieldExpr')
            return (name, fieldExpr')

        let fieldTypes = map (\(name, ex) -> TypeRow{trName=name, trMut=RFree, trTyVar=edata ex}) fields'

        recordTy <- newIORef $ TRecord $ RecordType RecordClose fieldTypes
        return $ ERecordLiteral recordTy (HashMap.fromList fields')

    EIdentifier _ (UnknownReference "_unsafe_js") ->
        throwIO $ IntrinsicError () "Intrinsic _unsafe_js is not a value"
    EIdentifier _ (UnknownReference "_unsafe_coerce") ->
        error "Intrinsic _unsafe_coerce is not a value"
    EIdentifier _pos txt -> do
        (rr, tyref) <- resolveName env txt
        tyref' <- instantiate env tyref
        return $ EIdentifier tyref' rr
    ESemi _ lhs rhs -> do
        lhs' <- check env lhs
        rhs' <- check env rhs
        return $ ESemi (edata rhs') lhs' rhs'

    EMethodApp pos lhs methodName args -> do
        -- lhs must be typechecked so that, if it has a concrete type, we know
        -- the location of that type.
        lhs' <- check env lhs
        lhsType <- walkMutableTypeVar (edata lhs')
        -- TODO: use followTypeVar instead of readIORef
        moduleName <- readIORef lhsType >>= \case
            TUserType TUserTypeDef{..} _ -> do
                return tuModuleName
            TPrimitive _ -> do
                return "Prelude"
            _ -> do
                ts <- showTypeVarIO lhsType
                throwIO $ TdnrLhsTypeUnknown () ts

        check env $ EApp
            pos
            (EIdentifier pos $ KnownReference moduleName methodName)
            (lhs : args)

    -- TEMP: For now, intrinsics are too polymorphic.
    -- Arithmetic operators like + and - have type (a, a) -> a
    -- Relational operators like <= and != have type (a, a) -> Bool
    EBinIntrinsic _ bi lhs rhs -> do
        lhs' <- check env lhs
        rhs' <- check env rhs

        if | isArithmeticOp bi -> do
                unify (edata lhs') (edata rhs')
                return $ EBinIntrinsic (edata lhs') bi lhs' rhs'
           | isRelationalOp bi -> do
                unify (edata lhs') (edata rhs')
                booleanType <- resolveBooleanType (edata expr) env
                return $ EBinIntrinsic booleanType bi lhs' rhs'
           | isBooleanOp bi -> do
                booleanType <- resolveBooleanType (edata lhs) env
                unify (edata lhs') booleanType
                unify (edata rhs') booleanType
                return $ EBinIntrinsic booleanType bi lhs' rhs'
           | otherwise ->
                error "This should be impossible: Check EBinIntrinsic"

    EIfThenElse _ condition ifTrue ifFalse -> do
        booleanType <- resolveBooleanType (edata expr) env

        condition' <- check env condition
        unify booleanType (edata condition')
        ifTrue' <- check env ifTrue
        ifFalse' <- check env ifFalse

        unify (edata ifTrue') (edata ifFalse')

        return $ EIfThenElse (edata ifTrue') condition' ifTrue' ifFalse'

    EWhile _ cond body -> do
        booleanType <- resolveBooleanType (edata expr) env
        unitType <- newIORef $ TPrimitive Unit

        condition' <- check env cond
        unify booleanType (edata condition')

        let env' = env { eInLoop = True }
        body' <- check env' body
        unify unitType (edata body')

        return $ EWhile unitType condition' body'

    EFor pos name over body -> do
        unitType <- newIORef $ TPrimitive Unit

        (arrayType, iteratorType) <- resolveArrayType pos env
        over' <- checkExpecting arrayType env over

        bindings' <- HashTable.clone (eBindings env)
        HashTable.insert name (Local name, LImmutable, iteratorType) bindings'

        let env' = env { eBindings = bindings', eInLoop = True }
        body' <- check env' body
        unify unitType (edata body')

        return $ EFor unitType name over' body'

    EReturn _ rv -> do
        rv' <- check env rv
        case eReturnType env of
            Nothing ->
                error "Cannot return outside of functions"
            Just rt -> do
                unify rt $ edata rv'
                retTy <- freshType env
                return $ EReturn retTy rv'

    EBreak _ -> do
        when (not $ eInLoop env) $
            error "Cannot use 'break' outside of a loop"
        t <- freshType env
        return $ EBreak t

freezeTypeDef :: TUserTypeDef TypeVar -> IO (TUserTypeDef ImmutableTypeVar)
freezeTypeDef TUserTypeDef{..} = do
    parameters' <- mapM freezeTypeVar tuParameters
    -- Huge hack: don't try to freeze variants because they can be recursive
    let variants' = []
    let td = TUserTypeDef
            {tuName, tuModuleName, tuParameters=parameters', tuVariants=variants'}
    return td

freezeTypeVar :: TypeVar -> IO ImmutableTypeVar
freezeTypeVar tv = do
    tv' <- readIORef tv
    case tv' of
        TUnbound i -> do
            return $ IUnbound i
        TBound tv'' -> do
            freezeTypeVar tv''
        TQuant i ->
            return $ IQuant i
        TFun arg body -> do
            arg' <- mapM freezeTypeVar arg
            body' <- freezeTypeVar body
            return $ IFun arg' body'
        TUserType def tvars -> do
            tvars' <- mapM freezeTypeVar tvars
            def' <- freezeTypeDef def
            return $ IUserType def' tvars'
        TRecord rt -> do
            rt' <- traverse freezeTypeVar rt
            return $ IRecord rt'
        TPrimitive t ->
            return $ IPrimitive t

unfreezeTypeDef :: TUserTypeDef ImmutableTypeVar -> IO (TUserTypeDef TypeVar)
unfreezeTypeDef TUserTypeDef{..} = do
    parameters' <- mapM unfreezeTypeVar tuParameters
    -- Huge hack: don't try to freeze variants because they can be recursive
    let variants' = []
    let td = TUserTypeDef {tuName, tuModuleName, tuParameters=parameters', tuVariants=variants'}
    return td

unfreezeTypeVar :: ImmutableTypeVar -> IO TypeVar
unfreezeTypeVar imt = newIORef =<< case imt of
    IUnbound name ->
        return $ TUnbound name
    IQuant varname -> do
        return $ TQuant varname
    IFun params body -> do
        params' <- mapM unfreezeTypeVar params
        body' <- unfreezeTypeVar body
        return $ TFun params' body'
    IUserType td params -> do
        td' <- unfreezeTypeDef td
        params' <- mapM unfreezeTypeVar params
        return $ TUserType td' params'
    IRecord rt -> do
        rt' <- traverse unfreezeTypeVar rt
        return $ TRecord rt'
    IPrimitive pt -> do
        return $ TPrimitive pt

freezeIntrinsic :: IntrinsicId i TypeVar -> IO (IntrinsicId i ImmutableTypeVar)
freezeIntrinsic = traverse freeze

freeze :: Expression i TypeVar -> IO (Expression i ImmutableTypeVar)
freeze = traverse freezeTypeVar

freezeDecl :: Declaration i TypeVar -> IO (Declaration i ImmutableTypeVar)
freezeDecl (Declaration export decl) = fmap (Declaration export) $ traverse freezeTypeVar decl

freezeModule :: Module a TypeVar -> IO (Module a ImmutableTypeVar)
freezeModule Module{..} = do
    decls <- mapM freezeDecl mDecls
    return $ Module
        { mImports=mImports
        , mDecls=decls
        }

typeForFunDef :: FunDef a b -> b
typeForFunDef (FunDef ty _ _ _ _) = ty


checkDecl :: Env -> Declaration UnresolvedReference Pos -> IO (Declaration ResolvedReference TypeVar)
checkDecl env (Declaration export decl) = fmap (Declaration export) $ case decl of
    DDeclare name typeIdent -> do
        return $ DDeclare name typeIdent
    DData name moduleName typeVars variants -> do
        -- TODO: Verify that all types referred to by variants exist, or are typeVars
        return $ DData name moduleName typeVars variants
    DJSData name moduleName variants -> do
        return $ DJSData name moduleName variants
    DType (TypeAlias name typeVars ident) -> do
        return $ DType $ TypeAlias name typeVars ident
    DFun (FunDef pos name args returnAnn body) ->
        let expr = EFun pos args returnAnn body
        in withPositionInformation expr $ do
            ty <- freshType env
            HashTable.insert name (ThisModule name, LImmutable, ty) (eBindings env)
            expr'@(EFun _ _ _ body') <- check env expr
            unify (edata expr') ty
            quantify ty
            return $ DFun $ FunDef (edata expr') name args returnAnn body'
    DLet pos mut pat maybeAnnot expr ->
        let fakeExpr = ELet pos mut pat maybeAnnot expr
        in withPositionInformation fakeExpr $ do
            env' <- childEnv env
            ty <- freshType env'
            forM_ maybeAnnot $ \annotation -> do
                annotTy <- resolveTypeIdent env' NewTypesAreQuantified annotation
                unify ty annotTy

            expr' <- check env' expr
            unify ty (edata expr')

            case pat of
                PWildcard -> do
                    return ()
                PBinding name -> do
                    HashTable.insert name (ThisModule name, mut, ty) (eBindings env)
            quantify ty
            return $ DLet (edata expr') mut pat maybeAnnot expr'

exportedDecls :: [Declaration a b] -> [DeclarationType a b]
exportedDecls decls = [dt | (Declaration Export dt) <- decls]

buildTypeEnvironment :: (Show j, Show a) => HashMap ModuleName LoadedModule -> Module j a -> IO Env
buildTypeEnvironment loadedModules modul = do
    -- built-in types. would be nice to move into the prelude somehow.
    numTy  <- newIORef $ TPrimitive Number
    unitTy <- newIORef $ TPrimitive Unit
    strTy  <- newIORef $ TPrimitive String

    e <- newEnv loadedModules Nothing
    typeEnv <- newIORef $ HashMap.fromList
        [ ("Number", (Builtin "Number", numTy))
        , ("Unit", (Builtin "Unit", unitTy))
        , ("String", (Builtin "String", strTy))
        ]

    qvarsRef <- HashTable.new :: IO (IORef (HashMap Name [(TypeVariable, TypeVar)]))

    let insertDataType scope name moduleName typeVarNames variants = do
            typeVars <- forM typeVarNames $ const $ do
                ft <- freshType e
                quantify ft
                return ft
            HashTable.insert name (zip typeVarNames typeVars) qvarsRef

            variants' <- forM variants $ \Variant{..} -> do
                tvParameters <- forM vparameters $ const $ freshType e
                let tvName = vname
                return TVariant{..}

            let typeDef = TUserTypeDef
                    { tuName = name
                    , tuModuleName = moduleName
                    , tuParameters = typeVars
                    , tuVariants = variants'
                    }
            userType <- newIORef $ TUserType typeDef typeVars
            HashTable.insert name (scope name, userType) typeEnv
            return typeDef

    -- inject stuff from the prelude into this global environment
    -- TODO: rather than injecting symbols, we may need a mechanism to refer
    -- to imported symbols
    forM_ (mImports modul) $ \(UnqualifiedImport importName) -> do
        importedModule <- case HashMap.lookup importName loadedModules of
            Just im -> return im
            Nothing -> fail $ "dependent module not loaded: " <> (Text.unpack $ printModuleName importName)

        forM_ (exportedDecls $ mDecls importedModule) $ \decl -> case decl of
            DDeclare _ _ -> do
                fail "TODO: export declare"

            DLet tr _mutability pat _ _ -> do
                tr' <- unfreezeTypeVar tr
                case pat of
                    PWildcard -> return ()
                    PBinding name -> do
                        HashTable.insert name (OtherModule importName name, LImmutable, tr') (eBindings e)

            DFun (FunDef tr name _params _retAnn _body) -> do
                tr' <- unfreezeTypeVar tr
                HashTable.insert name (OtherModule importName name, LImmutable, tr') (eBindings e)

            DData dname _ typeVariables variants -> do
                -- IS THIS RIGHT??
                -- no tests

                env <- childEnv e

                tyVars <- forM typeVariables $ \tv ->
                    resolveTypeIdent env NewTypesAreQuantified (TypeIdent tv [])

                typeDef <- insertDataType (OtherModule importName) dname importName typeVariables variants

                dataType <- newIORef $ TUserType typeDef tyVars

                forM_ variants $ \(Variant vname params) -> do
                    vt <- case params of
                        [] ->
                            return dataType
                        _ -> do
                            paramTys <- forM params $ \param ->
                                resolveTypeIdent env NewTypesAreErrors param
                            newIORef $ TFun paramTys dataType

                    HashTable.insert vname (OtherModule importName vname, LImmutable, vt) (eBindings e)

            DJSData name moduleName variants -> do
                let encodeJSVariant (JSVariant n _) = TVariant n []
                tr <- newIORef $ TUserType (TUserTypeDef name moduleName [] $ map encodeJSVariant variants) []
                HashTable.insert name (OtherModule importName name, tr) typeEnv

                forM_ variants $ \(JSVariant vname _) -> do
                    HashTable.insert vname (OtherModule importName vname, LImmutable, tr) (eBindings e)

            DType (TypeAlias _name _params _ident) -> do
                fail "TODO: export type alias"

    typeAliasesRef <- HashTable.new

    -- First, populate the type environment.  Variant parameter types are all initially free.
    forM_ (mDecls modul) $ \(Declaration _ decl) -> case decl of
        DDeclare _name _typeIdent -> do
            return ()

        DData name moduleName typeVarNames variants ->
            void $ insertDataType ThisModule name moduleName typeVarNames variants

        DJSData name moduleName variants -> do
            -- jsffi data never has type parameters, so we can just blast through the whole thing in one pass
            variants' <- forM variants $ \(JSVariant variantName _value) -> do
                let tvParameters = []
                let tvName = variantName
                return TVariant{..}

            let typeDef = TUserTypeDef
                    { tuName = name
                    , tuModuleName = moduleName
                    , tuParameters = []
                    , tuVariants = variants'
                    }
            userType <- newIORef $ TUserType typeDef []
            modifyIORef' typeEnv $ HashMap.insert name (ThisModule name, userType)

        DType ty@(TypeAlias name _ _) -> do
            HashTable.insert name ty typeAliasesRef

        DFun _ -> do
            return ()

        DLet _ _ _ _ _ -> do
            return ()

    qvars <- readIORef qvarsRef
    te <- readIORef typeEnv
    ta <- readIORef typeAliasesRef
    let env = e{eTypeBindings=typeEnv, eTypeAliases=ta}

    -- Second, unify parameter types
    forM_ (mDecls modul) $ \(Declaration _ decl) -> case decl of
        DData name _ _typeVarNames variants -> do
            Just (_, ty) <- HashTable.lookup name typeEnv
            TUserType typeDef _ <- readIORef ty

            let TUserTypeDef { tuVariants = tvariants } = typeDef
            forM_ (zip variants tvariants) $ \(v, tv) -> do
                forM_ (zip (vparameters v) (tvParameters tv)) $ \(typeIdent, typeVar) -> do
                    let Just qv = HashMap.lookup name qvars
                    env' <- addQvarTable env qv
                    t <- resolveTypeIdent env' NewTypesAreErrors typeIdent
                    unify typeVar t
        _ -> return ()

    let computeVariantType ty qvarNames _name argTypeIdents = case argTypeIdents of
            [] ->
                return ty
            _ -> do
                env' <- addQvarTable env qvarNames
                resolvedArgTypes <- mapM (resolveTypeIdent env' NewTypesAreErrors) argTypeIdents
                newIORef $ TFun resolvedArgTypes ty

    intrinsics <- Intrinsic.intrinsics

    forM_ (HashMap.toList intrinsics) $ \(name, intrin) -> do
        let Intrinsic{..} = intrin
        HashTable.insert name (Builtin name, LImmutable, iType) (eBindings env)

    -- Note to self: Here we need to match the names of the types of each variant up with concrete types, but also
    -- with the TypeVars created in the type environment.
    forM_ (mDecls modul) $ \(Declaration _ decl) -> case decl of
        DDeclare name typeIdent -> do
            t <- resolveTypeIdent env NewTypesAreQuantified typeIdent
            HashTable.insert name (ThisModule name, LImmutable, t) (eBindings env)
        DData name _ _ variants -> do
            let Just (_, userType) = HashMap.lookup name te
            let Just qvarTable = HashMap.lookup name qvars
            forM_ variants $ \(Variant vname vdata) -> do
                ctorType <- computeVariantType userType qvarTable vname vdata
                HashTable.insert vname (ThisModule vname, LImmutable, ctorType) (eBindings env)
        DJSData name _ variants -> do
            let Just (_, userType) = HashMap.lookup name te
            forM_ variants $ \(JSVariant variantName _value) -> do
                HashTable.insert variantName (ThisModule variantName, LImmutable, userType) (eBindings env)
        _ -> return ()

    return env

addQvarTable :: Env -> [(Name, TypeVar)] -> IO Env
addQvarTable env@Env{..} qvarTable = do
    newBindings <- HashTable.mergeImmutable eTypeBindings
        (HashMap.fromList [(name, (ThisModule name, ty)) | (name, ty) <- qvarTable])
    return env { eTypeBindings = newBindings }

data ResolvePolicy = NewTypesAreErrors | NewTypesAreQuantified
    deriving (Eq)

resolveTypeIdent :: Env -> ResolvePolicy -> TypeIdent -> IO TypeVar
resolveTypeIdent env@Env{..} resolvePolicy typeIdent =
    go typeIdent
  where
    go (TypeIdent typeName typeParameters) = do
        res <- HashTable.lookup typeName eTypeBindings
        case res of
            Just (_, ty) -> do
                -- TODO: use followTypeVar instead of readIORef
                ty' <- readIORef ty
                case ty' of
                    TPrimitive {}
                        | [] == typeParameters ->
                            return ty
                        | otherwise ->
                            error "Primitive types don't take type parameters"
                    TUserType def@TUserTypeDef{tuParameters} _
                        | length tuParameters == length typeParameters -> do
                            params <- mapM go typeParameters
                            newIORef $ TUserType def params
                        | otherwise ->
                            error $ printf "Type %s takes %i type parameters.  %i given" (show $ tuName def) (length tuParameters) (length typeParameters)
                    _ ->
                        return ty
            Nothing -> case HashMap.lookup typeName eTypeAliases of
                Just (TypeAlias aliasName aliasParams aliasedIdent) -> do
                    when (length aliasParams /= length typeParameters) $
                        error $ printf "Type alias %s takes %i parameters.  %i given" (Text.unpack aliasName) (length aliasParams) (length typeParameters)

                    argTypes <- mapM (resolveTypeIdent env resolvePolicy) typeParameters

                    env' <- addQvarTable env (zip aliasParams argTypes)

                    resolveTypeIdent env' resolvePolicy aliasedIdent
                Nothing | NewTypesAreQuantified == resolvePolicy && not (isCapitalized typeName) -> do
                    tyVar <- freshType env
                    quantify tyVar

                    HashTable.insert typeName (ThisModule typeName, tyVar) eTypeBindings
                    return tyVar
                Nothing ->
                    error $ printf "Constructor refers to nonexistent type %s" (show typeName)

    go (RecordIdent rows) = do
        rows' <- forM rows $ \(trName, mut, rowTypeIdent) -> do
            let trMut = case mut of
                    Nothing -> RFree
                    Just LMutable -> RMutable
                    Just LImmutable -> RImmutable
            trTyVar <- go rowTypeIdent
            return TypeRow{..}
        newIORef $ TRecord $ RecordType RecordClose rows'

    go (FunctionIdent argTypes retPrimitive) = do
        argTypes' <- mapM go argTypes
        retPrimitive' <- go retPrimitive
        newIORef $ TFun argTypes' retPrimitive'

run :: HashMap ModuleName LoadedModule -> Module UnresolvedReference Pos -> IO LoadedModule
run loadedModules modul = do
        env <- buildTypeEnvironment loadedModules modul
        decls <- forM (mDecls modul) (checkDecl env)
        freezeModule modul
            { mDecls=decls
            }

throwTypeError :: UnificationError Pos -> IO b
throwTypeError (UnificationError pos message at bt) = do
    as <- showTypeVarIO at
    bs <- showTypeVarIO bt
    let m
            | null message = ""
            | otherwise = "\n" ++ message
    error $ printf "Unification error at %i,%i\n\t%s\n\t%s%s" (posLine pos) (posCol pos) as bs m
throwTypeError (RecordMutabilityUnificationError pos key message) =
    error $ printf "Unification error at %i,%i: Could not unify mutability of record field %s: %s" (posLine pos) (posCol pos) (show key) message
throwTypeError (UnboundSymbol pos message) =
    error $ printf "Unbound symbol at %i,%i\n\t%s" (posLine pos) (posCol pos) (show message)
throwTypeError (OccursCheckFailed pos) =
    error $ printf "Occurs check failed at %i,%i" (posLine pos) (posCol pos)
throwTypeError (IntrinsicError pos message) =
    error $ printf "%s at %i,%i" message (posLine pos) (posCol pos)
throwTypeError (NotAnLVar pos s) = do
    error $ printf "Not an LVar at %i,%i\n\t%s" (posLine pos) (posCol pos) s
throwTypeError (TdnrLhsTypeUnknown pos s) = do
    error $ printf "TDNR %i,%i\n\t%s" (posLine pos) (posCol pos) s
