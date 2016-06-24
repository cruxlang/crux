{-# LANGUAGE ScopedTypeVariables, TupleSections #-}

module Crux.Typecheck
    ( run
    ) where

import Crux.AST
import Crux.Error
import Crux.Module.Types
import qualified Crux.SymbolTable as SymbolTable
import Crux.Prelude
import Crux.Typecheck.Env
import Crux.Typecheck.Monad
import Crux.Typecheck.Types
import Crux.Typecheck.Unify
import Crux.TypeVar
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import Prelude hiding (String)
import Text.Printf (printf)

-- | Build up an environment for a case of a match block.
-- exprType is the type of the expression.  We unify this with the constructor of the pattern
-- TODO: wipe this out and replace it with ePatternBindings in Env
buildPatternEnv :: Env -> Pos -> TypeVar -> Mutability -> Pattern () -> TC (Pattern PatternTag)
buildPatternEnv env pos exprType mut = \case
    PWildcard -> do
        return PWildcard

    PBinding pname -> do
        SymbolTable.insert (eValueBindings env) pos SymbolTable.DisallowDuplicates pname (ValueReference (Local, pname) mut exprType)
        return $ PBinding pname

    PConstructor unresolvedReference () cargs -> do
        ref <- resolvePatternReference env pos unresolvedReference
        let cname = getUnresolvedReferenceLeaf unresolvedReference

        let (PatternReference typeDef patternTag) = ref
        def' <- instantiateDataTypeDef env typeDef

        let [thisVariantParameters] = [tvParameters | TVariant{..} <- tuVariants def', tvName == cname]
        unify pos exprType $ TDataType def'

        when (length thisVariantParameters /= length cargs) $
            fail $ printf "Pattern %s should specify %i args but got %i" (Text.unpack cname) (length thisVariantParameters) (length cargs)

        args' <- for (zip cargs thisVariantParameters) $ \(arg, vp) -> do
            buildPatternEnv env pos vp mut arg

        return $ PConstructor unresolvedReference patternTag args'

lookupBinding :: MonadIO m => Name -> Env -> m (Maybe (ResolvedReference, Mutability, TypeVar))
lookupBinding name Env{..} = do
    SymbolTable.lookup eValueBindings name >>= \case
        Just (ValueReference a b c) -> return $ Just (a, b, c)
        _ -> return $ Nothing

isLValue :: MonadIO m => Env -> Expression ResolvedReference tagtype TypeVar -> m Bool
isLValue env expr = case expr of
    EIdentifier _ name -> do
        l <- lookupBinding (resolvedReferenceName name) env
        return $ case l of
            Just (_, Mutable, _) -> True
            _ -> False
    ELookup _ lhs propName -> do
        lty' <- followTypeVar (edata lhs)
        case lty' of
            TRecord ref' -> followRecordTypeVar' ref' >>= \(ref, RecordType recordEData rows) -> do
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

                        -- TODO: just unify this thing with a record with a mutable field
                        writeIORef ref $ RRecord $ RecordType recordEData newFields

                        return True
                    Nothing -> do
                        -- This should  be impossible because type inference should have either failed, or
                        -- caused this record type to include the field by now.
                        ltys <- showTypeVarIO lty'
                        error $ printf "Internal compiler error: calling isLValue on a nonexistent property %s of record %s" (show propName) ltys
            _ ->
                error "Internal compiler error: calling isLValue on a property lookup of a non-record type"
    _ -> return False

-- TODO: rename to checkNew or some other function that conveys "typecheck, but
-- I don't know or care what type you will be." and port all uses of check to it.
check :: Env -> Expression UnresolvedReference () Pos -> TC (Expression ResolvedReference PatternTag TypeVar)
check env expr = do
    newType <- freshType env
    checkExpecting newType env expr

checkExpecting :: TypeVar -> Env -> Expression UnresolvedReference () Pos -> TC (Expression ResolvedReference PatternTag TypeVar)
checkExpecting expectedType env expr = do
    e <- check' expectedType env expr
    unify (edata expr) (edata e) expectedType
    return e

-- We could have this return a poison type.
resumableTypeError :: Pos -> TypeError -> TC a
resumableTypeError pos = failError . TypeError pos

weaken :: MonadIO m => TypeLevel -> Expression idtype tagtype TypeVar -> m (Expression idtype tagtype TypeVar)
weaken level e = do
    t' <- weaken' (edata e)
    return $ setEdata e t'
  where
    weaken' t = case t of
        TypeVar ref ->
            readIORef ref >>= \case
                TUnbound Strong lvl tn
                    | level <= lvl -> do
                        writeIORef ref $ TUnbound Weak lvl tn
                        return t
                TUnbound {} ->
                    return t

                TBound tv -> do
                    tv' <- weaken' tv
                    writeIORef ref $ TBound tv'
                    return t
        TQuant {} ->
            return t
        TFun args ret -> do
            args' <- for args weaken'
            ret' <- weaken' ret
            return $ TFun args' ret'
        TDataType typeDef -> do
            tyvars' <- for (tuParameters typeDef) weaken'
            return $ TDataType typeDef{ tuParameters=tyvars' }
        TRecord rtv -> do
            weakenRecord rtv
            return t
        TTypeFun a b -> do
            b' <- weaken' b
            return $ TTypeFun a b'

    weakenRecord rtv = readIORef rtv >>= \case
        RRecord (RecordType open rows) -> do
            rows' <- for rows $ \tr@(TypeRow{..}) -> do
                ty' <- weaken' trTyVar
                return tr { trTyVar = ty' }
            writeIORef rtv $ RRecord $ RecordType open rows'

        RBound rtv' ->
            weakenRecord rtv'

check' :: TypeVar -> Env -> Expression UnresolvedReference () Pos -> TC (Expression ResolvedReference PatternTag TypeVar)
check' expectedType env = \case
    EFun pos FunctionDecl{..} -> do --  params retAnn body
        valueBindings' <- SymbolTable.clone (eValueBindings env)

        -- If we know the expected function type, then use its type variables
        -- rather than make new ones.
        (paramTypes, returnType) <- followTypeVar expectedType >>= \case
            TFun paramTypes returnType -> do
                return (paramTypes, returnType)
            _ -> do
                paramTypes <- for fdParams $ \_ -> do
                    freshType env
                returnType <- freshType env
                return (paramTypes, returnType)

        env' <- childEnv env >>= \x -> return x
            { eValueBindings=valueBindings'
            , eReturnType=Just returnType
            , eInLoop=False
            }

        for_ fdForall $ \name -> do
            tv <- freshQuantized env'
            SymbolTable.insert (eTypeBindings env') pos SymbolTable.DisallowDuplicates name (TypeReference tv)

        params' <- for (zip fdParams paramTypes) $ \((p, pAnn), pt) -> do
            for_ pAnn $ \ann -> do
                annTy <- resolveTypeIdent env' pos NewTypesAreErrors ann
                unify pos pt annTy
            -- TODO: exhaustiveness check on this pattern
            param' <- buildPatternEnv env' pos pt Immutable p
            return (param', pAnn)

        for_ fdReturnAnnot $ \ann -> do
            annTy <- resolveTypeIdent env' pos NewTypesAreErrors ann
            -- annTy <- resolveTypeIdent env pos NewTypesAreQuantified ann
            unify pos returnType annTy

        body' <- check env' fdBody
        unify pos returnType $ edata body'

        let ty = TFun paramTypes returnType
        return $ EFun ty FunctionDecl
            { fdParams=params'
            , fdReturnAnnot
            , fdForall
            , fdBody=body'
            }

    -- Compiler intrinsics
    EApp _ (EIdentifier _ (UnqualifiedReference "_debug_type")) [arg] -> do
        arg' <- check env arg
        argType <- renderTypeVarIO $ edata arg'
        liftIO $ putStrLn $ "Debug Type: " ++ argType
        return arg'
    EApp _ (EIdentifier _ (UnqualifiedReference "_unsafe_js")) [ELiteral _ (LString txt)] -> do
        t <- freshType env
        return $ EIntrinsic t (IUnsafeJs txt)
    EApp _ (EIdentifier _ (UnqualifiedReference "_unsafe_js")) _ ->
        error "_unsafe_js takes just one string literal"

    EApp _ (EIdentifier _ (UnqualifiedReference "_unsafe_coerce")) [subExpr] -> do
        t <- freshType env
        subExpr' <- check env subExpr
        return $ EIntrinsic t (IUnsafeCoerce subExpr')
    EApp _ (EIdentifier _ (UnqualifiedReference "_unsafe_coerce")) _ ->
        error "_unsafe_coerce takes just one argument"

    EApp pos fn args -> do
        fn' <- check env fn
        followTypeVar (edata fn') >>= \case
            -- in the case that the type of the function is known, we propagate
            -- the known argument types into the environment so tdnr works
            TFun argTypes resultType | length argTypes == length args -> do
                args' <- for (zip argTypes args) $ \(argType, arg) -> do
                    checkExpecting argType env arg

                let appTy = TFun (map edata args') resultType
                unify pos (edata fn') appTy

                return $ EApp resultType fn' args'
            _ -> do
                args' <- for args $ check env
                result <- freshType env
                let ty = TFun (map edata args') result
                unify pos (edata fn') ty
                return $ EApp result fn' args'

    EIntrinsic {} -> do
        error "Unexpected: EIntrinsic encountered during typechecking"

    ELookup pos lhs propName -> do
        let valueLookup = do
                -- if lhs is ident and in bindings, then go go go
                -- else turn into QualifiedReference and go go go
                lhs' <- check env lhs
                ty <- freshType env
                row <- freshRowVariable env
                rec <- newIORef $ RRecord $ RecordType (RecordFree row) [TypeRow{trName=propName, trMut=RFree, trTyVar=ty}]
                unify pos (edata lhs') $ TRecord rec
                return $ ELookup ty lhs' propName
        case lhs of
            EIdentifier pos' (UnqualifiedReference name) -> do
                SymbolTable.lookup (eValueBindings env) name >>= \case
                    Just (ModuleReference mn) -> do
                        check env $ EIdentifier pos' $ KnownReference mn propName
                    _ -> valueLookup
            _ -> valueLookup

    EMatch pos matchExpr cases -> do
        resultType <- freshType env

        matchExpr' <- check env matchExpr

        cases' <- for cases $ \(Case patt caseExpr) -> do
            env' <- childEnv env
            patt' <- buildPatternEnv env' pos (edata matchExpr') Immutable patt
            caseExpr' <- check env' caseExpr
            unify pos resultType (edata caseExpr')
            return $ Case patt' caseExpr'

        return $ EMatch resultType matchExpr' cases'

    ELet pos mut pat maybeAnnot expr' -> do
        ty <- freshType env
        env' <- childEnv env
        expr'' <- check env' expr'
        expr''' <- case mut of
            Mutable -> weaken (eLevel env') expr''
            Immutable -> return expr''
        -- TODO: exhaustiveness check on this pattern
        pat' <- buildPatternEnv env pos ty mut pat
        unify pos ty (edata expr''')
        for_ maybeAnnot $ \annotation -> do
            annotTy <- resolveTypeIdent env pos NewTypesAreQuantified annotation
            unify pos ty annotTy

        unitType <- resolveVoidType env pos
        return $ ELet unitType mut pat' maybeAnnot expr'''

    EAssign pos lhs rhs -> do
        lhs' <- check env lhs
        rhs' <- check env rhs

        unify pos (edata lhs') (edata rhs')

        islvalue <- isLValue env lhs'
        when (not islvalue) $ do
            resumableTypeError pos $ NotAnLVar $ show lhs

        unitType <- resolveVoidType env pos
        return $ EAssign unitType lhs' rhs'

    ELiteral pos lit -> do
        litType <- case lit of
            LInteger _ -> resolveNumberType env pos
            LString _ -> resolveStringType env pos
            LUnit -> resolveVoidType env pos
        return $ ELiteral litType lit

    EArrayLiteral pos mutability elements -> do
        (arrayType, elementType) <- resolveArrayType env pos mutability
        elements' <- for elements $ \element -> do
            elementExpr <- check env element
            unify pos elementType (edata elementExpr)
            return elementExpr
        return $ EArrayLiteral arrayType mutability elements'

    ERecordLiteral pos fields -> do
        env' <- childEnv env
        fields' <- for (HashMap.toList fields) $ \(name, fieldExpr) -> do
            ty <- freshType env'
            fieldExpr' <- check env' fieldExpr >>= weaken (eLevel env')
            unify pos ty (edata fieldExpr')
            return (name, fieldExpr')

        let fieldTypes = map (\(name, ex) -> TypeRow{trName=name, trMut=RFree, trTyVar=edata ex}) fields'

        rec <- newIORef $ RRecord $ RecordType RecordClose fieldTypes
        let recordTy = TRecord rec
        return $ ERecordLiteral recordTy (HashMap.fromList fields')

    -- TODO: put all the intrinsics in one list so we can do a simple membership test here and not duplicate in the EApp handler
    EIdentifier pos (UnqualifiedReference "_unsafe_js") ->
        resumableTypeError pos $ IntrinsicError "Intrinsic _unsafe_js is not a value"
    EIdentifier pos (UnqualifiedReference "_unsafe_coerce") ->
        resumableTypeError pos $ IntrinsicError "Intrinsic _unsafe_coerce is not a value"
    EIdentifier pos txt -> do
        (rr, tyref) <- resolveValueReference env pos txt >>= \case
            (a@(Local, _), _mutability, b) -> do
                -- Don't instantiate locals.  Let generalization is tricky.
                return (a, b)
            (a, _mutability, b) -> do
                b' <- instantiate env b
                return (a, b')

        return $ EIdentifier tyref rr
    ESemi _ lhs rhs -> do
        lhs' <- check env lhs
        rhs' <- check env rhs
        return $ ESemi (edata rhs') lhs' rhs'

    EMethodApp pos lhs methodName args -> do
        -- lhs must be typechecked so that, if it has a concrete type, we know
        -- the location of that type.
        lhs' <- check env lhs
        moduleName <- followTypeVar (edata lhs') >>= \case
            TDataType TDataTypeDef{..} -> do
                return tuModuleName
            _ -> do
                ts <- showTypeVarIO $ edata lhs'
                resumableTypeError pos $ TdnrLhsTypeUnknown ts

        check env $ EApp
            pos
            (EIdentifier pos $ KnownReference moduleName methodName)
            (lhs : args)

    -- TEMP: For now, intrinsics are too polymorphic.
    -- Arithmetic operators like + and - have type (a, a) -> a
    -- Relational operators like <= and != have type (a, a) -> Bool
    EBinIntrinsic pos bi lhs rhs -> do
        lhs' <- check env lhs
        rhs' <- check env rhs

        if | isArithmeticOp bi -> do
                unify pos (edata lhs') (edata rhs')
                return $ EBinIntrinsic (edata lhs') bi lhs' rhs'
           | isRelationalOp bi -> do
                unify pos (edata lhs') (edata rhs')
                booleanType <- resolveBooleanType env pos
                return $ EBinIntrinsic booleanType bi lhs' rhs'
           | isBooleanOp bi -> do
                booleanType <- resolveBooleanType env (edata lhs)
                unify pos (edata lhs') booleanType
                unify pos (edata rhs') booleanType
                return $ EBinIntrinsic booleanType bi lhs' rhs'
           | otherwise ->
                error "This should be impossible: Check EBinIntrinsic"

    EIfThenElse pos condition ifTrue ifFalse -> do
        booleanType <- resolveBooleanType env pos

        condition' <- check env condition
        unify pos booleanType (edata condition')
        ifTrue' <- check env ifTrue
        ifFalse' <- check env ifFalse

        unify pos (edata ifTrue') (edata ifFalse')

        return $ EIfThenElse (edata ifTrue') condition' ifTrue' ifFalse'

    EWhile pos cond body -> do
        booleanType <- resolveBooleanType env pos
        unitType <- resolveVoidType env pos

        condition' <- check env cond
        unify pos booleanType (edata condition')

        let env' = env { eInLoop = True }
        body' <- check env' body
        unify pos unitType (edata body')

        return $ EWhile unitType condition' body'

    EFor pos name over body -> do
        unitType <- resolveVoidType env pos

        (arrayType, iteratorType) <- resolveArrayType env pos Immutable
        over' <- checkExpecting arrayType env over

        bindings' <- SymbolTable.clone (eValueBindings env)
        SymbolTable.insert bindings' pos SymbolTable.DisallowDuplicates name (ValueReference (Local, name) Immutable iteratorType)

        let env' = env { eValueBindings = bindings', eInLoop = True }
        body' <- check env' body
        unify pos unitType (edata body')

        return $ EFor unitType name over' body'

    EReturn pos rv -> do
        rv' <- check env rv
        case eReturnType env of
            Nothing ->
                error "Cannot return outside of functions"
            Just rt -> do
                unify pos rt $ edata rv'
                retTy <- freshType env
                return $ EReturn retTy rv'

    EBreak _ -> do
        when (not $ eInLoop env) $
            error "Cannot use 'break' outside of a loop"
        t <- freshType env
        return $ EBreak t

    EThrow pos exceptionName throwExpr -> do
        free <- freshType env
        ExceptionReference rr tyVar <- resolveExceptionReference env pos exceptionName
        {-
        ty <- HashTable.lookup exceptionName (eExceptionBindings env) >>= \case
            Just tyVar -> return tyVar
            Nothing -> do
                failTypeError pos $ UnboundException exceptionName
        -}
        throwExpr' <- checkExpecting tyVar env throwExpr
        return $ EThrow free rr throwExpr'

    ETryCatch pos tryBody exceptionName binding catchBody -> do
        tryBody' <- check env tryBody
        catchEnv <- childEnv env

        ExceptionReference rr ty <- resolveExceptionReference env pos exceptionName

        -- TODO: exhaustiveness check on this pattern
        binding' <- buildPatternEnv catchEnv pos ty Immutable binding
        catchBody' <- checkExpecting (edata tryBody') catchEnv catchBody
        return $ ETryCatch (edata tryBody') tryBody' rr binding' catchBody'

exportValue :: ExportFlag -> Env -> Pos -> Name -> (ResolvedReference, Mutability, TypeVar) -> TC ()
exportValue export env pos name value = do
    when (export == Export) $ do
        SymbolTable.insert (eExportedValues env) pos SymbolTable.DisallowDuplicates name value

exportType :: ExportFlag -> Env -> Pos -> Name -> TypeVar -> TC ()
exportType export env pos name typeVar = do
    when (export == Export) $ do
        SymbolTable.insert (eExportedTypes env) pos SymbolTable.DisallowDuplicates name typeVar

exportPattern :: ExportFlag -> Env -> Pos -> Name -> PatternReference -> TC ()
exportPattern export env pos name patternRef = do
    when (export == Export) $ do
        SymbolTable.insert (eExportedPatterns env) pos SymbolTable.DisallowDuplicates name patternRef

exportException :: ExportFlag -> Env -> Pos -> Name -> TypeVar -> TC ()
exportException export env pos name typeVar = do
    when (export == Export) $ do
        SymbolTable.insert (eExportedExceptions env) pos SymbolTable.DisallowDuplicates name typeVar

checkDecl :: Env -> Declaration UnresolvedReference () Pos -> TC (Declaration ResolvedReference PatternTag TypeVar)
checkDecl env (Declaration export pos decl) = fmap (Declaration export pos) $ g decl
 where
  g :: DeclarationType UnresolvedReference () Pos -> TC (DeclarationType ResolvedReference PatternTag TypeVar)
  g = \case

    DExportImport pos' name -> do
        SymbolTable.lookup (eValueBindings env) name >>= \case
            Just (ModuleReference mn) -> do
                case HashMap.lookup mn (eLoadedModules env) of
                    Just loadedModule -> do
                        for_ (lmExportedValues loadedModule) $ \(name', v) -> do
                            exportValue export env pos' name' v
                        for_ (lmExportedTypes loadedModule) $ \(name', t) -> do
                            exportType export env pos' name' t
                        for_ (lmExportedPatterns loadedModule) $ \(name', p) -> do
                            exportPattern export env pos' name' p
                        for_ (lmExportedExceptions loadedModule) $ \(name', e) -> do
                            exportException export env pos' name' e
                        -- TODO: introduce some dummy type? we don't need a type here
                        unitType <- resolveVoidType env pos
                        return $ DExportImport unitType name
                    Nothing ->
                        fail "ICE: module not loaded!"
            _ -> fail "Export import is not a module reference"

    {- VALUE DEFINITIONS -}

    DDeclare pos' name typeIdent -> do
        ty <- resolveTypeIdent env pos NewTypesAreQuantified typeIdent
        let resolvedRef = (Ambient, name)
        let mut = Immutable
        SymbolTable.insert (eValueBindings env) pos' SymbolTable.DisallowDuplicates name (ValueReference resolvedRef mut ty)
        exportValue export env pos' name (resolvedRef, mut, ty)
        return $ DDeclare ty name typeIdent
    DLet pos' mut pat maybeAnnot expr -> do
        env' <- childEnv env
        ty <- freshType env'
        for_ maybeAnnot $ \annotation -> do
            annotTy <- resolveTypeIdent env' pos NewTypesAreQuantified annotation
            unify pos' ty annotTy

        expr' <- check env' expr
        unify pos' ty (edata expr')

        expr'' <- case mut of
            Mutable -> weaken (eLevel env') expr'
            Immutable -> return expr'

        -- This is not unified with buildPatternEnv because ThisModule has different
        -- behavior from Local with respect to let generalization.
        pat' <- case pat of
            PWildcard -> do
                return PWildcard
                -- TODO: warn if export
            PBinding name -> do
                let rr = (FromModule $ eThisModule env, name)
                SymbolTable.insert (eValueBindings env) pos' SymbolTable.DisallowDuplicates name (ValueReference rr mut ty)
                exportValue export env pos' name (rr, mut, ty)
                return $ PBinding name
            PConstructor {} ->
                error "Patterns on top-level let bindings are not supported yet.  also TODO: export"
        quantify ty

        return $ DLet (edata expr'') mut pat' maybeAnnot expr''
    DFun pos' name fd -> do
        let expr = EFun pos' fd
        ty <- freshType env
        let rr = (FromModule $ eThisModule env, name)
        exportValue export env pos' name (rr, Immutable, ty)
        SymbolTable.insert (eValueBindings env) pos' SymbolTable.DisallowDuplicates name (ValueReference rr Immutable ty)
        expr'@(EFun _ fd') <- check env expr
        let FunctionDecl{fdBody=body', fdParams=args'} = fd'
        unify pos' (edata expr') ty
        quantify ty
        return $ DFun (edata expr') name FunctionDecl
            { fdParams = args'
            , fdReturnAnnot = fdReturnAnnot fd
            , fdForall = fdForall fd
            , fdBody = body'
            }

    {- TYPE DEFINITIONS -}

    DData pos' name typeParameters variants -> do
        -- TODO: add an internal compiler error if the name is not in bindings
        -- TODO: error when a name is inserted into type bindings twice at top level
        -- TODO: is there a better way to carry this information from environment
        -- setup through type checking of decls?
        (Just (TypeReference typeVar)) <- SymbolTable.lookup (eTypeBindings env) name

        exportType export env pos' name typeVar

        typedVariants <- for variants $ \(Variant _pos vname vparameters) -> do
            (Just (ValueReference rr mut ctorType)) <- SymbolTable.lookup (eValueBindings env) vname
            exportValue export env pos' vname (rr, mut, ctorType)
            return $ Variant ctorType vname vparameters

        let def = case typeVar of
                TDataType d -> d
                TTypeFun _ (TDataType d) -> d
                _ -> error $ "Internal compiler error: data decl registered incorrectly " ++ show typeVar
        for_ variants $ \(Variant _vtype vname _typeIdent) -> do
            exportPattern export env pos' vname $ PatternReference def $ TagVariant vname

        return $ DData typeVar name typeParameters typedVariants

    DJSData pos' name variants -> do
        -- TODO: add an internal compiler error if the name is not in bindings
        -- TODO: error when a name is inserted into type bindings twice at top level
        -- TODO: is there a better way to carry this information from environment
        -- setup through type checking of decls?
        (Just (TypeReference typeVar)) <- SymbolTable.lookup (eTypeBindings env) name

        exportType export env pos' name typeVar

        let (TDataType def) = typeVar

        for_ variants $ \(JSVariant vname literal) -> do
            let rr = (FromModule $ eThisModule env, vname)
            exportValue export env pos' vname (rr, Immutable, typeVar)
            exportPattern export env pos' vname $ PatternReference def $ TagLiteral literal

        return $ DJSData typeVar name variants

    DTypeAlias pos' name typeVars ident -> do
        -- TODO: add an internal compiler error if the name is not in bindings
        -- TODO: error when a name is inserted into type bindings twice at top level
        -- TODO: is there a better way to carry this information from environment
        -- setup through type checking of decls?
        (Just (TypeReference typeVar)) <- SymbolTable.lookup (eTypeBindings env) name
        exportType export env pos' name typeVar
        return $ DTypeAlias typeVar name typeVars ident
        
    DTrait _ traitName typeName contents -> do
        env' <- childEnv env
        _ <- newQuantifiedTypeVar env' pos typeName
        contents' <- for contents $ \(name, pos', typeIdent) -> do
            tv <- resolveTypeIdent env' pos' NewTypesAreErrors typeIdent
            return (name, tv, typeIdent)
        -- TODO: introduce some dummy type? we don't need a type here
        unitType <- resolveVoidType env pos
        return $ DTrait unitType traitName typeName contents'
        
    DImpl _ _ _ -> do
        fail "found impl"

    DException pos' name typeIdent -> do
        typeVar <- resolveTypeIdent env pos NewTypesAreErrors typeIdent
        exportException export env pos' name typeVar
        return $ DException typeVar name typeIdent

run :: HashMap ModuleName LoadedModule -> Module UnresolvedReference () Pos -> ModuleName -> TC LoadedModule
run loadedModules thisModule thisModuleName = do
    {-
    populate environment:
        eTypeBindings
        eValueBindings
        ePatternBindings

    phase 1:
        register all qualified imports
        register all unqualified imports

    phase 2:
      a. register all jsffi types (both data constructors and patterns)
      b. register all data types (and only the types)
      c. register all type aliases
      d. register all data type constructors and patterns (using same qvars from before)

    phase 3:
        type check all values in order
    -}

    -- Phases 1 and 2
    env <- buildTypeEnvironment thisModuleName loadedModules thisModule

    -- Phase 3
    decls <- for (mDecls thisModule) $ \decl -> do
        checkDecl env decl

    let lmModule = thisModule{ mDecls = decls }
    lmExportedValues <- HashMap.toList <$> SymbolTable.readAll (eExportedValues env)
    lmExportedTypes <- HashMap.toList <$> SymbolTable.readAll (eExportedTypes env)
    lmExportedPatterns <- HashMap.toList <$> SymbolTable.readAll (eExportedPatterns env)
    lmExportedExceptions <- HashMap.toList <$> SymbolTable.readAll (eExportedExceptions env)
    return $ LoadedModule{..}
