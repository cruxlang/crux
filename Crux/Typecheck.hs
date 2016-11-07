{-# LANGUAGE ScopedTypeVariables, TupleSections #-}

module Crux.Typecheck
    ( run
    ) where

import Crux.AST
import Crux.Error
import qualified Crux.HashTable as HashTable
import Crux.Module.Types
import Crux.ModuleName as ModuleName
import qualified Crux.SymbolTable as SymbolTable
import Crux.Pos (Pos(..), dummyPos)
import Crux.Prelude
import Crux.Typecheck.Env
import Crux.Typecheck.Inst
import Crux.Typecheck.Monad
import Crux.Typecheck.TypeAlloc
import Crux.Typecheck.Types
import Crux.Typecheck.Quantify
import Crux.Typecheck.Unify
import Crux.TypeVar
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import Prelude hiding (String)
import Text.Printf (printf)
import qualified Data.Set as Set

type ParsedExpression = Expression UnresolvedReference () Pos
type TypedExpression = Expression ResolvedReference PatternTag TypeVar

type ParsedDeclaration = Declaration UnresolvedReference () Pos
type TypedDeclaration = Declaration ResolvedReference PatternTag TypeVar

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
        unify env pos exprType $ TDataType def'

        when (length thisVariantParameters /= length cargs) $
            fail $ printf "Pattern %s should specify %i args but got %i" (Text.unpack cname) (length thisVariantParameters) (length cargs)

        args' <- for (zip cargs thisVariantParameters) $ \(arg, vp) -> do
            buildPatternEnv env pos vp mut arg

        return $ PConstructor unresolvedReference patternTag args'

    PTuple elements -> do
        let ctorRef = case elements of
                [] -> KnownReference "types" "Void"
                _ -> KnownReference "tuple" $ "Tuple" <> (Text.pack $ show $ length elements)
        buildPatternEnv env pos exprType mut $ PConstructor ctorRef () elements

checkLValue :: Env -> ParsedExpression -> TypedExpression -> TC ()
checkLValue env parsedExpr typedExpr = case (parsedExpr, typedExpr) of
    (EIdentifier pos name, EIdentifier _ _) -> do
        resolveValueReference env pos name >>= \case
            (_, Mutable, _) -> do
                pure ()
            (_, Immutable, _) -> do
                resumableTypeError pos $ ImmutableAssignment ImmutableBinding $ getUnresolvedReferenceLeaf name
    (ELookup pos _ _, ELookup lookupType lhs propName) -> do
        let field = RecordField
                { trName = propName
                , trMut = RMutable
                , trTyVar = lookupType
                }
        let recordConstraint = RecordConstraint
                { rcFields = [field]
                , rcFieldType = Nothing
                }
        let constraintSet = ConstraintSet (Just recordConstraint) mempty
        typeVar <- freshTypeConstrained env constraintSet

        unify env pos (edata lhs) typeVar
    _ -> fail "Unsupported assignment"

-- TODO: rename to checkNew or some other function that conveys "typecheck, but
-- I don't know or care what type you will be." and port all uses of check to it.
check :: Env -> ParsedExpression -> TC TypedExpression
check env expr = do
    newType <- freshType env
    checkExpecting newType env expr

checkInNewScope :: Env -> ParsedExpression -> TC TypedExpression
checkInNewScope env expr = do
    env' <- childEnv env
    check env' expr

checkExpecting :: TypeVar -> Env -> ParsedExpression -> TC TypedExpression
checkExpecting expectedType env expr = do
    e <- check' expectedType env expr
    unify env (edata expr) (edata e) expectedType
    return e

-- TODO: We should have this return a poison type.
resumableTypeError :: Pos -> TypeError -> TC a
resumableTypeError pos = failTypeError pos

weaken :: MonadIO m => TypeLevel -> TypedExpression -> m TypedExpression
weaken level e = do
    t' <- weaken' (edata e)
    return $ setEdata e t'
  where
    weaken' t = case t of
        TypeVar ref ->
            readIORef ref >>= \case
                TUnbound Strong lvl constraints tn
                    | level <= lvl -> do
                        -- TODO: weaken record typevars
                        writeIORef ref $ TUnbound Weak lvl constraints tn
                        return t
                TUnbound {} ->
                    -- TOD: weaken record typevars
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
        TRecord fields -> do
            weakenRecord fields
        TTypeFun a b -> do
            b' <- weaken' b
            return $ TTypeFun a b'

    weakenRecord fields = do
        fields' <- for fields $ \field@RecordField{..} -> do
            ty' <- weaken' trTyVar
            return field { trTyVar = ty' }
        return $ TRecord fields'

accumulateTraitReferences' :: MonadIO m
    => IORef (Set TypeNumber)
    -> IORef [(TypeVar, TypeNumber, TraitIdentity)]
    -> TypeVar
    -> m ()
accumulateTraitReferences' seen out tv = case tv of
    TypeVar ref -> readIORef ref >>= \case
        TUnbound _str _level constraints typeNumber -> do
            append constraints tv typeNumber
        TBound tv2 ->
            accumulateTraitReferences' seen out tv2
    TQuant _ constraints typeNumber -> do
        append constraints tv typeNumber
    TFun paramTypes returnType -> do
        for_ paramTypes $ accumulateTraitReferences' seen out
        accumulateTraitReferences' seen out returnType
    TDataType TDataTypeDef{..} -> do
        for_ tuParameters $
            accumulateTraitReferences' seen out
    TRecord rows -> do
        for_ rows $ \RecordField{..} ->
            accumulateTraitReferences' seen out trTyVar
    TTypeFun _ _ -> do
        fail "ICE: what does this mean"

  where
    append :: MonadIO m => ConstraintSet -> TypeVar -> TypeNumber -> m ()
    append (ConstraintSet record traits) typeVar typeNumber = do
        seen' <- readIORef seen
        when (not $ Set.member typeNumber seen') $ do
            modifyIORef seen (Set.insert typeNumber)

            -- TODO: we need to sort this list into a canonical order
            for_ (Set.toList traits) $ \traitIdentity -> do
                modifyIORef out ((typeVar, typeNumber, traitIdentity):)
            for_ record $ \RecordConstraint{..} -> do
                for_ rcFieldType $ accumulateTraitReferences' seen out

accumulateTraitReferences :: MonadIO m => [TypeVar] -> m [(TypeVar, TypeNumber, TraitIdentity)]
accumulateTraitReferences typeVars = do
    seen <- newIORef mempty
    out <- newIORef mempty
    for_ typeVars $ accumulateTraitReferences' seen out
    reverse <$> readIORef out

check' :: TypeVar -> Env -> ParsedExpression -> TC TypedExpression
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

        params' <- for (zip fdParams paramTypes) $ \((p, pAnn), pt) -> do
            for_ pAnn $ \(ann, maybeAlias) -> do
                annTy <- resolveTypeIdent env' pos ann
                unify env pos pt annTy
                for maybeAlias $ \alias -> do
                    SymbolTable.insert (eTypeBindings env') pos SymbolTable.DisallowDuplicates alias annTy

            -- TODO: exhaustiveness check on this pattern
            param' <- buildPatternEnv env' pos pt Immutable p
            return (param', pAnn)

        for_ fdReturnAnnot $ \ann -> do
            annTy <- resolveTypeIdent env' pos ann
            unify env pos returnType annTy

        body' <- check env' fdBody
        unify env pos returnType $ edata body'

        let ty = TFun paramTypes returnType
        return $ EFun ty FunctionDecl
            { fdParams=params'
            , fdReturnAnnot
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
                unify env pos (edata fn') appTy

                return $ EApp resultType fn' args'
            _ -> do
                args' <- for args $ check env
                result <- freshType env
                let ty = TFun (map edata args') result
                unify env pos (edata fn') ty
                return $ EApp result fn' args'

    EIntrinsic {} -> do
        error "Unexpected: EIntrinsic encountered during typechecking"

    ELookup pos lhs propName -> do
        let valueLookup = do
                -- if lhs is ident and in bindings, then go go go
                -- else turn into QualifiedReference and go go go
                lhs' <- check env lhs
                ty <- freshType env
                let field = RecordField
                        { trName = propName
                        , trMut = RFree
                        , trTyVar = ty}
                let recordConstraint = RecordConstraint
                        { rcFields = [field]
                        , rcFieldType = Nothing
                        }
                let constraintSet = ConstraintSet (Just recordConstraint) mempty
                recordType <- freshTypeConstrained env constraintSet
                unify env pos (edata lhs') recordType
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
            unify env pos resultType (edata caseExpr')
            return $ Case patt' caseExpr'

        return $ EMatch resultType matchExpr' cases'

    ELet pos mut pat forall maybeAnnot expr' -> do
        ty <- freshType env
        env' <- childEnv env
        _ <- registerExplicitTypeVariables env' forall
        expr'' <- check env' expr'
        expr''' <- case mut of
            Mutable -> weaken (eLevel env') expr''
            Immutable -> return expr''
        -- TODO: exhaustiveness check on this pattern
        pat' <- buildPatternEnv env pos ty mut pat
        unify env pos ty (edata expr''')
        for_ maybeAnnot $ \annotation -> do
            annotTy <- resolveTypeIdent env pos annotation
            unify env pos ty annotTy

        unitType <- resolveVoidType env pos
        return $ ELet unitType mut pat' forall maybeAnnot expr'''

    EAssign pos lhs rhs -> do
        lhs' <- check env lhs
        rhs' <- check env rhs

        unify env pos (edata lhs') (edata rhs')

        checkLValue env lhs lhs'

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
            unify env pos elementType (edata elementExpr)
            return elementExpr
        return $ EArrayLiteral arrayType mutability elements'

    ETupleLiteral pos elements -> do
        let ctor = EIdentifier pos $ KnownReference "tuple" $ "Tuple" <> (Text.pack $ show $ length elements)
        check env $ EApp pos ctor elements

    ERecordLiteral pos fields -> do
        env' <- childEnv env
        fields' <- for (HashMap.toList fields) $ \(name, (fieldMut, fieldExpr)) -> do
            ty <- freshType env'
            fieldExpr' <- check env' fieldExpr >>= weaken (eLevel env')
            unify env pos ty (edata fieldExpr')
            return (name, (fieldMut, fieldExpr'))

        let xlateMut Immutable = RImmutable
            xlateMut Mutable = RMutable
        let mapField (name, (mut, ex)) = RecordField
                { trName = name
                , trMut = xlateMut mut
                , trTyVar = edata ex
                }
        let fieldTypes = fmap mapField fields'
        let recordTy = TRecord fieldTypes
        return $ ERecordLiteral recordTy (HashMap.fromList fields')

    -- TODO: put all the intrinsics in one list so we can do a simple membership test here and not duplicate in the EApp handler
    EIdentifier pos (UnqualifiedReference "_unsafe_js") ->
        resumableTypeError pos $ IntrinsicError "Intrinsic _unsafe_js is not a value"
    EIdentifier pos (UnqualifiedReference "_unsafe_coerce") ->
        resumableTypeError pos $ IntrinsicError "Intrinsic _unsafe_coerce is not a value"
    EIdentifier pos txt -> do
        resolveValueReference env pos txt >>= \case
            (ref@(Local, _), _mutability, tv) -> do
                -- Don't instantiate locals.  TODO: Let generalization is tricky.
                return $ EIdentifier tv ref
            (ref, _mutability, tv) -> do
                tv' <- instantiate env tv
                traits <- accumulateTraitReferences [tv']
                case traits of
                    [] -> do
                        return $ EIdentifier tv' ref
                    _ -> do
                        placeholders <- for traits $ \(typeVar, _typeNumber, traitNumber) -> do
                            return $ EInstancePlaceholder typeVar traitNumber
                        return $ EApp
                            tv'
                            (EIdentifier tv' ref)
                            placeholders

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
                ts <- renderTypeVarIO $ edata lhs'
                resumableTypeError pos $ TdnrLhsTypeUnknown ts

        check env $ EApp
            pos
            (EIdentifier pos $ KnownReference moduleName methodName)
            (lhs : args)

    -- TEMP: For now, intrinsics are too polymorphic.
    -- Arithmetic operators like + and - have type (a, a) -> a
    -- Relational operators like <= and != have type (a, a) -> Bool
    EBinIntrinsic pos bi lhs rhs -> do
        let backingFunction = case bi of
                BIEqual -> Just "eq"
                BINotEqual -> Just "neq"
                BILess -> Just "lt"
                BILessEqual -> Just "lte"
                BIGreater -> Just "gt"
                BIGreaterEqual -> Just "gte"
                _ -> Nothing
        case backingFunction of
            Just name -> do
                check env $ EApp pos (EIdentifier pos $ KnownReference "cmp" name) [lhs, rhs]
            Nothing -> do
                lhs' <- check env lhs
                rhs' <- check env rhs

                if | isArithmeticOp bi -> do
                        unify env pos (edata lhs') (edata rhs')
                        return $ EBinIntrinsic (edata lhs') bi lhs' rhs'
                   | isBooleanOp bi -> do
                        booleanType <- resolveBooleanType env (edata lhs)
                        unify env pos (edata lhs') booleanType
                        unify env pos (edata rhs') booleanType
                        return $ EBinIntrinsic booleanType bi lhs' rhs'
                   | otherwise ->
                        error "This should be impossible: Check EBinIntrinsic"

    EIfThenElse pos condition ifTrue ifFalse -> do
        booleanType <- resolveBooleanType env pos

        condition' <- checkInNewScope env condition
        unify env pos booleanType (edata condition')
        ifTrue' <- checkInNewScope env ifTrue
        ifFalse' <- checkInNewScope env ifFalse

        unify env pos (edata ifTrue') (edata ifFalse')

        return $ EIfThenElse (edata ifTrue') condition' ifTrue' ifFalse'

    EWhile pos cond body -> do
        booleanType <- resolveBooleanType env pos
        unitType <- resolveVoidType env pos

        condition' <- check env cond
        unify env pos booleanType (edata condition')

        let env' = env { eInLoop = True }
        body' <- check env' body
        unify env pos unitType (edata body')

        return $ EWhile unitType condition' body'

    EFor pos name over body -> do
        unitType <- resolveVoidType env pos

        (arrayType, iteratorType) <- resolveArrayType env pos Immutable
        over' <- checkExpecting arrayType env over

        bindings' <- SymbolTable.clone (eValueBindings env)
        SymbolTable.insert bindings' pos SymbolTable.DisallowDuplicates name (ValueReference (Local, name) Immutable iteratorType)

        let env' = env { eValueBindings = bindings', eInLoop = True }
        body' <- check env' body
        unify env pos unitType (edata body')

        return $ EFor unitType name over' body'

    EReturn pos rv -> do
        rv' <- check env rv
        case eReturnType env of
            Nothing ->
                error "Cannot return outside of functions"
            Just rt -> do
                unify env pos rt $ edata rv'
                retTy <- freshType env
                return $ EReturn retTy rv'

    EBreak _ -> do
        when (not $ eInLoop env) $
            error "Cannot use 'break' outside of a loop"
        t <- freshType env
        return $ EBreak t

    EThrow pos exceptionName throwExpr -> do
        free <- freshType env
        (rr, tyVar) <- resolveExceptionReference env pos exceptionName
        throwExpr' <- checkExpecting tyVar env throwExpr
        return $ EThrow free rr throwExpr'

    ETryCatch pos tryBody exceptionName binding catchBody -> do
        tryBody' <- check env tryBody
        catchEnv <- childEnv env

        (rr, ty) <- resolveExceptionReference env pos exceptionName

        -- TODO: exhaustiveness check on this pattern
        binding' <- buildPatternEnv catchEnv pos ty Immutable binding
        catchBody' <- checkExpecting (edata tryBody') catchEnv catchBody
        return $ ETryCatch (edata tryBody') tryBody' rr binding' catchBody'

    EInstancePlaceholder _ _ -> do
        fail "ICE: placeholders are not typechecked"
    EInstanceDict _ _ _ -> do
        fail "ICE: instance dicts are not typechecked"
    EInstanceArgument _ _ -> do
        fail "ICE: instance arguments are not typechecked"

-- TODO: move into IR / backend
renderInstanceArgumentName :: TraitIdentity -> TypeNumber -> Name
renderInstanceArgumentName (TraitIdentity traitModule traitName) typeNumber =
    "$" <> (Text.intercalate "$" $ ModuleName.toList traitModule) <> "$" <> traitName <> "$" <> (Text.pack $ show typeNumber)

-- TODO: this replacement algorithm could be quadratic depending on the number of
-- nested functions, so we should switch the placeholders over to IORefs
resolveInstanceDictPlaceholders :: Env -> TypedExpression -> TC TypedExpression
resolveInstanceDictPlaceholders env = recurse
  where
    recurse = \case
        ELet tv mut pat forall typeIdent body -> do
            body' <- recurse body
            return $ ELet tv mut pat forall typeIdent body'
        ELookup tv body name -> do
            body' <- recurse body
            return $ ELookup tv body' name
        EApp tv fn args -> do
            fn' <- recurse fn
            args' <- for args recurse
            return $ EApp tv fn' args'
        EMatch tv expr cases -> do
            expr' <- recurse expr
            cases' <- for cases $ \(Case pat body) -> do
                body' <- recurse body
                return $ Case pat body'
            return $ EMatch tv expr' cases'
        EAssign tv lhs rhs -> do
            lhs' <- recurse lhs
            rhs' <- recurse rhs
            return $ EAssign tv lhs' rhs'
        expr@EIdentifier{} -> do
            return expr
        ESemi tv lhs rhs -> do
            lhs' <- recurse lhs
            rhs' <- recurse rhs
            return $ ESemi tv lhs' rhs'
        EMethodApp tv expr name args -> do
            expr' <- recurse expr
            args' <- for args recurse
            return $ EMethodApp tv expr' name args'
        EFun tv fd@FunctionDecl{fdBody} -> do
            fdBody' <- recurse fdBody
            return $ EFun tv fd{fdBody=fdBody'}
        ERecordLiteral tv fields -> do
            fields' <- for fields $ \(mut, expr) -> do
                expr' <- recurse expr
                return (mut, expr')
            return $ ERecordLiteral tv fields'
        EArrayLiteral tv mut elements -> do
            elements' <- for elements recurse
            return $ EArrayLiteral tv mut elements'
        ETupleLiteral tv elements -> do
            elements' <- for elements recurse
            return $ ETupleLiteral tv elements'
        expr@ELiteral{} -> do
            return expr
        EBinIntrinsic tv bi lhs rhs -> do
            lhs' <- recurse lhs
            rhs' <- recurse rhs
            return $ EBinIntrinsic tv bi lhs' rhs'
        EIntrinsic tv intrinsic -> do
            intrinsic' <- for intrinsic recurse
            return $ EIntrinsic tv intrinsic'
        EIfThenElse tv cond ifTrue ifFalse -> do
            cond' <- recurse cond
            ifTrue' <- recurse ifTrue
            ifFalse' <- recurse ifFalse
            return $ EIfThenElse tv cond' ifTrue' ifFalse'
        EWhile tv cond body -> do
            cond' <- recurse cond
            body' <- recurse body
            return $ EWhile tv cond' body'
        EFor tv name iter body -> do
            iter' <- recurse iter
            body' <- recurse body
            return $ EFor tv name iter' body'
        EReturn tv value -> do
            value' <- recurse value
            return $ EReturn tv value'
        EBreak tv -> do
            return $ EBreak tv
        EThrow tv ident value -> do
            value' <- recurse value
            return $ EThrow tv ident value'
        ETryCatch tv try ident pat catch -> do
            try' <- recurse try
            catch' <- recurse catch
            return $ ETryCatch tv try' ident pat catch'

        EInstancePlaceholder tv traitIdentity -> followTypeVar tv >>= \case
            TypeVar ref -> readIORef ref >>= \case
                TUnbound _str _level _constraints typeNumber ->
                    -- TODO: this needs a better error message - it just means we are trying to resolve
                    -- a trait impl for a polymorphic type
                    fail $ "should have been quantified by now: " ++ show tv ++ " -- " ++ show traitIdentity ++ " -- " ++ show typeNumber
                _ ->
                    fail "never happens"
            TQuant _ _constraints typeNumber -> do
                return $ EInstanceArgument tv $ renderInstanceArgumentName traitIdentity typeNumber
                --append constraints tv typeNumber
            TFun _paramTypes _returnType -> do
                fail "Don't support traits on functions"
            TDataType def -> do
                let dtid = dataTypeIdentity def
                generateImplReference tv traitIdentity dtid
            TRecord fields -> do
                -- TODO: if we frequently refer to instances for the same set of (field, typevar), cache them
                -- generate field map function
                let dontcare = tv
                let thisDict = EInstanceDict dontcare traitIdentity RecordIdentity
                let fieldMap = EInstanceFieldMap dontcare traitIdentity
                -- TODO: it would be nice to have a dummy TypeVar for these kind of made-up type situations.
                transformedFieldList <- for fields $ \RecordField{..} -> do
                    let recarg = EIdentifier tv (Local, "rec")
                    let rhs = ELookup trTyVar recarg trName
                    let transformedValue = EApp dontcare fieldMap [rhs]
                    return (trName, (Immutable, transformedValue))
                let funDecl = FunctionDecl
                        { fdParams = [(PBinding "rec", Nothing)]
                        , fdReturnAnnot = Nothing
                        , fdBody = ERecordLiteral dontcare $ HashMap.fromList transformedFieldList
                        }
                let fieldMapFunction = EFun dontcare funDecl
                return $ EApp tv thisDict [fieldMapFunction]
            TTypeFun _ _ -> do
                fail "ICE: what does this mean"

        EInstanceDict _ _ _ -> do
            fail "I don't think we're supposed to do this"
        EInstanceArgument _ _ -> do
            fail "I don't think we're supposed to do this either"

    generateImplReference :: TypeVar -> TraitIdentity -> TraitImplIdentity -> TC TypedExpression
    generateImplReference tv traitIdentity traitImplIdentity = do
        instanceDesc <- HashTable.lookup (traitIdentity, traitImplIdentity) (eKnownInstances env) >>= \case
            Just instanceDesc -> return instanceDesc
            Nothing -> fail "Unification has already validated we have an instance" -- failTypeError pos $ NoTraitOnType tv (tdName traitDesc) (tdModule traitDesc)

        instanceType <- instantiate env $ idTypeVar instanceDesc
        traits <- accumulateTraitReferences [instanceType]
        -- TODO: find a real Pos somewhere
        unify env dummyPos instanceType tv

        let thisDict = EInstanceDict tv traitIdentity traitImplIdentity
        case traitImplIdentity of
            DataIdentity{} -> do
                case traits of
                    [] -> do
                        return thisDict
                    _ -> do
                        argDicts <- for traits $ \(typeVar, _typeNumber, traitNumber) -> do
                            --quantify typeVar -- this feels dirty
                            resolveInstanceDictPlaceholders env $ EInstancePlaceholder typeVar traitNumber
                        return $ EApp tv thisDict argDicts
            RecordIdentity -> do
                fail "TODO: remove this variant"

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

exportTrait :: ExportFlag -> Env -> Pos -> Name -> ResolvedReference -> TraitIdentity -> TraitDesc -> TC ()
exportTrait export env pos name traitRef traitIdentity traitDesc = do
    when (export == Export) $ do
        SymbolTable.insert (eExportedTraits env) pos SymbolTable.DisallowDuplicates name (traitRef, traitIdentity, traitDesc)

exportException :: ExportFlag -> Env -> Pos -> Name -> (ResolvedReference, TypeVar) -> TC ()
exportException export env pos name typeVar = do
    when (export == Export) $ do
        SymbolTable.insert (eExportedExceptions env) pos SymbolTable.DisallowDuplicates name typeVar

checkDecl :: Env -> ParsedDeclaration -> TC TypedDeclaration
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
                        for_ (lmExportedTraits loadedModule) $ \(name', (traitRef, traitIdentity, traitDesc)) -> do
                            exportTrait export env pos' name' traitRef traitIdentity traitDesc
                        for_ (lmExportedExceptions loadedModule) $ \(name', e) -> do
                            exportException export env pos' name' e
                        -- TODO: introduce some dummy type? we don't need a type here
                        unitType <- resolveVoidType env pos
                        return $ DExportImport unitType name
                    Nothing ->
                        fail "ICE: module not loaded!"
            _ -> fail "Export import is not a module reference"

    {- VALUE DEFINITIONS -}

    DDeclare pos' name forall typeIdent -> do
        env' <- childEnv env
        _ <- registerExplicitTypeVariables env' forall
        ty <- resolveTypeIdent env' pos typeIdent
        let resolvedRef = (Ambient, name)
        let mut = Immutable
        SymbolTable.insert (eValueBindings env) pos' SymbolTable.DisallowDuplicates name (ValueReference resolvedRef mut ty)
        exportValue export env pos' name (resolvedRef, mut, ty)
        return $ DDeclare ty name forall typeIdent
    DLet pos' mut pat forall maybeAnnot expr -> do
        env' <- childEnv env
        ty <- freshType env'
        _ <- registerExplicitTypeVariables env' forall
        for_ maybeAnnot $ \annotation -> do
            annotTy <- resolveTypeIdent env' pos annotation
            unify env pos' ty annotTy

        expr' <- check env' expr
        unify env pos' ty (edata expr')

        expr'' <- case mut of
            Mutable -> weaken (eLevel env') expr'
            Immutable -> return expr'

        -- This is not unified with buildPatternEnv because ThisModule has different
        -- behavior from Local with respect to let generalization.
        -- TODO: try again
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
            PTuple {} ->
                error "Tuple patterns on top-level let bindings are not supported yet.  also TODO: export"

        quantify ty

        traitRefs <- accumulateTraitReferences [ty]
        dictArgs <- for traitRefs $ \(_, typeNumber, traitIdentity) -> do
            return $ PBinding $ renderInstanceArgumentName traitIdentity typeNumber

        expr''' <- resolveInstanceDictPlaceholders env expr''

        let expr'''' = case dictArgs of
                [] -> expr'''
                _ -> EFun (edata expr''') $ FunctionDecl
                    { fdParams = map (\p -> (p, Nothing)) dictArgs
                    , fdReturnAnnot = Nothing
                    , fdBody = expr'''
                    }

        return $ DLet (edata expr'''') mut pat' forall maybeAnnot expr''''

    DFun pos' name forall fd -> do
        ty <- freshType env
        let rr = (FromModule $ eThisModule env, name)
        exportValue export env pos' name (rr, Immutable, ty)
        SymbolTable.insert (eValueBindings env) pos' SymbolTable.DisallowDuplicates name (ValueReference rr Immutable ty)
        env' <- childEnv env
        _ <- registerExplicitTypeVariables env' forall
        expr'@(EFun _ fd') <- check env' $ EFun pos' fd
        let FunctionDecl{fdBody=body', fdParams=args'} = fd'
        unify env pos' (edata expr') ty

        quantify ty

        traitRefs <- accumulateTraitReferences [ty]
        dictArgs <- for traitRefs $ \(_, typeNumber, traitIdentity) -> do
            return $ PBinding $ renderInstanceArgumentName traitIdentity typeNumber

        body'' <- resolveInstanceDictPlaceholders env body'

        let innerFD = FunctionDecl
                { fdParams = args'
                , fdReturnAnnot = fdReturnAnnot fd
                , fdBody = body''
                }
        let outerFD = case dictArgs of
                [] -> innerFD
                _ ->
                    FunctionDecl
                        { fdParams = map (\p -> (p, Nothing)) dictArgs
                        , fdReturnAnnot = Nothing
                        , fdBody = EFun (edata expr') innerFD
                        }

        return $ DFun (edata expr') name forall outerFD

    {- TYPE DEFINITIONS -}

    DData pos' name typeParameters variants -> do
        -- TODO: add an internal compiler error if the name is not in bindings
        -- TODO: error when a name is inserted into type bindings twice at top level
        -- TODO: is there a better way to carry this information from environment
        -- setup through type checking of decls?
        (Just typeVar) <- SymbolTable.lookup (eTypeBindings env) name

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
        (Just typeVar) <- SymbolTable.lookup (eTypeBindings env) name

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
        (Just typeVar) <- SymbolTable.lookup (eTypeBindings env) name
        exportType export env pos' name typeVar
        return $ DTypeAlias typeVar name typeVars ident

    DTrait pos' traitName contents -> do
        -- TODO: add an ICE if it's not already set up in the environment
        Just (traitRef, traitNumber, traitDesc) <- SymbolTable.lookup (eTraitBindings env) traitName
        env' <- childEnv env
        let typeName = "self"
        _ <- newQuantifiedConstrainedTypeVar env' pos typeName traitNumber
        contents' <- for contents $ \(name, pos'', typeIdent) -> do
            tv <- resolveTypeIdent env' pos'' typeIdent
            let rr = (FromModule $ eThisModule env, name)
            exportValue export env pos'' name (rr, Immutable, tv)
            return (name, tv, typeIdent)
        -- TODO: introduce some dummy type? we don't need a type here
        unitType <- resolveVoidType env pos
        exportTrait export env pos' traitName traitRef traitNumber traitDesc
        return $ DTrait unitType traitName contents'

    DImpl pos' traitReference implType implValues -> do
        (traitRef, traitIdentity, traitDesc) <- resolveTraitReference env pos traitReference
        env' <- childEnv env

        (typeIdentity, typeVar, implType') <- case implType of
            ImplTypeNominal ImplNominal{..} -> do
                typeVar <- resolveTypeReference env pos' inTypeName

                let traitNames = Set.fromList $ fmap fst $ tdMethods traitDesc
                let implNames = Set.fromList $ fmap fst implValues

                let notImplemented = Set.difference traitNames implNames
                when (not $ Set.null notImplemented) $ do
                    failTypeError pos' $ IncompleteImpl $ Set.toList notImplemented

                instanceParameterTypes <- registerExplicitTypeVariables env' inTypeParams
                instanceTypeVar <- applyTypeFunction env pos inTypeName DisallowTypeFunctions typeVar instanceParameterTypes

                traitRefs <- accumulateTraitReferences [instanceTypeVar]
                dictArgs <- for traitRefs $ \(_, typeNumber, traitIdentity') -> do
                    return $ renderInstanceArgumentName traitIdentity' typeNumber

                typeIdentity <- case instanceTypeVar of
                    TDataType def -> return $ dataTypeIdentity def
                    TRecord _ -> return RecordIdentity
                    _ -> fail "hot dog"

                let implType' = ImplTypeNominal $ ImplNominal
                        { inTypeName = inTypeName
                        , inTypeParams = inTypeParams
                        , inContextDictArgs = dictArgs
                        }
                return (typeIdentity, instanceTypeVar, implType')

            ImplTypeRecord fieldFunction -> do
                concreteRecordType <- freshTypeConstrained env' $ ConstraintSet
                    -- This RecordConstraint isn't strictly necessary but might produce better error messages.
                    (Just RecordConstraint{rcFields=[], rcFieldType=Nothing})
                    (Set.singleton traitIdentity)
                quantify concreteRecordType

                -- typecheck the field transformer
                typedFieldFunction@(EFun (TFun [_] retType) _) <- check env' fieldFunction

                let recordConstraint = RecordConstraint
                        { rcFields = []
                        , rcFieldType = Just retType
                        }
                transformedRecordType <- freshTypeConstrained env' $ ConstraintSet (Just recordConstraint) mempty

                -- insert a fieldMap function of type concrete-record -> unified-record into environment
                let fieldMapType = TFun [concreteRecordType] transformedRecordType

                -- also quantifies the fieldFunction type
                quantify fieldMapType

                let fieldMapRef = ValueReference (Local, "fieldMap") Immutable fieldMapType
                SymbolTable.insert (eValueBindings env') pos' SymbolTable.DisallowDuplicates "fieldMap" fieldMapRef

                let implType' = ImplTypeRecord typedFieldFunction
                return (RecordIdentity, concreteRecordType, implType')

        let instanceDesc = InstanceDesc
                { idModuleName = eThisModule env
                , idTypeVar = typeVar
                }
        HashTable.insert (traitIdentity, typeIdentity) instanceDesc (eKnownInstances env)

        -- instantiate all of the methods in the same subst dict
        -- so the typevar is unified across methods
        subst <- newIORef mempty
        let inst = instantiate' subst env'
        traitParameter <- inst $ tdTypeVar traitDesc
        unify env' pos' typeVar traitParameter

        newMethods <- for (tdMethods traitDesc) $ \(name, methodType) -> do
            methodType' <- inst methodType
            return (name, methodType')

        values' <- for implValues $ \(methodName, expr) -> do
            when (1 < (length $ filter (\(name, _) -> name == methodName) implValues)) $ do
                -- TODO: use the pos of the method here
                failTypeError pos' $ DuplicateSymbol methodName

            methodTypeVar <- case lookup methodName newMethods of
                Just tv -> return tv
                -- TODO: use the pos of the method here
                Nothing -> failTypeError pos' $ UnexpectedImplMethod methodName

            expr' <- checkExpecting methodTypeVar env' expr
            expr'' <- resolveInstanceDictPlaceholders env' expr'
            return (methodName, expr'')
        
        return $ DImpl typeVar traitRef implType' values'

    DException pos' name typeIdent -> do
        -- TODO: look it up in the current environment
        typeVar <- resolveTypeIdent env pos typeIdent
        exportException export env pos' name ((FromModule $ eThisModule env, name), typeVar)
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
    lmExportedTraits <- HashMap.toList <$> SymbolTable.readAll (eExportedTraits env)
    lmExportedExceptions <- HashMap.toList <$> SymbolTable.readAll (eExportedExceptions env)
    lmKnownInstances <- HashTable.read (eKnownInstances env)
    return $ LoadedModule{..}
