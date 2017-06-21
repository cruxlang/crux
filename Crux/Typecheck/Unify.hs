{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Crux.Typecheck.Unify where

import Crux.AST
import Crux.Error
import qualified Crux.HashTable as HashTable
import Crux.Prelude
import Crux.Typecheck.Inst
import Crux.Typecheck.Monad
import Crux.Typecheck.Occurs
import Crux.Typecheck.TypeAlloc
import Crux.Typecheck.Types
import Crux.TypeVar
import Data.List (sort)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import qualified Crux.SymbolTable as SymbolTable
import Crux.Module.Types
import Crux.Pos (Pos)
import qualified Crux.Error as Error
import Crux.ModuleName (ModuleName)
import Crux.Util
import qualified Data.Text as Text

-- Export Resolution

findExportByName :: (LoadedModule -> [(Name, a)]) -> Env -> Pos -> ModuleName -> Name -> TC a
findExportByName getExports env pos moduleName valueName = do
    modul <- case HashMap.lookup moduleName (eLoadedModules env) of
        Just modul -> return modul
        Nothing -> failICE pos $ DependentModuleNotLoaded moduleName
    let r = findFirstOf (getExports modul) $ \(name, v) ->
            if name == valueName then
                Just v
            else
                Nothing
    case r of
        Just e -> return e
        Nothing -> do
            failTypeError pos $ ModuleReferenceError moduleName valueName

resolveImportName :: Env -> Pos -> Name -> TC ModuleName
resolveImportName env pos importName = do
    SymbolTable.lookup (eValueBindings env) importName >>= \case
        Just (ModuleReference moduleName) -> return moduleName
        -- TODO: should we differentiate between these cases?
        Just _ -> failTypeError pos $ Error.UnboundSymbol "import" importName
        _ -> failTypeError pos $ Error.UnboundSymbol "import" importName

-- I could merge this into ResolveReference but that's a bit tricky because of the ModuleReference stuff
resolveValueReference :: Env -> Pos -> UnresolvedReference -> TC (ResolvedReference, Mutability, TypeVar)
resolveValueReference env pos = \case
    UnqualifiedReference name -> do
        SymbolTable.lookup (eValueBindings env) name >>= \case
            Just (ValueReference rr mut t) -> return (rr, mut, t)
            -- TODO: turn this into a custom error message
            Just (ModuleReference _) -> failTypeError pos $ Error.UnboundSymbol "value" name
            Nothing -> failTypeError pos $ Error.UnboundSymbol "value" name
    QualifiedReference importName name -> do
        moduleName <- resolveImportName env pos importName
        resolveValueReference env pos $ KnownReference moduleName name
    KnownReference moduleName name -> do
        if moduleName == eThisModule env then do
            resolveValueReference env pos (UnqualifiedReference name)
        else do
            findExportByName lmExportedValues env pos moduleName name

resolveReference :: [Char] -> (Env -> SymbolTable.SymbolTable a) -> (LoadedModule -> [(Name, a)]) -> Env -> Pos -> UnresolvedReference -> TC a
resolveReference symbolType bindingTable exportTable env pos = \case
    UnqualifiedReference name -> do
        SymbolTable.lookup (bindingTable env) name >>= \case
            Just er -> return er
            Nothing -> failTypeError pos $ Error.UnboundSymbol symbolType name
    QualifiedReference importName name -> do
        moduleName <- resolveImportName env pos importName
        resolveReference symbolType bindingTable exportTable env pos $ KnownReference moduleName name
    KnownReference moduleName name -> do
        if moduleName == eThisModule env then do
            resolveReference symbolType bindingTable exportTable env pos (UnqualifiedReference name)
        else do
            findExportByName exportTable env pos moduleName name

resolveTypeReference :: Env -> Pos -> UnresolvedReference -> TC TypeVar
resolveTypeReference = resolveReference "type" eTypeBindings lmExportedTypes

resolvePatternReference :: Env -> Pos -> UnresolvedReference -> TC PatternReference
resolvePatternReference = resolveReference "pattern" ePatternBindings lmExportedPatterns

resolveTraitReference :: Env -> Pos -> UnresolvedReference -> TC (ResolvedReference, TraitIdentity, TraitDesc)
resolveTraitReference = resolveReference "trait" eTraitBindings lmExportedTraits

resolveExceptionReference :: Env -> Pos -> UnresolvedReference -> TC (ResolvedReference, TypeVar)
resolveExceptionReference = resolveReference "exception" eExceptionBindings lmExportedExceptions

-- Type Application

data TypeApplicationPolicy = AllowTypeFunctions | DisallowTypeFunctions
    deriving (Eq)

applyTypeFunction :: Env -> Pos -> UnresolvedReference -> TypeApplicationPolicy -> TypeVar -> [TypeVar] -> TC TypeVar
applyTypeFunction env pos typeReference typeApplicationPolicy inputType typeArguments = do
    let typeName = getUnresolvedReferenceLeaf typeReference
    ty <- followTypeVar inputType
    case ty of
        TDataType TDataTypeDef{tuName}
            | [] == typeArguments -> do
                return ty
            | otherwise -> do
                failTypeError pos $ Error.IllegalTypeApplication tuName
        TTypeFun tuParameters _rt
            | [] == typeArguments -> case typeApplicationPolicy of
                AllowTypeFunctions -> return ty
                DisallowTypeFunctions -> do
                    failTypeError pos $ Error.TypeApplicationMismatch typeName (length tuParameters) 0
            | length tuParameters == length typeArguments -> do
                (TTypeFun tuParameters' rt') <- instantiate env ty
                for_ (zip tuParameters' typeArguments) $ \(a, b) -> do
                    unify env pos a b
                return rt'
            | otherwise -> do
                failTypeError pos $ Error.TypeApplicationMismatch typeName (length tuParameters) (length typeArguments)
        _
            | [] == typeArguments ->
                return ty
            | otherwise ->
                -- TODO: make this error message sane
                failTypeError pos $ Error.IllegalTypeApplication (Text.pack $ show ty)

-- Builtin Type Resolution

resolveArrayType :: Env -> Pos -> Mutability -> TC (TypeVar, TypeVar)
resolveArrayType env pos mutability = do
    elementType <- case mutability of
            Immutable -> freshType env
            Mutable -> freshWeakQVar env

    let typeReference = case mutability of
            Immutable -> KnownReference "array" "Array"
            Mutable -> KnownReference "mutarray" "MutableArray"
    arrayType <- resolveTypeReference env pos typeReference
    newArrayType <- applyTypeFunction env pos typeReference DisallowTypeFunctions arrayType [elementType]
    return (newArrayType, elementType)

resolveOptionType :: Env -> Pos -> TC (TypeVar, TypeVar)
resolveOptionType env pos = do
    elementType <- freshType env

    let typeReference = KnownReference "option" "Option"
    optionType <- resolveTypeReference env pos typeReference
    newOptionType <- applyTypeFunction env pos typeReference DisallowTypeFunctions optionType [elementType]
    return (newOptionType, elementType)

resolveBooleanType :: Env -> Pos -> TC TypeVar
resolveBooleanType env pos = do
    resolveTypeReference env pos (KnownReference "types" "Boolean")

resolveNumberType :: Env -> Pos -> TC TypeVar
resolveNumberType env pos = do
    resolveTypeReference env pos (KnownReference "number" "Number")

resolveStringType :: Env -> Pos -> TC TypeVar
resolveStringType env pos = do
    resolveTypeReference env pos (KnownReference "string" "String")

resolveVoidType :: Env -> Pos -> TC TypeVar
resolveVoidType env pos = do
    resolveTypeReference env pos (KnownReference "types" "Void")

resolveTupleType :: Env -> Pos -> [TypeVar] -> TC TypeVar
resolveTupleType env pos elements = do
    let tupleReference = KnownReference "tuple" $ "Tuple" <> (Text.pack $ show $ length elements)
    tupleType <- resolveTypeReference env pos tupleReference
    applyTypeFunction env pos tupleReference DisallowTypeFunctions tupleType elements

-- Unification

unificationError :: Pos -> String -> TypeVar -> TypeVar -> TC a
unificationError pos message a b = do
    failTypeError pos $ UnificationError message a b

lookupRecordField :: Name -> [RecordField t] -> Maybe (FieldMutability, t)
lookupRecordField name = \case
    [] -> Nothing
    (RecordField{..}:rest)
        | trName == name -> Just (trMut, trTyVar)
        | otherwise -> lookupRecordField name rest

unifyFieldConstraint :: Env -> Pos -> Maybe TypeVar -> RecordField TypeVar -> TC (RecordField TypeVar)
unifyFieldConstraint env pos constraint tr@RecordField{..} = do
    case constraint of
        Nothing -> return tr
        Just ctv -> do
            trMut' <- case trMut of
                RMutable -> failTypeError pos $ RecordMutabilityUnificationError trName "Row variable constraints don't support mutable fields"
                RImmutable -> return RImmutable
                RQuantified -> failTypeError pos $ RecordMutabilityUnificationError trName "Row variable constraints don't support mutable (even quantified) fields"
                RFree -> return RImmutable
            unify env pos ctv trTyVar
            return tr{ trMut=trMut' }

unifyConcreteRecord :: Env -> Pos -> TypeVar -> TypeVar -> [RecordField TypeVar] -> [RecordField TypeVar] -> TC ()
unifyConcreteRecord env pos tvA tvB fieldsA fieldsB = do
    let aFields = sort $ map trName fieldsA
    let bFields = sort $ map trName fieldsB

    let coincidentRows = [(a, b) | a <- fieldsA, b <- fieldsB, trName a == trName b]
    let aOnlyRows = filter (\row -> trName row `notElem` bFields) fieldsA
    let bOnlyRows = filter (\row -> trName row `notElem` aFields) fieldsB
    --let names trs = map trName trs

    for_ coincidentRows $ \(lhs, rhs) -> do
        mut <- unifyRecordMutability (trName lhs) pos (trMut lhs) (trMut rhs)
        unify env pos (trTyVar lhs) (trTyVar rhs)
        return lhs { trMut = mut }

    if null aOnlyRows && null bOnlyRows then do
        return ()
    else do
        -- TODO: better error messages here
        unificationError pos "Closed row types must match exactly" tvA tvB

unifyRecordMutability :: Name -> Pos -> FieldMutability -> FieldMutability -> TC FieldMutability
unifyRecordMutability propName pos m1 m2 = case (m1, m2) of
    (RFree, _) -> return m2
    (_, RFree) -> return m1
    (RImmutable, RImmutable) -> return RImmutable
    (RImmutable, RMutable) -> failTypeError pos $ RecordMutabilityUnificationError propName "Record field mutability does not match"
    (RMutable, RMutable) -> return RMutable
    (RMutable, RImmutable) -> failTypeError pos $ RecordMutabilityUnificationError propName "Record field mutability does not match"
    (RQuantified, _) -> fail "Quant!! D:"
    (_, RQuantified) -> fail "Quant2!! D:"

isOptionalField :: MonadIO m => RecordField TypeVar -> m Bool
isOptionalField RecordField{trTyVar} = do
    tv <- followTypeVar trTyVar
    case tv of
        TDataType (TDataTypeDef{tuName, tuModuleName}) ->
            return $ (tuName, tuModuleName) `elem`
                [ ("JSOption", "js.option")
                , ("Option", "option")
                ]
        _ ->
            return False

validateFields :: Env -> Pos -> TypeVar -> [RecordField TypeVar] -> [RecordField TypeVar] -> TC ()
validateFields env pos actualType actual expected = do
    for_ expected $ \field -> do
        optional <- isOptionalField field
        let found = filter (\f -> trName field == trName f) actual
        case found of
            [] | optional ->
                return ()
               | otherwise ->
                failTypeError pos $ RecordMissingField actualType (trName field)
            [cf] -> do
                -- TODO: if we are going to unify record mutability, we need to actually put the new mutability state somewhere
                _ <- unifyRecordMutability (trName cf) pos (trMut cf) (trMut field)

                unify env pos (trTyVar cf) (trTyVar field)
            _ -> fail "Internal error: found multiple properties with the same name"

validateRecordConstraint :: Env -> Pos -> RecordConstraint -> TypeVar -> TC ()
validateRecordConstraint env pos (RecordConstraint fields fieldType) typeVar = case typeVar of
    TypeVar _ -> do
        fail "We already handled this case"
    TQuant _source (ConstraintSet record _traits) _number -> do
        -- validate 
        quantRecordConstraint <- case record of
            Nothing -> fail "Quantified type variable is not a record"
            Just rc -> return rc

        validateFields env pos typeVar (rcFields quantRecordConstraint) fields

        for_ fieldType $ \fieldType' -> do
            for_ (rcFields quantRecordConstraint) $ \quantField -> do
                when ([] == filter (\f -> trName quantField == trName f) fields) $ do
                    unify env pos (trTyVar quantField) fieldType'

            for_ (rcFieldType quantRecordConstraint) $ \qft -> do
                unify env pos fieldType' qft

    TFun _ _ -> do
        fail "Functions are not records"
    TDataType _def -> do
        fail "Data types are not records"
    TRecord closedFields -> do
        validateFields env pos typeVar closedFields fields
        for_ fieldType $ \ft -> do
            for_ closedFields $ \cf -> do
                when ([] == filter (\f -> trName cf == trName f) fields) $ do
                    unify env pos (trTyVar cf) ft
    TTypeFun _ _ -> do
        fail "Type functions are not records"

validateTraitConstraint :: Env -> Pos -> TraitIdentity -> TraitDesc -> TypeVar -> TC ()
validateTraitConstraint env pos traitIdentity traitDesc typeVar = case typeVar of
    TypeVar _ -> do
        fail "We already handled this case"
    TQuant _source (ConstraintSet _record traits) _tn -> do
        when (not $ Set.member traitIdentity traits) $ do
            failTypeError pos $ NoTraitOnType typeVar (tdName traitDesc) (tdModule traitDesc)
    TFun _ _ -> do
        fail "Functions do not implement traits (yet)"
    TDataType def -> do
        let key = (traitIdentity, dataTypeIdentity def)
        HashTable.lookup key (eKnownInstances env) >>= \case
            Just InstanceDesc{idTypeVar} -> do
                idTypeVar' <- instantiate env idTypeVar
                unify env pos idTypeVar' typeVar
                return ()
            Nothing -> do
                failTypeError pos $ NoTraitOnType typeVar (tdName traitDesc) (tdModule traitDesc)
    TRecord _fields -> do
        let key = (traitIdentity, RecordIdentity)
        HashTable.lookup key (eKnownInstances env) >>= \case
            Just InstanceDesc{..} -> do
                -- TODO: what do we do here?
                -- start unifying types with the field transformer?
                return ()
            Nothing -> do
                failTypeError pos $ NoTraitOnRecord typeVar (tdName traitDesc) (tdModule traitDesc)
    TTypeFun _ (TDataType def) -> do
        let key = (traitIdentity, dataTypeIdentity def)
        HashTable.lookup key (eKnownInstances env) >>= \case
            Just InstanceDesc{idTypeVar} -> do
                -- generalize this code with TDataType above
                idTypeVar' <- instantiate env idTypeVar
                unify env pos idTypeVar' typeVar
                return ()
            Nothing -> do
                failTypeError pos $ NoTraitOnRecord typeVar (tdName traitDesc) (tdModule traitDesc)
    TTypeFun _ _ -> do
        fail "Wat? Type functions definitely don't implement constraints"

-- TODO: is this right
unifyStrength :: Strength -> Strength -> Strength
unifyStrength Strong Strong = Strong
unifyStrength Strong Weak = Weak
unifyStrength Weak Strong = Weak
unifyStrength Weak Weak = Weak

-- TODO: how do we merge level?
unifyLevel :: TypeLevel -> TypeLevel -> TypeLevel
unifyLevel _left right = right

unifyRecordConstraint :: Env -> Pos -> RecordConstraint -> RecordConstraint -> TC RecordConstraint
unifyRecordConstraint env pos rcA rcB = do
    let aRows = rcFields rcA
    let bRows = rcFields rcB

    let constraintA = rcFieldType rcA
    let constraintB = rcFieldType rcB

    let aFields = sort $ map trName aRows
    let bFields = sort $ map trName bRows

    let coincidentRows = [(a, b) | a <- aRows, b <- bRows, trName a == trName b]
    let aOnlyRows = filter (\row -> trName row `notElem` bFields) aRows
    let bOnlyRows = filter (\row -> trName row `notElem` aFields) bRows

    coincidentRows' <- for coincidentRows $ \(lhs, rhs) -> do
        mut <- unifyRecordMutability (trName lhs) pos (trMut lhs) (trMut rhs)
        unify env pos (trTyVar lhs) (trTyVar rhs)
        return lhs { trMut = mut }

    aOnlyRows' <- for aOnlyRows $ unifyFieldConstraint env pos constraintB
    bOnlyRows' <- for bOnlyRows $ unifyFieldConstraint env pos constraintA
    newConstraint <- case (constraintA, constraintB) of
        (Nothing, Nothing) -> do
            return Nothing
        (Just a, Nothing) -> do
            return $ Just a
        (Nothing, Just b) -> do
            return $ Just b
        (Just a, Just b) -> do
            unify env pos a b
            return $ Just a

    return $ RecordConstraint
        { rcFields = coincidentRows' <> aOnlyRows' <> bOnlyRows'
        , rcFieldType = newConstraint
        }

unifyConstraints :: Env -> Pos -> ConstraintSet -> ConstraintSet -> TC ConstraintSet
unifyConstraints env pos (ConstraintSet recordA traitsA) (ConstraintSet recordB traitsB) = do
    mergedRecord <- case (recordA, recordB) of
        (Nothing, Nothing) -> return Nothing
        (Just a, Nothing) -> return $ Just a
        (Nothing, Just b) -> return $ Just b
        (Just a, Just b) -> Just <$> unifyRecordConstraint env pos a b

    return $ ConstraintSet mergedRecord $ traitsA <> traitsB

validateConstraintSet :: Env -> Pos -> ConstraintSet -> TypeVar -> TC ()
validateConstraintSet env pos (ConstraintSet record traits) typeVar = do
    for_ record $ \recordConstraint -> do
        validateRecordConstraint env pos recordConstraint typeVar
    for_ (Set.toList traits) $ \traitIdentity@(TraitIdentity moduleName name) -> do
        (_, _, traitDesc) <- resolveTraitReference env pos $ KnownReference moduleName name
        validateTraitConstraint env pos traitIdentity traitDesc typeVar

unify :: Env -> Pos -> TypeVar -> TypeVar -> TC ()
unify env pos av' bv' = do
    av <- followTypeVar av'
    bv <- followTypeVar bv'
    if av == bv then
        return ()
    else case (av, bv) of
        -- thanks to followTypeVar, the only TypeVar case here is TUnbound
        (TypeVar aref, TypeVar bref) -> do
            (TUnbound strengthA levelA constraintsA numberA) <- readIORef aref
            (TUnbound strengthB levelB constraintsB numberB) <- readIORef bref

            when (numberA /= numberB) $ do
                occurs pos numberA bv
                writeIORef aref $ TBound bv
                let strength = unifyStrength strengthA strengthB
                let level = unifyLevel levelA levelB
                constraints <- unifyConstraints env pos constraintsA constraintsB
                writeIORef bref $ TUnbound strength level constraints numberB
        (TypeVar _, _) -> do
            -- flip around so we only have to handle one case
            -- TODO: fix the error messages
            unify env pos bv av
        (_, TypeVar bref) -> do
            TUnbound _ _ constraintsB bNumber <- readIORef bref
            validateConstraintSet env pos constraintsB av
            occurs pos bNumber av
            writeIORef bref $ TBound av

        (TDataType ad, TDataType bd)
            | dataTypeIdentity ad == dataTypeIdentity bd -> do
                -- TODO: assert the two lists have the same length
                for_ (zip (tuParameters ad) (tuParameters bd)) $ uncurry $ unify env pos
            | otherwise -> do
                unificationError pos "" av bv

        (TRecord fieldsA, TRecord fieldsB) ->
            unifyConcreteRecord env pos av bv fieldsA fieldsB

        (TFun aa ar, TFun ba br) -> do
            when (length aa /= length ba) $
                unificationError pos "" av bv

            for_ (zip aa ba) $ uncurry $ unify env pos
            unify env pos ar br

        (TQuant _ cI i, TQuant _ cJ j) | (cI, i) == (cJ, j) ->
            return ()

        _ ->
            unificationError pos "" av bv
