{-# LANGUAGE TupleSections #-}

module Crux.Typecheck.Env
    ( newEnv
    , childEnv
    , buildTypeEnvironment
    , registerTypeVarIdent
    , registerExplicitTypeVariables
    , newQuantifiedTypeVar
    , newQuantifiedConstrainedTypeVar
    , resolveTypeIdent
    , exportedDecls

    , resolveValueReference
    , resolveTypeReference
    , resolveBooleanType
    , resolveStringType
    , resolveArrayType
    , resolveNumberType
    , resolveVoidType
    , resolvePatternReference
    , resolveTraitReference
    , resolveExceptionReference
    ) where

import Crux.AST
import Crux.ModuleName
import qualified Crux.Error as Error
import Crux.Module.Types
import Crux.Prelude
import Crux.Pos (Pos)
import Crux.Typecheck.Monad
import Crux.Typecheck.TypeAlloc
import Crux.Typecheck.Types
import Crux.Typecheck.Unify
import Crux.Typecheck.Quantify
import Crux.TypeVar
import Data.Maybe (isJust)
import qualified Data.HashMap.Strict as HashMap
import Prelude hiding (String)
import qualified Crux.SymbolTable as SymbolTable
import qualified Crux.HashTable as HashTable
import qualified Data.Set as Set

newEnv :: MonadIO m => ModuleName -> HashMap ModuleName LoadedModule -> Maybe TypeVar -> m Env
newEnv eThisModule eLoadedModules eReturnType = do
    eNextTypeIndex <- newIORef 0

    eValueBindings <- SymbolTable.new
    eTypeBindings <- SymbolTable.new
    ePatternBindings <- SymbolTable.new
    eTraitBindings <- SymbolTable.new
    eExceptionBindings <- SymbolTable.new

    eKnownInstances <- HashTable.new

    eExportedValues <- SymbolTable.new
    eExportedTypes <- SymbolTable.new
    eExportedPatterns <- SymbolTable.new
    eExportedTraits <- SymbolTable.new
    eExportedExceptions <- SymbolTable.new
 
    return Env
        { eInLoop = False
        , eLevel = 1
        , ..
        }

childEnv :: MonadIO m => Env -> m Env
childEnv env@Env{..} = do
    valueBindings <- SymbolTable.clone eValueBindings
    typeBindings <- SymbolTable.clone eTypeBindings
    patternBindings <- SymbolTable.clone ePatternBindings
    exceptionBindings <- SymbolTable.clone eExceptionBindings
    return env
        { eValueBindings = valueBindings
        , eTypeBindings = typeBindings
        , ePatternBindings = patternBindings
        , eExceptionBindings = exceptionBindings
        , eLevel = eLevel + 1
        }

-- TypeIdent Resolution

exportedDecls :: [Declaration a b c] -> [DeclarationType a b c]
exportedDecls decls = [dt | (Declaration Export _ dt) <- decls]

resolveTypeIdent :: Env -> Pos -> TypeIdent -> TC TypeVar
resolveTypeIdent env@Env{..} pos typeIdent =
    go typeIdent
  where
    go :: TypeIdent -> TC TypeVar
    go (TupleTypeIdent elements) = case elements of
        [] -> resolveVoidType env pos
        _ -> do
            elements' <- for elements $ resolveTypeIdent env pos
            resolveTupleType env pos elements'
    
    go (TypeIdent typeName typeArguments) = do
        ty <- resolveTypeReference env pos typeName
        typeArguments' <- for typeArguments $ resolveTypeIdent env pos
        applyTypeFunction env pos typeName AllowTypeFunctions ty typeArguments'

    go (RecordIdent fields) = do
        fields' <- for fields $ \(trName, mut, rowTypeIdent) -> do
            let trMut = case mut of
                    Nothing -> RFree
                    Just Mutable -> RMutable
                    Just Immutable -> RImmutable
            trTyVar <- go rowTypeIdent
            return RecordField{..}
        return $ TRecord fields'

    go (FunctionIdent argTypes retPrimitive) = do
        argTypes' <- for argTypes go
        retPrimitive' <- go retPrimitive
        return $ TFun argTypes' retPrimitive'

    go (ArrayIdent mutability elementType) = do
        elementType' <- go elementType
        (arrayType, elementType'') <- resolveArrayType env pos mutability
        unify env pos elementType' elementType''
        return arrayType

    go (OptionIdent elementType) = do
        elementType' <- go elementType
        (optionType, elementType'') <- resolveOptionType env pos
        unify env pos elementType' elementType''
        return optionType

    go WildcardIdent = do
        freshType env

registerTypeVarIdent :: Env -> TypeVarIdent -> TC TypeVar
registerTypeVarIdent env (TypeVarIdent name pos (ConstraintSetIdent recordConstraint traits)) = do
    recordConstraint' <- for recordConstraint $ \(RecordConstraintIdent fields fieldType) -> do
        fields' <- for fields $ \(fname, ident) -> do
            tyVar <- resolveTypeIdent env pos ident
            return RecordField
                { trName = fname
                , trMut = RImmutable
                , trTyVar = tyVar
                }
        fieldType' <- for fieldType $ resolveTypeIdent env pos
        return $ RecordConstraint
            { rcFields = fields'
            , rcFieldType = fieldType'
            }

    traitIdentities <- for traits $ \traitName -> do
        (_, traitIdentity, _) <- resolveTraitReference env pos traitName
        return traitIdentity
    -- TODO: eliminate duplicates? at least warn
    let constraints = ConstraintSet recordConstraint' $ Set.fromList traitIdentities
    tyVar <- freshTypeConstrained env constraints
    quantify tyVar

    SymbolTable.insert (eTypeBindings env) pos SymbolTable.DisallowDuplicates name tyVar
    return tyVar

registerExplicitTypeVariables :: Env -> [TypeVarIdent] -> TC [TypeVar]
registerExplicitTypeVariables env forall' = do
    for forall' $ registerTypeVarIdent env
    
newQuantifiedTypeVar :: Env -> Pos -> Name -> TC TypeVar
newQuantifiedTypeVar env pos name = do
    tyVar <- freshType env
    quantify tyVar

    SymbolTable.insert (eTypeBindings env) pos SymbolTable.DisallowDuplicates name tyVar
    return tyVar

newQuantifiedConstrainedTypeVar :: Env -> Pos -> Name -> TraitIdentity -> TC TypeVar
newQuantifiedConstrainedTypeVar env pos name traitIdentity = do
    let constraints = ConstraintSet Nothing $ Set.singleton traitIdentity
    tyVar <- freshTypeConstrained env constraints
    quantify tyVar

    SymbolTable.insert (eTypeBindings env) pos SymbolTable.DisallowDuplicates name tyVar
    return tyVar

-- TODO: what do we do with this when Variants know their own types
createUserTypeDef :: Env
                  -> Name
                  -> ModuleName
                  -> [TypeVar]
                  -> [Variant _tagtype _edata]
                  -> TC (TDataTypeDef TypeVar)
createUserTypeDef env name moduleName typeVars variants = do
    variants' <- for variants $ \(Variant _tag _typeVar vname vparameters) -> do
        -- the variant parameters are unified with the corresponding typeidents later
        tvParameters <- for vparameters $ const $ freshType env
        let tvName = vname
        return TVariant {..}

    return $ TDataTypeDef
        { tuName = name
        , tuModuleName = moduleName
        , tuParameters = typeVars
        , tuVariants = variants'
        }

buildTypeEnvironment :: ModuleName -> HashMap ModuleName LoadedModule -> Module UnresolvedReference () Pos -> TC Env
buildTypeEnvironment thisModuleName loadedModules thisModule = do
    let imports = mImports thisModule

    -- built-in types. would be nice to move into the prelude somehow.
    env <- newEnv thisModuleName loadedModules Nothing

    -- Phase 1
    for_ imports $ \(pos, Import moduleName importType) -> do
        importedModule <- case HashMap.lookup moduleName loadedModules of
            Just im -> return im
            Nothing -> failICE pos $ Error.DependentModuleNotLoaded moduleName

        for_ (HashMap.toList $ lmKnownInstances importedModule) $ \((a, b), c) -> do
            HashTable.insert (a, b) c $ eKnownInstances env

        case importType of
            UnqualifiedImport -> do
                -- populate types
                for_ (lmExportedTypes importedModule) $ \(name, typeVar) -> do
                    SymbolTable.insert (eTypeBindings env) pos SymbolTable.DisallowDuplicates name typeVar

                -- populate values
                for_ (lmExportedValues importedModule) $ \(name, (resolvedReference, mutability, tr)) -> do
                    SymbolTable.insert (eValueBindings env) pos SymbolTable.DisallowDuplicates name (ValueReference resolvedReference mutability tr)

                -- populate patterns
                for_ (lmExportedPatterns importedModule) $ \(name, pb) -> do
                    SymbolTable.insert (ePatternBindings env) pos SymbolTable.DisallowDuplicates name pb

                -- populate traits
                for_ (lmExportedTraits importedModule) $ \(name, trait) -> do
                    SymbolTable.insert (eTraitBindings env) pos SymbolTable.DisallowDuplicates name trait

                -- populate exceptions
                for_ (lmExportedExceptions importedModule) $ \(name, exc) -> do
                    SymbolTable.insert (eExceptionBindings env) pos SymbolTable.DisallowDuplicates name exc

            SelectiveImport names -> do
                -- populate types
                for_ (lmExportedTypes importedModule) $ \(name, typeVar) -> do
                    when (name `elem` names) $ do
                        SymbolTable.insert (eTypeBindings env) pos SymbolTable.DisallowDuplicates name typeVar

                -- populate values
                for_ (lmExportedValues importedModule) $ \(name, (resolvedReference, mutability, tr)) -> do
                    when (name `elem` names) $ do
                        SymbolTable.insert (eValueBindings env) pos SymbolTable.DisallowDuplicates name (ValueReference resolvedReference mutability tr)

                -- populate patterns
                for_ (lmExportedPatterns importedModule) $ \(name, pb) -> do
                    when (name `elem` names) $ do
                        SymbolTable.insert (ePatternBindings env) pos SymbolTable.DisallowDuplicates name pb

                -- populate traits
                for_ (lmExportedTraits importedModule) $ \(name, trait) -> do
                    when (name `elem` names) $ do
                        SymbolTable.insert (eTraitBindings env) pos SymbolTable.DisallowDuplicates name trait

                -- populate exceptions
                for_ (lmExportedExceptions importedModule) $ \(name, exc) -> do
                    when (name `elem` names) $ do
                        SymbolTable.insert (eExceptionBindings env) pos SymbolTable.DisallowDuplicates name exc

            QualifiedImport importName -> do
                for_ importName $ \importName' -> do
                    SymbolTable.insert (eValueBindings env) pos SymbolTable.DisallowDuplicates importName' (ModuleReference moduleName)

    addThisModuleDataDeclsToEnvironment env thisModule

    return env

-- Phase 2a
registerJSFFIDecl :: Env -> DeclarationType UnresolvedReference () Pos -> TC ()
registerJSFFIDecl env = \case
    DExportImport {} -> return ()

    DDeclare {} -> return ()
    DLet {} -> return ()
    DFun {} -> return ()

    DData {} -> return ()
    DJSData pos name variants -> do
        -- jsffi data never has type parameters, so we can just blast through the whole thing in one pass
        variants' <- for variants $ \(JSVariant variantName _value) -> do
            let tvParameters = []
            let tvName = variantName
            return TVariant{..}

        let typeDef = TDataTypeDef
                { tuName = name
                , tuModuleName = eThisModule env
                , tuParameters = []
                , tuVariants = variants'
                }
        let userType = TDataType typeDef
        SymbolTable.insert (eTypeBindings env) pos SymbolTable.DisallowDuplicates name userType

        for_ variants $ \(JSVariant variantName value) -> do
            SymbolTable.insert (eValueBindings env) pos SymbolTable.DisallowDuplicates variantName (ValueReference (Local, variantName) Immutable userType)
            SymbolTable.insert (ePatternBindings env) pos SymbolTable.DisallowDuplicates variantName (PatternReference typeDef $ TagLiteral value)
        return ()
    DTypeAlias {} -> return ()

    DTrait {} -> return ()
    DImpl {} -> return ()

    DException {} -> return ()

registerExceptionDecl :: Env -> DeclarationType UnresolvedReference () Pos -> TC ()
registerExceptionDecl env = \case
    DExportImport {} -> return ()

    DDeclare {} -> return ()
    DLet {} -> return ()
    DFun {} -> return ()

    DData {} -> return ()
    DJSData {} -> return ()
    DTypeAlias {} -> return ()

    DTrait {} -> return ()
    DImpl {} -> return ()

    DException pos exceptionName typeIdent -> do
        tyVar <- resolveTypeIdent env pos typeIdent
        SymbolTable.insert (eExceptionBindings env) pos SymbolTable.DisallowDuplicates exceptionName ((FromModule $ eThisModule env, exceptionName), tyVar)
        return ()

addThisModuleDataDeclsToEnvironment
    :: Env
    -> Module UnresolvedReference () Pos
    -> TC ()
addThisModuleDataDeclsToEnvironment env thisModule = do
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
        b. register all data types (and only the types) and traits (but not their bodies)
        c. register all type aliases
        d. register all data type constructors and patterns (using same qvars from before)

    phase 3:
        register all exceptions

    phase 4:
        a. register all trait definitions
        b. type check all values and impls in order
    -}

    let decls = [decl | Declaration _ _ decl <- mDecls thisModule]

    -- Phase 2a
    for_ decls $ \decl -> do
        registerJSFFIDecl env decl

    dataDecls' <- newIORef []
    traitDecls' <- newIORef []

    -- Phase 2b
    for_ decls $ \case
        DData pos typeName typeVarNames variants -> do
            e <- childEnv env
            tyVars <- registerExplicitTypeVariables e typeVarNames

            typeDef <- createUserTypeDef e typeName (eThisModule env) tyVars variants
            let tyVar = TDataType typeDef
            let typeRef = case tyVars of
                    [] -> tyVar
                    _ -> TTypeFun tyVars tyVar
            SymbolTable.insert (eTypeBindings env) pos SymbolTable.DisallowDuplicates typeName typeRef

            let qvars = zip typeVarNames tyVars
            modifyIORef dataDecls' ((pos, typeDef, tyVar, qvars, variants):)

        DTrait pos name defns -> do
            env' <- childEnv env

            let tn = TraitIdentity (eThisModule env) name

            typeVar <- newQuantifiedConstrainedTypeVar env' pos name tn
            let typeVarName = "self" -- a la Rust
            SymbolTable.insert (eTypeBindings env') pos SymbolTable.DisallowDuplicates typeVarName typeVar
            methods <- for defns $ \(mname, mpos, typeIdent, maybeExpr) -> do
                tv <- resolveTypeIdent env' mpos typeIdent
                return (mname, mpos, tv, isJust maybeExpr)

            let desc = TraitDesc
                    { tdName = name
                    , tdModule = eThisModule env
                    , tdTypeVar = typeVar
                    , tdMethods = map (\(a, _, b, c) -> (a, b, c)) methods
                    }
            SymbolTable.insert (eTraitBindings env) pos SymbolTable.DisallowDuplicates name ((FromModule (eThisModule env), name), tn, desc)
            modifyIORef traitDecls' (methods:)

        _ -> do
            return ()

    dataDecls <- reverse <$> readIORef dataDecls'
    traitDecls <- reverse <$> readIORef traitDecls'

    -- Phase 2c.1.
    aliasDecls <- fmap catMaybes $ for decls $ \case
        DTypeAlias pos name params ident -> do
            bodyTypeVar <- freshType env

            paramTypes <- for params $ \tvName -> do
                tv <- freshType env
                quantify tv
                return (tvName, tv)

            let registeredType = case params of
                    [] -> bodyTypeVar
                    _ -> TTypeFun (fmap snd paramTypes) bodyTypeVar
            SymbolTable.insert (eTypeBindings env) pos SymbolTable.DisallowDuplicates name registeredType
            return $ Just (pos, paramTypes, ident, bodyTypeVar)
        _ -> do
            return Nothing

    -- Phase 2c.2.
    for_ aliasDecls $ \(pos, paramTypes, ident, bodyTypeVar) -> do
        env' <- childEnv env
        for_ paramTypes $ \(tvName, tv) -> do
            SymbolTable.insert (eTypeBindings env') pos SymbolTable.DisallowDuplicates tvName tv
        resolvedType <- resolveTypeIdent env' pos ident
        unify env pos bodyTypeVar resolvedType

    -- Phase 2d.
    for_ dataDecls $ \(pos, typeDef, tyVar, qvars, variants) -> do
        e <- childEnv env
        for_ qvars $ \(TypeVarIdent qvName _ _, qvTypeVar) ->
            SymbolTable.insert (eTypeBindings e) pos SymbolTable.DisallowDuplicates qvName qvTypeVar

        for_ (zip variants $ tuVariants typeDef) $ \(Variant () vpos _ typeIdents, TVariant _ typeVars) -> do
            for_ (zip typeIdents typeVars) $ \(typeIdent, typeVar) -> do
                tv' <- resolveTypeIdent e vpos typeIdent
                unify env vpos typeVar tv'

        let computeVariantType [] = tyVar
            computeVariantType argTypeIdents = TFun argTypeIdents tyVar

        for_ variants $ \(Variant () _typeVar vname vparameters) -> do
            parameterTypeVars <- traverse (resolveTypeIdent e pos) vparameters
            let ctorType = computeVariantType parameterTypeVars
            SymbolTable.insert
                (eValueBindings env)
                pos
                SymbolTable.DisallowDuplicates
                vname
                (ValueReference (FromModule $ eThisModule env, vname) Immutable ctorType)

        -- TOTAL HACK, NOT GENERALIZED AT ALL YET
        -- Detect the shape of Option and insert optimized tag checks
        -- and ensure that None is encoded with `null`
        case variants of
            [Variant () _ vname1 [], Variant () _ vname2 [_]] -> do
                SymbolTable.insert
                    (ePatternBindings env)
                    pos
                    SymbolTable.DisallowDuplicates
                    vname1
                    (PatternReference typeDef $ TagNullish)
                SymbolTable.insert
                    (ePatternBindings env)
                    pos
                    SymbolTable.DisallowDuplicates
                    vname2
                    (PatternReference typeDef $ TagNonNullish $ TagBoxedVariant vname2)
            _ -> do
                for_ variants $ \(Variant () _typeVar vname vparameters) -> do
                    let tag = case vparameters of
                            [] -> TagNamedVariant vname
                            _ -> TagBoxedVariant vname
                    SymbolTable.insert
                        (ePatternBindings env)
                        pos
                        SymbolTable.DisallowDuplicates
                        vname
                        (PatternReference typeDef tag)

    -- Phase 3.
    for_ decls $ \decl -> do
        registerExceptionDecl env decl

    -- Phase 4a.
    for_ traitDecls $ \defns -> do
        for_ defns $ \(defName, defPos, defTypeVar, _maybeExpr) -> do
            SymbolTable.insert
                (eValueBindings env)
                defPos
                SymbolTable.DisallowDuplicates
                defName
                (ValueReference (FromModule $ eThisModule env, defName) Immutable defTypeVar)
