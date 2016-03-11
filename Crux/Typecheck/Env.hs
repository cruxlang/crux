{-# LANGUAGE TupleSections #-}

module Crux.Typecheck.Env
    ( ResolvePolicy (..)
    , newEnv
    , childEnv
    , buildTypeEnvironment
    , resolveTypeIdent
    , exportedDecls
    , getAllExportedValues
    , findExportedValueByName
    , getAllExportedTypes
    , findExportedTypeByName
    , getAllExportedPatterns
    , findExportedPatternByName

    , resolveValueReference
    , resolveTypeReference
    , resolveBooleanType
    , resolveArrayType
    ) where

import           Crux.AST
import qualified Crux.Error as Error
import           Crux.Intrinsic        (Intrinsic (..))
import qualified Crux.Intrinsic        as Intrinsic
import qualified Crux.MutableHashTable as HashTable
import           Crux.Prelude
import           Crux.Text             (isCapitalized)
import           Crux.Typecheck.Types
import Data.Maybe (catMaybes)
import           Crux.Typecheck.Unify
import qualified Data.HashMap.Strict   as HashMap
import           Prelude               hiding (String)
import           Text.Printf           (printf)
import Crux.Util
import Crux.TypeVar
import Crux.Module.Types
import Crux.Typecheck.Monad

data ResolvePolicy = NewTypesAreErrors | NewTypesAreQuantified
    deriving (Eq)

newEnv :: MonadIO m => ModuleName -> HashMap ModuleName LoadedModule -> Maybe TypeVar -> m Env
newEnv eThisModule eLoadedModules eReturnType = do
    eNextTypeIndex <- newIORef 0
    eValueBindings <- newIORef HashMap.empty
    eTypeBindings <- newIORef HashMap.empty
    ePatternBindings <- newIORef HashMap.empty
    eExceptionBindings <- newIORef HashMap.empty
    return Env
        { eInLoop = False
        , eLevel = 1
        , ..
        }

childEnv :: MonadIO m => Env -> m Env
childEnv env@Env{..} = do
    valueBindings' <- HashTable.clone eValueBindings
    typeBindings <- HashTable.clone eTypeBindings
    return env
        { eValueBindings = valueBindings'
        , eTypeBindings = typeBindings
        , eLevel = eLevel + 1
        }

exportedDecls :: [Declaration a b] -> [DeclarationType a b]
exportedDecls decls = [dt | (Declaration Export _ dt) <- decls]

resolveTypeIdentifier :: Env -> TypeIdentifier -> TC (Name, Maybe TypeReference)
resolveTypeIdentifier env = \case
    UnqualifiedType name -> do
        (name,) <$> HashTable.lookup name (eTypeBindings env)
    QualifiedType importName name -> do
        maybeTypeVar <- HashTable.lookup importName (eValueBindings env) >>= \case
            Nothing -> fail "TODO: qualified type references unknown import"
            Just (ModuleReference moduleName) -> do
                return $ fmap snd $ findExportedTypeByName env moduleName name
            Just _ -> do
                fail "TODO: qualified type is not a module"
        return (name, fmap TypeReference maybeTypeVar)

-- TODO: move into TC so errors are recorded properly
resolveTypeIdent :: Env -> Pos -> ResolvePolicy -> TypeIdent -> TC TypeVar
resolveTypeIdent env@Env{..} pos resolvePolicy typeIdent =
    go typeIdent
  where
    go :: TypeIdent -> TC TypeVar
    go UnitTypeIdent = do
        return $ TPrimitive Unit

    go (TypeIdent typeName typeParameters) = do
        resolveTypeIdentifier env typeName >>= \case
            (_, Just (TypeReference ty)) -> do
                followTypeVar ty >>= \case
                    TPrimitive {}
                        | [] == typeParameters ->
                            return ty
                        | otherwise ->
                            error "Primitive types don't take type parameters"
                    TUserType def@TUserTypeDef{tuParameters} _
                        | length tuParameters == length typeParameters -> do
                            params <- for typeParameters go
                            return $ TUserType def params
                        | otherwise ->
                            fail $ printf "Type %s takes %i type parameters.  %i given" (show $ tuName def) (length tuParameters) (length typeParameters)
                    TTypeFun tuParameters _rt
                        | length tuParameters == length typeParameters -> do
                            (TTypeFun tuParameters' rt') <- instantiate env ty
                            for_ (zip tuParameters' typeParameters) $ \(a, b) -> do
                                b' <- resolveTypeIdent env pos NewTypesAreErrors b
                                unify pos a b'
                            return rt'
                        | otherwise -> do
                            fail $ printf "Type %s takes %i type parameters.  %i given" (show $ typeName) (length tuParameters) (length typeParameters)
                    _ ->
                        return ty

            (typeName', Nothing) | NewTypesAreQuantified == resolvePolicy && not (isCapitalized typeName') -> do
                tyVar <- freshType env
                quantify tyVar

                HashTable.insert typeName' (TypeReference tyVar) eTypeBindings
                return tyVar
            (typeName', Nothing) ->
                failTypeError pos $ Error.UnboundType typeName'

    go (RecordIdent rows) = do
        rows' <- for rows $ \(trName, mut, rowTypeIdent) -> do
            let trMut = case mut of
                    Nothing -> RFree
                    Just Mutable -> RMutable
                    Just Immutable -> RImmutable
            trTyVar <- go rowTypeIdent
            return TypeRow{..}
        ref <- newIORef $ RRecord $ RecordType RecordClose rows'
        return $ TRecord $ ref

    go (FunctionIdent argTypes retPrimitive) = do
        argTypes' <- for argTypes go
        retPrimitive' <- go retPrimitive
        return $ TFun argTypes' retPrimitive'

    go (ArrayIdent mutability elementType) = do
        elementType' <- go elementType
        (arrayType, elementType'') <- resolveArrayType env pos mutability
        unify pos elementType' elementType''
        return arrayType

resolveTypeReference :: Pos -> Env -> UnresolvedReference -> TC TypeVar
resolveTypeReference pos env ref@(UnqualifiedReference name) = do
    HashTable.lookup name (eTypeBindings env) >>= \case
        Just (TypeReference t) -> return t
        Nothing -> failTypeError pos $ Error.UnboundSymbol ref
resolveTypeReference pos env (KnownReference moduleName name) = do
    if moduleName == eThisModule env then do
        resolveTypeReference pos env (UnqualifiedReference name)
    else do
        case findExportedTypeByName env moduleName name of
            Just (_, tv) -> return tv
            Nothing -> failTypeError pos $ Error.ModuleReferenceError moduleName name

resolveValueReference :: Pos -> Env -> UnresolvedReference -> TC (Maybe (ResolvedReference, Mutability, TypeVar))
resolveValueReference pos env ref = case ref of
    UnqualifiedReference name -> do
        result <- HashTable.lookup name (eValueBindings env)
        return $ case result of
            Just (ValueReference rr mut t) -> Just (rr, mut, t)
            _ -> Nothing
    KnownReference moduleName name -> do
        -- TODO: better error messages
        case findExportedValueByName env moduleName name of
            Just (rr, mutability, typevar) ->
                return $ Just (rr, mutability, typevar)
            Nothing -> failTypeError pos $ Error.ModuleReferenceError moduleName name

resolveArrayType :: Env -> Pos -> Mutability -> TC (TypeVar, TypeVar)
resolveArrayType env pos mutability = do
    elementType <- case mutability of
            Immutable -> freshType env
            Mutable -> freshWeakQVar env

    let typeReference = case mutability of
            Immutable -> KnownReference "array" "Array"
            Mutable -> KnownReference "mutarray" "MutableArray"
    arrayType <- resolveTypeReference pos env typeReference
    followTypeVar arrayType >>= \case
        TUserType td [_elementType] -> do
            let newArrayType = TUserType td [elementType]
            return (newArrayType, elementType)
        _ -> fail "Unexpected Array type"

resolveBooleanType :: Pos -> Env -> TC TypeVar
resolveBooleanType pos env = do
    resolveTypeReference pos env (KnownReference "boolean" "Boolean")

-- TODO: what do we do with this when Variants know their own types
createUserTypeDef :: Env
                  -> Name
                  -> ModuleName
                  -> [TypeVar]
                  -> [Variant _edata]
                  -> TC (TUserTypeDef TypeVar, TypeVar)
createUserTypeDef env name moduleName typeVars variants = do
    variants' <- for variants $ \(Variant _typeVar vname vparameters) -> do
        tvParameters <- for vparameters $ const $ freshType env
        let tvName = vname
        return TVariant {..}

    let typeDef = TUserTypeDef
            { tuName = name
            , tuModuleName = moduleName
            , tuParameters = typeVars
            , tuVariants = variants'
            }

    let tyVar = TUserType typeDef typeVars
    return (typeDef, tyVar)

buildTypeEnvironment :: ModuleName -> HashMap ModuleName LoadedModule -> Module UnresolvedReference Pos -> TC Env
buildTypeEnvironment thisModuleName loadedModules thisModule = do
    let imports = mImports thisModule

    -- built-in types. would be nice to move into the prelude somehow.
    let numTy = TPrimitive Number
    let strTy = TPrimitive String
    env <- newEnv thisModuleName loadedModules Nothing

    HashTable.insert "Number" (TypeReference numTy) (eTypeBindings env)
    HashTable.insert "String" (TypeReference strTy) (eTypeBindings env)

    for_ (HashMap.toList Intrinsic.intrinsics) $ \(name, intrin) -> do
        let Intrinsic{..} = intrin
        HashTable.insert name (ValueReference (Builtin name) Immutable iType) (eValueBindings env)

    for_ imports $ \case
        (pos, UnqualifiedImport importName) -> do
            importedModule <- case HashMap.lookup importName loadedModules of
                Just im -> return im
                Nothing -> failICE $ Error.DependentModuleNotLoaded pos importName

            -- populate types
            for_ (getAllExportedTypes $ importedModule) $ \(name, typeVar) -> do
                HashTable.insert name (TypeReference typeVar) (eTypeBindings env)

            -- populate values
            for_ (getAllExportedValues $ importedModule) $ \(name, mutability, tr) -> do
                HashTable.insert name (ValueReference (OtherModule importName name) mutability tr) (eValueBindings env)

            -- populate patterns
            for_ (getAllExportedPatterns $ importedModule) $ \(name, pb) -> do
                HashTable.insert name pb (ePatternBindings env)

        (_pos, QualifiedImport moduleName importName) -> do
            HashTable.insert importName (ModuleReference moduleName) (eValueBindings env)

    addThisModuleDataDeclsToEnvironment env thisModule

    return env

getAllExportedValues :: LoadedModule -> [(Name, Mutability, TypeVar)]
getAllExportedValues loadedModule = mconcat $ (flip fmap $ exportedDecls $ mDecls loadedModule) $ \case
    DDeclare typeVar name _typeIdent -> [(name, Immutable, typeVar)]
    -- TODO: support trickier patterns, like export let (x, y) = (1, 2)
    DLet typeVar mutability binding _ _ -> case binding of
        PBinding name -> [(name, mutability, typeVar)]
        PWildcard -> []
    DFun typeVar name _ _ _ -> [(name, Immutable, typeVar)]
    DData _ _ _ _ variants -> fmap (\(Variant typeVar name _) -> (name, Immutable, typeVar)) variants
    DJSData typeVar _ _ variants -> fmap (\(JSVariant name _) -> (name, Immutable, typeVar)) variants
    DTypeAlias _ _ _ _ -> []
    DException _ _ _ -> []

findExportedValueByName :: Env -> ModuleName -> Name -> Maybe (ResolvedReference, Mutability, TypeVar)
findExportedValueByName env moduleName valueName = do
    modul <- HashMap.lookup moduleName (eLoadedModules env)
    (typeVar, mutability) <- findFirstOf (getAllExportedValues modul) $ \(name, mutability, typeVar) ->
        if name == valueName then
            Just (typeVar, mutability)
        else
            Nothing
    return (OtherModule moduleName valueName, mutability, typeVar)

getAllExportedTypes :: LoadedModule -> [(Name, TypeVar)]
getAllExportedTypes loadedModule = mconcat $ (flip fmap $ exportedDecls $ mDecls loadedModule) $ \case
    DDeclare {} -> []
    DLet {} -> []
    DFun {} -> []
    DData typeVar name _ _ _ -> [(name, typeVar)]
    DJSData typeVar name _ _ -> [(name, typeVar)]
    DTypeAlias typeVar name _ _ -> [(name, typeVar)]
    DException _ _ _ -> []

findExportedTypeByName :: Env -> ModuleName -> Name -> Maybe (ResolvedReference, TypeVar)
findExportedTypeByName env moduleName typeName = do
    modul <- HashMap.lookup moduleName (eLoadedModules env)
    typeVar <- findFirstOf (getAllExportedTypes modul) $ \(name, typeVar) ->
        if name == typeName then
            Just typeVar
        else
            Nothing
    return (OtherModule moduleName typeName, typeVar)

-- we can just use PatternBinding for now
getAllExportedPatterns :: LoadedModule -> [(Name, PatternBinding)]
getAllExportedPatterns loadedModule = mconcat $ (flip fmap $ exportedDecls $ mDecls loadedModule) $ \case
    DDeclare {} -> []
    DLet {} -> []
    DFun {} -> []
    DData typeVar _name _ _ variants ->
        let TUserType def typeVars = typeVar in
        (flip fmap) variants $ \(Variant _vtype name _typeIdent) ->
            (name, PatternBinding def typeVars)

    DJSData typeVar _name _ jsVariants ->
        let (TUserType def _) = typeVar in
        (flip fmap) jsVariants $ \(JSVariant name _literal) ->
            (name, PatternBinding def [])

    DTypeAlias _ _ _ _ -> []
    DException _ _ _ -> []

findExportedPatternByName :: Env -> ModuleName -> Name -> Maybe PatternBinding
findExportedPatternByName env moduleName patternName = do
    modul <- HashMap.lookup moduleName (eLoadedModules env)
    findFirstOf (getAllExportedPatterns modul) $ \(name, binding) ->
        if name == patternName then
            Just binding
        else
            Nothing

-- Phase 2a
registerJSFFIDecl :: Env -> DeclarationType UnresolvedReference Pos -> TC ()
registerJSFFIDecl env = \case
    DDeclare {} -> return ()
    DLet {} -> return ()
    DFun {} -> return ()

    DData {} -> return ()
    DJSData _pos name moduleName variants -> do
        -- jsffi data never has type parameters, so we can just blast through the whole thing in one pass
        variants' <- for variants $ \(JSVariant variantName _value) -> do
            let tvParameters = []
            let tvName = variantName
            return TVariant{..}

        let typeDef = TUserTypeDef
                { tuName = name
                , tuModuleName = moduleName
                , tuParameters = []
                , tuVariants = variants'
                }
        let userType = TUserType typeDef []
        HashTable.insert name (TypeReference userType) (eTypeBindings env)

        for_ variants $ \(JSVariant variantName _value) -> do
            HashTable.insert variantName (ValueReference (Local variantName) Immutable userType) (eValueBindings env)
            HashTable.insert variantName (PatternBinding typeDef []) (ePatternBindings env)
        return ()
    DTypeAlias {} -> return ()
    DException {} -> return ()

registerExceptionDecl :: Env -> DeclarationType UnresolvedReference Pos -> TC ()
registerExceptionDecl env = \case
    DDeclare {} -> return ()
    DLet {} -> return ()
    DFun {} -> return ()

    DData {} -> return ()
    DJSData {} -> return ()
    DTypeAlias {} -> return ()
    DException pos exceptionName typeIdent -> do
        tyVar <- resolveTypeIdent env pos NewTypesAreErrors typeIdent
        HashTable.insert exceptionName tyVar (eExceptionBindings env)
        return ()

addThisModuleDataDeclsToEnvironment
    :: Env
    -> Module UnresolvedReference Pos
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
        b. register all data types (and only the types)
        c. register all type aliases
        d. register all data type constructors and patterns (using same qvars from before)

    phase 3:
        register all exceptions

    phase 4:
        type check all values in order
    -}

    let decls = [decl | Declaration _ _ decl <- mDecls thisModule]

    -- Phase 2a
    for_ decls $ \decl -> do
        registerJSFFIDecl env decl

    -- Phase 2b.
    dataDecls <- fmap catMaybes $ for decls $ \case
        DData pos name moduleName typeVarNames variants ->
            return $ Just (pos, name, moduleName, typeVarNames, variants)
        _ ->
            return Nothing

    dataDecls' <- for dataDecls $ \(pos, typeName, moduleName, typeVarNames, variants) -> do
        e <- childEnv env
        tyVars <- for typeVarNames $ \tvName ->
            resolveTypeIdent e pos NewTypesAreQuantified (TypeIdent (UnqualifiedType tvName) [])

        (typeDef, tyVar) <- createUserTypeDef e typeName moduleName tyVars variants
        HashTable.insert typeName (TypeReference tyVar) (eTypeBindings env)

        let qvars = zip typeVarNames tyVars
        return (pos, typeDef, tyVar, qvars, variants)

    -- Phase 2c.
    aliasDecls <- fmap catMaybes $ for decls $ \case
        DTypeAlias pos name params ident -> do
            typeVar <- freshType env
            HashTable.insert name (TypeReference typeVar) $ eTypeBindings env
            return $ Just (pos, params, ident, typeVar)
        _ -> do
            return Nothing

    for_ aliasDecls $ \(pos, params, ident, typeVar) -> do
        env' <- childEnv env
        for_ params $ \tvName -> do
            tv <- freshType env'
            quantify tv
            HashTable.insert tvName (TypeReference tv) $ eTypeBindings env'
        resolvedType <- resolveTypeIdent env' pos NewTypesAreErrors ident
        unify pos typeVar resolvedType

    for_ decls $ \case
        DJSData {} -> return ()
        DTypeAlias pos name params ident -> do
            -- TODO: This feels buggy to me somehow.  -chad
            env' <- childEnv env
            for_ params $ \tvName -> do
                tv <- freshType env'
                quantify tv
                HashTable.insert tvName (TypeReference tv) $ eTypeBindings env'
            abc <- resolveTypeIdent env' pos NewTypesAreErrors ident
            HashTable.insert name (TypeReference abc) (eTypeBindings env)
        DDeclare {} -> return ()
        DData {}    -> return ()
        DFun {}     -> return ()
        DLet {}     -> return ()
        DException {} -> return ()

    -- Phase 2d.
    for_ dataDecls' $ \(pos, typeDef, tyVar, qvars, variants) -> do
        e <- childEnv env
        for_ qvars $ \(qvName, qvTypeVar) ->
            HashTable.insert qvName (TypeReference qvTypeVar) (eTypeBindings e)

        let computeVariantType [] = tyVar
            computeVariantType argTypeIdents = TFun argTypeIdents tyVar

        for_ variants $ \(Variant _typeVar vname vparameters) -> do
            parameterTypeVars <- traverse (resolveTypeIdent e pos NewTypesAreErrors) vparameters
            let ctorType = computeVariantType parameterTypeVars
            HashTable.insert vname (ValueReference (ThisModule vname) Immutable ctorType) (eValueBindings env)
            HashTable.insert vname (PatternBinding typeDef (fmap snd qvars)) (ePatternBindings env)

    -- Phase 3.
    for_ decls $ \decl -> do
        registerExceptionDecl env decl
