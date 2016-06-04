{-# LANGUAGE TupleSections #-}

module Crux.Typecheck.Env
    ( ResolvePolicy (..)
    , newEnv
    , childEnv
    , buildTypeEnvironment
    , resolveTypeIdent
    , exportedDecls
    , findExportedPatternByName

    , resolveValueReference
    , resolveTypeReference
    , resolveBooleanType
    , resolveArrayType
    , resolvePatternReference
    , resolveExceptionReference
    ) where

import Crux.AST
import qualified Crux.Error as Error
import Crux.Intrinsic (Intrinsic (..))
import qualified Crux.Intrinsic as Intrinsic
import Crux.Module.Types
import Crux.Prelude
import Crux.Text (isCapitalized)
import Crux.Typecheck.Monad
import Crux.Typecheck.Types
import Crux.Typecheck.Unify
import Crux.TypeVar
import Crux.Util
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import Prelude hiding (String)
import qualified Crux.SymbolTable as SymbolTable

data ResolvePolicy = NewTypesAreErrors | NewTypesAreQuantified
    deriving (Eq)

newEnv :: MonadIO m => ModuleName -> HashMap ModuleName LoadedModule -> Maybe TypeVar -> m Env
newEnv eThisModule eLoadedModules eReturnType = do
    eNextTypeIndex <- newIORef 0
    eValueBindings <- SymbolTable.new
    eTypeBindings <- SymbolTable.new
    ePatternBindings <- SymbolTable.new
    eExceptionBindings <- SymbolTable.new
    eExportedValues <- SymbolTable.new
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

exportedDecls :: [Declaration a b c] -> [DeclarationType a b c]
exportedDecls decls = [dt | (Declaration Export _ dt) <- decls]

resolveTypeIdent :: Env -> Pos -> ResolvePolicy -> TypeIdent -> TC TypeVar
resolveTypeIdent env@Env{..} pos resolvePolicy typeIdent =
    go typeIdent
  where
    go :: TypeIdent -> TC TypeVar
    go UnitTypeIdent = do
        return $ TPrimitive Unit

    go (TypeIdent typeName typeParameters) = do
        ty <- resolveTypeReference env pos resolvePolicy typeName >>= followTypeVar
        case ty of
            TPrimitive pt
                | [] == typeParameters -> do
                    return ty
                | otherwise -> do
                    failTypeError pos $ Error.IllegalTypeApplication (primitiveTypeName pt)
            TUserType TUserTypeDef{tuName}
                | [] == typeParameters -> do
                    return ty
                | otherwise -> do
                    failTypeError pos $ Error.IllegalTypeApplication tuName
            TTypeFun tuParameters _rt
                | [] == typeParameters -> do
                    return ty
                | length tuParameters == length typeParameters -> do
                    (TTypeFun tuParameters' rt') <- instantiate env ty
                    for_ (zip tuParameters' typeParameters) $ \(a, b) -> do
                        b' <- resolveTypeIdent env pos NewTypesAreQuantified b
                        unify pos a b'
                    return rt'
                | otherwise -> do
                    failTypeError pos $ Error.TypeApplicationMismatch (getUnresolvedReferenceLeaf typeName) (length tuParameters) (length typeParameters)
            _
                | [] == typeParameters ->
                    return ty
                | otherwise ->
                    failTypeError pos $ Error.IllegalTypeApplication (Text.pack $ show ty)

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

resolveImportName :: Env -> Pos -> Name -> TC ModuleName
resolveImportName env pos importName = do
    SymbolTable.lookup (eValueBindings env) importName >>= \case
        Just (ModuleReference moduleName) -> return moduleName
        -- TODO: should we differentiate between these cases?
        Just _ -> failTypeError pos $ Error.UnboundSymbol "import" importName
        _ -> failTypeError pos $ Error.UnboundSymbol "import" importName

resolveTypeReference :: Env -> Pos -> ResolvePolicy -> UnresolvedReference -> TC TypeVar
resolveTypeReference env pos resolvePolicy = \case
    UnqualifiedReference name -> do
        SymbolTable.lookup (eTypeBindings env) name >>= \case
            Just (TypeReference t) -> do
                return t
            Nothing | NewTypesAreQuantified == resolvePolicy && not (isCapitalized name) -> do
                tyVar <- freshType env
                quantify tyVar

                SymbolTable.insert (eTypeBindings env) SymbolTable.DisallowDuplicates name (TypeReference tyVar)
                return tyVar
            Nothing -> do
                failTypeError pos $ Error.UnboundSymbol "type" name
    QualifiedReference importName name -> do
        moduleName <- resolveImportName env pos importName
        resolveTypeReference env pos resolvePolicy $ KnownReference moduleName name
    KnownReference moduleName name -> do
        if moduleName == eThisModule env then do
            resolveTypeReference env pos NewTypesAreErrors (UnqualifiedReference name)
        else do
            case findExportedTypeByName env moduleName name of
                Just tv -> return tv
                Nothing -> failTypeError pos $ Error.ModuleReferenceError moduleName name

resolveValueReference :: Env -> Pos -> UnresolvedReference -> TC (ResolvedReference, Mutability, TypeVar)
resolveValueReference env pos ref = case ref of
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
            case findExportedValueByName env moduleName name of
                Just (rref, mutability, typevar) ->
                    return (rref, mutability, typevar)
                Nothing -> failTypeError pos $ Error.ModuleReferenceError moduleName name

{-
resolveReference :: String -> (Env -> HashTable Name a) -> Env -> Pos -> UnresolvedReference -> TC a
resolveReference symbolType symbolTable env pos ref = do
-}

resolvePatternReference :: Env -> Pos -> UnresolvedReference -> TC PatternReference
resolvePatternReference env pos ref = case ref of
    UnqualifiedReference name -> do
        SymbolTable.lookup (ePatternBindings env) name >>= \case
            Just er -> return er
            Nothing -> failTypeError pos $ Error.UnboundSymbol "pattern" name
    QualifiedReference importName name -> do
        moduleName <- resolveImportName env pos importName
        resolvePatternReference env pos $ KnownReference moduleName name
    KnownReference moduleName name -> do
        case findExportedPatternByName env moduleName name of
            Just p -> return p
            Nothing -> failTypeError pos $ Error.ModuleReferenceError moduleName name

resolveExceptionReference :: Env -> Pos -> UnresolvedReference -> TC ExceptionReference
resolveExceptionReference env pos ref = case ref of
    UnqualifiedReference name -> do
        SymbolTable.lookup (eExceptionBindings env) name >>= \case
            Just er -> return er
            Nothing -> failTypeError pos $ Error.UnboundSymbol "exception" name
    QualifiedReference importName name -> do
        moduleName <- resolveImportName env pos importName
        resolveExceptionReference env pos $ KnownReference moduleName name
    KnownReference moduleName name -> do
        case findExportedExceptionByName env moduleName name of
            Just typevar -> return $ ExceptionReference (OtherModule moduleName name) typevar
            Nothing -> failTypeError pos $ Error.ModuleReferenceError moduleName name

resolveArrayType :: Env -> Pos -> Mutability -> TC (TypeVar, TypeVar)
resolveArrayType env pos mutability = do
    elementType <- case mutability of
            Immutable -> freshType env
            Mutable -> freshWeakQVar env

    let typeReference = case mutability of
            Immutable -> KnownReference "array" "Array"
            Mutable -> KnownReference "mutarray" "MutableArray"
    arrayType <- resolveTypeReference env pos NewTypesAreErrors typeReference
    followTypeVar arrayType >>= \case
        TTypeFun [_argType] (TUserType td) -> do
            let newArrayType = TUserType td{ tuParameters=[elementType] }
            return (newArrayType, elementType)
        _ -> fail "Unexpected Array type"

resolveBooleanType :: Env -> Pos -> TC TypeVar
resolveBooleanType env pos = do
    resolveTypeReference env pos NewTypesAreErrors (KnownReference "boolean" "Boolean")

-- TODO: what do we do with this when Variants know their own types
createUserTypeDef :: Env
                  -> Name
                  -> ModuleName
                  -> [TypeVar]
                  -> [Variant _edata]
                  -> TC (TUserTypeDef TypeVar)
createUserTypeDef env name moduleName typeVars variants = do
    variants' <- for variants $ \(Variant _typeVar vname vparameters) -> do
        -- the variant parameters are unified with the corresponding typeidents later
        tvParameters <- for vparameters $ const $ freshType env
        let tvName = vname
        return TVariant {..}

    return $ TUserTypeDef
        { tuName = name
        , tuType = TDNormal
        , tuModuleName = moduleName
        , tuParameters = typeVars
        , tuVariants = variants'
        }

buildTypeEnvironment :: ModuleName -> HashMap ModuleName LoadedModule -> Module UnresolvedReference () Pos -> TC Env
buildTypeEnvironment thisModuleName loadedModules thisModule = do
    let imports = mImports thisModule

    -- built-in types. would be nice to move into the prelude somehow.
    let numTy = TPrimitive Number
    let strTy = TPrimitive String
    env <- newEnv thisModuleName loadedModules Nothing

    SymbolTable.insert (eTypeBindings env) SymbolTable.DisallowDuplicates "Number" (TypeReference numTy)
    SymbolTable.insert (eTypeBindings env) SymbolTable.DisallowDuplicates "String" (TypeReference strTy)

    for_ (HashMap.toList Intrinsic.intrinsics) $ \(name, intrin) -> do
        let Intrinsic{..} = intrin
        SymbolTable.insert (eValueBindings env) SymbolTable.DisallowDuplicates name (ValueReference (Builtin name) Immutable iType)

    for_ imports $ \case
        (pos, UnqualifiedImport importName) -> do
            importedModule <- case HashMap.lookup importName loadedModules of
                Just im -> return im
                Nothing -> failICE $ Error.DependentModuleNotLoaded pos importName

            -- populate types
            for_ (getAllExportedTypes $ importedModule) $ \(name, typeVar) -> do
                SymbolTable.insert (eTypeBindings env) SymbolTable.DisallowDuplicates name (TypeReference typeVar)

            -- populate values
            for_ (getAllExportedValues $ importedModule) $ \(name, (resolvedReference, mutability, tr)) -> do
                SymbolTable.insert (eValueBindings env) SymbolTable.DisallowDuplicates name (ValueReference resolvedReference mutability tr)

            -- populate patterns
            for_ (getAllExportedPatterns $ importedModule) $ \(name, pb) -> do
                SymbolTable.insert (ePatternBindings env) SymbolTable.DisallowDuplicates name pb

        (_pos, QualifiedImport moduleName importName) -> do
            for_ importName $ \importName' -> do
                SymbolTable.insert (eValueBindings env) SymbolTable.DisallowDuplicates importName' (ModuleReference moduleName)

    addThisModuleDataDeclsToEnvironment env thisModule

    return env

getAllExportedValues :: LoadedModule -> [(Name, (ResolvedReference, Mutability, TypeVar))]
getAllExportedValues loadedModule =
    let f (name, ref, mut, tv) = (name, (ref, mut, tv))
    in fmap f $ lmExportedValues loadedModule

getAllExportedExceptions :: LoadedModule -> [(Name, TypeVar)]
getAllExportedExceptions LoadedModule{..} = mconcat $ (flip fmap $ exportedDecls $ mDecls lmModule) $ \case
    DDeclare _ _ _ -> []
    DLet _ _ _ _ _ -> []
    DFun {} -> []
    DData _ _ _ _ _ -> []
    DJSData _ _ _ _ -> []
    DTypeAlias _ _ _ _ -> []
    DException typeVar name _ -> [(name, typeVar)]

getAllExportedTypes :: LoadedModule -> [(Name, TypeVar)]
getAllExportedTypes LoadedModule{..} = mconcat $ (flip fmap $ exportedDecls $ mDecls lmModule) $ \case
    DDeclare {} -> []
    DLet {} -> []
    DFun {} -> []
    DData typeVar name _ _ _ -> [(name, typeVar)]
    DJSData typeVar name _ _ -> [(name, typeVar)]
    DTypeAlias typeVar name _ _ -> [(name, typeVar)]
    DException _ _ _ -> []

getAllExportedPatterns :: LoadedModule -> [(Name, PatternReference)]
getAllExportedPatterns LoadedModule{..} = mconcat $ (flip fmap $ exportedDecls $ mDecls lmModule) $ \case
    DDeclare {} -> []
    DLet {} -> []
    DFun {} -> []
    DData typeVar _name _ _ variants -> do
        let def = case typeVar of
                TUserType d -> d
                TTypeFun _ (TUserType d) -> d
                _ -> error $ "Internal compiler error: data decl registered incorrectly " ++ show typeVar
        (flip fmap) variants $ \(Variant _vtype name _typeIdent) ->
            (name, PatternReference def $ TagVariant name)

    DJSData typeVar _name _ jsVariants ->
        let (TUserType def) = typeVar in
        (flip fmap) jsVariants $ \(JSVariant name literal) ->
            (name, PatternReference def $ TagLiteral literal)

    DTypeAlias _ _ _ _ -> []
    DException _ _ _ -> []

findExportByName :: (LoadedModule -> [(Name, a)]) -> Env -> ModuleName -> Name -> Maybe a
findExportByName getExports env moduleName valueName = do
    modul <- HashMap.lookup moduleName (eLoadedModules env)
    findFirstOf (getExports modul) $ \(name, v) ->
        if name == valueName then
            Just v
        else
            Nothing

findExportedValueByName :: Env -> ModuleName -> Name -> Maybe (ResolvedReference, Mutability, TypeVar)
findExportedValueByName = findExportByName getAllExportedValues

findExportedTypeByName :: Env -> ModuleName -> Name -> Maybe TypeVar
findExportedTypeByName = findExportByName getAllExportedTypes

findExportedExceptionByName :: Env -> ModuleName -> Name -> Maybe TypeVar
findExportedExceptionByName = findExportByName getAllExportedExceptions

findExportedPatternByName :: Env -> ModuleName -> Name -> Maybe PatternReference
findExportedPatternByName = findExportByName getAllExportedPatterns

-- Phase 2a
registerJSFFIDecl :: Env -> DeclarationType UnresolvedReference () Pos -> TC ()
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
                , tuType = TDFfi
                , tuModuleName = moduleName
                , tuParameters = []
                , tuVariants = variants'
                }
        let userType = TUserType typeDef
        SymbolTable.insert (eTypeBindings env) SymbolTable.DisallowDuplicates name (TypeReference userType)

        for_ variants $ \(JSVariant variantName value) -> do
            SymbolTable.insert (eValueBindings env) SymbolTable.DisallowDuplicates variantName (ValueReference (Local variantName) Immutable userType)
            SymbolTable.insert (ePatternBindings env) SymbolTable.DisallowDuplicates variantName (PatternReference typeDef $ TagLiteral value)
        return ()
    DTypeAlias {} -> return ()
    DException {} -> return ()

registerExceptionDecl :: Env -> DeclarationType UnresolvedReference () Pos -> TC ()
registerExceptionDecl env = \case
    DDeclare {} -> return ()
    DLet {} -> return ()
    DFun {} -> return ()

    DData {} -> return ()
    DJSData {} -> return ()
    DTypeAlias {} -> return ()
    DException pos exceptionName typeIdent -> do
        tyVar <- resolveTypeIdent env pos NewTypesAreErrors typeIdent
        SymbolTable.insert (eExceptionBindings env) SymbolTable.DisallowDuplicates exceptionName (ExceptionReference (ThisModule exceptionName) tyVar)
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
            resolveTypeReference e pos NewTypesAreQuantified (UnqualifiedReference tvName)

        typeDef <- createUserTypeDef e typeName moduleName tyVars variants
        let tyVar = TUserType typeDef
        let typeRef = case tyVars of
                [] -> tyVar
                _ -> TTypeFun tyVars tyVar
        SymbolTable.insert (eTypeBindings env) SymbolTable.DisallowDuplicates typeName (TypeReference typeRef)

        let qvars = zip typeVarNames tyVars
        return (pos, typeDef, tyVar, qvars, variants)

    -- Phase 2c.
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
            SymbolTable.insert (eTypeBindings env) SymbolTable.DisallowDuplicates name (TypeReference registeredType)
            return $ Just (pos, paramTypes, ident, bodyTypeVar)
        _ -> do
            return Nothing

    -- Phase 2c.2.
    for_ aliasDecls $ \(pos, paramTypes, ident, bodyTypeVar) -> do
        env' <- childEnv env
        for_ paramTypes $ \(tvName, tv) -> do
            SymbolTable.insert (eTypeBindings env') SymbolTable.DisallowDuplicates tvName (TypeReference tv)
        resolvedType <- resolveTypeIdent env' pos NewTypesAreErrors ident
        unify pos bodyTypeVar resolvedType

    -- Phase 2d.
    for_ dataDecls' $ \(pos, typeDef, tyVar, qvars, variants) -> do
        e <- childEnv env
        for_ qvars $ \(qvName, qvTypeVar) ->
            SymbolTable.insert (eTypeBindings e) SymbolTable.DisallowDuplicates qvName (TypeReference qvTypeVar)

        for_ (zip variants $ tuVariants typeDef) $ \(Variant vpos _ typeIdents, TVariant _ typeVars) -> do
            for_ (zip typeIdents typeVars) $ \(typeIdent, typeVar) -> do
                tv' <- resolveTypeIdent e vpos NewTypesAreErrors typeIdent
                unify vpos typeVar tv'

        let computeVariantType [] = tyVar
            computeVariantType argTypeIdents = TFun argTypeIdents tyVar

        for_ variants $ \(Variant _typeVar vname vparameters) -> do
            parameterTypeVars <- traverse (resolveTypeIdent e pos NewTypesAreErrors) vparameters
            let ctorType = computeVariantType parameterTypeVars
            SymbolTable.insert (eValueBindings env) SymbolTable.DisallowDuplicates vname (ValueReference (ThisModule vname) Immutable ctorType)
            SymbolTable.insert (ePatternBindings env) SymbolTable.DisallowDuplicates vname (PatternReference typeDef $ TagVariant vname)

    -- Phase 3.
    for_ decls $ \decl -> do
        registerExceptionDecl env decl
