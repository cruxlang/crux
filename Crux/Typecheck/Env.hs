
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
    return Env
        { eInLoop = False
        , ..
        }

childEnv :: MonadIO m => Env -> m Env
childEnv env@Env{..} = do
    valueBindings' <- HashTable.clone eValueBindings
    typeBindings <- HashTable.clone eTypeBindings
    return env
        { eValueBindings = valueBindings'
        , eTypeBindings = typeBindings
        }

exportedDecls :: [Declaration a b] -> [DeclarationType a b]
exportedDecls decls = [dt | (Declaration Export _ dt) <- decls]

-- TODO: move into TC so errors are recorded properly
resolveTypeIdent :: Env -> Pos -> ResolvePolicy -> TypeIdent -> TC TypeVar
resolveTypeIdent env@Env{..} pos resolvePolicy typeIdent =
    go typeIdent
  where
    go :: TypeIdent -> TC TypeVar
    go UnitTypeIdent = do
        return $ TPrimitive Unit

    go (TypeIdent typeName typeParameters) = do
        HashTable.lookup typeName eTypeBindings >>= \case
            Just (TypeReference ty) -> do
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

            Nothing | NewTypesAreQuantified == resolvePolicy && not (isCapitalized typeName) -> do
                tyVar <- freshType env
                quantify tyVar

                HashTable.insert typeName (TypeReference tyVar) eTypeBindings
                return tyVar
            Nothing ->
                failTypeError pos $ Error.UnboundType typeName

    go (RecordIdent rows) = do
        rows' <- for rows $ \(trName, mut, rowTypeIdent) -> do
            let trMut = case mut of
                    Nothing -> RFree
                    Just LMutable -> RMutable
                    Just LImmutable -> RImmutable
            trTyVar <- go rowTypeIdent
            return TypeRow{..}
        ref <- newIORef $ RRecord $ RecordType RecordClose rows'
        return $ TRecord $ ref

    go (FunctionIdent argTypes retPrimitive) = do
        argTypes' <- for argTypes go
        retPrimitive' <- go retPrimitive
        return $ TFun argTypes' retPrimitive'

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
        HashTable.insert name (ValueReference (Builtin name) LImmutable iType) (eValueBindings env)

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

getAllExportedValues :: LoadedModule -> [(Name, LetMutability, TypeVar)]
getAllExportedValues loadedModule = mconcat $ (flip fmap $ exportedDecls $ mDecls loadedModule) $ \case
    DDeclare typeVar name _typeIdent -> [(name, LImmutable, typeVar)]
    -- TODO: support trickier patterns, like export let (x, y) = (1, 2)
    DLet typeVar mutability binding _ _ -> case binding of
        PBinding name -> [(name, mutability, typeVar)]
        PWildcard -> []
    DFun typeVar name _ _ _ -> [(name, LImmutable, typeVar)]
    DData _ _ _ _ variants -> fmap (\(Variant typeVar name _) -> (name, LImmutable, typeVar)) variants
    DJSData typeVar _ _ variants -> fmap (\(JSVariant name _) -> (name, LImmutable, typeVar)) variants
    DTypeAlias _ _ _ _ -> []

findExportedValueByName :: Env -> ModuleName -> Name -> Maybe (ResolvedReference, LetMutability, TypeVar)
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
    DFun {} -> return()

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
            HashTable.insert variantName (ValueReference (Local variantName) LImmutable userType) (eValueBindings env)
            HashTable.insert variantName (PatternBinding typeDef []) (ePatternBindings env)
        return ()
    DTypeAlias {} -> return ()

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
            resolveTypeIdent e pos NewTypesAreQuantified (TypeIdent tvName [])

        (typeDef, tyVar) <- createUserTypeDef e typeName moduleName tyVars variants
        HashTable.insert typeName (TypeReference tyVar) (eTypeBindings env)

        let qvars = zip typeVarNames tyVars
        return (pos, typeDef, tyVar, qvars, variants)

    -- Phase 3b.
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

    -- Phase 4b.
    for_ dataDecls' $ \(pos, typeDef, tyVar, qvars, variants) -> do
        e <- childEnv env
        for_ qvars $ \(qvName, qvTypeVar) ->
            HashTable.insert qvName (TypeReference qvTypeVar) (eTypeBindings e)

        let computeVariantType [] = tyVar
            computeVariantType argTypeIdents = TFun argTypeIdents tyVar

        for_ variants $ \(Variant _typeVar vname vparameters) -> do
            parameterTypeVars <- traverse (resolveTypeIdent e pos NewTypesAreErrors) vparameters
            let ctorType = computeVariantType parameterTypeVars
            HashTable.insert vname (ValueReference (ThisModule vname) LImmutable ctorType) (eValueBindings env)
            HashTable.insert vname (PatternBinding typeDef (fmap snd qvars)) (ePatternBindings env)
