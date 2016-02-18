
module Crux.Typecheck.Env
    ( ResolvePolicy (..)
    , newEnv
    , childEnv
    , buildTypeEnvironment
    , addThisModuleDataDeclsToEnvironment
    , resolveTypeIdent
    , exportedDecls
    , getAllExportedValues
    , findExportedValueByName
    , getAllExportedTypes
    , findExportedTypeByName
    , addDataType
    , resolveVariantTypes
    , addVariants
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
import qualified Data.Text             as Text
import           Prelude               hiding (String)
import           Text.Printf           (printf)
import Crux.Util
import Crux.TypeVar
import Crux.Module.Types

data ResolvePolicy = NewTypesAreErrors | NewTypesAreQuantified
    deriving (Eq)

newEnv :: ModuleName -> HashMap ModuleName LoadedModule -> Maybe TypeVar -> IO Env
newEnv eThisModule eLoadedModules eReturnType = do
    eNextTypeIndex <- newIORef 0
    eValueBindings <- newIORef HashMap.empty
    eTypeBindings <- newIORef HashMap.empty
    ePatternBindings <- newIORef HashMap.empty
    return Env
        { eInLoop = False
        , ..
        }

childEnv :: Env -> IO Env
childEnv env@Env{..} = do
    valueBindings' <- HashTable.clone eValueBindings
    typeBindings <- HashTable.clone eTypeBindings
    return env
        { eValueBindings = valueBindings'
        , eTypeBindings = typeBindings
        }

exportedDecls :: [Declaration a b] -> [DeclarationType a b]
exportedDecls decls = [dt | (Declaration Export _ dt) <- decls]

resolveTypeIdent :: Env -> ResolvePolicy -> TypeIdent -> IO TypeVar
resolveTypeIdent env@Env{..} resolvePolicy typeIdent =
    go typeIdent
  where
    go UnitTypeIdent = do
        return $ TPrimitive Unit

    go (TypeIdent typeName typeParameters) = do
        HashTable.lookup typeName eTypeBindings >>= \case
            Just (TypeBinding _ ty) -> do
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
                    _ ->
                        return ty

            Just (TypeAlias aliasName aliasParams aliasedIdent) -> do
                when (length aliasParams /= length typeParameters) $
                    fail $ printf "Type alias %s takes %i parameters.  %i given" (Text.unpack aliasName) (length aliasParams) (length typeParameters)

                argTypes <- for typeParameters $ resolveTypeIdent env resolvePolicy

                env' <- addQvarTable env (zip aliasParams argTypes)

                resolveTypeIdent env' resolvePolicy aliasedIdent
            Nothing | NewTypesAreQuantified == resolvePolicy && not (isCapitalized typeName) -> do
                tyVar <- freshType env
                quantify tyVar

                HashTable.insert typeName (TypeBinding (ThisModule typeName) tyVar) eTypeBindings
                return tyVar
            Nothing ->
                fail $ printf "Constructor refers to nonexistent type %s" (show typeName)

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

addQvarTable :: Env -> [(Name, TypeVar)] -> IO Env
addQvarTable env@Env{..} qvarTable = do
    newBindings <- HashTable.mergeImmutable eTypeBindings
        (HashMap.fromList [(name, TypeBinding (ThisModule name) ty) | (name, ty) <- qvarTable])
    return env { eTypeBindings = newBindings }

-- TODO: what do we do with this when Variants know their own types
createUserTypeDef :: Env
                  -> Name
                  -> ModuleName
                  -> [TypeVar]
                  -> [Variant _edata]
                  -> IO (TUserTypeDef TypeVar, TypeVar)
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

type QVars = [(Name, (ResolvedReference, TypeVar))]

-- TODO: what do we do with this when types are resolved at type-checking time
addDataType :: Env
            -> ModuleName
            -> Name
            -> [TypeName]
            -> [Variant _edata]
            -> IO (TUserTypeDef TypeVar, TypeVar, QVars)
addDataType env importName typeName typeVariables variants = do
    e <- childEnv env
    tyVars <- for typeVariables $ \tv ->
        resolveTypeIdent e NewTypesAreQuantified (TypeIdent tv [])

    {-
    typeVarNames <- for tyVars $ renderTypeVarIO
    putStrLn $ "addDataType: " ++ show typeName ++ " " ++ show typeVarNames
    -}

    (typeDef, tyVar) <- createUserTypeDef e typeName importName tyVars variants
    HashTable.insert typeName (TypeBinding (OtherModule importName typeName) tyVar) (eTypeBindings env)

    let t = [(tn, (OtherModule importName tn, tv)) | (tn, tv) <- zip typeVariables tyVars]
    return (typeDef, tyVar, t)

-- TODO: what do we do with this when types are resolved at type-checking time
resolveVariantTypes :: Env
                    -> QVars
                    -> TUserTypeDef TypeVar
                    -> [Variant zzz]
                    -> IO ()
resolveVariantTypes env qvars typeDef variants = do
    e <- childEnv env
    for_ qvars $ \(name, (qName, qTyVar)) ->
        HashTable.insert name (TypeBinding qName qTyVar) (eTypeBindings e)

    let TUserTypeDef { tuVariants } = typeDef
    for_ (zip variants tuVariants) $ \(Variant _typeVar _name vparameters, tv) -> do
        for_ (zip vparameters (tvParameters tv)) $ \(typeIdent, typeVar) -> do
            t <- resolveTypeIdent e NewTypesAreErrors typeIdent
            unify typeVar t

-- TODO(chad): return an Either instead of throwing exceptions
buildTypeEnvironment :: ModuleName -> HashMap ModuleName LoadedModule -> [Import] -> IO (Either Error.Error Env)
buildTypeEnvironment thisModuleName loadedModules imports = runEitherT $ do
    -- built-in types. would be nice to move into the prelude somehow.
    let numTy = TPrimitive Number
    let strTy = TPrimitive String

    env <- liftIO $ newEnv thisModuleName loadedModules Nothing
    HashTable.insert "Number" (TypeBinding (Builtin "Number") numTy) (eTypeBindings env)
    HashTable.insert "String" (TypeBinding (Builtin "String") strTy) (eTypeBindings env)

    intrinsics <- liftIO $ Intrinsic.intrinsics
    for_ (HashMap.toList intrinsics) $ \(name, intrin) -> do
        let Intrinsic{..} = intrin
        HashTable.insert name (ValueReference (Builtin name) LImmutable iType) (eValueBindings env)

    for_ imports $ \case
        UnqualifiedImport importName -> do
            importedModule <- case HashMap.lookup importName loadedModules of
                Just im -> return im
                Nothing -> left $ Error.InternalCompilerError $ Error.DependentModuleNotLoaded importName

            liftIO $ addLoadedDataDeclsToEnvironment env importedModule (exportedDecls $ mDecls importedModule) (OtherModule importName)

            for_ (getAllExportedValues $ importedModule) $ \(name, mutability, tr) -> do
                HashTable.insert name (ValueReference (OtherModule importName name) mutability tr) (eValueBindings env)

            patBindings <- lift $ getAllExportedPatterns importedModule
            for_ patBindings $ \(name, pb) -> do
                HashTable.insert name pb (ePatternBindings env)

        QualifiedImport moduleName importName -> do
            HashTable.insert importName (ModuleReference moduleName) (eValueBindings env)

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
    DTypeAlias _ _ _ -> []

findExportedValueByName :: Env -> ModuleName -> Name -> IO (Maybe (ResolvedReference, LetMutability, TypeVar))
findExportedValueByName env moduleName valueName = runMaybeT $ do
    modul <- MaybeT $ return $ HashMap.lookup moduleName (eLoadedModules env)
    (typeVar, mutability) <- MaybeT $ return $ findFirstOf (getAllExportedValues modul) $ \(name, mutability, typeVar) ->
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
    DTypeAlias _ _ _ -> [] -- TODO refer to exported type aliases

findExportedTypeByName :: Env -> ModuleName -> Name -> IO (Maybe (ResolvedReference, TypeVar))
findExportedTypeByName env moduleName typeName = runMaybeT $ do
    modul <- MaybeT $ return $ HashMap.lookup moduleName (eLoadedModules env)
    typeVar <- MaybeT $ return $ findFirstOf (getAllExportedTypes modul) $ \(name, typeVar) ->
        if name == typeName then
            Just typeVar
        else
            Nothing
    return (OtherModule moduleName typeName, typeVar)

-- we can just use PatternBinding for now
getAllExportedPatterns :: LoadedModule -> IO [(Name, PatternBinding)]
getAllExportedPatterns loadedModule = mconcat <$> for (exportedDecls $ mDecls loadedModule) fromDecl
  where fromDecl :: DeclarationType idtype TypeVar -> IO [(Name, PatternBinding)]
        fromDecl = \case
            DDeclare {} -> return []
            DLet {} -> return []
            DFun {} -> return []
            DData typeVar _name _ _ variants -> do
                for variants $ \(Variant vtype name _typeIdent) -> do
                    let (TUserType def typeVars) = typeVar
                    let args :: [TypeVar]
                        args = case vtype of
                            TFun args_ _rv -> args_
                            _ -> []
                    return (name, PatternBinding def typeVars args)

            DJSData typeVar _name _ jsVariants -> do
                for jsVariants $ \(JSVariant name _literal) -> do
                    let (TUserType def _) = typeVar
                    return (name, PatternBinding def [] [])

            DTypeAlias _ _ _ -> return []

addLoadedDataDeclsToEnvironment ::
       Env
    -> Module idtype TypeVar
    -> [DeclarationType idtype TypeVar]
    -> (Name -> ResolvedReference)
    -> IO ()
addLoadedDataDeclsToEnvironment env modul decls mkName = do
    -- First, populate the type environment.  Variant parameter types are all initially free.
    for_ decls $ \case
        DJSData typeVar name _moduleName _variants -> do
            HashTable.insert name (TypeBinding (mkName name) typeVar) (eTypeBindings env)

        DTypeAlias name params ident ->
            HashTable.insert name (TypeAlias name params ident) (eTypeBindings env)

        DDeclare {} -> return ()
        DData {}    -> return ()
        DFun {}     -> return ()
        DLet {}     -> return ()

    dataDecls <- fmap catMaybes $ for (mDecls modul) $ \(Declaration exportFlag pos decl) -> case decl of
        -- TODO: we should use typeVar here
        DData _typeVar name moduleName typeVarNames variants ->
            return $ Just (name, exportFlag, pos, moduleName, typeVarNames, variants)
        _ ->
            return Nothing

    dataDecls' <- for dataDecls $ \(name, _exportFlag, _pos, moduleName, typeVarNames, variants) -> do
        (typeDef, _tyVar, qvars) <- addDataType env moduleName name typeVarNames variants
        return (typeDef, qvars, variants)

    for_ dataDecls' $ \(typeDef, qvars, variants) ->
        resolveVariantTypes env qvars typeDef variants

addThisModuleDataDeclsToEnvironment
    :: Env
    -> Module idtype edata
    -> [DeclarationType t1 t2]
    -> (Name -> ResolvedReference)
    -> IO ()
addThisModuleDataDeclsToEnvironment env modul decls mkName = do
    -- First, populate the type environment.  Variant parameter types are all initially free.
    for_ decls $ \case
        DJSData {} -> return ()

        DTypeAlias name params ident ->
            HashTable.insert name (TypeAlias name params ident) (eTypeBindings env)

        DDeclare {} -> return ()
        DData {}    -> return ()
        DFun {}     -> return ()
        DLet {}     -> return ()

    dataDecls <- fmap catMaybes $ for (mDecls modul) $ \(Declaration _exportFlag _pos decl) -> case decl of
        DData _pos name moduleName typeVarNames variants ->
            return $ Just (name, moduleName, typeVarNames, variants)
        _ ->
            return Nothing

    dataDecls' <- for dataDecls $ \(name, moduleName, typeVarNames, variants) -> do
        (typeDef, tyVar, qvars) <- addDataType env moduleName name typeVarNames variants
        return (typeDef, tyVar, qvars, variants)

    for_ dataDecls' $ \(typeDef, _tyVar, qvars, variants) ->
        resolveVariantTypes env qvars typeDef variants

    for_ dataDecls' $ \(typeDef, tyVar, qvars, variants) ->
        addVariants env typeDef qvars tyVar variants mkName

addVariants
    :: Env
    -> TUserTypeDef TypeVar
    -> QVars
    -> TypeVar
    -> [Variant edata]
    -> (Name -> ResolvedReference)
    -> IO ()
addVariants env typeDef qvars userTypeVar variants mkName = do
    {-
    userTypeName <- renderTypeVarIO userTypeVar
    putStrLn $ "addVariants for " ++ userTypeName ++ ": qvars:"
    for_ qvars $ \(name, (rr, tv)) -> do
        typeName <- renderTypeVarIO tv
        putStrLn $ "  " ++ show name ++ " " ++ show rr ++ " " ++ typeName
    -}

    e <- childEnv env
    for_ qvars $ \(qvName, (qvTypeName, qvTypeVar)) ->
        HashTable.insert qvName (TypeBinding qvTypeName qvTypeVar) (eTypeBindings e)

    let computeVariantType [] = userTypeVar
        computeVariantType argTypeIdents = TFun argTypeIdents userTypeVar

    for_ variants $ \(Variant _typeVar vname vparameters) -> do
        parameterTypeVars <- traverse (resolveTypeIdent e NewTypesAreErrors) vparameters
        let ctorType = computeVariantType parameterTypeVars
        HashTable.insert vname (ValueReference (mkName vname) LImmutable ctorType) (eValueBindings env)
        HashTable.insert vname (PatternBinding typeDef (fmap (snd . snd) qvars) parameterTypeVars) (ePatternBindings env)
