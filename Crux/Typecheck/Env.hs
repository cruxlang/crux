
module Crux.Typecheck.Env
    ( ResolvePolicy (..)
    , newEnv
    , childEnv
    , buildTypeEnvironment
    , addThisModuleDataDeclsToEnvironment
    , unfreezeTypeVar
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

resolveTypeIdent :: Env -> ResolvePolicy -> TypeIdent -> IO TypeVar
resolveTypeIdent env@Env{..} resolvePolicy typeIdent =
    go typeIdent
  where
    go UnitTypeIdent = do
        newIORef $ TPrimitive Unit

    go (TypeIdent typeName typeParameters) = do
        HashTable.lookup typeName eTypeBindings >>= \case
            Just (TypeBinding _ ty) -> do
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
                            fail $ printf "Type %s takes %i type parameters.  %i given" (show $ tuName def) (length tuParameters) (length typeParameters)
                    _ ->
                        return ty

            Just (TypeAlias aliasName aliasParams aliasedIdent) -> do
                when (length aliasParams /= length typeParameters) $
                    fail $ printf "Type alias %s takes %i parameters.  %i given" (Text.unpack aliasName) (length aliasParams) (length typeParameters)

                argTypes <- mapM (resolveTypeIdent env resolvePolicy) typeParameters

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

addQvarTable :: Env -> [(Name, TypeVar)] -> IO Env
addQvarTable env@Env{..} qvarTable = do
    newBindings <- HashTable.mergeImmutable eTypeBindings
        (HashMap.fromList [(name, TypeBinding (ThisModule name) ty) | (name, ty) <- qvarTable])
    return env { eTypeBindings = newBindings }

-- TODO: what do we do with this when Variants know their own types
createUserTypeDef :: Env
                  -> Name
                  -> ModuleName
                  -> [a]
                  -> [Variant _edata]
                  -> IO (TUserTypeDef TypeVar, IORef MutableTypeVar)
createUserTypeDef env name moduleName typeVarNames variants = do
    typeVars <- forM typeVarNames $ const $ do
        ft <- freshType env
        quantify ft
        return ft

    variants' <- forM variants $ \(Variant _typeVar vname vparameters) -> do
        tvParameters <- forM vparameters $ const $ freshType env
        let tvName = vname
        return TVariant {..}

    let typeDef = TUserTypeDef
            { tuName = name
            , tuModuleName = moduleName
            , tuParameters = typeVars
            , tuVariants = variants'
            }

    tyVar <- newIORef $ TUserType typeDef typeVars
    return (typeDef, tyVar)

type QVars = [(Name, (ResolvedReference, TypeVar))]

-- TODO: what do we do with this when types are resolved at type-checking type
addDataType :: Env
            -> ModuleName
            -> Name
            -> [TypeName]
            -> [Variant _edata]
            -> IO (TUserTypeDef TypeVar, TypeVar, QVars)
addDataType env importName typeName typeVariables variants = do
    e <- childEnv env
    tyVars <- forM typeVariables $ \tv ->
        resolveTypeIdent e NewTypesAreQuantified (TypeIdent tv [])

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
    forM_ qvars $ \(name, (qName, qTyVar)) ->
        HashTable.insert name (TypeBinding qName qTyVar) (eTypeBindings e)

    let TUserTypeDef { tuVariants } = typeDef
    forM_ (zip variants tuVariants) $ \(Variant _typeVar _name vparameters, tv) -> do
        forM_ (zip vparameters (tvParameters tv)) $ \(typeIdent, typeVar) -> do
            t <- resolveTypeIdent e NewTypesAreErrors typeIdent
            unify typeVar t

-- TODO(chad): return an Either instead of throwing exceptions
buildTypeEnvironment :: ModuleName -> HashMap ModuleName LoadedModule -> [Import] -> IO (Either Error.Error Env)
buildTypeEnvironment thisModuleName loadedModules imports = runEitherT $ do
    -- built-in types. would be nice to move into the prelude somehow.
    numTy  <- newIORef $ TPrimitive Number
    strTy  <- newIORef $ TPrimitive String

    env <- liftIO $ newEnv thisModuleName loadedModules Nothing
    HashTable.insert "Number" (TypeBinding (Builtin "Number") numTy) (eTypeBindings env)
    HashTable.insert "String" (TypeBinding (Builtin "String") strTy) (eTypeBindings env)

    intrinsics <- liftIO $ Intrinsic.intrinsics
    forM_ (HashMap.toList intrinsics) $ \(name, intrin) -> do
        let Intrinsic{..} = intrin
        HashTable.insert name (ValueReference (Builtin name) LImmutable iType) (eValueBindings env)

    forM_ imports $ \case
        UnqualifiedImport importName -> do
            importedModule <- case HashMap.lookup importName loadedModules of
                Just im -> return im
                Nothing -> left $ Error.InternalCompilerError $ Error.DependentModuleNotLoaded importName

            liftIO $ addLoadedDataDeclsToEnvironment env importedModule (exportedDecls $ mDecls importedModule) (OtherModule importName)

            for_ (getAllExportedValues $ importedModule) $ \(name, mutability, tr) -> do
                tr' <- lift $ unfreezeTypeVar tr
                HashTable.insert name (ValueReference (OtherModule importName name) mutability tr') (eValueBindings env)

            patBindings <- lift $ getAllExportedPatterns importedModule
            for_ patBindings $ \(name, pb) -> do
                HashTable.insert name pb (ePatternBindings env)

        QualifiedImport moduleName importName -> do
            HashTable.insert importName (ModuleReference moduleName) (eValueBindings env)

    return env

getAllExportedValues :: LoadedModule -> [(Name, LetMutability, ImmutableTypeVar)]
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
    tv <- lift $ unfreezeTypeVar typeVar
    return (OtherModule moduleName valueName, mutability, tv)

getAllExportedTypes :: LoadedModule -> [(Name, ImmutableTypeVar)]
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
    tv <- lift $ unfreezeTypeVar typeVar
    return (OtherModule moduleName typeName, tv)

-- we can just use PatternBinding for now
getAllExportedPatterns :: LoadedModule -> IO [(Name, PatternBinding)]
getAllExportedPatterns loadedModule = mconcat <$> for (exportedDecls $ mDecls loadedModule) fromDecl
  where fromDecl :: DeclarationType idtype ImmutableTypeVar -> IO [(Name, PatternBinding)]
        fromDecl = \case
            DDeclare {} -> return []
            DLet {} -> return []
            DFun {} -> return []
            DData typeVar _name _ _ variants -> do
                for variants $ \(Variant vtype name _typeIdent) -> do
                    let (IUserType def typeVars) = typeVar
                    let args :: [ImmutableTypeVar]
                        args = case vtype of
                            IFun args_ _rv -> args_
                            _ -> []
                    def' <- traverse unfreezeTypeVar def
                    typeVars' <- traverse unfreezeTypeVar typeVars
                    args' <- traverse unfreezeTypeVar args
                    return (name, PatternBinding def' typeVars' args')

            DJSData typeVar _name _ jsVariants -> do
                for jsVariants $ \(JSVariant name _literal) -> do
                    let (IUserType def _) = typeVar
                    newDef <- traverse unfreezeTypeVar def
                    return (name, PatternBinding newDef [] [])

            DTypeAlias _ _ _ -> return []

addLoadedDataDeclsToEnvironment ::
       Env
    -> Module idtype ImmutableTypeVar
    -> [DeclarationType idtype ImmutableTypeVar]
    -> (Name -> ResolvedReference)
    -> IO ()
addLoadedDataDeclsToEnvironment env modul decls mkName = do
    -- First, populate the type environment.  Variant parameter types are all initially free.
    forM_ decls $ \case
        DJSData typeVar name _moduleName _variants -> do
            userType <- unfreezeTypeVar typeVar
            HashTable.insert name (TypeBinding (mkName name) userType) (eTypeBindings env)

        DTypeAlias name params ident ->
            HashTable.insert name (TypeAlias name params ident) (eTypeBindings env)

        DDeclare {} -> return ()
        DData {}    -> return ()
        DFun {}     -> return ()
        DLet {}     -> return ()

    dataDecls <- fmap catMaybes $ forM (mDecls modul) $ \(Declaration exportFlag pos decl) -> case decl of
        -- TODO: we should use typeVar here
        DData _typeVar name moduleName typeVarNames variants ->
            return $ Just (name, exportFlag, pos, moduleName, typeVarNames, variants)
        _ ->
            return Nothing

    dataDecls' <- forM dataDecls $ \(name, _exportFlag, _pos, moduleName, typeVarNames, variants) -> do
        (typeDef, _tyVar, qvars) <- addDataType env moduleName name typeVarNames variants
        return (typeDef, qvars, variants)

    for_ dataDecls' $ \(typeDef, qvars, variants) ->
        resolveVariantTypes env qvars typeDef variants

addThisModuleDataDeclsToEnvironment ::
       Env
    -> Module idtype edata
    -> [DeclarationType t1 t2]
    -> (Name -> ResolvedReference)
    -> IO ()
addThisModuleDataDeclsToEnvironment env modul decls mkName = do
    -- First, populate the type environment.  Variant parameter types are all initially free.
    forM_ decls $ \case
        DJSData {} -> return ()

        DTypeAlias name params ident ->
            HashTable.insert name (TypeAlias name params ident) (eTypeBindings env)

        DDeclare {} -> return ()
        DData {}    -> return ()
        DFun {}     -> return ()
        DLet {}     -> return ()

    dataDecls <- fmap catMaybes $ forM (mDecls modul) $ \(Declaration _exportFlag _pos decl) -> case decl of
        DData _pos name moduleName typeVarNames variants ->
            return $ Just (name, moduleName, typeVarNames, variants)
        _ ->
            return Nothing

    dataDecls' <- forM dataDecls $ \(name, moduleName, typeVarNames, variants) -> do
        (typeDef, tyVar, qvars) <- addDataType env moduleName name typeVarNames variants
        return (typeDef, tyVar, qvars, variants)

    forM_ dataDecls' $ \(typeDef, _tyVar, qvars, variants) ->
        resolveVariantTypes env qvars typeDef variants

    forM_ dataDecls' $ \(typeDef, tyVar, qvars, variants) ->
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
    e <- childEnv env
    forM_ qvars $ \(qvName, (qvTypeName, qvTypeVar)) ->
        HashTable.insert qvName (TypeBinding qvTypeName qvTypeVar) (eTypeBindings e)

    let computeVariantType [] = return userTypeVar
        computeVariantType argTypeIdents = newIORef $ TFun argTypeIdents userTypeVar

    forM_ variants $ \(Variant _typeVar vname vparameters) -> do
        parameterTypeVars <- traverse (resolveTypeIdent e NewTypesAreErrors) vparameters
        ctorType <- computeVariantType parameterTypeVars
        HashTable.insert vname (ValueReference (mkName vname) LImmutable ctorType) (eValueBindings env)
        HashTable.insert vname (PatternBinding typeDef (fmap (snd . snd) qvars) parameterTypeVars) (ePatternBindings env)
