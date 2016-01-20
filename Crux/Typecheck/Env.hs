
module Crux.Typecheck.Env
    ( ResolvePolicy (..)
    , newEnv
    , childEnv
    , buildTypeEnvironment
    , unfreezeTypeVar
    , resolveTypeIdent

    , addDataType
    , resolveVariantTypes
    , addVariants
    ) where

import           Crux.AST
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

data ResolvePolicy = NewTypesAreErrors | NewTypesAreQuantified
    deriving (Eq)

newEnv :: HashMap ModuleName LoadedModule -> Maybe TypeVar -> IO Env
newEnv eLoadedModules eReturnType = do
    eNextTypeIndex <- newIORef 0
    eValueBindings <- newIORef HashMap.empty
    eTypeBindings <- newIORef HashMap.empty
    eTypeAliases <- newIORef HashMap.empty
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
            Nothing -> do
                res2 <- HashTable.lookup typeName eTypeAliases
                case res2 of
                    Just (TypeAlias aliasName aliasParams aliasedIdent) -> do
                        when (length aliasParams /= length typeParameters) $
                            fail $ printf "Type alias %s takes %i parameters.  %i given" (Text.unpack aliasName) (length aliasParams) (length typeParameters)

                        argTypes <- mapM (resolveTypeIdent env resolvePolicy) typeParameters

                        env' <- addQvarTable env (zip aliasParams argTypes)

                        resolveTypeIdent env' resolvePolicy aliasedIdent
                    Nothing | NewTypesAreQuantified == resolvePolicy && not (isCapitalized typeName) -> do
                        tyVar <- freshType env
                        quantify tyVar

                        HashTable.insert typeName (ThisModule typeName, tyVar) eTypeBindings
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
        (HashMap.fromList [(name, (ThisModule name, ty)) | (name, ty) <- qvarTable])
    return env { eTypeBindings = newBindings }

-- TODO: merge this with findExportedDeclaration
findDeclByName :: Module idtype edata -> Name -> Maybe (Declaration idtype edata)
findDeclByName modul name =
    let go decls = case decls of
            [] -> Nothing
            (d:rest) ->
                if match then Just d else go rest
              where
                Declaration _ _ dt = d
                match = case dt of
                    DDeclare n _              | n == name -> True
                    DLet _ _ (PBinding n) _ _ | n == name -> True
                    DFun (FunDef _ n _ _ _)   | n == name -> True
                    DData n _ _ _             | n == name -> True
                    DJSData n _ _             | n == name -> True
                    DType (TypeAlias n _ _)   | n == name -> True
                    _ -> False
    in go (mDecls modul)

createUserTypeDef :: Env
                  -> Name
                  -> ModuleName
                  -> [a]
                  -> [Variant]
                  -> IO (TUserTypeDef TypeVar, IORef MutableTypeVar)
createUserTypeDef env name moduleName typeVarNames variants = do
    typeVars <- forM typeVarNames $ const $ do
        ft <- freshType env
        quantify ft
        return ft

    variants' <- forM variants $ \Variant{..} -> do
        tvParameters <- forM vparameters $
            const $ freshType env
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

addDataType :: Env
            -> ModuleName
            -> Name
            -> [TypeName]
            -> [Variant]
            -> IO (TUserTypeDef TypeVar, TypeVar, QVars)
addDataType env importName dname typeVariables variants = do
    e <- childEnv env
    tyVars <- forM typeVariables $ \tv ->
        resolveTypeIdent e NewTypesAreQuantified (TypeIdent tv [])

    (typeDef, tyVar) <- createUserTypeDef e dname importName tyVars variants
    HashTable.insert dname (OtherModule importName dname, tyVar) (eTypeBindings env)

    let t = [(tn, (OtherModule importName tn, tv)) | (tn, tv) <- zip typeVariables tyVars]
    return (typeDef, tyVar, t)

resolveVariantTypes :: Env
                    -> QVars
                    -> TUserTypeDef TypeVar
                    -> [Variant]
                    -> IO ()
resolveVariantTypes env qvars typeDef variants = do
    e <- childEnv env
    forM_ qvars $ \(name, tyvar) ->
        HashTable.insert name tyvar (eTypeBindings e)

    let TUserTypeDef { tuVariants } = typeDef
    forM_ (zip variants tuVariants) $ \(v, tv) -> do
        forM_ (zip (vparameters v) (tvParameters tv)) $ \(typeIdent, typeVar) -> do
            t <- resolveTypeIdent e NewTypesAreErrors typeIdent
            unify typeVar t

addVariants ::
             (Show a, Typeable a) =>
                Env
             -> Name
             -> Module idtype edata
             -> ExportFlag
             -> a
             -> QVars
             -> IORef MutableTypeVar
             -> [Variant]
             -> (Name -> ResolvedReference)
             -> IO ()
addVariants env name modul exportFlag declPos qvars userTypeVar variants mkName = do
    e <- childEnv env
    forM_ qvars $ \(qvName, qvType) ->
        HashTable.insert qvName qvType (eTypeBindings e)

    let computeVariantType argTypeIdents = case argTypeIdents of
            [] ->
                return userTypeVar
            _ -> do
                resolvedArgTypes <- mapM (resolveTypeIdent e NewTypesAreErrors) argTypeIdents
                newIORef $ TFun resolvedArgTypes userTypeVar

    forM_ variants $ \(Variant vname vparameters) -> do

        forM_ vparameters $ \parameter ->
            case (exportFlag, parameter) of
                (Export, TypeIdent tiName _)
                    | Just (Declaration NoExport dpos _) <- findDeclByName modul tiName ->
                        throwIO $ ExportError declPos $
                            printf "Variant %s of exported data type %s depends on nonexported data type %s (defined at %s)"
                                (show $ vname) (show name) (show tiName) (formatPos dpos)
                _ ->
                    return ()

        ctorType <- computeVariantType vparameters
        HashTable.insert vname (ValueReference (mkName vname) LImmutable ctorType) (eValueBindings env)

buildTypeEnvironment :: (Show j, Show a) => HashMap ModuleName LoadedModule -> Module j a -> IO Env
buildTypeEnvironment loadedModules modul = do
    -- built-in types. would be nice to move into the prelude somehow.
    numTy  <- newIORef $ TPrimitive Number
    strTy  <- newIORef $ TPrimitive String

    env <- newEnv loadedModules Nothing
    HashTable.insert "Number" (Builtin "Number", numTy) (eTypeBindings env)
    HashTable.insert "String" (Builtin "String", strTy) (eTypeBindings env)

    forM_ (mImports modul) $ \case
        UnqualifiedImport importName -> do
            importedModule <- case HashMap.lookup importName loadedModules of
                Just im -> return im
                Nothing -> fail $ "dependent module not loaded: " <> (Text.unpack $ printModuleName importName)

            addDataDeclsToEnvironment env importedModule (exportedDecls $ mDecls importedModule) (OtherModule importName)

            forM_ (exportedDecls $ mDecls importedModule) $ \case
                DLet tr _mutability pat _ _ -> do
                    tr' <- unfreezeTypeVar tr
                    case pat of
                        PWildcard -> return ()
                        PBinding name -> do
                            HashTable.insert name (ValueReference (OtherModule importName name) LImmutable tr') (eValueBindings env)

                DFun (FunDef tr name _params _retAnn _body) -> do
                    tr' <- unfreezeTypeVar tr
                    HashTable.insert name (ValueReference (OtherModule importName name) LImmutable tr') (eValueBindings env)

                _ -> return ()
        QualifiedImport moduleName importName -> do
            HashTable.insert importName (ModuleReference moduleName) (eValueBindings env)

    addDataDeclsToEnvironment env modul [decl | Declaration _ _ decl <- mDecls modul] ThisModule

    return env

addDataDeclsToEnvironment ::
       Env
    -> Module idtype edata
    -> [DeclarationType t1 t2]
    -> (Name -> ResolvedReference)
    -> IO ()
addDataDeclsToEnvironment env modul decls mkName = do
    -- First, populate the type environment.  Variant parameter types are all initially free.
    forM_ decls $ \decl -> case decl of
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
            modifyIORef' (eTypeBindings env) $ HashMap.insert name (mkName name, userType)

        DType ty@(TypeAlias name _ _) ->
            HashTable.insert name ty (eTypeAliases env)

        DDeclare {} -> return ()
        DData {}    -> return ()
        DFun {}     -> return ()
        DLet {}     -> return ()

    dataDecls <- fmap catMaybes $ forM (mDecls modul) $ \(Declaration exportFlag pos decl) -> case decl of
        DData name moduleName typeVarNames variants ->
            return $ Just (name, exportFlag, pos, moduleName, typeVarNames, variants)
        _ ->
            return Nothing

    dataDecls' <- forM dataDecls $ \(name, exportFlag, pos, moduleName, typeVarNames, variants) -> do
        (typeDef, tyVar, qvars) <- addDataType env moduleName name typeVarNames variants
        return (name, exportFlag, pos, typeDef, tyVar, qvars, variants)

    forM_ dataDecls' $ \(_name, _exportFlag, _pos, typeDef, _tyVar, qvars, variants) ->
        resolveVariantTypes env qvars typeDef variants

    forM_ dataDecls' $ \(name, exportFlag, pos, _typeDef, tyVar, qvars, variants) ->
        addVariants env name modul exportFlag pos qvars tyVar variants mkName

    intrinsics <- Intrinsic.intrinsics

    forM_ (HashMap.toList intrinsics) $ \(name, intrin) -> do
        let Intrinsic{..} = intrin
        HashTable.insert name (ValueReference (Builtin name) LImmutable iType) (eValueBindings env)

    -- Note to self: Here we need to match the names of the types of each variant up with concrete types, but also
    -- with the TypeVars created in the type environment.
    forM_ (mDecls modul) $ \(Declaration _ _ decl) -> case decl of
        DDeclare name typeIdent -> do
            t <- resolveTypeIdent env NewTypesAreQuantified typeIdent
            HashTable.insert name (ValueReference (mkName name) LImmutable t) (eValueBindings env)
        DData {} -> do
            return ()
        DJSData name _ variants -> do
            (_, userType) <- HashTable.lookup name (eTypeBindings env) >>= \case
                Nothing -> error $ printf "DJSData: Could not find name %s" (show name)
                Just a -> return a
            forM_ variants $ \(JSVariant variantName _value) -> do
                HashTable.insert variantName (ValueReference (mkName variantName) LImmutable userType) (eValueBindings env)
        _ -> return ()
