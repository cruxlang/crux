
module Crux.Typecheck.Env
    ( ResolvePolicy (..)
    , newEnv
    , childEnv
    , buildTypeEnvironment
    , unfreezeTypeVar
    , resolveTypeIdent
    ) where

import           Crux.AST
import           Crux.Intrinsic        (Intrinsic (..))
import qualified Crux.Intrinsic        as Intrinsic
import qualified Crux.MutableHashTable as HashTable
import           Crux.Prelude
import           Crux.Text             (isCapitalized)
import           Crux.Typecheck.Types
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
    eBindings <- newIORef HashMap.empty
    eLocalBindings <- newIORef HashMap.empty
    eTypeBindings <- newIORef HashMap.empty
    let eTypeAliases = HashMap.empty
        eInLoop = False
    return Env {..}

childEnv :: Env -> IO Env
childEnv env@Env{..} = do
    bindings'    <- HashTable.clone eBindings
    localBindings' <- HashTable.clone eLocalBindings
    typeBindings <- HashTable.clone eTypeBindings
    return env
        { eBindings      = bindings'
        , eLocalBindings = localBindings'
        , eTypeBindings  = typeBindings
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
            Nothing -> case HashMap.lookup typeName eTypeAliases of
                Just (TypeAlias aliasName aliasParams aliasedIdent) -> do
                    when (length aliasParams /= length typeParameters) $
                        error $ printf "Type alias %s takes %i parameters.  %i given" (Text.unpack aliasName) (length aliasParams) (length typeParameters)

                    argTypes <- mapM (resolveTypeIdent env resolvePolicy) typeParameters

                    env' <- addQvarTable env (zip aliasParams argTypes)

                    resolveTypeIdent env' resolvePolicy aliasedIdent
                Nothing | NewTypesAreQuantified == resolvePolicy && not (isCapitalized typeName) -> do
                    tyVar <- freshType env
                    quantify tyVar

                    HashTable.insert typeName (ThisModule typeName, tyVar) eTypeBindings
                    return tyVar
                Nothing ->
                    error $ printf "Constructor refers to nonexistent type %s" (show typeName)

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

buildTypeEnvironment :: (Show j, Show a) => HashMap ModuleName LoadedModule -> Module j a -> IO Env
buildTypeEnvironment loadedModules modul = do
    -- built-in types. would be nice to move into the prelude somehow.
    numTy  <- newIORef $ TPrimitive Number
    strTy  <- newIORef $ TPrimitive String

    e <- newEnv loadedModules Nothing
    typeEnv <- newIORef $ HashMap.fromList
        [ ("Number", (Builtin "Number", numTy))
        , ("String", (Builtin "String", strTy))
        ]

    qvarsRef <- HashTable.new :: IO (IORef (HashMap Name [(TypeVariable, TypeVar)]))

    let insertDataType scope name moduleName typeVarNames variants = do
            typeVars <- forM typeVarNames $ const $ do
                ft <- freshType e
                quantify ft
                return ft
            HashTable.insert name (zip typeVarNames typeVars) qvarsRef

            variants' <- forM variants $ \Variant{..} -> do
                tvParameters <- forM vparameters $ const $ freshType e
                let tvName = vname
                return TVariant{..}

            let typeDef = TUserTypeDef
                    { tuName = name
                    , tuModuleName = moduleName
                    , tuParameters = typeVars
                    , tuVariants = variants'
                    }
            userType <- newIORef $ TUserType typeDef typeVars
            HashTable.insert name (scope name, userType) typeEnv
            return typeDef

    -- inject stuff from the prelude into this global environment
    -- TODO: rather than injecting symbols, we may need a mechanism to refer
    -- to imported symbols
    forM_ (mImports modul) $ \(UnqualifiedImport importName) -> do
        importedModule <- case HashMap.lookup importName loadedModules of
            Just im -> return im
            Nothing -> fail $ "dependent module not loaded: " <> (Text.unpack $ printModuleName importName)

        forM_ (exportedDecls $ mDecls importedModule) $ \decl -> case decl of
            DDeclare _ _ -> do
                fail "TODO: export declare"

            DLet tr _mutability pat _ _ -> do
                tr' <- unfreezeTypeVar tr
                case pat of
                    PWildcard -> return ()
                    PBinding name -> do
                        HashTable.insert name (OtherModule importName name, LImmutable, tr') (eBindings e)

            DFun (FunDef tr name _params _retAnn _body) -> do
                tr' <- unfreezeTypeVar tr
                HashTable.insert name (OtherModule importName name, LImmutable, tr') (eBindings e)

            DData dname _ typeVariables variants -> do
                env <- childEnv e

                tyVars <- forM typeVariables $ \tv ->
                    resolveTypeIdent env NewTypesAreQuantified (TypeIdent tv [])

                typeDef <- insertDataType (OtherModule importName) dname importName typeVariables variants

                dataType <- newIORef $ TUserType typeDef tyVars

                forM_ variants $ \(Variant vname params) -> do
                    vt <- case params of
                        [] ->
                            return dataType
                        _ -> do
                            paramTys <- forM params $ \param ->
                                resolveTypeIdent env NewTypesAreErrors param
                            newIORef $ TFun paramTys dataType

                    HashTable.insert vname (OtherModule importName vname, LImmutable, vt) (eBindings e)

            DJSData name moduleName variants -> do
                let encodeJSVariant (JSVariant n _) = TVariant n []
                tr <- newIORef $ TUserType (TUserTypeDef name moduleName [] $ map encodeJSVariant variants) []
                HashTable.insert name (OtherModule importName name, tr) typeEnv

                forM_ variants $ \(JSVariant vname _) -> do
                    HashTable.insert vname (OtherModule importName vname, LImmutable, tr) (eBindings e)

            DType (TypeAlias _name _params _ident) -> do
                fail "TODO: export type alias"

    typeAliasesRef <- HashTable.new

    -- First, populate the type environment.  Variant parameter types are all initially free.
    forM_ (mDecls modul) $ \(Declaration _ _pos decl) -> case decl of
        DDeclare _name _typeIdent -> do
            return ()

        DData name moduleName typeVarNames variants ->
            void $ insertDataType ThisModule name moduleName typeVarNames variants

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
            modifyIORef' typeEnv $ HashMap.insert name (ThisModule name, userType)

        DType ty@(TypeAlias name _ _) -> do
            HashTable.insert name ty typeAliasesRef

        DFun _ -> do
            return ()

        DLet _ _ _ _ _ -> do
            return ()

    qvars <- readIORef qvarsRef
    te <- readIORef typeEnv
    ta <- readIORef typeAliasesRef
    let env = e{eTypeBindings=typeEnv, eTypeAliases=ta}

    -- Second, unify parameter types
    forM_ (mDecls modul) $ \(Declaration exportFlag declPos decl) -> case decl of
        DData name _ _typeVarNames variants -> do
            Just (_, ty) <- HashTable.lookup name typeEnv
            TUserType typeDef _ <- readIORef ty

            let TUserTypeDef { tuVariants = tvariants } = typeDef
            forM_ (zip variants tvariants) $ \(v, tv) -> do
                forM_ (zip (vparameters v) (tvParameters tv)) $ \(typeIdent, typeVar) -> do
                    let Just qv = HashMap.lookup name qvars
                    env' <- addQvarTable env qv

                    case (exportFlag, typeIdent) of
                        (Export, TypeIdent tiName _)
                            | Just (Declaration NoExport dpos _) <- findDeclByName modul tiName ->
                                throwIO $ ExportError declPos $
                                    printf "Variant %s of exported data type %s depends on nonexported data type %s (defined at %s)"
                                        (show $ vname v) (show name) (show tiName) (formatPos dpos)
                        _ ->
                            return ()

                    t <- resolveTypeIdent env' NewTypesAreErrors typeIdent
                    unify typeVar t
        _ -> return ()

    let computeVariantType ty qvarNames _name argTypeIdents = case argTypeIdents of
            [] ->
                return ty
            _ -> do
                env' <- addQvarTable env qvarNames
                resolvedArgTypes <- mapM (resolveTypeIdent env' NewTypesAreErrors) argTypeIdents
                newIORef $ TFun resolvedArgTypes ty

    intrinsics <- Intrinsic.intrinsics

    forM_ (HashMap.toList intrinsics) $ \(name, intrin) -> do
        let Intrinsic{..} = intrin
        HashTable.insert name (Builtin name, LImmutable, iType) (eBindings env)

    -- Note to self: Here we need to match the names of the types of each variant up with concrete types, but also
    -- with the TypeVars created in the type environment.
    forM_ (mDecls modul) $ \(Declaration _ _ decl) -> case decl of
        DDeclare name typeIdent -> do
            t <- resolveTypeIdent env NewTypesAreQuantified typeIdent
            HashTable.insert name (ThisModule name, LImmutable, t) (eBindings env)
        DData name _ _ variants -> do
            let Just (_, userType) = HashMap.lookup name te
            let Just qvarTable = HashMap.lookup name qvars
            forM_ variants $ \(Variant vname vdata) -> do
                ctorType <- computeVariantType userType qvarTable vname vdata
                HashTable.insert vname (ThisModule vname, LImmutable, ctorType) (eBindings env)
        DJSData name _ variants -> do
            let Just (_, userType) = HashMap.lookup name te
            forM_ variants $ \(JSVariant variantName _value) -> do
                HashTable.insert variantName (ThisModule variantName, LImmutable, userType) (eBindings env)
        _ -> return ()

    return env
