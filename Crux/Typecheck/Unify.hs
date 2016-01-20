{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Crux.Typecheck.Unify where

import           Crux.AST
import qualified Crux.MutableHashTable as HashTable
import           Crux.Prelude
import           Crux.Typecheck.Types
import           Data.List             (sort)
import           Text.Printf           (printf)

freshTypeIndex :: Env -> IO Int
freshTypeIndex Env{eNextTypeIndex} = do
    modifyIORef' eNextTypeIndex (+1)
    readIORef eNextTypeIndex

freshType :: Env -> IO TypeVar
freshType env = do
    index <- freshTypeIndex env
    newIORef $ TUnbound index

freshRowVariable :: Env -> IO RowVariable
freshRowVariable env =
    RowVariable <$> freshTypeIndex env

data RecordSubst
    = SFree RowVariable
    | SQuant RowVariable
    | SRows [TypeRow TypeVar]

instantiateUserType :: IORef (HashMap Int TypeVar) -> Env -> TUserTypeDef TypeVar -> [TypeVar] -> IO (TypeVar, [TVariant TypeVar])
instantiateUserType subst env def tyVars = do
    recordSubst <- HashTable.new
    typeVars' <- mapM (instantiate' subst recordSubst env) tyVars
    let typeVars'' = if and (map fst typeVars')
            then map snd typeVars'
            else tyVars
    userType <- newIORef $ TUserType def typeVars''
    variants <- forM (tuVariants def) $ \variant -> do
        paramTypes <- forM (tvParameters variant) $ \param -> do
            instantiate' subst recordSubst env param
        return variant{tvParameters=map snd paramTypes}
    return (userType, variants)

instantiateRecord
    :: IORef (HashMap Int TypeVar)
    -> IORef (HashMap RowVariable TypeVar)
    -> Env
    -> [TypeRow TypeVar]
    -> RecordOpen
    -> IO (Bool, IORef MutableTypeVar)
instantiateRecord subst recordSubst env rows open = do
    rows' <- forM rows $ \TypeRow{..} -> do
        (b, rowTy') <- instantiate' subst recordSubst env trTyVar
        let mut' = case trMut of
                RQuantified -> RFree
                _ -> trMut
        return (b, TypeRow{trName, trMut=mut', trTyVar=rowTy'})

    (changedOpen, open') <- case open of
        RecordQuantified i -> do
            -- ???
            return (True, RecordFree i)
        _ -> do
            return (False, open)

    tr <- newIORef $ TRecord $ RecordType open' (map snd rows')
    return (changedOpen || or (map fst rows'), tr)

instantiate' :: IORef (HashMap Int TypeVar) -> IORef (HashMap RowVariable TypeVar) -> Env -> TypeVar -> IO (Bool, TypeVar)
instantiate' subst recordSubst env ty = do
    ty' <- readIORef ty
    case ty' of
        TUnbound _ -> do
            return (False, ty)
        TBound tv' -> do
            instantiate' subst recordSubst env tv'
        TQuant name -> do
            mv <- HashTable.lookup name subst
            case mv of
                Just v ->
                    return (True, v)
                Nothing -> do
                    tv <- freshType env
                    HashTable.insert name tv subst
                    return (True, tv)
        TFun param ret -> do
            ty1 <- mapM (instantiate' subst recordSubst env) param
            (b2, ty2) <- instantiate' subst recordSubst env ret
            let b = b2 || any fst ty1
            tfun <- newIORef $ TFun (map snd ty1) ty2
            return (b, tfun)
        TUserType def tyVars -> do
            typeVars' <- mapM (instantiate' subst recordSubst env) tyVars
            tut <- newIORef $ TUserType def (map snd typeVars')
            return (and (map fst typeVars'), tut)
        TRecord (RecordType open rows) -> do
            let rv = case open of
                    RecordFree r -> Just r
                    RecordQuantified r -> Just r
                    _ -> Nothing
            case rv of
                Just rv' -> do
                    r <- HashTable.lookup rv' recordSubst
                    case r of
                        Just rec -> return (True, rec)
                        Nothing -> do
                            (co, tr) <- instantiateRecord subst recordSubst env rows open
                            HashTable.insert rv' tr recordSubst
                            return (co, tr)
                Nothing ->
                    instantiateRecord subst recordSubst env rows open

        TPrimitive {} -> return (False, ty)

quantify :: TypeVar -> IO ()
quantify ty = do
    ty' <- readIORef ty
    case ty' of
        TUnbound i -> do
            writeIORef ty (TQuant i)
        TBound t -> do
            quantify t
        TQuant _ -> do
            return ()
        TFun param ret -> do
            mapM_ quantify param
            quantify ret
        TUserType _ tyParams ->
            forM_ tyParams quantify
        TRecord (RecordType open rows') -> do
            forM_ rows' $ \TypeRow{..} -> do
                quantify trTyVar
            let open' = case open of
                    RecordFree ti -> RecordQuantified ti
                    _             -> open

            writeIORef ty $ TRecord $ RecordType open' rows'
        TPrimitive {} ->
            return ()

instantiate :: Env -> TypeVar -> IO TypeVar
instantiate env t = do
    subst <- HashTable.new
    recordSubst <- HashTable.new
    (didAnything, t') <- instantiate' subst recordSubst env t

    return $ if didAnything
        then t'
        else t

occurs :: MutableTypeVar -> TypeVar -> IO ()
occurs tvr ty = do
    ty' <- readIORef ty
    case ty' of
        TUnbound _
            | ty' == tvr -> do
                throwIO $ OccursCheckFailed ()
            | otherwise -> do
                return ()
        TBound ty''
            | ty' == tvr -> do
                throwIO $ OccursCheckFailed ()
            | otherwise -> do
                occurs tvr ty''
        TFun arg ret -> do
            mapM_ (occurs tvr) arg
            occurs tvr ret
        TUserType _ tvars -> do
            mapM_ (occurs tvr) tvars
        TRecord (RecordType _ rows) -> do
            forM_ rows $ \TypeRow{..} ->
                occurs tvr trTyVar
        TPrimitive {} ->
            return ()
        TQuant {} ->
            return ()

unificationError :: [Char] -> TypeVar -> TypeVar -> IO a
unificationError message a b = do
    throwIO $ UnificationError () message a b

lookupTypeRow :: Name -> [TypeRow t] -> Maybe (RowMutability, t)
lookupTypeRow name rows = case rows of
    [] -> Nothing
    (TypeRow{..}:rest)
        | trName == name -> Just (trMut, trTyVar)
        | otherwise -> lookupTypeRow name rest

unifyRecord :: TypeVar -> TypeVar -> IO ()
unifyRecord av bv = do
    -- do
    --     putStrLn " -- unifyRecord --"
    --     putStr "\t" >> showTypeVarIO av >>= putStrLn
    --     putStr "\t" >> showTypeVarIO bv >>= putStrLn

    TRecord (RecordType aOpen aRows) <- readIORef av
    TRecord (RecordType bOpen bRows) <- readIORef bv
    let aFields = sort $ map trName aRows
    let bFields = sort $ map trName bRows

    let coincidentRows = [(a, b) | a <- aRows, b <- bRows, trName a == trName b]
    let aOnlyRows = filter (\row -> trName row `notElem` bFields) aRows
    let bOnlyRows = filter (\row -> trName row `notElem` aFields) bRows
    let names trs = map trName trs

    coincidentRows' <- forM coincidentRows $ \(lhs, rhs) -> do
        case unifyRecordMutability (trMut lhs) (trMut rhs) of
            Left err -> throwIO $ RecordMutabilityUnificationError () (trName lhs) err
            Right mut -> do
                unify (trTyVar lhs) (trTyVar rhs)
                return TypeRow
                    { trName = trName lhs
                    , trMut = mut
                    , trTyVar = trTyVar lhs
                    }

    case (aOpen, bOpen) of
        (RecordClose, RecordClose)
            | null aOnlyRows && null bOnlyRows -> do
                writeIORef bv (TRecord $ RecordType RecordClose coincidentRows')
                writeIORef av (TBound bv)
            | otherwise ->
                unificationError "Closed row types must match exactly" av bv
        (RecordClose, RecordFree {})
            | null bOnlyRows -> do
                writeIORef av (TRecord $ RecordType RecordClose (coincidentRows' ++ aOnlyRows))
                writeIORef bv (TBound av)
            | otherwise ->
                unificationError (printf "Record has fields %s not in closed record" (show $ names bOnlyRows)) av bv
        (RecordFree {}, RecordClose)
            | null aOnlyRows -> do
                writeIORef bv (TRecord $ RecordType RecordClose (coincidentRows' ++ bOnlyRows))
                writeIORef av (TBound bv)
            | otherwise ->
                unificationError (printf "Record has fields %s not in closed record" (show $ names aOnlyRows)) av bv
        (RecordClose, RecordQuantified {}) ->
            error "Cannot unify closed record with quantified record"
        (RecordQuantified {}, RecordClose) ->
            error "Cannot unify closed record with quantified record"
        (RecordFree {}, RecordFree {}) -> do
            writeIORef bv (TRecord $ RecordType aOpen (coincidentRows' ++ aOnlyRows ++ bOnlyRows))
            writeIORef av (TBound bv)
        (RecordFree {}, RecordQuantified {})
            | null aOnlyRows -> do
                writeIORef bv (TRecord $ RecordType bOpen (coincidentRows' ++ bOnlyRows))
                writeIORef av (TBound bv)
            | otherwise ->
                error "lhs record has rows not in quantified record"
        (RecordQuantified {}, RecordFree {})
            | null bOnlyRows -> do
                writeIORef av (TRecord $ RecordType aOpen (coincidentRows' ++ aOnlyRows))
                writeIORef bv (TBound av)
            | otherwise ->
                error "rhs record has rows not in quantified record"
        (RecordQuantified a, RecordQuantified b)
            | not (null aOnlyRows) ->
                error "lhs quantified record has rows not in rhs quantified record"
            | not (null bOnlyRows) ->
                error "rhs quantified record has rows not in lhs quantified record"
            | a /= b ->
                error "Quantified records do not have the same qvar!"
            | otherwise -> do
                writeIORef av (TRecord $ RecordType aOpen coincidentRows')
                writeIORef av (TBound bv)

    -- do
    --     putStrLn "\t -- after --"
    --     putStr "\t" >> showTypeVarIO av >>= putStrLn
    --     putStr "\t" >> showTypeVarIO bv >>= putStrLn
    --     putStrLn ""

unifyRecordMutability :: RowMutability -> RowMutability -> Either Prelude.String RowMutability
unifyRecordMutability m1 m2 = case (m1, m2) of
    (RImmutable, RImmutable) -> Right RImmutable
    (RImmutable, RMutable) -> Left "Record field mutability does not match"
    (RImmutable, RFree) -> Right RImmutable
    (RMutable, RMutable) -> Right RMutable
    (RMutable, RImmutable) -> Left "Record field mutability does not match"
    (RMutable, RFree) -> Right RMutable
    (RFree, RFree) -> Right RFree
    (RFree, RImmutable) -> Right RImmutable
    (RFree, RMutable) -> Right RMutable
    (RQuantified, _) -> Left "Quant!! D:"
    (_, RQuantified) -> Left "Quant2!! D:"

unify :: TypeVar -> TypeVar -> IO ()
unify av bv
    | av == bv =
        return ()
    | otherwise = do
        a <- readIORef av
        b <- readIORef bv

        case (a, b) of
            (TUnbound aid, TUnbound bid) | aid == bid -> return ()
            (_, TBound bl) -> unify av bl
            (TBound al, _) -> unify al bv
            (TUnbound _, _) -> do
                occurs a bv
                writeIORef av $ TBound bv

            (_, TUnbound {}) -> unify bv av
            --(_, TBound {}) -> unify bv av

            (TPrimitive aType, TPrimitive bType)
                | aType == bType ->
                    return ()
                | otherwise -> do
                    unificationError "" av bv

            (TUserType ad atv, TUserType bd btv)
                | tuName ad == tuName bd -> do
                    mapM_ (uncurry unify) (zip atv btv)
                | otherwise -> do
                    unificationError "" av bv

            (TRecord {}, TRecord {}) ->
                unifyRecord av bv

            (TFun aa ar, TFun ba br) -> do
                when (length aa /= length ba) $
                    unificationError "" av bv

                mapM_ (uncurry unify) (zip aa ba)
                unify ar br

            (TFun {}, TPrimitive {}) ->
                unificationError "" av bv
            (TPrimitive {}, TFun {}) ->
                unificationError "" av bv

            (TQuant i, TQuant j) | i == j ->
                return ()

            _ ->
                unificationError "" av bv
