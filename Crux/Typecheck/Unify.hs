{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Crux.Typecheck.Unify where

import           Crux.AST
import qualified Crux.MutableHashTable as HashTable
import           Crux.Prelude
import           Crux.Typecheck.Types
import           Data.List             (sort)
import           Text.Printf           (printf)
import Crux.TypeVar

freshTypeIndex :: Env -> IO Int
freshTypeIndex Env{eNextTypeIndex} = do
    modifyIORef' eNextTypeIndex (+1)
    readIORef eNextTypeIndex

freshType :: Env -> IO TypeVar
freshType env = do
    index <- freshTypeIndex env
    newTypeVar $ TUnbound index

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
    typeVars' <- for tyVars $ instantiate' subst recordSubst env
    let typeVars'' = if and (map fst typeVars')
            then map snd typeVars'
            else tyVars
    userType <- newTypeVar $ TUserType def typeVars''
    variants <- for (tuVariants def) $ \variant -> do
        paramTypes <- for (tvParameters variant) $ \param -> do
            instantiate' subst recordSubst env param
        return variant{tvParameters=map snd paramTypes}
    return (userType, variants)

instantiateRecord
    :: IORef (HashMap Int TypeVar)
    -> IORef (HashMap RowVariable TypeVar)
    -> Env
    -> [TypeRow TypeVar]
    -> RecordOpen
    -> IO (Bool, TypeVar)
instantiateRecord subst recordSubst env rows open = do
    rows' <- for rows $ \TypeRow{..} -> do
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

    reft <- newIORef $ RRecord $ RecordType open' (map snd rows')
    tr <- newTypeVar $ TRecord $ reft
    return (changedOpen || or (map fst rows'), tr)

instantiate' :: IORef (HashMap Int TypeVar) -> IORef (HashMap RowVariable TypeVar) -> Env -> TypeVar -> IO (Bool, TypeVar)
instantiate' subst recordSubst env ty = do
    readTypeVar ty >>= \case
        TUnbound _ -> do
            return (False, ty)
        TBound tv' -> do
            instantiate' subst recordSubst env tv'
        TQuant name -> do
            HashTable.lookup name subst >>= \case
                Just v ->
                    return (True, v)
                Nothing -> do
                    tv <- freshType env
                    HashTable.insert name tv subst
                    return (True, tv)
        TFun param ret -> do
            ty1 <- for param $ instantiate' subst recordSubst env
            (b2, ty2) <- instantiate' subst recordSubst env ret
            let b = b2 || any fst ty1
            tfun <- newTypeVar $ TFun (map snd ty1) ty2
            return (b, tfun)
        TUserType def tyVars -> do
            typeVars' <- for tyVars $ instantiate' subst recordSubst env
            tut <- newTypeVar $ TUserType def (map snd typeVars')
            return (and (map fst typeVars'), tut)
        TRecord ref' -> followRecordTypeVar ref' >>= \(RecordType open rows) -> do
            let rv = case open of
                    RecordFree r -> Just r
                    RecordQuantified r -> Just r
                    _ -> Nothing
            case rv of
                Just rv' -> do
                    HashTable.lookup rv' recordSubst >>= \case
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
    readTypeVar ty >>= \case
        TUnbound i -> do
            writeTypeVar ty (TQuant i)
        TBound t -> do
            quantify t
        TQuant _ -> do
            return ()
        TFun param ret -> do
            for_ param quantify
            quantify ret
        TUserType _ tyParams ->
            for_ tyParams quantify
        TRecord ref -> followRecordTypeVar ref >>= \(RecordType open rows) -> do
            for_ rows $ \TypeRow{..} -> do
                quantify trTyVar
            case open of
                RecordFree ti -> do
                    writeIORef ref $ RRecord $ RecordType (RecordQuantified ti) rows
                _ -> return ()
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
    ty' <- readTypeVar ty
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
            for_ arg $ occurs tvr
            occurs tvr ret
        TUserType _ tvars -> do
            for_ tvars $ occurs tvr
        TRecord ref -> followRecordTypeVar ref >>= \(RecordType _open rows) -> do
            for_ rows $ \TypeRow{..} ->
                occurs tvr trTyVar
        TPrimitive {} ->
            return ()
        TQuant {} ->
            return ()

unificationError :: [Char] -> TypeVar -> TypeVar -> IO a
unificationError message a b = do
    throwIO $ UnificationError () message a b

lookupTypeRow :: Name -> [TypeRow t] -> Maybe (RowMutability, t)
lookupTypeRow name = \case
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

    TRecord aRef <- readTypeVar av
    TRecord bRef <- readTypeVar bv

    RecordType aOpen aRows <- followRecordTypeVar aRef
    RecordType bOpen bRows <- followRecordTypeVar bRef

    let aFields = sort $ map trName aRows
    let bFields = sort $ map trName bRows

    let coincidentRows = [(a, b) | a <- aRows, b <- bRows, trName a == trName b]
    let aOnlyRows = filter (\row -> trName row `notElem` bFields) aRows
    let bOnlyRows = filter (\row -> trName row `notElem` aFields) bRows
    let names trs = map trName trs

    coincidentRows' <- for coincidentRows $ \(lhs, rhs) -> do
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
                writeIORef bRef $ RRecord $ RecordType RecordClose coincidentRows'
                writeIORef aRef $ RBound $ bRef
            | otherwise ->
                unificationError "Closed row types must match exactly" av bv
        (RecordClose, RecordFree {})
            | null bOnlyRows -> do
                writeIORef aRef $ RRecord $ RecordType RecordClose (coincidentRows' ++ aOnlyRows)
                writeIORef bRef $ RBound $ aRef
            | otherwise ->
                unificationError (printf "Record has fields %s not in closed record" (show $ names bOnlyRows)) av bv
        (RecordFree {}, RecordClose)
            | null aOnlyRows -> do
                writeIORef bRef $ RRecord $ RecordType RecordClose (coincidentRows' ++ bOnlyRows)
                writeIORef aRef $ RBound bRef
            | otherwise ->
                unificationError (printf "Record has fields %s not in closed record" (show $ names aOnlyRows)) av bv
        (RecordClose, RecordQuantified {}) ->
            error "Cannot unify closed record with quantified record"
        (RecordQuantified {}, RecordClose) ->
            error "Cannot unify closed record with quantified record"
        (RecordFree {}, RecordFree {}) -> do
            writeIORef bRef $ RRecord $ RecordType aOpen (coincidentRows' ++ aOnlyRows ++ bOnlyRows)
            writeIORef aRef $ RBound bRef
        (RecordFree {}, RecordQuantified {})
            | null aOnlyRows -> do
                writeIORef bRef $ RRecord $ RecordType bOpen (coincidentRows' ++ bOnlyRows)
                writeIORef aRef $ RBound bRef
            | otherwise ->
                error "lhs record has rows not in quantified record"
        (RecordQuantified {}, RecordFree {})
            | null bOnlyRows -> do
                writeIORef aRef $ RRecord $ RecordType aOpen (coincidentRows' ++ aOnlyRows)
                writeIORef bRef $ RBound aRef
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
                writeIORef aRef $ RRecord $ RecordType aOpen coincidentRows'
                -- Is this a bug? I copied it verbatim from what was here. -- chad
                writeIORef aRef $ RBound bRef
                {-
                writeTypeVar av (TRecord $ RecordType aOpen coincidentRows')
                writeTypeVar av (TBound bv)
                -}

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
        a <- readTypeVar av
        b <- readTypeVar bv

        case (a, b) of
            (TUnbound aid, TUnbound bid) | aid == bid -> return ()
            (_, TBound bl) -> unify av bl
            (TBound al, _) -> unify al bv
            (TUnbound _, _) -> do
                occurs a bv
                writeTypeVar av $ TBound bv

            (_, TUnbound {}) -> unify bv av
            --(_, TBound {}) -> unify bv av

            (TPrimitive aType, TPrimitive bType)
                | aType == bType ->
                    return ()
                | otherwise -> do
                    unificationError "" av bv

            (TUserType ad atv, TUserType bd btv)
                | userTypeIdentity ad == userTypeIdentity bd -> do
                    -- TODO: assert the two lists have the same length
                    for_ (zip atv btv) $ uncurry unify
                | otherwise -> do
                    unificationError "" av bv

            (TRecord {}, TRecord {}) ->
                unifyRecord av bv

            (TFun aa ar, TFun ba br) -> do
                when (length aa /= length ba) $
                    unificationError "" av bv

                for_ (zip aa ba) $ uncurry unify
                unify ar br

            (TFun {}, TPrimitive {}) ->
                unificationError "" av bv
            (TPrimitive {}, TFun {}) ->
                unificationError "" av bv

            (TQuant i, TQuant j) | i == j ->
                return ()

            _ ->
                unificationError "" av bv
