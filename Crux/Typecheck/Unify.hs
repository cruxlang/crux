{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Crux.Typecheck.Unify where

import           Crux.AST
import qualified Crux.MutableHashTable as HashTable
import           Crux.Prelude
import           Crux.Typecheck.Types
import           Data.List             (sort)
import           Text.Printf           (printf)
import Crux.TypeVar
import Crux.Error
import Crux.Typecheck.Monad

freshTypeIndex :: MonadIO m => Env -> m Int
freshTypeIndex Env{eNextTypeIndex} = do
    modifyIORef' eNextTypeIndex (+1)
    readIORef eNextTypeIndex

freshType :: MonadIO m => Env -> m TypeVar
freshType env = do
    index <- freshTypeIndex env
    newTypeVar $ TUnbound Strong (eLevel env) index

freshWeakQVar :: MonadIO m => Env -> m TypeVar
freshWeakQVar env = do
    index <- freshTypeIndex env
    newTypeVar $ TUnbound Weak (eLevel env) index

freshRowVariable :: MonadIO m => Env -> m RowVariable
freshRowVariable env =
    RowVariable <$> freshTypeIndex env

data RecordSubst
    = SFree RowVariable
    | SQuant RowVariable
    | SRows [TypeRow TypeVar]

instantiateUserTypeDef' :: MonadIO m => IORef (HashMap Int TypeVar) -> IORef (HashMap RowVariable TypeVar) -> Env -> TUserTypeDef TypeVar -> m (TUserTypeDef TypeVar)
instantiateUserTypeDef' subst recordSubst env def = do
    for def $ instantiate' subst recordSubst env

instantiateAll :: (MonadIO m, Traversable c) => Env -> c TypeVar -> m (c TypeVar)
instantiateAll env container = do
    subst <- HashTable.new
    recordSubst <- HashTable.new
    for container $ instantiate' subst recordSubst env

instantiateUserTypeDef :: MonadIO m => Env -> TUserTypeDef TypeVar -> m (TUserTypeDef TypeVar)
instantiateUserTypeDef env def = do
    subst <- HashTable.new
    recordSubst <- HashTable.new
    instantiateUserTypeDef' subst recordSubst env def

instantiateRecord
    :: MonadIO m
    => IORef (HashMap Int TypeVar)
    -> IORef (HashMap RowVariable TypeVar)
    -> Env
    -> [TypeRow TypeVar]
    -> RecordOpen
    -> m TypeVar
instantiateRecord subst recordSubst env rows open = do
    rows' <- for rows $ \TypeRow{..} -> do
        rowTy' <- instantiate' subst recordSubst env trTyVar
        let mut' = case trMut of
                RQuantified -> RFree
                _ -> trMut
        return TypeRow{trName, trMut=mut', trTyVar=rowTy'}

    open' <- case open of
        RecordQuantified i -> do
            return $ RecordFree i
        RecordFree _ -> do
            fail $ "Instantiation of a free row variable -- this should never happen"
        RecordClose -> do
            return $ RecordClose

    recordType <- newIORef $ RRecord $ RecordType open' rows'
    return $ TRecord recordType

instantiate' :: MonadIO m => IORef (HashMap Int TypeVar) -> IORef (HashMap RowVariable TypeVar) -> Env -> TypeVar -> m TypeVar
instantiate' subst recordSubst env ty = case ty of
    TypeVar ref -> do
        readIORef ref >>= \case
            TUnbound {} -> do
                -- We instantiate unbound type variables when recursively
                -- referencing a function whose parameter and return types are
                -- not yet quantified.
                return ty
            TBound tv' -> do
                instantiate' subst recordSubst env tv'
    TQuant name -> do
        HashTable.lookup name subst >>= \case
            Just v ->
                return v
            Nothing -> do
                tv <- freshType env
                HashTable.insert name tv subst
                return tv
    TFun param ret -> do
        ty1 <- for param $ instantiate' subst recordSubst env
        ty2 <- instantiate' subst recordSubst env ret
        return $ TFun ty1 ty2
    TUserType def -> do
        typeVars' <- for (tuParameters def) $ instantiate' subst recordSubst env
        return $ TUserType def{ tuParameters = typeVars' }
    TRecord ref' -> followRecordTypeVar ref' >>= \(RecordType open rows) -> do
        let rv = case open of
                RecordFree r -> Just r
                RecordQuantified r -> Just r
                _ -> Nothing
        case rv of
            Just rv' -> do
                HashTable.lookup rv' recordSubst >>= \case
                    Just rec -> return rec
                    Nothing -> do
                        tr <- instantiateRecord subst recordSubst env rows open
                        HashTable.insert rv' tr recordSubst
                        return tr
            Nothing ->
                instantiateRecord subst recordSubst env rows open

    TPrimitive {} -> return ty
    TTypeFun {} -> fail "TODO: instantiate on typefun"

quantify :: MonadIO m => TypeVar -> m ()
quantify ty = case ty of
    TypeVar ref -> do
        readIORef ref >>= \case
            TUnbound Strong _ i -> do
                writeIORef ref $ TBound $ TQuant i
            TUnbound Weak _ _ -> do
                return ()
            TBound t -> do
                quantify t
    TQuant {} -> do
        return ()
    TFun param ret -> do
        for_ param quantify
        quantify ret
    TUserType def ->
        for_ (tuParameters def) quantify
    TRecord ref -> followRecordTypeVar ref >>= \(RecordType open rows) -> do
        for_ rows $ \TypeRow{..} -> do
            quantify trTyVar
        case open of
            RecordFree ti -> do
                writeIORef ref $ RRecord $ RecordType (RecordQuantified ti) rows
            _ -> return ()
    TPrimitive {} ->
        return ()
    TTypeFun {} -> fail "TODO: quantify on TypeFun"

instantiate :: MonadIO m => Env -> TypeVar -> m TypeVar
instantiate env t = do
    subst <- HashTable.new
    recordSubst <- HashTable.new
    instantiate' subst recordSubst env t

typeError :: Pos -> TypeError -> TC a
typeError pos = failError . TypeError pos

occurs :: Pos -> Int -> TypeVar -> TC ()
occurs pos tvn = \case
    TypeVar ref -> readIORef ref >>= \case
        TUnbound _ _ q | tvn == q -> do
            typeError pos OccursCheckFailed
        TUnbound {} -> do
            return ()
        TBound next -> do
            occurs pos tvn next
    TFun arg ret -> do
        for_ arg $ occurs pos tvn
        occurs pos tvn ret
    TUserType def -> do
        for_ (tuParameters def) $ occurs pos tvn
    TRecord ref -> followRecordTypeVar ref >>= \(RecordType _open rows) -> do
        for_ rows $ \TypeRow{..} ->
            occurs pos tvn trTyVar
    TPrimitive {} ->
        return ()
    TQuant {} ->
        return ()
    TTypeFun {} -> fail "TODO: occurs on TypeFun"

unificationError :: Pos -> String -> TypeVar -> TypeVar -> TC a
unificationError pos message a b = do
    typeError pos $ UnificationError message a b

lookupTypeRow :: Name -> [TypeRow t] -> Maybe (RowMutability, t)
lookupTypeRow name = \case
    [] -> Nothing
    (TypeRow{..}:rest)
        | trName == name -> Just (trMut, trTyVar)
        | otherwise -> lookupTypeRow name rest

unifyRecord :: Pos -> TypeVar -> TypeVar -> TC ()
unifyRecord pos av bv = do
    -- do
    --     putStrLn " -- unifyRecord --"
    --     putStr "\t" >> showTypeVarIO av >>= putStrLn
    --     putStr "\t" >> showTypeVarIO bv >>= putStrLn

    let TRecord aRef = av
    let TRecord bRef = bv
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
            Left err -> typeError pos $ RecordMutabilityUnificationError (trName lhs) err
            Right mut -> do
                unify pos (trTyVar lhs) (trTyVar rhs)
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
                unificationError pos "Closed row types must match exactly" av bv
        (RecordClose, RecordFree {})
            | null bOnlyRows -> do
                writeIORef aRef $ RRecord $ RecordType RecordClose (coincidentRows' ++ aOnlyRows)
                writeIORef bRef $ RBound $ aRef
            | otherwise ->
                unificationError pos (printf "Record has fields %s not in closed record" (show $ names bOnlyRows)) av bv
        (RecordFree {}, RecordClose)
            | null aOnlyRows -> do
                writeIORef bRef $ RRecord $ RecordType RecordClose (coincidentRows' ++ bOnlyRows)
                writeIORef aRef $ RBound bRef
            | otherwise ->
                unificationError pos (printf "Record has fields %s not in closed record" (show $ names aOnlyRows)) av bv
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

unify :: Pos -> TypeVar -> TypeVar -> TC ()
unify pos av' bv' = do
    av <- followTypeVar av'
    bv <- followTypeVar bv'
    if av == bv then
        return ()
    else case (av, bv) of
        -- thanks to followTypeVar, the only TypeVar case here is TUnbound
        (TypeVar aref, TypeVar bref) -> do
            (TUnbound _ _ a') <- readIORef aref
            (TUnbound _ _ b') <- readIORef bref
            when (a' /= b') $ do
                occurs pos a' bv
                writeIORef aref $ TBound bv
        (TypeVar aref, _) -> do
            (TUnbound _ _ a') <- readIORef aref
            occurs pos a' bv
            writeIORef aref $ TBound bv
        (_, TypeVar bref) -> do
            (TUnbound _ _ b') <- readIORef bref
            occurs pos b' av
            writeIORef bref $ TBound av

        (TPrimitive aType, TPrimitive bType)
            | aType == bType ->
                return ()
            | otherwise -> do
                unificationError pos "" av bv

        (TUserType ad, TUserType bd)
            | userTypeIdentity ad == userTypeIdentity bd -> do
                -- TODO: assert the two lists have the same length
                for_ (zip (tuParameters ad) (tuParameters bd)) $ uncurry $ unify pos
            | otherwise -> do
                unificationError pos "" av bv

        (TRecord {}, TRecord {}) ->
            unifyRecord pos av bv

        (TFun aa ar, TFun ba br) -> do
            when (length aa /= length ba) $
                unificationError pos "" av bv

            for_ (zip aa ba) $ uncurry $ unify pos
            unify pos ar br

        (TQuant i, TQuant j) | i == j ->
            return ()

        _ ->
            unificationError pos "" av bv
