{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Sneak.Typecheck.Unify where

import           Data.List              (nub, sort)
import qualified Data.Text              as Text
import           Sneak.AST
import qualified Sneak.MutableHashTable as HashTable
import           Sneak.Prelude
import           Sneak.Typecheck.Types
import           Text.Printf            (printf)

freshType :: Env -> IO TypeVar
freshType Env{eNextTypeIndex} = do
    modifyIORef' eNextTypeIndex (+1)
    index <- readIORef eNextTypeIndex
    newIORef $ TVar index (Unbound index)

instantiateUserType :: IORef (HashMap Int TypeVar) -> Env -> TUserTypeDef TypeVar -> [TypeVar] -> IO (TypeVar, [TVariant TypeVar])
instantiateUserType subst env def tyVars = do
    typeVars' <- mapM (instantiate' subst env) tyVars
    let typeVars'' = if and (map fst typeVars')
            then map snd typeVars'
            else tyVars
    userType <- newIORef $ TUserType def typeVars''
    variants <- forM (tuVariants def) $ \variant -> do
        paramTypes <- forM (tvParameters variant) $ \param -> do
            instantiate' subst env param
        return variant{tvParameters=map snd paramTypes}
    return (userType, variants)

instantiate' :: IORef (HashMap Int TypeVar) -> Env -> TypeVar -> IO (Bool, TypeVar)
instantiate' subst env ty = do
    ty' <- readIORef ty
    case ty' of
        TQuant name -> do
            mv <- HashTable.lookup name subst
            case mv of
                Just v ->
                    return (True, v)
                Nothing -> do
                    tv <- freshType env
                    HashTable.insert name tv subst
                    return (True, tv)
        TVar _ vl -> do
            case vl of
                Link tv'  -> instantiate' subst env tv'
                Unbound _ -> return (False, ty)
        TFun param ret -> do
            ty1 <- mapM (instantiate' subst env) param
            (b2, ty2) <- instantiate' subst env ret
            let b = b2 || and (map fst ty1)
            tfun <- newIORef $ TFun (map snd ty1) ty2
            return (b, tfun)
        TUserType def tyVars -> do
            typeVars' <- mapM (instantiate' subst env) tyVars
            tut <- newIORef $ TUserType def (map snd typeVars')
            return (and (map fst typeVars'), tut)
        TRecord (RecordType open rows) -> do
            rows' <- forM rows $ \TypeRow{..} -> do
                (b, rowTy') <- instantiate' subst env trTyVar
                let mut' = case trMut of
                        RQuantified -> RFree
                        _ -> trMut
                return (b, TypeRow{trName, trMut=mut', trTyVar=rowTy'})

            let (changedOpen, open') = case open of
                    RecordQuantified -> (True, RecordFree)
                    _ -> (False, open)
            tr <- newIORef $ TRecord $ RecordType open' (map snd rows')
            return (changedOpen || or (map fst rows'), tr)
        TPrimitive {} -> return (False, ty)

quantify :: TypeVar -> IO ()
quantify ty = do
    ty' <- readIORef ty
    case ty' of
        TVar i tv' -> do
            case tv' of
                Unbound j -> do
                    qTy <- newIORef $ TQuant j
                    writeIORef ty (TVar i $ Link qTy)
                Link t' ->
                    quantify t'
        TFun param ret -> do
            mapM_ quantify param
            quantify ret
        TRecord (RecordType open rows') -> do
            forM_ rows' $ \TypeRow{..} -> do
                quantify trTyVar
            let open' = case open of
                    RecordFree -> RecordQuantified
                    _ -> open
            writeIORef ty $ TRecord $ RecordType open' rows'
        _ ->
            return ()

instantiate :: Env -> TypeVar -> IO TypeVar
instantiate env t = do
    subst <- HashTable.new
    (didAnything, t') <- instantiate' subst env t
    return $ if didAnything
        then t'
        else t

occurs :: VarLink -> TypeVar -> IO ()
occurs tvr ty = do
    tyy <- readIORef ty
    case tyy of
        TVar _ ty''
            | tvr == ty'' -> do
                error $ "Occurs check failed"
            | otherwise -> do
                case ty'' of
                    Link ty''' -> occurs tvr ty'''
                    _ -> return ()
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
    sa <- showTypeVarIO a
    sb <- showTypeVarIO b
    error $ "Unification error: " ++ message ++ " " ++ sa ++ " and " ++ sb

lookupTypeRow :: Name -> [TypeRow t] -> Maybe (RowMutability, t)
lookupTypeRow name rows = case rows of
    [] -> Nothing
    (TypeRow{..}:rest)
        | trName == name -> Just (trMut, trTyVar)
        | otherwise -> lookupTypeRow name rest

unifyRecord :: TypeVar -> TypeVar -> IO ()
unifyRecord av bv = do
    TRecord a <- readIORef av
    TRecord b <- readIORef bv
    let RecordType aOpen aRows = a
        RecordType bOpen bRows = b
    let aFields = sort $ map trName aRows
    let bFields = sort $ map trName bRows

    when (aOpen == RecordQuantified || bOpen == RecordQuantified) $
        error "Internal error: Encountered a quantified record.  This should have been instantiated away"

    let aRequired = RecordClose == aOpen
        bRequired = RecordClose == bOpen

    let allKeys = nub $ sort (aFields ++ bFields)
    newFields <- forM allKeys $ \key -> do
        case (aRequired, lookupTypeRow key aRows, bRequired, lookupTypeRow key bRows) of
            (_, Just (m1, t1), _, Just (m2, t2)) -> do
                case unifyRecordMutability m1 m2 of
                    Left err -> error $ printf "Could not unify mutability of record field %s: %s" (show key) err
                    Right mut -> do
                        unify t1 t2
                        return TypeRow{trName=key, trMut=mut, trTyVar=t1}

            (_    , Just (m1, t1), False, Nothing) ->
                return TypeRow{trName=key, trMut=m1, trTyVar=t1}
            (False, Nothing, _    , Just (m2, t2)) ->
                return TypeRow{trName=key, trMut=m2, trTyVar=t2}

            (True, Nothing, _, _) ->
                unificationError ("Field '" ++ Text.unpack key ++ "' not found in quantified record") av bv

            (_, _, True, Nothing) ->
                unificationError ("Field '" ++ Text.unpack key ++ "' not found in quantified record") av bv

            (False, Nothing, False, Nothing) -> do
                error "Internal error in unifyRecord: This should be very impossible"

    let open' = case (aOpen, bOpen) of
            (RecordClose, _)         -> RecordClose
            (_, RecordClose)         -> RecordClose
            (RecordQuantified, _)    -> RecordQuantified
            (_, RecordQuantified)    -> RecordQuantified
            (RecordFree, RecordFree) -> RecordFree

    writeIORef av $ TRecord $ RecordType open' newFields

    let i = 888 -- hack
    writeIORef bv (TVar i $ Link av)

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
            (TVar aid _, TVar bid _)
                | aid == bid ->
                    return ()

            (_, TVar _ (Link bl)) ->
                unify av bl
            (TVar _ (Link al), _) ->
                unify al bv

            (TVar i a'@(Unbound _), _) -> do
                occurs a' bv
                writeIORef av (TVar i $ Link bv)

            (_, TVar {}) -> do
                unify bv av

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

            -- These should never happen: Quantified type variables should be instantiated before we get here.
            (TQuant {}, _) -> do
                lt <- showTypeVarIO av
                rt <- showTypeVarIO bv
                error $ printf "Internal error: QVar made it to unify %s and %s" lt rt
            (_, TQuant {}) -> do
                lt <- showTypeVarIO av
                rt <- showTypeVarIO bv
                error $ printf "Internal error: QVar made it to unify %s and %s" lt rt

            _ ->
                unificationError "" av bv
