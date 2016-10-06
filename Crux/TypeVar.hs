{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, DeriveGeneric #-}

module Crux.TypeVar
    ( RecordOpen(..)
    , RecordType(..)
    , RecordTypeVar(..)
    , TypeRow(..)
    , RowMutability(..)
    , RowVariable(..)
    , TVariant(..)
    , TDataTypeIdentity(..)
    , TDataTypeDef(..)
    , TraitDesc(..)
    , TraitIdentity(..)
    , Strength (..)
    , TypeSource(..)
    , TypeNumber
    , TypeVar(..)
    , TypeState(..)
    , TypeLevel(..)
    , newTypeVar
    , followRecordTypeVar'
    , followRecordTypeVar
    , followTypeVar
    , renderTypeVarIO
    , dataTypeIdentity
    ) where

import Crux.ModuleName (ModuleName)
import Crux.Prelude
import qualified Data.Text as Text
import System.IO.Unsafe (unsafePerformIO)
import Crux.Pos
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type Name = Text

-- Per http://okmij.org/ftp/ML/generalization.html
newtype TypeLevel = TypeLevel { unTypeLevel :: Int }
    deriving (Eq, Num, Ord)

instance Show TypeLevel where
    show (TypeLevel i) = show i

data TVariant typevar = TVariant
    { tvName       :: Name
    , tvParameters :: [typevar]
    } deriving (Show, Eq, Functor, Foldable, Traversable)

data TDataTypeDef typevar = TDataTypeDef
    { tuName       :: !Name
    , tuModuleName :: !ModuleName
    , tuParameters :: ![typevar]
    , tuVariants   :: ![TVariant typevar]
    } deriving (Show, Eq, Functor, Foldable, Traversable)

data TDataTypeIdentity = TDataTypeIdentity Name ModuleName
    deriving (Show, Eq, Ord, Generic)

instance Hashable TDataTypeIdentity
    
dataTypeIdentity :: TDataTypeDef a -> TDataTypeIdentity
dataTypeIdentity ut = TDataTypeIdentity (tuName ut) (tuModuleName ut)

data RowMutability
    = RMutable
    | RImmutable
    | RQuantified
    | RFree
    deriving (Show, Eq)

data TypeRow typevar = TypeRow
    { trName  :: Name
    , trMut   :: RowMutability
    , trTyVar :: typevar
    } deriving (Show, Eq, Functor, Foldable, Traversable)

{-
fun hypot(p) { sqrt(p.x * p.x + p.y * p.y); };
let p = { x:9,y:22,z:33 };
let ps = hypot(p);

We instantiate the argument type {x:Number, y:Number, ...} and unify with {x:Number, y:Number, z:Number}
This yields {x:Number, y:Number, z:Number}
-}

newtype RowVariable = RowVariable { unRowVariable :: Int }
    deriving (Eq)

instance Hashable RowVariable where
    hashWithSalt salt (RowVariable i) = hashWithSalt salt i

instance Show RowVariable where
    show (RowVariable i) = show i

instance Num RowVariable where
    (+) = error "Cannot add RowVariable"
    (*) = error "Cannot multiply RowVariable"
    (-) = error "Cannot subtract RowVariable"
    abs = error "Cannot abs RowVariable"
    signum = error "Cannot signum RowVariable"
    fromInteger = RowVariable . fromInteger

-- An open record can be unified with another record type that has extra properties.
-- F u F == Free record with union of properties
-- F u Q == Verify that LHS fields are all present in RHS.  Unifies to RHS.
-- Q u Q == Quantified record with intersecting properties only
-- C u C == Fields must intersect exactly.  Types unify.  Closed record.
-- F u C == Fields of free record must be present in the closed record and they must unify.  Closed record.
-- Q u C == I think this always fails to unify.
data RecordOpen
    = RecordFree RowVariable (Maybe TypeVar)
    | RecordQuantified RowVariable (Maybe TypeVar)
    | RecordClose
    deriving (Show, Eq)

data RecordTypeVar
    = RBound (IORef RecordTypeVar)
    | RRecord (RecordType TypeVar)
    deriving (Eq)

followRecordTypeVar' :: MonadIO m => IORef RecordTypeVar -> m (IORef RecordTypeVar, RecordType TypeVar)
followRecordTypeVar' ref = readIORef ref >>= \case
    RBound ref' -> followRecordTypeVar' ref'
    RRecord rt -> return (ref, rt)

followRecordTypeVar :: MonadIO m => IORef RecordTypeVar -> m (RecordType TypeVar)
followRecordTypeVar ref = snd <$> followRecordTypeVar' ref

data RecordType typeVar = RecordType RecordOpen [TypeRow typeVar]
    deriving (Show, Eq, Functor, Foldable, Traversable)

data TraitDesc = TraitDesc
    { tdName :: Name
    , tdModule :: ModuleName
    , tdTypeVar :: TypeVar
    , tdMethods :: [(Name, TypeVar)]
    }
    deriving (Eq, Show)

data TraitIdentity = TraitIdentity ModuleName Name
    deriving (Eq, Ord, Show, Generic)

instance Hashable TraitIdentity

type TypeNumber = Int

data Strength = Strong | Weak
    deriving (Eq, Show)

data TypeSource
    = ExplicitName Name Pos
    -- TODO: put a source on Unbound type variables too
    | Instantiation
    deriving (Eq, Show)

-- this should be called Type probably, but tons of code calls it TypeVar
data TypeVar
    = TypeVar (IORef TypeState)
    | TQuant TypeSource (Set TraitIdentity) TypeNumber
    | TFun [TypeVar] TypeVar
    | TDataType (TDataTypeDef TypeVar)
    | TRecord (IORef RecordTypeVar)
    | TTypeFun [TypeVar] TypeVar
    deriving (Eq)

unsafeShowRef :: Show a => IORef a -> String
unsafeShowRef ref = show $ unsafePerformIO $ readIORef ref

-- This instance is only for debugging
-- TODO: showsPrec
instance Show TypeVar where
    show (TypeVar r) = "(TypeVar " ++ unsafeShowRef r ++ ")"
    show (TQuant source constraints tn) = "(TQuant " ++ show source ++ " " ++ show constraints ++ " " ++ show tn ++ ")"
    show (TFun args rv) = "(TFun " ++ show args ++ " " ++ show rv ++ ")"
    show (TDataType def) = "(TDataType " ++ show def ++ ")"
    show (TRecord _) = "(TRecord ???)" -- TODO
    show (TTypeFun args rv) = "(TTypeFun " ++ show args ++ " " ++ show rv ++ ")"

data TypeState
    = TUnbound Strength TypeLevel (Set TraitIdentity) TypeNumber
    | TBound TypeVar
    deriving (Eq, Show)

newTypeVar :: MonadIO m => TypeState -> m TypeVar
newTypeVar tv = TypeVar <$> newIORef tv

followTypeVar :: MonadIO m => TypeVar -> m TypeVar
followTypeVar tvar = case tvar of
    TypeVar ref ->
        readIORef ref >>= \case
            TBound tv -> followTypeVar tv
            TUnbound {} -> return tvar
    _ ->
        return tvar

{-
-- TODO: move this into Crux.TypeVar.Internal.  Only Unify should use it.
readTypeVar :: TypeVar -> IO MutableTypeVar
readTypeVar (TypeVar r) = readIORef r

followTypeVar :: TypeVar -> IO TypeVar
followTypeVar tv@(TypeVar ref) = readIORef ref >>= \case
    TBound target -> followTypeVar target
    _ -> return tv

-- TODO: move this into Crux.TypeVar.Internal.  Only Unify should use it.
writeTypeVar :: TypeVar -> MutableTypeVar -> IO ()
writeTypeVar (TypeVar r) = writeIORef r
-}

-- TypeVar Renderer state
data TVRState = TVRState
    { tvrCount :: IORef Int
    , tvrNames :: IORef (Map.Map Int String)
    }

getNextVarName :: MonadIO m => TVRState -> m String
getNextVarName state = do
    current <- readIORef $ tvrCount state
    let next = current + 1
    writeIORef (tvrCount state) next
    return $ "_t" <> show next

showRecordTypeVarIO' :: MonadIO m => TVRState -> IORef RecordTypeVar -> m String
showRecordTypeVarIO' state ref = readIORef ref >>= \case
    RRecord (RecordType open' rows) -> do
        rows' <- for rows $ \TypeRow{..} -> do
            typeName <- showTypeVarIO' state trTyVar
            let mutPrefix = case trMut of
                    RMutable -> "mutable "
                    RImmutable -> ""
                    RFree -> "mutable? "
                    RQuantified -> "mutable? "
            return $ mutPrefix <> (Text.unpack trName) <> ": " <> typeName
        dotdotdot <- case open' of
            RecordFree i constraint -> do
                constr <- for constraint $ \tv -> do
                    tv' <- showTypeVarIO' state tv
                    return $ ": " ++ tv'
                return ["..._" ++ show i ++ constr ?? ""]
            RecordQuantified i constraint -> do
                constr <- for constraint $ \tv -> do
                    tv' <- showTypeVarIO' state tv
                    return $ ": " ++ tv'
                return ["...t" ++ show i ++ constr ?? ""]
            RecordClose -> return []
        return $ "{" <> (intercalate ", " (rows' <> dotdotdot)) <> "}"
    RBound ref' -> do
        showRecordTypeVarIO' state ref'
        
showTypeVarIO' :: MonadIO m => TVRState -> TypeVar -> m String
showTypeVarIO' state = \case
    TypeVar ref -> readIORef ref >>= \case
        TUnbound _strength _level constraints i -> do
            -- TODO: should we show the user the strength?
            -- TODO: put constraints in an addendum list
            names <- readIORef $ tvrNames state
            name' <- case Map.lookup i names of
                Just name -> return name
                Nothing -> do
                    name <- getNextVarName state
                    writeIORef (tvrNames state) $ Map.insert i name names
                    return name
            let constraintsString = intercalate "+" (fmap (\(TraitIdentity _m n) -> Text.unpack n) $ Set.toList constraints)
            return $ name' <> if constraints == mempty then "" else (": " <> constraintsString)
        TBound tv -> do
            showTypeVarIO' state tv
    TQuant typeSource constraints i -> do
        return $ "TQuant " ++ show typeSource ++ " " ++ show constraints ++ " " ++ show i
    TFun args ret -> do
        as <- for args $ showTypeVarIO' state
        rs <- showTypeVarIO' state ret
        return $ "(" ++ intercalate "," as ++ ") => " ++ rs
    TDataType def -> do
        tvs <- for (tuParameters def) $ showTypeVarIO' state
        return $ case (tuModuleName def, tuName def) of
            ("tuple", "Tuple2") -> "(" <> intercalate ", " tvs <> ")"
            ("tuple", "Tuple3") -> "(" <> intercalate ", " tvs <> ")"
            ("tuple", "Tuple4") -> "(" <> intercalate ", " tvs <> ")"
            ("tuple", "Tuple5") -> "(" <> intercalate ", " tvs <> ")"
            ("tuple", "Tuple6") -> "(" <> intercalate ", " tvs <> ")"
            ("tuple", "Tuple7") -> "(" <> intercalate ", " tvs <> ")"
            ("tuple", "Tuple8") -> "(" <> intercalate ", " tvs <> ")"
            _ -> (Text.unpack $ tuName def) ++ if tvs /= [] then "<" ++ (intercalate ", " tvs) ++ ">" else ""
    TRecord rtv -> do
        showRecordTypeVarIO' state rtv
    TTypeFun args ret -> do
        args' <- for args $ showTypeVarIO' state
        rs <- showTypeVarIO' state ret
        return $ "TTypeFun " ++ show args' ++ " " ++ rs

-- | Returns a human-readable representation of a given TypeVar
-- in Crux syntax.
renderTypeVarIO :: MonadIO m => TypeVar -> m String
renderTypeVarIO tv = do
    count <- newIORef 0
    names <- newIORef mempty
    let state = TVRState
            { tvrCount = count
            , tvrNames = names
            }
    showTypeVarIO' state tv
