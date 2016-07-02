{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, DeriveGeneric #-}

module Crux.TypeVar
    ( RecordOpen(..)
    , RecordType(..)
    , RecordTypeVar(..)
    , TypeRow(..)
    , RowMutability(..)
    , RowVariable(..)
    , TVariant(..)
    , TDataTypeIdentity
    , TDataTypeDef(..)
    , TraitDesc(..)
    , TraitNumber(..)
    , Strength (..)
    , TypeNumber
    , TypeVar(..)
    , TypeState(..)
    , TypeLevel(..)
    , newTypeVar
    , followRecordTypeVar'
    , followRecordTypeVar
    , followTypeVar
    , showTypeVarIO
    , renderTypeVarIO
    , dataTypeIdentity
    ) where

import Crux.AST (ModuleName)
import Crux.Prelude
import qualified Data.Text as Text
import System.IO.Unsafe (unsafePerformIO)

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
    deriving (Show, Eq, Generic)

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

-- An open record can be unified with another record type that has extra properties.
-- F u F == Free record with union of properties
-- F u Q == Verify that LHS fields are all present in RHS.  Unifies to RHS.
-- Q u Q == Quantified record with intersecting properties only
-- C u C == Fields must intersect exactly.  Types unify.  Closed record.
-- F u C == Fields of free record must be present in the closed record and they must unify.  Closed record.
-- Q u C == I think this always fails to unify.
data RecordOpen = RecordFree RowVariable | RecordQuantified RowVariable | RecordClose
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
    }
    deriving (Eq, Show)

newtype TraitNumber = TraitNumber Int
    deriving (Eq, Ord, Show, Hashable)

type TypeNumber = Int

data Strength = Strong | Weak
    deriving (Eq, Show)

-- this should be called Type probably, but tons of code calls it TypeVar
data TypeVar
    = TypeVar (IORef TypeState)
    | TQuant (HashMap TraitNumber TraitDesc) TypeNumber
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
    show (TQuant constraints tn) = "(TQuant " ++ show constraints ++ " " ++ show tn ++ ")"
    show (TFun args rv) = "(TFun " ++ show args ++ " " ++ show rv ++ ")"
    show (TDataType def) = "(TDataType " ++ show def ++ ")"
    show (TRecord _) = "(TRecord ???)" -- TODO
    show (TTypeFun args rv) = "(TTypeFun " ++ show args ++ " " ++ show rv ++ ")"

data TypeState
    = TUnbound Strength TypeLevel (HashMap TraitNumber TraitDesc) TypeNumber
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

showRecordTypeVarIO' :: MonadIO m => Bool -> IORef RecordTypeVar -> m String
showRecordTypeVarIO' showBound ref = readIORef ref >>= \case
    RRecord (RecordType open' rows') -> do
        let rowNames = map trName rows'
        rowTypes <- for rows' $ showTypeVarIO' showBound . trTyVar
        let showRow (name, ty) = Text.unpack name <> ": " <> ty
        let dotdotdot = case open' of
                RecordFree i -> ["..._" ++ show i]
                RecordQuantified i -> ["...t" ++ show i]
                RecordClose -> []
        return $ "{" <> (intercalate "," (map showRow (zip rowNames rowTypes) <> dotdotdot)) <> "}"
    RBound ref' -> do
        inner <- showRecordTypeVarIO' showBound ref'
        return $ if showBound
            then "(RBound " ++ inner ++ ")"
            else inner

showTypeVarIO' :: MonadIO m => Bool -> TypeVar -> m String
showTypeVarIO' showBound = \case
    TypeVar ref -> readIORef ref >>= \case
        TUnbound str _level constraints i -> do
            let strstr | str == Strong = ""
                       | otherwise = "Weak "
            return $ "(TUnbound " ++ strstr ++ show constraints ++ " " ++ show i ++ ")"
        TBound tv -> do
            inner <- showTypeVarIO' showBound tv
            if showBound
                then return $ "(TBound " ++ inner ++ ")"
                else return inner
    TQuant constraints i -> do
        return $ "TQuant " ++ show constraints ++ " " ++ show i
    TFun args ret -> do
        as <- for args $ showTypeVarIO' showBound
        rs <- showTypeVarIO' showBound ret
        return $ "(" ++ intercalate "," as ++ ") -> " ++ rs
    TDataType def -> do
        tvs <- for (tuParameters def) $ showTypeVarIO' showBound
        return $ (Text.unpack $ tuName def) ++ if tvs /= [] then " " ++ (intercalate " " tvs) else ""
    TRecord rtv -> do
        showRecordTypeVarIO' showBound rtv
    TTypeFun args ret -> do
        args' <- for args $ showTypeVarIO' showBound
        rs <- showTypeVarIO' showBound ret
        return $ "TTypeFun " ++ show args' ++ " " ++ rs

showTypeVarIO :: MonadIO m => TypeVar -> m String
showTypeVarIO = showTypeVarIO' True

renderTypeVarIO :: MonadIO m => TypeVar -> m String
renderTypeVarIO = showTypeVarIO' False
