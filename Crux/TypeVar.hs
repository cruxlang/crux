{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Crux.TypeVar
    ( PrimitiveType(..)
    , RecordOpen(..)
    , RecordType(..)
    , RecordTypeVar(..)
    , TypeRow(..)
    , RowMutability(..)
    , RowVariable(..)
    , TVariant(..)
    , TUserTypeDef(..)
    , TypeVar(..)
    , TypeState(..)
    , newTypeVar
    , followRecordTypeVar'
    , followRecordTypeVar
    , followTypeVar
    , showTypeVarIO
    , renderTypeVarIO
    , userTypeIdentity
    ) where

import qualified Data.Text as Text
import Data.List (intercalate)
import Crux.AST (ModuleName)
import Crux.Prelude

type Name = Text

data PrimitiveType
    = Number
    | String
    | Unit
    deriving (Show, Eq)

data TVariant typevar = TVariant
    { tvName       :: Name
    , tvParameters :: [typevar]
    } deriving (Show, Eq, Functor, Foldable, Traversable)

data TUserTypeDef typevar = TUserTypeDef
    { tuName       :: !Name
    , tuModuleName :: !ModuleName
    , tuParameters :: ![typevar]
    , tuVariants   :: ![TVariant typevar]
    } deriving (Show, Eq, Functor, Foldable, Traversable)

userTypeIdentity :: TUserTypeDef a -> (Name, ModuleName)
userTypeIdentity ut = (tuName ut, tuModuleName ut)

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

followRecordTypeVar' :: IORef RecordTypeVar -> IO (IORef RecordTypeVar, RecordType TypeVar)
followRecordTypeVar' ref = readIORef ref >>= \case
    RBound ref' -> followRecordTypeVar' ref'
    RRecord rt -> return (ref, rt)

followRecordTypeVar :: IORef RecordTypeVar -> IO (RecordType TypeVar)
followRecordTypeVar ref = snd <$> followRecordTypeVar' ref

data RecordType typeVar = RecordType RecordOpen [TypeRow typeVar]
    deriving (Show, Eq, Functor, Foldable, Traversable)

-- this should be called Type probably, but tons of code calls it TypeVar
data TypeVar
    = TypeVar (IORef TypeState)
    | TQuant Int
    | TFun [TypeVar] TypeVar
    | TUserType (TUserTypeDef TypeVar) [TypeVar]
    | TRecord (IORef RecordTypeVar)
    | TPrimitive PrimitiveType
    deriving (Eq)

data TypeState
    = TUnbound Int
    | TBound TypeVar
    deriving (Eq)

newTypeVar :: TypeState -> IO TypeVar
newTypeVar tv = TypeVar <$> newIORef tv

followTypeVar :: TypeVar -> IO TypeVar
followTypeVar (TypeVar ref) = do
    readIORef ref >>= \case
        TBound tv -> followTypeVar tv
        TUnbound _ -> return $ TypeVar ref
followTypeVar tv = return tv

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

showRecordTypeVarIO' :: Bool -> IORef RecordTypeVar -> IO String
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

showTypeVarIO' :: Bool -> TypeVar -> IO String
showTypeVarIO' showBound = \case
    TypeVar ref -> readIORef ref >>= \case
        TUnbound i -> do
            return $ "(TUnbound " ++ show i ++ ")"
        TBound tv -> do
            inner <- showTypeVarIO' showBound tv
            if showBound
                then return $ "(TBound " ++ inner ++ ")" ++ show showBound
                else return inner
    TQuant i -> do
        return $ "TQuant " ++ show i
    TFun arg ret -> do
        as <- for arg $ showTypeVarIO' showBound
        rs <- showTypeVarIO' showBound ret
        return $ "(" ++ intercalate "," as ++ ") -> " ++ rs
    TUserType def tvars -> do
        tvs <- for tvars $ showTypeVarIO' showBound
        return $ (Text.unpack $ tuName def) ++ " " ++ (intercalate " " tvs)
    TRecord rtv -> do
        showRecordTypeVarIO' showBound rtv
    TPrimitive ty ->
        return $ show ty

showTypeVarIO :: TypeVar -> IO String
showTypeVarIO = showTypeVarIO' True

renderTypeVarIO :: TypeVar -> IO String
renderTypeVarIO = showTypeVarIO' False
