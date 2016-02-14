{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Crux.TypeVar where

import Crux.AST (ModuleName)
import Crux.Prelude

type Name = Text
type TypeVar = IORef MutableTypeVar

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

data RecordType typeVar = RecordType RecordOpen [TypeRow typeVar]
    deriving (Show, Eq, Functor, Foldable, Traversable)

data MutableTypeVar
    = TUnbound Int
    | TBound TypeVar
    | TQuant Int
    | TFun [TypeVar] TypeVar
    | TUserType (TUserTypeDef TypeVar) [TypeVar]
    | TRecord (RecordType TypeVar)
    | TPrimitive PrimitiveType
    deriving (Eq)

data ImmutableTypeVar
    = IUnbound Int
    | IQuant Int
    | IFun [ImmutableTypeVar] ImmutableTypeVar
    | IUserType (TUserTypeDef ImmutableTypeVar) [ImmutableTypeVar]
    | IRecord (RecordType ImmutableTypeVar)
    | IPrimitive PrimitiveType
    deriving (Show, Eq)
