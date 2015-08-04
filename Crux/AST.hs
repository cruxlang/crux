{-# LANGUAGE RecordWildCards #-}
module Crux.AST where

import           Data.HashMap.Strict (HashMap)
import           Data.IORef          (IORef)
import           Data.Text           (Text)

type Name = Text -- Temporary
type TypeName = Text
type TypeVariable = Text -- :)
type Pattern = Name -- Temporary

data TypeIdent = TypeIdent TypeName [TypeIdent]
    deriving (Show, Eq)

data Recursive
    = Rec
    | NoRec
    deriving (Show, Eq)

data Literal
    = LInteger Integer
    | LString Text
    | LUnit
    deriving (Show, Eq)

data Variant = Variant
    { vname       :: Name
    , vparameters :: [TypeIdent]
    } deriving (Show, Eq)

data Declaration edata
    = DLet edata Name (Expression edata)
    | DFun edata Name [Name] (Expression edata)
    | DData Name [TypeVariable] [Variant]
    deriving (Show, Eq)

instance Functor Declaration where
    fmap f d = case d of
        DLet ddata name subExpr -> DLet (f ddata) name (fmap f subExpr)
        DFun ddata name args body -> DFun (f ddata) name args (fmap f body)
        DData name vars variants -> DData name vars variants

data Pattern2
    = PConstructor Name [Pattern2]
    | PPlaceholder Name
    deriving (Show, Eq)

data Case edata = Case Pattern2 (Expression edata)
    deriving (Show, Eq)

instance Functor Case where
    fmap f (Case patt subExpr) = Case patt (fmap f subExpr)

data BinIntrinsic
    = BIPlus
    | BIMinus
    | BIMultiply
    | BIDivide
    deriving (Show, Eq)

data IntrinsicId edata
    = IIUnsafeJs Text
    | IIUnsafeCoerce (Expression edata)
    | IIPrint [Expression edata]
    | IIToString (Expression edata)
    deriving (Show, Eq)

data Expression edata
    = ELet edata Recursive Pattern (Expression edata)
    | EFun edata [Text] (Expression edata)
    | ERecordLiteral edata (HashMap Name (Expression edata))
    | ELookup edata (Expression edata) Name
    | EApp edata (Expression edata) [Expression edata]
    | EMatch edata (Expression edata) [Case edata]
    | ELiteral edata Literal
    | EIdentifier edata Text
    | ESemi edata (Expression edata) (Expression edata)
    | EBinIntrinsic edata BinIntrinsic (Expression edata) (Expression edata)
    | EIntrinsic edata (IntrinsicId edata)
    | EIfThenElse edata (Expression edata) (Expression edata) (Expression edata)
    | EReturn edata (Expression edata)
    deriving (Show, Eq)

instance Functor Expression where
    fmap f expr = case expr of
        ELet d rec pat subExpr -> ELet (f d) rec pat (fmap f subExpr)
        EFun d argNames body -> EFun (f d) argNames (fmap f body)
        ERecordLiteral d fields -> ERecordLiteral (f d) (fmap (fmap f) fields)
        ELookup d subExpr prop -> ELookup (f d) (fmap f subExpr) prop
        EApp d lhs args -> EApp (f d) (fmap f lhs) (map (fmap f) args)
        EMatch d matchExpr cases -> EMatch (f d) (fmap f matchExpr) (fmap (fmap f) cases)
        ELiteral d l -> ELiteral (f d) l
        EIdentifier d i -> EIdentifier (f d) i
        ESemi d lhs rhs -> ESemi (f d) (fmap f lhs) (fmap f rhs)
        EBinIntrinsic d intrin lhs rhs -> EBinIntrinsic (f d) intrin (fmap f lhs) (fmap f rhs)
        EIntrinsic d i -> case i of
            IIUnsafeJs txt -> EIntrinsic (f d) (IIUnsafeJs txt)
            IIUnsafeCoerce subExpr -> EIntrinsic (f d) (IIUnsafeCoerce $ fmap f subExpr)
            IIPrint args -> EIntrinsic (f d) (IIPrint $ map (fmap f) args)
            IIToString arg -> EIntrinsic (f d) (IIToString $ fmap f arg)
        EIfThenElse d condition ifTrue ifFalse -> EIfThenElse (f d) (fmap f condition) (fmap f ifTrue) (fmap f ifFalse)
        EReturn d rv -> EReturn (f d) (fmap f rv)

edata :: Expression edata -> edata
edata expr = case expr of
    ELet ed _ _ _ -> ed
    EFun ed _ _ -> ed
    ERecordLiteral ed _ -> ed
    ELookup ed _ _ -> ed
    EApp ed _ _ -> ed
    EMatch ed _ _ -> ed
    ELiteral ed _ -> ed
    EIdentifier ed _ -> ed
    ESemi ed _ _ -> ed
    EBinIntrinsic ed _ _ _ -> ed
    EIntrinsic ed _ -> ed
    EIfThenElse ed _ _ _ -> ed
    EReturn ed _ -> ed

data Type
    = Number
    | String
    | Unit
    | Boolean
    deriving (Show, Eq)

data VarLink a
    = Unbound
    | Link a
    deriving (Show, Eq)

data TVariant typevar = TVariant
    { tvName       :: Name
    , tvParameters :: [typevar]
    } deriving (Show, Eq)

data TUserTypeDef typevar = TUserTypeDef
    { tuName       :: Name
    , tuParameters :: [typevar]
    , tuVariants   :: [TVariant typevar]
    } deriving (Show, Eq)

type TypeRow typevar = (Name, typevar)

{-
fun hypot(p) { sqrt(p.x * p.x + p.y * p.y); };
let p = { x:9,y:22,z:33 };
let ps = hypot(p);

We instantiate the argument type {x:Number, y:Number, ...} and unify with {x:Number, y:Number, z:Number}
This yields {x:Number, y:Number, z:Number}
-}

-- An open record can be unified with another record type that has extra properties.
-- Open u Open == open record combining properties
-- Open u Closed == Assert that lhs has all the fields of the rhs, unify their types, and unify to the closed record
-- Closed u Closed == All properties must exist in both types and unify.
data RecordOpen = RecordOpen | RecordClose
    deriving (Show, Eq)

data TypeVar
    = TVar Int (IORef (VarLink TypeVar))
    | TQuant Int
    | TFun [TypeVar] TypeVar
    | TUserType (TUserTypeDef TypeVar) [TypeVar]
    | TRecord (IORef RecordOpen) (IORef [TypeRow TypeVar])
    | TType Type

data ImmutableTypeVar
    = IVar Int (VarLink ImmutableTypeVar)
    | IQuant Int
    | IFun [ImmutableTypeVar] ImmutableTypeVar
    | IUserType (TUserTypeDef ImmutableTypeVar) [ImmutableTypeVar]
    | IRecord RecordOpen [TypeRow ImmutableTypeVar]
    | IType Type
    deriving (Show, Eq)
