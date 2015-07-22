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
    = DLet edata Recursive Pattern (Expression edata)
    | DData Name [TypeVariable] [Variant]
    deriving (Show, Eq)

instance Functor Declaration where
    fmap f d = case d of
        DLet ddata rec pat subExpr -> DLet (f ddata) rec pat (fmap f subExpr)
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

data Expression edata
    = EBlock edata [Expression edata]
    | ELet edata Recursive Pattern (Expression edata)
    | EFun edata [Text] [Expression edata]
    | ERecordLiteral edata (HashMap Name (Expression edata))
    | ELookup edata (Expression edata) Name
    | EApp edata (Expression edata) [Expression edata]
    | EMatch edata (Expression edata) [Case edata]
    | EPrint edata (Expression edata)
    | EToString edata (Expression edata)
    | ELiteral edata Literal
    | EIdentifier edata Text
    | ESemi edata (Expression edata) (Expression edata)
    | EBinIntrinsic edata BinIntrinsic (Expression edata) (Expression edata)
    | EIfThenElse edata (Expression edata) (Expression edata) (Expression edata)
    deriving (Show, Eq)

instance Functor Expression where
    fmap f expr = case expr of
        EBlock d s -> EBlock (f d) (fmap (fmap f) s)
        ELet d rec pat subExpr -> ELet (f d) rec pat (fmap f subExpr)
        EFun d argNames subExprs -> EFun (f d) argNames (fmap (fmap f) subExprs)
        ERecordLiteral d fields -> ERecordLiteral (f d) (fmap (fmap f) fields)
        ELookup d subExpr prop -> ELookup (f d) (fmap f subExpr) prop
        EApp d lhs args -> EApp (f d) (fmap f lhs) (map (fmap f) args)
        EMatch d matchExpr cases -> EMatch (f d) (fmap f matchExpr) (fmap (fmap f) cases)
        EPrint d subExpr -> EPrint (f d) (fmap f subExpr)
        EToString d subExpr -> EToString (f d) (fmap f subExpr)
        ELiteral d l -> ELiteral (f d) l
        EIdentifier d i -> EIdentifier (f d) i
        ESemi d lhs rhs -> ESemi (f d) (fmap f lhs) (fmap f rhs)
        EBinIntrinsic d intrin lhs rhs -> EBinIntrinsic (f d) intrin (fmap f lhs) (fmap f rhs)
        EIfThenElse d condition ifTrue ifFalse -> EIfThenElse (f d) (fmap f condition) (fmap f ifTrue) (fmap f ifFalse)

edata :: Expression edata -> edata
edata expr = case expr of
    EBlock ed _ -> ed
    ELet ed _ _ _ -> ed
    EFun ed _ _ -> ed
    ERecordLiteral ed _ -> ed
    ELookup ed _ _ -> ed
    EApp ed _ _ -> ed
    EMatch ed _ _ -> ed
    EPrint ed _ -> ed
    EToString ed _ -> ed
    ELiteral ed _ -> ed
    EIdentifier ed _ -> ed
    ESemi ed _ _ -> ed
    EBinIntrinsic ed _ _ _ -> ed
    EIfThenElse ed _ _ _ -> ed

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
