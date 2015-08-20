{-# LANGUAGE RecordWildCards, DeriveFunctor #-}
module Crux.AST where

import           Data.HashMap.Strict (HashMap)
import           Data.IORef          (IORef)
import           Data.Text           (Text)
import qualified Crux.JSTree as JSTree

type Name = Text -- Temporary
type TypeName = Text
type TypeVariable = Text -- :)

data Literal
    = LInteger Integer
    | LString Text
    | LUnit
    deriving (Show, Eq)

data Variant = Variant
    { vname       :: Name
    , vparameters :: [TypeIdent]
    } deriving (Show, Eq)

data FunDef edata = FunDef edata Name [Name] (Expression edata)
    deriving (Show, Eq, Functor)

data JSVariant = JSVariant Name JSTree.Literal
    deriving (Show, Eq)

data TypeAlias = TypeAlias Name [Name] TypeIdent
    deriving (Show, Eq)

-- TODO: to support the "let rec" proposal, change DFun into DFunGroup
-- note that individual functions in a function group can be exported.
data DeclarationType edata
    = DLet edata Name (Maybe TypeIdent) (Expression edata)
    | DFun (FunDef edata)
    | DData Name [TypeVariable] [Variant]
    | DJSData Name [JSVariant]
    | DType TypeAlias
    deriving (Show, Eq, Functor)

data ExportFlag = Export | NoExport
    deriving (Show, Eq)

data Declaration edata = Declaration ExportFlag (DeclarationType edata)
    deriving (Show, Eq, Functor)

data Module edata = Module
    -- { mName :: Text
    -- , mFileName
    -- , mImports
    -- , mExports
    { mDecls :: [Declaration edata]
    }
    deriving (Show, Eq)

data Pattern
    = PConstructor Name [Pattern]
    | PPlaceholder Name
    deriving (Show, Eq)

data Case edata = Case Pattern (Expression edata)
    deriving (Show, Eq)

instance Functor Case where
    fmap f (Case patt subExpr) = Case patt (fmap f subExpr)

data BinIntrinsic
    = BIPlus
    | BIMinus
    | BIMultiply
    | BIDivide
    deriving (Show, Eq)

data Intrinsic input
    = IUnsafeJs Text
    | IUnsafeCoerce input
    | IPrint [input]
    | IToString input
    deriving (Show, Eq)

mapIntrinsicInputs :: Monad m => (a -> m b) -> Intrinsic a -> m (Intrinsic b)
mapIntrinsicInputs action intrin = do
    case intrin of
        IUnsafeJs text -> do
            return $ IUnsafeJs text
        IUnsafeCoerce input -> do
            input' <- action input
            return $ IUnsafeCoerce input'
        IPrint inputs -> do
            inputs' <- mapM action inputs
            return $ IPrint inputs'
        IToString input -> do
            input' <- action input
            return $ IToString input'

type IntrinsicId edata = Intrinsic (Expression edata)

data Expression edata
    = ELet edata Name (Maybe TypeIdent) (Expression edata)
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
        ELet d pat typeAnn subExpr -> ELet (f d) pat typeAnn (fmap f subExpr)
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
            IUnsafeJs txt -> EIntrinsic (f d) (IUnsafeJs txt)
            IUnsafeCoerce subExpr -> EIntrinsic (f d) (IUnsafeCoerce $ fmap f subExpr)
            IPrint args -> EIntrinsic (f d) (IPrint $ map (fmap f) args)
            IToString arg -> EIntrinsic (f d) (IToString $ fmap f arg)
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

data TypeIdent
    = TypeIdent TypeName [TypeIdent]
    | RecordIdent [(Name, TypeIdent)]
    | FunctionIdent [TypeIdent] TypeIdent
    deriving (Show, Eq)

data PrimitiveType
    = Number
    | String
    | Unit
    | Boolean
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
-- F u F == Free record with union of properties
-- F u Q == Verify that LHS fields are all present in RHS.  Unifies to RHS.
-- Q u Q == Quantified record with intersecting properties only
-- C u C == Fields must intersect exactly.  Types unify.  Closed record.
-- F u C == Fields of free record must be present in the closed record and they must unify.  Closed record.
-- Q u C == I think this always fails to unify.
data RecordOpen = RecordFree | RecordQuantified | RecordClose
    deriving (Show, Eq)

type TypeVar = IORef MutableTypeVar

data VarLink a
    = Unbound Int
    | Link a
    deriving (Show, Eq)

data RecordType typeVar = RecordType RecordOpen [TypeRow typeVar]
    deriving (Show, Eq)

data MutableTypeVar
    = TVar Int (VarLink TypeVar)
    | TQuant Int
    | TFun [TypeVar] TypeVar
    | TUserType (TUserTypeDef TypeVar) [TypeVar]
    | TRecord (RecordType TypeVar)
    | TPrimitive PrimitiveType

data ImmutableTypeVar
    = IVar Int (VarLink ImmutableTypeVar)
    | IQuant Int
    | IFun [ImmutableTypeVar] ImmutableTypeVar
    | IUserType (TUserTypeDef ImmutableTypeVar) [ImmutableTypeVar]
    | IRecord (RecordType ImmutableTypeVar)
    | IType PrimitiveType
    deriving (Show, Eq)
