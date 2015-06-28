
module Crux.AST where

import           Data.IORef (IORef)
import           Data.Text  (Text)
import qualified Data.Text  as T

type Name = Text -- Temporary
type TypeName = Text
type Pattern = Name -- Temporary

data Literal
    = LInteger Integer
    | LString Text
    | LUnit
    deriving (Show, Eq)

data Variant = Variant
    { vname       :: Name
    , vparameters :: [TypeName]
    } deriving (Show, Eq)

data Declaration edata
    = DLet edata Pattern (Expression edata)
    | DData Name [Variant]
    deriving (Show, Eq)

data Pattern2
    = PConstructor Name [Pattern2]
    | PPlaceholder Name
    deriving (Show, Eq)

data Case edata = Case Pattern2 (Expression edata)
    deriving (Show, Eq)

data BinIntrinsic
    = BIPlus
    | BIMinus
    | BIMultiply
    | BIDivide
    deriving (Show, Eq)

data Expression edata
    = EBlock edata [Expression edata]
    | ELet edata Pattern (Expression edata)
    | EFun edata [Text] [Expression edata]
    | EApp edata (Expression edata) (Expression edata)
    | EMatch edata (Expression edata) [Case edata]
    | EPrint edata (Expression edata)
    | EToString edata (Expression edata)
    | ELiteral edata Literal
    | EIdentifier edata Text
    | ESemi edata (Expression edata) (Expression edata)
    | EBinIntrinsic edata BinIntrinsic (Expression edata) (Expression edata)
    deriving (Show, Eq)

edata :: Expression edata -> edata
edata expr = case expr of
    EBlock ed _ -> ed
    ELet ed _ _ -> ed
    EFun ed _ _ -> ed
    EApp ed _ _ -> ed
    EMatch ed _ _ -> ed
    EPrint ed _ -> ed
    EToString ed _ -> ed
    ELiteral ed _ -> ed
    EIdentifier ed _ -> ed
    ESemi ed _ _ -> ed
    EBinIntrinsic ed _ _ _ -> ed

data Type
    = Number
    | String
    | UserType Name [Variant]
    | Unit
    deriving (Eq)

instance Show Type where
    show ty = case ty of
        Number -> "Number"
        String -> "String"
        UserType name _ -> T.unpack name
        Unit -> "Unit"

data VarLink a
    = Unbound
    | Link a
    deriving (Show, Eq)

data TypeVar
    = TVar Int (IORef (VarLink TypeVar))
    | TQuant Int
    | TFun [TypeVar] TypeVar
    | TType Type

data ImmutableTypeVar
    = IVar Int (VarLink ImmutableTypeVar)
    | IQuant Int
    | IFun [ImmutableTypeVar] ImmutableTypeVar
    | IType Type
    deriving (Show, Eq)
