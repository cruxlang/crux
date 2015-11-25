{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Crux.AST where

import qualified Data.Text     as Text
import qualified Crux.JSTree  as JSTree
import           Crux.Prelude
import qualified Crux.Tokens  as Tokens

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

data FunDef idtype edata = FunDef edata Name [(Name, Maybe TypeIdent)] (Maybe TypeIdent) (Expression idtype edata)
    deriving (Show, Eq, Functor, Foldable, Traversable)

data JSVariant = JSVariant Name JSTree.Literal
    deriving (Show, Eq)

data TypeAlias = TypeAlias Name [Name] TypeIdent
    deriving (Show, Eq)

-- TODO: to support the "let rec" proposal, change DFun into DFunGroup
-- note that individual functions in a function group can be exported.
data DeclarationType idtype edata
    = DDeclare Name TypeIdent
    | DLet edata LetMutability Name (Maybe TypeIdent) (Expression idtype edata)
    | DFun (FunDef idtype edata)
    | DData Name [TypeVariable] [Variant]
    | DJSData Name [JSVariant]
    | DType TypeAlias
    deriving (Show, Eq, Functor, Foldable, Traversable)

data ExportFlag = Export | NoExport
    deriving (Show, Eq)

data Declaration idtype edata = Declaration ExportFlag (DeclarationType idtype edata)
    deriving (Show, Eq, Functor)

newtype ModuleSegment = ModuleSegment { unModuleSegment :: Text }
    deriving (Show, Eq, Ord, Generic)
instance Hashable ModuleSegment

data ModuleName = ModuleName [ModuleSegment] ModuleSegment
    deriving (Show, Eq, Ord, Generic)
instance Hashable ModuleName

-- TODO: assert that first letter is capitalized, remainder are alphanumeric
toModuleSegment :: Text -> ModuleSegment
toModuleSegment = ModuleSegment

instance IsString ModuleName where
    fromString s =
        let t = Text.pack s in
        let p = Text.splitOn "." t in
        case map toModuleSegment p of
            [] -> error "Invalid module name"
            xs -> ModuleName (init xs) (last xs)

printModuleName :: ModuleName -> Text
printModuleName (ModuleName a b) = Text.intercalate "." $ fmap unModuleSegment $ a <> [b]

type UnresolvedReference = Text -- TODO: allow qualified references
data ResolvedReference = Local Text | ThisModule Text | OtherModule ModuleName Text | Builtin Text
    deriving (Show, Eq)

resolvedReferenceName :: ResolvedReference -> Text
resolvedReferenceName (Local t) = t
resolvedReferenceName (ThisModule t) = t
resolvedReferenceName (OtherModule _ t) = t
resolvedReferenceName (Builtin t) = t

data Import = UnqualifiedImport ModuleName
    deriving (Show, Eq)

data Module idtype edata = Module
    { mImports :: [Import]
    , mDecls   :: [Declaration idtype edata]
    }
    deriving (Show, Eq)

type ParsedModule = Module UnresolvedReference Tokens.Pos
type LoadedModule = Module ResolvedReference ImmutableTypeVar

data Program = Program
    { pMainModule :: LoadedModule
    , pOtherModules :: HashMap ModuleName LoadedModule
    }
    deriving (Show, Eq)

data Pattern
    = PConstructor Name [Pattern]
    | PPlaceholder Name
    deriving (Show, Eq)

data Case idtype edata = Case Pattern (Expression idtype edata)
    deriving (Show, Eq, Foldable, Traversable)

instance Functor (Case idtype) where
    fmap f (Case patt subExpr) = Case patt (fmap f subExpr)

data BinIntrinsic
    = BIPlus
    | BIMinus
    | BIMultiply
    | BIDivide
    | BILess
    | BIGreater
    | BILessEqual
    | BIGreaterEqual
    | BIEqual
    | BINotEqual
    deriving (Show, Eq)

isArithmeticOp :: BinIntrinsic -> Bool
isArithmeticOp op = case op of
    BIPlus -> True
    BIMinus -> True
    BIMultiply -> True
    BIDivide -> True
    _ -> False

isRelationalOp :: BinIntrinsic -> Bool
isRelationalOp op = case op of
    BILess -> True
    BIGreater -> True
    BILessEqual -> True
    BIGreaterEqual -> True
    BIEqual -> True
    BINotEqual -> True
    _ -> False

data Intrinsic input
    = IUnsafeJs Text
    | IUnsafeCoerce input
    | INot input
    deriving (Show, Eq, Functor, Foldable, Traversable)

type IntrinsicId idtype edata = Intrinsic (Expression idtype edata)

data LetMutability
    = LMutable
    | LImmutable
    deriving (Show, Eq)

data Expression idtype edata
    = ELet edata LetMutability Name (Maybe TypeIdent) (Expression idtype edata)
    | ELookup edata (Expression idtype edata) Name
    | EApp edata (Expression idtype edata) [Expression idtype edata]
    | EMatch edata (Expression idtype edata) [Case idtype edata]
    | EAssign edata (Expression idtype edata) (Expression idtype edata)
    | EIdentifier edata idtype
    | ESemi edata (Expression idtype edata) (Expression idtype edata)

    | EMethodApp edata (Expression idtype edata) Name [Expression idtype edata]

    -- literals
    | EFun edata [(Name, Maybe TypeIdent)] (Maybe TypeIdent) (Expression idtype edata)
    | ERecordLiteral edata (HashMap Name (Expression idtype edata))
    | EArrayLiteral edata [Expression idtype edata]
    | ELiteral edata Literal

    -- intrinsics
    | EBinIntrinsic edata BinIntrinsic (Expression idtype edata) (Expression idtype edata)
    | EIntrinsic edata (IntrinsicId idtype edata)

    -- flow control
    | EIfThenElse edata (Expression idtype edata) (Expression idtype edata) (Expression idtype edata)
    | EWhile edata (Expression idtype edata) (Expression idtype edata)
    | EFor edata Name (Expression idtype edata) (Expression idtype edata)
    | EReturn edata (Expression idtype edata)
    | EBreak edata
    deriving (Show, Eq, Functor, Foldable, Traversable)

edata :: Expression idtype edata -> edata
edata expr = case expr of
    ELet ed _ _ _ _ -> ed
    EFun ed _ _ _ -> ed
    ELookup ed _ _ -> ed
    EApp ed _ _ -> ed
    EMatch ed _ _ -> ed
    EAssign ed _ _ -> ed
    ELiteral ed _ -> ed
    EArrayLiteral ed _ -> ed
    ERecordLiteral ed _ -> ed
    EIdentifier ed _ -> ed
    ESemi ed _ _ -> ed
    EMethodApp ed _ _ _ -> ed
    EBinIntrinsic ed _ _ _ -> ed
    EIntrinsic ed _ -> ed
    EIfThenElse ed _ _ _ -> ed
    EWhile ed _ _ -> ed
    EFor ed _ _ _ -> ed
    EReturn ed _ -> ed
    EBreak ed -> ed

data TypeIdent
    = TypeIdent TypeName [TypeIdent]
    | RecordIdent [(Name, Maybe LetMutability, TypeIdent)]
    | FunctionIdent [TypeIdent] TypeIdent
    deriving (Show, Eq)

data PrimitiveType
    = Number
    | String
    | Unit
    deriving (Show, Eq)

data TVariant typevar = TVariant
    { tvName       :: Name
    , tvParameters :: [typevar]
    } deriving (Show, Eq)

data TUserTypeDef typevar = TUserTypeDef
    { tuName       :: !Name
    , tuModuleName :: !ModuleName
    , tuParameters :: ![typevar]
    , tuVariants   :: ![TVariant typevar]
    } deriving (Show, Eq)

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
