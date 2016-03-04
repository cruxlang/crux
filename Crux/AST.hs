{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Crux.AST
    ( module Crux.AST
    , Pos(..)
    ) where

import qualified Crux.JSTree   as JSTree
import           Crux.Prelude
import Crux.Tokens (Pos)
import qualified Crux.Tokens   as Tokens
import qualified Data.Text     as Text

type Name = Text -- Temporary

data Literal
    = LInteger Integer
    | LString Text
    | LUnit
    deriving (Show, Eq)

data Variant edata = Variant edata Name [TypeIdent]
    deriving (Show, Eq, Functor, Foldable, Traversable)

data JSVariant = JSVariant Name JSTree.Literal
    deriving (Show, Eq)

-- Irrefutable only -- should it be called IrrefutablePattern?
-- This very easily could grow PTuple or irrefutable constructor matches.
-- TODO: eventually this will need to be merged with RefutablePattern when we have
-- dynamic checks for refutability, like a data type with a single data constructor.
data Pattern
    = PWildcard
    | PBinding Name
    deriving (Show, Eq)

instance IsString Pattern where
    fromString = PBinding . fromString

data RefutablePattern
    = RPConstructor (Maybe Name) Name [RefutablePattern]
    | RPIrrefutable Pattern
    deriving (Show, Eq)

-- TODO: to support the "let rec" proposal, change DFun into DFunGroup
-- note that individual functions in a function group can be exported.
data DeclarationType idtype edata
    -- Values
    = DDeclare edata Name TypeIdent
    | DLet edata Mutability Pattern (Maybe TypeIdent) (Expression idtype edata)
    | DFun edata Name [(Pattern, Maybe TypeIdent)] (Maybe TypeIdent) (Expression idtype edata)
    -- Types
    | DData edata Name ModuleName [Text] [Variant edata]
    | DJSData edata Name ModuleName [JSVariant]
    | DTypeAlias edata Name [Name] TypeIdent
    deriving (Show, Eq, Functor, Foldable, Traversable)

data ExportFlag = Export | NoExport
    deriving (Show, Eq)

data Declaration idtype edata = Declaration ExportFlag Pos (DeclarationType idtype edata)
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

data UnresolvedReference
    = UnqualifiedReference Name
    | KnownReference ModuleName Name
    deriving (Show, Eq)

-- hack for convenience
instance IsString UnresolvedReference where
    fromString = UnqualifiedReference . fromString

data ResolvedReference
    = Ambient Name
    | Local Name
    | ThisModule Name
    | OtherModule ModuleName Name
    | Builtin Name
    deriving (Show, Eq)

resolvedReferenceName :: ResolvedReference -> Text
resolvedReferenceName (Ambient t) = t
resolvedReferenceName (Local t) = t
resolvedReferenceName (ThisModule t) = t
resolvedReferenceName (OtherModule _ t) = t
resolvedReferenceName (Builtin t) = t

data Import
    = UnqualifiedImport ModuleName
    | QualifiedImport ModuleName Name
    deriving (Show, Eq)

data Pragma
    = PNoBuiltin
    deriving (Show, Eq)

data Module idtype edata = Module
    { mPragmas :: [Pragma]
    , mImports :: [(Pos, Import)]
    , mDecls   :: [Declaration idtype edata]
    }
    deriving (Show, Eq)

type ParsedModule = Module UnresolvedReference Tokens.Pos

data Case idtype edata = Case RefutablePattern (Expression idtype edata)
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
    | BAnd
    | BOr
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

isBooleanOp :: BinIntrinsic -> Bool
isBooleanOp op = case op of
    BAnd -> True
    BOr -> True
    _ -> False

data Intrinsic input
    = IUnsafeJs Text
    | IUnsafeCoerce input
    | INot input
    deriving (Show, Eq, Functor, Foldable, Traversable)

type IntrinsicId idtype edata = Intrinsic (Expression idtype edata)

data Mutability
    = Mutable
    | Immutable
    deriving (Show, Eq)

data Expression idtype edata
    -- Mutable Wildcard makes no sense -- disallow that?
    -- TODO: should mutability status be part of the pattern?
    = ELet edata Mutability Pattern (Maybe TypeIdent) (Expression idtype edata)
    | ELookup edata (Expression idtype edata) Name
    | EApp edata (Expression idtype edata) [Expression idtype edata]
    | EMatch edata (Expression idtype edata) [Case idtype edata]
    | EAssign edata (Expression idtype edata) (Expression idtype edata)
    | EIdentifier edata idtype
    | ESemi edata (Expression idtype edata) (Expression idtype edata)

    | EMethodApp edata (Expression idtype edata) Name [Expression idtype edata]

    -- literals
    | EFun edata [(Pattern, Maybe TypeIdent)] (Maybe TypeIdent) (Expression idtype edata)
    | ERecordLiteral edata (HashMap Name (Expression idtype edata))
    | EArrayLiteral edata Mutability [Expression idtype edata]
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
    EArrayLiteral ed _ _ -> ed
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

data TypeIdentifier
    = QualifiedType Name Name
    | UnqualifiedType Name
    deriving (Show, Eq)

instance IsString TypeIdentifier where
    fromString s = UnqualifiedType $ fromString s

data TypeIdent
    = UnitTypeIdent
    | TypeIdent TypeIdentifier [TypeIdent]
    | RecordIdent [(Name, Maybe Mutability, TypeIdent)]
    | FunctionIdent [TypeIdent] TypeIdent
    deriving (Show, Eq)
