{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable,
             OverloadedStrings, TypeFamilies #-}

module Crux.AST
    ( module Crux.AST
    ) where

import qualified Crux.JSTree as JSTree
import Crux.Prelude
import Crux.Pos (Pos (..))
import Crux.ModuleName
import Crux.TypeVar

type ParsedModule = Module UnresolvedReference () Pos

type Name = Text -- Temporary

data Literal
    = LInteger Integer
    | LString Text
    | LUnit
    deriving (Show, Eq)

data Variant tagtype edata = Variant tagtype edata Name [TypeIdent]
    deriving (Show, Eq, Functor, Foldable, Traversable)

data JSVariant = JSVariant Name JSTree.Literal
    deriving (Show, Eq)

data Pattern tag
    = PWildcard
    | PBinding Name
    | PConstructor UnresolvedReference tag [Pattern tag]
    | PTuple [Pattern tag] -- empty list is unit type
    deriving (Show, Eq)

instance IsString (Pattern tagtype) where
    fromString = PBinding . fromString

data FunctionDecl idtype tagtype edata = FunctionDecl
    { fdParams      :: ![(Pattern tagtype, Maybe (TypeIdent, Maybe Name))]
    , fdReturnAnnot :: !(Maybe TypeIdent)
    , fdBody        :: !(Expression idtype tagtype edata)
    } deriving (Eq, Show, Functor, Foldable, Traversable)

data RecordConstraintIdent = RecordConstraintIdent [(Name, TypeIdent)] (Maybe TypeIdent)
    deriving (Eq, Show)

data ConstraintSetIdent = ConstraintSetIdent (Maybe RecordConstraintIdent) [UnresolvedReference]
    deriving (Eq, Show)

data TypeVarIdent = TypeVarIdent Name Pos ConstraintSetIdent
    deriving (Eq, Show)

data ImplNominal = ImplNominal
    { inTypeName :: UnresolvedReference
    , inTypeParams :: [TypeVarIdent]
    }
    deriving (Eq, Show)

data ImplType idtype tagtype edata
    = ImplTypeNominal ImplNominal
    | ImplTypeFunction Int
    | ImplTypeRecord (Expression idtype tagtype edata) {- field function -}
    deriving (Eq, Show, Functor, Foldable, Traversable)

-- TODO: to support the "let rec" proposal, change DFun into DFunGroup
-- note that individual functions in a function group can be exported.
data DeclarationType idtype tagtype edata
    -- Exports
    = DExportImport edata Name
    -- Values
    | DDeclare edata Name [TypeVarIdent] TypeIdent
    | DLet !edata !Mutability (Pattern tagtype) [TypeVarIdent] (Maybe TypeIdent) (Expression idtype tagtype edata)
    | DFun !edata !Name [TypeVarIdent] !(FunctionDecl idtype tagtype edata)
    -- Types
    | DData edata Name [TypeVarIdent] [Variant tagtype edata]
    | DJSData edata Name [JSVariant]
    | DTypeAlias edata Name [Name] TypeIdent
    -- Traits
    | DTrait edata Name [(Name, edata, TypeIdent)]
    | DImpl
        edata     -- ^ TypeVar of the type parameter
        idtype -- ^ Trait name
        (ImplType idtype tagtype edata) -- ^ nominal vs. record ident
        [Name] -- ^ context dict args
        [(Name, Expression idtype tagtype edata)] -- ^ methods
    -- Exceptions
    | DException edata Name TypeIdent
    deriving (Show, Eq, Functor, Foldable, Traversable)

data ExportFlag = Export | NoExport
    deriving (Show, Eq)

data Declaration idtype tagtype edata = Declaration ExportFlag Pos (DeclarationType idtype tagtype edata)
    deriving (Show, Eq, Functor)

data UnresolvedReference
    = UnqualifiedReference Name
    | QualifiedReference Name Name
    | KnownReference ModuleName Name
    deriving (Show, Eq)

getUnresolvedReferenceLeaf :: UnresolvedReference -> Text
getUnresolvedReferenceLeaf (UnqualifiedReference n) = n
getUnresolvedReferenceLeaf (QualifiedReference _ n) = n
getUnresolvedReferenceLeaf (KnownReference _ n) = n

-- hack for convenience
instance IsString UnresolvedReference where
    fromString = UnqualifiedReference . fromString

data ReferenceType = Ambient | Local | FromModule ModuleName
    deriving (Show, Eq)
type ResolvedReference = (ReferenceType, Name)

resolvedReferenceName :: ResolvedReference -> Text
resolvedReferenceName (_, n) = n

data ImportType
    = UnqualifiedImport
    | SelectiveImport [Name]
    | QualifiedImport (Maybe Name)
    deriving (Show, Eq)
data Import = Import ModuleName ImportType
    deriving (Show, Eq)

data Pragma
    = PNoBuiltin
    deriving (Show, Eq)

data Module idtype tagtype edata = Module
    { mPragmas :: [Pragma]
    , mImports :: [(Pos, Import)]
    , mDecls   :: [Declaration idtype tagtype edata]
    }
    deriving (Show, Eq)

data Case idtype tagtype edata = Case (Pattern tagtype) (Expression idtype tagtype edata)
    deriving (Show, Eq, Foldable, Traversable)

instance Functor (Case idtype tagtype) where
    fmap f (Case patt subExpr) = Case patt (fmap f subExpr)

data UnIntrinsic
    = UINegate
    deriving (Show, Eq)

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

type IntrinsicId idtype tagtype edata = Intrinsic (Expression idtype tagtype edata)

data Mutability
    = Mutable
    | Immutable
    deriving (Show, Eq)

data CatchBinding idtype tagtype
    = CruxException idtype (Pattern tagtype)
    | WildcardException
    deriving (Show, Eq)

data Expression idtype tagtype edata
    -- Mutable Wildcard makes no sense -- disallow that?
    -- TODO: should mutability status be part of the pattern?
    = ELet edata Mutability (Pattern tagtype) [TypeVarIdent] (Maybe TypeIdent) (Expression idtype tagtype edata)
    | ELookup edata (Expression idtype tagtype edata) Name
    | EApp edata (Expression idtype tagtype edata) [Expression idtype tagtype edata]
    | EMatch edata (Expression idtype tagtype edata) [Case idtype tagtype edata]
    | EAssign edata (Expression idtype tagtype edata) (Expression idtype tagtype edata)
    | EIdentifier edata idtype
    | ESemi edata (Expression idtype tagtype edata) (Expression idtype tagtype edata)
    | EMethodApp edata (Expression idtype tagtype edata) Name [Expression idtype tagtype edata]
    | ETypeLookup edata idtype Name
    | EAs edata (Expression idtype tagtype edata) TypeIdent

    -- literals
    | EFun edata (FunctionDecl idtype tagtype edata)
    | ERecordLiteral edata (HashMap Name (Mutability, Expression idtype tagtype edata))
    | EArrayLiteral edata Mutability [Expression idtype tagtype edata]
    | ETupleLiteral edata [Expression idtype tagtype edata]
    | ELiteral edata Literal

    -- intrinsics
    | EBinIntrinsic edata BinIntrinsic (Expression idtype tagtype edata) (Expression idtype tagtype edata)
    | EUnIntrinsic edata UnIntrinsic (Expression idtype tagtype edata)
    | EIntrinsic edata (IntrinsicId idtype tagtype edata)

    -- flow control
    | EIfThenElse edata (Expression idtype tagtype edata) (Expression idtype tagtype edata) (Expression idtype tagtype edata)
    | EWhile edata (Expression idtype tagtype edata) (Expression idtype tagtype edata)
    | EFor edata (Pattern tagtype) (Expression idtype tagtype edata) (Expression idtype tagtype edata)
    | EReturn edata (Expression idtype tagtype edata)
    | EBreak edata
    | EThrow edata idtype (Expression idtype tagtype edata)
    | ETryCatch edata (Expression idtype tagtype edata) (CatchBinding idtype tagtype) (Expression idtype tagtype edata)

    -- trait dictionary conversion
    -- instance dict placeholders to be resolved after quantification
    | EInstancePlaceholder edata TraitIdentity
    | EInstanceDict edata TraitIdentity TraitImplIdentity
    | EInstanceArgument edata Name
    | EInstanceFieldMap edata TraitIdentity

    deriving (Show, Eq, Functor, Foldable, Traversable)

edata :: Expression idtype tagtype edata -> edata
edata expr = case expr of
    ELet ed _ _ _ _ _ -> ed
    EFun ed _ -> ed
    ELookup ed _ _ -> ed
    EApp ed _ _ -> ed
    EMatch ed _ _ -> ed
    EAssign ed _ _ -> ed
    ELiteral ed _ -> ed
    EArrayLiteral ed _ _ -> ed
    ETupleLiteral ed _ -> ed
    ERecordLiteral ed _ -> ed
    EIdentifier ed _ -> ed
    ESemi ed _ _ -> ed
    EMethodApp ed _ _ _ -> ed
    ETypeLookup ed _ _ -> ed
    EAs ed _ _ -> ed
    EUnIntrinsic ed _ _ -> ed
    EBinIntrinsic ed _ _ _ -> ed
    EIntrinsic ed _ -> ed
    EIfThenElse ed _ _ _ -> ed
    EWhile ed _ _ -> ed
    EFor ed _ _ _ -> ed
    EReturn ed _ -> ed
    EBreak ed -> ed
    EThrow ed _ _ -> ed
    ETryCatch ed _ _ _ -> ed
    EInstancePlaceholder ed _ -> ed
    EInstanceDict ed _ _ -> ed
    EInstanceArgument ed _ -> ed
    EInstanceFieldMap ed _ -> ed

setEdata :: Expression idtype tagtype edata -> edata -> Expression idtype tagtype edata
setEdata expr e = case expr of
    ELet _ a b c d f      -> ELet e a b c d f
    EFun _ a              -> EFun e a
    ELookup _ a b         -> ELookup e a b
    EApp _ a b            -> EApp e a b
    EMatch _ a b          -> EMatch e a b
    EAssign _ a b         -> EAssign e a b
    ELiteral _ a          -> ELiteral e a
    EArrayLiteral _ a b   -> EArrayLiteral e a b
    ETupleLiteral _ a     -> ETupleLiteral e a
    ERecordLiteral _ a    -> ERecordLiteral e a
    EIdentifier _ a       -> EIdentifier e a
    ESemi _ a b           -> ESemi e a b
    EMethodApp _ a b c    -> EMethodApp e a b c
    ETypeLookup _ a b     -> ETypeLookup e a b
    EAs _ a b             -> EAs e a b
    EUnIntrinsic _ a b    -> EUnIntrinsic e a b
    EBinIntrinsic _ a b c -> EBinIntrinsic e a b c
    EIntrinsic _ a        -> EIntrinsic e a
    EIfThenElse _ a b c   -> EIfThenElse e a b c
    EWhile _ a b          -> EWhile e a b
    EFor _ a b c          -> EFor e a b c
    EReturn _ a           -> EReturn e a
    EBreak _              -> EBreak e
    EThrow _ a b          -> EThrow e a b
    ETryCatch _ a b c     -> ETryCatch e a b c
    EInstancePlaceholder _ a -> EInstancePlaceholder e a
    EInstanceDict _ a b   -> EInstanceDict e a b
    EInstanceArgument _ a -> EInstanceArgument e a
    EInstanceFieldMap _ a -> EInstanceFieldMap e a

data TypeIdent
    -- TODO: split into two?  TypeIdent and TypeApplication
    = TypeIdent UnresolvedReference [TypeIdent]
    | RecordIdent [(Name, Maybe Mutability, TypeIdent)]
    | FunctionIdent [TypeIdent] TypeIdent
    | ArrayIdent Mutability TypeIdent
    | TupleTypeIdent [TypeIdent]
    | OptionIdent TypeIdent
    | WildcardIdent
    deriving (Show, Eq)
