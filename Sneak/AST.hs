{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
module Sneak.AST where

import Sneak.Prelude
import qualified Data.Text as Text
import qualified Sneak.JSTree as JSTree
import qualified Sneak.Tokens as Tokens

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

data FunDef idtype edata = FunDef edata Name [Name] (Expression idtype edata)
    deriving (Show, Eq, Functor)

data JSVariant = JSVariant Name JSTree.Literal
    deriving (Show, Eq)

data TypeAlias = TypeAlias Name [Name] TypeIdent
    deriving (Show, Eq)

-- TODO: to support the "let rec" proposal, change DFun into DFunGroup
-- note that individual functions in a function group can be exported.
data DeclarationType idtype edata
    = DLet edata LetMutability Name (Maybe TypeIdent) (Expression idtype edata)
    | DFun (FunDef idtype edata)
    | DData Name [TypeVariable] [Variant]
    | DJSData Name [JSVariant]
    | DType TypeAlias
    deriving (Show, Eq, Functor)

data ExportFlag = Export | NoExport
    deriving (Show, Eq)

data Declaration idtype edata = Declaration ExportFlag (DeclarationType idtype edata)
    deriving (Show, Eq, Functor)

newtype ModuleSegment = ModuleSegment { unModuleSegment :: Text }
    deriving (Show, Eq, Generic)
instance Hashable ModuleSegment

data ModuleName = ModuleName [ModuleSegment] ModuleSegment
    deriving (Show, Eq, Generic)
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
    , mDecls :: [Declaration idtype edata]
    }
    deriving (Show, Eq)

type ParsedModule = Module UnresolvedReference Tokens.Pos
type LoadedModule = Module ResolvedReference ImmutableTypeVar

data Pattern
    = PConstructor Name [Pattern]
    | PPlaceholder Name
    deriving (Show, Eq)

data Case idtype edata = Case Pattern (Expression idtype edata)
    deriving (Show, Eq)

instance Functor (Case idtype) where
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
    | INot input
    | IPrint [input]
    | IToString input
    deriving (Show, Eq)

mapIntrinsicInputs :: Monad m => (a -> m b) -> Intrinsic a -> m (Intrinsic b)
mapIntrinsicInputs action intrin = do
    case intrin of
        IUnsafeJs text ->
            return $ IUnsafeJs text
        IUnsafeCoerce input ->
            fmap IUnsafeCoerce $ action input
        INot input ->
            fmap INot $ action input
        IPrint inputs ->
            fmap IPrint $ mapM action inputs
        IToString input ->
            fmap IToString $ action input

type IntrinsicId idtype edata = Intrinsic (Expression idtype edata)

data LetMutability
    = LMutable
    | LImmutable
    deriving (Show, Eq)

data Expression idtype edata
    = ELet edata LetMutability Name (Maybe TypeIdent) (Expression idtype edata)
    | EFun edata [Text] (Expression idtype edata)
    | ERecordLiteral edata (HashMap Name (Expression idtype edata))
    | ELookup edata (Expression idtype edata) Name
    | EApp edata (Expression idtype edata) [Expression idtype edata]
    | EMatch edata (Expression idtype edata) [Case idtype edata]
    | EAssign edata (Expression idtype edata) (Expression idtype edata)
    | ELiteral edata Literal
    | EIdentifier edata idtype
    | ESemi edata (Expression idtype edata) (Expression idtype edata)
    | EBinIntrinsic edata BinIntrinsic (Expression idtype edata) (Expression idtype edata)
    | EIntrinsic edata (IntrinsicId idtype edata)
    | EIfThenElse edata (Expression idtype edata) (Expression idtype edata) (Expression idtype edata)
    | EWhile edata (Expression idtype edata) (Expression idtype edata)
    | EReturn edata (Expression idtype edata)
    | EBreak edata
    deriving (Show, Eq)

instance Functor (Expression idtype) where
    fmap f expr = case expr of
        ELet d mut pat typeAnn subExpr -> ELet (f d) mut pat typeAnn (fmap f subExpr)
        EFun d argNames body -> EFun (f d) argNames (fmap f body)
        ERecordLiteral d fields -> ERecordLiteral (f d) (fmap (fmap f) fields)
        ELookup d subExpr prop -> ELookup (f d) (fmap f subExpr) prop
        EApp d lhs args -> EApp (f d) (fmap f lhs) (map (fmap f) args)
        EMatch d matchExpr cases -> EMatch (f d) (fmap f matchExpr) (fmap (fmap f) cases)
        EAssign d lhs rhs -> EAssign (f d) (fmap f lhs) (fmap f rhs)
        ELiteral d l -> ELiteral (f d) l
        EIdentifier d i -> EIdentifier (f d) i
        ESemi d lhs rhs -> ESemi (f d) (fmap f lhs) (fmap f rhs)
        EBinIntrinsic d intrin lhs rhs -> EBinIntrinsic (f d) intrin (fmap f lhs) (fmap f rhs)
        EIntrinsic d i -> case i of
            IUnsafeJs txt -> EIntrinsic (f d) (IUnsafeJs txt)
            IUnsafeCoerce subExpr -> EIntrinsic (f d) (IUnsafeCoerce $ fmap f subExpr)
            INot subExpr -> EIntrinsic (f d) (INot (fmap f subExpr))
            IPrint args -> EIntrinsic (f d) (IPrint $ map (fmap f) args)
            IToString arg -> EIntrinsic (f d) (IToString $ fmap f arg)
        EIfThenElse d condition ifTrue ifFalse -> EIfThenElse (f d) (fmap f condition) (fmap f ifTrue) (fmap f ifFalse)
        EWhile d cond body -> EWhile (f d) (fmap f cond) (fmap f body)
        EReturn d rv -> EReturn (f d) (fmap f rv)
        EBreak d -> EBreak (f d)

edata :: Expression idtype edata -> edata
edata expr = case expr of
    ELet ed _ _ _ _ -> ed
    EFun ed _ _ -> ed
    ERecordLiteral ed _ -> ed
    ELookup ed _ _ -> ed
    EApp ed _ _ -> ed
    EMatch ed _ _ -> ed
    EAssign ed _ _ -> ed
    ELiteral ed _ -> ed
    EIdentifier ed _ -> ed
    ESemi ed _ _ -> ed
    EBinIntrinsic ed _ _ _ -> ed
    EIntrinsic ed _ -> ed
    EIfThenElse ed _ _ _ -> ed
    EWhile ed _ _ -> ed
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
    } deriving (Show, Eq, Functor, Foldable, Traversable)

data TUserTypeDef typevar = TUserTypeDef
    { tuName       :: Name
    , tuParameters :: [typevar]
    , tuVariants   :: [TVariant typevar]
    } deriving (Show, Eq)

-- Huge hack: don't try to flatten variants because they can be recursive
-- TODO: figure this out so we can use automagically-generated maps

instance Functor TUserTypeDef where
    fmap f TUserTypeDef{..} = TUserTypeDef
        { tuName = tuName
        , tuParameters = fmap f tuParameters
        , tuVariants = []
        }

instance Foldable TUserTypeDef where
    foldMap f TUserTypeDef{..} = mconcat $ map f tuParameters

instance Traversable TUserTypeDef where
    traverse f TUserTypeDef{..} = TUserTypeDef <$> pure tuName <*> traverse f tuParameters <*> pure []

data RowMutability
    = RMutable
    | RImmutable
    | RQuantified
    | RFree
    deriving (Show, Eq)

data TypeRow typevar = TypeRow
    { trName :: Name
    , trMut :: RowMutability
    , trTyVar :: typevar
    } deriving (Show, Eq)

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

data Type
    = TVar Int (VarLink TypeVar)
    | TQuant Int
    | TFun [TypeVar] TypeVar
    | TUserType (TUserTypeDef TypeVar) [TypeVar]
    | TRecord (RecordType TypeVar)
    | TPrimitive PrimitiveType

newtype MutableTypeVar = MutableTypeVar Type

data ITV
    = IVar Int (VarLink ImmutableTypeVar)
    | IQuant Int
    | IFun [ImmutableTypeVar] ImmutableTypeVar
    | IUserType (TUserTypeDef ImmutableTypeVar) [ImmutableTypeVar]
    | IRecord (RecordType ImmutableTypeVar)
    | IPrimitive PrimitiveType
    deriving (Show, Eq)

newtype ImmutableTypeVar = ImmutableTypeVar ITV
    deriving (Show, Eq)
