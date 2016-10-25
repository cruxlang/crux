{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable, DeriveGeneric #-}

module Crux.TypeVar
    ( RecordField(..)
    , FieldMutability(..)
    , TVariant(..)
    , TraitImplIdentity(..)
    , TDataTypeDef(..)
    , TraitDesc(..)
    , TraitIdentity(..)
    , Strength (..)
    , TypeSource(..)
    , TypeNumber
    , TypeVar(..)
    , RecordConstraint(..)
    , ConstraintSet(..)
    , emptyConstraintSet
    , TypeState(..)
    , TypeLevel(..)
    , newTypeVar
    , followTypeVar
    , renderTypeVarIO
    , dataTypeIdentity
    ) where

import Crux.ModuleName (ModuleName)
import Crux.Prelude
import qualified Data.Text as Text
import System.IO.Unsafe (unsafePerformIO)
import Crux.Pos
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type Name = Text

-- TDataTypeDef

data TVariant typevar = TVariant
    { tvName       :: Name
    , tvParameters :: [typevar]
    } deriving (Show, Eq, Functor, Foldable, Traversable)

data TDataTypeDef typevar = TDataTypeDef
    { tuName       :: !Name
    , tuModuleName :: !ModuleName
    , tuParameters :: ![typevar]
    , tuVariants   :: ![TVariant typevar]
    } deriving (Show, Eq, Functor, Foldable, Traversable)

-- Traits

data TraitImplIdentity
    = DataIdentity Name ModuleName
    | RecordIdentity
    deriving (Show, Eq, Ord, Generic)

instance Hashable TraitImplIdentity
    
dataTypeIdentity :: TDataTypeDef a -> TraitImplIdentity
dataTypeIdentity ut = DataIdentity (tuName ut) (tuModuleName ut)

data TraitDesc = TraitDesc
    { tdName :: Name
    , tdModule :: ModuleName
    , tdTypeVar :: TypeVar
    , tdMethods :: [(Name, TypeVar)]
    }
    deriving (Eq, Show)

data TraitIdentity = TraitIdentity ModuleName Name
    deriving (Eq, Ord, Show, Generic)

instance Hashable TraitIdentity

-- Records

-- Concrete, closed fields must be Mutable | Immutable
-- Quantified type variables can be RQuantified
-- Free type variables can be RFree
data FieldMutability
    = RMutable
    | RImmutable
    | RQuantified
    | RFree
    deriving (Show, Eq)

data RecordField typevar = RecordField
    { trName  :: Name
    , trMut   :: FieldMutability
    , trTyVar :: typevar
    } deriving (Show, Eq, Functor, Foldable, Traversable)

-- Constraints

data RecordConstraint = RecordConstraint
    { rcFields :: [RecordField TypeVar]
    , rcFieldType :: Maybe TypeVar
    }
    deriving (Eq, Show)

data ConstraintSet = ConstraintSet (Maybe RecordConstraint) (Set TraitIdentity)
    deriving (Eq, Show)

emptyConstraintSet :: ConstraintSet
emptyConstraintSet = ConstraintSet Nothing mempty

-- Type Variables

type TypeNumber = Int

data Strength = Strong | Weak
    deriving (Eq, Show)

-- Per http://okmij.org/ftp/ML/generalization.html
newtype TypeLevel = TypeLevel { unTypeLevel :: Int }
    deriving (Eq, Num, Ord)

instance Show TypeLevel where
    show (TypeLevel i) = show i

data TypeSource
    = ExplicitName Name Pos
    -- TODO: put a source on Unbound type variables too
    | Instantiation
    deriving (Eq, Show)

data TypeState
    = TUnbound Strength TypeLevel ConstraintSet TypeNumber
    | TBound TypeVar
    deriving (Eq, Show)

-- this should be called Type probably, but tons of code calls it TypeVar
data TypeVar
    = TypeVar (IORef TypeState)
    | TQuant TypeSource ConstraintSet TypeNumber
    | TFun [TypeVar] TypeVar
    | TDataType (TDataTypeDef TypeVar)
    | TRecord [RecordField TypeVar]
    | TTypeFun [TypeVar] TypeVar
    deriving (Eq)

-- Functions and Instances

unsafeShowRef :: Show a => IORef a -> String
unsafeShowRef ref = show $ unsafePerformIO $ readIORef ref

-- This instance is only for debugging
-- TODO: showsPrec
instance Show TypeVar where
    show (TypeVar r) = "(TypeVar " ++ unsafeShowRef r ++ ")"
    show (TQuant source constraints tn) = "(TQuant " ++ show source ++ " " ++ show constraints ++ " " ++ show tn ++ ")"
    show (TFun args rv) = "(TFun " ++ show args ++ " " ++ show rv ++ ")"
    show (TDataType def) = "(TDataType " ++ show def ++ ")"
    show (TRecord rows) = "(TRecord " ++ show rows ++ ")"
    show (TTypeFun args rv) = "(TTypeFun " ++ show args ++ " " ++ show rv ++ ")"

newTypeVar :: MonadIO m => TypeState -> m TypeVar
newTypeVar tv = TypeVar <$> newIORef tv

followTypeVar :: MonadIO m => TypeVar -> m TypeVar
followTypeVar tvar = case tvar of
    TypeVar ref ->
        readIORef ref >>= \case
            TBound tv -> followTypeVar tv
            TUnbound {} -> return tvar
    _ ->
        return tvar

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

-- Render TypeVar

-- TypeVar Renderer state
data TVRState = TVRState
    { tvrCount :: IORef Int
    , tvrNames :: IORef (Map.Map Int String)
    }

getNextVarName :: MonadIO m => TVRState -> m String
getNextVarName state = do
    current <- readIORef $ tvrCount state
    let next = current + 1
    writeIORef (tvrCount state) next
    return $ "_t" <> show next

showRecordFieldsIO' :: MonadIO m => TVRState -> [RecordField TypeVar] -> m String
showRecordFieldsIO' state fields = do
    fields' <- for fields $ showRecordField state
    return $ "{" <> intercalate ", " fields' <> "}"

showRecordField :: MonadIO m => TVRState -> RecordField TypeVar -> m String
showRecordField state RecordField{..} = do
    typeName <- showTypeVarIO' state trTyVar
    let mutPrefix = case trMut of
            RMutable -> "mutable "
            RImmutable -> ""
            RFree -> "mutable? "
            RQuantified -> "mutable? "
    return $ mutPrefix <> (Text.unpack trName) <> ": " <> typeName

showRecordConstraint :: MonadIO m => TVRState -> RecordConstraint -> m String
showRecordConstraint state RecordConstraint{..} = do
    fields <- for rcFields $ showRecordField state
    elts <- case rcFieldType of
        Nothing -> do
            return $ fields ++ ["..."]
        Just tv -> do
            first <- showTypeVarIO' state tv
            return $ fields ++ ["...: " ++ first]
    return $ "{" <> intercalate ", " elts <> "}"

showConstraintSet :: MonadIO m => TVRState -> ConstraintSet -> m String
showConstraintSet state (ConstraintSet recordConstraint traitSet) = do
    x <- for recordConstraint $ showRecordConstraint state
    let xs = fmap (\(TraitIdentity _m n) -> Text.unpack n) $ Set.toList traitSet
    return $ intercalate "+" $ case x of
        Nothing -> xs
        Just one -> one : xs 

showTypeVarIO' :: MonadIO m => TVRState -> TypeVar -> m String
showTypeVarIO' state = \case
    TypeVar ref -> readIORef ref >>= \case
        TUnbound _strength _level constraints i -> do
            -- TODO: should we show the user the strength?
            -- TODO: put constraints in an addendum list
            names <- readIORef $ tvrNames state
            name' <- case Map.lookup i names of
                Just name -> return name
                Nothing -> do
                    name <- getNextVarName state
                    writeIORef (tvrNames state) $ Map.insert i name names
                    return name
            constraintsString <- showConstraintSet state constraints
            return $ name' <> if constraints == emptyConstraintSet then "" else (": " <> constraintsString)
        TBound tv -> do
            showTypeVarIO' state tv
    TQuant typeSource constraints i -> do
        name' <- case typeSource of
            ExplicitName name _pos -> return $ Text.unpack name
            Instantiation -> do
                names <- readIORef $ tvrNames state
                case Map.lookup i names of
                    Just name -> return name
                    Nothing -> do
                        name <- getNextVarName state
                        writeIORef (tvrNames state) $ Map.insert i name names
                        return name
        constraintsString <- showConstraintSet state constraints
        return $ name' <> if constraints == emptyConstraintSet then "" else (": " <> constraintsString)
    TFun args ret -> do
        as <- for args $ showTypeVarIO' state
        rs <- showTypeVarIO' state ret
        return $ "(" ++ intercalate "," as ++ ") => " ++ rs
    TDataType def -> do
        tvs <- for (tuParameters def) $ showTypeVarIO' state
        return $ case (tuModuleName def, tuName def) of
            ("tuple", "Tuple2") -> "(" <> intercalate ", " tvs <> ")"
            ("tuple", "Tuple3") -> "(" <> intercalate ", " tvs <> ")"
            ("tuple", "Tuple4") -> "(" <> intercalate ", " tvs <> ")"
            ("tuple", "Tuple5") -> "(" <> intercalate ", " tvs <> ")"
            ("tuple", "Tuple6") -> "(" <> intercalate ", " tvs <> ")"
            ("tuple", "Tuple7") -> "(" <> intercalate ", " tvs <> ")"
            ("tuple", "Tuple8") -> "(" <> intercalate ", " tvs <> ")"
            _ -> (Text.unpack $ tuName def) ++ if tvs /= [] then "<" ++ (intercalate ", " tvs) ++ ">" else ""
    TRecord fields -> do
        showRecordFieldsIO' state fields
    TTypeFun args ret -> do
        args' <- for args $ showTypeVarIO' state
        rs <- showTypeVarIO' state ret
        return $ "TTypeFun " ++ show args' ++ " " ++ rs

-- | Returns a human-readable representation of a given TypeVar
-- in Crux syntax.
renderTypeVarIO :: MonadIO m => TypeVar -> m String
renderTypeVarIO tv = do
    count <- newIORef 0
    names <- newIORef mempty
    let state = TVRState
            { tvrCount = count
            , tvrNames = names
            }
    showTypeVarIO' state tv
