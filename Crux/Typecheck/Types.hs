{-# LANGUAGE DeriveFunctor #-}

module Crux.Typecheck.Types
    ( ValueReference(..)
    , TypeReference(..)
    , PatternBinding(..)
    , Env(..)
    , TypeError(..)
    , formatPos
    , errorToString
    ) where

import Crux.TypeVar
    ( TypeVar(..), RecordOpen(..), RecordType(..)
    , TUserTypeDef(..), TypeRow(..)
    , showTypeVarIO, renderTypeVarIO
    )
import Crux.Module.Types (LoadedModule)
import Crux.AST
    ( LetMutability, ModuleName
    , ResolvedReference, UnresolvedReference
    , TypeIdent
    , printModuleName
    )
import           Crux.Prelude
import           Crux.Tokens  (Pos (..))
import qualified Data.Text    as Text
import           Text.Printf  (printf)

-- TODO: newtype this somewhere and import it
type Name = Text

type HashTable k v = IORef (HashMap k v)

data ValueReference
    = ValueReference ResolvedReference LetMutability TypeVar
    | ModuleReference ModuleName

data TypeReference
    = TypeBinding ResolvedReference TypeVar
    | TypeAlias Name [Name] TypeIdent
    deriving (Eq)
instance Show TypeReference where
    show (TypeBinding rr _tv) = "TypeBinding " ++ show rr ++ " <typevar>"
    show (TypeAlias name params typeIdent) = "TypeAlias " ++ show name ++ " " ++ show params ++ " " ++ show typeIdent

-- same structure as TUserType constructor
data PatternBinding = PatternBinding
    (TUserTypeDef TypeVar) -- type of value being pattern matched
    [TypeVar] -- type parameters to type
    [TypeVar] -- types of data constructor arguments

data Env = Env
    { eThisModule :: ModuleName
    , eLoadedModules :: HashMap ModuleName LoadedModule
    , eNextTypeIndex :: IORef Int
    , eValueBindings :: HashTable Name ValueReference
    , eTypeBindings :: HashTable Name TypeReference
    , ePatternBindings :: HashTable Name PatternBinding
    , eReturnType :: Maybe TypeVar -- Nothing if top-level expression
    , eInLoop :: !Bool
    }

data TypeError a
    = UnificationError a String TypeVar TypeVar
    | RecordMutabilityUnificationError a Name String
    | UnboundSymbol a UnresolvedReference
    | OccursCheckFailed a
    | IntrinsicError a String
    | NotAnLVar a String
    | TdnrLhsTypeUnknown a String
    | ExportError a String
    | ModuleReferenceError a ModuleName Name
    deriving (Eq, Typeable, Functor)

instance Show a => Show (TypeError a) where
    show (UnificationError a s _ _) = "UnificationError " ++ show a ++ " " ++ s ++ " _ _"
    show (RecordMutabilityUnificationError a s m) = "RecordMutabilityUnificationError " ++ show a ++ " " ++ show s ++ " " ++ show m
    show (UnboundSymbol a s) = "UnboundSymbol " ++ show a ++ " " ++ show s
    show (OccursCheckFailed a) = "OccursCheckFailed " ++ show a
    show (IntrinsicError a s) = "IntrinsicError " ++ show a ++ " " ++ show s
    show (NotAnLVar a t) = "NotAnLVar " ++ show a ++ " " ++ show t
    show (TdnrLhsTypeUnknown a s) = "TdnrLhsTypeUnknown " ++ show a ++ " " ++ s
    show (ExportError a s) = "ExportError " ++ show a ++ " " ++ s
    show (ModuleReferenceError a mn n) = "ModuleReferenceError " ++ show a ++ " " ++ show mn ++ " " ++ show n

instance (Show a, Typeable a) => Exception (TypeError a)

formatPos :: Pos -> String
formatPos Pos{..} = printf "%i,%i" posLine posCol

errorToString :: TypeError Pos -> IO String
errorToString (UnificationError pos message at bt) = do
    as <- showTypeVarIO at
    bs <- showTypeVarIO bt
    let m
            | null message = ""
            | otherwise = "\n" ++ message
    return $ printf "Unification error at %i,%i\n\t%s\n\t%s%s" (posLine pos) (posCol pos) as bs m
errorToString (RecordMutabilityUnificationError pos key message) =
    return $ printf "Unification error at %i,%i: Could not unify mutability of record field %s: %s" (posLine pos) (posCol pos) (show key) message
errorToString (UnboundSymbol pos message) =
    return $ printf "Unbound symbol at %i,%i\n\t%s" (posLine pos) (posCol pos) (show message)
errorToString (OccursCheckFailed pos) =
    return $ printf "Occurs check failed at %i,%i" (posLine pos) (posCol pos)
errorToString (IntrinsicError pos message) =
    return $ printf "%s at %i,%i" message (posLine pos) (posCol pos)
errorToString (NotAnLVar pos s) = do
    return $ printf "Not an LVar at %i,%i\n\t%s" (posLine pos) (posCol pos) s
errorToString (TdnrLhsTypeUnknown pos s) = do
    return $ printf "Methods only work on values with known concrete types at %i,%i\n\t%s" (posLine pos) (posCol pos) s
errorToString (ExportError pos s) = do
    return $ printf "Export error at %s: %s" (formatPos pos) s
errorToString (ModuleReferenceError pos moduleName name) = do
    return $ printf "Module %s does not export %s at %i,%i" (Text.unpack $ printModuleName moduleName) (Text.unpack name) (posLine pos) (posCol pos)
