module Crux.Typecheck.Types
    ( Env(..)
    , UnificationError(..)
    , showTypeVarIO
    , formatPos
    , errorToString
    ) where

import Crux.AST
    ( LetMutability, LoadedModule, ModuleName
    , MutableTypeVar (..), RecordOpen (..)
    , RecordType (..), ResolvedReference
    , TUserTypeDef (..), TypeAlias, TypeRow (..)
    , TypeVar, UnresolvedReference
    )
import           Crux.Prelude
import           Crux.Tokens  (Pos (..))
import           Data.List    (intercalate)
import qualified Data.Text    as Text
import           Text.Printf  (printf)

-- TODO: newtype this somewhere and import it
type Name = Text

data Env = Env
    { eLoadedModules :: !(HashMap ModuleName LoadedModule)
    , eNextTypeIndex :: !(IORef Int)
    , eBindings      :: !(IORef (HashMap Name (ResolvedReference, LetMutability, TypeVar)))
    , eLocalBindings :: !(IORef (HashMap Name (ResolvedReference, LetMutability, TypeVar)))
    , eTypeBindings  :: !(IORef (HashMap Name (ResolvedReference, TypeVar)))
    , eTypeAliases   :: !(IORef (HashMap Text TypeAlias))
    , eReturnType    :: !(Maybe TypeVar) -- Nothing if top-level expression
    , eInLoop        :: !Bool
    }

data UnificationError a
    = UnificationError a String TypeVar TypeVar
    | RecordMutabilityUnificationError a Name String
    | UnboundSymbol a UnresolvedReference
    | OccursCheckFailed a
    | IntrinsicError a String
    | NotAnLVar a String
    | TdnrLhsTypeUnknown a String
    | ExportError a String
    deriving (Eq, Typeable)

instance Show a => Show (UnificationError a) where
    show (UnificationError a s _ _) = "UnificationError " ++ show a ++ " " ++ s ++ " _ _"
    show (RecordMutabilityUnificationError a s m) = "RecordMutabilityUnificationError " ++ show a ++ " " ++ show s ++ " " ++ show m
    show (UnboundSymbol a s) = "UnboundSymbol " ++ show a ++ " " ++ show s
    show (OccursCheckFailed a) = "OccursCheckFailed " ++ show a
    show (IntrinsicError a s) = "IntrinsicError " ++ show a ++ " " ++ show s
    show (NotAnLVar a t) = "NotAnLVar " ++ show a ++ " " ++ show t
    show (TdnrLhsTypeUnknown a s) = "TdnrLhsTypeUnknown " ++ show a ++ " " ++ s
    show (ExportError a s) = "ExportError " ++ show a ++ " " ++ s

instance (Show a, Typeable a) => Exception (UnificationError a)

showTypeVarIO :: TypeVar -> IO [Char]
showTypeVarIO tvar = do
    tvar' <- readIORef tvar
    case tvar' of
        TUnbound i -> do
            return $ "(TUnbound " ++ show i ++ ")"
        TBound x -> do
            inner <- showTypeVarIO x
            return $ "(TBound " ++ show inner ++ ")"
        TQuant i -> do
            return $ "TQuant " ++ show i
        TFun arg ret -> do
            as <- mapM showTypeVarIO arg
            rs <- showTypeVarIO ret
            return $ "(" ++ intercalate "," as ++ ") -> " ++ rs
        TUserType def tvars -> do
            tvs <- mapM showTypeVarIO tvars
            return $ (Text.unpack $ tuName def) ++ " " ++ (intercalate " " tvs)
        TRecord (RecordType open' rows') -> do
            let rowNames = map trName rows'
            rowTypes <- mapM (showTypeVarIO . trTyVar) rows'
            let showRow (name, ty) = Text.unpack name <> ": " <> ty
            let dotdotdot = case open' of
                    RecordFree i -> ["..._" ++ show i]
                    RecordQuantified i -> ["...t" ++ show i]
                    RecordClose -> []
            return $ "{" <> (intercalate "," (map showRow (zip rowNames rowTypes) <> dotdotdot)) <> "}"
        TPrimitive ty ->
            return $ show ty

formatPos :: Pos -> String
formatPos Pos{..} = printf "%i,%i" posLine posCol

errorToString :: UnificationError Pos -> IO String
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
