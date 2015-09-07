module Sneak.Typecheck.Types where

import           Data.List     (intercalate)
import qualified Data.Text     as Text
import           Sneak.AST     (LetMutability, MutableTypeVar (..),
                                RecordOpen (..), RecordType (..),
                                ResolvedReference, TUserTypeDef (..), TypeAlias,
                                TypeRow (..), TypeVar, UnresolvedReference,
                                VarLink (..))
import           Sneak.Prelude

data Env = Env
    { eNextTypeIndex :: IORef Int
    , eBindings      :: IORef (HashMap UnresolvedReference (ResolvedReference, LetMutability, TypeVar))
    , eTypeBindings  :: HashMap UnresolvedReference (ResolvedReference, TypeVar)
    , eTypeAliases   :: HashMap Text TypeAlias
    , eReturnType    :: Maybe TypeVar -- Nothing if top-level expression
    , eInLoop        :: !Bool
    }

showTypeVarIO :: TypeVar -> IO [Char]
showTypeVarIO tvar = do
    tvar' <- readIORef tvar
    case tvar' of
        TVar i o' -> do
            os <- case o' of
                Unbound -> return $ "Unbound " ++ show i
                Link x -> showTypeVarIO x
            return $ "(TVar " ++ show i ++ " " ++ os ++ ")"
        TQuant i ->
            return $ "TQuant " ++ show i
        TFun arg ret -> do
            as <- mapM showTypeVarIO arg
            rs <- showTypeVarIO ret
            return $ "TFun (" ++ intercalate "," as ++ ") -> " ++ rs
        TUserType def tvars -> do
            tvs <- mapM showTypeVarIO tvars
            return $ (Text.unpack $ tuName def) ++ " " ++ (intercalate " " tvs)
        TRecord (RecordType open' rows') -> do
            let rowNames = map trName rows'
            rowTypes <- mapM (showTypeVarIO . trTyVar) rows'
            let showRow (name, ty) = Text.unpack name <> ": " <> ty
            let dotdotdot = case open' of
                    RecordFree -> ["f..."]
                    RecordQuantified -> ["q..."]
                    RecordClose -> []
            return $ "{" <> (intercalate "," (map showRow (zip rowNames rowTypes) <> dotdotdot)) <> "}"
        TPrimitive ty ->
            return $ show ty
