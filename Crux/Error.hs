{-# LANGUAGE DeriveFunctor #-}

module Crux.Error
    ( InternalCompilerError(..)
    , TypeError(..)
    , Error(..)
    , renderError
    ) where

import qualified Crux.AST as AST
import qualified Text.Parsec as P
import qualified Data.Text as Text
import qualified Crux.Tokens as Tokens
import Crux.Prelude
import Crux.TypeVar (TypeVar, showTypeVarIO)
import Text.Printf

type Name = Text

data InternalCompilerError
    = DependentModuleNotLoaded AST.ModuleName
    | StoppedCheckingWithNoError
    deriving (Eq, Show)

data TypeError a
    = UnificationError a String TypeVar TypeVar
    | RecordMutabilityUnificationError a Name String
    | UnboundSymbol a AST.UnresolvedReference
    | OccursCheckFailed a
    | IntrinsicError a String
    | NotAnLVar a String
    | TdnrLhsTypeUnknown a String
    | ExportError a String
    | ModuleReferenceError a AST.ModuleName Name
    deriving (Eq, Typeable, Functor)

-- TODO: kill this
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

-- TODO: kill this
instance (Show a, Typeable a) => Exception (TypeError a)

data Error
    = LexError P.ParseError
    | ParseError P.ParseError
    | UnknownModule AST.ModuleName
    | ModuleNotFound AST.ModuleName
    | InternalCompilerError InternalCompilerError
    | TypeError (TypeError Tokens.Pos)
    deriving (Eq, Show)

renderError :: Error -> IO String
renderError (LexError e) = return $ "Lex error: " ++ show e
renderError (ParseError e) = return $ "Parse error: " ++ show e
renderError (UnknownModule mn) = return $ "Unknown module: " ++ (Text.unpack $ AST.printModuleName mn)
renderError (ModuleNotFound mn) = return $ "Module not found: " ++ (Text.unpack $ AST.printModuleName mn)
renderError (InternalCompilerError ice) = return $ "ICE: " ++ case ice of
    DependentModuleNotLoaded mn -> "Dependent module not loaded: " ++ (Text.unpack $ AST.printModuleName mn)
    StoppedCheckingWithNoError -> "Stopped type checking but no errors were recorder"
renderError (TypeError ue) = typeErrorToString ue

formatPos :: Tokens.Pos -> String
formatPos Tokens.Pos{..} = printf "%i,%i" posLine posCol

typeErrorToString :: TypeError Tokens.Pos -> IO String
typeErrorToString (UnificationError pos message at bt) = do
    as <- showTypeVarIO at
    bs <- showTypeVarIO bt
    let m
            | null message = ""
            | otherwise = "\n" ++ message
    return $ printf "Unification error at %s\n\t%s\n\t%s%s" (formatPos pos) as bs m
typeErrorToString (RecordMutabilityUnificationError pos key message) =
    return $ printf "Unification error at %s: Could not unify mutability of record field %s: %s" (formatPos pos) (show key) message
typeErrorToString (UnboundSymbol pos message) =
    return $ printf "Unbound symbol at %s\n\t%s" (formatPos pos) (show message)
typeErrorToString (OccursCheckFailed pos) =
    return $ printf "Occurs check failed at %s" (formatPos pos)
typeErrorToString (IntrinsicError pos message) =
    return $ printf "%s at %s" message (formatPos pos)
typeErrorToString (NotAnLVar pos s) = do
    return $ printf "Not an LVar at %s\n\t%s" (formatPos pos) s
typeErrorToString (TdnrLhsTypeUnknown pos s) = do
    return $ printf "Methods only work on values with known concrete types at %s\n\t%s" (formatPos pos) s
typeErrorToString (ExportError pos s) = do
    return $ printf "Export error at %s: %s" (formatPos pos) s
typeErrorToString (ModuleReferenceError pos moduleName name) = do
    return $ printf "Module %s does not export %s at %s" (Text.unpack $ AST.printModuleName moduleName) (Text.unpack name) (formatPos pos)
