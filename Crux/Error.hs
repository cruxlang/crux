{-# LANGUAGE DeriveFunctor #-}

module Crux.Error
    ( InternalCompilerError(..)
    , TypeError(..)
    , Error(..)
    , getErrorName
    , getTypeErrorName
    , renderError
    , renderError'
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
    = DependentModuleNotLoaded Tokens.Pos AST.ModuleName
    | StoppedCheckingWithNoError
    deriving (Eq, Show)

data TypeError
    = UnificationError String TypeVar TypeVar
    | RecordMutabilityUnificationError Name String
    | UnboundSymbol String Name
    | OccursCheckFailed
    | IntrinsicError String
    | NotAnLVar String
    | TdnrLhsTypeUnknown String
    | ExportError String
    | ModuleReferenceError AST.ModuleName Name
    deriving (Eq)

instance Show TypeError where
    show (UnificationError s _ _) = "UnificationError " ++ s ++ " _ _"
    show (RecordMutabilityUnificationError s m) = "RecordMutabilityUnificationError " ++ show s ++ " " ++ show m
    show (UnboundSymbol t s) = "UnboundImport " ++ t ++ " " ++ show s
    show (OccursCheckFailed) = "OccursCheckFailed "
    show (IntrinsicError s) = "IntrinsicError " ++ show s
    show (NotAnLVar t) = "NotAnLVar " ++ show t
    show (TdnrLhsTypeUnknown s) = "TdnrLhsTypeUnknown " ++ s
    show (ExportError s) = "ExportError " ++ s
    show (ModuleReferenceError mn n) = "ModuleReferenceError " ++ show mn ++ " " ++ show n

data Error
    = LexError P.ParseError
    | ParseError P.ParseError
    | ModuleNotFound AST.ModuleName
    | CircularImport AST.ModuleName
    | InternalCompilerError InternalCompilerError
    | TypeError Tokens.Pos TypeError
    deriving (Eq, Show)

renderError :: AST.ModuleName -> Error -> IO String
renderError moduleName err = do
    e <- renderError' err
    return $ "At " ++ Text.unpack (AST.printModuleName moduleName) ++ ": " ++ e

renderError' :: Error -> IO String
renderError' (LexError e) = return $ "Lex error: " ++ show e
renderError' (ParseError e) = return $ "Parse error: " ++ show e
renderError' (ModuleNotFound mn) = return $ "Module not found: " ++ (Text.unpack $ AST.printModuleName mn)
renderError' (CircularImport mn) = return $ "Circular import: " ++ (Text.unpack $ AST.printModuleName mn)
renderError' (InternalCompilerError ice) = return $ "ICE: " ++ case ice of
    DependentModuleNotLoaded _pos mn -> "Dependent module not loaded: " ++ (Text.unpack $ AST.printModuleName mn)
    StoppedCheckingWithNoError -> "Stopped type checking but no errors were recorder"
renderError' (TypeError pos ue) = do
    te <- typeErrorToString ue
    return $ "Type error at " ++ formatPos pos ++ "\n" ++ te

formatPos :: Tokens.Pos -> String
formatPos Tokens.Pos{..} = printf "%i,%i" posLine posCol

getErrorName :: Error -> Text
getErrorName = \case
    LexError _ -> "text"
    ParseError _ -> "parse"
    ModuleNotFound _ -> "module-not-found"
    CircularImport _ -> "circular-import"
    InternalCompilerError _ -> "internal"
    TypeError _ _ -> "type"

getTypeErrorName :: TypeError -> Text
getTypeErrorName = \case
    UnificationError{} -> "unification"
    RecordMutabilityUnificationError{} -> "record-mutability-unification"
    UnboundSymbol t _ -> "unbound-" <> Text.pack t
    OccursCheckFailed{} -> "occurs-check"
    IntrinsicError{} -> "intrinsic"
    NotAnLVar{} -> "not-an-lvar"
    TdnrLhsTypeUnknown{} -> "tdnr-lhs-type-unknown"
    ExportError{} -> "export"
    ModuleReferenceError{} -> "module-reference"

typeErrorToString :: TypeError -> IO String
typeErrorToString (UnificationError message at bt) = do
    as <- showTypeVarIO at
    bs <- showTypeVarIO bt
    let m
            | null message = ""
            | otherwise = "\n" ++ message
    return $ printf "Unification error:\n\t%s\n\t%s\n%s" as bs m
typeErrorToString (RecordMutabilityUnificationError key message) =
    return $ printf "Unification error: Could not unify mutability of record field %s: %s" (show key) message
typeErrorToString (UnboundSymbol type_ name) =
    return $ printf "Unbound %s %s" (show type_) (show name)
typeErrorToString (OccursCheckFailed) =
    return $ printf "Occurs check failed"
typeErrorToString (IntrinsicError message) =
    return $ printf "%s" message
typeErrorToString (NotAnLVar s) = do
    return $ printf "Not an LVar\n\t%s" s
typeErrorToString (TdnrLhsTypeUnknown s) = do
    return $ printf "Methods only work on values with known concrete types\n\t%s" s
typeErrorToString (ExportError s) = do
    return $ printf "Export error at %s" s
typeErrorToString (ModuleReferenceError moduleName name) = do
    return $ printf "Module %s does not export %s" (Text.unpack $ AST.printModuleName moduleName) (Text.unpack name)
