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

import Crux.ModuleName (ModuleName, printModuleName)
import Crux.Prelude
import Crux.TypeVar (TypeVar, renderTypeVarIO)
import qualified Data.Text as Text
import qualified Text.Parsec as P
import Text.Printf
import Crux.Pos (Pos(..))

type Name = Text

data InternalCompilerError
    = DependentModuleNotLoaded Pos ModuleName
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
    | ModuleReferenceError ModuleName Name
    | IllegalTypeApplication Name
    | TypeApplicationMismatch Name Int Int
    -- trait errors
    | NoTraitOnType TypeVar Name ModuleName
    | IncompleteImpl [Name]
    | UnexpectedImplMethod Name
    deriving (Eq, Show)

data Error
    = LexError P.ParseError
    | ParseError P.ParseError
    | ModuleNotFound ModuleName
    | CircularImport ModuleName
    | InternalCompilerError InternalCompilerError
    | TypeError Pos TypeError
    | DuplicateSymbol Pos Text
    deriving (Eq, Show)

renderError :: ModuleName -> Error -> IO String
renderError moduleName err = do
    e <- renderError' err
    return $ "At " ++ Text.unpack (printModuleName moduleName) ++ ": " ++ e

renderError' :: Error -> IO String
renderError' = \case
    LexError e -> return $ "Lex error: " ++ show e
    ParseError e -> return $ "Parse error: " ++ show e
    ModuleNotFound mn -> return $ "Module not found: " ++ (Text.unpack $ printModuleName mn)
    CircularImport mn -> return $ "Circular import: " ++ (Text.unpack $ printModuleName mn)
    InternalCompilerError ice -> return $ "ICE: " ++ case ice of
        DependentModuleNotLoaded _pos mn -> "Dependent module not loaded: " ++ (Text.unpack $ printModuleName mn)
        StoppedCheckingWithNoError -> "Stopped type checking but no errors were recorder"
    TypeError pos ue -> do
        te <- typeErrorToString ue
        return $ "Type error at " ++ formatPos pos ++ "\n" ++ te
    DuplicateSymbol pos name -> do
        return $ "Duplicate symbol at " ++ formatPos pos ++ ": " ++ Text.unpack name

formatPos :: Pos -> String
formatPos Pos{..} = printf "%i,%i" posLine posCol

getErrorName :: Error -> Text
getErrorName = \case
    LexError _ -> "text"
    ParseError _ -> "parse"
    ModuleNotFound _ -> "module-not-found"
    CircularImport _ -> "circular-import"
    InternalCompilerError _ -> "internal"
    TypeError _ _ -> "type"
    DuplicateSymbol _ _ -> "duplicate-symbol"

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
    IllegalTypeApplication{} -> "illegal-type-application"
    TypeApplicationMismatch{} -> "type-application-mismatch"
    NoTraitOnType{} -> "no-trait-on-type"
    IncompleteImpl{} -> "incomplete-impl"
    UnexpectedImplMethod{} -> "unexpected-impl-method"

typeErrorToString :: TypeError -> IO String
typeErrorToString (UnificationError message at bt) = do
    as <- renderTypeVarIO at
    bs <- renderTypeVarIO bt
    let m
            | null message = ""
            | otherwise = "\n" ++ message
    return $ printf "Unification error:\n\t%s\n\t%s\n%s" as bs m
typeErrorToString (RecordMutabilityUnificationError key message) =
    return $ printf "Unification error: Could not unify mutability of record field %s: %s" (show key) message
typeErrorToString (UnboundSymbol type_ name) =
    return $ printf "Unbound %s %s" (type_) (show name)
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
    return $ printf "Module %s does not export %s" (Text.unpack $ printModuleName moduleName) (Text.unpack name)
typeErrorToString (IllegalTypeApplication pt) = do
    return $ printf "Type %s does not take parameters" (show pt)
typeErrorToString (TypeApplicationMismatch name total applied) = do
    return $ printf "Type %s takes %i type parameters.  %i given" (show name) total applied
typeErrorToString (NoTraitOnType typeVar traitName traitModule) = do
    ts <- renderTypeVarIO typeVar
    return $ printf "Type %s does not implement trait %s (defined in %s)" ts (Text.unpack traitName) (show traitModule)
typeErrorToString (IncompleteImpl missingMethods) = do
    return $ "Impl is missing methods: " <> intercalate ", " (fmap Text.unpack missingMethods)
typeErrorToString (UnexpectedImplMethod name) = do
    return $ "Impl has method not defined by trait: " <> Text.unpack name
