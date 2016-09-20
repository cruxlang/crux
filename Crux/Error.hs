module Crux.Error
    ( InternalCompilerError(..)
    , TypeError(..)
    , ErrorType(..)
    , Error(..)
    , getErrorName
    , getTypeErrorName
    , renderError
    ) where

import Crux.ModuleName (ModuleName, printModuleName)
import Crux.Prelude
import Crux.TypeVar (TypeVar, renderTypeVarIO)
import qualified Data.Text as Text
import Text.Printf
import Crux.Pos (Pos(..))

type Name = Text

data InternalCompilerError
    = DependentModuleNotLoaded ModuleName
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
    | DuplicateSymbol Text
    deriving (Eq, Show)

data ErrorType
    = ModuleNotFound ModuleName [FilePath]
    | LexError String
    | ParseError String
    | CircularImport ModuleName
    | InternalCompilerError InternalCompilerError
    | TypeError TypeError
    deriving (Eq, Show)

data Error = Error Pos ErrorType
    deriving (Eq, Show)

renderErrorType :: ErrorType -> IO String
renderErrorType = \case
    LexError s -> do
        return s
    ParseError s -> do
        return s
    ModuleNotFound mn triedPaths -> return $ "Module not found: " ++ (Text.unpack $ printModuleName mn) ++ "\nTried paths:\n" ++ mconcat (fmap (<> "\n") triedPaths)
    CircularImport mn -> return $ "Circular import: " ++ (Text.unpack $ printModuleName mn)
    InternalCompilerError ice -> return $ "ICE: " ++ case ice of
        DependentModuleNotLoaded mn -> "Dependent module not loaded: " ++ (Text.unpack $ printModuleName mn)
    TypeError ue -> do
        typeErrorToString ue

renderError :: Error -> IO String
renderError (Error pos errorType) = do
    let loc = formatPos pos
    rendered <- renderErrorType errorType
    return $ loc ++ ": error: " ++ rendered

formatPos :: Pos -> String
formatPos Pos{..} = printf "%s:%i:%i" posFileName posLine posColumn

getErrorName :: ErrorType -> Text
getErrorName = \case
    LexError _ -> "text"
    ParseError _ -> "parse"
    ModuleNotFound _ _ -> "module-not-found"
    CircularImport _ -> "circular-import"
    InternalCompilerError _ -> "internal"
    TypeError _ -> "type"

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
    DuplicateSymbol{} -> "duplicate-symbol"

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
    return $ (printf "unbound %s `" type_) ++ Text.unpack name ++ "`"
typeErrorToString (OccursCheckFailed) =
    return $ printf "occurs check failed"
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
typeErrorToString (DuplicateSymbol name) = do
        return $ "Duplicate symbol: " ++ Text.unpack name
