module Crux.Error
    ( InternalCompilerError(..)
    , AssignmentType(..)
    , TypeError(..)
    , ErrorType(..)
    , Error(..)
    , getErrorName
    , getTypeErrorName
    , renderError
    , printError
    ) where

import Crux.ModuleName (ModuleName, printModuleName)
import Crux.Prelude
import Crux.TypeVar (TraitIdentity(..), TypeVar, renderTypeVarIO)
import qualified Data.Text as Text
import System.Console.ANSI
import Text.Printf
import System.IO (Handle, hPutStrLn, hIsTerminalDevice)
import Crux.Pos (Pos(..), PosRec(..))

type Name = Text

data InternalCompilerError
    = DependentModuleNotLoaded ModuleName
    | InternalError String
    deriving (Eq, Show)

data AssignmentType
    = ImmutableBinding
    | ImmutableProperty
    | MaybeImmutableProperty
    deriving (Eq, Show)

data TypeError
    = UnificationError String TypeVar TypeVar
    | RecordMissingField TypeVar Name
    | RecordMutabilityUnificationError Name String
    | UnboundSymbol String Name
    | ImmutableAssignment AssignmentType Name
    | OccursCheckFailed
    | IntrinsicError String
    | TdnrLhsTypeUnknown String
    | ExportError String
    | ModuleReferenceError ModuleName Name
    | IllegalTypeApplication Name
    | TypeApplicationMismatch Name Int Int
    -- trait errors
    | NoTraitOnType TypeVar TraitIdentity
    | NoTraitOnRecord TypeVar TraitIdentity
    | AmbiguousPolymorphism TraitIdentity
    | IncompleteImpl [Name]
    | UnexpectedImplMethod Name
    | DuplicateSymbol Text
    deriving (Eq, Show)

data ErrorType
    -- Loading Errors
    = ModuleNotFound ModuleName [FilePath]
    | MainModuleNotFound FilePath
    | LexError String
    | ParseError String
    | CircularImport ModuleName
    -- Internal Compiler Errors
    | InternalCompilerError InternalCompilerError
    -- Type Errors
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
    MainModuleNotFound mainPath -> return $ "Main module not found: " ++ mainPath
    CircularImport mn -> return $ "Circular import: " ++ (Text.unpack $ printModuleName mn)
    InternalCompilerError ice -> return $ "compiler assertion failure: " ++ case ice of
        DependentModuleNotLoaded mn -> "Dependent module not loaded: " ++ (Text.unpack $ printModuleName mn)
        InternalError str -> str
    TypeError ue -> do
        typeErrorToString ue

renderError :: Error -> IO String
renderError (Error pos errorType) = do
    let loc = formatPos pos
    rendered <- renderErrorType errorType
    return $ loc ++ ": error: " ++ rendered

printError :: Handle -> Error -> IO ()
printError handle (Error pos errorType) = do
    isatty' <- hIsTerminalDevice handle
    let errorString = if isatty'
            then setSGRCode [SetColor Foreground Vivid Red] ++ "error" ++ setSGRCode []
            else "error"

    let loc = formatPos pos
    rendered <- renderErrorType errorType
    hPutStrLn handle $ loc ++ ": " ++ errorString ++ ": " ++ rendered

formatPos :: Pos -> String
formatPos (Pos PosRec{..}) = printf "%s:%i:%i" posFileName posLine posColumn
formatPos (SyntaxDependency filename) = printf "%s:<syntax-dependency>" filename
formatPos (GeneratedMainCall filename) = printf "%s:<generated-main-call>" filename
formatPos (InternalErrorPos filename) = printf "%s:<internal-error>" filename

getErrorName :: ErrorType -> Text
getErrorName = \case
    LexError _ -> "text"
    ParseError _ -> "parse"
    ModuleNotFound _ _ -> "module-not-found"
    MainModuleNotFound _ -> "main-module-not-found"
    CircularImport _ -> "circular-import"
    InternalCompilerError _ -> "internal"
    TypeError _ -> "type"

getTypeErrorName :: TypeError -> Text
getTypeErrorName = \case
    UnificationError{} -> "unification"
    RecordMissingField{} -> "record-missing-field"
    RecordMutabilityUnificationError{} -> "record-mutability-unification"
    UnboundSymbol t _ -> "unbound-" <> Text.pack t
    ImmutableAssignment{} -> "immutable-assignment"
    OccursCheckFailed{} -> "occurs-check"
    IntrinsicError{} -> "intrinsic"
    TdnrLhsTypeUnknown{} -> "tdnr-lhs-type-unknown"
    ExportError{} -> "export"
    ModuleReferenceError{} -> "module-reference"
    IllegalTypeApplication{} -> "illegal-type-application"
    TypeApplicationMismatch{} -> "type-application-mismatch"
    NoTraitOnType{} -> "no-trait-on-type"
    NoTraitOnRecord{} -> "no-trait-on-record"
    AmbiguousPolymorphism{} -> "ambiguous-polymorphism"
    IncompleteImpl{} -> "incomplete-impl"
    UnexpectedImplMethod{} -> "unexpected-impl-method"
    DuplicateSymbol{} -> "duplicate-symbol"

renderTraitIdentity :: TraitIdentity -> String
renderTraitIdentity (TraitIdentity traitModule traitName) = Text.unpack (printModuleName traitModule <> "." <> traitName)

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
typeErrorToString (RecordMissingField type_ fieldName) = do
    rt <- renderTypeVarIO type_
    return $ printf "Unification error: type %s has no field %s" rt fieldName
typeErrorToString (UnboundSymbol type_ name) =
    return $ (printf "unbound %s `" type_) ++ Text.unpack name ++ "`"
typeErrorToString (ImmutableAssignment atype name) = do
    let atype' = case atype of
            ImmutableBinding -> "immutable binding"
            ImmutableProperty -> "immutable property"
            MaybeImmutableProperty -> "possibly-immutable property"
    return $ printf "assignment to %s %s" (atype' :: String) name
typeErrorToString (OccursCheckFailed) =
    return $ printf "occurs check failed"
typeErrorToString (IntrinsicError message) =
    return $ printf "%s" message
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
typeErrorToString (NoTraitOnType typeVar traitIdentity) = do
    ts <- renderTypeVarIO typeVar
    return $ printf "Type %s does not implement trait %s" ts (renderTraitIdentity traitIdentity)
typeErrorToString (NoTraitOnRecord _typeVar traitIdentity) = do
    --_ts <- renderTypeVarIO typeVar
    return $ printf "Record type does not implement trait %s" (renderTraitIdentity traitIdentity)
typeErrorToString (AmbiguousPolymorphism traitIdentity) = do
    return $ printf "No concrete type in use of trait %s" (renderTraitIdentity traitIdentity)
typeErrorToString (IncompleteImpl missingMethods) = do
    return $ "impl is missing methods: " <> intercalate ", " (fmap Text.unpack missingMethods)
typeErrorToString (UnexpectedImplMethod name) = do
    return $ "impl has method not defined by trait: " <> Text.unpack name
typeErrorToString (DuplicateSymbol name) = do
        return $ "Duplicate symbol: " ++ Text.unpack name
