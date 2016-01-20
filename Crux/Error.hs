module Crux.Error
    ( InternalCompilerError(..)
    , Error(..)
    , renderError
    ) where

import qualified Crux.AST as AST
import qualified Text.Parsec as P
import qualified Data.Text as Text
import qualified Crux.Typecheck.Types as Typecheck
import qualified Crux.Tokens as Tokens

data InternalCompilerError
    = DependentModuleNotLoaded AST.ModuleName
    deriving (Eq, Show)

data Error
    = LexError P.ParseError
    | ParseError P.ParseError
    | UnknownModule AST.ModuleName
    | ModuleNotFound AST.ModuleName
    | InternalCompilerError InternalCompilerError
    | TypeError (Typecheck.TypeError Tokens.Pos)
    deriving (Eq, Show)

renderError :: Error -> IO String
renderError (LexError e) = return $ "Lex error: " ++ show e
renderError (ParseError e) = return $ "Parse error: " ++ show e
renderError (UnknownModule mn) = return $ "Unknown module: " ++ (Text.unpack $ AST.printModuleName mn)
renderError (ModuleNotFound mn) = return $ "Module not found: " ++ (Text.unpack $ AST.printModuleName mn)
renderError (InternalCompilerError (DependentModuleNotLoaded mn)) = return $ "ICE: Dependent module not loaded: " ++ (Text.unpack $ AST.printModuleName mn)
renderError (TypeError ue) = Typecheck.errorToString ue
