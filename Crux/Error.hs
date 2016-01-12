module Crux.Error
    ( Error(..)
    , renderError
    ) where

import qualified Crux.AST as AST
import qualified Text.Parsec as P
import qualified Data.Text as Text
import qualified Crux.Typecheck.Types as Typecheck
import qualified Crux.Tokens as Tokens

data Error
    = LexError P.ParseError
    | ParseError P.ParseError
    | UnknownModule AST.ModuleName
    | UnificationError (Typecheck.UnificationError Tokens.Pos)
    deriving (Eq, Show)

renderError :: Error -> String
renderError (LexError e) = "Lex error: " ++ show e
renderError (ParseError e) = "Parse error: " ++ show e
renderError (UnknownModule mn) = "Unknown module: " ++ (Text.unpack $ AST.printModuleName mn)
renderError (UnificationError ue) = "Unification error: " ++ show ue
