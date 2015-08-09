module Crux.Module where

import Crux.Prelude
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import qualified Crux.AST as AST
import qualified Crux.Lex as Lex
import qualified Crux.Parse as Parse
import qualified Crux.Typecheck as Typecheck

loadModuleFromSource :: FilePath -> Text -> IO (Either String (AST.Module AST.ImmutableTypeVar))
loadModuleFromSource filename source = do
    let l = Lex.lexSource filename source
    case l of
        Left err ->
            return $ Left $ "Lex error: " <> show err
        Right l' -> do
            p <- Parse.parse filename l'
            case p of
                Left err ->
                    return $ Left $ "Parse error: " <> show err
                Right mod' -> do
                    typetree <- Typecheck.run $ AST.mDecls mod'
                    typetree' <- Typecheck.flattenProgram typetree
                    return $ Right $ AST.Module { AST.mDecls = typetree' }

loadModuleFromFile :: FilePath -> IO (Either String (AST.Module AST.ImmutableTypeVar))
loadModuleFromFile filename = do
    source <- BS.readFile filename
    loadModuleFromSource filename $ TE.decodeUtf8 source
