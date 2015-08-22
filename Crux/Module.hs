{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Crux.Module where

import Crux.Prelude
import Control.Exception (throwIO, ErrorCall(..))
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import Text.RawString.QQ (r)
import qualified Crux.AST as AST
import qualified Crux.Lex as Lex
import qualified Crux.Parse as Parse
import qualified Crux.Typecheck as Typecheck

preludeSource :: Text
preludeSource = Text.pack $ [r|
data jsffi Boolean {
    True = true;
    False = false;
};
|]

preludeModule :: IO (AST.Module AST.ImmutableTypeVar)
preludeModule = do
    rv <- loadModuleFromSource' Nothing "Prelude" preludeSource
    case rv of
        Left err -> do
            throwIO $ ErrorCall $ "Failed to load Prelude: " ++ show err
        Right m -> return m

loadModuleFromSource' :: Maybe (AST.Module AST.ImmutableTypeVar) -> FilePath -> Text -> IO (Either String (AST.Module AST.ImmutableTypeVar))
loadModuleFromSource' prelude filename source = do
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
                    typetree <- Typecheck.run prelude mod'
                    typetree' <- Typecheck.flattenModule typetree
                    return $ Right typetree'

loadModuleFromSource :: FilePath -> Text -> IO (Either String (AST.Module AST.ImmutableTypeVar))
loadModuleFromSource filename source = do
    prelude <- preludeModule
    loadModuleFromSource' (Just prelude) filename source

loadModuleFromFile :: FilePath -> IO (Either String (AST.Module AST.ImmutableTypeVar))
loadModuleFromFile filename = do
    source <- BS.readFile filename
    loadModuleFromSource filename $ TE.decodeUtf8 source
