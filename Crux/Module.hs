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

{-
newtype IVal a = IVal (IORef a)
    deriving (Eq)

mkIVal :: a -> IO (IVal a)
mkIVal = IVal <$> newIORef a

rdIVal :: IVal a -> a
rdIVal (IVal r) = unsafeDuplablePerformIO r

instance Show a => Show (IVal a) where
    show v = "IVal " ++ show (rdIVal v)
-}

preludeSource :: Text
preludeSource = Text.pack $ [r|
data jsffi Boolean {
    True = true;
    False = false;
};
|]

type LoadedModules = HashMap AST.ModuleName

type LoadedModule = AST.Module AST.ResolvedReference AST.ImmutableTypeVar
type ModuleLoader = AST.ModuleName -> IO (Either String Parse.ParseModule)

defaultModuleLoader :: ModuleLoader
defaultModuleLoader name = do
    if name == "Prelude" then
        parseModuleFromSource "Prelude" preludeSource
    else
        return $ Left $ "unknown module"

preludeModule :: IO LoadedModule
preludeModule = do
    rv <- loadModuleFromSource' Nothing "Prelude" preludeSource
    case rv of
        Left err -> do
            throwIO $ ErrorCall $ "Failed to load Prelude: " ++ show err
        Right m -> return m

parseModuleFromSource :: FilePath -> Text -> IO (Either String Parse.ParseModule)
parseModuleFromSource filename source = do
    let l = Lex.lexSource filename source
    case l of
        Left err ->
            return $ Left $ "Lex error: " <> show err
        Right l' -> do
            p <- Parse.parse filename l'
            case p of
                Left err ->
                    return $ Left $ "Parse error: " <> show err
                Right mod' ->
                    return $ Right mod'

loadModuleFromSource' :: Maybe LoadedModule -> FilePath -> Text -> IO (Either String LoadedModule)
loadModuleFromSource' prelude filename source = do
    p <- parseModuleFromSource filename source
    case p of
        Left err ->
            return $ Left err
        Right mod' -> do
            fmap Right $ Typecheck.run prelude mod'

loadModuleFromSource :: FilePath -> Text -> IO (Either String LoadedModule)
loadModuleFromSource filename source = do
    prelude <- preludeModule
    loadModuleFromSource' (Just prelude) filename source

loadModuleFromFile :: FilePath -> IO (Either String LoadedModule)
loadModuleFromFile filename = do
    source <- BS.readFile filename
    loadModuleFromSource filename $ TE.decodeUtf8 source
