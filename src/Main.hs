{-# LANGUAGE OverloadedStrings, TemplateHaskell, LambdaCase #-}

module Main
    ( main
    ) where

import Crux.Module (newMemoryLoader, loadProgram, pathToModuleName)
import Data.FileEmbed (embedDir, embedFile)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import GHCJS.Foreign.Callback (Callback, syncCallback1')
import GHCJS.Types (JSVal, jsval)
import qualified JavaScript.Object as Object
import qualified Crux.AST as AST
import qualified Crux.Error as Error
import qualified Crux.Gen as Gen
import qualified Crux.JSBackend as JS
import Crux.ModuleName (ModuleName)
import qualified Data.HashMap.Strict as HashMap
import GHCJS.Marshal.Pure (pFromJSVal, pToJSVal)
import qualified Data.Text.Encoding as TE

baseModuleFiles :: [(FilePath, ByteString)]
baseModuleFiles = $(embedDir "../crux/lib")

rtsFile :: ByteString
rtsFile = $(embedFile "../crux/rts/rts.js")

compile :: Text -> IO (Either (Maybe ModuleName, Error.Error) Text)
compile source = do
    let decodeBaseModule (path, contents) =
            (pathToModuleName path, decodeUtf8 contents)
    let baseModules = fmap decodeBaseModule baseModuleFiles

    let mapping = mconcat
            [ HashMap.fromList baseModules
            , HashMap.singleton "main" source
            ]
    let loader = newMemoryLoader mapping
    {-
        loader = newMemoryLoader $ HashMap.fromList
            [ ("Prelude", preludeSource)
            , ("Main", source)
            ]
    -}
    loadProgram loader "main" >>= \case
        Left err -> return $ Left err
        Right program -> do
            program' <- Gen.generateProgram program
            return $ Right $ JS.generateJS (TE.decodeUtf8 rtsFile) program'

foreign import javascript unsafe "global[$1] = $2"
    js_exportFunction :: JSVal -> JSVal -> IO ()

exportCallback :: String -> IO (Callback a) -> IO ()
exportCallback name cbAction = do
    cb <- cbAction
    js_exportFunction (pToJSVal name) (jsval cb)

main :: IO ()
main = do
    -- TODO: we can return the result object instead of taking it as an argument
    -- when we upgrade ghcjs.  we would use syncCallback1' in that case.
    exportCallback "compileCrux" $ syncCallback1' $ \source -> do
        resultObject <- Object.create
        r <- compile $ pFromJSVal source
        case r of
            Right code ->
                Object.setProp "result" (pToJSVal code) resultObject
            Left (moduleName, err) -> do
                s <- Error.renderError moduleName err
                Object.setProp "error" (pToJSVal s) resultObject
        return $ jsval resultObject
