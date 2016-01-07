{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main
    ( main
    ) where

import Control.Exception (try)
import Crux.Module (newMemoryLoader, loadProgram)
import Crux.Typecheck.Types (errorToString)
import Data.FileEmbed (embedFile)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import GHCJS.Foreign.Callback (Callback, syncCallback1')
import Data.JSString.Text (textToJSString, textFromJSString)
import GHCJS.Types (JSVal, JSString, jsval)
import qualified JavaScript.Object as Object
import qualified Crux.Gen as Gen
import qualified Crux.JSBackend as JS
import qualified Data.HashMap.Strict as HashMap
import GHCJS.Marshal.Pure (pFromJSVal, pToJSVal)

compile :: Text -> IO Text
compile source = do
    let preludeSource = decodeUtf8 $(embedFile "../lib/Prelude.cx")

        loader = newMemoryLoader $ HashMap.fromList
            [ ("Prelude", preludeSource)
            , ("Main", source)
            ]

    program <- loadProgram loader "Main"
    program' <- Gen.generateProgram program
    return $ JS.generateJS program'

compileJS :: JSString -> IO JSString
compileJS src =
    textToJSString <$> compile (textFromJSString src)

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
        r <- try $ compileJS $ pFromJSVal source
        case r of
            Right code ->
                Object.setProp "result" (pToJSVal code) resultObject
            Left err -> do
                s <- errorToString err
                Object.setProp "error" (pToJSVal s) resultObject
        return $ jsval resultObject
