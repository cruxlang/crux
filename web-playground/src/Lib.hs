{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib
    ( run
    ) where

import Control.Exception (try)
import Crux.Module (newMemoryLoader, loadProgram)
import Crux.Typecheck.Types (errorToString)
import Data.FileEmbed (embedFile)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import GHCJS.Concurrent (OnBlocked(ThrowWouldBlock))
import GHCJS.Foreign.Callback (syncCallback2)
import Data.JSString.Text (textToJSString, textFromJSString)
import GHCJS.Types (JSVal, JSString)
import JavaScript.Object (setProp)
import qualified Crux.Gen as Gen
import qualified Crux.JSBackend as JS
import qualified Data.HashMap.Strict as HashMap
import GHCJS.Marshal.Pure (pFromJSVal, pToJSVal)
import Unsafe.Coerce (unsafeCoerce)

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

foreign import javascript unsafe "window.hs_compileCrux = $1; window.postMessage('crux-playground-loaded', '*')"
    js_setTheFunction :: JSVal -> IO ()

run :: IO ()
run = do
    callback <- syncCallback2 ThrowWouldBlock $ \source resultObject -> do
        r <- try $ compileJS $ pFromJSVal source
        case r of
            Right code ->
                setProp "result" (pToJSVal code) (unsafeCoerce resultObject)
            Left err -> do
                s <- errorToString err
                setProp "error" (pToJSVal s) (unsafeCoerce resultObject)
    js_setTheFunction $ unsafeCoerce callback
