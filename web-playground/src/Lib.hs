{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib
    ( run
    ) where

import qualified Data.HashMap.Strict as HashMap
import Control.Exception (try)
import GHCJS.Foreign
import qualified Crux.JSBackend as JS
import Crux.Typecheck.Types (errorToString)
import GHCJS.Types (JSRef, JSString)
import qualified Crux.Gen as Gen
import Crux.Module (newMemoryLoader, loadProgram)
import Data.FileEmbed (embedFile)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Eval (js_setTheFunction)
import GHCJS.DOM (runWebGUI)

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
    toJSString <$> compile (fromJSString src)

run :: IO ()
run = runWebGUI $ \_webView -> do
    let returnViaArgument :: (JSRef a -> IO (JSRef b)) -> JSRef a -> JSRef c -> IO ()
        returnViaArgument f arg retObj = do
            r <- try $ f arg
            case r of
                Right code ->
                    setProp ("result" :: String) code retObj
                Left err -> do
                    s <- toJSString <$> errorToString err
                    setProp ("error" :: String) s retObj

    callback <- syncCallback2 NeverRetain False (returnViaArgument compileJS)
    js_setTheFunction callback

    putStrLn "Setup done!"
