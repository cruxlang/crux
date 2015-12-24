{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib
    ( run
    ) where

import           Control.Monad                 (void)
import           Control.Monad.Trans           (liftIO)
import qualified Data.HashMap.Strict as HashMap
import           Control.Exception (try)
import           GHCJS.Foreign
import           Crux.AST                      (ModuleName (..),
                                                ModuleSegment (..))
import qualified Crux.AST                      as AST
import qualified Crux.JSBackend                as JS
import           Crux.Typecheck.Types          (UnificationError, errorToString)
import           GHCJS.Types                   (JSRef, JSString)
import qualified Crux.Gen                      as Gen
import           Crux.Module                   (newMemoryLoader, loadProgram)
import           Data.FileEmbed                (embedFile)
import           Data.IORef                    (newIORef, readIORef)
import           Data.String                   (fromString)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Text.Encoding            (decodeUtf8)
import           Eval                          (js_eval, setTextContent, js_setTheFunction)
import           GHCJS.DOM                     (runWebGUI,
                                                webViewGetDomDocument)
import           GHCJS.DOM.Document            (documentCreateElement,
                                                documentGetBody,
                                                documentGetElementById)
import           GHCJS.DOM.Element             (elementOnclick,
                                                elementQuerySelector)
import           GHCJS.DOM.HTMLElement         (castToHTMLElement,
                                                htmlElementGetInnerText,
                                                htmlElementSetInnerText)
import           GHCJS.DOM.HTMLTextAreaElement (castToHTMLTextAreaElement)
import           GHCJS.DOM.HTMLTextAreaElement (htmlTextAreaElementGetValue)
import           GHCJS.DOM.Node                (nodeAppendChild)
import           GHCJS.Foreign                 (toJSString)

compile source = do
    let mainModuleName = "Main"
        preludeSource = decodeUtf8 $(embedFile "../lib/Prelude.cx")

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
