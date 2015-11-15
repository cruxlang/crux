{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib
    ( run
    ) where

import           Control.Monad                 (void)
import           Control.Monad.Trans           (liftIO)
import GHCJS.Foreign
import           Crux.AST                      (ModuleName (..),
                                                ModuleSegment (..))
import qualified Crux.AST                      as AST
import qualified Crux.Backend.JS               as JS
import GHCJS.Types (JSRef, JSString)
import qualified Crux.Gen                      as Gen
import           Crux.Module                   (loadModule,
                                                loadPreludeFromSource,
                                                loadProgramFromSource,
                                                newMemoryLoader)
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
        loader = newMemoryLoader mainModuleName source
        preludeSource = decodeUtf8 $(embedFile "../lib/Prelude.cx")

    prelude <- loadPreludeFromSource preludeSource
    loadedModules <- newIORef [(fromString "Prelude", prelude)]

    mainModule <- loadModule loader loadedModules (fromString "Main")

    otherModules <- readIORef loadedModules
    let program = AST.Program
            { AST.pMainModule = mainModule
            , AST.pOtherModules = otherModules
            }

    program' <- Gen.generateProgram program
    return $ JS.generateJS program'

compileJS :: JSString -> IO JSString
compileJS src =
    toJSString <$> compile (fromJSString src)

run :: IO ()
run = runWebGUI $ \_webView -> do
    let returnViaArgument :: (JSRef a -> IO (JSRef b)) -> JSRef a -> JSRef c -> IO ()
        returnViaArgument f arg retObj = do
            r <- f arg
            setProp "ret" r retObj

    callback <- syncCallback2 NeverRetain False (returnViaArgument compileJS)
    js_setTheFunction callback

    putStrLn "Setup done!"
