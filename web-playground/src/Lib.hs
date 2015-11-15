{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib
    ( run
    ) where

import           Control.Monad                 (void)
import           Control.Monad.Trans           (liftIO)
import           Crux.AST                      (ModuleName (..),
                                                ModuleSegment (..))
import qualified Crux.AST                      as AST
import qualified Crux.Backend.JS               as JS
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
import           Eval                          (js_eval, setTextContent)
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

run :: IO ()
run = runWebGUI $ \webView -> do
    Just doc <- webViewGetDomDocument webView
    Just body <- documentGetBody doc

    Just textArea <- (fmap castToHTMLTextAreaElement) <$> documentGetElementById doc "crux_source"
    Just compileButton <- (fmap castToHTMLElement) <$> documentGetElementById doc "compile"
    Just runButton <- (fmap castToHTMLElement) <$> documentGetElementById doc "run"
    Just resultDiv <- (fmap castToHTMLElement) <$> documentGetElementById doc "js_source"

    let build = do
            source <- htmlTextAreaElementGetValue textArea
            js <- compile source
            setTextContent resultDiv js
            return js

    let run = do
            js <- build
            js_eval (toJSString js)

    elementOnclick compileButton $ liftIO $ void build
    elementOnclick runButton $ liftIO $ run

    putStrLn "Setup done!"
