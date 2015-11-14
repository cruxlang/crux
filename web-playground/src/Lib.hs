{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib
    ( run
    ) where

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
import           GHCJS.DOM                     (runWebGUI,
                                                webViewGetDomDocument)
import           GHCJS.DOM.Document            (documentCreateElement,
                                                documentGetBody)
import           GHCJS.DOM.Element             (elementOnclick,
                                                elementQuerySelector)
import           GHCJS.DOM.HTMLElement         (castToHTMLElement,
                                                htmlElementGetInnerText,
                                                htmlElementSetInnerText)
import           GHCJS.DOM.HTMLTextAreaElement (castToHTMLTextAreaElement)
import           GHCJS.DOM.HTMLTextAreaElement (htmlTextAreaElementGetValue)
import           GHCJS.DOM.Node                (nodeAppendChild)

compile source = do
    putStrLn $ "compile " ++ show source
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

    Just textArea <- (fmap castToHTMLTextAreaElement) <$> documentCreateElement doc "textarea"
    nodeAppendChild body (Just textArea)

    Just runButton <- (fmap castToHTMLElement) <$> documentCreateElement doc "button"
    nodeAppendChild body (Just runButton)

    Just resultDiv <- (fmap castToHTMLElement) <$> documentCreateElement doc "div"
    nodeAppendChild body (Just resultDiv)

    elementOnclick runButton $ liftIO $ do
        source <- htmlTextAreaElementGetValue textArea
        js <- compile source
        htmlElementSetInnerText resultDiv js

    putStrLn "Setup done!"
