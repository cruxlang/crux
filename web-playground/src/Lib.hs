module Lib
    ( run
    ) where

import qualified Crux.Backend.JS as JS
import qualified Crux.Gen        as Gen
import           Crux.Module     (loadProgramFromSource)
import           GHCJS.DOM       (runWebGUI, webViewGetDomDocument)
import GHCJS.DOM.Document (documentGetBody)
import GHCJS.DOM.Element (elementQuerySelector, elementOnclick)
import GHCJS.DOM.HTMLElement (htmlElementGetInnerText, castToHTMLElement)
import Control.Monad.Trans (liftIO)

run :: IO ()
run = runWebGUI $ \webView -> do
    Just doc <- webViewGetDomDocument webView
    Just body <- documentGetBody doc

    Just textArea <- (fmap castToHTMLElement) <$> elementQuerySelector body "#textArea"
    Just runButton <- (fmap castToHTMLElement) <$> elementQuerySelector body "#run"

    elementOnclick runButton $ liftIO $ do
        content <- htmlElementGetInnerText textArea
        program <- loadProgramFromSource "<playground>" content
        program' <- Gen.generateProgram program
        let js = JS.generateJS program'
        print js

    putStrLn "Setup done!"
