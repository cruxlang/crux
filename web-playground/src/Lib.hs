module Lib
    ( run
    ) where

import qualified Crux.Backend.JS as JS
import qualified Crux.Gen        as Gen
import           Crux.Module     (loadProgramFromFile)
import           GHCJS.DOM       (runWebGUI)

run :: IO ()
run = runWebGUI $ \webView -> do
    Just doc <- webViewGetDomDocument webView
    Just body <- documentGetBody doc

    Just textArea <- elementQuerySelector body "#textArea"
    Just runButton <- elementQuerySelector body "#run"

    elementOnClick runButton $ \_ _ -> do
        content <- HtmlElementGetInnerText textArea
        undefined

    putStrLn "Setup done!"
