
module Eval
    ( js_eval
    , setTextContent
    ) where

import           GHCJS.Foreign                 (toJSString)
import GHCJS.Types (JSRef, JSString)
import GHCJS.DOM.Types
import GHCJS.DOM.HTMLElement (HTMLElement)

foreign import javascript unsafe "eval($1)"
    js_eval :: JSString -> IO ()

foreign import javascript unsafe "$1[\"textContent\"] = $2"
    js_setTextContent :: JSRef HTMLElement -> JSString -> IO ()

setTextContent self str =
    js_setTextContent (unHTMLElement (toHTMLElement self)) (toJSString str)
