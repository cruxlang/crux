
module Eval
    ( js_eval
    , js_setTheFunction
    , setTextContent
    ) where

import GHCJS.Foreign (ToJSString (..))
import GHCJS.Types (JSRef, JSString, JSFun)
import GHCJS.DOM.Types
-- import GHCJS.DOM.HTMLElement (HTMLElement)

foreign import javascript unsafe "eval($1)"
    js_eval :: JSString -> IO ()

foreign import javascript unsafe "$1[\"textContent\"] = $2"
    js_setTextContent :: JSRef HTMLElement -> JSString -> IO ()

foreign import javascript unsafe "window.hs_compileCrux = $1"
    js_setTheFunction :: JSFun a -> IO ()

setTextContent :: (IsHTMLElement el, ToJSString s) => el -> s -> IO ()
setTextContent self str =
    js_setTextContent (unHTMLElement (toHTMLElement self)) (toJSString str)
