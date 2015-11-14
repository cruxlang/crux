
module Eval
    ( js_eval
    ) where

import GHCJS.Types (JSString)

foreign import javascript unsafe "eval($1)"
    js_eval :: JSString -> IO ()
