module Crux.Text where

import           Data.Char (isUpper)
import qualified Data.Text as T

isCapitalized :: T.Text -> Bool
isCapitalized txt = isUpper $ T.head txt
