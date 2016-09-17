module Crux.Pos where

import Crux.ModuleName (ModuleName)

-- TODO: we should record the start and end positions of tokens
-- All of these numbers are one-based, because that's how editors standardized.
-- (Except emacs.  Emacs users will have to deal with column numbers being off
-- by one.)
data Pos = Pos
    { posFileName   :: String
    , posModuleName :: ModuleName
    , posLine       :: Int
    , posColumn     :: Int
    }
    deriving (Show, Eq)

data ParsePos = ParsePos
    { ppLineStart :: Int
    , ppLine :: Int
    , ppColumn :: Int
    }
    deriving (Show)

-- TODO: eliminate references to dummyPos
dummyPos :: Pos
dummyPos = Pos
    { posFileName = "<dummy>"
    , posModuleName = "<dummy>"
    , posLine = 0
    , posColumn = 0
    }