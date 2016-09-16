module Crux.Pos where

-- TODO: we should record the start and end positions of tokens
-- All of these numbers are one-based, because that's how editors standardized.
-- (Except emacs.  Emacs users will have to deal with column numbers being off
-- by one.)
data Pos = Pos
    -- This token's line number.
    { posLine      :: Int
    -- This token's column number.
    , posColumn    :: Int
    }
    deriving (Eq)

instance Show Pos where
    show Pos{..} = "Pos " ++ show posLine ++ " " ++ show posColumn


data ParsePos = ParsePos
    { ppLineStart :: Int
    , ppPos :: Pos
    }
    deriving (Show)