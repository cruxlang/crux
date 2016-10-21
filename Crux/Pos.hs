module Crux.Pos where

data ParsePos = ParsePos
    { ppLineStart :: Int
    , ppLine :: Int
    , ppColumn :: Int
    }
    deriving (Show)

-- TODO: we should record the start and end positions of tokens
-- All of these numbers are one-based, because that's how editors standardized.
-- (Except emacs.  Emacs users will have to deal with column numbers being off
-- by one.)
data PosRec = PosRec
    { posFileName   :: FilePath
    , posLine       :: Int
    , posColumn     :: Int
    }
    deriving (Show, Eq)

data Pos
    = Pos PosRec
    | SyntaxDependency FilePath
    | GeneratedMainCall FilePath
    | InternalErrorPos FilePath
    deriving (Show, Eq)

-- TODO: eliminate references to dummyPos
dummyPos :: Pos
dummyPos = Pos $ PosRec
    { posFileName = "<dummy>"
    , posLine = 0
    , posColumn = 0
    }