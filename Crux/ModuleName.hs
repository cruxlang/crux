{-# LANGUAGE DeriveGeneric #-}

module Crux.ModuleName where

import qualified Data.Text as Text
import Crux.Prelude

newtype ModuleSegment = ModuleSegment { unModuleSegment :: Text }
    deriving (Show, Eq, Ord, Generic)
instance Hashable ModuleSegment

data ModuleName = ModuleName [ModuleSegment] ModuleSegment
    deriving (Eq, Ord, Generic)
instance Show ModuleName where
    show (ModuleName prefixes base) = show $ Text.intercalate "." $ fmap (unModuleSegment) (prefixes ++ [base])
instance Hashable ModuleName

-- TODO: assert that first letter is capitalized, remainder are alphanumeric
toModuleSegment :: Text -> ModuleSegment
toModuleSegment = ModuleSegment

instance IsString ModuleName where
    fromString s =
        let t = Text.pack s in
        let p = Text.splitOn "." t in
        case map toModuleSegment p of
            [] -> error "Invalid module name"
            xs -> ModuleName (init xs) (last xs)

printModuleName :: ModuleName -> Text
printModuleName mn = Text.intercalate "." $ toList mn

toList :: ModuleName -> [Text]
toList (ModuleName a b) = fmap unModuleSegment $ a <> [b]
