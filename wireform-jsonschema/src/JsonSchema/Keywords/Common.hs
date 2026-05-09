{-# LANGUAGE OverloadedStrings #-}
-- | Common helper functions for keyword implementations
module JsonSchema.Keywords.Common
  ( extractPropertyNames
  ) where

import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import qualified Data.Set as Set
import Data.Text (Text)

-- | Extract property names from a KeyMap as a Set of Text
--
-- This helper function extracts all property names from an object's KeyMap
-- and converts them to a Set of Text values. Used by multiple keywords that
-- need to work with object properties.
extractPropertyNames :: KeyMap.KeyMap a -> Set.Set Text
extractPropertyNames objMap = Set.fromList $ do
  k <- KeyMap.keys objMap
  pure $ Key.toText k

