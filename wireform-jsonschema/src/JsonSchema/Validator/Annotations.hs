{-# LANGUAGE OverloadedStrings #-}

-- | Shared helpers for validation annotations.
--
-- These utilities are used by both the legacy validator logic and the
-- keyword-based validators to track which properties/items were evaluated
-- and to re-root annotations produced by nested validations.
module JsonSchema.Validator.Annotations
  ( annotateProperties
  , annotateItems
  , shiftAnnotations
  , arrayIndexPointer
  , propertyPointer
  ) where

import Data.Aeson (Value(..))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector, fromList)

import JsonPointer (JsonPointer(..), emptyPointer)
import JsonSchema.Types (ValidationAnnotations(..))

-- | Create annotation for evaluated properties at current location.
annotateProperties :: Set Text -> ValidationAnnotations
annotateProperties props =
  if Set.null props
    then mempty
    else ValidationAnnotations $
           Map.singleton emptyPointer $
             Map.singleton "properties" (Array $ fromListText props)
  where
    fromListText :: Set Text -> Vector Value
    fromListText = fromList . map String . Set.toList

-- | Create annotation for evaluated array items at current location.
annotateItems :: Set Int -> ValidationAnnotations
annotateItems indices =
  if Set.null indices
    then mempty
    else ValidationAnnotations $
           Map.singleton emptyPointer $
             Map.singleton "items" (Array $ fromListNumber indices)
  where
    fromListNumber :: Set Int -> Vector Value
    fromListNumber = fromList . map (Number . fromIntegral) . Set.toList

-- | Shift all annotation pointers by prepending a prefix.
shiftAnnotations :: JsonPointer -> ValidationAnnotations -> ValidationAnnotations
shiftAnnotations prefix (ValidationAnnotations annMap) =
  ValidationAnnotations $ Map.mapKeys (prefix <>) annMap

-- | Create a JSON Pointer for an array index.
arrayIndexPointer :: Int -> JsonPointer
arrayIndexPointer idx = JsonPointer [T.pack (show idx)]

-- | Create a JSON Pointer for an object property.
propertyPointer :: Text -> JsonPointer
propertyPointer prop = JsonPointer [prop]

