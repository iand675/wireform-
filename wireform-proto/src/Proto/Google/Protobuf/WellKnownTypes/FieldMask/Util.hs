{-# LANGUAGE ScopedTypeVariables #-}
-- | Utility functions for @google.protobuf.FieldMask@.
--
-- Provides set operations (union, intersection), path normalization,
-- containment checks, construction helpers, validation against a message
-- schema, and an @allFieldMask@ constructor — mirroring utilities found in
-- Go (@fieldmaskpb@), Java (@com.google.protobuf.util.FieldMaskUtil@),
-- and C++ (@google::protobuf::util::FieldMaskUtil@).
module Proto.Google.Protobuf.WellKnownTypes.FieldMask.Util
  ( -- * Construction
    fromPaths
  , toPaths
  , allFieldMask

    -- * Set operations
  , union
  , intersection
  , subtractMask

    -- * Normalization
  , normalize

    -- * Querying
  , contains
  , isEmpty

    -- * Validation
  , isValid

    -- * Path utilities
  , canonicalForm
  , toCamelCase
  , toSnakeCase
  ) where

import Data.Char (isUpper, toLower, toUpper)
import Data.Proxy (Proxy)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import Proto.Google.Protobuf.WellKnownTypes.FieldMask (FieldMask(..), defaultFieldMask)
import Proto.Schema (ProtoMessage(..), SomeFieldDescriptor(..), FieldDescriptor(..))
import qualified Data.IntMap.Strict as IntMap

-- | Construct a 'FieldMask' from a list of paths.
fromPaths :: [Text] -> FieldMask
fromPaths ps = defaultFieldMask { fieldMaskPaths = V.fromList ps }

-- | Extract the list of paths from a 'FieldMask'.
toPaths :: FieldMask -> [Text]
toPaths = V.toList . fieldMaskPaths

-- | Construct a 'FieldMask' covering all top-level fields of a message.
allFieldMask :: forall a. ProtoMessage a => Proxy a -> FieldMask
allFieldMask p =
  let descs = protoFieldDescriptors p
      names = fmap (\(SomeField fd) -> fdName fd) (IntMap.elems descs)
  in defaultFieldMask { fieldMaskPaths = V.fromList names }

-- | Union of two 'FieldMask' values (all paths from both).
--
-- The result is normalized: sorted, deduplicated, and sub-paths are
-- removed when a parent path is present.
union :: FieldMask -> FieldMask -> FieldMask
union a b = normalize $ defaultFieldMask
  { fieldMaskPaths = fieldMaskPaths a V.++ fieldMaskPaths b }

-- | Intersection of two 'FieldMask' values.
--
-- A path is included if it appears in both masks, or if one mask contains
-- a parent path that covers the other's more specific path.
intersection :: FieldMask -> FieldMask -> FieldMask
intersection a b =
  let setPaths fm = Set.fromList (V.toList (fieldMaskPaths fm))
      sa = setPaths a
      sb = setPaths b
      result = Set.filter (\p -> containedIn p sb) sa
               `Set.union`
               Set.filter (\p -> containedIn p sa) sb
  in defaultFieldMask { fieldMaskPaths = V.fromList (Set.toAscList result) }
  where
    containedIn path pathSet =
      Set.member path pathSet || hasAncestor path pathSet

    hasAncestor path pathSet =
      any (\candidate -> isProperPrefix candidate path) (Set.toList pathSet)

-- | Subtract one 'FieldMask' from another. The result contains paths from
-- the first mask that are not covered by the second.
subtractMask :: FieldMask -> FieldMask -> FieldMask
subtractMask a b =
  let sb = Set.fromList (V.toList (fieldMaskPaths b))
      filtered = V.filter (\p -> not (coveredBy p sb)) (fieldMaskPaths a)
  in defaultFieldMask { fieldMaskPaths = filtered }
  where
    coveredBy path pathSet =
      Set.member path pathSet
      || any (\candidate -> isProperPrefix candidate path) (Set.toList pathSet)

-- | Normalize a 'FieldMask': sort paths, remove duplicates, and remove
-- paths that are sub-paths of another entry.
--
-- For example, @[\"a.b\", \"a\", \"c\"]@ normalizes to @[\"a\", \"c\"]@.
normalize :: FieldMask -> FieldMask
normalize fm =
  let sorted = Set.toAscList (Set.fromList (V.toList (fieldMaskPaths fm)))
      pruned = removeRedundant sorted
  in defaultFieldMask { fieldMaskPaths = V.fromList pruned }

removeRedundant :: [Text] -> [Text]
removeRedundant = go Set.empty
  where
    go _ [] = []
    go ancestors (p : ps)
      | any (\a -> isProperPrefix a p) (Set.toList ancestors) = go ancestors ps
      | otherwise = p : go (Set.insert p ancestors) ps

isProperPrefix :: Text -> Text -> Bool
isProperPrefix prefix path =
  T.isPrefixOf prefix path
  && T.length prefix < T.length path
  && T.index path (T.length prefix) == '.'

-- | Check whether a 'FieldMask' contains a specific path, accounting for
-- parent-path coverage.
--
-- @contains (fromPaths [\"a\"]) \"a.b\"@ is 'True' because @\"a\"@ covers
-- all sub-paths.
contains :: FieldMask -> Text -> Bool
contains fm path =
  V.any (\p -> p == path || isProperPrefix p path) (fieldMaskPaths fm)

-- | Is the 'FieldMask' empty (no paths)?
isEmpty :: FieldMask -> Bool
isEmpty = V.null . fieldMaskPaths

-- | Validate a 'FieldMask' against a message's schema. Each top-level
-- path segment must correspond to a known field name.
isValid :: forall a. ProtoMessage a => Proxy a -> FieldMask -> Bool
isValid p fm =
  let descs = protoFieldDescriptors p
      validNames = Set.fromList (fmap (\(SomeField fd) -> fdName fd) (IntMap.elems descs))
  in V.all (\path -> topLevel path `Set.member` validNames) (fieldMaskPaths fm)
  where
    topLevel path = case T.breakOn "." path of
      (first, _) -> first

-- | Produce a canonical text form: sorted, deduplicated, sub-paths removed.
canonicalForm :: FieldMask -> Text
canonicalForm = T.intercalate "," . toPaths . normalize

-- | Convert a snake_case field path to lowerCamelCase (for JSON mapping).
--
-- @\"foo_bar.baz_qux\"@ becomes @\"fooBar.bazQux\"@.
toCamelCase :: Text -> Text
toCamelCase = T.intercalate "." . fmap segmentToCamel . T.splitOn "."
  where
    segmentToCamel seg =
      let parts = T.splitOn "_" seg
      in case parts of
        [] -> ""
        (first : rest) -> first <> T.concat (fmap capitalize rest)
    capitalize t
      | T.null t = t
      | otherwise = T.cons (toUpper (T.head t)) (T.tail t)

-- | Convert a lowerCamelCase field path to snake_case (from JSON mapping).
--
-- @\"fooBar.bazQux\"@ becomes @\"foo_bar.baz_qux\"@.
toSnakeCase :: Text -> Text
toSnakeCase = T.intercalate "." . fmap segmentToSnake . T.splitOn "."
  where
    segmentToSnake = T.concatMap $ \c ->
      if isUpper c
        then T.pack ['_', toLower c]
        else T.singleton c
