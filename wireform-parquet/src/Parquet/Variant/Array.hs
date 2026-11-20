{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Internal shared binary accessors for Variant reading and writing.
module Parquet.Variant.Array (BytesReader (..), compileBytes, alignVariantColumns) where

import Arrow.Column qualified as C
import Arrow.Types qualified as A
import Data.ByteString (ByteString)
import Data.Map.Strict qualified as M
import Data.Vector qualified as V
import Data.Vector.Storable qualified as S
import Foreign.Storable (Storable)
import GHC.Exts (Int (I#), Int#)
import Parquet.Variant.Schema qualified as Schema


newtype BytesReader = BytesReader {bytesAt# :: Int# -> (# ByteString | () #)}


newtype IndexReader = IndexReader {indexAt# :: Int# -> (# Int# | () #)}


-- Only metadata may be dictionary or run-end encoded. Resolve one logical
-- index at a time; no expanded metadata column is allocated.
compileBytes :: C.ColumnArray -> Either String BytesReader
compileBytes column = case column of
  C.ColDictionary _ keys values -> do
    entries <- compileBytes values
    indices <- compileIndices keys
    pure $ BytesReader $ \index -> case indexAt# indices index of
      (# | () #) -> (# | () #)
      (# key | #) -> bytesAt# entries key
  C.ColRunEndEncoded offset _ ends values -> do
    entries <- compileBytes values
    case ends of
      C.ColInt16 _ xs -> pure (runs entries offset xs)
      C.ColInt32 _ xs -> pure (runs entries offset xs)
      C.ColInt64 _ xs -> pure (runs entries offset xs)
      _ -> Left "Parquet.Variant.Arrow: invalid metadata run ends"
  C.ColBinary validity offsets bytes -> pure (variableBytes validity offsets bytes)
  C.ColLargeBinary validity offsets bytes -> pure (variableBytes validity offsets bytes)
  C.ColBinaryView {} -> pure $ BytesReader $ \index -> case C.anyBytesAt column (I# index) of
    Nothing -> (# | () #)
    Just bytes -> (# bytes | #)
  _ -> Left "Parquet.Variant.Arrow: expected binary storage"
  where
    runs :: (Storable a, Integral a) => BytesReader -> Int -> S.Vector a -> BytesReader
    runs entries offset ends = BytesReader $ \index ->
      let position = offset + I# index
          search !low !high
            | low == high = low
            | fromIntegral (S.unsafeIndex ends middle) <= position = search (middle + 1) high
            | otherwise = search low middle
            where
              middle = low + (high - low) `quot` 2
      in case search 0 (S.length ends) of I# run -> bytesAt# entries run


-- The public dictionary accessor returns a boxed index and must dispatch
-- on its integer type for each row. Bind the concrete key vector once.
compileIndices :: C.ColumnArray -> Either String IndexReader
compileIndices column = case column of
  C.ColInt8 v xs -> pure (indexKeys v xs)
  C.ColInt16 v xs -> pure (indexKeys v xs)
  C.ColInt32 v xs -> pure (indexKeys v xs)
  C.ColInt64 v xs -> pure (indexKeys v xs)
  C.ColUInt8 v xs -> pure (indexKeys v xs)
  C.ColUInt16 v xs -> pure (indexKeys v xs)
  C.ColUInt32 v xs -> pure (indexKeys v xs)
  C.ColUInt64 v xs -> pure (indexKeys v xs)
  _ -> Left "Parquet.Variant.Arrow: dictionary keys must be integers"


indexKeys :: (Storable a, Integral a) => Maybe C.Validity -> S.Vector a -> IndexReader
indexKeys validity values = IndexReader $ \index ->
  if C.isValidAt validity (I# index)
    then case fromIntegral (S.unsafeIndex values (I# index)) of I# key -> (# key | #)
    else (# | () #)
{-# INLINE indexKeys #-}


variableBytes :: (Storable a, Integral a) => Maybe C.Validity -> S.Vector a -> ByteString -> BytesReader
variableBytes validity offsets bytes =
  let array = C.BytesArray validity offsets bytes
  in BytesReader $ \index -> case C.unsafeBytesAt array (I# index) of
       Nothing -> (# | () #)
       Just value -> (# value | #)
{-# INLINE variableBytes #-}


-- Names carry Variant semantics even when the in-memory column order differs
-- from its field. Bind once before structural validation or positional row
-- lowering. Normal IPC columns take the linear already-ordered fast path;
-- a permuted wide object builds one name map. No value buffer is copied.
alignVariantColumns :: A.Field -> C.ColumnArray -> Either String C.ColumnArray
alignVariantColumns = go False
  where
    -- The common ordered case aliases the complete original column and
    -- never revalidates offsets or creates replacement container vectors.
    needed parent field column =
      let active = parent || Schema.isVariantField field
          fields = A.fieldChildren field
          one value = case V.toList fields of [item] -> needed active item value; _ -> False
      in case column of
           C.ColStruct _ _ columns
             | V.length columns == V.length fields ->
                 (active && V.or (V.zipWith (\item (name, _) -> A.fieldName item /= name) fields columns))
                   || V.or (V.zipWith (\item (_, value) -> needed active item value) fields columns)
           C.ColList _ _ values -> one values
           C.ColLargeList _ _ values -> one values
           C.ColListView _ _ _ values -> one values
           C.ColMap _ _ keys values -> case V.toList fields of
             [entries] -> case V.toList (A.fieldChildren entries) of
               [key, value] -> needed active key keys || needed active value values
               _ -> False
             _ -> False
           _ -> False
    go parent field column | not (needed parent field column) = Right column
    go parent field column =
      let active = parent || Schema.isVariantField field
          fields = A.fieldChildren field
          child value = case V.toList fields of
            [item] -> go active item value
            _ -> Left "Parquet.Variant: list storage requires one child field"
      in case column of
           C.ColStruct count validity columns -> do
             if V.length columns /= V.length fields then Left "Parquet.Variant: struct child count mismatch" else Right ()
             ordered <-
               if not active || V.and (V.zipWith (\item (name, _) -> A.fieldName item == name) fields columns)
                 then Right columns
                 else do
                   let named = M.fromList (V.toList columns)
                   if M.size named /= V.length columns then Left "Parquet.Variant: duplicate named storage child" else Right ()
                   V.mapM (\item -> maybe (Left "Parquet.Variant: missing named storage child") (\value -> Right (A.fieldName item, value)) (M.lookup (A.fieldName item) named)) fields
             aligned <- V.zipWithM (\item (name, value) -> (,) name <$> go active item value) fields ordered
             C.mkStruct count validity aligned
           C.ColList validity offsets values -> child values >>= C.mkList validity offsets
           C.ColLargeList validity offsets values -> child values >>= C.mkLargeList validity offsets
           C.ColListView validity offsets sizes values -> child values >>= C.mkListView validity offsets sizes
           C.ColMap validity offsets keys values -> case V.toList fields of
             [entries] -> case V.toList (A.fieldChildren entries) of
               [key, value] -> do
                 alignedKeys <- go active key keys
                 alignedValues <- go active value values
                 C.mkMap validity offsets alignedKeys alignedValues
               _ -> Left "Parquet.Variant: map entries require key and value fields"
             _ -> Left "Parquet.Variant: map storage requires entries"
           _ -> Right column
