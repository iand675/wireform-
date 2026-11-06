{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

{- | Parquet Variant v1 columns. A null column slot is distinct
from the present Variant null value. Binary payloads are decoded only by
these explicit semantic operations; ordinary Parquet binary reads preserve
bytes, including payloads that are not valid Variants.
-}
module Parquet.Variant.Parquet (
  VariantReader,
  compileVariantReader,
  compileVariantReaderWithLimits,
  variantAt,
  variantAt#,
  variantToNestedRow,
  variantToNestedRowWithLimits,
  buildVariantParquetFile,
  buildVariantParquetFileWithLimits,
  readVariantColumn,
  readVariantColumnWithLimits,
) where

import Arrow.Column qualified as C
import Arrow.Types qualified as A
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Vector qualified as V
import GHC.Exts (Int (I#), Int#)
import Parquet.Arrow qualified as Arrow
import Parquet.Nested qualified as N
import Parquet.Read qualified as Read
import Parquet.Types qualified as P
import Parquet.Variant qualified as Variant
import Parquet.Variant.Arrow qualified as VariantArrow
import Parquet.Variant.Schema qualified as VariantSchema


data VariantReader = VariantReader
  { readerLength :: {-# UNPACK #-} !Int
  , readerValidity :: !(Maybe C.Validity)
  , readerMetadata :: !C.ColumnArray
  , readerValue :: !C.ColumnArray
  , readerLimits :: !Variant.VariantLimits
  }


compileVariantReader :: C.ColumnArray -> Either String VariantReader
compileVariantReader = compileVariantReaderWithLimits Variant.defaultVariantLimits


{- | Compile named binary child views once. Accepts ordinary/large/view
binary arrays. Required children are checked only when the parent is
present, so null parents mask arbitrary child storage.
-}
compileVariantReaderWithLimits :: Variant.VariantLimits -> C.ColumnArray -> Either String VariantReader
compileVariantReaderWithLimits limits column = case column of
  C.ColStruct count validity children -> do
    unless
      (count >= 0 && V.length children == 2 && V.length (V.filter ((== "metadata") . fst) children) == 1 && V.length (V.filter ((== "value") . fst) children) == 1)
      (Left "Parquet.Variant: unshredded column requires unique metadata and value fields")
    metadata <- named "metadata" children
    value <- named "value" children
    unless
      (binary metadata && binary value && C.columnLength metadata >= count && C.columnLength value >= count)
      (Left "Parquet.Variant: metadata/value must be binary arrays covering the group")
    unless
      (Variant.variantMaxDepth limits >= 0 && Variant.variantMaxValues limits > 0 && Variant.variantMaxDictionaryEntries limits >= 0)
      (Left "Parquet.Variant: invalid resource limits")
    pure (VariantReader count validity metadata value limits)
  _ -> Left "Parquet.Variant: expected a struct column"
  where
    named name children = maybe (Left "Parquet.Variant: missing binary field") (Right . snd) (V.find ((== name) . fst) children)
    binary value = case value of
      C.ColBinary {} -> True
      C.ColLargeBinary {} -> True
      C.ColBinaryView {} -> True
      _ -> False


variantAt :: VariantReader -> Int -> Either String (Maybe Variant.Variant)
variantAt reader (I# index) = case variantAt# reader index of
  (# message | | #) -> Left message
  (# | value | #) -> Right (Just value)
  (# | | () #) -> Right Nothing


variantAt# :: VariantReader -> Int# -> (# String | Variant.Variant | () #)
variantAt# reader index#
  | index < 0 || index >= readerLength reader = (# "Parquet.Variant: row index out of range" | | #)
  | not (C.isValidAt (readerValidity reader) index) = (# | | () #)
  | otherwise = case C.anyBytesAt (readerMetadata reader) index of
      Nothing -> (# "Parquet.Variant: present group has null metadata" | | #)
      Just metadata -> case C.anyBytesAt (readerValue reader) index of
        Nothing -> (# "Parquet.Variant: present unshredded group has null value" | | #)
        Just value -> case Variant.decodeVariantWithLimits (readerLimits reader) metadata value of
          Left message -> (# message | | #)
          Right decoded -> (# | decoded | #)
  where
    index = I# index#
{-# INLINE variantAt# #-}


variantToNestedRow :: Maybe Variant.Variant -> Either String N.NestedRow
variantToNestedRow = variantToNestedRowWithLimits Variant.defaultVariantLimits


variantToNestedRowWithLimits :: Variant.VariantLimits -> Maybe Variant.Variant -> Either String N.NestedRow
variantToNestedRowWithLimits _ Nothing = Right N.NRNull
variantToNestedRowWithLimits limits (Just value) = do
  encoded <- Variant.encodeVariantWithLimits limits value
  pure (N.NRVariantBytes (Variant.variantMetadata encoded) (Variant.variantValue encoded))


buildVariantParquetFile :: Text -> V.Vector (Maybe Variant.Variant) -> Either String ByteString
buildVariantParquetFile = buildVariantParquetFileWithLimits Variant.defaultVariantLimits


buildVariantParquetFileWithLimits :: Variant.VariantLimits -> Text -> V.Vector (Maybe Variant.Variant) -> Either String ByteString
buildVariantParquetFileWithLimits limits name values = do
  rows <- V.mapM (variantToNestedRowWithLimits limits) values
  N.buildNestedFile (V.singleton (name, N.NSOptional (N.NSVariantWithLimits limits))) (V.singleton rows)


readVariantColumn :: Read.ParquetFile -> Int -> Text -> Either String (V.Vector (Maybe Variant.Variant))
readVariantColumn = readVariantColumnWithLimits Variant.defaultVariantLimits


{- | Read one top-level VARIANT(1) column, reconstructing typed primitives,
objects and arrays with the original shared metadata dictionary.
-}
readVariantColumnWithLimits :: Variant.VariantLimits -> Read.ParquetFile -> Int -> Text -> Either String (V.Vector (Maybe Variant.Variant))
readVariantColumnWithLimits limits file group name = do
  schema <- Arrow.parquetFileArrowSchemaChecked file
  field <- maybe (Left "Parquet.Variant: column not found") Right (V.find ((== name) . A.fieldName) (A.arrowFields schema))
  unless (VariantSchema.isVariantField field) (Left "Parquet.Variant: expected VARIANT(1) schema")
  columns <- either (Left . show) Right (Arrow.parquetRowGroupToArrow schema {A.arrowFields = V.singleton field} file group)
  let schemaLimits = VariantSchema.VariantSchemaLimits 128 (V.length (P.fmSchema (Read.pfFooter file)))
  reader <- VariantArrow.compileVariantArrowReaderWithLimits limits schemaLimits field (V.head columns)
  V.generateM (VariantArrow.variantArrowLength reader) (VariantArrow.variantArrowAt reader)
