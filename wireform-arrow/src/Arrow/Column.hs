{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- The Eq, Show and NFData instances of 'ColumnArray' (defined in
-- "Arrow.Column.Internal") live here because they are written with
-- the accessors and row model of this module.
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Arrow columns as Arrow buffers.

A 'ColumnArray' holds Arrow-native buffers: storable vectors for
fixed-width values and offsets, LSB-first bitmaps for validity and
booleans, and 'ByteString' regions for variable-length data. A column
decoded from IPC aliases the input bytes (zero copy) and keeps them
alive; 'copyColumn' detaches a column (for example a small slice of a
large input) by copying only the bytes it references.

Nullability is per value, not per type: every array with a validity
slot carries @Maybe Validity@, 'Nothing' meaning no nulls (a validity
whose null count is 0 is normalised to 'Nothing').

Construction is checked. The patterns exported here only match; build
columns with the builders ("Arrow.Column.Builder", re-exported), the
@from*@ conversions, or the validating @mk*@ constructors. The raw
constructors live in "Arrow.Column.Internal". Every column that user
code can build therefore satisfies the invariants listed there, which
is what lets the @*At@ accessors read without re-validating.

'Eq' is logical and O(n): two columns are equal when they have the
same type, the same length, the same validity per row, and equal
values in valid rows. Null slots, bit offsets, offset bases and
dictionary layouts do not matter; floating point compares bit
patterns. 'Show' prints logical rows.
-}
module Arrow.Column (
  -- * Columns
  ColumnArray,
  pattern ColNull,
  pattern ColPrim,
  pattern ColInt8,
  pattern ColInt16,
  pattern ColInt32,
  pattern ColInt64,
  pattern ColUInt8,
  pattern ColUInt16,
  pattern ColUInt32,
  pattern ColUInt64,
  pattern ColFloat16,
  pattern ColFloat,
  pattern ColDouble,
  pattern ColDate32,
  pattern ColDate64,
  pattern ColTime32,
  pattern ColTime64,
  pattern ColTimestamp,
  pattern ColDuration,
  pattern ColIntervalYearMonth,
  pattern ColIntervalDayTime,
  pattern ColIntervalMonthDayNano,
  pattern ColDecimal128,
  pattern ColDecimal256,
  pattern ColBool,
  pattern ColUtf8,
  pattern ColBinary,
  pattern ColLargeUtf8,
  pattern ColLargeBinary,
  pattern ColFixedSizeBinary,
  pattern ColUtf8View,
  pattern ColBinaryView,
  pattern ColStruct,
  pattern ColList,
  pattern ColLargeList,
  pattern ColListView,
  pattern ColLargeListView,
  pattern ColFixedSizeList,
  pattern ColMap,
  pattern ColDenseUnion,
  pattern ColSparseUnion,
  pattern ColDictionary,
  pattern ColRunEndEncoded,

  -- * Element tags
  PrimType (..),
  withPrim,
  primWidth,
  SomePrimType (..),
  primTypeFor,
  Offset,

  -- * Buffers
  Bitmap,
  bitmapBytes,
  bitmapOffset,
  bitmapLength,
  mkBitmap,
  emptyBitmap,
  bitAt,
  bitmapSetCount,
  bitmapGenerate,
  bitmapFromBools,
  bitmapToBools,
  Validity,
  validityBits,
  validityNullCount,
  mkValidity,
  validityGenerate,
  validityFromBools,
  isValidAt,
  Float16 (..),
  float16ToDouble,
  IntervalDayTime (..),
  IntervalMonthDayNano (..),
  Decimal128 (..),
  decimal128ToInteger,
  decimal128FromInteger,
  Decimal256 (..),
  decimal256ToInteger,
  decimal256FromInteger,

  -- * Shape
  columnLength,
  nullCount,
  validity,
  columnTag,
  hasValiditySlot,

  -- * Typed views (O(1); index functions INLINE)
  PrimArray (..),
  asPrim,
  primArrayLength,
  primAt,
  unsafePrimAt,
  primValueAt,
  unsafePrimValueAt,
  BytesArray (..),
  Utf8Array (..),
  asUtf8,
  asLargeUtf8,
  asBinary,
  asLargeBinary,
  bytesArrayLength,
  bytesAt,
  unsafeBytesAt,
  textAt,
  unsafeTextAt,
  BoolArray (..),
  asBool,
  boolArrayAt,
  boolAt,
  anyBytesAt,
  anyTextAt,
  ChildRange (..),
  listRange,
  dictKeyAt,

  -- * Conversions (cost in the name)
  toStorable,
  toMaybeVector,
  toTextVector,
  toBytesVector,
  toBoolVector,
  toListVector,
  copyColumn,

  -- * Construction
  primColumn,
  primColumnV,
  mkPrim,
  fromMaybes,
  fromBools,
  fromMaybeBools,
  fromTexts,
  fromMaybeTexts,
  fromMaybeLargeTexts,
  fromByteStrings,
  fromMaybeByteStrings,
  fromMaybeLargeByteStrings,
  fromMaybeFixedSizeBinary,
  fromMaybeUtf8View,
  fromMaybeBinaryView,
  mkBool,
  mkUtf8,
  mkBinary,
  mkLargeUtf8,
  mkLargeBinary,
  mkFixedSizeBinary,
  mkUtf8View,
  mkBinaryView,
  mkStruct,
  mkList,
  mkLargeList,
  mkListView,
  mkLargeListView,
  mkFixedSizeList,
  mkMap,
  mkDenseUnion,
  mkSparseUnion,
  mkDictionary,
  mkRunEndEncoded,
  emptyColumnFor,
  placeholderColumn,
  fillerColumn,

  -- * Builders
  module Arrow.Column.Builder,

  -- * Row operations
  sliceColumnArray,
  concatColumnArray,
  concatColumnArrays,
  takeColumnArray,
  rebaseRunEnds,

  -- * Dictionaries
  expandDictionary,
  resolveDictionaryColumn,

  -- * Nullability
  maskValidity,
  toNullableColumn,

  -- * Map invariants
  validateMapKeysSorted,
) where

import Arrow.Column.Builder
import Arrow.Column.Internal (ColumnArray)
import Arrow.Column.Internal hiding (ColumnArray (..))
import Arrow.Column.Internal qualified as I
import Arrow.Types (ArrowType (..), DictionaryEncoding (..), Field (..), UnionMode (..))
import Columnar.SIMD qualified as K
import Control.DeepSeq (NFData (..))
import Control.Monad (forM_, when)
import Control.Monad.ST (stToIO)
import Data.Bits (unsafeShiftR)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List (intersperse)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Array qualified as TA
import Data.Text.Foreign qualified as TF
import Data.Text.Internal qualified as TI
import Data.Type.Equality ((:~:) (..))
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as VSM
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.ForeignPtr (castForeignPtr)
import GHC.ForeignPtr (unsafeWithForeignPtr)
import Foreign.Marshal.Utils (copyBytes, fillBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable (..))
import System.IO.Unsafe (unsafeDupablePerformIO)


-- ============================================================
-- Patterns
-- ============================================================

-- | An Arrow NULL column of the given length (no buffers; every row null).
pattern ColNull :: Int -> ColumnArray
pattern ColNull n = I.ColNull n


-- | Any fixed-width column, with its element tag.
pattern ColPrim :: () => forall a. PrimType a -> Maybe Validity -> VS.Vector a -> ColumnArray
pattern ColPrim t v xs <- I.ColPrim t v xs


pattern ColInt8 :: Maybe Validity -> VS.Vector Int8 -> ColumnArray
pattern ColInt8 v xs <- I.ColPrim PInt8 v xs


pattern ColInt16 :: Maybe Validity -> VS.Vector Int16 -> ColumnArray
pattern ColInt16 v xs <- I.ColPrim PInt16 v xs


pattern ColInt32 :: Maybe Validity -> VS.Vector Int32 -> ColumnArray
pattern ColInt32 v xs <- I.ColPrim PInt32 v xs


pattern ColInt64 :: Maybe Validity -> VS.Vector Int64 -> ColumnArray
pattern ColInt64 v xs <- I.ColPrim PInt64 v xs


pattern ColUInt8 :: Maybe Validity -> VS.Vector Word8 -> ColumnArray
pattern ColUInt8 v xs <- I.ColPrim PUInt8 v xs


pattern ColUInt16 :: Maybe Validity -> VS.Vector Word16 -> ColumnArray
pattern ColUInt16 v xs <- I.ColPrim PUInt16 v xs


pattern ColUInt32 :: Maybe Validity -> VS.Vector Word32 -> ColumnArray
pattern ColUInt32 v xs <- I.ColPrim PUInt32 v xs


pattern ColUInt64 :: Maybe Validity -> VS.Vector Word64 -> ColumnArray
pattern ColUInt64 v xs <- I.ColPrim PUInt64 v xs


pattern ColFloat16 :: Maybe Validity -> VS.Vector Float16 -> ColumnArray
pattern ColFloat16 v xs <- I.ColPrim PFloat16 v xs


pattern ColFloat :: Maybe Validity -> VS.Vector Float -> ColumnArray
pattern ColFloat v xs <- I.ColPrim PFloat v xs


pattern ColDouble :: Maybe Validity -> VS.Vector Double -> ColumnArray
pattern ColDouble v xs <- I.ColPrim PDouble v xs


pattern ColDate32 :: Maybe Validity -> VS.Vector Int32 -> ColumnArray
pattern ColDate32 v xs <- I.ColPrim PDate32 v xs


pattern ColDate64 :: Maybe Validity -> VS.Vector Int64 -> ColumnArray
pattern ColDate64 v xs <- I.ColPrim PDate64 v xs


pattern ColTime32 :: Maybe Validity -> VS.Vector Int32 -> ColumnArray
pattern ColTime32 v xs <- I.ColPrim PTime32 v xs


pattern ColTime64 :: Maybe Validity -> VS.Vector Int64 -> ColumnArray
pattern ColTime64 v xs <- I.ColPrim PTime64 v xs


pattern ColTimestamp :: Maybe Validity -> VS.Vector Int64 -> ColumnArray
pattern ColTimestamp v xs <- I.ColPrim PTimestamp v xs


pattern ColDuration :: Maybe Validity -> VS.Vector Int64 -> ColumnArray
pattern ColDuration v xs <- I.ColPrim PDuration v xs


pattern ColIntervalYearMonth :: Maybe Validity -> VS.Vector Int32 -> ColumnArray
pattern ColIntervalYearMonth v xs <- I.ColPrim PIntervalYearMonth v xs


pattern ColIntervalDayTime :: Maybe Validity -> VS.Vector IntervalDayTime -> ColumnArray
pattern ColIntervalDayTime v xs <- I.ColPrim PIntervalDayTime v xs


pattern ColIntervalMonthDayNano :: Maybe Validity -> VS.Vector IntervalMonthDayNano -> ColumnArray
pattern ColIntervalMonthDayNano v xs <- I.ColPrim PIntervalMonthDayNano v xs


-- | Precision, scale, validity, values.
pattern ColDecimal128 :: Int -> Int -> Maybe Validity -> VS.Vector Decimal128 -> ColumnArray
pattern ColDecimal128 p s v xs <- I.ColPrim (PDecimal128 p s) v xs


pattern ColDecimal256 :: Int -> Int -> Maybe Validity -> VS.Vector Decimal256 -> ColumnArray
pattern ColDecimal256 p s v xs <- I.ColPrim (PDecimal256 p s) v xs


-- | Validity, values bitmap.
pattern ColBool :: Maybe Validity -> Bitmap -> ColumnArray
pattern ColBool v b <- I.ColBool v b


-- | Validity, offsets (rows + 1, need not start at 0), UTF-8 data.
pattern ColUtf8 :: Maybe Validity -> VS.Vector Int32 -> ByteString -> ColumnArray
pattern ColUtf8 v o d <- I.ColUtf8 v o d


pattern ColBinary :: Maybe Validity -> VS.Vector Int32 -> ByteString -> ColumnArray
pattern ColBinary v o d <- I.ColBinary v o d


pattern ColLargeUtf8 :: Maybe Validity -> VS.Vector Int64 -> ByteString -> ColumnArray
pattern ColLargeUtf8 v o d <- I.ColLargeUtf8 v o d


pattern ColLargeBinary :: Maybe Validity -> VS.Vector Int64 -> ByteString -> ColumnArray
pattern ColLargeBinary v o d <- I.ColLargeBinary v o d


-- | Width, rows, validity, data (row @i@ is bytes @[i * width, (i + 1) * width)@).
pattern ColFixedSizeBinary :: Int -> Int -> Maybe Validity -> ByteString -> ColumnArray
pattern ColFixedSizeBinary w n v d <- I.ColFixedSizeBinary w n v d


-- | Validity, 16-byte views, variadic data buffers.
pattern ColUtf8View :: Maybe Validity -> ByteString -> V.Vector ByteString -> ColumnArray
pattern ColUtf8View v views bufs <- I.ColUtf8View v views bufs


pattern ColBinaryView :: Maybe Validity -> ByteString -> V.Vector ByteString -> ColumnArray
pattern ColBinaryView v views bufs <- I.ColBinaryView v views bufs


-- | Rows, validity, named children (each has at least rows rows).
pattern ColStruct :: Int -> Maybe Validity -> V.Vector (Text, ColumnArray) -> ColumnArray
pattern ColStruct n v cs <- I.ColStruct n v cs


pattern ColList :: Maybe Validity -> VS.Vector Int32 -> ColumnArray -> ColumnArray
pattern ColList v o c <- I.ColList v o c


pattern ColLargeList :: Maybe Validity -> VS.Vector Int64 -> ColumnArray -> ColumnArray
pattern ColLargeList v o c <- I.ColLargeList v o c


-- | Validity, offsets, sizes, child.
pattern ColListView :: Maybe Validity -> VS.Vector Int32 -> VS.Vector Int32 -> ColumnArray -> ColumnArray
pattern ColListView v o s c <- I.ColListView v o s c


pattern ColLargeListView :: Maybe Validity -> VS.Vector Int64 -> VS.Vector Int64 -> ColumnArray -> ColumnArray
pattern ColLargeListView v o s c <- I.ColLargeListView v o s c


-- | List size, rows, validity, child.
pattern ColFixedSizeList :: Int -> Int -> Maybe Validity -> ColumnArray -> ColumnArray
pattern ColFixedSizeList w n v c <- I.ColFixedSizeList w n v c


-- | Validity, offsets, keys, values.
pattern ColMap :: Maybe Validity -> VS.Vector Int32 -> ColumnArray -> ColumnArray -> ColumnArray
pattern ColMap v o k x <- I.ColMap v o k x


-- | Child index per row (not the schema type id), offset per row, children.
pattern ColDenseUnion :: VS.Vector Int8 -> VS.Vector Int32 -> V.Vector ColumnArray -> ColumnArray
pattern ColDenseUnion t o cs <- I.ColDenseUnion t o cs


pattern ColSparseUnion :: VS.Vector Int8 -> V.Vector ColumnArray -> ColumnArray
pattern ColSparseUnion t cs <- I.ColSparseUnion t cs


-- | Dictionary id, indices (an integer column at wire width, carrying the row validity), values.
pattern ColDictionary :: Int64 -> ColumnArray -> ColumnArray -> ColumnArray
pattern ColDictionary did ix vals <- I.ColDictionary did ix vals


-- | Logical offset, logical length, run ends, values.
pattern ColRunEndEncoded :: Int -> Int -> ColumnArray -> ColumnArray -> ColumnArray
pattern ColRunEndEncoded off len re vals <- I.ColRunEndEncoded off len re vals


{-# COMPLETE
  ColNull
  , ColPrim
  , ColBool
  , ColUtf8
  , ColBinary
  , ColLargeUtf8
  , ColLargeBinary
  , ColFixedSizeBinary
  , ColUtf8View
  , ColBinaryView
  , ColStruct
  , ColList
  , ColLargeList
  , ColListView
  , ColLargeListView
  , ColFixedSizeList
  , ColMap
  , ColDenseUnion
  , ColSparseUnion
  , ColDictionary
  , ColRunEndEncoded
  #-}


{-# COMPLETE
  ColNull
  , ColInt8
  , ColInt16
  , ColInt32
  , ColInt64
  , ColUInt8
  , ColUInt16
  , ColUInt32
  , ColUInt64
  , ColFloat16
  , ColFloat
  , ColDouble
  , ColDate32
  , ColDate64
  , ColTime32
  , ColTime64
  , ColTimestamp
  , ColDuration
  , ColIntervalYearMonth
  , ColIntervalDayTime
  , ColIntervalMonthDayNano
  , ColDecimal128
  , ColDecimal256
  , ColBool
  , ColUtf8
  , ColBinary
  , ColLargeUtf8
  , ColLargeBinary
  , ColFixedSizeBinary
  , ColUtf8View
  , ColBinaryView
  , ColStruct
  , ColList
  , ColLargeList
  , ColListView
  , ColLargeListView
  , ColFixedSizeList
  , ColMap
  , ColDenseUnion
  , ColSparseUnion
  , ColDictionary
  , ColRunEndEncoded
  #-}


-- ============================================================
-- Shape
-- ============================================================

-- | Row count. O(1).
columnLength :: ColumnArray -> Int
columnLength = \case
  I.ColNull n -> n
  I.ColPrim t _ xs -> withPrim t (VS.length xs)
  I.ColBool _ b -> bitmapLength b
  I.ColUtf8 _ o _ -> offsetRows o
  I.ColBinary _ o _ -> offsetRows o
  I.ColLargeUtf8 _ o _ -> offsetRows o
  I.ColLargeBinary _ o _ -> offsetRows o
  I.ColFixedSizeBinary _ n _ _ -> n
  I.ColUtf8View _ views _ -> BS.length views `quot` 16
  I.ColBinaryView _ views _ -> BS.length views `quot` 16
  I.ColStruct n _ _ -> n
  I.ColList _ o _ -> offsetRows o
  I.ColLargeList _ o _ -> offsetRows o
  I.ColListView _ o _ _ -> VS.length o
  I.ColLargeListView _ o _ _ -> VS.length o
  I.ColFixedSizeList _ n _ _ -> n
  I.ColMap _ o _ _ -> offsetRows o
  I.ColDenseUnion t _ _ -> VS.length t
  I.ColSparseUnion t _ -> VS.length t
  I.ColDictionary _ ix _ -> columnLength ix
  I.ColRunEndEncoded _ n _ _ -> n


offsetRows :: Storable o => VS.Vector o -> Int
offsetRows o = max 0 (VS.length o - 1)
{-# INLINE offsetRows #-}


{- | Null rows of the array itself, O(1). A 'ColNull' is all null;
unions and run-end-encoded columns have no validity of their own (0).
-}
nullCount :: ColumnArray -> Int
nullCount = \case
  I.ColNull n -> n
  c -> maybe 0 validityNullCount (validity c)


-- | The array's own validity ('Nothing' when it has no nulls or no validity slot).
validity :: ColumnArray -> Maybe Validity
validity = \case
  I.ColNull _ -> Nothing
  I.ColPrim _ v _ -> v
  I.ColBool v _ -> v
  I.ColUtf8 v _ _ -> v
  I.ColBinary v _ _ -> v
  I.ColLargeUtf8 v _ _ -> v
  I.ColLargeBinary v _ _ -> v
  I.ColFixedSizeBinary _ _ v _ -> v
  I.ColUtf8View v _ _ -> v
  I.ColBinaryView v _ _ -> v
  I.ColStruct _ v _ -> v
  I.ColList v _ _ -> v
  I.ColLargeList v _ _ -> v
  I.ColListView v _ _ _ -> v
  I.ColLargeListView v _ _ _ -> v
  I.ColFixedSizeList _ _ v _ -> v
  I.ColMap v _ _ _ -> v
  I.ColDenseUnion {} -> Nothing
  I.ColSparseUnion {} -> Nothing
  I.ColDictionary _ ix _ -> validity ix
  I.ColRunEndEncoded {} -> Nothing


-- | The constructor name (@ColInt64@, @ColUtf8@, ...), for messages.
columnTag :: ColumnArray -> String
columnTag = \case
  I.ColNull _ -> "ColNull"
  I.ColPrim t _ _ -> "Col" ++ primTypeName t
  I.ColBool {} -> "ColBool"
  I.ColUtf8 {} -> "ColUtf8"
  I.ColBinary {} -> "ColBinary"
  I.ColLargeUtf8 {} -> "ColLargeUtf8"
  I.ColLargeBinary {} -> "ColLargeBinary"
  I.ColFixedSizeBinary {} -> "ColFixedSizeBinary"
  I.ColUtf8View {} -> "ColUtf8View"
  I.ColBinaryView {} -> "ColBinaryView"
  I.ColStruct {} -> "ColStruct"
  I.ColList {} -> "ColList"
  I.ColLargeList {} -> "ColLargeList"
  I.ColListView {} -> "ColListView"
  I.ColLargeListView {} -> "ColLargeListView"
  I.ColFixedSizeList {} -> "ColFixedSizeList"
  I.ColMap {} -> "ColMap"
  I.ColDenseUnion {} -> "ColDenseUnion"
  I.ColSparseUnion {} -> "ColSparseUnion"
  I.ColDictionary {} -> "ColDictionary"
  I.ColRunEndEncoded {} -> "ColRunEndEncoded"


{- | Whether the array has a validity bitmap slot (everything except
'ColNull', unions and run-end-encoded columns).
-}
hasValiditySlot :: ColumnArray -> Bool
hasValiditySlot = \case
  I.ColNull _ -> False
  I.ColDenseUnion {} -> False
  I.ColSparseUnion {} -> False
  I.ColRunEndEncoded {} -> False
  _ -> True


-- ============================================================
-- Typed views
-- ============================================================

-- | A fixed-width column's validity and values.
data PrimArray a = PrimArray !(Maybe Validity) !(VS.Vector a)


-- | O(1). 'Nothing' unless the column has this tag (decimal precision and scale are not compared).
asPrim :: PrimType a -> ColumnArray -> Maybe (PrimArray a)
asPrim t = \case
  I.ColPrim t' v xs | Just Refl <- samePrimTag t t' -> Just (PrimArray v xs)
  _ -> Nothing
{-# INLINE asPrim #-}


primArrayLength :: Storable a => PrimArray a -> Int
primArrayLength (PrimArray _ xs) = VS.length xs
{-# INLINE primArrayLength #-}


-- | Row @i@: 'Nothing' when null or out of range. A bit test and a load.
primAt :: Storable a => PrimArray a -> Int -> Maybe a
primAt (PrimArray v xs) i
  | i < 0 || i >= VS.length xs = Nothing
  | unsafeIsValidAt v i = Just $! VS.unsafeIndex xs i
  | otherwise = Nothing
{-# INLINE primAt #-}


-- | 'primAt' without the range check.
unsafePrimAt :: Storable a => PrimArray a -> Int -> Maybe a
unsafePrimAt (PrimArray v xs) i
  | unsafeIsValidAt v i = Just $! VS.unsafeIndex xs i
  | otherwise = Nothing
{-# INLINE unsafePrimAt #-}


-- | The value slot of row @i@, ignoring validity (arrow-rs @value()@); errors out of range.
primValueAt :: Storable a => PrimArray a -> Int -> a
primValueAt (PrimArray _ xs) i = xs VS.! i
{-# INLINE primValueAt #-}


unsafePrimValueAt :: Storable a => PrimArray a -> Int -> a
unsafePrimValueAt (PrimArray _ xs) i = VS.unsafeIndex xs i
{-# INLINE unsafePrimValueAt #-}


-- | O(1) alias of the values; null slots hold unspecified values.
toStorable :: PrimArray a -> VS.Vector a
toStorable (PrimArray _ xs) = xs


-- | O(n), boxes every row.
toMaybeVector :: Storable a => PrimArray a -> V.Vector (Maybe a)
toMaybeVector arr = generateStrict (primArrayLength arr) (unsafePrimAt arr)
{-# INLINE toMaybeVector #-}


-- | A var-length column's validity, offsets and data.
data BytesArray o = BytesArray !(Maybe Validity) !(VS.Vector o) !ByteString


{- | A utf8 column: its data is valid UTF-8 at every row boundary, so
'textAt' copies without re-validating. Only 'asUtf8' and
'asLargeUtf8' produce one.
-}
newtype Utf8Array o = Utf8Array (BytesArray o)


asUtf8 :: ColumnArray -> Maybe (Utf8Array Int32)
asUtf8 = \case
  I.ColUtf8 v o d -> Just (Utf8Array (BytesArray v o d))
  _ -> Nothing
{-# INLINE asUtf8 #-}


asLargeUtf8 :: ColumnArray -> Maybe (Utf8Array Int64)
asLargeUtf8 = \case
  I.ColLargeUtf8 v o d -> Just (Utf8Array (BytesArray v o d))
  _ -> Nothing
{-# INLINE asLargeUtf8 #-}


-- | Binary columns, and utf8 columns viewed as bytes.
asBinary :: ColumnArray -> Maybe (BytesArray Int32)
asBinary = \case
  I.ColBinary v o d -> Just (BytesArray v o d)
  I.ColUtf8 v o d -> Just (BytesArray v o d)
  _ -> Nothing
{-# INLINE asBinary #-}


asLargeBinary :: ColumnArray -> Maybe (BytesArray Int64)
asLargeBinary = \case
  I.ColLargeBinary v o d -> Just (BytesArray v o d)
  I.ColLargeUtf8 v o d -> Just (BytesArray v o d)
  _ -> Nothing
{-# INLINE asLargeBinary #-}


bytesArrayLength :: Storable o => BytesArray o -> Int
bytesArrayLength (BytesArray _ o _) = offsetRows o
{-# INLINE bytesArrayLength #-}


-- | Row @i@ as a zero-copy slice; 'Nothing' when null or out of range.
bytesAt :: (Storable o, Integral o) => BytesArray o -> Int -> Maybe ByteString
bytesAt arr@(BytesArray _ o _) i
  | i < 0 || i >= VS.length o - 1 = Nothing
  | otherwise = unsafeBytesAt arr i
{-# INLINE bytesAt #-}


unsafeBytesAt :: (Storable o, Integral o) => BytesArray o -> Int -> Maybe ByteString
unsafeBytesAt (BytesArray v o d) i
  | unsafeIsValidAt v i =
      let !s = fromIntegral (VS.unsafeIndex o i)
          !e = fromIntegral (VS.unsafeIndex o (i + 1))
      in Just (BSU.unsafeTake (e - s) (BSU.unsafeDrop s d))
  | otherwise = Nothing
{-# INLINE unsafeBytesAt #-}


-- | Row @i@ copied into a fresh 'Text' (one copy, no re-validation).
textAt :: (Storable o, Integral o) => Utf8Array o -> Int -> Maybe Text
textAt (Utf8Array arr) i = utf8ToText <$> bytesAt arr i
{-# INLINE textAt #-}


unsafeTextAt :: (Storable o, Integral o) => Utf8Array o -> Int -> Maybe Text
unsafeTextAt (Utf8Array arr) i = utf8ToText <$> unsafeBytesAt arr i
{-# INLINE unsafeTextAt #-}


-- | Copy bytes known to be valid UTF-8 into a 'Text'.
utf8ToText :: ByteString -> Text
utf8ToText bs
  | BS.null bs = T.empty
  | otherwise = unsafeDupablePerformIO $ withBytesPtr bs $ \p -> TF.fromPtr p (fromIntegral (BS.length bs))


data BoolArray = BoolArray !(Maybe Validity) !Bitmap


asBool :: ColumnArray -> Maybe BoolArray
asBool = \case
  I.ColBool v b -> Just (BoolArray v b)
  _ -> Nothing
{-# INLINE asBool #-}


boolArrayAt :: BoolArray -> Int -> Maybe Bool
boolArrayAt (BoolArray v b) i
  | i < 0 || i >= bitmapLength b = Nothing
  | unsafeIsValidAt v i = if unsafeBitAt b i then justTrue else justFalse
  | otherwise = Nothing
{-# INLINE boolArrayAt #-}


boolAt :: ColumnArray -> Int -> Maybe Bool
boolAt c i = asBool c >>= \arr -> boolArrayAt arr i
{-# INLINE boolAt #-}


{- | Row @i@ of any byte-like column (utf8, binary, their large and
view variants, fixed-size binary) as a zero-copy slice.
-}
anyBytesAt :: ColumnArray -> Int -> Maybe ByteString
anyBytesAt c i
  | i < 0 || i >= columnLength c || not (unsafeIsValidAt (validity c) i) = Nothing
  | otherwise = case c of
      I.ColUtf8 _ o d -> Just $! varSlice o d i
      I.ColBinary _ o d -> Just $! varSlice o d i
      I.ColLargeUtf8 _ o d -> Just $! varSlice o d i
      I.ColLargeBinary _ o d -> Just $! varSlice o d i
      I.ColUtf8View _ views bufs -> Just $! viewAt views bufs i
      I.ColBinaryView _ views bufs -> Just $! viewAt views bufs i
      I.ColFixedSizeBinary w _ _ d -> Just $! BSU.unsafeTake w (BSU.unsafeDrop (i * w) d)
      _ -> Nothing


-- | Row @i@ of a utf8, large utf8 or utf8 view column, copied into a 'Text'.
anyTextAt :: ColumnArray -> Int -> Maybe Text
anyTextAt c i = case c of
  I.ColUtf8 {} -> textOf (anyBytesAt c i)
  I.ColLargeUtf8 {} -> textOf (anyBytesAt c i)
  I.ColUtf8View {} -> textOf (anyBytesAt c i)
  _ -> Nothing
  where
    textOf = \case
      Just bs -> Just $! utf8ToText bs
      Nothing -> Nothing


-- | Shared closures for boxed bool rows, so materializing bools allocates nothing per row.
justTrue, justFalse :: Maybe Bool
justTrue = Just True
justFalse = Just False
{-# NOINLINE justTrue #-}
{-# NOINLINE justFalse #-}


varSlice :: (Storable o, Integral o) => VS.Vector o -> ByteString -> Int -> ByteString
varSlice o d i =
  let !s = fromIntegral (VS.unsafeIndex o i)
      !e = fromIntegral (VS.unsafeIndex o (i + 1))
  in BSU.unsafeTake (e - s) (BSU.unsafeDrop s d)
{-# INLINE varSlice #-}


-- | Little-endian int32 at byte @k@ (no alignment needed).
le32 :: ByteString -> Int -> Int
le32 bs k =
  let b j = fromIntegral (BSU.unsafeIndex bs (k + j)) :: Word32
      !w = b 0 + b 1 * 0x100 + b 2 * 0x10000 + b 3 * 0x1000000
  in fromIntegral (fromIntegral w :: Int32)
{-# INLINE le32 #-}


-- | The bytes of view @i@ (the views are valid).
viewAt :: ByteString -> V.Vector ByteString -> Int -> ByteString
viewAt views bufs i =
  let !base = i * 16
      !len = le32 views base
  in if len <= 12
       then BSU.unsafeTake len (BSU.unsafeDrop (base + 4) views)
       else
         let !bi = le32 views (base + 8)
             !off = le32 views (base + 12)
         in BSU.unsafeTake len (BSU.unsafeDrop off (V.unsafeIndex bufs bi))


{- | The child range of row @i@ of a list, large list, list view, map
or fixed-size list column; 'Nothing' when the row is null, out of
range, or the column is not list-like.
-}
listRange :: ColumnArray -> Int -> Maybe ChildRange
listRange c i
  | i < 0 || i >= columnLength c || not (unsafeIsValidAt (validity c) i) = Nothing
  | otherwise = case c of
      I.ColList _ o _ -> Just (offsetRange o i)
      I.ColLargeList _ o _ -> Just (offsetRange o i)
      I.ColMap _ o _ _ -> Just (offsetRange o i)
      I.ColListView _ o s _ -> Just (ChildRange (fromIntegral (VS.unsafeIndex o i)) (fromIntegral (VS.unsafeIndex s i)))
      I.ColLargeListView _ o s _ -> Just (ChildRange (fromIntegral (VS.unsafeIndex o i)) (fromIntegral (VS.unsafeIndex s i)))
      I.ColFixedSizeList w _ _ _ -> Just (ChildRange (i * w) w)
      _ -> Nothing


offsetRange :: (Storable o, Integral o) => VS.Vector o -> Int -> ChildRange
offsetRange o i =
  let !s = fromIntegral (VS.unsafeIndex o i)
  in ChildRange s (fromIntegral (VS.unsafeIndex o (i + 1)) - s)
{-# INLINE offsetRange #-}


-- | The dictionary key of row @i@ of a dictionary column ('Nothing' when null or out of range).
dictKeyAt :: ColumnArray -> Int -> Maybe Int
dictKeyAt c i = case c of
  I.ColDictionary _ ix _ | i >= 0 && i < columnLength ix && unsafeIsValidAt (validity ix) i -> Just (keyAt ix i)
  _ -> Nothing


-- | Key @i@ of an integer column (validity ignored); 0 for a non-integer column.
keyAt :: ColumnArray -> Int -> Int
keyAt c i = case c of
  I.ColPrim t _ xs | Just IntegralPrim <- integralPrim t -> fromIntegral (VS.unsafeIndex xs i)
  _ -> 0
{-# INLINE keyAt #-}


-- ============================================================
-- Conversions
-- ============================================================

{- | O(n): utf8, large utf8 and utf8 view columns, and dictionaries whose
values are one of those.

Utf8 and large utf8: the bytes the column references are copied once
into a fresh array and every row is a 'Text' slice of it (no per-row
copy, no re-validation). The rows share that array, so keeping any one
of them keeps the whole column's text alive ('T.copy' detaches a row).
Utf8 views copy each row on its own. Dictionaries convert their values
once and the rows share the resulting 'Text's (and their 'Just' boxes).
-}
toTextVector :: ColumnArray -> Either String (V.Vector (Maybe Text))
toTextVector c = case c of
  I.ColUtf8 v o d -> Right (textSlices v o d)
  I.ColLargeUtf8 v o d -> Right (textSlices v o d)
  I.ColUtf8View v views bufs -> Right (generateStrict (columnLength c) (\i -> if unsafeIsValidAt v i then Just $! utf8ToText (viewAt views bufs i) else Nothing))
  I.ColDictionary _ keys vals -> toTextVector vals >>= gatherDictionary "Arrow.Column.toTextVector" keys
  _ -> Left ("Arrow.Column.toTextVector: not a utf8 column: " ++ columnTag c)


-- | Every row of a utf8 column as a slice of one fresh copy of the referenced bytes.
textSlices :: (Storable o, Integral o) => Maybe Validity -> VS.Vector o -> ByteString -> V.Vector (Maybe Text)
textSlices v o d
  | VS.length o < 2 = V.empty
  | otherwise =
      let !n = offsetRows o
          !base = fromIntegral (VS.unsafeIndex o 0) :: Int
          !total = fromIntegral (VS.unsafeIndex o n) - base
          !arr = copyToTextArray d base total
          row i =
            let !s = fromIntegral (VS.unsafeIndex o i)
                !e = fromIntegral (VS.unsafeIndex o (i + 1))
            in if e == s then T.empty else TI.Text arr (s - base) (e - s)
      in generateStrict n (\i -> if unsafeIsValidAt v i then Just $! row i else Nothing)
{-# INLINE textSlices #-}


-- | @len@ bytes of @bs@ from byte @off@, copied into a fresh text array.
copyToTextArray :: ByteString -> Int -> Int -> TA.Array
copyToTextArray bs off len
  | len <= 0 = TA.empty
  | otherwise = unsafeDupablePerformIO $ withBytesPtr bs $ \p -> stToIO $ do
      ma <- TA.new len
      TA.copyFromPointer ma 0 (p `plusPtr` off) len
      TA.unsafeFreeze ma


{- | O(n): every byte-like column (zero-copy slices of the column's
buffers), and dictionaries whose values are byte-like (the values are
converted once and the rows share them).
-}
toBytesVector :: ColumnArray -> Either String (V.Vector (Maybe ByteString))
toBytesVector c = case c of
  I.ColUtf8 v o d -> Right (varSlices v o d)
  I.ColBinary v o d -> Right (varSlices v o d)
  I.ColLargeUtf8 v o d -> Right (varSlices v o d)
  I.ColLargeBinary v o d -> Right (varSlices v o d)
  I.ColUtf8View v views bufs -> Right (rows v (viewAt views bufs))
  I.ColBinaryView v views bufs -> Right (rows v (viewAt views bufs))
  I.ColFixedSizeBinary w _ v d -> Right (rows v (\i -> BSU.unsafeTake w (BSU.unsafeDrop (i * w) d)))
  I.ColDictionary _ keys vals -> toBytesVector vals >>= gatherDictionary "Arrow.Column.toBytesVector" keys
  _ -> Left ("Arrow.Column.toBytesVector: not a byte column: " ++ columnTag c)
  where
    rows :: Maybe Validity -> (Int -> ByteString) -> V.Vector (Maybe ByteString)
    rows v at = generateStrict (columnLength c) (\i -> if unsafeIsValidAt v i then Just $! at i else Nothing)
    {-# INLINE rows #-}
    varSlices :: (Storable o, Integral o) => Maybe Validity -> VS.Vector o -> ByteString -> V.Vector (Maybe ByteString)
    varSlices v o d = rows v (varSlice o d)
    {-# INLINE varSlices #-}


-- | O(n), boxes every row (the two 'Just' boxes are shared).
toBoolVector :: ColumnArray -> Either String (V.Vector (Maybe Bool))
toBoolVector c = case c of
  I.ColBool v b -> Right (generateStrict (bitmapLength b) (\i -> if unsafeIsValidAt v i then (if unsafeBitAt b i then justTrue else justFalse) else Nothing))
  _ -> Left ("Arrow.Column.toBoolVector: not a bool column: " ++ columnTag c)


{- | O(n) plus the child conversion: list, large list, list view and
fixed-size list columns. The child rows the lists reference are
converted once with the given function, and every list row is a slice
of that vector (the rows share it); null rows are 'Nothing'. The
function must return one element per child row it is given.
-}
toListVector :: forall a. (ColumnArray -> Either String (V.Vector a)) -> ColumnArray -> Either String (V.Vector (Maybe (V.Vector a)))
toListVector conv c = case c of
  I.ColList v o child -> offsets v o child
  I.ColLargeList v o child -> offsets v o child
  I.ColListView v o z child -> views v o z child
  I.ColLargeListView v o z child -> views v o z child
  I.ColFixedSizeList w n v child -> do
    kids <- convert (sliceColumnArray 0 (n * w) child) (n * w)
    Right (rows n v (\i -> V.unsafeSlice (i * w) w kids))
  _ -> Left ("Arrow.Column.toListVector: not a list column: " ++ columnTag c)
  where
    convert ch len = do
      kids <- conv ch
      if V.length kids == len
        then Right kids
        else Left ("Arrow.Column.toListVector: the child conversion returned " ++ show (V.length kids) ++ " rows for " ++ show len)
    rows :: Int -> Maybe Validity -> (Int -> V.Vector a) -> V.Vector (Maybe (V.Vector a))
    rows n v at = generateStrict n (\i -> if unsafeIsValidAt v i then Just $! at i else Nothing)
    {-# INLINE rows #-}
    offsets :: (Storable o, Integral o) => Maybe Validity -> VS.Vector o -> ColumnArray -> Either String (V.Vector (Maybe (V.Vector a)))
    offsets v o child
      | VS.length o < 2 = Right V.empty
      | otherwise = do
          let !n = offsetRows o
              !base = fromIntegral (VS.unsafeIndex o 0)
              !len = fromIntegral (VS.unsafeIndex o n) - base
          kids <- convert (sliceColumnArray base len child) len
          Right $ rows n v $ \i ->
            let !s = fromIntegral (VS.unsafeIndex o i)
            in V.unsafeSlice (s - base) (fromIntegral (VS.unsafeIndex o (i + 1)) - s) kids
    {-# INLINE offsets #-}
    views :: (Storable o, Integral o) => Maybe Validity -> VS.Vector o -> VS.Vector o -> ColumnArray -> Either String (V.Vector (Maybe (V.Vector a)))
    views v o z child = do
      kids <- convert child (columnLength child)
      Right (rows (VS.length o) v (\i -> V.unsafeSlice (fromIntegral (VS.unsafeIndex o i)) (fromIntegral (VS.unsafeIndex z i)) kids))
    {-# INLINE views #-}


{- | Rows of a dictionary column from its converted values: row @i@ is
the value its key selects (shared, not copied), 'Nothing' where the key
is null. Every valid key is checked against the values first.
-}
gatherDictionary :: forall a. String -> ColumnArray -> V.Vector (Maybe a) -> Either String (V.Vector (Maybe a))
gatherDictionary what keys vals = do
  validateKeys what keys (V.length vals)
  case keys of
    I.ColPrim t kv xs -> case t of
      PInt8 -> Right (go kv xs)
      PInt16 -> Right (go kv xs)
      PInt32 -> Right (go kv xs)
      PInt64 -> Right (go kv xs)
      PUInt8 -> Right (go kv xs)
      PUInt16 -> Right (go kv xs)
      PUInt32 -> Right (go kv xs)
      PUInt64 -> Right (go kv xs)
      _ -> notInt
    _ -> notInt
  where
    notInt = Left (what ++ ": dictionary indices must be an integer column")
    go :: (Storable k, Integral k) => Maybe Validity -> VS.Vector k -> V.Vector (Maybe a)
    go kv xs = generateStrict (VS.length xs) $ \i ->
      if unsafeIsValidAt kv i then V.unsafeIndex vals (fromIntegral (VS.unsafeIndex xs i)) else Nothing
    {-# INLINE go #-}


{- | 'V.generate' that forces each element to WHNF as it is written. The
boxed conversions go through it so their results hold no thunks: a lazy
element would allocate a thunk per row and keep the column (and the
decoded input buffer it aliases) alive until every row is forced.

The loop writes two elements per iteration. On AArch64, GHC 9.8 compiles
'writeArray#' to a store-release ('stlr') plus the card mark, and a loop
whose body is one fresh allocation followed by one such write runs about
3.5x slower on Apple cores than the same work unrolled by two (measured:
100k boxed @Maybe Int64@, 0.80 ms against 0.23 ms; a C loop with the same
store sequence shows the same cliff). Unrolling further gains nothing.
-}
generateStrict :: Int -> (Int -> a) -> V.Vector a
generateStrict n f = V.create $ do
  mv <- VM.unsafeNew n
  let go !i
        | i + 1 < n = do
            VM.unsafeWrite mv i $! f i
            VM.unsafeWrite mv (i + 1) $! f (i + 1)
            go (i + 2)
        | i < n = do
            VM.unsafeWrite mv i $! f i
            pure mv
        | otherwise = pure mv
  go 0
{-# INLINE generateStrict #-}


-- ============================================================
-- Construction
-- ============================================================

-- | O(1): a fixed-width column without nulls.
primColumn :: PrimType a -> VS.Vector a -> ColumnArray
primColumn t xs = I.ColPrim t Nothing xs


-- | O(1) length check plus a popcount: values with a validity bitmap (bit set = valid).
primColumnV :: PrimType a -> Bitmap -> VS.Vector a -> Either String ColumnArray
primColumnV t b xs = withPrim t $ do
  _ <- mkBitmap (bitmapBytes b) (bitmapOffset b) (bitmapLength b)
  if bitmapLength b /= VS.length xs
    then Left "Arrow.Column.primColumnV: bitmap length differs from the number of values"
    else Right (I.ColPrim t (mkValidity b) xs)


-- | A fixed-width column with an optional validity (checked and normalised).
mkPrim :: PrimType a -> Maybe Validity -> VS.Vector a -> Either String ColumnArray
mkPrim t v xs = withPrim t $ I.ColPrim t <$> checkValidity "Arrow.Column.mkPrim" (VS.length xs) v <*> pure xs


-- | O(n), one pass: 'Nothing' rows become nulls (their slots hold zero bytes).
fromMaybes :: PrimType a -> V.Vector (Maybe a) -> ColumnArray
fromMaybes t vals = withPrim t $
  let !n = V.length vals
      !w = primWidth t
      bytes = createAligned (n * w) $ \p -> do
        fillBytes p 0 (n * w)
        forM_ [0 .. n - 1] $ \i -> case V.unsafeIndex vals i of
          Just x -> pokeElemOff (castPtr p) i x
          Nothing -> pure ()
  in I.ColPrim t (validityGenerate n (\i -> isJustAt vals i)) (unsafeBytesToStorable bytes)


isJustAt :: V.Vector (Maybe a) -> Int -> Bool
isJustAt v i = case V.unsafeIndex v i of
  Just _ -> True
  Nothing -> False
{-# INLINE isJustAt #-}


fromBools :: V.Vector Bool -> ColumnArray
fromBools v = I.ColBool Nothing (bitmapFromBools v)


fromMaybeBools :: V.Vector (Maybe Bool) -> ColumnArray
fromMaybeBools v =
  I.ColBool
    (validityGenerate (V.length v) (isJustAt v))
    (bitmapGenerate (V.length v) (\i -> V.unsafeIndex v i == Just True))


-- | Utf8 column, two passes (lengths, then one copy per row). Errors past 2^31 - 1 bytes.
fromTexts :: V.Vector Text -> ColumnArray
fromTexts = fromMaybeTexts . V.map Just


fromMaybeTexts :: V.Vector (Maybe Text) -> ColumnArray
fromMaybeTexts = varFromMaybes I.ColUtf8 TF.lengthWord8 TF.unsafeCopyToPtr


fromMaybeLargeTexts :: V.Vector (Maybe Text) -> ColumnArray
fromMaybeLargeTexts = varFromMaybes I.ColLargeUtf8 TF.lengthWord8 TF.unsafeCopyToPtr


fromByteStrings :: V.Vector ByteString -> ColumnArray
fromByteStrings = fromMaybeByteStrings . V.map Just


fromMaybeByteStrings :: V.Vector (Maybe ByteString) -> ColumnArray
fromMaybeByteStrings = varFromMaybes I.ColBinary BS.length copyBS


fromMaybeLargeByteStrings :: V.Vector (Maybe ByteString) -> ColumnArray
fromMaybeLargeByteStrings = varFromMaybes I.ColLargeBinary BS.length copyBS


copyBS :: ByteString -> Ptr Word8 -> IO ()
copyBS bs dst = withBytesPtr bs $ \src -> copyBytes dst src (BS.length bs)


varFromMaybes
  :: forall o x
   . Offset o
  => (Maybe Validity -> VS.Vector o -> ByteString -> ColumnArray)
  -> (x -> Int)
  -> (x -> Ptr Word8 -> IO ())
  -> V.Vector (Maybe x)
  -> ColumnArray
varFromMaybes mk len copy vals
  | toInteger total > toInteger (maxBound :: o) =
      errorWithoutStackTrace "Arrow.Column: var-length data exceeds the 32-bit offset range (use the large variant)"
  | otherwise = mk (validityGenerate n (isJustAt vals)) offs dat
  where
    !n = V.length vals
    rowLen i = maybe 0 len (V.unsafeIndex vals i)
    offs :: VS.Vector o
    offs = VS.scanl' (\acc i -> acc + fromIntegral (rowLen i)) 0 (VS.enumFromN 0 n :: VS.Vector Int)
    !total = fromIntegral (VS.last offs) :: Int
    dat = createAligned total $ \p ->
      forM_ [0 .. n - 1] $ \i -> case V.unsafeIndex vals i of
        Just x -> copy x (p `plusPtr` fromIntegral (VS.unsafeIndex offs i))
        Nothing -> pure ()


-- | Fixed-size binary rows; every present row must have the given width.
fromMaybeFixedSizeBinary :: Int -> V.Vector (Maybe ByteString) -> Either String ColumnArray
fromMaybeFixedSizeBinary w vals
  | w < 0 = Left "Arrow.Column.fromMaybeFixedSizeBinary: negative width"
  | V.any (maybe False ((/= w) . BS.length)) vals =
      Left ("Arrow.Column.fromMaybeFixedSizeBinary: a row is not " ++ show w ++ " bytes")
  | otherwise =
      let !n = V.length vals
          dat = createAligned (n * w) $ \p -> do
            fillBytes p 0 (n * w)
            forM_ [0 .. n - 1] $ \i -> forM_ (V.unsafeIndex vals i) $ \bs -> copyBS bs (p `plusPtr` (i * w))
      in Right (I.ColFixedSizeBinary w n (validityGenerate n (isJustAt vals)) dat)


-- | Utf8 view column: short strings inline, long ones in one data buffer.
fromMaybeUtf8View :: V.Vector (Maybe Text) -> ColumnArray
fromMaybeUtf8View vals = buildViews I.ColUtf8View (V.map (fmap textBytes) vals)
  where
    textBytes t = createAligned (TF.lengthWord8 t) (TF.unsafeCopyToPtr t)


fromMaybeBinaryView :: V.Vector (Maybe ByteString) -> ColumnArray
fromMaybeBinaryView = buildViews I.ColBinaryView


buildViews :: (Maybe Validity -> ByteString -> V.Vector ByteString -> ColumnArray) -> V.Vector (Maybe ByteString) -> ColumnArray
buildViews mk vals
  | outOfLine > fromIntegral (maxBound :: Int32) =
      errorWithoutStackTrace "Arrow.Column: view data exceeds 2^31 - 1 bytes"
  | otherwise = mk (validityGenerate n (isJustAt vals)) views (if outOfLine == 0 then V.empty else V.singleton dat)
  where
    !n = V.length vals
    long = maybe 0 (\b -> if BS.length b > 12 then BS.length b else 0)
    !outOfLine = V.sum (V.map long vals)
    dat = createAligned outOfLine $ \p ->
      V.foldM'_ (\off mb -> case mb of
        Just b | BS.length b > 12 -> copyBS b (p `plusPtr` off) >> pure (off + BS.length b)
        _ -> pure off) 0 vals
    views = createAligned (n * 16) $ \p -> do
      fillBytes p 0 (n * 16)
      V.ifoldM'_ (\off i mb -> case mb of
        Nothing -> pure off
        Just b -> do
          let !base = p `plusPtr` (i * 16)
              !len = BS.length b
          pokeByteOff base 0 (fromIntegral len :: Int32)
          if len <= 12
            then copyBS b (base `plusPtr` 4) >> pure off
            else do
              copyBS (BS.take 4 b) (base `plusPtr` 4)
              pokeByteOff base 8 (0 :: Int32)
              pokeByteOff base 12 (fromIntegral off :: Int32)
              pure (off + len)) 0 vals


mkBool :: Maybe Validity -> Bitmap -> Either String ColumnArray
mkBool v b = do
  _ <- mkBitmap (bitmapBytes b) (bitmapOffset b) (bitmapLength b)
  v' <- checkValidity "Arrow.Column.mkBool" (bitmapLength b) v
  Right (I.ColBool v' b)


mkVar :: Offset o => String -> Bool -> (Maybe Validity -> VS.Vector o -> ByteString -> ColumnArray) -> Maybe Validity -> VS.Vector o -> ByteString -> Either String ColumnArray
mkVar what utf8 mk v offs dat = do
  validateOffsets what (BS.length dat) offs
  when utf8 (validateUtf8 what offs dat)
  v' <- checkValidity what (offsetRows offs) v
  Right (mk v' offs dat)


-- | Validates offsets, UTF-8 and character boundaries (C kernels).
mkUtf8 :: Maybe Validity -> VS.Vector Int32 -> ByteString -> Either String ColumnArray
mkUtf8 = mkVar "Arrow.Column.mkUtf8" True I.ColUtf8


mkBinary :: Maybe Validity -> VS.Vector Int32 -> ByteString -> Either String ColumnArray
mkBinary = mkVar "Arrow.Column.mkBinary" False I.ColBinary


mkLargeUtf8 :: Maybe Validity -> VS.Vector Int64 -> ByteString -> Either String ColumnArray
mkLargeUtf8 = mkVar "Arrow.Column.mkLargeUtf8" True I.ColLargeUtf8


mkLargeBinary :: Maybe Validity -> VS.Vector Int64 -> ByteString -> Either String ColumnArray
mkLargeBinary = mkVar "Arrow.Column.mkLargeBinary" False I.ColLargeBinary


-- | Width, rows, validity, data.
mkFixedSizeBinary :: Int -> Int -> Maybe Validity -> ByteString -> Either String ColumnArray
mkFixedSizeBinary w n v dat
  | w < 0 || n < 0 = Left "Arrow.Column.mkFixedSizeBinary: negative width or row count"
  | toInteger (BS.length dat) < toInteger w * toInteger n = Left "Arrow.Column.mkFixedSizeBinary: data shorter than width * rows"
  | otherwise = (\v' -> I.ColFixedSizeBinary w n v' dat) <$> checkValidity "Arrow.Column.mkFixedSizeBinary" n v


mkViews :: String -> Bool -> (Maybe Validity -> ByteString -> V.Vector ByteString -> ColumnArray) -> Maybe Validity -> ByteString -> V.Vector ByteString -> Either String ColumnArray
mkViews what utf8 mk v views bufs
  | BS.length views `rem` 16 /= 0 = Left (what ++ ": views buffer is not a multiple of 16 bytes")
  | otherwise = do
      let !rows = BS.length views `quot` 16
      v' <- checkValidity what rows v
      validateViews what utf8 rows v' views bufs
      Right (mk v' views bufs)


mkUtf8View :: Maybe Validity -> ByteString -> V.Vector ByteString -> Either String ColumnArray
mkUtf8View = mkViews "Arrow.Column.mkUtf8View" True I.ColUtf8View


mkBinaryView :: Maybe Validity -> ByteString -> V.Vector ByteString -> Either String ColumnArray
mkBinaryView = mkViews "Arrow.Column.mkBinaryView" False I.ColBinaryView


-- | Rows, validity, children (each with at least rows rows).
mkStruct :: Int -> Maybe Validity -> V.Vector (Text, ColumnArray) -> Either String ColumnArray
mkStruct n v cs
  | n < 0 = Left "Arrow.Column.mkStruct: negative row count"
  | V.any ((< n) . columnLength . snd) cs = Left "Arrow.Column.mkStruct: a child has fewer rows than the struct"
  | otherwise = (\v' -> I.ColStruct n v' cs) <$> checkValidity "Arrow.Column.mkStruct" n v


mkListLike :: Offset o => String -> (Maybe Validity -> VS.Vector o -> ColumnArray -> ColumnArray) -> Maybe Validity -> VS.Vector o -> ColumnArray -> Either String ColumnArray
mkListLike what mk v offs child = do
  validateOffsets what (columnLength child) offs
  v' <- checkValidity what (offsetRows offs) v
  Right (mk v' offs child)


mkList :: Maybe Validity -> VS.Vector Int32 -> ColumnArray -> Either String ColumnArray
mkList = mkListLike "Arrow.Column.mkList" I.ColList


mkLargeList :: Maybe Validity -> VS.Vector Int64 -> ColumnArray -> Either String ColumnArray
mkLargeList = mkListLike "Arrow.Column.mkLargeList" I.ColLargeList


-- | Validity, offsets, sizes, child.
mkListView :: Maybe Validity -> VS.Vector Int32 -> VS.Vector Int32 -> ColumnArray -> Either String ColumnArray
mkListView v o s c = do
  v' <- checkValidity "Arrow.Column.mkListView" (VS.length o) v
  validateListView "Arrow.Column.mkListView" v' o s (columnLength c)
  Right (I.ColListView v' o s c)


mkLargeListView :: Maybe Validity -> VS.Vector Int64 -> VS.Vector Int64 -> ColumnArray -> Either String ColumnArray
mkLargeListView v o s c = do
  v' <- checkValidity "Arrow.Column.mkLargeListView" (VS.length o) v
  validateListView "Arrow.Column.mkLargeListView" v' o s (columnLength c)
  Right (I.ColLargeListView v' o s c)


-- | List size, rows, validity, child (at least rows * size rows).
mkFixedSizeList :: Int -> Int -> Maybe Validity -> ColumnArray -> Either String ColumnArray
mkFixedSizeList w n v c
  | w < 0 || n < 0 = Left "Arrow.Column.mkFixedSizeList: negative size or row count"
  | toInteger (columnLength c) < toInteger w * toInteger n = Left "Arrow.Column.mkFixedSizeList: child shorter than rows * size"
  | otherwise = (\v' -> I.ColFixedSizeList w n v' c) <$> checkValidity "Arrow.Column.mkFixedSizeList" n v


-- | Validity, offsets, keys, values (offsets checked against both children).
mkMap :: Maybe Validity -> VS.Vector Int32 -> ColumnArray -> ColumnArray -> Either String ColumnArray
mkMap v offs k x = do
  validateOffsets "Arrow.Column.mkMap" (min (columnLength k) (columnLength x)) offs
  v' <- checkValidity "Arrow.Column.mkMap" (offsetRows offs) v
  Right (I.ColMap v' offs k x)


-- | Child index per row, offset per row, children.
mkDenseUnion :: VS.Vector Int8 -> VS.Vector Int32 -> V.Vector ColumnArray -> Either String ColumnArray
mkDenseUnion t o cs = do
  validateDenseUnion "Arrow.Column.mkDenseUnion" t o (VS.fromList (map (fromIntegral . columnLength) (V.toList cs)))
  Right (I.ColDenseUnion t o cs)


-- | Child index per row, children (each with at least as many rows as the union).
mkSparseUnion :: VS.Vector Int8 -> V.Vector ColumnArray -> Either String ColumnArray
mkSparseUnion t cs
  | V.any ((< VS.length t) . columnLength) cs = Left "Arrow.Column.mkSparseUnion: a child has fewer rows than the union"
  | otherwise = do
      validateSparseUnionTypes "Arrow.Column.mkSparseUnion" t (V.length cs)
      Right (I.ColSparseUnion t cs)


-- | Dictionary id, indices (an integer column), values. Checks every valid key (C kernel).
mkDictionary :: Int64 -> ColumnArray -> ColumnArray -> Either String ColumnArray
mkDictionary did ix vals = do
  validateKeys "Arrow.Column.mkDictionary" ix (columnLength vals)
  Right (I.ColDictionary did ix vals)


{- | Run ends (an int16, int32 or int64 column, positive and strictly
increasing) and values (one row per run). The logical length is the
last run end.
-}
mkRunEndEncoded :: ColumnArray -> ColumnArray -> Either String ColumnArray
mkRunEndEncoded re vals = do
  validateRunEnds "Arrow.Column.mkRunEndEncoded" re 0
  let !runs = columnLength re
      !len = if runs == 0 then 0 else keyAt re (runs - 1)
  if columnLength vals < runs
    then Left "Arrow.Column.mkRunEndEncoded: fewer values than runs"
    else Right (I.ColRunEndEncoded 0 len re vals)


emptyPrim :: PrimType a -> ColumnArray
emptyPrim t = withPrim t (I.ColPrim t Nothing VS.empty)


offsets0 :: Storable o => Num o => VS.Vector o
offsets0 = VS.singleton 0


{- | The zero-row column of a field's type, recursively for children,
with dictionary encoding (indices of the declared index type and a
'placeholderColumn' as values).
-}
emptyColumnFor :: Field -> Either String ColumnArray
emptyColumnFor f = case fieldDictionary f of
  Just de -> do
    vals <- placeholderColumn f
    ix <- case primTypeFor (deIndexType de) of
      Just (SomePrimType t) | Just IntegralPrim <- integralPrim t -> Right (emptyPrim t)
      _ -> Left ("Arrow.Column: dictionary index type must be an integer type, got " ++ show (deIndexType de))
    Right (I.ColDictionary (deId de) ix vals)
  Nothing -> case fieldType f of
    ANull -> Right (I.ColNull 0)
    AInt w _
      | Just (SomePrimType t) <- primTypeFor (fieldType f) -> Right (emptyPrim t)
      | otherwise -> Left ("Arrow.Column: unsupported integer bit width " ++ show w)
    ty | Just (SomePrimType t) <- primTypeFor ty -> Right (emptyPrim t)
    ABool -> Right (I.ColBool Nothing emptyBitmap)
    AUtf8 -> Right (I.ColUtf8 Nothing offsets0 BS.empty)
    ABinary -> Right (I.ColBinary Nothing offsets0 BS.empty)
    ALargeUtf8 -> Right (I.ColLargeUtf8 Nothing offsets0 BS.empty)
    ALargeBinary -> Right (I.ColLargeBinary Nothing offsets0 BS.empty)
    AFixedSizeBinary w -> Right (I.ColFixedSizeBinary w 0 Nothing BS.empty)
    AUtf8View -> Right (I.ColUtf8View Nothing BS.empty V.empty)
    ABinaryView -> Right (I.ColBinaryView Nothing BS.empty V.empty)
    AStruct -> I.ColStruct 0 Nothing <$> V.mapM (\c -> (,) (fieldName c) <$> emptyColumnFor c) (fieldChildren f)
    AList -> I.ColList Nothing offsets0 <$> onlyChild
    ALargeList -> I.ColLargeList Nothing offsets0 <$> onlyChild
    AListView -> I.ColListView Nothing VS.empty VS.empty <$> onlyChild
    ALargeListView -> I.ColLargeListView Nothing VS.empty VS.empty <$> onlyChild
    AFixedSizeList n -> I.ColFixedSizeList n 0 Nothing <$> onlyChild
    AMap _ -> case V.toList (fieldChildren f) of
      [entries] | [kf, vf] <- V.toList (fieldChildren entries) -> I.ColMap Nothing offsets0 <$> emptyColumnFor kf <*> emptyColumnFor vf
      _ -> Left "Arrow.Column: map field must have one entries struct child with key and value"
    AUnion Dense _ -> I.ColDenseUnion VS.empty VS.empty <$> V.mapM emptyColumnFor (fieldChildren f)
    AUnion Sparse _ -> I.ColSparseUnion VS.empty <$> V.mapM emptyColumnFor (fieldChildren f)
    ARunEndEncoded -> case V.toList (fieldChildren f) of
      [ref, vf] -> I.ColRunEndEncoded 0 0 <$> emptyColumnFor ref {fieldNullable = False} <*> emptyColumnFor vf
      _ -> Left "Arrow.Column: RunEndEncoded field must have exactly two children (run_ends, values)"
    ty -> Left ("Arrow.Column: no column for type " ++ show ty)
  where
    onlyChild = case V.toList (fieldChildren f) of
      [c] -> emptyColumnFor c
      _ -> Left ("Arrow.Column: " ++ show (fieldType f) ++ " field must have exactly one child")


{- | The empty values column of a dictionary field, held by an
unresolved dictionary column until 'resolveDictionaryColumn'.
-}
placeholderColumn :: Field -> Either String ColumnArray
placeholderColumn f = emptyColumnFor f {fieldNullable = False, fieldDictionary = Nothing}


zeroBytes :: Int -> ByteString
zeroBytes n = createAligned n (\p -> fillBytes p 0 n)


zeroStorable :: forall a. Storable a => Int -> VS.Vector a
zeroStorable n = unsafeBytesToStorable (zeroBytes (n * sizeOf (undefined :: a)))


{- | @n@ rows with the shape of the given column (same tag, widths,
decimal parameters, child shapes and dictionary id), every row valid
and zero or empty: lists are empty, unions select their first child,
a run-end-encoded column is one run, a 'ColNull' stays null. Used
where a column needs a row count but its rows carry no meaning (the
children under null struct rows, the expansion of null rows over an
empty dictionary).
-}
fillerColumn :: Int -> ColumnArray -> ColumnArray
fillerColumn n0 col = case col of
  I.ColNull _ -> I.ColNull n
  I.ColPrim t _ _ -> withPrim t (I.ColPrim t Nothing (zeroStorable n))
  I.ColBool _ _ -> I.ColBool Nothing (Bitmap (zeroBytes ((n + 7) `unsafeShiftR` 3)) 0 n)
  I.ColUtf8 {} -> I.ColUtf8 Nothing (zeroStorable (n + 1)) BS.empty
  I.ColBinary {} -> I.ColBinary Nothing (zeroStorable (n + 1)) BS.empty
  I.ColLargeUtf8 {} -> I.ColLargeUtf8 Nothing (zeroStorable (n + 1)) BS.empty
  I.ColLargeBinary {} -> I.ColLargeBinary Nothing (zeroStorable (n + 1)) BS.empty
  I.ColFixedSizeBinary w _ _ _ -> I.ColFixedSizeBinary w n Nothing (zeroBytes (n * w))
  I.ColUtf8View {} -> I.ColUtf8View Nothing (zeroBytes (n * 16)) V.empty
  I.ColBinaryView {} -> I.ColBinaryView Nothing (zeroBytes (n * 16)) V.empty
  I.ColStruct _ _ cs -> I.ColStruct n Nothing (V.map (fmap (fillerColumn n)) cs)
  I.ColList _ _ c -> I.ColList Nothing (zeroStorable (n + 1)) (empty c)
  I.ColLargeList _ _ c -> I.ColLargeList Nothing (zeroStorable (n + 1)) (empty c)
  I.ColListView _ _ _ c -> I.ColListView Nothing (zeroStorable n) (zeroStorable n) (empty c)
  I.ColLargeListView _ _ _ c -> I.ColLargeListView Nothing (zeroStorable n) (zeroStorable n) (empty c)
  I.ColFixedSizeList w _ _ c -> I.ColFixedSizeList w n Nothing (fillerColumn (n * w) c)
  I.ColMap _ _ k x -> I.ColMap Nothing (zeroStorable (n + 1)) (empty k) (empty x)
  I.ColDenseUnion _ _ cs
    | n == 0 || V.null cs -> I.ColDenseUnion VS.empty VS.empty (V.map empty cs)
    | otherwise -> I.ColDenseUnion (zeroStorable n) (zeroStorable n) (V.imap (\i c -> if i == 0 then fillerColumn 1 c else empty c) cs)
  I.ColSparseUnion _ cs -> I.ColSparseUnion (zeroStorable n) (V.map (fillerColumn n) cs)
  I.ColDictionary did ix vals
    | n == 0 -> I.ColDictionary did (fillerColumn 0 ix) vals
    | columnLength vals == 0 -> I.ColDictionary did (fillerColumn n ix) (fillerColumn 1 vals)
    | otherwise -> I.ColDictionary did (fillerColumn n ix) vals
  I.ColRunEndEncoded _ _ re vals
    | n == 0 -> I.ColRunEndEncoded 0 0 (fillerColumn 0 re) (empty vals)
    | otherwise -> I.ColRunEndEncoded 0 n (singleRunEnd re n) (fillerColumn 1 vals)
  where
    !n = max 0 n0
    empty = sliceColumnArray 0 0


-- | A one-element run-end column of the same tag holding @n@.
singleRunEnd :: ColumnArray -> Int -> ColumnArray
singleRunEnd re n = case re of
  I.ColPrim t _ _ | Just IntegralPrim <- integralPrim t -> I.ColPrim t Nothing (VS.singleton (fromIntegral n))
  _ -> re


-- ============================================================
-- Slicing
-- ============================================================

{- | Rows @[start, start + len)@, clamped to the column (a negative
start becomes 0, a length past the end is truncated). O(1) for flat
and offset columns (bit offsets move, offsets are not rebased, the
child is kept), O(fields) for structs and sparse unions (children are
sliced lazily), O(log runs) for run-end-encoded columns. Slicing a
validity recounts its nulls with a popcount.
-}
sliceColumnArray :: Int -> Int -> ColumnArray -> ColumnArray
sliceColumnArray !start0 !len0 col =
  let !n = columnLength col
      !start = min n (max 0 start0)
      !len = max 0 (min len0 (n - start))
  in if len == n && start == 0
       then col
       else sliceRows start len col


-- | Slice rows @[s, s + l)@; the caller has clamped the window.
sliceRows :: Int -> Int -> ColumnArray -> ColumnArray
sliceRows !s !l = \case
  I.ColNull _ -> I.ColNull l
  I.ColPrim t v xs -> withPrim t (I.ColPrim t (sv v) (VS.slice s l xs))
  I.ColBool v b -> I.ColBool (sv v) (sliceBitmap s l b)
  I.ColUtf8 v o d -> I.ColUtf8 (sv v) (VS.slice s (l + 1) o) d
  I.ColBinary v o d -> I.ColBinary (sv v) (VS.slice s (l + 1) o) d
  I.ColLargeUtf8 v o d -> I.ColLargeUtf8 (sv v) (VS.slice s (l + 1) o) d
  I.ColLargeBinary v o d -> I.ColLargeBinary (sv v) (VS.slice s (l + 1) o) d
  I.ColFixedSizeBinary w _ v d -> I.ColFixedSizeBinary w l (sv v) (BSU.unsafeDrop (s * w) d)
  I.ColUtf8View v views bufs -> I.ColUtf8View (sv v) (sliceViews views) bufs
  I.ColBinaryView v views bufs -> I.ColBinaryView (sv v) (sliceViews views) bufs
  I.ColStruct _ v cs -> I.ColStruct l (sv v) (V.map (fmap (sliceColumnArray s l)) cs)
  I.ColList v o c -> I.ColList (sv v) (VS.slice s (l + 1) o) c
  I.ColLargeList v o c -> I.ColLargeList (sv v) (VS.slice s (l + 1) o) c
  I.ColListView v o z c -> I.ColListView (sv v) (VS.slice s l o) (VS.slice s l z) c
  I.ColLargeListView v o z c -> I.ColLargeListView (sv v) (VS.slice s l o) (VS.slice s l z) c
  I.ColFixedSizeList w _ v c -> I.ColFixedSizeList w l (sv v) (sliceColumnArray (s * w) (l * w) c)
  I.ColMap v o k x -> I.ColMap (sv v) (VS.slice s (l + 1) o) k x
  I.ColDenseUnion t o cs -> I.ColDenseUnion (VS.slice s l t) (VS.slice s l o) cs
  I.ColSparseUnion t cs -> I.ColSparseUnion (VS.slice s l t) (V.map (sliceColumnArray s l) cs)
  I.ColDictionary did ix vals -> I.ColDictionary did (sliceColumnArray s l ix) vals
  I.ColRunEndEncoded off _ re vals
    | l == 0 -> I.ColRunEndEncoded 0 0 (sliceColumnArray 0 0 re) (sliceColumnArray 0 0 vals)
    | otherwise ->
        let !lo = off + s
            !i = physicalRun re lo
            !j = physicalRun re (lo + l - 1)
            !k = j - i + 1
        in I.ColRunEndEncoded lo l (sliceColumnArray i k re) (sliceColumnArray i k vals)
  where
    sv = sliceValidity s l
    sliceViews = BSU.unsafeTake (l * 16) . BSU.unsafeDrop (s * 16)


{- | Index of the run holding logical position @p@ (already offset):
the first run whose end is greater than @p@. Binary search.
-}
physicalRun :: ColumnArray -> Int -> Int
physicalRun re p = go 0 (columnLength re)
  where
    go !lo !hi
      | lo >= hi = lo
      | otherwise =
          let !mid = (lo + hi) `unsafeShiftR` 1
          in if keyAt re mid > p then go lo mid else go (mid + 1) hi


-- ============================================================
-- Detaching
-- ============================================================

{- | A copy that references only fresh memory: each buffer's logical
range is copied (offsets rebased to 0, bitmaps to bit offset 0), so
the result no longer keeps the decoded input alive. O(referenced bytes).
-}
copyColumn :: ColumnArray -> ColumnArray
copyColumn = \case
  I.ColNull n -> I.ColNull n
  I.ColPrim t v xs -> withPrim t (I.ColPrim t (copyValidity v) (copyStorable xs))
  I.ColBool v b -> I.ColBool (copyValidity v) (copyBitmap b)
  I.ColUtf8 v o d -> copyVar I.ColUtf8 v o d
  I.ColBinary v o d -> copyVar I.ColBinary v o d
  I.ColLargeUtf8 v o d -> copyVar I.ColLargeUtf8 v o d
  I.ColLargeBinary v o d -> copyVar I.ColLargeBinary v o d
  I.ColFixedSizeBinary w n v d -> I.ColFixedSizeBinary w n (copyValidity v) (freshBytes (BSU.unsafeTake (w * n) d))
  I.ColUtf8View v views bufs -> I.ColUtf8View (copyValidity v) (freshBytes views) (V.map freshBytes bufs)
  I.ColBinaryView v views bufs -> I.ColBinaryView (copyValidity v) (freshBytes views) (V.map freshBytes bufs)
  I.ColStruct n v cs -> I.ColStruct n (copyValidity v) (forceElems (V.map (\(nm, c) -> let !c' = copyColumn (sliceColumnArray 0 n c) in (nm, c')) cs))
  I.ColList v o c -> copyList I.ColList v o c
  I.ColLargeList v o c -> copyList I.ColLargeList v o c
  I.ColListView v o z c -> I.ColListView (copyValidity v) (copyStorable o) (copyStorable z) (copyColumn c)
  I.ColLargeListView v o z c -> I.ColLargeListView (copyValidity v) (copyStorable o) (copyStorable z) (copyColumn c)
  I.ColFixedSizeList w n v c -> I.ColFixedSizeList w n (copyValidity v) (copyColumn (sliceColumnArray 0 (w * n) c))
  I.ColMap v o k x ->
    let !(o', r) = rebaseToZero o
    in I.ColMap (copyValidity v) o' (copyColumn (sliceRange r k)) (copyColumn (sliceRange r x))
  I.ColDenseUnion t o cs -> I.ColDenseUnion (copyStorable t) (copyStorable o) (forceElems (V.map copyColumn cs))
  I.ColSparseUnion t cs -> I.ColSparseUnion (copyStorable t) (forceElems (V.map (copyColumn . sliceColumnArray 0 (VS.length t)) cs))
  I.ColDictionary did ix vals -> I.ColDictionary did (copyColumn ix) (copyColumn vals)
  I.ColRunEndEncoded off n re vals -> I.ColRunEndEncoded off n (copyColumn re) (copyColumn (sliceColumnArray 0 (columnLength re) vals))
  where
    copyVar :: Offset o => (Maybe Validity -> VS.Vector o -> ByteString -> ColumnArray) -> Maybe Validity -> VS.Vector o -> ByteString -> ColumnArray
    copyVar mk v o d =
      let !(o', ChildRange s l) = rebaseToZero o
      in mk (copyValidity v) o' (freshBytes (BSU.unsafeTake l (BSU.unsafeDrop s d)))
    copyList :: Offset o => (Maybe Validity -> VS.Vector o -> ColumnArray -> ColumnArray) -> Maybe Validity -> VS.Vector o -> ColumnArray -> ColumnArray
    copyList mk v o c =
      let !(o', r) = rebaseToZero o
      in mk (copyValidity v) o' (copyColumn (sliceRange r c))


sliceRange :: ChildRange -> ColumnArray -> ColumnArray
sliceRange (ChildRange s l) = sliceColumnArray s l


freshBytes :: ByteString -> ByteString
freshBytes bs = createAligned (BS.length bs) (copyBS bs)


-- ============================================================
-- Concatenation
-- ============================================================

-- | @concatColumnArrays [a, b]@.
concatColumnArray :: ColumnArray -> ColumnArray -> Either String ColumnArray
concatColumnArray a b = concatColumnArrays [a, b]


{- | Append columns of the same type (same tag, decimal parameters,
widths, struct field names, union arity, dictionary index type and
run-end type). Per buffer: one allocation of the summed size, a
memcpy per input, bit copies for bitmaps at any bit offset, and one
rebase pass per input for offsets. Var-length data and list children
keep only the referenced ranges. Dictionaries with identical values
(same buffers or logically equal) concatenate their keys; otherwise
the values are concatenated and later keys shifted, which must fit
the key width. Offsets that overflow their width are rejected.
-}
concatColumnArrays :: [ColumnArray] -> Either String ColumnArray
concatColumnArrays = \case
  [] -> Left "Arrow.Column.concatColumnArrays: no columns"
  [c] -> Right c
  cols@(c0 : _) -> case c0 of
    I.ColNull _ -> I.ColNull . sum <$> traverse (\case I.ColNull n -> Right n; c -> mismatch c) cols
    I.ColPrim t _ _ -> withPrim t $ do
      parts <- traverse (primPart t) cols
      Right (I.ColPrim t (concatValidity (map (\(v, xs) -> (VS.length xs, v)) parts)) (concatStorable (map snd parts)))
    I.ColBool {} -> do
      parts <- traverse (\case I.ColBool v b -> Right (v, b); c -> mismatch c) cols
      Right (I.ColBool (concatValidity (map (\(v, b) -> (bitmapLength b, v)) parts)) (concatBitmaps (map snd parts)))
    I.ColUtf8 {} -> traverse (\case I.ColUtf8 v o d -> Right (v, o, d); c -> mismatch c) cols >>= concatVar I.ColUtf8
    I.ColBinary {} -> traverse (\case I.ColBinary v o d -> Right (v, o, d); c -> mismatch c) cols >>= concatVar I.ColBinary
    I.ColLargeUtf8 {} -> traverse (\case I.ColLargeUtf8 v o d -> Right (v, o, d); c -> mismatch c) cols >>= concatVar I.ColLargeUtf8
    I.ColLargeBinary {} -> traverse (\case I.ColLargeBinary v o d -> Right (v, o, d); c -> mismatch c) cols >>= concatVar I.ColLargeBinary
    I.ColFixedSizeBinary w _ _ _ -> do
      parts <- traverse (\case I.ColFixedSizeBinary w' n v d | w' == w -> Right (n, v, d); c -> mismatch c) cols
      Right $
        I.ColFixedSizeBinary
          w
          (sum (map fst3 parts))
          (concatValidity (map (\(n, v, _) -> (n, v)) parts))
          (concatBytes (map (\(n, _, d) -> BSU.unsafeTake (n * w) d) parts))
    I.ColUtf8View {} -> traverse (\case I.ColUtf8View v vs bs -> Right (v, vs, bs); c -> mismatch c) cols >>= concatViews I.ColUtf8View
    I.ColBinaryView {} -> traverse (\case I.ColBinaryView v vs bs -> Right (v, vs, bs); c -> mismatch c) cols >>= concatViews I.ColBinaryView
    I.ColStruct _ _ cs0 -> do
      parts <- traverse (\case I.ColStruct n v cs | V.map fst cs == V.map fst cs0 -> Right (n, v, cs); c -> mismatch c) cols
      children <-
        V.generateM (V.length cs0) $ \j ->
          (,) (fst (V.unsafeIndex cs0 j))
            <$> concatColumnArrays (map (\(n, _, cs) -> sliceColumnArray 0 n (snd (V.unsafeIndex cs j))) parts)
      Right (I.ColStruct (sum (map fst3 parts)) (concatValidity (map (\(n, v, _) -> (n, v)) parts)) children)
    I.ColList {} -> traverse (\case I.ColList v o c -> Right (v, o, c); c -> mismatch c) cols >>= concatLists I.ColList
    I.ColLargeList {} -> traverse (\case I.ColLargeList v o c -> Right (v, o, c); c -> mismatch c) cols >>= concatLists I.ColLargeList
    I.ColListView {} -> traverse (\case I.ColListView v o z c -> Right (v, o, z, c); c -> mismatch c) cols >>= concatListViews I.ColListView
    I.ColLargeListView {} -> traverse (\case I.ColLargeListView v o z c -> Right (v, o, z, c); c -> mismatch c) cols >>= concatListViews I.ColLargeListView
    I.ColFixedSizeList w _ _ _ -> do
      parts <- traverse (\case I.ColFixedSizeList w' n v c | w' == w -> Right (n, v, c); c -> mismatch c) cols
      child <- concatColumnArrays (map (\(n, _, c) -> sliceColumnArray 0 (n * w) c) parts)
      Right (I.ColFixedSizeList w (sum (map fst3 parts)) (concatValidity (map (\(n, v, _) -> (n, v)) parts)) child)
    I.ColMap {} -> do
      parts <- traverse (\case I.ColMap v o k x -> Right (v, o, k, x); c -> mismatch c) cols
      (offs, ranges) <- concatOffsets (map (\(_, o, _, _) -> o) parts)
      keys <- concatColumnArrays (zipWith (\r (_, _, k, _) -> sliceRange r k) ranges parts)
      vals <- concatColumnArrays (zipWith (\r (_, _, _, x) -> sliceRange r x) ranges parts)
      Right (I.ColMap (concatValidity (map (\(v, o, _, _) -> (offsetRows o, v)) parts)) offs keys vals)
    I.ColDenseUnion _ _ cs0 -> do
      parts <- traverse (\case I.ColDenseUnion t o cs | V.length cs == V.length cs0 -> Right (t, o, cs); c -> mismatch c) cols
      let !k = V.length cs0
      children <- V.generateM k $ \j -> concatColumnArrays (map (\(_, _, cs) -> V.unsafeIndex cs j) parts)
      when (V.any (\c -> columnLength c > fromIntegral (maxBound :: Int32)) children) $
        Left "Arrow.Column.concatColumnArrays: dense union child exceeds 32-bit offsets"
      let bases = scanl (\acc (_, _, cs) -> zipWith (+) acc (map columnLength (V.toList cs))) (replicate k 0) parts
          shifted =
            zipWith
              ( \base (t, o, _) ->
                  let !bv = VS.fromList (map fromIntegral base) :: VS.Vector Int32
                  in VS.zipWith (\ti oi -> oi + VS.unsafeIndex bv (fromIntegral ti)) t o
              )
              bases
              parts
      Right (I.ColDenseUnion (concatStorable (map fst3 parts)) (concatStorable shifted) children)
    I.ColSparseUnion _ cs0 -> do
      parts <- traverse (\case I.ColSparseUnion t cs | V.length cs == V.length cs0 -> Right (t, cs); c -> mismatch c) cols
      children <-
        V.generateM (V.length cs0) $ \j ->
          concatColumnArrays (map (\(t, cs) -> sliceColumnArray 0 (VS.length t) (V.unsafeIndex cs j)) parts)
      Right (I.ColSparseUnion (concatStorable (map fst parts)) children)
    I.ColDictionary did ix0 vals0 -> do
      parts <- traverse (\case I.ColDictionary _ ix vals | sameShape ix ix0 -> Right (ix, vals); c -> mismatch c) cols
      if all (\(_, vals) -> identical vals vals0 || vals == vals0) parts
        then (\ix -> I.ColDictionary did ix vals0) <$> concatColumnArrays (map fst parts)
        else do
          vals <- concatColumnArrays (map snd parts)
          let bases = scanl (+) 0 (map (columnLength . snd) parts)
          shifted <- sequence (zipWith (\base (ix, _) -> shiftKeys base ix) bases parts)
          ix <- concatColumnArrays shifted
          Right (I.ColDictionary did ix vals)
    I.ColRunEndEncoded _ _ re0 _ -> do
      parts <- traverse (\case I.ColRunEndEncoded off n re vals | sameShape re re0 -> Right (off, n, re, vals); c -> mismatch c) cols
      concatRuns re0 parts
  where
    mismatch :: ColumnArray -> Either String b
    mismatch c = Left ("Arrow.Column.concatColumnArrays: incompatible columns " ++ columnTag c ++ " and the first column")


fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a


primPart :: PrimType a -> ColumnArray -> Either String (Maybe Validity, VS.Vector a)
primPart t = \case
  I.ColPrim t' v xs | samePrimType t t', Just Refl <- samePrimTag t t' -> Right (v, xs)
  c -> Left ("Arrow.Column.concatColumnArrays: incompatible columns " ++ columnTag c ++ " and Col" ++ primTypeName t)


-- | One aligned allocation holding the bytes in order.
concatBytes :: [ByteString] -> ByteString
concatBytes bss =
  createAligned (sum (map BS.length bss)) $ \dst ->
    let go !_ [] = pure ()
        go !off (b : rest) = copyBS b (dst `plusPtr` off) >> go (off + BS.length b) rest
    in go 0 bss


concatStorable :: Storable a => [VS.Vector a] -> VS.Vector a
concatStorable = unsafeBytesToStorable . concatBytes . map storableToBytes


{- | Offsets of consecutive list-like pieces as one offsets vector
starting at 0 (one rebase pass per piece), and each piece's child range.
-}
concatOffsets :: forall o. Offset o => [VS.Vector o] -> Either String (VS.Vector o, [ChildRange])
concatOffsets offss
  | toInteger total > toInteger (maxBound :: o) = Left "Arrow.Column.concatColumnArrays: offsets overflow their width"
  | otherwise = Right (unsafeBytesToStorable bytes, ranges)
  where
    ranges = map (\o -> ChildRange (fromIntegral (VS.unsafeHead o)) (fromIntegral (VS.unsafeLast o - VS.unsafeHead o))) offss
    !total = sum (map childLength ranges)
    !rows = sum (map offsetRows offss)
    !w = sizeOf (0 :: o)
    bytes = createAligned ((rows + 1) * w) $ \dst -> do
      pokeElemOff (castPtr dst) 0 (0 :: o)
      let go !_ !_ [] = pure ()
          go !pos !base (o : rest) = do
            let !m = offsetRows o
                !o0 = VS.unsafeHead o
            VS.unsafeWith o $ \src ->
              cRebaseOffsets (castPtr (dst `plusPtr` ((pos + 1) * w)) :: Ptr o) (src `plusPtr` w) m (fromIntegral base - fromIntegral o0)
            go (pos + m) (base + fromIntegral (VS.unsafeLast o - o0) :: Int) rest
      go 0 0 offss


concatVar :: Offset o => (Maybe Validity -> VS.Vector o -> ByteString -> ColumnArray) -> [(Maybe Validity, VS.Vector o, ByteString)] -> Either String ColumnArray
concatVar mk parts = do
  (offs, ranges) <- concatOffsets (map (\(_, o, _) -> o) parts)
  let dat = concatBytes (zipWith (\(ChildRange s l) (_, _, d) -> BSU.unsafeTake l (BSU.unsafeDrop s d)) ranges parts)
  Right (mk (concatValidity (map (\(v, o, _) -> (offsetRows o, v)) parts)) offs dat)


concatLists :: Offset o => (Maybe Validity -> VS.Vector o -> ColumnArray -> ColumnArray) -> [(Maybe Validity, VS.Vector o, ColumnArray)] -> Either String ColumnArray
concatLists mk parts = do
  (offs, ranges) <- concatOffsets (map (\(_, o, _) -> o) parts)
  child <- concatColumnArrays (zipWith (\r (_, _, c) -> sliceRange r c) ranges parts)
  Right (mk (concatValidity (map (\(v, o, _) -> (offsetRows o, v)) parts)) offs child)


concatListViews
  :: forall o
   . Offset o
  => (Maybe Validity -> VS.Vector o -> VS.Vector o -> ColumnArray -> ColumnArray)
  -> [(Maybe Validity, VS.Vector o, VS.Vector o, ColumnArray)]
  -> Either String ColumnArray
concatListViews mk parts = do
  child <- concatColumnArrays (map (\(_, _, _, c) -> c) parts)
  when (toInteger (columnLength child) > toInteger (maxBound :: o)) $
    Left "Arrow.Column.concatColumnArrays: list-view child exceeds the offset width"
  let bases = scanl (+) 0 (map (\(_, _, _, c) -> columnLength c) parts)
      shifted = zipWith (\base (_, o, _, _) -> VS.map (+ fromIntegral base) o) bases parts
  Right $
    mk
      (concatValidity (map (\(v, o, _, _) -> (VS.length o, v)) parts))
      (concatStorable shifted)
      (concatStorable (map (\(_, _, z, _) -> z) parts))
      child


-- | Views of later pieces point past the variadic buffers of earlier pieces.
concatViews :: (Maybe Validity -> ByteString -> V.Vector ByteString -> ColumnArray) -> [(Maybe Validity, ByteString, V.Vector ByteString)] -> Either String ColumnArray
concatViews mk parts =
  let bases = scanl (+) 0 (map (\(_, _, bs) -> V.length bs) parts)
      rows = map (\(_, vs, _) -> BS.length vs `quot` 16) parts
      views = createAligned (16 * sum rows) $ \dst ->
        let go !_ [] = pure ()
            go !pos ((base, (_, vs, _)) : rest) = do
              let !m = BS.length vs `quot` 16
              copyBS (BSU.unsafeTake (m * 16) vs) (dst `plusPtr` (pos * 16))
              when (base /= 0) $ forM_ [0 .. m - 1] $ \i -> do
                let !p = dst `plusPtr` ((pos + i) * 16)
                len <- peekByteOff p 0 :: IO Int32
                when (len > 12) $ do
                  bi <- peekByteOff p 8 :: IO Int32
                  pokeByteOff p 8 (bi + fromIntegral base)
              go (pos + m) rest
        in go 0 (zip bases parts)
  in Right (mk (concatValidity (zip rows (map (\(v, _, _) -> v) parts))) views (V.concat (map (\(_, _, bs) -> bs) parts)))


-- | Add @base@ to every valid key (null slots become 0); 'Left' if a key would overflow its width.
shiftKeys :: Int -> ColumnArray -> Either String ColumnArray
shiftKeys base col
  | base == 0 = Right col
  | otherwise = case col of
      I.ColPrim t v xs | Just IntegralPrim <- integralPrim t ->
        let ok i x = not (unsafeIsValidAt v i) || toInteger x + toInteger base <= toInteger (maxBound `asTypeOf` x)
        in if VS.and (VS.imap ok xs)
             then Right (I.ColPrim t v (VS.imap (\i x -> if unsafeIsValidAt v i then x + fromIntegral base else 0) xs))
             else Left "Arrow.Column.concatColumnArrays: combined dictionary does not fit the key width"
      _ -> Left "Arrow.Column.concatColumnArrays: dictionary indices must be an integer column"


-- | Run ends of a run-end column as a list of Ints.
runEndList :: ColumnArray -> [Int]
runEndList re = map (keyAt re) [0 .. columnLength re - 1]


{- | The IPC layout of a run-end-encoded column: logical offset 0 and
run ends rebased so the last one is exactly the length (one pass over
the runs in the window). Any other column is returned unchanged.
-}
rebaseRunEnds :: ColumnArray -> Either String ColumnArray
rebaseRunEnds col = case col of
  I.ColRunEndEncoded off n re vals
    | off == 0 && (if runs == 0 then n == 0 else keyAt re (runs - 1) == n) && columnLength vals == runs -> Right col
    | otherwise -> concatRuns re [(off, n, re, vals)]
    where
      !runs = columnLength re
  _ -> Right col


{- | Concatenate run-end-encoded pieces: each piece's runs are cut to
its logical window, rebased, and shifted past the earlier pieces.
-}
concatRuns :: ColumnArray -> [(Int, Int, ColumnArray, ColumnArray)] -> Either String ColumnArray
concatRuns re0 parts = case re0 of
  I.ColPrim t _ _ | Just IntegralPrim <- integralPrim t -> do
    let windows =
          map
            ( \(off, n, re, vals) ->
                if n == 0
                  then ([], sliceColumnArray 0 0 vals)
                  else
                    let !i = physicalRun re off
                        !j = physicalRun re (off + n - 1)
                        cut = map (\e -> min e (off + n) - off) (take (j - i + 1) (drop i (runEndList re)))
                    in (cut, sliceColumnArray i (j - i + 1) vals)
            )
            parts
        lens = map (\(_, n, _, _) -> n) parts
        bases = scanl (+) 0 lens
        ends = concat (zipWith (\b (es, _) -> map (+ b) es) bases windows)
        !total = sum lens
    when (toInteger total > toInteger (maxBound `asTypeOf` VS.head (vecOf t re0))) $
      Left "Arrow.Column.concatColumnArrays: run end overflows its integer width"
    vals <- concatColumnArrays (map snd windows)
    Right (I.ColRunEndEncoded 0 total (I.ColPrim t Nothing (VS.fromList (map fromIntegral ends))) vals)
  _ -> Left "Arrow.Column.concatColumnArrays: run ends must be an integer column"
  where
    vecOf :: PrimType a -> ColumnArray -> VS.Vector a
    vecOf t c = case c of
      I.ColPrim t' _ xs | Just Refl <- samePrimTag t t' -> xs
      _ -> withPrim t VS.empty


-- ============================================================
-- Gathering
-- ============================================================

{- | Rows by index (repeats allowed); any index outside the column is
a 'Left'. Fixed-width buffers and bitmaps go through C gather
kernels; var-length columns take two passes (offsets, then bytes);
lists gather their child ranges; dictionaries, dense unions and list
views gather only their own buffers and share the rest.
-}
takeColumnArray :: VS.Vector Int -> ColumnArray -> Either String ColumnArray
takeColumnArray ix col
  | VS.any (\i -> i < 0 || i >= n) ix =
      Left ("Arrow.Column.takeColumnArray: index out of range for a column of " ++ show n ++ " rows")
  | otherwise = takeRows ix col
  where
    !n = columnLength col


-- | 'takeColumnArray' after the range check.
takeRows :: VS.Vector Int -> ColumnArray -> Either String ColumnArray
takeRows ix col = case col of
  I.ColNull _ -> Right (I.ColNull k)
  I.ColPrim t v xs -> withPrim t (Right (I.ColPrim t (tv v) (gatherStorable ix xs)))
  I.ColBool v b -> Right (I.ColBool (tv v) (fst (takeBitmap ix b)))
  I.ColUtf8 v o d -> (\(o', d') -> I.ColUtf8 (tv v) o' d') <$> takeVar ix o d
  I.ColBinary v o d -> (\(o', d') -> I.ColBinary (tv v) o' d') <$> takeVar ix o d
  I.ColLargeUtf8 v o d -> (\(o', d') -> I.ColLargeUtf8 (tv v) o' d') <$> takeVar ix o d
  I.ColLargeBinary v o d -> (\(o', d') -> I.ColLargeBinary (tv v) o' d') <$> takeVar ix o d
  I.ColFixedSizeBinary w _ v d -> Right (I.ColFixedSizeBinary w k (tv v) (gatherBytes w ix d))
  I.ColUtf8View v views bufs -> Right (I.ColUtf8View (tv v) (gatherBytes 16 ix views) bufs)
  I.ColBinaryView v views bufs -> Right (I.ColBinaryView (tv v) (gatherBytes 16 ix views) bufs)
  I.ColStruct _ v cs -> I.ColStruct k (tv v) <$> V.mapM (traverse (takeRows ix)) cs
  I.ColList v o c -> takeList I.ColList v o c
  I.ColLargeList v o c -> takeList I.ColLargeList v o c
  I.ColListView v o z c -> Right (I.ColListView (tv v) (gatherStorable ix o) (gatherStorable ix z) c)
  I.ColLargeListView v o z c -> Right (I.ColLargeListView (tv v) (gatherStorable ix o) (gatherStorable ix z) c)
  I.ColFixedSizeList w _ v c ->
    I.ColFixedSizeList w k (tv v) <$> takeRows (VS.generate (k * w) (\j -> VS.unsafeIndex ix (j `quot` w) * w + j `rem` w)) c
  I.ColMap v o keys vals -> do
    (o', childIx) <- takeOffsetRanges ix o
    I.ColMap (tv v) o' <$> takeRows childIx keys <*> takeRows childIx vals
  I.ColDenseUnion t o cs -> Right (I.ColDenseUnion (gatherStorable ix t) (gatherStorable ix o) cs)
  I.ColSparseUnion t cs -> I.ColSparseUnion (gatherStorable ix t) <$> V.mapM (takeRows ix) cs
  I.ColDictionary did keys vals -> (\keys' -> I.ColDictionary did keys' vals) <$> takeRows ix keys
  I.ColRunEndEncoded off _ re vals -> takeRuns ix off re vals
  where
    !k = VS.length ix
    tv = takeValidity ix
    takeList :: Offset o => (Maybe Validity -> VS.Vector o -> ColumnArray -> ColumnArray) -> Maybe Validity -> VS.Vector o -> ColumnArray -> Either String ColumnArray
    takeList mk v o c = do
      (o', childIx) <- takeOffsetRanges ix o
      mk (tv v) o' <$> takeRows childIx c


gatherStorable :: forall a. Storable a => VS.Vector Int -> VS.Vector a -> VS.Vector a
gatherStorable ix xs = unsafeBytesToStorable (gatherBytes (sizeOf (undefined :: a)) ix (storableToBytes xs))


-- | Gather @w@-byte elements by index into one fresh buffer.
gatherBytes :: Int -> VS.Vector Int -> ByteString -> ByteString
gatherBytes w ix src =
  createAligned (n * w) $ \dst -> withBytesPtr src $ \ps -> VS.unsafeWith ix $ \pix -> case w of
    1 -> K.gather1 dst ps pix n
    2 -> K.gather2 dst ps pix n
    4 -> K.gather4 dst ps pix n
    8 -> K.gather8 dst ps pix n
    16 -> K.gather16 dst ps pix n
    _ -> forM_ [0 .. n - 1] $ \j -> copyBytes (dst `plusPtr` (j * w)) (ps `plusPtr` (VS.unsafeIndex ix j * w)) w
  where
    !n = VS.length ix


takeVar :: forall o. Offset o => VS.Vector Int -> VS.Vector o -> ByteString -> Either String (VS.Vector o, ByteString)
takeVar ix offs dat = unsafeDupablePerformIO $ do
  let !n = VS.length ix
  ofp <- mallocAligned ((n + 1) * sizeOf (0 :: o))
  total <- unsafeWithForeignPtr ofp $ \po -> VS.unsafeWith offs $ \ps -> VS.unsafeWith ix $ \pix ->
    cTakeOffsets (castPtr po) ps pix n
  if total < 0
    then pure (Left "Arrow.Column.takeColumnArray: gathered data exceeds the offset width")
    else do
      let bytes = createAligned total $ \dst -> withBytesPtr dat $ \pd -> VS.unsafeWith offs $ \ps -> VS.unsafeWith ix $ \pix ->
            cTakeBytes dst ps pd pix n
      pure (Right (VS.unsafeFromForeignPtr0 (castForeignPtr ofp) (n + 1), bytes))


-- | New offsets (from 0) for the selected rows, and the child indices they cover.
takeOffsetRanges :: forall o. Offset o => VS.Vector Int -> VS.Vector o -> Either String (VS.Vector o, VS.Vector Int)
takeOffsetRanges ix offs = unsafeDupablePerformIO $ do
  let !n = VS.length ix
  ofp <- mallocAligned ((n + 1) * sizeOf (0 :: o))
  total <- unsafeWithForeignPtr ofp $ \po -> VS.unsafeWith offs $ \ps -> VS.unsafeWith ix $ \pix ->
    cTakeOffsets (castPtr po) ps pix n
  if total < 0
    then pure (Left "Arrow.Column.takeColumnArray: gathered child exceeds the offset width")
    else do
      let childIx = VS.create $ do
            mv <- VSM.unsafeNew total
            let go !j !pos
                  | j >= n = pure ()
                  | otherwise = do
                      let !r = VS.unsafeIndex ix j
                          !s = fromIntegral (VS.unsafeIndex offs r) :: Int
                          !e = fromIntegral (VS.unsafeIndex offs (r + 1)) :: Int
                      forM_ [0 .. e - s - 1] $ \q -> VSM.unsafeWrite mv (pos + q) (s + q)
                      go (j + 1) (pos + e - s)
            go 0 0
            pure mv
      pure (Right (VS.unsafeFromForeignPtr0 (castForeignPtr ofp) (n + 1), childIx))


-- | Gather a run-end-encoded column: one run per change of physical run.
takeRuns :: VS.Vector Int -> Int -> ColumnArray -> ColumnArray -> Either String ColumnArray
takeRuns ix off re vals = case re of
  I.ColPrim t _ xs | Just IntegralPrim <- integralPrim t -> do
    let phys = map (\i -> physicalRun re (off + i)) (VS.toList ix)
        grouped = groupRuns phys
        ends = scanl1 (+) (map snd grouped)
        !k = VS.length ix
    when (toInteger k > toInteger (maxBound `asTypeOf` VS.head xs)) $
      Left "Arrow.Column.takeColumnArray: run end overflows its integer width"
    vals' <- takeRows (VS.fromList (map fst grouped)) vals
    Right (I.ColRunEndEncoded 0 k (I.ColPrim t Nothing (VS.fromList (map fromIntegral ends))) vals')
  _ -> Left "Arrow.Column.takeColumnArray: run ends must be an integer column"
  where
    groupRuns :: [Int] -> [(Int, Int)]
    groupRuns = foldr step []
    step p ((q, c) : rest) | p == q = (q, c + 1) : rest
    step p acc = (p, 1) : acc


-- ============================================================
-- Dictionaries
-- ============================================================

{- | Replace a dictionary column by the values its keys select, with
the key validity applied; any other column is returned unchanged. A
column whose rows are all null may reference an empty dictionary; it
expands to all-null rows of the value type.
-}
expandDictionary :: ColumnArray -> Either String ColumnArray
expandDictionary = \case
  I.ColDictionary _ keys vals -> do
    let !n = columnLength keys
        !kv = validity keys
    dense <-
      if columnLength vals == 0
        then
          if nullCount keys == n
            then Right (fillerColumn n vals)
            else Left "Arrow.Column.expandDictionary: dictionary key out of range for an empty dictionary"
        else takeColumnArray (VS.generate n (\i -> if unsafeIsValidAt kv i then keyAt keys i else 0)) vals
    maskValidity kv dense
  col -> Right col


{- | Replace the values of every dictionary column (at any depth) with
the dictionary registered for its id, checking every valid key against
it (C kernel). A column with no valid key keeps its placeholder when
the id is unknown; otherwise an unknown id is a 'Left'. Values the
lookup returns are used as they are (dictionaries nested in them must
already be resolved).
-}
resolveDictionaryColumn :: (Int64 -> Maybe ColumnArray) -> ColumnArray -> Either String ColumnArray
resolveDictionaryColumn lookupVals = go
  where
    go col = case col of
      I.ColDictionary did keys _ -> case lookupVals did of
        Nothing
          | nullCount keys == columnLength keys -> Right col
          | otherwise -> Left ("Arrow.Column: no dictionary batch for dictionary id " ++ show did)
        Just vals -> do
          validateKeys ("Arrow.Column: dictionary id " ++ show did) keys (columnLength vals)
          Right (I.ColDictionary did keys vals)
      I.ColStruct n v cs -> I.ColStruct n v <$> V.mapM (traverse go) cs
      I.ColList v o c -> I.ColList v o <$> go c
      I.ColLargeList v o c -> I.ColLargeList v o <$> go c
      I.ColListView v o z c -> I.ColListView v o z <$> go c
      I.ColLargeListView v o z c -> I.ColLargeListView v o z <$> go c
      I.ColFixedSizeList w n v c -> I.ColFixedSizeList w n v <$> go c
      I.ColMap v o k x -> I.ColMap v o <$> go k <*> go x
      I.ColDenseUnion t o cs -> I.ColDenseUnion t o <$> V.mapM go cs
      I.ColSparseUnion t cs -> I.ColSparseUnion t <$> V.mapM go cs
      I.ColRunEndEncoded off n re vals -> I.ColRunEndEncoded off n re <$> go vals
      _ -> Right col


-- ============================================================
-- Nullability
-- ============================================================

{- | Null out the rows whose mask bit is clear (rows already null stay
null). The mask must have the column's length. Columns without a
validity slot (unions, run-end-encoded) accept only 'Nothing'; a
'ColNull' is unchanged.
-}
maskValidity :: Maybe Validity -> ColumnArray -> Either String ColumnArray
maskValidity Nothing col = Right col
maskValidity m@(Just mv) col
  | bitmapLength (validityBits mv) /= columnLength col =
      Left "Arrow.Column.maskValidity: mask length differs from the column length"
  | otherwise = case col of
      I.ColNull _ -> Right col
      I.ColPrim t v xs -> Right (I.ColPrim t (a v) xs)
      I.ColBool v b -> Right (I.ColBool (a v) b)
      I.ColUtf8 v o d -> Right (I.ColUtf8 (a v) o d)
      I.ColBinary v o d -> Right (I.ColBinary (a v) o d)
      I.ColLargeUtf8 v o d -> Right (I.ColLargeUtf8 (a v) o d)
      I.ColLargeBinary v o d -> Right (I.ColLargeBinary (a v) o d)
      I.ColFixedSizeBinary w n v d -> Right (I.ColFixedSizeBinary w n (a v) d)
      I.ColUtf8View v views bufs -> Right (I.ColUtf8View (a v) views bufs)
      I.ColBinaryView v views bufs -> Right (I.ColBinaryView (a v) views bufs)
      I.ColStruct n v cs -> Right (I.ColStruct n (a v) cs)
      I.ColList v o c -> Right (I.ColList (a v) o c)
      I.ColLargeList v o c -> Right (I.ColLargeList (a v) o c)
      I.ColListView v o z c -> Right (I.ColListView (a v) o z c)
      I.ColLargeListView v o z c -> Right (I.ColLargeListView (a v) o z c)
      I.ColFixedSizeList w n v c -> Right (I.ColFixedSizeList w n (a v) c)
      I.ColMap v o k x -> Right (I.ColMap (a v) o k x)
      I.ColDictionary did keys vals -> (\keys' -> I.ColDictionary did keys' vals) <$> maskValidity m keys
      I.ColDenseUnion {} -> noSlot
      I.ColSparseUnion {} -> noSlot
      I.ColRunEndEncoded {} -> noSlot
  where
    a = andValidity m
    noSlot = Left ("Arrow.Column.maskValidity: " ++ columnTag col ++ " has no validity bitmap")


{- | Every column with a validity slot can hold nulls, so this is the
identity on those (and on 'ColNull'); unions and run-end-encoded
columns are rejected because they have no validity of their own.
-}
toNullableColumn :: ColumnArray -> Either String ColumnArray
toNullableColumn col = case col of
  I.ColDenseUnion {} -> noSlot
  I.ColSparseUnion {} -> noSlot
  I.ColRunEndEncoded {} -> noSlot
  _ -> Right col
  where
    noSlot = Left ("Arrow.Column: " ++ columnTag col ++ " has no validity bitmap and cannot be made nullable")


-- ============================================================
-- Map invariants
-- ============================================================

{- | Check that a 'ColMap' column satisfies the @keysSorted@ promise of
its 'AMap' field: within every non-null entry the keys are non-null
and non-decreasing.

Keys compare by value: integers, dates, times, timestamps and
durations numerically; floating point (half floats included)
numerically with @-0 == 0@ and every NaN equal to every other NaN
and greater than any number; booleans with @False < True@; strings by
code point; binary (fixed-size and views included) as unsigned bytes;
decimals as signed integers; dictionary keys by their resolved
values. Interval, nested, union, run-end-encoded and null key columns
have no order and are rejected, as is any column that is not a map.
-}
validateMapKeysSorted :: ColumnArray -> Either String ()
validateMapKeysSorted = \case
  I.ColMap v offs keys _ -> do
    (cmp, isNull) <- keyOrder keys
    let !nk = columnLength keys
        !nEntries = offsetRows offs
        entry !i
          | i >= nEntries = Right ()
          | otherwise = do
              let !s = fromIntegral (VS.unsafeIndex offs i) :: Int
                  !e = fromIntegral (VS.unsafeIndex offs (i + 1)) :: Int
              if s < 0 || e < s || e > nk
                then Left ("Arrow.Column.validateMapKeysSorted: entry " ++ show i ++ " has offsets outside the key column")
                else
                  if unsafeIsValidAt v i
                    then keysIn i s e s >> entry (i + 1)
                    else entry (i + 1)
        keysIn i s e !j
          | j >= e = Right ()
          | isNull j = Left ("Arrow.Column.validateMapKeysSorted: entry " ++ show i ++ " has a null key")
          | j > s && cmp (j - 1) j == GT =
              Left ("Arrow.Column.validateMapKeysSorted: entry " ++ show i ++ " key " ++ show (j - s) ++ " is smaller than the key before it")
          | otherwise = keysIn i s e (j + 1)
    entry 0
  c -> Left ("Arrow.Column.validateMapKeysSorted: expected a map column, got " ++ columnTag c)


-- | Row comparator and null test for an orderable key column.
keyOrder :: ColumnArray -> Either String (Int -> Int -> Ordering, Int -> Bool)
keyOrder col = case col of
  I.ColPrim t v xs -> case t of
    PInt8 -> ordered v xs
    PInt16 -> ordered v xs
    PInt32 -> ordered v xs
    PInt64 -> ordered v xs
    PUInt8 -> ordered v xs
    PUInt16 -> ordered v xs
    PUInt32 -> ordered v xs
    PUInt64 -> ordered v xs
    PDate32 -> ordered v xs
    PDate64 -> ordered v xs
    PTime32 -> ordered v xs
    PTime64 -> ordered v xs
    PTimestamp -> ordered v xs
    PDuration -> ordered v xs
    PDecimal128 _ _ -> ordered v xs
    PDecimal256 _ _ -> ordered v xs
    PFloat16 -> by v (\i j -> compareFloating (float16ToDouble (VS.unsafeIndex xs i)) (float16ToDouble (VS.unsafeIndex xs j)))
    PFloat -> by v (\i j -> compareFloating (VS.unsafeIndex xs i) (VS.unsafeIndex xs j))
    PDouble -> by v (\i j -> compareFloating (VS.unsafeIndex xs i) (VS.unsafeIndex xs j))
    _ -> unordered
  I.ColBool v b -> by v (\i j -> compare (unsafeBitAt b i) (unsafeBitAt b j))
  I.ColUtf8 {} -> bytes
  I.ColBinary {} -> bytes
  I.ColLargeUtf8 {} -> bytes
  I.ColLargeBinary {} -> bytes
  I.ColUtf8View {} -> bytes
  I.ColBinaryView {} -> bytes
  I.ColFixedSizeBinary {} -> bytes
  I.ColDictionary _ keys vals -> do
    (cmp, isNull) <- keyOrder vals
    let !nv = columnLength vals
        kv = validity keys
    if all (\i -> not (unsafeIsValidAt kv i) || (keyAt keys i >= 0 && keyAt keys i < nv)) [0 .. columnLength keys - 1]
      then
        Right
          ( \i j -> cmp (keyAt keys i) (keyAt keys j)
          , \i -> not (unsafeIsValidAt kv i) || isNull (keyAt keys i)
          )
      else Left "Arrow.Column.validateMapKeysSorted: dictionary key index outside its dictionary (is the dictionary resolved?)"
  _ -> unordered
  where
    unordered = Left ("Arrow.Column.validateMapKeysSorted: map keys of type " ++ columnTag col ++ " have no defined order")
    v0 = validity col
    by v cmp = Right (cmp, \i -> not (unsafeIsValidAt v i))
    ordered :: (Storable a, Ord a) => Maybe Validity -> VS.Vector a -> Either String (Int -> Int -> Ordering, Int -> Bool)
    ordered v xs = by v (\i j -> compare (VS.unsafeIndex xs i) (VS.unsafeIndex xs j))
    bytes = by v0 (\i j -> compare (rowBytes col i) (rowBytes col j))


-- | Bytes of row @i@ of a byte-like column, ignoring validity.
rowBytes :: ColumnArray -> Int -> ByteString
rowBytes c i = case c of
  I.ColUtf8 _ o d -> varSlice o d i
  I.ColBinary _ o d -> varSlice o d i
  I.ColLargeUtf8 _ o d -> varSlice o d i
  I.ColLargeBinary _ o d -> varSlice o d i
  I.ColUtf8View _ views bufs -> viewAt views bufs i
  I.ColBinaryView _ views bufs -> viewAt views bufs i
  I.ColFixedSizeBinary w _ _ d -> BSU.unsafeTake w (BSU.unsafeDrop (i * w) d)
  _ -> BS.empty


-- | Numeric order with @-0 == 0@; NaNs equal to each other and above every number.
compareFloating :: RealFloat a => a -> a -> Ordering
compareFloating x y = case (isNaN x, isNaN y) of
  (True, True) -> EQ
  (True, False) -> GT
  (False, True) -> LT
  (False, False) -> compare x y


-- ============================================================
-- Eq, Show, NFData
-- ============================================================

instance NFData ColumnArray where
  rnf = (`seq` ())


-- | Logical, O(n); see the module header.
instance Eq ColumnArray where
  a == b =
    sameShape a b
      && columnLength a == columnLength b
      && (identical a b || rowsEqual a b)


-- | Same type: tag, parameters, widths, field names, arity, recursively.
sameShape :: ColumnArray -> ColumnArray -> Bool
sameShape a b = case (a, b) of
  (I.ColNull _, I.ColNull _) -> True
  (I.ColPrim t _ _, I.ColPrim t' _ _) -> samePrimType t t'
  (I.ColBool {}, I.ColBool {}) -> True
  (I.ColUtf8 {}, I.ColUtf8 {}) -> True
  (I.ColBinary {}, I.ColBinary {}) -> True
  (I.ColLargeUtf8 {}, I.ColLargeUtf8 {}) -> True
  (I.ColLargeBinary {}, I.ColLargeBinary {}) -> True
  (I.ColFixedSizeBinary w _ _ _, I.ColFixedSizeBinary w' _ _ _) -> w == w'
  (I.ColUtf8View {}, I.ColUtf8View {}) -> True
  (I.ColBinaryView {}, I.ColBinaryView {}) -> True
  (I.ColStruct _ _ cs, I.ColStruct _ _ cs') ->
    V.length cs == V.length cs' && V.and (V.zipWith (\(n, x) (n', y) -> n == n' && sameShape x y) cs cs')
  (I.ColList _ _ c, I.ColList _ _ c') -> sameShape c c'
  (I.ColLargeList _ _ c, I.ColLargeList _ _ c') -> sameShape c c'
  (I.ColListView _ _ _ c, I.ColListView _ _ _ c') -> sameShape c c'
  (I.ColLargeListView _ _ _ c, I.ColLargeListView _ _ _ c') -> sameShape c c'
  (I.ColFixedSizeList w _ _ c, I.ColFixedSizeList w' _ _ c') -> w == w' && sameShape c c'
  (I.ColMap _ _ k x, I.ColMap _ _ k' x') -> sameShape k k' && sameShape x x'
  (I.ColDenseUnion _ _ cs, I.ColDenseUnion _ _ cs') -> sameChildren cs cs'
  (I.ColSparseUnion _ cs, I.ColSparseUnion _ cs') -> sameChildren cs cs'
  (I.ColDictionary did k x, I.ColDictionary did' k' x') -> did == did' && sameShape k k' && sameShape x x'
  (I.ColRunEndEncoded _ _ r x, I.ColRunEndEncoded _ _ r' x') -> sameShape r r' && sameShape x x'
  _ -> False
  where
    sameChildren cs cs' = V.length cs == V.length cs' && V.and (V.zipWith sameShape cs cs')


-- | Buffer identity of flat columns: same memory, same window.
identical :: ColumnArray -> ColumnArray -> Bool
identical a b = case (a, b) of
  (I.ColPrim t v xs, I.ColPrim t' v' ys) ->
    samePrimType t t' && sameV v v' && withPrim t (withPrim t' (sameBS (storableToBytes xs) (storableToBytes ys)))
  (I.ColBool v x, I.ColBool v' y) -> sameV v v' && sameBitmap x y
  (I.ColUtf8 v o d, I.ColUtf8 v' o' d') -> sameV v v' && sameBS (storableToBytes o) (storableToBytes o') && sameBS d d'
  (I.ColBinary v o d, I.ColBinary v' o' d') -> sameV v v' && sameBS (storableToBytes o) (storableToBytes o') && sameBS d d'
  (I.ColLargeUtf8 v o d, I.ColLargeUtf8 v' o' d') -> sameV v v' && sameBS (storableToBytes o) (storableToBytes o') && sameBS d d'
  (I.ColLargeBinary v o d, I.ColLargeBinary v' o' d') -> sameV v v' && sameBS (storableToBytes o) (storableToBytes o') && sameBS d d'
  _ -> False
  where
    sameBS (BSI.BS p l) (BSI.BS p' l') = p == p' && l == l'
    sameBitmap (Bitmap x o l) (Bitmap y o' l') = sameBS x y && o == o' && l == l'
    sameV Nothing Nothing = True
    sameV (Just (Validity x _)) (Just (Validity y _)) = sameBitmap x y
    sameV _ _ = False


-- | Every row equal (shapes and lengths already agree).
rowsEqual :: ColumnArray -> ColumnArray -> Bool
rowsEqual a b = case (a, b) of
  (I.ColPrim t Nothing xs, I.ColPrim t' Nothing ys) ->
    withPrim t (withPrim t' (storableToBytes xs == storableToBytes ys))
  _ -> all (\i -> rowEq a i b i) [0 .. columnLength a - 1]


-- | Whether row @i@ is logically valid (unions and run-end columns defer to their child rows).
rowValid :: ColumnArray -> Int -> Bool
rowValid c i = case c of
  I.ColNull _ -> False
  -- A valid key that selects a null value is a null row (arrow-rs logical nulls).
  I.ColDictionary _ k x ->
    unsafeIsValidAt (validity k) i
      && let !ki = keyAt k i in ki < 0 || ki >= columnLength x || rowValid x ki
  _ -> unsafeIsValidAt (validity c) i


-- | Row @i@ of @a@ equals row @j@ of @b@ (same shape).
rowEq :: ColumnArray -> Int -> ColumnArray -> Int -> Bool
rowEq a i b j =
  let !va = rowValid a i
      !vb = rowValid b j
  in if va /= vb then False else not va || valueEq a i b j


valueEq :: ColumnArray -> Int -> ColumnArray -> Int -> Bool
valueEq a i b j = case (a, b) of
  (I.ColPrim t _ xs, I.ColPrim t' _ ys) ->
    let !w = primWidth t
    in withPrim t (withPrim t' (slot w (storableToBytes xs) i == slot w (storableToBytes ys) j))
  (I.ColBool _ x, I.ColBool _ y) -> unsafeBitAt x i == unsafeBitAt y j
  (I.ColStruct _ _ cs, I.ColStruct _ _ cs') -> V.and (V.zipWith (\(_, x) (_, y) -> rowEq x i y j) cs cs')
  (I.ColList _ o c, I.ColList _ o' c') -> rangesEq c (offsetRange o i) c' (offsetRange o' j)
  (I.ColLargeList _ o c, I.ColLargeList _ o' c') -> rangesEq c (offsetRange o i) c' (offsetRange o' j)
  (I.ColListView {}, I.ColListView {}) -> viaRanges
  (I.ColLargeListView {}, I.ColLargeListView {}) -> viaRanges
  (I.ColFixedSizeList w _ _ c, I.ColFixedSizeList _ _ _ c') -> rangesEq c (ChildRange (i * w) w) c' (ChildRange (j * w) w)
  (I.ColMap _ o k x, I.ColMap _ o' k' x') ->
    let r = offsetRange o i
        r' = offsetRange o' j
    in rangesEq k r k' r' && rangesEq x r x' r'
  (I.ColDenseUnion t o cs, I.ColDenseUnion t' o' cs') ->
    let !ci = fromIntegral (VS.unsafeIndex t i)
    in ci == (fromIntegral (VS.unsafeIndex t' j) :: Int)
         && rowEq (V.unsafeIndex cs ci) (fromIntegral (VS.unsafeIndex o i)) (V.unsafeIndex cs' ci) (fromIntegral (VS.unsafeIndex o' j))
  (I.ColSparseUnion t cs, I.ColSparseUnion t' cs') ->
    let !ci = fromIntegral (VS.unsafeIndex t i)
    in ci == (fromIntegral (VS.unsafeIndex t' j) :: Int) && rowEq (V.unsafeIndex cs ci) i (V.unsafeIndex cs' ci) j
  (I.ColDictionary _ k x, I.ColDictionary _ k' x') ->
    let !ki = keyAt k i
        !kj = keyAt k' j
    in if ki >= 0 && ki < columnLength x && kj >= 0 && kj < columnLength x'
         then rowEq x ki x' kj
         else ki == kj
  (I.ColRunEndEncoded off _ r x, I.ColRunEndEncoded off' _ r' x') ->
    rowEq x (physicalRun r (off + i)) x' (physicalRun r' (off' + j))
  _ -> rowBytes a i == rowBytes b j
  where
    slot w bs k = BSU.unsafeTake w (BSU.unsafeDrop (k * w) bs)
    viaRanges = case (listRange a i, listRange b j) of
      (Just r, Just r') -> rangesEq (listChild a) r (listChild b) r'
      _ -> False


listChild :: ColumnArray -> ColumnArray
listChild = \case
  I.ColListView _ _ _ c -> c
  I.ColLargeListView _ _ _ c -> c
  c -> c


rangesEq :: ColumnArray -> ChildRange -> ColumnArray -> ChildRange -> Bool
rangesEq c (ChildRange s l) c' (ChildRange s' l') =
  l == l' && all (\q -> rowEq c (s + q) c' (s' + q)) [0 .. l - 1]


-- | @Tag [row, row, ...]@ with nulls as @null@; parameters after the tag.
instance Show ColumnArray where
  showsPrec d c =
    showParen (d > 10) $
      showString (columnTag c) . params . showChar ' ' . showRows c
    where
      params = case c of
        I.ColPrim (PDecimal128 p s) _ _ -> showChar ' ' . shows p . showChar ' ' . shows s
        I.ColPrim (PDecimal256 p s) _ _ -> showChar ' ' . shows p . showChar ' ' . shows s
        I.ColFixedSizeBinary w _ _ _ -> showChar ' ' . shows w
        I.ColFixedSizeList w _ _ _ -> showChar ' ' . shows w
        I.ColDictionary did _ _ -> showChar ' ' . shows did
        _ -> id


showRows :: ColumnArray -> ShowS
showRows c = showChar '[' . foldr (.) id (intersperse (showChar ',') (map (showRowAt c) [0 .. columnLength c - 1])) . showChar ']'


showRowAt :: ColumnArray -> Int -> ShowS
showRowAt c i
  | not (rowValid c i) = showString "null"
  | otherwise = case c of
      I.ColPrim t _ xs -> withPrim t (shows (VS.unsafeIndex xs i))
      I.ColBool _ b -> shows (unsafeBitAt b i)
      I.ColUtf8 {} -> shows (utf8ToText (rowBytes c i))
      I.ColLargeUtf8 {} -> shows (utf8ToText (rowBytes c i))
      I.ColUtf8View {} -> shows (utf8ToText (rowBytes c i))
      I.ColStruct _ _ cs ->
        showChar '{'
          . foldr (.) id (intersperse (showString ", ") (map (\(n, x) -> showString (T.unpack n) . showString ": " . showRowAt x i) (V.toList cs)))
          . showChar '}'
      I.ColMap _ o k x ->
        let ChildRange s l = offsetRange o i
        in showChar '{'
             . foldr (.) id (intersperse (showString ", ") (map (\q -> showRowAt k (s + q) . showString " => " . showRowAt x (s + q)) [0 .. l - 1]))
             . showChar '}'
      I.ColDenseUnion t o cs ->
        let !ci = fromIntegral (VS.unsafeIndex t i)
        in showChar '<' . shows ci . showString ": " . showRowAt (V.unsafeIndex cs ci) (fromIntegral (VS.unsafeIndex o i)) . showChar '>'
      I.ColSparseUnion t cs ->
        let !ci = fromIntegral (VS.unsafeIndex t i)
        in showChar '<' . shows ci . showString ": " . showRowAt (V.unsafeIndex cs ci) i . showChar '>'
      I.ColDictionary _ k x ->
        let !ki = keyAt k i
        in if ki >= 0 && ki < columnLength x then showRowAt x ki else showChar '#' . shows ki
      I.ColRunEndEncoded off _ r x -> showRowAt x (physicalRun r (off + i))
      _ -> case listRange c i of
        Just (ChildRange s l) ->
          let child = case c of
                I.ColList _ _ x -> x
                I.ColLargeList _ _ x -> x
                I.ColFixedSizeList _ _ _ x -> x
                x -> listChild x
          in showChar '[' . foldr (.) id (intersperse (showChar ',') (map (\q -> showRowAt child (s + q)) [0 .. l - 1])) . showChar ']'
        Nothing -> shows (rowBytes c i)


-- | Evaluate every element of a boxed vector to WHNF.
forceElems :: V.Vector a -> V.Vector a
forceElems v = V.foldl' (flip seq) () v `seq` v
