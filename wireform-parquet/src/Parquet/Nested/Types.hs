{-# LANGUAGE BangPatterns #-}

{- | Shared writer data types. Kept internal so Variant schema validation
and nested writing can depend on each other without a module cycle.
-}
module Parquet.Nested.Types where

import Arrow.Types qualified as A
import Data.Bits (shiftL, shiftR)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int32, Int64)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Vector.Primitive qualified as VP
import Data.Word (Word16)
import Parquet.File qualified as F
import Parquet.Types (LtTimeUnit (..))
import Parquet.Variant qualified as Variant


{- | Description of one column tree, mirroring the user's logical
type. 'NSOptional' / 'NSRequired' wrap a child to set the leaf or
group's repetition (Optional / Required); 'NSList' / 'NSMap' /
'NSStruct' name the compound types; 'NSVariant' emits the
Iceberg V3 / Spark Variant 2-leaf binary group
(@{required metadata: BINARY, required value: BINARY}@) annotated
with the @VARIANT(1)@ logical type.

Each named node accepts at most one Optional / Required wrapper. Map
keys must be required, and structs must contain at least one field.
The public writer validates these constraints before rows. 'NSVariantTyped'
uses an opaque canonical schema from Parquet.Variant.Schema, preserving typed
storage and original shared metadata/fallback bytes.
-}
data NestedSchema
  = NSPrimitive !LeafType
  | NSOptional !NestedSchema
  | NSRequired !NestedSchema
  | NSList !NestedSchema
  | NSMap !NestedSchema !NestedSchema -- key, value
  | NSStruct !(V.Vector (Text, NestedSchema))
  | NSFile !F.FileSchema
  | NSVariant
  | NSVariantWithLimits !Variant.VariantLimits
  | NSVariantTyped !VariantStorageSchema
  deriving (Show, Eq)


-- | Leaf primitive type and its Parquet physical encoding.
data LeafType
  = LtInt32
  | LtInt64
  | LtFloat16
  | LtFloat
  | LtDouble
  | LtBool
  | LtString
  | LtJson
  | LtBinary
  | LtInteger !Int32 !Bool
  | LtDate
  | LtTime !Bool !LtTimeUnit
  | LtTimestamp !Bool !LtTimeUnit
  | -- | Signed96 little-endian Unix-epoch counts in fixed12 bytes.
    LtTimestamp96 !Bool !LtTimeUnit
  | -- | Legacy signed Julian day plus nanoseconds-within-day, in12bytes.
    LtInt96
  | {- | Precision1..76 and scale0..precision; narrowest standard numeric
    carrier, then fixed16/32bytes. Wide values use signed big-endian bytes.
    -}
    LtDecimal !Int32 !Int32
  | LtFixedBinary !Int32
  | LtUuid
  deriving (Show, Eq)


-- | One value at a schema node, with fields in schema order.
data NestedRow
  = -- | a primitive value (matched against an NSPrimitive node)
    NRLeaf !LeafValue
  | -- | a Nothing at an NSOptional wrapper
    NRNull
  | -- | a list (possibly empty)
    NRList !(V.Vector NestedRow)
  | -- | struct fields, indexed in NSStruct order
    NRStruct !(V.Vector NestedRow)
  | -- | a map, as a vector of (key, value) pairs
    NRMapEntries !(V.Vector (NestedRow, NestedRow))
  | -- | A FILE reference. Use "Parquet.File" for schemas and resolution.
    NRFile !F.FileReference
  | {- | Pre-encoded Variant @(metadataBytes, valueBytes)@, matching
    the two binary leaves an 'NSVariant' schema emits. Use
    'encodeVariant' from "Parquet.Variant" to obtain the bytes.
    -}
    NRVariantBytes !ByteString !ByteString
  deriving (Show, Eq)


-- | Concrete value in a primitive leaf.
data LeafValue
  = LvInt32 !Int32
  | LvInt64 !Int64
  | -- | IEEE binary16 bits, retaining signed zeros and NaN payloads.
    LvFloat16 !Word16
  | LvFloat !Float
  | LvDouble !Double
  | LvBool !Bool
  | LvString !Text
  | LvBinary !ByteString
  | -- | Unprefixed fixed-size bytes (including UUID and wide DECIMAL).
    LvFixedBinary !ByteString
  deriving (Show, Eq)


-- The constructor is package-private. Only the canonical schema compiler
-- builds these plans; public callers receive an opaque validated value.
data VariantStorageSchema = VariantStorageSchema
  { storageLimits :: !Variant.VariantLimits
  , storageField :: !A.Field
  , storageLayout :: !NestedSchema
  , storageMetadataIndex :: {-# UNPACK #-} !Int
  , storageFieldIds :: !(V.Vector (Maybe Int32))
  , storageNode :: !VariantStorageNode
  }
  deriving (Show, Eq)


data VariantStorageNode = VariantStorageNode
  { storageFieldCount :: {-# UNPACK #-} !Int
  , storageValueIndex :: {-# UNPACK #-} !Int
  , storageTypedIndex :: {-# UNPACK #-} !Int
  , storageValueRequired :: !Bool
  , storageTypedRequired :: !Bool
  , storageTyped :: !(Maybe VariantStorageTyped)
  , storageNormalizationBelow :: !Bool
  }
  deriving (Show, Eq)


data VariantStorageTyped
  = StoragePrimitive
  | StorageObject !(V.Vector VariantStorageChild) !(Set.Set Text)
  | StorageArray !VariantStorageNode
  deriving (Show, Eq)


data VariantStorageChild = VariantStorageChild
  { storageChildName :: !Text
  , storageChildIndex :: {-# UNPACK #-} !Int
  , storageChildNode :: !VariantStorageNode
  }
  deriving (Show, Eq)


-- Shared by both shredders so the hot path can inline validation.
leafValueMatches :: LeafType -> LeafValue -> Bool
leafValueMatches typ value = case (typ, value) of
  (LtInteger 64 _, LvInt64 {}) -> True
  (LtInteger width signed, LvInt32 x)
    | width == 32 -> True
    | width == 8 || width == 16 ->
        if signed
          then let bound = (1 :: Int32) `shiftL` (fromIntegral width - 1) in x >= negate bound && x < bound
          else x >= 0 && x < (1 :: Int32) `shiftL` fromIntegral width
  (LtDate, LvInt32 {}) -> True
  (LtTime _ LtMillis, LvInt32 x) -> x >= 0 && x < 86400000
  (LtTime _ LtMicros, LvInt64 x) -> x >= 0 && x < 86400000000
  (LtTime _ LtNanos, LvInt64 x) -> x >= 0 && x < 86400000000000
  (LtTimestamp {}, LvInt64 {}) -> True
  (LtTimestamp96 {}, LvFixedBinary bytes) -> BS.length bytes == 12
  (LtInt96, LvFixedBinary bytes) -> BS.length bytes == 12
  (LtInt32, LvInt32 {}) -> True
  (LtInt64, LvInt64 {}) -> True
  (LtFloat16, LvFloat16 {}) -> True
  (LtFloat, LvFloat {}) -> True
  (LtDouble, LvDouble {}) -> True
  (LtBool, LvBool {}) -> True
  (LtString, LvString {}) -> True
  (LtJson, LvString {}) -> True
  (LtBinary, LvBinary {}) -> True
  (LtFixedBinary width, LvFixedBinary bytes) -> BS.length bytes == fromIntegral width
  (LtUuid, LvFixedBinary bytes) -> BS.length bytes == 16
  (LtDecimal precision _, LvInt32 raw) | precision <= 9 -> decimalFitsSmall precision (fromIntegral raw)
  (LtDecimal precision _, LvInt64 raw) | precision > 9 && precision <= 18 -> decimalFitsSmall precision raw
  (LtDecimal precision _, LvFixedBinary bytes)
    | precision > 18 && precision <= 76 ->
        BS.length bytes == (if precision <= 38 then 16 else 32) && decimalFitsBytes precision bytes
  _ -> False
{-# INLINE leafValueMatches #-}


-- Bounds are shared across all rows. Do not exponentiate, construct an
-- Integer, or decode wide bytes to Integer in the value-validation loop.
decimalFitsSmall :: Int32 -> Int64 -> Bool
decimalFitsSmall precision raw
  | precision < 1 || precision > 18 = False
  | otherwise = let !bound = VP.unsafeIndex decimalSmallBounds (fromIntegral precision) in raw > negate bound && raw < bound
{-# INLINE decimalFitsSmall #-}


decimalSmallBounds :: VP.Vector Int64
decimalSmallBounds = VP.generate 19 (\precision -> 10 ^ precision)
{-# NOINLINE decimalSmallBounds #-}


data DecimalByteBounds = DecimalByteBounds !ByteString !ByteString


decimalByteBounds :: V.Vector DecimalByteBounds
decimalByteBounds = V.generate 77 $ \precision ->
  let width = if precision <= 38 then 16 else 32
      bound = 10 ^ precision :: Integer
      bytes value = BS.pack (map (\index -> fromInteger (value `shiftR` (8 * (width - 1 - index)))) [0 .. width - 1])
  in DecimalByteBounds (bytes (negate bound)) (bytes bound)
{-# NOINLINE decimalByteBounds #-}


decimalFitsBytes :: Int32 -> ByteString -> Bool
decimalFitsBytes precision bytes
  | precision < 19 || precision > 76 || BS.null bytes = False
  | otherwise = case V.unsafeIndex decimalByteBounds (fromIntegral precision) of
      DecimalByteBounds low high -> if BS.head bytes >= 128 then bytes > low else bytes < high
{-# INLINE decimalFitsBytes #-}
