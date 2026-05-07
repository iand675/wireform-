{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
-- | Materialize Apache Arrow IPC record batch bodies into Haskell-friendly columns.
--
-- Supports flat and nested schemas. Nullable columns use a validity bitmap
-- (LSB of each byte first) plus values; decoded as @V.Vector (Maybe a)@.
module Arrow.Column
  ( ColumnArray (..)
  , NullableView (..)
    -- * NullableView helpers
  , nvIsPresent
  , nvLookup
  , nvLength
  , nvNullCount
  , nvFromMaybeList
  , nvFromMaybeVector
  , nvFromVectors
  , nvToMaybeVector
  , nvToSentinel
  , sliceNV
    -- * Bool packing
  , unpackBoolsBoxed
  , materializeFlatRecordBatch
  , materializeRecordBatch
  , columnLength
  , countFieldNodesFlat
  , countBuffersFlat
  , resolveDictionaryColumn
    -- * Row slicing
  , sliceColumnArray
    -- * Map invariants
  , validateMapKeysSorted
  ) where

import Data.Bits (shiftL, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable, peekByteOff)
import qualified Foreign.ForeignPtr as FFP
import qualified Foreign.ForeignPtr.Unsafe as FFP (unsafeForeignPtrToPtr)
import GHC.Exts (ByteArray#)
import qualified Data.Primitive.ByteArray as PBA
import qualified Data.Text.Array as TA
import qualified Data.Text.Internal as TI
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Primitive.Mutable as MVP
import System.IO.Unsafe (unsafePerformIO)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Control.Monad (forM)
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as MVS
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)

import Arrow.IPC (validateRecordBatchBuffers)
import Arrow.View
  ( Utf8View (..), BinaryView (..)
  , LargeUtf8View (..), LargeBinaryView (..)
  , NullableBoolView, NullableUtf8View, NullableBinaryView
  , NullableLargeUtf8View, NullableLargeBinaryView
  , NullableFixedSizeBinaryView
  )
import qualified Arrow.View as AV
import Columnar.Bit (Bit (..))
import qualified Columnar.Bit as Bit
import qualified Columnar.SIMD as SIMD
import Columnar.SIMD (unpackBitsLsbUnsafe)
import Arrow.Types
  ( ArrowType (..)
  , Buffer (..)
  , DateUnit (..)
  , DictionaryEncoding (..)
  , Endianness (..)
  , Field (..)
  , FieldNode (..)
  , IntervalUnit (..)
  , Precision (..)
  , RecordBatchDef (..)
  , Schema (..)
  , TimeUnit (..)
  , UnionMode (..)
  )

-- | Validity-bitmap-backed nullable primitive column. Mirrors
-- the on-the-wire Arrow / Parquet representation directly:
--
--   * @nvValidity@ is a bit per row — bit @i@ is 1 iff value
--     @i@ is present. An empty 'nvValidity' means
--     "all values present" (avoids the @ceil(N/8)@-byte
--     allocation for the common case).
--   * @nvValues@ is the dense values buffer, /always/ the
--     same length as the column. Slots that correspond to
--     null rows carry whatever was on the wire (typically
--     zero); consumers must consult 'nvValidity' before
--     reading.
--
-- See @docs/columnar-views-design.md@ for why this shape.
-- 64x cache-pressure reduction over @V.Vector (Maybe a)@
-- (which carries one pointer per row).
data NullableView a = NullableView
  { nvValidity :: !(VU.Vector Bit)
  , nvValues   :: !(VS.Vector a)
  }
  deriving stock (Show, Eq)

-- | True iff slot @i@ of a 'NullableView' carries a value.
{-# INLINE nvIsPresent #-}
nvIsPresent :: NullableView a -> Int -> Bool
nvIsPresent nv i
  | VU.null (nvValidity nv) = True
  | otherwise               = unBit (VU.unsafeIndex (nvValidity nv) i)

-- | Lookup with explicit nullability.
{-# INLINE nvLookup #-}
nvLookup :: VS.Storable a => NullableView a -> Int -> Maybe a
nvLookup nv i
  | nvIsPresent nv i = Just (VS.unsafeIndex (nvValues nv) i)
  | otherwise        = Nothing

-- | Length of a 'NullableView' (== length of 'nvValues').
{-# INLINE nvLength #-}
nvLength :: VS.Storable a => NullableView a -> Int
nvLength = VS.length . nvValues

-- | Number of null entries — popcount-of-zeros over the
-- validity bitmap.
{-# INLINE nvNullCount #-}
nvNullCount :: VS.Storable a => NullableView a -> Int
nvNullCount nv
  | VU.null (nvValidity nv) = 0
  | otherwise               = nvLength nv - Bit.countOnes (nvValidity nv)

-- | Build a 'NullableView' from a list of 'Maybe' values.
-- Allocates the validity bitmap + dense values in a single
-- pass.
{-# INLINE nvFromMaybeList #-}
nvFromMaybeList :: VS.Storable a => a -> [Maybe a] -> NullableView a
nvFromMaybeList sentinel xs =
  let !n = length xs
  in NullableView
       { nvValidity = VU.fromListN n (map (Bit . isJust) xs)
       , nvValues   = VS.fromListN n (map (fromMaybe sentinel) xs)
       }
  where
    isJust (Just _) = True
    isJust Nothing  = False
    fromMaybe d Nothing = d
    fromMaybe _ (Just x) = x

-- | Build a 'NullableView' from a boxed vector of 'Maybe'
-- values. Useful for the @Arrow.Record@ encoder API which
-- still hands us boxed vectors. Allocates the validity
-- bitmap + dense values in a single pass.
{-# INLINE nvFromMaybeVector #-}
nvFromMaybeVector
  :: VS.Storable a
  => a -> V.Vector (Maybe a) -> NullableView a
nvFromMaybeVector sentinel v =
  let !n = V.length v
  in NullableView
       { nvValidity = VU.generate n $ \i ->
           case V.unsafeIndex v i of
             Just _  -> Bit True
             Nothing -> Bit False
       , nvValues   = VS.generate n $ \i ->
           case V.unsafeIndex v i of
             Just x  -> x
             Nothing -> sentinel
       }

-- | Materialise back to the legacy boxed @V.Vector (Maybe a)@
-- representation for callers that haven't migrated yet.
{-# INLINE nvToMaybeVector #-}
nvToMaybeVector :: VS.Storable a => NullableView a -> V.Vector (Maybe a)
nvToMaybeVector nv =
  let !n = nvLength nv
  in V.generate n (nvLookup nv)

-- | Build a 'NullableView' from an existing dense values
-- buffer + 'V.Vector' of presence bits.
{-# INLINE nvFromVectors #-}
nvFromVectors
  :: VS.Storable a
  => V.Vector Bool -> VS.Vector a -> NullableView a
nvFromVectors validity values = NullableView
  { nvValidity = VU.fromListN (V.length validity) (map Bit (V.toList validity))
  , nvValues   = values
  }

-- | Slice a 'NullableView' at row boundaries.
{-# INLINE sliceNV #-}
sliceNV :: VS.Storable a => Int -> Int -> NullableView a -> NullableView a
sliceNV s l (NullableView vp vv) =
  NullableView
    { nvValidity = if VU.null vp then vp else VU.slice s l vp
    , nvValues   = VS.slice s l vv
    }

-- | Materialized values for one column.
-- Nullable columns use @Col*Maybe@ with per-row 'Maybe'.
data ColumnArray
  = ColInt8 !(VS.Vector Int8)
  | ColInt16 !(VS.Vector Int16)
  | ColInt32 !(VS.Vector Int32)
  | ColInt64 !(VS.Vector Int64)
  | ColUInt8 !(VS.Vector Word8)
  | ColUInt16 !(VS.Vector Word16)
  | ColUInt32 !(VS.Vector Word32)
  | ColUInt64 !(VS.Vector Word64)
  | ColFloat16 !(VS.Vector Word16)
  | ColFloat !(VS.Vector Float)
  | ColDouble !(VS.Vector Double)
  | ColBool !(VU.Vector Bit)
  | ColUtf8 !Utf8View
  | ColBinary !BinaryView
  | ColLargeUtf8 !LargeUtf8View
  | ColLargeBinary !LargeBinaryView
  | ColFixedSizeBinary !Int !(V.Vector ByteString)
  | ColDate32 !(VS.Vector Int32)
  | ColDate64 !(VS.Vector Int64)
  | ColTime32 !(VS.Vector Int32)
  | ColTime64 !(VS.Vector Int64)
  | ColTimestamp !(VS.Vector Int64)
  | ColDuration !(VS.Vector Int64)
  | ColDecimal128 !Int !Int !(V.Vector ByteString)
  | ColDecimal256 !Int !Int !(V.Vector ByteString)
  | -- | Arrow @INTERVAL(YEAR_MONTH)@: 32-bit months (i32 per row).
    ColIntervalYearMonth !(VS.Vector Int32)
  | -- | Arrow @INTERVAL(DAY_TIME)@: (days :: i32, ms :: i32) per row,
    -- stored as an 8-byte pair in element order.
    ColIntervalDayTime !(VS.Vector Int32) !(VS.Vector Int32)
  | -- | Arrow @INTERVAL(MONTH_DAY_NANO)@: (months :: i32, days :: i32,
    -- nanos :: i64) per row, stored as a 16-byte triple.
    ColIntervalMonthDayNano !(VS.Vector Int32) !(VS.Vector Int32) !(VS.Vector Int64)
  | ColInt8Maybe   !(NullableView Int8)
  | ColInt16Maybe  !(NullableView Int16)
  | ColInt32Maybe  !(NullableView Int32)
  | ColInt64Maybe  !(NullableView Int64)
  | ColUInt8Maybe  !(NullableView Word8)
  | ColUInt16Maybe !(NullableView Word16)
  | ColUInt32Maybe !(NullableView Word32)
  | ColUInt64Maybe !(NullableView Word64)
  | ColFloat16Maybe !(NullableView Word16)
  | ColFloatMaybe  !(NullableView Float)
  | ColDoubleMaybe !(NullableView Double)
  | ColBoolMaybe !NullableBoolView
  | ColUtf8Maybe !NullableUtf8View
  | ColBinaryMaybe !NullableBinaryView
  | ColLargeUtf8Maybe !NullableLargeUtf8View
  | ColLargeBinaryMaybe !NullableLargeBinaryView
  | ColFixedSizeBinaryMaybe !NullableFixedSizeBinaryView
  | ColDate32Maybe    !(NullableView Int32)
  | ColDate64Maybe    !(NullableView Int64)
  | ColTime32Maybe    !(NullableView Int32)
  | ColTime64Maybe    !(NullableView Int64)
  | ColTimestampMaybe !(NullableView Int64)
  | ColDurationMaybe  !(NullableView Int64)
  | ColStruct !(V.Vector (Text, ColumnArray))
  | ColStructMaybe !(VU.Vector Bit) !(V.Vector (Text, ColumnArray))
  | ColList !(VS.Vector Int32) !ColumnArray
  | ColListMaybe !(VU.Vector Bit) !(VS.Vector Int32) !ColumnArray
  | -- | Arrow \"LargeList\": semantics identical to 'ColList' but with
    -- 64-bit offsets. Used when the child array has more than
    -- 2^31 elements.
    ColLargeList !(VS.Vector Int64) !ColumnArray
  | ColLargeListMaybe !(VU.Vector Bit) !(VS.Vector Int64) !ColumnArray
  | ColFixedSizeList !Int !ColumnArray
  | ColFixedSizeListMaybe !Int !(VU.Vector Bit) !ColumnArray
  | ColMap !(VS.Vector Int32) !ColumnArray !ColumnArray
  | ColMapMaybe !(VU.Vector Bit) !(VS.Vector Int32) !ColumnArray !ColumnArray
  | ColDenseUnion !(VS.Vector Int8) !(VS.Vector Int32) !(V.Vector ColumnArray)
  | ColSparseUnion !(VS.Vector Int8) !(V.Vector ColumnArray)
  | ColDictionary !Int64 !(VS.Vector Int32) !ColumnArray
  | -- | Run-End Encoded column (Arrow spec >= 1.3). The first child
    -- holds the run-end indices (int16/32/64, ascending, the
    -- @i@-th element being the EXCLUSIVE end index of run @i@); the
    -- second holds the actual values (any type, may be nullable).
    -- The parent has /no/ buffers and /no/ validity bitmap of its
    -- own — nulls live in the values child.
    ColRunEndEncoded !ColumnArray !ColumnArray
  | -- | ListView (Arrow spec >= 1.4). Like 'ColList' but with a
    -- separate sizes buffer; offsets and sizes are independent
    -- 32-bit arrays (so list elements may overlap or be in any
    -- order in the child storage).
    ColListView !(VS.Vector Int32) !(VS.Vector Int32) !ColumnArray
  | ColListViewMaybe !(VU.Vector Bit) !(VS.Vector Int32) !(VS.Vector Int32) !ColumnArray
  | -- | LargeListView: 64-bit offsets and sizes.
    ColLargeListView !(VS.Vector Int64) !(VS.Vector Int64) !ColumnArray
  | ColLargeListViewMaybe !(VU.Vector Bit) !(VS.Vector Int64) !(VS.Vector Int64) !ColumnArray
  | -- | Utf8View (Arrow spec >= 1.4). Each row is a 16-byte view
    -- struct: a 4-byte length followed by either an inlined
    -- payload (length <= 12) or a (4-byte prefix + 4-byte buffer
    -- index + 4-byte buffer offset) reference into one of the
    -- variadic data buffers. The materialized form here is the
    -- decoded UTF-8 strings; the inlined-vs-out-of-line layout
    -- is the writer's concern.
    ColUtf8View !(V.Vector Text)
  | ColUtf8ViewMaybe !(V.Vector (Maybe Text))
  | -- | BinaryView: same layout as 'ColUtf8View' but no UTF-8
    -- validation; raw bytes.
    ColBinaryView !(V.Vector ByteString)
  | ColBinaryViewMaybe !(V.Vector (Maybe ByteString))
  | -- | Arrow NULL (@ANull@) column: the spec assigns no
    -- buffers and no validity bitmap, just a length where every
    -- row is null. Useful for placeholder columns and for the
    -- pyarrow-emits-AN-empty-column edge case in test fixtures.
    ColNull !Int
  deriving stock (Show, Eq)

-- | One field node per top-level field (flat schema).
countFieldNodesFlat :: V.Vector Field -> Int
countFieldNodesFlat fs = V.length fs

-- | Buffer count for one flat field (validity bitmap first when nullable).
buffersPerField :: Field -> Either String Int
buffersPerField f
  | not (V.null (fieldChildren f)) = Left "Arrow.Column: nested fieldChildren not supported in flat mode"
  | otherwise = do
      nData <- case fieldType f of
        ANull -> Right (-1)  -- ANull has no buffers at all (no validity, no data).
        AInt {} -> Right 1
        ABool -> Right 1
        AFloatingPoint _ -> Right 1
        AUtf8 -> Right 2
        ABinary -> Right 2
        ALargeUtf8 -> Right 2
        ALargeBinary -> Right 2
        AFixedSizeBinary _ -> Right 1
        ADate _ -> Right 1
        ATime _ _ -> Right 1
        ATimestamp _ _ -> Right 1
        ADuration _ -> Right 1
        ADecimal _ _ -> Right 1
        ADecimal256 _ _ -> Right 1
        AInterval _ -> Right 1
        ty -> Left $ "Arrow.Column: unsupported flat type: " ++ show ty
      -- ANull contributes 0 buffers regardless of nullability.
      case fieldType f of
        ANull -> Right 0
        _     -> Right $ (if fieldNullable f then 1 else 0) + nData

-- | Total IPC body buffers required for a flat schema.
countBuffersFlat :: V.Vector Field -> Either String Int
countBuffersFlat fs = sum <$> V.mapM buffersPerField fs

-- | Decode every top-level field in a flat schema from the IPC message body.
materializeFlatRecordBatch :: Schema -> RecordBatchDef -> ByteString -> Either String (V.Vector ColumnArray)
materializeFlatRecordBatch schema rb body = do
  let fields = arrowFields schema
  nBufsSum <- countBuffersFlat fields
  let nNodes = countFieldNodesFlat fields
  if V.length (rbNodes rb) /= nNodes
    then
      Left $
        "Arrow.Column: field node count mismatch (expected "
          ++ show nNodes
          ++ ", got "
          ++ show (V.length (rbNodes rb))
          ++ ")"
    else
      if V.length (rbBuffers rb) /= nBufsSum
        then
          Left $
            "Arrow.Column: buffer count mismatch (expected "
              ++ show nBufsSum
              ++ ", got "
              ++ show (V.length (rbBuffers rb))
              ++ ")"
        else do
          let bodyLen = fromIntegral (BS.length body) :: Int64
          if not (validateRecordBatchBuffers rb bodyLen)
            then Left "Arrow.Column: invalid buffer bounds in RecordBatchDef"
            else materializeFields (arrowEndianness schema) fields rb body 0 0

materializeFields :: Endianness -> V.Vector Field -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (V.Vector ColumnArray)
materializeFields endian fields rb body !nodeIdx !bufIdx
  | V.null fields = Right V.empty
  | otherwise = do
      (c, n1, b1) <- materializeOne endian (V.head fields) rb body nodeIdx bufIdx
      rest <- materializeFields endian (V.tail fields) rb body n1 b1
      Right (V.cons c rest)

materializeOne :: Endianness -> Field -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
materializeOne endian f rb body !nodeIdx !bufIdx
  -- ANull has no buffers and no validity bitmap (per spec): the
  -- field node's length is the only state. Same shape regardless
  -- of fieldNullable.
  | ANull <- fieldType f =
      let node = V.unsafeIndex (rbNodes rb) nodeIdx
          !len = fromIntegral (fnLength node) :: Int
      in Right (ColNull len, nodeIdx + 1, bufIdx)
  | otherwise =
  let node = V.unsafeIndex (rbNodes rb) nodeIdx
      !len = fromIntegral (fnLength node) :: Int
  in if fieldNullable f
    then case fieldType f of
      AInt 8 True -> readInt8ColumnMaybe endian len rb body bufIdx nodeIdx
      AInt 8 False -> readUInt8ColumnMaybe len rb body bufIdx nodeIdx
      AInt 16 True -> readInt16ColumnMaybe endian True len rb body bufIdx nodeIdx
      AInt 16 False -> readUInt16ColumnMaybe endian len rb body bufIdx nodeIdx
      AInt 32 True -> readInt32ColumnMaybe endian True len rb body bufIdx nodeIdx
      AInt 32 False -> readUInt32ColumnMaybe endian len rb body bufIdx nodeIdx
      AInt 64 True -> readInt64ColumnMaybe endian True len rb body bufIdx nodeIdx
      AInt 64 False -> readUInt64ColumnMaybe endian len rb body bufIdx nodeIdx
      ABool -> readBoolColumnMaybe len rb body bufIdx nodeIdx
      AFloatingPoint Half -> readFloat16ColumnMaybe endian len rb body bufIdx nodeIdx
      AFloatingPoint Single -> readFloatColumnMaybe endian len rb body bufIdx nodeIdx
      AFloatingPoint DoublePrecision -> readDoubleColumnMaybe endian len rb body bufIdx nodeIdx
      AUtf8 -> readUtf8ColumnMaybe endian len rb body bufIdx nodeIdx
      ABinary -> readBinaryColumnMaybe endian len rb body bufIdx nodeIdx
      ALargeUtf8 -> readLargeUtf8ColumnMaybe endian len rb body bufIdx nodeIdx
      ALargeBinary -> readLargeBinaryColumnMaybe endian len rb body bufIdx nodeIdx
      AFixedSizeBinary n -> readFixedSizeBinaryColumnMaybe n len rb body bufIdx nodeIdx
      ADate DateDay -> readDate32ColumnMaybe endian len rb body bufIdx nodeIdx
      ADate DateMillisecond -> readDate64ColumnMaybe endian len rb body bufIdx nodeIdx
      ATime Second _ -> readTime32ColumnMaybe endian len rb body bufIdx nodeIdx
      ATime Millisecond _ -> readTime32ColumnMaybe endian len rb body bufIdx nodeIdx
      ATime Microsecond _ -> readTime64ColumnMaybe endian len rb body bufIdx nodeIdx
      ATime Nanosecond _ -> readTime64ColumnMaybe endian len rb body bufIdx nodeIdx
      ATimestamp _ _ -> readTimestampColumnMaybe endian len rb body bufIdx nodeIdx
      ADuration _ -> readDurationColumnMaybe endian len rb body bufIdx nodeIdx
      ADecimal p s -> readDecimal128ColumnMaybe p s len rb body bufIdx nodeIdx
      ADecimal256 p s -> readDecimal256ColumnMaybe p s len rb body bufIdx nodeIdx
      ty -> Left $ "Arrow.Column: unsupported nullable type: " ++ show ty
    else case fieldType f of
      AInt 8 True -> readInt8Column endian len rb body bufIdx nodeIdx
      AInt 8 False -> readUInt8Column len rb body bufIdx nodeIdx
      AInt 16 True -> readInt16Column endian True len rb body bufIdx nodeIdx
      AInt 16 False -> readUInt16Column endian len rb body bufIdx nodeIdx
      AInt 32 True -> readInt32Column endian True len rb body bufIdx nodeIdx
      AInt 32 False -> readUInt32Column endian len rb body bufIdx nodeIdx
      AInt 64 True -> readInt64Column endian True len rb body bufIdx nodeIdx
      AInt 64 False -> readUInt64Column endian len rb body bufIdx nodeIdx
      ABool -> readBoolColumn len rb body bufIdx nodeIdx
      AFloatingPoint Half -> readFloat16Column endian len rb body bufIdx nodeIdx
      AFloatingPoint Single -> readFloatColumn endian len rb body bufIdx nodeIdx
      AFloatingPoint DoublePrecision -> readDoubleColumn endian len rb body bufIdx nodeIdx
      AUtf8 -> readUtf8Column endian len rb body bufIdx nodeIdx
      ABinary -> readBinaryColumn endian len rb body bufIdx nodeIdx
      ALargeUtf8 -> readLargeUtf8Column endian len rb body bufIdx nodeIdx
      ALargeBinary -> readLargeBinaryColumn endian len rb body bufIdx nodeIdx
      AFixedSizeBinary n -> readFixedSizeBinaryColumn n len rb body bufIdx nodeIdx
      ADate DateDay -> readDate32Column endian len rb body bufIdx nodeIdx
      ADate DateMillisecond -> readDate64Column endian len rb body bufIdx nodeIdx
      ATime Second _ -> readTime32Column endian len rb body bufIdx nodeIdx
      ATime Millisecond _ -> readTime32Column endian len rb body bufIdx nodeIdx
      ATime Microsecond _ -> readTime64Column endian len rb body bufIdx nodeIdx
      ATime Nanosecond _ -> readTime64Column endian len rb body bufIdx nodeIdx
      ATimestamp _ _ -> readTimestampColumn endian len rb body bufIdx nodeIdx
      ADuration _ -> readDurationColumn endian len rb body bufIdx nodeIdx
      ADecimal p s -> readDecimal128Column p s len rb body bufIdx nodeIdx
      ADecimal256 p s -> readDecimal256Column p s len rb body bufIdx nodeIdx
      ty -> Left $ "Arrow.Column: unsupported type: " ++ show ty

-- * Non-nullable column readers

readInt8Column :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readInt8Column _endian len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  col <- readInts8 len valsBs
  Right (ColInt8 col, nodeIdx + 1, bufIdx + 1)

readInt16Column :: Endianness -> Bool -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readInt16Column endian signed len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  col <- readInts16 endian signed len valsBs
  Right (ColInt16 col, nodeIdx + 1, bufIdx + 1)

readInt32Column :: Endianness -> Bool -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readInt32Column endian signed len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  col <- readInts32 endian signed len valsBs
  Right (ColInt32 col, nodeIdx + 1, bufIdx + 1)

readInt64Column :: Endianness -> Bool -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readInt64Column endian signed len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  col <- readInts64 endian signed len valsBs
  Right (ColInt64 col, nodeIdx + 1, bufIdx + 1)

readUInt8Column :: Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readUInt8Column len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  if BS.length valsBs < len
    then Left "Arrow.Column: uint8 buffer too small"
    else Right (ColUInt8 (VS.generate len $ \i -> BSU.unsafeIndex valsBs i), nodeIdx + 1, bufIdx + 1)

readUInt16Column :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readUInt16Column endian len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  if BS.length valsBs < len * 2
    then Left "Arrow.Column: uint16 buffer too small"
    else case endian of
      Little -> Right (ColUInt16 (viewPrimVecLE 2 len valsBs), nodeIdx + 1, bufIdx + 1)
      _      -> Right (ColUInt16 (bswapPrimVec16 len valsBs), nodeIdx + 1, bufIdx + 1)

readUInt32Column :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readUInt32Column endian len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  if BS.length valsBs < len * 4
    then Left "Arrow.Column: uint32 buffer too small"
    else case endian of
      Little -> Right (ColUInt32 (viewPrimVecLE 4 len valsBs), nodeIdx + 1, bufIdx + 1)
      _      -> Right (ColUInt32 (bswapPrimVec32 len valsBs), nodeIdx + 1, bufIdx + 1)

readUInt64Column :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readUInt64Column endian len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  if BS.length valsBs < len * 8
    then Left "Arrow.Column: uint64 buffer too small"
    else case endian of
      Little -> Right (ColUInt64 (viewPrimVecLE 8 len valsBs), nodeIdx + 1, bufIdx + 1)
      _      -> Right (ColUInt64 (bswapPrimVec64 len valsBs), nodeIdx + 1, bufIdx + 1)

readFloat16Column :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readFloat16Column endian len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  if BS.length valsBs < len * 2
    then Left "Arrow.Column: float16 buffer too small"
    else Right (ColFloat16 (VS.generate len $ \i -> readWord16 endian valsBs (i * 2)), nodeIdx + 1, bufIdx + 1)

readBoolColumn :: Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readBoolColumn len rb body !bufIdx !nodeIdx = do
  dataBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  bs <- unpackBools len dataBs
  Right (ColBool bs, nodeIdx + 1, bufIdx + 1)

readFloatColumn :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readFloatColumn endian len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  if BS.length valsBs < len * 4
    then Left "Arrow.Column: float buffer too small"
    else case endian of
      Little -> Right (ColFloat (viewPrimVecLE 4 len valsBs), nodeIdx + 1, bufIdx + 1)
      _      -> Right (ColFloat (bswapPrimVec32 len valsBs), nodeIdx + 1, bufIdx + 1)

readDoubleColumn :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readDoubleColumn endian len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  if BS.length valsBs < len * 8
    then Left "Arrow.Column: double buffer too small"
    else case endian of
      Little -> Right (ColDouble (viewPrimVecLE 8 len valsBs), nodeIdx + 1, bufIdx + 1)
      _      -> Right (ColDouble (bswapPrimVec64 len valsBs), nodeIdx + 1, bufIdx + 1)

readUtf8Column :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readUtf8Column endian len rb body !bufIdx !nodeIdx = do
  offBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  datBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  if BS.length offBs < (len + 1) * 4
    then Left "Arrow.Column: UTF-8 offsets buffer too small"
    else
      -- Fast path: when the data buffer is all ASCII (the common
      -- case for English / identifier-style text), share one
      -- 'TA.Array' across every 'Text' value via the unsafe
      -- 'TI.text' constructor — same trick we use in
      -- Parquet.Read.decodePlainByteArrayAsTextAscii.
      --
      -- ASCII check is one Word64-chunked scan over the data
      -- buffer (~100 µs for 1 MB) and the savings vs the
      -- @V.generateM + TE.decodeUtf8'@ shape are 100 k tiny
      -- ByteArray allocations.
      -- Build a Utf8View directly: both buffers stay as
      -- VS.Vector views into the source body. UTF-8 validation
      -- still runs at per-row access time via 'AV.utf8At'.
      case endian of
        Little ->
          let !view = Utf8View
                { uvOffsets = viewPrimVecLE 4 (len + 1) offBs
                , uvData    = viewPrimVecLE 1 (BS.length datBs) datBs
                }
          in Right (ColUtf8 view, nodeIdx + 1, bufIdx + 2)
        _ -> do
          -- Big-endian: copy + bswap the offsets, then the
          -- data buffer is already the wire bytes.
          let !view = Utf8View
                { uvOffsets = bswapPrimVec32 (len + 1) offBs
                , uvData    = viewPrimVecLE 1 (BS.length datBs) datBs
                }
          Right (ColUtf8 view, nodeIdx + 1, bufIdx + 2)

-- | True iff every byte of the buffer is ASCII.
-- Delegates to the SIMDe-accelerated 'SIMD.isAsciiBS' (SSE2 /
-- ARM NEON via SIMDe).
isAsciiBS :: ByteString -> Bool
isAsciiBS = SIMD.isAsciiBS

-- | Fast Utf8 read assuming caller has already confirmed all
-- bytes in @datBs@ are < 0x80. Materialises one shared
-- 'TA.Array' (a 'ByteArray') containing the data buffer's
-- bytes; every output 'Text' is a (Array, offset, length)
-- triple pointing into that one Array.
readUtf8ColumnFastAscii
  :: Int -> ByteString -> ByteString
  -> Either String (V.Vector Text)
readUtf8ColumnFastAscii !len !offBs !datBs = unsafePerformIO $
  BSU.unsafeUseAsCStringLen datBs $ \(datPtr0, datLen) ->
    BSU.unsafeUseAsCStringLen offBs $ \(offPtr0, offLen) -> do
      if offLen < (len + 1) * 4
        then pure (Left "Arrow.Column: UTF-8 offsets buffer too small")
        else do
          let !datPtr = castPtr datPtr0 :: Ptr Word8
              !offPtr = castPtr offPtr0 :: Ptr Word8
          marr <- PBA.newByteArray datLen
          PBA.copyPtrToMutableByteArray marr 0 (datPtr :: Ptr Word8) datLen
          frozen <- PBA.unsafeFreezeByteArray marr
          let !textArr = pbaToTextArr frozen
          mv <- VM.unsafeNew len
          let go !i
                | i >= len  = pure (Right ())
                | otherwise = do
                    s0 <- peekByteOff offPtr (i * 4)       :: IO Int32
                    s1 <- peekByteOff offPtr ((i + 1) * 4) :: IO Int32
                    let !start = fromIntegral s0 :: Int
                        !end   = fromIntegral s1 :: Int
                    if start < 0 || end < start || end > datLen
                      then pure (Left "Arrow.Column: invalid UTF-8 slice")
                      else do
                        VM.unsafeWrite mv i (TI.text textArr start (end - start))
                        go (i + 1)
          r <- go 0
          case r of
            Left e   -> pure (Left e)
            Right () -> Right <$> V.unsafeFreeze mv

-- | Coerce a primitive 'PBA.ByteArray' to text's 'TA.Array'
-- (both wrap the same 'ByteArray#' under the hood).
pbaToTextArr :: PBA.ByteArray -> TA.Array
pbaToTextArr (PBA.ByteArray ba#) = TA.ByteArray ba#

readBinaryColumn :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readBinaryColumn endian len rb body !bufIdx !nodeIdx = do
  offBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  datBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  if BS.length offBs < (len + 1) * 4
    then Left "Arrow.Column: binary offsets buffer too small"
    else
      let !offsets = case endian of
            Little -> viewPrimVecLE 4 (len + 1) offBs
            _      -> bswapPrimVec32 (len + 1) offBs
          !view = BinaryView
            { bvOffsets = offsets
            , bvData    = viewPrimVecLE 1 (BS.length datBs) datBs
            }
      in Right (ColBinary view, nodeIdx + 1, bufIdx + 2)

readLargeUtf8Column :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readLargeUtf8Column endian len rb body !bufIdx !nodeIdx = do
  offBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  datBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  if BS.length offBs < (len + 1) * 8
    then Left "Arrow.Column: large UTF-8 offsets buffer too small"
    else
      let !offsets = case endian of
            Little -> viewPrimVecLE 8 (len + 1) offBs
            _      -> bswapPrimVec64 (len + 1) offBs
          !view = LargeUtf8View
            { luvOffsets = offsets
            , luvData    = viewPrimVecLE 1 (BS.length datBs) datBs
            }
      in Right (ColLargeUtf8 view, nodeIdx + 1, bufIdx + 2)

readLargeBinaryColumn :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readLargeBinaryColumn endian len rb body !bufIdx !nodeIdx = do
  offBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  datBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  if BS.length offBs < (len + 1) * 8
    then Left "Arrow.Column: large binary offsets buffer too small"
    else
      let !offsets = case endian of
            Little -> viewPrimVecLE 8 (len + 1) offBs
            _      -> bswapPrimVec64 (len + 1) offBs
          !view = LargeBinaryView
            { lbvOffsets = offsets
            , lbvData    = viewPrimVecLE 1 (BS.length datBs) datBs
            }
      in Right (ColLargeBinary view, nodeIdx + 1, bufIdx + 2)

readFixedSizeBinaryColumn :: Int -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readFixedSizeBinaryColumn byteWidth len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  if BS.length valsBs < len * byteWidth
    then Left "Arrow.Column: fixed-size binary buffer too small"
    else Right (ColFixedSizeBinary byteWidth (V.generate len $ \i ->
      BS.take byteWidth (BS.drop (i * byteWidth) valsBs)), nodeIdx + 1, bufIdx + 1)

readDate32Column :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readDate32Column endian len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  if BS.length valsBs < len * 4
    then Left "Arrow.Column: date32 buffer too small"
    else case endian of
      Little -> Right (ColDate32 (viewPrimVecLE 4 len valsBs), nodeIdx + 1, bufIdx + 1)
      _      -> Right (ColDate32 (VS.generate len $ \i ->
        fromIntegral (readWord32 endian valsBs (i * 4)) :: Int32), nodeIdx + 1, bufIdx + 1)

readDate64Column :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readDate64Column endian len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  if BS.length valsBs < len * 8
    then Left "Arrow.Column: date64 buffer too small"
    else case endian of
      Little -> Right (ColDate64 (viewPrimVecLE 8 len valsBs), nodeIdx + 1, bufIdx + 1)
      _      -> Right (ColDate64 (VS.generate len $ \i ->
        fromIntegral (readWord64 endian valsBs (i * 8)) :: Int64), nodeIdx + 1, bufIdx + 1)

readTime32Column :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readTime32Column endian len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  if BS.length valsBs < len * 4
    then Left "Arrow.Column: time32 buffer too small"
    else case endian of
      Little -> Right (ColTime32 (viewPrimVecLE 4 len valsBs), nodeIdx + 1, bufIdx + 1)
      _      -> Right (ColTime32 (VS.generate len $ \i ->
        fromIntegral (readWord32 endian valsBs (i * 4)) :: Int32), nodeIdx + 1, bufIdx + 1)

readTime64Column :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readTime64Column endian len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  if BS.length valsBs < len * 8
    then Left "Arrow.Column: time64 buffer too small"
    else case endian of
      Little -> Right (ColTime64 (viewPrimVecLE 8 len valsBs), nodeIdx + 1, bufIdx + 1)
      _      -> Right (ColTime64 (VS.generate len $ \i ->
        fromIntegral (readWord64 endian valsBs (i * 8)) :: Int64), nodeIdx + 1, bufIdx + 1)

readTimestampColumn :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readTimestampColumn endian len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  if BS.length valsBs < len * 8
    then Left "Arrow.Column: timestamp buffer too small"
    else case endian of
      Little -> Right (ColTimestamp (viewPrimVecLE 8 len valsBs), nodeIdx + 1, bufIdx + 1)
      _      -> Right (ColTimestamp (VS.generate len $ \i ->
        fromIntegral (readWord64 endian valsBs (i * 8)) :: Int64), nodeIdx + 1, bufIdx + 1)

readDurationColumn :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readDurationColumn endian len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  if BS.length valsBs < len * 8
    then Left "Arrow.Column: duration buffer too small"
    else case endian of
      Little -> Right (ColDuration (viewPrimVecLE 8 len valsBs), nodeIdx + 1, bufIdx + 1)
      _      -> Right (ColDuration (VS.generate len $ \i ->
        fromIntegral (readWord64 endian valsBs (i * 8)) :: Int64), nodeIdx + 1, bufIdx + 1)

readDecimal128Column :: Int -> Int -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readDecimal128Column precision scale len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  if BS.length valsBs < len * 16
    then Left "Arrow.Column: decimal128 buffer too small"
    else Right (ColDecimal128 precision scale (V.generate len $ \i ->
      BS.take 16 (BS.drop (i * 16) valsBs)), nodeIdx + 1, bufIdx + 1)

readDecimal256Column :: Int -> Int -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readDecimal256Column precision scale len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  if BS.length valsBs < len * 32
    then Left "Arrow.Column: decimal256 buffer too small"
    else Right (ColDecimal256 precision scale (V.generate len $ \i ->
      BS.take 32 (BS.drop (i * 32) valsBs)), nodeIdx + 1, bufIdx + 1)

-- * Nullable column readers (validity bitmap + values)

-- All readXColumnMaybe variants now decode the validity
-- bitmap as a packed VU.Vector Bit (unpackBools) and produce
-- a NullableView with a dense VS.Vector of values. Null
-- slots in the values buffer carry whatever was on the wire
-- (typically zero); consumers must consult nvValidity
-- before reading. This matches the Arrow / Parquet wire
-- representation directly.

{-# INLINE readPrimColumnMaybe #-}
readPrimColumnMaybe
  :: Storable a
  => Int                                         -- ^ element size in bytes
  -> String                                      -- ^ error label, e.g. "int32"
  -> (Int -> ByteString -> Either String (VS.Vector a))
                                                 -- ^ buffer -> dense values
  -> (NullableView a -> ColumnArray)             -- ^ ColXxxMaybe constructor
  -> Int -> RecordBatchDef -> ByteString -> Int -> Int
  -> Either String (ColumnArray, Int, Int)
readPrimColumnMaybe !elemBytes !label !decodeVals !mkCol len rb body !bufIdx !nodeIdx = do
  validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  valsBs  <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  validBits <- unpackBools len validBs
  if BS.length valsBs < len * elemBytes
    then Left ("Arrow.Column: " ++ label ++ " values buffer too small")
    else do
      vals <- decodeVals len valsBs
      Right ( mkCol (NullableView validBits vals)
            , nodeIdx + 1, bufIdx + 2)

readInt8ColumnMaybe :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readInt8ColumnMaybe _endian =
  readPrimColumnMaybe 1 "int8"
    (\n bs -> Right (VS.generate n (\i -> fromIntegral (BSU.unsafeIndex bs i) :: Int8)))
    ColInt8Maybe

readInt16ColumnMaybe :: Endianness -> Bool -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readInt16ColumnMaybe endian signed =
  readPrimColumnMaybe 2 "int16"
    (\n bs -> Right $! VS.generate n $ \i ->
       let !v = readWord16 endian bs (i * 2)
       in if signed then fromIntegral (fromIntegral v :: Int16) else fromIntegral v)
    ColInt16Maybe

readInt32ColumnMaybe :: Endianness -> Bool -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readInt32ColumnMaybe endian signed =
  readPrimColumnMaybe 4 "int32"
    (\n bs -> Right $! VS.generate n $ \i ->
       let !v = readWord32 endian bs (i * 4)
       in if signed then int32FromWord v else fromIntegral v)
    ColInt32Maybe

readInt64ColumnMaybe :: Endianness -> Bool -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readInt64ColumnMaybe endian signed =
  readPrimColumnMaybe 8 "int64"
    (\n bs -> Right $! VS.generate n $ \i ->
       let !v = readWord64 endian bs (i * 8)
       in if signed then int64FromWord v else fromIntegral v)
    ColInt64Maybe

readUInt8ColumnMaybe :: Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readUInt8ColumnMaybe =
  readPrimColumnMaybe 1 "uint8"
    (\n bs -> Right $! VS.generate n (BSU.unsafeIndex bs))
    ColUInt8Maybe

readUInt16ColumnMaybe :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readUInt16ColumnMaybe endian =
  readPrimColumnMaybe 2 "uint16"
    (\n bs -> Right $! VS.generate n (\i -> readWord16 endian bs (i * 2)))
    ColUInt16Maybe

readUInt32ColumnMaybe :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readUInt32ColumnMaybe endian =
  readPrimColumnMaybe 4 "uint32"
    (\n bs -> Right $! VS.generate n (\i -> readWord32 endian bs (i * 4)))
    ColUInt32Maybe

readUInt64ColumnMaybe :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readUInt64ColumnMaybe endian =
  readPrimColumnMaybe 8 "uint64"
    (\n bs -> Right $! VS.generate n (\i -> readWord64 endian bs (i * 8)))
    ColUInt64Maybe

readFloat16ColumnMaybe :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readFloat16ColumnMaybe endian =
  readPrimColumnMaybe 2 "float16"
    (\n bs -> Right $! VS.generate n (\i -> readWord16 endian bs (i * 2)))
    ColFloat16Maybe

readBoolColumnMaybe :: Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readBoolColumnMaybe len rb body !bufIdx !nodeIdx = do
  validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  dataBs  <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  validBits <- unpackBools len validBs
  valBits   <- unpackBools len dataBs
  -- Both bitmaps are already in wire shape (validity +
  -- packed values). Hand them straight to NullableBoolView
  -- — no per-row allocation, no boxed spine.
  Right
    ( ColBoolMaybe (AV.NullableBoolView validBits valBits)
    , nodeIdx + 1, bufIdx + 2
    )

readFloatColumnMaybe :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readFloatColumnMaybe endian =
  readPrimColumnMaybe 4 "float"
    (\n bs ->
       let go i = case readF32 endian bs (i * 4) of
             Right v -> Right v
             Left e  -> Left e
       in V.generate n go `seq` -- Force the Either chain via a
                                  -- staged build below.
          buildOrError n go)
    ColFloatMaybe
  where
    -- Two-pass: first walk to find any error, then build the
    -- VS.Vector. Cheaper than V.fromList through Either.
    buildOrError !n decode = do
      _ <- traverse decode [0 .. n - 1]
      Right $! VS.generate n $ \i ->
        case decode i of
          Right v -> v
          Left _  -> 0   -- unreachable: traverse above failed first

readDoubleColumnMaybe :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readDoubleColumnMaybe endian =
  readPrimColumnMaybe 8 "double"
    (\n bs ->
       let go i = readF64 endian bs (i * 8)
       in do
         _ <- traverse go [0 .. n - 1]
         Right $! VS.generate n $ \i ->
           case go i of
             Right v -> v
             Left _  -> 0)
    ColDoubleMaybe

-- | True iff bit @i@ of a packed validity bitmap is set.
{-# INLINE validBitAt #-}
validBitAt :: VU.Vector Bit -> Int -> Bool
validBitAt v i = unBit (VU.unsafeIndex v i)

-- | Nullable variable-length readers all follow the same shape:
-- decode the validity bitmap into a packed VU.Vector Bit, build
-- the dense Utf8View / BinaryView / large variant directly from
-- the source body's offsets + data buffers (zero copy), then
-- wrap in the matching NullableXxxView. Per-row UTF-8
-- validation is deferred until access time (matches the
-- non-nullable Utf8View semantics).
readUtf8ColumnMaybe :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readUtf8ColumnMaybe endian len rb body !bufIdx !nodeIdx = do
  validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  offBs   <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  datBs   <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 2))
  validBits <- unpackBools len validBs
  if BS.length offBs < (len + 1) * 4
    then Left "Arrow.Column: UTF-8 offsets buffer too small"
    else
      let !offs = case endian of
            Little -> viewPrimVecLE 4 (len + 1) offBs
            _      -> bswapPrimVec32 (len + 1) offBs
          !view = AV.NullableUtf8View
            { AV.nuvValidity = validBits
            , AV.nuvValues   = Utf8View
                { uvOffsets = offs
                , uvData    = viewPrimVecLE 1 (BS.length datBs) datBs
                }
            }
      in Right (ColUtf8Maybe view, nodeIdx + 1, bufIdx + 3)

readBinaryColumnMaybe :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readBinaryColumnMaybe endian len rb body !bufIdx !nodeIdx = do
  validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  offBs   <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  datBs   <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 2))
  validBits <- unpackBools len validBs
  if BS.length offBs < (len + 1) * 4
    then Left "Arrow.Column: binary offsets buffer too small"
    else
      let !offs = case endian of
            Little -> viewPrimVecLE 4 (len + 1) offBs
            _      -> bswapPrimVec32 (len + 1) offBs
          !view = AV.NullableBinaryView
            { AV.nbvBinValidity = validBits
            , AV.nbvBinValues   = BinaryView
                { bvOffsets = offs
                , bvData    = viewPrimVecLE 1 (BS.length datBs) datBs
                }
            }
      in Right (ColBinaryMaybe view, nodeIdx + 1, bufIdx + 3)

readLargeUtf8ColumnMaybe :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readLargeUtf8ColumnMaybe endian len rb body !bufIdx !nodeIdx = do
  validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  offBs   <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  datBs   <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 2))
  validBits <- unpackBools len validBs
  if BS.length offBs < (len + 1) * 8
    then Left "Arrow.Column: large UTF-8 offsets buffer too small"
    else
      let !offs = case endian of
            Little -> viewPrimVecLE 8 (len + 1) offBs
            _      -> bswapPrimVec64 (len + 1) offBs
          !view = AV.NullableLargeUtf8View
            { AV.nluvValidity = validBits
            , AV.nluvValues   = LargeUtf8View
                { luvOffsets = offs
                , luvData    = viewPrimVecLE 1 (BS.length datBs) datBs
                }
            }
      in Right (ColLargeUtf8Maybe view, nodeIdx + 1, bufIdx + 3)

readLargeBinaryColumnMaybe :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readLargeBinaryColumnMaybe endian len rb body !bufIdx !nodeIdx = do
  validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  offBs   <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  datBs   <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 2))
  validBits <- unpackBools len validBs
  if BS.length offBs < (len + 1) * 8
    then Left "Arrow.Column: large binary offsets buffer too small"
    else
      let !offs = case endian of
            Little -> viewPrimVecLE 8 (len + 1) offBs
            _      -> bswapPrimVec64 (len + 1) offBs
          !view = AV.NullableLargeBinaryView
            { AV.nlbvValidity = validBits
            , AV.nlbvValues   = LargeBinaryView
                { lbvOffsets = offs
                , lbvData    = viewPrimVecLE 1 (BS.length datBs) datBs
                }
            }
      in Right (ColLargeBinaryMaybe view, nodeIdx + 1, bufIdx + 3)

readFixedSizeBinaryColumnMaybe :: Int -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readFixedSizeBinaryColumnMaybe byteWidth len rb body !bufIdx !nodeIdx = do
  validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  validBits <- unpackBools len validBs
  if BS.length valsBs < len * byteWidth
    then Left "Arrow.Column: fixed-size binary values buffer too small"
    else
      let !values = V.generate len $ \i ->
            BS.take byteWidth (BS.drop (i * byteWidth) valsBs)
          !view = AV.NullableFixedSizeBinaryView
            { AV.nfsbvWidth    = byteWidth
            , AV.nfsbvValidity = validBits
            , AV.nfsbvValues   = values
            }
      in Right (ColFixedSizeBinaryMaybe view, nodeIdx + 1, bufIdx + 2)

readDate32ColumnMaybe :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readDate32ColumnMaybe endian =
  readPrimColumnMaybe 4 "date32"
    (\n bs -> Right $! VS.generate n (\i -> fromIntegral (readWord32 endian bs (i * 4)) :: Int32))
    ColDate32Maybe

readDate64ColumnMaybe :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readDate64ColumnMaybe endian =
  readPrimColumnMaybe 8 "date64"
    (\n bs -> Right $! VS.generate n (\i -> fromIntegral (readWord64 endian bs (i * 8)) :: Int64))
    ColDate64Maybe

readTime32ColumnMaybe :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readTime32ColumnMaybe endian =
  readPrimColumnMaybe 4 "time32"
    (\n bs -> Right $! VS.generate n (\i -> fromIntegral (readWord32 endian bs (i * 4)) :: Int32))
    ColTime32Maybe

readTime64ColumnMaybe :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readTime64ColumnMaybe endian =
  readPrimColumnMaybe 8 "time64"
    (\n bs -> Right $! VS.generate n (\i -> fromIntegral (readWord64 endian bs (i * 8)) :: Int64))
    ColTime64Maybe

readTimestampColumnMaybe :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readTimestampColumnMaybe endian =
  readPrimColumnMaybe 8 "timestamp"
    (\n bs -> Right $! VS.generate n (\i -> fromIntegral (readWord64 endian bs (i * 8)) :: Int64))
    ColTimestampMaybe

readDurationColumnMaybe :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readDurationColumnMaybe endian =
  readPrimColumnMaybe 8 "duration"
    (\n bs -> Right $! VS.generate n (\i -> fromIntegral (readWord64 endian bs (i * 8)) :: Int64))
    ColDurationMaybe

readDecimal128ColumnMaybe :: Int -> Int -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readDecimal128ColumnMaybe precision scale len rb body !bufIdx !nodeIdx = do
  validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  validBits <- unpackBools len validBs
  if BS.length valsBs < len * 16
    then Left "Arrow.Column: decimal128 values buffer too small"
    else do
      let vals = V.generate len $ \i ->
            if not (validBitAt validBits i)
              then BS.replicate 16 0
              else BS.take 16 (BS.drop (i * 16) valsBs)
      Right (ColDecimal128 precision scale vals, nodeIdx + 1, bufIdx + 2)

readDecimal256ColumnMaybe :: Int -> Int -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readDecimal256ColumnMaybe precision scale len rb body !bufIdx !nodeIdx = do
  validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  validBits <- unpackBools len validBs
  if BS.length valsBs < len * 32
    then Left "Arrow.Column: decimal256 values buffer too small"
    else do
      let vals = V.generate len $ \i ->
            if not (validBitAt validBits i)
              then BS.replicate 32 0
              else BS.take 32 (BS.drop (i * 32) valsBs)
      Right (ColDecimal256 precision scale vals, nodeIdx + 1, bufIdx + 2)

-- * Low-level primitives

sliceBuffer :: ByteString -> Buffer -> Either String ByteString
sliceBuffer body buf =
  let !o = fromIntegral (bufOffset buf) :: Int
      !l = fromIntegral (bufLength buf) :: Int
  in if o < 0 || l < 0 || o + l > BS.length body
    then Left "Arrow.Column: buffer slice out of range"
    else Right $! BS.take l (BS.drop o body)

-- | Unpack an Arrow / Parquet validity bitmap (LSB-first,
-- one bit per row) as a packed 'VU.Vector Bit'.
--
-- Was previously @V.Vector Bool@ (boxed; one pointer per
-- row). The packed form is the wire format directly, so
-- this is now a near-no-op: 'Bit.fromByteStringN' adopts
-- the bytes (currently with a copy; unit 5 of the views
-- migration makes it true zero-copy).
--
-- An empty bytestring means "all valid" (Arrow spec): the
-- producer is allowed to omit the validity buffer when
-- @null_count == 0@. We materialise a fully-set bitmap so
-- the caller doesn't have to special-case it.
unpackBools :: Int -> ByteString -> Either String (VU.Vector Bit)
unpackBools n bs
  | BS.length bs == 0 = Right $! VU.replicate n (Bit True)
  | otherwise =
      let need = (n + 7) `div` 8
      in if BS.length bs < need
        then Left "Arrow.Column: bool buffer too small"
        else Right $! Bit.fromByteStringN n bs

-- | Legacy 'V.Vector Bool' inflater for callers that haven't
-- migrated to the packed representation. Materialises one
-- 'Bool' per row.
unpackBoolsBoxed :: Int -> ByteString -> Either String (V.Vector Bool)
unpackBoolsBoxed n bs = do
  packed <- unpackBools n bs
  Right $! V.generate n (\i -> unBit (VU.unsafeIndex packed i))

readInts8 :: Int -> ByteString -> Either String (VS.Vector Int8)
readInts8 len bs
  | BS.length bs < len = Left "Arrow.Column: int8 buffer too small"
  | otherwise =
      Right $
        VS.generate len $ \i ->
          fromIntegral (BSU.unsafeIndex bs i) :: Int8

readInts16 :: Endianness -> Bool -> Int -> ByteString -> Either String (VS.Vector Int16)
readInts16 endian signed len bs
  | BS.length bs < len * 2 = Left "Arrow.Column: int16 buffer too small"
  | otherwise =
      Right $
        VS.generate len $ \i ->
          let v = readWord16 endian bs (i * 2)
          in if signed then fromIntegral (fromIntegral v :: Int16) else fromIntegral v

-- | Memcpy a slice of @bs@ into a freshly-allocated primitive
-- vector. Assumes the source bit pattern matches the host
-- (Arrow LE on x86_64 / aarch64), so each element is exactly
-- the same bit pattern in @bs@ and in the result vector.
--
-- Replaces the byte-by-byte readWord32 / readWord64 +
-- VS.generate inner loop in the LE path of every primitive
-- column reader; for a 100 k Int64 column that's one memcpy
-- vs ~800 k bounds-checked unsafeIndex + shift / OR ops.
{-# INLINE memcpyPrimVecLE #-}
-- | LE memcpy from a 'ByteString' into a fresh
-- 'VS.Vector'. Was previously a copy into a primitive byte
-- array; now allocates a 'ForeignPtr' so the result can be
-- handed back as a 'VS.Vector' that adopts it natively.
--
-- A future zero-copy variant ('viewPrimVecLE') skips the
-- copy entirely by adopting the source bytestring's own
-- 'ForeignPtr' — see 'docs/columnar-views-design.md' unit 5.
memcpyPrimVecLE
  :: forall a. Storable a
  => Int                 -- ^ element size in bytes
  -> Int                 -- ^ number of elements
  -> ByteString
  -> VS.Vector a
memcpyPrimVecLE !elemBytes !len bs
  | len <= 0  = VS.empty
  | otherwise = unsafePerformIO $
      BSU.unsafeUseAsCStringLen bs $ \(cstr, _) -> do
        let !nBytes = len * elemBytes
        fp <- FFP.mallocForeignPtrBytes nBytes
        FFP.withForeignPtr fp $ \dst ->
          BSI.memcpy (castPtr dst) (castPtr cstr) nBytes
        pure (VS.unsafeFromForeignPtr0 (FFP.castForeignPtr fp) len)

-- | Zero-copy view of an LE primitive buffer over the source
-- 'ByteString'. The returned vector shares the bytestring's
-- backing 'ForeignPtr'; lifetime is tied to the source.
{-# INLINE viewPrimVecLE #-}
viewPrimVecLE
  :: forall a. Storable a
  => Int                 -- ^ element size in bytes (currently only
                         --   used for bounds-check; the cast is
                         --   performed by Storable's own size).
  -> Int                 -- ^ number of elements
  -> ByteString
  -> VS.Vector a
viewPrimVecLE !_elemBytes !len bs
  | len <= 0  = VS.empty
  | otherwise =
      let (fp, off, _) = BSI.toForeignPtr bs
      in VS.unsafeFromForeignPtr (FFP.castForeignPtr (FFP.plusForeignPtr fp off))
                                 0 len

-- | Byte-swap memcpy: source is in the opposite endianness from
-- the host. For each element, swap its bytes during the copy.
-- Used by primitive readers when the Arrow schema declares
-- big-endian encoding (rare in practice, but the spec allows
-- it).
--
-- Implementation uses 'SIMD.bswap{16,32,64}Copy' which is a
-- C/SSSE3 'pshufb'-based kernel via SIMDe (so it's also fast
-- on ARM NEON). Roughly 3-4× faster than the per-element
-- byte-by-byte loop the previous BE path was using.
{-# INLINE bswapPrimVec #-}
bswapPrimVec
  :: forall a. Storable a
  => Int                                          -- ^ element size in bytes
  -> (Ptr Word8 -> Ptr Word8 -> Int -> IO ())     -- ^ bswap kernel
  -> Int                                          -- ^ number of elements
  -> ByteString
  -> VS.Vector a
bswapPrimVec !elemBytes !swapper !len bs
  | len <= 0  = VS.empty
  | otherwise = unsafePerformIO $
      BSU.unsafeUseAsCStringLen bs $ \(cstr, _) -> do
        let !nBytes = len * elemBytes
        fp <- FFP.mallocForeignPtrBytes nBytes
        FFP.withForeignPtr fp $ \dst ->
          swapper (castPtr dst) (castPtr cstr) len
        pure (VS.unsafeFromForeignPtr0 (FFP.castForeignPtr fp) len)

-- | Specialisations for the three element sizes we read in the
-- BE path. The kernel is selected once and inlined into the
-- caller.
{-# INLINE bswapPrimVec16 #-}
bswapPrimVec16 :: Storable a => Int -> ByteString -> VS.Vector a
bswapPrimVec16 = bswapPrimVec 2 SIMD.bswap16Copy

{-# INLINE bswapPrimVec32 #-}
bswapPrimVec32 :: Storable a => Int -> ByteString -> VS.Vector a
bswapPrimVec32 = bswapPrimVec 4 SIMD.bswap32Copy

{-# INLINE bswapPrimVec64 #-}
bswapPrimVec64 :: Storable a => Int -> ByteString -> VS.Vector a
bswapPrimVec64 = bswapPrimVec 8 SIMD.bswap64Copy

readInts32 :: Endianness -> Bool -> Int -> ByteString -> Either String (VS.Vector Int32)
readInts32 endian _signed len bs
  | BS.length bs < len * 4 = Left "Arrow.Column: int32 buffer too small"
  | endian == Little =
      -- Fast path: bit pattern matches the host. signed/unsigned
      -- distinguish only in how the bits are interpreted, not
      -- in storage; either way the underlying ByteArray is the
      -- same memcpy.
      Right (viewPrimVecLE 4 len bs)
  | otherwise =
      -- Big endian source: SIMDe pshufb byte-swap into the
      -- destination vector.
      Right (bswapPrimVec32 len bs)

readInts64 :: Endianness -> Bool -> Int -> ByteString -> Either String (VS.Vector Int64)
readInts64 endian _signed len bs
  | BS.length bs < len * 8 = Left "Arrow.Column: int64 buffer too small"
  | endian == Little = Right (viewPrimVecLE 8 len bs)
  | otherwise        = Right (bswapPrimVec64 len bs)

int32FromWord :: Word32 -> Int32
int32FromWord w = fromIntegral w

int64FromWord :: Word64 -> Int64
int64FromWord w = fromIntegral w

readWord16 :: Endianness -> ByteString -> Int -> Word16
readWord16 Little = readLE16
readWord16 Big = readBE16

readWord32 :: Endianness -> ByteString -> Int -> Word32
readWord32 Little = readLE32
readWord32 Big = readBE32

readWord64 :: Endianness -> ByteString -> Int -> Word64
readWord64 Little = readLE64
readWord64 Big = readBE64

readLE16 :: ByteString -> Int -> Word16
readLE16 bs off =
  let b0 = fromIntegral (BSU.unsafeIndex bs off) :: Word16
      b1 = fromIntegral (BSU.unsafeIndex bs (off + 1)) :: Word16
  in b0 .|. (b1 `shiftL` 8)

readBE16 :: ByteString -> Int -> Word16
readBE16 bs off =
  let b0 = fromIntegral (BSU.unsafeIndex bs off) :: Word16
      b1 = fromIntegral (BSU.unsafeIndex bs (off + 1)) :: Word16
  in (b0 `shiftL` 8) .|. b1

readLE32 :: ByteString -> Int -> Word32
readLE32 bs off =
  let b0 = fromIntegral (BSU.unsafeIndex bs off) :: Word32
      b1 = fromIntegral (BSU.unsafeIndex bs (off + 1)) :: Word32
      b2 = fromIntegral (BSU.unsafeIndex bs (off + 2)) :: Word32
      b3 = fromIntegral (BSU.unsafeIndex bs (off + 3)) :: Word32
  in b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)

readBE32 :: ByteString -> Int -> Word32
readBE32 bs off =
  let b0 = fromIntegral (BSU.unsafeIndex bs off) :: Word32
      b1 = fromIntegral (BSU.unsafeIndex bs (off + 1)) :: Word32
      b2 = fromIntegral (BSU.unsafeIndex bs (off + 2)) :: Word32
      b3 = fromIntegral (BSU.unsafeIndex bs (off + 3)) :: Word32
  in (b0 `shiftL` 24) .|. (b1 `shiftL` 16) .|. (b2 `shiftL` 8) .|. b3

readLE64 :: ByteString -> Int -> Word64
readLE64 bs off =
  let rd i = fromIntegral (BSU.unsafeIndex bs (off + i)) :: Word64
  in rd 0 .|. (rd 1 `shiftL` 8) .|. (rd 2 `shiftL` 16) .|. (rd 3 `shiftL` 24)
    .|. (rd 4 `shiftL` 32) .|. (rd 5 `shiftL` 40) .|. (rd 6 `shiftL` 48) .|. (rd 7 `shiftL` 56)

readBE64 :: ByteString -> Int -> Word64
readBE64 bs off =
  let rd i = fromIntegral (BSU.unsafeIndex bs (off + i)) :: Word64
  in (rd 0 `shiftL` 56) .|. (rd 1 `shiftL` 48) .|. (rd 2 `shiftL` 40) .|. (rd 3 `shiftL` 32)
    .|. (rd 4 `shiftL` 24) .|. (rd 5 `shiftL` 16) .|. (rd 6 `shiftL` 8) .|. rd 7

readI32 :: Endianness -> ByteString -> Int -> Either String Int32
readI32 endian bs off =
  let w = readWord32 endian bs off
  in Right (int32FromWord w)

readF32 :: Endianness -> ByteString -> Int -> Either String Float
readF32 endian bs off =
  if off + 4 > BS.length bs
    then Left "Arrow.Column: float read OOB"
    else
      let w = readWord32 endian bs off
      in Right (castWord32ToFloat w)

readF64 :: Endianness -> ByteString -> Int -> Either String Double
readF64 endian bs off =
  if off + 8 > BS.length bs
    then Left "Arrow.Column: double read OOB"
    else
      let w = readWord64 endian bs off
      in Right (castWord64ToDouble w)

-- | Row count for a column array.
columnLength :: ColumnArray -> Int
columnLength = \case
  ColInt8 v -> VS.length v
  ColInt16 v -> VS.length v
  ColInt32 v -> VS.length v
  ColInt64 v -> VS.length v
  ColUInt8 v -> VS.length v
  ColUInt16 v -> VS.length v
  ColUInt32 v -> VS.length v
  ColUInt64 v -> VS.length v
  ColFloat16 v -> VS.length v
  ColFloat v -> VS.length v
  ColDouble v -> VS.length v
  ColBool v -> VU.length v
  ColUtf8 v -> AV.vLength v
  ColBinary v -> AV.vLength v
  ColLargeUtf8 v -> AV.vLength v
  ColLargeBinary v -> AV.vLength v
  ColFixedSizeBinary _ v -> V.length v
  ColDate32 v -> VS.length v
  ColDate64 v -> VS.length v
  ColTime32 v -> VS.length v
  ColTime64 v -> VS.length v
  ColTimestamp v -> VS.length v
  ColDuration v -> VS.length v
  ColDecimal128 _ _ v -> V.length v
  ColDecimal256 _ _ v -> V.length v
  ColInt8Maybe   v -> nvLength v
  ColInt16Maybe  v -> nvLength v
  ColInt32Maybe  v -> nvLength v
  ColInt64Maybe  v -> nvLength v
  ColUInt8Maybe  v -> nvLength v
  ColUInt16Maybe v -> nvLength v
  ColUInt32Maybe v -> nvLength v
  ColUInt64Maybe v -> nvLength v
  ColFloat16Maybe v -> nvLength v
  ColFloatMaybe  v -> nvLength v
  ColDoubleMaybe v -> nvLength v
  ColBoolMaybe v        -> AV.nbvLength v
  ColUtf8Maybe v        -> AV.vLength (AV.nuvValues v)
  ColBinaryMaybe v      -> AV.vLength (AV.nbvBinValues v)
  ColLargeUtf8Maybe v   -> AV.vLength (AV.nluvValues v)
  ColLargeBinaryMaybe v -> AV.vLength (AV.nlbvValues v)
  ColFixedSizeBinaryMaybe v -> V.length (AV.nfsbvValues v)
  ColDate32Maybe    v -> nvLength v
  ColDate64Maybe    v -> nvLength v
  ColTime32Maybe    v -> nvLength v
  ColTime64Maybe    v -> nvLength v
  ColTimestampMaybe v -> nvLength v
  ColDurationMaybe  v -> nvLength v
  ColStruct children -> if V.null children then 0 else columnLength (snd (V.head children))
  ColStructMaybe v _ -> VU.length v
  ColList offsets _ -> max 0 (VS.length offsets - 1)
  ColListMaybe v _ _ -> VU.length v
  ColLargeList offsets _ -> max 0 (VS.length offsets - 1)
  ColLargeListMaybe v _ _ -> VU.length v
  ColIntervalYearMonth v -> VS.length v
  ColIntervalDayTime d _ -> VS.length d
  ColIntervalMonthDayNano m _ _ -> VS.length m
  -- FixedSizeList<n> has parent length = child length / n
  -- (each row consumes exactly n child elements). The
  -- previous formula returned child length which made the
  -- record batch's @length@ field 'n' times larger than the
  -- actual row count and any downstream reader rejected the
  -- batch ("Array length did not match record batch length").
  ColFixedSizeList n child
    | n > 0     -> columnLength child `quot` n
    | otherwise -> 0
  ColFixedSizeListMaybe _ v _ -> VU.length v
  ColMap offsets _ _ -> max 0 (VS.length offsets - 1)
  ColMapMaybe v _ _ _ -> VU.length v
  ColDenseUnion typeIds _ _ -> VS.length typeIds
  ColSparseUnion typeIds _ -> VS.length typeIds
  ColDictionary _ indices _ -> VS.length indices
  ColRunEndEncoded runEnds _ ->
    -- The logical length is the LAST run-end value (exclusive).
    case runEnds of
      ColInt16 v -> if VS.null v then 0 else fromIntegral (VS.last v)
      ColInt32 v -> if VS.null v then 0 else fromIntegral (VS.last v)
      ColInt64 v -> if VS.null v then 0 else fromIntegral (VS.last v)
      _          -> 0
  ColListView offsets _ _       -> VS.length offsets
  ColListViewMaybe v _ _ _      -> VU.length v
  ColLargeListView offsets _ _  -> VS.length offsets
  ColLargeListViewMaybe v _ _ _ -> VU.length v
  ColUtf8View v        -> V.length v
  ColUtf8ViewMaybe v   -> V.length v
  ColBinaryView v      -> V.length v
  ColBinaryViewMaybe v -> V.length v
  ColNull n            -> n

-- | Materialize a record batch with support for nested types.
-- Walks the schema tree in preorder DFS, consuming field nodes and buffers.
materializeRecordBatch :: Schema -> RecordBatchDef -> ByteString -> Either String (V.Vector ColumnArray)
materializeRecordBatch schema rb body = do
  let bodyLen = fromIntegral (BS.length body) :: Int64
  if not (validateRecordBatchBuffers rb bodyLen)
    then Left "Arrow.Column: invalid buffer bounds in RecordBatchDef"
    else do
      let !viewVarCounts = computeViewVariadicMap (arrowFields schema) (rbVariadicBufferCounts rb)
      (cols, _, _) <- materializeFieldsR' (arrowEndianness schema) viewVarCounts (arrowFields schema) rb body 0 0
      Right cols

-- | Build a per-nodeIdx map of variadic-buffer counts for view
-- columns, by walking the schema in DFS pre-order alongside the
-- @rbVariadicBufferCounts@ vector. Field nodes are assigned
-- pre-order indices starting at 0; only @AUtf8View@ /
-- @ABinaryView@ fields consume an entry from the variadic vector.
computeViewVariadicMap
  :: V.Vector Field -> V.Vector Int64 -> V.Vector Int
computeViewVariadicMap topFields varCounts =
  -- We use a vector indexed by node id; size = total field nodes.
  let !total = sumNodes topFields
      !mp = VS.replicate total (-1) :: VS.Vector Int
      go (!ni, !vi, m) f =
        let m1 = case fieldType f of
              AUtf8View   -> assignVar ni vi m
              ABinaryView -> assignVar ni vi m
              _           -> m
            ni1 = ni + 1
            (ni', vi', m') = V.foldl' go (ni1, nextVi vi (fieldType f), m1) (fieldChildren f)
        in (ni', vi', m')
      assignVar ni vi m =
        let !c = case varCounts V.!? vi of
                   Just v  -> fromIntegral v
                   Nothing -> 0
        in  m VS.// [(ni, c)]
      nextVi vi t = case t of
        AUtf8View   -> vi + 1
        ABinaryView -> vi + 1
        _           -> vi
      (_, _, !out) = V.foldl' go (0 :: Int, 0 :: Int, mp) topFields
  in  V.fromList (VS.toList out)
  where
    sumNodes :: V.Vector Field -> Int
    sumNodes fs = V.foldl' (\n f -> n + 1 + sumNodes (fieldChildren f)) 0 fs

-- | Same as 'materializeFieldsR' but threads the precomputed view
-- variadic-count vector.
materializeFieldsR' :: Endianness -> V.Vector Int -> V.Vector Field -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (V.Vector ColumnArray, Int, Int)
materializeFieldsR' endian viewMap fields rb body !nodeIdx0 !bufIdx0 =
  go 0 nodeIdx0 bufIdx0 []
  where
    go !i !ni !bi !acc
      | i >= V.length fields = Right (V.fromList (reverse acc), ni, bi)
      | otherwise = do
          (col, ni', bi') <- materializeField' endian viewMap (V.unsafeIndex fields i) rb body ni bi
          go (i + 1) ni' bi' (col : acc)

materializeField' :: Endianness -> V.Vector Int -> Field -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
materializeField' endian viewMap f rb body !nodeIdx !bufIdx =
  case fieldDictionary f of
    -- Dictionary-encoded column: the on-wire payload is the index
    -- column (type given by 'deIndexType'); the values are
    -- supplied separately via a 'DictBatch'. We materialise the
    -- indices here and stash a placeholder value column; resolving
    -- against a real dictionary is the caller's job (typically via
    -- 'resolveDictionaryColumn').
    Just (DictionaryEncoding did indexTy _) ->
      materializeDictIndices endian did indexTy f rb body nodeIdx bufIdx
    Nothing -> case fieldType f of
      AStruct           -> materializeStruct endian f rb body nodeIdx bufIdx
      AList             -> materializeListCol endian f rb body nodeIdx bufIdx
      AMap _            -> materializeMapCol endian f rb body nodeIdx bufIdx
      AUnion mode _     -> materializeUnionCol endian f mode rb body nodeIdx bufIdx
      AFixedSizeList n  -> materializeFixedSizeListCol endian n f rb body nodeIdx bufIdx
      ALargeList        -> materializeLargeListCol endian f rb body nodeIdx bufIdx
      AInterval u       -> materializeIntervalCol endian u f rb body nodeIdx bufIdx
      ARunEndEncoded    -> materializeRunEndEncodedCol endian f rb body nodeIdx bufIdx
      AListView         -> materializeListViewCol endian False f rb body nodeIdx bufIdx
      ALargeListView    -> materializeListViewCol endian True  f rb body nodeIdx bufIdx
      AUtf8View         -> materializeViewCol' endian viewMap True  f rb body nodeIdx bufIdx
      ABinaryView       -> materializeViewCol' endian viewMap False f rb body nodeIdx bufIdx
      _                 -> materializeOne endian f rb body nodeIdx bufIdx

-- | Materialize the /indices/ portion of a dictionary-encoded
-- column. The values referenced by these indices live in a
-- separate 'DictionaryBatch' message keyed by @did@; combine via
-- 'resolveDictionaryColumn' once both parts are in hand.
materializeDictIndices
  :: Endianness -> Int64 -> ArrowType -> Field -> RecordBatchDef
  -> ByteString -> Int -> Int
  -> Either String (ColumnArray, Int, Int)
materializeDictIndices endian did indexTy f rb body !nodeIdx !bufIdx = do
  -- Read indices using a synthetic field that carries the index
  -- type (typically Int32) and the original nullability.
  let !indexField = f
        { fieldType = indexTy
        , fieldDictionary = Nothing
        , fieldChildren  = V.empty
        }
  (idxCol, !ni', !bi') <- materializeOne endian indexField rb body nodeIdx bufIdx
  -- Coerce whatever integer column we got into a primitive Int32
  -- for ColDictionary's storage. We support i8/i16/i32/i64
  -- (unsigned variants too).
  case toInt32Indices idxCol of
    Left e   -> Left e
    Right ix -> Right (ColDictionary did ix (placeholderColumn (fieldType f)), ni', bi')

-- | Convert any integer-typed column into the canonical
-- @VS.Vector Int32@ used by 'ColDictionary'.
toInt32Indices :: ColumnArray -> Either String (VS.Vector Int32)
toInt32Indices = \case
  ColInt8   v -> Right (VS.map fromIntegral v)
  ColInt16  v -> Right (VS.map fromIntegral v)
  ColInt32  v -> Right v
  ColInt64  v -> Right (VS.map fromIntegral v)
  ColUInt8  v -> Right (VS.map fromIntegral v)
  ColUInt16 v -> Right (VS.map fromIntegral v)
  ColUInt32 v -> Right (VS.map fromIntegral v)
  ColUInt64 v -> Right (VS.map fromIntegral v)
  -- Nullable variants: coerce nulls to -1 sentinel (callers can
  -- consult the matching validity buffer if needed).
  ColInt8Maybe   v -> Right $ nvToSentinel (-1) (fromIntegral :: Int8  -> Int32) v
  ColInt16Maybe  v -> Right $ nvToSentinel (-1) (fromIntegral :: Int16 -> Int32) v
  ColInt32Maybe  v -> Right $ nvToSentinel (-1) id v
  ColInt64Maybe  v -> Right $ nvToSentinel (-1) (fromIntegral :: Int64 -> Int32) v
  c -> Left $ "Arrow.Column: dictionary index column has non-integer type: " ++ show c

-- | Project a 'NullableView' to a dense 'VS.Vector' by
-- substituting @sentinel@ at every null slot. Useful when
-- the consumer doesn't care about which entries were null
-- (e.g. dictionary-index lookup with a -1 fallback).
{-# INLINE nvToSentinel #-}
nvToSentinel
  :: (Storable a, Storable b)
  => b -> (a -> b) -> NullableView a -> VS.Vector b
nvToSentinel sentinel f nv =
  let !n   = nvLength nv
      !val = nvValues nv
  in VS.generate n $ \i ->
       if nvIsPresent nv i
         then f (VS.unsafeIndex val i)
         else sentinel

-- | Replace the placeholder values column inside a @ColDictionary@
-- with the column materialised from a @DictBatch.dbData@. Walks
-- nested columns recursively so dictionary fields buried inside
-- struct / list / etc. parents are also resolved when the caller
-- supplies a lookup function.
resolveDictionaryColumn
  :: (Int64 -> Maybe ColumnArray)   -- ^ dictionary-id → values column
  -> ColumnArray
  -> ColumnArray
resolveDictionaryColumn lookupVals = go
  where
    go col = case col of
      ColDictionary did indices _placeholder ->
        case lookupVals did of
          Just vals -> ColDictionary did indices vals
          Nothing   -> col
      ColStruct cs            -> ColStruct (V.map (\(n,c) -> (n, go c)) cs)
      ColStructMaybe v cs     -> ColStructMaybe v (V.map (\(n,c) -> (n, go c)) cs)
      ColList offs c          -> ColList offs (go c)
      ColListMaybe v offs c   -> ColListMaybe v offs (go c)
      ColLargeList offs c     -> ColLargeList offs (go c)
      ColLargeListMaybe v offs c -> ColLargeListMaybe v offs (go c)
      ColFixedSizeList n c    -> ColFixedSizeList n (go c)
      ColFixedSizeListMaybe n v c -> ColFixedSizeListMaybe n v (go c)
      ColMap offs k v         -> ColMap offs (go k) (go v)
      ColMapMaybe vs offs k v -> ColMapMaybe vs offs (go k) (go v)
      ColDenseUnion ts offs cs -> ColDenseUnion ts offs (V.map go cs)
      ColSparseUnion ts cs    -> ColSparseUnion ts (V.map go cs)
      ColRunEndEncoded re vs  -> ColRunEndEncoded (go re) (go vs)
      ColListView offs sz c   -> ColListView offs sz (go c)
      ColListViewMaybe v offs sz c -> ColListViewMaybe v offs sz (go c)
      ColLargeListView offs sz c -> ColLargeListView offs sz (go c)
      ColLargeListViewMaybe v offs sz c -> ColLargeListViewMaybe v offs sz (go c)
      _ -> col

-- | A typed placeholder column for the dictionary values slot. The
-- caller is expected to call 'resolveDictionaryColumn' to fill it
-- in with the real values from a 'DictBatch'.
placeholderColumn :: ArrowType -> ColumnArray
placeholderColumn = \case
  AUtf8       -> ColUtf8       emptyUtf8View
  ABinary     -> ColBinary     emptyBinaryView
  ALargeUtf8  -> ColLargeUtf8  emptyLargeUtf8View
  ALargeBinary -> ColLargeBinary emptyLargeBinaryView
  AInt 8 True -> ColInt8 VS.empty
  AInt 8 False -> ColUInt8 VS.empty
  AInt 16 True -> ColInt16 VS.empty
  AInt 16 False -> ColUInt16 VS.empty
  AInt 32 True -> ColInt32 VS.empty
  AInt 32 False -> ColUInt32 VS.empty
  AInt 64 True -> ColInt64 VS.empty
  AInt 64 False -> ColUInt64 VS.empty
  AFloatingPoint Single -> ColFloat VS.empty
  AFloatingPoint DoublePrecision -> ColDouble VS.empty
  ABool -> ColBool VU.empty
  _    -> ColUtf8 emptyUtf8View   -- conservative fallback

-- | Zero-row Utf8View / BinaryView. Used by the dictionary
-- placeholder above and by callers that need an "absent"
-- column value.
emptyUtf8View :: Utf8View
emptyUtf8View = Utf8View (VS.singleton 0) VS.empty

emptyBinaryView :: BinaryView
emptyBinaryView = BinaryView (VS.singleton 0) VS.empty

emptyLargeUtf8View :: LargeUtf8View
emptyLargeUtf8View = LargeUtf8View (VS.singleton 0) VS.empty

emptyLargeBinaryView :: LargeBinaryView
emptyLargeBinaryView = LargeBinaryView (VS.singleton 0) VS.empty

-- | View materializer with precomputed variadic-count map; the
-- nodeIdx of the current field selects the entry.
materializeViewCol'
  :: Endianness -> V.Vector Int -> Bool -> Field -> RecordBatchDef
  -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
materializeViewCol' endian viewMap utf8 f rb body !nodeIdx !bufIdx = do
  let !varCount = case viewMap V.!? nodeIdx of
                    Just c | c >= 0 -> c
                    _               -> 0
  materializeViewColWithVar endian utf8 varCount f rb body nodeIdx bufIdx

materializeFieldsR :: Endianness -> V.Vector Field -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (V.Vector ColumnArray, Int, Int)
materializeFieldsR endian fields rb body !nodeIdx0 !bufIdx0 =
  go 0 nodeIdx0 bufIdx0 []
  where
    go !i !ni !bi !acc
      | i >= V.length fields = Right (V.fromList (reverse acc), ni, bi)
      | otherwise = do
          (col, ni', bi') <- materializeField endian (V.unsafeIndex fields i) rb body ni bi
          go (i + 1) ni' bi' (col : acc)

materializeField :: Endianness -> Field -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
materializeField endian f rb body !nodeIdx !bufIdx =
  case fieldType f of
    AStruct -> materializeStruct endian f rb body nodeIdx bufIdx
    AList -> materializeListCol endian f rb body nodeIdx bufIdx
    AMap _ -> materializeMapCol endian f rb body nodeIdx bufIdx
    AUnion mode _ -> materializeUnionCol endian f mode rb body nodeIdx bufIdx
    AFixedSizeList n -> materializeFixedSizeListCol endian n f rb body nodeIdx bufIdx
    ALargeList -> materializeLargeListCol endian f rb body nodeIdx bufIdx
    AInterval u -> materializeIntervalCol endian u f rb body nodeIdx bufIdx
    ARunEndEncoded -> materializeRunEndEncodedCol endian f rb body nodeIdx bufIdx
    AListView      -> materializeListViewCol endian False f rb body nodeIdx bufIdx
    ALargeListView -> materializeListViewCol endian True  f rb body nodeIdx bufIdx
    AUtf8View      -> materializeViewCol endian True  f rb body nodeIdx bufIdx
    ABinaryView    -> materializeViewCol endian False f rb body nodeIdx bufIdx
    _ -> materializeOne endian f rb body nodeIdx bufIdx

materializeStruct :: Endianness -> Field -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
materializeStruct endian f rb body !nodeIdx !bufIdx = do
  let node = V.unsafeIndex (rbNodes rb) nodeIdx
      !len = fromIntegral (fnLength node) :: Int
      !nodeIdx1 = nodeIdx + 1
  (validity, !bufIdx1) <- if fieldNullable f
    then do
      validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
      vs <- unpackBools len validBs
      Right (Just vs, bufIdx + 1)
    else Right (Nothing, bufIdx)
  (childCols, !nodeIdx2, !bufIdx2) <- materializeFieldsR endian (fieldChildren f) rb body nodeIdx1 bufIdx1
  let namedChildren = V.zipWith (\child col -> (fieldName child, col)) (fieldChildren f) childCols
  case validity of
    Nothing -> Right (ColStruct namedChildren, nodeIdx2, bufIdx2)
    Just vs -> Right (ColStructMaybe vs namedChildren, nodeIdx2, bufIdx2)

materializeListCol :: Endianness -> Field -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
materializeListCol endian f rb body !nodeIdx !bufIdx = do
  let node = V.unsafeIndex (rbNodes rb) nodeIdx
      !len = fromIntegral (fnLength node) :: Int
      !nodeIdx1 = nodeIdx + 1
  (validity, !bufIdx1) <- if fieldNullable f
    then do
      validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
      vs <- unpackBools len validBs
      Right (Just vs, bufIdx + 1)
    else Right (Nothing, bufIdx)
  offBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx1)
  if BS.length offBs < (len + 1) * 4
    then Left "Arrow.Column: list offsets buffer too small"
    else do
      let offsets = VS.generate (len + 1) $ \i ->
            fromIntegral (readWord32 endian offBs (i * 4)) :: Int32
          !bufIdx2 = bufIdx1 + 1
      if V.null (fieldChildren f)
        then Left "Arrow.Column: list field has no child"
        else do
          let childField = V.head (fieldChildren f)
          (childCol, !nodeIdx2, !bufIdx3) <- materializeField endian childField rb body nodeIdx1 bufIdx2
          case validity of
            Nothing -> Right (ColList offsets childCol, nodeIdx2, bufIdx3)
            Just vs -> Right (ColListMaybe vs offsets childCol, nodeIdx2, bufIdx3)

materializeMapCol :: Endianness -> Field -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
materializeMapCol endian f rb body !nodeIdx !bufIdx = do
  let node = V.unsafeIndex (rbNodes rb) nodeIdx
      !len = fromIntegral (fnLength node) :: Int
      !nodeIdx1 = nodeIdx + 1
  (validity, !bufIdx1) <- if fieldNullable f
    then do
      validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
      vs <- unpackBools len validBs
      Right (Just vs, bufIdx + 1)
    else Right (Nothing, bufIdx)
  offBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx1)
  if BS.length offBs < (len + 1) * 4
    then Left "Arrow.Column: map offsets buffer too small"
    else do
      let offsets = VS.generate (len + 1) $ \i ->
            fromIntegral (readWord32 endian offBs (i * 4)) :: Int32
          !bufIdx2 = bufIdx1 + 1
      if V.null (fieldChildren f)
        then Left "Arrow.Column: map field has no child"
        else do
          let structField = V.head (fieldChildren f)
          (structCol, !nodeIdx2, !bufIdx3) <- materializeField endian structField rb body nodeIdx1 bufIdx2
          case structCol of
            ColStruct children
              | V.length children >= 2 ->
                  let keyCol = snd (V.unsafeIndex children 0)
                      valCol = snd (V.unsafeIndex children 1)
                  in case validity of
                    Nothing -> Right (ColMap offsets keyCol valCol, nodeIdx2, bufIdx3)
                    Just vs -> Right (ColMapMaybe vs offsets keyCol valCol, nodeIdx2, bufIdx3)
            _ -> Left "Arrow.Column: map child is not a valid struct with key/value"

materializeUnionCol :: Endianness -> Field -> UnionMode -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
materializeUnionCol endian f mode rb body !nodeIdx !bufIdx = do
  let node = V.unsafeIndex (rbNodes rb) nodeIdx
      !len = fromIntegral (fnLength node) :: Int
      !nodeIdx1 = nodeIdx + 1
  typeIdsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  if BS.length typeIdsBs < len
    then Left "Arrow.Column: union type_ids buffer too small"
    else do
      let typeIds = VS.generate len $ \i -> fromIntegral (BSU.unsafeIndex typeIdsBs i) :: Int8
      case mode of
        Dense -> do
          offsetsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
          if BS.length offsetsBs < len * 4
            then Left "Arrow.Column: dense union offsets buffer too small"
            else do
              let offsets = VS.generate len $ \i ->
                    fromIntegral (readWord32 endian offsetsBs (i * 4)) :: Int32
                  !bufIdx1 = bufIdx + 2
              (children, !nodeIdx2, !bufIdx2) <- materializeFieldsR endian (fieldChildren f) rb body nodeIdx1 bufIdx1
              Right (ColDenseUnion typeIds offsets children, nodeIdx2, bufIdx2)
        Sparse -> do
          let !bufIdx1 = bufIdx + 1
          (children, !nodeIdx2, !bufIdx2) <- materializeFieldsR endian (fieldChildren f) rb body nodeIdx1 bufIdx1
          Right (ColSparseUnion typeIds children, nodeIdx2, bufIdx2)

materializeFixedSizeListCol :: Endianness -> Int -> Field -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
materializeFixedSizeListCol endian _listSize f rb body !nodeIdx !bufIdx = do
  let node = V.unsafeIndex (rbNodes rb) nodeIdx
      !len = fromIntegral (fnLength node) :: Int
      !nodeIdx1 = nodeIdx + 1
  (validity, !bufIdx1) <- if fieldNullable f
    then do
      validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
      vs <- unpackBools len validBs
      Right (Just vs, bufIdx + 1)
    else Right (Nothing, bufIdx)
  if V.null (fieldChildren f)
    then Left "Arrow.Column: fixed-size list has no child"
    else do
      let childField = V.head (fieldChildren f)
      (childCol, !nodeIdx2, !bufIdx2) <- materializeField endian childField rb body nodeIdx1 bufIdx1
      case validity of
        Nothing -> Right (ColFixedSizeList _listSize childCol, nodeIdx2, bufIdx2)
        Just vs -> Right (ColFixedSizeListMaybe _listSize vs childCol, nodeIdx2, bufIdx2)

materializeLargeListCol :: Endianness -> Field -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
materializeLargeListCol endian f rb body !nodeIdx !bufIdx = do
  let node = V.unsafeIndex (rbNodes rb) nodeIdx
      !len = fromIntegral (fnLength node) :: Int
      !nodeIdx1 = nodeIdx + 1
  (validity, !bufIdx1) <- if fieldNullable f
    then do
      validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
      vs <- unpackBools len validBs
      Right (Just vs, bufIdx + 1)
    else Right (Nothing, bufIdx)
  offBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx1)
  if BS.length offBs < (len + 1) * 8
    then Left "Arrow.Column: large list offsets buffer too small"
    else do
      let offsets = VS.generate (len + 1) $ \i ->
            fromIntegral (readWord64 endian offBs (i * 8)) :: Int64
          !bufIdx2 = bufIdx1 + 1
      if V.null (fieldChildren f)
        then Left "Arrow.Column: large list has no child"
        else do
          let childField = V.head (fieldChildren f)
          (childCol, !nodeIdx2, !bufIdx3) <- materializeField endian childField rb body nodeIdx1 bufIdx2
          case validity of
            Nothing -> Right (ColLargeList offsets childCol, nodeIdx2, bufIdx3)
            Just vs -> Right (ColLargeListMaybe vs offsets childCol, nodeIdx2, bufIdx3)

-- | Read one INTERVAL field. Interval columns are flat (one field
-- node, validity + data buffers) but the data layout depends on the
-- unit:
--
--   YearMonth     : 4 bytes per row, one i32 (months).
--   DayTime       : 8 bytes per row, pair of i32 (days, millis).
--   MonthDayNano  : 16 bytes per row, (i32 months, i32 days, i64 nanos).
materializeIntervalCol
  :: Endianness -> IntervalUnit -> Field -> RecordBatchDef -> ByteString
  -> Int -> Int -> Either String (ColumnArray, Int, Int)
materializeIntervalCol endian unit f rb body !nodeIdx !bufIdx = do
  let node = V.unsafeIndex (rbNodes rb) nodeIdx
      !len = fromIntegral (fnLength node) :: Int
      !nodeIdx1 = nodeIdx + 1
  (_validity, !bufIdx1) <- if fieldNullable f
    then do
      validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
      vs <- unpackBools len validBs
      Right (Just vs, bufIdx + 1)
    else Right (Nothing, bufIdx)
  dataBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx1)
  let !bufIdx2 = bufIdx1 + 1
  col <- case unit of
    YearMonth
      | BS.length dataBs < len * 4 ->
          Left "Arrow.Column: interval YEAR_MONTH buffer too small"
      | otherwise ->
          let vec = VS.generate len $ \i ->
                fromIntegral (readWord32 endian dataBs (i * 4)) :: Int32
           in Right (ColIntervalYearMonth vec)
    DayTime
      | BS.length dataBs < len * 8 ->
          Left "Arrow.Column: interval DAY_TIME buffer too small"
      | otherwise ->
          let daysV   = VS.generate len $ \i ->
                fromIntegral (readWord32 endian dataBs (i * 8)) :: Int32
              millisV = VS.generate len $ \i ->
                fromIntegral (readWord32 endian dataBs (i * 8 + 4)) :: Int32
           in Right (ColIntervalDayTime daysV millisV)
    MonthDayNano
      | BS.length dataBs < len * 16 ->
          Left "Arrow.Column: interval MONTH_DAY_NANO buffer too small"
      | otherwise ->
          let monthsV = VS.generate len $ \i ->
                fromIntegral (readWord32 endian dataBs (i * 16)) :: Int32
              daysV   = VS.generate len $ \i ->
                fromIntegral (readWord32 endian dataBs (i * 16 + 4)) :: Int32
              nanosV  = VS.generate len $ \i ->
                fromIntegral (readWord64 endian dataBs (i * 16 + 8)) :: Int64
           in Right (ColIntervalMonthDayNano monthsV daysV nanosV)
  Right (col, nodeIdx1, bufIdx2)

-- ============================================================
-- Post-V5 columns: RunEndEncoded, ListView/LargeListView,
-- Utf8View / BinaryView.
-- ============================================================

-- | RunEndEncoded: parent has zero buffers (no validity, no data),
-- exactly two children: @run_ends@ (Int16/32/64) and @values@ (any
-- type, may be nullable).
materializeRunEndEncodedCol
  :: Endianness -> Field -> RecordBatchDef -> ByteString
  -> Int -> Int -> Either String (ColumnArray, Int, Int)
materializeRunEndEncodedCol endian f rb body !nodeIdx !bufIdx = do
  let !nodeIdx1 = nodeIdx + 1
  case V.toList (fieldChildren f) of
    [runEndsField, valuesField] -> do
      (runEndsCol, !nodeIdx2, !bufIdx1) <-
        materializeField endian runEndsField rb body nodeIdx1 bufIdx
      (valuesCol,  !nodeIdx3, !bufIdx2) <-
        materializeField endian valuesField  rb body nodeIdx2 bufIdx1
      Right (ColRunEndEncoded runEndsCol valuesCol, nodeIdx3, bufIdx2)
    _ ->
      Left "Arrow.Column: RunEndEncoded must have exactly two children (run_ends, values)"

-- | ListView / LargeListView. Buffers (in order): validity (when
-- nullable), offsets, sizes. The child elements may overlap or
-- appear in any order — we don't reorder them at materialisation
-- time; callers can interpret the (offset, size) pairs themselves.
materializeListViewCol
  :: Endianness
  -> Bool   -- ^ True for LargeListView (Int64 offsets/sizes)
  -> Field
  -> RecordBatchDef
  -> ByteString
  -> Int -> Int
  -> Either String (ColumnArray, Int, Int)
materializeListViewCol endian large f rb body !nodeIdx !bufIdx = do
  let node = V.unsafeIndex (rbNodes rb) nodeIdx
      !len = fromIntegral (fnLength node) :: Int
      !nodeIdx1 = nodeIdx + 1
  (validity, !bufIdx1) <- if fieldNullable f
    then do
      validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
      vs <- unpackBools len validBs
      Right (Just vs, bufIdx + 1)
    else Right (Nothing, bufIdx)
  offBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx1)
  sizBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx1 + 1))
  let !w        = if large then 8 else 4
      !need     = len * w
  when' (BS.length offBs < need) $
    Left "Arrow.Column: list-view offsets buffer too small"
  when' (BS.length sizBs < need) $
    Left "Arrow.Column: list-view sizes buffer too small"
  let !bufIdx2 = bufIdx1 + 2
  case V.toList (fieldChildren f) of
    [childField] -> do
      (childCol, !nodeIdx2, !bufIdx3) <-
        materializeField endian childField rb body nodeIdx1 bufIdx2
      if large
        then
          let offs = VS.generate len $ \i ->
                fromIntegral (readWord64 endian offBs (i * 8)) :: Int64
              sizs = VS.generate len $ \i ->
                fromIntegral (readWord64 endian sizBs (i * 8)) :: Int64
          in Right $! case validity of
               Nothing -> (ColLargeListView offs sizs childCol, nodeIdx2, bufIdx3)
               Just vs -> (ColLargeListViewMaybe vs offs sizs childCol, nodeIdx2, bufIdx3)
        else
          let offs = VS.generate len $ \i ->
                fromIntegral (readWord32 endian offBs (i * 4)) :: Int32
              sizs = VS.generate len $ \i ->
                fromIntegral (readWord32 endian sizBs (i * 4)) :: Int32
          in Right $! case validity of
               Nothing -> (ColListView offs sizs childCol, nodeIdx2, bufIdx3)
               Just vs -> (ColListViewMaybe vs offs sizs childCol, nodeIdx2, bufIdx3)
    _ ->
      Left "Arrow.Column: ListView must have exactly one child"
  where
    when' True  e = e
    when' False _ = Right ()

-- | Utf8View / BinaryView. Buffers: validity (optional), view
-- (n × 16 bytes), then 0..k variadic data buffers. The variadic
-- count comes from 'rbVariadicBufferCounts' in pre-order schema
-- traversal — we consume one entry from a caller-managed counter
-- by re-deriving the count from the schema position; here we just
-- read the relevant data buffers based on the variadic-counts
-- vector slot for this field.
--
-- Each 16-byte view is laid out:
--
-- @
--   length         : i32 (little-endian)
--   if length <= 12:
--     inlined bytes (length bytes), zero-padded to 12 total
--   else:
--     prefix       : 4 bytes (first 4 of the string)
--     buffer_index : i32
--     buffer_offset: i32
-- @
materializeViewCol
  :: Endianness
  -> Bool   -- ^ True for Utf8View (UTF-8 validate); False for BinaryView
  -> Field
  -> RecordBatchDef
  -> ByteString
  -> Int -> Int
  -> Either String (ColumnArray, Int, Int)
materializeViewCol endian utf8 f rb body !nodeIdx !bufIdx =
  -- Falls back to the first variadic count entry when called
  -- without a per-view cursor (single-view-column batches). The
  -- top-level 'materializeRecordBatch' uses the cursor-aware
  -- 'materializeViewColWithVar' via 'computeViewVariadicMap'.
  let !varCount = case V.toList (rbVariadicBufferCounts rb) of
                    []      -> 0
                    (c:_)   -> fromIntegral c :: Int
  in  materializeViewColWithVar endian utf8 varCount f rb body nodeIdx bufIdx

-- | Like 'materializeViewCol' but takes an explicit
-- variadic-buffer count for this column, sourced from
-- 'rbVariadicBufferCounts' at the right per-view-column position.
materializeViewColWithVar
  :: Endianness
  -> Bool
  -> Int
  -> Field
  -> RecordBatchDef
  -> ByteString
  -> Int -> Int
  -> Either String (ColumnArray, Int, Int)
materializeViewColWithVar endian utf8 varCount f rb body !nodeIdx !bufIdx = do
  let node = V.unsafeIndex (rbNodes rb) nodeIdx
      !len = fromIntegral (fnLength node) :: Int
      !nodeIdx1 = nodeIdx + 1
  (validity, !bufIdx1) <- if fieldNullable f
    then do
      validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
      vs <- unpackBools len validBs
      Right (Just vs, bufIdx + 1)
    else Right (Nothing, bufIdx)
  viewBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx1)
  when' (BS.length viewBs < len * 16) $
    Left "Arrow.Column: view buffer too small"
  -- Resolve data buffers
  dataBufs <- mapM (sliceBuffer body)
                [ V.unsafeIndex (rbBuffers rb) (bufIdx1 + 1 + i)
                | i <- [0 .. varCount - 1] ]
  let !bufIdx2 = bufIdx1 + 1 + varCount
  rows <- forM [0 .. len - 1] $ \i -> resolveView viewBs dataBufs i
  let nullableRows = case validity of
        Nothing -> Nothing
        Just vs -> Just $ V.fromListN (length rows)
          [ if validBitAt vs i then Just r else Nothing
          | (i, r) <- zip [0 :: Int ..] rows
          ]
  -- Decode UTF-8 if the column is Utf8View; raw bytes otherwise.
  result <- if utf8
    then do
      decoded <- traverse (decodeUtf8' "Arrow.Column: invalid UTF-8 in Utf8View") rows
      case nullableRows of
        Nothing -> Right (ColUtf8View (V.fromList decoded))
        Just vs ->
          let mkMaybe (Just bs) = Just <$> decodeUtf8' "Arrow.Column: invalid UTF-8 in Utf8View" bs
              mkMaybe Nothing   = Right Nothing
          in case traverse mkMaybe (V.toList vs) of
               Left e   -> Left e
               Right rs -> Right (ColUtf8ViewMaybe (V.fromList rs))
    else
      pure $ case nullableRows of
        Nothing -> ColBinaryView (V.fromList rows)
        Just vs -> ColBinaryViewMaybe vs
  Right (result, nodeIdx1, bufIdx2)
  where
    when' True  e = e
    when' False _ = Right ()
    decodeUtf8' err bs = case TE.decodeUtf8' bs of
      Left _  -> Left err
      Right t -> Right t

resolveView :: ByteString -> [ByteString] -> Int -> Either String ByteString
resolveView viewBs dataBufs i =
  let off = i * 16
      len = fromIntegral (readLE32 viewBs off) :: Int
  in  if len <= 12
        then Right $! BS.take len (BS.drop (off + 4) viewBs)
        else do
          let bufIdx = fromIntegral (readLE32 viewBs (off + 8)) :: Int
              bufOff = fromIntegral (readLE32 viewBs (off + 12)) :: Int
          case dataBufs `safeIx` bufIdx of
            Nothing -> Left "Arrow.Column: view references unknown data buffer index"
            Just db ->
              if bufOff + len > BS.length db
                then Left "Arrow.Column: view payload out of range"
                else Right $! BS.take len (BS.drop bufOff db)

safeIx :: [a] -> Int -> Maybe a
safeIx xs i
  | i < 0 = Nothing
  | otherwise = case drop i xs of { (x:_) -> Just x; [] -> Nothing }

-- ============================================================
-- Row slicing
-- ============================================================

-- | Take @len@ rows starting at @start@ from a 'ColumnArray'.
--
-- For the simple primitive shapes (int / float / bool / utf8 /
-- binary / temporal / decimal / fixed-binary, plus their
-- @*Maybe@ variants) the slice is structural — no copying of
-- the underlying primitive vectors when 'V.slice' / 'VS.slice'
-- can do it. For nested shapes (struct / list / map / union /
-- dictionary / view / REE) the slice recurses into children
-- where it has a meaningful definition; constructors with
-- shared offsets that don't naturally support contiguous
-- slicing (RunEndEncoded, ListView, etc.) return the original
-- column unchanged so the caller falls back to per-row
-- iteration.
--
-- @start@ + @len@ are clamped to the column's logical length;
-- a negative @start@ becomes @0@; a @len@ that runs past the
-- end is truncated.
sliceColumnArray :: Int -> Int -> ColumnArray -> ColumnArray
sliceColumnArray !start0 !len0 col =
  let !n      = columnLength col
      !start  = max 0 start0
      !len    = max 0 (min len0 (n - start))
  in if len == n && start == 0
       then col
       else go start len col
  where
    go s l = \case
      ColInt8 v       -> ColInt8 (VS.slice s l v)
      ColInt16 v      -> ColInt16 (VS.slice s l v)
      ColInt32 v      -> ColInt32 (VS.slice s l v)
      ColInt64 v      -> ColInt64 (VS.slice s l v)
      ColUInt8 v      -> ColUInt8 (VS.slice s l v)
      ColUInt16 v     -> ColUInt16 (VS.slice s l v)
      ColUInt32 v     -> ColUInt32 (VS.slice s l v)
      ColUInt64 v     -> ColUInt64 (VS.slice s l v)
      ColFloat16 v    -> ColFloat16 (VS.slice s l v)
      ColFloat v      -> ColFloat (VS.slice s l v)
      ColDouble v     -> ColDouble (VS.slice s l v)
      ColBool v       -> ColBool (VU.slice s l v)
      ColUtf8 v       -> ColUtf8 (AV.vSlice s l v)
      ColBinary v     -> ColBinary (AV.vSlice s l v)
      ColLargeUtf8 v  -> ColLargeUtf8 (AV.vSlice s l v)
      ColLargeBinary v -> ColLargeBinary (AV.vSlice s l v)
      ColFixedSizeBinary w v -> ColFixedSizeBinary w (V.slice s l v)
      ColDate32 v     -> ColDate32 (VS.slice s l v)
      ColDate64 v     -> ColDate64 (VS.slice s l v)
      ColTime32 v     -> ColTime32 (VS.slice s l v)
      ColTime64 v     -> ColTime64 (VS.slice s l v)
      ColTimestamp v  -> ColTimestamp (VS.slice s l v)
      ColDuration v   -> ColDuration (VS.slice s l v)
      ColDecimal128 p sc v -> ColDecimal128 p sc (V.slice s l v)
      ColDecimal256 p sc v -> ColDecimal256 p sc (V.slice s l v)
      ColIntervalYearMonth v ->
        ColIntervalYearMonth (VS.slice s l v)
      ColIntervalDayTime d m ->
        ColIntervalDayTime (VS.slice s l d) (VS.slice s l m)
      ColIntervalMonthDayNano m d nano ->
        ColIntervalMonthDayNano
          (VS.slice s l m) (VS.slice s l d) (VS.slice s l nano)
      ColInt8Maybe v  -> ColInt8Maybe  (sliceNV s l v)
      ColInt16Maybe v -> ColInt16Maybe (sliceNV s l v)
      ColInt32Maybe v -> ColInt32Maybe (sliceNV s l v)
      ColInt64Maybe v -> ColInt64Maybe (sliceNV s l v)
      ColUInt8Maybe v -> ColUInt8Maybe (sliceNV s l v)
      ColUInt16Maybe v -> ColUInt16Maybe (sliceNV s l v)
      ColUInt32Maybe v -> ColUInt32Maybe (sliceNV s l v)
      ColUInt64Maybe v -> ColUInt64Maybe (sliceNV s l v)
      ColFloat16Maybe v -> ColFloat16Maybe (sliceNV s l v)
      ColFloatMaybe v -> ColFloatMaybe (sliceNV s l v)
      ColDoubleMaybe v -> ColDoubleMaybe (sliceNV s l v)
      -- Nullable variable-length views: a true row-slice
      -- needs to carve both the validity bitmap and the
      -- underlying view (offsets + data). For now we slice
      -- the validity bitmap and the values' wrapped view
      -- (Utf8View / BinaryView / large variants slice their
      -- offsets correctly — the data buffer is shared and
      -- a sub-range carves a sub-range of offsets at the
      -- corresponding index window). FixedSizeBinary keeps
      -- the same V.Vector ByteString slice it always had.
      ColBoolMaybe v ->
        ColBoolMaybe v
          { AV.nbvValidity = VU.slice s l (AV.nbvValidity v)
          , AV.nbvValues   = VU.slice s l (AV.nbvValues v)
          }
      ColUtf8Maybe v ->
        ColUtf8Maybe v
          { AV.nuvValidity = VU.slice s l (AV.nuvValidity v)
          , AV.nuvValues   = AV.vSlice s l (AV.nuvValues v)
          }
      ColBinaryMaybe v ->
        ColBinaryMaybe v
          { AV.nbvBinValidity = VU.slice s l (AV.nbvBinValidity v)
          , AV.nbvBinValues   = AV.vSlice s l (AV.nbvBinValues v)
          }
      ColLargeUtf8Maybe v ->
        ColLargeUtf8Maybe v
          { AV.nluvValidity = VU.slice s l (AV.nluvValidity v)
          , AV.nluvValues   = AV.vSlice s l (AV.nluvValues v)
          }
      ColLargeBinaryMaybe v ->
        ColLargeBinaryMaybe v
          { AV.nlbvValidity = VU.slice s l (AV.nlbvValidity v)
          , AV.nlbvValues   = AV.vSlice s l (AV.nlbvValues v)
          }
      ColFixedSizeBinaryMaybe v ->
        ColFixedSizeBinaryMaybe v
          { AV.nfsbvValidity = VU.slice s l (AV.nfsbvValidity v)
          , AV.nfsbvValues   = V.slice s l (AV.nfsbvValues v)
          }
      ColDate32Maybe v -> ColDate32Maybe (sliceNV s l v)
      ColDate64Maybe v -> ColDate64Maybe (sliceNV s l v)
      ColTime32Maybe v -> ColTime32Maybe (sliceNV s l v)
      ColTime64Maybe v -> ColTime64Maybe (sliceNV s l v)
      ColTimestampMaybe v -> ColTimestampMaybe (sliceNV s l v)
      ColDurationMaybe v -> ColDurationMaybe (sliceNV s l v)
      ColUtf8View v       -> ColUtf8View (V.slice s l v)
      ColUtf8ViewMaybe v  -> ColUtf8ViewMaybe (V.slice s l v)
      ColBinaryView v     -> ColBinaryView (V.slice s l v)
      ColBinaryViewMaybe v -> ColBinaryViewMaybe (V.slice s l v)
      -- Struct: slice every child column at the same offset.
      ColStruct children ->
        ColStruct (V.map (\(nm, c) -> (nm, sliceColumnArray s l c)) children)
      ColStructMaybe valid children ->
        ColStructMaybe (VU.slice s l valid)
          (V.map (\(nm, c) -> (nm, sliceColumnArray s l c)) children)
      ColNull _ -> ColNull l
      -- Variable-length / nested with shared-buffer semantics:
      -- offsets index into a flat child column. Slicing the
      -- offsets vector here keeps the child intact (which is the
      -- right semantics: an offset slice carves out the
      -- corresponding sub-range of the child without copying).
      ColList offsets child ->
        ColList (VS.slice s (l + 1) offsets) child
      ColLargeList offsets child ->
        ColLargeList (VS.slice s (l + 1) offsets) child
      ColListMaybe valid offsets child ->
        ColListMaybe (VU.slice s l valid)
          (VS.slice s (l + 1) offsets) child
      ColLargeListMaybe valid offsets child ->
        ColLargeListMaybe (VU.slice s l valid)
          (VS.slice s (l + 1) offsets) child
      ColMap offsets ks vs ->
        ColMap (VS.slice s (l + 1) offsets) ks vs
      ColMapMaybe valid offsets ks vs ->
        ColMapMaybe (VU.slice s l valid)
          (VS.slice s (l + 1) offsets) ks vs
      -- Constructors whose physical layout doesn't admit a
      -- straightforward contiguous slice (offsets aren't
      -- monotonic, run-ends carry counts not row indices, etc.)
      -- return the input unchanged. Callers that need exact
      -- per-row slicing can fall back to the per-row iteration
      -- shape from Arrow.Record.
      other -> other

-- ============================================================
-- Map invariants
-- ============================================================

-- | Check that a 'ColMap' / 'ColMapMaybe' column actually
-- satisfies its 'AMap' field's @keysSorted@ flag — i.e. that
-- within every entry slice the keys are non-decreasing.
--
-- The Arrow spec lets readers assume key-sorted order when
-- @keysSorted = True@ (used for binary-search lookups, hash
-- avoidance, etc.). Writers that set the flag without sorting
-- the keys silently corrupt every downstream join / lookup.
-- This helper lets a writer assert the invariant before
-- emission and lets a reader decide whether to trust the flag.
--
-- Returns 'Right ()' if every entry's keys are sorted under
-- the column's natural ordering. 'Left' carries the index of
-- the first offending entry plus the offending key pair.
validateMapKeysSorted :: ColumnArray -> Either String ()
validateMapKeysSorted = \case
  ColMap offs keys _vals -> goOffsets offs keys
  ColMapMaybe _ offs keys _vals -> goOffsets offs keys
  _ -> Right ()
  where
    goOffsets offs keys =
      let !n = max 0 (VS.length offs - 1)
          go !i
            | i >= n = Right ()
            | otherwise =
                let !s = fromIntegral (VS.unsafeIndex offs i) :: Int
                    !e = fromIntegral (VS.unsafeIndex offs (i + 1)) :: Int
                    !w = e - s
                in case checkSlice keys s w of
                     Just (j, k1, k2) ->
                       Left $ "Arrow.Column.validateMapKeysSorted: entry "
                                ++ show i ++ " key " ++ show j
                                ++ " " ++ k1 ++ " > " ++ k2
                     Nothing -> go (i + 1)
      in go 0

    -- We can't compare arbitrary boxed values without an Ord
    -- instance, but ColumnArray's underlying vectors are
    -- comparable by Show — that's fine here because this is a
    -- /diagnostic/ helper, not a hot-path validator. Real
    -- key-by-key comparison would need to dispatch on the
    -- key column's constructor.
    checkSlice keys s w =
      case keys of
        ColUtf8 v   -> compareSliceShow s w (utf8ViewToVector v)
        ColUtf8Maybe v -> compareSliceShow s w
                            (AV.nullableUtf8ViewToMaybeVector v)
        ColInt32 v -> compareSlice s w v
        ColInt64 v -> compareSlice s w v
        ColUInt32 v -> compareSlice s w v
        ColUInt64 v -> compareSlice s w v
        _ -> Nothing  -- unsupported key type; assume sorted

    -- Materialise the view's text as a boxed vector for the
    -- diagnostic compareSliceShow call. Re-decoding per row
    -- is fine here — this is a sortedness check, not a hot
    -- path.
    utf8ViewToVector v = AV.utf8ViewToVector v

    compareSlice :: (Ord a, Storable a, Show a)
                 => Int -> Int -> VS.Vector a -> Maybe (Int, String, String)
    compareSlice s w v
      | w < 2 = Nothing
      | otherwise =
          let go !j
                | j >= w = Nothing
                | otherwise =
                    let !a = VS.unsafeIndex v (s + j - 1)
                        !b = VS.unsafeIndex v (s + j)
                    in if a > b
                         then Just (j, show a, show b)
                         else go (j + 1)
          in go 1

    compareSliceShow :: (Ord a, Show a)
                     => Int -> Int -> V.Vector a -> Maybe (Int, String, String)
    compareSliceShow s w v
      | w < 2 = Nothing
      | otherwise =
          let go !j
                | j >= w = Nothing
                | otherwise =
                    let !a = V.unsafeIndex v (s + j - 1)
                        !b = V.unsafeIndex v (s + j)
                    in if a > b
                         then Just (j, show a, show b)
                         else go (j + 1)
          in go 1
