{-# LANGUAGE BangPatterns #-}

{- | Materialize Apache Arrow IPC record batch bodies into Haskell-friendly columns.

Supports flat and nested schemas. Nullable columns use a validity bitmap
(LSB of each byte first) plus values; decoded as @V.Vector (Maybe a)@.
-}
module Arrow.Column (
  ColumnArray (..),
  materializeFlatRecordBatch,
  materializeRecordBatch,
  columnLength,
  countFieldNodesFlat,
  countBuffersFlat,
  resolveDictionaryColumn,
  placeholderColumn,
  emptyColumnFor,
  expandDictionary,

  -- * Row slicing, concatenation and gathering
  sliceColumnArray,
  concatColumnArray,
  concatColumnArrays,
  takeColumnArray,

  -- * Nullability
  isNullableColumn,
  toNullableColumn,
  maskValidity,

  -- * Map invariants
  validateMapKeysSorted,

  -- * Buffer bounds
  validateRecordBatchBuffers,
) where

import Arrow.Types (
  ArrowType (..),
  Buffer (..),
  DateUnit (..),
  DictionaryEncoding (..),
  Endianness (..),
  Field (..),
  FieldNode (..),
  IntervalUnit (..),
  Precision (..),
  RecordBatchDef (..),
  Schema (..),
  TimeUnit (..),
  UnionMode (..),
 )
import Columnar.SIMD (unpackBitsLsbUnsafe)
import Control.DeepSeq (NFData)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Vector.Primitive qualified as VP
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Storable (pokeElemOff)
import GHC.Generics (Generic)
import System.IO.Unsafe (unsafePerformIO)
import Wireform.FFI (validateArrowBuffers)


{- | Validate that all buffer offset/length pairs in a 'RecordBatchDef' are
non-negative, within the given body length, and non-overlapping.
Uses SIMD-accelerated pairwise checks.
-}
validateRecordBatchBuffers :: RecordBatchDef -> Int64 -> Bool
validateRecordBatchBuffers rb bodyLen = unsafePerformIO $ do
  let !bufs = rbBuffers rb
      !n = V.length bufs
  if n == 0
    then pure True
    else allocaArray (n * 2) $ \ptr -> do
      V.iforM_ bufs $ \i buf -> do
        pokeElemOff ptr (i * 2) (bufOffset buf)
        pokeElemOff ptr (i * 2 + 1) (bufLength buf)
      pure $! validateArrowBuffers ptr n bodyLen
{-# INLINE validateRecordBatchBuffers #-}


{- | Materialized values for one column.
Nullable columns use @Col*Maybe@ with per-row 'Maybe'.
-}
data ColumnArray
  = ColInt8 !(VP.Vector Int8)
  | ColInt16 !(VP.Vector Int16)
  | ColInt32 !(VP.Vector Int32)
  | ColInt64 !(VP.Vector Int64)
  | ColUInt8 !(VP.Vector Word8)
  | ColUInt16 !(VP.Vector Word16)
  | ColUInt32 !(VP.Vector Word32)
  | ColUInt64 !(VP.Vector Word64)
  | ColFloat16 !(VP.Vector Word16)
  | ColFloat !(VP.Vector Float)
  | ColDouble !(VP.Vector Double)
  | ColBool !(V.Vector Bool)
  | ColUtf8 !(V.Vector Text)
  | ColBinary !(V.Vector ByteString)
  | ColLargeUtf8 !(V.Vector Text)
  | ColLargeBinary !(V.Vector ByteString)
  | ColFixedSizeBinary !Int !(V.Vector ByteString)
  | ColDate32 !(VP.Vector Int32)
  | ColDate64 !(VP.Vector Int64)
  | ColTime32 !(VP.Vector Int32)
  | ColTime64 !(VP.Vector Int64)
  | ColTimestamp !(VP.Vector Int64)
  | ColDuration !(VP.Vector Int64)
  | ColDecimal128 !Int !Int !(V.Vector ByteString)
  | ColDecimal256 !Int !Int !(V.Vector ByteString)
  | -- | Arrow @INTERVAL(YEAR_MONTH)@: 32-bit months (i32 per row).
    ColIntervalYearMonth !(VP.Vector Int32)
  | {- | Arrow @INTERVAL(DAY_TIME)@: (days :: i32, ms :: i32) per row,
    stored as an 8-byte pair in element order.
    -}
    ColIntervalDayTime !(VP.Vector Int32) !(VP.Vector Int32)
  | {- | Arrow @INTERVAL(MONTH_DAY_NANO)@: (months :: i32, days :: i32,
    nanos :: i64) per row, stored as a 16-byte triple.
    -}
    ColIntervalMonthDayNano !(VP.Vector Int32) !(VP.Vector Int32) !(VP.Vector Int64)
  | ColInt8Maybe !(V.Vector (Maybe Int8))
  | ColInt16Maybe !(V.Vector (Maybe Int16))
  | ColInt32Maybe !(V.Vector (Maybe Int32))
  | ColInt64Maybe !(V.Vector (Maybe Int64))
  | ColUInt8Maybe !(V.Vector (Maybe Word8))
  | ColUInt16Maybe !(V.Vector (Maybe Word16))
  | ColUInt32Maybe !(V.Vector (Maybe Word32))
  | ColUInt64Maybe !(V.Vector (Maybe Word64))
  | ColFloat16Maybe !(V.Vector (Maybe Word16))
  | ColFloatMaybe !(V.Vector (Maybe Float))
  | ColDoubleMaybe !(V.Vector (Maybe Double))
  | ColBoolMaybe !(V.Vector (Maybe Bool))
  | ColUtf8Maybe !(V.Vector (Maybe Text))
  | ColBinaryMaybe !(V.Vector (Maybe ByteString))
  | ColLargeUtf8Maybe !(V.Vector (Maybe Text))
  | ColLargeBinaryMaybe !(V.Vector (Maybe ByteString))
  | ColFixedSizeBinaryMaybe !Int !(V.Vector (Maybe ByteString))
  | ColDate32Maybe !(V.Vector (Maybe Int32))
  | ColDate64Maybe !(V.Vector (Maybe Int64))
  | ColTime32Maybe !(V.Vector (Maybe Int32))
  | ColTime64Maybe !(V.Vector (Maybe Int64))
  | ColTimestampMaybe !(V.Vector (Maybe Int64))
  | ColDurationMaybe !(V.Vector (Maybe Int64))
  | -- | Nullable 'ColDecimal128': each present row is the 16-byte little-endian two's-complement value.
    ColDecimal128Maybe !Int !Int !(V.Vector (Maybe ByteString))
  | -- | Nullable 'ColDecimal256': each present row is the 32-byte little-endian two's-complement value.
    ColDecimal256Maybe !Int !Int !(V.Vector (Maybe ByteString))
  | -- | Nullable 'ColIntervalYearMonth'.
    ColIntervalYearMonthMaybe !(V.Vector (Maybe Int32))
  | -- | Nullable 'ColIntervalDayTime': @(days, milliseconds)@ per present row.
    ColIntervalDayTimeMaybe !(V.Vector (Maybe (Int32, Int32)))
  | -- | Nullable 'ColIntervalMonthDayNano': @(months, days, nanoseconds)@ per present row.
    ColIntervalMonthDayNanoMaybe !(V.Vector (Maybe (Int32, Int32, Int64)))
  | ColStruct !(V.Vector (Text, ColumnArray))
  | ColStructMaybe !(V.Vector Bool) !(V.Vector (Text, ColumnArray))
  | ColList !(VP.Vector Int32) !ColumnArray
  | ColListMaybe !(V.Vector Bool) !(VP.Vector Int32) !ColumnArray
  | {- | Arrow \"LargeList\": semantics identical to 'ColList' but with
    64-bit offsets. Used when the child array has more than
    2^31 elements.
    -}
    ColLargeList !(VP.Vector Int64) !ColumnArray
  | ColLargeListMaybe !(V.Vector Bool) !(VP.Vector Int64) !ColumnArray
  | ColFixedSizeList !Int !ColumnArray
  | ColFixedSizeListMaybe !Int !(V.Vector Bool) !ColumnArray
  | ColMap !(VP.Vector Int32) !ColumnArray !ColumnArray
  | ColMapMaybe !(V.Vector Bool) !(VP.Vector Int32) !ColumnArray !ColumnArray
  | {- | Dense union. The first vector holds one /child index/ per row
    (an index into the children vector, not the raw wire type id):
    the reader maps the schema's @AUnion _ typeIds@ onto child
    positions and the writer maps them back, so the column is
    self-describing without its 'Field'. The second vector is the
    per-row offset into the selected child.
    -}
    ColDenseUnion !(VP.Vector Int8) !(VP.Vector Int32) !(V.Vector ColumnArray)
  | {- | Sparse union. Per-row child indices (see 'ColDenseUnion'); every
    child has the same length as the union.
    -}
    ColSparseUnion !(VP.Vector Int8) !(V.Vector ColumnArray)
  | {- | Dictionary-encoded column with non-nullable indices: dictionary
    id, one index per row into the values column, values column. The
    reader leaves a typed empty placeholder in the values slot until
    'resolveDictionaryColumn' fills it from the stream's dictionary
    batches. Indices are stored as 'Int32' whatever the wire index type
    (any signed or unsigned 8/16/32/64-bit integer); a wire index that
    does not fit, or a negative one, is rejected by the reader.
    -}
    ColDictionary !Int64 !(VP.Vector Int32) !ColumnArray
  | {- | Dictionary-encoded column whose field is nullable: a 'Nothing'
    index is a null row (the index validity bitmap), independent of
    any nulls inside the values column. The writer emits the validity
    bitmap from the 'Nothing' positions; the reader produces this
    constructor whenever the dictionary field is nullable.
    -}
    ColDictionaryMaybe !Int64 !(V.Vector (Maybe Int32)) !ColumnArray
  | {- | Run-End Encoded column (Arrow spec >= 1.3). The first child
    holds the run-end indices (int16/32/64, ascending, the
    @i@-th element being the EXCLUSIVE end index of run @i@); the
    second holds the actual values (any type, may be nullable).
    The parent has /no/ buffers and /no/ validity bitmap of its
    own — nulls live in the values child.
    -}
    ColRunEndEncoded !ColumnArray !ColumnArray
  | {- | ListView (Arrow spec >= 1.4). Like 'ColList' but with a
    separate sizes buffer; offsets and sizes are independent
    32-bit arrays (so list elements may overlap or be in any
    order in the child storage).
    -}
    ColListView !(VP.Vector Int32) !(VP.Vector Int32) !ColumnArray
  | ColListViewMaybe !(V.Vector Bool) !(VP.Vector Int32) !(VP.Vector Int32) !ColumnArray
  | -- | LargeListView: 64-bit offsets and sizes.
    ColLargeListView !(VP.Vector Int64) !(VP.Vector Int64) !ColumnArray
  | ColLargeListViewMaybe !(V.Vector Bool) !(VP.Vector Int64) !(VP.Vector Int64) !ColumnArray
  | {- | Utf8View (Arrow spec >= 1.4). Each row is a 16-byte view
    struct: a 4-byte length followed by either an inlined
    payload (length <= 12) or a (4-byte prefix + 4-byte buffer
    index + 4-byte buffer offset) reference into one of the
    variadic data buffers. The materialized form here is the
    decoded UTF-8 strings; the inlined-vs-out-of-line layout
    is the writer's concern.
    -}
    ColUtf8View !(V.Vector Text)
  | ColUtf8ViewMaybe !(V.Vector (Maybe Text))
  | {- | BinaryView: same layout as 'ColUtf8View' but no UTF-8
    validation; raw bytes.
    -}
    ColBinaryView !(V.Vector ByteString)
  | ColBinaryViewMaybe !(V.Vector (Maybe ByteString))
  | {- | Arrow NULL (@ANull@) column: the spec assigns no
    buffers and no validity bitmap, just a length where every
    row is null. Useful for placeholder columns and for the
    pyarrow-emits-AN-empty-column edge case in test fixtures.
    -}
    ColNull !Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- | One field node per top-level field (flat schema).
countFieldNodesFlat :: V.Vector Field -> Int
countFieldNodesFlat fs = V.length fs


-- | Buffer count for one flat field (validity bitmap first when nullable).
buffersPerField :: Field -> Either String Int
buffersPerField f
  | not (V.null (fieldChildren f)) = Left "Arrow.Column: nested fieldChildren not supported in flat mode"
  | otherwise = do
      nData <- case fieldType f of
        ANull -> Right (-1) -- ANull has no buffers at all (no validity, no data).
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
        _ -> Right $ (if fieldNullable f then 1 else 0) + nData


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
            else do
              _ <- planNodes fields rb
              materializeFields (arrowEndianness schema) fields rb body 0 0


materializeFields :: Endianness -> V.Vector Field -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (V.Vector ColumnArray)
materializeFields endian fields rb body !nodeIdx !bufIdx
  | V.null fields = Right V.empty
  | otherwise = do
      (c, n1, b1) <- materializeOne endian (V.head fields) rb body nodeIdx bufIdx
      rest <- materializeFields endian (V.tail fields) rb body n1 b1
      Right (V.cons c rest)


materializeOne :: Endianness -> Field -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
materializeOne endian f rb body !nodeIdx !bufIdx = do
  len <- nodeLenAt rb nodeIdx
  case fieldType f of
    -- ANull has no buffers and no validity bitmap (per spec): the
    -- field node's length is the only state. Same shape regardless
    -- of fieldNullable.
    ANull -> Right (ColNull len, nodeIdx + 1, bufIdx)
    AFixedSizeBinary n
      | n < 0 -> Left ("Arrow.Column: negative fixed-size binary width " ++ show n)
    _ ->
         if fieldNullable f
           then case fieldType f of
             AInt 8 True -> readInt8ColumnMaybe endian len rb body bufIdx nodeIdx
             AInt 8 False -> readUInt8ColumnMaybe len rb body bufIdx nodeIdx
             AInt 16 True -> readInt16ColumnMaybe endian len rb body bufIdx nodeIdx
             AInt 16 False -> readUInt16ColumnMaybe endian len rb body bufIdx nodeIdx
             AInt 32 True -> readInt32ColumnMaybe endian len rb body bufIdx nodeIdx
             AInt 32 False -> readUInt32ColumnMaybe endian len rb body bufIdx nodeIdx
             AInt 64 True -> readInt64ColumnMaybe endian len rb body bufIdx nodeIdx
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
             AInt 16 True -> readInt16Column endian len rb body bufIdx nodeIdx
             AInt 16 False -> readUInt16Column endian len rb body bufIdx nodeIdx
             AInt 32 True -> readInt32Column endian len rb body bufIdx nodeIdx
             AInt 32 False -> readUInt32Column endian len rb body bufIdx nodeIdx
             AInt 64 True -> readInt64Column endian len rb body bufIdx nodeIdx
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
  valsBs <- sliceBufAt rb body bufIdx
  col <- readInts8 len valsBs
  Right (ColInt8 col, nodeIdx + 1, bufIdx + 1)


readInt16Column :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readInt16Column endian len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBufAt rb body bufIdx
  col <- readInts16 endian len valsBs
  Right (ColInt16 col, nodeIdx + 1, bufIdx + 1)


readInt32Column :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readInt32Column endian len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBufAt rb body bufIdx
  col <- readInts32 endian len valsBs
  Right (ColInt32 col, nodeIdx + 1, bufIdx + 1)


readInt64Column :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readInt64Column endian len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBufAt rb body bufIdx
  col <- readInts64 endian len valsBs
  Right (ColInt64 col, nodeIdx + 1, bufIdx + 1)


readUInt8Column :: Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readUInt8Column len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBufAt rb body bufIdx
  if BS.length valsBs < len
    then Left "Arrow.Column: uint8 buffer too small"
    else Right (ColUInt8 (VP.generate len $ \i -> BSU.unsafeIndex valsBs i), nodeIdx + 1, bufIdx + 1)


readUInt16Column :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readUInt16Column endian len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBufAt rb body bufIdx
  if BS.length valsBs < len * 2
    then Left "Arrow.Column: uint16 buffer too small"
    else Right (ColUInt16 (VP.generate len $ \i -> readWord16 endian valsBs (i * 2)), nodeIdx + 1, bufIdx + 1)


readUInt32Column :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readUInt32Column endian len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBufAt rb body bufIdx
  if BS.length valsBs < len * 4
    then Left "Arrow.Column: uint32 buffer too small"
    else Right (ColUInt32 (VP.generate len $ \i -> readWord32 endian valsBs (i * 4)), nodeIdx + 1, bufIdx + 1)


readUInt64Column :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readUInt64Column endian len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBufAt rb body bufIdx
  if BS.length valsBs < len * 8
    then Left "Arrow.Column: uint64 buffer too small"
    else Right (ColUInt64 (VP.generate len $ \i -> readWord64 endian valsBs (i * 8)), nodeIdx + 1, bufIdx + 1)


readFloat16Column :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readFloat16Column endian len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBufAt rb body bufIdx
  if BS.length valsBs < len * 2
    then Left "Arrow.Column: float16 buffer too small"
    else Right (ColFloat16 (VP.generate len $ \i -> readWord16 endian valsBs (i * 2)), nodeIdx + 1, bufIdx + 1)


readBoolColumn :: Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readBoolColumn len rb body !bufIdx !nodeIdx = do
  dataBs <- sliceBufAt rb body bufIdx
  bs <- unpackBits len dataBs
  Right (ColBool bs, nodeIdx + 1, bufIdx + 1)


readFloatColumn :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readFloatColumn endian len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBufAt rb body bufIdx
  if BS.length valsBs < len * 4
    then Left "Arrow.Column: float buffer too small"
    else Right (ColFloat (VP.generate len $ \i -> castWord32ToFloat (readWord32 endian valsBs (i * 4))), nodeIdx + 1, bufIdx + 1)


readDoubleColumn :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readDoubleColumn endian len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBufAt rb body bufIdx
  if BS.length valsBs < len * 8
    then Left "Arrow.Column: double buffer too small"
    else Right (ColDouble (VP.generate len $ \i -> castWord64ToDouble (readWord64 endian valsBs (i * 8))), nodeIdx + 1, bufIdx + 1)


{- | Offsets at @bufIdx@ (@len + 1@ entries of @offWidth@ bytes; may be
empty when @len == 0@) and data at @bufIdx + 1@. Returns a row
reader that range-checks @[offsets[i], offsets[i + 1])@ against the
data buffer before slicing it.
-}
varLengthRows
  :: String
  -> Int
  -> (ByteString -> Int -> Int)
  -> Int
  -> RecordBatchDef
  -> ByteString
  -> Int
  -> Either String (Int -> Either String ByteString)
varLengthRows what offWidth offAt len rb body !bufIdx = do
  offBs <- sliceBufAt rb body bufIdx
  datBs <- sliceBufAt rb body (bufIdx + 1)
  if len > 0 && BS.length offBs < (len + 1) * offWidth
    then Left ("Arrow.Column: " ++ what ++ " offsets buffer too small")
    else Right $ \i ->
      let !start = offAt offBs i
          !end = offAt offBs (i + 1)
      in if start < 0 || end < start || end > BS.length datBs
           then Left ("Arrow.Column: invalid " ++ what ++ " slice at row " ++ show i)
           else Right $! BSU.unsafeTake (end - start) (BSU.unsafeDrop start datBs)


readUtf8Column :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readUtf8Column endian len rb body !bufIdx !nodeIdx = do
  row <- varLengthRows "UTF-8" 4 (offset32At endian) len rb body bufIdx
  strs <- V.generateM len (\i -> row i >>= utf8Row "UTF-8")
  Right (ColUtf8 strs, nodeIdx + 1, bufIdx + 2)


readBinaryColumn :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readBinaryColumn endian len rb body !bufIdx !nodeIdx = do
  row <- varLengthRows "binary" 4 (offset32At endian) len rb body bufIdx
  bins <- V.generateM len row
  Right (ColBinary bins, nodeIdx + 1, bufIdx + 2)


readLargeUtf8Column :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readLargeUtf8Column endian len rb body !bufIdx !nodeIdx = do
  row <- varLengthRows "large UTF-8" 8 (offset64At endian) len rb body bufIdx
  strs <- V.generateM len (\i -> row i >>= utf8Row "large UTF-8")
  Right (ColLargeUtf8 strs, nodeIdx + 1, bufIdx + 2)


readLargeBinaryColumn :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readLargeBinaryColumn endian len rb body !bufIdx !nodeIdx = do
  row <- varLengthRows "large binary" 8 (offset64At endian) len rb body bufIdx
  bins <- V.generateM len row
  Right (ColLargeBinary bins, nodeIdx + 1, bufIdx + 2)


readFixedSizeBinaryColumn :: Int -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readFixedSizeBinaryColumn byteWidth len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBufAt rb body bufIdx
  if BS.length valsBs < len * byteWidth
    then Left "Arrow.Column: fixed-size binary buffer too small"
    else
      Right
        ( ColFixedSizeBinary
            byteWidth
            ( V.generate len $ \i ->
                BS.take byteWidth (BS.drop (i * byteWidth) valsBs)
            )
        , nodeIdx + 1
        , bufIdx + 1
        )


readDate32Column :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readDate32Column endian len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBufAt rb body bufIdx
  if BS.length valsBs < len * 4
    then Left "Arrow.Column: date32 buffer too small"
    else
      Right
        ( ColDate32
            ( VP.generate len $ \i ->
                fromIntegral (readWord32 endian valsBs (i * 4)) :: Int32
            )
        , nodeIdx + 1
        , bufIdx + 1
        )


readDate64Column :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readDate64Column endian len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBufAt rb body bufIdx
  if BS.length valsBs < len * 8
    then Left "Arrow.Column: date64 buffer too small"
    else
      Right
        ( ColDate64
            ( VP.generate len $ \i ->
                fromIntegral (readWord64 endian valsBs (i * 8)) :: Int64
            )
        , nodeIdx + 1
        , bufIdx + 1
        )


readTime32Column :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readTime32Column endian len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBufAt rb body bufIdx
  if BS.length valsBs < len * 4
    then Left "Arrow.Column: time32 buffer too small"
    else
      Right
        ( ColTime32
            ( VP.generate len $ \i ->
                fromIntegral (readWord32 endian valsBs (i * 4)) :: Int32
            )
        , nodeIdx + 1
        , bufIdx + 1
        )


readTime64Column :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readTime64Column endian len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBufAt rb body bufIdx
  if BS.length valsBs < len * 8
    then Left "Arrow.Column: time64 buffer too small"
    else
      Right
        ( ColTime64
            ( VP.generate len $ \i ->
                fromIntegral (readWord64 endian valsBs (i * 8)) :: Int64
            )
        , nodeIdx + 1
        , bufIdx + 1
        )


readTimestampColumn :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readTimestampColumn endian len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBufAt rb body bufIdx
  if BS.length valsBs < len * 8
    then Left "Arrow.Column: timestamp buffer too small"
    else
      Right
        ( ColTimestamp
            ( VP.generate len $ \i ->
                fromIntegral (readWord64 endian valsBs (i * 8)) :: Int64
            )
        , nodeIdx + 1
        , bufIdx + 1
        )


readDurationColumn :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readDurationColumn endian len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBufAt rb body bufIdx
  if BS.length valsBs < len * 8
    then Left "Arrow.Column: duration buffer too small"
    else
      Right
        ( ColDuration
            ( VP.generate len $ \i ->
                fromIntegral (readWord64 endian valsBs (i * 8)) :: Int64
            )
        , nodeIdx + 1
        , bufIdx + 1
        )


readDecimal128Column :: Int -> Int -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readDecimal128Column precision scale len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBufAt rb body bufIdx
  if BS.length valsBs < len * 16
    then Left "Arrow.Column: decimal128 buffer too small"
    else
      Right
        ( ColDecimal128
            precision
            scale
            ( V.generate len $ \i ->
                BS.take 16 (BS.drop (i * 16) valsBs)
            )
        , nodeIdx + 1
        , bufIdx + 1
        )


readDecimal256Column :: Int -> Int -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readDecimal256Column precision scale len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBufAt rb body bufIdx
  if BS.length valsBs < len * 32
    then Left "Arrow.Column: decimal256 buffer too small"
    else
      Right
        ( ColDecimal256
            precision
            scale
            ( V.generate len $ \i ->
                BS.take 32 (BS.drop (i * 32) valsBs)
            )
        , nodeIdx + 1
        , bufIdx + 1
        )


-- * Nullable column readers (validity bitmap + values)


{- | Validity bitmap at @bufIdx@ plus a values buffer at @bufIdx + 1@
holding @width@ bytes per row. The values buffer is size-checked
before the bitmap is expanded, so a wire length the body cannot back
never reaches an allocation. @get valsBs i@ reads row @i@ and may
assume @valsBs@ holds at least @len * width@ bytes.
-}
readNullable
  :: String
  -> Int
  -> Int
  -> RecordBatchDef
  -> ByteString
  -> Int
  -> (ByteString -> Int -> a)
  -> Either String (V.Vector (Maybe a))
readNullable what width len rb body !bufIdx get = do
  validBs <- sliceBufAt rb body bufIdx
  valsBs <- sliceBufAt rb body (bufIdx + 1)
  if BS.length valsBs < len * width
    then Left ("Arrow.Column: " ++ what ++ " values buffer too small")
    else do
      validFlags <- unpackValidity len validBs
      Right $! V.generate len $ \i ->
        if V.unsafeIndex validFlags i then Just (get valsBs i) else Nothing
{-# INLINE readNullable #-}


type NullableReader = Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)


-- | Build a fixed-width nullable reader from a row decoder.
fixedNullable :: String -> Int -> (V.Vector (Maybe a) -> ColumnArray) -> (Endianness -> ByteString -> Int -> a) -> NullableReader
fixedNullable what width con get endian len rb body !bufIdx !nodeIdx = do
  xs <- readNullable what width len rb body bufIdx (get endian)
  Right (con xs, nodeIdx + 1, bufIdx + 2)
{-# INLINE fixedNullable #-}


readInt8ColumnMaybe :: NullableReader
readInt8ColumnMaybe =
  fixedNullable "int8" 1 ColInt8Maybe $ \_ bs i -> fromIntegral (BSU.unsafeIndex bs i) :: Int8


readInt16ColumnMaybe :: NullableReader
readInt16ColumnMaybe =
  fixedNullable "int16" 2 ColInt16Maybe $ \e bs i -> fromIntegral (readWord16 e bs (i * 2))


readInt32ColumnMaybe :: NullableReader
readInt32ColumnMaybe =
  fixedNullable "int32" 4 ColInt32Maybe $ \e bs i -> int32FromWord (readWord32 e bs (i * 4))


readInt64ColumnMaybe :: NullableReader
readInt64ColumnMaybe =
  fixedNullable "int64" 8 ColInt64Maybe $ \e bs i -> int64FromWord (readWord64 e bs (i * 8))


readUInt8ColumnMaybe :: Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readUInt8ColumnMaybe =
  fixedNullable "uint8" 1 ColUInt8Maybe (\_ bs i -> BSU.unsafeIndex bs i) Little


readUInt16ColumnMaybe :: NullableReader
readUInt16ColumnMaybe =
  fixedNullable "uint16" 2 ColUInt16Maybe $ \e bs i -> readWord16 e bs (i * 2)


readUInt32ColumnMaybe :: NullableReader
readUInt32ColumnMaybe =
  fixedNullable "uint32" 4 ColUInt32Maybe $ \e bs i -> readWord32 e bs (i * 4)


readUInt64ColumnMaybe :: NullableReader
readUInt64ColumnMaybe =
  fixedNullable "uint64" 8 ColUInt64Maybe $ \e bs i -> readWord64 e bs (i * 8)


readFloat16ColumnMaybe :: NullableReader
readFloat16ColumnMaybe =
  fixedNullable "float16" 2 ColFloat16Maybe $ \e bs i -> readWord16 e bs (i * 2)


readBoolColumnMaybe :: Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readBoolColumnMaybe len rb body !bufIdx !nodeIdx = do
  validBs <- sliceBufAt rb body bufIdx
  dataBs <- sliceBufAt rb body (bufIdx + 1)
  valFlags <- unpackBits len dataBs
  validFlags <- unpackValidity len validBs
  let !xs = V.zipWith (\ok v -> if ok then Just v else Nothing) validFlags valFlags
  Right (ColBoolMaybe xs, nodeIdx + 1, bufIdx + 2)


readFloatColumnMaybe :: NullableReader
readFloatColumnMaybe =
  fixedNullable "float" 4 ColFloatMaybe $ \e bs i -> castWord32ToFloat (readWord32 e bs (i * 4))


readDoubleColumnMaybe :: NullableReader
readDoubleColumnMaybe =
  fixedNullable "double" 8 ColDoubleMaybe $ \e bs i -> castWord64ToDouble (readWord64 e bs (i * 8))


{- | Validity at @bufIdx@, then the 'varLengthRows' layout (offsets,
data) at @bufIdx + 1@. Only non-null rows are sliced; @conv@ turns a
slice into a row value.
-}
readVarNullable
  :: String
  -> Int
  -> (ByteString -> Int -> Int)
  -> (ByteString -> Either String a)
  -> Int
  -> RecordBatchDef
  -> ByteString
  -> Int
  -> Either String (V.Vector (Maybe a))
readVarNullable what offWidth offAt conv len rb body !bufIdx = do
  validBs <- sliceBufAt rb body bufIdx
  row <- varLengthRows what offWidth offAt len rb body (bufIdx + 1)
  validFlags <- unpackValidity len validBs
  V.generateM len $ \i ->
    if V.unsafeIndex validFlags i then Just <$> (row i >>= conv) else Right Nothing


offset32At :: Endianness -> ByteString -> Int -> Int
offset32At endian bs i = fromIntegral (int32FromWord (readWord32 endian bs (i * 4)))
{-# INLINE offset32At #-}


-- | 64-bit offsets above 'maxBound :: Int' wrap negative; callers reject them via their @start < 0@ checks.
offset64At :: Endianness -> ByteString -> Int -> Int
offset64At endian bs i = fromIntegral (readWord64 endian bs (i * 8))
{-# INLINE offset64At #-}


utf8Row :: String -> ByteString -> Either String Text
utf8Row what bs = case TE.decodeUtf8' bs of
  Right t -> Right t
  Left _ -> Left ("Arrow.Column: invalid " ++ what ++ " bytes")


readUtf8ColumnMaybe :: NullableReader
readUtf8ColumnMaybe endian len rb body !bufIdx !nodeIdx = do
  xs <- readVarNullable "UTF-8" 4 (offset32At endian) (utf8Row "UTF-8") len rb body bufIdx
  Right (ColUtf8Maybe xs, nodeIdx + 1, bufIdx + 3)


readBinaryColumnMaybe :: NullableReader
readBinaryColumnMaybe endian len rb body !bufIdx !nodeIdx = do
  xs <- readVarNullable "binary" 4 (offset32At endian) Right len rb body bufIdx
  Right (ColBinaryMaybe xs, nodeIdx + 1, bufIdx + 3)


readLargeUtf8ColumnMaybe :: NullableReader
readLargeUtf8ColumnMaybe endian len rb body !bufIdx !nodeIdx = do
  xs <- readVarNullable "large UTF-8" 8 (offset64At endian) (utf8Row "large UTF-8") len rb body bufIdx
  Right (ColLargeUtf8Maybe xs, nodeIdx + 1, bufIdx + 3)


readLargeBinaryColumnMaybe :: NullableReader
readLargeBinaryColumnMaybe endian len rb body !bufIdx !nodeIdx = do
  xs <- readVarNullable "large binary" 8 (offset64At endian) Right len rb body bufIdx
  Right (ColLargeBinaryMaybe xs, nodeIdx + 1, bufIdx + 3)


readFixedSizeBinaryColumnMaybe :: Int -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readFixedSizeBinaryColumnMaybe byteWidth len rb body !bufIdx !nodeIdx = do
  xs <- readNullable "fixed-size binary" byteWidth len rb body bufIdx $ \bs i ->
    BSU.unsafeTake byteWidth (BSU.unsafeDrop (i * byteWidth) bs)
  Right (ColFixedSizeBinaryMaybe byteWidth xs, nodeIdx + 1, bufIdx + 2)


readDate32ColumnMaybe :: NullableReader
readDate32ColumnMaybe =
  fixedNullable "date32" 4 ColDate32Maybe $ \e bs i -> int32FromWord (readWord32 e bs (i * 4))


readDate64ColumnMaybe :: NullableReader
readDate64ColumnMaybe =
  fixedNullable "date64" 8 ColDate64Maybe $ \e bs i -> int64FromWord (readWord64 e bs (i * 8))


readTime32ColumnMaybe :: NullableReader
readTime32ColumnMaybe =
  fixedNullable "time32" 4 ColTime32Maybe $ \e bs i -> int32FromWord (readWord32 e bs (i * 4))


readTime64ColumnMaybe :: NullableReader
readTime64ColumnMaybe =
  fixedNullable "time64" 8 ColTime64Maybe $ \e bs i -> int64FromWord (readWord64 e bs (i * 8))


readTimestampColumnMaybe :: NullableReader
readTimestampColumnMaybe =
  fixedNullable "timestamp" 8 ColTimestampMaybe $ \e bs i -> int64FromWord (readWord64 e bs (i * 8))


readDurationColumnMaybe :: NullableReader
readDurationColumnMaybe =
  fixedNullable "duration" 8 ColDurationMaybe $ \e bs i -> int64FromWord (readWord64 e bs (i * 8))


readDecimal128ColumnMaybe :: Int -> Int -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readDecimal128ColumnMaybe precision scale len rb body !bufIdx !nodeIdx = do
  vals <- readNullable "decimal128" 16 len rb body bufIdx (fixedBytesAt 16)
  Right (ColDecimal128Maybe precision scale vals, nodeIdx + 1, bufIdx + 2)


readDecimal256ColumnMaybe :: Int -> Int -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readDecimal256ColumnMaybe precision scale len rb body !bufIdx !nodeIdx = do
  vals <- readNullable "decimal256" 32 len rb body bufIdx (fixedBytesAt 32)
  Right (ColDecimal256Maybe precision scale vals, nodeIdx + 1, bufIdx + 2)


-- | Row @i@ of a buffer holding @width@ bytes per row (caller checked the size).
fixedBytesAt :: Int -> ByteString -> Int -> ByteString
fixedBytesAt width valsBs i = BSU.unsafeTake width (BSU.unsafeDrop (i * width) valsBs)


-- * Low-level primitives


{- | Checked lookup of the @i@-th body buffer descriptor. Record batch
headers come off the wire, so a schema/batch mismatch or a corrupt
header must surface as 'Left', never as an out-of-bounds read.
-}
bufAt :: RecordBatchDef -> Int -> Either String Buffer
bufAt rb i = case rbBuffers rb V.!? i of
  Just b -> Right b
  Nothing ->
    Left $
      "Arrow.Column: record batch has "
        ++ show (V.length (rbBuffers rb))
        ++ " buffers, schema needs buffer #"
        ++ show i
{-# INLINE bufAt #-}


-- | Checked lookup of the @i@-th field node; see 'bufAt'.
nodeAt :: RecordBatchDef -> Int -> Either String FieldNode
nodeAt rb i = case rbNodes rb V.!? i of
  Just n -> Right n
  Nothing ->
    Left $
      "Arrow.Column: record batch has "
        ++ show (V.length (rbNodes rb))
        ++ " field nodes, schema needs node #"
        ++ show i
{-# INLINE nodeAt #-}


-- | Slice a buffer descriptor out of the body; offsets are wire values.
sliceBuffer :: ByteString -> Buffer -> Either String ByteString
sliceBuffer body buf =
  let !o = fromIntegral (bufOffset buf) :: Int
      !l = fromIntegral (bufLength buf) :: Int
      !n = BS.length body
  in -- Compare against @n - l@ rather than @o + l@ so huge wire values
     -- cannot overflow past the check.
     if o < 0 || l < 0 || l > n || o > n - l
       then Left "Arrow.Column: buffer slice out of range"
       else Right $! BSU.unsafeTake l (BSU.unsafeDrop o body)


-- | 'bufAt' followed by 'sliceBuffer'.
sliceBufAt :: RecordBatchDef -> ByteString -> Int -> Either String ByteString
sliceBufAt rb body i = bufAt rb i >>= sliceBuffer body
{-# INLINE sliceBufAt #-}


{- | Upper bound on any array length taken from a field node. Far above
any batch that fits in memory, and small enough that @len * 32@ and
@(len + 1) * 8@ (the largest per-row byte counts the readers compute)
cannot overflow 'Int'.
-}
maxArrayLength :: Int
maxArrayLength = 1 `shiftL` 40


{- | Checked length of the @i@-th field node. Wire lengths are 'Int64';
negative or absurd values are rejected here so every reader can trust
@0 <= len <= 'maxArrayLength'@ when sizing buffers.
-}
nodeLenAt :: RecordBatchDef -> Int -> Either String Int
nodeLenAt rb i = do
  node <- nodeAt rb i
  let !l = fnLength node
  if l < 0 || l > fromIntegral maxArrayLength
    then Left ("Arrow.Column: field node #" ++ show i ++ " has invalid length " ++ show l)
    else Right (fromIntegral l)
{-# INLINE nodeLenAt #-}


{- | Decode a validity bitmap. Per spec an empty buffer means "all
valid" (producers may omit the bitmap when @null_count == 0@);
otherwise it must hold at least @ceil(n / 8)@ bytes.
-}
unpackValidity :: Int -> ByteString -> Either String (V.Vector Bool)
unpackValidity n bs
  | BS.null bs = Right $! V.replicate n True
  | otherwise = unpackBits n bs


-- | Decode a packed LSB-first bit buffer holding exactly @n@ bits of data.
unpackBits :: Int -> ByteString -> Either String (V.Vector Bool)
unpackBits n bs
  | BS.length bs < (n + 7) `quot` 8 =
      Left
        ( "Arrow.Column: bitmap has "
            ++ show (BS.length bs)
            ++ " bytes, "
            ++ show n
            ++ " rows need "
            ++ show ((n + 7) `quot` 8)
        )
  | otherwise = Right $! unpackBitsLsbUnsafe n bs


readInts8 :: Int -> ByteString -> Either String (VP.Vector Int8)
readInts8 len bs
  | BS.length bs < len = Left "Arrow.Column: int8 buffer too small"
  | otherwise =
      Right $
        VP.generate len $ \i ->
          fromIntegral (BSU.unsafeIndex bs i) :: Int8


readInts16 :: Endianness -> Int -> ByteString -> Either String (VP.Vector Int16)
readInts16 endian len bs
  | BS.length bs < len * 2 = Left "Arrow.Column: int16 buffer too small"
  | otherwise =
      Right $
        VP.generate len $ \i ->
          let v = readWord16 endian bs (i * 2)
          in fromIntegral v


readInts32 :: Endianness -> Int -> ByteString -> Either String (VP.Vector Int32)
readInts32 endian len bs
  | BS.length bs < len * 4 = Left "Arrow.Column: int32 buffer too small"
  | otherwise =
      Right $
        VP.generate len $ \i ->
          let v = readWord32 endian bs (i * 4)
          in int32FromWord v


readInts64 :: Endianness -> Int -> ByteString -> Either String (VP.Vector Int64)
readInts64 endian len bs
  | BS.length bs < len * 8 = Left "Arrow.Column: int64 buffer too small"
  | otherwise =
      Right $
        VP.generate len $ \i ->
          let v = readWord64 endian bs (i * 8)
          in int64FromWord v


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
  in rd 0
       .|. (rd 1 `shiftL` 8)
       .|. (rd 2 `shiftL` 16)
       .|. (rd 3 `shiftL` 24)
       .|. (rd 4 `shiftL` 32)
       .|. (rd 5 `shiftL` 40)
       .|. (rd 6 `shiftL` 48)
       .|. (rd 7 `shiftL` 56)


readBE64 :: ByteString -> Int -> Word64
readBE64 bs off =
  let rd i = fromIntegral (BSU.unsafeIndex bs (off + i)) :: Word64
  in (rd 0 `shiftL` 56)
       .|. (rd 1 `shiftL` 48)
       .|. (rd 2 `shiftL` 40)
       .|. (rd 3 `shiftL` 32)
       .|. (rd 4 `shiftL` 24)
       .|. (rd 5 `shiftL` 16)
       .|. (rd 6 `shiftL` 8)
       .|. rd 7


-- | Row count for a column array.
columnLength :: ColumnArray -> Int
columnLength = \case
  ColInt8 v -> VP.length v
  ColInt16 v -> VP.length v
  ColInt32 v -> VP.length v
  ColInt64 v -> VP.length v
  ColUInt8 v -> VP.length v
  ColUInt16 v -> VP.length v
  ColUInt32 v -> VP.length v
  ColUInt64 v -> VP.length v
  ColFloat16 v -> VP.length v
  ColFloat v -> VP.length v
  ColDouble v -> VP.length v
  ColBool v -> V.length v
  ColUtf8 v -> V.length v
  ColBinary v -> V.length v
  ColLargeUtf8 v -> V.length v
  ColLargeBinary v -> V.length v
  ColFixedSizeBinary _ v -> V.length v
  ColDate32 v -> VP.length v
  ColDate64 v -> VP.length v
  ColTime32 v -> VP.length v
  ColTime64 v -> VP.length v
  ColTimestamp v -> VP.length v
  ColDuration v -> VP.length v
  ColDecimal128 _ _ v -> V.length v
  ColDecimal256 _ _ v -> V.length v
  ColInt8Maybe v -> V.length v
  ColInt16Maybe v -> V.length v
  ColInt32Maybe v -> V.length v
  ColInt64Maybe v -> V.length v
  ColUInt8Maybe v -> V.length v
  ColUInt16Maybe v -> V.length v
  ColUInt32Maybe v -> V.length v
  ColUInt64Maybe v -> V.length v
  ColFloat16Maybe v -> V.length v
  ColFloatMaybe v -> V.length v
  ColDoubleMaybe v -> V.length v
  ColBoolMaybe v -> V.length v
  ColUtf8Maybe v -> V.length v
  ColBinaryMaybe v -> V.length v
  ColLargeUtf8Maybe v -> V.length v
  ColLargeBinaryMaybe v -> V.length v
  ColFixedSizeBinaryMaybe _ v -> V.length v
  ColDate32Maybe v -> V.length v
  ColDate64Maybe v -> V.length v
  ColTime32Maybe v -> V.length v
  ColTime64Maybe v -> V.length v
  ColTimestampMaybe v -> V.length v
  ColDurationMaybe v -> V.length v
  ColDecimal128Maybe _ _ v -> V.length v
  ColDecimal256Maybe _ _ v -> V.length v
  ColIntervalYearMonthMaybe v -> V.length v
  ColIntervalDayTimeMaybe v -> V.length v
  ColIntervalMonthDayNanoMaybe v -> V.length v
  ColStruct children -> if V.null children then 0 else columnLength (snd (V.head children))
  ColStructMaybe v _ -> V.length v
  ColList offsets _ -> max 0 (VP.length offsets - 1)
  ColListMaybe v _ _ -> V.length v
  ColLargeList offsets _ -> max 0 (VP.length offsets - 1)
  ColLargeListMaybe v _ _ -> V.length v
  ColIntervalYearMonth v -> VP.length v
  ColIntervalDayTime d _ -> VP.length d
  ColIntervalMonthDayNano m _ _ -> VP.length m
  -- FixedSizeList<n> has parent length = child length / n
  -- (each row consumes exactly n child elements). The
  -- previous formula returned child length which made the
  -- record batch's @length@ field 'n' times larger than the
  -- actual row count and any downstream reader rejected the
  -- batch ("Array length did not match record batch length").
  ColFixedSizeList n child
    | n > 0 -> columnLength child `quot` n
    | otherwise -> 0
  ColFixedSizeListMaybe _ v _ -> V.length v
  ColMap offsets _ _ -> max 0 (VP.length offsets - 1)
  ColMapMaybe v _ _ _ -> V.length v
  ColDenseUnion typeIds _ _ -> VP.length typeIds
  ColSparseUnion typeIds _ -> VP.length typeIds
  ColDictionary _ indices _ -> VP.length indices
  ColDictionaryMaybe _ indices _ -> V.length indices
  ColRunEndEncoded runEnds _ ->
    -- The logical length is the LAST run-end value (exclusive).
    case runEnds of
      ColInt16 v -> if VP.null v then 0 else fromIntegral (VP.last v)
      ColInt32 v -> if VP.null v then 0 else fromIntegral (VP.last v)
      ColInt64 v -> if VP.null v then 0 else fromIntegral (VP.last v)
      _ -> 0
  ColListView offsets _ _ -> VP.length offsets
  ColListViewMaybe v _ _ _ -> V.length v
  ColLargeListView offsets _ _ -> VP.length offsets
  ColLargeListViewMaybe v _ _ _ -> V.length v
  ColUtf8View v -> V.length v
  ColUtf8ViewMaybe v -> V.length v
  ColBinaryView v -> V.length v
  ColBinaryViewMaybe v -> V.length v
  ColNull n -> n


{- | Materialize a record batch with support for nested types.
Walks the schema tree in preorder DFS, consuming field nodes and buffers.

Every wire-derived count, length, offset and index is checked before
it is used, so a corrupt or hostile batch yields 'Left', never an
out-of-bounds read or an allocation the body cannot back.
-}
materializeRecordBatch :: Schema -> RecordBatchDef -> ByteString -> Either String (V.Vector ColumnArray)
materializeRecordBatch schema rb body = do
  let bodyLen = fromIntegral (BS.length body) :: Int64
  if not (validateRecordBatchBuffers rb bodyLen)
    then Left "Arrow.Column: invalid buffer bounds in RecordBatchDef"
    else do
      views <- planNodes (arrowFields schema) rb
      let !ctx = Ctx (arrowEndianness schema) views rb body
      (cols, _, _) <- materializeFieldsN ctx (arrowFields schema) 0 0
      Right cols


-- | Read-only state shared by the nested materializers.
data Ctx = Ctx
  { ctxEndian :: !Endianness
  , ctxViews :: !(VP.Vector Int)
  -- ^ Per field node (preorder): the variadic data-buffer count of a
  -- view column, @-1@ for every other node.
  , ctxRb :: !RecordBatchDef
  , ctxBody :: !ByteString
  }


{- | Rows a batch may materialize without any body bytes backing them
(see 'planNodes'). Generous for real data, small enough that a hostile
length cannot turn a few header bytes into gigabytes of vectors.
-}
maxUnbackedRows :: Int
maxUnbackedRows = 1 `shiftL` 20


{- | Walk the schema in node order before materializing anything:

* every field node the schema needs must exist and carry a valid
  length ('nodeLenAt');
* every view column must have its entry in @rbVariadicBufferCounts@,
  and that count must fit in the buffer list;
* the rows of arrays whose length no buffer pays for (nullable
  structs or fixed-size lists over null-typed or run-end-encoded
  children, zero-width fixed-size binary) are summed and capped by
  'maxUnbackedRows'. Every other array's length is checked against
  the size of one of its own buffers by its reader.

Returns the per-node variadic-count table used by 'materializeViewCol'.
-}
planNodes :: V.Vector Field -> RecordBatchDef -> Either String (VP.Vector Int)
planNodes topFields rb = do
  (!nodes, _, !unbacked, acc) <- goFields topFields (0, 0, 0, [])
  if unbacked > maxUnbackedRows
    then
      Left
        ( "Arrow.Column: record batch claims "
            ++ show unbacked
            ++ " rows with no buffer backing them (limit "
            ++ show maxUnbackedRows
            ++ ")"
        )
    else Right $! VP.fromListN nodes (reverse acc)
  where
    !nBufs = V.length (rbBuffers rb)

    goFields :: V.Vector Field -> (Int, Int, Int, [Int]) -> Either String (Int, Int, Int, [Int])
    goFields fs st = V.foldM' goField st fs

    goField (!ni, !vi, !ub, acc) f = do
      len <- nodeLenAt rb ni
      let !ub' = if allocatesUnbacked f then ub + len else ub
      if ub' > maxUnbackedRows
        then Right (ni + 1, vi, ub', acc)
        else case fieldDictionary f of
          -- The wire payload of a dictionary-encoded field is a single
          -- index array: one node, no children.
          Just _ -> Right (ni + 1, vi, ub', (-1) : acc)
          Nothing -> do
            (vc, vi') <- case fieldType f of
              AUtf8View -> viewCount vi
              ABinaryView -> viewCount vi
              _ -> Right (-1, vi)
            goFields (fieldChildren f) (ni + 1, vi', ub', vc : acc)

    viewCount vi = case rbVariadicBufferCounts rb V.!? vi of
      Nothing -> Left "Arrow.Column: view column has no variadicBufferCounts entry"
      Just c
        | c < 0 || c > fromIntegral nBufs ->
            Left ("Arrow.Column: invalid variadic buffer count " ++ show c)
        | otherwise -> Right (fromIntegral c, vi + 1)


-- | Does materializing this field allocate rows that no buffer backs?
allocatesUnbacked :: Field -> Bool
allocatesUnbacked f
  | Just _ <- fieldDictionary f = False
  | otherwise = case fieldType f of
      AStruct -> fieldNullable f && not (lengthBacked f)
      AFixedSizeList _ -> fieldNullable f && not (lengthBacked f)
      AFixedSizeBinary 0 -> True
      _ -> False


{- | Is the length of an array of this field paid for by bytes in some
buffer of the array or its descendants? Null arrays have no buffers,
and a run-end-encoded array's logical length can be any value its last
run end names, so neither backs a parent's validity bitmap.
-}
lengthBacked :: Field -> Bool
lengthBacked f
  | Just _ <- fieldDictionary f = True
  | otherwise = case fieldType f of
      ANull -> False
      ARunEndEncoded -> False
      AStruct -> V.any lengthBacked (fieldChildren f)
      AFixedSizeList n -> n > 0 && V.any lengthBacked (fieldChildren f)
      AFixedSizeBinary w -> w > 0
      _ -> True


materializeFieldsN :: Ctx -> V.Vector Field -> Int -> Int -> Either String (V.Vector ColumnArray, Int, Int)
materializeFieldsN ctx fields !nodeIdx0 !bufIdx0 = go 0 nodeIdx0 bufIdx0 []
  where
    go !i !ni !bi acc
      | i >= V.length fields = Right (V.fromListN i (reverse acc), ni, bi)
      | otherwise = do
          (col, ni', bi') <- materializeNode ctx (V.unsafeIndex fields i) ni bi
          go (i + 1) ni' bi' (col : acc)


materializeNode :: Ctx -> Field -> Int -> Int -> Either String (ColumnArray, Int, Int)
materializeNode ctx f !nodeIdx !bufIdx =
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
      AStruct -> materializeStruct ctx f nodeIdx bufIdx
      AList -> materializeListCol ctx False f nodeIdx bufIdx
      ALargeList -> materializeListCol ctx True f nodeIdx bufIdx
      AMap _ -> materializeMapCol ctx f nodeIdx bufIdx
      AUnion mode ids -> materializeUnionCol ctx mode ids f nodeIdx bufIdx
      AFixedSizeList n -> materializeFixedSizeListCol ctx n f nodeIdx bufIdx
      AInterval u -> materializeIntervalCol endian u f rb body nodeIdx bufIdx
      ARunEndEncoded -> materializeRunEndEncodedCol ctx f nodeIdx bufIdx
      AListView -> materializeListViewCol ctx False f nodeIdx bufIdx
      ALargeListView -> materializeListViewCol ctx True f nodeIdx bufIdx
      AUtf8View -> materializeViewCol ctx True f nodeIdx bufIdx
      ABinaryView -> materializeViewCol ctx False f nodeIdx bufIdx
      _ -> materializeOne endian f rb body nodeIdx bufIdx
  where
    !endian = ctxEndian ctx
    !rb = ctxRb ctx
    !body = ctxBody ctx


{- | Materialize the /indices/ portion of a dictionary-encoded
column. The values referenced by these indices live in a
separate 'DictionaryBatch' message keyed by @did@; combine via
'resolveDictionaryColumn' once both parts are in hand.

Any signed or unsigned 8/16/32/64-bit index type is accepted. A
nullable field yields 'ColDictionaryMaybe' (null index rows are
'Nothing'); a non-nullable one yields 'ColDictionary'. Negative
indices and indices above @maxBound :: Int32@ are rejected.
-}
materializeDictIndices
  :: Endianness
  -> Int64
  -> ArrowType
  -> Field
  -> RecordBatchDef
  -> ByteString
  -> Int
  -> Int
  -> Either String (ColumnArray, Int, Int)
materializeDictIndices endian did indexTy f rb body !nodeIdx !bufIdx = do
  case indexTy of
    AInt w _ | w == 8 || w == 16 || w == 32 || w == 64 -> Right ()
    _ -> Left ("Arrow.Column: dictionary index type must be an 8/16/32/64-bit integer, got " ++ show indexTy)
  let !indexField =
        f
          { fieldType = indexTy
          , fieldDictionary = Nothing
          , fieldChildren = V.empty
          }
  (idxCol, !ni', !bi') <- materializeOne endian indexField rb body nodeIdx bufIdx
  placeholder <- placeholderColumn f
  col <-
    if fieldNullable f
      then (\ix -> ColDictionaryMaybe did ix placeholder) <$> toNullableInt32Indices idxCol
      else (\ix -> ColDictionary did ix placeholder) <$> toInt32Indices idxCol
  Right (col, ni', bi')


{- | Convert a non-nullable integer column of any width and
signedness into the @VP.Vector Int32@ used by 'ColDictionary'.
-}
toInt32Indices :: ColumnArray -> Either String (VP.Vector Int32)
toInt32Indices = \case
  ColInt8 v -> checked (VP.all (>= 0) v) (VP.map fromIntegral v)
  ColInt16 v -> checked (VP.all (>= 0) v) (VP.map fromIntegral v)
  ColInt32 v -> checked (VP.all (>= 0) v) v
  ColInt64 v -> checked (VP.all (\x -> x >= 0 && x <= int32Max) v) (VP.map fromIntegral v)
  ColUInt8 v -> Right (VP.map fromIntegral v)
  ColUInt16 v -> Right (VP.map fromIntegral v)
  ColUInt32 v -> checked (VP.all (<= fromIntegral int32Max) v) (VP.map fromIntegral v)
  ColUInt64 v -> checked (VP.all (<= fromIntegral int32Max) v) (VP.map fromIntegral v)
  c -> Left ("Arrow.Column: dictionary index column must be a non-nullable integer column, got " ++ columnTag c)
  where
    checked ok ix = if ok then Right ix else Left dictIndexRangeError


-- | Nullable counterpart of 'toInt32Indices', for 'ColDictionaryMaybe'.
toNullableInt32Indices :: ColumnArray -> Either String (V.Vector (Maybe Int32))
toNullableInt32Indices = \case
  ColInt8Maybe v -> checked (>= 0) v
  ColInt16Maybe v -> checked (>= 0) v
  ColInt32Maybe v -> checked (>= 0) v
  ColInt64Maybe v -> checked (\x -> x >= 0 && x <= int32Max) v
  ColUInt8Maybe v -> checked (const True) v
  ColUInt16Maybe v -> checked (const True) v
  ColUInt32Maybe v -> checked (<= fromIntegral int32Max) v
  ColUInt64Maybe v -> checked (<= fromIntegral int32Max) v
  c -> Left ("Arrow.Column: dictionary index column must be a nullable integer column, got " ++ columnTag c)
  where
    checked :: Integral a => (a -> Bool) -> V.Vector (Maybe a) -> Either String (V.Vector (Maybe Int32))
    checked ok v =
      if V.all (maybe True ok) v
        then Right (V.map (fmap fromIntegral) v)
        else Left dictIndexRangeError


int32Max :: Int64
int32Max = fromIntegral (maxBound :: Int32)


dictIndexRangeError :: String
dictIndexRangeError = "Arrow.Column: dictionary index is negative or does not fit in Int32"


-- | Constructor name of a column, for error messages.
columnTag :: ColumnArray -> String
columnTag = takeWhile (/= ' ') . show


{- | Replace the placeholder values column inside every
'ColDictionary' / 'ColDictionaryMaybe' (at any nesting depth) with
the dictionary registered for its id.

Fails when a column references a dictionary id the lookup does not
know (unless the column has no non-null index, in which case the
typed placeholder is kept) and when any non-null index is outside
the dictionary. Dictionary values themselves are not searched for
further dictionary columns.
-}
resolveDictionaryColumn
  :: (Int64 -> Maybe ColumnArray)
  -- ^ dictionary-id to values column
  -> ColumnArray
  -> Either String ColumnArray
resolveDictionaryColumn lookupVals = go
  where
    go col = case col of
      ColDictionary did indices placeholder ->
        ColDictionary did indices
          <$> resolve did placeholder (VP.null indices) (\n -> VP.all (inRange n) indices)
      ColDictionaryMaybe did indices placeholder ->
        ColDictionaryMaybe did indices
          <$> resolve did placeholder (V.all isNothing indices) (\n -> V.all (maybe True (inRange n)) indices)
      ColStruct cs -> ColStruct <$> V.mapM (traverse go) cs
      ColStructMaybe v cs -> ColStructMaybe v <$> V.mapM (traverse go) cs
      ColList offs c -> ColList offs <$> go c
      ColListMaybe v offs c -> ColListMaybe v offs <$> go c
      ColLargeList offs c -> ColLargeList offs <$> go c
      ColLargeListMaybe v offs c -> ColLargeListMaybe v offs <$> go c
      ColFixedSizeList n c -> ColFixedSizeList n <$> go c
      ColFixedSizeListMaybe n v c -> ColFixedSizeListMaybe n v <$> go c
      ColMap offs k v -> ColMap offs <$> go k <*> go v
      ColMapMaybe vs offs k v -> ColMapMaybe vs offs <$> go k <*> go v
      ColDenseUnion ts offs cs -> ColDenseUnion ts offs <$> V.mapM go cs
      ColSparseUnion ts cs -> ColSparseUnion ts <$> V.mapM go cs
      ColRunEndEncoded re vs -> ColRunEndEncoded re <$> go vs
      ColListView offs sz c -> ColListView offs sz <$> go c
      ColListViewMaybe v offs sz c -> ColListViewMaybe v offs sz <$> go c
      ColLargeListView offs sz c -> ColLargeListView offs sz <$> go c
      ColLargeListViewMaybe v offs sz c -> ColLargeListViewMaybe v offs sz <$> go c
      _ -> Right col

    inRange :: Int -> Int32 -> Bool
    inRange n i = i >= 0 && fromIntegral i < n

    resolve did placeholder noIndices allIn = case lookupVals did of
      Nothing
        | noIndices -> Right placeholder
        | otherwise -> Left ("Arrow.Column: no dictionary batch for dictionary id " ++ show did)
      Just vals
        | allIn (columnLength vals) -> Right vals
        | otherwise ->
            Left
              ( "Arrow.Column: dictionary index out of range for dictionary id "
                  ++ show did
                  ++ " ("
                  ++ show (columnLength vals)
                  ++ " values)"
              )


{- | A typed, empty values column for a dictionary field, used until
'resolveDictionaryColumn' substitutes the real dictionary. The field's
value type and children determine the shape; nullability of the
values is not known before the dictionary batch arrives, so the
top-level placeholder is non-nullable.
-}
placeholderColumn :: Field -> Either String ColumnArray
placeholderColumn f = emptyColumnFor f {fieldNullable = False, fieldDictionary = Nothing}


{- | The zero-row column of a field's type, honouring the field's
nullability and any dictionary encoding (recursively for children).
-}
emptyColumnFor :: Field -> Either String ColumnArray
emptyColumnFor f = case fieldDictionary f of
  Just de -> do
    vals <- placeholderColumn f
    Right $
      if fieldNullable f
        then ColDictionaryMaybe (deId de) V.empty vals
        else ColDictionary (deId de) VP.empty vals
  Nothing -> case fieldType f of
    ANull -> Right (ColNull 0)
    AInt 8 True -> pick (ColInt8 VP.empty) (ColInt8Maybe V.empty)
    AInt 16 True -> pick (ColInt16 VP.empty) (ColInt16Maybe V.empty)
    AInt 32 True -> pick (ColInt32 VP.empty) (ColInt32Maybe V.empty)
    AInt 64 True -> pick (ColInt64 VP.empty) (ColInt64Maybe V.empty)
    AInt 8 False -> pick (ColUInt8 VP.empty) (ColUInt8Maybe V.empty)
    AInt 16 False -> pick (ColUInt16 VP.empty) (ColUInt16Maybe V.empty)
    AInt 32 False -> pick (ColUInt32 VP.empty) (ColUInt32Maybe V.empty)
    AInt 64 False -> pick (ColUInt64 VP.empty) (ColUInt64Maybe V.empty)
    AInt w _ -> Left ("Arrow.Column: unsupported integer bit width " ++ show w)
    AFloatingPoint Half -> pick (ColFloat16 VP.empty) (ColFloat16Maybe V.empty)
    AFloatingPoint Single -> pick (ColFloat VP.empty) (ColFloatMaybe V.empty)
    AFloatingPoint DoublePrecision -> pick (ColDouble VP.empty) (ColDoubleMaybe V.empty)
    ABinary -> pick (ColBinary V.empty) (ColBinaryMaybe V.empty)
    AUtf8 -> pick (ColUtf8 V.empty) (ColUtf8Maybe V.empty)
    ABool -> pick (ColBool V.empty) (ColBoolMaybe V.empty)
    ADecimal p s -> pick (ColDecimal128 p s V.empty) (ColDecimal128Maybe p s V.empty)
    ADecimal256 p s -> pick (ColDecimal256 p s V.empty) (ColDecimal256Maybe p s V.empty)
    ADate DateDay -> pick (ColDate32 VP.empty) (ColDate32Maybe V.empty)
    ADate DateMillisecond -> pick (ColDate64 VP.empty) (ColDate64Maybe V.empty)
    ATime u _
      | u == Second || u == Millisecond -> pick (ColTime32 VP.empty) (ColTime32Maybe V.empty)
      | otherwise -> pick (ColTime64 VP.empty) (ColTime64Maybe V.empty)
    ATimestamp _ _ -> pick (ColTimestamp VP.empty) (ColTimestampMaybe V.empty)
    ADuration _ -> pick (ColDuration VP.empty) (ColDurationMaybe V.empty)
    AInterval YearMonth -> pick (ColIntervalYearMonth VP.empty) (ColIntervalYearMonthMaybe V.empty)
    AInterval DayTime -> pick (ColIntervalDayTime VP.empty VP.empty) (ColIntervalDayTimeMaybe V.empty)
    AInterval MonthDayNano ->
      pick (ColIntervalMonthDayNano VP.empty VP.empty VP.empty) (ColIntervalMonthDayNanoMaybe V.empty)
    AFixedSizeBinary w -> pick (ColFixedSizeBinary w V.empty) (ColFixedSizeBinaryMaybe w V.empty)
    ALargeBinary -> pick (ColLargeBinary V.empty) (ColLargeBinaryMaybe V.empty)
    ALargeUtf8 -> pick (ColLargeUtf8 V.empty) (ColLargeUtf8Maybe V.empty)
    AUtf8View -> pick (ColUtf8View V.empty) (ColUtf8ViewMaybe V.empty)
    ABinaryView -> pick (ColBinaryView V.empty) (ColBinaryViewMaybe V.empty)
    AStruct -> do
      cs <- V.mapM (\c -> (,) (fieldName c) <$> emptyColumnFor c) (fieldChildren f)
      pick (ColStruct cs) (ColStructMaybe V.empty cs)
    AList -> do
      c <- onlyChild
      pick (ColList zero32 c) (ColListMaybe V.empty zero32 c)
    ALargeList -> do
      c <- onlyChild
      pick (ColLargeList zero64 c) (ColLargeListMaybe V.empty zero64 c)
    AFixedSizeList n -> do
      c <- onlyChild
      pick (ColFixedSizeList n c) (ColFixedSizeListMaybe n V.empty c)
    AListView -> do
      c <- onlyChild
      pick (ColListView VP.empty VP.empty c) (ColListViewMaybe V.empty VP.empty VP.empty c)
    ALargeListView -> do
      c <- onlyChild
      pick (ColLargeListView VP.empty VP.empty c) (ColLargeListViewMaybe V.empty VP.empty VP.empty c)
    AMap _ -> case V.toList (fieldChildren f) of
      [entries] | [kf, vf] <- V.toList (fieldChildren entries) -> do
        k <- emptyColumnFor kf
        v <- emptyColumnFor vf
        pick (ColMap zero32 k v) (ColMapMaybe V.empty zero32 k v)
      _ -> Left "Arrow.Column: map field must have one entries struct child with key and value"
    AUnion Dense _ -> ColDenseUnion VP.empty VP.empty <$> V.mapM emptyColumnFor (fieldChildren f)
    AUnion Sparse _ -> ColSparseUnion VP.empty <$> V.mapM emptyColumnFor (fieldChildren f)
    ARunEndEncoded -> case V.toList (fieldChildren f) of
      [ref, vf] -> ColRunEndEncoded <$> emptyColumnFor ref {fieldNullable = False} <*> emptyColumnFor vf
      _ -> Left "Arrow.Column: RunEndEncoded field must have exactly two children (run_ends, values)"
  where
    pick nonNull nullable = Right (if fieldNullable f then nullable else nonNull)
    zero32 = VP.singleton 0
    zero64 = VP.singleton 0
    onlyChild = case V.toList (fieldChildren f) of
      [c] -> emptyColumnFor c
      _ -> Left ("Arrow.Column: " ++ show (fieldType f) ++ " field must have exactly one child")


{- | The raw validity bitmap slot of a nullable field (and the next
buffer index). Callers expand it with 'unpackValidity' only after the
array's length has been checked against a buffer that backs it.
-}
validitySlot :: Ctx -> Field -> Int -> Either String (Maybe ByteString, Int)
validitySlot ctx f !bufIdx
  | fieldNullable f = do
      bs <- sliceBufAt (ctxRb ctx) (ctxBody ctx) bufIdx
      Right (Just bs, bufIdx + 1)
  | otherwise = Right (Nothing, bufIdx)
{-# INLINE validitySlot #-}


singleChild :: String -> Field -> Either String Field
singleChild what f = case V.toList (fieldChildren f) of
  [c] -> Right c
  cs -> Left ("Arrow.Column: " ++ what ++ " field must have exactly one child, has " ++ show (length cs))


{- | Read @len + 1@ list offsets (@width@ is 4 or 8 bytes). A
zero-length array may omit the offsets buffer entirely.
-}
readListOffsets
  :: VP.Prim a
  => String -> Int -> (ByteString -> Int -> a) -> a -> Int -> ByteString -> Either String (VP.Vector a)
readListOffsets what width get zero len bs
  | len == 0 && BS.null bs = Right (VP.singleton zero)
  | BS.length bs < (len + 1) * width =
      Left ("Arrow.Column: " ++ what ++ " offsets buffer too small")
  | otherwise = Right $! VP.generate (len + 1) (\i -> get bs (i * width))


{- | List offsets must start at or above zero, never decrease, and end
within the child array. One pass, no allocation.
-}
checkListOffsets :: (VP.Prim a, Integral a) => String -> Int -> VP.Vector a -> Either String ()
checkListOffsets what childLen offs
  | n == 0 = Right ()
  | VP.unsafeHead offs < 0 = Left ("Arrow.Column: " ++ what ++ " offsets start below zero")
  | toInteger (VP.unsafeLast offs) > toInteger childLen =
      Left
        ( "Arrow.Column: "
            ++ what
            ++ " offsets end at "
            ++ show (toInteger (VP.unsafeLast offs))
            ++ " but the child has "
            ++ show childLen
            ++ " rows"
        )
  | otherwise = go 1
  where
    !n = VP.length offs
    go !i
      | i >= n = Right ()
      | VP.unsafeIndex offs i < VP.unsafeIndex offs (i - 1) =
          Left ("Arrow.Column: " ++ what ++ " offsets decrease at row " ++ show (i - 1))
      | otherwise = go (i + 1)


-- | Every child of a struct-like parent must cover the parent's rows.
checkChildLengths :: String -> Int -> V.Vector ColumnArray -> Either String ()
checkChildLengths what len cols = case V.findIndex (\c -> columnLength c < len) cols of
  Nothing -> Right ()
  Just k ->
    Left
      ( "Arrow.Column: "
          ++ what
          ++ " child #"
          ++ show k
          ++ " has "
          ++ show (columnLength (V.unsafeIndex cols k))
          ++ " rows, parent has "
          ++ show len
      )


materializeStruct :: Ctx -> Field -> Int -> Int -> Either String (ColumnArray, Int, Int)
materializeStruct ctx f !nodeIdx !bufIdx = do
  len <- nodeLenAt (ctxRb ctx) nodeIdx
  (validBs, !bufIdx1) <- validitySlot ctx f bufIdx
  (childCols, !nodeIdx2, !bufIdx2) <- materializeFieldsN ctx (fieldChildren f) (nodeIdx + 1) bufIdx1
  checkChildLengths "struct" len childCols
  validity <- traverse (unpackValidity len) validBs
  let namedChildren = V.zipWith (\child col -> (fieldName child, col)) (fieldChildren f) childCols
  case validity of
    Nothing -> Right (ColStruct namedChildren, nodeIdx2, bufIdx2)
    Just vs -> Right (ColStructMaybe vs namedChildren, nodeIdx2, bufIdx2)


-- | List (32-bit offsets) or LargeList (64-bit offsets).
materializeListCol :: Ctx -> Bool -> Field -> Int -> Int -> Either String (ColumnArray, Int, Int)
materializeListCol ctx large f !nodeIdx !bufIdx = do
  let !rb = ctxRb ctx
      !endian = ctxEndian ctx
      !what = if large then "large list" else "list"
  len <- nodeLenAt rb nodeIdx
  (validBs, !bufIdx1) <- validitySlot ctx f bufIdx
  offBs <- sliceBufAt rb (ctxBody ctx) bufIdx1
  childField <- singleChild what f
  if large
    then do
      offsets <- readListOffsets what 8 (\bs o -> int64FromWord (readWord64 endian bs o)) 0 len offBs
      validity <- traverse (unpackValidity len) validBs
      (childCol, !nodeIdx2, !bufIdx2) <- materializeNode ctx childField (nodeIdx + 1) (bufIdx1 + 1)
      checkListOffsets what (columnLength childCol) offsets
      Right (maybe (ColLargeList offsets childCol) (\vs -> ColLargeListMaybe vs offsets childCol) validity, nodeIdx2, bufIdx2)
    else do
      offsets <- readListOffsets what 4 (\bs o -> int32FromWord (readWord32 endian bs o)) 0 len offBs
      validity <- traverse (unpackValidity len) validBs
      (childCol, !nodeIdx2, !bufIdx2) <- materializeNode ctx childField (nodeIdx + 1) (bufIdx1 + 1)
      checkListOffsets what (columnLength childCol) offsets
      Right (maybe (ColList offsets childCol) (\vs -> ColListMaybe vs offsets childCol) validity, nodeIdx2, bufIdx2)


materializeMapCol :: Ctx -> Field -> Int -> Int -> Either String (ColumnArray, Int, Int)
materializeMapCol ctx f !nodeIdx !bufIdx = do
  let !rb = ctxRb ctx
      !endian = ctxEndian ctx
  len <- nodeLenAt rb nodeIdx
  (validBs, !bufIdx1) <- validitySlot ctx f bufIdx
  offBs <- sliceBufAt rb (ctxBody ctx) bufIdx1
  offsets <- readListOffsets "map" 4 (\bs o -> int32FromWord (readWord32 endian bs o)) 0 len offBs
  validity <- traverse (unpackValidity len) validBs
  structField <- singleChild "map" f
  (structCol, !nodeIdx2, !bufIdx2) <- materializeNode ctx structField (nodeIdx + 1) (bufIdx1 + 1)
  case structCol of
    ColStruct children
      | V.length children == 2 -> do
          let keyCol = snd (V.unsafeIndex children 0)
              valCol = snd (V.unsafeIndex children 1)
          checkListOffsets "map" (min (columnLength keyCol) (columnLength valCol)) offsets
          case validity of
            Nothing -> Right (ColMap offsets keyCol valCol, nodeIdx2, bufIdx2)
            Just vs -> Right (ColMapMaybe vs offsets keyCol valCol, nodeIdx2, bufIdx2)
    _ -> Left "Arrow.Column: map child must be a non-nullable struct of exactly (key, value)"


{- | Union columns carry no validity bitmap. The wire type ids are
mapped to child positions through the field's declared @typeIds@
(positional when absent), so the materialized type-id vector holds
child indices; unknown ids are rejected. Dense offsets must index
inside the selected child; sparse children must cover every row.
-}
materializeUnionCol :: Ctx -> UnionMode -> V.Vector Int32 -> Field -> Int -> Int -> Either String (ColumnArray, Int, Int)
materializeUnionCol ctx mode declaredIds f !nodeIdx !bufIdx = do
  let !rb = ctxRb ctx
      !body = ctxBody ctx
      !nChildren = V.length (fieldChildren f)
  len <- nodeLenAt rb nodeIdx
  childOf <- unionChildTable nChildren declaredIds
  typeIdsBs <- sliceBufAt rb body bufIdx
  if BS.length typeIdsBs < len
    then Left "Arrow.Column: union type_ids buffer too small"
    else Right ()
  let wireId i = fromIntegral (BSU.unsafeIndex typeIdsBs i) :: Int8
  case firstRow len (\i -> childFor childOf (wireId i) < 0) of
    Just i -> Left ("Arrow.Column: union row " ++ show i ++ " has undeclared type id " ++ show (wireId i))
    Nothing -> Right ()
  let !typeIds = VP.generate len (\i -> fromIntegral (childFor childOf (wireId i)) :: Int8)
  case mode of
    Dense -> do
      offsetsBs <- sliceBufAt rb body (bufIdx + 1)
      if BS.length offsetsBs < len * 4
        then Left "Arrow.Column: dense union offsets buffer too small"
        else Right ()
      let !offsets = VP.generate len $ \i -> int32FromWord (readWord32 (ctxEndian ctx) offsetsBs (i * 4))
      (children, !nodeIdx2, !bufIdx2) <- materializeFieldsN ctx (fieldChildren f) (nodeIdx + 1) (bufIdx + 2)
      let !childLens = VP.generate nChildren (\k -> columnLength (V.unsafeIndex children k))
          badRow i =
            let !off = VP.unsafeIndex offsets i
            in off < 0 || fromIntegral off >= VP.unsafeIndex childLens (fromIntegral (VP.unsafeIndex typeIds i))
      case firstRow len badRow of
        Just i -> Left ("Arrow.Column: dense union row " ++ show i ++ " has out-of-range child offset " ++ show (VP.unsafeIndex offsets i))
        Nothing -> Right (ColDenseUnion typeIds offsets children, nodeIdx2, bufIdx2)
    Sparse -> do
      (children, !nodeIdx2, !bufIdx2) <- materializeFieldsN ctx (fieldChildren f) (nodeIdx + 1) (bufIdx + 1)
      checkChildLengths "sparse union" len children
      Right (ColSparseUnion typeIds children, nodeIdx2, bufIdx2)


-- | First row in @[0, n)@ satisfying the predicate; a plain loop, no allocation.
firstRow :: Int -> (Int -> Bool) -> Maybe Int
firstRow n p = go 0
  where
    go !i
      | i >= n = Nothing
      | p i = Just i
      | otherwise = go (i + 1)
{-# INLINE firstRow #-}


{- | Map from wire type id (0..127) to child position, @-1@ when the id
is not declared. Empty @declaredIds@ means child @k@ has id @k@.
-}
unionChildTable :: Int -> V.Vector Int32 -> Either String (VP.Vector Int)
unionChildTable nChildren declaredIds
  | nChildren > 128 = Left ("Arrow.Column: union has " ++ show nChildren ++ " children, at most 128 allowed")
  | V.null declaredIds = Right $! VP.generate 128 (\t -> if t < nChildren then t else -1)
  | V.length declaredIds /= nChildren =
      Left
        ( "Arrow.Column: union declares "
            ++ show (V.length declaredIds)
            ++ " type ids for "
            ++ show nChildren
            ++ " children"
        )
  | V.any (\t -> t < 0 || t > 127) declaredIds = Left "Arrow.Column: union type ids must be in 0..127"
  | otherwise =
      let table = VP.accum (\_ k -> k) (VP.replicate 128 (-1)) (V.toList (V.imap (\k t -> (fromIntegral t, k)) declaredIds))
      in if VP.length (VP.filter (>= 0) table) /= nChildren
           then Left "Arrow.Column: union declares a type id twice"
           else Right table


childFor :: VP.Vector Int -> Int8 -> Int
childFor table t
  | t < 0 = -1
  | otherwise = VP.unsafeIndex table (fromIntegral t)
{-# INLINE childFor #-}


materializeFixedSizeListCol :: Ctx -> Int -> Field -> Int -> Int -> Either String (ColumnArray, Int, Int)
materializeFixedSizeListCol ctx listSize f !nodeIdx !bufIdx = do
  len <- nodeLenAt (ctxRb ctx) nodeIdx
  if listSize < 0 || (listSize > 0 && len > maxArrayLength `quot` listSize)
    then Left ("Arrow.Column: fixed-size list of size " ++ show listSize ++ " cannot have " ++ show len ++ " rows")
    else Right ()
  (validBs, !bufIdx1) <- validitySlot ctx f bufIdx
  childField <- singleChild "fixed-size list" f
  (childCol, !nodeIdx2, !bufIdx2) <- materializeNode ctx childField (nodeIdx + 1) bufIdx1
  if columnLength childCol < len * listSize
    then
      Left
        ( "Arrow.Column: fixed-size list child has "
            ++ show (columnLength childCol)
            ++ " rows, needs "
            ++ show (len * listSize)
        )
    else Right ()
  validity <- traverse (unpackValidity len) validBs
  case validity of
    Nothing -> Right (ColFixedSizeList listSize childCol, nodeIdx2, bufIdx2)
    Just vs -> Right (ColFixedSizeListMaybe listSize vs childCol, nodeIdx2, bufIdx2)


{- | Read one INTERVAL field. Interval columns are flat (one field
node, validity + data buffers) but the data layout depends on the
unit:

  YearMonth     : 4 bytes per row, one i32 (months).
  DayTime       : 8 bytes per row, pair of i32 (days, millis).
  MonthDayNano  : 16 bytes per row, (i32 months, i32 days, i64 nanos).
-}
materializeIntervalCol
  :: Endianness
  -> IntervalUnit
  -> Field
  -> RecordBatchDef
  -> ByteString
  -> Int
  -> Int
  -> Either String (ColumnArray, Int, Int)
materializeIntervalCol endian unit f rb body !nodeIdx !bufIdx = do
  len <- nodeLenAt rb nodeIdx
  let !nodeIdx1 = nodeIdx + 1
      months w bs i = int32FromWord (readWord32 endian bs (i * w))
      dayTime bs i = (int32FromWord (readWord32 endian bs (i * 8)), int32FromWord (readWord32 endian bs (i * 8 + 4)))
      mdn bs i =
        ( int32FromWord (readWord32 endian bs (i * 16))
        , int32FromWord (readWord32 endian bs (i * 16 + 4))
        , int64FromWord (readWord64 endian bs (i * 16 + 8))
        )
  if fieldNullable f
    then do
      col <- case unit of
        YearMonth -> ColIntervalYearMonthMaybe <$> readNullable "interval YEAR_MONTH" 4 len rb body bufIdx (months 4)
        DayTime -> ColIntervalDayTimeMaybe <$> readNullable "interval DAY_TIME" 8 len rb body bufIdx dayTime
        MonthDayNano -> ColIntervalMonthDayNanoMaybe <$> readNullable "interval MONTH_DAY_NANO" 16 len rb body bufIdx mdn
      Right (col, nodeIdx1, bufIdx + 2)
    else do
      dataBs <- sliceBufAt rb body bufIdx
      let width = case unit of
            YearMonth -> 4
            DayTime -> 8
            MonthDayNano -> 16
      if BS.length dataBs < len * width
        then Left ("Arrow.Column: interval " ++ show unit ++ " buffer too small")
        else do
          let col = case unit of
                YearMonth -> ColIntervalYearMonth (VP.generate len (months 4 dataBs))
                DayTime ->
                  ColIntervalDayTime
                    (VP.generate len (fst . dayTime dataBs))
                    (VP.generate len (snd . dayTime dataBs))
                MonthDayNano ->
                  ColIntervalMonthDayNano
                    (VP.generate len (\i -> let (m, _, _) = mdn dataBs i in m))
                    (VP.generate len (\i -> let (_, d, _) = mdn dataBs i in d))
                    (VP.generate len (\i -> let (_, _, n) = mdn dataBs i in n))
          Right (col, nodeIdx1, bufIdx + 1)


-- ============================================================
-- Post-V5 columns: RunEndEncoded, ListView/LargeListView,
-- Utf8View / BinaryView.
-- ============================================================

{- | RunEndEncoded: parent has zero buffers (no validity, no data),
exactly two children: @run_ends@ (Int16/32/64) and @values@ (any
type, may be nullable). Run ends must be non-null, strictly
increasing and positive, the last must reach the parent's length,
and there must be a value for every run.
-}
materializeRunEndEncodedCol :: Ctx -> Field -> Int -> Int -> Either String (ColumnArray, Int, Int)
materializeRunEndEncodedCol ctx f !nodeIdx !bufIdx = do
  len <- nodeLenAt (ctxRb ctx) nodeIdx
  case V.toList (fieldChildren f) of
    [runEndsField, valuesField] -> do
      (runEndsCol, !nodeIdx2, !bufIdx1) <- materializeNode ctx runEndsField (nodeIdx + 1) bufIdx
      (valuesCol, !nodeIdx3, !bufIdx2) <- materializeNode ctx valuesField nodeIdx2 bufIdx1
      nRuns <- case runEndsCol of
        ColInt16 v -> checkRunEnds len v
        ColInt32 v -> checkRunEnds len v
        ColInt64 v -> checkRunEnds len v
        _ -> Left "Arrow.Column: run_ends must be a non-nullable int16, int32 or int64 array"
      if columnLength valuesCol < nRuns
        then
          Left
            ( "Arrow.Column: run-end encoded array has "
                ++ show nRuns
                ++ " runs but "
                ++ show (columnLength valuesCol)
                ++ " values"
            )
        else Right (ColRunEndEncoded runEndsCol valuesCol, nodeIdx3, bufIdx2)
    _ ->
      Left "Arrow.Column: RunEndEncoded must have exactly two children (run_ends, values)"


-- | Validate run ends against the logical length; returns the run count.
checkRunEnds :: (VP.Prim a, Integral a) => Int -> VP.Vector a -> Either String Int
checkRunEnds len ends
  | n == 0 =
      if len == 0 then Right 0 else Left "Arrow.Column: run-end encoded array has rows but no runs"
  | VP.unsafeHead ends <= 0 = Left "Arrow.Column: run ends must be positive"
  | toInteger (VP.unsafeLast ends) < toInteger len =
      Left "Arrow.Column: last run end is below the array length"
  | otherwise = case firstRow (n - 1) (\i -> VP.unsafeIndex ends (i + 1) <= VP.unsafeIndex ends i) of
      Just i -> Left ("Arrow.Column: run ends not strictly increasing at run " ++ show (i + 1))
      Nothing -> Right n
  where
    !n = VP.length ends


{- | ListView / LargeListView. Buffers (in order): validity (when
nullable), offsets, sizes. The child elements may overlap or
appear in any order; every row's @[offset, offset + size)@ must lie
within the child array.
-}
materializeListViewCol :: Ctx -> Bool -> Field -> Int -> Int -> Either String (ColumnArray, Int, Int)
materializeListViewCol ctx large f !nodeIdx !bufIdx = do
  let !rb = ctxRb ctx
      !body = ctxBody ctx
      !endian = ctxEndian ctx
  len <- nodeLenAt rb nodeIdx
  (validBs, !bufIdx1) <- validitySlot ctx f bufIdx
  offBs <- sliceBufAt rb body bufIdx1
  sizBs <- sliceBufAt rb body (bufIdx1 + 1)
  let !w = if large then 8 else 4
      !need = len * w
  if BS.length offBs < need || BS.length sizBs < need
    then Left "Arrow.Column: list-view offsets or sizes buffer too small"
    else Right ()
  validity <- traverse (unpackValidity len) validBs
  childField <- singleChild "list-view" f
  (childCol, !nodeIdx2, !bufIdx2) <- materializeNode ctx childField (nodeIdx + 1) (bufIdx1 + 2)
  let !childLen = columnLength childCol
      check :: (VP.Prim a, Integral a) => VP.Vector a -> VP.Vector a -> Either String ()
      check offs sizs = case firstRow len (\i -> outOfRange (VP.unsafeIndex offs i) (VP.unsafeIndex sizs i)) of
        Just i -> Left ("Arrow.Column: list-view row " ++ show i ++ " points outside its child")
        Nothing -> Right ()
      outOfRange :: Integral a => a -> a -> Bool
      outOfRange o0 s0 =
        let !o = fromIntegral o0 :: Int
            !s = fromIntegral s0 :: Int
        in o < 0 || s < 0 || o > childLen || s > childLen - o
  if large
    then do
      let offs = VP.generate len $ \i -> int64FromWord (readWord64 endian offBs (i * 8))
          sizs = VP.generate len $ \i -> int64FromWord (readWord64 endian sizBs (i * 8))
      check offs sizs
      Right $! case validity of
        Nothing -> (ColLargeListView offs sizs childCol, nodeIdx2, bufIdx2)
        Just vs -> (ColLargeListViewMaybe vs offs sizs childCol, nodeIdx2, bufIdx2)
    else do
      let offs = VP.generate len $ \i -> int32FromWord (readWord32 endian offBs (i * 4))
          sizs = VP.generate len $ \i -> int32FromWord (readWord32 endian sizBs (i * 4))
      check offs sizs
      Right $! case validity of
        Nothing -> (ColListView offs sizs childCol, nodeIdx2, bufIdx2)
        Just vs -> (ColListViewMaybe vs offs sizs childCol, nodeIdx2, bufIdx2)


{- | Utf8View / BinaryView. Buffers: validity (optional), view
(n × 16 bytes), then the column's variadic data buffers; their count
is this column's entry in 'rbVariadicBufferCounts' (preorder over
view columns, resolved by 'planNodes').

Each 16-byte view is laid out:

@
  length         : i32 (little-endian)
  if length <= 12:
    inlined bytes (length bytes), zero-padded to 12 total
  else:
    prefix       : 4 bytes (first 4 of the string)
    buffer_index : i32
    buffer_offset: i32
@

Null rows are not resolved; every valid row's reference is checked
against the referenced data buffer.
-}
materializeViewCol :: Ctx -> Bool -> Field -> Int -> Int -> Either String (ColumnArray, Int, Int)
materializeViewCol ctx utf8 f !nodeIdx !bufIdx = do
  let !rb = ctxRb ctx
      !body = ctxBody ctx
  len <- nodeLenAt rb nodeIdx
  varCount <- case ctxViews ctx VP.!? nodeIdx of
    Just c | c >= 0 -> Right c
    _ -> Left "Arrow.Column: view column has no variadic buffer count"
  (validBs, !bufIdx1) <- validitySlot ctx f bufIdx
  viewBs <- sliceBufAt rb body bufIdx1
  if BS.length viewBs < len * 16
    then Left "Arrow.Column: view buffer too small"
    else Right ()
  validity <- traverse (unpackValidity len) validBs
  dataBufs <- V.generateM varCount (\i -> sliceBufAt rb body (bufIdx1 + 1 + i))
  let !bufIdx2 = bufIdx1 + 1 + varCount
      row :: Int -> Either String ByteString
      row = resolveView viewBs dataBufs
      utf8Row' i = row i >>= utf8Row "Utf8View"
      isValid i = maybe True (`V.unsafeIndex` i) validity
      maybeRow :: (Int -> Either String a) -> Int -> Either String (Maybe a)
      maybeRow get i = if isValid i then Just <$> get i else Right Nothing
  result <- case (utf8, validity) of
    (True, Nothing) -> ColUtf8View <$> V.generateM len utf8Row'
    (True, Just _) -> ColUtf8ViewMaybe <$> V.generateM len (maybeRow utf8Row')
    (False, Nothing) -> ColBinaryView <$> V.generateM len row
    (False, Just _) -> ColBinaryViewMaybe <$> V.generateM len (maybeRow row)
  Right (result, nodeIdx + 1, bufIdx2)


-- | Bytes of view @i@; the caller guarantees @viewBs@ holds @16 * (i + 1)@ bytes.
resolveView :: ByteString -> V.Vector ByteString -> Int -> Either String ByteString
resolveView viewBs dataBufs i =
  let !off = i * 16
      !len = fromIntegral (readLE32 viewBs off) :: Int
  in if len <= 12
       then Right $! BSU.unsafeTake len (BSU.unsafeDrop (off + 4) viewBs)
       else do
         let !bufIdx = fromIntegral (readLE32 viewBs (off + 8)) :: Int
             !bufOff = fromIntegral (readLE32 viewBs (off + 12)) :: Int
         case dataBufs V.!? bufIdx of
           Nothing -> Left "Arrow.Column: view references unknown data buffer index"
           Just db ->
             if bufOff > BS.length db - len
               then Left "Arrow.Column: view payload out of range"
               else Right $! BSU.unsafeTake len (BSU.unsafeDrop bufOff db)


-- ============================================================
-- Row slicing
-- ============================================================

{- | Take @len@ rows starting at @start@ from a 'ColumnArray'.

Total over every constructor; the result always has exactly
@len@ rows (after clamping). Flat columns slice their vectors
without copying. Offset-based columns (lists, maps, list views)
slice the offsets and keep the child intact, so the offsets of a
slice need not start at zero. Struct and sparse-union children are
sliced in step; a dense union slices its type ids and offsets;
fixed-size lists slice the child by @size * row@; a dictionary
slices its indices; a run-end-encoded column keeps only the runs
overlapping the window and clips their ends.

@start@ + @len@ are clamped to the column's logical length;
a negative @start@ becomes @0@; a @len@ that runs past the
end is truncated.
-}
sliceColumnArray :: Int -> Int -> ColumnArray -> ColumnArray
sliceColumnArray !start0 !len0 col =
  let !n = columnLength col
      !start = min n (max 0 start0)
      !len = max 0 (min len0 (n - start))
  in if len == n && start == 0
       then col
       else sliceRows start len col


-- | Clamped 'VP.slice'.
pslice :: VP.Prim a => Int -> Int -> VP.Vector a -> VP.Vector a
pslice s l v =
  let !s' = min (VP.length v) (max 0 s)
  in VP.slice s' (max 0 (min l (VP.length v - s'))) v


-- | Clamped 'V.slice'.
bslice :: Int -> Int -> V.Vector a -> V.Vector a
bslice s l v =
  let !s' = min (V.length v) (max 0 s)
  in V.slice s' (max 0 (min l (V.length v - s'))) v


-- | Slice rows @[s, s + l)@; the caller has clamped the window.
sliceRows :: Int -> Int -> ColumnArray -> ColumnArray
sliceRows !s !l = \case
  ColInt8 v -> ColInt8 (pslice s l v)
  ColInt16 v -> ColInt16 (pslice s l v)
  ColInt32 v -> ColInt32 (pslice s l v)
  ColInt64 v -> ColInt64 (pslice s l v)
  ColUInt8 v -> ColUInt8 (pslice s l v)
  ColUInt16 v -> ColUInt16 (pslice s l v)
  ColUInt32 v -> ColUInt32 (pslice s l v)
  ColUInt64 v -> ColUInt64 (pslice s l v)
  ColFloat16 v -> ColFloat16 (pslice s l v)
  ColFloat v -> ColFloat (pslice s l v)
  ColDouble v -> ColDouble (pslice s l v)
  ColBool v -> ColBool (bslice s l v)
  ColUtf8 v -> ColUtf8 (bslice s l v)
  ColBinary v -> ColBinary (bslice s l v)
  ColLargeUtf8 v -> ColLargeUtf8 (bslice s l v)
  ColLargeBinary v -> ColLargeBinary (bslice s l v)
  ColFixedSizeBinary w v -> ColFixedSizeBinary w (bslice s l v)
  ColDate32 v -> ColDate32 (pslice s l v)
  ColDate64 v -> ColDate64 (pslice s l v)
  ColTime32 v -> ColTime32 (pslice s l v)
  ColTime64 v -> ColTime64 (pslice s l v)
  ColTimestamp v -> ColTimestamp (pslice s l v)
  ColDuration v -> ColDuration (pslice s l v)
  ColDecimal128 p sc v -> ColDecimal128 p sc (bslice s l v)
  ColDecimal256 p sc v -> ColDecimal256 p sc (bslice s l v)
  ColIntervalYearMonth v -> ColIntervalYearMonth (pslice s l v)
  ColIntervalDayTime d m -> ColIntervalDayTime (pslice s l d) (pslice s l m)
  ColIntervalMonthDayNano m d ns -> ColIntervalMonthDayNano (pslice s l m) (pslice s l d) (pslice s l ns)
  ColInt8Maybe v -> ColInt8Maybe (bslice s l v)
  ColInt16Maybe v -> ColInt16Maybe (bslice s l v)
  ColInt32Maybe v -> ColInt32Maybe (bslice s l v)
  ColInt64Maybe v -> ColInt64Maybe (bslice s l v)
  ColUInt8Maybe v -> ColUInt8Maybe (bslice s l v)
  ColUInt16Maybe v -> ColUInt16Maybe (bslice s l v)
  ColUInt32Maybe v -> ColUInt32Maybe (bslice s l v)
  ColUInt64Maybe v -> ColUInt64Maybe (bslice s l v)
  ColFloat16Maybe v -> ColFloat16Maybe (bslice s l v)
  ColFloatMaybe v -> ColFloatMaybe (bslice s l v)
  ColDoubleMaybe v -> ColDoubleMaybe (bslice s l v)
  ColBoolMaybe v -> ColBoolMaybe (bslice s l v)
  ColUtf8Maybe v -> ColUtf8Maybe (bslice s l v)
  ColBinaryMaybe v -> ColBinaryMaybe (bslice s l v)
  ColLargeUtf8Maybe v -> ColLargeUtf8Maybe (bslice s l v)
  ColLargeBinaryMaybe v -> ColLargeBinaryMaybe (bslice s l v)
  ColFixedSizeBinaryMaybe w v -> ColFixedSizeBinaryMaybe w (bslice s l v)
  ColDate32Maybe v -> ColDate32Maybe (bslice s l v)
  ColDate64Maybe v -> ColDate64Maybe (bslice s l v)
  ColTime32Maybe v -> ColTime32Maybe (bslice s l v)
  ColTime64Maybe v -> ColTime64Maybe (bslice s l v)
  ColTimestampMaybe v -> ColTimestampMaybe (bslice s l v)
  ColDurationMaybe v -> ColDurationMaybe (bslice s l v)
  ColDecimal128Maybe p sc v -> ColDecimal128Maybe p sc (bslice s l v)
  ColDecimal256Maybe p sc v -> ColDecimal256Maybe p sc (bslice s l v)
  ColIntervalYearMonthMaybe v -> ColIntervalYearMonthMaybe (bslice s l v)
  ColIntervalDayTimeMaybe v -> ColIntervalDayTimeMaybe (bslice s l v)
  ColIntervalMonthDayNanoMaybe v -> ColIntervalMonthDayNanoMaybe (bslice s l v)
  ColUtf8View v -> ColUtf8View (bslice s l v)
  ColUtf8ViewMaybe v -> ColUtf8ViewMaybe (bslice s l v)
  ColBinaryView v -> ColBinaryView (bslice s l v)
  ColBinaryViewMaybe v -> ColBinaryViewMaybe (bslice s l v)
  ColNull _ -> ColNull l
  ColStruct cs -> ColStruct (V.map (fmap (sliceColumnArray s l)) cs)
  ColStructMaybe valid cs -> ColStructMaybe (bslice s l valid) (V.map (fmap (sliceColumnArray s l)) cs)
  ColList offs c -> ColList (pslice s (l + 1) offs) c
  ColListMaybe valid offs c -> ColListMaybe (bslice s l valid) (pslice s (l + 1) offs) c
  ColLargeList offs c -> ColLargeList (pslice s (l + 1) offs) c
  ColLargeListMaybe valid offs c -> ColLargeListMaybe (bslice s l valid) (pslice s (l + 1) offs) c
  ColFixedSizeList w c -> ColFixedSizeList w (sliceColumnArray (s * w) (l * w) c)
  ColFixedSizeListMaybe w valid c ->
    ColFixedSizeListMaybe w (bslice s l valid) (sliceColumnArray (s * w) (l * w) c)
  ColMap offs ks vs -> ColMap (pslice s (l + 1) offs) ks vs
  ColMapMaybe valid offs ks vs -> ColMapMaybe (bslice s l valid) (pslice s (l + 1) offs) ks vs
  ColDenseUnion ts offs cs -> ColDenseUnion (pslice s l ts) (pslice s l offs) cs
  ColSparseUnion ts cs -> ColSparseUnion (pslice s l ts) (V.map (sliceColumnArray s l) cs)
  ColDictionary did ix vals -> ColDictionary did (pslice s l ix) vals
  ColDictionaryMaybe did ix vals -> ColDictionaryMaybe did (bslice s l ix) vals
  ColRunEndEncoded re vals -> sliceRunEnds s l re vals
  ColListView offs sz c -> ColListView (pslice s l offs) (pslice s l sz) c
  ColListViewMaybe valid offs sz c -> ColListViewMaybe (bslice s l valid) (pslice s l offs) (pslice s l sz) c
  ColLargeListView offs sz c -> ColLargeListView (pslice s l offs) (pslice s l sz) c
  ColLargeListViewMaybe valid offs sz c ->
    ColLargeListViewMaybe (bslice s l valid) (pslice s l offs) (pslice s l sz) c


{- | Slice a run-end-encoded column: keep the runs that overlap
@[s, s + l)@, rebase their ends to the window and clip the last one.
A column whose run ends are not an int16/32/64 column has logical
length zero (see 'columnLength'), so 'sliceColumnArray' never slices
it; it is returned unchanged here.
-}
sliceRunEnds :: Int -> Int -> ColumnArray -> ColumnArray -> ColumnArray
sliceRunEnds !s !l re vals = case re of
  ColInt16 v -> go ColInt16 v
  ColInt32 v -> go ColInt32 v
  ColInt64 v -> go ColInt64 v
  _ -> ColRunEndEncoded re vals
  where
    go :: (Integral a, VP.Prim a) => (VP.Vector a -> ColumnArray) -> VP.Vector a -> ColumnArray
    go con v
      | l == 0 = ColRunEndEncoded (con VP.empty) (sliceColumnArray 0 0 vals)
      | otherwise =
          let !e = s + l
              !i = fromMaybe (VP.length v) (VP.findIndex (\x -> fromIntegral x > s) v)
              !j = fromMaybe (VP.length v - 1) (VP.findIndex (\x -> fromIntegral x >= e) v)
              !k = max 0 (j - i + 1)
              ends = VP.map (\x -> fromIntegral (min (fromIntegral x) e - s)) (pslice i k v)
          in ColRunEndEncoded (con ends) (sliceColumnArray i k vals)


-- ============================================================
-- Concatenation
-- ============================================================

{- | Append the rows of the second column to the first.

Total over every constructor. Both columns must have the same
shape: the same constructor (a non-nullable column is promoted to
its nullable variant when the other side is nullable), the same
decimal precision and scale, fixed-size widths, struct field
names, and union arity. Offset-based columns are re-based so the
result's offsets start at zero and its children hold exactly the
referenced elements; dense-union offsets and list-view offsets of
the second column are shifted past the first column's children;
dictionary columns with different value columns concatenate their
values and shift the second column's indices. Offsets that would
overflow their integer width are rejected.
-}
concatColumnArray :: ColumnArray -> ColumnArray -> Either String ColumnArray
concatColumnArray a b = case (a, b) of
  (ColInt8 x, ColInt8 y) -> Right (ColInt8 (x VP.++ y))
  (ColInt16 x, ColInt16 y) -> Right (ColInt16 (x VP.++ y))
  (ColInt32 x, ColInt32 y) -> Right (ColInt32 (x VP.++ y))
  (ColInt64 x, ColInt64 y) -> Right (ColInt64 (x VP.++ y))
  (ColUInt8 x, ColUInt8 y) -> Right (ColUInt8 (x VP.++ y))
  (ColUInt16 x, ColUInt16 y) -> Right (ColUInt16 (x VP.++ y))
  (ColUInt32 x, ColUInt32 y) -> Right (ColUInt32 (x VP.++ y))
  (ColUInt64 x, ColUInt64 y) -> Right (ColUInt64 (x VP.++ y))
  (ColFloat16 x, ColFloat16 y) -> Right (ColFloat16 (x VP.++ y))
  (ColFloat x, ColFloat y) -> Right (ColFloat (x VP.++ y))
  (ColDouble x, ColDouble y) -> Right (ColDouble (x VP.++ y))
  (ColBool x, ColBool y) -> Right (ColBool (x V.++ y))
  (ColUtf8 x, ColUtf8 y) -> Right (ColUtf8 (x V.++ y))
  (ColBinary x, ColBinary y) -> Right (ColBinary (x V.++ y))
  (ColLargeUtf8 x, ColLargeUtf8 y) -> Right (ColLargeUtf8 (x V.++ y))
  (ColLargeBinary x, ColLargeBinary y) -> Right (ColLargeBinary (x V.++ y))
  (ColFixedSizeBinary w x, ColFixedSizeBinary w' y) | w == w' -> Right (ColFixedSizeBinary w (x V.++ y))
  (ColDate32 x, ColDate32 y) -> Right (ColDate32 (x VP.++ y))
  (ColDate64 x, ColDate64 y) -> Right (ColDate64 (x VP.++ y))
  (ColTime32 x, ColTime32 y) -> Right (ColTime32 (x VP.++ y))
  (ColTime64 x, ColTime64 y) -> Right (ColTime64 (x VP.++ y))
  (ColTimestamp x, ColTimestamp y) -> Right (ColTimestamp (x VP.++ y))
  (ColDuration x, ColDuration y) -> Right (ColDuration (x VP.++ y))
  (ColDecimal128 p s x, ColDecimal128 p' s' y) | p == p' && s == s' -> Right (ColDecimal128 p s (x V.++ y))
  (ColDecimal256 p s x, ColDecimal256 p' s' y) | p == p' && s == s' -> Right (ColDecimal256 p s (x V.++ y))
  (ColIntervalYearMonth x, ColIntervalYearMonth y) -> Right (ColIntervalYearMonth (x VP.++ y))
  (ColIntervalDayTime d m, ColIntervalDayTime d' m') -> Right (ColIntervalDayTime (d VP.++ d') (m VP.++ m'))
  (ColIntervalMonthDayNano m d ns, ColIntervalMonthDayNano m' d' ns') ->
    Right (ColIntervalMonthDayNano (m VP.++ m') (d VP.++ d') (ns VP.++ ns'))
  (ColInt8Maybe x, ColInt8Maybe y) -> Right (ColInt8Maybe (x V.++ y))
  (ColInt16Maybe x, ColInt16Maybe y) -> Right (ColInt16Maybe (x V.++ y))
  (ColInt32Maybe x, ColInt32Maybe y) -> Right (ColInt32Maybe (x V.++ y))
  (ColInt64Maybe x, ColInt64Maybe y) -> Right (ColInt64Maybe (x V.++ y))
  (ColUInt8Maybe x, ColUInt8Maybe y) -> Right (ColUInt8Maybe (x V.++ y))
  (ColUInt16Maybe x, ColUInt16Maybe y) -> Right (ColUInt16Maybe (x V.++ y))
  (ColUInt32Maybe x, ColUInt32Maybe y) -> Right (ColUInt32Maybe (x V.++ y))
  (ColUInt64Maybe x, ColUInt64Maybe y) -> Right (ColUInt64Maybe (x V.++ y))
  (ColFloat16Maybe x, ColFloat16Maybe y) -> Right (ColFloat16Maybe (x V.++ y))
  (ColFloatMaybe x, ColFloatMaybe y) -> Right (ColFloatMaybe (x V.++ y))
  (ColDoubleMaybe x, ColDoubleMaybe y) -> Right (ColDoubleMaybe (x V.++ y))
  (ColBoolMaybe x, ColBoolMaybe y) -> Right (ColBoolMaybe (x V.++ y))
  (ColUtf8Maybe x, ColUtf8Maybe y) -> Right (ColUtf8Maybe (x V.++ y))
  (ColBinaryMaybe x, ColBinaryMaybe y) -> Right (ColBinaryMaybe (x V.++ y))
  (ColLargeUtf8Maybe x, ColLargeUtf8Maybe y) -> Right (ColLargeUtf8Maybe (x V.++ y))
  (ColLargeBinaryMaybe x, ColLargeBinaryMaybe y) -> Right (ColLargeBinaryMaybe (x V.++ y))
  (ColFixedSizeBinaryMaybe w x, ColFixedSizeBinaryMaybe w' y) | w == w' -> Right (ColFixedSizeBinaryMaybe w (x V.++ y))
  (ColDate32Maybe x, ColDate32Maybe y) -> Right (ColDate32Maybe (x V.++ y))
  (ColDate64Maybe x, ColDate64Maybe y) -> Right (ColDate64Maybe (x V.++ y))
  (ColTime32Maybe x, ColTime32Maybe y) -> Right (ColTime32Maybe (x V.++ y))
  (ColTime64Maybe x, ColTime64Maybe y) -> Right (ColTime64Maybe (x V.++ y))
  (ColTimestampMaybe x, ColTimestampMaybe y) -> Right (ColTimestampMaybe (x V.++ y))
  (ColDurationMaybe x, ColDurationMaybe y) -> Right (ColDurationMaybe (x V.++ y))
  (ColDecimal128Maybe p s x, ColDecimal128Maybe p' s' y) | p == p' && s == s' -> Right (ColDecimal128Maybe p s (x V.++ y))
  (ColDecimal256Maybe p s x, ColDecimal256Maybe p' s' y) | p == p' && s == s' -> Right (ColDecimal256Maybe p s (x V.++ y))
  (ColIntervalYearMonthMaybe x, ColIntervalYearMonthMaybe y) -> Right (ColIntervalYearMonthMaybe (x V.++ y))
  (ColIntervalDayTimeMaybe x, ColIntervalDayTimeMaybe y) -> Right (ColIntervalDayTimeMaybe (x V.++ y))
  (ColIntervalMonthDayNanoMaybe x, ColIntervalMonthDayNanoMaybe y) -> Right (ColIntervalMonthDayNanoMaybe (x V.++ y))
  (ColUtf8View x, ColUtf8View y) -> Right (ColUtf8View (x V.++ y))
  (ColUtf8ViewMaybe x, ColUtf8ViewMaybe y) -> Right (ColUtf8ViewMaybe (x V.++ y))
  (ColBinaryView x, ColBinaryView y) -> Right (ColBinaryView (x V.++ y))
  (ColBinaryViewMaybe x, ColBinaryViewMaybe y) -> Right (ColBinaryViewMaybe (x V.++ y))
  (ColNull x, ColNull y) -> Right (ColNull (x + y))
  (ColStruct x, ColStruct y) -> ColStruct <$> concatStructChildren x y
  (ColStructMaybe vx x, ColStructMaybe vy y) -> ColStructMaybe (vx V.++ vy) <$> concatStructChildren x y
  (ColList ox cx, ColList oy cy) -> do
    (o, cs) <- concatOffsetChildren int32Max ox [cx] oy [cy]
    one (ColList o) cs
  (ColListMaybe vx ox cx, ColListMaybe vy oy cy) -> do
    (o, cs) <- concatOffsetChildren int32Max ox [cx] oy [cy]
    one (ColListMaybe (vx V.++ vy) o) cs
  (ColLargeList ox cx, ColLargeList oy cy) -> do
    (o, cs) <- concatOffsetChildren maxBound ox [cx] oy [cy]
    one (ColLargeList o) cs
  (ColLargeListMaybe vx ox cx, ColLargeListMaybe vy oy cy) -> do
    (o, cs) <- concatOffsetChildren maxBound ox [cx] oy [cy]
    one (ColLargeListMaybe (vx V.++ vy) o) cs
  (ColMap ox kx vx, ColMap oy ky vy) -> do
    (o, cs) <- concatOffsetChildren int32Max ox [kx, vx] oy [ky, vy]
    two (ColMap o) cs
  (ColMapMaybe nx ox kx vx, ColMapMaybe ny oy ky vy) -> do
    (o, cs) <- concatOffsetChildren int32Max ox [kx, vx] oy [ky, vy]
    two (ColMapMaybe (nx V.++ ny) o) cs
  (ColFixedSizeList w x, ColFixedSizeList w' y)
    | w == w' -> ColFixedSizeList w <$> concatFixedChildren w (columnLength a) x (columnLength b) y
  (ColFixedSizeListMaybe w vx x, ColFixedSizeListMaybe w' vy y)
    | w == w' -> ColFixedSizeListMaybe w (vx V.++ vy) <$> concatFixedChildren w (V.length vx) x (V.length vy) y
  (ColDenseUnion tx ox cx, ColDenseUnion ty oy cy)
    | V.length cx == V.length cy -> do
        let !k = V.length cx
            shifts = V.map columnLength cx
        if VP.all (\t -> t >= 0 && fromIntegral t < k) ty
          then Right ()
          else Left "Arrow.Column.concatColumnArray: dense union type id out of range"
        children <- V.zipWithM concatColumnArray cx cy
        if V.all (\c -> fromIntegral (columnLength c) <= int32Max) children
          then Right ()
          else Left "Arrow.Column.concatColumnArray: dense union child exceeds Int32 offsets"
        let oy' = VP.zipWith (\t o -> o + fromIntegral (V.unsafeIndex shifts (fromIntegral t))) ty oy
        Right (ColDenseUnion (tx VP.++ ty) (ox VP.++ oy') children)
  (ColSparseUnion tx cx, ColSparseUnion ty cy)
    | V.length cx == V.length cy ->
        ColSparseUnion (tx VP.++ ty)
          <$> V.zipWithM
            (\x y -> concatColumnArray (sliceColumnArray 0 (VP.length tx) x) (sliceColumnArray 0 (VP.length ty) y))
            cx
            cy
  (ColDictionary did ix vx, ColDictionary _ iy vy)
    | vx == vy -> Right (ColDictionary did (ix VP.++ iy) vx)
    | otherwise -> do
        vals <- concatDictValues vx vy
        let !k = fromIntegral (columnLength vx)
        Right (ColDictionary did (ix VP.++ VP.map (+ k) iy) vals)
  (ColDictionaryMaybe did ix vx, ColDictionaryMaybe _ iy vy)
    | vx == vy -> Right (ColDictionaryMaybe did (ix V.++ iy) vx)
    | otherwise -> do
        vals <- concatDictValues vx vy
        let !k = fromIntegral (columnLength vx)
        Right (ColDictionaryMaybe did (ix V.++ V.map (fmap (+ k)) iy) vals)
  (ColRunEndEncoded rx vx, ColRunEndEncoded ry vy) -> do
    re <- concatRunEnds (columnLength a) rx ry
    vs <- concatColumnArray (sliceColumnArray 0 (runCount rx) vx) (sliceColumnArray 0 (runCount ry) vy)
    Right (ColRunEndEncoded re vs)
  (ColListView ox sx cx, ColListView oy sy cy) -> do
    (oy', c) <- concatViewChildren int32Max cx oy cy
    Right (ColListView (ox VP.++ oy') (sx VP.++ sy) c)
  (ColListViewMaybe vx ox sx cx, ColListViewMaybe vy oy sy cy) -> do
    (oy', c) <- concatViewChildren int32Max cx oy cy
    Right (ColListViewMaybe (vx V.++ vy) (ox VP.++ oy') (sx VP.++ sy) c)
  (ColLargeListView ox sx cx, ColLargeListView oy sy cy) -> do
    (oy', c) <- concatViewChildren maxBound cx oy cy
    Right (ColLargeListView (ox VP.++ oy') (sx VP.++ sy) c)
  (ColLargeListViewMaybe vx ox sx cx, ColLargeListViewMaybe vy oy sy cy) -> do
    (oy', c) <- concatViewChildren maxBound cx oy cy
    Right (ColLargeListViewMaybe (vx V.++ vy) (ox VP.++ oy') (sx VP.++ sy) c)
  _
    | isNullableColumn a /= isNullableColumn b -> do
        a' <- toNullableColumn a
        b' <- toNullableColumn b
        if isNullableColumn a' && isNullableColumn b'
          then concatColumnArray a' b'
          else mismatch
    | otherwise -> mismatch
  where
    mismatch =
      Left ("Arrow.Column.concatColumnArray: incompatible columns " ++ columnTag a ++ " and " ++ columnTag b)
    one k = \case
      [c] -> Right (k c)
      _ -> Left "Arrow.Column.concatColumnArray: internal child count mismatch"
    two k = \case
      [c1, c2] -> Right (k c1 c2)
      _ -> Left "Arrow.Column.concatColumnArray: internal child count mismatch"
    concatDictValues vx vy = do
      vals <- concatColumnArray vx vy
      if fromIntegral (columnLength vals) <= int32Max
        then Right vals
        else Left "Arrow.Column.concatColumnArray: dictionary exceeds Int32 indices"
    runCount = \case
      ColInt16 v -> VP.length v
      ColInt32 v -> VP.length v
      ColInt64 v -> VP.length v
      _ -> 0


concatStructChildren :: V.Vector (Text, ColumnArray) -> V.Vector (Text, ColumnArray) -> Either String (V.Vector (Text, ColumnArray))
concatStructChildren xs ys
  | V.map fst xs /= V.map fst ys = Left "Arrow.Column.concatColumnArray: struct field names differ"
  | otherwise = V.zipWithM (\(nm, x) (_, y) -> (,) nm <$> concatColumnArray x y) xs ys


concatFixedChildren :: Int -> Int -> ColumnArray -> Int -> ColumnArray -> Either String ColumnArray
concatFixedChildren w na x nb y =
  concatColumnArray (sliceColumnArray 0 (na * w) x) (sliceColumnArray 0 (nb * w) y)


{- | Concatenate offset-addressed children (list / large list / map):
each side is re-based so its offsets start at zero and its children
hold exactly the referenced range, then the second side's offsets
are shifted by the first side's element count.
-}
concatOffsetChildren
  :: (Integral o, VP.Prim o)
  => Int64
  -> VP.Vector o
  -> [ColumnArray]
  -> VP.Vector o
  -> [ColumnArray]
  -> Either String (VP.Vector o, [ColumnArray])
concatOffsetChildren maxOff ox cx oy cy = do
  (ox', cx') <- rebaseOffsets ox cx
  (oy', cy') <- rebaseOffsets oy cy
  let !na = VP.last ox'
  if toInteger na + toInteger (VP.last oy') > toInteger maxOff
    then Left "Arrow.Column.concatColumnArray: offsets overflow"
    else do
      cs <- sequence (zipWith concatColumnArray cx' cy')
      Right (ox' VP.++ VP.map (+ na) (VP.drop 1 oy'), cs)


-- | Shift offsets to start at zero and cut the children to the referenced range.
rebaseOffsets :: (Integral o, VP.Prim o) => VP.Vector o -> [ColumnArray] -> Either String (VP.Vector o, [ColumnArray])
rebaseOffsets offs cs
  | VP.null offs = Right (VP.singleton 0, map (sliceColumnArray 0 0) cs)
  | otherwise =
      let !o0 = VP.head offs
          !oN = VP.last offs
          !s = fromIntegral o0 :: Int
          !l = fromIntegral (oN - o0) :: Int
          decreasing = any (\i -> VP.unsafeIndex offs i > VP.unsafeIndex offs (i + 1)) [0 .. VP.length offs - 2]
      in if o0 < 0 || l < 0 || not (all (\c -> s + l <= columnLength c) cs) || decreasing
           then Left "Arrow.Column.concatColumnArray: offsets are not a valid non-decreasing range of the child"
           else Right (VP.map (subtract o0) offs, map (sliceColumnArray s l) cs)


-- | List-view concatenation: the child is appended and the second side's offsets shift past the first child.
concatViewChildren :: (Integral o, VP.Prim o) => Int64 -> ColumnArray -> VP.Vector o -> ColumnArray -> Either String (VP.Vector o, ColumnArray)
concatViewChildren maxOff cx oy cy = do
  c <- concatColumnArray cx cy
  if fromIntegral (columnLength c) > maxOff
    then Left "Arrow.Column.concatColumnArray: list-view offsets overflow"
    else
      let !k = fromIntegral (columnLength cx)
      in Right (VP.map (+ k) oy, c)


concatRunEnds :: Int -> ColumnArray -> ColumnArray -> Either String ColumnArray
concatRunEnds shift rx ry = case (rx, ry) of
  (ColInt16 x, ColInt16 y) -> ColInt16 <$> go (fromIntegral (maxBound :: Int16)) x y
  (ColInt32 x, ColInt32 y) -> ColInt32 <$> go int32Max x y
  (ColInt64 x, ColInt64 y) -> ColInt64 <$> go maxBound x y
  _ -> Left ("Arrow.Column.concatColumnArray: run ends must be matching int16/32/64 columns, got " ++ columnTag rx ++ " and " ++ columnTag ry)
  where
    go :: (Integral a, VP.Prim a) => Int64 -> VP.Vector a -> VP.Vector a -> Either String (VP.Vector a)
    go maxEnd x y
      | not (VP.null y) && toInteger (VP.last y) + toInteger shift > toInteger maxEnd =
          Left "Arrow.Column.concatColumnArray: run end overflows its integer width"
      | otherwise = Right (x VP.++ VP.map (+ fromIntegral shift) y)


-- | Balanced concatenation of a non-empty list of columns.
concatColumnArrays :: [ColumnArray] -> Either String ColumnArray
concatColumnArrays = \case
  [] -> Left "Arrow.Column.concatColumnArrays: no columns"
  [c] -> Right c
  cs -> do
    let (l, r) = splitAt (length cs `div` 2) cs
    x <- concatColumnArrays l
    y <- concatColumnArrays r
    concatColumnArray x y


-- ============================================================
-- Nullability helpers
-- ============================================================

-- | Whether the column carries per-row nulls (a @*Maybe@ constructor or 'ColNull').
isNullableColumn :: ColumnArray -> Bool
isNullableColumn = \case
  ColInt8Maybe {} -> True
  ColInt16Maybe {} -> True
  ColInt32Maybe {} -> True
  ColInt64Maybe {} -> True
  ColUInt8Maybe {} -> True
  ColUInt16Maybe {} -> True
  ColUInt32Maybe {} -> True
  ColUInt64Maybe {} -> True
  ColFloat16Maybe {} -> True
  ColFloatMaybe {} -> True
  ColDoubleMaybe {} -> True
  ColBoolMaybe {} -> True
  ColUtf8Maybe {} -> True
  ColBinaryMaybe {} -> True
  ColLargeUtf8Maybe {} -> True
  ColLargeBinaryMaybe {} -> True
  ColFixedSizeBinaryMaybe {} -> True
  ColDate32Maybe {} -> True
  ColDate64Maybe {} -> True
  ColTime32Maybe {} -> True
  ColTime64Maybe {} -> True
  ColTimestampMaybe {} -> True
  ColDurationMaybe {} -> True
  ColDecimal128Maybe {} -> True
  ColDecimal256Maybe {} -> True
  ColIntervalYearMonthMaybe {} -> True
  ColIntervalDayTimeMaybe {} -> True
  ColIntervalMonthDayNanoMaybe {} -> True
  ColStructMaybe {} -> True
  ColListMaybe {} -> True
  ColLargeListMaybe {} -> True
  ColFixedSizeListMaybe {} -> True
  ColMapMaybe {} -> True
  ColDictionaryMaybe {} -> True
  ColListViewMaybe {} -> True
  ColLargeListViewMaybe {} -> True
  ColUtf8ViewMaybe {} -> True
  ColBinaryViewMaybe {} -> True
  ColNull {} -> True
  ColInt8 {} -> False
  ColInt16 {} -> False
  ColInt32 {} -> False
  ColInt64 {} -> False
  ColUInt8 {} -> False
  ColUInt16 {} -> False
  ColUInt32 {} -> False
  ColUInt64 {} -> False
  ColFloat16 {} -> False
  ColFloat {} -> False
  ColDouble {} -> False
  ColBool {} -> False
  ColUtf8 {} -> False
  ColBinary {} -> False
  ColLargeUtf8 {} -> False
  ColLargeBinary {} -> False
  ColFixedSizeBinary {} -> False
  ColDate32 {} -> False
  ColDate64 {} -> False
  ColTime32 {} -> False
  ColTime64 {} -> False
  ColTimestamp {} -> False
  ColDuration {} -> False
  ColDecimal128 {} -> False
  ColDecimal256 {} -> False
  ColIntervalYearMonth {} -> False
  ColIntervalDayTime {} -> False
  ColIntervalMonthDayNano {} -> False
  ColStruct {} -> False
  ColList {} -> False
  ColLargeList {} -> False
  ColFixedSizeList {} -> False
  ColMap {} -> False
  ColDenseUnion {} -> False
  ColSparseUnion {} -> False
  ColDictionary {} -> False
  ColRunEndEncoded {} -> False
  ColListView {} -> False
  ColLargeListView {} -> False
  ColUtf8View {} -> False
  ColBinaryView {} -> False


{- | The nullable variant of a column with every row valid. Nullable
columns are returned unchanged. Unions and run-end-encoded columns
have no validity bitmap of their own, so they are rejected.
-}
toNullableColumn :: ColumnArray -> Either String ColumnArray
toNullableColumn col = case col of
  ColInt8 v -> Right (ColInt8Maybe (justs v))
  ColInt16 v -> Right (ColInt16Maybe (justs v))
  ColInt32 v -> Right (ColInt32Maybe (justs v))
  ColInt64 v -> Right (ColInt64Maybe (justs v))
  ColUInt8 v -> Right (ColUInt8Maybe (justs v))
  ColUInt16 v -> Right (ColUInt16Maybe (justs v))
  ColUInt32 v -> Right (ColUInt32Maybe (justs v))
  ColUInt64 v -> Right (ColUInt64Maybe (justs v))
  ColFloat16 v -> Right (ColFloat16Maybe (justs v))
  ColFloat v -> Right (ColFloatMaybe (justs v))
  ColDouble v -> Right (ColDoubleMaybe (justs v))
  ColBool v -> Right (ColBoolMaybe (V.map Just v))
  ColUtf8 v -> Right (ColUtf8Maybe (V.map Just v))
  ColBinary v -> Right (ColBinaryMaybe (V.map Just v))
  ColLargeUtf8 v -> Right (ColLargeUtf8Maybe (V.map Just v))
  ColLargeBinary v -> Right (ColLargeBinaryMaybe (V.map Just v))
  ColFixedSizeBinary w v -> Right (ColFixedSizeBinaryMaybe w (V.map Just v))
  ColDate32 v -> Right (ColDate32Maybe (justs v))
  ColDate64 v -> Right (ColDate64Maybe (justs v))
  ColTime32 v -> Right (ColTime32Maybe (justs v))
  ColTime64 v -> Right (ColTime64Maybe (justs v))
  ColTimestamp v -> Right (ColTimestampMaybe (justs v))
  ColDuration v -> Right (ColDurationMaybe (justs v))
  ColDecimal128 p s v -> Right (ColDecimal128Maybe p s (V.map Just v))
  ColDecimal256 p s v -> Right (ColDecimal256Maybe p s (V.map Just v))
  ColIntervalYearMonth v -> Right (ColIntervalYearMonthMaybe (justs v))
  ColIntervalDayTime d m -> Right (ColIntervalDayTimeMaybe (V.zipWith (\x y -> Just (x, y)) (V.convert d) (V.convert m)))
  ColIntervalMonthDayNano m d ns ->
    Right (ColIntervalMonthDayNanoMaybe (V.zipWith3 (\x y z -> Just (x, y, z)) (V.convert m) (V.convert d) (V.convert ns)))
  ColUtf8View v -> Right (ColUtf8ViewMaybe (V.map Just v))
  ColBinaryView v -> Right (ColBinaryViewMaybe (V.map Just v))
  ColStruct cs -> Right (ColStructMaybe allValid cs)
  ColList o c -> Right (ColListMaybe allValid o c)
  ColLargeList o c -> Right (ColLargeListMaybe allValid o c)
  ColFixedSizeList w c -> Right (ColFixedSizeListMaybe w allValid c)
  ColMap o k v -> Right (ColMapMaybe allValid o k v)
  ColDictionary did ix v -> Right (ColDictionaryMaybe did (justs ix) v)
  ColListView o s c -> Right (ColListViewMaybe allValid o s c)
  ColLargeListView o s c -> Right (ColLargeListViewMaybe allValid o s c)
  ColDenseUnion {} -> noValidity
  ColSparseUnion {} -> noValidity
  ColRunEndEncoded {} -> noValidity
  _ -> Right col
  where
    justs :: VP.Prim a => VP.Vector a -> V.Vector (Maybe a)
    justs = V.map Just . V.convert
    allValid = V.replicate (columnLength col) True
    noValidity = Left ("Arrow.Column: " ++ columnTag col ++ " has no validity bitmap and cannot be made nullable")


{- | Null out the rows whose flag is 'False' in a nullable column (rows
already null stay null).
-}
maskValidity :: V.Vector Bool -> ColumnArray -> Either String ColumnArray
maskValidity valid col = case col of
  ColInt8Maybe v -> Right (ColInt8Maybe (m v))
  ColInt16Maybe v -> Right (ColInt16Maybe (m v))
  ColInt32Maybe v -> Right (ColInt32Maybe (m v))
  ColInt64Maybe v -> Right (ColInt64Maybe (m v))
  ColUInt8Maybe v -> Right (ColUInt8Maybe (m v))
  ColUInt16Maybe v -> Right (ColUInt16Maybe (m v))
  ColUInt32Maybe v -> Right (ColUInt32Maybe (m v))
  ColUInt64Maybe v -> Right (ColUInt64Maybe (m v))
  ColFloat16Maybe v -> Right (ColFloat16Maybe (m v))
  ColFloatMaybe v -> Right (ColFloatMaybe (m v))
  ColDoubleMaybe v -> Right (ColDoubleMaybe (m v))
  ColBoolMaybe v -> Right (ColBoolMaybe (m v))
  ColUtf8Maybe v -> Right (ColUtf8Maybe (m v))
  ColBinaryMaybe v -> Right (ColBinaryMaybe (m v))
  ColLargeUtf8Maybe v -> Right (ColLargeUtf8Maybe (m v))
  ColLargeBinaryMaybe v -> Right (ColLargeBinaryMaybe (m v))
  ColFixedSizeBinaryMaybe w v -> Right (ColFixedSizeBinaryMaybe w (m v))
  ColDate32Maybe v -> Right (ColDate32Maybe (m v))
  ColDate64Maybe v -> Right (ColDate64Maybe (m v))
  ColTime32Maybe v -> Right (ColTime32Maybe (m v))
  ColTime64Maybe v -> Right (ColTime64Maybe (m v))
  ColTimestampMaybe v -> Right (ColTimestampMaybe (m v))
  ColDurationMaybe v -> Right (ColDurationMaybe (m v))
  ColDecimal128Maybe p s v -> Right (ColDecimal128Maybe p s (m v))
  ColDecimal256Maybe p s v -> Right (ColDecimal256Maybe p s (m v))
  ColIntervalYearMonthMaybe v -> Right (ColIntervalYearMonthMaybe (m v))
  ColIntervalDayTimeMaybe v -> Right (ColIntervalDayTimeMaybe (m v))
  ColIntervalMonthDayNanoMaybe v -> Right (ColIntervalMonthDayNanoMaybe (m v))
  ColUtf8ViewMaybe v -> Right (ColUtf8ViewMaybe (m v))
  ColBinaryViewMaybe v -> Right (ColBinaryViewMaybe (m v))
  ColDictionaryMaybe did v vals -> Right (ColDictionaryMaybe did (m v) vals)
  ColStructMaybe v cs -> Right (ColStructMaybe (b v) cs)
  ColListMaybe v o c -> Right (ColListMaybe (b v) o c)
  ColLargeListMaybe v o c -> Right (ColLargeListMaybe (b v) o c)
  ColFixedSizeListMaybe w v c -> Right (ColFixedSizeListMaybe w (b v) c)
  ColMapMaybe v o k vs -> Right (ColMapMaybe (b v) o k vs)
  ColListViewMaybe v o s c -> Right (ColListViewMaybe (b v) o s c)
  ColLargeListViewMaybe v o s c -> Right (ColLargeListViewMaybe (b v) o s c)
  ColNull _ -> Right col
  _ -> Left ("Arrow.Column: cannot mask nulls into non-nullable column " ++ columnTag col)
  where
    m :: V.Vector (Maybe a) -> V.Vector (Maybe a)
    m = V.zipWith (\ok x -> if ok then x else Nothing) valid
    b = V.zipWith (&&) valid


-- ============================================================
-- Gather / dictionary expansion
-- ============================================================

{- | Gather rows by index (repeats allowed). Any index outside the
column is a 'Left'. Flat columns and the index-carrying parts of
structs, unions, dictionaries and list views are permuted directly;
other nested columns are rebuilt from runs of consecutive indices
with 'concatColumnArray'.
-}
takeColumnArray :: VP.Vector Int -> ColumnArray -> Either String ColumnArray
takeColumnArray ix col
  | VP.any (\i -> i < 0 || i >= n) ix =
      Left ("Arrow.Column.takeColumnArray: index out of range for a column of " ++ show n ++ " rows")
  | otherwise = case col of
      ColInt8 v -> Right (ColInt8 (pb v))
      ColInt16 v -> Right (ColInt16 (pb v))
      ColInt32 v -> Right (ColInt32 (pb v))
      ColInt64 v -> Right (ColInt64 (pb v))
      ColUInt8 v -> Right (ColUInt8 (pb v))
      ColUInt16 v -> Right (ColUInt16 (pb v))
      ColUInt32 v -> Right (ColUInt32 (pb v))
      ColUInt64 v -> Right (ColUInt64 (pb v))
      ColFloat16 v -> Right (ColFloat16 (pb v))
      ColFloat v -> Right (ColFloat (pb v))
      ColDouble v -> Right (ColDouble (pb v))
      ColBool v -> Right (ColBool (bb v))
      ColUtf8 v -> Right (ColUtf8 (bb v))
      ColBinary v -> Right (ColBinary (bb v))
      ColLargeUtf8 v -> Right (ColLargeUtf8 (bb v))
      ColLargeBinary v -> Right (ColLargeBinary (bb v))
      ColFixedSizeBinary w v -> Right (ColFixedSizeBinary w (bb v))
      ColDate32 v -> Right (ColDate32 (pb v))
      ColDate64 v -> Right (ColDate64 (pb v))
      ColTime32 v -> Right (ColTime32 (pb v))
      ColTime64 v -> Right (ColTime64 (pb v))
      ColTimestamp v -> Right (ColTimestamp (pb v))
      ColDuration v -> Right (ColDuration (pb v))
      ColDecimal128 p s v -> Right (ColDecimal128 p s (bb v))
      ColDecimal256 p s v -> Right (ColDecimal256 p s (bb v))
      ColIntervalYearMonth v -> Right (ColIntervalYearMonth (pb v))
      ColIntervalDayTime d m -> Right (ColIntervalDayTime (pb d) (pb m))
      ColIntervalMonthDayNano m d ns -> Right (ColIntervalMonthDayNano (pb m) (pb d) (pb ns))
      ColInt8Maybe v -> Right (ColInt8Maybe (bb v))
      ColInt16Maybe v -> Right (ColInt16Maybe (bb v))
      ColInt32Maybe v -> Right (ColInt32Maybe (bb v))
      ColInt64Maybe v -> Right (ColInt64Maybe (bb v))
      ColUInt8Maybe v -> Right (ColUInt8Maybe (bb v))
      ColUInt16Maybe v -> Right (ColUInt16Maybe (bb v))
      ColUInt32Maybe v -> Right (ColUInt32Maybe (bb v))
      ColUInt64Maybe v -> Right (ColUInt64Maybe (bb v))
      ColFloat16Maybe v -> Right (ColFloat16Maybe (bb v))
      ColFloatMaybe v -> Right (ColFloatMaybe (bb v))
      ColDoubleMaybe v -> Right (ColDoubleMaybe (bb v))
      ColBoolMaybe v -> Right (ColBoolMaybe (bb v))
      ColUtf8Maybe v -> Right (ColUtf8Maybe (bb v))
      ColBinaryMaybe v -> Right (ColBinaryMaybe (bb v))
      ColLargeUtf8Maybe v -> Right (ColLargeUtf8Maybe (bb v))
      ColLargeBinaryMaybe v -> Right (ColLargeBinaryMaybe (bb v))
      ColFixedSizeBinaryMaybe w v -> Right (ColFixedSizeBinaryMaybe w (bb v))
      ColDate32Maybe v -> Right (ColDate32Maybe (bb v))
      ColDate64Maybe v -> Right (ColDate64Maybe (bb v))
      ColTime32Maybe v -> Right (ColTime32Maybe (bb v))
      ColTime64Maybe v -> Right (ColTime64Maybe (bb v))
      ColTimestampMaybe v -> Right (ColTimestampMaybe (bb v))
      ColDurationMaybe v -> Right (ColDurationMaybe (bb v))
      ColDecimal128Maybe p s v -> Right (ColDecimal128Maybe p s (bb v))
      ColDecimal256Maybe p s v -> Right (ColDecimal256Maybe p s (bb v))
      ColIntervalYearMonthMaybe v -> Right (ColIntervalYearMonthMaybe (bb v))
      ColIntervalDayTimeMaybe v -> Right (ColIntervalDayTimeMaybe (bb v))
      ColIntervalMonthDayNanoMaybe v -> Right (ColIntervalMonthDayNanoMaybe (bb v))
      ColUtf8View v -> Right (ColUtf8View (bb v))
      ColUtf8ViewMaybe v -> Right (ColUtf8ViewMaybe (bb v))
      ColBinaryView v -> Right (ColBinaryView (bb v))
      ColBinaryViewMaybe v -> Right (ColBinaryViewMaybe (bb v))
      ColNull _ -> Right (ColNull (VP.length ix))
      ColDictionary did v vals -> Right (ColDictionary did (pb v) vals)
      ColDictionaryMaybe did v vals -> Right (ColDictionaryMaybe did (bb v) vals)
      ColStruct cs
        | V.null cs -> Right col
        | otherwise -> ColStruct <$> V.mapM (traverse (takeColumnArray ix)) cs
      ColStructMaybe v cs -> ColStructMaybe (bb v) <$> V.mapM (traverse (takeColumnArray ix)) cs
      ColDenseUnion ts offs cs -> Right (ColDenseUnion (pb ts) (pb offs) cs)
      ColSparseUnion ts cs -> ColSparseUnion (pb ts) <$> V.mapM (takeColumnArray ix) cs
      ColListView o s c -> Right (ColListView (pb o) (pb s) c)
      ColListViewMaybe v o s c -> Right (ColListViewMaybe (bb v) (pb o) (pb s) c)
      ColLargeListView o s c -> Right (ColLargeListView (pb o) (pb s) c)
      ColLargeListViewMaybe v o s c -> Right (ColLargeListViewMaybe (bb v) (pb o) (pb s) c)
      ColList {} -> viaRuns
      ColListMaybe {} -> viaRuns
      ColLargeList {} -> viaRuns
      ColLargeListMaybe {} -> viaRuns
      ColFixedSizeList {} -> viaRuns
      ColFixedSizeListMaybe {} -> viaRuns
      ColMap {} -> viaRuns
      ColMapMaybe {} -> viaRuns
      ColRunEndEncoded {} -> viaRuns
  where
    !n = columnLength col
    pb :: VP.Prim a => VP.Vector a -> VP.Vector a
    pb v = VP.backpermute v ix
    bb :: V.Vector a -> V.Vector a
    bb v = V.backpermute v (V.convert ix)
    viaRuns
      | VP.null ix = Right (sliceColumnArray 0 0 col)
      | otherwise = concatColumnArrays (map (\(s, l) -> sliceColumnArray s l col) (indexRuns ix))


-- | Split an index vector into maximal runs of consecutive indices, as @(start, length)@.
indexRuns :: VP.Vector Int -> [(Int, Int)]
indexRuns = VP.foldr step []
  where
    step i ((s, l) : rest) | i + 1 == s = (i, l + 1) : rest
    step i acc = (i, 1) : acc


{- | Replace a dictionary-encoded column by the values its indices
select ('ColDictionary' becomes a column of the value type,
'ColDictionaryMaybe' its nullable variant with the null-index rows
null). Any other column is returned unchanged. The dictionary must
already be resolved (see 'resolveDictionaryColumn'). Null rows over
an empty dictionary cannot be expanded and are rejected.
-}
expandDictionary :: ColumnArray -> Either String ColumnArray
expandDictionary = \case
  ColDictionary _ ix vals -> takeColumnArray (VP.map fromIntegral ix) vals
  ColDictionaryMaybe _ ix vals -> do
    let !rows = V.length ix
        !valid = V.map isJust ix
        !fill = maybe 0 fromIntegral (V.foldr (\x acc -> maybe acc Just x) Nothing ix)
    dense <-
      if columnLength vals == 0
        then
          if rows == 0
            then Right vals
            else Left "Arrow.Column.expandDictionary: null rows over an empty dictionary"
        else takeColumnArray (V.convert (V.map (maybe fill fromIntegral) ix)) vals
    nullable <- toNullableColumn dense
    maskValidity valid nullable
  col -> Right col


-- ============================================================
-- Map invariants
-- ============================================================

{- | Check that a 'ColMap' / 'ColMapMaybe' column satisfies the
@keysSorted@ promise of its 'AMap' field: within every non-null
entry the keys are non-decreasing.

Offsets are bounds-checked (non-decreasing, inside the key column)
and map keys must not be null. Keys are compared by value:
integers, dates, times, timestamps and durations numerically;
floating point (including half floats) numerically with @-0 == 0@
and every NaN equal to every other NaN and greater than any number;
booleans with @False < True@; strings by code point; binary
(including fixed-size and views) as unsigned bytes; decimals as
signed two's-complement integers; dictionary-encoded keys by their
resolved values. Interval, nested, union, run-end-encoded and null
key columns have no defined order and are rejected with 'Left', as
is any column that is not a map.
-}
validateMapKeysSorted :: ColumnArray -> Either String ()
validateMapKeysSorted = \case
  ColMap offs keys _ -> checkMapKeys (const True) offs keys
  ColMapMaybe valid offs keys _ -> checkMapKeys (\i -> fromMaybe False (valid V.!? i)) offs keys
  c -> Left ("Arrow.Column.validateMapKeysSorted: expected a map column, got " ++ columnTag c)


checkMapKeys :: (Int -> Bool) -> VP.Vector Int32 -> ColumnArray -> Either String ()
checkMapKeys entryValid offs keys = do
  (cmp, isNull) <- keyOrder keys
  let !nk = columnLength keys
      !nEntries = max 0 (VP.length offs - 1)
      entry !i
        | i >= nEntries = Right ()
        | otherwise = do
            let !s = fromIntegral (VP.unsafeIndex offs i) :: Int
                !e = fromIntegral (VP.unsafeIndex offs (i + 1)) :: Int
            if s < 0 || e < s || e > nk
              then Left ("Arrow.Column.validateMapKeysSorted: entry " ++ show i ++ " has offsets outside the key column")
              else
                if entryValid i
                  then keysIn i s e s >> entry (i + 1)
                  else entry (i + 1)
      keysIn i s e !j
        | j >= e = Right ()
        | isNull j = Left ("Arrow.Column.validateMapKeysSorted: entry " ++ show i ++ " has a null key")
        | j > s && cmp (j - 1) j == GT =
            Left
              ( "Arrow.Column.validateMapKeysSorted: entry "
                  ++ show i
                  ++ " key "
                  ++ show (j - s)
                  ++ " is smaller than the key before it"
              )
        | otherwise = keysIn i s e (j + 1)
  entry 0


{- | Row comparator and null test for an orderable map-key column
(see 'validateMapKeysSorted' for the order).
-}
keyOrder :: ColumnArray -> Either String (Int -> Int -> Ordering, Int -> Bool)
keyOrder col = case col of
  ColInt8 v -> prim v
  ColInt16 v -> prim v
  ColInt32 v -> prim v
  ColInt64 v -> prim v
  ColUInt8 v -> prim v
  ColUInt16 v -> prim v
  ColUInt32 v -> prim v
  ColUInt64 v -> prim v
  ColDate32 v -> prim v
  ColDate64 v -> prim v
  ColTime32 v -> prim v
  ColTime64 v -> prim v
  ColTimestamp v -> prim v
  ColDuration v -> prim v
  ColFloat16 v -> nonNull (\i j -> compareFloating (halfToDouble (VP.unsafeIndex v i)) (halfToDouble (VP.unsafeIndex v j)))
  ColFloat v -> nonNull (\i j -> compareFloating (VP.unsafeIndex v i) (VP.unsafeIndex v j))
  ColDouble v -> nonNull (\i j -> compareFloating (VP.unsafeIndex v i) (VP.unsafeIndex v j))
  ColBool v -> boxed v
  ColUtf8 v -> boxed v
  ColLargeUtf8 v -> boxed v
  ColUtf8View v -> boxed v
  ColBinary v -> boxed v
  ColLargeBinary v -> boxed v
  ColBinaryView v -> boxed v
  ColFixedSizeBinary _ v -> boxed v
  ColDecimal128 _ _ v -> nonNull (\i j -> compareTwosComplementLE (V.unsafeIndex v i) (V.unsafeIndex v j))
  ColDecimal256 _ _ v -> nonNull (\i j -> compareTwosComplementLE (V.unsafeIndex v i) (V.unsafeIndex v j))
  ColInt8Maybe v -> maybes compare v
  ColInt16Maybe v -> maybes compare v
  ColInt32Maybe v -> maybes compare v
  ColInt64Maybe v -> maybes compare v
  ColUInt8Maybe v -> maybes compare v
  ColUInt16Maybe v -> maybes compare v
  ColUInt32Maybe v -> maybes compare v
  ColUInt64Maybe v -> maybes compare v
  ColDate32Maybe v -> maybes compare v
  ColDate64Maybe v -> maybes compare v
  ColTime32Maybe v -> maybes compare v
  ColTime64Maybe v -> maybes compare v
  ColTimestampMaybe v -> maybes compare v
  ColDurationMaybe v -> maybes compare v
  ColFloat16Maybe v -> maybes (\x y -> compareFloating (halfToDouble x) (halfToDouble y)) v
  ColFloatMaybe v -> maybes compareFloating v
  ColDoubleMaybe v -> maybes compareFloating v
  ColBoolMaybe v -> maybes compare v
  ColUtf8Maybe v -> maybes compare v
  ColLargeUtf8Maybe v -> maybes compare v
  ColUtf8ViewMaybe v -> maybes compare v
  ColBinaryMaybe v -> maybes compare v
  ColLargeBinaryMaybe v -> maybes compare v
  ColBinaryViewMaybe v -> maybes compare v
  ColFixedSizeBinaryMaybe _ v -> maybes compare v
  ColDecimal128Maybe _ _ v -> maybes compareTwosComplementLE v
  ColDecimal256Maybe _ _ v -> maybes compareTwosComplementLE v
  ColDictionary _ ix vals -> do
    (cmp, isNull) <- keyOrder vals
    let !nv = columnLength vals
    if VP.all (\i -> i >= 0 && fromIntegral i < nv) ix
      then
        Right
          ( \i j -> cmp (fromIntegral (VP.unsafeIndex ix i)) (fromIntegral (VP.unsafeIndex ix j))
          , isNull . fromIntegral . VP.unsafeIndex ix
          )
      else Left unresolvedDict
  ColDictionaryMaybe _ ix vals -> do
    (cmp, isNull) <- keyOrder vals
    let !nv = columnLength vals
        at i = maybe 0 fromIntegral (V.unsafeIndex ix i)
    if V.all (maybe True (\i -> i >= 0 && fromIntegral i < nv)) ix
      then Right (\i j -> cmp (at i) (at j), \i -> maybe True (isNull . fromIntegral) (V.unsafeIndex ix i))
      else Left unresolvedDict
  _ -> Left ("Arrow.Column.validateMapKeysSorted: map keys of type " ++ columnTag col ++ " have no defined order")
  where
    unresolvedDict = "Arrow.Column.validateMapKeysSorted: dictionary key index outside its dictionary (is the dictionary resolved?)"
    prim :: (VP.Prim a, Ord a) => VP.Vector a -> Either String (Int -> Int -> Ordering, Int -> Bool)
    prim v = nonNull (\i j -> compare (VP.unsafeIndex v i) (VP.unsafeIndex v j))
    boxed :: Ord a => V.Vector a -> Either String (Int -> Int -> Ordering, Int -> Bool)
    boxed v = nonNull (\i j -> compare (V.unsafeIndex v i) (V.unsafeIndex v j))
    nonNull cmp = Right (cmp, const False)
    maybes :: (a -> a -> Ordering) -> V.Vector (Maybe a) -> Either String (Int -> Int -> Ordering, Int -> Bool)
    maybes cmp v =
      Right
        ( \i j -> case (V.unsafeIndex v i, V.unsafeIndex v j) of
            (Just x, Just y) -> cmp x y
            (Nothing, Nothing) -> EQ
            (Nothing, Just _) -> LT
            (Just _, Nothing) -> GT
        , isNothing . V.unsafeIndex v
        )


{- | Total order on floating point used for map keys: numeric order,
@-0 == 0@, all NaNs equal to each other and greater than every number.
-}
compareFloating :: RealFloat a => a -> a -> Ordering
compareFloating x y = case (isNaN x, isNaN y) of
  (True, True) -> EQ
  (True, False) -> GT
  (False, True) -> LT
  (False, False) -> compare x y


-- | IEEE 754 binary16 to 'Double'.
halfToDouble :: Word16 -> Double
halfToDouble w =
  let !sign = if w .&. 0x8000 /= 0 then -1 else 1
      !ex = fromIntegral ((w `shiftR` 10) .&. 0x1f) :: Int
      !mant = fromIntegral (w .&. 0x3ff) :: Double
  in case ex of
       0 -> sign * mant * 2 ** (-24)
       31 -> if mant == 0 then sign * (1 / 0) else 0 / 0
       _ -> sign * (1 + mant / 1024) * 2 ^^ (ex - 15)


{- | Compare two equal-width little-endian two's-complement integers
(decimal payloads). Rows of different widths compare by width.
-}
compareTwosComplementLE :: ByteString -> ByteString -> Ordering
compareTwosComplementLE x y
  | BS.length x /= BS.length y = compare (BS.length x) (BS.length y)
  | BS.null x = EQ
  | otherwise =
      let !nx = BS.last x >= 0x80
          !ny = BS.last y >= 0x80
      in case (nx, ny) of
           (True, False) -> LT
           (False, True) -> GT
           _ -> compare (BS.reverse x) (BS.reverse y)
