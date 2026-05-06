{-# LANGUAGE BangPatterns #-}
-- | Materialize Apache Arrow IPC record batch bodies into Haskell-friendly columns.
--
-- Supports flat and nested schemas. Nullable columns use a validity bitmap
-- (LSB of each byte first) plus values; decoded as @V.Vector (Maybe a)@.
module Arrow.Column
  ( ColumnArray (..)
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

import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import Foreign.Ptr (Ptr, castPtr)
import qualified Data.Primitive.ByteArray as PBA
import qualified Data.Vector.Primitive.Mutable as MVP
import System.IO.Unsafe (unsafePerformIO)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Control.Monad (forM)
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)

import Arrow.IPC (validateRecordBatchBuffers)
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

-- | Materialized values for one column.
-- Nullable columns use @Col*Maybe@ with per-row 'Maybe'.
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
  | -- | Arrow @INTERVAL(DAY_TIME)@: (days :: i32, ms :: i32) per row,
    -- stored as an 8-byte pair in element order.
    ColIntervalDayTime !(VP.Vector Int32) !(VP.Vector Int32)
  | -- | Arrow @INTERVAL(MONTH_DAY_NANO)@: (months :: i32, days :: i32,
    -- nanos :: i64) per row, stored as a 16-byte triple.
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
  | ColStruct !(V.Vector (Text, ColumnArray))
  | ColStructMaybe !(V.Vector Bool) !(V.Vector (Text, ColumnArray))
  | ColList !(VP.Vector Int32) !ColumnArray
  | ColListMaybe !(V.Vector Bool) !(VP.Vector Int32) !ColumnArray
  | -- | Arrow \"LargeList\": semantics identical to 'ColList' but with
    -- 64-bit offsets. Used when the child array has more than
    -- 2^31 elements.
    ColLargeList !(VP.Vector Int64) !ColumnArray
  | ColLargeListMaybe !(V.Vector Bool) !(VP.Vector Int64) !ColumnArray
  | ColFixedSizeList !Int !ColumnArray
  | ColFixedSizeListMaybe !Int !(V.Vector Bool) !ColumnArray
  | ColMap !(VP.Vector Int32) !ColumnArray !ColumnArray
  | ColMapMaybe !(V.Vector Bool) !(VP.Vector Int32) !ColumnArray !ColumnArray
  | ColDenseUnion !(VP.Vector Int8) !(VP.Vector Int32) !(V.Vector ColumnArray)
  | ColSparseUnion !(VP.Vector Int8) !(V.Vector ColumnArray)
  | ColDictionary !Int64 !(VP.Vector Int32) !ColumnArray
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
    ColListView !(VP.Vector Int32) !(VP.Vector Int32) !ColumnArray
  | ColListViewMaybe !(V.Vector Bool) !(VP.Vector Int32) !(VP.Vector Int32) !ColumnArray
  | -- | LargeListView: 64-bit offsets and sizes.
    ColLargeListView !(VP.Vector Int64) !(VP.Vector Int64) !ColumnArray
  | ColLargeListViewMaybe !(V.Vector Bool) !(VP.Vector Int64) !(VP.Vector Int64) !ColumnArray
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
    else Right (ColUInt8 (VP.generate len $ \i -> BSU.unsafeIndex valsBs i), nodeIdx + 1, bufIdx + 1)

readUInt16Column :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readUInt16Column endian len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  if BS.length valsBs < len * 2
    then Left "Arrow.Column: uint16 buffer too small"
    else Right (ColUInt16 (VP.generate len $ \i -> readWord16 endian valsBs (i * 2)), nodeIdx + 1, bufIdx + 1)

readUInt32Column :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readUInt32Column endian len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  if BS.length valsBs < len * 4
    then Left "Arrow.Column: uint32 buffer too small"
    else case endian of
      Little -> Right (ColUInt32 (memcpyPrimVecLE 4 len valsBs), nodeIdx + 1, bufIdx + 1)
      _      -> Right (ColUInt32 (VP.generate len $ \i -> readWord32 endian valsBs (i * 4)), nodeIdx + 1, bufIdx + 1)

readUInt64Column :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readUInt64Column endian len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  if BS.length valsBs < len * 8
    then Left "Arrow.Column: uint64 buffer too small"
    else case endian of
      Little -> Right (ColUInt64 (memcpyPrimVecLE 8 len valsBs), nodeIdx + 1, bufIdx + 1)
      _      -> Right (ColUInt64 (VP.generate len $ \i -> readWord64 endian valsBs (i * 8)), nodeIdx + 1, bufIdx + 1)

readFloat16Column :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readFloat16Column endian len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  if BS.length valsBs < len * 2
    then Left "Arrow.Column: float16 buffer too small"
    else Right (ColFloat16 (VP.generate len $ \i -> readWord16 endian valsBs (i * 2)), nodeIdx + 1, bufIdx + 1)

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
      Little -> Right (ColFloat (memcpyPrimVecLE 4 len valsBs), nodeIdx + 1, bufIdx + 1)
      _      -> do
        vec <- V.generateM len $ \i -> readF32 endian valsBs (i * 4)
        Right (ColFloat (V.convert vec), nodeIdx + 1, bufIdx + 1)

readDoubleColumn :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readDoubleColumn endian len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  if BS.length valsBs < len * 8
    then Left "Arrow.Column: double buffer too small"
    else case endian of
      Little -> Right (ColDouble (memcpyPrimVecLE 8 len valsBs), nodeIdx + 1, bufIdx + 1)
      _      -> do
        vec <- V.generateM len $ \i -> readF64 endian valsBs (i * 8)
        Right (ColDouble (V.convert vec), nodeIdx + 1, bufIdx + 1)

readUtf8Column :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readUtf8Column endian len rb body !bufIdx !nodeIdx = do
  offBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  datBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  if BS.length offBs < (len + 1) * 4
    then Left "Arrow.Column: UTF-8 offsets buffer too small"
    else do
      strs <-
        V.generateM len $ \i -> do
          s0 <- readI32 endian offBs (i * 4)
          s1 <- readI32 endian offBs ((i + 1) * 4)
          let !start = fromIntegral s0
              !end = fromIntegral s1
          if start < 0 || end < start || end > BS.length datBs
            then Left "Arrow.Column: invalid UTF-8 slice"
            else case TE.decodeUtf8' (BS.take (end - start) (BS.drop start datBs)) of
              Right t -> Right t
              Left _ -> Left "Arrow.Column: invalid UTF-8 bytes"
      Right (ColUtf8 strs, nodeIdx + 1, bufIdx + 2)

readBinaryColumn :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readBinaryColumn endian len rb body !bufIdx !nodeIdx = do
  offBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  datBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  if BS.length offBs < (len + 1) * 4
    then Left "Arrow.Column: binary offsets buffer too small"
    else do
      bins <-
        V.generateM len $ \i -> do
          s0 <- readI32 endian offBs (i * 4)
          s1 <- readI32 endian offBs ((i + 1) * 4)
          let !start = fromIntegral s0
              !end = fromIntegral s1
          if start < 0 || end < start || end > BS.length datBs
            then Left "Arrow.Column: invalid binary slice"
            else Right $! BS.take (end - start) (BS.drop start datBs)
      Right (ColBinary bins, nodeIdx + 1, bufIdx + 2)

readLargeUtf8Column :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readLargeUtf8Column endian len rb body !bufIdx !nodeIdx = do
  offBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  datBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  if BS.length offBs < (len + 1) * 8
    then Left "Arrow.Column: large UTF-8 offsets buffer too small"
    else do
      strs <- V.generateM len $ \i -> do
        let !s0 = fromIntegral (readWord64 endian offBs (i * 8)) :: Int
            !s1 = fromIntegral (readWord64 endian offBs ((i + 1) * 8)) :: Int
        if s0 < 0 || s1 < s0 || s1 > BS.length datBs
          then Left "Arrow.Column: invalid large UTF-8 slice"
          else case TE.decodeUtf8' (BS.take (s1 - s0) (BS.drop s0 datBs)) of
            Right t -> Right t
            Left _ -> Left "Arrow.Column: invalid large UTF-8 bytes"
      Right (ColLargeUtf8 strs, nodeIdx + 1, bufIdx + 2)

readLargeBinaryColumn :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readLargeBinaryColumn endian len rb body !bufIdx !nodeIdx = do
  offBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  datBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  if BS.length offBs < (len + 1) * 8
    then Left "Arrow.Column: large binary offsets buffer too small"
    else do
      bins <- V.generateM len $ \i -> do
        let !s0 = fromIntegral (readWord64 endian offBs (i * 8)) :: Int
            !s1 = fromIntegral (readWord64 endian offBs ((i + 1) * 8)) :: Int
        if s0 < 0 || s1 < s0 || s1 > BS.length datBs
          then Left "Arrow.Column: invalid large binary slice"
          else Right $! BS.take (s1 - s0) (BS.drop s0 datBs)
      Right (ColLargeBinary bins, nodeIdx + 1, bufIdx + 2)

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
      Little -> Right (ColDate32 (memcpyPrimVecLE 4 len valsBs), nodeIdx + 1, bufIdx + 1)
      _      -> Right (ColDate32 (VP.generate len $ \i ->
        fromIntegral (readWord32 endian valsBs (i * 4)) :: Int32), nodeIdx + 1, bufIdx + 1)

readDate64Column :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readDate64Column endian len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  if BS.length valsBs < len * 8
    then Left "Arrow.Column: date64 buffer too small"
    else case endian of
      Little -> Right (ColDate64 (memcpyPrimVecLE 8 len valsBs), nodeIdx + 1, bufIdx + 1)
      _      -> Right (ColDate64 (VP.generate len $ \i ->
        fromIntegral (readWord64 endian valsBs (i * 8)) :: Int64), nodeIdx + 1, bufIdx + 1)

readTime32Column :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readTime32Column endian len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  if BS.length valsBs < len * 4
    then Left "Arrow.Column: time32 buffer too small"
    else case endian of
      Little -> Right (ColTime32 (memcpyPrimVecLE 4 len valsBs), nodeIdx + 1, bufIdx + 1)
      _      -> Right (ColTime32 (VP.generate len $ \i ->
        fromIntegral (readWord32 endian valsBs (i * 4)) :: Int32), nodeIdx + 1, bufIdx + 1)

readTime64Column :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readTime64Column endian len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  if BS.length valsBs < len * 8
    then Left "Arrow.Column: time64 buffer too small"
    else case endian of
      Little -> Right (ColTime64 (memcpyPrimVecLE 8 len valsBs), nodeIdx + 1, bufIdx + 1)
      _      -> Right (ColTime64 (VP.generate len $ \i ->
        fromIntegral (readWord64 endian valsBs (i * 8)) :: Int64), nodeIdx + 1, bufIdx + 1)

readTimestampColumn :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readTimestampColumn endian len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  if BS.length valsBs < len * 8
    then Left "Arrow.Column: timestamp buffer too small"
    else case endian of
      Little -> Right (ColTimestamp (memcpyPrimVecLE 8 len valsBs), nodeIdx + 1, bufIdx + 1)
      _      -> Right (ColTimestamp (VP.generate len $ \i ->
        fromIntegral (readWord64 endian valsBs (i * 8)) :: Int64), nodeIdx + 1, bufIdx + 1)

readDurationColumn :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readDurationColumn endian len rb body !bufIdx !nodeIdx = do
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  if BS.length valsBs < len * 8
    then Left "Arrow.Column: duration buffer too small"
    else case endian of
      Little -> Right (ColDuration (memcpyPrimVecLE 8 len valsBs), nodeIdx + 1, bufIdx + 1)
      _      -> Right (ColDuration (VP.generate len $ \i ->
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

readInt8ColumnMaybe :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readInt8ColumnMaybe _endian len rb body !bufIdx !nodeIdx = do
  validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  validFlags <- unpackBools len validBs
  if BS.length valsBs < len
    then Left "Arrow.Column: int8 values buffer too small"
    else do
      xs <-
        forM [0 .. len - 1] $ \i ->
          if not (V.unsafeIndex validFlags i)
            then pure Nothing
            else pure $ Just (fromIntegral (BSU.unsafeIndex valsBs i) :: Int8)
      Right (ColInt8Maybe (V.fromList xs), nodeIdx + 1, bufIdx + 2)

readInt16ColumnMaybe :: Endianness -> Bool -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readInt16ColumnMaybe endian signed len rb body !bufIdx !nodeIdx = do
  validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  validFlags <- unpackBools len validBs
  if BS.length valsBs < len * 2
    then Left "Arrow.Column: int16 values buffer too small"
    else do
      xs <-
        forM [0 .. len - 1] $ \i ->
          if not (V.unsafeIndex validFlags i)
            then pure Nothing
            else do
              let v = readWord16 endian valsBs (i * 2)
                  w = if signed then fromIntegral (fromIntegral v :: Int16) else fromIntegral v
              pure $ Just w
      Right (ColInt16Maybe (V.fromList xs), nodeIdx + 1, bufIdx + 2)

readInt32ColumnMaybe :: Endianness -> Bool -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readInt32ColumnMaybe endian signed len rb body !bufIdx !nodeIdx = do
  validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  validFlags <- unpackBools len validBs
  if BS.length valsBs < len * 4
    then Left "Arrow.Column: int32 values buffer too small"
    else do
      xs <-
        forM [0 .. len - 1] $ \i ->
          if not (V.unsafeIndex validFlags i)
            then pure Nothing
            else do
              let v = readWord32 endian valsBs (i * 4)
                  w = if signed then int32FromWord v else fromIntegral v
              pure $ Just w
      Right (ColInt32Maybe (V.fromList xs), nodeIdx + 1, bufIdx + 2)

readInt64ColumnMaybe :: Endianness -> Bool -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readInt64ColumnMaybe endian signed len rb body !bufIdx !nodeIdx = do
  validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  validFlags <- unpackBools len validBs
  if BS.length valsBs < len * 8
    then Left "Arrow.Column: int64 values buffer too small"
    else do
      xs <-
        forM [0 .. len - 1] $ \i ->
          if not (V.unsafeIndex validFlags i)
            then pure Nothing
            else do
              let v = readWord64 endian valsBs (i * 8)
                  w = if signed then int64FromWord v else fromIntegral v
              pure $ Just w
      Right (ColInt64Maybe (V.fromList xs), nodeIdx + 1, bufIdx + 2)

readUInt8ColumnMaybe :: Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readUInt8ColumnMaybe len rb body !bufIdx !nodeIdx = do
  validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  validFlags <- unpackBools len validBs
  if BS.length valsBs < len
    then Left "Arrow.Column: uint8 values buffer too small"
    else do
      xs <-
        forM [0 .. len - 1] $ \i ->
          if not (V.unsafeIndex validFlags i)
            then pure Nothing
            else pure $ Just (BSU.unsafeIndex valsBs i)
      Right (ColUInt8Maybe (V.fromList xs), nodeIdx + 1, bufIdx + 2)

readUInt16ColumnMaybe :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readUInt16ColumnMaybe endian len rb body !bufIdx !nodeIdx = do
  validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  validFlags <- unpackBools len validBs
  if BS.length valsBs < len * 2
    then Left "Arrow.Column: uint16 values buffer too small"
    else do
      xs <-
        forM [0 .. len - 1] $ \i ->
          if not (V.unsafeIndex validFlags i)
            then pure Nothing
            else pure $ Just (readWord16 endian valsBs (i * 2))
      Right (ColUInt16Maybe (V.fromList xs), nodeIdx + 1, bufIdx + 2)

readUInt32ColumnMaybe :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readUInt32ColumnMaybe endian len rb body !bufIdx !nodeIdx = do
  validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  validFlags <- unpackBools len validBs
  if BS.length valsBs < len * 4
    then Left "Arrow.Column: uint32 values buffer too small"
    else do
      xs <-
        forM [0 .. len - 1] $ \i ->
          if not (V.unsafeIndex validFlags i)
            then pure Nothing
            else pure $ Just (readWord32 endian valsBs (i * 4))
      Right (ColUInt32Maybe (V.fromList xs), nodeIdx + 1, bufIdx + 2)

readUInt64ColumnMaybe :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readUInt64ColumnMaybe endian len rb body !bufIdx !nodeIdx = do
  validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  validFlags <- unpackBools len validBs
  if BS.length valsBs < len * 8
    then Left "Arrow.Column: uint64 values buffer too small"
    else do
      xs <-
        forM [0 .. len - 1] $ \i ->
          if not (V.unsafeIndex validFlags i)
            then pure Nothing
            else pure $ Just (readWord64 endian valsBs (i * 8))
      Right (ColUInt64Maybe (V.fromList xs), nodeIdx + 1, bufIdx + 2)

readFloat16ColumnMaybe :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readFloat16ColumnMaybe endian len rb body !bufIdx !nodeIdx = do
  validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  validFlags <- unpackBools len validBs
  if BS.length valsBs < len * 2
    then Left "Arrow.Column: float16 values buffer too small"
    else do
      xs <-
        forM [0 .. len - 1] $ \i ->
          if not (V.unsafeIndex validFlags i)
            then pure Nothing
            else pure $ Just (readWord16 endian valsBs (i * 2))
      Right (ColFloat16Maybe (V.fromList xs), nodeIdx + 1, bufIdx + 2)

readBoolColumnMaybe :: Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readBoolColumnMaybe len rb body !bufIdx !nodeIdx = do
  validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  dataBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  validFlags <- unpackBools len validBs
  valFlags <- unpackBools len dataBs
  xs <-
    forM [0 .. len - 1] $ \i ->
      if not (V.unsafeIndex validFlags i)
        then pure Nothing
        else pure $ Just (V.unsafeIndex valFlags i)
  Right (ColBoolMaybe (V.fromList xs), nodeIdx + 1, bufIdx + 2)

readFloatColumnMaybe :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readFloatColumnMaybe endian len rb body !bufIdx !nodeIdx = do
  validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  validFlags <- unpackBools len validBs
  if BS.length valsBs < len * 4
    then Left "Arrow.Column: float values buffer too small"
    else do
      xs <-
        forM [0 .. len - 1] $ \i ->
          if not (V.unsafeIndex validFlags i)
            then pure Nothing
            else do
              v <- readF32 endian valsBs (i * 4)
              pure (Just v)
      Right (ColFloatMaybe (V.fromList xs), nodeIdx + 1, bufIdx + 2)

readDoubleColumnMaybe :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readDoubleColumnMaybe endian len rb body !bufIdx !nodeIdx = do
  validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  validFlags <- unpackBools len validBs
  if BS.length valsBs < len * 8
    then Left "Arrow.Column: double values buffer too small"
    else do
      xs <-
        forM [0 .. len - 1] $ \i ->
          if not (V.unsafeIndex validFlags i)
            then pure Nothing
            else do
              v <- readF64 endian valsBs (i * 8)
              pure (Just v)
      Right (ColDoubleMaybe (V.fromList xs), nodeIdx + 1, bufIdx + 2)

readUtf8ColumnMaybe :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readUtf8ColumnMaybe endian len rb body !bufIdx !nodeIdx = do
  validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  offBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  datBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 2))
  validFlags <- unpackBools len validBs
  if BS.length offBs < (len + 1) * 4
    then Left "Arrow.Column: UTF-8 offsets buffer too small"
    else do
      strs <-
        forM [0 .. len - 1] $ \i ->
          if not (V.unsafeIndex validFlags i)
            then pure Nothing
            else do
              s0 <- readI32 endian offBs (i * 4)
              s1 <- readI32 endian offBs ((i + 1) * 4)
              let !start = fromIntegral s0
                  !end = fromIntegral s1
              if start < 0 || end < start || end > BS.length datBs
                then Left "Arrow.Column: invalid UTF-8 slice (nullable)"
                else case TE.decodeUtf8' (BS.take (end - start) (BS.drop start datBs)) of
                  Right t -> pure $ Just t
                  Left _ -> Left "Arrow.Column: invalid UTF-8 bytes"
      Right (ColUtf8Maybe (V.fromList strs), nodeIdx + 1, bufIdx + 3)

readBinaryColumnMaybe :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readBinaryColumnMaybe endian len rb body !bufIdx !nodeIdx = do
  validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  offBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  datBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 2))
  validFlags <- unpackBools len validBs
  if BS.length offBs < (len + 1) * 4
    then Left "Arrow.Column: binary offsets buffer too small"
    else do
      bins <-
        forM [0 .. len - 1] $ \i ->
          if not (V.unsafeIndex validFlags i)
            then pure Nothing
            else do
              s0 <- readI32 endian offBs (i * 4)
              s1 <- readI32 endian offBs ((i + 1) * 4)
              let !start = fromIntegral s0
                  !end = fromIntegral s1
              if start < 0 || end < start || end > BS.length datBs
                then Left "Arrow.Column: invalid binary slice (nullable)"
                else pure $ Just $! BS.take (end - start) (BS.drop start datBs)
      Right (ColBinaryMaybe (V.fromList bins), nodeIdx + 1, bufIdx + 3)

readLargeUtf8ColumnMaybe :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readLargeUtf8ColumnMaybe endian len rb body !bufIdx !nodeIdx = do
  validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  offBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  datBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 2))
  validFlags <- unpackBools len validBs
  if BS.length offBs < (len + 1) * 8
    then Left "Arrow.Column: large UTF-8 offsets buffer too small"
    else do
      strs <-
        forM [0 .. len - 1] $ \i ->
          if not (V.unsafeIndex validFlags i)
            then pure Nothing
            else do
              let !s0 = fromIntegral (readWord64 endian offBs (i * 8)) :: Int
                  !s1 = fromIntegral (readWord64 endian offBs ((i + 1) * 8)) :: Int
              if s0 < 0 || s1 < s0 || s1 > BS.length datBs
                then Left "Arrow.Column: invalid large UTF-8 slice (nullable)"
                else case TE.decodeUtf8' (BS.take (s1 - s0) (BS.drop s0 datBs)) of
                  Right t -> pure $ Just t
                  Left _ -> Left "Arrow.Column: invalid large UTF-8 bytes"
      Right (ColLargeUtf8Maybe (V.fromList strs), nodeIdx + 1, bufIdx + 3)

readLargeBinaryColumnMaybe :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readLargeBinaryColumnMaybe endian len rb body !bufIdx !nodeIdx = do
  validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  offBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  datBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 2))
  validFlags <- unpackBools len validBs
  if BS.length offBs < (len + 1) * 8
    then Left "Arrow.Column: large binary offsets buffer too small"
    else do
      bins <-
        forM [0 .. len - 1] $ \i ->
          if not (V.unsafeIndex validFlags i)
            then pure Nothing
            else do
              let !s0 = fromIntegral (readWord64 endian offBs (i * 8)) :: Int
                  !s1 = fromIntegral (readWord64 endian offBs ((i + 1) * 8)) :: Int
              if s0 < 0 || s1 < s0 || s1 > BS.length datBs
                then Left "Arrow.Column: invalid large binary slice (nullable)"
                else pure $ Just $! BS.take (s1 - s0) (BS.drop s0 datBs)
      Right (ColLargeBinaryMaybe (V.fromList bins), nodeIdx + 1, bufIdx + 3)

readFixedSizeBinaryColumnMaybe :: Int -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readFixedSizeBinaryColumnMaybe byteWidth len rb body !bufIdx !nodeIdx = do
  validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  validFlags <- unpackBools len validBs
  if BS.length valsBs < len * byteWidth
    then Left "Arrow.Column: fixed-size binary values buffer too small"
    else do
      xs <-
        forM [0 .. len - 1] $ \i ->
          if not (V.unsafeIndex validFlags i)
            then pure Nothing
            else pure $ Just $! BS.take byteWidth (BS.drop (i * byteWidth) valsBs)
      Right (ColFixedSizeBinaryMaybe byteWidth (V.fromList xs), nodeIdx + 1, bufIdx + 2)

readDate32ColumnMaybe :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readDate32ColumnMaybe endian len rb body !bufIdx !nodeIdx = do
  validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  validFlags <- unpackBools len validBs
  if BS.length valsBs < len * 4
    then Left "Arrow.Column: date32 values buffer too small"
    else do
      xs <-
        forM [0 .. len - 1] $ \i ->
          if not (V.unsafeIndex validFlags i)
            then pure Nothing
            else pure $ Just (fromIntegral (readWord32 endian valsBs (i * 4)) :: Int32)
      Right (ColDate32Maybe (V.fromList xs), nodeIdx + 1, bufIdx + 2)

readDate64ColumnMaybe :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readDate64ColumnMaybe endian len rb body !bufIdx !nodeIdx = do
  validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  validFlags <- unpackBools len validBs
  if BS.length valsBs < len * 8
    then Left "Arrow.Column: date64 values buffer too small"
    else do
      xs <-
        forM [0 .. len - 1] $ \i ->
          if not (V.unsafeIndex validFlags i)
            then pure Nothing
            else pure $ Just (fromIntegral (readWord64 endian valsBs (i * 8)) :: Int64)
      Right (ColDate64Maybe (V.fromList xs), nodeIdx + 1, bufIdx + 2)

readTime32ColumnMaybe :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readTime32ColumnMaybe endian len rb body !bufIdx !nodeIdx = do
  validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  validFlags <- unpackBools len validBs
  if BS.length valsBs < len * 4
    then Left "Arrow.Column: time32 values buffer too small"
    else do
      xs <-
        forM [0 .. len - 1] $ \i ->
          if not (V.unsafeIndex validFlags i)
            then pure Nothing
            else pure $ Just (fromIntegral (readWord32 endian valsBs (i * 4)) :: Int32)
      Right (ColTime32Maybe (V.fromList xs), nodeIdx + 1, bufIdx + 2)

readTime64ColumnMaybe :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readTime64ColumnMaybe endian len rb body !bufIdx !nodeIdx = do
  validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  validFlags <- unpackBools len validBs
  if BS.length valsBs < len * 8
    then Left "Arrow.Column: time64 values buffer too small"
    else do
      xs <-
        forM [0 .. len - 1] $ \i ->
          if not (V.unsafeIndex validFlags i)
            then pure Nothing
            else pure $ Just (fromIntegral (readWord64 endian valsBs (i * 8)) :: Int64)
      Right (ColTime64Maybe (V.fromList xs), nodeIdx + 1, bufIdx + 2)

readTimestampColumnMaybe :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readTimestampColumnMaybe endian len rb body !bufIdx !nodeIdx = do
  validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  validFlags <- unpackBools len validBs
  if BS.length valsBs < len * 8
    then Left "Arrow.Column: timestamp values buffer too small"
    else do
      xs <-
        forM [0 .. len - 1] $ \i ->
          if not (V.unsafeIndex validFlags i)
            then pure Nothing
            else pure $ Just (fromIntegral (readWord64 endian valsBs (i * 8)) :: Int64)
      Right (ColTimestampMaybe (V.fromList xs), nodeIdx + 1, bufIdx + 2)

readDurationColumnMaybe :: Endianness -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readDurationColumnMaybe endian len rb body !bufIdx !nodeIdx = do
  validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  validFlags <- unpackBools len validBs
  if BS.length valsBs < len * 8
    then Left "Arrow.Column: duration values buffer too small"
    else do
      xs <-
        forM [0 .. len - 1] $ \i ->
          if not (V.unsafeIndex validFlags i)
            then pure Nothing
            else pure $ Just (fromIntegral (readWord64 endian valsBs (i * 8)) :: Int64)
      Right (ColDurationMaybe (V.fromList xs), nodeIdx + 1, bufIdx + 2)

readDecimal128ColumnMaybe :: Int -> Int -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readDecimal128ColumnMaybe precision scale len rb body !bufIdx !nodeIdx = do
  validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  validFlags <- unpackBools len validBs
  if BS.length valsBs < len * 16
    then Left "Arrow.Column: decimal128 values buffer too small"
    else do
      let vals = V.generate len $ \i ->
            if not (V.unsafeIndex validFlags i)
              then BS.replicate 16 0
              else BS.take 16 (BS.drop (i * 16) valsBs)
      Right (ColDecimal128 precision scale vals, nodeIdx + 1, bufIdx + 2)

readDecimal256ColumnMaybe :: Int -> Int -> Int -> RecordBatchDef -> ByteString -> Int -> Int -> Either String (ColumnArray, Int, Int)
readDecimal256ColumnMaybe precision scale len rb body !bufIdx !nodeIdx = do
  validBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) bufIdx)
  valsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
  validFlags <- unpackBools len validBs
  if BS.length valsBs < len * 32
    then Left "Arrow.Column: decimal256 values buffer too small"
    else do
      let vals = V.generate len $ \i ->
            if not (V.unsafeIndex validFlags i)
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

unpackBools :: Int -> ByteString -> Either String (V.Vector Bool)
unpackBools n bs
  -- Spec: an empty validity bitmap means "all values valid". When
  -- the producer's @null_count@ is 0 the bitmap is allowed to be
  -- omitted, in which case we still see a 0-length buffer entry
  -- (validity slot in the buffer list, but no body bytes).
  | BS.length bs == 0 = Right $! V.replicate n True
  | otherwise =
      let need = (n + 7) `div` 8
      in if BS.length bs < need
        then Left "Arrow.Column: bool buffer too small"
        else Right $! unpackBitsLsbUnsafe n bs

readInts8 :: Int -> ByteString -> Either String (VP.Vector Int8)
readInts8 len bs
  | BS.length bs < len = Left "Arrow.Column: int8 buffer too small"
  | otherwise =
      Right $
        VP.generate len $ \i ->
          fromIntegral (BSU.unsafeIndex bs i) :: Int8

readInts16 :: Endianness -> Bool -> Int -> ByteString -> Either String (VP.Vector Int16)
readInts16 endian signed len bs
  | BS.length bs < len * 2 = Left "Arrow.Column: int16 buffer too small"
  | otherwise =
      Right $
        VP.generate len $ \i ->
          let v = readWord16 endian bs (i * 2)
          in if signed then fromIntegral (fromIntegral v :: Int16) else fromIntegral v

-- | Memcpy a slice of @bs@ into a freshly-allocated primitive
-- vector. Assumes the source bit pattern matches the host
-- (Arrow LE on x86_64 / aarch64), so each element is exactly
-- the same bit pattern in @bs@ and in the result vector.
--
-- Replaces the byte-by-byte readWord32 / readWord64 +
-- VP.generate inner loop in the LE path of every primitive
-- column reader; for a 100 k Int64 column that's one memcpy
-- vs ~800 k bounds-checked unsafeIndex + shift / OR ops.
{-# INLINE memcpyPrimVecLE #-}
memcpyPrimVecLE
  :: forall a. VP.Prim a
  => Int                 -- ^ element size in bytes
  -> Int                 -- ^ number of elements
  -> ByteString
  -> VP.Vector a
memcpyPrimVecLE !elemBytes !len bs
  | len <= 0  = VP.empty
  | otherwise = unsafePerformIO $
      BSU.unsafeUseAsCStringLen bs $ \(cstr, _) -> do
        mv <- MVP.unsafeNew len
        let !srcPtr = castPtr cstr :: Ptr Word8
            !nBytes = len * elemBytes
        case mv of
          MVP.MVector dstOffElems _ dstMba ->
            PBA.copyPtrToMutableByteArray
              dstMba (dstOffElems * elemBytes) srcPtr nBytes
        VP.unsafeFreeze mv

readInts32 :: Endianness -> Bool -> Int -> ByteString -> Either String (VP.Vector Int32)
readInts32 endian signed len bs
  | BS.length bs < len * 4 = Left "Arrow.Column: int32 buffer too small"
  | endian == Little =
      -- Fast path: bit pattern matches the host. signed/unsigned
      -- distinguish only in how the bits are interpreted, not
      -- in storage; either way the underlying ByteArray is the
      -- same memcpy.
      Right (memcpyPrimVecLE 4 len bs)
  | otherwise =
      Right $
        VP.generate len $ \i ->
          let v = readWord32 endian bs (i * 4)
          in if signed then int32FromWord v else fromIntegral v

readInts64 :: Endianness -> Bool -> Int -> ByteString -> Either String (VP.Vector Int64)
readInts64 endian signed len bs
  | BS.length bs < len * 8 = Left "Arrow.Column: int64 buffer too small"
  | endian == Little = Right (memcpyPrimVecLE 8 len bs)
  | otherwise =
      Right $
        VP.generate len $ \i ->
          let v = readWord64 endian bs (i * 8)
          in if signed then int64FromWord v else fromIntegral v

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
    | n > 0     -> columnLength child `quot` n
    | otherwise -> 0
  ColFixedSizeListMaybe _ v _ -> V.length v
  ColMap offsets _ _ -> max 0 (VP.length offsets - 1)
  ColMapMaybe v _ _ _ -> V.length v
  ColDenseUnion typeIds _ _ -> VP.length typeIds
  ColSparseUnion typeIds _ -> VP.length typeIds
  ColDictionary _ indices _ -> VP.length indices
  ColRunEndEncoded runEnds _ ->
    -- The logical length is the LAST run-end value (exclusive).
    case runEnds of
      ColInt16 v -> if VP.null v then 0 else fromIntegral (VP.last v)
      ColInt32 v -> if VP.null v then 0 else fromIntegral (VP.last v)
      ColInt64 v -> if VP.null v then 0 else fromIntegral (VP.last v)
      _          -> 0
  ColListView offsets _ _       -> VP.length offsets
  ColListViewMaybe v _ _ _      -> V.length v
  ColLargeListView offsets _ _  -> VP.length offsets
  ColLargeListViewMaybe v _ _ _ -> V.length v
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
      !mp = VP.replicate total (-1) :: VP.Vector Int
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
        in  m VP.// [(ni, c)]
      nextVi vi t = case t of
        AUtf8View   -> vi + 1
        ABinaryView -> vi + 1
        _           -> vi
      (_, _, !out) = V.foldl' go (0 :: Int, 0 :: Int, mp) topFields
  in  V.fromList (VP.toList out)
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
-- @VP.Vector Int32@ used by 'ColDictionary'.
toInt32Indices :: ColumnArray -> Either String (VP.Vector Int32)
toInt32Indices = \case
  ColInt8   v -> Right (VP.map fromIntegral v)
  ColInt16  v -> Right (VP.map fromIntegral v)
  ColInt32  v -> Right v
  ColInt64  v -> Right (VP.map fromIntegral v)
  ColUInt8  v -> Right (VP.map fromIntegral v)
  ColUInt16 v -> Right (VP.map fromIntegral v)
  ColUInt32 v -> Right (VP.map fromIntegral v)
  ColUInt64 v -> Right (VP.map fromIntegral v)
  -- Nullable variants: coerce nulls to -1 sentinel (callers can
  -- consult the matching validity buffer if needed).
  ColInt8Maybe   v -> Right $ VP.fromList [fromMaybe (-1) (fmap fromIntegral m) | m <- V.toList v]
  ColInt16Maybe  v -> Right $ VP.fromList [fromMaybe (-1) (fmap fromIntegral m) | m <- V.toList v]
  ColInt32Maybe  v -> Right $ VP.fromList [fromMaybe (-1) (fmap fromIntegral m) | m <- V.toList v]
  ColInt64Maybe  v -> Right $ VP.fromList [fromMaybe (-1) (fmap fromIntegral m) | m <- V.toList v]
  c -> Left $ "Arrow.Column: dictionary index column has non-integer type: " ++ show c

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
  AUtf8       -> ColUtf8       V.empty
  ABinary     -> ColBinary     V.empty
  ALargeUtf8  -> ColLargeUtf8  V.empty
  ALargeBinary -> ColLargeBinary V.empty
  AInt 8 True -> ColInt8 VP.empty
  AInt 8 False -> ColUInt8 VP.empty
  AInt 16 True -> ColInt16 VP.empty
  AInt 16 False -> ColUInt16 VP.empty
  AInt 32 True -> ColInt32 VP.empty
  AInt 32 False -> ColUInt32 VP.empty
  AInt 64 True -> ColInt64 VP.empty
  AInt 64 False -> ColUInt64 VP.empty
  AFloatingPoint Single -> ColFloat VP.empty
  AFloatingPoint DoublePrecision -> ColDouble VP.empty
  ABool -> ColBool V.empty
  _    -> ColUtf8 V.empty   -- conservative fallback

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
      let offsets = VP.generate (len + 1) $ \i ->
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
      let offsets = VP.generate (len + 1) $ \i ->
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
      let typeIds = VP.generate len $ \i -> fromIntegral (BSU.unsafeIndex typeIdsBs i) :: Int8
      case mode of
        Dense -> do
          offsetsBs <- sliceBuffer body (V.unsafeIndex (rbBuffers rb) (bufIdx + 1))
          if BS.length offsetsBs < len * 4
            then Left "Arrow.Column: dense union offsets buffer too small"
            else do
              let offsets = VP.generate len $ \i ->
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
      let offsets = VP.generate (len + 1) $ \i ->
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
          let vec = VP.generate len $ \i ->
                fromIntegral (readWord32 endian dataBs (i * 4)) :: Int32
           in Right (ColIntervalYearMonth vec)
    DayTime
      | BS.length dataBs < len * 8 ->
          Left "Arrow.Column: interval DAY_TIME buffer too small"
      | otherwise ->
          let daysV   = VP.generate len $ \i ->
                fromIntegral (readWord32 endian dataBs (i * 8)) :: Int32
              millisV = VP.generate len $ \i ->
                fromIntegral (readWord32 endian dataBs (i * 8 + 4)) :: Int32
           in Right (ColIntervalDayTime daysV millisV)
    MonthDayNano
      | BS.length dataBs < len * 16 ->
          Left "Arrow.Column: interval MONTH_DAY_NANO buffer too small"
      | otherwise ->
          let monthsV = VP.generate len $ \i ->
                fromIntegral (readWord32 endian dataBs (i * 16)) :: Int32
              daysV   = VP.generate len $ \i ->
                fromIntegral (readWord32 endian dataBs (i * 16 + 4)) :: Int32
              nanosV  = VP.generate len $ \i ->
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
          let offs = VP.generate len $ \i ->
                fromIntegral (readWord64 endian offBs (i * 8)) :: Int64
              sizs = VP.generate len $ \i ->
                fromIntegral (readWord64 endian sizBs (i * 8)) :: Int64
          in Right $! case validity of
               Nothing -> (ColLargeListView offs sizs childCol, nodeIdx2, bufIdx3)
               Just vs -> (ColLargeListViewMaybe vs offs sizs childCol, nodeIdx2, bufIdx3)
        else
          let offs = VP.generate len $ \i ->
                fromIntegral (readWord32 endian offBs (i * 4)) :: Int32
              sizs = VP.generate len $ \i ->
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
        Just vs -> Just (V.zipWith (\v r -> if v then Just r else Nothing) vs (V.fromList rows))
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
-- the underlying primitive vectors when 'V.slice' / 'VP.slice'
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
      ColInt8 v       -> ColInt8 (VP.slice s l v)
      ColInt16 v      -> ColInt16 (VP.slice s l v)
      ColInt32 v      -> ColInt32 (VP.slice s l v)
      ColInt64 v      -> ColInt64 (VP.slice s l v)
      ColUInt8 v      -> ColUInt8 (VP.slice s l v)
      ColUInt16 v     -> ColUInt16 (VP.slice s l v)
      ColUInt32 v     -> ColUInt32 (VP.slice s l v)
      ColUInt64 v     -> ColUInt64 (VP.slice s l v)
      ColFloat16 v    -> ColFloat16 (VP.slice s l v)
      ColFloat v      -> ColFloat (VP.slice s l v)
      ColDouble v     -> ColDouble (VP.slice s l v)
      ColBool v       -> ColBool (V.slice s l v)
      ColUtf8 v       -> ColUtf8 (V.slice s l v)
      ColBinary v     -> ColBinary (V.slice s l v)
      ColLargeUtf8 v  -> ColLargeUtf8 (V.slice s l v)
      ColLargeBinary v -> ColLargeBinary (V.slice s l v)
      ColFixedSizeBinary w v -> ColFixedSizeBinary w (V.slice s l v)
      ColDate32 v     -> ColDate32 (VP.slice s l v)
      ColDate64 v     -> ColDate64 (VP.slice s l v)
      ColTime32 v     -> ColTime32 (VP.slice s l v)
      ColTime64 v     -> ColTime64 (VP.slice s l v)
      ColTimestamp v  -> ColTimestamp (VP.slice s l v)
      ColDuration v   -> ColDuration (VP.slice s l v)
      ColDecimal128 p sc v -> ColDecimal128 p sc (V.slice s l v)
      ColDecimal256 p sc v -> ColDecimal256 p sc (V.slice s l v)
      ColIntervalYearMonth v ->
        ColIntervalYearMonth (VP.slice s l v)
      ColIntervalDayTime d m ->
        ColIntervalDayTime (VP.slice s l d) (VP.slice s l m)
      ColIntervalMonthDayNano m d nano ->
        ColIntervalMonthDayNano
          (VP.slice s l m) (VP.slice s l d) (VP.slice s l nano)
      ColInt8Maybe v  -> ColInt8Maybe (V.slice s l v)
      ColInt16Maybe v -> ColInt16Maybe (V.slice s l v)
      ColInt32Maybe v -> ColInt32Maybe (V.slice s l v)
      ColInt64Maybe v -> ColInt64Maybe (V.slice s l v)
      ColUInt8Maybe v -> ColUInt8Maybe (V.slice s l v)
      ColUInt16Maybe v -> ColUInt16Maybe (V.slice s l v)
      ColUInt32Maybe v -> ColUInt32Maybe (V.slice s l v)
      ColUInt64Maybe v -> ColUInt64Maybe (V.slice s l v)
      ColFloat16Maybe v -> ColFloat16Maybe (V.slice s l v)
      ColFloatMaybe v -> ColFloatMaybe (V.slice s l v)
      ColDoubleMaybe v -> ColDoubleMaybe (V.slice s l v)
      ColBoolMaybe v -> ColBoolMaybe (V.slice s l v)
      ColUtf8Maybe v -> ColUtf8Maybe (V.slice s l v)
      ColBinaryMaybe v -> ColBinaryMaybe (V.slice s l v)
      ColLargeUtf8Maybe v -> ColLargeUtf8Maybe (V.slice s l v)
      ColLargeBinaryMaybe v -> ColLargeBinaryMaybe (V.slice s l v)
      ColFixedSizeBinaryMaybe w v -> ColFixedSizeBinaryMaybe w (V.slice s l v)
      ColDate32Maybe v -> ColDate32Maybe (V.slice s l v)
      ColDate64Maybe v -> ColDate64Maybe (V.slice s l v)
      ColTime32Maybe v -> ColTime32Maybe (V.slice s l v)
      ColTime64Maybe v -> ColTime64Maybe (V.slice s l v)
      ColTimestampMaybe v -> ColTimestampMaybe (V.slice s l v)
      ColDurationMaybe v -> ColDurationMaybe (V.slice s l v)
      ColUtf8View v       -> ColUtf8View (V.slice s l v)
      ColUtf8ViewMaybe v  -> ColUtf8ViewMaybe (V.slice s l v)
      ColBinaryView v     -> ColBinaryView (V.slice s l v)
      ColBinaryViewMaybe v -> ColBinaryViewMaybe (V.slice s l v)
      -- Struct: slice every child column at the same offset.
      ColStruct children ->
        ColStruct (V.map (\(nm, c) -> (nm, sliceColumnArray s l c)) children)
      ColStructMaybe valid children ->
        ColStructMaybe (V.slice s l valid)
          (V.map (\(nm, c) -> (nm, sliceColumnArray s l c)) children)
      ColNull _ -> ColNull l
      -- Variable-length / nested with shared-buffer semantics:
      -- offsets index into a flat child column. Slicing the
      -- offsets vector here keeps the child intact (which is the
      -- right semantics: an offset slice carves out the
      -- corresponding sub-range of the child without copying).
      ColList offsets child ->
        ColList (VP.slice s (l + 1) offsets) child
      ColLargeList offsets child ->
        ColLargeList (VP.slice s (l + 1) offsets) child
      ColListMaybe valid offsets child ->
        ColListMaybe (V.slice s l valid)
          (VP.slice s (l + 1) offsets) child
      ColLargeListMaybe valid offsets child ->
        ColLargeListMaybe (V.slice s l valid)
          (VP.slice s (l + 1) offsets) child
      ColMap offsets ks vs ->
        ColMap (VP.slice s (l + 1) offsets) ks vs
      ColMapMaybe valid offsets ks vs ->
        ColMapMaybe (V.slice s l valid)
          (VP.slice s (l + 1) offsets) ks vs
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
      let !n = max 0 (VP.length offs - 1)
          go !i
            | i >= n = Right ()
            | otherwise =
                let !s = fromIntegral (VP.unsafeIndex offs i) :: Int
                    !e = fromIntegral (VP.unsafeIndex offs (i + 1)) :: Int
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
        ColUtf8 v   -> compareSliceShow s w v
        ColUtf8Maybe v -> compareSliceShow s w v
        ColInt32 v -> compareSlice s w v
        ColInt64 v -> compareSlice s w v
        ColUInt32 v -> compareSlice s w v
        ColUInt64 v -> compareSlice s w v
        _ -> Nothing  -- unsupported key type; assume sorted

    compareSlice :: (Ord a, VP.Prim a, Show a)
                 => Int -> Int -> VP.Vector a -> Maybe (Int, String, String)
    compareSlice s w v
      | w < 2 = Nothing
      | otherwise =
          let go !j
                | j >= w = Nothing
                | otherwise =
                    let !a = VP.unsafeIndex v (s + j - 1)
                        !b = VP.unsafeIndex v (s + j)
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
