{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
-- | Apache Arrow IPC column encoders and stream\/file writers.
module Arrow.Write
  ( encodePlainInt32Column
  , encodePlainInt64Column
  , encodePlainFloat
  , encodePlainDouble
  , encodePlainBool
  , encodePlainUtf8
  , encodeNullBitmap
  , buildRecordBatch
  , writeArrowStream
  , writeArrowFile
    -- * Re-exported internals for alternative metadata framings
    -- (see "Arrow.FlatBufferIPC").
  , encodeColumns
  , emptyBuildAcc
  , BuildAcc (..)
  ) where

import Control.Monad (when)
import Data.Bits ((.&.), (.|.), shiftL, complement)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.IORef as IORef
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Primitive.ByteArray as PBA
import qualified Data.Text.Array as TA
import qualified Data.Text.Internal as TI
import qualified Foreign.Ptr
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable, pokeByteOff, sizeOf)
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import System.IO.Unsafe (unsafePerformIO)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Maybe (isJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Columnar.Bit (Bit (..))
import qualified Columnar.Bit as Bit

import Arrow.Types
import Arrow.IPC (encodeIPCMessage)
import Arrow.Column (ColumnArray (..), columnLength)
import qualified Arrow.Column as AC
import qualified Arrow.View as AV
import Foreign.ForeignPtr (castForeignPtr, withForeignPtr)

-- * Plain column encoders
--
-- Implementation note (perf): the previous shape was
--
--     BL.toStrict $ B.toLazyByteString $
--       VP.foldl' (\\acc v -> acc <> B.intXXLE v) mempty vec
--
-- which builds a 100k-deep chain of @Builder@ thunks, then
-- materialises the lazy chunk list, then copies it into a
-- fresh strict ByteString — ~3-4x slower than just allocating
-- the strict ByteString once and writing each element with a
-- single poke. Arrow's wire format is little-endian and we run
-- on x86_64 / aarch64 (both LE), so a host-order poke IS the
-- wire format.
--
-- 'pokePrimVecLE' is the shared helper; the per-type encoders
-- below dispatch on element size.

-- | Encode a primitive vector as a contiguous LE byte buffer.
{-# INLINE pokePrimVecLE #-}
pokePrimVecLE
  :: forall a. Storable a
  => Int                                 -- ^ element size in bytes
  -> (Ptr Word8 -> Int -> a -> IO ())    -- ^ poker (base, byte off, value)
  -> VS.Vector a
  -> ByteString
pokePrimVecLE !elemBytes poker vec =
  let !n = VS.length vec
  in BSI.unsafeCreate (n * elemBytes) $ \ptr -> do
       let go !i !off
             | i >= n    = pure ()
             | otherwise = do
                 poker ptr off (VS.unsafeIndex vec i)
                 go (i + 1) (off + elemBytes)
       go 0 0

encodePlainInt32Column :: VS.Vector Int32 -> ByteString
encodePlainInt32Column = pokePrimVecLE 4 (\p o v -> pokeByteOff p o v)

encodePlainInt64Column :: VS.Vector Int64 -> ByteString
encodePlainInt64Column = pokePrimVecLE 8 (\p o v -> pokeByteOff p o v)

encodePlainFloat :: VS.Vector Float -> ByteString
encodePlainFloat = pokePrimVecLE 4
  (\p o v -> pokeByteOff p o (castFloatToWord32 v))

encodePlainDouble :: VS.Vector Double -> ByteString
encodePlainDouble = pokePrimVecLE 8
  (\p o v -> pokeByteOff p o (castDoubleToWord64 v))

-- | Pack a boxed Bool vector into Arrow's bit-packed layout
-- (1 bit per value, LSB first, byte-aligned). Pre-allocates the
-- destination ByteString and writes packed bytes directly.
encodePlainBool :: V.Vector Bool -> ByteString
encodePlainBool vec =
  let !n      = V.length vec
      !nBytes = (n + 7) `quot` 8
  in BSI.unsafeCreate nBytes $ \ptr -> do
       let goByte !b
             | b >= nBytes = pure ()
             | otherwise = do
                 let !base = b * 8
                     packBit !bit !acc
                       | bit >= 8        = acc
                       | base + bit >= n = acc
                       | otherwise =
                           let !x = V.unsafeIndex vec (base + bit)
                               !acc' = if x
                                         then acc .|. (1 `shiftL` bit)
                                         else acc
                           in  packBit (bit + 1) acc'
                     !out = packBit 0 (0 :: Word8)
                 pokeByteOff ptr b out
                 goByte (b + 1)
       goByte 0

-- | Encode an Arrow Utf8 column as @(offsets, data)@.
--
-- Implementation note (perf): a 'Text' in text-2.x already
-- stores its bytes as UTF-8 in a 'TA.Array' (a 'ByteArray')
-- with O(1) access to the byte length and offset. The
-- previous shape called 'TE.encodeUtf8' on every value
-- (~100 k tiny ByteString allocations) and then did a
-- second pass to memcpy them into the output buffer. We can
-- skip the intermediate ByteStrings entirely: walk the
-- vector twice, once to size the output (using the Text's
-- stored byte length) and once to copy bytes straight from
-- each Text's ByteArray into the destination via
-- 'PBA.copyByteArrayToAddr'.
encodePlainUtf8 :: V.Vector Text -> (ByteString, ByteString)
encodePlainUtf8 vec =
  let !n          = V.length vec
      -- Total byte count: 'TI.Text' stores byte length
      -- directly (third field) so this is O(n) reads with
      -- no per-Text allocation.
      !totalBytes = V.foldl' (\a t -> case t of
                                 TI.Text _ _ l -> a + l) 0 vec
      !offsetsBs  = BSI.unsafeCreate ((n + 1) * 4) $ \ptr -> do
        let go !i !off
              | i >= n = pokeByteOff ptr (i * 4) (fromIntegral off :: Int32)
              | otherwise = do
                  pokeByteOff ptr (i * 4) (fromIntegral off :: Int32)
                  case V.unsafeIndex vec i of
                    TI.Text _ _ l -> go (i + 1) (off + l)
        go 0 0
      !dataBs = BSI.unsafeCreate totalBytes $ \ptr -> do
        let go !i !off
              | i >= n = pure ()
              | otherwise = do
                  case V.unsafeIndex vec i of
                    TI.Text (TA.ByteArray src#) srcOff lx -> do
                      let !srcBA = PBA.ByteArray src#
                      PBA.copyByteArrayToAddr
                        (ptr `Foreign.Ptr.plusPtr` off) srcBA srcOff lx
                      go (i + 1) (off + lx)
        go 0 0
  in (offsetsBs, dataBs)

-- | Pack a packed validity bitmap to its on-the-wire bytes.
-- Identity-ish (the 'VU.Vector Bit' is already byte-packed
-- internally) — emits the literal storage when the slice is
-- byte-aligned and rebuilds it via Bit.toByteString
-- otherwise.
encodeNullBitmap :: VU.Vector Bit -> ByteString
encodeNullBitmap = Bit.toByteString

-- * Internal column encoders

encodePlainBinary :: V.Vector ByteString -> (ByteString, ByteString)
encodePlainBinary vec = encodePlainBinaryInternal (V.length vec) vec

-- | Shared engine for 'encodePlainUtf8' and 'encodePlainBinary'.
-- Walks the vector once with a strict offset accumulator,
-- pre-allocates both output buffers to their exact final size,
-- and fills them with one @memcpy@ + one @poke@ per row.
encodePlainBinaryInternal
  :: Int -> V.Vector ByteString -> (ByteString, ByteString)
encodePlainBinaryInternal !n encVec =
  let !totalBytes = V.foldl' (\a bs -> a + BS.length bs) 0 encVec
      !offsetsBs  = BSI.unsafeCreate ((n + 1) * 4) $ \ptr -> do
        let go !i !off
              | i >= n = pokeByteOff ptr (i * 4) (fromIntegral off :: Int32)
              | otherwise = do
                  pokeByteOff ptr (i * 4) (fromIntegral off :: Int32)
                  go (i + 1) (off + BS.length (V.unsafeIndex encVec i))
        go 0 0
      !dataBs = BSI.unsafeCreate totalBytes $ \ptr -> do
        let go !i !off
              | i >= n = pure ()
              | otherwise = do
                  let !bs = V.unsafeIndex encVec i
                      !lx = BS.length bs
                  BSU.unsafeUseAsCStringLen bs $ \(srcPtr, _) ->
                    BSI.memcpy (ptr `Foreign.Ptr.plusPtr` off)
                               (Foreign.Ptr.castPtr srcPtr) lx
                  go (i + 1) (off + lx)
        go 0 0
  in (offsetsBs, dataBs)

encodeInt8s :: VS.Vector Int8 -> ByteString
encodeInt8s = pokePrimVecLE 1 (\p o v -> pokeByteOff p o v)

encodeInt16s :: VS.Vector Int16 -> ByteString
encodeInt16s = pokePrimVecLE 2 (\p o v -> pokeByteOff p o v)

-- Unsigned integer encoders.
encodeUInt8s :: VS.Vector Word8 -> ByteString
encodeUInt8s = pokePrimVecLE 1 (\p o v -> pokeByteOff p o v)

encodeUInt16s :: VS.Vector Word16 -> ByteString
encodeUInt16s = pokePrimVecLE 2 (\p o v -> pokeByteOff p o v)

encodeUInt32s :: VS.Vector Word32 -> ByteString
encodeUInt32s = pokePrimVecLE 4 (\p o v -> pokeByteOff p o v)

encodeUInt64s :: VS.Vector Word64 -> ByteString
encodeUInt64s = pokePrimVecLE 8 (\p o v -> pokeByteOff p o v)

-- Half-precision floats are just raw 16-bit words.
encodeFloat16s :: VS.Vector Word16 -> ByteString
encodeFloat16s = encodeUInt16s

-- Int64 encoder for LargeList / LargeBinary / LargeUtf8 offsets.
encodePlainInt64Offsets :: VS.Vector Int64 -> ByteString
encodePlainInt64Offsets = pokePrimVecLE 8 (\p o v -> pokeByteOff p o v)

-- Int32 array encoder that accepts the same shape as 'encodeInt16s'.
-- (Left as an alias for discoverability from the encodeCol site.)
encodeInt32s :: VS.Vector Int32 -> ByteString
encodeInt32s = encodePlainInt32Column

encodeInt64s :: VS.Vector Int64 -> ByteString
encodeInt64s = encodePlainInt64Column

-- Large variable-length (Int64 offsets) encoders. Two-pass:
-- first compute total payload size, then allocate the
-- offsets buffer ((n+1) * 8 bytes) and the data buffer
-- (totalSize bytes) once each and write straight in. Avoids
-- the Builder.<> loop that previously allocated one Builder
-- closure per element.
encodePlainLargeUtf8 :: V.Vector Text -> (ByteString, ByteString)
encodePlainLargeUtf8 vec =
  let !n = V.length vec
      -- First pass: encode each Text to ByteString, collect
      -- a parallel V.Vector of ByteStrings + record total
      -- size. encodeUtf8 caches the result inline so the
      -- second pass doesn't re-encode.
      !bss = V.map TE.encodeUtf8 vec
      !totalSize = V.foldl' (\acc bs -> acc + BS.length bs) 0 bss
  in (encodeLargeOffsetsBuf bss n, encodeContiguousPayload bss n totalSize)

encodePlainLargeBinary :: V.Vector ByteString -> (ByteString, ByteString)
encodePlainLargeBinary vec =
  let !n = V.length vec
      !totalSize = V.foldl' (\acc bs -> acc + BS.length bs) 0 vec
  in (encodeLargeOffsetsBuf vec n, encodeContiguousPayload vec n totalSize)

-- Allocate (n+1) * 8 bytes, write Int64-LE running offsets.
{-# INLINE encodeLargeOffsetsBuf #-}
encodeLargeOffsetsBuf :: V.Vector ByteString -> Int -> ByteString
encodeLargeOffsetsBuf vec n =
  let !sz = (n + 1) * 8
  in BSI.unsafeCreate sz $ \p ->
       let go !i !off
             | i >= n = pokeByteOff p (i * 8) (off :: Int64)
             | otherwise = do
                 pokeByteOff p (i * 8) (off :: Int64)
                 let !len = fromIntegral (BS.length (V.unsafeIndex vec i)) :: Int64
                 go (i + 1) (off + len)
       in go 0 0

-- Allocate totalSize bytes and memcpy each value's payload in.
{-# INLINE encodeContiguousPayload #-}
encodeContiguousPayload :: V.Vector ByteString -> Int -> Int -> ByteString
encodeContiguousPayload vec n totalSize =
  BSI.unsafeCreate totalSize $ \p ->
    let go !i !off
          | i >= n = pure ()
          | otherwise = do
              let !bs  = V.unsafeIndex vec i
                  !len = BS.length bs
              BSU.unsafeUseAsCStringLen bs $ \(srcPtr, _) ->
                BSI.memcpy (p `Foreign.Ptr.plusPtr` off)
                           (Foreign.Ptr.castPtr srcPtr) len
              go (i + 1) (off + len)
    in go 0 0

-- Fixed-size binary: just concatenate the fixed-width payloads. The
-- caller is expected to have enforced the width (we do a best-effort
-- pad / truncate here so ragged inputs don't corrupt the downstream
-- offsets).
encodePlainFixedSizeBinary :: Int -> V.Vector ByteString -> ByteString
encodePlainFixedSizeBinary !w vec
  | w <= 0 || V.null vec = BS.empty
  | otherwise =
      let !n  = V.length vec
          !sz = n * w
      in BSI.unsafeCreate sz $ \p ->
           let go !i !off
                 | i >= n = pure ()
                 | otherwise = do
                     let !bs  = V.unsafeIndex vec i
                         !raw = BS.length bs
                         !copyLen = min raw w
                     BSU.unsafeUseAsCStringLen bs $ \(srcPtr, _) ->
                       BSI.memcpy (p `Foreign.Ptr.plusPtr` off)
                                  (Foreign.Ptr.castPtr srcPtr) copyLen
                     -- Zero-pad if the input is shorter than w.
                     when (copyLen < w) $ do
                       _ <- BSI.memset (p `Foreign.Ptr.plusPtr` (off + copyLen))
                                       0 (fromIntegral (w - copyLen))
                       pure ()
                     go (i + 1) (off + w)
           in go 0 0

-- Interval encoders (YearMonth / DayTime / MonthDayNano).
encodeIntervalYearMonth :: VS.Vector Int32 -> ByteString
encodeIntervalYearMonth = encodePlainInt32Column

encodeIntervalDayTime :: VS.Vector Int32 -> VS.Vector Int32 -> ByteString
encodeIntervalDayTime days millis =
  let !n = min (VS.length days) (VS.length millis)
      !sz = n * 8
  in BSI.unsafeCreate sz $ \p ->
       let go !i !off
             | i >= n = pure ()
             | otherwise = do
                 pokeByteOff p off       (VS.unsafeIndex days i  :: Int32)
                 pokeByteOff p (off + 4) (VS.unsafeIndex millis i :: Int32)
                 go (i + 1) (off + 8)
       in go 0 0

encodeIntervalMonthDayNano
  :: VS.Vector Int32 -> VS.Vector Int32 -> VS.Vector Int64 -> ByteString
encodeIntervalMonthDayNano months days nanos =
  let !n = min (VS.length months) (min (VS.length days) (VS.length nanos))
      !sz = n * 16
  in BSI.unsafeCreate sz $ \p ->
       let go !i !off
             | i >= n = pure ()
             | otherwise = do
                 pokeByteOff p off       (VS.unsafeIndex months i :: Int32)
                 pokeByteOff p (off + 4) (VS.unsafeIndex days i   :: Int32)
                 pokeByteOff p (off + 8) (VS.unsafeIndex nanos i  :: Int64)
                 go (i + 1) (off + 16)
       in go 0 0

alignUp8 :: Int -> Int
alignUp8 n = (n + 7) .&. complement 7

-- * Record batch builder accumulator

data BuildAcc = BuildAcc
  { baOffset    :: !Int64
  , baNodes     :: ![FieldNode]
  , baBufs      :: ![Buffer]
  , baBody      :: !B.Builder
  , baVariadic  :: ![Int64]
    -- ^ For each Utf8View / BinaryView field encountered (in
    -- pre-order DFS), the number of variadic data buffers emitted.
    -- The Arrow @RecordBatch.variadicBufferCounts@ vector is the
    -- reverse of this list at the end of encoding.
  }

emptyBuildAcc :: BuildAcc
emptyBuildAcc = BuildAcc 0 [] [] mempty []

addBufData :: ByteString -> BuildAcc -> BuildAcc
addBufData bs (BuildAcc off ns bufs body var) =
  let !rawLen = BS.length bs
      !padded = alignUp8 rawLen
      !pad = padded - rawLen
  in BuildAcc
      (off + fromIntegral padded)
      ns
      (Buffer off (fromIntegral rawLen) : bufs)
      (body <> B.byteString bs <> B.byteString (BS.replicate pad 0))
      var

addFieldNode :: Int64 -> Int64 -> BuildAcc -> BuildAcc
addFieldNode len nc (BuildAcc off ns bufs body var) =
  BuildAcc off (FieldNode len nc : ns) bufs body var

-- | Record one entry in the variadic-buffer-counts vector for the
-- /current/ view column. Called exactly once per ColUtf8View /
-- ColBinaryView encoded.
addVariadicCount :: Int64 -> BuildAcc -> BuildAcc
addVariadicCount c (BuildAcc off ns bufs body var) =
  BuildAcc off ns bufs body (c : var)

countNulls :: V.Vector (Maybe a) -> Int
countNulls = V.foldl' (\c x -> case x of Nothing -> c + 1; Just _ -> c) 0

-- * Top-level record batch encoder

-- | Encode a record batch as a complete IPC message (continuation + metadata + body).
--
-- This is the /simplified/ writer that emits the hand-rolled
-- metadata format from "Arrow.IPC" (matching its decoder). Body
-- compression isn't supported on this path — the simplified
-- metadata format predates the spec's @BodyCompression@ table
-- and pyarrow / arrow-cpp readers don't accept the simplified
-- shape anyway. Use 'Arrow.Stream.encodeArrowStream' /
-- 'Arrow.Stream.encodeArrowFile' for a spec-compliant writer
-- that supports body compression, dictionary batches, and the
-- full metadata surface.
buildRecordBatch :: Schema -> V.Vector ColumnArray -> ByteString
buildRecordBatch schema cols =
  let !acc = encodeColumns (arrowFields schema) cols emptyBuildAcc
      !nodes = V.fromList (reverse (baNodes acc))
      !bufs = V.fromList (reverse (baBufs acc))
      !numRows = if V.null cols then 0 else columnLength (V.head cols)
      !rb = RecordBatchDef
        { rbLength = fromIntegral numRows
        , rbNodes = nodes
        , rbBuffers = bufs
        , rbVariadicBufferCounts = V.fromList (reverse (baVariadic acc))
        , rbBodyCompression = Nothing
        }
      !bodyLen = baOffset acc
      !metaBs = encodeRecordBatchMeta rb bodyLen
      !metaLen = BS.length metaBs
      !paddedMetaLen = alignUp8 metaLen
      !metaPad = paddedMetaLen - metaLen
      !bodyBs = BL.toStrict (B.toLazyByteString (baBody acc))
  in BL.toStrict $ B.toLazyByteString $
      B.word32LE 0xFFFFFFFF
      <> B.int32LE (fromIntegral paddedMetaLen)
      <> B.byteString metaBs
      <> B.byteString (BS.replicate metaPad 0)
      <> B.byteString bodyBs

-- Metadata in the same simplified format as Arrow.IPC
encodeRecordBatchMeta :: RecordBatchDef -> Int64 -> ByteString
encodeRecordBatchMeta rb bodyLen = BL.toStrict $ B.toLazyByteString $
  B.int16LE 4
  <> B.word8 3
  <> B.int32LE (fromIntegral (BS.length headerBs))
  <> B.byteString headerBs
  <> B.int64LE bodyLen
  where
    headerBs = BL.toStrict $ B.toLazyByteString $
      B.int64LE (rbLength rb)
      <> B.int32LE (fromIntegral (V.length (rbNodes rb)))
      <> V.foldl' (\acc n -> acc <> B.int64LE (fnLength n) <> B.int64LE (fnNullCount n)) mempty (rbNodes rb)
      <> B.int32LE (fromIntegral (V.length (rbBuffers rb)))
      <> V.foldl' (\acc b -> acc <> B.int64LE (bufOffset b) <> B.int64LE (bufLength b)) mempty (rbBuffers rb)

-- * Column encoding (DFS preorder, matching Arrow spec)

encodeColumns :: V.Vector Field -> V.Vector ColumnArray -> BuildAcc -> BuildAcc
encodeColumns fields cols acc =
  V.ifoldl' (\a i f -> encodeCol f (V.unsafeIndex cols i) a) acc fields

-- | Encode one column (depth-first preorder per the Arrow IPC spec).
-- Every 'ColumnArray' constructor has a handler; non-null primitive
-- columns emit a single data buffer, nullable primitives prepend a
-- validity bitmap, variable-length columns emit (offsets, data),
-- large variants use 64-bit offsets, and nested columns recurse into
-- their children.
encodeCol :: Field -> ColumnArray -> BuildAcc -> BuildAcc
encodeCol f col acc = case col of
  -- ============================================================
  -- Non-null primitives
  -- ============================================================
  ColInt8  v -> primFlat (encodeInt8s  v) (VS.length v) acc
  ColInt16 v -> primFlat (encodeInt16s v) (VS.length v) acc
  ColInt32 v -> primFlat (encodeInt32s v) (VS.length v) acc
  ColInt64 v -> primFlat (encodeInt64s v) (VS.length v) acc
  ColUInt8  v -> primFlat (encodeUInt8s  v) (VS.length v) acc
  ColUInt16 v -> primFlat (encodeUInt16s v) (VS.length v) acc
  ColUInt32 v -> primFlat (encodeUInt32s v) (VS.length v) acc
  ColUInt64 v -> primFlat (encodeUInt64s v) (VS.length v) acc
  ColFloat16 v -> primFlat (encodeFloat16s v) (VS.length v) acc
  ColFloat   v -> primFlat (encodePlainFloat  v) (VS.length v) acc
  ColDouble  v -> primFlat (encodePlainDouble v) (VS.length v) acc
  ColBool    v -> primFlat (Bit.toByteString  v) (VU.length v) acc

  -- Date / time / timestamp / duration are all fixed-width integer
  -- payloads under the hood.
  ColDate32    v -> primFlat (encodeInt32s v) (VS.length v) acc
  ColDate64    v -> primFlat (encodeInt64s v) (VS.length v) acc
  ColTime32    v -> primFlat (encodeInt32s v) (VS.length v) acc
  ColTime64    v -> primFlat (encodeInt64s v) (VS.length v) acc
  ColTimestamp v -> primFlat (encodeInt64s v) (VS.length v) acc
  ColDuration  v -> primFlat (encodeInt64s v) (VS.length v) acc

  -- Interval / decimal / fixed-size binary: fixed-width payloads
  -- with unit-specific strides.
  ColIntervalYearMonth v ->
    primFlat (encodeIntervalYearMonth v) (VS.length v) acc
  ColIntervalDayTime days millis ->
    primFlat (encodeIntervalDayTime days millis) (VS.length days) acc
  ColIntervalMonthDayNano months days nanos ->
    primFlat (encodeIntervalMonthDayNano months days nanos) (VS.length months) acc
  ColDecimal128 _ _ v ->
    primFlat (encodePlainFixedSizeBinary 16 v) (V.length v) acc
  ColDecimal256 _ _ v ->
    primFlat (encodePlainFixedSizeBinary 32 v) (V.length v) acc
  ColFixedSizeBinary w v ->
    primFlat (encodePlainFixedSizeBinary w v) (V.length v) acc

  -- ============================================================
  -- Non-null variable-length columns
  -- ============================================================
  -- Utf8 / Binary columns now carry view types whose
  -- (offsets, data) buffers are already in wire format. We
  -- can dump them straight to the body without any
  -- per-element work — this is one VS.Vector ByteString
  -- conversion per buffer (one syscall under the hood).
  ColUtf8 v ->
    let !offBs = vsByteString (AV.uvOffsets v)
        !datBs = vsByteString (AV.uvData v)
    in varFlat offBs datBs (AV.vLength v) acc
  ColBinary v ->
    let !offBs = vsByteString (AV.bvOffsets v)
        !datBs = vsByteString (AV.bvData v)
    in varFlat offBs datBs (AV.vLength v) acc
  ColLargeUtf8 v ->
    let !offBs = vsByteString (AV.luvOffsets v)
        !datBs = vsByteString (AV.luvData v)
    in varFlat offBs datBs (AV.vLength v) acc
  ColLargeBinary v ->
    let !offBs = vsByteString (AV.lbvOffsets v)
        !datBs = vsByteString (AV.lbvData v)
    in varFlat offBs datBs (AV.vLength v) acc

  -- ============================================================
  -- Nullable primitives (one validity bitmap + one data buffer)
  -- ============================================================
  ColInt8Maybe   v -> primNullable encodeInt8s  (0 :: Int8)  v acc
  ColInt16Maybe  v -> primNullable encodeInt16s (0 :: Int16) v acc
  ColInt32Maybe  v -> primNullable encodeInt32s (0 :: Int32) v acc
  ColInt64Maybe  v -> primNullable encodeInt64s (0 :: Int64) v acc
  ColUInt8Maybe  v -> primNullable encodeUInt8s  (0 :: Word8)  v acc
  ColUInt16Maybe v -> primNullable encodeUInt16s (0 :: Word16) v acc
  ColUInt32Maybe v -> primNullable encodeUInt32s (0 :: Word32) v acc
  ColUInt64Maybe v -> primNullable encodeUInt64s (0 :: Word64) v acc
  ColFloat16Maybe v -> primNullable encodeFloat16s (0 :: Word16) v acc
  ColFloatMaybe   v -> primNullable encodePlainFloat  (0 :: Float)  v acc
  ColDoubleMaybe  v -> primNullable encodePlainDouble (0 :: Double) v acc
  -- Nullable Bool: validity bitmap + values bitmap, both
  -- already in wire format.
  ColBoolMaybe v ->
    let !validity = encodeNullBitmap (AV.nbvValidity v)
        !values   = encodeNullBitmap (AV.nbvValues v)
        !n        = fromIntegral (AV.nbvLength v) :: Int64
        !nc       = fromIntegral (AV.nbvNullCount v) :: Int64
    in addBufData values
       $ addBufData validity
       $ addFieldNode n nc acc
  ColDate32Maybe    v -> primNullable encodeInt32s (0 :: Int32) v acc
  ColDate64Maybe    v -> primNullable encodeInt64s (0 :: Int64) v acc
  ColTime32Maybe    v -> primNullable encodeInt32s (0 :: Int32) v acc
  ColTime64Maybe    v -> primNullable encodeInt64s (0 :: Int64) v acc
  ColTimestampMaybe v -> primNullable encodeInt64s (0 :: Int64) v acc
  ColDurationMaybe  v -> primNullable encodeInt64s (0 :: Int64) v acc

  -- Nullable variable-length: validity + offsets + data,
  -- all already in wire format. Three buffer handoffs.
  ColUtf8Maybe v -> writeNullableVar
                      (AV.nuvValidity v)
                      (AV.uvOffsets (AV.nuvValues v))
                      (AV.uvData (AV.nuvValues v))
                      acc
  ColBinaryMaybe v -> writeNullableVar
                        (AV.nbvBinValidity v)
                        (AV.bvOffsets (AV.nbvBinValues v))
                        (AV.bvData (AV.nbvBinValues v))
                        acc
  ColLargeUtf8Maybe v -> writeNullableVar
                           (AV.nluvValidity v)
                           (AV.luvOffsets (AV.nluvValues v))
                           (AV.luvData (AV.nluvValues v))
                           acc
  ColLargeBinaryMaybe v -> writeNullableVar
                             (AV.nlbvValidity v)
                             (AV.lbvOffsets (AV.nlbvValues v))
                             (AV.lbvData (AV.nlbvValues v))
                             acc
  ColFixedSizeBinaryMaybe v ->
    primNullableBoxed
      (encodePlainFixedSizeBinary (AV.nfsbvWidth v))
      (BS.replicate (AV.nfsbvWidth v) 0)
      (AV.nullableFixedSizeBinaryViewToMaybeVector v)
      acc

  -- ============================================================
  -- Nested columns
  -- ============================================================
  ColStruct children ->
    let !n = if V.null children
               then 0
               else fromIntegral (columnLength (snd (V.head children))) :: Int64
        acc1 = addFieldNode n 0 acc
        childFields = fieldChildren f
    in V.ifoldl' (\a i (_, cc) -> encodeCol (V.unsafeIndex childFields i) cc a) acc1 children

  ColStructMaybe validity children ->
    let !n = fromIntegral (VU.length validity) :: Int64
        !nc = validityNullCount validity
        acc1 = addBufData (encodeNullBitmap validity) $ addFieldNode n nc acc
        childFields = fieldChildren f
    in V.ifoldl' (\a i (_, cc) -> encodeCol (V.unsafeIndex childFields i) cc a) acc1 children

  ColList offsets child ->
    let !n = fromIntegral (max 0 (VS.length offsets - 1)) :: Int64
        acc1 = addBufData (encodePlainInt32Column offsets) $ addFieldNode n 0 acc
        childField = childFieldAt f 0
    in encodeCol childField child acc1

  ColListMaybe validity offsets child ->
    let !n = fromIntegral (VU.length validity) :: Int64
        !nc = validityNullCount validity
        acc1 = addBufData (encodePlainInt32Column offsets)
             $ addBufData (encodeNullBitmap validity)
             $ addFieldNode n nc acc
        childField = childFieldAt f 0
    in encodeCol childField child acc1

  ColLargeList offsets child ->
    let !n = fromIntegral (max 0 (VS.length offsets - 1)) :: Int64
        acc1 = addBufData (encodePlainInt64Offsets offsets) $ addFieldNode n 0 acc
        childField = childFieldAt f 0
    in encodeCol childField child acc1

  ColLargeListMaybe validity offsets child ->
    let !n = fromIntegral (VU.length validity) :: Int64
        !nc = validityNullCount validity
        acc1 = addBufData (encodePlainInt64Offsets offsets)
             $ addBufData (encodeNullBitmap validity)
             $ addFieldNode n nc acc
        childField = childFieldAt f 0
    in encodeCol childField child acc1

  -- FixedSizeList has no offsets buffer: the length is implicit in
  -- the schema's FixedSizeList type, and the child array is exactly
  -- @parentLen * size@ long. We emit a single FieldNode for the
  -- parent then recurse into the child.
  ColFixedSizeList w child ->
    let !listLen   = max 1 w
        !parentLen = fromIntegral (columnLength child `quot` listLen) :: Int64
        acc1 = addFieldNode parentLen 0 acc
        childField = childFieldAt f 0
    in encodeCol childField child acc1

  ColFixedSizeListMaybe _ validity child ->
    let !n = fromIntegral (VU.length validity) :: Int64
        !nc = validityNullCount validity
        acc1 = addBufData (encodeNullBitmap validity) $ addFieldNode n nc acc
        childField = childFieldAt f 0
    in encodeCol childField child acc1

  ColMap offsets keyChild valChild ->
    let !n = fromIntegral (max 0 (VS.length offsets - 1)) :: Int64
        acc1 = addBufData (encodePlainInt32Column offsets) $ addFieldNode n 0 acc
        -- Map child is a single struct field; the struct itself has
        -- one FieldNode + (keys, values) sub-fields.
        structField = childFieldAt f 0
        keyField    = childFieldAt structField 0
        valField    = childFieldAt structField 1
        !structLen  = fromIntegral (columnLength keyChild) :: Int64
        acc2 = addFieldNode structLen 0 acc1
        acc3 = encodeCol keyField keyChild acc2
    in encodeCol valField valChild acc3

  ColMapMaybe validity offsets keyChild valChild ->
    let !n = fromIntegral (VU.length validity) :: Int64
        !nc = validityNullCount validity
        acc1 = addBufData (encodePlainInt32Column offsets)
             $ addBufData (encodeNullBitmap validity)
             $ addFieldNode n nc acc
        structField = childFieldAt f 0
        keyField    = childFieldAt structField 0
        valField    = childFieldAt structField 1
        !structLen  = fromIntegral (columnLength keyChild) :: Int64
        acc2 = addFieldNode structLen 0 acc1
        acc3 = encodeCol keyField keyChild acc2
    in encodeCol valField valChild acc3

  -- Union columns don't carry a top-level validity bitmap (nulls are
  -- represented via the child arrays + the type_ids buffer).
  ColDenseUnion typeIds offsets children ->
    let !n = fromIntegral (VS.length typeIds) :: Int64
        acc1 = addBufData (encodePlainInt32Column offsets)
             $ addBufData (encodeInt8s typeIds)
             $ addFieldNode n 0 acc
        childFields = fieldChildren f
    in V.ifoldl' (\a i cc -> encodeCol (V.unsafeIndex childFields i) cc a) acc1 children

  ColSparseUnion typeIds children ->
    let !n = fromIntegral (VS.length typeIds) :: Int64
        acc1 = addBufData (encodeInt8s typeIds) $ addFieldNode n 0 acc
        childFields = fieldChildren f
    in V.ifoldl' (\a i cc -> encodeCol (V.unsafeIndex childFields i) cc a) acc1 children

  -- Dictionary-encoded columns emit the indices like a regular Int32
  -- column; the dictionary itself lives in a separate DictionaryBatch
  -- IPC message (handled at the stream-writer level, out of scope for
  -- this per-column encoder).
  ColDictionary _dictId indices _dictValues ->
    primFlat (encodePlainInt32Column indices) (VS.length indices) acc

  -- RunEndEncoded: parent emits ZERO buffers (no validity, no data)
  -- and one FieldNode for itself; the run_ends and values children
  -- carry their own buffers and field nodes via recursive encodeCol.
  ColRunEndEncoded runEnds values ->
    let !logicalLen = fromIntegral (columnLength (ColRunEndEncoded runEnds values)) :: Int64
        acc1 = addFieldNode logicalLen 0 acc
        runEndsField = childFieldAt f 0
        valuesField  = childFieldAt f 1
        acc2 = encodeCol runEndsField runEnds acc1
    in encodeCol valuesField values acc2

  -- ListView: validity (when nullable), offsets (i32), sizes (i32),
  -- then the child column.
  ColListView offsets sizes child ->
    let !n = fromIntegral (VS.length offsets) :: Int64
        acc1 = addBufData (encodePlainInt32Column sizes)
             $ addBufData (encodePlainInt32Column offsets)
             $ addFieldNode n 0 acc
        childField = childFieldAt f 0
    in encodeCol childField child acc1
  ColListViewMaybe validity offsets sizes child ->
    let !n  = fromIntegral (VU.length validity) :: Int64
        !nc = validityNullCount validity
        acc1 = addBufData (encodePlainInt32Column sizes)
             $ addBufData (encodePlainInt32Column offsets)
             $ addBufData (encodeNullBitmap validity)
             $ addFieldNode n nc acc
        childField = childFieldAt f 0
    in encodeCol childField child acc1
  ColLargeListView offsets sizes child ->
    let !n = fromIntegral (VS.length offsets) :: Int64
        acc1 = addBufData (encodePlainInt64Offsets sizes)
             $ addBufData (encodePlainInt64Offsets offsets)
             $ addFieldNode n 0 acc
        childField = childFieldAt f 0
    in encodeCol childField child acc1
  ColLargeListViewMaybe validity offsets sizes child ->
    let !n  = fromIntegral (VU.length validity) :: Int64
        !nc = validityNullCount validity
        acc1 = addBufData (encodePlainInt64Offsets sizes)
             $ addBufData (encodePlainInt64Offsets offsets)
             $ addBufData (encodeNullBitmap validity)
             $ addFieldNode n nc acc
        childField = childFieldAt f 0
    in encodeCol childField child acc1

  -- Utf8View / BinaryView: 2 buffers (validity + view) plus zero
  -- variadic data buffers. We INLINE every payload — if any value
  -- exceeds 12 bytes we fall back to using one variadic buffer
  -- holding all out-of-line payloads concatenated.
  ColUtf8View vs ->
    encodeViewColumn (V.map (Just . TE.encodeUtf8) vs) False acc
  ColUtf8ViewMaybe vs ->
    encodeViewColumn (V.map (fmap TE.encodeUtf8) vs) True  acc
  ColBinaryView vs ->
    encodeViewColumn (V.map Just vs) False acc
  ColBinaryViewMaybe vs ->
    encodeViewColumn vs True acc

  -- ANull column: just emits a field-node with the row count;
  -- no buffers, no validity bitmap (per spec).
  ColNull n ->
    acc { baNodes = baNodes acc ++
                       [ FieldNode
                           { fnLength    = fromIntegral n
                           , fnNullCount = fromIntegral n
                           }
                       ]
        }

-- ============================================================
-- Helpers
-- ============================================================

-- | Pick the i-th child field, defaulting to the parent on an out-of-
-- range index (which should never happen for a well-formed schema,
-- but we'd rather produce a degenerate record batch than crash).
childFieldAt :: Field -> Int -> Field
childFieldAt f i =
  let !cs = fieldChildren f
  in if i < V.length cs then V.unsafeIndex cs i else f

primFlat :: ByteString -> Int -> BuildAcc -> BuildAcc
primFlat bs n acc =
  addBufData bs (addFieldNode (fromIntegral n) 0 acc)

-- | Encode one Utf8View / BinaryView column. Each row is laid out
-- as a 16-byte view struct; payloads <= 12 bytes are stored
-- inline, longer payloads are pooled into a single variadic data
-- buffer.
--
-- Buffers emitted (in order, append-to-acc semantics): variadic
-- data buffer (only when needed), view buffer, validity bitmap
-- (when @nullable@). 'addBufData' prepends to the buffer list, so
-- we add validity LAST to match the spec order
-- @[validity, view, ...variadic]@.
encodeViewColumn :: V.Vector (Maybe ByteString) -> Bool -> BuildAcc -> BuildAcc
encodeViewColumn vs nullable acc =
  let !n = V.length vs
      (variadicLen, viewBytes) = buildViews vs
      hasVariadic = variadicLen > 0
      validity = boxedToBitVec (V.map (maybe False (const True)) vs)
      !nc = if nullable
              then fromIntegral (V.foldl' (\c m -> if isJust m then c else c + 1) (0 :: Int) vs) :: Int64
              else 0
      -- Emission order = final buffer-list order. Final order:
      --   [validity (if nullable), view, variadic (if needed)].
      acc1 = addFieldNode (fromIntegral n) nc acc
      acc2 = if nullable
               then addBufData (encodeNullBitmap validity) acc1
               else acc1
      acc3 = addBufData viewBytes acc2
      acc4 = if hasVariadic
               then addBufData (variadicPayload vs) acc3
               else acc3
  in addVariadicCount (fromIntegral (if hasVariadic then 1 else 0 :: Int)) acc4

-- | Pack each row into its 16-byte view struct + collect the
-- variadic-buffer payload.  Returns @(variadicTotalLen, viewBytes)@
-- where @viewBytes@ has length @n*16@.
buildViews :: V.Vector (Maybe ByteString) -> (Int, ByteString)
buildViews vs =
  let !n = V.length vs
      -- First pass: compute variadic offsets.
      offsets :: V.Vector Int
      offsets = V.unfoldrExactN n step (0 :: Int)
        where
          step !off
            | off >= 0 =
                let i = (V.length vs - 1) - (V.length vs - V.length vs)
                in (off, off)
            | otherwise = (off, off)
      _ = offsets   -- unused; computed inline below
      -- Allocate output bytestring builder.
      go (!off, accB) (Just bs)
        | BS.length bs <= 12 =
            let !len = BS.length bs
                !padded = bs <> BS.replicate (12 - len) 0
                !rec = BL.toStrict (B.toLazyByteString $
                         B.int32LE (fromIntegral len) <> B.byteString padded)
            in (off, accB <> B.byteString rec)
        | otherwise =
            let !len = BS.length bs
                !prefix = BS.take 4 bs
                !padPrefix = prefix <> BS.replicate (4 - BS.length prefix) 0
                !rec = BL.toStrict (B.toLazyByteString $
                         B.int32LE (fromIntegral len)
                         <> B.byteString padPrefix
                         <> B.int32LE 0          -- buffer index
                         <> B.int32LE (fromIntegral off))
            in (off + len, accB <> B.byteString rec)
      go (!off, accB) Nothing =
        -- Null view: 16 zero bytes.
        (off, accB <> B.byteString (BS.replicate 16 0))
      (variadicLen, builder) = V.foldl' go (0 :: Int, mempty) vs
  in  ( variadicLen
      , BL.toStrict (B.toLazyByteString builder)
      )

-- | Concatenated variadic-buffer payload for the rows that need
-- out-of-line storage.
variadicPayload :: V.Vector (Maybe ByteString) -> ByteString
variadicPayload vs = BL.toStrict $ B.toLazyByteString $ V.foldl'
  (\acc m -> case m of
     Just bs | BS.length bs > 12 -> acc <> B.byteString bs
     _                           -> acc)
  mempty vs

varFlat :: ByteString -> ByteString -> Int -> BuildAcc -> BuildAcc
varFlat offBs datBs n acc =
  addBufData datBs $ addBufData offBs
    $ addFieldNode (fromIntegral n) 0 acc

-- | Encode a @V.Vector (Maybe a)@ for an @Unboxed/Primitive a@ value
-- payload. Builds a validity bitmap + a dense payload (nulls filled
-- with the caller-supplied zero).
--
-- Implementation note (perf): the previous version did
-- @V.map isJust vec@ to materialise a boxed @V.Vector Bool@
-- and then packed it via 'encodeNullBitmap', plus an extra
-- 'countNulls' pass. Now we do one fused walk: a single
-- 'encodeMaybeNullBitmap' that packs bits straight from the
-- source @V.Vector (Maybe a)@ and returns the null count
-- alongside, so the boxed-Bool intermediate is gone.
-- | Encode a primitive nullable column from a 'NullableView'.
-- The validity bitmap is already in the right shape (one
-- bit per row, LSB-first); the values buffer is already
-- dense and storable. So this is one Bit.toByteString call
-- plus one encoder call — no per-element walk required.
primNullable
  :: Storable a
  => (VS.Vector a -> ByteString) -> a
  -> AC.NullableView a -> BuildAcc -> BuildAcc
primNullable enc _zero nv acc =
  let !vals       = AC.nvValues nv
      !n          = fromIntegral (VS.length vals) :: Int64
      !validity   = AC.nvValidity nv
      !validityBs = if VU.null validity
                      then BS.empty   -- "all valid" — Arrow allows omitting the buffer
                      else Bit.toByteString validity
      !nc         = fromIntegral (AC.nvNullCount nv)
  in addBufData (enc vals)
     $ addBufData validityBs
     $ addFieldNode n nc acc

-- | Encode a @V.Vector (Maybe a)@ backed by 'V.Vector' (i.e. not
-- primitive — 'Bool' / 'ByteString' / 'Text'). The 'V.Vector'-side
-- variant can't reuse 'primNullable' because the payload lives in a
-- boxed vector.
primNullableBoxed
  :: (V.Vector a -> ByteString) -> a
  -> V.Vector (Maybe a) -> BuildAcc -> BuildAcc
primNullableBoxed enc zero vec acc =
  let !n  = fromIntegral (V.length vec) :: Int64
      !(validityBs, !nc) = encodeMaybeNullBitmap vec
      vals = V.map (fromMaybe zero) vec
  in addBufData (enc vals)
     $ addBufData validityBs
     $ addFieldNode n (fromIntegral nc) acc

-- | Encode a @V.Vector (Maybe a)@ that serialises as an
-- (offsets, data) pair (Utf8 / Binary / LargeUtf8 / LargeBinary).
varNullableBoxed
  :: (V.Vector a -> (ByteString, ByteString)) -> a
  -> V.Vector (Maybe a) -> BuildAcc -> BuildAcc
varNullableBoxed enc zero vec acc =
  let !n  = fromIntegral (V.length vec) :: Int64
      !(validityBs, !nc) = encodeMaybeNullBitmap vec
      vals = V.map (fromMaybe zero) vec
      (offBs, datBs) = enc vals
  in addBufData datBs
     $ addBufData offBs
     $ addBufData validityBs
     $ addFieldNode n (fromIntegral nc) acc

-- | Pack the LSB-first validity bitmap for a @V.Vector (Maybe a)@
-- and count nulls in the same pass. Allocates a single strict
-- 'ByteString' of @ceil (n / 8)@ bytes.
--
-- Replaces the previous two-pass shape
--
--   validity   = V.map isJust vec        -- N boxed Bools
--   nc         = countNulls vec          -- one more walk
--   bytesOut   = encodeNullBitmap validity   -- pack bits from boxed Bools
--
-- with a single ST loop straight to a pinned strict ByteString.
{-# INLINE encodeMaybeNullBitmap #-}
encodeMaybeNullBitmap :: V.Vector (Maybe a) -> (ByteString, Int)
encodeMaybeNullBitmap v =
  let !n      = V.length v
      !nBytes = (n + 7) `quot` 8
      -- Walk once: pack bits + tally nulls.
      goByte !p !b !nc
        | b >= nBytes = pure nc
        | otherwise = do
            let !base = b * 8
                packBit !bit !accBits !accNulls
                  | bit >= 8        = pure (accBits, accNulls)
                  | base + bit >= n = pure (accBits, accNulls)
                  | otherwise =
                      case V.unsafeIndex v (base + bit) of
                        Just _  -> packBit (bit + 1) (accBits .|. (1 `shiftL` bit)) accNulls
                        Nothing -> packBit (bit + 1) accBits (accNulls + 1)
            (out, nc') <- packBit 0 (0 :: Word8) nc
            pokeByteOff p b out
            goByte p (b + 1) nc'
      -- Run the IO action via unsafeCreate's body and capture
      -- the null count out-of-band by sharing an IORef. Since
      -- BSI.unsafeCreate's callback returns (), thread the
      -- count via a small IORef.
      (resBs, ncOut) = unsafePerformIO $ do
        ncRef <- newIORef (0 :: Int)
        bs <- pure $ BSI.unsafeCreate nBytes $ \p -> do
          nc <- goByte p 0 0
          writeIORef ncRef nc
        -- Force the ByteString contents so 'unsafeCreate's
        -- writer runs and ncRef gets populated.
        BS.length bs `seq` pure ()
        nc <- readIORef ncRef
        pure (bs, nc)
  in (resBs, ncOut)

-- | Count @False@ entries in a validity bitmap.
-- | Count nulls (0-bits) in a validity bitmap.
validityNullCount :: VU.Vector Bit -> Int64
validityNullCount v = fromIntegral (VU.length v - Bit.countOnes v)

-- | Internal helper: pack a boxed @V.Vector Bool@ into the
-- @VU.Vector Bit@ representation the writer's view-builder
-- now expects.
{-# INLINE boxedToBitVec #-}
boxedToBitVec :: V.Vector Bool -> VU.Vector Bit
boxedToBitVec v = VU.generate (V.length v) (\i -> Bit (V.unsafeIndex v i))

-- | Build a nullable variable-length column field node:
-- (validity bitmap, offsets, data buffer, nullCount). All
-- three buffers are already in wire format on the source
-- views, so this is three ForeignPtr handoffs plus one
-- bit-popcount.
{-# INLINE writeNullableVar #-}
writeNullableVar
  :: Storable a
  => VU.Vector Bit -> VS.Vector a -> VS.Vector Word8
  -> BuildAcc -> BuildAcc
writeNullableVar !validity !offsets !dataBuf acc =
  let !validityBs = encodeNullBitmap validity
      !offsetsBs  = vsByteString offsets
      !dataBs     = vsByteString dataBuf
      !n          = fromIntegral (VU.length validity) :: Int64
      !nc         = validityNullCount validity
  in addBufData dataBs
     $ addBufData offsetsBs
     $ addBufData validityBs
     $ addFieldNode n nc acc

-- | Reinterpret a 'VS.Vector' as a strict 'ByteString' /without
-- copying/. Both representations are a 'ForeignPtr' + length
-- pair underneath; we just hand the underlying foreign pointer
-- to BSI.fromForeignPtr.
--
-- Used in the variable-length writer to dump the offsets and
-- data buffers of a 'Utf8View' / 'BinaryView' / large variants
-- straight into the body. With this in place, an Arrow IPC
-- write of a string column is a single ForeignPtr handoff
-- per buffer — no per-element walk, no allocation.
{-# INLINE vsByteString #-}
vsByteString :: forall a. Storable a => VS.Vector a -> ByteString
vsByteString v =
  let !(fp, off, len) = VS.unsafeToForeignPtr v
      !bytesPerElem   = sizeOf (undefined :: a)
      !byteOff        = off * bytesPerElem
      !byteLen        = len * bytesPerElem
  in BSI.fromForeignPtr (castForeignPtr fp) byteOff byteLen

-- * Stream / File writers

arrowMagic :: ByteString
arrowMagic = "ARROW1"

-- | Write a complete Arrow IPC stream (schema + record batches + EOS).
writeArrowStream :: Schema -> V.Vector (V.Vector ColumnArray) -> ByteString
writeArrowStream schema batches =
  let !schemaBs = encodeIPCMessage (SchemaMessage schema)
      batchParts = V.toList $ V.map (buildRecordBatch schema) batches
      eos = BL.toStrict $ B.toLazyByteString $
        B.word32LE 0xFFFFFFFF <> B.int32LE 0
  in BS.concat (schemaBs : batchParts ++ [eos])

-- | Write a complete Arrow IPC file (magic + schema + batches + footer + magic).
writeArrowFile :: Schema -> V.Vector (V.Vector ColumnArray) -> ByteString
writeArrowFile schema batches =
  let !schemaBs = encodeIPCMessage (SchemaMessage schema)
      !paddedSchemaLen = alignUp8 (BS.length schemaBs)
      !schemaPad = paddedSchemaLen - BS.length schemaBs
      !headerSize = 8 + paddedSchemaLen

      batchBss = V.map (buildRecordBatch schema) batches

      (_, revOffsets) = V.foldl' (\(!off, !acc) bbs ->
        (off + fromIntegral (BS.length bbs), off : acc)
        ) (fromIntegral headerSize :: Int64, []) batchBss
      blockOffsets = reverse revOffsets

      footerBs = encodeFooter schema blockOffsets
      !footerLen = BS.length footerBs
  in BS.concat
      [ arrowMagic, BS.pack [0, 0]
      , schemaBs, BS.replicate schemaPad 0
      , BS.concat (V.toList batchBss)
      , footerBs
      , BL.toStrict (B.toLazyByteString (B.int32LE (fromIntegral footerLen)))
      , arrowMagic
      ]

encodeFooter :: Schema -> [Int64] -> ByteString
encodeFooter schema blockOffsets =
  let !schemaBs = encodeIPCMessage (SchemaMessage schema)
  in BL.toStrict $ B.toLazyByteString $
      B.int32LE (fromIntegral (BS.length schemaBs))
      <> B.byteString schemaBs
      <> B.int32LE (fromIntegral (length blockOffsets))
      <> foldl' (\acc off -> acc <> B.int64LE off) mempty blockOffsets
  where
    foldl' _ z [] = z
    foldl' g !z (x:xs) = foldl' g (g z x) xs
