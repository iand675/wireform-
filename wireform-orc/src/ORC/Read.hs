{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

-- | Access ORC file payload using footer metadata.
--
-- Stripe footers decode to a list of physical 'ORC.Stripe.Stream' entries;
-- stream payloads still use ORC encodings (RLE, etc.). Use "ORC.Stripe" to parse
-- the protobuf stripe footer from a raw stripe slice.
--
-- This module also provides column-level decoders that combine stream
-- decompression, RLE decoding, and null-mask interleaving.
module ORC.Read
  ( ORCFile (..)
  , loadORCFile
  , loadORCFilePath
  , openORCReader
  , stripeSlice
  , stripeTotalLength
  , loadStripeFooter
  , stripeColumnStreams
    -- * RLE decoders (re-exported from "ORC.RLE")
  , decodeRLEv1Int
  , decodeRLEv2Int
  , decodeBooleanRLE
  , decodePresentStream
    -- * Stream decompression
  , decompressORCStream
  , decompressORCStreamSized
  , defaultORCCompressionBlockSize
    -- (re-exported from "ORC.Compress" for backward compatibility)
    -- * Column decoders
  , decodeIntColumn
  , decodeIntColumnNV
  , decodeBoolColumn
  , decodeBoolColumnNV
  , decodeStringColumn
  , decodeStringDictColumn
  , decodeFloatColumn
  , decodeFloatColumnNV
  , decodeDoubleColumn
  , decodeDoubleColumnNV
  , decodeTimestampColumn
  , ORCTimestamp (..)
  , decodeDateColumn
  , decodeDecimalColumn
  , decodeDecimal128Stream
  , decodeBinaryColumn
  , decodeShortColumn
  , decodeTinyIntColumn
    -- * End-to-end column reader
  , readColumn
  ) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad.ST (ST, runST)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Primitive.ByteArray as PBA
import qualified Data.Vector.Primitive.Mutable as MVP
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peekByteOff)
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Arrow.Column as AC
import qualified Arrow.View as AV
import Columnar.Bit (Bit (..))
import Foreign.Marshal.Utils (copyBytes)
import Data.Word (Word8, Word32, Word64)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import System.IO.Unsafe (unsafePerformIO)

import qualified Codec.Compression.Zlib.Raw as ZlibRaw
#ifdef HAVE_ZSTD
import Codec.Compression.Zstd (Decompress (..), decompress)
#endif
#ifdef HAVE_SNAPPY
import qualified Codec.Compression.Snappy as Snappy
#endif
#ifdef HAVE_LZ4
import qualified Columnar.LZ4 as LZ4
#endif

import qualified Columnar.IO as IO
import qualified Columnar.Stream as IS
import ORC.Compress
  ( decompressORCStream
  , decompressORCStreamSized
  , defaultORCCompressionBlockSize
  )
import ORC.Footer (readORCCompression, readORCCompressionBlockSize, readORCFooter)
import ORC.RLE
  ( decodeBooleanRLE
  , decodePresentStream
  , decodeRLEv1Int
  , decodeRLEv2Int
  , decodeRLEv2IntAll
  )
import ORC.Stripe (Stream (..), StripeFooter, decodeStripeFooter, stripeFooterBytes, stripeStreamSlices)
import ORC.Types

------------------------------------------------------------------------
-- ORCFile
------------------------------------------------------------------------

-- | ORC file bytes paired with parsed footer and compression metadata.
data ORCFile = ORCFile
  { ofBytes       :: !ByteString
  , ofFooter      :: !ORCFooter
  , ofCompression :: !CompressionKind
  , ofCompressionBlockSize :: !Int
    -- ^ Maximum uncompressed length of any one chunk
    -- (ORC PostScript field 4). Decompressors that need to
    -- size their output buffer (LZ4, LZO) use this. Defaults
    -- to 'defaultORCCompressionBlockSize' (256 KiB) for files
    -- whose PostScript omits the field.
  } deriving stock (Show, Eq)

-- | Read postscript, footer, and parse protobuf footer metadata.
loadORCFile :: ByteString -> Either String ORCFile
loadORCFile bs = do
  ft  <- readORCFooter bs
  ck  <- readORCCompression bs
  blk <- readORCCompressionBlockSize bs
  let !blk' = if blk == 0 then defaultORCCompressionBlockSize else blk
  Right ORCFile
    { ofBytes = bs
    , ofFooter = ft
    , ofCompression = ck
    , ofCompressionBlockSize = blk'
    }

-- | Read an ORC file from disk and parse its footer.
--
-- Uses 'Columnar.IO.loadFile' under the hood, which mmaps
-- files above 64 KiB and reads smaller files eagerly. Per-
-- stripe slices into the resulting 'ByteString' are pointer
-- arithmetic, so opening a multi-GB file costs only the
-- footer's worth of page-ins.
loadORCFilePath :: FilePath -> IO (Either String ORCFile)
loadORCFilePath path = do
  bs <- IO.loadFile path
  pure (loadORCFile bs)

-- | Open an ORC file as an 'IS.IterIO' over its stripe
-- indices. Each step yields one stripe index on demand;
-- callers join with 'ORC.Arrow.orcStripeToArrow' (or
-- 'stripeColumnStreams') to materialise stripe data.
openORCReader
  :: FilePath
  -> IO (Either String (ORCFile, IS.IterIO Int))
openORCReader path = do
  loaded <- loadORCFilePath path
  case loaded of
    Left e -> pure (Left e)
    Right ofile ->
      let !nStripes = V.length (orcStripes (ofFooter ofile))
          mkIter k = IS.IterIO $ pure $
            if k >= nStripes
              then Right IS.IterIODone
              else Right (IS.IterIOYield k (mkIter (k + 1)))
      in pure (Right (ofile, mkIter 0))

------------------------------------------------------------------------
-- Stripe access
------------------------------------------------------------------------

-- | Combined length of index, data, and footer sections for a stripe.
stripeTotalLength :: StripeInformation -> Word64
stripeTotalLength si =
  siIndexLength si + siDataLength si + siFooterLength si

-- | Raw bytes covering one stripe (@index + data + stripe footer@).
stripeSlice :: ORCFile -> Int -> Either String ByteString
stripeSlice ofile idx = do
  let ft = ofFooter ofile
      ss = orcStripes ft
  if idx < 0 || idx >= V.length ss
    then Left "ORC.Read: stripe index out of range"
    else do
      let si = V.unsafeIndex ss idx
          !off = fromIntegral (siOffset si) :: Int
          !len = fromIntegral (stripeTotalLength si) :: Int
          !bs = ofBytes ofile
      if off < 0 || len < 0 || off + len > BS.length bs
        then Left "ORC.Read: stripe slice out of bounds"
        else Right $! BS.take len (BS.drop off bs)

-- | Parse the protobuf stripe footer for a stripe index. ORC
-- wraps stripe footers in the file's compression envelope (same
-- as data streams + the file footer), so we decompress here
-- before handing bytes to 'decodeStripeFooter'.
loadStripeFooter :: ORCFile -> Int -> Either String StripeFooter
loadStripeFooter ofile idx = do
  stripe <- stripeSlice ofile idx
  let ft = ofFooter ofile
      ss = orcStripes ft
  if idx < 0 || idx >= V.length ss
    then Left "ORC.Read: stripe index out of range"
    else do
      let si = V.unsafeIndex ss idx
      rawFb <- stripeFooterBytes stripe si
      fb    <- decompressORCStreamSized
                 (ofCompressionBlockSize ofile)
                 (ofCompression ofile)
                 rawFb
      decodeStripeFooter fb

-- | Physical stream payloads for one stripe (footer order), using lengths from
-- the stripe footer. The input blob is the full stripe (@index + data + footer@).
stripeColumnStreams :: ORCFile -> Int -> Either String (V.Vector (Stream, ByteString))
stripeColumnStreams ofile idx = do
  bs <- stripeSlice ofile idx
  sf <- loadStripeFooter ofile idx
  stripeStreamSlices bs sf

------------------------------------------------------------------------
-- Stream decompression
------------------------------------------------------------------------
--
-- All ORC stream-decompression logic lives in "ORC.Compress"
-- so 'ORC.Footer' (which needs to decompress the footer) and
-- 'ORC.Read' (which needs to decompress column streams) can
-- share the same implementation without forming a cycle. The
-- 'decompressORCStream' / 'decompressORCStreamSized' /
-- 'defaultORCCompressionBlockSize' names are re-exported from
-- this module's export list so callers that already import
-- them from ORC.Read keep working.

------------------------------------------------------------------------
-- Column decoders
------------------------------------------------------------------------

-- | Decode an integer column (signed or unsigned) with optional null mask.
decodeIntColumn
  :: Bool -> Int -> ByteString -> Maybe ByteString
  -> Either String (V.Vector (Maybe Int64))
decodeIntColumn signed numRows dataBs mPresentBs = case mPresentBs of
  Nothing -> do
    vals <- decodeRLEv2Int signed numRows dataBs
    Right $! V.generate (VP.length vals) (\i -> Just (VP.unsafeIndex vals i))
  Just presentBs -> do
    present <- decodePresentStream numRows presentBs
    let !numPresent = countTrue present
    vals <- decodeRLEv2Int signed numPresent dataBs
    Right $! interleaveInt present vals

-- | Flat-shape variant of 'decodeIntColumn': returns a
-- @(VU.Vector Bit, VS.Vector Int64)@ pair already in the
-- bridge's 'AC.NullableView' shape. Skips the
-- @V.Vector (Maybe Int64)@ intermediate that 'decodeIntColumn'
-- builds (one Just/Nothing constructor per row + N pointer
-- slots in the boxed backing array, all immediately garbage
-- after the bridge converts to NullableView).
--
-- For the no-PRESENT case (no nulls), the validity bitmap
-- is left empty -- 'nvIsPresent' returns True for every
-- index when the bitmap is empty, matching Arrow's
-- "@null_count == 0@ producers may omit the validity buffer"
-- convention.
decodeIntColumnNV
  :: Bool -> Int -> ByteString -> Maybe ByteString
  -> Either String (AC.NullableView Int64)
decodeIntColumnNV signed numRows dataBs mPresentBs = case mPresentBs of
  Nothing -> do
    vals <- decodeRLEv2Int signed numRows dataBs
    -- vals :: VP.Vector Int64 of length numRows; convert to
    -- VS.Vector once. (VS.convert is one linear copy; the
    -- bridge's previous code did the same work + a separate
    -- N-pointer V.Vector wrap + a third walk in
    -- 'nvFromMaybeVector'.)
    Right $! AC.NullableView VU.empty (VS.convert vals)
  Just presentBs -> do
    present <- decodePresentStream numRows presentBs
    let !numPresent = countTrue present
    vals <- decodeRLEv2Int signed numPresent dataBs
    Right $! interleaveIntNV present vals
{-# INLINE decodeIntColumnNV #-}

interleaveIntNV
  :: V.Vector Bool -> VP.Vector Int64 -> AC.NullableView Int64
interleaveIntNV present vals = runST $ do
  let !n = V.length present
  bitM <- VUM.unsafeNew n
  valM <- VSM.unsafeNew n
  let go !i !j
        | i >= n = pure ()
        | V.unsafeIndex present i = do
            VUM.unsafeWrite bitM i (Bit True)
            VSM.unsafeWrite valM i (VP.unsafeIndex vals j)
            go (i + 1) (j + 1)
        | otherwise = do
            VUM.unsafeWrite bitM i (Bit False)
            VSM.unsafeWrite valM i 0
            go (i + 1) j
  go 0 0
  !validity <- VU.unsafeFreeze bitM
  !values   <- VS.unsafeFreeze valM
  pure (AC.NullableView validity values)
{-# INLINE interleaveIntNV #-}

-- | Decode a boolean column with optional null mask.
decodeBoolColumn
  :: Int -> ByteString -> Maybe ByteString
  -> Either String (V.Vector (Maybe Bool))
decodeBoolColumn numRows dataBs mPresentBs = case mPresentBs of
  Nothing -> do
    vals <- decodeBooleanRLE numRows dataBs
    Right $! V.map Just vals
  Just presentBs -> do
    present <- decodePresentStream numRows presentBs
    let !numPresent = countTrue present
    vals <- decodeBooleanRLE numPresent dataBs
    Right $! interleaveBool present vals

-- | Flat-shape Bool reader. Returns 'AV.NullableBoolView'
-- (validity bits + value bits both bit-packed) directly.
decodeBoolColumnNV
  :: Int -> ByteString -> Maybe ByteString
  -> Either String AV.NullableBoolView
decodeBoolColumnNV numRows dataBs mPresentBs = case mPresentBs of
  Nothing -> do
    vals <- decodeBooleanRLE numRows dataBs
    -- vals :: V.Vector Bool. Pack into VU.Vector Bit; leave
    -- validity empty (= all valid).
    let !packed = VU.generate (V.length vals)
                    (\i -> Bit (V.unsafeIndex vals i))
    Right $! AV.NullableBoolView VU.empty packed
  Just presentBs -> do
    present <- decodePresentStream numRows presentBs
    let !numPresent = countTrue present
    vals <- decodeBooleanRLE numPresent dataBs
    Right $! interleaveBoolNV present vals
{-# INLINE decodeBoolColumnNV #-}

interleaveBoolNV
  :: V.Vector Bool -> V.Vector Bool -> AV.NullableBoolView
interleaveBoolNV present vals = runST $ do
  let !n = V.length present
  bitM <- VUM.unsafeNew n
  valM <- VUM.unsafeNew n
  let go !i !j
        | i >= n = pure ()
        | V.unsafeIndex present i = do
            VUM.unsafeWrite bitM i (Bit True)
            VUM.unsafeWrite valM i (Bit (V.unsafeIndex vals j))
            go (i + 1) (j + 1)
        | otherwise = do
            VUM.unsafeWrite bitM i (Bit False)
            VUM.unsafeWrite valM i (Bit False)
            go (i + 1) j
  go 0 0
  !validity <- VU.unsafeFreeze bitM
  !values   <- VU.unsafeFreeze valM
  pure (AV.NullableBoolView validity values)
{-# INLINE interleaveBoolNV #-}

-- | Decode a string column, auto-dispatching between DIRECT_V2 and
-- DICTIONARY_V2 encodings based on whether the dictionary data stream
-- is present.
--
-- Arguments: @numRows@, @data@, @length stream@ (RLE v2 unsigned),
-- @dictionary stream@, @present stream@. For a DIRECT_V2 column the
-- dictionary stream is empty and @data@ carries the UTF-8 bytes. For a
-- DICTIONARY_V2 column the dictionary stream carries the unique UTF-8
-- entries and @data@ carries the per-row dictionary indices.
decodeStringColumn
  :: Int -> ByteString -> ByteString -> ByteString -> Maybe ByteString
  -> Either String (V.Vector (Maybe T.Text))
decodeStringColumn numRows dataBs lengthBs dictBs mPresentBs
  -- DICTIONARY_V2: the data stream is the per-row index stream, the
  -- length stream is the dictionary entry lengths, dictBs is the raw
  -- UTF-8 dictionary payload. 'decodeStringDictColumn' takes arguments
  -- in the order (dictData, length, index, present) so we reshuffle
  -- the DIRECT-shaped arguments accordingly.
  | not (BS.null dictBs) =
      decodeStringDictColumn numRows dictBs lengthBs dataBs mPresentBs
  | otherwise = do
      (numPresent, mPresent) <- resolvePresent numRows mPresentBs
      lengths <- decodeRLEv2Int False numPresent lengthBs
      strings <- splitByLengths dataBs lengths
      case mPresent of
        Nothing -> Right $! V.map Just strings
        Just present -> Right $! interleaveText present strings

-- | Decode an IEEE 754 single-precision float column (little-endian).
--
-- No-present (no nulls) fast path: bulk-decode all bytes
-- straight into a primitive 'VP.Vector' Float via 'memcpy',
-- then wrap in 'Just'. Replaces the per-element
-- @V.generate (Just . readFloatLE)@ shape which was reading
-- bytes one at a time.
decodeFloatColumn
  :: Int -> ByteString -> Maybe ByteString
  -> Either String (V.Vector (Maybe Float))
decodeFloatColumn numRows dataBs mPresentBs = case mPresentBs of
  Nothing
    | BS.length dataBs < numRows * 4 ->
        Left "ORC.Read: float data stream too short"
    | otherwise ->
        let !raw = memcpyPrimVec 4 numRows dataBs :: VP.Vector Float
        in  Right $! V.generate numRows
              (\i -> Just (VP.unsafeIndex raw i))
  Just presentBs -> do
    present <- decodePresentStream numRows presentBs
    let !numPresent = countTrue present
    if BS.length dataBs < numPresent * 4
      then Left "ORC.Read: float data stream too short"
      else Right $! interleaveFloat present dataBs

-- | Flat-shape Float reader. See 'decodeIntColumnNV'.
decodeFloatColumnNV
  :: Int -> ByteString -> Maybe ByteString
  -> Either String (AC.NullableView Float)
decodeFloatColumnNV numRows dataBs mPresentBs = case mPresentBs of
  Nothing
    | BS.length dataBs < numRows * 4 ->
        Left "ORC.Read: float data stream too short"
    | otherwise ->
        Right $! AC.NullableView VU.empty
          (memcpyPrimVecVS 4 numRows dataBs)
  Just presentBs -> do
    present <- decodePresentStream numRows presentBs
    let !numPresent = countTrue present
    if BS.length dataBs < numPresent * 4
      then Left "ORC.Read: float data stream too short"
      else Right $! interleaveFloatNV present dataBs
{-# INLINE decodeFloatColumnNV #-}

interleaveFloatNV
  :: V.Vector Bool -> ByteString -> AC.NullableView Float
interleaveFloatNV present dataBs = runST $ do
  let !n = V.length present
  bitM <- VUM.unsafeNew n
  valM <- VSM.unsafeNew n
  let go !i !j
        | i >= n = pure ()
        | V.unsafeIndex present i = do
            VUM.unsafeWrite bitM i (Bit True)
            VSM.unsafeWrite valM i (readFloatLE dataBs (j * 4))
            go (i + 1) (j + 1)
        | otherwise = do
            VUM.unsafeWrite bitM i (Bit False)
            VSM.unsafeWrite valM i 0
            go (i + 1) j
  go 0 0
  !validity <- VU.unsafeFreeze bitM
  !values   <- VS.unsafeFreeze valM
  pure (AC.NullableView validity values)
{-# INLINE interleaveFloatNV #-}

-- | Decode an IEEE 754 double-precision float column (little-endian).
decodeDoubleColumn
  :: Int -> ByteString -> Maybe ByteString
  -> Either String (V.Vector (Maybe Double))
decodeDoubleColumn numRows dataBs mPresentBs = case mPresentBs of
  Nothing
    | BS.length dataBs < numRows * 8 ->
        Left "ORC.Read: double data stream too short"
    | otherwise ->
        let !raw = memcpyPrimVec 8 numRows dataBs :: VP.Vector Double
        in  Right $! V.generate numRows
              (\i -> Just (VP.unsafeIndex raw i))
  Just presentBs -> do
    present <- decodePresentStream numRows presentBs
    let !numPresent = countTrue present
    if BS.length dataBs < numPresent * 8
      then Left "ORC.Read: double data stream too short"
      else Right $! interleaveDouble present dataBs

-- | Flat-shape Double reader. See 'decodeIntColumnNV'.
decodeDoubleColumnNV
  :: Int -> ByteString -> Maybe ByteString
  -> Either String (AC.NullableView Double)
decodeDoubleColumnNV numRows dataBs mPresentBs = case mPresentBs of
  Nothing
    | BS.length dataBs < numRows * 8 ->
        Left "ORC.Read: double data stream too short"
    | otherwise ->
        Right $! AC.NullableView VU.empty
          (memcpyPrimVecVS 8 numRows dataBs)
  Just presentBs -> do
    present <- decodePresentStream numRows presentBs
    let !numPresent = countTrue present
    if BS.length dataBs < numPresent * 8
      then Left "ORC.Read: double data stream too short"
      else Right $! interleaveDoubleNV present dataBs
{-# INLINE decodeDoubleColumnNV #-}

interleaveDoubleNV
  :: V.Vector Bool -> ByteString -> AC.NullableView Double
interleaveDoubleNV present dataBs = runST $ do
  let !n = V.length present
  bitM <- VUM.unsafeNew n
  valM <- VSM.unsafeNew n
  let go !i !j
        | i >= n = pure ()
        | V.unsafeIndex present i = do
            VUM.unsafeWrite bitM i (Bit True)
            VSM.unsafeWrite valM i (readDoubleLE dataBs (j * 8))
            go (i + 1) (j + 1)
        | otherwise = do
            VUM.unsafeWrite bitM i (Bit False)
            VSM.unsafeWrite valM i 0
            go (i + 1) j
  go 0 0
  !validity <- VU.unsafeFreeze bitM
  !values   <- VS.unsafeFreeze valM
  pure (AC.NullableView validity values)
{-# INLINE interleaveDoubleNV #-}

-- | Memcpy a slice of @bs@ into a freshly-allocated primitive
-- vector. Same shape as Arrow.Column's helper of the same
-- name; assumes the source bit pattern matches the host
-- (LE on x86_64 / aarch64), which matches ORC's IEEE-LE wire
-- format.
{-# INLINE memcpyPrimVec #-}
memcpyPrimVec :: forall a. VP.Prim a => Int -> Int -> ByteString -> VP.Vector a
memcpyPrimVec !elemBytes !len bs
  | len <= 0  = VP.empty
  | otherwise = unsafePerformIO $
      BSU.unsafeUseAsCStringLen bs $ \(cstr, _) -> do
        mv <- MVP.unsafeNew len
        let !srcPtr = castPtr cstr
            !nBytes = len * elemBytes
        case mv of
          MVP.MVector dstOffElems _ dstMba ->
            PBA.copyPtrToMutableByteArray
              dstMba (dstOffElems * elemBytes) (srcPtr :: Ptr Word8) nBytes
        VP.unsafeFreeze mv

-- | Storable-vector variant: 'memcpy' from the source
-- bytestring straight into a fresh 'ForeignPtr'-backed
-- 'VS.Vector'. One copy. Used by the flat-shape 'NullableView'
-- decoders so the per-page output buffer is already in the
-- bridge's target shape with no second 'VS.convert' walk.
{-# INLINE memcpyPrimVecVS #-}
memcpyPrimVecVS :: forall a. VS.Storable a => Int -> Int -> ByteString -> VS.Vector a
memcpyPrimVecVS !elemBytes !len bs
  | len <= 0  = VS.empty
  | otherwise = unsafePerformIO $
      BSU.unsafeUseAsCStringLen bs $ \(cstr, _) -> do
        mv <- VSM.unsafeNew len :: IO (VSM.IOVector a)
        let !srcPtr = castPtr cstr :: Ptr Word8
            !nBytes = len * elemBytes
        VSM.unsafeWith mv $ \dstPtr ->
          copyBytes (castPtr dstPtr) srcPtr nBytes
        VS.unsafeFreeze mv

-- | ORC timestamp: seconds since the ORC epoch + nanosecond adjustment.
data ORCTimestamp = ORCTimestamp
  { otsSeconds :: {-# UNPACK #-} !Int64
  , otsNanos   :: {-# UNPACK #-} !Int64
  } deriving stock (Show, Eq)

-- | Decode a timestamp column (DATA = signed seconds, SECONDARY = unsigned nanos).
--
-- The nanosecond encoding packs a scale in the top 3 bits and the
-- fractional value in the remaining bits.
decodeTimestampColumn
  :: Int -> ByteString -> ByteString -> Maybe ByteString
  -> Either String (V.Vector (Maybe ORCTimestamp))
decodeTimestampColumn numRows secBs nanoBs mPresentBs = do
  (numPresent, mPresent) <- resolvePresent numRows mPresentBs
  secs  <- decodeRLEv2Int True  numPresent secBs
  nanos <- decodeRLEv2Int False numPresent nanoBs
  let !n = VP.length secs
  if VP.length nanos /= n
    then Left "ORC.Read: timestamp seconds/nanos length mismatch"
    else do
      let timestamps = V.generate n $ \i ->
            let !s = VP.unsafeIndex secs i
                !rawNano = VP.unsafeIndex nanos i
                !nanoVal = decodeORCNano rawNano
            in ORCTimestamp s nanoVal
      case mPresent of
        Nothing -> Right $! V.map Just timestamps
        Just present -> Right $! interleaveWith present timestamps

-- | Decode the ORC nanosecond encoding: top 3 bits = trailing-zero scale,
-- lower bits = the nano value before scaling.
{-# INLINE decodeORCNano #-}
decodeORCNano :: Int64 -> Int64
decodeORCNano !raw =
  let !encoded = fromIntegral raw :: Word64
      -- Bottom 3 bits = trailing-zero scale, upper bits = nano value.
      -- nanos = (raw >>> 3) * 10^(bottom 3 bits)
      !trailingZeros = fromIntegral (encoded .&. 0x7) :: Int
      !nanoBase = fromIntegral (encoded `shiftR` 3) :: Int64
  in nanoBase * pow10 trailingZeros

{-# INLINE pow10 #-}
pow10 :: Int -> Int64
pow10 !n = case n of
  0 -> 1; 1 -> 10; 2 -> 100; 3 -> 1000; 4 -> 10000
  5 -> 100000; 6 -> 1000000; 7 -> 10000000; 8 -> 100000000
  _ -> 1000000000

-- | Decode a date column (signed days since 1970-01-01).
decodeDateColumn
  :: Int -> ByteString -> Maybe ByteString
  -> Either String (V.Vector (Maybe Int32))
decodeDateColumn numRows dataBs mPresentBs = do
  (numPresent, mPresent) <- resolvePresent numRows mPresentBs
  vals <- decodeRLEv2Int True numPresent dataBs
  let dates = V.generate (VP.length vals) $ \i ->
        fromIntegral (VP.unsafeIndex vals i) :: Int32
  case mPresent of
    Nothing -> Right $! V.map Just dates
    Just present -> Right $! interleaveWith present dates

-- | Decode a DECIMAL64 column (precision <= 18).
--
-- @numRows@, @scale@, DATA stream, optional PRESENT stream.
-- Returns unscaled Int64 values; divide by @10^scale@ for the actual decimal.
decodeDecimalColumn
  :: Int -> Int -> ByteString -> Maybe ByteString
  -> Either String (V.Vector (Maybe Int64))
decodeDecimalColumn numRows _scale dataBs mPresentBs = do
  (numPresent, mPresent) <- resolvePresent numRows mPresentBs
  vals <- decodeRLEv2Int True numPresent dataBs
  let ints = V.generate (VP.length vals) $ \i -> VP.unsafeIndex vals i
  case mPresent of
    Nothing -> Right $! V.map Just ints
    Just present -> Right $! interleaveWith present ints

-- | Decode the @DATA@ stream of a DECIMAL128 column - a sequence of
-- LEB128 zig-zag signed varints, one per row group entry. Pair this
-- with the column's RLE-v2 @SECONDARY@ stream (the per-row scale) and
-- optional @PRESENT@ stream to materialise full decimal values.
--
-- Inverse of 'ORC.Write.encodeDecimalRawColumn' for the data half.
decodeDecimal128Stream
  :: Int        -- ^ expected number of present values
  -> ByteString -- ^ DATA stream bytes
  -> Either String (V.Vector Integer)
decodeDecimal128Stream n bs
  | n <= 0    = if BS.null bs
                  then Right V.empty
                  else Left "ORC.Read.decodeDecimal128Stream: trailing bytes"
  | otherwise = runST $ do
      mv <- MV.unsafeNew n
      let !len = BS.length bs
          go !i !off
            | i >= n =
                if off /= len
                  then pure (Left "ORC.Read.decodeDecimal128Stream: trailing bytes")
                  else pure (Right ())
            | otherwise =
                case readVarSigned bs off of
                  Left err     -> pure (Left err)
                  Right (v, off') -> do
                    MV.unsafeWrite mv i v
                    go (i + 1) off'
      r <- go 0 0
      case r of
        Left err -> pure (Left err)
        Right () -> Right <$> V.unsafeFreeze mv

readVarSigned :: ByteString -> Int -> Either String (Integer, Int)
readVarSigned bs off0 = do
  (u, off') <- readVarUnsigned bs off0
  -- zig-zag decode
  let !v = if u `mod` 2 == 0 then u `div` 2 else negate (u `div` 2 + 1)
  Right (v, off')

readVarUnsigned :: ByteString -> Int -> Either String (Integer, Int)
readVarUnsigned bs = go 0 0
  where
    !len = BS.length bs
    go !shift !acc !off
      | off >= len = Left "ORC.Read.readVarUnsigned: truncated varint"
      | otherwise =
          let !b = BS.index bs off
              !chunk = fromIntegral (b .&. 0x7F) :: Integer
              !acc' = acc .|. (chunk `shiftL` shift)
           in if b .&. 0x80 == 0
                then Right (acc', off + 1)
                else go (shift + 7) acc' (off + 1)

-- | Decode a DICTIONARY_V2-encoded string column.
--
-- @numRows@, dictionary data bytes, length stream, index stream, present stream.
decodeStringDictColumn
  :: Int -> ByteString -> ByteString -> ByteString -> Maybe ByteString
  -> Either String (V.Vector (Maybe T.Text))
decodeStringDictColumn numRows dictDataBs lengthBs indexBs mPresentBs = do
  -- Decode the dictionary. The dictionary length stream has one entry
  -- per unique string and the count is not recorded anywhere else in
  -- the column metadata, so we consume the stream to EOF.
  dictLengths <- decodeRLEv2IntAll False lengthBs
  dictEntries <- splitByLengths dictDataBs dictLengths
  -- Decode indices
  (numPresent, mPresent) <- resolvePresent numRows mPresentBs
  indices <- decodeRLEv2Int False numPresent indexBs
  let !dictSize = V.length dictEntries
  strings <- V.generateM (VP.length indices) $ \i -> do
    let !idx = fromIntegral (VP.unsafeIndex indices i) :: Int
    if idx < 0 || idx >= dictSize
      then Left $ "ORC.Read: dictionary index " ++ show idx ++ " out of range"
      else Right (V.unsafeIndex dictEntries idx)
  case mPresent of
    Nothing -> Right $! V.map Just strings
    Just present -> Right $! interleaveText present strings

-- | Decode a binary/bytes column: DATA (raw bytes) + LENGTH (RLE v2 unsigned).
decodeBinaryColumn
  :: Int -> ByteString -> ByteString -> Maybe ByteString
  -> Either String (V.Vector (Maybe ByteString))
decodeBinaryColumn numRows dataBs lengthBs mPresentBs = do
  (numPresent, mPresent) <- resolvePresent numRows mPresentBs
  lengths <- decodeRLEv2Int False numPresent lengthBs
  blobs <- splitByLengthsRaw dataBs lengths
  case mPresent of
    Nothing -> Right $! V.map Just blobs
    Just present -> Right $! interleaveWith present blobs

-- | Decode a SHORT (Int16) column: DATA stream is RLE v2 signed.
decodeShortColumn
  :: Int -> ByteString -> Maybe ByteString
  -> Either String (V.Vector (Maybe Int16))
decodeShortColumn numRows dataBs mPresentBs = do
  (numPresent, mPresent) <- resolvePresent numRows mPresentBs
  vals <- decodeRLEv2Int True numPresent dataBs
  let shorts = V.generate (VP.length vals) $ \i ->
        fromIntegral (VP.unsafeIndex vals i) :: Int16
  case mPresent of
    Nothing -> Right $! V.map Just shorts
    Just present -> Right $! interleaveWith present shorts

-- | Decode a TINYINT (Int8) column: DATA stream is raw bytes (one per value).
decodeTinyIntColumn
  :: Int -> ByteString -> Maybe ByteString
  -> Either String (V.Vector (Maybe Int8))
decodeTinyIntColumn numRows dataBs mPresentBs = do
  (numPresent, mPresent) <- resolvePresent numRows mPresentBs
  if BS.length dataBs < numPresent
    then Left "ORC.Read: tinyint data stream too short"
    else do
      -- Use unsafeIndex now that we've bounds-checked the slice.
      let bytes = V.generate numPresent $ \i ->
            fromIntegral (BSU.unsafeIndex dataBs i) :: Int8
      case mPresent of
        Nothing -> Right $! V.map Just bytes
        Just present -> Right $! interleaveWith present bytes

------------------------------------------------------------------------
-- End-to-end column reader
------------------------------------------------------------------------

-- ORC stream kind constants
skPresent, skData :: Word64
skPresent = 0
skData    = 1

-- | Read and decode a full integer column from a stripe.
--
-- Arguments: file, stripe index, column index, expected 'TypeKind' as 'Int'
-- (use 'typeKindToInt'). Returns an error if the column type doesn't match.
readColumn :: ORCFile -> Int -> Int -> Int -> Either String (V.Vector (Maybe Int64))
readColumn ofile stripeIdx colIdx expectedKind = do
  let !types = orcTypes (ofFooter ofile)
  if colIdx < 0 || colIdx >= V.length types
    then Left "ORC.Read: column index out of range"
    else do
      let !colType    = V.unsafeIndex types colIdx
          !actualKind = typeKindToInt (otKind colType)
      if actualKind /= expectedKind
        then Left $ "ORC.Read: expected type kind " ++ show expectedKind
                  ++ " but column has kind " ++ show actualKind
        else do
          streams <- stripeColumnStreams ofile stripeIdx
          let !col64       = fromIntegral colIdx :: Word64
              !mDataBs     = findStreamPayload streams col64 skData
              !mPresentBs  = findStreamPayload streams col64 skPresent
              !comp        = ofCompression ofile
              !blk         = ofCompressionBlockSize ofile
          case mDataBs of
            Nothing -> Left "ORC.Read: no DATA stream for column"
            Just rawData -> do
              dataBs <- decompressORCStreamSized blk comp rawData
              mPresent <- case mPresentBs of
                Nothing -> Right Nothing
                Just rp -> Just <$> decompressORCStreamSized blk comp rp
              let !stripes = orcStripes (ofFooter ofile)
                  !nRows   = fromIntegral (siNumberOfRows (V.unsafeIndex stripes stripeIdx)) :: Int
              decodeIntColumn True nRows dataBs mPresent

findStreamPayload :: V.Vector (Stream, ByteString) -> Word64 -> Word64 -> Maybe ByteString
findStreamPayload streams colIdx kindIdx =
  case V.find (\(s, _) -> stColumn s == colIdx && stKind s == kindIdx) streams of
    Just (_, bs) -> Just bs
    Nothing      -> Nothing

------------------------------------------------------------------------
-- Interleaving helpers
------------------------------------------------------------------------

countTrue :: V.Vector Bool -> Int
countTrue = V.foldl' (\a b -> if b then a + 1 else a) 0
{-# INLINE countTrue #-}

interleaveInt :: V.Vector Bool -> VP.Vector Int64 -> V.Vector (Maybe Int64)
interleaveInt present vals = runST $ do
  let !n = V.length present
  out <- MV.unsafeNew n
  let go !i !j
        | i >= n = pure ()
        | V.unsafeIndex present i = do
            MV.unsafeWrite out i (Just (VP.unsafeIndex vals j))
            go (i + 1) (j + 1)
        | otherwise = do
            MV.unsafeWrite out i Nothing
            go (i + 1) j
  go 0 0
  V.unsafeFreeze out

interleaveBool :: V.Vector Bool -> V.Vector Bool -> V.Vector (Maybe Bool)
interleaveBool present vals = runST $ do
  let !n = V.length present
  out <- MV.unsafeNew n
  let go !i !j
        | i >= n = pure ()
        | V.unsafeIndex present i = do
            MV.unsafeWrite out i (Just (V.unsafeIndex vals j))
            go (i + 1) (j + 1)
        | otherwise = do
            MV.unsafeWrite out i Nothing
            go (i + 1) j
  go 0 0
  V.unsafeFreeze out

interleaveText :: V.Vector Bool -> V.Vector T.Text -> V.Vector (Maybe T.Text)
interleaveText present vals = runST $ do
  let !n = V.length present
  out <- MV.unsafeNew n
  let go !i !j
        | i >= n = pure ()
        | V.unsafeIndex present i = do
            MV.unsafeWrite out i (Just (V.unsafeIndex vals j))
            go (i + 1) (j + 1)
        | otherwise = do
            MV.unsafeWrite out i Nothing
            go (i + 1) j
  go 0 0
  V.unsafeFreeze out

interleaveFloat :: V.Vector Bool -> ByteString -> V.Vector (Maybe Float)
interleaveFloat present dataBs = runST $ do
  let !n = V.length present
  out <- MV.unsafeNew n
  let go !i !j
        | i >= n = pure ()
        | V.unsafeIndex present i = do
            MV.unsafeWrite out i (Just (readFloatLE dataBs (j * 4)))
            go (i + 1) (j + 1)
        | otherwise = do
            MV.unsafeWrite out i Nothing
            go (i + 1) j
  go 0 0
  V.unsafeFreeze out

interleaveDouble :: V.Vector Bool -> ByteString -> V.Vector (Maybe Double)
interleaveDouble present dataBs = runST $ do
  let !n = V.length present
  out <- MV.unsafeNew n
  let go !i !j
        | i >= n = pure ()
        | V.unsafeIndex present i = do
            MV.unsafeWrite out i (Just (readDoubleLE dataBs (j * 8)))
            go (i + 1) (j + 1)
        | otherwise = do
            MV.unsafeWrite out i Nothing
            go (i + 1) j
  go 0 0
  V.unsafeFreeze out

------------------------------------------------------------------------
-- IEEE 754 little-endian readers
------------------------------------------------------------------------

-- | LE Float read via a single 4-byte 'peekByteOff'. Replaces
-- the byte-by-byte BS.index + shifts shape; matches the
-- pattern used in Parquet.Read.
{-# INLINE readFloatLE #-}
readFloatLE :: ByteString -> Int -> Float
readFloatLE bs !off = castWord32ToFloat $! unsafePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(cstr, _) ->
    peekByteOff (cstr `plusPtr` off) 0

{-# INLINE readDoubleLE #-}
readDoubleLE :: ByteString -> Int -> Double
readDoubleLE bs !off = castWord64ToDouble $! unsafePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(cstr, _) ->
    peekByteOff (cstr `plusPtr` off) 0

------------------------------------------------------------------------
-- String helpers
------------------------------------------------------------------------

-- | Resolve a present stream into (numPresent, Maybe presentVector).
resolvePresent :: Int -> Maybe ByteString -> Either String (Int, Maybe (V.Vector Bool))
resolvePresent numRows Nothing = Right (numRows, Nothing)
resolvePresent numRows (Just pbs) = do
  p <- decodePresentStream numRows pbs
  Right (countTrue p, Just p)

-- | Generic interleave for any boxed type.
interleaveWith :: V.Vector Bool -> V.Vector a -> V.Vector (Maybe a)
interleaveWith present vals = runST $ do
  let !n = V.length present
  out <- MV.unsafeNew n
  let go !i !j
        | i >= n = pure ()
        | V.unsafeIndex present i = do
            MV.unsafeWrite out i (Just (V.unsafeIndex vals j))
            go (i + 1) (j + 1)
        | otherwise = do
            MV.unsafeWrite out i Nothing
            go (i + 1) j
  go 0 0
  V.unsafeFreeze out

splitByLengths :: ByteString -> VP.Vector Int64 -> Either String (V.Vector T.Text)
splitByLengths dataBs lengths = runST $ do
  let !n = VP.length lengths
  out <- MV.unsafeNew n
  result <- go out 0 0
  case result of
    Left e   -> return (Left e)
    Right () -> Right <$> V.unsafeFreeze out
  where
    go :: MV.MVector s T.Text -> Int -> Int -> ST s (Either String ())
    go out !i !off
      | i >= VP.length lengths = return (Right ())
      | otherwise = do
          let !len = fromIntegral (VP.unsafeIndex lengths i) :: Int
          if off + len > BS.length dataBs
            then return (Left "ORC.Read: string data underflow")
            else case TE.decodeUtf8' (BS.take len (BS.drop off dataBs)) of
              Left _  -> return (Left "ORC.Read: invalid UTF-8 in string column")
              Right t -> do
                MV.unsafeWrite out i t
                go out (i + 1) (off + len)

-- | Like 'splitByLengths' but returns raw ByteStrings.
splitByLengthsRaw :: ByteString -> VP.Vector Int64 -> Either String (V.Vector ByteString)
splitByLengthsRaw dataBs lengths = runST $ do
  let !n = VP.length lengths
  out <- MV.unsafeNew n
  result <- go out 0 0
  case result of
    Left e   -> return (Left e)
    Right () -> Right <$> V.unsafeFreeze out
  where
    go :: MV.MVector s ByteString -> Int -> Int -> ST s (Either String ())
    go out !i !off
      | i >= VP.length lengths = return (Right ())
      | otherwise = do
          let !len = fromIntegral (VP.unsafeIndex lengths i) :: Int
          if off + len > BS.length dataBs
            then return (Left "ORC.Read: binary data underflow")
            else do
              MV.unsafeWrite out i (BS.take len (BS.drop off dataBs))
              go out (i + 1) (off + len)
