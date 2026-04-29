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
    -- * Column decoders
  , decodeIntColumn
  , decodeBoolColumn
  , decodeStringColumn
  , decodeStringDictColumn
  , decodeFloatColumn
  , decodeDoubleColumn
  , decodeTimestampColumn
  , ORCTimestamp (..)
  , decodeDateColumn
  , decodeDecimalColumn
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
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Primitive as VP
import Data.Word (Word32, Word64)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import System.IO.Unsafe (unsafePerformIO)

import qualified Codec.Compression.Zlib.Raw as ZlibRaw
#ifdef HAVE_ZSTD
import Codec.Compression.Zstd (Decompress (..), decompress)
#endif
#ifdef HAVE_SNAPPY
import qualified Codec.Compression.Snappy as Snappy
#endif

import ORC.Footer (readORCCompression, readORCFooter)
import ORC.RLE (decodeBooleanRLE, decodePresentStream, decodeRLEv1Int, decodeRLEv2Int, decodeRLEv2IntAll)
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
  } deriving stock (Show, Eq)

-- | Read postscript, footer, and parse protobuf footer metadata.
loadORCFile :: ByteString -> Either String ORCFile
loadORCFile bs = do
  ft <- readORCFooter bs
  ck <- readORCCompression bs
  Right ORCFile {ofBytes = bs, ofFooter = ft, ofCompression = ck}

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

-- | Parse the protobuf stripe footer for a stripe index.
loadStripeFooter :: ORCFile -> Int -> Either String StripeFooter
loadStripeFooter ofile idx = do
  stripe <- stripeSlice ofile idx
  let ft = ofFooter ofile
      ss = orcStripes ft
  if idx < 0 || idx >= V.length ss
    then Left "ORC.Read: stripe index out of range"
    else do
      let si = V.unsafeIndex ss idx
      fb <- stripeFooterBytes stripe si
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

-- | Decompress an ORC stream using the file's compression codec.
--
-- ORC wraps each compressed chunk with a 3-byte LE header encoding
-- @(chunkLength * 2 + isOriginal)@. 'CompressionNone' passes through.
{-# INLINE decompressORCStream #-}
decompressORCStream :: CompressionKind -> ByteString -> Either String ByteString
decompressORCStream CompressionNone bs = Right bs
decompressORCStream kind bs = decompressChunks kind bs 0 []

decompressChunks :: CompressionKind -> ByteString -> Int -> [ByteString] -> Either String ByteString
decompressChunks kind bs !off !acc
  | off >= BS.length bs = Right $! BS.concat (reverse acc)
  | off + 3 > BS.length bs = Left "ORC.Read: truncated compression header"
  | otherwise = do
      let !b0     = fromIntegral (BS.index bs off) :: Word64
          !b1     = fromIntegral (BS.index bs (off + 1)) :: Word64
          !b2     = fromIntegral (BS.index bs (off + 2)) :: Word64
          !header = b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16)
          !isOrig = header .&. 1 == 1
          !cLen   = fromIntegral (header `shiftR` 1) :: Int
      if off + 3 + cLen > BS.length bs
        then Left "ORC.Read: compression chunk extends past stream end"
        else do
          let !chunk = BS.take cLen (BS.drop (off + 3) bs)
          decoded <- if isOrig
            then Right chunk
            else decompressBlock kind chunk
          decompressChunks kind bs (off + 3 + cLen) (decoded : acc)

decompressBlock :: CompressionKind -> ByteString -> Either String ByteString
decompressBlock CompressionNone bs = Right bs
decompressBlock CompressionZlib bs = tryZlibRaw bs
decompressBlock CompressionSnappy bs = trySnappy bs
#ifdef HAVE_ZSTD
decompressBlock CompressionZstd bs = tryZstd bs
#endif
decompressBlock c _ =
  Left $
    "ORC.Read: compression "
      ++ show c
      ++ " not supported (use None, Zlib, Snappy with -fsnappy"
#ifdef HAVE_ZSTD
      ++ ", Zstandard with -fzstd"
#endif
      ++ ")"

tryZlibRaw :: ByteString -> Either String ByteString
tryZlibRaw bs =
  unsafePerformIO $ do
    er <- try @SomeException $ evaluate $ BL.toStrict $ ZlibRaw.decompress $ BL.fromStrict bs
    case er of
      Left e  -> pure $ Left $ "ORC.Read: zlib decompress failed: " ++ show e
      Right x -> pure $ Right x

trySnappy :: ByteString -> Either String ByteString
#ifdef HAVE_SNAPPY
trySnappy bs = Right (Snappy.decompress bs)
#else
trySnappy _ =
  Left "ORC.Read: Snappy requires building wireform with -fsnappy"
#endif

#ifdef HAVE_ZSTD
tryZstd :: ByteString -> Either String ByteString
tryZstd bs =
  case decompress bs of
    Decompress out -> Right out
    Skip           -> Left "ORC.Read: zstd decompress skipped"
    Error msg      -> Left $ "ORC.Read: zstd decompress failed: " ++ msg
#endif

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

-- | Decode a string column.  Routes to DIRECT decoding when the
-- dictionary stream is empty, or DICTIONARY_V2 decoding otherwise.
--
-- Arguments: @numRows@, @data@ (DIRECT: UTF-8 bytes; DICTIONARY:
-- dictionary entry bytes), @length stream@ (RLE v2 unsigned),
-- @dictionary data stream@ (empty for DIRECT; for DICTIONARY this is
-- the @data@ argument and the @data@ argument carries the index
-- stream), @present stream@.
decodeStringColumn
  :: Int -> ByteString -> ByteString -> ByteString -> Maybe ByteString
  -> Either String (V.Vector (Maybe T.Text))
decodeStringColumn numRows dataBs lengthBs dictBs mPresentBs
  | not (BS.null dictBs) =
      -- DICTIONARY_V2: dictBs is the dictionary's UTF-8 bytes; lengthBs
      -- gives both dictionary entry lengths and (typically) the index
      -- stream is in dataBs. The standard ORC layout splits these into
      -- separate streams; callers that already know the layout should
      -- prefer 'decodeStringDictColumn' directly.
      decodeStringDictColumn numRows dictBs lengthBs dataBs mPresentBs
  | otherwise = do
      (numPresent, mPresent) <- case mPresentBs of
        Nothing -> Right (numRows, Nothing)
        Just pbs -> do
          p <- decodePresentStream numRows pbs
          Right (countTrue p, Just p)
      lengths <- decodeRLEv2Int False numPresent lengthBs
      strings <- splitByLengths dataBs lengths
      case mPresent of
        Nothing -> Right $! V.map Just strings
        Just present -> Right $! interleaveText present strings

-- | Decode an IEEE 754 single-precision float column (little-endian).
decodeFloatColumn
  :: Int -> ByteString -> Maybe ByteString
  -> Either String (V.Vector (Maybe Float))
decodeFloatColumn numRows dataBs mPresentBs = case mPresentBs of
  Nothing -> do
    if BS.length dataBs < numRows * 4
      then Left "ORC.Read: float data stream too short"
      else Right $! V.generate numRows $ \i ->
             Just (readFloatLE dataBs (i * 4))
  Just presentBs -> do
    present <- decodePresentStream numRows presentBs
    let !numPresent = countTrue present
    if BS.length dataBs < numPresent * 4
      then Left "ORC.Read: float data stream too short"
      else Right $! interleaveFloat present dataBs

-- | Decode an IEEE 754 double-precision float column (little-endian).
decodeDoubleColumn
  :: Int -> ByteString -> Maybe ByteString
  -> Either String (V.Vector (Maybe Double))
decodeDoubleColumn numRows dataBs mPresentBs = case mPresentBs of
  Nothing -> do
    if BS.length dataBs < numRows * 8
      then Left "ORC.Read: double data stream too short"
      else Right $! V.generate numRows $ \i ->
             Just (readDoubleLE dataBs (i * 8))
  Just presentBs -> do
    present <- decodePresentStream numRows presentBs
    let !numPresent = countTrue present
    if BS.length dataBs < numPresent * 8
      then Left "ORC.Read: double data stream too short"
      else Right $! interleaveDouble present dataBs

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

-- | Decode a DICTIONARY_V2-encoded string column.
--
-- @numRows@, dictionary data bytes, length stream, index stream, present stream.
decodeStringDictColumn
  :: Int -> ByteString -> ByteString -> ByteString -> Maybe ByteString
  -> Either String (V.Vector (Maybe T.Text))
decodeStringDictColumn numRows dictDataBs lengthBs indexBs mPresentBs = do
  -- Decode dictionary length stream (count is implicit — read all values).
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
      let bytes = V.generate numPresent $ \i ->
            fromIntegral (BS.index dataBs i) :: Int8
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
          case mDataBs of
            Nothing -> Left "ORC.Read: no DATA stream for column"
            Just rawData -> do
              dataBs <- decompressORCStream comp rawData
              mPresent <- case mPresentBs of
                Nothing -> Right Nothing
                Just rp -> Just <$> decompressORCStream comp rp
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

{-# INLINE readFloatLE #-}
readFloatLE :: ByteString -> Int -> Float
readFloatLE bs !off =
  let !b0 = fromIntegral (BS.index bs off) :: Word32
      !b1 = fromIntegral (BS.index bs (off + 1)) :: Word32
      !b2 = fromIntegral (BS.index bs (off + 2)) :: Word32
      !b3 = fromIntegral (BS.index bs (off + 3)) :: Word32
  in castWord32ToFloat (b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24))

{-# INLINE readDoubleLE #-}
readDoubleLE :: ByteString -> Int -> Double
readDoubleLE bs !off =
  let !b0 = fromIntegral (BS.index bs off) :: Word64
      !b1 = fromIntegral (BS.index bs (off + 1)) :: Word64
      !b2 = fromIntegral (BS.index bs (off + 2)) :: Word64
      !b3 = fromIntegral (BS.index bs (off + 3)) :: Word64
      !b4 = fromIntegral (BS.index bs (off + 4)) :: Word64
      !b5 = fromIntegral (BS.index bs (off + 5)) :: Word64
      !b6 = fromIntegral (BS.index bs (off + 6)) :: Word64
      !b7 = fromIntegral (BS.index bs (off + 7)) :: Word64
  in castWord64ToDouble
       ( b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)
         .|. (b4 `shiftL` 32) .|. (b5 `shiftL` 40) .|. (b6 `shiftL` 48) .|. (b7 `shiftL` 56)
       )

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
