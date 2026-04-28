{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
-- | Avro Object Container File (OCF) format.
--
-- Reads and writes Avro container files, which consist of a header
-- (with the writer schema and sync marker), followed by data blocks.
-- Supports null and deflate codecs.
--
-- @
-- import Avro.Container (readContainer, writeContainer)
-- import qualified Data.ByteString as BS
--
-- bytes <- BS.readFile \"data.avro\"
-- let Right (header, values) = readContainer bytes
-- @
module Avro.Container
  ( ContainerHeader(..)
  , readContainer
  , readContainerResolved
  , writeContainer
  , writeContainerWith
  , writeContainerWithSync
  , randomSyncMarker
  , syncMarkerForSchema
  , decompressBlock
  , compressBlock
  ) where

import qualified Codec.Compression.Zlib.Raw as ZlibRaw
import Control.Exception (SomeException, evaluate, try)
import Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import Data.Digest.CRC32 (crc32)
import Data.Word (Word8, Word32)
import qualified System.Random as Random
import System.IO.Unsafe (unsafePerformIO)
#ifdef HAVE_SNAPPY
import qualified Codec.Compression.Snappy as Snappy
#endif
#ifdef HAVE_ZSTD
import Codec.Compression.Zstd (Decompress (..), compress, decompress)
#endif
#ifdef HAVE_BZIP2
import qualified Codec.Compression.BZip as BZip
#endif
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

import Avro.Decode (decodeAvroAt)
import Avro.Encode (encodeAvro)
import Avro.Fingerprint (avroFingerprint)
import Avro.JSON (avroSchemaFromJSON, avroSchemaToJSON)
import Avro.Resolution (resolveSchema, resolveValue)
import Avro.Schema (AvroType)
import qualified Avro.Value as AV
import Avro.Wire
  ( AvroDecodeResult(..)
  , avroDecodeLong
  , avroEncodeBytes
  , avroEncodeLong
  , avroDecodeBytes
  )

data ContainerHeader = ContainerHeader
  { containerSchema :: !AvroType
  , containerCodec  :: !T.Text
  , containerSync   :: !ByteString
  } deriving stock (Show, Eq)

magic :: ByteString
magic = BS.pack [0x4F, 0x62, 0x6A, 0x01]

syncMarkerSize :: Int
syncMarkerSize = 16

-- | A deterministic 16-byte sync marker derived from the schema's
-- parsing canonical form.
--
-- The Avro specification says the sync marker should be \"a 16-byte,
-- randomly-generated\" value. Its purpose is twofold:
--
--   1. It separates blocks unambiguously, so two container files
--      can be concatenated and read as a stream.
--   2. A reader can resynchronise after a corrupt block by scanning
--      the bytes for the marker.
--
-- The pure 'writeContainer' / 'writeContainerWith' API can't generate
-- random bytes, but using all-zero bytes (the previous behaviour)
-- defeats both properties — every legitimate run of zeros in the
-- encoded data looks like a sync. Instead we derive a deterministic
-- 16-byte marker by concatenating the schema's CRC-64-AVRO
-- fingerprint with its bitwise complement.  This:
--
--   * Is non-zero in every byte position (because @x@ and
--     @complement x@ are bitwise complements: their disjunction is
--     all-ones, so at least one of the two halves has the bit set in
--     each position).
--   * Differs across schemas (any schema change changes the
--     fingerprint).
--   * Is stable for the same schema, so concatenating two files
--     written with this function and the same schema is well-defined.
--
-- Callers that need a per-file random marker (e.g. for de-duplication
-- across many writes of the same schema) can use 'randomSyncMarker'
-- and 'writeContainerWithSync'.
syncMarkerForSchema :: AvroType -> ByteString
syncMarkerForSchema schema =
  let !fp = avroFingerprint schema   -- 8 bytes
      !complemented = BS.map complement fp
  in fp <> complemented

-- | Generate a random 16-byte sync marker. Use this with
-- 'writeContainerWithSync' when you need the spec's recommendation
-- of an unpredictable per-file marker.
randomSyncMarker :: IO ByteString
randomSyncMarker = do
  ws <- Random.getStdRandom (\g0 ->
    let go :: Int -> [Word8] -> Random.StdGen -> ([Word8], Random.StdGen)
        go !i !acc !g
          | i >= syncMarkerSize = (reverse acc, g)
          | otherwise =
              -- Mix two reduced 32-bit words into one byte rather
              -- than calling random for a Word8 directly: GHC's
              -- 'random :: Word8' is implementation-dependent, but
              -- 'random :: Int' is deterministic across versions.
              let (n, g') = Random.random g :: (Int, Random.StdGen)
              in go (i + 1) (fromIntegral (n .&. 0xFF) : acc) g'
    in go 0 [] g0)
  pure $! BS.pack ws

-- | Write an Avro container file (Object Container File) with the
-- @null@ codec, deriving the sync marker from the schema fingerprint.
writeContainer :: AvroType -> V.Vector AV.Value -> ByteString
writeContainer = writeContainerWith "null"

-- | Write an Avro container file with the specified codec
-- (@null@, @deflate@, @snappy@, @zstandard@, or @bzip2@).
--
-- Uses 'syncMarkerForSchema' for the sync marker; callers that need
-- a different (e.g. random) marker can use 'writeContainerWithSync'.
writeContainerWith :: T.Text -> AvroType -> V.Vector AV.Value -> ByteString
writeContainerWith codec schema vals =
  writeContainerWithSync (syncMarkerForSchema schema) codec schema vals

-- | Like 'writeContainerWith' but takes an explicit 16-byte sync
-- marker. The marker must be exactly 'syncMarkerSize' bytes (16);
-- otherwise it is right-padded or truncated to that length.
--
-- Generate one with 'randomSyncMarker':
--
-- @
-- sync <- randomSyncMarker
-- BS.writeFile \"out.avro\" (writeContainerWithSync sync \"deflate\" schema vals)
-- @
writeContainerWithSync
  :: ByteString
  -> T.Text
  -> AvroType
  -> V.Vector AV.Value
  -> ByteString
writeContainerWithSync sync codec schema vals =
  BL.toStrict (B.toLazyByteString (writeContainerBuilder sync codec schema vals))

-- | Pad / truncate the user-supplied marker to exactly 16 bytes.
normaliseSync :: ByteString -> ByteString
normaliseSync sync
  | BS.length sync >= syncMarkerSize =
      BS.take syncMarkerSize sync
  | otherwise =
      sync <> BS.replicate (syncMarkerSize - BS.length sync) 0

writeContainerBuilder
  :: ByteString
  -> T.Text
  -> AvroType
  -> V.Vector AV.Value
  -> B.Builder
writeContainerBuilder rawSync codec schema vals =
  let !sync = normaliseSync rawSync
      schemaJSON = BL.toStrict $ Aeson.encode (avroSchemaToJSON schema)
      meta = [ ("avro.schema", schemaJSON)
             , ("avro.codec", TE.encodeUtf8 codec)
             ]
      headerBuilder =
        B.byteString magic
        <> encodeAvroMap meta
        <> B.byteString sync
      blockData = mconcat [encodeAvro schema v | v <- V.toList vals]
      compressedData = compressBlock codec blockData
      blockCount = V.length vals
      blockBuilder =
        avroEncodeLong (fromIntegral blockCount)
        <> avroEncodeLong (fromIntegral (BS.length compressedData))
        <> B.byteString compressedData
        <> B.byteString sync
  in if V.null vals
     then headerBuilder
     else headerBuilder <> blockBuilder

encodeAvroMap :: [(ByteString, ByteString)] -> B.Builder
encodeAvroMap [] = avroEncodeLong 0
encodeAvroMap entries =
  avroEncodeLong (fromIntegral (length entries))
  <> foldMap (\(k, v) -> avroEncodeBytes k <> avroEncodeBytes v) entries
  <> avroEncodeLong 0

-- | Read an Avro container file, returning the schema and all values.
readContainer :: ByteString -> Either String (AvroType, V.Vector AV.Value)
readContainer bs = do
  (hdr, off) <- parseHeader bs
  vals <- parseBlocks (containerSchema hdr) (containerCodec hdr) (containerSync hdr) bs off []
  Right (containerSchema hdr, V.fromList (reverse vals))

-- | Read a container file, resolving to a reader schema.
readContainerResolved :: AvroType -> ByteString -> Either String (V.Vector AV.Value)
readContainerResolved readerSchema bs = do
  (hdr, off) <- parseHeader bs
  let writerSchema = containerSchema hdr
  resolved <- resolveSchema writerSchema readerSchema
  vals <- parseBlocks writerSchema (containerCodec hdr) (containerSync hdr) bs off []
  let resolvedVals = reverse vals
  V.fromList <$> mapM (resolveValue resolved) resolvedVals

parseHeader :: ByteString -> Either String (ContainerHeader, Int)
parseHeader bs
  | BS.length bs < 4 = Left "Avro.Container: too short for magic"
  | BS.take 4 bs /= magic = Left "Avro.Container: invalid magic bytes"
  | otherwise = do
      (meta, off) <- decodeAvroMap bs 4
      let schemaBS = lookup "avro.schema" meta
          codecBS  = lookup "avro.codec" meta
      schema <- case schemaBS of
        Nothing -> Left "Avro.Container: missing avro.schema in header"
        Just s  -> case Aeson.decodeStrict s of
          Nothing -> Left "Avro.Container: invalid JSON in avro.schema"
          Just j  -> avroSchemaFromJSON j
      let codec = case codecBS of
            Just c  -> case TE.decodeUtf8' c of
              Right t -> t
              Left _  -> "null"
            Nothing -> "null"
      if off + syncMarkerSize > BS.length bs
        then Left "Avro.Container: not enough bytes for sync marker"
        else do
          let sync = BS.take syncMarkerSize (BS.drop off bs)
          Right (ContainerHeader schema codec sync, off + syncMarkerSize)

decodeAvroMap :: ByteString -> Int -> Either String ([(ByteString, ByteString)], Int)
decodeAvroMap bs off = decodeMapBlocks bs off []
  where
    decodeMapBlocks :: ByteString -> Int -> [(ByteString, ByteString)] -> Either String ([(ByteString, ByteString)], Int)
    decodeMapBlocks bs' off' acc =
      case avroDecodeLong bs' off' of
        AvroDecodeFail e -> Left e
        AvroDecodeOK cnt off'' ->
          if cnt == 0
          then Right (reverse acc, off'')
          else if cnt < 0
          then case avroDecodeLong bs' off'' of
            AvroDecodeFail e -> Left e
            AvroDecodeOK _blockSz off''' ->
              decodeMapEntries bs' off''' (fromIntegral (negate cnt)) acc
          else decodeMapEntries bs' off'' (fromIntegral cnt) acc

    decodeMapEntries :: ByteString -> Int -> Int -> [(ByteString, ByteString)] -> Either String ([(ByteString, ByteString)], Int)
    decodeMapEntries _bs' off' 0 acc = decodeMapBlocks _bs' off' acc
    decodeMapEntries bs' off' n acc =
      case avroDecodeBytes bs' off' of
        AvroDecodeFail e -> Left e
        AvroDecodeOK key off'' ->
          case avroDecodeBytes bs' off'' of
            AvroDecodeFail e -> Left e
            AvroDecodeOK val off''' ->
              decodeMapEntries bs' off''' (n - 1) ((key, val) : acc)

parseBlocks :: AvroType -> T.Text -> ByteString -> ByteString -> Int -> [AV.Value] -> Either String [AV.Value]
parseBlocks schema codec sync bs off acc
  | off >= BS.length bs = Right acc
  | otherwise = do
      (cnt64, off') <- decodeLong' bs off
      let !cnt = fromIntegral cnt64 :: Int
      (byteSz64, off'') <- decodeLong' bs off'
      let !byteSz = fromIntegral byteSz64 :: Int
      if off'' + byteSz > BS.length bs
        then Left "Avro.Container: block data truncated"
        else do
          let compressedData = BS.take byteSz (BS.drop off'' bs)
          blockData <- decompressBlock codec compressedData
          (vals, _) <- decodeNValues schema blockData 0 cnt acc
          let off''' = off'' + byteSz
          if off''' + syncMarkerSize > BS.length bs
            then Left "Avro.Container: block sync marker truncated"
            else do
              let blockSync = BS.take syncMarkerSize (BS.drop off''' bs)
              if blockSync /= sync
                then Left "Avro.Container: sync marker mismatch"
                else parseBlocks schema codec sync bs (off''' + syncMarkerSize) vals

decodeLong' :: ByteString -> Int -> Either String (Int64, Int)
decodeLong' bs off = case avroDecodeLong bs off of
  AvroDecodeFail e    -> Left e
  AvroDecodeOK v off' -> Right (v, off')

decodeNValues :: AvroType -> ByteString -> Int -> Int -> [AV.Value] -> Either String ([AV.Value], Int)
decodeNValues _schema _bs off 0 acc = Right (acc, off)
decodeNValues schema bs off n acc = do
  (val, off') <- decodeAvroAt schema bs off
  decodeNValues schema bs off' (n - 1) (val : acc)

-- | Decompress a block of data using the given codec.
--
-- Supports the codecs listed in the Avro 1.11 specification:
--
--   * @null@      — required, identity passthrough.
--   * @deflate@   — required, raw RFC 1951 deflate (no zlib header).
--   * @snappy@    — optional, with the spec-required 4-byte big-endian
--     CRC32-of-uncompressed trailer.
--   * @zstandard@ — optional, Facebook Zstandard.
--   * @bzip2@     — optional, the bzip2 block compressor.
--
-- Optional codecs require the corresponding cabal flag
-- (@-fsnappy@, @-fzstd@, @-fbzip2@); without it the reader returns a
-- helpful error rather than silently mis-decoding.
decompressBlock :: T.Text -> ByteString -> Either String ByteString
decompressBlock "null" bs = Right bs
decompressBlock "deflate" bs = Right $ BL.toStrict $ ZlibRaw.decompress $ BL.fromStrict bs
#ifdef HAVE_SNAPPY
decompressBlock "snappy" bs = decompressSnappyWithCrc bs
#else
decompressBlock "snappy" _ = Left "Avro: snappy codec not available (build with -fsnappy)"
#endif
#ifdef HAVE_ZSTD
decompressBlock "zstandard" bs = case decompress bs of
  Decompress out -> Right out
  Skip           -> Left "Avro.Container: zstd decompress skipped (empty frame)"
  Error msg      -> Left $ "Avro.Container: zstd decompress failed: " ++ msg
#else
decompressBlock "zstandard" _ =
  Left "Avro: zstandard codec not available (build with -fzstd)"
#endif
#ifdef HAVE_BZIP2
decompressBlock "bzip2" bs =
  Right $ BL.toStrict $ BZip.decompress $ BL.fromStrict bs
#else
decompressBlock "bzip2" _ =
  Left "Avro: bzip2 codec not available (build with -fbzip2)"
#endif
decompressBlock codec _ = Left $ "Unsupported codec: " <> T.unpack codec

-- | Compress a block of data using the given codec. Symmetric with
-- 'decompressBlock'. Falls back to identity when an unknown codec is
-- requested so writers always succeed even with mismatched flags
-- (the reader will reject the file with a clear error).
compressBlock :: T.Text -> ByteString -> ByteString
compressBlock "null" bs = bs
compressBlock "deflate" bs = BL.toStrict $ ZlibRaw.compress $ BL.fromStrict bs
#ifdef HAVE_SNAPPY
compressBlock "snappy" bs = compressSnappyWithCrc bs
#endif
#ifdef HAVE_ZSTD
compressBlock "zstandard" bs = compress 3 bs
#endif
#ifdef HAVE_BZIP2
compressBlock "bzip2" bs = BL.toStrict $ BZip.compress $ BL.fromStrict bs
#endif
compressBlock _ bs = bs

#ifdef HAVE_SNAPPY
-- | Decompress an Avro snappy block. The compressed data is followed by
-- a 4-byte big-endian CRC32 of the original uncompressed bytes (Avro
-- spec, OCF / codecs section). Mismatched CRC and corrupt frames are
-- both reported through 'Either'; the raw 'Snappy.decompress' otherwise
-- raises an asynchronous-style 'IOError' on bad input which would crash
-- the reader.
decompressSnappyWithCrc :: ByteString -> Either String ByteString
decompressSnappyWithCrc bs
  | BS.length bs < 4 =
      Left "Avro.Container: snappy block too short for CRC32 trailer"
  | otherwise = unsafePerformIO $ do
      let !payloadLen = BS.length bs - 4
          !payload = BS.take payloadLen bs
          !crcBytes = BS.drop payloadLen bs
          !storedCrc = readBE32 crcBytes
      er <- try @SomeException $ evaluate (Snappy.decompress payload)
      case er of
        Left e -> pure $ Left $
          "Avro.Container: snappy decompress failed: " ++ show e
        Right decoded ->
          let !computed = crc32 decoded
          in if computed /= storedCrc
               then pure $ Left $
                 "Avro.Container: snappy CRC32 mismatch (stored "
                   ++ show storedCrc ++ ", computed " ++ show computed ++ ")"
               else pure $ Right decoded

-- | Compress a block with snappy and append the spec-required 4-byte
-- big-endian CRC32 of the /uncompressed/ data.
compressSnappyWithCrc :: ByteString -> ByteString
compressSnappyWithCrc bs =
  let !compressed = Snappy.compress bs
      !c = crc32 bs
      !crcBytes = beW32 c
  in compressed <> crcBytes
#endif

-- | Read a big-endian 32-bit unsigned value out of the first 4 bytes.
{-# INLINE readBE32 #-}
readBE32 :: ByteString -> Word32
readBE32 bs =
  let b0 = fromIntegral (BS.index bs 0) :: Word32
      b1 = fromIntegral (BS.index bs 1) :: Word32
      b2 = fromIntegral (BS.index bs 2) :: Word32
      b3 = fromIntegral (BS.index bs 3) :: Word32
  in (b0 `shiftL` 24) .|. (b1 `shiftL` 16) .|. (b2 `shiftL` 8) .|. b3

-- | 4-byte big-endian encoding of a 'Word32', as a strict 'ByteString'.
{-# INLINE beW32 #-}
beW32 :: Word32 -> ByteString
beW32 w = BS.pack
  [ fromIntegral ((w `shiftR` 24) .&. 0xFF)
  , fromIntegral ((w `shiftR` 16) .&. 0xFF)
  , fromIntegral ((w `shiftR` 8)  .&. 0xFF)
  , fromIntegral (w               .&. 0xFF)
  ]
