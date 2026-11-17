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
  , decompressBlock
  , compressBlock
  ) where

import qualified Codec.Compression.Zlib.Raw as ZlibRaw
#ifdef HAVE_SNAPPY
import qualified Codec.Compression.Snappy as Snappy
#endif
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Wireform.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

import Avro.Decode (decodeAvroAt)
import Avro.Encode (encodeAvro)
import Avro.JSON (avroSchemaFromJSON, avroSchemaToJSON)
import Avro.Resolution (resolveSchema, resolveValue)
import Avro.Schema (AvroType)
import qualified Avro.Value as AV
import Avro.Wire
  ( AvroDecodeResult(..)
  , avroDecodeBlockEnd
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

nullSync :: ByteString
nullSync = BS.replicate syncMarkerSize 0

-- | Write an Avro container file (Object Container File) with null codec.
writeContainer :: AvroType -> V.Vector AV.Value -> ByteString
writeContainer = writeContainerWith "null"

-- | Write an Avro container file with the specified codec ("null" or "deflate").
writeContainerWith :: T.Text -> AvroType -> V.Vector AV.Value -> ByteString
writeContainerWith codec schema vals =
  BL.toStrict (B.toLazyByteString (writeContainerBuilder codec schema vals))

writeContainerBuilder :: T.Text -> AvroType -> V.Vector AV.Value -> B.Builder
writeContainerBuilder codec schema vals =
  let schemaJSON = BL.toStrict $ Aeson.encode (avroSchemaToJSON schema)
      meta = [ ("avro.schema", schemaJSON)
             , ("avro.codec", TE.encodeUtf8 codec)
             ]
      headerBuilder =
        B.byteString magic
        <> encodeAvroMap meta
        <> B.byteString nullSync
      blockData = mconcat [encodeAvro schema v | v <- V.toList vals]
      compressedData = compressBlock codec blockData
      blockCount = V.length vals
      blockBuilder =
        avroEncodeLong (fromIntegral blockCount)
        <> avroEncodeLong (fromIntegral (BS.length compressedData))
        <> B.byteString compressedData
        <> B.byteString nullSync
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
decodeAvroMap bs off = decodeMapBlocks off []
  where
    decodeMapBlocks next acc =
      case avroDecodeLong bs next of
        AvroDecodeOK 0 start -> Right (reverse acc, start)
        AvroDecodeOK count start
          | count == minBound -> Left "Avro.Container: invalid metadata block count"
          | count < 0 -> case avroDecodeBlockEnd bs start of
              AvroDecodeOK end payload -> decodeMapEntries (BS.take end bs) payload (negate count) end acc
              AvroDecodeFail e -> Left e
          | otherwise -> decodeMapEntries bs start count (-1) acc
        AvroDecodeFail e -> Left e

    decodeMapEntries input next count end acc
      | count == 0 =
          if end >= 0 && next /= end
            then Left "Avro.Container: metadata block size does not match its entries"
            else decodeMapBlocks next acc
      | otherwise =
          case avroDecodeBytes input next of
            AvroDecodeFail e -> Left e
            AvroDecodeOK key valueStart ->
              case avroDecodeBytes input valueStart of
                AvroDecodeFail e -> Left e
                AvroDecodeOK val entryEnd ->
                  decodeMapEntries input entryEnd (count - 1) end ((key, val) : acc)

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
decompressBlock :: T.Text -> ByteString -> Either String ByteString
decompressBlock "null" bs = Right bs
decompressBlock "deflate" bs = Right $ BL.toStrict $ ZlibRaw.decompress $ BL.fromStrict bs
#ifdef HAVE_SNAPPY
decompressBlock "snappy" bs = Right (Snappy.decompress bs)
#else
decompressBlock "snappy" _ = Left "Avro: snappy codec not available (build with -fsnappy)"
#endif
decompressBlock codec _ = Left $ "Unsupported codec: " <> T.unpack codec

-- | Compress a block of data using the given codec.
compressBlock :: T.Text -> ByteString -> ByteString
compressBlock "null" bs = bs
compressBlock "deflate" bs = BL.toStrict $ ZlibRaw.compress $ BL.fromStrict bs
#ifdef HAVE_SNAPPY
compressBlock "snappy" bs = Snappy.compress bs
#endif
compressBlock _ bs = bs
