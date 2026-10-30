{-# LANGUAGE OverloadedStrings #-}

module Protocol.RecordBatchSpec (tests) where

import Data.Bits (shiftR, xor)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List qualified
import Data.Vector qualified as V
import Data.Word (Word64, Word8)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Kafka.Compression.Types qualified as Compression
import Kafka.Protocol.RecordBatch qualified as RB
import Kafka.Protocol.RecordBatchWire qualified as RBW
import Test.Syd
import Test.Syd.Hedgehog ()


{- | Arbitrary bytes for record payloads, which add up to hundreds of
KiB per batch. 'Gen.bytes' builds a shrink tree per byte, so a 400 KiB
batch cost ~150 MB of allocation and each property here took ~10 s in a
loaded suite. This draws the length, the alphabet and a seed through
Hedgehog (the length still shrinks) and expands the seed with
splitmix64. The alphabet split matches 'Gen.bytes': lowercase ASCII
(compressible) or any byte (incompressible).
-}
genBulkBytes :: Range Int -> Gen ByteString
genBulkBytes range = do
  n <- Gen.int range
  lowercase <- Gen.bool
  seed <- Gen.word64 Range.constantBounded
  pure (fst (BS.unfoldrN n (step lowercase) seed))
  where
    step :: Bool -> Word64 -> Maybe (Word8, Word64)
    step lowercase s =
      let s' = s + 0x9e3779b97f4a7c15
          z = mix s'
          b
            | lowercase = 97 + fromIntegral (z `rem` 26)
            | otherwise = fromIntegral (z `shiftR` 56)
      in Just (b, s')
    mix z0 =
      let z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xbf58476d1ce4e5b9
          z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb
      in z2 `xor` (z2 `shiftR` 31)


-- | Generate a RecordHeader
genRecordHeader :: Gen RB.RecordHeader
genRecordHeader = do
  key <- Gen.bytes (Range.linear 0 100)
  value <- Gen.maybe (genBulkBytes (Range.linear 0 1000))
  return $ RB.RecordHeader key value


-- | Generate a Record
genRecord :: Gen RB.Record
genRecord = do
  timestampDelta <- Gen.int64 (Range.linear 0 10000)
  offsetDelta <- Gen.int32 (Range.linear 0 1000)
  key <- Gen.maybe (Gen.bytes (Range.linear 0 100))
  -- 'Nothing' exercises null values (tombstones) on the wire.
  value <- Gen.maybe (genBulkBytes (Range.linear 0 10000))
  headersCount <- Gen.int (Range.linear 0 10)
  headers <- Gen.list (Range.singleton headersCount) genRecordHeader
  return $ RB.Record timestampDelta offsetDelta key value headers


-- | Generate a TimestampType
genTimestampType :: Gen RB.TimestampType
genTimestampType = Gen.element [RB.CreateTime, RB.LogAppendTime]


-- | Generate Attributes
genAttributes :: Gen RB.Attributes
genAttributes = do
  codec <-
    Gen.element
      [ Compression.NoCompression
      , Compression.Gzip
      , Compression.Lz4
      , Compression.Zstd
      ]
  timestampType <- genTimestampType
  isTransactional <- Gen.bool
  isControl <- Gen.bool
  hasDeleteHorizon <- Gen.bool
  return $ RB.mkAttributes codec timestampType isTransactional isControl hasDeleteHorizon


-- | Generate a RecordBatch
genRecordBatch :: Gen RB.RecordBatch
genRecordBatch = do
  baseOffset <- Gen.int64 (Range.linear 0 1000000)
  leaderEpoch <- Gen.int32 (Range.linear (-1) 1000)
  attrs <- genAttributes
  baseTimestamp <- Gen.int64 (Range.linear 0 1000000000)
  producerId <- Gen.int64 (Range.linear (-1) 1000000)
  producerEpoch <- Gen.int16 (Range.linear (-1) 1000)
  baseSequence <- Gen.int32 (Range.linear (-1) 1000000)
  recordsCount <- Gen.int (Range.linear 0 20)
  records <- V.fromList <$> Gen.list (Range.singleton recordsCount) genRecord
  return $
    RB.mkRecordBatch
      baseOffset
      leaderEpoch
      attrs
      baseTimestamp
      producerId
      producerEpoch
      baseSequence
      records


-- | Test that encoding and decoding a RecordBatch is a round-trip
prop_recordBatch_roundtrip :: Property
prop_recordBatch_roundtrip = property $ do
  batch <- forAll genRecordBatch
  let encoded = RBW.encodeRecordBatchWire batch
  annotate $ "Encoded size: " ++ show (BS.length encoded)
  case RBW.decodeRecordBatchWire encoded of
    Left err -> do
      annotate $ "Decode error: " ++ err
      failure
    Right decoded -> batch === decoded


-- The Wire codec works at the batch level via
-- 'RBW.encodeRecordBatchWire' / 'RBW.decodeRecordBatchWire'; the
-- full-batch round-trip properties below exercise the per-record
-- path implicitly (every encoded record has to round-trip
-- identically for the batch assertion to hold).

-- | Test that an empty batch encodes and decodes correctly
prop_empty_batch :: Property
prop_empty_batch = property $ do
  let batch = RB.mkSimpleBatch 0 0 V.empty
  let encoded = RBW.encodeRecordBatchWire batch
  case RBW.decodeRecordBatchWire encoded of
    Left err -> do
      annotate $ "Decode error: " ++ err
      failure
    Right decoded -> batch === decoded


-- | Test that a single-record batch encodes and decodes correctly
prop_single_record_batch :: Property
prop_single_record_batch = property $ do
  record <- forAll genRecord
  let batch = RB.mkSimpleBatch 0 0 (V.singleton record)
  let encoded = RBW.encodeRecordBatchWire batch
  case RBW.decodeRecordBatchWire encoded of
    Left err -> do
      annotate $ "Decode error: " ++ err
      failure
    Right decoded -> batch === decoded


-- | Test that the CRC is validated on decode
prop_crc_validation :: Property
prop_crc_validation = property $ do
  batch <- forAll genRecordBatch
  let encoded = RBW.encodeRecordBatchWire batch
  -- Corrupt a byte in the middle (but not the CRC itself)
  let corrupted =
        if BS.length encoded > 30
          then
            let (before, afterBytes) = BS.splitAt 30 encoded
                (corruptByte, rest) = (BS.head afterBytes, BS.tail afterBytes)
                newByte = corruptByte `xor` 0x01
            in before <> BS.singleton newByte <> rest
          else encoded

  if corrupted == encoded
    then success -- Too small to corrupt meaningfully
    else case RBW.decodeRecordBatchWire corrupted of
      Left err -> do
        annotate $ "Expected error: " ++ err
        -- Wire-shape decoder wraps the message in "user error (...)";
        -- substring check is robust across both shapes.
        assert $ "CRC" `Data.List.isInfixOf` err
      Right _ -> do
        annotate "Expected CRC error but decode succeeded"
        failure


{- | Test that 'calculateBatchSize' is a /safe upper bound/ on the
actual encoded size. The function is a worst-case estimator
(uses the worst-case varint width per field), not an exact
computation; the exact size requires a full encode round.
-}
prop_batch_size :: Property
prop_batch_size = property $ do
  batch <- forAll genRecordBatch
  let encoded = RBW.encodeRecordBatchWire batch
      calculatedSize = RB.calculateBatchSize batch
  diff (BS.length encoded) (<=) calculatedSize


-- | Test that compressed RecordBatch round-trips correctly
prop_compressed_batch_gzip :: Property
prop_compressed_batch_gzip = property $ do
  batch <- forAll genRecordBatch
  -- Override codec to use Gzip
  let attrs = RB.batchAttributes batch
  let attrsWithGzip = attrs {RB.attrCompressionType = Compression.Gzip}
  let batchWithGzip = batch {RB.batchAttributes = attrsWithGzip}

  encoded <- evalIO $ RBW.encodeRecordBatchWireCompressed batchWithGzip
  case encoded of
    Left err -> do
      annotate $ "Encode error: " ++ err
      failure
    Right encodedBytes -> do
      decoded <- evalIO $ RBW.decodeRecordBatchWireWithDecompression encodedBytes
      case decoded of
        Left err -> do
          annotate $ "Decode error: " ++ err
          failure
        Right decodedBatch -> batchWithGzip === decodedBatch


-- | Test that compressed RecordBatch with Zstd round-trips correctly
prop_compressed_batch_zstd :: Property
prop_compressed_batch_zstd = property $ do
  batch <- forAll genRecordBatch
  -- Override codec to use Zstd
  let attrs = RB.batchAttributes batch
  let attrsWithZstd = attrs {RB.attrCompressionType = Compression.Zstd}
  let batchWithZstd = batch {RB.batchAttributes = attrsWithZstd}

  encoded <- evalIO $ RBW.encodeRecordBatchWireCompressed batchWithZstd
  case encoded of
    Left err -> do
      annotate $ "Encode error: " ++ err
      failure
    Right encodedBytes -> do
      decoded <- evalIO $ RBW.decodeRecordBatchWireWithDecompression encodedBytes
      case decoded of
        Left err -> do
          annotate $ "Decode error: " ++ err
          failure
        Right decodedBatch -> batchWithZstd === decodedBatch


-- | Test that compressed RecordBatch with LZ4 round-trips correctly
prop_compressed_batch_lz4 :: Property
prop_compressed_batch_lz4 = property $ do
  batch <- forAll genRecordBatch
  -- Override codec to use LZ4
  let attrs = RB.batchAttributes batch
  let attrsWithLz4 = attrs {RB.attrCompressionType = Compression.Lz4}
  let batchWithLz4 = batch {RB.batchAttributes = attrsWithLz4}

  encoded <- evalIO $ RBW.encodeRecordBatchWireCompressed batchWithLz4
  case encoded of
    Left err -> do
      annotate $ "Encode error: " ++ err
      failure
    Right encodedBytes -> do
      decoded <- evalIO $ RBW.decodeRecordBatchWireWithDecompression encodedBytes
      case decoded of
        Left err -> do
          annotate $ "Decode error: " ++ err
          failure
        Right decodedBatch -> batchWithLz4 === decodedBatch


-- | Test that compressed RecordBatch with Snappy round-trips correctly
prop_compressed_batch_snappy :: Property
prop_compressed_batch_snappy = property $ do
  batch <- forAll genRecordBatch
  -- Override codec to use Snappy
  let attrs = RB.batchAttributes batch
  let attrsWithSnappy = attrs {RB.attrCompressionType = Compression.Snappy}
  let batchWithSnappy = batch {RB.batchAttributes = attrsWithSnappy}

  encoded <- evalIO $ RBW.encodeRecordBatchWireCompressed batchWithSnappy
  case encoded of
    Left err -> do
      annotate $ "Encode error: " ++ err
      failure
    Right encodedBytes -> do
      decoded <- evalIO $ RBW.decodeRecordBatchWireWithDecompression encodedBytes
      case decoded of
        Left err -> do
          annotate $ "Decode error: " ++ err
          failure
        Right decodedBatch -> batchWithSnappy === decodedBatch


-- | Test that compression actually reduces size for repeated data
prop_compression_reduces_size :: Property
prop_compression_reduces_size = property $ do
  -- Create a batch with highly compressible data (repeated pattern)
  let repeatedValue = BS.replicate 1000 42
  let records = V.replicate 10 (RB.Record 0 0 Nothing (Just repeatedValue) [])
  let attrs = RB.mkAttributes Compression.Gzip RB.CreateTime False False False
  let batch = RB.mkSimpleBatch 0 0 records
  let batchWithGzip = batch {RB.batchAttributes = attrs}

  uncompressedSize <- evalIO $ do
    let uncompressed = RBW.encodeRecordBatchWire batchWithGzip
    return $ BS.length uncompressed

  compressedResult <- evalIO $ RBW.encodeRecordBatchWireCompressed batchWithGzip
  case compressedResult of
    Left err -> do
      annotate $ "Compression error: " ++ err
      failure
    Right compressed -> do
      let compressedSize = BS.length compressed
      annotate $ "Uncompressed size: " ++ show uncompressedSize
      annotate $ "Compressed size: " ++ show compressedSize
      -- Compressed should be significantly smaller (at least 50% reduction for this data)
      assert $ compressedSize < uncompressedSize `div` 2


-- | All tests for RecordBatch
tests :: Spec
tests =
  describe "RecordBatch" $
    sequence_
      [ describe "Basic Encoding" $
          sequence_
            [ it "RecordBatch round-trip" prop_recordBatch_roundtrip
            , it "Empty batch round-trip" prop_empty_batch
            , it "Single record batch round-trip" prop_single_record_batch
            , it "CRC validation" prop_crc_validation
            , it "Batch size calculation" prop_batch_size
            ]
      , describe "Compression" $
          sequence_
            [ it "Gzip compression round-trip" prop_compressed_batch_gzip
            , it "Zstd compression round-trip" prop_compressed_batch_zstd
            , it "LZ4 compression round-trip" prop_compressed_batch_lz4
            , it "Snappy compression round-trip" prop_compressed_batch_snappy
            , it "Compression reduces size" prop_compression_reduces_size
            ]
      ]
