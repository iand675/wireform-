{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Round-trip tests for the direct-poke
'Kafka.Protocol.RecordBatchWire' codec — the production path for
record batches in both produce and consume. Every encode
round-trips through the matching Wire decoder, with and without
compression.
-}
module Protocol.RecordBatchWireSpec (tests) where

import Data.ByteString qualified as BS
import Data.Vector qualified as V
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Kafka.Compression.Types qualified as Compression
import Kafka.Protocol.RecordBatch qualified as RB
import Kafka.Protocol.RecordBatchWire qualified as RBW
import Test.Syd
import Test.Syd.Hedgehog ()


tests :: Spec
tests =
  describe "RecordBatchWire round-trips" $
    sequence_
      [ it
          "empty batch round-trips"
          empty_round_trip
      , it
          "single-record batch round-trips"
          single_round_trip
      , it
          "any uncompressed batch round-trips"
          prop_round_trip_uncompressed
      , describe "compressed encoder" $
          sequence_
            [ it
                "gzip: encode . decode == id"
                (prop_round_trip_compressed Compression.Gzip)
            , it
                "snappy: encode . decode == id"
                (prop_round_trip_compressed Compression.Snappy)
            , it
                "lz4: encode . decode == id"
                (prop_round_trip_compressed Compression.Lz4)
            , it
                "zstd: encode . decode == id"
                (prop_round_trip_compressed Compression.Zstd)
            ]
      , describe "sliced decoder" $
          sequence_
            [ it
                "agrees with V.Vector Record decoder on key + value + offsets"
                prop_sliced_matches_record
            , it
                "empty batch returns sbCount=0 + empty slice vectors"
                sliced_empty_batch
            ]
      ]


empty_round_trip :: IO ()
empty_round_trip = do
  let !b = mkBatch []
      !bs = RBW.encodeRecordBatchWire b
  case RBW.decodeRecordBatchWire bs of
    Left err -> error err
    Right b' -> if b == b' then pure () else error "mismatch"


single_round_trip :: IO ()
single_round_trip = do
  let !b = mkBatch [mkRecord 0 (Just "k") "v"]
      !bs = RBW.encodeRecordBatchWire b
  case RBW.decodeRecordBatchWire bs of
    Left err -> error err
    Right b' -> if b == b' then pure () else error "mismatch"


prop_round_trip_uncompressed :: Property
prop_round_trip_uncompressed = property $ do
  records <- forAll (Gen.list (Range.linear 0 16) genRecord)
  let !b = mkBatch records
      !bs = RBW.encodeRecordBatchWire b
  case RBW.decodeRecordBatchWire bs of
    Left err -> annotate err >> failure
    Right b' -> b' === b


{- | Compressed encode + decompress + decode round-trips through the
Wire codec end-to-end.
-}
prop_round_trip_compressed :: Compression.CompressionCodec -> Property
prop_round_trip_compressed codec = property $ do
  records <- forAll (Gen.list (Range.linear 1 16) genRecord)
  let !b = mkCompressedBatch codec records
  wire <- evalIO (RBW.encodeRecordBatchWireCompressed b)
  case wire of
    Left err -> annotate err >> failure
    Right wbs -> do
      back <- evalIO (RBW.decodeRecordBatchWireWithDecompression wbs)
      case back of
        Left err -> annotate err >> failure
        Right b' -> b' === b


mkCompressedBatch :: Compression.CompressionCodec -> [RB.Record] -> RB.RecordBatch
mkCompressedBatch codec records =
  let attrs = RB.defaultAttributes {RB.attrCompressionType = codec}
  in RB.mkRecordBatch
       0
       RB.noPartitionLeaderEpoch
       attrs
       0
       RB.noProducerId
       RB.noProducerEpoch
       RB.noSequence
       (V.fromList records)


----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

mkBatch :: [RB.Record] -> RB.RecordBatch
mkBatch records =
  RB.mkRecordBatch
    0 -- baseOffset
    RB.noPartitionLeaderEpoch
    RB.defaultAttributes
    0 -- baseTimestamp
    RB.noProducerId
    RB.noProducerEpoch
    RB.noSequence
    (V.fromList records)


mkRecord :: Int -> Maybe BS.ByteString -> BS.ByteString -> RB.Record
mkRecord ix k v =
  RB.Record
    { RB.recordTimestampDelta = 0
    , RB.recordOffsetDelta = fromIntegral ix
    , RB.recordKey = k
    , RB.recordValue = Just v
    , RB.recordHeaders = []
    }


genRecord :: Gen RB.Record
genRecord = do
  ofs <- Gen.int32 (Range.linear (-1000) 1000)
  ts <- Gen.int64 (Range.linear (-1000) 1000)
  k <- Gen.maybe (Gen.bytes (Range.linear 0 32))
  -- 'Nothing' exercises null values (tombstones) on the wire.
  v <- Gen.maybe (Gen.bytes (Range.linear 0 256))
  hs <- Gen.list (Range.linear 0 4) genHeader
  pure
    RB.Record
      { RB.recordTimestampDelta = ts
      , RB.recordOffsetDelta = ofs
      , RB.recordKey = k
      , RB.recordValue = v
      , RB.recordHeaders = hs
      }


genHeader :: Gen RB.RecordHeader
genHeader = do
  k <- Gen.bytes (Range.linear 0 16)
  v <- Gen.maybe (Gen.bytes (Range.linear 0 64))
  pure RB.RecordHeader {RB.headerKey = k, RB.headerValue = v}


----------------------------------------------------------------------
-- Sliced decoder
----------------------------------------------------------------------

{- | The sliced decoder shares the per-record bytes with the
record-shape decoder (modulo headers, which the sliced view
doesn't expose). For every record the test asserts: same
absolute offset, same absolute timestamp, same key bytes,
same value bytes (with the v2 null-value-as-empty
normalisation matching).

We deliberately generate records /without/ headers here so
'sbHeaderCounts' is uniformly zero; the sliced decoder is
documented to skip over header bytes without exposing them.
-}
prop_sliced_matches_record :: Property
prop_sliced_matches_record = property $ do
  records <- forAll (Gen.list (Range.linear 0 16) genRecord)
  let !b = mkBatch records
      !bs = RBW.encodeRecordBatchWire b
  case (RBW.decodeRecordBatchWire bs, RBW.decodeRecordBatchWireSliced bs) of
    (Left e, _) -> annotate ("record decode: " <> e) >> failure
    (_, Left e) -> annotate ("sliced decode: " <> e) >> failure
    (Right rb, Right sb) -> do
      let recs = V.toList (RB.batchRecords rb)
      RBW.slicedRecordCount sb === length recs
      RBW.sbBaseOffset sb === RB.batchBaseOffset rb
      RBW.sbBaseTimestamp sb === RB.batchBaseTimestamp rb
      mapM_
        ( \(i, rec) -> do
            RBW.slicedRecordOffset sb i
              === RB.batchBaseOffset rb
                + fromIntegral (RB.recordOffsetDelta rec)
            RBW.slicedRecordTimestamp sb i
              === RB.batchBaseTimestamp rb
                + RB.recordTimestampDelta rec
            RBW.slicedRecordKey sb i === RB.recordKey rec
            RBW.slicedRecordValue sb i === RB.recordValue rec
            -- KIP-82 headers: count + per-header content matches
            RBW.slicedRecordHeaderCount sb i === length (RB.recordHeaders rec)
            let slicedHdrs = RBW.slicedRecordHeaders sb i
                recHdrs =
                  [ (RB.headerKey h, RB.headerValue h)
                  | h <- RB.recordHeaders rec
                  ]
            slicedHdrs === recHdrs
        )
        (zip [0 ..] recs)


sliced_empty_batch :: IO ()
sliced_empty_batch =
  let !b = mkBatch []
      !bs = RBW.encodeRecordBatchWire b
  in case RBW.decodeRecordBatchWireSliced bs of
       Left err -> error err
       Right sb -> do
         if RBW.slicedRecordCount sb == 0
           then pure ()
           else
             error ("expected sbCount=0, got " ++ show (RBW.slicedRecordCount sb))
