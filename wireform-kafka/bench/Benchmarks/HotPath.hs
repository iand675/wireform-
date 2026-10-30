{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Benchmarks.HotPath
Description : Producer + consumer hot paths against the in-memory mock cluster

Measures what every Kafka client spends its CPU on in steady state:

  * Producer-side
    - 'BA.appendRecordStamped' (STM accumulator).
    - 'Sender.buildRecordBatch' (per-batch wire shape).
    - 'RB.encodeRecordBatch' (the actual byte writer).
    - 'MockProducer.sendMockH' (full mock-cluster round-trip,
      no network).
  * Consumer-side
    - 'RB.decodeRecordBatch' (the per-batch wire reader).
    - 'MockConsumer.pollMC' (full mock-cluster round-trip).

Setup that creates STM-/IORef-backed handles is shared across runs
via top-level @NOINLINE@ + 'unsafePerformIO' (the standard
benchmark-fixture idiom). Each measurement therefore times only
the operation under test, not the cluster construction.

No broker / Docker required; everything runs in-process.
-}
module Benchmarks.HotPath (benchmarks) where

import Criterion (Benchmark, bench, bgroup, nf, whnfIO)
import Data.ByteString qualified as BS
import Data.Vector qualified as V
import GHC.IO (unsafePerformIO)
import Kafka.Client.Internal.BatchAccumulator qualified as BA
import Kafka.Client.Internal.ProducerSender qualified as Sender
import Kafka.Client.Mock.Cluster qualified as MC
import Kafka.Client.Mock.Consumer qualified as MC
import Kafka.Client.Mock.Fault qualified as MC
import Kafka.Client.Mock.Producer qualified as MP
import Kafka.Compression.Types qualified as Compression
import Kafka.Protocol.RecordBatch qualified as RB
import Kafka.Protocol.RecordBatchWire qualified as RBW


----------------------------------------------------------------------
-- Producer hot paths
----------------------------------------------------------------------

producerBench :: Benchmark
producerBench =
  bgroup
    "Producer"
    [ bgroup
        "MockProducer.sendMockH (full mock round-trip, no network)"
        [ bench "1 record / 100B value" $
            whnfIO $
              MP.sendMockH
                sharedMockProducer
                "topic"
                0
                (Just "key")
                payload100
                0
                []
        , bench "1 record / 1 KiB value" $
            whnfIO $
              MP.sendMockH
                sharedMockProducer
                "topic"
                0
                (Just "key")
                payload1k
                0
                []
        , bench "1000 records / 100B value (sequential)" $
            whnfIO $
              sendNTimes sharedMockProducer 1000 payload100
        , bench "10000 records / 100B value (sequential)" $
            whnfIO $
              sendNTimes sharedMockProducer 10000 payload100
        ]
    , bgroup
        "BatchAccumulator.appendRecordStamped (STM hot path)"
        [ bench "single append (no-stamp)" $
            whnfIO $
              BA.appendRecordStamped
                sharedAccumulator
                (BA.TopicPartition "topic" 0)
                sampleRecord
                BA.NoRecordCallback
                BA.noStamp
        , bench "100 appends" $
            whnfIO $
              appendNRecords sharedAccumulator 100
        , bench "1000 appends" $
            whnfIO $
              appendNRecords sharedAccumulator 1000
        ]
    , bgroup
        "RecordBatch encode (Wire serializer; producer hot path)"
        [ bench "buildRecordBatch + encode (1 record)" $
            nf
              ( BS.length
                  . RBW.encodeRecordBatchWire
                  . Sender.buildRecordBatch
              )
              (sampleBatch 1)
        , bench "buildRecordBatch + encode (10 records)" $
            nf
              ( BS.length
                  . RBW.encodeRecordBatchWire
                  . Sender.buildRecordBatch
              )
              (sampleBatch 10)
        , bench "buildRecordBatch + encode (100 records)" $
            nf
              ( BS.length
                  . RBW.encodeRecordBatchWire
                  . Sender.buildRecordBatch
              )
              (sampleBatch 100)
        , bench "encodeRecordBatchWire only (1 record, prebuilt)" $
            nf (BS.length . RBW.encodeRecordBatchWire) builtBatch1
        , bench "encodeRecordBatchWire only (10 records, prebuilt)" $
            nf (BS.length . RBW.encodeRecordBatchWire) builtBatch10
        , bench "encodeRecordBatchWire only (100 records, prebuilt)" $
            nf (BS.length . RBW.encodeRecordBatchWire) builtBatch100
        ]
    , bgroup
        "RecordBatch encode: gzip"
        [ bench "encodeRecordBatchWireCompressed gzip (10 records)" $
            whnfIO (sizeOfRight =<< RBW.encodeRecordBatchWireCompressed gzipBatch10)
        , bench "encodeRecordBatchWireCompressed gzip (100 records)" $
            whnfIO (sizeOfRight =<< RBW.encodeRecordBatchWireCompressed gzipBatch100)
        ]
    ]


----------------------------------------------------------------------
-- Consumer hot paths
----------------------------------------------------------------------

consumerBench :: Benchmark
consumerBench =
  bgroup
    "Consumer"
    [ bgroup
        "MockConsumer.pollMC (full mock round-trip)"
        [ bench "1 record waiting" $
            whnfIO $
              MC.pollMC sharedConsumer1
        , bench "100 records waiting" $
            whnfIO $
              MC.pollMC sharedConsumer100
        ]
    , bgroup
        "RecordBatch decode (Wire reader; consumer hot path)"
        [ bench "decodeRecordBatchWire (1 record)" $
            nf decodeWire encoded1
        , bench "decodeRecordBatchWire (10 records)" $
            nf decodeWire encoded10
        , bench "decodeRecordBatchWire (100 records)" $
            nf decodeWire encoded100
        ]
    , bgroup
        "RecordBatch decode: sliced vs record (head-to-head)"
        [ bench "record  (10 records)" $ nf decodeWire encoded10
        , bench "sliced  (10 records)" $ nf decodeSliced encoded10
        , bench "record  (100 records)" $ nf decodeWire encoded100
        , bench "sliced  (100 records)" $ nf decodeSliced encoded100
        , bench "record  (1000 records)" $ nf decodeWire encoded1000
        , bench "sliced  (1000 records)" $ nf decodeSliced encoded1000
        ]
    ]


----------------------------------------------------------------------
-- Public group
----------------------------------------------------------------------

benchmarks :: Benchmark
benchmarks =
  bgroup
    "HotPath"
    [ producerBench
    , consumerBench
    ]


----------------------------------------------------------------------
-- Shared setup (one cluster / accumulator / consumer per run)
----------------------------------------------------------------------

{-# NOINLINE sharedMockProducer #-}
sharedMockProducer :: MP.MockProducer
sharedMockProducer = unsafePerformIO $ do
  c <- MC.newMockCluster 1
  MC.createTopic c "topic" 1
  fp <- MC.noFaults
  MP.newMockProducer c fp Nothing


{-# NOINLINE sharedAccumulator #-}
sharedAccumulator :: BA.BatchAccumulator
sharedAccumulator =
  unsafePerformIO $
    BA.createBatchAccumulator
      (16 * 1024 * 1024) -- 16 MiB ceiling so we never split mid-bench
      100_000 -- linger 100 s — never roll over by time
      Compression.NoCompression
      (Compression.defaultLevel Compression.NoCompression)


{-# NOINLINE sharedConsumer1 #-}
sharedConsumer1 :: MC.MockConsumer
sharedConsumer1 = unsafePerformIO (mkConsumer 1)


{-# NOINLINE sharedConsumer100 #-}
sharedConsumer100 :: MC.MockConsumer
sharedConsumer100 = unsafePerformIO (mkConsumer 100)


mkConsumer :: Int -> IO MC.MockConsumer
mkConsumer n = do
  c <- MC.newMockCluster 1
  MC.createTopic c "topic" 1
  -- Seed n records into partition 0 so the first poll has something
  -- to drain. We re-seed inside the benchmark loop because pollMC
  -- consumes the records; for steady-state numbers see the
  -- 'pollMC' bench in the dedicated subgroup above.
  let !payload = BS.replicate 100 0x41
  mapM_
    ( \i ->
        MC.appendToPartition
          c
          "topic"
          0
          (Just "k")
          payload
          (fromIntegral i)
          []
          Nothing
    )
    [0 .. n - 1]
  fp <- MC.noFaults
  cons <- MC.newMockConsumer c fp (MC.GroupId "g") MC.ReadUncommitted (max 1 n)
  MC.subscribeMC cons ["topic"]
  pure cons


----------------------------------------------------------------------
-- Sample data
----------------------------------------------------------------------

payload100, payload1k :: BS.ByteString
payload100 = BS.replicate 100 0x41
payload1k = BS.replicate 1024 0x41


sampleRecord :: RB.Record
sampleRecord =
  RB.Record
    { RB.recordTimestampDelta = 0
    , RB.recordOffsetDelta = 0
    , RB.recordKey = Just "k"
    , RB.recordValue = Just payload100
    , RB.recordHeaders = []
    }


sampleBatch :: Int -> BA.ProducerBatch
sampleBatch n =
  BA.ProducerBatch
    { BA.batchTopicPartition = BA.TopicPartition "topic" 0
    , BA.batchRecords =
        V.fromList
          [ RB.Record 0 (fromIntegral i) (Just "k") (Just payload100) []
          | i <- [0 .. n - 1]
          ]
    , BA.batchSizeBytes = n * 120
    , BA.batchCreateTime = 0
    , BA.batchBaseTimestamp = 0
    , BA.batchState = BA.Ready
    , BA.batchCompression = Compression.NoCompression
    , BA.batchCompressionLevel =
        Compression.defaultLevel Compression.NoCompression
    , BA.batchCallbacks = V.replicate n BA.NoRecordCallback
    , BA.batchAttempts = 0
    , BA.batchProducerId = RB.noProducerId
    , BA.batchProducerEpoch = RB.noProducerEpoch
    , BA.batchBaseSequence = RB.noSequence
    , BA.batchIsTransactional = False
    }


builtBatch1, builtBatch10, builtBatch100 :: RB.RecordBatch
builtBatch1 = Sender.buildRecordBatch (sampleBatch 1)
builtBatch10 = Sender.buildRecordBatch (sampleBatch 10)
builtBatch100 = Sender.buildRecordBatch (sampleBatch 100)


-- Same shape but with the gzip compression bit set on the
-- batch attributes — used by the compressed-encoder bench
-- group below.
gzipBatch10, gzipBatch100 :: RB.RecordBatch
gzipBatch10 = withGzipAttrs builtBatch10
gzipBatch100 = withGzipAttrs builtBatch100


withGzipAttrs :: RB.RecordBatch -> RB.RecordBatch
withGzipAttrs b =
  b
    { RB.batchAttributes =
        (RB.batchAttributes b)
          { RB.attrCompressionType = Compression.Gzip
          }
    }


{- | Force the size of the @Right@ payload so 'whnfIO' actually
measures the encoder work. Without this, criterion only times
'whnfIO' on @Either String ByteString@, which is two
nanoseconds of pattern-matching.
-}
sizeOfRight :: Either e BS.ByteString -> IO Int
sizeOfRight (Left _) = pure 0
sizeOfRight (Right b) = pure $! BS.length b


encoded1, encoded10, encoded100, encoded1000 :: BS.ByteString
encoded1 = RBW.encodeRecordBatchWire builtBatch1
encoded10 = RBW.encodeRecordBatchWire builtBatch10
encoded100 = RBW.encodeRecordBatchWire builtBatch100
encoded1000 = RBW.encodeRecordBatchWire builtBatch1000


builtBatch1000 :: RB.RecordBatch
builtBatch1000 = Sender.buildRecordBatch (sampleBatch 1000)


decodeWire :: BS.ByteString -> Int
decodeWire bs = case RBW.decodeRecordBatchWire bs of
  Left e -> error e
  Right rb -> length (RB.batchRecords rb)


-- forces every record; Vector elements are strict.

decodeSliced :: BS.ByteString -> Int
decodeSliced bs = case RBW.decodeRecordBatchWireSliced bs of
  Left e -> error e
  Right sb -> RBW.slicedRecordCount sb


----------------------------------------------------------------------
-- Loops
----------------------------------------------------------------------

sendNTimes :: MP.MockProducer -> Int -> BS.ByteString -> IO ()
sendNTimes p !n !val = go n
  where
    go 0 = pure ()
    go !k = do
      _ <- MP.sendMockH p "topic" 0 (Just "k") val 0 []
      go (k - 1)


appendNRecords :: BA.BatchAccumulator -> Int -> IO ()
appendNRecords acc !n = go n
  where
    go 0 = pure ()
    go !k = do
      _ <-
        BA.appendRecordStamped
          acc
          (BA.TopicPartition "topic" 0)
          sampleRecord
          BA.NoRecordCallback
          BA.noStamp
      go (k - 1)
