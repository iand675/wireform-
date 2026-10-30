{-# LANGUAGE OverloadedStrings #-}

{- | Benchmarks for the new librdkafka-comparable surfaces:

  * Stats JSON snapshot encoding (per-second @stats_cb@ shape).
  * BatchStamp + record-batch building (the hot loop the
    producer exercises before every send).
-}
module Benchmarks.StatsAndStamping (benchmarks) where

import Criterion (Benchmark, bench, bgroup, nf, whnf)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Vector qualified as V
import Kafka.Client.Internal.BatchAccumulator qualified as BA
import Kafka.Client.Internal.ProducerSender qualified as Sender
import Kafka.Compression.Types qualified as Compression
import Kafka.Protocol.RecordBatch qualified as RB
import Kafka.Protocol.RecordBatchWire qualified as RBW
import Kafka.Telemetry.StatsJson qualified as Stats


benchmarks :: Benchmark
benchmarks =
  bgroup
    "StatsAndStamping"
    [ bgroup
        "stats-json"
        [ bench "render small (no topics)" $
            nf Stats.renderStats smallSnapshot
        , bench "render medium (10 topics)" $
            nf Stats.renderStats mediumSnapshot
        ]
    , bgroup
        "record-batch"
        [ bench "buildRecordBatch (1 record, non-txn)" $
            whnf Sender.buildRecordBatch (sampleBatch False 1)
        , bench "buildRecordBatch (32 records, non-txn)" $
            whnf Sender.buildRecordBatch (sampleBatch False 32)
        , bench "buildRecordBatch (32 records, txn)" $
            whnf Sender.buildRecordBatch (sampleBatch True 32)
        ]
    , bgroup
        "encode-record-batch"
        [ bench "encodeRecordBatchWire (1 record)" $
            nf RBW.encodeRecordBatchWire (Sender.buildRecordBatch (sampleBatch False 1))
        , bench "encodeRecordBatchWire (32 records)" $
            nf RBW.encodeRecordBatchWire (Sender.buildRecordBatch (sampleBatch False 32))
        ]
    ]


smallSnapshot :: Stats.StatsSnapshot
smallSnapshot = Stats.defaultSnapshot "wfkafka" "client-1" Stats.StatsProducer


mediumSnapshot :: Stats.StatsSnapshot
mediumSnapshot =
  smallSnapshot
    { Stats.ssTopics =
        Map.fromList
          [ ( "t" <> tshow i
            , Stats.TopicStats
                ("t" <> tshow i)
                (fromIntegral i)
                (fromIntegral (i * 2))
                (fromIntegral (i * 1024))
                (fromIntegral (i * 1024))
                0
            )
          | i <- [0 .. 9 :: Int]
          ]
    }
  where
    tshow = T.pack . show


sampleBatch :: Bool -> Int -> BA.ProducerBatch
sampleBatch isTxn n =
  BA.ProducerBatch
    { BA.batchTopicPartition = BA.TopicPartition "t" 0
    , BA.batchRecords =
        V.fromList
          [ RB.Record 0 (fromIntegral i) (Just "k") (Just "v") []
          | i <- [0 .. n - 1]
          ]
    , BA.batchSizeBytes = n * 50
    , BA.batchCreateTime = 0
    , BA.batchBaseTimestamp = 0
    , BA.batchState = BA.Ready
    , BA.batchCompression = Compression.NoCompression
    , BA.batchCompressionLevel =
        Compression.defaultLevel Compression.NoCompression
    , BA.batchCallbacks = V.replicate n BA.NoRecordCallback
    , BA.batchAttempts = 0
    , BA.batchProducerId = if isTxn then 12345 else RB.noProducerId
    , BA.batchProducerEpoch = if isTxn then 7 else RB.noProducerEpoch
    , BA.batchBaseSequence = if isTxn then 0 else RB.noSequence
    , BA.batchIsTransactional = isTxn
    }
