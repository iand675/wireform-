{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Conformance.T0086.PurgeLocal
Description : librdkafka @tests\/0086-purge_local.c@

librdkafka's @0086-purge_local@ enqueues messages into the producer
without a broker, calls @rd_kafka_purge@, and asserts that the
producer's queue is drained without leaking memory. Our local
analogue is 'Kafka.Client.Producer.purgeProducer', whose core queue
operation lives in the in-memory batch accumulator.
-}
module Conformance.T0086.PurgeLocal (tests) where

import Data.ByteString qualified as BS
import Data.HashMap.Strict qualified as HashMap
import Data.IORef
import Data.Text (Text)
import Kafka.Client.Internal.BatchAccumulator qualified as BA
import Kafka.Compression.Types qualified as Compression
import Kafka.Protocol.RecordBatch qualified as RB
import Test.Syd


tests :: Spec
tests =
  describe "0086-purge_local" $
    sequence_
      [ it "fresh accumulator has no ready batches" $ do
          acc <-
            BA.createBatchAccumulator
              16384 -- 16 KB batch size
              5 -- 5 ms linger
              Compression.NoCompression
              0 -- compression level
          ready <- BA.hasReadyBatches acc
          ready `shouldBe` False
      , it "appendRecord succeeds on an open accumulator" $ do
          acc <- BA.createBatchAccumulator 16384 5 Compression.NoCompression 0
          let tp = BA.TopicPartition "purge-test" 0
              payload = BS.replicate 64 0x41
              rec =
                RB.Record
                  { RB.recordTimestampDelta = 0
                  , RB.recordOffsetDelta = 0
                  , RB.recordKey = Nothing
                  , RB.recordValue = Just payload
                  , RB.recordHeaders = []
                  }
          ok <- BA.appendRecord acc tp rec
          ok `shouldBe` True
      , it "appendRecord fails on a closed accumulator (purge equivalent)" $ do
          acc <- BA.createBatchAccumulator 16384 5 Compression.NoCompression 0
          BA.closeBatchAccumulator acc
          let tp = BA.TopicPartition "purge-test" 0
              rec = RB.Record 0 0 Nothing (Just (BS.replicate 32 0x42)) []
          ok <- BA.appendRecord acc tp rec
          ok `shouldBe` False
      , it "drainReadyBatches after close returns the buffered batch" $ do
          acc <- BA.createBatchAccumulator 16384 5 Compression.NoCompression 0
          let tp = BA.TopicPartition "purge-test" 0
              rec = RB.Record 0 0 Nothing (Just (BS.replicate 64 0x43)) []
          _ <- BA.appendRecord acc tp rec
          BA.closeBatchAccumulator acc
          drained <- BA.drainReadyBatches acc
          length drained `shouldBe` 1
      , it "purgePendingBatches drops filling records, fails callbacks, and stays open" $ do
          acc <- BA.createBatchAccumulator 16384 5000 Compression.NoCompression 0
          events <- newIORef []
          let tp = BA.TopicPartition "purge-test" 0
              rec = RB.Record 0 0 Nothing (Just (BS.replicate 64 0x44)) []
              reason = "local purge" :: Text
              callback result = modifyIORef' events (result :)
          ok <- BA.appendRecordWithCallback acc tp rec (BA.RecordCallback callback)
          ok `shouldBe` True
          counts <- BA.purgePendingBatches acc reason
          HashMap.lookup tp counts `shouldBe` Just 1
          readIORef events >>= (`shouldBe` [Left reason])
          readyAfterPurge <- BA.hasReadyBatches acc
          readyAfterPurge `shouldBe` False
          reopened <- BA.appendRecord acc tp rec
          reopened `shouldBe` True
      , it "purgePendingBatches removes ready and filling batches per partition" $ do
          acc <- BA.createBatchAccumulator 96 5000 Compression.NoCompression 0
          let tp0 = BA.TopicPartition "purge-test" 0
              tp1 = BA.TopicPartition "purge-test" 1
              rec = RB.Record 0 0 Nothing (Just (BS.replicate 80 0x45)) []
          _ <- BA.appendRecord acc tp0 rec
          _ <- BA.appendRecord acc tp0 rec
          _ <- BA.appendRecord acc tp1 rec
          counts <- BA.purgePendingBatches acc "local purge"
          HashMap.lookup tp0 counts `shouldBe` Just 2
          HashMap.lookup tp1 counts `shouldBe` Just 1
          drained <- BA.drainReadyBatches acc
          length drained `shouldBe` 0
      ]
