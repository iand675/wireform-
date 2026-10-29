{-# LANGUAGE OverloadedStrings #-}

module Client.ProducerTimeoutSpec (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Data.Int
import Data.Time.Clock.POSIX qualified as Time
import Data.Vector qualified as V
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Kafka.Client.Internal.BatchAccumulator qualified as BA
import Kafka.Client.Internal.ProducerSender qualified as Sender
import Kafka.Client.Producer qualified as Producer
import Kafka.Compression.Types qualified as Compression
import Kafka.Protocol.RecordBatch qualified as RB
import Test.Syd
import Test.Syd.Hedgehog ()


-- | Test that delivery timeout configuration is properly set
prop_deliveryTimeoutConfigured :: Property
prop_deliveryTimeoutConfigured = property $ do
  let config =
        Producer.defaultProducerConfig
          { Producer.producerDeliveryTimeoutMs = 60000
          }
  annotate $ "Delivery timeout: " ++ show (Producer.producerDeliveryTimeoutMs config)
  assert $ Producer.producerDeliveryTimeoutMs config == 60000


-- | Test that default delivery timeout is 120000ms (2 minutes)
unit_defaultDeliveryTimeout :: Spec
unit_defaultDeliveryTimeout = it "Default delivery timeout is 120000ms" $ do
  let config = Producer.defaultProducerConfig
  Producer.producerDeliveryTimeoutMs config `shouldBe` 120000


-- | Test that batch timeout detection works correctly
unit_batchTimeoutDetection :: Spec
unit_batchTimeoutDetection = it "Batch timeout detection" $ do
  -- Create a test batch with a specific creation time
  let currentTime = 10000 -- 10 seconds in ms
      batchCreateTime = 5000 -- Created at 5 seconds
      deliveryTimeout = 3000 -- 3 second timeout

  -- Create a mock batch
  let tp = BA.TopicPartition "test-topic" 0
      record = RB.Record 0 0 Nothing (Just "value") []
      batch =
        BA.ProducerBatch
          { BA.batchTopicPartition = tp
          , BA.batchRecords = V.singleton record
          , BA.batchSizeBytes = 100
          , BA.batchCreateTime = batchCreateTime
          , BA.batchBaseTimestamp = batchCreateTime
          , BA.batchState = BA.Ready
          , BA.batchCompression = Compression.NoCompression
          , BA.batchCompressionLevel = Compression.defaultLevel Compression.NoCompression
          , BA.batchCallbacks = V.empty
          , BA.batchAttempts = 0
          , BA.batchProducerId = RB.noProducerId
          , BA.batchProducerEpoch = RB.noProducerEpoch
          , BA.batchBaseSequence = RB.noSequence
          , BA.batchIsTransactional = False
          }

  -- Test that the batch is detected as timed out
  let timedOut = Sender.isBatchTimedOut currentTime deliveryTimeout batch
      elapsed = currentTime - batchCreateTime

  ( if (timedOut)
      then pure ()
      else
        expectationFailure
          ( "Batch should be timed out (elapsed: "
              ++ show elapsed
              ++ "ms, timeout: "
              ++ show deliveryTimeout
              ++ "ms)"
          )
    )


-- | Test that batch timeout detection doesn't false-positive
unit_batchNotTimedOut :: Spec
unit_batchNotTimedOut = it "Batch not timed out when within timeout" $ do
  -- Create a test batch with a recent creation time
  let currentTime = 10000 -- 10 seconds in ms
      batchCreateTime = 9000 -- Created at 9 seconds (1 second ago)
      deliveryTimeout = 3000 -- 3 second timeout

  -- Create a mock batch
  let tp = BA.TopicPartition "test-topic" 0
      record = RB.Record 0 0 Nothing (Just "value") []
      batch =
        BA.ProducerBatch
          { BA.batchTopicPartition = tp
          , BA.batchRecords = V.singleton record
          , BA.batchSizeBytes = 100
          , BA.batchCreateTime = batchCreateTime
          , BA.batchBaseTimestamp = batchCreateTime
          , BA.batchState = BA.Ready
          , BA.batchCompression = Compression.NoCompression
          , BA.batchCompressionLevel = Compression.defaultLevel Compression.NoCompression
          , BA.batchCallbacks = V.empty
          , BA.batchAttempts = 0
          , BA.batchProducerId = RB.noProducerId
          , BA.batchProducerEpoch = RB.noProducerEpoch
          , BA.batchBaseSequence = RB.noSequence
          , BA.batchIsTransactional = False
          }

  -- Test that the batch is NOT detected as timed out
  let timedOut = Sender.isBatchTimedOut currentTime deliveryTimeout batch
      elapsed = currentTime - batchCreateTime

  ( if (not timedOut)
      then pure ()
      else
        expectationFailure
          ( "Batch should NOT be timed out (elapsed: "
              ++ show elapsed
              ++ "ms, timeout: "
              ++ show deliveryTimeout
              ++ "ms)"
          )
    )


-- | Test property: a batch is timed out iff elapsed time > timeout
prop_timeoutDetectionCorrectness :: Property
prop_timeoutDetectionCorrectness = property $ do
  currentTime <- forAll $ Gen.int64 (Range.linear 10000 1000000)
  batchCreateTime <- forAll $ Gen.int64 (Range.linear 0 (currentTime - 1))
  deliveryTimeoutMs <- forAll $ Gen.int32 (Range.linear 100 10000)

  let elapsed = currentTime - batchCreateTime
      shouldBeTimedOut = elapsed > fromIntegral deliveryTimeoutMs

      tp = BA.TopicPartition "test-topic" 0
      record = RB.Record 0 0 Nothing (Just "value") []
      batch =
        BA.ProducerBatch
          { BA.batchTopicPartition = tp
          , BA.batchRecords = V.singleton record
          , BA.batchSizeBytes = 100
          , BA.batchCreateTime = batchCreateTime
          , BA.batchBaseTimestamp = batchCreateTime
          , BA.batchState = BA.Ready
          , BA.batchCompression = Compression.NoCompression
          , BA.batchCompressionLevel = Compression.defaultLevel Compression.NoCompression
          , BA.batchCallbacks = V.empty
          , BA.batchAttempts = 0
          , BA.batchProducerId = RB.noProducerId
          , BA.batchProducerEpoch = RB.noProducerEpoch
          , BA.batchBaseSequence = RB.noSequence
          , BA.batchIsTransactional = False
          }

      isTimedOut = Sender.isBatchTimedOut currentTime deliveryTimeoutMs batch

  annotate $ "Current time: " ++ show currentTime
  annotate $ "Batch create time: " ++ show batchCreateTime
  annotate $ "Elapsed: " ++ show elapsed ++ "ms"
  annotate $ "Timeout: " ++ show deliveryTimeoutMs ++ "ms"
  annotate $ "Should be timed out: " ++ show shouldBeTimedOut
  annotate $ "Is timed out: " ++ show isTimedOut

  isTimedOut === shouldBeTimedOut


-- | Test that very old batches are always timed out
prop_veryOldBatchesTimeout :: Property
prop_veryOldBatchesTimeout = property $ do
  currentTime <- forAll $ Gen.int64 (Range.linear 200000 1000000)
  deliveryTimeoutMs <- forAll $ Gen.int32 (Range.linear 1000 10000)

  -- Create a batch that's much older than the timeout
  let batchCreateTime = currentTime - (fromIntegral deliveryTimeoutMs * 10)

      tp = BA.TopicPartition "test-topic" 0
      record = RB.Record 0 0 Nothing (Just "value") []
      batch =
        BA.ProducerBatch
          { BA.batchTopicPartition = tp
          , BA.batchRecords = V.singleton record
          , BA.batchSizeBytes = 100
          , BA.batchCreateTime = batchCreateTime
          , BA.batchBaseTimestamp = batchCreateTime
          , BA.batchState = BA.Ready
          , BA.batchCompression = Compression.NoCompression
          , BA.batchCompressionLevel = Compression.defaultLevel Compression.NoCompression
          , BA.batchCallbacks = V.empty
          , BA.batchAttempts = 0
          , BA.batchProducerId = RB.noProducerId
          , BA.batchProducerEpoch = RB.noProducerEpoch
          , BA.batchBaseSequence = RB.noSequence
          , BA.batchIsTransactional = False
          }

      isTimedOut = Sender.isBatchTimedOut currentTime deliveryTimeoutMs batch

  annotate $ "Batch is 10x older than timeout"
  assert isTimedOut


-- | Test that fresh batches are never timed out
prop_freshBatchesNeverTimeout :: Property
prop_freshBatchesNeverTimeout = property $ do
  currentTime <- forAll $ Gen.int64 (Range.linear 100000 1000000)
  deliveryTimeoutMs <- forAll $ Gen.int32 (Range.linear 5000 30000)

  -- Create a batch that was just created (within 10% of timeout)
  let recentDelta = fromIntegral deliveryTimeoutMs `div` 10
  batchCreateTime <- forAll $ Gen.int64 (Range.linear (currentTime - recentDelta) currentTime)

  let tp = BA.TopicPartition "test-topic" 0
      record = RB.Record 0 0 Nothing (Just "value") []
      batch =
        BA.ProducerBatch
          { BA.batchTopicPartition = tp
          , BA.batchRecords = V.singleton record
          , BA.batchSizeBytes = 100
          , BA.batchCreateTime = batchCreateTime
          , BA.batchBaseTimestamp = batchCreateTime
          , BA.batchState = BA.Ready
          , BA.batchCompression = Compression.NoCompression
          , BA.batchCompressionLevel = Compression.defaultLevel Compression.NoCompression
          , BA.batchCallbacks = V.empty
          , BA.batchAttempts = 0
          , BA.batchProducerId = RB.noProducerId
          , BA.batchProducerEpoch = RB.noProducerEpoch
          , BA.batchBaseSequence = RB.noSequence
          , BA.batchIsTransactional = False
          }

      isTimedOut = Sender.isBatchTimedOut currentTime deliveryTimeoutMs batch
      elapsed = currentTime - batchCreateTime

  annotate $ "Elapsed: " ++ show elapsed ++ "ms (should be < 10% of timeout)"
  assert $ not isTimedOut


-- | Test different timeout values
prop_differentTimeoutValues :: Property
prop_differentTimeoutValues = property $ do
  timeoutMs <- forAll $ Gen.int32 (Range.linear 100 300000)

  let config =
        Producer.defaultProducerConfig
          { Producer.producerDeliveryTimeoutMs = fromIntegral timeoutMs
          }

  Producer.producerDeliveryTimeoutMs config === fromIntegral timeoutMs


tests :: Spec
tests =
  describe "Producer Timeout (KIP-91)" $
    sequence_
      [ describe "Properties" $
          sequence_
            [ it "Delivery timeout is configured" prop_deliveryTimeoutConfigured
            , it "Timeout detection correctness" prop_timeoutDetectionCorrectness
            , it "Very old batches timeout" prop_veryOldBatchesTimeout
            , it "Fresh batches never timeout" prop_freshBatchesNeverTimeout
            , it "Different timeout values" prop_differentTimeoutValues
            ]
      , describe "Unit Tests" $
          sequence_
            [ unit_defaultDeliveryTimeout
            , unit_batchTimeoutDetection
            , unit_batchNotTimedOut
            ]
      ]
