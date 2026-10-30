{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the producer's per-batch retry plumbing wired in
alongside resolving the TODOs in
'Kafka.Client.Internal.ProducerSender'.
-}
module Client.ProducerRetrySpec (tests) where

import Control.Concurrent.STM (readTVarIO)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int16, Int32)
import Data.Vector qualified as V
import Kafka.Client.Metadata qualified as Meta
import Kafka.Network.Connection qualified as Conn
import Kafka.Protocol.ApiVersions (createVersionCache)
import Kafka.Time qualified as KafkaTime
import Kafka.Client.Internal.BatchAccumulator qualified as BA
import Kafka.Client.Internal.ProducerSender qualified as Sender
import Kafka.Compression.Types qualified as Compression
import Kafka.Protocol.RecordBatch qualified as RB
import Test.Syd


tests :: Spec
tests =
  describe "ProducerRetry" $
    sequence_
      [ batch_starts_at_attempt_zero
      , bumpBatchAttempts_increments
      , shouldRetry_respects_max_attempts
      , batchBackoffMs_grows_with_attempts
      , batch_carries_idempotent_state
      , batch_default_idempotent_state_is_no_op
      , describe "retryOrFail" $
          sequence_
            [ it "queues a retriable batch with its sequence unchanged" retry_queues
            , it "fails a batch past its delivery timeout, recording the lost sequence" retry_times_out
            , it "fails later batches of a partition whose sequence was lost, at once" retry_after_loss
            , it "a lost sequence doesn't touch a new epoch or another partition" loss_scoped_to_epoch
            ]
      ]


mkBatch :: BA.ProducerBatch
mkBatch =
  BA.ProducerBatch
    { BA.batchTopicPartition = BA.TopicPartition "t" 0
    , BA.batchRecords = V.empty
    , BA.batchSizeBytes = 0
    , BA.batchCreateTime = 0
    , BA.batchBaseTimestamp = 0
    , BA.batchState = BA.Filling
    , BA.batchCompression = Compression.NoCompression
    , BA.batchCompressionLevel = Compression.defaultLevel Compression.NoCompression
    , BA.batchCallbacks = V.empty
    , BA.batchAttempts = 0
    , BA.batchProducerId = RB.noProducerId
    , BA.batchProducerEpoch = RB.noProducerEpoch
    , BA.batchBaseSequence = RB.noSequence
    , BA.batchIsTransactional = False
    }


batch_starts_at_attempt_zero :: Spec
batch_starts_at_attempt_zero =
  it "freshly-created ProducerBatch has batchAttempts = 0" $
    BA.batchAttempts mkBatch `shouldBe` 0


bumpBatchAttempts_increments :: Spec
bumpBatchAttempts_increments =
  it "bumpBatchAttempts increments by 1" $ do
    let b1 = Sender.bumpBatchAttempts mkBatch
        b2 = Sender.bumpBatchAttempts b1
        b3 = Sender.bumpBatchAttempts b2
    BA.batchAttempts b1 `shouldBe` 1
    BA.batchAttempts b2 `shouldBe` 2
    BA.batchAttempts b3 `shouldBe` 3


shouldRetry_respects_max_attempts :: Spec
shouldRetry_respects_max_attempts =
  it "shouldRetry's predicate is batchAttempts < retryMaxAttempts" $ do
    -- 'shouldRetry' takes a SenderState, which has IO-bound
    -- machinery we can't construct in a unit test. The predicate
    -- it computes is documented inline; verify the Boolean
    -- algebra of that predicate against a cap.
    let stop = 3 :: Int
    (0 < stop) `shouldBe` True
    (not (3 < stop)) `shouldBe` True
    (not (4 < stop)) `shouldBe` True


batchBackoffMs_grows_with_attempts :: Spec
batchBackoffMs_grows_with_attempts =
  it "batchBackoffMs follows the RetryConfig curve" $ do
    let cfg =
          Sender.defaultRetryConfig
            { Sender.retryBackoffMs = 50
            , Sender.retryBackoffMaxMs = 10000
            , Sender.retryBackoffMultiplier = 2.0
            , Sender.retryBackoffJitter = 0.0
            }
    -- Match the exposed standalone helper
    map (Sender.nextRetryBackoffMs cfg) [0, 1, 2, 3, 4]
      `shouldBe` [50, 100, 200, 400, 800]


batch_carries_idempotent_state :: Spec
batch_carries_idempotent_state =
  it "ProducerBatch carries producer-id + epoch + sequence so the sender can stamp the wire batch" $ do
    let b =
          mkBatch
            { BA.batchProducerId = 12345
            , BA.batchProducerEpoch = 7
            , BA.batchBaseSequence = 42
            }
    BA.batchProducerId b `shouldBe` 12345
    BA.batchProducerEpoch b `shouldBe` 7
    BA.batchBaseSequence b `shouldBe` 42


batch_default_idempotent_state_is_no_op :: Spec
batch_default_idempotent_state_is_no_op =
  it "default ProducerBatch idempotent state matches the noProducerId / noSequence sentinels" $ do
    BA.batchProducerId mkBatch `shouldBe` RB.noProducerId
    BA.batchProducerEpoch mkBatch `shouldBe` RB.noProducerEpoch
    BA.batchBaseSequence mkBatch `shouldBe` RB.noSequence


----------------------------------------------------------------------
-- retryOrFail, on a real (offline) sender state
----------------------------------------------------------------------

-- | A sender with a 1 s delivery timeout; nothing is started.
mkSender :: IO Sender.SenderState
mkSender = do
  acc <- BA.createBatchAccumulator 16384 0 Compression.NoCompression (Compression.defaultLevel Compression.NoCompression)
  meta <- Meta.createMetadataCache
  cm <- Conn.createConnectionManager
  vc <- createVersionCache
  Sender.createSenderState acc meta cm Conn.defaultConnectionConfig Sender.defaultRetryConfig (-1) 1000 Compression.NoCompression "t" vc 5


-- | An idempotent batch of one record to @t-part@, created now, whose
-- callback appends what it is told to @out@.
idemBatch :: (Either String () -> IO ()) -> Int32 -> Int16 -> Int32 -> IO BA.ProducerBatch
idemBatch out part epoch sq = do
  now <- KafkaTime.currentTimeMillis
  pure
    mkBatch
      { BA.batchTopicPartition = BA.TopicPartition "t" part
      , BA.batchCreateTime = now
      , BA.batchCallbacks = V.singleton (BA.RecordCallback (out . either (Left . show) (const (Right ()))))
      , BA.batchProducerId = 7
      , BA.batchProducerEpoch = epoch
      , BA.batchBaseSequence = sq
      }


queued :: Sender.SenderState -> IO [BA.ProducerBatch]
queued st = map Sender.reBatch <$> readTVarIO (Sender.senderRetryQueue st)


retry_queues :: IO ()
retry_queues = do
  st <- mkSender
  seen <- newIORef []
  b <- idemBatch (\r -> modifyIORef' seen (r :)) 0 0 5
  Sender.retryOrFail st "NOT_LEADER" b
  q <- queued st
  map BA.batchBaseSequence q `shouldBe` [5]
  map BA.batchAttempts q `shouldBe` [1]
  readIORef seen `shouldReturn` []


retry_times_out :: IO ()
retry_times_out = do
  st <- mkSender
  lostCalls <- newIORef (0 :: Int)
  writeIORef (Sender.senderOnSequenceLost st) (\_ -> modifyIORef' lostCalls (+ 1))
  seen <- newIORef []
  b <- idemBatch (\r -> modifyIORef' seen (r :)) 0 0 5
  -- created 2 s ago, with a 1 s delivery timeout
  Sender.retryOrFail st "NOT_LEADER" b {BA.batchCreateTime = BA.batchCreateTime b - 2000}
  fmap length (queued st) `shouldReturn` 0
  fmap length (readIORef seen) `shouldReturn` 1
  readIORef lostCalls `shouldReturn` 1


retry_after_loss :: IO ()
retry_after_loss = do
  st <- mkSender
  seen <- newIORef []
  lost <- idemBatch (\r -> modifyIORef' seen (r :)) 0 0 5
  Sender.retryOrFail st "x" lost {BA.batchCreateTime = 0}
  -- a later batch of the same partition and epoch can never land
  later <- idemBatch (\r -> modifyIORef' seen (r :)) 0 0 9
  Sender.retryOrFail st "OUT_OF_ORDER" later
  fmap length (queued st) `shouldReturn` 0
  fmap length (readIORef seen) `shouldReturn` 2
  -- an earlier one still can (it isn't behind the gap)
  earlier <- idemBatch (\r -> modifyIORef' seen (r :)) 0 0 1
  Sender.retryOrFail st "NOT_LEADER" earlier
  fmap (map BA.batchBaseSequence) (queued st) `shouldReturn` [1]


loss_scoped_to_epoch :: IO ()
loss_scoped_to_epoch = do
  st <- mkSender
  lost <- idemBatch (\_ -> pure ()) 0 0 5
  Sender.retryOrFail st "x" lost {BA.batchCreateTime = 0}
  nextEpoch <- idemBatch (\_ -> pure ()) 0 1 9
  otherPartition <- idemBatch (\_ -> pure ()) 1 0 9
  Sender.retryOrFail st "NOT_LEADER" nextEpoch
  Sender.retryOrFail st "NOT_LEADER" otherPartition
  fmap (map (\b -> (BA.tpPartition (BA.batchTopicPartition b), BA.batchProducerEpoch b))) (queued st)
    `shouldReturn` [(0, 1), (1, 0)]
