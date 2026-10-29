{-# LANGUAGE OverloadedStrings #-}

module Client.BatchAccumulatorSpec (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait)
import Control.Monad (replicateM, replicateM_, unless, when)
import Data.ByteString qualified as BS
import Data.IORef (
  IORef,
  atomicModifyIORef',
  newIORef,
  readIORef,
 )
import Data.Int
import Data.Set qualified as Set
import Data.Vector qualified as V
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Kafka.Client.Internal.BatchAccumulator qualified as BA
import Kafka.Compression.Types qualified as Compression
import Kafka.Protocol.RecordBatch qualified as RB
import Kafka.Time qualified as KafkaTime
import System.Timeout (timeout)
import Test.Syd
import Test.Syd.Hedgehog ()


-- | Generate a simple record for testing
genRecord :: Gen RB.Record
genRecord = do
  timestampDelta <- Gen.int64 (Range.linear 0 1000)
  offsetDelta <- Gen.int32 (Range.linear 0 100)
  key <- Gen.maybe (Gen.bytes (Range.linear 0 100))
  value <- Gen.bytes (Range.linear 0 1000)
  return $ RB.Record timestampDelta offsetDelta key (Just value) []


-- | Generate a topic-partition
genTopicPartition :: Gen BA.TopicPartition
genTopicPartition = do
  topic <- Gen.text (Range.linear 1 10) Gen.alphaNum
  partition <- Gen.int32 (Range.linear 0 10)
  return $ BA.TopicPartition topic partition


-- | Test that records can be appended to the accumulator
prop_canAppendRecords :: Property
prop_canAppendRecords = property $ do
  record <- forAll genRecord
  tp <- forAll genTopicPartition

  accumulator <- evalIO $ BA.createBatchAccumulator 16384 100 Compression.NoCompression (Compression.defaultLevel Compression.NoCompression)

  -- Append a record
  success <- evalIO $ BA.appendRecord accumulator tp record
  assert success

  -- Close accumulator
  evalIO $ BA.closeBatchAccumulator accumulator


-- | Test that batches become ready when they reach size limit
prop_batchReadyWhenFull :: Property
prop_batchReadyWhenFull = property $ do
  -- Use a small batch size to make it easy to fill
  let batchSize = 100
  accumulator <- evalIO $ BA.createBatchAccumulator batchSize 10000 Compression.NoCompression (Compression.defaultLevel Compression.NoCompression)

  tp <- forAll genTopicPartition

  -- Create records that will fill the batch
  let largeValue = BS.replicate 50 42 -- 50 bytes
      largeRecord = RB.Record 0 0 Nothing (Just largeValue) []

  -- Append enough records to fill a batch (with overhead, 3 records should do it)
  evalIO $ replicateM_ 3 $ BA.appendRecord accumulator tp largeRecord

  -- Check that we have ready batches
  hasReady <- evalIO $ BA.hasReadyBatches accumulator
  assert hasReady

  -- Drain batches
  batches <- evalIO $ BA.drainReadyBatches accumulator
  annotate $ "Got " ++ show (length batches) ++ " batches"
  assert $ not (null batches)

  evalIO $ BA.closeBatchAccumulator accumulator


{- | Test that batches become ready once the linger time has elapsed,
and never before. Readiness is judged against the wall clock, so the
test does not sleep and then hope: it polls (bounded by a timeout)
until the batch is ready and checks that at least @lingerMs@ passed
since before the append. The "not yet ready" side uses a linger long
enough that no scheduling delay can reach it.
-}
prop_batchReadyAfterLingerTime :: Property
prop_batchReadyAfterLingerTime = property $ do
  lingerMs <- forAll $ Gen.int (Range.linear 0 10)
  tp <- forAll genTopicPartition
  record <- forAll genRecord
  let level = Compression.defaultLevel Compression.NoCompression

  -- A batch under a one-minute linger is not ready right after the append.
  waiting <- evalIO $ BA.createBatchAccumulator 16384 60000 Compression.NoCompression level
  evalIO $ BA.appendRecord waiting tp record
  hasReadyBefore <- evalIO $ BA.hasReadyBatches waiting
  assert $ not hasReadyBefore
  evalIO $ BA.closeBatchAccumulator waiting

  accumulator <- evalIO $ BA.createBatchAccumulator 16384 lingerMs Compression.NoCompression level
  beforeAppend <- evalIO KafkaTime.currentTimeMillis
  evalIO $ BA.appendRecord accumulator tp record
  ready <- evalIO $ timeout 10_000_000 (awaitReady accumulator)
  readyObservedAt <- evalIO KafkaTime.currentTimeMillis
  ready === Just ()
  annotateShow (readyObservedAt - beforeAppend)
  assert $ readyObservedAt - beforeAppend >= fromIntegral lingerMs

  -- Can drain the batch
  batches <- evalIO $ BA.drainReadyBatches accumulator
  assert $ not (null batches)

  evalIO $ BA.closeBatchAccumulator accumulator
  where
    awaitReady acc = do
      r <- BA.hasReadyBatches acc
      unless r $ threadDelay 200 >> awaitReady acc


-- | Test that records are accumulated per partition
prop_perPartitionAccumulation :: Property
prop_perPartitionAccumulation = property $ do
  accumulator <- evalIO $ BA.createBatchAccumulator 16384 10000 Compression.NoCompression (Compression.defaultLevel Compression.NoCompression)

  -- Create two different partitions
  tp1 <- forAll genTopicPartition
  tp2 <- forAll genTopicPartition

  -- Ensure they're different
  when (tp1 == tp2) discard

  record <- forAll genRecord

  -- Append records to both partitions
  evalIO $ BA.appendRecord accumulator tp1 record
  evalIO $ BA.appendRecord accumulator tp2 record

  -- Close accumulator to mark all batches as ready
  evalIO $ BA.closeBatchAccumulator accumulator

  -- Drain batches
  batches <- evalIO $ BA.drainReadyBatches accumulator

  -- Should have at least 2 batches (one per partition)
  annotate $ "Got " ++ show (length batches) ++ " batches"
  assert $ length batches >= 2


-- | Test concurrent appends from multiple threads
prop_concurrentAppends :: Property
prop_concurrentAppends = property $ do
  accumulator <- evalIO $ BA.createBatchAccumulator 16384 100 Compression.NoCompression (Compression.defaultLevel Compression.NoCompression)

  tp <- forAll genTopicPartition

  -- Create multiple threads appending concurrently
  let numThreads = 10
      recordsPerThread = 10

  evalIO $ do
    threads <- replicateM numThreads $ async $ do
      replicateM_ recordsPerThread $ do
        let record = RB.Record 0 0 Nothing (Just "test") []
        BA.appendRecord accumulator tp record

    -- Wait for all threads to complete
    mapM_ wait threads

    -- Close and drain
    BA.closeBatchAccumulator accumulator
    batches <- BA.drainReadyBatches accumulator

    -- Count total records across all batches
    let totalRecords = sum $ map (V.length . BA.batchRecords) batches

    -- Should have all records
    return $ totalRecords == numThreads * recordsPerThread

  success


-- | Test that closing the accumulator marks all batches as ready
prop_closeMarksAllReady :: Property
prop_closeMarksAllReady = property $ do
  accumulator <- evalIO $ BA.createBatchAccumulator 16384 10000 Compression.NoCompression (Compression.defaultLevel Compression.NoCompression)

  tp <- forAll genTopicPartition
  record <- forAll genRecord

  -- Append a record
  evalIO $ BA.appendRecord accumulator tp record

  -- Close accumulator
  evalIO $ BA.closeBatchAccumulator accumulator

  -- Should have ready batches
  hasReady <- evalIO $ BA.hasReadyBatches accumulator
  assert hasReady

  -- Can drain them
  batches <- evalIO $ BA.drainReadyBatches accumulator
  assert $ not (null batches)


-- | Test that appending after close returns False
prop_appendAfterCloseReturnsFalse :: Property
prop_appendAfterCloseReturnsFalse = property $ do
  accumulator <- evalIO $ BA.createBatchAccumulator 16384 100 Compression.NoCompression (Compression.defaultLevel Compression.NoCompression)

  tp <- forAll genTopicPartition
  record <- forAll genRecord

  -- Close accumulator
  evalIO $ BA.closeBatchAccumulator accumulator

  -- Try to append after close
  success <- evalIO $ BA.appendRecord accumulator tp record
  assert $ not success


-- | Test batch state transitions
prop_batchStateTransitions :: Property
prop_batchStateTransitions = property $ do
  accumulator <- evalIO $ BA.createBatchAccumulator 100 10000 Compression.NoCompression (Compression.defaultLevel Compression.NoCompression)

  tp <- forAll genTopicPartition

  -- Create large records to fill batch quickly
  let largeRecord = RB.Record 0 0 Nothing (Just (BS.replicate 50 42)) []

  -- Append records to fill batch
  evalIO $ replicateM_ 3 $ BA.appendRecord accumulator tp largeRecord

  -- Drain batches
  batches <- evalIO $ BA.drainReadyBatches accumulator

  -- All drained batches should be in Ready state
  assert $ all (\b -> BA.batchState b == BA.Ready) batches

  evalIO $ BA.closeBatchAccumulator accumulator


{- | Regression test for the seal-vs-append race.

Pre-fix shape: the producer would call @atomicModifyIORef'
baHotState@ to incorporate its record /after/ a concurrent
sealer had already swapped the partition's currentBatch to
'Nothing' AND read the hot state for the snapshot. The
producer's CAS would succeed on the now-orphaned batch — the
record reported as "appended" but never made it into any
ready batch.

Post-fix shape ('bhSealed' interlock + 'casAppend' /
'snapshotBatch' contending on the same 'IORef'): every record
whose append CAS reports success ('AppendedSize') /must/
appear in some sealed batch, because either it landed before
the seal CAS (included in the snapshot) or it landed on a
fresh batch the slow path installed (included in some later
snapshot).

The test:
  * tiny batch-size limit so each record triggers a seal-on-
    size in the producer thread;
  * a parallel "sealer" thread continuously calls
    'drainReadyBatches' (which also seals the in-progress
    batch on linger expiry) so it races the producer's
    seal-on-size on every iteration;
  * after both threads finish, drain the rest and assert the
    bag of recordValues we received from the accumulator is
    /exactly/ the bag of recordValues we asked it to accept.
    A silently dropped record would be in the asked-for bag
    but not the received-from bag.
-}
prop_appendAndSealRaceIsLossless :: Property
prop_appendAndSealRaceIsLossless = withTests 50 . property $ do
  -- Record count + batch-size limit are picked so we exercise the
  -- race window many times per run: tiny limit -> nearly every
  -- record seals; concurrent drain thread sees several sealed
  -- batches per loop iteration.
  let !nRecords = 4000
      !batchSizeCap = 64 -- forces seal after ~1-2 records
      !lingerMs = 1 -- linger thread sees seals constantly
      !payloadBytes = 24
      !tp = BA.TopicPartition "race" 0

  acc <-
    evalIO $
      BA.createBatchAccumulator
        batchSizeCap
        lingerMs
        Compression.NoCompression
        (Compression.defaultLevel Compression.NoCompression)

  -- Drained-batches accumulator and a "stop draining" flag the
  -- producer flips when it's done appending. This avoids
  -- 'threadDelay' in the test (per the project's no-flaky-test
  -- rule).
  drained <- evalIO $ newIORef ([] :: [BA.ProducerBatch])
  done <- evalIO $ newIORef False

  evalIO $ do
    drainAsync <- async $ do
      let loop = do
            bs <- BA.drainReadyBatches acc
            unless (null bs) $
              atomicModifyIORef' drained $
                \xs -> (bs ++ xs, ())
            isDone <- readIORef done
            if isDone
              then do
                -- Final drain after producer signalled done.
                final <- BA.drainReadyBatches acc
                unless (null final) $
                  atomicModifyIORef' drained $
                    \xs -> (final ++ xs, ())
              else loop
      loop

    producerAsync <- async $ do
      let payloadFor i = BS.pack (replicate payloadBytes (fromIntegral (i `mod` 251)))
          appendOne i = do
            let !record =
                  RB.Record
                    { RB.recordTimestampDelta = 0
                    , RB.recordOffsetDelta = fromIntegral i
                    , RB.recordKey = Nothing
                    , RB.recordValue = Just (payloadFor i)
                    , RB.recordHeaders = []
                    }
            ok <- BA.appendRecordStampedUnsafe acc tp record BA.NoRecordCallback BA.noStamp
            unless ok $ error "append rejected mid-test"
          go !i
            | i >= nRecords = pure ()
            | otherwise = do
                appendOne i
                go (i + 1)
      go 0
      atomicModifyIORef' done $ \_ -> (True, ())

    wait producerAsync
    wait drainAsync
    BA.closeBatchAccumulator acc
    finalBatches <- BA.drainReadyBatches acc
    atomicModifyIORef' drained $ \xs -> (finalBatches ++ xs, ())

  collected <- evalIO $ readIORef drained
  let receivedRecords =
        concatMap (V.toList . BA.batchRecords) collected
      receivedKeys =
        Set.fromList (map RB.recordOffsetDelta receivedRecords)
      expectedKeys =
        Set.fromList [fromIntegral i | i <- [0 .. nRecords - 1] :: [Int32]]
      missing = expectedKeys `Set.difference` receivedKeys
      duplicates = length receivedRecords - Set.size receivedKeys

  annotate $ "drained " <> show (length collected) <> " batches"
  annotate $ "received " <> show (length receivedRecords) <> " records"
  annotate $ "missing offsets count: " <> show (Set.size missing)
  annotate $ "duplicate count: " <> show duplicates
  Set.size missing === 0
  duplicates === 0


-- | All tests for BatchAccumulator
tests :: Spec
tests =
  describe "BatchAccumulator" $
    sequence_
      [ it "Can append records" prop_canAppendRecords
      , it "Batch ready when full" prop_batchReadyWhenFull
      , it "Batch ready after linger time" prop_batchReadyAfterLingerTime
      , it "Per-partition accumulation" prop_perPartitionAccumulation
      , it "Concurrent appends" prop_concurrentAppends
      , it "Close marks all ready" prop_closeMarksAllReady
      , it "Append after close returns false" prop_appendAfterCloseReturnsFalse
      , it "Batch state transitions" prop_batchStateTransitions
      , it "Append-vs-seal race is lossless" prop_appendAndSealRaceIsLossless
      ]
