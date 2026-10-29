{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Kafka.Streams.Internal.RecordCollector
Description : Per-task record collector — accumulates sink emissions
              until the next commit

The Java equivalent is @StreamsRecordCollector@. Sink processors
never write to the producer directly; they hand their records to
the collector, which is owned by the task. At commit time the
runtime atomically:

  1. flushes all attached state stores
  2. flushes the collector to the producer (block until ack)
  3. commits consumer offsets
  4. (EOS) calls @commitTransaction@

Inside 'TopologyTestDriver' the collector simply enqueues records
to in-memory per-topic FIFOs; the test harness drains them via
'readOutput'.
-}
module Kafka.Streams.Internal.RecordCollector (
  RecordCollector (..),
  inMemoryCollector,
  drainCollector,
  recordsForTopic,
  collectorHasPending,
  CollectedRecord (..),
  crValueBytes,
) where

import Control.Concurrent.STM
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Kafka.Streams.Time (Timestamp)
import Kafka.Streams.Types (
  Headers,
  TopicName,
 )


-- | A finalised, byte-encoded record ready to publish.
data CollectedRecord = CollectedRecord
  { crTopic :: !TopicName
  , crKey :: !(Maybe ByteString)
  , crValue :: !(Maybe ByteString)
  -- ^ 'Nothing' is a null value (a tombstone on a compacted topic)
  , crTimestamp :: !Timestamp
  , crHeaders :: !Headers
  , crPartition :: !(Maybe Int)
  {- ^ Hint computed by an explicit partitioner; 'Nothing' means
  "let the producer pick".
  -}
  }


-- | The value's bytes, with a tombstone read as empty.
crValueBytes :: CollectedRecord -> ByteString
crValueBytes = maybe mempty id . crValue


{- | Sink-side collector. The runtime calls 'collectorSend' from
inside the sink processor; 'collectorFlush' is invoked at commit.
-}
data RecordCollector = RecordCollector
  { collectorSend :: !(CollectedRecord -> IO ())
  , collectorFlush :: !(IO ())
  , collectorClose :: !(IO ())
  , collectorPeek :: !(IO (Map TopicName (Seq CollectedRecord)))
  -- ^ Read-only snapshot. Used by the test driver.
  , collectorTake :: !(TopicName -> IO [CollectedRecord])
  -- ^ Drain a single topic's queue.
  }


{- | An in-memory collector used by 'TopologyTestDriver' and by tests.
Implementations targeting a real broker should call into the
"Kafka.Client.Producer" instead — see
"Kafka.Streams.Runtime.RecordCollector".
-}
inMemoryCollector :: IO RecordCollector
inMemoryCollector = do
  ref <- newTVarIO (Map.empty :: Map TopicName (Seq CollectedRecord))
  pure
    RecordCollector
      { collectorSend = \r -> atomically $
          modifyTVar' ref $ \m ->
            Map.insertWith
              (\new old -> old <> new)
              (crTopic r)
              (Seq.singleton r)
              m
      , collectorFlush = pure ()
      , collectorClose = atomically (writeTVar ref Map.empty)
      , collectorPeek = atomically (readTVar ref)
      , collectorTake = \topic -> atomically $ do
          m <- readTVar ref
          case Map.lookup topic m of
            Nothing -> pure []
            Just s -> do
              writeTVar ref (Map.delete topic m)
              pure (foldr (:) [] s)
      }


{- | Drain every topic. Returns @[(topic, [records-in-order])]@ and
empties the collector.
-}
drainCollector :: RecordCollector -> IO [(TopicName, [CollectedRecord])]
drainCollector c = do
  m <- collectorPeek c
  let tops = Map.keys m
  out <-
    mapM
      ( \t -> do
          rs <- collectorTake c t
          pure (t, rs)
      )
      tops
  pure out


-- | Read-only: get all records currently sitting in the topic queue.
recordsForTopic :: RecordCollector -> TopicName -> IO [CollectedRecord]
recordsForTopic c t = do
  m <- collectorPeek c
  case Map.lookup t m of
    Nothing -> pure []
    Just s -> pure (foldr (:) [] s)


collectorHasPending :: RecordCollector -> IO Bool
collectorHasPending c = do
  m <- collectorPeek c
  pure (not (Map.null m) && any (not . Seq.null) (Map.elems m))
