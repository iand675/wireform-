{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Kafka.Streams.Runtime.NativeDriver
Description : Pluggable driver layer used by Kafka.Streams.Runtime

The streams runtime engine never talks to "Kafka.Client.Producer"
or "Kafka.Client.Consumer" directly. It talks to a 'StreamDriver'
record-of-IO that carries:

  * the consumer-side hooks ('sdConsumerSubscribe',
    'sdConsumerPoll', 'sdConsumerCommit', 'sdConsumerClose'),
  * the producer-side hooks ('sdProducerSend', 'sdProducerFlush',
    'sdProducerClose'),
  * the EOS-V2 transactional hooks
    ('sdProducerBeginTxn', 'sdProducerCommitTxn',
    'sdProducerAbortTxn', 'sdProducerSendOffsetsToTxn'),
  * the rebalance signal channel ('sdRebalanceEvent').

Two constructors ship out of the box:

  * 'newNativeDriver' wires the driver against a real
    'KP.Producer' + 'KC.Consumer' (and an optional bound
    'KT.Transaction' for EOS-V2). Production code uses this.
  * 'newMockDriver' returns a driver whose IO surface is
    in-memory. Tests use it to exercise the runtime
    deterministically without a broker or even a producer/consumer
    process.

The runtime treats the two as interchangeable; this module is the
seam that keeps the engine decoupled from the wire layer.
-}
module Kafka.Streams.Runtime.NativeDriver (
  StreamDriver (..),
  RebalanceEvent (..),
  newNativeDriver,

  -- * Mock driver (tests)
  MockDriverHandle,
  newMockDriver,
  mockDriverInjectPoll,
  mockDriverInjectRebalance,
  mockDriverProbeRequests,
  mockDriverFlushCount,
  mockDriverClosed,
  mockDriverSubscribed,
  mockDriverDrainSends,
  mockDriverTxnLog,
  mockDriverCommittedOffsets,
  mockDriverCommitCount,
  mockDriverSetPartitions,
  MockSend (..),
  MockTxnEvent (..),
  offsetResetForConsumer,
) where

import Control.Concurrent.STM
import Data.ByteString (ByteString)
import Data.Foldable qualified as Foldable
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.IORef
import Data.Int (Int32, Int64)
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import GHC.Generics (Generic)
import Kafka.Client.Consumer qualified as KC
import Kafka.Client.Producer qualified as KP
import Kafka.Client.Transaction qualified as KT


-- | Rebalance signal handed to the driver's listener.
data RebalanceEvent
  = RebalanceAssigned ![KC.TopicPartition]
  | RebalanceRevoked ![KC.TopicPartition]
  | RebalanceLost ![KC.TopicPartition]
  deriving stock (Eq, Show, Generic)


{- | The full surface the streams runtime needs to talk to a
broker. Records of IO so the engine can swap in a mock under
test or a native driver in production.
-}
data StreamDriver
  = {- | Subscribe the consumer to a list of source topics. Called
    once at startup.
    -}
    StreamDriver
    { sdConsumerSubscribe
        :: !([Text] -> IO (Either String ()))
    , sdConsumerPoll
        :: !(Int -> IO (Either String [KC.ConsumerRecord]))
    -- ^ Poll the consumer for records.
    , sdConsumerCommit
        :: !(IO (Either String ()))
    {- ^ Synchronously commit current consumer offsets (non-EOS
    path). Returns 'Right' on success.
    -}
    , sdConsumerClose
        :: !(IO ())
    -- ^ Tear down the consumer.
    , sdConsumerCloseWith
        :: !(Bool -> Int -> IO ())
    {- ^ Tear down the consumer with caller-supplied options.
    KIP-812: when @leaveGroup = True@ (the default) the
    client sends a @LeaveGroup@ request so the broker can
    trigger a rebalance immediately; when @False@ we close
    silently and let the session timeout reassign the
    partitions. The @timeoutMs@ bounds how long we wait
    for the leave-group ack before forcing the close.
    -}
    , sdProducerSend
        :: !(KP.ProducerRecord -> IO (Either String KP.RecordMetadata))
    {- ^ Send a record. The runtime fills in the sink record's
    headers, timestamp and (when a 'Kafka.Streams.Produced.StreamPartitioner'
    chose one) its partition; 'Nothing' for @partition@ defers to
    the producer's partitioner. Returns Right metadata on success.
    -}
    , sdPartitionsFor
        :: !(Text -> IO (Either String Int))
    {- ^ Partition count of a topic, from producer metadata.
    Consulted only for sinks that carry a
    'Kafka.Streams.Produced.StreamPartitioner' (mirrors the JVM
    @RecordCollectorImpl@ calling @producer.partitionsFor@).
    -}
    , sdProducerFlush
        :: !(IO (Either String ()))
    {- ^ Block until every previously sent record has been
    acknowledged by the broker.
    -}
    , sdProducerClose
        :: !(IO ())
    -- ^ Tear down the producer.
    , sdProducerBeginTxn
        :: !(IO (Either String ()))
    -- ^ Begin a transaction (EOS-V2). 'Right' on success.
    , sdProducerCommitTxn
        :: !(IO (Either String ()))
    -- ^ Commit a transaction.
    , sdProducerAbortTxn
        :: !(IO (Either String ()))
    -- ^ Abort a transaction.
    , sdProducerSendOffsetsToTxn
        :: !( Text
              -> HashMap KC.TopicPartition Int64
              -> IO (Either String ())
            )
    {- ^ Send consumer offsets as part of the open transaction
    (KIP-447).
    -}
    , sdRebalanceEvent
        :: !(IO (Maybe RebalanceEvent))
    {- ^ Drain an event from the rebalance listener queue, if
    one is pending.
    -}
    , sdRequestProbingRebalance
        :: !(IO ())
    {- ^ KIP-441: ask the consumer-group protocol to issue a
    fresh JoinGroup so the leader can re-evaluate whether
    any warmup replicas are ready for promotion. The
    native driver delegates to the consumer's rejoin
    hook; the mock driver records the request in
    'mockDriverProbeRequests' so tests can observe the
    runtime's probing cadence.
    -}
    }


{- | Build a 'StreamDriver' that delegates to a live 'Producer' +
'Consumer' pair. The optional 'KT.Transaction' is required for
EOS-V2 (binding it via 'KP.bindTransaction' is the caller's
responsibility before passing the producer in).
-}
newNativeDriver
  :: KP.Producer
  -> KC.Consumer
  -> Maybe KT.Transaction
  -> IO StreamDriver
newNativeDriver producer consumer mTxn = do
  rebalanceQ <- newTQueueIO
  -- Wire the consumer's rebalance callbacks straight into the
  -- driver's event channel. The streams runtime then drains
  -- these events on every loop tick (see
  -- 'Kafka.Streams.Runtime.drainRebalances').
  let pushReb ev = atomically (writeTQueue rebalanceQ ev)
  KC.setRebalanceListener
    consumer
    (\tps -> pushReb (RebalanceAssigned tps))
    (\tps -> pushReb (RebalanceRevoked tps))
    (\tps -> pushReb (RebalanceLost tps))
  pure
    StreamDriver
      { sdConsumerSubscribe = KC.subscribe consumer
      , sdConsumerPoll = \timeoutMs -> KC.poll consumer timeoutMs
      , sdConsumerCommit = KC.commitSync consumer
      , sdConsumerClose = KC.closeConsumer consumer
      , sdConsumerCloseWith = \leaveGroup tmoMs ->
          if leaveGroup
            then KC.closeConsumerWithTimeout consumer tmoMs
            else KC.closeConsumerWithoutLeavingGroup consumer tmoMs
      , sdProducerSend = KP.sendRecord producer
      , sdPartitionsFor = \topic ->
          fmap length <$> KP.partitionsFor producer topic
      , sdProducerFlush = KP.flushProducer producer
      , sdProducerClose = KP.closeProducer producer
      , sdProducerBeginTxn = case mTxn of
          Nothing -> pure (Left "EOS not configured: producer has no bound Transaction")
          Just t -> do
            r <- KT.beginTransaction t
            pure $ case r of
              Left e -> Left (show e)
              Right () -> Right ()
      , sdProducerCommitTxn = case mTxn of
          Nothing -> pure (Right ()) -- non-transactional: no-op
          Just t -> do
            r <- KT.commitTransaction t
            pure $ case r of
              Left e -> Left (show e)
              Right () -> Right ()
      , sdProducerAbortTxn = case mTxn of
          Nothing -> pure (Right ())
          Just t -> do
            r <- KT.abortTransaction t
            pure $ case r of
              Left e -> Left (show e)
              Right () -> Right ()
      , sdProducerSendOffsetsToTxn = \groupId offsets -> case mTxn of
          Nothing -> pure (Left "EOS not configured: producer has no bound Transaction")
          Just t -> do
            r <- KT.commitOffsetsInTransaction t groupId offsets
            pure $ case r of
              Left e -> Left (show e)
              Right () -> Right ()
      , sdRebalanceEvent = atomically (tryReadTQueue rebalanceQ)
      , sdRequestProbingRebalance = do
          -- KIP-441: trigger a fresh JoinGroup so the leader
          -- can promote ready warmup replicas. 'requestRejoin'
          -- flips HB.hbNeedsRebalance so the next 'poll'
          -- transparently re-runs JoinGroup / SyncGroup.
          -- Returns False for unsubscribed consumers — treat
          -- as a no-op.
          _ <- KC.requestRejoin consumer
          pure ()
      }


{- | Translate a 'KC.OffsetResetStrategy' into the value the
runtime should use for @auto.offset.reset@ on the underlying
consumer. Wired by the engine when constructing the consumer
for a source topic.
-}
offsetResetForConsumer :: KC.OffsetResetStrategy -> Text
offsetResetForConsumer = \case
  KC.Earliest -> "earliest"
  KC.Latest -> "latest"
  KC.None -> "none"


----------------------------------------------------------------------
-- Mock driver
----------------------------------------------------------------------

{- | A captured outbound record (what 'sdProducerSend' was called
with).
-}
data MockSend = MockSend
  { mockSendTopic :: !Text
  , mockSendKey :: !(Maybe ByteString)
  , mockSendValue :: !ByteString
  , mockSendHeaders :: ![(Text, ByteString)]
  , mockSendPartition :: !(Maybe Int32)
  , mockSendTimestamp :: !(Maybe Int64)
  }
  deriving stock (Eq, Show, Generic)


-- | A captured EOS-V2 transactional event in call order.
data MockTxnEvent
  = MockTxnBegin
  | MockTxnCommit
  | MockTxnAbort
  | MockTxnSendOffsets !Text !(HashMap KC.TopicPartition Int64)
  deriving stock (Eq, Show, Generic)


{- | Test-side handle. Lets the test driver:

  * push records the runtime will see on the next poll
    ('mockDriverInjectPoll'),
  * read everything the runtime tried to send
    ('mockDriverDrainSends'),
  * inspect the EOS call sequence ('mockDriverTxnLog'),
  * inspect the offsets the runtime committed
    ('mockDriverCommittedOffsets').
-}
data MockDriverHandle = MockDriverHandle
  { pollQueue :: !(TVar (Seq [KC.ConsumerRecord]))
  , sendsOut :: !(TVar (Seq MockSend))
  , flushed :: !(TVar Int)
  , closed :: !(TVar (Bool, Bool))
  -- ^ (consumerClosed, producerClosed)
  , subscribed :: !(TVar [Text])
  , commits :: !(TVar Int)
  , txnLog :: !(TVar (Seq MockTxnEvent))
  , sentOffsets :: !(IORef (Seq (Text, HashMap KC.TopicPartition Int64)))
  , rebalances :: !(TQueue RebalanceEvent)
  , probeRequests :: !(TVar Int)
  {- ^ Count of 'sdRequestProbingRebalance' invocations the
  runtime has issued; used by tests to verify the
  probing cadence.
  -}
  , partitionCounts :: !(TVar (HashMap Text Int))
  {- ^ What 'sdPartitionsFor' reports per topic (set with
  'mockDriverSetPartitions'); unknown topics fail the lookup.
  -}
  }


{- | Build a fresh mock driver. The driver starts out idle: every
poll returns @Right []@ until 'mockDriverInjectPoll' is called.
-}
newMockDriver :: IO (StreamDriver, MockDriverHandle)
newMockDriver = do
  pollQ <- newTVarIO Seq.empty
  sends <- newTVarIO Seq.empty
  flushd <- newTVarIO 0
  closed <- newTVarIO (False, False)
  subs <- newTVarIO []
  cmts <- newTVarIO 0
  txn <- newTVarIO Seq.empty
  offs <- newIORef Seq.empty
  reb <- newTQueueIO
  probes <- newTVarIO 0
  parts <- newTVarIO HashMap.empty
  let h = MockDriverHandle pollQ sends flushd closed subs cmts txn offs reb probes parts
      drv =
        StreamDriver
          { sdConsumerSubscribe = \topics -> do
              atomically (writeTVar subs topics)
              pure (Right ())
          , sdConsumerPoll = \_timeoutMs -> atomically $ do
              q <- readTVar pollQ
              case Seq.viewl q of
                Seq.EmptyL -> pure (Right [])
                h_ Seq.:< rest -> do
                  writeTVar pollQ rest
                  pure (Right h_)
          , sdConsumerCommit = do
              atomically $ modifyTVar' cmts (+ 1)
              pure (Right ())
          , sdConsumerClose =
              atomically $
                modifyTVar' closed (\(_, p) -> (True, p))
          , sdConsumerCloseWith = \_leaveGroup _tmoMs ->
              atomically $
                modifyTVar' closed (\(_, p) -> (True, p))
          , sdProducerSend = \pr -> do
              atomically $
                modifyTVar'
                  sends
                  ( \s ->
                      s
                        |> MockSend
                          { mockSendTopic = pr.topic
                          , mockSendKey = pr.key
                          , mockSendValue = pr.value
                          , mockSendHeaders = pr.headers
                          , mockSendPartition = pr.partition
                          , mockSendTimestamp = pr.timestamp
                          }
                  )
              pure
                ( Right
                    KP.RecordMetadata
                      { topic = pr.topic
                      , partition = maybe 0 id pr.partition
                      , offset = 0
                      , timestamp = maybe 0 id pr.timestamp
                      }
                )
          , sdPartitionsFor = \topic -> do
              m <- readTVarIO parts
              pure $ case HashMap.lookup topic m of
                Just n -> Right n
                Nothing ->
                  Left ("mock driver: no partition count for " <> show topic)
          , sdProducerFlush = do
              atomically $ modifyTVar' flushd (+ 1)
              pure (Right ())
          , sdProducerClose =
              atomically $
                modifyTVar' closed (\(c, _) -> (c, True))
          , sdProducerBeginTxn = do
              atomically $ modifyTVar' txn (\s -> s |> MockTxnBegin)
              pure (Right ())
          , sdProducerCommitTxn = do
              atomically $ modifyTVar' txn (\s -> s |> MockTxnCommit)
              pure (Right ())
          , sdProducerAbortTxn = do
              atomically $ modifyTVar' txn (\s -> s |> MockTxnAbort)
              pure (Right ())
          , sdProducerSendOffsetsToTxn = \gid o -> do
              atomically $ modifyTVar' txn (\s -> s |> MockTxnSendOffsets gid o)
              modifyIORef' offs (\s -> s |> (gid, o))
              pure (Right ())
          , sdRebalanceEvent = atomically (tryReadTQueue reb)
          , sdRequestProbingRebalance =
              atomically (modifyTVar' probes (+ 1))
          }
  pure (drv, h)


{- | Push a 'RebalanceEvent' that the runtime will pick up on its
next loop tick. Used by tests to simulate the consumer-group
coordinator's assign / revoke / lost decisions without
spinning up a real broker.
-}
mockDriverInjectRebalance :: MockDriverHandle -> RebalanceEvent -> IO ()
mockDriverInjectRebalance h ev =
  atomically $ writeTQueue (h.rebalances) ev


{- | Number of times the runtime has called
'sdRequestProbingRebalance' on this mock driver.
-}
mockDriverProbeRequests :: MockDriverHandle -> IO Int
mockDriverProbeRequests h = readTVarIO h.probeRequests


-- | How many times the runtime has called 'sdProducerFlush'.
mockDriverFlushCount :: MockDriverHandle -> IO Int
mockDriverFlushCount h = readTVarIO h.flushed


{- | @(consumerClosed, producerClosed)@ — whether the runtime
has torn down each side of the driver.
-}
mockDriverClosed :: MockDriverHandle -> IO (Bool, Bool)
mockDriverClosed h = readTVarIO h.closed


-- | Most recent topics the runtime subscribed the consumer to.
mockDriverSubscribed :: MockDriverHandle -> IO [Text]
mockDriverSubscribed h = readTVarIO h.subscribed


{- | Push a batch of consumer records the runtime will receive on
the next 'sdConsumerPoll'.
-}
mockDriverInjectPoll :: MockDriverHandle -> [KC.ConsumerRecord] -> IO ()
mockDriverInjectPoll h batch =
  atomically $ modifyTVar' (h.pollQueue) (\s -> s |> batch)


{- | Drain everything the runtime has sent so far. Subsequent
calls only see records produced after the previous drain.
-}
mockDriverDrainSends :: MockDriverHandle -> IO [MockSend]
mockDriverDrainSends h = atomically $ do
  s <- readTVar (h.sendsOut)
  writeTVar (h.sendsOut) Seq.empty
  pure (Foldable.toList s)


-- | Read the captured EOS-V2 call sequence in order.
mockDriverTxnLog :: MockDriverHandle -> IO [MockTxnEvent]
mockDriverTxnLog h =
  Foldable.toList <$> atomically (readTVar (h.txnLog))


{- | List every (groupId, offsets) pair that was committed inside
a transaction, in order.
-}
mockDriverCommittedOffsets
  :: MockDriverHandle
  -> IO [(Text, HashMap KC.TopicPartition Int64)]
mockDriverCommittedOffsets h =
  Foldable.toList <$> readIORef (h.sentOffsets)


-- | Number of times 'sdConsumerCommit' fired (non-EOS path).
mockDriverCommitCount :: MockDriverHandle -> IO Int
mockDriverCommitCount h = atomically $ readTVar (h.commits)


{- | Set the partition count 'sdPartitionsFor' reports for a topic.
Topics never set fail the lookup, the same as a topic the broker
has no metadata for.
-}
mockDriverSetPartitions :: MockDriverHandle -> Text -> Int -> IO ()
mockDriverSetPartitions h topic n =
  atomically $ modifyTVar' h.partitionCounts (HashMap.insert topic n)
