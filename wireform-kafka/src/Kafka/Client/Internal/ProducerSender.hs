{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Kafka.Client.Internal.ProducerSender
Description : Producer background sender thread with retry logic
Copyright   : (c) 2025
License     : BSD-3-Clause
Maintainer  : kafka-native

This module implements the background sender thread for the Kafka producer.

The sender thread:
- Continuously drains ready batches from the accumulator
- Groups batches by broker
- Sends ProduceRequests to each broker
- Handles retries with exponential backoff
- Tracks in-flight requests
- Updates batch states and notifies waiting threads

Design patterns:
- Dedicated sender thread per producer (similar to Java Kafka client)
- In-flight request tracking with correlation IDs
- Exponential backoff for retries
- Graceful shutdown support
-}
module Kafka.Client.Internal.ProducerSender (
  -- * Sender Thread
  SenderState (..),
  createSenderState,
  startSenderThread,
  stopSenderThread,

  -- * Batch Sending
  sendBatches,
  retryOrFail,
  RetryEntry (..),
  LostSequence (..),
  retryBatch,
  bumpBatchAttempts,
  batchBackoffMs,
  shouldRetry,

  -- * Pipelined sender (per-broker in-flight tracking)
  senderTotalInFlight,

  -- * Pure batch construction (exposed for testing)
  buildRecordBatch,

  -- * Timeout Checking (KIP-91)
  isBatchTimedOut,

  -- * Configuration
  RetryConfig (..),
  defaultRetryConfig,
  nextRetryBackoffMs,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.Chan.Unagi qualified as U
import Control.Concurrent.Chan.Unagi.Bounded qualified as UB
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_, forever, unless, void, when)
import Data.ByteString qualified as BS
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (hash)
import Data.Maybe (isNothing, mapMaybe)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Int
import Data.List (groupBy, partition, sortBy)
import Data.List qualified
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Kafka.Client.Internal.BatchAccumulator qualified as BA
import Kafka.Client.Internal.Request qualified as Req
import Kafka.Client.RetryClassifier qualified as RC
import Kafka.Telemetry.Log qualified as KLog
import Kafka.Client.Metadata qualified as Meta
import Kafka.Compression.Types (CompressionCodec (NoCompression))
import Kafka.Network.Connection qualified as Conn
import Kafka.Protocol.ApiVersions qualified as AV
import Kafka.Protocol.RecordBatch qualified as RB
import Kafka.Protocol.RecordBatchWire qualified as RBW
import Kafka.Protocol.VersionNegotiation qualified as VN
import Kafka.Time qualified as KafkaTime
import "wireform-kafka-protocol" Kafka.Protocol.Generated.ProduceRequest qualified as PR
import "wireform-kafka-protocol" Kafka.Protocol.Generated.ProduceResponse qualified as PResp
import "wireform-kafka-protocol" Kafka.Protocol.Primitives qualified as P
import "wireform-kafka-protocol" Kafka.Protocol.Wire.Codec qualified as WC


{- | Retry configuration for failed sends. Mirrors librdkafka's
@retries@ + @retry.backoff.ms@ + @retry.backoff.max.ms@ +
@retry.backoff.multiplier@ knobs. The producer threads a
'RetryConfig' through 'sendMessage' / 'sendMessageAsync' so a
per-batch retry loop can compute its own next-backoff via
'nextRetryBackoffMs'.
-}
data RetryConfig = RetryConfig
  { retryMaxAttempts :: !Int
  -- ^ Maximum number of retry attempts (default: 2147483647).
  , retryBackoffMs :: !Int
  -- ^ Initial backoff in ms (default: 100).
  , retryBackoffMaxMs :: !Int
  -- ^ Ceiling for the exponential progression in ms (default: 1000).
  , retryBackoffMultiplier :: !Double
  -- ^ Multiplier between consecutive backoffs (default: 2.0).
  , retryBackoffJitter :: !Double
  {- ^ Jitter band in [0.0, 1.0]; the actual backoff is
  @backoff * (1 ± jitter)@ randomised uniformly. Default 0.2.
  -}
  }
  deriving (Eq, Show)


{- | Default retry configuration. Matches the producer-side
@defaultProducerConfig@.
-}
defaultRetryConfig :: RetryConfig
defaultRetryConfig =
  RetryConfig
    { retryMaxAttempts = 2147483647
    , retryBackoffMs = 100
    , retryBackoffMaxMs = 1000
    , retryBackoffMultiplier = 2.0
    , retryBackoffJitter = 0.2
    }


{- | Compute the next backoff for the given attempt number
(0-indexed: attempt 0 returns @retryBackoffMs@). The curve is
@backoff_n = min(retryBackoffMaxMs, retryBackoffMs * retryBackoffMultiplier^n)@
with deterministic jitter applied as a function of @n@ so two
runs of the same test produce identical numbers.
-}
nextRetryBackoffMs :: RetryConfig -> Int -> Int
nextRetryBackoffMs RetryConfig {..} attempt =
  let !raw =
        fromIntegral retryBackoffMs
          * (retryBackoffMultiplier ^ max 0 attempt)
      !capped = min (fromIntegral retryBackoffMaxMs) raw :: Double
      -- Sin-based jitter: deterministic per attempt, no PRNG, so
      -- tests can reproduce the curve exactly.
      !jit = sin (fromIntegral attempt) * retryBackoffJitter
  in max 0 (round (capped * (1 + jit)))


{- | Per-broker pipelined-send state.

The sender holds one 'BrokerPipe' per (broker, connection)
pair. Each pipe owns:

  * A dedicated 'Connection' (separate from
    'producerConnManager' so the pipelined writer doesn't race
    against the on-demand metadata refresh path that uses the
    shared cache).

  * A bounded outbox 'TBQueue OutboundProduce' that the sender
    main loop pushes encoded ProduceRequests into; the bound
    equals 'producerMaxInFlight' so the producer-side back-
    pressures naturally without an extra semaphore.

  * An unbounded in-flight FIFO 'TBQueue PendingProduce'
    coordinating writer → reader hand-off. Kafka guarantees
    responses come back in request order on a given
    connection, so a FIFO is sufficient (no correlation-ID
    map needed).

  * A single writer 'Async' that pulls outbox items, frames
    and writes the bytes, and pushes the matching pending
    entry onto the in-flight FIFO.

  * A single reader 'Async' that reads framed responses,
    pulls the head of the in-flight FIFO, parses the body, and
    dispatches per-batch callbacks via 'processProduceResponse'.

Together the writer + reader unlock pipelining: the producer
can stream up to 'senderMaxInFlight' ProduceRequests onto the
wire before any of their responses come back. Without
pipelining the sender pays a full broker round-trip per
ProduceRequest; with pipelining it pays one round-trip per
'senderMaxInFlight' requests, which is what librdkafka and
the JVM client do via @max.in.flight.requests.per.connection@.
| Pool of 'BrokerPipe's targeting one broker. The sender
round-robins outbound work across the pipes; each pipe owns
its own TCP socket + writer + reader so multiple
ProduceRequests can be in flight on different sockets in
parallel. This breaks the single-writer bottleneck a single
per-broker pipe imposes, which is the main thing keeping us
below librdkafka's per-broker throughput on small-record
workloads.
-}
newtype BrokerPool = BrokerPool {bpPipes :: V.Vector BrokerPipe}


data BrokerPipe = BrokerPipe
  { bpAddr :: !Conn.BrokerAddress
  , bpConn :: !Conn.Connection
  , bpOutboxIn :: !(UB.InChan OutboundProduce)
  , bpOutboxOut :: !(UB.OutChan OutboundProduce)
  {- ^ Bounded MPMC channel from
  "Control.Concurrent.Chan.Unagi.Bounded" carrying the
  round of work the main sender loop hands off to this
  pipe's writer thread. The bound is
  'senderMaxInFlight'; @writeChan@ blocks once that many
  requests are queued + in-flight, providing the same
  producer-side backpressure the prior 'TBQueue' shape
  gave us. Replaces 'TBQueue' on this hot path because
  @writeChan@ / @readChan@ are CAS-loop based with no
  STM commit overhead — at the per-record append rate
  (~12 K writes/sec at 3 M rec/s, single batch-per-
  round), STM's per-transaction allocation was visible.
  -}
  , bpInFlightIn :: !(U.InChan PendingProduce)
  , bpInFlightOut :: !(U.OutChan PendingProduce)
  {- ^ Unbounded MPMC channel from
  "Control.Concurrent.Chan.Unagi" carrying the in-flight
  FIFO from writer to reader. Unbounded because depth is
  already capped by 'bpOutboxIn's bound (a request can
  only land in-flight after the outbox releases it).
  -}
  , bpWriter :: !(Async ())
  , bpReader :: !(Async ())
  , bpInFlightCount :: !(TVar Int)
  {- ^ Mirrors @length(outbox) + length(inFlight)@ for the
  'flushProducer' / 'closeProducer' drain check; kept
  separate (a 'TVar' rather than peeked from the chans
  themselves) because unagi-chan doesn't expose a length
  primitive — the count is updated atomically by the
  sender on enqueue + the reader on ack.
  -}
  }


{- | Outbound work item written to a 'BrokerPipe' outbox by the
main sender loop. Carries the unencoded inputs; the per-broker
writer thread builds the ProduceRequest, runs the wire
encoder, and writes to the socket. Pushing the encoding off
the main sender loop frees the loop to fan more rounds out to
the writer in parallel.
-}
data OutboundProduce = OutboundProduce
  { opCorrId :: !Int32
  , opApiKey :: !Int16
  , opApiVersion :: !Int16
  , opClientId :: !P.KafkaString
  , opTransactionalId :: !(Maybe Text)
  , opAcks :: !Int16
  , opTimeoutMs :: !Int32
  , opMetadata :: !Meta.MetadataCache
  , opBatches :: ![BA.ProducerBatch]
  , opPending :: !PendingProduce
  }


{- | In-flight pending entry handed off from writer to reader.
Holds the per-batch context the response handler needs to
dispatch callbacks.
-}
data PendingProduce = PendingProduce
  { ppCorrId :: !Int32
  , ppApiVersion :: !Int16
  , ppBatches :: ![BA.ProducerBatch]
  }


-- | State for the sender thread
data SenderState = SenderState
  { senderAccumulator :: !BA.BatchAccumulator
  -- ^ Batch accumulator to drain from
  , senderMetadata :: !Meta.MetadataCache
  -- ^ Metadata cache for broker lookup
  , senderConnManager :: !Conn.ConnectionManager
  -- ^ Connection manager for getting broker connections
  , senderConnConfig :: !Conn.ConnectionConfig
  {- ^ Connection-level knobs (TLS / SASL / socket buffers /
  keepalive / etc.) the sender uses every time it opens a
  per-broker pipe. Threaded in from the @createProducer@
  caller so 'Kafka.Client.Env'-derived overrides (TLS,
  SASL, request timeouts) take effect for all sender
  connections, not just the bootstrap one.
  -}
  , senderRetryConfig :: !RetryConfig
  -- ^ Retry configuration
  , senderRunning :: !(TVar Bool)
  -- ^ Whether the sender thread should keep running
  , senderBusy :: !(IORef Bool)
  {- ^ True while the sender loop is mid-iteration on a drained
  batch (i.e. has called 'drainReadyBatches' but the broker
  reply for those records has not yet landed).

  Required for correctness of 'flushProducer' /
  'closeProducer'\'s drain check: polling
  'BA.hasReadyBatches' alone races the in-flight window in
  which the sender has /pulled/ the batches off the
  accumulator but is still waiting for a 'ProduceResponse'.
  Without this flag a producer that 'flushProducer' +
  'closeProducer' immediately can lose every record that
  was on the wire at the time of the flush. Plain 'IORef'
  is sufficient because the sender thread is the only
  writer.
  -}
  , senderMaxInFlight :: !Int
  {- ^ Maximum concurrent ProduceRequests in flight per broker
  pipe. Mirrors @max.in.flight.requests.per.connection@.
  -}
  , senderConnsPerBroker :: !Int
  {- ^ Number of parallel TCP connections per broker. Each
  connection has its own writer + reader thread, so
  throughput scales roughly linearly with this for
  broker-bound workloads. librdkafka uses 1 by default but
  benchmarks frequently bump it; the JVM client does not
  expose this knob (always 1).
  -}
  , senderBrokerPipes :: !(IORef (HashMap Conn.BrokerAddress BrokerPool))
  {- ^ Per-broker pool of pipelined-send pipes. Lazily
  populated on first send to a broker. Each pipe in the
  pool owns its own 'Connection' (so the writer / reader
  threads don't race against each other or against the
  on-demand metadata refresh path that uses the shared
  'producerConnManager').
  -}
  , senderAcks :: !Int16
  -- ^ Required acks: 0 = none, 1 = leader, -1 = all ISRs
  , senderTimeoutMs :: !Int32
  -- ^ Request timeout in milliseconds (used in ProduceRequest)
  , senderDeliveryTimeoutMs :: !Int32
  -- ^ Delivery timeout in milliseconds (KIP-91: total time including retries)
  , senderCompression :: !CompressionCodec
  -- ^ Compression codec to use
  , senderClientId :: !Text
  -- ^ Client ID for requests
  , senderCorrelationId :: !(IORef Int32)
  {- ^ Next correlation ID to use. Single sender thread per
  producer is the sole writer; SPSC counter served by
  'IORef' + 'atomicModifyIORef\'' instead of an STM
  transaction on every Produce request.
  -}
  , senderVersionCache :: !AV.ApiVersionCache
  {- ^ Per-broker negotiated API version cache. Populated on
  first contact with each leader broker via
  'VN.ensureVersionsNegotiated' so the Produce-version
  selection ('VN.pickApiVersion') matches what the broker
  actually accepts (rather than the hardcoded v3 the
  sender used to emit unconditionally).
  -}
  , senderTransactionalId :: !(IORef (Maybe Text))
  {- ^ Transactional id the producer is bound to.

  Tier 3 of the STM-replacement work: single-writer (the
  producer's @bindTransaction@ path) / single-reader (the
  sender thread on every produce); 'IORef' is sufficient.

  For transactional sends ('attrIsTransactional == True' on
  the batch) the broker requires the @transactionalId@
  field on the ProduceRequest envelope to match the txn id
  the producer's records claim. Sending @Null@ here makes
  the broker reject the produce with
  @TRANSACTIONAL_ID_AUTHORIZATION_FAILED@ (53) — the
  broker's authorization gate runs before the no-auth
  shortcut, and a null transactionalId on a transactional
  ProduceRequest is, definitionally, unauthorised.

  'Nothing' for non-transactional / idempotent-only
  producers; written by 'Producer.bindTransaction' when a
  'Transaction' is bound. The sender reads this TVar each
  send and folds it into the request, so a producer that
  /unbinds/ a transaction mid-flight stops attaching the
  id from the next send onwards.
  -}
  , senderRetryQueue :: !(TVar [RetryEntry])
  {- ^ Batches waiting to be sent again (in the order they
  joined), after a retriable produce error, an unknown leader or
  a transport failure: with their sequence numbers unchanged, so
  an idempotent producer's retry is deduplicated by the broker.
  -}
  , senderLostSequences :: !(TVar (HashMap BA.TopicPartition LostSequence))
  {- ^ Per partition, the first sequence number (under its
  producer id and epoch) whose batch failed for good. Every
  later batch under the same epoch would be rejected as out of
  order, so it fails at once instead of being retried until its
  delivery timeout; a new epoch (which restarts the sequences)
  leaves the entry behind.
  -}
  , senderRefreshMetadata :: !(IORef ([Text] -> IO ()))
  {- ^ Refreshes the metadata for some topics; called when a
  batch's leader is unknown or has moved. Set by the producer
  (it owns the connections the refresh goes through).
  -}
  , senderOnSequenceLost :: !(IORef (BA.ProducerBatch -> IO ()))
  {- ^ Called when an idempotent or transactional batch fails for
  good, leaving a gap in its partition's sequence: the producer
  bumps its epoch (KIP-360), at once for an idempotent producer,
  with the abort for a transactional one.
  -}
  }


-- | A batch due to be sent again at 'reDueMs'.
data RetryEntry = RetryEntry
  { reDueMs :: {-# UNPACK #-} !Int64
  , reBatch :: !BA.ProducerBatch
  }


-- | Where a partition's sequence was lost: see 'senderLostSequences'.
data LostSequence = LostSequence
  { lsProducerId :: {-# UNPACK #-} !Int64
  , lsEpoch :: {-# UNPACK #-} !Int16
  , lsSequence :: {-# UNPACK #-} !Int32
  }


-- | Create a new sender state
createSenderState
  :: BA.BatchAccumulator
  -> Meta.MetadataCache
  -> Conn.ConnectionManager
  -> Conn.ConnectionConfig
  {- ^ Per-broker connection-level config (TLS,
  SASL, socket buffers, …). Used every time
  the sender opens a new pipe so caller
  overrides (including 'Kafka.Client.Env'
  ones) apply to all sender connections.
  -}
  -> RetryConfig
  -> Int16
  -- ^ Required acks
  -> Int
  -- ^ Delivery timeout (ms) - KIP-91
  -> CompressionCodec
  -> Text
  -- ^ Client ID
  -> AV.ApiVersionCache
  {- ^ Per-broker negotiated API-version cache (shared
  with the producer's bootstrap handshake)
  -}
  -> Int
  {- ^ Max in-flight ProduceRequests per broker pipe
  ('producerMaxInFlight'). The pipelined sender
  keeps up to this many ProduceRequests on the
  wire before blocking the main sender loop on
  the per-broker outbox bound.
  -}
  -> IO SenderState
createSenderState accumulator metadata connManager connConfig retryConfig acks deliveryTimeoutMs compression clientId versionCache maxInFlight = do
  running <- newTVarIO True
  busy <- newIORef False
  correlationId <- newIORef 0
  txnIdVar <- newIORef Nothing
  brokerPipes <- newIORef HashMap.empty
  retryQueue <- newTVarIO []
  lostSeqs <- newTVarIO HashMap.empty
  refresh <- newIORef (\_ -> pure ())
  onLost <- newIORef (\_ -> pure ())
  -- Use the smaller of delivery timeout and 30 seconds for individual request timeout
  -- The delivery timeout covers all retries, while request timeout is per-request
  let requestTimeoutMs = min 30000 (fromIntegral deliveryTimeoutMs)
  return
    SenderState
      { senderAccumulator = accumulator
      , senderMetadata = metadata
      , senderConnManager = connManager
      , senderConnConfig = connConfig
      , senderRetryConfig = retryConfig
      , senderRunning = running
      , senderBusy = busy
      , senderMaxInFlight = max 1 maxInFlight
      , senderConnsPerBroker = 16
      , senderBrokerPipes = brokerPipes
      , senderAcks = acks
      , senderTimeoutMs = requestTimeoutMs
      , senderDeliveryTimeoutMs = fromIntegral deliveryTimeoutMs
      , senderCompression = compression
      , senderClientId = clientId
      , senderCorrelationId = correlationId
      , senderVersionCache = versionCache
      , senderTransactionalId = txnIdVar
      , senderRetryQueue = retryQueue
      , senderLostSequences = lostSeqs
      , senderRefreshMetadata = refresh
      , senderOnSequenceLost = onLost
      }


-- | Start the sender background thread
startSenderThread :: SenderState -> IO (Async ())
startSenderThread state = async $ senderLoop state


{- | Stop the sender thread gracefully and tear down every
per-broker pipelined send pipe (cancel writer + reader,
disconnect the dedicated socket).
-}
stopSenderThread :: SenderState -> Async () -> IO ()
stopSenderThread state thread = do
  atomically $ writeTVar (senderRunning state) False
  cancel thread
  pools <- readIORef (senderBrokerPipes state)
  forM_ (HashMap.elems pools) $ \BrokerPool {..} ->
    forM_ (V.toList bpPipes) $ \BrokerPipe {..} -> do
      cancel bpWriter
      cancel bpReader
      void (try (Conn.disconnect bpConn) :: IO (Either SomeException ()))
  atomicModifyIORef' (senderBrokerPipes state) $ \_ -> (HashMap.empty, ())


-- | Main sender loop
senderLoop :: SenderState -> IO ()
senderLoop state@SenderState {..} = do
  -- Check if we should continue running
  shouldRun <- atomically $ readTVar senderRunning

  if not shouldRun
    then return () -- Exit loop
    else do
      -- Mark the sender busy /before/ taking anything off the
      -- accumulator or the retry queue: from then until
      -- 'sendBatches' has handed the batches to a pipe (or back to
      -- the retry queue) they are nowhere else, and 'flushProducer'
      -- / 'closeProducer' wait on (not hasReady) AND (not
      -- senderBusy) AND nothing in flight or queued.
      writeIORef senderBusy True
      hasReady <- BA.hasReadyBatches senderAccumulator
      now <- KafkaTime.currentTimeMillis
      -- A partition's queued batches go out together, in sequence
      -- order, once every one of them is due: sending a later
      -- batch ahead of an earlier one would only earn it
      -- OUT_OF_ORDER_SEQUENCE_NUMBER.
      due <- atomically $ do
        q <- readTVar senderRetryQueue
        let lastDue = HashMap.fromListWith max (map (\e -> (BA.batchTopicPartition (reBatch e), reDueMs e)) q)
            ready e = HashMap.findWithDefault 0 (BA.batchTopicPartition (reBatch e)) lastDue <= now
            (go, waiting) = partition ready q
        writeTVar senderRetryQueue waiting
        pure go

      if hasReady || not (null due)
        then do
          fresh <- if hasReady then BA.drainReadyBatches senderAccumulator else pure []
          -- A fresh batch for a partition that still has a retry
          -- waiting goes after it, so the partition's batches
          -- reach the broker in sequence order.
          held <- atomically $ do
            q <- readTVar senderRetryQueue
            let waitingFor = HashMap.fromListWith max (map (\e -> (BA.batchTopicPartition (reBatch e), reDueMs e)) q)
                (behind, go) = partition (\b -> HashMap.member (BA.batchTopicPartition b) waitingFor) fresh
                queued = map (\b -> RetryEntry (HashMap.findWithDefault 0 (BA.batchTopicPartition b) waitingFor) b) behind
            writeTVar senderRetryQueue (q ++ queued)
            pure go
          let retried = sortBy (comparing (\b -> (BA.batchTopicPartition b, BA.batchBaseSequence b))) (map reBatch due)
          result <- try $ sendBatches state (retried ++ held)
          writeIORef senderBusy False
          case result of
            Left (e :: SomeException) ->
              KLog.logError KLog.kafkaInstrumentationLibrary "producer: sender loop error" [KLog.attrShow "exception.message" e]
            Right () -> return ()
          senderLoop state
        else do
          writeIORef senderBusy False
          threadDelay 10000 -- 10ms
          senderLoop state


{- | Send a collection of batches to their respective brokers via
the per-broker pipelined send pipe (see 'BrokerPipe').

Batches are first grouped by broker (the partition leader) and
then split into rounds where every (topic, partition) appears
at most once per round; each round becomes one ProduceRequest
(the Kafka wire spec requires one 'PartitionProduceData' per
partition per request).

For each round, the sender:
  1. Builds the ProduceRequest record + serialised body.
  2. Pushes it to the broker pipe's outbox (blocks if the
     pipe is at 'senderMaxInFlight').
  3. Returns immediately — the writer thread takes over and
     hands the pending entry to the reader thread which
     dispatches per-batch callbacks once the broker replies.

Net effect: the sender main loop can drain and queue many
ProduceRequests without paying a full broker round-trip per
request, which is the librdkafka /
@max.in.flight.requests.per.connection@ model.
-}
sendBatches :: SenderState -> [BA.ProducerBatch] -> IO ()
sendBatches state batches = do
  batchesByBroker <- groupBatchesByBroker state batches
  forM_ (Map.toList batchesByBroker) $ \(broker, brokerBatches) -> do
    poolR <- ensureBrokerPool state broker
    case poolR of
      Left err ->
        forM_ brokerBatches $ retryOrFail state (T.pack ("Failed to open broker pipe: " <> err))
      Right pool -> do
        -- Every batch of a partition goes down the same pipe: one
        -- TCP connection delivers in order, so the partition's
        -- batches reach the broker in sequence order (pipes are
        -- for parallelism across partitions).
        let !pipes = bpPipes pool
            !nPipes = V.length pipes
            byPipe =
              HashMap.toList $
                HashMap.fromListWith
                  (flip (++))
                  (map (\b -> (hash (BA.batchTopicPartition b) `mod` nPipes, [b])) brokerBatches)
        forM_ byPipe $ \(ix, pipeBatches) ->
          forM_ (roundsPerPartition pipeBatches) $ \roundBatches -> do
            result <- try $ enqueueRound state (pipes V.! ix) broker roundBatches
            case result of
              Left (e :: SomeException) ->
                forM_ roundBatches $ retryOrFail state ("Exception enqueueing batch: " <> T.pack (show e))
              Right () -> pure ()
  where
    -- Split batches into rounds such that every (topic, partition)
    -- appears at most once per round. The k-th round contains the
    -- k-th batch (in arrival order) for every partition that had
    -- at least k queued.
    roundsPerPartition :: [BA.ProducerBatch] -> [[BA.ProducerBatch]]
    roundsPerPartition bs =
      let groups :: [[BA.ProducerBatch]]
          groups =
            groupBy (\a b -> tpKey a == tpKey b) $
              sortBy (comparing tpKey) bs
          tpKey :: BA.ProducerBatch -> BA.TopicPartition
          tpKey = BA.batchTopicPartition
      in transposeNonEmpty groups

    transposeNonEmpty :: [[a]] -> [[a]]
    transposeNonEmpty xss =
      case [(h, t) | (h : t) <- xss] of
        [] -> []
        hts -> map fst hts : transposeNonEmpty (map snd hts)


{- | Build the ProduceRequest for one round of batches, encode it,
and push it to the broker's outbox. Blocks on
'senderMaxInFlight' backpressure (the outbox is a bounded
'TBQueue').
-}
enqueueRound
  :: SenderState
  -> BrokerPipe
  -> Meta.BrokerMetadata
  -> [BA.ProducerBatch]
  -> IO ()
enqueueRound state@SenderState {..} pipe (_broker :: Meta.BrokerMetadata) batches = do
  -- Drop any batches that have exceeded the per-batch delivery
  -- timeout (KIP-91) before we hit the wire.
  currentTime <- KafkaTime.currentTimeMillis
  let (timedOut, valid) = partition (isBatchTimedOut currentTime senderDeliveryTimeoutMs) batches
  forM_ timedOut $ \batch -> do
    let createTime = BA.batchCreateTime batch
        elapsed = currentTime - createTime
        msg =
          "Delivery timeout exceeded: batch created "
            <> T.pack (show elapsed)
            <> "ms ago, timeout is "
            <> T.pack (show senderDeliveryTimeoutMs)
            <> "ms"
    forM_ (BA.batchCallbacks batch) $ \cb -> BA.runRecordCallback cb (Left msg)
  when (not (null valid)) $ do
    -- The ApiVersions handshake was already run in
    -- 'ensureBrokerPipe' before the writer / reader threads
    -- started.
    txnIdM <- readIORef senderTransactionalId
    verR <-
      VN.pickApiVersionForRange @PR.ProduceRequest
        3
        13
        senderVersionCache
        (bpAddr pipe)
        3
    let !apiVersion = case verR of
          Right v -> v
          Left _ -> 3
        !apiKey = 0
        !clientId = P.mkKafkaString senderClientId
    cid <- atomicModifyIORef' senderCorrelationId $ \k -> (k + 1, k)
    let !pending =
          PendingProduce
            { ppCorrId = cid
            , ppApiVersion = apiVersion
            , ppBatches = valid
            }
        !out =
          OutboundProduce
            { opCorrId = cid
            , opApiKey = apiKey
            , opApiVersion = apiVersion
            , opClientId = clientId
            , opTransactionalId = txnIdM
            , opAcks = senderAcks
            , opTimeoutMs = senderTimeoutMs
            , opMetadata = senderMetadata
            , opBatches = valid
            , opPending = pending
            }
    -- Bound on outbox depth = senderMaxInFlight provides
    -- producer-side backpressure: 'UB.writeChan' blocks once
    -- the pipe has 'senderMaxInFlight' requests queued + in
    -- flight, matching the librdkafka /
    -- 'max.in.flight.requests.per.connection' semantics.
    --
    -- The counter and the chan write are no longer one STM
    -- transaction (unagi-chan isn't STM-aware). Bumping the
    -- counter /before/ the chan write keeps the
    -- 'flushProducer' poll conservative — the count can read
    -- high for a brief window while the writeChan is in
    -- progress, but never low.
    atomically $ modifyTVar' (bpInFlightCount pipe) (+ 1)
    UB.writeChan (bpOutboxIn pipe) out


{- | Look up (or lazily open) the pool of pipelined-send pipes
for a broker. The pool size is 'senderConnsPerBroker'; each
pipe opens its own dedicated TCP connection and starts a
writer + reader async pair.
-}
ensureBrokerPool
  :: SenderState
  -> Meta.BrokerMetadata
  -> IO (Either String BrokerPool)
ensureBrokerPool state@SenderState {..} brokerMeta = do
  let !addr = Meta.brokerMetaAddress brokerMeta
  existing <- HashMap.lookup addr <$> readIORef senderBrokerPipes
  case existing of
    Just pool -> pure (Right pool)
    Nothing -> do
      let !n = max 1 senderConnsPerBroker
      pipesE <-
        mapM
          (const (openOnePipe state addr))
          [1 .. n]
      case sequence pipesE of
        Left err -> do
          -- Tear down the pipes that did open before failing.
          forM_ pipesE $ \case
            Right p -> do
              cancel (bpWriter p)
              cancel (bpReader p)
              void (try (Conn.disconnect (bpConn p)) :: IO (Either SomeException ()))
            Left _ -> pure ()
          pure (Left err)
        Right pipes -> do
          let !pool = BrokerPool {bpPipes = V.fromList pipes}
          inserted <- atomicModifyIORef' senderBrokerPipes $ \m ->
            case HashMap.lookup addr m of
              Just existingPool -> (m, Right existingPool)
              Nothing -> (HashMap.insert addr pool m, Left pool)
          case inserted of
            Right p -> do
              -- A racing caller already populated the entry;
              -- tear down our just-built pool and use theirs.
              forM_ pipes $ \pp -> do
                cancel (bpWriter pp)
                cancel (bpReader pp)
                void (try (Conn.disconnect (bpConn pp)) :: IO (Either SomeException ()))
              pure (Right p)
            Left p -> pure (Right p)


-- | Open + start one 'BrokerPipe'.
openOnePipe :: SenderState -> Conn.BrokerAddress -> IO (Either String BrokerPipe)
openOnePipe state@SenderState {..} addr = do
  connR <- Conn.connect addr senderConnConfig
  case connR of
    Left err -> pure (Left err)
    Right conn -> do
      let nextCid = atomicModifyIORef' senderCorrelationId $ \cid -> (cid + 1, cid)
      _ <- VN.ensureVersionsNegotiated conn addr senderVersionCache nextCid
      (outboxIn, outboxOut) <- UB.newChan (max 1 senderMaxInFlight)
      (inflightIn, inflightOut) <- U.newChan
      counter <- newTVarIO 0
      let !pipeStub =
            PipeBootstrap
              { pbsAddr = addr
              , pbsConn = conn
              , pbsOutboxOut = outboxOut
              , pbsInFlightIn = inflightIn
              , pbsInFlightOut = inflightOut
              , pbsCount = counter
              , pbsState = state
              }
      writerA <- async (brokerWriterLoop pipeStub)
      readerA <- async (brokerReaderLoop pipeStub)
      pure $
        Right
          BrokerPipe
            { bpAddr = addr
            , bpConn = conn
            , bpOutboxIn = outboxIn
            , bpOutboxOut = outboxOut
            , bpInFlightIn = inflightIn
            , bpInFlightOut = inflightOut
            , bpWriter = writerA
            , bpReader = readerA
            , bpInFlightCount = counter
            }


-- | Bootstrap helper for the writer / reader threads.
data PipeBootstrap = PipeBootstrap
  { pbsAddr :: !Conn.BrokerAddress
  , pbsConn :: !Conn.Connection
  , pbsOutboxOut :: !(UB.OutChan OutboundProduce)
  , pbsInFlightIn :: !(U.InChan PendingProduce)
  , pbsInFlightOut :: !(U.OutChan PendingProduce)
  , pbsCount :: !(TVar Int)
  , pbsState :: !SenderState
  }


{- | Per-broker writer loop. Pulls outbox items, frames + writes
the request bytes to the wire, and pushes the matching pending
entry onto the in-flight FIFO so the reader knows what
batches the next response corresponds to.
-}
brokerWriterLoop :: PipeBootstrap -> IO ()
brokerWriterLoop PipeBootstrap {..} = forever $ do
  out <- UB.readChan pbsOutboxOut
  -- Encoding lives here (in the writer thread) rather than in
  -- the main sender loop so the main loop can fan more rounds
  -- out to this thread in parallel with us hitting the wire.
  request <-
    buildProduceRequest
      (opMetadata out)
      (opTransactionalId out)
      (opAcks out)
      (opTimeoutMs out)
      (opBatches out)
  let !requestBody = WC.runEncodeVer @PR.ProduceRequest (opApiVersion out) request
      !framed =
        Req.frameRequest
          (opApiKey out)
          (opApiVersion out)
          (opCorrId out)
          (opClientId out)
          requestBody
  result <-
    try (Req.sendRawRequest pbsConn framed)
      :: IO (Either SomeException ())
  case result of
    Left e -> do
      -- Wire write failed; surface to all the round's batches
      -- and don't enqueue a pending entry (no response will
      -- ever come). The reader thread keeps going against
      -- whatever is already in flight; a subsequent request
      -- on this pipe will hit the broken socket and surface.
      let !errMsg = "Wire write failed: " <> T.pack (show (e :: SomeException))
      forM_ (ppBatches (opPending out)) $ retryOrFail pbsState errMsg
      atomically $ modifyTVar' pbsCount (subtract 1)
    Right () ->
      -- Single-producer-single-consumer hand-off from this
      -- writer to the matching reader; unagi's @writeChan@ is
      -- a CAS-loop on a segmented array, no STM commit.
      U.writeChan pbsInFlightIn (opPending out)


{- | Per-broker reader loop. Reads framed responses off the wire,
pulls the head of the in-flight FIFO (Kafka guarantees
per-connection in-order responses), parses the body, and
dispatches per-batch callbacks via 'processProduceResponse'.
-}
brokerReaderLoop :: PipeBootstrap -> IO ()
brokerReaderLoop pbs@PipeBootstrap {..} = do
  rawR <- try (Req.receiveRawResponse pbsConn) :: IO (Either SomeException BS.ByteString)
  case rawR of
    Left e -> do
      -- Drain in-flight: surface a transport error to whatever
      -- is left waiting. Then exit the loop (the broken
      -- connection means the writer's next write will also
      -- fail and the user will see an error from the next
      -- send).
      drained <- drainUnagi pbsInFlightOut
      forM_ drained $ \pp ->
        forM_ (ppBatches pp) $ retryOrFail pbsState ("Wire read failed: " <> T.pack (show (e :: SomeException)))
      atomically $ writeTVar pbsCount 0
      void (try (Conn.disconnect pbsConn) :: IO (Either SomeException ()))
      -- Forget this broker's pool, so the retries open fresh pipes.
      atomicModifyIORef' (senderBrokerPipes pbsState) $ \m -> (HashMap.delete pbsAddr m, ())
    Right raw -> do
      pp <- U.readChan pbsInFlightOut
      let parsed = Req.parseResponseFrame 0 (ppApiVersion pp) raw
      case parsed of
        Left err ->
          forM_ (ppBatches pp) $ failBatch pbsState ("Response decode error: " <> T.pack err)
        Right (responseCorrId, body)
          | responseCorrId /= ppCorrId pp ->
              forM_ (ppBatches pp) $ failBatch pbsState "Correlation ID mismatch"
          | otherwise ->
              case WC.runDecodeVer @PResp.ProduceResponse (ppApiVersion pp) body of
                Left err ->
                  forM_ (ppBatches pp) $ failBatch pbsState ("Failed to parse response: " <> T.pack err)
                Right resp ->
                  processProduceResponse pbsState (ppBatches pp) resp
      atomically $ modifyTVar' pbsCount (subtract 1)
      brokerReaderLoop pbs


{- | Best-effort drain of a 'U.OutChan' to a list of items
currently sitting in the chan. Used by the broker reader's
error path to surface a transport failure to every batch
still in-flight.

/Not atomic/: a writer that lands a new item between two
'U.tryReadChan' iterations would have its item missed by
this drain. That's the same window the prior STM-based
'flushTBQueueAll' had against any non-STM writer; in our
protocol the writer that fed this in-flight chan is dead
(the connection is broken), so no new writes are coming.
-}
drainUnagi :: U.OutChan a -> IO [a]
drainUnagi out = go []
  where
    go !acc = do
      (element, _) <- U.tryReadChan out
      m <- U.tryRead element
      case m of
        Nothing -> pure (reverse acc)
        Just x -> go (x : acc)


{- | Total number of ProduceRequests currently queued or in-flight
across every broker pipe. The producer's
'Kafka.Client.Producer.flushProducer' /
'Kafka.Client.Producer.closeProducer' wait paths consult this
to know when every record has been acked by the broker (the
pipelined writer / reader hand-off makes 'BA.hasReadyBatches'
false the moment the sender pulls batches off the accumulator,
so we need a separate signal that the broker has finished
replying for them).
-}
senderTotalInFlight :: SenderState -> IO Int
senderTotalInFlight SenderState {..} = do
  pools <- readIORef senderBrokerPipes
  queued <- length <$> readTVarIO senderRetryQueue
  fmap ((+ queued) . sum) $ forM (HashMap.elems pools) $ \BrokerPool {..} ->
    fmap sum $ forM (V.toList bpPipes) $ \BrokerPipe {..} ->
      atomically (readTVar bpInFlightCount)


{- | Group batches by their target broker. A batch whose leader isn't
known (a topic just created, a leader election) waits for a metadata
refresh and is retried.
-}
groupBatchesByBroker
  :: SenderState
  -> [BA.ProducerBatch]
  -> IO (Map Meta.BrokerMetadata [BA.ProducerBatch])
groupBatchesByBroker state batches = do
  batchesWithBroker <- forM batches $ \batch -> do
    let tp = BA.batchTopicPartition batch
    brokerM <- atomically $ Meta.getPartitionLeader (senderMetadata state) (BA.tpTopic tp) (BA.tpPartition tp)
    return (brokerM, batch)
  let unknown = map snd (filter (isNothing . fst) batchesWithBroker)
  unless (null unknown) $ do
    refresh <- readIORef (senderRefreshMetadata state)
    refresh (Data.List.nub (map (BA.tpTopic . BA.batchTopicPartition) unknown))
    forM_ unknown $ \batch -> do
      let tp = BA.batchTopicPartition batch
      retryOrFail state ("Unknown leader for topic " <> BA.tpTopic tp <> " partition " <> T.pack (show (BA.tpPartition tp))) batch
  pure $
    Map.fromListWith
      (flip (++))
      (mapMaybe (\(brokerM, batch) -> fmap (\b -> (b, [batch])) brokerM) batchesWithBroker)


{- | Send @batch@ again after its backoff, with its sequence numbers
unchanged, unless it has run out of attempts or past its delivery
timeout, or an earlier batch's sequence on its partition is lost; then
fail it with @why@.
-}
retryOrFail :: SenderState -> Text -> BA.ProducerBatch -> IO ()
retryOrFail state why batch = do
  now <- KafkaTime.currentTimeMillis
  lost <- sequenceLostBefore state batch
  if lost || isBatchTimedOut now (senderDeliveryTimeoutMs state) batch || not (shouldRetry state batch)
    then failBatch state why batch
    else do
      KLog.logWarn KLog.kafkaInstrumentationLibrary ("producer: retrying a batch: " <> why) $
        KLog.attrInt "messaging.kafka.produce.attempt" (BA.batchAttempts batch + 1) : batchAttrs batch
      let !entry = RetryEntry (now + fromIntegral (batchBackoffMs state batch)) (bumpBatchAttempts batch)
      atomically $ modifyTVar' (senderRetryQueue state) (++ [entry])


-- | Log attributes naming @batch@'s partition and sequence.
batchAttrs :: BA.ProducerBatch -> [KLog.Attr]
batchAttrs batch =
  KLog.attrInt "messaging.kafka.produce.base_sequence" (BA.batchBaseSequence batch)
    : KLog.topicAttrs (BA.tpTopic tp) (fromIntegral (BA.tpPartition tp))
  where
    tp = BA.batchTopicPartition batch


-- | Whether an earlier batch of @batch@'s partition, under its epoch, failed for good.
sequenceLostBefore :: SenderState -> BA.ProducerBatch -> IO Bool
sequenceLostBefore state batch
  | BA.batchBaseSequence batch < 0 = pure False
  | otherwise = do
      m <- readTVarIO (senderLostSequences state)
      pure $ case HashMap.lookup (BA.batchTopicPartition batch) m of
        Just ls ->
          lsProducerId ls == BA.batchProducerId batch
            && lsEpoch ls == BA.batchProducerEpoch batch
            && lsSequence ls < BA.batchBaseSequence batch
        Nothing -> False


{- | Fail every record of @batch@ with @why@. An idempotent or
transactional batch leaves a gap in its partition's sequence: record
it ('senderLostSequences') and tell the producer
('senderOnSequenceLost').
-}
failBatch :: SenderState -> Text -> BA.ProducerBatch -> IO ()
failBatch state why batch = do
  when (BA.batchBaseSequence batch >= 0) $ do
    let !ls = LostSequence (BA.batchProducerId batch) (BA.batchProducerEpoch batch) (BA.batchBaseSequence batch)
        keepEarliest new old
          | lsProducerId old == lsProducerId new && lsEpoch old == lsEpoch new = if lsSequence old <= lsSequence new then old else new
          | otherwise = new
    atomically $ modifyTVar' (senderLostSequences state) (HashMap.insertWith keepEarliest (BA.batchTopicPartition batch) ls)
    onLost <- readIORef (senderOnSequenceLost state)
    onLost batch
  forM_ (BA.batchCallbacks batch) $ \cb -> BA.runRecordCallback cb (Left why)


-- | Check if a batch has exceeded the delivery timeout (KIP-91)
isBatchTimedOut :: Int64 -> Int32 -> BA.ProducerBatch -> Bool
isBatchTimedOut currentTime deliveryTimeoutMs batch =
  let createTime = BA.batchCreateTime batch
      elapsed = currentTime - createTime
  in elapsed > fromIntegral deliveryTimeoutMs


{- | Build a ProduceRequest from a list of batches.
Compression happens here, so this is an IO operation.
The metadata cache is consulted per-topic to populate the
KIP-516 TopicId field; on pre-v10 brokers (or topics not yet
in the cache) the field stays 'P.nullUuid' which is fine for
v0-v12 (where the encoder ignores it). v13+ requires a real
topic id.

The @transactionalId@ is plumbed in from the caller (read off
the 'SenderState' TVar populated by @bindTransaction@). When
'Just' it goes onto the request envelope verbatim; @Nothing@
sends @KafkaString Null@. The broker requires this to match
the txn id any transactional records in the request claim or
it rejects with @TRANSACTIONAL_ID_AUTHORIZATION_FAILED@ (53).
-}
buildProduceRequest
  :: Meta.MetadataCache
  -> Maybe Text
  -> Int16
  -> Int32
  -> [BA.ProducerBatch]
  -> IO PR.ProduceRequest
buildProduceRequest metaCache transactionalIdM acks timeoutMs batches = do
  -- Group batches by topic
  let batchesByTopic =
        groupBy (\b1 b2 -> BA.tpTopic (BA.batchTopicPartition b1) == BA.tpTopic (BA.batchTopicPartition b2)) $
          sortBy (comparing (BA.tpTopic . BA.batchTopicPartition)) batches

  -- Build TopicProduceData for each topic (with compression, so IO)
  topicData <- mapM (buildTopicProduceData metaCache) batchesByTopic

  return $
    PR.ProduceRequest
      { PR.produceRequestTransactionalId = case transactionalIdM of
          Nothing -> P.KafkaString P.Null
          Just tid -> P.mkKafkaString tid
      , PR.produceRequestAcks = acks
      , PR.produceRequestTimeoutMs = timeoutMs
      , PR.produceRequestTopicData = P.mkKafkaArray (V.fromList topicData)
      }


{- | Build TopicProduceData for a group of batches from the same topic.
Compression happens here, so this is an IO operation. Kafka 4.0.0
still keys produce requests by topic name; KIP-516 topic-id
support arrived in later schema revisions.
-}
buildTopicProduceData
  :: Meta.MetadataCache -> [BA.ProducerBatch] -> IO PR.TopicProduceData
buildTopicProduceData _metaCache batches = do
  let topic = BA.tpTopic $ BA.batchTopicPartition $ head batches
  -- 'sendBatches' upstream guarantees that each (topic,
  -- partition) appears at most once in this batch list, so a
  -- straight per-batch mapM produces one PartitionProduceData
  -- per partition.
  --
  -- Kafka 4.0.0 still keys produce requests by topic name (the
  -- KIP-516 topic-id field went away in this schema revision),
  -- so the previous 'Meta.getTopicId' lookup here is no longer
  -- needed and 'metaCache' is unused — see commit cc058b76 on
  -- main for the schema bump that removed the field.
  partitionData <- mapM buildPartitionProduceData batches
  return $
    PR.TopicProduceData
      { PR.topicProduceDataName = P.mkKafkaString topic
      , PR.topicProduceDataPartitionData = P.mkKafkaArray (V.fromList partitionData)
      }


{- | Build PartitionProduceData for a single batch.
Applies compression using the batch's compression codec and level.

'sendBatches' upstream guarantees that each round contains at
most one batch per (topic, partition), so the pre-fix shape of
"one PartitionProduceData per ProducerBatch" is now safe — the
protocol's "one PartitionProduceData per partition per request"
invariant is upheld by the round splitter rather than by
coalescing here.
-}
buildPartitionProduceData :: BA.ProducerBatch -> IO PR.PartitionProduceData
buildPartitionProduceData batch = do
  let partition = BA.tpPartition $ BA.batchTopicPartition batch
      recordBatch = buildRecordBatch batch
      compressionLevel = BA.batchCompressionLevel batch
      codec = RB.attrCompressionType (RB.batchAttributes recordBatch)
  if codec == NoCompression
    then
      pure $
        PR.PartitionProduceData
          { PR.partitionProduceDataIndex = partition
          , PR.partitionProduceDataRecords = P.mkKafkaBytes (RBW.encodeRecordBatchWire recordBatch)
          }
    else do
      encodeResult <- RBW.encodeRecordBatchWireCompressedWithLevel recordBatch compressionLevel
      case encodeResult of
        Left err -> do
          KLog.logWarn
            KLog.kafkaInstrumentationLibrary
            "producer: compression failed for a batch, sending it uncompressed"
            [KLog.attrText "exception.message" (T.pack err)]
          pure $
            PR.PartitionProduceData
              { PR.partitionProduceDataIndex = partition
              , PR.partitionProduceDataRecords = P.mkKafkaBytes (RBW.encodeRecordBatchWire recordBatch)
              }
        Right recordBytes ->
          pure $
            PR.PartitionProduceData
              { PR.partitionProduceDataIndex = partition
              , PR.partitionProduceDataRecords = P.mkKafkaBytes recordBytes
              }


{- | Build a RecordBatch from a ProducerBatch. Idempotent /
transactional state is read off the batch itself
('batchProducerId', 'batchProducerEpoch', 'batchBaseSequence',
'batchIsTransactional'); the producer-side 'sendMessage' /
'sendBatch' path stamps these fields when @producerIdempotent@
or @producerTransactional@ is enabled, via 'BA.appendRecordStamped'.
-}
buildRecordBatch :: BA.ProducerBatch -> RB.RecordBatch
buildRecordBatch batch =
  let !srcRecords = BA.batchRecords batch
      !nRec = V.length srcRecords
      -- The producer-side append path leaves 'recordOffsetDelta'
      -- at 0 because the offset within the batch is only known
      -- once the batch is sealed (the same record could in
      -- principle live in different positions across retries).
      -- Stamp the per-record delta here, immediately before
      -- handing the records to the wire encoder; without it the
      -- broker sees N records all claiming delta 0 and silently
      -- collapses them, which is the data-loss bug the
      -- 'PartitionProduceData' consolidation in
      -- 'buildTopicProduceData' was supposed to expose.
      --
      -- 'V.imap' walks the source 'V.Vector' once with the slot
      -- index inline; the prior 'V.generate' + 'Seq.index'
      -- pattern paid an O(log n) tree walk per slot AND went
      -- through the @Seq -> List -> Vector@ shape conversion
      -- the heap profile flagged.
      !records =
        V.imap
          (\i r -> r {RB.recordOffsetDelta = fromIntegral i})
          srcRecords
      attrs =
        RB.Attributes
          { RB.attrCompressionType = BA.batchCompression batch
          , RB.attrTimestampType = RB.CreateTime
          , RB.attrIsTransactional = BA.batchIsTransactional batch
          , RB.attrIsControl = False
          , RB.attrHasDeleteHorizon = False
          }
      baseTimestamp = BA.batchBaseTimestamp batch
      -- Single fold instead of (V.toList + map + maximum +
      -- intermediate list). 'V.foldl'' walks the vector in one
      -- pass and computes the max delta directly.
      !maxTimestamp =
        if nRec == 0
          then baseTimestamp
          else
            baseTimestamp
              + V.foldl'
                (\acc r -> max acc (RB.recordTimestampDelta r))
                0
                records
  in RB.RecordBatch
       { RB.batchBaseOffset = 0 -- Broker will assign
       , RB.batchPartitionLeaderEpoch = RB.noPartitionLeaderEpoch
       , RB.batchAttributes = attrs
       , RB.batchLastOffsetDelta = fromIntegral (V.length records) - 1
       , RB.batchBaseTimestamp = baseTimestamp
       , RB.batchMaxTimestamp = maxTimestamp
       , RB.batchProducerId = BA.batchProducerId batch
       , RB.batchProducerEpoch = BA.batchProducerEpoch batch
       , RB.batchBaseSequence = BA.batchBaseSequence batch
       , RB.batchRecords = records
       }


{- | Determine if a batch should be retried. Compares the batch's
accrued 'BA.batchAttempts' against the sender's configured
'retryMaxAttempts'. The sender increments 'batchAttempts' via
'bumpBatchAttempts' before re-enqueuing the batch.
-}
shouldRetry :: SenderState -> BA.ProducerBatch -> Bool
shouldRetry SenderState {..} batch =
  BA.batchAttempts batch < retryMaxAttempts senderRetryConfig


{- | Compute the next backoff for /this specific/ batch's accrued
attempt count.
-}
batchBackoffMs :: SenderState -> BA.ProducerBatch -> Int
batchBackoffMs SenderState {..} batch =
  nextRetryBackoffMs senderRetryConfig (BA.batchAttempts batch)


{- | Increment the batch's attempt counter, returning the updated
batch. The sender calls this just before re-enqueueing the
batch onto the partition queue.
-}
bumpBatchAttempts :: BA.ProducerBatch -> BA.ProducerBatch
bumpBatchAttempts batch =
  batch {BA.batchAttempts = BA.batchAttempts batch + 1}


{- | Retry a failed batch: wait for this batch's exponential
backoff, then bump its attempt counter. The caller is
responsible for actually re-enqueueing the returned batch.
-}
retryBatch :: SenderState -> BA.ProducerBatch -> IO BA.ProducerBatch
retryBatch state batch = do
  let backoffMs = batchBackoffMs state batch
  threadDelay (backoffMs * 1000)
  pure (bumpBatchAttempts batch)


{- | Process a ProduceResponse and update batch states. Honours
the broker's @ThrottleTimeMs@ (KIP-219): if the broker has
requested back-pressure, the sender thread sleeps for that
duration /before/ returning to its loop, so the next produce
request waits the broker-requested interval. This is the same
behaviour the JVM client / librdkafka implement.
-}
processProduceResponse
  :: SenderState
  -> [BA.ProducerBatch]
  -> PResp.ProduceResponse
  -> IO ()
processProduceResponse state batches response = do
  -- KIP-219 throttle. Negative / zero values are no-ops.
  let throttleMs = PResp.produceResponseThrottleTimeMs response
  when (throttleMs > 0) $
    threadDelay (fromIntegral throttleMs * 1000)

  -- Extract topic responses
  let topics = case P.unKafkaArray (PResp.produceResponseResponses response) of
        P.Null -> V.empty
        P.NotNull vec -> vec

  -- Pre-index outbound batches by their (topic, partition) so
  -- the per-response lookup is O(1).
  let batchIndex :: HashMap BA.TopicPartition [BA.ProducerBatch]
      !batchIndex =
        HashMap.fromListWith
          (++)
          [(BA.batchTopicPartition b, [b]) | b <- batches]

  -- Process each topic response
  V.forM_ topics $ \topicResp -> do
    let topicName = extractText $ PResp.topicProduceResponseName topicResp
        partitions = case P.unKafkaArray (PResp.topicProduceResponsePartitionResponses topicResp) of
          P.Null -> V.empty
          P.NotNull vec -> vec

    -- Process each partition response
    V.forM_ partitions $ \partResp -> do
      let partitionId = PResp.partitionProduceResponseIndex partResp
          errorCode = PResp.partitionProduceResponseErrorCode partResp
          baseOffset = PResp.partitionProduceResponseBaseOffset partResp
          curLeader = PResp.partitionProduceResponseCurrentLeader partResp
          curLeaderId = PResp.leaderIdAndEpochLeaderId curLeader

      -- KIP-466: if the broker reports a CurrentLeader for this
      -- partition (>= 0), patch the metadata cache so the next
      -- produce goes to that broker without an extra
      -- MetadataRequest round-trip.
      when (curLeaderId >= 0) $
        atomically $
          Meta.updatePartitionLeader (senderMetadata state) topicName partitionId curLeaderId

      let !key = BA.TopicPartition topicName partitionId
          matchingBatches = HashMap.lookupDefault [] key batchIndex

      -- Update batch state based on error code
      forM_ matchingBatches $ \batch -> do
        -- DUPLICATE_SEQUENCE_NUMBER: the broker already has this
        -- batch (a retry of one whose response was lost).
        if errorCode == 0 || errorCode == 46
          then do
            -- Success: complete each record callback with its
            -- assigned offset. We deliberately don't log on every
            -- success — application code observes deliveries via
            -- the per-record callbacks, which are the public
            -- success channel.
            let callbacks = BA.batchCallbacks batch
            timestamp <- KafkaTime.currentTimeMillis
            -- (Per-record timestamps aren't surfaced in
            -- ProduceResponse; the per-record
            -- 'PartitionProduceResponse.LogAppendTimeMs' is
            -- only set when the topic is in @LogAppendTime@ mode,
            -- and we can't tell here, so we use the wall-clock
            -- timestamp as a reasonable fallback.)
            -- Skip the entire per-record metadata construction
            -- when the callback is 'NoRecordCallback'. That's the
            -- shape every '*Drop*' send variant uses; it
            -- dominates the bench harness traffic. The
            -- 'RecordCallback' branch still pays for the strict
            -- UNPACK'd 'BA.BatchAck' (faster than the prior lazy
            -- tuple for callers that /actually consume/ the
            -- metadata), so this trade-off helps both shapes.
            -- 'V.iforM_' walks the 'V.Vector' with its index;
            -- no per-record (idx, callback) tuple allocation
            -- (the prior 'Seq.mapWithIndex (,) callbacks'
            -- materialised a fresh 'Seq' of 2-tuples).
            V.iforM_ callbacks $ \idx callback ->
              case callback of
                BA.NoRecordCallback -> pure ()
                BA.RecordCallback f -> do
                  let !recordOffset = baseOffset + fromIntegral (idx :: Int)
                      !meta =
                        BA.BatchAck
                          { BA.ackTopic = topicName
                          , BA.ackPartition = partitionId
                          , BA.ackOffset = recordOffset
                          , BA.ackTimestamp = timestamp
                          }
                  f (Right meta)
          else do
            let why = "Kafka error: " <> T.pack (show errorCode) <> " (" <> RC.errorMessage errorCode <> ")"
                -- OUT_OF_ORDER_SEQUENCE_NUMBER: an earlier batch of
                -- this partition hasn't landed yet (it is being
                -- retried); 'retryOrFail' gives up at once if that
                -- earlier batch is lost.
                retriable = RC.isRetriable errorCode || (errorCode == 45 && BA.batchBaseSequence batch >= 0)
            when (errorCode `elem` [3, 5, 6, 74, 75]) $ do
              refresh <- readIORef (senderRefreshMetadata state)
              refresh [topicName]
            if retriable
              then retryOrFail state why batch
              else do
                KLog.logError KLog.kafkaInstrumentationLibrary ("producer: batch failed: " <> why) $
                  KLog.errorAttr (RC.errorMessage errorCode) : batchAttrs batch
                failBatch state why batch


-- | Extract text from a KafkaString
extractText :: P.KafkaString -> Text
extractText (P.KafkaString P.Null) = ""
extractText (P.KafkaString (P.NotNull t)) = t
