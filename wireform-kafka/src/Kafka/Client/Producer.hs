{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Kafka.Client.Producer
Description : Send records to a Kafka topic
Copyright   : (c) 2025
License     : BSD-3-Clause

Open a connection to a Kafka cluster and publish records to it.

A 'Producer' is a long-lived handle. Internally it keeps a connection
pool, a per-partition batch accumulator, and a background sender
thread; you keep that handle for the lifetime of your service and
publish records through it concurrently from however many threads
you like.

= Quick start

@
import qualified Kafka.Client.Producer as Producer

main :: IO ()
main =
  Producer.'withProducer' [\"localhost:9092\"] Producer.'defaultProducerConfig' $ \\p -> do
    md \<- 'sendMessage' p \"events\" Nothing \"hello\"
    print md
@

'withProducer' is the recommended entry point: it opens the
connection, hands you the handle, and guarantees the producer is
flushed and closed even if your body throws. If you can't use a
bracket, you can still call 'createProducer' / 'closeProducer'
manually.

= Picking a send function

You almost always want 'sendMessage' (synchronous, returns
'Either') or 'sendMessageAsync' (returns immediately, hands you a
future). The other @send*@ variants exist for very high
throughput pipelines that have specific latency or back-pressure
needs; see the \"Performance-tuned send variants\" section below.

= Configuration

'ProducerConfig' has a knob for every behavior Kafka exposes:
compression, batching, retries, delivery guarantees, idempotence,
transactions, partitioner choice. Start from 'defaultProducerConfig'
and override only the fields you care about. The defaults track the
Kafka 3.x JVM client. Environment variables of the form @KAFKA_*@
are layered on top automatically by 'createProducer' — see
'applyKafkaEnvToProducerConfig'.
-}
module Kafka.Client.Producer (
  -- * Producer lifecycle

  --

  {- | A producer holds a network connection pool, a batch
  accumulator, and a background sender thread; use
  'withProducer' to make sure it is shut down cleanly.
  -}
  Producer,
  withProducer,
  withProducer',
  createProducer,
  closeProducer,
  closeProducerWithTimeout,

  -- * Sending records (untyped)

  --

  {- | 'sendMessage' is the everyday choice: it waits for the
  broker to acknowledge and returns the assigned offset.
  'sendMessageAsync' hands back an 'MVar' you can take on
  later — same record, no blocking. The
  'Performance-tuned send variants' section below covers the
  fire-and-forget @_@-suffixed variants.
  -}
  sendMessage,
  sendMessageAsync,
  ProducerRecord (..),
  RecordMetadata (..),
  PartitionInfo (..),

  -- * Sending records (typed)

  --

  {- | The 'publish' family takes a 'Kafka.Topic.Topic' that
  bundles the topic name with its key and value serdes, so
  callers don't have to round-trip every send through
  'Data.Text.Encoding.encodeUtf8' \/ JSON encoders by hand.
  -}
  publish,
  publish_,

  -- * Flushing

  --

  {- | 'flushProducer' blocks until everything currently buffered
  has reached the broker. Always call it (or rely on
  'withProducer', which does) before tearing down the producer
  if you care about at-least-once delivery.
  -}
  flushProducer,
  purgeProducer,

  -- * Configuration
  ProducerConfig (..),
  defaultProducerConfig,
  DeliveryGuarantee (..),
  validateProducerConfig,

  -- * Partitioning

  --

  {- | By default, records with a key are routed by a hash of
  the key (so the same key always lands on the same
  partition); records without a key use the sticky
  partitioner to maximise batching. Override with
  'roundRobinPartitioner', 'hashPartitioner', or write your
  own 'Partitioner'.
  -}
  Partitioner,
  defaultPartitioner,
  roundRobinPartitioner,
  hashPartitioner,
  stickyPartitioner,

  -- * Transactions

  --

  {- | The transaction lifecycle (init / begin / commit / abort /
  send offsets) lives in "Kafka.Client.Transaction". Once you
  have a 'Txn.Transaction', call 'bindTransaction' to make
  subsequent 'sendMessage' calls participate in it.

  After binding:

  * 'sendMessage' is rejected with 'Left' unless the
    transaction is in the 'Txn.InTransaction' state;
  * the first 'sendMessage' on a (topic, partition) issues an
    @AddPartitionsToTxn@ to the coordinator before enqueuing
    the record;
  * outgoing record batches are stamped with the
    transactional producer-id / epoch / sequence and have
    their @attrIsTransactional@ bit set.
  -}
  bindTransaction,
  newBoundTransaction,
  producerBoundTransaction,
  producerTxnGate,

  -- * Cluster info
  producerClusterId,
  partitionsFor,
  producerHealthy,

  -- * KIP-714 client telemetry id

  --

  {- | Returns the producer's client-instance id. Mirrors
  @KafkaProducer.clientInstanceId(Duration)@. Deterministic
  locally-derived UUID (same shape as
  'Kafka.Client.Consumer.clientInstanceId'); broker-side
  telemetry assignment (KIP-714 client side) lands here
  when the @GetTelemetrySubscriptions@ pipeline does.
  -}
  producerClientInstanceId,

  -- * Environment-variable overlay

  --

  {- | 'createProducer' already reads @KAFKA_*@ env vars and
  layers them on top of the supplied 'ProducerConfig'
  automatically. These helpers are exported for callers
  that want to inspect or pre-apply the overlay manually
  (e.g. to log the effective config before connecting).
  -}
  applyKafkaEnvToProducerConfig,
  producerConfigFromEnv,

  -- * Performance-tuned send variants

  --

  {- | The standard send functions are 'sendMessage' (synchronous
  ack) and 'sendMessageAsync' (future-based). The variants
  below trade safety or feedback for throughput; reach for
  them only when a benchmark shows you need them.

  * 'sendMessage_' — fire-and-forget, no future, no
    wait. Still uses the partitioner and accumulator.
  * 'sendMessageUnsafe_' — same as 'sendMessage_' but
    skips the txn / idempotence guard rails. Only safe on a
    plain non-transactional producer.
  * 'sendMessageFastest_' — caches the last touched
    batch so repeated sends to the same (topic, partition)
    skip the per-record map lookup. Use in tight inner
    loops that fan-in to one partition.
  * 'sendMessages_' — bulk variant of
    'sendMessageUnsafe_' for a list of records.
  * 'sendBatch' — bypasses the accumulator entirely and
    ships an explicit batch. Used by the perf tool.
  -}
  sendMessage_,
  sendRecord,
  sendRecordAsync,
  sendMessageUnsafe_,
  sendMessageFastest_,
  sendMessages_,
  sendBatch,

  -- * Enhanced ack callbacks

  --

  {- | 'EnhancedCallback' is the per-stage producer hook shape
  (enqueue, send, ack, retry, delivered). Use it when you want
  a single bundle of observability hooks instead of wiring
  'producerInterceptor' / 'producerOnAcknowledgement'
  separately.
  -}
  EnhancedCallback (..),
  noopEnhancedCallback,
  dispatchEnhanced,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar)
import Control.Concurrent.STM
import Control.Exception (SomeException, bracket, throwIO, try)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (hash)
import Data.Hashable qualified as Hashable
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Int
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)
import Kafka.Client.ConfigValidation (ConfigError, renderConfigErrors)
import Kafka.Client.ConfigValidation qualified as CV
import Kafka.Client.Consumer qualified as KCC
import Kafka.Client.Env qualified as Env
import Kafka.Client.Internal.BatchAccumulator qualified as BA
import Kafka.Client.Internal.Murmur2 qualified as Murmur2
import Kafka.Client.Internal.ProducerSender qualified as Sender
import Kafka.Client.Internal.TransactionCoordinator qualified as TC
import Kafka.Client.Metadata qualified as Meta
import Kafka.Client.RecordMetadata qualified as RM
import Kafka.Client.Telemetry qualified as Telemetry
import Kafka.Client.TopicId qualified as TopicIdImp
import Kafka.Client.Transaction qualified as Txn
import Kafka.Compression.Types (CompressionCodec, defaultCodec)
import Kafka.Compression.Types qualified as Compression
import Kafka.Errors qualified as Errors
import Kafka.Headers qualified as H
import Kafka.Network.Connection qualified as Conn
import Kafka.Protocol.ApiVersions qualified as AV
import Kafka.Protocol.RecordBatch qualified as RB
import Kafka.Protocol.VersionNegotiation qualified as VN
import Kafka.Serde qualified as Serde
import Kafka.Time qualified as KafkaTime
import Kafka.Topic qualified as Topic
import ListT qualified
import StmContainers.Map qualified as StmMap
import System.Timeout (timeout)


-- | Delivery guarantee level.
data DeliveryGuarantee
  = -- | Fire and forget, no acknowledgment
    AtMostOnce
  | -- | Wait for acknowledgment, may duplicate
    AtLeastOnce
  | -- | Transactional, exactly-once semantics
    ExactlyOnce
  deriving (Eq, Show, Generic)


-- | Producer configuration.
data ProducerConfig = ProducerConfig
  { producerClientId :: !Text
  -- ^ Client identifier
  , producerCompression :: !CompressionCodec
  -- ^ Compression codec to use
  , producerCompressionLevel :: !(Maybe Int)
  {- ^ Compression level (Nothing = use codec default; KIP-353/776/909)
  Gzip: 0-9, Zstd: 1-22, LZ4: 0-16, Snappy: 0-9 (placeholder)
  -}
  , producerBatchSize :: !Int
  -- ^ Maximum batch size in bytes (default: 16384)
  , producerLingerMs :: !Int
  -- ^ Time to wait for batching in milliseconds (default: 0)
  , producerMaxInFlight :: !Int
  -- ^ Maximum in-flight requests per connection (default: 5)
  , producerRetries :: !Int
  {- ^ Number of retries on transient errors. Default: @2147483647@
  (librdkafka @retries@: effectively unlimited; capped by
  'producerDeliveryTimeoutMs').
  -}
  , producerRetryBackoffMs :: !Int
  {- ^ Initial backoff after a retriable error in ms. Default 100.
  Mirrors librdkafka @retry.backoff.ms@.
  -}
  , producerRetryBackoffMaxMs :: !Int
  {- ^ Ceiling for the exponential backoff in ms. Default 1000.
  Mirrors librdkafka @retry.backoff.max.ms@.
  -}
  , producerRetryBackoffMultiplier :: !Double
  -- ^ Multiplier between consecutive retry backoffs. Default 2.0.
  , producerRetryBackoffJitter :: !Double
  {- ^ Jitter band [0.0, 1.0]; the actual backoff is
  uniformly randomised in @backoff * (1 ± jitter)@.
  Default 0.2.
  -}
  , producerDeliveryTimeoutMs :: !Int
  -- ^ Maximum time for a record to be delivered including retries (default: 120000ms = 2 minutes)
  , producerRequestTimeoutMs :: !Int
  {- ^ Per-request timeout in ms. Bounded above by
  'producerDeliveryTimeoutMs'. Default 30000.
  Mirrors librdkafka @request.timeout.ms@ /
  @socket.timeout.ms@.
  -}
  , producerMaxRequestSize :: !Int
  {- ^ Cap for the size of a single ProduceRequest in bytes.
  Default 1048576 (1 MiB), matching librdkafka @message.max.bytes@.
  -}
  , producerQueueBufferingMaxMessages :: !Int
  {- ^ Max records the accumulator buffers across all partitions.
  Default 100000. Mirrors librdkafka
  @queue.buffering.max.messages@.
  -}
  , producerQueueBufferingMaxKbytes :: !Int
  {- ^ Max bytes the accumulator buffers across all partitions.
  Default 1048576 (1 GiB). Mirrors librdkafka
  @queue.buffering.max.kbytes@.
  -}
  , producerTransactionTimeoutMs :: !Int
  {- ^ How long the broker holds an open transaction before
  aborting it. Default 60000. Mirrors librdkafka
  @transaction.timeout.ms@.
  -}
  , producerEnableGaplessGuarantee :: !Bool
  {- ^ For idempotent producers, fail-fast on a sequence-number
  gap rather than dedup. Default 'False'. Mirrors librdkafka
  @enable.gapless.guarantee@.
  -}
  , producerStickyPartitioningLingerMs :: !Int
  {- ^ Sticky partitioner: linger this long on a partition
  before switching, regardless of batch size (KIP-480).
  Default 10. Mirrors librdkafka
  @sticky.partitioning.linger.ms@.
  -}
  , producerPartitioner :: !Partitioner
  -- ^ Partitioning strategy (default: DefaultPartitioner with sticky behavior - KIP-480)
  , producerDelivery :: !DeliveryGuarantee
  -- ^ Delivery guarantee (default: AtLeastOnce)
  , producerIdempotent :: !Bool
  -- ^ Enable idempotent producer (default: False)
  , producerTransactional :: !(Maybe Text)
  -- ^ Transactional ID (Nothing = non-transactional)
  , producerInterceptor :: !(ProducerRecord -> IO ProducerRecord)
  {- ^ Pre-send interceptor. Mirrors
  @org.apache.kafka.clients.producer.ProducerInterceptor.onSend@.
  Applied to every record /before/ partition selection +
  batch accumulation. Defaults to 'pure', i.e. the record
  is forwarded unchanged. Exceptions in the interceptor
  are propagated to the caller of 'sendMessage' (the JVM
  client logs + skips, but a typed exception is more
  useful in Haskell).
  -}
  , producerOnAcknowledgement
      :: !(ProducerRecord -> Either String RecordMetadata -> IO ())
  {- ^ Per-record acknowledgement callback. Mirrors
  @ProducerInterceptor.onAcknowledgement@. Called from the
  sender thread when the broker ACK arrives (success path)
  or when the record's delivery times out / fails. Exceptions
  are caught and dropped so an interceptor bug can't take
  down the sender loop.
  -}
  , producerCompressionDictionary :: !(Maybe ByteString)
  {- ^ Optional zstd dictionary used by the configured
  'producerCompression' codec (currently only honoured for
  'Kafka.Compression.Zstd'). Mirrors librdkafka's
  @compression.dictionary@. Default 'Nothing' (no dict).
  -}
  }
  deriving (Generic)


{- | Default producer configuration.

Defaults track the Java 3.x client one-for-one, which means
__idempotent + acks=all out of the box__. In our typed enum:

  * 'producerIdempotent' = 'True'  (Java @enable.idempotence=true@)
  * 'producerDelivery'   = 'ExactlyOnce' (Java @acks=all@)
  * 'producerMaxInFlight' = 5      (Java @max.in.flight.requests.per.connection@)

These three together give the strongest single-producer
delivery guarantees Kafka offers (no duplicates, no
reordering) and are the right default for almost every
application. Override to 'AtLeastOnce' \/ 'AtMostOnce' if you
specifically need lower latency or want to skip the
@InitProducerId@ round-trip.
-}
defaultProducerConfig :: ProducerConfig
defaultProducerConfig =
  ProducerConfig
    { producerClientId = "kafka-native-producer"
    , producerCompression = defaultCodec
    , producerCompressionLevel = Nothing -- Use codec default
    , producerBatchSize = 16384
    , producerLingerMs = 0 -- Java @linger.ms=0@
    , producerMaxInFlight = 5
    , producerRetries = 2_147_483_647 -- Java @retries=MAX_VALUE@
    , producerRetryBackoffMs = 100
    , producerRetryBackoffMaxMs = 1000
    , producerRetryBackoffMultiplier = 2.0
    , producerRetryBackoffJitter = 0.2
    , producerDeliveryTimeoutMs = 120_000 -- KIP-91
    , producerRequestTimeoutMs = 30_000
    , producerMaxRequestSize = 1_048_576 -- 1 MiB
    , producerQueueBufferingMaxMessages = 100_000
    , producerQueueBufferingMaxKbytes = 1_048_576 -- 1 GiB worth of records
    , producerTransactionTimeoutMs = 60_000
    , producerEnableGaplessGuarantee = False
    , producerStickyPartitioningLingerMs = 10
    , producerPartitioner = defaultPartitioner
    , producerDelivery = ExactlyOnce -- Java @acks=all@
    , producerIdempotent = True -- Java @enable.idempotence=true@
    , producerTransactional = Nothing
    , producerInterceptor = pure
    , producerOnAcknowledgement = \_ _ -> pure ()
    , producerCompressionDictionary = Nothing
    }


{- | Overlay a parsed 'Env.KafkaEnv' onto a 'ProducerConfig'. Only
fields whose corresponding @KAFKA_*@ variable was set get
touched, so this composes cleanly on top of
'defaultProducerConfig' or any already-customised config.

Returns @Right@ in the current implementation; the result type
is kept as 'Either' so future cross-field validation (e.g.
@KAFKA_ENABLE_IDEMPOTENCE=true@ with @KAFKA_ACKS=0@) can be
added without breaking callers.
-}
applyKafkaEnvToProducerConfig
  :: Env.KafkaEnv
  -> ProducerConfig
  -> Either [ConfigError] ProducerConfig
applyKafkaEnvToProducerConfig env cfg =
  Right
    cfg
      { producerClientId =
          maybe (producerClientId cfg) id (Env.envClientId env)
      , producerCompression =
          maybe (producerCompression cfg) id (Env.envCompressionType env)
      , producerCompressionLevel = case Env.envCompressionLevel env of
          Just _ -> Env.envCompressionLevel env
          Nothing -> producerCompressionLevel cfg
      , producerBatchSize =
          maybe (producerBatchSize cfg) id (Env.envBatchSize env)
      , producerLingerMs =
          maybe (producerLingerMs cfg) id (Env.envLingerMs env)
      , producerMaxInFlight =
          maybe (producerMaxInFlight cfg) id (Env.envMaxInFlightRequestsPerConn env)
      , producerRetries =
          maybe (producerRetries cfg) id (Env.envRetries env)
      , producerRetryBackoffMs =
          maybe (producerRetryBackoffMs cfg) id (Env.envRetryBackoffMs env)
      , producerRetryBackoffMaxMs =
          maybe (producerRetryBackoffMaxMs cfg) id (Env.envRetryBackoffMaxMs env)
      , producerDeliveryTimeoutMs =
          maybe (producerDeliveryTimeoutMs cfg) id (Env.envDeliveryTimeoutMs env)
      , producerRequestTimeoutMs =
          maybe (producerRequestTimeoutMs cfg) id (Env.envRequestTimeoutMs env)
      , producerMaxRequestSize =
          maybe (producerMaxRequestSize cfg) id (Env.envMaxRequestSize env)
      , producerTransactionTimeoutMs =
          maybe (producerTransactionTimeoutMs cfg) id (Env.envTransactionTimeoutMs env)
      , producerDelivery =
          maybe (producerDelivery cfg) acksToDelivery (Env.envAcks env)
      , producerIdempotent =
          maybe (producerIdempotent cfg) id (Env.envEnableIdempotence env)
      , producerTransactional = case Env.envTransactionalId env of
          Just _ -> Env.envTransactionalId env
          Nothing -> producerTransactional cfg
      }
  where
    acksToDelivery Env.EnvAcksZero = AtMostOnce
    acksToDelivery Env.EnvAcksOne = AtLeastOnce
    acksToDelivery Env.EnvAcksAll = ExactlyOnce


{- | Read every @KAFKA_*@ variable from the process environment
and overlay them on top of the supplied 'ProducerConfig'.
This is the @IO@ wrapper around 'applyKafkaEnvToProducerConfig'
+ 'Env.loadKafkaEnv'.
-}
producerConfigFromEnv
  :: MonadIO m
  => ProducerConfig
  -> m (Either [ConfigError] ProducerConfig)
producerConfigFromEnv cfg = liftIO $ do
  r <- Env.loadKafkaEnv
  case r of
    Left errs -> pure (Left errs)
    Right env -> pure (applyKafkaEnvToProducerConfig env cfg)


-- | A record to be sent to Kafka.
data ProducerRecord = ProducerRecord
  { topic :: !Text
  -- ^ Target topic
  , key :: !(Maybe ByteString)
  -- ^ Optional message key (for partitioning and compaction)
  , value :: !(Maybe ByteString)
  -- ^ Message value; 'Nothing' sends a null value (a tombstone)
  , headers :: ![(Text, ByteString)]
  -- ^ Optional headers
  , partition :: !(Maybe Int32)
  -- ^ Explicit partition (overrides partitioner)
  , timestamp :: !(Maybe Int64)
  -- ^ Message timestamp (Nothing = broker assigns)
  }
  deriving (Eq, Show, Generic)


-- | Metadata about a successfully sent record.
data RecordMetadata = RecordMetadata
  { topic :: !Text
  -- ^ Topic name
  , partition :: !Int32
  -- ^ Partition number
  , offset :: !Int64
  -- ^ Offset within partition
  , timestamp :: !Int64
  -- ^ Broker-assigned timestamp
  }
  deriving (Eq, Show, Generic)


{- | Metadata for one topic partition. Mirrors the useful subset of
Java's @PartitionInfo@: partition id, leader broker id, replicas,
and in-sync replicas.
-}
data PartitionInfo = PartitionInfo
  { partitionInfoTopic :: !Text
  , partitionInfoPartition :: !Int32
  , partitionInfoLeader :: !(Maybe Int32)
  , partitionInfoReplicas :: ![Int32]
  , partitionInfoIsrs :: ![Int32]
  }
  deriving (Eq, Show, Generic)


{- | Partitioning strategy for messages.
| Partitioner function type. The default partitioner uses
sticky partitioning to maximise batching when no key is set.

A partitioner determines which partition a message goes to.
This affects batching, ordering, and load distribution.

The function receives:
  - Producer state (for sticky/round-robin counters)
  - Topic name
  - Optional message key
  - Partition count for the topic

And returns the selected partition (0-indexed).
-}
type Partitioner = Producer -> Text -> Maybe ByteString -> Int32 -> IO Int32


{- | Default partitioner: hash-based when a key is present,
sticky-per-batch otherwise.

This is the Kafka default behavior:
  - If a key is provided, use a hash of the key
  - If no key, use sticky partitioning for better batching
-}
defaultPartitioner :: Partitioner
defaultPartitioner producer topic keyM partitionCount = case keyM of
  Just key -> return $ hashPartition key partitionCount
  Nothing -> getStickyPartition producer topic partitionCount


{- | Round-robin partitioner: evenly distribute across partitions.

Cycles through partitions in order, providing even distribution
but potentially worse batching than sticky partitioner.
-}
roundRobinPartitioner :: Partitioner
roundRobinPartitioner producer topic _key partitionCount =
  getRoundRobinPartition producer topic partitionCount


{- | Hash partitioner: always use key hash (requires key).

If no key is provided, falls back to partition 0.
For best results, always provide keys when using this partitioner.
-}
hashPartitioner :: Partitioner
hashPartitioner _producer _topic keyM partitionCount = case keyM of
  Just key -> return $ hashPartition key partitionCount
  Nothing -> return 0 -- Fallback if no key


{- | Sticky partitioner: maximise batching by sticking to the
same partition until the current batch is ready.

Sticks to the same partition until the batch is full, then switches.
This improves batching efficiency and throughput compared to round-robin.
-}
stickyPartitioner :: Partitioner
stickyPartitioner producer topic _key partitionCount =
  getStickyPartition producer topic partitionCount


-- | Kafka producer handle.
data Producer = Producer
  { producerConfig :: !ProducerConfig
  -- ^ Configuration
  , producerMetadata :: !Meta.MetadataCache
  -- ^ Metadata cache for partition leaders
  , producerAccumulator :: !BA.BatchAccumulator
  -- ^ Batch accumulator for buffering records
  , producerConnManager :: !Conn.ConnectionManager
  -- ^ Connection manager for broker connections
  , producerConnConfig :: !Conn.ConnectionConfig
  {- ^ Connection-level configuration (TLS / SASL / socket
  buffers / etc.) used for every broker connection this
  producer opens. Populated by 'createProducer' from
  'Conn.defaultConnectionConfig' with any
  'Kafka.Client.Env' overrides applied on top.
  -}
  , producerSender :: !(Async ())
  -- ^ Background sender thread
  , producerSenderState :: !Sender.SenderState
  -- ^ State for the sender thread
  , producerStickyPartitions :: !(IORef (HashMap.HashMap Text Int32))
  {- ^ Sticky partition state per topic (KIP-480). Consulted on
  every 'sendMessageAsync' / 'sendMessage_' call, so
  moving from 'StmMap' to 'IORef HashMap' (single
  'readIORef' per call instead of an STM transaction) is
  worth a measurable ~80-100 ns per record on the
  sender-side hot path.
  -}
  , producerRoundRobinCounters :: !(IORef (HashMap.HashMap Text Int32))
  {- ^ Round-robin counters per topic. Same hot-path
  conversion as 'producerStickyPartitions'.
  -}
  , producerPartitionCount :: !(IORef (HashMap.HashMap Text Int32))
  {- ^ Per-topic cached partition count. Populated lazily on
  first send to a topic from the metadata cache. Without
  this cache, every 'selectPartition' call paid an STM
  transaction against 'producerMetadata' just to read a
  value that almost never changes. The cache is a strict
  superset of the sticky-partition state; the sticky map
  only kicks in for the partitioner's choice.
  -}
  , producerLastBatch :: !(IORef (Maybe (Text, Int32, BA.PartitionQueue, BA.BatchAccumulating)))
  {- ^ Producer-local cache of the last-touched
  @(topic, partition, queue, batch)@ tuple for
  'sendMessageFastest_'. Saves the per-record sticky
  lookup, partition-map lookup, and 'queueCurrentBatch'
  read on the hot path; invalidated when the cached
  batch seals or the topic / partition changes. Cache
  miss falls through to 'sendMessageUnsafe_'.
  -}
  , producerIdempotentId :: !(TVar Int64)
  {- ^ Producer id (KIP-98). 'noProducerId' (= -1) for
  non-idempotent / non-transactional producers; otherwise
  set by 'InitProducerId' on the broker. We initialise it
  to 'noProducerId' and let the transactional bootstrap
  path overwrite it.
  -}
  , producerIdempotentEpoch :: !(TVar Int16)
  {- ^ Producer epoch from 'InitProducerId'. Pairs with
  'producerIdempotentId'.
  -}
  , producerSequenceNumbers :: !(StmMap.Map BA.TopicPartition Int32)
  {- ^ Per-(topic, partition) next sequence number to stamp
  onto the next batch's 'batchBaseSequence'. KIP-98.
  -}
  , producerTransaction :: !(TVar (Maybe Txn.Transaction))
  {- ^ When 'Just', the producer is bound to a 'Txn.Transaction'
  and 'sendMessage' participates in its KIP-98 / KIP-447
  transactional lifecycle: state is enforced to be
  'Txn.InTransaction', record batches are stamped with the
  transaction's producer-id / epoch and the
  'attrIsTransactional' bit, and the first send to a
  (topic, partition) lazily issues an
  @AddPartitionsToTxn@ to the coordinator. Set via
  'bindTransaction'; cleared on 'closeProducer' (which also
  aborts an open txn).
  -}
  , producerRegisteredPartitions :: !(StmMap.Map BA.TopicPartition ())
  {- ^ Partitions that have already been registered with the
  transaction coordinator inside the /current/
  transaction. Reset by 'beginTransaction' / on each
  commit/abort cycle. Used to make
  'AddPartitionsToTxn' idempotent without re-issuing the
  request on every send.
  -}
  }


{- | Create a new Kafka producer.

Steps:
  1. Parse broker addresses
  2. Connect to bootstrap brokers and fetch metadata
  3. Initialize batch accumulator
  4. Start background sender thread

Transactional initialisation is /not/ part of 'createProducer': set
'producerTransactional' here to mark the producer as
transactional, then create the coordinator handle via
'Kafka.Client.Transaction.createTransaction' /
'initTransactions' and 'bindTransaction' it to this producer
before the first 'sendMessage'.
| Pure config-validation rules. Each rule mirrors a
check the JVM client performs in
@org.apache.kafka.clients.producer.ProducerConfig@; we run them
before opening any socket so an obviously broken config (e.g.
@delivery.timeout.ms < request.timeout.ms@) fails fast with a
clear message instead of being deferred to the broker as an
opaque @POLICY_VIOLATION@ much later.

Returns the empty list when the config is acceptable.
-}
validateProducerConfig :: ProducerConfig -> [ConfigError]
validateProducerConfig ProducerConfig {..} =
  concat
    [ CV.check
        (T.null producerClientId)
        "client.id"
        "must be non-empty"
    , CV.check
        (producerBatchSize < 0)
        "batch.size"
        "must be >= 0"
    , CV.check
        (producerLingerMs < 0)
        "linger.ms"
        "must be >= 0"
    , CV.check
        (producerMaxInFlight < 1)
        "max.in.flight.requests.per.connection"
        "must be >= 1"
    , CV.check
        (producerRetries < 0)
        "retries"
        "must be >= 0"
    , CV.check
        (producerRetryBackoffMs < 0)
        "retry.backoff.ms"
        "must be >= 0"
    , CV.check
        (producerRetryBackoffMaxMs < producerRetryBackoffMs)
        "retry.backoff.max.ms"
        "must be >= retry.backoff.ms"
    , CV.check
        (producerRetryBackoffMultiplier < 1.0)
        "retry.backoff.multiplier"
        "must be >= 1.0 (otherwise backoff shrinks)"
    , CV.check
        (producerRetryBackoffJitter < 0.0 || producerRetryBackoffJitter > 1.0)
        "retry.backoff.jitter"
        "must be in [0.0, 1.0]"
    , CV.check
        (producerRequestTimeoutMs <= 0)
        "request.timeout.ms"
        "must be > 0"
    , CV.check
        (producerDeliveryTimeoutMs < producerRequestTimeoutMs + producerLingerMs)
        "delivery.timeout.ms"
        "must be >= request.timeout.ms + linger.ms (KIP-91 invariant)"
    , CV.check
        (producerMaxRequestSize <= 0)
        "message.max.bytes"
        "must be > 0"
    , CV.check
        (producerQueueBufferingMaxMessages <= 0)
        "queue.buffering.max.messages"
        "must be > 0"
    , CV.check
        (producerQueueBufferingMaxKbytes <= 0)
        "queue.buffering.max.kbytes"
        "must be > 0"
    , CV.check
        (producerTransactionTimeoutMs <= 0)
        "transaction.timeout.ms"
        "must be > 0"
    , CV.check
        (producerStickyPartitioningLingerMs < 0)
        "sticky.partitioning.linger.ms"
        "must be >= 0"
    , -- Idempotence / EOS coupling. KIP-679 hard-cap of in-flight=5
      -- for the idempotent producer to preserve sequence ordering.
      CV.check
        (producerIdempotent && producerMaxInFlight > 5)
        "max.in.flight.requests.per.connection"
        "must be <= 5 when enable.idempotence=true (KIP-679)"
    , CV.check
        (producerIdempotent && producerDelivery == AtMostOnce)
        "acks"
        "must be all/-1 (ExactlyOnce/AtLeastOnce) when enable.idempotence=true"
    , CV.check
        (isJustNonEmpty producerTransactional && not producerIdempotent)
        "enable.idempotence"
        "transactional.id requires enable.idempotence=true (KIP-98)"
    , CV.check
        ( isJustNonEmpty producerTransactional
            && producerDelivery /= ExactlyOnce
        )
        "acks"
        "transactional producers require acks=all (ExactlyOnce delivery guarantee)"
    ]
  where
    isJustNonEmpty (Just t) = not (T.null t)
    isJustNonEmpty Nothing = False


createProducer
  :: MonadIO m
  => [Text]
  {- ^ Bootstrap broker addresses (host:port format).
  Falls back to @KAFKA_BOOTSTRAP_SERVERS@ when empty.
  -}
  -> ProducerConfig
  {- ^ Configuration. 'Kafka.Client.Env' env-var
  overrides are layered on top automatically;
  to opt out, ensure no @KAFKA_*@ variables
  are set in the process environment.
  -}
  -> m (Either String Producer)
createProducer brokerAddrs0 config0 = liftIO $ do
  envR <- Env.loadKafkaEnv
  case envR of
    Left errs -> return $ Left $ renderConfigErrors errs
    Right env -> case applyKafkaEnvToProducerConfig env config0 of
      Left errs -> return $ Left $ renderConfigErrors errs
      Right cfg1 ->
        case Env.applyKafkaEnvToConnectionConfig
          env
          ( Conn.defaultConnectionConfig
              { Conn.connClientId = producerClientId cfg1
              }
          ) of
          Left errs -> return $ Left $ renderConfigErrors errs
          Right connConfig0 ->
            let brokerAddrs = case Env.envBootstrapServers env of
                  Just bs | null brokerAddrs0 -> bs
                  _ -> brokerAddrs0
            in case validateProducerConfig cfg1 of
                 errs@(_ : _) -> return $ Left $ renderConfigErrors errs
                 [] -> createProducer' brokerAddrs cfg1 connConfig0


createProducer'
  :: [Text]
  -> ProducerConfig
  -> Conn.ConnectionConfig
  -> IO (Either String Producer)
createProducer' brokerAddrs config connConfig = do
  -- Parse broker addresses
  let parsedBrokers = map parseBrokerAddress brokerAddrs

  -- Validate all brokers parsed successfully
  case sequence parsedBrokers of
    Left err -> return $ Left $ "Failed to parse broker addresses: " ++ err
    Right [] ->
      return $
        Left
          "createProducer: no bootstrap brokers (pass them as the first arg or set KAFKA_BOOTSTRAP_SERVERS)"
    Right brokers -> do
      -- Create metadata cache
      metadataCache <- Meta.createMetadataCache

      -- Create connection manager
      connManager <- Conn.createConnectionManager

      -- Fetch initial metadata from first bootstrap broker
      let firstBroker = head brokers
      connResult <- Conn.getOrCreateConnection connManager firstBroker connConfig
      case connResult of
        Left err -> return $ Left $ "Failed to connect to bootstrap broker: " ++ err
        Right conn -> do
          -- Per-producer API version cache (shared with the
          -- Sender below). We negotiate against the bootstrap
          -- broker right after connect so the sender's first
          -- request finds a populated cache; brokers that
          -- don't speak ApiVersions silently leave the cache
          -- empty and the sender falls back to its compiled-in
          -- defaults.
          versionCache <- AV.createVersionCache
          handshakeCorrId <- newIORef 0
          let nextHandshakeCid = atomicModifyIORef' handshakeCorrId $ \cid -> (cid + 1, cid)
          _ <-
            VN.ensureVersionsNegotiated
              conn
              firstBroker
              versionCache
              nextHandshakeCid

          fetchResult <- Meta.refreshMetadata conn metadataCache 0
          case fetchResult of
            Left err -> return $ Left $ "Failed to fetch initial metadata: " ++ err
            Right _ ->
              setupProducer config connConfig metadataCache connManager versionCache
  where
    setupProducer config connConfig' metadataCache connManager versionCache = do
      -- Create batch accumulator
      -- Determine compression level to use
      let compressionLevel = case producerCompressionLevel config of
            Just level -> level
            Nothing -> Compression.defaultLevel (producerCompression config)

      accumulator <-
        BA.createBatchAccumulator
          (producerBatchSize config)
          (producerLingerMs config)
          (producerCompression config)
          compressionLevel

      -- Determine required acks based on delivery guarantee
      let acks = case producerDelivery config of
            AtMostOnce -> 0 -- Fire and forget
            AtLeastOnce -> 1 -- Leader acknowledgment
            ExactlyOnce -> (-1) -- All ISRs (requires idempotent/transactional)

      -- Create sender state
      let retryConfig =
            Sender.RetryConfig
              { Sender.retryMaxAttempts = producerRetries config
              , Sender.retryBackoffMs = producerRetryBackoffMs config
              , Sender.retryBackoffMaxMs = producerRetryBackoffMaxMs config
              , Sender.retryBackoffMultiplier = producerRetryBackoffMultiplier config
              , Sender.retryBackoffJitter = producerRetryBackoffJitter config
              }

      senderState <-
        Sender.createSenderState
          accumulator
          metadataCache
          connManager
          connConfig'
          retryConfig
          acks
          (producerDeliveryTimeoutMs config) -- KIP-91: delivery timeout
          (producerCompression config)
          (producerClientId config)
          versionCache
          (producerMaxInFlight config)

      -- Start sender thread
      senderThread <- Sender.startSenderThread senderState

      -- Initialize sticky partition and round-robin state (KIP-480)
      stickyPartitions <- newIORef HashMap.empty
      roundRobinCounters <- newIORef HashMap.empty
      partitionCounts <- newIORef HashMap.empty
      lastBatchCache <- newIORef Nothing

      -- Idempotent / transactional producer state (KIP-98). The
      -- producer id + epoch are populated by 'InitProducerId' on
      -- first transactional bootstrap; until then they default to
      -- 'noProducerId' / 'noProducerEpoch'. Non-idempotent
      -- producers leave both at the no-op sentinels and the
      -- sender writes 'RB.noProducerId' / 'RB.noSequence' onto
      -- every batch.
      idempotentPid <- newTVarIO RB.noProducerId
      idempotentEpoch <- newTVarIO RB.noProducerEpoch
      sequenceNumbers <- StmMap.newIO
      transaction <- newTVarIO Nothing
      registeredParts <- StmMap.newIO

      -- Return producer handle
      return $
        Right
          Producer
            { producerConfig = config
            , producerMetadata = metadataCache
            , producerAccumulator = accumulator
            , producerConnManager = connManager
            , producerConnConfig = connConfig'
            , producerSender = senderThread
            , producerSenderState = senderState
            , producerStickyPartitions = stickyPartitions
            , producerRoundRobinCounters = roundRobinCounters
            , producerPartitionCount = partitionCounts
            , producerLastBatch = lastBatchCache
            , producerIdempotentId = idempotentPid
            , producerIdempotentEpoch = idempotentEpoch
            , producerSequenceNumbers = sequenceNumbers
            , producerTransaction = transaction
            , producerRegisteredPartitions = registeredParts
            }


-- | Parse broker address in "host:port" format
parseBrokerAddress :: Text -> Either String Conn.BrokerAddress
parseBrokerAddress addr =
  case T.splitOn ":" addr of
    [host, portText] ->
      case reads (T.unpack portText) of
        [(port, "")] -> Right $ Conn.BrokerAddress (T.unpack host) port
        _ -> Left $ "Invalid port: " ++ T.unpack portText
    _ -> Left $ "Invalid broker address format (expected host:port): " ++ T.unpack addr


{- | Open a producer, run an action with it, and tear it down safely.

This is the recommended way to use a 'Producer'. The bracket
guarantees that 'closeProducer' runs even if the body throws,
which in turn flushes any buffered records and aborts an open
transaction. Any startup failure (broker unreachable, config
invalid, etc.) is raised as an 'IOError' so you can decide
whether to retry the whole bracket.

@
'withProducer' [\"localhost:9092\"] 'defaultProducerConfig' $ \\p -> do
  _ <- 'sendMessage' p \"events\" Nothing \"hello\"
  _ <- 'sendMessage' p \"events\" Nothing \"again\"
  pure ()
@

If you can't structure your program around a bracket — for
example, the producer lives in a long-running service whose
handle is stored in some larger record — drop down to
'createProducer' and 'closeProducer' and own the lifetime
explicitly.
-}
withProducer
  :: MonadUnliftIO m
  => [Text]
  {- ^ Bootstrap brokers, e.g. @[\"localhost:9092\"]@.
  Empty list falls back to @KAFKA_BOOTSTRAP_SERVERS@.
  -}
  -> ProducerConfig
  -- ^ Configuration; start from 'defaultProducerConfig'.
  -> (Producer -> m a)
  -> m a
withProducer brokers cfg = withProducer' brokers cfg closeProducer
{-# INLINEABLE withProducer #-}
{-# SPECIALIZE withProducer :: [Text] -> ProducerConfig -> (Producer -> IO a) -> IO a #-}


{- | Same as 'withProducer' but lets you swap in a custom
shutdown function. Use 'closeProducerWithTimeout' when you
want to bound how long the producer waits for in-flight
records to drain.

@
'withProducer'' brokers cfg (\\p -> 'closeProducerWithTimeout' p 5000) $ \\p ->
  pump p
@
-}
withProducer'
  :: MonadUnliftIO m
  => [Text]
  -> ProducerConfig
  -> (Producer -> IO ())
  {- ^ Shutdown function applied on exit; in 'IO'
  so the typical 'closeProducer' fits without
  ceremony, even when the body is in a custom
  monad stack.
  -}
  -> (Producer -> m a)
  -> m a
withProducer' brokers cfg shutdown body =
  withRunInIO $ \run -> bracket open shutdown (run . body)
  where
    open :: IO Producer
    open = do
      r <- createProducer brokers cfg
      case r of
        Left err ->
          throwIO $
            Errors.connectError
              (T.pack ("wireform-kafka: createProducer failed: " <> err))
        Right p -> pure p
{-# INLINEABLE withProducer' #-}
{-# SPECIALIZE withProducer' :: [Text] -> ProducerConfig -> (Producer -> IO ()) -> (Producer -> IO a) -> IO a #-}


{- | Close the producer and flush pending messages.

Steps:
  1. If a transaction is bound and currently in 'Txn.InTransaction',
     abort it so the broker doesn't keep the txn id locked until
     its @transaction.timeout.ms@ deadline elapses.
  2. Close the batch accumulator (stops accepting new messages).
  3. Wait up to 'producerDeliveryTimeoutMs' for the sender thread
     to drain queued + in-flight batches.
  4. Stop the sender thread.
  5. Close all connections.
-}
closeProducer :: MonadIO m => Producer -> m ()
closeProducer p@Producer {..} = liftIO $ do
  -- If a transaction is bound and currently open, abort it before
  -- shutdown. Mirrors the JVM client's @KafkaProducer.close@
  -- behaviour: an open transaction at close time is rolled back
  -- so the broker doesn't keep the txn id locked until its
  -- @transaction.timeout.ms@ deadline elapses.
  mTxn <- readTVarIO producerTransaction
  case mTxn of
    Nothing -> pure ()
    Just txn -> do
      st <- Txn.getTransactionState txn
      case st of
        Txn.InTransaction -> do
          _ <- Txn.abortTransaction txn
          pure ()
        _ -> pure ()

  -- Close accumulator (marks all batches as ready). After this
  -- returns no new records can be appended; the sender thread
  -- will drain everything that's already been accumulated.
  BA.closeBatchAccumulator producerAccumulator

  -- Wait for the sender thread to actually finish draining,
  -- bounded by the producer's delivery timeout. This replaces a
  -- previous fixed 1-second sleep that didn't actually wait for
  -- the work to complete.
  let deadlineMs = max 1000 (producerDeliveryTimeoutMs producerConfig)
  _ <- waitForDrain p deadlineMs

  -- Stop sender thread (idempotent if already stopped).
  Sender.stopSenderThread producerSenderState producerSender

  -- Close all connections.
  Conn.closeAllConnections producerConnManager


{- | Poll the accumulator until it has no pending batches /and/
the sender thread is not currently mid-iteration on a drained
batch (i.e. 'senderBusy' is False), or the caller-supplied
deadline elapses. Returns @True@ if everything drained cleanly,
@False@ on timeout. The polling interval is 10ms which keeps
the close path responsive without burning CPU.

Checking the busy flag in addition to 'hasReadyBatches' is
required for correctness: 'BA.drainReadyBatches' removes
batches from the accumulator queue /before/ the sender has
received the broker's 'ProduceResponse' for them. A poller
that watched 'hasReadyBatches' alone would race the in-flight
window and a 'closeProducer' that fires immediately after
'flushProducer' returned could lose every record on the wire.
-}
waitForDrain :: Producer -> Int -> IO Bool
waitForDrain Producer {..} deadlineMs = do
  start <- nowMillis
  let loop = do
        pending <- BA.hasReadyBatches producerAccumulator
        busy <- readIORef (Sender.senderBusy producerSenderState)
        inFlight <- Sender.senderTotalInFlight producerSenderState
        now <- nowMillis
        let elapsed = now - start
        if not pending && not busy && inFlight == 0
          then pure True
          else
            if elapsed >= fromIntegral deadlineMs
              then pure False
              else do
                threadDelay (10 * 1000) -- 10ms
                loop
  loop
  where
    nowMillis :: IO Int64
    nowMillis = KafkaTime.currentTimeMillis


{- | Close the producer with a specified timeout.

Attempts to send all pending messages before closing, waiting up to
the specified timeout in milliseconds. If the timeout expires, any
remaining messages may be lost.
-}
closeProducerWithTimeout :: MonadIO m => Producer -> Int -> m ()
closeProducerWithTimeout Producer {..} timeoutMs = liftIO $ do
  -- Close accumulator (marks all batches as ready)
  BA.closeBatchAccumulator producerAccumulator

  -- Wait for sender to drain with timeout
  let timeoutMicros = timeoutMs * 1000
      waitIncrement = 100000 -- 100ms
      maxWaits = max 1 (timeoutMicros `div` waitIncrement)

  -- Poll until sender finishes or timeout
  waitForDrain maxWaits

  -- Stop sender thread
  Sender.stopSenderThread producerSenderState producerSender

  -- Close all connections
  Conn.closeAllConnections producerConnManager
  where
    waitForDrain 0 = return () -- Timeout expired
    waitForDrain n = do
      hasReady <- BA.hasReadyBatches producerAccumulator
      busy <- readIORef (Sender.senderBusy producerSenderState)
      inFlight <- Sender.senderTotalInFlight producerSenderState
      if not hasReady && not busy && inFlight == 0
        then return () -- All batches sent
        else do
          threadDelay 100000 -- 100ms
          waitForDrain (n - 1)


{- | Flush all pending producer records.

Blocks until all buffered records have been sent to the broker and
acknowledged, or until the delivery timeout expires. This is useful
when you need to ensure all records are sent before proceeding.

Returns 'Right ()' on success, or 'Left error' if flush fails.
-}
flushProducer :: MonadIO m => Producer -> m (Either String ())
flushProducer Producer {..} = liftIO $ do
  -- Mark all current batches as ready /without/ closing the
  -- accumulator: this is a drain-checkpoint and the producer
  -- must remain usable for subsequent sends.  (The pre-fix code
  -- called 'closeBatchAccumulator' here, which silently turned
  -- every later send into a "Failed to append record
  -- (accumulator closed or full)" — manifested as missing
  -- records in the head-to-head producer benchmark.)
  BA.flushPendingBatches producerAccumulator

  -- Wait for all batches to be sent
  let deliveryTimeout = producerDeliveryTimeoutMs producerConfig
      timeoutMicros = deliveryTimeout * 1000
      waitIncrement = 100000 -- 100ms
      maxWaits = max 1 (timeoutMicros `div` waitIncrement)

  -- Poll until all batches are sent or timeout. Both
  -- 'hasReadyBatches' (queued in the accumulator) and 'senderBusy'
  -- (drained by the sender but the broker reply hasn't landed)
  -- must be clear before we can claim a record is durable.
  result <- waitForAllBatches maxWaits

  return result
  where
    waitForAllBatches 0 = return $ Left "Flush timeout: not all records sent within delivery timeout"
    waitForAllBatches n = do
      hasReady <- BA.hasReadyBatches producerAccumulator
      busy <- readIORef (Sender.senderBusy producerSenderState)
      inFlight <- Sender.senderTotalInFlight producerSenderState
      if not hasReady && not busy && inFlight == 0
        then return $ Right () -- All batches sent
        else do
          threadDelay 100000 -- 100ms
          waitForAllBatches (n - 1)


{- | Drop all locally queued producer records without closing the
producer.

Mirrors librdkafka's local queue purge operation for records that
have not yet been handed to the sender thread. Every purged
record's callback/future receives a local failure, and the
producer remains usable for later sends. Records already in flight
to a broker are not affected.

Returns the number of locally purged records.
-}
purgeProducer :: MonadIO m => Producer -> m Int
purgeProducer Producer {..} = liftIO $ do
  writeIORef producerLastBatch Nothing
  purged <- BA.purgePendingBatches producerAccumulator producerPurgeError
  atomically $
    HashMap.foldrWithKey
      (\tp n rest -> rewindSequence tp n >> rest)
      (pure ())
      purged
  pure (HashMap.foldl' (+) 0 purged)
  where
    rewindSequence :: BA.TopicPartition -> Int -> STM ()
    rewindSequence tp n = do
      curM <- StmMap.lookup tp producerSequenceNumbers
      case curM of
        Nothing -> pure ()
        Just cur -> do
          let !next = max 0 (cur - fromIntegral n)
          if next == 0
            then StmMap.delete tp producerSequenceNumbers
            else StmMap.insert next tp producerSequenceNumbers


producerPurgeError :: Text
producerPurgeError = "Local: message purged from producer queue"


{- | Bind a 'Txn.Transaction' to this producer.

After this call returns:

  * 'sendMessage' (and the variants below) read the
    transaction's state on every call: a send is rejected with
    a typed error unless the transaction is in
    'Txn.InTransaction';
  * record batches are stamped with the transaction's
    producer-id / epoch (set by 'Txn.initTransactions') and the
    'attrIsTransactional' bit;
  * the first send to a (topic, partition) registers it with
    the transaction coordinator via 'AddPartitionsToTxn'
    (KIP-98).

The producer takes a non-owning reference: closing the producer
aborts the transaction if one is open at that point but does
not close the 'Transaction' value itself. Multiple producers
bound to the same transaction would race on the underlying
coordinator state, so don't do that.

It is safe to call 'bindTransaction' /before/
'Txn.initTransactions': sends will simply be rejected (the
transaction's state is 'Txn.Uninitialized' until init runs).
-}
bindTransaction :: MonadIO m => Producer -> Txn.Transaction -> m ()
bindTransaction Producer {..} txn = liftIO $ do
  -- 'senderTransactionalId' moved to IORef in Tier 3 of the
  -- STM-replacement work; the previous shape did the txn id
  -- mirror, the producerTransaction swap, and the
  -- registered-partitions reset under one STM transaction. Split
  -- into two: the producerTransaction TVar still composes with the
  -- registered-partitions StmMap reset (those remain STM), and
  -- the senderTransactionalId mirror is a separate IORef write.
  -- A producer that races bindTransaction against a sender's
  -- read of the txn id sees one of the two consistent states
  -- (old or new) — same as before.
  let txnIdText = Txn.unTransactionalId (Txn.txnTransactionalId txn)
  writeIORef (Sender.senderTransactionalId producerSenderState) (Just txnIdText)
  atomically $ do
    writeTVar producerTransaction (Just txn)
    -- Reset the per-transaction registered-partition memo. The
    -- 'beginTransaction' lifecycle should also clear this, but we
    -- do it on bind so a freshly bound transaction starts clean.
    resetStmMap producerRegisteredPartitions


{- | A transaction coordinator handle for this producer's
@transactional.id@ ('producerTransactional'), sharing its connections
and TLS / SASL settings, already bound to it ('bindTransaction'). The
caller still runs 'Txn.initTransactions' before the first send.
@bootstrap@ (\"host:port\") is where the coordinator is looked up.
-}
newBoundTransaction :: Producer -> Text -> IO (Either String Txn.Transaction)
newBoundTransaction p bootstrap =
  case producerTransactional (producerConfig p) of
    Nothing -> pure (Left "newBoundTransaction: the producer has no transactional.id")
    Just tid -> case parseBrokerAddress bootstrap of
      Left e -> pure (Left e)
      Right addr -> do
        vc <- AV.createVersionCache
        txn <-
          Txn.createTransaction
            (Txn.TransactionalId tid)
            (producerConnManager p)
            (producerConnConfig p)
            vc
            (producerClientId (producerConfig p))
            addr
            (fromIntegral (producerTransactionTimeoutMs (producerConfig p)))
        bindTransaction p txn
        pure (Right txn)


{- | The currently bound transaction, if any. Mostly useful for
test scaffolding and observability; production code should hold
onto its 'Txn.Transaction' explicitly.
-}
producerBoundTransaction :: MonadIO m => Producer -> m (Maybe Txn.Transaction)
producerBoundTransaction p = liftIO $ readTVarIO (producerTransaction p)


{- | Send a message synchronously (blocks until acknowledged).

Steps:
  1. If a transaction is bound, verify it's in
     'Txn.InTransaction' state; reject otherwise.
  2. Determine target partition (if not specified).
  3. If transactional, register the partition with the
     coordinator on first observation (idempotent within the
     txn).
  4. Allocate a per-(topic, partition) sequence number for
     idempotent / transactional producers.
  5. Append record to the batch accumulator with the
     appropriate stamp and completion callback.
  6. Wait for acknowledgment from broker.
-}
sendMessage
  :: MonadIO m
  => Producer
  -> Text
  -- ^ Topic
  -> Maybe ByteString
  -- ^ Key (optional)
  -> ByteString
  -- ^ Value
  -> m (Either String RecordMetadata)
sendMessage p@Producer {..} topic key value = liftIO $ do
  -- 1. Run the user-supplied interceptor first (KIP-388 / JVM
  --    ProducerInterceptor.onSend). This is allowed to rewrite
  --    the record (e.g. attach trace headers, drop a field, …).
  --    Errors propagate.
  let preInterceptRecord =
        ProducerRecord
          { topic = topic
          , key = key
          , value = Just value
          , headers = []
          , partition = Nothing
          , timestamp = Nothing
          }
  iceptedRecord <- producerInterceptor producerConfig preInterceptRecord
  let icTopic = iceptedRecord.topic
      icKey = iceptedRecord.key
      icValue = iceptedRecord.value
      icHdrs = iceptedRecord.headers
  -- 2. Decide whether the producer is in a transactional /
  --    idempotent mode that requires stamping. We read the
  --    transaction state once up front; if the txn finishes
  --    between this read and the broker round-trip, the broker
  --    fences us — that's the intended JVM-client behaviour.
  preCheck <- producerPreSendCheck p icTopic icKey
  case preCheck of
    Left err -> do
      -- Surface the failure through the ack interceptor /before/
      -- returning to the caller, so observability tools see every
      -- send attempt regardless of where it failed.
      runAckInterceptor producerConfig iceptedRecord (Left err)
      return (Left err)
    Right (partition, stamp) -> do
      let record =
            RB.Record
              { RB.recordTimestampDelta = 0
              , RB.recordOffsetDelta = 0
              , RB.recordKey = icKey
              , RB.recordValue = icValue
              , RB.recordHeaders =
                  map (\(k, v) -> RB.RecordHeader (TE.encodeUtf8 k) (Just v)) icHdrs
              }
          topicPartition = BA.TopicPartition icTopic partition

      resultVar <- newEmptyTMVarIO
      let callback result =
            atomically $ putTMVar resultVar $ case result of
              Left err -> Left (T.unpack err)
              Right ack ->
                Right
                  RecordMetadata
                    { topic = BA.ackTopic ack
                    , partition = BA.ackPartition ack
                    , offset = BA.ackOffset ack
                    , timestamp = BA.ackTimestamp ack
                    }

      success <-
        BA.appendRecordStamped
          producerAccumulator
          topicPartition
          record
          (BA.RecordCallback callback)
          stamp

      if success
        then do
          let timeoutMicros = producerDeliveryTimeoutMs producerConfig * 1000
          result <-
            timeout timeoutMicros $
              atomically (readTMVar resultVar)
          case result of
            Nothing -> do
              let err =
                    "Delivery timeout exceeded ("
                      <> show (producerDeliveryTimeoutMs producerConfig)
                      <> "ms)"
              runAckInterceptor producerConfig iceptedRecord (Left err)
              return $ Left err
            Just r -> do
              runAckInterceptor producerConfig iceptedRecord r
              return r
        else do
          let err = "Failed to append record (accumulator closed or full)"
          runAckInterceptor producerConfig iceptedRecord (Left err)
          return (Left err)


{- | Send a 'ProducerRecord' honouring its @partition@ and
@timestamp@ fields. Equivalent to 'sendMessage' but takes the
full record shape so callers can override:

  * @partition@ — bypasses the partitioner (key-hash / sticky /
    custom) and ships to the supplied partition index.
  * @timestamp@ — sets the per-record wire timestamp exactly.
    The 'Kafka.Client.Internal.BatchAccumulator' computes the
    correct delta against the batch's base timestamp, or, when
    this record opens a fresh batch, uses @timestamp@ as the
    batch base. 'Nothing' falls back to wall-clock at batch
    creation, matching 'sendMessage'.

Use 'sendMessage' when you don't need explicit partition /
timestamp control; that path is fractionally cheaper because
the partition is picked by the producer's configured
partitioner instead of being threaded through.
-}
sendRecord
  :: MonadIO m
  => Producer
  -> ProducerRecord
  -> m (Either String RecordMetadata)
sendRecord p@Producer {..} pr0 = liftIO $ do
  resultVar <- newEmptyTMVarIO
  appended <- sendRecordWith p pr0 (atomically . putTMVar resultVar)
  case appended of
    Left err -> pure (Left err)
    Right () -> do
      let timeoutMicros = producerDeliveryTimeoutMs producerConfig * 1000
      result <- timeout timeoutMicros (atomically (readTMVar resultVar))
      pure $ case result of
        Nothing ->
          Left
            ( "Delivery timeout exceeded ("
                <> show (producerDeliveryTimeoutMs producerConfig)
                <> "ms)"
            )
        Just r -> r


{- | Append a 'ProducerRecord' to the accumulator and return without
waiting for the broker: @onAck@ runs (on the sender thread) once
the record is acknowledged or fails, after the ack interceptor.
'Left' means the record was never appended (pre-send check or a
full / closed accumulator); @onAck@ is then not called. Pair with
'flushProducer' to wait for everything sent so far (the JVM's
@send(record, callback)@ + @flush()@).
-}
sendRecordAsync
  :: MonadIO m
  => Producer
  -> ProducerRecord
  -> (Either String RecordMetadata -> IO ())
  -> m (Either String ())
sendRecordAsync p pr onAck = liftIO (sendRecordWith p pr onAck)


sendRecordWith
  :: Producer
  -> ProducerRecord
  -> (Either String RecordMetadata -> IO ())
  -> IO (Either String ())
sendRecordWith p@Producer {..} pr0 onAck = do
  -- 1. Interceptor — the JVM contract lets it rewrite every
  --    field, including 'partition' and 'timestamp', so we run
  --    it before we look at any of them.
  iceptedRecord <- producerInterceptor producerConfig pr0
  let icTopic = iceptedRecord.topic
      icKey = iceptedRecord.key
      icValue = iceptedRecord.value
      icHdrs = iceptedRecord.headers
      icPart = iceptedRecord.partition
      icTs = iceptedRecord.timestamp

  -- 2. Compute the (partition, stamp) pair. If the caller
  --    supplied a partition we still need the transactional /
  --    idempotent stamp, but not the partitioner round-trip.
  preCheck <- case icPart of
    Nothing -> producerPreSendCheck p icTopic icKey
    Just explicit -> do
      mTxn <- readTVarIO producerTransaction
      case mTxn of
        Nothing -> do
          pid <- readTVarIO producerIdempotentId
          epoch <- readTVarIO producerIdempotentEpoch
          if pid == RB.noProducerId && epoch == RB.noProducerEpoch
            then pure (Right (explicit, BA.noStamp))
            else do
              -- Idempotent path: the sequence number must be
              -- allocated against the explicit partition, not the
              -- one the partitioner would have picked.
              fullPreSendCheck p icTopic icKey (Just explicit) mTxn
        Just _ ->
          -- Transactional path: register + sequence the explicit
          -- partition with the coordinator.
          fullPreSendCheck p icTopic icKey (Just explicit) mTxn

  case preCheck of
    Left err -> do
      runAckInterceptor producerConfig iceptedRecord (Left err)
      pure (Left err)
    Right (partition, stamp) -> do
      let record =
            RB.Record
              { RB.recordTimestampDelta = 0 -- accumulator overwrites
              , RB.recordOffsetDelta = 0
              , RB.recordKey = icKey
              , RB.recordValue = icValue
              , RB.recordHeaders =
                  map (\(k, v) -> RB.RecordHeader (TE.encodeUtf8 k) (Just v)) icHdrs
              }
          topicPartition = BA.TopicPartition icTopic partition

      let callback result = do
            let !r = case result of
                  Left e -> Left (T.unpack e)
                  Right ack ->
                    Right
                      RecordMetadata
                        { topic = BA.ackTopic ack
                        , partition = BA.ackPartition ack
                        , offset = BA.ackOffset ack
                        , timestamp = BA.ackTimestamp ack
                        }
            runAckInterceptor producerConfig iceptedRecord r
            onAck r

      success <-
        BA.appendRecordStampedAt
          producerAccumulator
          topicPartition
          record
          (BA.RecordCallback callback)
          stamp
          icTs

      if success
        then pure (Right ())
        else do
          let err = "Failed to append record (accumulator closed or full)"
          runAckInterceptor producerConfig iceptedRecord (Left err)
          pure (Left err)


{- | Typed send. Encodes the key (when present) and value through
the topic's 'Kafka.Topic.Topic' serdes and forwards to
'sendRecord'. Returns a 'Left' if the serde encoding never
fails (in practice never — serialisers in 'Kafka.Serde' are
total) but the broker rejects the record.

Any headers the key or value serde contributes (via
'Kafka.Serde.serializeHeaders' — e.g. the schema-identity headers
from 'Kafka.Serde.Proto.Buf.bufProtoSerde') are stamped onto the
outgoing record automatically, value headers after key headers.
Plain serdes contribute none, so the common case is unchanged.

@
let events = Kafka.'Kafka.Topic.textTopic' \"events\"
_ <- 'publish' p events (Just \"k1\") \"hello\"
@
-}
publish
  :: MonadIO m
  => Producer
  -> Topic.Topic k v
  -> Maybe k
  -> v
  -> m (Either String RecordMetadata)
publish p t mk v =
  let ks = Topic.topicKeySerde t
      vs = Topic.topicValueSerde t
      keyHdrs = maybe mempty (Serde.serializeHeaders ks) mk
      hdrs = H.toList (keyHdrs <> Serde.serializeHeaders vs v)
  in sendRecord
       p
       ProducerRecord
         { topic = Topic.topicName t
         , key = Serde.serialize ks <$> mk
         , value = Just (Serde.serialize vs v)
         , headers = hdrs
         , partition = Nothing
         , timestamp = Nothing
         }
{-# INLINEABLE publish #-}
{-# SPECIALIZE publish :: Producer -> Topic.Topic k v -> Maybe k -> v -> IO (Either String RecordMetadata) #-}


-- | Discarding variant of 'publish'.
publish_
  :: MonadIO m
  => Producer
  -> Topic.Topic k v
  -> Maybe k
  -> v
  -> m ()
publish_ p t mk v = do
  _ <- publish p t mk v
  pure ()
{-# INLINEABLE publish_ #-}
{-# SPECIALIZE publish_ :: Producer -> Topic.Topic k v -> Maybe k -> v -> IO () #-}


{- | Read the broker-supplied cluster id off this
producer's metadata cache. Returns 'Nothing' until the first
successful metadata refresh; afterwards reflects whatever the
broker set in its @MetadataResponse@.
-}
producerClusterId :: MonadIO m => Producer -> m (Maybe Text)
producerClusterId Producer {..} =
  liftIO $
    atomically (Meta.getClusterId producerMetadata)


{- | Return partition metadata for a topic, refreshing the producer's
metadata cache on a cache miss. Mirrors
@KafkaProducer.partitionsFor(String)@.
-}
partitionsFor :: MonadIO m => Producer -> Text -> m (Either String [PartitionInfo])
partitionsFor p@Producer {..} topicName = liftIO $ do
  cached <- atomically (Meta.getTopicPartitions producerMetadata topicName)
  parts <- case cached of
    Just ps -> pure (Just ps)
    Nothing -> do
      refreshTopicOnDemand p topicName
      atomically (Meta.getTopicPartitions producerMetadata topicName)
  pure $ case parts of
    Nothing -> Left ("partitionsFor: no metadata for topic " <> T.unpack topicName)
    Just ps -> Right (map (partitionInfoFromMetadata topicName) ps)


partitionInfoFromMetadata :: Text -> Meta.PartitionMetadata -> PartitionInfo
partitionInfoFromMetadata topicName pm =
  PartitionInfo
    { partitionInfoTopic = topicName
    , partitionInfoPartition = Meta.partitionMetaId pm
    , partitionInfoLeader =
        let !leader = Meta.partitionMetaLeader pm
        in if leader < 0 then Nothing else Just leader
    , partitionInfoReplicas = Meta.partitionMetaReplicas pm
    , partitionInfoIsrs = Meta.partitionMetaIsrs pm
    }


{- | Cheap health probe: returns 'True' iff the background sender
thread is still running. A 'False' return means the sender
terminated (typically due to an unrecoverable error in a worker
callback) and the producer should be recreated.

Suitable for a Kubernetes @livenessProbe@. Does not contact
the broker — it only checks in-process state — so it is safe
to call at high frequency.
-}
producerHealthy :: MonadIO m => Producer -> m Bool
producerHealthy Producer {..} = liftIO $ do
  status <- Async.poll producerSender
  pure $ case status of
    Nothing -> True
    Just _ -> False


{- | Returns the producer's client-instance id (KIP-714).
Deterministic locally-derived UUID padded from the configured
@client.id@; same encoding the consumer uses. When the
broker-side telemetry pipeline lands, this returns the
broker-assigned id instead; call sites don't need to change.
-}
producerClientInstanceId :: MonadIO m => Producer -> m TopicIdImp.TopicId
producerClientInstanceId Producer {..} =
  liftIO $
    pure
      ( TopicIdImp.TopicId
          (Telemetry.clientInstanceIdFromText (producerClientId producerConfig))
      )


{- | Best-effort dispatch of the ack interceptor. Wraps in 'try' so
a buggy interceptor can't take down the sender thread / caller.
-}
runAckInterceptor
  :: ProducerConfig
  -> ProducerRecord
  -> Either String RecordMetadata
  -> IO ()
runAckInterceptor cfg rec_ outcome = do
  r <-
    try (producerOnAcknowledgement cfg rec_ outcome)
      :: IO (Either SomeException ())
  case r of
    Right () -> pure ()
    Left _ -> pure ()


{- | Combined gate + partitioning + sequence allocation. Run in a
single STM transaction (modulo the IO-scoped partitioner +
'AddPartitionsToTxn' coordinator round-trip) so the
(state-check, partition-pick, sequence-bump) tuple is atomic.
-}
producerPreSendCheck
  :: Producer
  -> Text
  -> Maybe ByteString
  -> IO (Either String (Int32, BA.BatchStamp))
producerPreSendCheck p@Producer {..} topic key = do
  mTxn <- readTVarIO producerTransaction
  -- Fast path for the common-case non-transactional /
  -- non-idempotent producer: skip the entire STM transaction
  -- below for sequence tracking and just call the partitioner +
  -- return the no-stamp sentinel. This is what 'sendMessage_'
  -- does inline and what 'sendMessageAsync' should do too —
  -- pre-fix the per-record STM commit was the dominant
  -- 'sendMessageAsync' cost (~150 ns / record on the bench).
  case mTxn of
    Nothing -> do
      pid <- readTVarIO producerIdempotentId
      epoch <- readTVarIO producerIdempotentEpoch
      if pid == RB.noProducerId && epoch == RB.noProducerEpoch
        then do
          partition <- selectPartition p topic key
          pure (Right (partition, BA.noStamp))
        else fullPath mTxn
    _ -> fullPath mTxn
  where
    fullPath mTxn = fullPreSendCheck p topic key Nothing mTxn


fullPreSendCheck
  :: Producer
  -> Text
  -> Maybe ByteString
  -> Maybe Int32
  -- ^ Explicit partition ('ProducerRecord' @partition@); 'Nothing'
  -- runs the configured partitioner. The txn registration and the
  -- sequence number are both keyed on the partition chosen here, so
  -- it must be the one the record is actually appended to.
  -> Maybe Txn.Transaction
  -> IO (Either String (Int32, BA.BatchStamp))
fullPreSendCheck p@Producer {..} topic key mExplicit mTxn = do
  -- 1. Transactional state guard.
  txnGate <- case mTxn of
    Nothing -> return (Right Nothing)
    Just txn -> do
      st <- Txn.getTransactionState txn
      mPid <- readIORef (Txn.txnProducerId txn)
      mEpoch <- readIORef (Txn.txnProducerEpoch txn)
      case producerTxnGate st mPid mEpoch of
        Left err -> return (Left err)
        Right (pid, epoch) -> return (Right (Just (txn, pid, epoch)))
  case txnGate of
    Left err -> return (Left err)
    Right txnInfo -> do
      realPartition <- case mExplicit of
        Just explicit -> pure explicit
        Nothing -> selectPartition p topic key
      let tp = BA.TopicPartition topic realPartition

      -- 2. For transactional sends, lazily register the partition
      --    with the coordinator. Memoised in
      --    'producerRegisteredPartitions' so subsequent sends are
      --    just an STM lookup.
      regResult <- case txnInfo of
        Nothing -> return (Right ())
        Just (txn, pid, ep) -> do
          alreadyKnown <- atomically $ do
            r <- StmMap.lookup tp producerRegisteredPartitions
            case r of
              Just () -> return True
              Nothing -> do
                StmMap.insert () tp producerRegisteredPartitions
                return False
          if alreadyKnown
            then return (Right ())
            else do
              -- Track in the Transaction's partition set too — that
              -- drives the commit-time AddPartitionsToTxn envelope
              -- and the per-partition sequence bookkeeping.
              _ <- Txn.sendInTransaction txn (KCC.TopicPartition topic realPartition)
              -- Best-effort coordinator round-trip; on failure
              -- back the memo out so a future send retries.
              mCoord <- readIORef (Txn.txnCoordinator txn)
              case mCoord of
                Nothing -> do
                  atomically $ StmMap.delete tp producerRegisteredPartitions
                  return $
                    Left
                      "transactional producer: no transaction \
                      \coordinator (initTransactions never \
                      \completed?)"
                Just coord -> do
                  r <-
                    TC.addPartitionsToTxn
                      (Txn.txnConnectionManager txn)
                      (Txn.txnConnConfig txn)
                      (Txn.txnVersionCache txn)
                      (Txn.txnCorrelationId txn)
                      (Txn.txnClientId txn)
                      coord
                      ( Txn.unTransactionalId
                          (Txn.txnTransactionalId txn)
                      )
                      pid
                      ep
                      [KCC.TopicPartition topic realPartition]
                  case r of
                    Left e -> do
                      atomically $ StmMap.delete tp producerRegisteredPartitions
                      return $
                        Left $
                          "transactional producer: \
                          \AddPartitionsToTxn failed: "
                            <> show e
                    Right () -> return (Right ())
      case regResult of
        Left err -> return (Left err)
        Right () -> do
          -- 3. Allocate sequence + producer-id stamp.
          stamp <- atomically $ do
            curM <- StmMap.lookup tp producerSequenceNumbers
            let !cur = case curM of
                  Just s -> s
                  Nothing -> 0
            StmMap.insert (cur + 1) tp producerSequenceNumbers
            (pid, ep) <- case txnInfo of
              Just (_, pid', ep') -> return (pid', ep')
              Nothing -> do
                pid' <- readTVar producerIdempotentId
                ep' <- readTVar producerIdempotentEpoch
                return (pid', ep')
            return
              BA.BatchStamp
                { BA.stampProducerId = pid
                , BA.stampProducerEpoch = ep
                , BA.stampBaseSequence = cur
                , BA.stampIsTransactional =
                    case txnInfo of
                      Just _ -> True
                      Nothing -> False
                }
          return (Right (realPartition, stamp))


{- | Drain every key from an stm-containers 'StmMap.Map' inside an
STM transaction. There's no built-in primitive for this, hence
the @ListT.toList@ + delete loop.
-}
resetStmMap :: (Eq k, Hashable.Hashable k) => StmMap.Map k v -> STM ()
resetStmMap m = do
  pairs <- ListT.toList (StmMap.listT m)
  mapM_ (\(k, _) -> StmMap.delete k m) pairs


{- | Pure transactional state gate. Given the current
'Txn.TransactionState' plus the (possibly populated) producer-id
/ epoch from the bound 'Txn.Transaction', either:

  * @Right (pid, epoch)@ — the producer is in 'Txn.InTransaction'
    with both values populated, so the caller may proceed and
    stamp the outgoing batch with @(pid, epoch)@; or
  * @Left reason@ — surface a typed-string error matching what
    'sendMessage' reports to the application.

Exposed so unit tests can drive every branch without spinning up
a real 'Producer' or transaction-coordinator round-trip.
-}
producerTxnGate
  :: Txn.TransactionState
  -> Maybe Txn.ProducerId
  -> Maybe Txn.ProducerEpoch
  -> Either String (Int64, Int16)
producerTxnGate st mPid mEpoch = case st of
  Txn.InTransaction -> case (mPid, mEpoch) of
    (Just (Txn.ProducerId pid), Just (Txn.ProducerEpoch ep)) ->
      Right (pid, ep)
    _ ->
      Left
        "transactional producer: missing producer-id/epoch \
        \(call initTransactions before sending)"
  Txn.Uninitialized ->
    Left
      "transactional producer: must call initTransactions before sending"
  Txn.Ready ->
    Left
      "transactional producer: must call beginTransaction before sending"
  Txn.Fenced ->
    Left
      "transactional producer: producer fenced"
  Txn.Error msg ->
    Left $
      "transactional producer in error state: " <> T.unpack msg
  _ ->
    Left $
      "transactional producer: cannot send in state " <> show st


{- | Send a message asynchronously: enqueue into the batch
accumulator and return immediately, /without/ waiting for the
broker ack.  Mirrors librdkafka's @rd_kafka_produce@ +
'Kafka.Producer.produceMessage' from @hw-kafka-client@: the
record sits in the in-memory queue and is flushed by the
background sender thread on the next batch round-trip.

Returns 'Left' only for /pre-enqueue/ failures (transactional
state checks, partition-selection errors, accumulator closed
or full).  Per-record broker errors surface through
'producerOnAcknowledgement', not through this 'IO ()' result.

For at-least-once semantics, pair with 'flushProducer' before
'closeProducer' so the queue drains.
| Non-blocking send. Hands back an 'MVar' that will be filled
with the broker's reply (or the delivery error) when the ack
arrives. Callers wait on the result with 'Control.Concurrent.MVar.takeMVar'
whenever they're ready:

@
handle <- 'sendMessageAsync' p \"events\" Nothing \"hello\"
... do other work ...
md <- 'Control.Concurrent.MVar.takeMVar' handle
@

The 'producerInterceptor' / 'producerOnAcknowledgement' hooks
still fire on the sender thread — they're the right place to
observe every send, regardless of whether the caller ever
reads the 'MVar'. If the caller never reads it, the 'MVar' is
garbage-collected with the rest of the closure.
-}
sendMessageAsync
  :: MonadIO m
  => Producer
  -> Text
  -> Maybe ByteString
  -> ByteString
  -> m (MVar (Either String RecordMetadata))
sendMessageAsync p@Producer {..} topic key value = liftIO $ do
  resultVar <- newEmptyMVar
  let preInterceptRecord =
        ProducerRecord
          { topic = topic
          , key = key
          , value = Just value
          , headers = []
          , partition = Nothing
          , timestamp = Nothing
          }
  iceptedRecord <- producerInterceptor producerConfig preInterceptRecord
  let icTopic = iceptedRecord.topic
      icKey = iceptedRecord.key
      icValue = iceptedRecord.value
      icHdrs = iceptedRecord.headers
  preCheck <- producerPreSendCheck p icTopic icKey
  case preCheck of
    Left err -> do
      runAckInterceptor producerConfig iceptedRecord (Left err)
      putMVar resultVar (Left err)
      pure resultVar
    Right (partition, stamp) -> do
      let record =
            RB.Record
              { RB.recordTimestampDelta = 0
              , RB.recordOffsetDelta = 0
              , RB.recordKey = icKey
              , RB.recordValue = icValue
              , RB.recordHeaders =
                  map (\(k, v) -> RB.RecordHeader (TE.encodeUtf8 k) (Just v)) icHdrs
              }
          topicPartition = BA.TopicPartition icTopic partition
          callback result = do
            let outcome = case result of
                  Left err -> Left (T.unpack err)
                  Right ack ->
                    Right
                      RecordMetadata
                        { topic = BA.ackTopic ack
                        , partition = BA.ackPartition ack
                        , offset = BA.ackOffset ack
                        , timestamp = BA.ackTimestamp ack
                        }
            runAckInterceptor producerConfig iceptedRecord outcome
            putMVar resultVar outcome

      success <-
        BA.appendRecordStamped
          producerAccumulator
          topicPartition
          record
          (BA.RecordCallback callback)
          stamp

      if success
        then pure resultVar
        else do
          let err = "Failed to append record (accumulator closed or full)"
          runAckInterceptor producerConfig iceptedRecord (Left err)
          putMVar resultVar (Left err)
          pure resultVar


{- | Bare-minimum-overhead async send: skips the user-installed
'producerInterceptor' / 'producerOnAcknowledgement' hooks, the
transactional / idempotent stamping path
('producerPreSendCheck'), and the per-record 'ProducerRecord'
struct allocation.

Suitable for high-throughput non-transactional producers that
don't need per-record ack callbacks (logs / telemetry / fire-
and-forget event streams).  Per-record CPU work is roughly:

  * 'selectPartition' (one TVar read for sticky / no work for
    hash partitioner);
  * one 'RB.Record' allocation;
  * one 'BA.appendRecordStamped' call (the STM hot path inside
    'BatchAccumulator', ~250 ns).

Returns 'Left' only when the accumulator rejects the record
(closed / over-capacity); the caller is expected to back off
+ retry.  Per-record broker errors are /silently dropped/ —
the user signed up for fire-and-forget by calling this.

For at-most-one-loss semantics, pair with 'flushProducer'
before 'closeProducer' so the in-memory queue drains.

This is the path the producer benchmark uses for the head-to-
head against librdkafka's @rd_kafka_produce@ + flush combo.
-}
sendMessage_
  :: MonadIO m
  => Producer
  -> Text
  -- ^ Topic
  -> Maybe ByteString
  -- ^ Optional key (used by hash partitioner)
  -> ByteString
  -- ^ Value
  -> m (Either String ())
{-# INLINE sendMessage_ #-}
sendMessage_ p@Producer {..} topic key value = liftIO $ do
  partition <- selectPartition p topic key
  let !record =
        RB.Record
          { RB.recordTimestampDelta = 0
          , RB.recordOffsetDelta = 0
          , RB.recordKey = key
          , RB.recordValue = Just value
          , RB.recordHeaders = []
          }
      !tp = BA.TopicPartition topic partition
  success <-
    BA.appendRecordStamped
      producerAccumulator
      tp
      record
      noOpRecordCallback
      BA.noStamp
  if success
    then return (Right ())
    else return (Left "Failed to append record (accumulator closed or full)")


{- | Top-level shared no-op callback used by 'sendMessage_' /
'sendMessages_'. Hoisting it out of the per-call lambda
avoids one closure allocation per record (the previous shape
allocated a fresh @\\_ -> pure ()@ on every send).
-}
{-# NOINLINE noOpRecordCallback #-}
noOpRecordCallback :: BA.RecordCallback
noOpRecordCallback = BA.NoRecordCallback


{- | Tightest-possible single-writer fast path. Caches the
in-progress @(topic, partition, queue, batch)@ tuple in
producer-local state ('producerLastBatch') so the steady-
state per-record cost is one 'readIORef' + one direct
'appendDirect' on the cached 'BatchAccumulating'.

Cache invalidation: the cache is invalidated when the cached
batch fills + seals, or when the supplied topic differs from
the cached topic; in either case we fall through to
'sendMessageUnsafe_' to refresh.

Same safety contract as 'sendMessageUnsafe_' — caller must
guarantee no concurrent sends on the same (topic, partition).
For multi-partition workloads, use 'sendMessage_' /
'sendMessageUnsafe_' instead.
-}
{-# INLINE sendMessageFastest_ #-}
sendMessageFastest_
  :: MonadIO m
  => Producer
  -> Text
  -- ^ Topic
  -> Maybe ByteString
  -- ^ Optional key
  -> ByteString
  -- ^ Value
  -> m (Either String ())
sendMessageFastest_ p@Producer {..} topic key value = liftIO $ do
  cache <- readIORef producerLastBatch
  case cache of
    Just (cachedTopic, _cachedPart, queue, ba)
      | cachedTopic == topic -> do
          let !record =
                RB.Record
                  { RB.recordTimestampDelta = 0
                  , RB.recordOffsetDelta = 0
                  , RB.recordKey = key
                  , RB.recordValue = Just value
                  , RB.recordHeaders = []
                  }
          stillCurrent <-
            BA.appendDirect
              producerAccumulator
              queue
              ba
              record
              noOpRecordCallback
          when (not stillCurrent) $
            -- Sealed: invalidate the cache so the next call
            -- refreshes via 'sendMessageUnsafe_'.
            writeIORef producerLastBatch Nothing
          pure (Right ())
    _ -> refreshAndSend p topic key value


{- | Cache miss path for 'sendMessageFastest_': call
'sendMessageUnsafe_' (which lazily creates the
'BatchAccumulating' if needed via 'slowAppendIO') and then
repopulate 'producerLastBatch' with the new handle.
-}
refreshAndSend
  :: Producer
  -> Text
  -> Maybe ByteString
  -> ByteString
  -> IO (Either String ())
refreshAndSend p@Producer {..} topic key value = do
  res <- sendMessageUnsafe_ p topic key value
  case res of
    Left e -> pure (Left e)
    Right () -> do
      partition <- selectPartition p topic key
      let !tp = BA.TopicPartition topic partition
      cur <- BA.currentBatchOf producerAccumulator tp
      case cur of
        Just (ba, queue) ->
          writeIORef producerLastBatch (Just (topic, partition, queue, ba))
        Nothing -> writeIORef producerLastBatch Nothing
      pure (Right ())


{- | Single-writer-per-partition variant of 'sendMessage_'
that swaps the per-partition CAS in the accumulator for a
plain read + write. /Caller must guarantee no concurrent
'sendMessage' / 'sendMessageAsync' / 'sendMessage_' calls
target the same (topic, partition)./ The single-producer-
thread workload every librdkafka and JVM @KafkaProducer@
benchmark uses is the canonical safe shape.

Same fire-and-forget semantics as 'sendMessage_'; ~25-35 %
faster on hot 4-core hardware because the producer's main
thread no longer pays the CAS-loop overhead per record.
-}
{-# INLINE sendMessageUnsafe_ #-}
sendMessageUnsafe_
  :: MonadIO m
  => Producer
  -> Text
  -- ^ Topic
  -> Maybe ByteString
  -- ^ Optional key
  -> ByteString
  -- ^ Value
  -> m (Either String ())
sendMessageUnsafe_ p@Producer {..} topic key value = liftIO $ do
  partition <- selectPartition p topic key
  let !record =
        RB.Record
          { RB.recordTimestampDelta = 0
          , RB.recordOffsetDelta = 0
          , RB.recordKey = key
          , RB.recordValue = Just value
          , RB.recordHeaders = []
          }
      !tp = BA.TopicPartition topic partition
  success <-
    BA.appendRecordStampedUnsafe
      producerAccumulator
      tp
      record
      noOpRecordCallback
      BA.noStamp
  if success
    then pure (Right ())
    else pure (Left "Failed to append record (accumulator closed)")


{- | Bulk variant of 'sendMessage_' for high-throughput
producers that already have a list of records to publish.

Amortises the per-record overhead 'sendMessage_' pays on
its hot path (interceptor, partitioner lookup, accumulator
closed check, partition map lookup, queue-current-batch
swap): the partitioner runs once for the whole vector, the
closed check + partition lookup happen once, and the
accumulator's 'appendRecordsStamped' folds every record into
the partition's filling batch in a single
'atomicModifyIORef\\\'' call. If the records overflow the
configured batch size, multiple ready batches are emitted
inside that one modify (no extra hot-path STM commits).

Same fire-and-forget semantics as 'sendMessage_': the
'producerInterceptor' / 'producerOnAcknowledgement' callback
machinery is /not/ run; broker errors are silently dropped.
Pair with 'flushProducer' before 'closeProducer' for
at-most-one-loss durability.
-}
sendMessages_
  :: MonadIO m
  => Producer
  -> Text
  -- ^ Topic
  -> [(Maybe ByteString, ByteString)]
  -- ^ (key, value) pairs in publish order
  -> m (Either String ())
sendMessages_ _ _ [] = pure (Right ())
sendMessages_ p@Producer {..} topic kvs = liftIO $ do
  -- One partitioner call for the whole list. With the default
  -- sticky partitioner this also costs ~10 ns regardless of
  -- list length (cache hit on 'producerStickyPartitions');
  -- with the round-robin partitioner the whole batch lands on
  -- one partition which is what high-throughput callers want
  -- anyway.
  partition <- selectPartition p topic (fst (head kvs))
  let !tp = BA.TopicPartition topic partition
      !records =
        Seq.fromList
          [ RB.Record
              { RB.recordTimestampDelta = 0
              , RB.recordOffsetDelta = 0
              , RB.recordKey = k
              , RB.recordValue = Just v
              , RB.recordHeaders = []
              }
          | (k, v) <- kvs
          ]
      -- Fire-and-forget: one no-op callback shared across all
      -- positions (the accumulator's 'appendRecordsStamped'
      -- still asks for one callback per record so the response
      -- handler can dispatch positionally).
      !cbs = Seq.replicate (length kvs) noOpRecordCallback
  success <-
    BA.appendRecordsStamped
      producerAccumulator
      tp
      records
      cbs
      BA.noStamp
  if success
    then pure (Right ())
    else pure (Left "Failed to append records (accumulator closed)")


-- | Select partition for a message based on configured partitioner (KIP-480).
{-# INLINE selectPartition #-}
selectPartition
  :: Producer
  -> Text
  -- ^ Topic
  -> Maybe ByteString
  -- ^ Key (optional)
  -> IO Int32
selectPartition producer@Producer {..} topic keyM = do
  -- Hottest path: the default sticky partitioner caches its
  -- chosen partition per-topic in 'producerStickyPartitions',
  -- so the steady-state call is one 'readIORef' +
  -- 'HashMap.lookup' and we never need 'partitionCount' at all.
  -- Fall through to the metadata-aware path on cache miss
  -- (first record per topic, or non-sticky partitioner).
  sticky <- readIORef producerStickyPartitions
  case HashMap.lookup topic sticky of
    Just partition -> pure partition
    Nothing -> selectPartitionSlow producer topic keyM


{- | Cold-path partition selection: consult the partition-count
cache (avoids the per-call STM transaction against
'producerMetadata' the previous shape paid) and dispatch on
the configured partitioner.
-}
selectPartitionSlow :: Producer -> Text -> Maybe ByteString -> IO Int32
selectPartitionSlow producer@Producer {..} topic keyM = do
  cache <- readIORef producerPartitionCount
  partCount <- case HashMap.lookup topic cache of
    Just n -> pure (Just n)
    Nothing -> do
      partCountM <- atomically $ Meta.getPartitionCount producerMetadata topic
      mn <- case partCountM of
        Just n -> pure (Just n)
        Nothing -> do
          refreshTopicOnDemand producer topic
          atomically $ Meta.getPartitionCount producerMetadata topic
      case mn of
        Just n -> do
          atomicModifyIORef' producerPartitionCount $ \m ->
            (HashMap.insertWith (\_ old -> old) topic n m, ())
          pure (Just n)
        Nothing -> pure Nothing
  case partCount of
    Nothing -> return 0 -- Refresh didn't help; sender will error.
    Just partitionCount -> do
      let partitioner = producerPartitioner producerConfig
      partitioner producer topic keyM partitionCount


{- | Synchronous metadata refresh for a single topic. Picks any
broker the producer already knows about (from the
bootstrap-time refresh in 'createProducer') and issues a
targeted MetadataRequest. Best-effort: errors are swallowed
because the caller falls back to partition 0 + sender-side
retry on failure.
-}
refreshTopicOnDemand :: Producer -> Text -> IO ()
refreshTopicOnDemand Producer {..} topic = do
  brokersM <- atomically (Meta.getAllBrokers producerMetadata)
  case brokersM of
    Just (b : _) -> do
      let addr = Meta.brokerMetaAddress b
      connRes <-
        Conn.getOrCreateConnection
          producerConnManager
          addr
          producerConnConfig
      case connRes of
        Right conn -> do
          -- Reuse the sender's correlation-id source so we
          -- don't tread on its in-flight numbering.
          cid <- atomicModifyIORef' (Sender.senderCorrelationId producerSenderState) $ \n -> (n + 1, n)
          _ <-
            Meta.refreshTopicMetadata
              conn
              producerMetadata
              (Just [topic])
              cid
          pure ()
        Left _ -> pure ()
    _ -> pure ()


{- | Hash-based partitioning using Kafka's murmur2 variant.

This is the SAME hash + selection rule the JVM client's
@DefaultPartitioner@ uses (via
@org.apache.kafka.common.utils.Utils.murmur2(byte[])@):
@(murmur2(key) & 0x7FFFFFFF) % numPartitions@. Every official
Kafka client (JVM, librdkafka, kafka-go, …) computes the same
result, so a record with @key=\"foo\"@ produced from any
language lands on the same partition.
-}
hashPartition :: ByteString -> Int32 -> Int32
hashPartition = Murmur2.partitionForKey


{- | Sticky partitioning (KIP-480): stick to partition until batch is ready,
then switch to a different partition (improves batching)
-}
getStickyPartition :: Producer -> Text -> Int32 -> IO Int32
getStickyPartition Producer {..} topic partCount = do
  -- Fast path: 'producerStickyPartitions' is consulted on every
  -- send (one-record-per-call interface), so the common case
  -- has to be a single 'readIORef' + 'HashMap.lookup' rather
  -- than an STM transaction.
  m <- readIORef producerStickyPartitions
  case HashMap.lookup topic m of
    Just partition -> pure partition
    Nothing -> do
      -- First record for this topic: allocate a partition by
      -- bumping the round-robin counter, then persist the
      -- sticky pick. Two 'atomicModifyIORef\'' calls — race
      -- against a concurrent first-record-for-this-topic call
      -- is harmless: both see the same partCount, both compute
      -- the same partition, the second 'atomicModifyIORef\''
      -- with 'insertWith (\_ old -> old)' preserves whichever
      -- caller landed first.
      partition <- atomicModifyIORef' producerRoundRobinCounters $ \rrm ->
        let !counter = HashMap.lookupDefault 0 topic rrm
            !p = counter `mod` partCount
        in (HashMap.insert topic (counter + 1) rrm, p)
      atomicModifyIORef' producerStickyPartitions $ \sm ->
        case HashMap.lookup topic sm of
          Just existing -> (sm, existing)
          Nothing -> (HashMap.insert topic partition sm, partition)


{- | Round-robin partitioning: cycle through partitions evenly.
Single 'atomicModifyIORef\'' bumps the per-topic counter and
returns the new partition; matches the pre-IORef STM
semantics.
-}
getRoundRobinPartition :: Producer -> Text -> Int32 -> IO Int32
getRoundRobinPartition Producer {..} topic partCount =
  atomicModifyIORef' producerRoundRobinCounters $ \rrm ->
    let !counter = HashMap.lookupDefault 0 topic rrm
        !partition = counter `mod` partCount
    in (HashMap.insert topic (counter + 1) rrm, partition)


{- | Send a batch of messages.

Sends multiple messages as a batch. More efficient than multiple individual sends.
-}
sendBatch
  :: MonadIO m
  => Producer
  -> [ProducerRecord]
  -> m (Either String [RecordMetadata])
sendBatch producer records = liftIO $ do
  -- Fire each enqueue in its own async so the accumulator can
  -- coalesce records targeting the same (topic, partition) into
  -- the same RecordBatch on the wire. Sequencing them via plain
  -- 'mapM' would block per-record on the broker round-trip and
  -- defeat the batching.
  asyncs <- mapM (Async.async . sendRecordIndividual producer) records
  results <- mapM Async.wait asyncs
  let (errors, successes) = partitionEithers results
  if null errors
    then return $ Right successes
    else
      return $
        Left $
          "Some messages failed: "
            <> show (length errors)
            <> " errors"
  where
    sendRecordIndividual :: Producer -> ProducerRecord -> IO (Either String RecordMetadata)
    sendRecordIndividual = sendRecord

    partitionEithers :: [Either a b] -> ([a], [b])
    partitionEithers = foldr (either left right) ([], [])
      where
        left a (l, r) = (a : l, r)
        right b (l, r) = (l, b : r)


----------------------------------------------------------------------
-- Enhanced ack callbacks
--
-- Previously lived in @Kafka.Client.ProducerExtras@. Folded in here
-- so the producer-side observability hooks are in one place.
----------------------------------------------------------------------

{- | Per-stage producer hooks. Each JVM 3.x producer callback
receives the same @Either ProducerError RecordMetadata@ outcome
at every stage of the send pipeline; this record bundles the
five hook points into a single value.
-}
data EnhancedCallback = EnhancedCallback
  { ecOnEnqueue :: !(ProducerRecord -> IO ())
  , ecOnSend :: !(ProducerRecord -> IO ())
  , ecOnAck
      :: !( ProducerRecord
            -> Either RM.ProducerError RecordMetadata
            -> IO ()
          )
  , ecOnRetry :: !(ProducerRecord -> Int -> IO ())
  , ecOnDelivered :: !(RecordMetadata -> IO ())
  }


{- | A no-op 'EnhancedCallback'. Override individual fields with
record-update syntax for the subset of hooks you actually want.
-}
noopEnhancedCallback :: EnhancedCallback
noopEnhancedCallback =
  EnhancedCallback
    { ecOnEnqueue = \_ -> pure ()
    , ecOnSend = \_ -> pure ()
    , ecOnAck = \_ _ -> pure ()
    , ecOnRetry = \_ _ -> pure ()
    , ecOnDelivered = \_ -> pure ()
    }


{- | Dispatch a single ack outcome through the enhanced callback,
swallowing any exception so a buggy hook can't tear down the
sender thread.
-}
dispatchEnhanced
  :: EnhancedCallback
  -> ProducerRecord
  -> Either RM.ProducerError RecordMetadata
  -> IO ()
dispatchEnhanced ec rec_ outcome = do
  r <- try (ecOnAck ec rec_ outcome) :: IO (Either SomeException ())
  case r of
    Right () -> pure ()
    Left _ -> pure ()


----------------------------------------------------------------------
-- SPECIALIZE pragmas for the IO hot path
--
-- The public functions are polymorphic over 'MonadIO' /
-- 'MonadUnliftIO' so callers in an application monad stack
-- (ReaderT IO, RIO, etc.) don't have to sprinkle 'liftIO'.
-- These pragmas materialise the monomorphic IO entry-points so
-- the dictionary-passing overhead is zero in the IO case.
----------------------------------------------------------------------

{-# INLINEABLE createProducer #-}
{-# SPECIALIZE createProducer :: [Text] -> ProducerConfig -> IO (Either String Producer) #-}


{-# INLINEABLE closeProducer #-}
{-# SPECIALIZE closeProducer :: Producer -> IO () #-}


{-# INLINEABLE closeProducerWithTimeout #-}
{-# SPECIALIZE closeProducerWithTimeout :: Producer -> Int -> IO () #-}


{-# INLINEABLE flushProducer #-}
{-# SPECIALIZE flushProducer :: Producer -> IO (Either String ()) #-}


{-# INLINEABLE sendMessage #-}
{-# SPECIALIZE sendMessage :: Producer -> Text -> Maybe ByteString -> ByteString -> IO (Either String RecordMetadata) #-}


{-# INLINEABLE sendMessageAsync #-}
{-# SPECIALIZE sendMessageAsync :: Producer -> Text -> Maybe ByteString -> ByteString -> IO (MVar (Either String RecordMetadata)) #-}


{-# SPECIALIZE sendMessage_ :: Producer -> Text -> Maybe ByteString -> ByteString -> IO (Either String ()) #-}


{-# SPECIALIZE sendMessageUnsafe_ :: Producer -> Text -> Maybe ByteString -> ByteString -> IO (Either String ()) #-}


{-# SPECIALIZE sendMessageFastest_ :: Producer -> Text -> Maybe ByteString -> ByteString -> IO (Either String ()) #-}


{-# INLINEABLE sendMessages_ #-}
{-# SPECIALIZE sendMessages_ :: Producer -> Text -> [(Maybe ByteString, ByteString)] -> IO (Either String ()) #-}


{-# INLINEABLE sendRecord #-}
{-# SPECIALIZE sendRecord :: Producer -> ProducerRecord -> IO (Either String RecordMetadata) #-}


{-# INLINEABLE sendBatch #-}
{-# SPECIALIZE sendBatch :: Producer -> [ProducerRecord] -> IO (Either String [RecordMetadata]) #-}


{-# INLINEABLE bindTransaction #-}
{-# SPECIALIZE bindTransaction :: Producer -> Txn.Transaction -> IO () #-}


{-# INLINEABLE producerBoundTransaction #-}
{-# SPECIALIZE producerBoundTransaction :: Producer -> IO (Maybe Txn.Transaction) #-}


{-# INLINEABLE producerClusterId #-}
{-# SPECIALIZE producerClusterId :: Producer -> IO (Maybe Text) #-}


{-# INLINEABLE producerHealthy #-}
{-# SPECIALIZE producerHealthy :: Producer -> IO Bool #-}


{-# INLINEABLE producerConfigFromEnv #-}
{-# SPECIALIZE producerConfigFromEnv :: ProducerConfig -> IO (Either [ConfigError] ProducerConfig) #-}
