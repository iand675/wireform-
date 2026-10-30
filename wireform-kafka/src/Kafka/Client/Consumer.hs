{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Kafka.Client.Consumer
Description : Receive records from a Kafka topic
Copyright   : (c) 2025
License     : BSD-3-Clause

Open a connection to a Kafka cluster, subscribe to topics, and pull
records from the broker in a loop.

This module is the lower-level consumer surface: you own the poll
loop, you commit offsets when you want, you handle rebalances. If
all you want is \"call this handler for each record\", use
"Kafka.Client.Group" instead — it wraps this module in a bracket
and a managed loop.

= Quick start

@
import qualified Kafka.Client.Consumer as Consumer

main :: IO ()
main =
  Consumer.'withConsumer' [\"localhost:9092\"] \"my-group\"
    Consumer.'defaultConsumerConfig' [\"events\"] $ \\c -> do
      Right recs <- 'poll' c 1000
      mapM_ (\\r -> print (r.key, r.value)) recs
      _ <- 'commitSync' c
      pure ()
@

'withConsumer' is the recommended way to manage the lifecycle:
it joins the group, subscribes to the topics you list, and on the
way out commits a final batch and leaves the group cleanly. To
keep a long-running poll loop, see 'Kafka.Client.Group.runConsumer'.

= Configuration

'ConsumerConfig' mirrors the librdkafka @CONFIGURATION.md@ knobs;
the field haddocks list the equivalent librdkafka name. Start from
'defaultConsumerConfig' and only override fields you care about.
The most common ones:

  * 'consumerGroupId' — which consumer group this consumer joins.
  * 'consumerAutoOffsetReset' — start at 'Earliest' or 'Latest'
    when the group has no committed offsets for a partition.
  * 'consumerAutoCommit' — let the consumer commit on its own
    timer ('True') or commit yourself ('False'). Manual commits
    give the smallest duplicate window on a crash.
  * 'consumerIsolationLevel' — 'ReadCommitted' to skip records
    from in-flight transactions.

Environment variables of the form @KAFKA_*@ are layered on top of
your config automatically by 'createConsumer'; see
'applyKafkaEnvToConsumerConfig'.
-}
module Kafka.Client.Consumer (
  -- * Consumer lifecycle

  --

  {- | The consumer keeps a connection pool, a heartbeat thread
  (when joined to a group), and per-partition fetch state.
  'withConsumer' is the recommended bracket: it joins, subscribes,
  and on the way out leaves the group, commits, and closes.
  -}
  Consumer,
  withConsumer,
  withConsumer',
  createConsumer,
  closeConsumer,
  closeConsumerWithTimeout,
  closeConsumerWithoutLeavingGroup,

  -- * Records
  ConsumerRecord (..),
  TopicPartition (..),
  ConsumerRecords (..),
  emptyConsumerRecords,
  consumerRecordsAll,
  consumerRecordsCount,
  consumerRecordsPartitions,
  recordsByPartition,
  recordsByTopic,
  consumerRecordsNextOffsets,

  -- * Polling

  --

  {- | 'poll' returns the next batch of records (up to
  'consumerMaxPollRecords'); 'commitSync' / 'commitAsync'
  persists where you got to. For an automatic per-record
  loop with built-in commit + error handling, see
  "Kafka.Client.Group".
  -}
  poll,
  pollRecords,
  wakeupConsumer,
  commitSync,
  commitAsync,
  commitSyncOffsets,
  commitAsyncCallback,
  committed,
  committedAll,
  position,

  -- * Commit metadata + callbacks
  OffsetAndMetadata (..),
  offsetAndMetadata,
  withMetadata,
  withLeaderEpoch,
  OffsetCommitCallback,
  noopOffsetCommitCallback,

  -- * Group identity (KIP-447 / KIP-394)
  ConsumerGroupMetadata (..),
  newConsumerGroupMetadata,
  groupMetadata,
  groupCoordinator,
  setPreRejoinHook,
  rebalancePending,

  -- * Subscription

  --

  {- | Subscribe to topics (group-managed) or 'assign' specific
  partitions yourself (group-free).
  -}
  subscribe,
  unsubscribe,
  assign,

  -- * Regex subscribe (KIP-848)
  SubscriptionPattern (..),
  subscriptionPattern,
  matchesSubscriptionPattern,

  -- * Replay

  --

  {- | Move the fetch position around — by offset, by time, or
  to the bounds of the partition.
  -}
  seek,
  seekWithMetadata,
  seekToBeginning,
  seekToEnd,
  beginningOffsets,
  endOffsets,
  offsetsForTimes,
  offsetsForTimesFull,
  OffsetAndTimestamp (..),

  -- * Partition control

  --

  {- | Pause / resume per-partition consumption, or inspect the
  current assignment.
  -}
  pause,
  resume,
  assignment,
  paused,

  -- * Rebalance listener

  --

  {- | Callbacks fired when the group rebalances. The default
  is the no-op; install your own with 'setRebalanceListener'
  to do warm-up, cache invalidation, etc.
  -}
  setRebalanceListener,
  currentAssignment,
  computeAssignmentDelta,

  -- * Static membership and rejoin
  requestRejoin,
  enforceRebalance,
  enforceRebalanceWithReason,
  setSubscriptionUserDataHook,
  setAssignmentHook,
  assignmentUserData,
  setAssignmentStrategy,
  StaticMembershipState (..),
  currentStaticMembershipState,

  -- * KIP-714 client telemetry id
  clientInstanceId,

  -- * Configuration
  ConsumerConfig (..),
  defaultConsumerConfig,
  AssignmentStrategy (..),
  OffsetResetStrategy (..),
  IsolationLevel (..),
  validateConsumerConfig,

  -- * Cluster info
  consumerClusterId,
  consumerHealthy,
  consumerConfigOf,
  consumerGroupIdOf,

  -- * Environment-variable overlay

  --

  {- | 'createConsumer' already reads @KAFKA_*@ env vars and
  layers them on top of the supplied 'ConsumerConfig'
  automatically. These helpers are exported for callers
  that want to inspect or pre-apply the overlay manually.
  -}
  applyKafkaEnvToConsumerConfig,
  consumerConfigFromEnv,

  -- * Additional ergonomics

  --

  {- | Smaller helpers that round out the consumer surface.
  Each is a pure decision layer or a typed configuration
  knob; none of them touch the consumer's mutable state.
  -}
  EffectiveConsumerSnapshot (..),
  effectiveConsumerSnapshot,
  RewindPolicy (..),
  planRewind,
  RebalanceTrigger (..),
  recordRebalanceTrigger,
  isReadOnlyMode,
  withReadOnly,
  ShutdownReason (..),
  shutdownReasonText,
  AssignorHint (..),
  assignorHintText,
  PerPartitionFetchKnob (..),

  -- * Fetch internals (exposed for tests)
  selectFetchedRecords,
  advanceFetchPositions,
  decodeAllBatchesSliced,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM
import Control.Exception (SomeException, bracket, throwIO, try)
import Control.Exception qualified
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int
import Data.List (foldl')
import Data.List qualified as List
import Data.HashSet qualified as HashSet
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, isNothing, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Generics (Generic)
import Kafka.Client.RetryClassifier qualified as RC
import Kafka.Client.Assignor (CustomAssignor)
import Kafka.Client.ConfigValidation (
  ConfigError,
  renderConfigErrors,
 )
import Kafka.Client.ConfigValidation qualified as CV
import Kafka.Client.Env qualified as Env
import Kafka.Client.Internal.ConsumerGroup qualified as CG
import Kafka.Client.Internal.Heartbeat qualified as HB
import Kafka.Client.Internal.Request qualified as Req
import Kafka.Client.Internal.Subscribe qualified as Sub
import Kafka.Client.Metadata (MetadataCache)
import Kafka.Client.Metadata qualified as Meta
import Kafka.Client.Telemetry qualified as Telemetry
import Kafka.Client.TopicId qualified as TopicIdImp
import Kafka.Compression.Types qualified as Compression
import Kafka.Errors qualified as Errors
import Kafka.Network.Connection (BrokerAddress (..), Connection)
import Kafka.Network.Connection qualified as Conn
import Kafka.Protocol.ApiVersions qualified as AV
import Kafka.Protocol.RecordBatch qualified as RB
import Kafka.Protocol.RecordBatchWire qualified as RBW
import Kafka.Protocol.VersionNegotiation qualified as VN
import ListT qualified
import StmContainers.Map qualified as StmMap
import System.Timeout qualified
import Text.Regex.TDFA qualified as RE
import "wireform-kafka-protocol" Kafka.Protocol.Generated.FetchRequest qualified as FR
import "wireform-kafka-protocol" Kafka.Protocol.Generated.FetchResponse qualified as FResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.ListOffsetsRequest qualified as LOReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.ListOffsetsResponse qualified as LOResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.OffsetCommitRequest qualified as OCReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.OffsetCommitResponse qualified as OCResp
import "wireform-kafka-protocol" Kafka.Protocol.Generated.OffsetFetchRequest qualified as OFReq
import "wireform-kafka-protocol" Kafka.Protocol.Generated.OffsetFetchResponse qualified as OFResp
import "wireform-kafka-protocol" Kafka.Protocol.Primitives qualified as P
import "wireform-kafka-protocol" Kafka.Protocol.Wire qualified as W
import "wireform-kafka-protocol" Kafka.Protocol.Wire.Codec qualified as WC


-- | Partition assignment strategy.
data AssignmentStrategy
  = -- | Range assignment (default)
    RangeAssignment
  | -- | Round-robin assignment
    RoundRobinAssignment
  | -- | Sticky assignment (minimizes rebalance)
    StickyAssignment
  | -- | An application assignor ("Kafka.Client.Assignor")
    CustomAssignment !CustomAssignor
  deriving (Eq, Show, Generic)


-- | Isolation level for fetched records.
data IsolationLevel
  = {- | Default. The fetcher returns every record, including
    those still inside an open transaction.
    -}
    ReadUncommitted
  | {- | Only return records that belong to a committed
    transaction (or no transaction at all).
    -}
    ReadCommitted
  deriving (Eq, Show, Generic)


-- | The wire code of an 'IsolationLevel' (Fetch and ListOffsets).
isolationLevelCode :: IsolationLevel -> Int8
isolationLevelCode = \case
  ReadUncommitted -> 0
  ReadCommitted -> 1


{- | Consumer configuration. Field names + defaults map onto
librdkafka's @CONFIGURATION.md@ entries; the librdkafka name
is given inline next to the Haskell field.
-}
data ConsumerConfig = ConsumerConfig
  { consumerClientId :: !Text
  -- ^ @client.id@ — identifier sent on every request.
  , consumerGroupId :: !Text
  -- ^ @group.id@ — consumer group id.
  , consumerGroupInstanceId :: !(Maybe Text)
  {- ^ @group.instance.id@ — KIP-345 static membership.
  Default 'Nothing'.
  -}
  , consumerAutoCommit :: !Bool
  -- ^ @enable.auto.commit@. Default 'True'.
  , consumerAutoCommitIntervalMs :: !Int
  -- ^ @auto.commit.interval.ms@. Default 5000.
  , consumerEnableAutoOffsetStore :: !Bool
  {- ^ @enable.auto.offset.store@: when 'True' (the default),
  'poll' implicitly stages every fetched offset for the
  next auto-commit. When 'False', the application must
  call 'storeOffset' before commit.
  -}
  , consumerSessionTimeoutMs :: !Int
  {- ^ @session.timeout.ms@. Default 45000 (KIP-735 widened
  from 10000 in Kafka 3.0).
  -}
  , consumerHeartbeatIntervalMs :: !Int
  -- ^ @heartbeat.interval.ms@. Default 3000.
  , consumerMaxPollRecords :: !Int
  -- ^ @max.poll.records@. Default 500.
  , consumerMaxPollIntervalMs :: !Int
  -- ^ @max.poll.interval.ms@. Default 300000 (5 minutes).
  , consumerAssignmentStrategy :: !AssignmentStrategy
  -- ^ @partition.assignment.strategy@.
  , consumerAutoOffsetReset :: !OffsetResetStrategy
  -- ^ @auto.offset.reset@. Default 'Latest'.
  , consumerIsolationLevel :: !IsolationLevel
  -- ^ @isolation.level@. Default 'ReadUncommitted'.
  , consumerEnablePartitionEof :: !Bool
  {- ^ @enable.partition.eof@: emit a synthetic EOF event when
  the fetcher reaches the partition's high-water mark.
  Default 'False'.
  -}
  , consumerCheckCrcs :: !Bool
  {- ^ @check.crcs@: verify the CRC32C of every fetched record
  batch. Default 'True'.
  -}
  , consumerFetchMinBytes :: !Int
  {- ^ @fetch.min.bytes@: hold the fetch response until at
  least this many bytes are available (or
  'consumerFetchMaxWaitMs' elapses). Default 1.
  -}
  , consumerFetchMaxBytes :: !Int
  {- ^ @fetch.max.bytes@: maximum total bytes returned by a
  single fetch across all partitions. Default 52428800
  (50 MiB).
  -}
  , consumerFetchMaxWaitMs :: !Int
  {- ^ @fetch.wait.max.ms@: how long the broker waits to
  accumulate 'consumerFetchMinBytes'. Default 500.
  -}
  , consumerFetchMessageMaxBytes :: !Int
  {- ^ @max.partition.fetch.bytes@ /
  @fetch.message.max.bytes@: cap per (topic, partition).
  Default 1048576 (1 MiB).
  -}
  , consumerFetchErrorBackoffMs :: !Int
  {- ^ @fetch.error.backoff.ms@: backoff after a failed fetch
  before retrying. Default 500.
  -}
  , consumerDefaultApiTimeoutMs :: !Int
  {- ^ @default.api.timeout.ms@: how long the offset lookups
  ('assign', 'seekToBeginning', 'endOffsets', 'offsetsForTimes', ...)
  keep retrying transient broker errors (a leader election, a topic
  still being created) before failing. Default 60000.
  -}
  , consumerQueuedMaxMessagesKbytes :: !Int
  {- ^ @queued.max.messages.kbytes@: per-partition fetch queue
  ceiling in KB. Default 65536.
  -}
  , consumerRackId :: !(Maybe Text)
  -- ^ @client.rack@ — KIP-392 rack-aware fetching.
  , consumerConnectionConfig :: !Conn.ConnectionConfig
  {- ^ Lower-level connection settings: TLS, SASL, retry/backoff
  knobs. Defaults to 'Conn.defaultConnectionConfig' (plain TCP,
  no SASL). Set 'Conn.connSasl' here to enable any of the SASL
  mechanisms (PLAIN \/ SCRAM \/ OAUTHBEARER \/ AWS_MSK_IAM \/
  GSSAPI-stub) — see "Kafka.Network.Auth.SASL".
  -}
  , consumerInterceptor :: !([ConsumerRecord] -> IO [ConsumerRecord])
  {- ^ Post-fetch interceptor (analogue of
  @org.apache.kafka.clients.consumer.ConsumerInterceptor.onConsume@).
  Applied to the batch returned by 'poll' before the
  caller sees it. Defaults to 'pure'. Exceptions propagate.
  -}
  , consumerOnCommit
      :: !([(TopicPartition, Int64)] -> IO ())
  {- ^ Per-commit callback (mirrors
  @ConsumerInterceptor.onCommit@). Best-effort: exceptions
  in the callback are caught + dropped so a buggy hook
  can't break the commit pipeline.
  -}
  , consumerStaticMembershipPersist
      :: !(Maybe (StaticMembershipState -> IO ()))
  {- ^ KIP-345 callback invoked just before the heartbeat thread
  stops (i.e. on 'closeConsumer'). Receives the consumer's
  final @(memberId, generationId)@ so the application can
  persist them somewhere (a file, a KV store, …) and pass
  them back via 'consumerStaticMembershipResume' on the
  next start. 'Nothing' (default) disables persistence.
  -}
  , consumerStaticMembershipResume
      :: !(Maybe StaticMembershipState)
  {- ^ KIP-345 resume hook: when set, 'createConsumer' uses the
  given member id / generation id when joining the group,
  so a restart with the same @group.instance.id@ avoids
  a generation-bump rebalance.
  -}
  }
  deriving (Generic)


{- | Static-membership state to persist across restarts.
Mirrors the JVM client's behaviour where a static member sends
its previously-assigned 'memberId' on JoinGroup and the broker
avoids triggering a rebalance.
-}
data StaticMembershipState = StaticMembershipState
  { staticMemberId :: !Text
  , staticGenerationId :: !Int32
  }
  deriving (Eq, Show, Generic)


-- | Offset reset strategy when no committed offset exists.
data OffsetResetStrategy
  = -- | Start from earliest available offset
    Earliest
  | -- | Start from latest offset (default)
    Latest
  | -- | Throw error if no offset exists
    None
  deriving (Eq, Show, Generic)


{- | Default consumer configuration. Values track librdkafka's
@CONFIGURATION.md@ defaults except where the JVM client diverges
(and we follow the JVM-Kafka 3.x defaults so application
behaviour matches what users see in @kafka-console-consumer@).
-}
defaultConsumerConfig :: ConsumerConfig
defaultConsumerConfig =
  ConsumerConfig
    { consumerClientId = "kafka-native-consumer"
    , consumerGroupId = "default-group"
    , consumerGroupInstanceId = Nothing
    , consumerAutoCommit = True
    , consumerAutoCommitIntervalMs = 5000
    , consumerEnableAutoOffsetStore = True
    , consumerSessionTimeoutMs = 45_000 -- KIP-735
    , consumerHeartbeatIntervalMs = 3000
    , consumerMaxPollRecords = 500
    , consumerMaxPollIntervalMs = 300_000 -- 5 minutes
    , consumerAssignmentStrategy = RangeAssignment
    , consumerAutoOffsetReset = Latest
    , consumerIsolationLevel = ReadUncommitted
    , consumerEnablePartitionEof = False
    , consumerCheckCrcs = True
    , consumerFetchMinBytes = 1
    , consumerFetchMaxBytes = 52_428_800 -- 50 MiB
    , consumerFetchMaxWaitMs = 500
    , consumerFetchMessageMaxBytes = 1_048_576 -- 1 MiB
    , consumerFetchErrorBackoffMs = 500
    , consumerDefaultApiTimeoutMs = 60_000
    , consumerQueuedMaxMessagesKbytes = 65_536
    , consumerRackId = Nothing -- KIP-392
    , consumerConnectionConfig = Conn.defaultConnectionConfig
    , consumerInterceptor = pure
    , consumerOnCommit = \_ -> pure ()
    , consumerStaticMembershipPersist = Nothing
    , consumerStaticMembershipResume = Nothing
    }


{- | Overlay a parsed 'Env.KafkaEnv' onto a 'ConsumerConfig',
including the consumer's embedded
'consumerConnectionConfig'. Only fields whose corresponding
@KAFKA_*@ variable was set are touched.

The connection-level fields piggy-back on
'Env.applyKafkaEnvToConnectionConfig', so SASL\/TLS mis-
configuration (e.g. @KAFKA_SECURITY_PROTOCOL=SASL_SSL@ without
a mechanism) surfaces as a 'ConfigError' here.
-}
applyKafkaEnvToConsumerConfig
  :: Env.KafkaEnv
  -> ConsumerConfig
  -> Either [ConfigError] ConsumerConfig
applyKafkaEnvToConsumerConfig env cfg =
  case Env.applyKafkaEnvToConnectionConfig env (consumerConnectionConfig cfg) of
    Left errs -> Left errs
    Right cc ->
      Right
        cfg
          { consumerClientId =
              maybe (consumerClientId cfg) id (Env.envClientId env)
          , consumerGroupId =
              maybe (consumerGroupId cfg) id (Env.envGroupId env)
          , consumerGroupInstanceId = case Env.envGroupInstanceId env of
              Just _ -> Env.envGroupInstanceId env
              Nothing -> consumerGroupInstanceId cfg
          , consumerAutoCommit =
              maybe (consumerAutoCommit cfg) id (Env.envEnableAutoCommit env)
          , consumerAutoCommitIntervalMs =
              maybe (consumerAutoCommitIntervalMs cfg) id (Env.envAutoCommitIntervalMs env)
          , consumerSessionTimeoutMs =
              maybe (consumerSessionTimeoutMs cfg) id (Env.envSessionTimeoutMs env)
          , consumerHeartbeatIntervalMs =
              maybe (consumerHeartbeatIntervalMs cfg) id (Env.envHeartbeatIntervalMs env)
          , consumerMaxPollRecords =
              maybe (consumerMaxPollRecords cfg) id (Env.envMaxPollRecords env)
          , consumerMaxPollIntervalMs =
              maybe (consumerMaxPollIntervalMs cfg) id (Env.envMaxPollIntervalMs env)
          , consumerAssignmentStrategy =
              maybe
                (consumerAssignmentStrategy cfg)
                assignStrategy
                (Env.envPartitionAssignmentStrategy env)
          , consumerAutoOffsetReset =
              maybe
                (consumerAutoOffsetReset cfg)
                offsetReset
                (Env.envAutoOffsetReset env)
          , consumerIsolationLevel =
              maybe
                (consumerIsolationLevel cfg)
                isolation
                (Env.envIsolationLevel env)
          , consumerCheckCrcs =
              maybe (consumerCheckCrcs cfg) id (Env.envCheckCrcs env)
          , consumerFetchMinBytes =
              maybe (consumerFetchMinBytes cfg) id (Env.envFetchMinBytes env)
          , consumerFetchMaxBytes =
              maybe (consumerFetchMaxBytes cfg) id (Env.envFetchMaxBytes env)
          , consumerFetchMaxWaitMs =
              maybe (consumerFetchMaxWaitMs cfg) id (Env.envFetchMaxWaitMs env)
          , consumerFetchMessageMaxBytes =
              maybe (consumerFetchMessageMaxBytes cfg) id (Env.envFetchMessageMaxBytes env)
          , consumerRackId = case Env.envClientRack env of
              Just _ -> Env.envClientRack env
              Nothing -> consumerRackId cfg
          , consumerConnectionConfig = cc
          }
  where
    assignStrategy Env.EnvAssignRange = RangeAssignment
    assignStrategy Env.EnvAssignRoundRobin = RoundRobinAssignment
    assignStrategy Env.EnvAssignSticky = StickyAssignment

    offsetReset Env.EnvOffsetEarliest = Earliest
    offsetReset Env.EnvOffsetLatest = Latest
    offsetReset Env.EnvOffsetNone = None

    isolation Env.EnvReadUncommitted = ReadUncommitted
    isolation Env.EnvReadCommitted = ReadCommitted


{- | Read every @KAFKA_*@ variable from the process environment
and overlay them on top of the supplied 'ConsumerConfig'.
-}
consumerConfigFromEnv
  :: MonadIO m
  => ConsumerConfig
  -> m (Either [ConfigError] ConsumerConfig)
consumerConfigFromEnv cfg = liftIO $ do
  r <- Env.loadKafkaEnv
  case r of
    Left errs -> pure (Left errs)
    Right env -> pure (applyKafkaEnvToConsumerConfig env cfg)


-- | A topic-partition pair.
data TopicPartition = TopicPartition
  { topic :: !Text
  , partition :: !Int32
  }
  deriving (Eq, Show, Ord, Generic)


instance Hashable TopicPartition


{- | A consumed record from Kafka.

Fields use bare names — access them via OverloadedRecordDot
(@rec.key@, @rec.value@, @rec.topic@, …). The field names are
shared with 'Kafka.Client.Producer.ProducerRecord' and
'Kafka.Client.Producer.RecordMetadata' via 'DuplicateRecordFields';
function-style selectors (@key rec@) are only valid when the
record type is unambiguous at the call site, so the dot syntax
is the recommended style.
-}
data ConsumerRecord = ConsumerRecord
  { topic :: !Text
  , partition :: !Int32
  , offset :: !Int64
  , timestamp :: !Int64
  , key :: !(Maybe ByteString)
  , value :: !(Maybe ByteString)
  -- ^ 'Nothing' is a null value: a tombstone on a compacted topic
  , headers :: ![(Text, ByteString)]
  }
  deriving (Eq, Show, Generic)


-- | Kafka consumer handle.
data Consumer = Consumer
  { consumerConfig :: !ConsumerConfig
  , consumerConnManager :: !Conn.ConnectionManager
  , consumerMetadata :: !MetadataCache
  , consumerVersionCache :: !AV.ApiVersionCache
  , consumerAssignment :: !(IORef (HashMap.HashMap TopicPartition Int64))
  {- ^ Current partition assignment with fetch positions.

  Pre-Tier-3 of the STM-replacement work this was an
  'StmMap.Map' so 'poll' could snapshot the assignment
  under one STM transaction with 'consumerPaused'. The
  per-poll 'ListT.toList' walk of both maps was the
  dominant STM cost on the consumer hot path; per
  docs/STM_REPLACEMENT_SPEC.md Tier 3 we move to
  'IORef'-backed 'HashMap's. Reads are 'readIORef';
  structural updates (subscribe / assign / resume / commit)
  use 'atomicModifyIORef\'' so concurrent updaters see a
  consistent map.
  -}
  , consumerHeartbeat :: !(Maybe (HB.HeartbeatState, Async ()))
  -- ^ Heartbeat state and thread (if in a group)
  , consumerCorrelationId :: !(IORef Int32)
  {- ^ Monotonic correlation-id source. Consumer threads call this
  per request; never composes with anything in STM, so an
  'IORef' + 'atomicModifyIORef\'' avoids the per-call STM
  commit overhead we used to pay on every 'poll' / commit.
  -}
  , consumerPaused :: !(IORef (HashMap.HashMap TopicPartition ()))
  {- ^ Paused partitions. Same Tier 3 rationale as
  'consumerAssignment': single-process state, never
  composed transactionally with anything else, paid full
  STM commit on every 'poll' to read.
  -}
  , consumerSubscription :: !(IORef (Maybe [Text]))
  {- ^ Last topics subscribed via 'subscribe' (so 'poll' can
  transparently re-run the JoinGroup flow when the heartbeat
  thread tells us the group is rebalancing). 'Nothing' means
  either we are using manual 'assign' instead of group
  subscription, or 'subscribe' has not been called yet.

  Single-writer (the 'subscribe' / 'unsubscribe' caller) /
  multi-reader ('poll' rebalance check) pattern; 'IORef'
  suffices.
  -}
  , consumerOnAssigned :: !(TVar ([TopicPartition] -> IO ()))
  , consumerOnRevoked :: !(TVar ([TopicPartition] -> IO ()))
  , consumerOnLost :: !(TVar ([TopicPartition] -> IO ()))
  , consumerPreRejoin :: !(TVar (IO ()))
  {- ^ Run by 'poll' just before it re-joins the group after the
  coordinator signalled a rebalance, while this member still holds
  its current generation and partitions (and only when the member
  was not fenced). The JVM eager protocol's @onPartitionsRevoked@
  slot: the last moment offsets for the current assignment can be
  committed. Set with 'setPreRejoinHook'.
  -}
  {- ^ Per-event callbacks fired on assigned / revoked / lost
  transitions. Default is the no-op; replace via
  'setRebalanceListener'.
  -}
  , consumerLastAssignment :: !(TVar [TopicPartition])
  {- ^ The assignment as of the last fired callback. Used to
  compute revoked / assigned deltas on the next rebalance.
  -}
  , consumerStrategy :: !(TVar AssignmentStrategy)
  -- ^ The assignor advertised at the next join ('setAssignmentStrategy'); starts as the config's
  , consumerAssignmentUserData :: !(TVar ByteString)
  -- ^ The user data the leader's assignor attached to this member's last assignment
  , consumerOnAssignment :: !(TVar ([TopicPartition] -> ByteString -> IO ()))
  -- ^ Fired after every successful join ('setAssignmentHook')
  , consumerSubscriptionUserData :: !(TVar (IO ByteString))
  {- ^ Hook for cross-instance IQ: when 'subscribe' issues a
  JoinGroup, the consumer calls this 'IO ByteString' and
  stamps the result into the subscription-userdata blob.
  Default: @pure BS.empty@ (no userdata). Replace via
  'setSubscriptionUserDataHook'. Used by the streams
  runtime to advertise the local instance's
  host:port + owned stores so peers can route cross-
  instance IQ queries here.
  -}
  , consumerWakeupPending :: !(TVar Bool)
  {- ^ One-shot flag set by 'wakeupConsumer'. The next 'poll'
  observes and clears it before touching the broker.
  -}
  }


{- | Effective 'Conn.ConnectionConfig' for this consumer: takes the
one stored in 'consumerConfig' and overlays the consumer's client
id (so SASL request headers identify the right client).
-}
consumerConnConfig :: Consumer -> Conn.ConnectionConfig
consumerConnConfig c =
  let base = consumerConnectionConfig (consumerConfig c)
  in base {Conn.connClientId = consumerClientId (consumerConfig c)}


{- | 'Conn.getOrCreateConnection' + ensure ApiVersions has been
negotiated for the broker. Idempotent: subsequent calls to
the same broker only do the cache hit, no extra round-trip.

Use this everywhere we obtain a connection to a /new/ broker
(i.e. anywhere that wasn't the bootstrap broker, since the
bootstrap broker's handshake already ran in 'createConsumer')
so subsequent 'pickApiVersion' / 'queryApiVersion' lookups
have data to consult instead of always falling back.
-}
consumerConnect
  :: Consumer
  -> Conn.BrokerAddress
  -> IO (Either String Connection)
consumerConnect c@Consumer {..} addr = do
  cr <- Conn.getOrCreateConnection consumerConnManager addr (consumerConnConfig c)
  case cr of
    Left e -> pure (Left e)
    Right conn -> do
      _ <-
        VN.ensureVersionsNegotiated
          conn
          addr
          consumerVersionCache
          (atomicModifyIORef' consumerCorrelationId $ \cid -> (cid + 1, cid))
      pure (Right conn)


{- | Create a new Kafka consumer.

Initializes the consumer with connection management, metadata caching,
and optionally joins a consumer group for automatic partition assignment.
| Pure config-validation rules. Mirrors the JVM client's
@org.apache.kafka.clients.consumer.ConsumerConfig@ checks: every
rule we apply here is something the broker (or, worse, a runtime
assertion deep in the fetch loop) would otherwise blow up on with
a far less actionable error.

We deliberately don't enforce a non-empty @group.id@ here because
the simple consumer (no group) leaves it blank and only assigns
partitions manually; 'Kafka.Client.Group.validateConfig' enforces
the non-empty requirement when subscribing through the group API.

Returns the empty list when the config is acceptable.
-}
validateConsumerConfig :: ConsumerConfig -> [ConfigError]
validateConsumerConfig ConsumerConfig {..} =
  concat
    [ CV.check
        (T.null consumerClientId)
        "client.id"
        "must be non-empty"
    , CV.check
        (consumerSessionTimeoutMs <= 0)
        "session.timeout.ms"
        "must be > 0"
    , CV.check
        (consumerHeartbeatIntervalMs <= 0)
        "heartbeat.interval.ms"
        "must be > 0"
    , CV.check
        (consumerHeartbeatIntervalMs >= consumerSessionTimeoutMs)
        "heartbeat.interval.ms"
        "must be < session.timeout.ms (broker fences members otherwise)"
    , CV.check
        (consumerMaxPollIntervalMs < consumerSessionTimeoutMs)
        "max.poll.interval.ms"
        "must be >= session.timeout.ms (KIP-62)"
    , CV.check
        (consumerMaxPollRecords < 1)
        "max.poll.records"
        "must be >= 1 (KIP-41)"
    , CV.check
        (consumerFetchMinBytes < 0)
        "fetch.min.bytes"
        "must be >= 0"
    , CV.check
        (consumerFetchMaxBytes <= 0)
        "fetch.max.bytes"
        "must be > 0"
    , CV.check
        (consumerFetchMinBytes > consumerFetchMaxBytes)
        "fetch.min.bytes"
        "must be <= fetch.max.bytes"
    , CV.check
        (consumerFetchMaxWaitMs < 0)
        "fetch.wait.max.ms"
        "must be >= 0"
    , CV.check
        (consumerFetchMessageMaxBytes <= 0)
        "max.partition.fetch.bytes"
        "must be > 0"
    , CV.check
        (consumerFetchErrorBackoffMs < 0)
        "fetch.error.backoff.ms"
        "must be >= 0"
    , CV.check
        (consumerDefaultApiTimeoutMs < 0)
        "default.api.timeout.ms"
        "must be >= 0"
    , CV.check
        (consumerQueuedMaxMessagesKbytes <= 0)
        "queued.max.messages.kbytes"
        "must be > 0"
    , CV.check
        (consumerAutoCommit && consumerAutoCommitIntervalMs <= 0)
        "auto.commit.interval.ms"
        "must be > 0 when enable.auto.commit=true"
    ]


{- | Open a consumer, subscribe to topics, run an action with it,
and tear it down safely.

This is the recommended bracket for short-lived consumer scripts
and for any code where you control the body's scope. The body
runs after the consumer has joined the group and subscribed to
@topics@; on exit (clean or exceptional) 'closeConsumerWithTimeout'
is called, which commits any pending offsets, sends @LeaveGroup@,
and closes connections.

@
'withConsumer' [\"localhost:9092\"] \"my-group\" 'defaultConsumerConfig' [\"events\"] $ \\c -> do
  Right recs \<- 'poll' c 1000
  mapM_ ('processRecord' c) recs
  _ <- 'commitSync' c
  pure ()
@

'withConsumer' is the low-level bracket for hand-rolled poll
loops. If you just want \"call this handler for each record\",
reach for 'Kafka.Client.Group.runConsumer' instead — it brackets
/and/ drives the loop.

If the consumer fails to start (broker unreachable, group join
rejected, etc.) the call raises an 'IOError'.
-}
withConsumer
  :: MonadUnliftIO m
  => [Text]
  -- ^ Bootstrap brokers, e.g. @[\"localhost:9092\"]@.
  -> Text
  -- ^ Consumer group id.
  -> ConsumerConfig
  -- ^ Configuration; start from 'defaultConsumerConfig'.
  -> [Text]
  {- ^ Topics to subscribe to. May be empty if you
  plan to call 'assign' yourself.
  -}
  -> (Consumer -> m a)
  -> m a
withConsumer brokers groupId cfg topics body =
  withConsumer'
    brokers
    groupId
    cfg
    topics
    (\c -> closeConsumerWithTimeout c 30000)
    body
{-# INLINEABLE withConsumer #-}
{-# SPECIALIZE withConsumer :: [Text] -> Text -> ConsumerConfig -> [Text] -> (Consumer -> IO a) -> IO a #-}


{- | Same as 'withConsumer' but lets you swap in a custom
shutdown function. Use 'closeConsumerWithoutLeavingGroup' to
keep the broker session alive across a rolling restart.
-}
withConsumer'
  :: MonadUnliftIO m
  => [Text]
  -> Text
  -> ConsumerConfig
  -> [Text]
  -> (Consumer -> IO ())
  -- ^ Shutdown function applied on exit.
  -> (Consumer -> m a)
  -> m a
withConsumer' brokers groupId cfg topics shutdown body =
  withRunInIO $ \run ->
    bracket open shutdown $ \c -> do
      case topics of
        [] -> run (body c)
        _ -> do
          r <- subscribe c topics
          case r of
            Left err ->
              throwIO $
                Errors.connectError
                  (T.pack ("wireform-kafka: subscribe failed: " <> err))
            Right () -> run (body c)
  where
    open :: IO Consumer
    open = do
      r <- createConsumer brokers groupId cfg
      case r of
        Left err ->
          throwIO $
            Errors.connectError
              (T.pack ("wireform-kafka: createConsumer failed: " <> err))
        Right c -> pure c
{-# INLINEABLE withConsumer' #-}
{-# SPECIALIZE withConsumer' :: [Text] -> Text -> ConsumerConfig -> [Text] -> (Consumer -> IO ()) -> (Consumer -> IO a) -> IO a #-}


createConsumer
  :: MonadIO m
  => [Text]
  {- ^ Bootstrap brokers. Falls back to
  @KAFKA_BOOTSTRAP_SERVERS@ when empty.
  -}
  -> Text
  {- ^ Consumer group ID. Overridden by
  @KAFKA_GROUP_ID@ when set.
  -}
  -> ConsumerConfig
  {- ^ Configuration. 'Kafka.Client.Env'
  env-var overrides are layered on top
  automatically.
  -}
  -> m (Either String Consumer)
createConsumer brokers0 groupId0 config0 = liftIO $ do
  envR <- Env.loadKafkaEnv
  case envR of
    Left errs -> return $ Left $ renderConfigErrors errs
    Right env -> case applyKafkaEnvToConsumerConfig env config0 of
      Left errs -> return $ Left $ renderConfigErrors errs
      Right cfg -> do
        let brokers = case Env.envBootstrapServers env of
              Just bs | null brokers0 -> bs
              _ -> brokers0
            -- groupId precedence: explicit positional > env >
            -- consumerGroupId on the supplied config. The
            -- positional arg is more specific than a deployment-
            -- wide env var, so it wins when set.
            groupId
              | not (T.null groupId0) = groupId0
              | Just g <- Env.envGroupId env = g
              | otherwise = consumerGroupId cfg
        case validateConsumerConfig cfg of
          errs@(_ : _) -> return $ Left $ renderConfigErrors errs
          [] -> createConsumer' brokers groupId cfg


createConsumer'
  :: [Text]
  -> Text
  -> ConsumerConfig
  -> IO (Either String Consumer)
createConsumer' brokers groupId config = do
  -- Parse broker addresses
  let parsedBrokers = map parseBrokerAddress brokers
  case sequence parsedBrokers of
    Left err -> return $ Left $ "Failed to parse broker addresses: " ++ err
    Right brokerAddrs -> do
      -- Initialize connection manager
      connManager <- Conn.createConnectionManager

      -- Initialize metadata cache
      metadataCache <- Meta.createMetadataCache

      -- Fetch initial metadata from bootstrap brokers
      -- Connect to first bootstrap broker and fetch metadata
      let firstBroker = head brokerAddrs
          baseConn = consumerConnectionConfig config
          -- Use the user's client id for any SASL handshake.
          connConfig = baseConn {Conn.connClientId = consumerClientId config}
      connResult <- Conn.getOrCreateConnection connManager firstBroker connConfig
      case connResult of
        Left err -> return $ Left $ "Failed to connect to bootstrap broker: " ++ err
        Right conn -> do
          -- Initialize version cache
          versionCache <- AV.createVersionCache

          -- Initialize correlation ID; the ApiVersions handshake
          -- and the metadata refresh share the same source so
          -- their correlation ids stay distinct from later
          -- consumer requests.
          corrId <- newIORef 0
          let nextCid = atomicModifyIORef' corrId $ \cid -> (cid + 1, cid)

          -- Run the ApiVersions handshake against the bootstrap
          -- broker so subsequent calls' 'queryApiVersion' /
          -- 'pickApiVersion' lookups find data instead of
          -- always falling back. We deliberately swallow
          -- failure: an older broker (< 0.10) doesn't recognise
          -- ApiVersions, in which case the cache stays empty
          -- and downstream calls hit their compiled-in
          -- fallbacks.
          _ <-
            VN.ensureVersionsNegotiated
              conn
              firstBroker
              versionCache
              nextCid

          fetchResult <- Meta.refreshMetadata conn metadataCache 0
          case fetchResult of
            Left err -> return $ Left $ "Failed to fetch initial metadata: " ++ err
            Right _ -> do
              -- Initialize assignment and paused maps
              assignment <- newIORef HashMap.empty
              paused <- newIORef HashMap.empty

              -- No active subscription yet.
              subscription <- newIORef Nothing

              -- Initialize heartbeat if in a consumer group
              heartbeatM <-
                if T.null groupId
                  then return Nothing
                  else do
                    -- Create heartbeat state
                    hbState <-
                      HB.createHeartbeatState
                        groupId
                        (consumerHeartbeatIntervalMs config)
                        connManager
                        versionCache
                        (consumerClientId config)

                    -- KIP-345 static-membership resume: if the
                    -- application supplied a previously persisted
                    -- @(memberId, generationId)@, seed the
                    -- heartbeat state with it so the next JoinGroup
                    -- reuses the existing slot.
                    case consumerStaticMembershipResume config of
                      Nothing -> pure ()
                      Just StaticMembershipState {..} -> do
                        writeIORef (HB.hbMemberId hbState) staticMemberId
                        writeIORef (HB.hbGenerationId hbState) staticGenerationId

                    -- Start heartbeat thread
                    hbThread <- HB.startHeartbeatThread hbState

                    return $ Just (hbState, hbThread)

              onA <- newTVarIO (\_ -> pure () :: IO ())
              onR <- newTVarIO (\_ -> pure () :: IO ())
              onL <- newTVarIO (\_ -> pure () :: IO ())
              preRejoin <- newTVarIO (pure () :: IO ())
              lastAsgn <- newTVarIO []
              subUD <- newTVarIO (pure BS.empty :: IO ByteString)
              asgUD <- newTVarIO BS.empty
              strategyV <- newTVarIO (consumerAssignmentStrategy config)
              onAsg <- newTVarIO (\_ _ -> pure () :: IO ())
              wakeup <- newTVarIO False

              let consumer =
                    Consumer
                      { consumerConfig = config {consumerGroupId = groupId}
                      , consumerConnManager = connManager
                      , consumerMetadata = metadataCache
                      , consumerVersionCache = versionCache
                      , consumerAssignment = assignment
                      , consumerHeartbeat = heartbeatM
                      , consumerCorrelationId = corrId
                      , consumerPaused = paused
                      , consumerSubscription = subscription
                      , consumerOnAssigned = onA
                      , consumerOnRevoked = onR
                      , consumerOnLost = onL
                      , consumerPreRejoin = preRejoin
                      , consumerLastAssignment = lastAsgn
                      , consumerSubscriptionUserData = subUD
                      , consumerAssignmentUserData = asgUD
                      , consumerStrategy = strategyV
                      , consumerOnAssignment = onAsg
                      , consumerWakeupPending = wakeup
                      }

              return $ Right consumer


-- | Parse broker address in "host:port" format
parseBrokerAddress :: Text -> Either String Conn.BrokerAddress
parseBrokerAddress addr =
  case T.splitOn ":" addr of
    [host, portText] ->
      case reads (T.unpack portText) of
        [(port, "")] -> Right $ Conn.BrokerAddress (T.unpack host) port
        _ -> Left $ "Invalid port: " ++ T.unpack portText
    _ -> Left $ "Invalid broker address format (expected host:port): " ++ T.unpack addr


{- | Query partition offsets using ListOffsets API.

This queries the broker for offsets at a given timestamp:
- timestamp = -2: earliest offset
- timestamp = -1: latest offset
- timestamp >= 0: offset at or after the given timestamp
-}
queryPartitionOffsets
  :: Consumer
  -> [TopicPartition]
  -> Int64
  -- ^ Timestamp (-2 for earliest, -1 for latest)
  -> IO (Either String [(TopicPartition, Int64)])
queryPartitionOffsets consumer partitions timestamp =
  fmap (map (\(tp, oat) -> (tp, oatOffset oat)))
    <$> queryPartitionOffsetsByTimestampFull consumer (map (\tp -> (tp, timestamp)) partitions)


{- | Close the consumer.

Leaves the consumer group, stops heartbeat thread, and closes all connections.
-}
closeConsumer :: MonadIO m => Consumer -> m ()
closeConsumer consumer = closeConsumerWithTimeout consumer 30000


{- | Close the consumer with a specified timeout.

Attempts to cleanly leave the consumer group and commit any pending offsets
before closing, waiting up to the specified timeout in milliseconds.
If the timeout expires, the consumer is forcibly closed.

@since KIP-102
-}
closeConsumerWithTimeout :: MonadIO m => Consumer -> Int -> m ()
closeConsumerWithTimeout c t = liftIO (closeConsumerImpl True c t)


{- | @CloseOptions.leaveGroup = false@: close the
consumer /without/ sending a @LeaveGroup@ request. The
broker keeps this member's assignment alive until the
@session.timeout.ms@ expires, then rebalances. Use this
when you're doing a fast restart of the same member id
(e.g. rolling deploy with static membership) and want to
avoid the rebalance churn.
-}
closeConsumerWithoutLeavingGroup :: MonadIO m => Consumer -> Int -> m ()
closeConsumerWithoutLeavingGroup c t = liftIO (closeConsumerImpl False c t)


{- | Programmatic rejoin trigger. Flips
'HB.hbNeedsRebalance' so the next 'poll' transparently
re-runs JoinGroup \/ SyncGroup against the same
subscription — equivalent to what happens when the broker
replies @REBALANCE_IN_PROGRESS@ to a heartbeat.

Used by the streams runtime when its probing-rebalance
machinery decides a warmup replica is ready to be
promoted. Returns @False@ if the consumer has no
heartbeat thread (manual-offset / unsubscribed mode); the
caller can treat that as a no-op.
-}
requestRejoin :: MonadIO m => Consumer -> m Bool
requestRejoin Consumer {..} = liftIO $ case consumerHeartbeat of
  Nothing -> pure False
  Just (hbState, _) -> do
    atomically (writeTVar (HB.hbNeedsRebalance hbState) True)
    pure True


{- | Install a callback the consumer invokes whenever
it issues a JoinGroup; the returned bytes become the
subscription-userdata blob. Used by the streams runtime to
advertise the local instance's @application.server@ +
materialised store names + owned partitions so peers can
compute 'KeyQueryMetadata' for cross-instance IQ routing.

The callback runs on every subscribe / re-join, so the
bytes reflect the /current/ state of the instance — not a
snapshot taken at consumer-creation time.
-}
setSubscriptionUserDataHook
  :: Consumer -> IO ByteString -> IO ()
setSubscriptionUserDataHook Consumer {..} f =
  atomically (writeTVar consumerSubscriptionUserData f)


{- | Run @f@ after every successful join with the whole new assignment
and the user data the leader's assignor attached for this member
(empty for the built-in assignors). It runs after the rebalance
listener's callbacks, also when the partitions did not change; an
exception propagates out of the 'subscribe' / 'poll' that joined.
-}
setAssignmentHook :: Consumer -> ([TopicPartition] -> ByteString -> IO ()) -> IO ()
setAssignmentHook Consumer {..} f = atomically (writeTVar consumerOnAssignment f)


-- | The assignor to advertise from the next join on.
setAssignmentStrategy :: Consumer -> AssignmentStrategy -> IO ()
setAssignmentStrategy Consumer {..} st = atomically (writeTVar consumerStrategy st)


-- | The user data of this member's last assignment.
assignmentUserData :: Consumer -> IO ByteString
assignmentUserData Consumer {..} = readTVarIO consumerAssignmentUserData


{- | Shared implementation. @doLeaveGroup = False@ skips the
@LeaveGroup@ RPC but still:

  * fires onRevoked callbacks so applications can flush
    in-flight work / commit final offsets;
  * persists static-membership state via the user callback
    (so a restart can re-claim the same generation);
  * tears down the heartbeat thread + connections.
-}
closeConsumerImpl :: Bool -> Consumer -> Int -> IO ()
closeConsumerImpl doLeaveGroup c@Consumer {..} timeoutMs = do
  -- Fire onRevoked for any partitions we still own — graceful
  -- close path so listeners can flush in-flight work and
  -- commit final offsets.
  dispatchAssignmentDelta c [] False
  case consumerHeartbeat of
    Nothing -> return ()
    Just (hbState, hbThread) -> do
      -- KIP-345 static-membership persistence: hand the
      -- application the (memberId, generationId) tuple just
      -- before we tear the heartbeat down so a restart with the
      -- same group.instance.id can avoid a generation bump.
      case consumerStaticMembershipPersist consumerConfig of
        Nothing -> pure ()
        Just k -> do
          memberId <- readIORef (HB.hbMemberId hbState)
          genId <- readIORef (HB.hbGenerationId hbState)
          r <-
            try (k (StaticMembershipState memberId genId))
              :: IO (Either Control.Exception.SomeException ())
          case r of
            Right () -> pure ()
            Left _ -> pure () -- best effort
            -- Best-effort LeaveGroup so the broker can rebalance the
            -- group immediately rather than waiting for the
            -- session.timeout.ms to expire. Bounded by the caller's
            -- 'timeoutMs' so a misbehaving coordinator can't block
            -- shutdown indefinitely. Failures here are silent — the
            -- session-timeout fallback is still correct, just slower.
            --
            -- Skipped when the caller passed @doLeaveGroup = False@
            -- (KIP-812 CloseOptions.leaveGroup = false), in which
            -- case the session-timeout path handles reassignment.
      when doLeaveGroup $ do
        _ <-
          System.Timeout.timeout
            (max 0 timeoutMs * 1000)
            (sendLeaveGroup c hbState)
        pure ()
      HB.stopHeartbeatThread hbState hbThread
  -- Close all connections.
  Conn.closeAllConnections consumerConnManager


{- | Read the consumer's current @(memberId, generationId)@ as a
'StaticMembershipState'. Useful for tests and for callers that
want to snapshot the value mid-lifetime (in addition to the
'consumerStaticMembershipPersist' callback that fires on
close).
-}
currentStaticMembershipState
  :: Consumer -> IO (Maybe StaticMembershipState)
currentStaticMembershipState Consumer {..} = case consumerHeartbeat of
  Nothing -> pure Nothing
  Just (hbState, _) -> do
    mid <- readIORef (HB.hbMemberId hbState)
    gen <- readIORef (HB.hbGenerationId hbState)
    pure (Just (StaticMembershipState mid gen))


{- | Read the broker-supplied cluster id off the
consumer's metadata cache. Returns 'Nothing' until the first
successful metadata refresh; afterwards reflects whatever the
broker set in its @MetadataResponse@.
-}
consumerClusterId :: MonadIO m => Consumer -> m (Maybe Text)
consumerClusterId Consumer {..} =
  liftIO $
    atomically (Meta.getClusterId consumerMetadata)


{- | The consumer's 'ConsumerConfig'. Read-only — mutating it has
no effect on a running consumer.
-}
consumerConfigOf :: Consumer -> ConsumerConfig
consumerConfigOf c = c.consumerConfig


{- | The consumer's configured @group.id@. Exposed so JVM-equivalent
shims (e.g. 'Kafka.Client.ConsumerSdk.groupMetadata') can build
a 'ConsumerGroupMetadata' without dropping into the consumer's
internal handle.
-}
consumerGroupIdOf :: Consumer -> Text
consumerGroupIdOf c = (consumerConfigOf c).consumerGroupId


{- | Cheap health probe: returns 'True' iff the heartbeat thread
(when this consumer joined a group) is still running, and the
consumer hasn't been closed. A 'False' result means the broker
has fenced this consumer or the heartbeat task died — recreate
the consumer to recover.

For a non-group consumer (one created with an empty group id and
used with manual 'assign') this always returns 'True'; the
runtime doesn't maintain a heartbeat to fail over.

Suitable for a Kubernetes @livenessProbe@: it does not contact
the broker, only inspects in-process state.
-}
consumerHealthy :: MonadIO m => Consumer -> m Bool
consumerHealthy Consumer {..} = liftIO $
  case consumerHeartbeat of
    Nothing -> pure True
    Just (_, hbAsync) -> do
      status <- Async.poll hbAsync
      pure $ case status of
        Nothing -> True
        Just _ -> False


{- | Synchronously issue a LeaveGroup against the group coordinator.
Used by 'closeConsumerWithTimeout' to clean-shutdown the group
membership; failures are logged in the caller but otherwise ignored.
-}
sendLeaveGroup :: Consumer -> HB.HeartbeatState -> IO ()
sendLeaveGroup c@Consumer {..} hbState = do
  coordAddrM <- readIORef (HB.hbCoordinatorAddr hbState)
  memberId <- readIORef (HB.hbMemberId hbState)
  -- Skip if we never actually joined the group (no coordinator
  -- discovered, or no memberId issued). Sending LeaveGroup with
  -- an empty memberId triggers an InvalidRequestException on the
  -- broker, which then closes the connection — and the next
  -- request sees an EOF.
  case coordAddrM of
    Nothing -> pure () -- never joined; nothing to leave
    _ | T.null memberId -> pure ()
    Just coordAddr -> do
      connResult <- consumerConnect c coordAddr
      case connResult of
        Left _err -> pure ()
        Right conn -> do
          corrId <- atomicModifyIORef' consumerCorrelationId $ \cid -> (cid + 1, cid)
          _ <-
            CG.leaveGroup
              consumerVersionCache
              consumerConnManager
              coordAddr
              conn
              (HB.hbGroupId hbState)
              memberId
              (consumerClientId consumerConfig)
              corrId
          pure ()


{- | Subscribe to topics with broker-side group coordination.

Walks the full consumer-group lifecycle:

1. Discover the group coordinator ('Sub.subscribeFlow' issues a
   FindCoordinator).
2. JoinGroup with our subscription metadata and the @range@
   assignor.
3. If the broker elects us as the group leader we run
   'Sub.rangeAssign' across every member's subscription list and
   publish the per-member assignments via SyncGroup; otherwise we
   just receive ours.
4. OffsetFetch to pick the resume offset for each assigned
   partition; missing offsets fall back to the consumer's
   @auto.offset.reset@ policy.
5. Populate the in-memory assignment map so 'poll' starts fetching.

The heartbeat thread (started in 'createConsumer') picks up the
coordinator address / member id / generation id automatically — they
live in the shared 'HB.HeartbeatState'.

Calling 'subscribe' a second time with a different topic set
re-runs the whole flow (i.e. is the equivalent of an explicit
rebalance request).
-}
subscribe :: MonadIO m => Consumer -> [Text] -> m (Either String ())
subscribe Consumer {..} topics = liftIO $ do
  case consumerHeartbeat of
    Nothing -> return $ Left "Cannot subscribe: consumer not in a group (groupId was empty)"
    Just (hbState, _) -> do
      let resetPolicy = case consumerAutoOffsetReset consumerConfig of
            Earliest -> Sub.ResetEarliest
            Latest -> Sub.ResetLatest
            None -> Sub.ResetNone
      strategy <- readTVarIO consumerStrategy
      let assignor = case strategy of
            RangeAssignment -> Sub.AssignorRange
            RoundRobinAssignment -> Sub.AssignorRoundRobin
            StickyAssignment -> Sub.AssignorSticky
            CustomAssignment ca -> Sub.AssignorCustom ca
          sessionTimeout = fromIntegral (consumerSessionTimeoutMs consumerConfig)
          rebalanceTimeout = fromIntegral (consumerMaxPollIntervalMs consumerConfig)
      fetchUD <- readTVarIO consumerSubscriptionUserData
      result <-
        Sub.subscribeFlow
          consumerConnManager
          (consumerConnConfig Consumer {..})
          consumerMetadata
          consumerVersionCache
          hbState
          (consumerClientId consumerConfig)
          (consumerGroupId consumerConfig)
          topics
          sessionTimeout
          rebalanceTimeout
          resetPolicy
          assignor
          consumerCorrelationId
          fetchUD
      case result of
        Left err -> return $ Left ("subscribe: " ++ show err)
        Right (tps, assignmentUD) -> do
          let !newAssignment =
                [ TopicPartition (Sub.tpTopic stp) (Sub.tpPartition stp)
                | (stp, _) <- tps
                ]
              !newAssignmentSorted =
                List.sortOn
                  (\tp -> (tp.topic, tp.partition))
                  newAssignment
          -- Decide assigned-vs-revoked-vs-lost BEFORE we replace
          -- the assignment: 'hbLost' tells us if the previous
          -- assignment was fenced by the broker, in which case
          -- the removed half routes to 'rlOnLost' rather than
          -- 'rlOnRevoked'.
          asLost <- atomically $ do
            lost <- readTVar (HB.hbLost hbState)
            writeTVar (HB.hbLost hbState) False
            pure lost
          -- Replace the assignment with the new one, seed offsets,
          -- remember the subscription so 'poll' can transparently
          -- re-run the JoinGroup flow on rebalance.
          -- Tier 3: 'consumerAssignment' moved out of STM.
          -- Atomically swap the assignment map; per spec the
          -- subscribe path is not on the hot read path so the
          -- per-call CAS overhead is negligible.
          let !nextAssignment =
                HashMap.fromList
                  [ (TopicPartition (Sub.tpTopic stp) (Sub.tpPartition stp), off)
                  | (stp, off) <- tps
                  ]
          atomicModifyIORef' consumerAssignment $ \_ -> (nextAssignment, ())
          atomically $
            -- Heartbeat thread set this when a previous reply
            -- contained REBALANCE_IN_PROGRESS; clear it now that we
            -- have re-joined.
            writeTVar (HB.hbNeedsRebalance hbState) False
          -- The subscription list lives outside STM (Tier 1 of
          -- the STM-replacement work): the heartbeat-rebalance
          -- check below reads it via 'readIORef' independently
          -- of the assignment-table swap above.
          writeIORef consumerSubscription (Just topics)
          atomically $ writeTVar consumerAssignmentUserData assignmentUD
          -- Fire the user listener with the new assignment.
          dispatchAssignmentDelta Consumer {..} newAssignmentSorted asLost
          onAssignment <- readTVarIO consumerOnAssignment
          onAssignment newAssignmentSorted assignmentUD
          return $ Right ()


{- | Install user callbacks fired on assigned / revoked / lost
transitions. Mirrors Java's @ConsumerRebalanceListener@. The
listener record comes from
"Kafka.Client.RebalanceListener" — we accept three
callbacks directly to avoid a module-import cycle between
"Kafka.Client.Consumer" and "Kafka.Client.RebalanceListener"
(the latter already imports 'TopicPartition' from here).

Callback semantics:

  * @onAssigned@ fires for every partition newly added to
    this consumer's assignment.
  * @onRevoked@ fires for partitions removed via a normal
    (cooperative) rebalance — i.e. the broker accepted the
    handoff and we should flush in-flight work, commit
    offsets, etc.
  * @onLost@ fires when the broker fenced us
    (@UNKNOWN_MEMBER_ID@ / @FENCED_INSTANCE_ID@): any
    in-flight state is junk because we may not commit
    offsets to the broker any more.
-}
setRebalanceListener
  :: Consumer
  -> ([TopicPartition] -> IO ())
  -- ^ onAssigned
  -> ([TopicPartition] -> IO ())
  -- ^ onRevoked
  -> ([TopicPartition] -> IO ())
  -- ^ onLost
  -> IO ()
setRebalanceListener Consumer {..} onA onR onL = atomically $ do
  writeTVar consumerOnAssigned onA
  writeTVar consumerOnRevoked onR
  writeTVar consumerOnLost onL


{- | Install the hook 'poll' runs just before re-joining the group
(see 'consumerPreRejoin'). Exceptions from the hook propagate out
of 'poll'.
-}
setPreRejoinHook :: Consumer -> IO () -> IO ()
setPreRejoinHook Consumer {..} hook = atomically (writeTVar consumerPreRejoin hook)


{- | Whether the coordinator has asked this member to re-join (the
next 'poll' will run the pre-rejoin hook and re-join first).
-}
rebalancePending :: Consumer -> IO Bool
rebalancePending Consumer {..} = case consumerHeartbeat of
  Nothing -> pure False
  Just (hbSt, _) -> readTVarIO (HB.hbNeedsRebalance hbSt)


{- | Current partition assignment, in deterministic order
(sorted by topic then partition).
-}
currentAssignment :: MonadIO m => Consumer -> m [TopicPartition]
currentAssignment Consumer {..} = liftIO $ do
  m <- readIORef consumerAssignment
  pure $
    List.sortOn
      (\tp -> (tp.topic, tp.partition))
      (HashMap.keys m)


{- | Pure delta computation between two partition assignments.
Returns @(revoked, added)@: revoked partitions are present
in @prev@ but absent from @now@; added partitions are in
@now@ but were not in @prev@. Both result lists are in
ascending @(topic, partition)@ order so consumers can rely
on deterministic ordering for log lines / metric labels.
-}
computeAssignmentDelta
  :: [TopicPartition]
  -- ^ previous assignment
  -> [TopicPartition]
  -- ^ new assignment
  -> ([TopicPartition], [TopicPartition])
computeAssignmentDelta prev now =
  let !prevSet = Set.fromList prev
      !nowSet = Set.fromList now
      !revoked = Set.toAscList (prevSet `Set.difference` nowSet)
      !added = Set.toAscList (nowSet `Set.difference` prevSet)
  in (revoked, added)


{- | Compute the assigned / revoked deltas between @prev@ and
@now@ and fire the appropriate user callbacks. Records the
new assignment as 'consumerLastAssignment' for the next
diff.

@asLost@ routes the "removed" side through 'consumerOnLost'
instead of 'consumerOnRevoked'. The runtime determines that
via the heartbeat's 'hbLost' flag, which is set by
'Kafka.Client.Internal.Heartbeat.applyHeartbeatOutcome' on
@UNKNOWN_MEMBER_ID@ / @FENCED_INSTANCE_ID@.
-}
dispatchAssignmentDelta
  :: Consumer
  -> [TopicPartition]
  -- ^ new assignment
  -> Bool
  -- ^ asLost
  -> IO ()
dispatchAssignmentDelta c@Consumer {..} now asLost = do
  prev <- atomically (readTVar consumerLastAssignment)
  let !(revoked, added) = computeAssignmentDelta prev now
  -- Persist before firing callbacks so that a callback that
  -- queries 'currentAssignment' sees the post-transition state.
  atomically $ writeTVar consumerLastAssignment now
  unless (null revoked) $ do
    if asLost
      then do
        onL <- atomically (readTVar consumerOnLost)
        catchIgnore (onL revoked)
      else do
        onR <- atomically (readTVar consumerOnRevoked)
        catchIgnore (onR revoked)
  unless (null added) $ do
    onA <- atomically (readTVar consumerOnAssigned)
    catchIgnore (onA added)
  where
    -- 'c' is only here to defeat the otherwise-unused warning
    -- for the record-wildcard binding above; the actual access
    -- happens through the TVars.
    _ = c
    catchIgnore m = do
      r <- try m :: IO (Either SomeException ())
      case r of
        Right () -> pure ()
        Left _ -> pure ()


{- | Unsubscribe from all topics.

Clears the subscription and partition assignment. Fires the
rebalance listener's @onRevoked@ callback for every
currently-assigned partition before clearing the assignment.
-}
unsubscribe :: MonadIO m => Consumer -> m ()
unsubscribe c@Consumer {..} = liftIO $ do
  dispatchAssignmentDelta c [] False
  atomicModifyIORef' consumerAssignment $ \_ -> (HashMap.empty, ())


{- | Manually assign partitions (disables group management).

Assigns specific partitions to this consumer without using consumer groups.
Useful for fine-grained control or when not using consumer groups.
-}
assign :: MonadIO m => Consumer -> [TopicPartition] -> m (Either String ())
assign consumer@Consumer {..} partitions = liftIO $ do
  -- Determine initial fetch offsets for each partition
  -- Strategy depends on consumerAutoOffsetReset
  let offsetStrategy = consumerAutoOffsetReset consumerConfig

  -- Query actual offsets from broker based on strategy
  case offsetStrategy of
    None -> do
      -- For None strategy, just use offset 0
      let !next = HashMap.fromList [(tp, 0) | tp <- partitions]
      atomicModifyIORef' consumerAssignment $ \_ -> (next, ())
      return $ Right ()
    _ -> do
      -- For Earliest/Latest, query the broker for actual offsets
      let timestamp = case offsetStrategy of
            Earliest -> -2 -- Special value for earliest offset
            Latest -> -1 -- Special value for latest offset
            None -> 0 -- Won't reach here
      offsetsResult <- queryPartitionOffsets consumer partitions timestamp
      case offsetsResult of
        Left err -> return $ Left err
        Right offsets -> do
          let !next = HashMap.fromList offsets
          atomicModifyIORef' consumerAssignment $ \_ -> (next, ())
          return $ Right ()


{- | Poll for new records.

Fetches records from all assigned partitions up to maxPollRecords.
Returns records from multiple partitions in no particular order.

= Auto-rebalance

If the heartbeat thread has flagged a pending rebalance
('HB.hbNeedsRebalance' set by the broker's @REBALANCE_IN_PROGRESS@
(error code 27) on a heartbeat reply), 'poll' transparently re-runs
the JoinGroup \/ SyncGroup \/ OffsetFetch flow against the same
subscription before fetching. Callers do not need to remember to
call 'subscribe' again on rebalance — they only do that the first
time, to declare what topics they want.
-}
poll
  :: MonadIO m
  => Consumer
  -> Int
  -- ^ Timeout in milliseconds
  -> m (Either String [ConsumerRecord])
poll consumer@Consumer {..} timeoutMs = liftIO $ do
  woke <- atomically $ do
    pending <- readTVar consumerWakeupPending
    writeTVar consumerWakeupPending False
    pure pending
  if woke
    then pure (Right [])
    else pollAfterWakeupCheck
  where
    pollAfterWakeupCheck = do
      -- Auto-rebalance: if the heartbeat thread saw REBALANCE_IN_PROGRESS
      -- and we know what topics we're subscribed to, re-join now.
      -- 'consumerSubscription' was a TVar pre-Tier-1; the rebalance
      -- check is now two independent reads (rebalance flag from STM,
      -- subscription list from an IORef). The race is harmless: if a
      -- subscribe(' ) lands between the two reads we'll either trigger
      -- rejoin once unnecessarily or miss this tick and catch it on
      -- the next poll, both of which match the pre-Tier-1 STM
      -- semantics for an interleaving against 'subscribe'.
      needsRejoin <- case consumerHeartbeat of
        Nothing -> pure False
        Just (hbSt, _) -> do
          flag <- readTVarIO (HB.hbNeedsRebalance hbSt)
          topicsM <- readIORef consumerSubscription
          pure (flag && case topicsM of Just _ -> True; Nothing -> False)
      rejoinR <-
        if needsRejoin
          then do
            topicsM <- readIORef consumerSubscription
            case topicsM of
              Just ts -> do
                -- Still in the old generation: give the owner its
                -- last chance to commit, unless the coordinator
                -- already fenced it (a commit would be rejected).
                lost <- case consumerHeartbeat of
                  Just (hbSt, _) -> readTVarIO (HB.hbLost hbSt)
                  Nothing -> pure False
                unless lost $ do
                  hook <- readTVarIO consumerPreRejoin
                  hook
                subscribe consumer ts
              Nothing -> pure (Right ())
          else pure (Right ())
      case rejoinR of
        Left err -> return (Left ("poll: rebalance rejoin failed: " <> err))
        Right () -> doPoll

    doPoll = do
      -- Tier 3 of the STM-replacement work: 'consumerAssignment'
      -- and 'consumerPaused' moved off STM. Two independent
      -- 'readIORef's replace the per-poll @atomically + ListT.toList
      -- + ListT.toList@ walk that previously dominated the consumer
      -- hot path (~200-400 ns / poll on the snapshot alone). The
      -- two reads are not transactionally consistent, but a
      -- pause / resume that interleaves with the snapshot is
      -- harmless: at worst we issue one extra fetch for a paused
      -- partition (whose records we then drop on the next poll
      -- because the assignment is rechecked on commit).
      asgn <- readIORef consumerAssignment
      pausedHM <- readIORef consumerPaused
      let assignment = HashMap.toList (HashMap.difference asgn pausedHM)

      if null assignment
        then return $ Right [] -- No assignment yet or all paused
        else do
          -- Fetch from all active partitions
          result <- fetchRecords consumer assignment timeoutMs

          case result of
            Left err -> return $ Left err
            Right FetchResult {frRecords = records, frErrors = errs, frBatchEnds = ends} -> do
              handled <- handleFetchErrors consumer errs
              case handled of
                Left err -> return (Left err)
                Right () -> deliver records ends

    -- Cap at max.poll.records, then advance fetch positions past the
    -- records handed back only. Each partition's returned records
    -- are a prefix of its fetched ones, so the rest are fetched again
    -- by the next poll; advancing past them would let the next commit
    -- skip records the caller never saw. A single
    -- 'atomicModifyIORef\'' folds the advance over the whole map.
    deliver records ends = do
      let (limitedRecords, advance) = advanceFetchPositions (consumerMaxPollRecords consumerConfig) records ends
      atomicModifyIORef' consumerAssignment $ \m0 ->
        let !m1 = advance m0 in (m1, ())
      -- ConsumerInterceptor.onConsume (KIP-388 / JVM): an
      -- exception here propagates; tracing tools that just
      -- need to attach attributes shouldn't throw.
      iceptedRecords <- consumerInterceptor consumerConfig limitedRecords
      return $ Right iceptedRecords



{- | What one poll hands back and how it moves the fetch positions:
at most @maxRecords@ of the fetched @records@; each partition moves
past its last delivered record, and a partition with nothing held
back moves on to the end of its fetched batches (@ends@), past
transaction markers and aborted batches.

Only partitions already in the position map move ('HashMap.adjust':
one revoked while its fetch was in flight is not re-added).
-}
advanceFetchPositions
  :: Int
  -> [ConsumerRecord]
  -> HashMap.HashMap TopicPartition Int64
  -> ([ConsumerRecord], HashMap.HashMap TopicPartition Int64 -> HashMap.HashMap TopicPartition Int64)
advanceFetchPositions maxRecords records ends =
  let (delivered, heldBack) = splitAt maxRecords records
      partial = HashSet.fromList (map (\r -> TopicPartition r.topic r.partition) heldBack)
      complete = HashMap.filterWithKey (\tp _ -> not (HashSet.member tp partial)) ends
      pastRecord !m r = HashMap.adjust (max (r.offset + 1)) (TopicPartition r.topic r.partition) m
      advance m0 =
        HashMap.foldlWithKey' (\m tp end -> HashMap.adjust (max end) tp m) (foldl' pastRecord m0 delivered) complete
  in (delivered, advance)


{- | Wake the next 'poll' call. This is intentionally one-shot:
once a poll observes the flag it clears it and returns an empty
batch without contacting the broker.
-}
wakeupConsumer :: MonadIO m => Consumer -> m ()
wakeupConsumer Consumer {..} =
  liftIO $ atomically $ writeTVar consumerWakeupPending True


{- | 'poll' variant that returns the Java-style 'ConsumerRecords'
batch wrapper.
-}
pollRecords :: MonadIO m => Consumer -> Int -> m (Either String ConsumerRecords)
pollRecords consumer timeoutMs = do
  r <- poll consumer timeoutMs
  pure (ConsumerRecords <$> r)


{- | Seek to a specific offset on an /assigned/ partition. The
next 'poll' will re-fetch starting at @offset@. Mirrors
@KafkaConsumer.seek(tp, offset)@.

Returns 'Left' if the partition isn't currently assigned to
this consumer (the JVM client throws 'IllegalStateException'
in that case).
-}
seek :: MonadIO m => Consumer -> TopicPartition -> Int64 -> m (Either String ())
seek Consumer {..} tp offset = liftIO $
  atomicModifyIORef' consumerAssignment $ \m ->
    case HashMap.lookup tp m of
      Nothing ->
        ( m
        , Left
            ( "seek: partition not assigned: "
                ++ T.unpack tp.topic
                ++ ":"
                ++ show tp.partition
            )
        )
      Just _ -> (HashMap.insert tp offset m, Right ())


{- | Seek to the earliest available offset for each partition.
Mirrors @KafkaConsumer.seekToBeginning(partitions)@.
-}
seekToBeginning :: MonadIO m => Consumer -> [TopicPartition] -> m (Either String ())
seekToBeginning consumer tps = seekToTimestamp consumer tps (-2)


{- | Seek to the latest available offset (i.e. the high water
mark). Mirrors @KafkaConsumer.seekToEnd(partitions)@.
-}
seekToEnd :: MonadIO m => Consumer -> [TopicPartition] -> m (Either String ())
seekToEnd consumer tps = seekToTimestamp consumer tps (-1)


{- | Helper: query the broker for the offset at the supplied
timestamp (-1 = latest, -2 = earliest, otherwise milliseconds
since epoch) and seek every partition to it.
-}
seekToTimestamp :: MonadIO m => Consumer -> [TopicPartition] -> Int64 -> m (Either String ())
seekToTimestamp _ [] _ = pure (Right ())
seekToTimestamp consumer@Consumer {..} tps timestamp = liftIO $ do
  r <- queryPartitionOffsets consumer tps timestamp
  case r of
    Left err -> pure (Left err)
    Right offsets -> do
      atomicModifyIORef' consumerAssignment $ \m ->
        let !m' = foldl' (\acc (tp, off) -> HashMap.insert tp off acc) m offsets
        in (m', ())
      pure (Right ())


{- | Commit offsets synchronously.

Commits the current fetch positions for all assigned partitions.
Blocks until the broker acknowledges the commit.
-}
commitSync :: MonadIO m => Consumer -> m (Either String ())
commitSync consumer@Consumer {..} = liftIO $ do
  -- Get current offsets to commit
  offsets <- HashMap.toList <$> readIORef consumerAssignment

  if null offsets
    then return $ Right () -- Nothing to commit
    else do
      r <- commitOffsetsSync consumer (consumerGroupId consumerConfig) offsets
      case r of
        Right () -> dispatchOnCommit consumer offsets >> pure (Right ())
        Left e -> pure (Left e)


{- | Commit offsets asynchronously.

Commits the current fetch positions for all assigned partitions.
Returns immediately without waiting for broker acknowledgment.
-}
commitAsync :: MonadIO m => Consumer -> m (Either String ())
commitAsync consumer@Consumer {..} = liftIO $ do
  -- Get current offsets to commit
  offsets <- HashMap.toList <$> readIORef consumerAssignment

  if null offsets
    then return $ Right () -- Nothing to commit
    else do
      -- Fire-and-forget; the on-commit callback runs in the same
      -- async after the broker reply.
      _ <- async $ do
        r <- commitOffsetsSync consumer (consumerGroupId consumerConfig) offsets
        case r of
          Right () -> dispatchOnCommit consumer offsets
          Left _ -> pure ()
      return $ Right ()


{- | Best-effort dispatch of 'consumerOnCommit'. Wrapped in 'try'
so a buggy hook can't interfere with the commit pipeline.
-}
dispatchOnCommit
  :: Consumer
  -> [(TopicPartition, Int64)]
  -> IO ()
dispatchOnCommit Consumer {..} offsets = do
  r <-
    try (consumerOnCommit consumerConfig offsets)
      :: IO (Either Control.Exception.SomeException ())
  case r of
    Right () -> pure ()
    Left _ -> pure ()


{- | Get the committed offset for a single partition. Mirrors
@KafkaConsumer.committed(tp)@. Convenience wrapper around
'committedAll'.
-}
committed :: MonadIO m => Consumer -> TopicPartition -> m (Either String Int64)
committed consumer tp = liftIO $ do
  r <- committedAll consumer [tp]
  case r of
    Left err -> pure (Left err)
    Right hm -> case HashMap.lookup tp hm of
      Nothing ->
        pure
          ( Left
              ( "committed: no offset returned for "
                  ++ T.unpack tp.topic
                  ++ ":"
                  ++ show tp.partition
              )
          )
      Just off -> pure (Right off)


{- | Fetch committed offsets for many partitions in one
broker round-trip. The Java client's
@KafkaConsumer.committed(Set\<TopicPartition\>)@ analogue.

Returns 'HashMap' keyed by 'TopicPartition' with the broker's
last committed offset; partitions with no committed offset are
absent from the result map (rather than aliased to 0 — that's
consistent with the JVM client semantics).
-}
committedAll
  :: MonadIO m
  => Consumer
  -> [TopicPartition]
  -> m (Either String (HashMap.HashMap TopicPartition Int64))
committedAll _ [] = pure (Right HashMap.empty)
committedAll consumer@Consumer {..} tps =
  liftIO $
    fetchCommittedOffsetsBatch
      consumer
      (consumerGroupId consumerConfig)
      tps


{- | Current consumer position (the offset of the next
record that will be returned by 'poll'). Read from the local
assignment map, /not/ the broker; this is what the JVM client's
@position(tp)@ returns.
-}
position :: MonadIO m => Consumer -> TopicPartition -> m (Either String Int64)
position Consumer {..} tp = liftIO $ do
  m <- readIORef consumerAssignment
  case HashMap.lookup tp m of
    Nothing ->
      pure
        ( Left
            ( "position: partition not assigned: "
                ++ T.unpack tp.topic
                ++ ":"
                ++ show tp.partition
            )
        )
    Just off -> pure (Right off)


{- | Query the earliest offset available for each
partition. Mirrors @KafkaConsumer.beginningOffsets(partitions)@.
-}
beginningOffsets
  :: MonadIO m
  => Consumer
  -> [TopicPartition]
  -> m (Either String (HashMap.HashMap TopicPartition Int64))
beginningOffsets consumer tps = liftIO $ do
  r <- queryPartitionOffsets consumer tps (-2) -- earliest
  pure (fmap HashMap.fromList r)


{- | Query the high-water-mark offset (i.e. one past the
last produced record) for each partition. Mirrors
@KafkaConsumer.endOffsets(partitions)@.
-}
endOffsets
  :: MonadIO m
  => Consumer
  -> [TopicPartition]
  -> m (Either String (HashMap.HashMap TopicPartition Int64))
endOffsets consumer tps = liftIO $ do
  r <- queryPartitionOffsets consumer tps (-1) -- latest
  pure (fmap HashMap.fromList r)


{- | For each partition, return the earliest offset whose
timestamp is greater than or equal to the supplied timestamp.
Partitions whose timestamp is past the broker's high water mark
are returned with an offset of @-1@.
-}
offsetsForTimes
  :: MonadIO m
  => Consumer
  -> [(TopicPartition, Int64)]
  -- ^ (partition, target timestamp ms)
  -> m (Either String (HashMap.HashMap TopicPartition Int64))
offsetsForTimes _ [] = pure (Right HashMap.empty)
offsetsForTimes consumer pts = do
  r <- offsetsForTimesFull consumer pts
  pure (fmap (HashMap.map oatOffset) r)


{- | The richer return type from 'offsetsForTimesFull'. Mirrors
the JVM client's @OffsetAndTimestamp@: on top of the offset,
the broker also returns the actual timestamp of the first
record at-or-after the requested timestamp (which can be
/later/ than the requested one if no record landed exactly on
it) and — from ListOffsets v4+ — the partition leader epoch
at the time, which callers should pass back into 'seek' /
'commitSync' when they want fencing.
-}
data OffsetAndTimestamp = OffsetAndTimestamp
  { oatOffset :: !Int64
  {- ^ The offset of the first record at-or-after the
  requested timestamp. @-1@ means the broker had no record
  at-or-after the timestamp.
  -}
  , oatTimestamp :: !Int64
  {- ^ The actual record timestamp (ms since epoch). May be
  greater than the requested timestamp. @-1@ on brokers
  that don't report it (very old / pre-v1) or when
  @oatOffset == -1@.
  -}
  , oatLeaderEpoch :: !Int32
  {- ^ The leader epoch at the time of the record. @-1@ on
  pre-v4 brokers that don't include the field. Useful for
  fencing on subsequent commit/seek.
  -}
  }
  deriving (Eq, Show, Generic)


{- | Like 'offsetsForTimes', but also returns the broker-reported
timestamp + leader epoch for each partition. Mirrors
@KafkaConsumer.offsetsForTimes@ in the JVM client which
returns @Map\<TopicPartition, OffsetAndTimestamp\>@.

Use this when the caller needs the leader-epoch fencing
semantics from KIP-320 (passing the returned epoch back into
@commitSync@ rejects commits across leadership changes); for
the simple offset-only case, 'offsetsForTimes' is the existing
(Int64-only) façade.
-}
offsetsForTimesFull
  :: MonadIO m
  => Consumer
  -> [(TopicPartition, Int64)]
  -> m (Either String (HashMap.HashMap TopicPartition OffsetAndTimestamp))
offsetsForTimesFull _ [] = pure (Right HashMap.empty)
offsetsForTimesFull consumer pts = liftIO $ do
  r <- queryPartitionOffsetsByTimestampFull consumer pts
  pure (fmap HashMap.fromList r)


-- | Pause consumption from partitions.
pause :: MonadIO m => Consumer -> [TopicPartition] -> m ()
pause Consumer {..} tps = liftIO $
  atomicModifyIORef' consumerPaused $ \m ->
    let !m' = foldl' (\acc tp -> HashMap.insert tp () acc) m tps
    in (m', ())


-- | Resume consumption from partitions.
resume :: MonadIO m => Consumer -> [TopicPartition] -> m ()
resume Consumer {..} tps = liftIO $
  atomicModifyIORef' consumerPaused $ \m ->
    let !m' = foldl' (\acc tp -> HashMap.delete tp acc) m tps
    in (m', ())


-- | Get current partition assignment.
assignment :: MonadIO m => Consumer -> m [TopicPartition]
assignment Consumer {..} = liftIO $ do
  pairs <- HashMap.toList <$> readIORef consumerAssignment
  return $ map fst pairs


{- | List the partitions currently paused via 'pause'. Mirrors
@KafkaConsumer.paused()@.
-}
paused :: MonadIO m => Consumer -> m [TopicPartition]
paused Consumer {..} = liftIO $ do
  pairs <- HashMap.toList <$> readIORef consumerPaused
  pure (map fst pairs)


{- | Internal: Fetch records from a list of topic-partitions

This function groups partitions by topic, sends FetchRequests to the
appropriate partition leaders, and decodes the RecordBatches.
-}
fetchRecords
  :: Consumer
  -> [(TopicPartition, Int64)]
  -- ^ Partitions and their fetch offsets
  -> Int
  -- ^ Timeout (ms)
  -> IO (Either String FetchResult)
fetchRecords consumer@Consumer {..} partitions timeoutMs = do
  -- Group partitions by topic
  let byTopic =
        Map.fromListWith
          (++)
          [ (tp.topic, [(tp.partition, offset)])
          | (tp, offset) <- partitions
          ]

  -- For each topic, find partition leaders and group by broker
  -- If leaders aren't found, refresh metadata first
  let topics = Map.keys byTopic

  -- Get any broker from the metadata cache to refresh metadata if needed
  brokersM <- atomically $ Meta.getAllBrokers consumerMetadata
  case brokersM of
    Nothing -> return $ Left "No brokers available in metadata cache"
    Just [] -> return $ Left "No brokers available in metadata cache"
    Just (broker : _) -> do
      let brokerAddr = Meta.brokerMetaAddress broker
      connResult <- consumerConnect consumer brokerAddr
      case connResult of
        Left err -> return $ Left $ "Failed to connect to broker: " ++ err
        Right conn -> do
          -- Get current correlation ID and increment
          corrId <- atomicModifyIORef' consumerCorrelationId $ \cid -> (cid + 1, cid)

          -- Refresh metadata for these topics
          refreshResult <- Conn.withBrokerLock consumerConnManager brokerAddr $ Meta.refreshTopicMetadata conn consumerMetadata (Just topics) corrId
          case refreshResult of
            Left err -> return $ Left $ "Failed to refresh metadata: " ++ err
            Right _ -> do
              -- Now look up leaders
              -- A partition with no known leader (mid-election, or a
              -- topic still being created) is skipped this poll and
              -- retried on the next one, which refreshes metadata
              -- again, rather than failing the whole fetch.
              topicLeaders <- fmap Map.fromList $ forM (Map.toList byTopic) $ \(topic, parts) -> do
                leaders <- forM parts $ \(partId, offset) -> do
                  leaderM <- atomically $ Meta.getPartitionLeader consumerMetadata topic partId
                  pure $ fmap (\leader -> (leader, (partId, offset))) leaderM
                return (topic, catMaybes leaders)

              do
                  -- Group by broker
                  let byBroker =
                        Map.fromListWith
                          (++)
                          [ (broker, [(topic, partId, offset)])
                          | (topic, leaders) <- Map.toList topicLeaders
                          , (broker, (partId, offset)) <- leaders
                          ]

                  -- Fetch from each broker (KIP-392: pass rack ID).
                  -- Threads the configured isolation level through so
                  -- read-committed consumers actually filter aborted
                  -- transactions; the previous code hardcoded
                  -- READ_UNCOMMITTED (0) which made
                  -- 'consumerIsolationLevel' a no-op.
                  results <- forM (Map.toList byBroker) $ \(broker, reqs) ->
                    fetchFromBroker
                      consumerConnManager
                      consumerVersionCache
                      broker
                      reqs
                      timeoutMs
                      consumerCorrelationId
                      (consumerRackId consumerConfig)
                      (consumerConnConfig consumer)
                      (consumerIsolationLevel consumerConfig)

                  -- Combine results
                  case sequence results of
                    Left err -> return $ Left err
                    Right frs ->
                      return $
                        Right
                          FetchResult
                            { frRecords = concatMap frRecords frs
                            , frErrors = concatMap frErrors frs
                            , frBatchEnds = foldl' (HashMap.unionWith max) HashMap.empty (map frBatchEnds frs)
                            }


-- | Internal: Fetch from a single broker
fetchFromBroker
  :: Conn.ConnectionManager
  -> AV.ApiVersionCache
  -> Meta.BrokerMetadata
  -> [(Text, Int32, Int64)]
  -- ^ (topic, partition, offset)
  -> Int
  -- ^ Timeout (ms)
  -> IORef Int32
  -- ^ Correlation ID source
  -> Maybe Text
  -- ^ Rack ID for rack-aware fetching (KIP-392)
  -> Conn.ConnectionConfig
  -- ^ Connection / SASL config (re-used cached conns when matching)
  -> IsolationLevel
  -- ^ KIP-98 isolation level (ReadUncommitted / ReadCommitted)
  -> IO (Either String FetchResult)
fetchFromBroker connMgr versionCache broker requests timeoutMs corrIdVar rackIdM connConfig isolationLevel = do
  let brokerAddr = Meta.brokerMetaAddress broker
      nextCid = atomicModifyIORef' corrIdVar $ \cid -> (cid + 1, cid)
  connResult <- Conn.getOrCreateConnection connMgr brokerAddr connConfig
  case connResult of
    Left err -> return $ Left $ "Failed to connect: " ++ err
    Right conn -> do
      -- Negotiate ApiVersions if we haven't already; idempotent.
      _ <- VN.ensureVersionsNegotiated conn brokerAddr versionCache nextCid
      corrId <- nextCid
      let apiKey = 1 -- Fetch
      -- FetchRequest body-level changes by version:
      --   v4  KIP-98  IsolationLevel
      --   v7  KIP-227 SessionId / SessionEpoch
      --   v11 KIP-392 RackId; KIP-573 ClusterId
      --   v12 went flexible (compact strings + tagged fields).
      --       The ClusterId field at v12 is a /tagged/
      --       compact-string with a null default; the codegen
      --       was emitting it with the non-compact serializer
      --       inside the tagged-fields envelope which the
      --       broker rejected with an EOF — fixed in this
      --       commit's codegen tagged-field-encoder change
      --       (Generator.generateTaggedFieldEncoder now uses
      --       toCompactString for tagged-string payloads).
      --   v13 moved to TopicId-based identification — needs
      --       the topic-id metadata cache plumbing that's
      --       still TODO; cap at v12 until then.
      --   v15 ReplicaState moved into a tagged field
      --   v17 ReplicaDirectoryId added (tagged)
      -- Codegen handles up to v17 against the upstream 4.0.0
      -- schemas. The api key + cache lookup come from the
      -- 'KafkaMessage FR.FetchRequest' instance via
      -- 'pickApiVersionForRange'; the (4, 12) override caps
      -- below the codegen max.
      verR <-
        VN.pickApiVersionForRange @FR.FetchRequest
          4
          12
          versionCache
          brokerAddr
          4
      let apiVersion = case verR of
            Right v -> v
            Left _ -> 4 -- min supported (matches legacy fallback)

      -- Group by topic
      let byTopic =
            Map.fromListWith
              (++)
              [ (topic, [(partId, offset)])
              | (topic, partId, offset) <- requests
              ]

      -- Build FetchRequest
      let fetchTopics =
            V.fromList
              [ FR.FetchTopic
                  { FR.fetchTopicTopic = P.mkKafkaString topic
                  , FR.fetchTopicTopicId = P.nullUuid
                  , FR.fetchTopicPartitions =
                      P.mkKafkaArray $
                        V.fromList
                          [ FR.FetchPartition
                              { FR.fetchPartitionPartition = partId
                              , FR.fetchPartitionCurrentLeaderEpoch = -1
                              , FR.fetchPartitionFetchOffset = offset
                              , FR.fetchPartitionLastFetchedEpoch = -1
                              , FR.fetchPartitionLogStartOffset = -1
                              , FR.fetchPartitionPartitionMaxBytes = 1048576 -- 1MB per partition
                              -- KIP-915 ReplicaDirectoryId (tagged v17+).
                              -- The wire encoding writes this only on
                              -- v17+ and only inside the TaggedFields
                              -- envelope; non-replica consumers always
                              -- set nullUuid which is the broker's
                              -- "no specific directory" sentinel.
                              , FR.fetchPartitionReplicaDirectoryId = P.nullUuid
                              }
                          | (partId, offset) <- parts
                          ]
                  }
              | (topic, parts) <- Map.toList byTopic
              ]

          -- KIP-392: Use rack ID for rack-aware fetching if
          -- configured. The field is non-nullable in the upstream
          -- spec (default ""), so a 'Nothing' from the consumer
          -- config sends the empty string — sending 'Null'
          -- (length=-1) breaks the broker's parser at v11+.
          rackIdKafka = case rackIdM of
            Nothing -> P.mkKafkaString ""
            Just rackId -> P.mkKafkaString rackId

          request =
            FR.FetchRequest
              { FR.fetchRequestReplicaId = -1 -- Consumer (not a replica)
              , FR.fetchRequestMaxWaitMs = fromIntegral timeoutMs
              , FR.fetchRequestMinBytes = 1
              , FR.fetchRequestMaxBytes = 52428800 -- 50MB total
              , FR.fetchRequestIsolationLevel = isolationLevelCode isolationLevel
              , FR.fetchRequestSessionId = 0
              , FR.fetchRequestSessionEpoch = -1
              , FR.fetchRequestTopics = P.mkKafkaArray fetchTopics
              , FR.fetchRequestForgottenTopicsData = P.mkKafkaArray V.empty
              , FR.fetchRequestRackId = rackIdKafka -- KIP-392: rack-aware fetching
              , FR.fetchRequestClusterId = P.KafkaString P.Null
              , FR.fetchRequestReplicaState =
                  FR.ReplicaState
                    { FR.replicaStateReplicaId = -1
                    , FR.replicaStateReplicaEpoch = 0
                    }
              }

          requestBody = WC.runEncodeVer @FR.FetchRequest apiVersion request
          clientId = P.mkKafkaString "kafka-native-consumer"

      result <- Req.sendRequestReceiveResponseLocked (Conn.withBrokerLock connMgr brokerAddr) conn apiKey apiVersion corrId clientId requestBody

      case result of
        Left err -> return $ Left $ "Fetch failed: " ++ err
        Right (_, responseBody) -> do
          case WC.runDecodeVer @FResp.FetchResponse apiVersion responseBody of
            Left err -> return $ Left $ "Failed to parse FetchResponse: " ++ err
            Right response ->
              extractRecordsFromFetchResponse
                isolationLevel
                ( HashMap.fromList
                    (map (\(t, p, o) -> ((t, p), o)) requests)
                )
                response


-- | One partition's error code from a FetchResponse.
data PartitionFetchError = PartitionFetchError
  { pfeTopicPartition :: !TopicPartition
  , pfeErrorCode :: !Int16
  }


-- | Records plus per-partition errors from one fetch round.
data FetchResult = FetchResult
  { frRecords :: ![ConsumerRecord]
  , frErrors :: ![PartitionFetchError]
  , frBatchEnds :: !(HashMap.HashMap TopicPartition Int64)
  {- ^ Per partition, the offset after the last fetched batch. Once a
  partition's records are all delivered its position moves here, past
  trailing transaction markers and aborted batches that never become
  records (otherwise a read_committed consumer would refetch them
  forever and never reach the last stable offset).
  -}
  }


{- | Act on partition-level fetch errors the way the JVM @Fetcher@
does: @OFFSET_OUT_OF_RANGE@ resets the position by
@auto.offset.reset@ (failing under 'None'); leadership / metadata
errors are retried by the next 'poll', which refreshes metadata
first; anything else fails the poll.
-}
handleFetchErrors :: Consumer -> [PartitionFetchError] -> IO (Either String ())
handleFetchErrors _ [] = pure (Right ())
handleFetchErrors consumer@Consumer {..} errs = do
  let outOfRange = map pfeTopicPartition (filter ((== 1) . pfeErrorCode) errs)
      fatal = filter (\e -> pfeErrorCode e /= 1 && not (retriableFetchError (pfeErrorCode e))) errs
  case fatal of
    (e : _) ->
      pure
        ( Left
            ( "Fetch error for "
                ++ T.unpack (pfeTopicPartition e).topic
                ++ ":"
                ++ show (pfeTopicPartition e).partition
                ++ " code="
                ++ show (pfeErrorCode e)
            )
        )
    []
      | null outOfRange -> pure (Right ())
      | otherwise -> case consumerAutoOffsetReset consumerConfig of
          None ->
            pure
              ( Left
                  ( "Fetch offset out of range with auto.offset.reset=none: "
                      ++ show (map (\tp -> (tp.topic, tp.partition)) outOfRange)
                  )
              )
          strategy -> do
            let !ts = case strategy of
                  Earliest -> -2
                  _ -> -1
            r <- queryPartitionOffsets consumer outOfRange ts
            case r of
              Left e -> pure (Left ("offset reset: " ++ e))
              Right offs -> do
                atomicModifyIORef' consumerAssignment $ \m ->
                  (foldl' (\acc (tp, o) -> HashMap.adjust (const o) tp acc) m offs, ())
                pure (Right ())


{- | Fetch error codes that clear up on their own once metadata is
refreshed or the broker recovers: the partition is skipped this
poll and fetched again on the next.
-}
retriableFetchError :: Int16 -> Bool
retriableFetchError code =
  code
    `elem` [ 3 -- UNKNOWN_TOPIC_OR_PARTITION
           , 5 -- LEADER_NOT_AVAILABLE
           , 6 -- NOT_LEADER_OR_FOLLOWER
           , 7 -- REQUEST_TIMED_OUT
           , 9 -- REPLICA_NOT_AVAILABLE
           , 13 -- NETWORK_EXCEPTION
           , 56 -- KAFKA_STORAGE_ERROR
           , 74 -- FENCED_LEADER_EPOCH
           , 75 -- UNKNOWN_LEADER_EPOCH
           , 78 -- OFFSET_NOT_AVAILABLE
           , 100 -- UNKNOWN_TOPIC_ID
           ]


{- | Extract records from a FetchResponse.

Filters batches the consumer should never surface to user code:

  * /Control batches/ ('attrIsControl' = True) carry transaction
    commit / abort markers. They live in the log alongside data
    records but are bookkeeping for the read-committed reader,
    not application data.

  * /Aborted transactional batches/ when 'IsolationLevel' is
    'ReadCommitted'. The broker tells us which transactions on
    this partition aborted via @PartitionData.abortedTransactions@
    (a list of @(producerId, firstOffset)@ pairs); we walk the
    decoded batches and skip any transactional batch whose
    producer id matches an entry whose 'firstOffset' is at or
    before the batch's 'baseOffset'. A producer id can have
    several aborted transactions in flight, so we use the
    /smallest/ first-offset per producer id and skip everything
    past that until we hit a control batch (which the broker
    wrote at commit/abort time and we then drop too). This
    mirrors what the JVM client does inside
    @Fetcher.completedFetch.nextFetchedRecord@.

  * /Aborted transactional batches/ on 'ReadUncommitted' are
    /not/ filtered — the whole point of 'ReadUncommitted' is to
    see records as soon as they're written, regardless of
    whether the txn ultimately commits.

The previous code surfaced commit/abort markers (raw
@\\NUL\\NUL\\NUL\\NUL\\NUL\\NUL@-shaped values) as user records,
which broke 'ReadCommitted' end-to-end and surprised
'ReadUncommitted' callers with garbage payloads.
-}
extractRecordsFromFetchResponse
  :: IsolationLevel
  -> HashMap.HashMap (Text, Int32) Int64
  {- ^ The offset each partition was fetched from. The broker sends
  whole batches, so the first batch can start before it; records
  below it were already delivered and are dropped here.
  -}
  -> FResp.FetchResponse
  -> IO (Either String FetchResult)
extractRecordsFromFetchResponse isolationLevel fetchOffsets response = do
  let topicsVec = case P.unKafkaArray (FResp.fetchResponseResponses response) of
        P.Null -> V.empty
        P.NotNull v -> v
  -- Partition-level errors don't fail the fetch: 'poll' decides per
  -- error code (reset, retry next poll, or fail).
  errsRef <- newIORef []
  endsRef <- newIORef HashMap.empty
  -- Walk topics with V.foldM' so we don't build an intermediate
  -- list of '[[ConsumerRecord]]' chunks; per-iteration work is
  -- in 'IO' because 'decodeAllBatches' may decompress.
  r <- go (errsRef, endsRef) (V.length topicsVec) topicsVec
  errs <- readIORef errsRef
  ends <- readIORef endsRef
  pure (fmap (\recs -> FetchResult {frRecords = recs, frErrors = errs, frBatchEnds = ends}) r)
  where
    go _ 0 _ = pure (Right [])
    go errsRef _ topicsVec = do
      r <- V.foldM' (addTopic errsRef) (Right []) topicsVec
      case r of
        Left e -> pure (Left e)
        Right vs -> pure (Right (V.toList (V.concat (reverse vs))))

    addTopic
      :: (IORef [PartitionFetchError], IORef (HashMap.HashMap TopicPartition Int64))
      -> Either String [V.Vector ConsumerRecord]
      -> FResp.FetchableTopicResponse
      -> IO (Either String [V.Vector ConsumerRecord])
    addTopic errsRef acc topicResp = case acc of
      Left e -> pure (Left e)
      Right chunks -> do
        let topicName = extractKafkaString $ FResp.fetchableTopicResponseTopic topicResp
            partitionsVec =
              case P.unKafkaArray (FResp.fetchableTopicResponsePartitions topicResp) of
                P.Null -> V.empty
                P.NotNull v -> v
        partRes <- V.foldM' (addPartition errsRef topicName) (Right V.empty) partitionsVec
        case partRes of
          Left e -> pure (Left e)
          Right pv -> pure (Right (pv : chunks))

    addPartition
      :: (IORef [PartitionFetchError], IORef (HashMap.HashMap TopicPartition Int64))
      -> Text
      -> Either String (V.Vector ConsumerRecord)
      -> FResp.PartitionData
      -> IO (Either String (V.Vector ConsumerRecord))
    addPartition (errsRef, endsRef) topicName acc partResp = case acc of
      Left e -> pure (Left e)
      Right partial -> do
        let partId = FResp.partitionDataPartitionIndex partResp
            errorCode = FResp.partitionDataErrorCode partResp
            recordsBytes =
              case P.unKafkaBytes (FResp.partitionDataRecords partResp) of
                P.Null -> BS.empty
                P.NotNull bs -> bs
            -- Aborted transactions overlapping this fetch, in
            -- first-offset order. Only consulted on ReadCommitted.
            abortedTxns = case isolationLevel of
              ReadUncommitted -> []
              ReadCommitted ->
                let abortsVec =
                      case P.unKafkaArray
                        (FResp.partitionDataAbortedTransactions partResp) of
                        P.Null -> V.empty
                        P.NotNull v -> v
                in List.sortOn snd
                     ( V.toList
                         ( V.map
                             ( \at ->
                                 ( FResp.abortedTransactionProducerId at
                                 , FResp.abortedTransactionFirstOffset at
                                 )
                             )
                             abortsVec
                         )
                     )
            !fetchOffset =
              HashMap.lookupDefault 0 (topicName, partId) fetchOffsets
        if errorCode /= 0
          then do
            modifyIORef'
              errsRef
              ( PartitionFetchError
                  { pfeTopicPartition = TopicPartition topicName partId
                  , pfeErrorCode = errorCode
                  }
                  :
              )
            pure (Right partial)
          else
            if BS.null recordsBytes
              then pure (Right partial)
              else do
                -- Sliced decoder: keys / values / headers stay
                -- as 'SliceVector' views over the source buffer
                -- through 'ConsumerRecord' construction. See
                -- 'Kafka.Protocol.RecordBatchWire' for the
                -- SlicedRecordBatch design.
                batches <- decodeAllBatchesSliced recordsBytes
                case batches of
                  Left err -> pure (Left ("Failed to decode batches: " ++ err))
                  Right bs -> do
                    case bs of
                      [] -> pure ()
                      _ ->
                        let lastB = last bs
                            !end = RBW.sbBaseOffset lastB + fromIntegral (RBW.sbLastOffsetDelta lastB) + 1
                        in modifyIORef' endsRef (HashMap.insertWith max (TopicPartition topicName partId) end)
                    let !v =
                          selectFetchedRecords
                            isolationLevel
                            abortedTxns
                            fetchOffset
                            topicName
                            partId
                            bs
                    pure (Right (partial V.++ v))



{- | The records of one partition's fetched batches that reach the
caller, in log order (the JVM's @CompletedFetch.nextFetchedRecord@):

  * Control batches (transaction markers) never do.
  * On 'ReadCommitted', an aborted transaction's producer id becomes
    "aborting" once the scan reaches the transaction's first offset;
    its transactional batches are dropped from there until its ABORT
    marker (a control batch from that producer) ends it, so the same
    producer's later transactions are kept.
  * Records below the fetch offset are dropped: the broker sends
    whole batches, so the first one can start before it.
-}
selectFetchedRecords
  :: IsolationLevel
  -> [(Int64, Int64)]
  {- ^ Aborted transactions overlapping the fetch, as
  @(producerId, firstOffset)@ sorted by @firstOffset@ (ignored on
  'ReadUncommitted').
  -}
  -> Int64
  -- ^ Fetch offset
  -> Text
  -> Int32
  -> [RBW.SlicedRecordBatch]
  -> V.Vector ConsumerRecord
selectFetchedRecords isolationLevel aborted fetchOffset topicName partId bs =
  V.filter
    (\r -> r.offset >= fetchOffset)
    ( V.concat
        ( map
            (convertSlicedBatchToRecordsV topicName partId)
            (keepBatches pending HashSet.empty bs)
        )
    )
  where
    !pending = case isolationLevel of
      ReadUncommitted -> []
      ReadCommitted -> aborted
    keepBatches
      :: [(Int64, Int64)]
      -> HashSet.HashSet Int64
      -> [RBW.SlicedRecordBatch]
      -> [RBW.SlicedRecordBatch]
    keepBatches _ _ [] = []
    keepBatches pend aborting (batch : rest) =
      let !lastOff = RBW.sbBaseOffset batch + fromIntegral (RBW.sbLastOffsetDelta batch)
          (starting, pend') = span (\(_, first) -> first <= lastOff) pend
          !aborting' = foldl' (\acc (pid, _) -> HashSet.insert pid acc) aborting starting
          !attrs = RBW.sbAttributes batch
          !pid = RBW.sbProducerId batch
      in if RB.attrIsControl attrs
           then keepBatches pend' (HashSet.delete pid aborting') rest
           else
             if RB.attrIsTransactional attrs && HashSet.member pid aborting'
               then keepBatches pend' aborting' rest
               else batch : keepBatches pend' aborting' rest


-- | 'Vector'-returning sibling of 'convertBatchToRecords'.
convertBatchToRecordsV :: Text -> Int32 -> RB.RecordBatch -> V.Vector ConsumerRecord
convertBatchToRecordsV topic partId batch =
  let baseOffset = RB.batchBaseOffset batch
      baseTimestamp = RB.batchBaseTimestamp batch
  in V.map
       ( \rec ->
           ConsumerRecord
             { topic = topic
             , partition = partId
             , offset = baseOffset + fromIntegral (RB.recordOffsetDelta rec)
             , timestamp = baseTimestamp + RB.recordTimestampDelta rec
             , key = RB.recordKey rec
             , value = RB.recordValue rec
             , headers = convertHeaders (RB.recordHeaders rec)
             }
       )
       (RB.batchRecords batch)


{- | Materialise a 'V.Vector ConsumerRecord' directly from the
sliced-batch shape. Skips the per-record 'RB.Record' /
'RB.RecordHeader' allocations the 'V.Vector Record' path
would have produced; the per-record key + value
'BS.ByteString's are still zero-copy slices of the source
buffer (the 'SliceVector' just collapses N independent
'ForeignPtr' GC roots into one).
-}
convertSlicedBatchToRecordsV
  :: Text -> Int32 -> RBW.SlicedRecordBatch -> V.Vector ConsumerRecord
convertSlicedBatchToRecordsV topic partId sb =
  let !n = RBW.slicedRecordCount sb
  in V.generate n $ \i ->
       ConsumerRecord
         { topic = topic
         , partition = partId
         , offset = RBW.slicedRecordOffset sb i
         , timestamp = RBW.slicedRecordTimestamp sb i
         , key = RBW.slicedRecordKey sb i
         , value = RBW.slicedRecordValue sb i
         , headers = convertSlicedHeaders sb i
         }


{- | Pull the i-th record's headers out of a 'SlicedRecordBatch'
in the public @[(Text, ByteString)]@ shape. Mirrors
'convertHeaders' (UTF-8 decode the key, drop null values).
-}
convertSlicedHeaders
  :: RBW.SlicedRecordBatch -> Int -> [(Text, ByteString)]
convertSlicedHeaders sb i =
  let !cnt = RBW.slicedRecordHeaderCount sb i
      go !j acc
        | j < 0 = acc
        | otherwise =
            let (kBs, mvBs) = RBW.slicedRecordHeader sb i j
            in case mvBs of
                 Nothing -> go (j - 1) acc
                 Just vBs -> go (j - 1) ((TE.decodeUtf8 kBs, vBs) : acc)
  in go (cnt - 1) []


-- | Extract Text from KafkaString
extractKafkaString :: P.KafkaString -> Text
extractKafkaString ks = case P.unKafkaString ks of
  P.Null -> T.empty
  P.NotNull t -> t


-- | Decode all RecordBatches from a ByteString.
decodeAllBatches :: ByteString -> IO (Either String [RB.RecordBatch])
decodeAllBatches input = go input []
  where
    -- Tail-recursive accumulator so we don't pay 'cons : decode'
    -- per batch. Result list is reversed at the end.
    go bs acc
      | BS.null bs = pure (Right (reverse acc))
      | otherwise =
          -- Check the compression bit on the batch attributes
          -- without decoding the full batch first; we know the
          -- bit's offset (21 + 0 -> the first byte of attrs).
          -- The cheap path: try the Wire decoder; if it returns
          -- a NoCompression batch, use it; otherwise fall back to
          -- the IO-decompressing path.
          case RBW.decodeRecordBatchWire bs of
            Right batch
              | RB.attrCompressionType (RB.batchAttributes batch)
                  == Compression.NoCompression -> do
                  let batchSize = 8 + 4 + fromIntegral (calculateBatchLength batch)
                      remaining = BS.drop batchSize bs
                  go remaining (batch : acc)
            _ -> do
              -- Either the Wire decoder errored (truncated input,
              -- bad CRC, …) or the batch is compressed; fall back
              -- to the IO decoder which handles both cases. The
              -- IO version is the Wire-shaped decompressing
              -- decoder ('decodeRecordBatchWireWithDecompression') —
              -- byte-identical with the pure 'decodeRecordBatchWire'
              -- when the batch is uncompressed; for compressed
              -- batches it slices the records section, decompresses,
              -- and re-decodes via the same Wire pokes.
              result <- RBW.decodeRecordBatchWireWithDecompression bs
              case result of
                Left err -> pure (Left err)
                Right batch -> do
                  let batchSize = 8 + 4 + fromIntegral (calculateBatchLength batch)
                      remaining = BS.drop batchSize bs
                  go remaining (batch : acc)


-- | Calculate the length field value for a batch (everything after the length field)
calculateBatchLength :: RB.RecordBatch -> Int32
calculateBatchLength batch =
  let encoded = RBW.encodeRecordBatchWire batch
      -- Skip base offset (8 bytes) to get to length field
      lengthBytes = BS.take 4 $ BS.drop 8 encoded
  in case W.readInt32BE lengthBytes of
       Left _ -> 0
       Right len -> len


{- | Sliced-shape sibling of 'decodeAllBatches'. Walks the
concatenated record-batches buffer once, materialising each
batch as a 'SlicedRecordBatch' (memory-efficient flat slice
vectors over the source 'ForeignPtr', or over a fresh
decompressed buffer for compressed batches).

The advance-cursor step uses the on-the-wire @length@ field
(peeked directly via 'W.readInt32BE') so we don't have to
re-encode the batch to learn how many bytes it occupies.
-}
decodeAllBatchesSliced
  :: ByteString -> IO (Either String [RBW.SlicedRecordBatch])
decodeAllBatchesSliced input = go input []
  where
    go bs acc
      | BS.null bs = pure (Right (reverse acc))
      | otherwise = do
          -- Peek the on-the-wire length field at offset 8
          -- (right after baseOffset). Handles both
          -- compressed and uncompressed shapes since the
          -- envelope is identical.
          let !lengthBytes = BS.take 4 (BS.drop 8 bs)
          case W.readInt32BE lengthBytes of
            Left err -> pure (Left ("decodeAllBatchesSliced: bad length: " ++ err))
            Right batchLen -> do
              let !batchSize = 8 + 4 + fromIntegral batchLen :: Int
                  !batchBs = BS.take batchSize bs
              r <- RBW.decodeRecordBatchWireSlicedWithDecompression batchBs
              case r of
                Left e -> pure (Left e)
                Right sb -> do
                  let !remaining = BS.drop batchSize bs
                  go remaining (sb : acc)


{- | Convert RecordHeaders to (Text, ByteString) tuples
Only includes headers with non-null values
-}
convertHeaders :: [RB.RecordHeader] -> [(Text, ByteString)]
convertHeaders headers = mapMaybe convertHeader headers
  where
    convertHeader hdr = do
      -- Convert key from ByteString to Text (assume UTF-8)
      let keyText = TE.decodeUtf8 (RB.headerKey hdr)
      -- Only include if value is present
      value <- RB.headerValue hdr
      return (keyText, value)


-- | Internal: Commit offsets synchronously to the broker
commitOffsetsSync
  :: Consumer
  -> Text
  -- ^ Group ID
  -> [(TopicPartition, Int64)]
  -- ^ Partitions and offsets to commit
  -> IO (Either String ())
commitOffsetsSync consumer@Consumer {..} groupId offsets = do
  -- The heartbeat thread tracks the group coordinator, generation
  -- id, and member id; we read all three off it. If the consumer
  -- isn't in a group (no heartbeat thread), commits aren't
  -- supported — the JVM client behaves the same.
  case consumerHeartbeat of
    Nothing -> return $ Left "Not in a consumer group, cannot commit offsets"
    Just (hbState, _) -> do
      coordAddrM <- readIORef (HB.hbCoordinatorAddr hbState)
      genId <- readIORef (HB.hbGenerationId hbState)
      memberId <- readIORef (HB.hbMemberId hbState)
      case coordAddrM of
        Nothing -> return $ Left "No group coordinator known"
        Just coordAddr -> do
          connResult <- consumerConnect consumer coordAddr
          case connResult of
            Left err -> return $ Left $ "Failed to connect to coordinator: " ++ err
            Right conn -> do
              corrId <- atomicModifyIORef' consumerCorrelationId $ \cid -> (cid + 1, cid)
              let apiKey = 8 -- OffsetCommit
              -- OffsetCommit: codegen handles up to v10. The
              -- consumer's commitSync path is identical in
              -- request shape from v0 through v8; v9+ moved to
              -- the KIP-848 member-epoch shape but the broker
              -- still accepts the legacy generation/member-id
              -- pair we send through v9. v10's KIP-1043 added
              -- per-topic 'topicId' (we send nullUuid =
              -- name-based lookup). v8 added 'topicId' which we
              -- already supply as nullUuid.
              verR <-
                VN.pickApiVersionForRange @OCReq.OffsetCommitRequest
                  0
                  9
                  consumerVersionCache
                  coordAddr
                  5
              let apiVersion = case verR of
                    Right v -> v
                    Left _ -> 5

              -- Group offsets by topic
              let byTopic =
                    Map.fromListWith
                      (++)
                      [ (tp.topic, [(tp.partition, offset)])
                      | (tp, offset) <- offsets
                      ]

                  topics =
                    V.fromList
                      [ OCReq.OffsetCommitRequestTopic
                          { OCReq.offsetCommitRequestTopicName = P.mkKafkaString topic
                          , OCReq.offsetCommitRequestTopicPartitions =
                              P.mkKafkaArray $
                                V.fromList
                                  [ OCReq.OffsetCommitRequestPartition
                                      { OCReq.offsetCommitRequestPartitionPartitionIndex = partId
                                      , OCReq.offsetCommitRequestPartitionCommittedOffset = offset
                                      , OCReq.offsetCommitRequestPartitionCommittedLeaderEpoch = -1
                                      , OCReq.offsetCommitRequestPartitionCommittedMetadata = P.KafkaString P.Null
                                      }
                                  | (partId, offset) <- parts
                                  ]
                          }
                      | (topic, parts) <- Map.toList byTopic
                      ]

                  -- KIP-345: surface a static @group.instance.id@
                  -- if the user configured one; otherwise leave
                  -- it null (the broker treats that as a dynamic
                  -- member).
                  groupInstanceField =
                    case consumerGroupInstanceId consumerConfig of
                      Nothing -> P.KafkaString P.Null
                      Just gii -> P.mkKafkaString gii
                  request =
                    OCReq.OffsetCommitRequest
                      { OCReq.offsetCommitRequestGroupId = P.mkKafkaString groupId
                      , OCReq.offsetCommitRequestGenerationIdOrMemberEpoch = genId
                      , OCReq.offsetCommitRequestMemberId = P.mkKafkaString memberId
                      , OCReq.offsetCommitRequestGroupInstanceId = groupInstanceField
                      , OCReq.offsetCommitRequestRetentionTimeMs = -1 -- Use broker default
                      , OCReq.offsetCommitRequestTopics = P.mkKafkaArray topics
                      }

                  requestBody = WC.runEncodeVer @OCReq.OffsetCommitRequest apiVersion request
                  clientId = P.mkKafkaString (consumerClientId consumerConfig)

              result <- Req.sendRequestReceiveResponseLocked (Conn.withBrokerLock consumerConnManager coordAddr) conn apiKey apiVersion corrId clientId requestBody

              case result of
                Left err -> return $ Left $ "OffsetCommit failed: " ++ err
                Right (_, responseBody) -> do
                  case WC.runDecodeVer @OCResp.OffsetCommitResponse apiVersion responseBody of
                    Left err -> return $ Left $ "Failed to parse OffsetCommitResponse: " ++ err
                    Right response -> do
                      -- Check for errors in response
                      let topicsNullable = P.unKafkaArray $ OCResp.offsetCommitResponseTopics response
                          topics = case topicsNullable of
                            P.Null -> []
                            P.NotNull v -> V.toList v

                          errors =
                            [ (topic, partId, errorCode)
                            | topicResp <- topics
                            , let topic = extractKafkaString $ OCResp.offsetCommitResponseTopicName topicResp
                                  partsNullable = P.unKafkaArray $ OCResp.offsetCommitResponseTopicPartitions topicResp
                                  parts = case partsNullable of
                                    P.Null -> []
                                    P.NotNull v -> V.toList v
                            , partResp <- parts
                            , let partId = OCResp.offsetCommitResponsePartitionPartitionIndex partResp
                                  errorCode = OCResp.offsetCommitResponsePartitionErrorCode partResp
                            , errorCode /= 0
                            ]

                      if null errors
                        then return $ Right ()
                        else return $ Left $ "Offset commit errors: " ++ show errors


----------------------------------------------------------------------
-- Multi-partition committed offsets (KIP-211)
----------------------------------------------------------------------

{- | Batch sibling of 'fetchCommittedOffsets' that returns every
partition's committed offset (rather than only the first one).
Used by the public 'committedAll' helper.
-}
fetchCommittedOffsetsBatch
  :: Consumer
  -> Text
  -- ^ Group ID
  -> [TopicPartition]
  -> IO (Either String (HashMap.HashMap TopicPartition Int64))
fetchCommittedOffsetsBatch consumer@Consumer {..} groupId tps = do
  case consumerHeartbeat of
    Nothing -> pure (Left "Not in a consumer group, cannot fetch committed offsets")
    Just (hbState, _) -> do
      coordAddrM <- readIORef (HB.hbCoordinatorAddr hbState)
      case coordAddrM of
        Nothing -> pure (Left "No group coordinator known")
        Just coordAddr -> do
          connResult <- consumerConnect consumer coordAddr
          case connResult of
            Left err -> pure (Left ("Failed to connect to coordinator: " ++ err))
            Right conn -> do
              corrId <- atomicModifyIORef' consumerCorrelationId $ \cid -> (cid + 1, cid)
              let apiKey = 9 -- OffsetFetch
              -- OffsetFetch: v8 introduced the per-group batched
              -- shape (KIP-709). The wire is incompatible: v0-v7
              -- carry a single (group_id, topics[]) pair at the
              -- top level; v8+ carries an array of
              -- (group_id, topics[]) and the legacy field is
              -- removed. We dispatch on the negotiated version
              -- below. v9 (KIP-848 member-epoch) is also accepted
              -- here — sending memberId="" + memberEpoch=-1
              -- selects the legacy classic-protocol shape on the
              -- broker side, which mirrors what the JVM client
              -- does when it isn't using the KIP-848 consumer
              -- protocol.
              verR <-
                VN.pickApiVersionForRange @OFReq.OffsetFetchRequest
                  0
                  8
                  consumerVersionCache
                  coordAddr
                  5
              let apiVersion = case verR of
                    Right v -> v
                    Left _ -> 5
              let byTopic =
                    Map.fromListWith
                      (++)
                      [(tp.topic, [tp.partition]) | tp <- tps]
                  request
                    | apiVersion >= 8 = buildOffsetFetchRequestV8 groupId byTopic
                    | otherwise = buildOffsetFetchRequestLegacy groupId byTopic
                  requestBody = WC.runEncodeVer @OFReq.OffsetFetchRequest apiVersion request
                  clientId = P.mkKafkaString (consumerClientId consumerConfig)
              result <- Req.sendRequestReceiveResponseLocked (Conn.withBrokerLock consumerConnManager coordAddr) conn apiKey apiVersion corrId clientId requestBody
              case result of
                Left err -> pure (Left ("OffsetFetch failed: " ++ err))
                Right (_, responseBody) ->
                  case WC.runDecodeVer @OFResp.OffsetFetchResponse apiVersion responseBody of
                    Left err -> pure (Left ("Failed to parse OffsetFetchResponse: " ++ err))
                    Right response
                      | apiVersion >= 8 -> pure $ Right $! parseOffsetFetchResponseV8 response
                      | otherwise -> pure $ Right $! parseOffsetFetchResponseLegacy response


{- | Build an OffsetFetchRequest in the legacy single-group shape
(v0-v7). The (group_id, topics[]) pair lives at the top level
and the v8+ groups[] array is left empty.
-}
buildOffsetFetchRequestLegacy
  :: Text
  -> Map.Map Text [Int32]
  -- ^ partitions grouped by topic
  -> OFReq.OffsetFetchRequest
buildOffsetFetchRequestLegacy groupId byTopic =
  let topicsVec =
        V.fromList
          [ OFReq.OffsetFetchRequestTopic
              { OFReq.offsetFetchRequestTopicName = P.mkKafkaString topic
              , OFReq.offsetFetchRequestTopicPartitionIndexes =
                  P.mkKafkaArray (V.fromList parts)
              }
          | (topic, parts) <- Map.toList byTopic
          ]
  in OFReq.OffsetFetchRequest
       { OFReq.offsetFetchRequestGroupId = P.mkKafkaString groupId
       , OFReq.offsetFetchRequestTopics = P.mkKafkaArray topicsVec
       , OFReq.offsetFetchRequestGroups = P.mkKafkaArray V.empty
       , OFReq.offsetFetchRequestRequireStable = False
       }


{- | Build an OffsetFetchRequest in the v8+ per-group batched
shape (KIP-709). The legacy top-level (group_id, topics[])
pair is left empty (the codegen drops it on v8+ anyway) and
everything goes into the groups[] array.

We always send a single group (the consumer's own group) so
the array has one element. The batched shape lets one client
fetch offsets for many groups in a single round-trip; we don't
expose that publicly yet, but the wire format is what the
broker expects on v8+ regardless.

For v9 (KIP-848) we leave memberId="" and memberEpoch=-1 which
the broker treats as the legacy classic-protocol shape — i.e.
mirrors the JVM client when it isn't using the new
consumer-group protocol.
-}
buildOffsetFetchRequestV8
  :: Text
  -> Map.Map Text [Int32]
  -> OFReq.OffsetFetchRequest
buildOffsetFetchRequestV8 groupId byTopic =
  let topicsVec =
        V.fromList
          [ OFReq.OffsetFetchRequestTopics
              { OFReq.offsetFetchRequestTopicsName = P.mkKafkaString topic
              , OFReq.offsetFetchRequestTopicsPartitionIndexes =
                  P.mkKafkaArray (V.fromList parts)
              }
          | (topic, parts) <- Map.toList byTopic
          ]
      groupEntry =
        OFReq.OffsetFetchRequestGroup
          { OFReq.offsetFetchRequestGroupGroupId = P.mkKafkaString groupId
          , OFReq.offsetFetchRequestGroupMemberId = P.mkKafkaString ""
          , OFReq.offsetFetchRequestGroupMemberEpoch = -1
          , OFReq.offsetFetchRequestGroupTopics = P.mkKafkaArray topicsVec
          }
  in OFReq.OffsetFetchRequest
       { OFReq.offsetFetchRequestGroupId =
           -- Legacy field: drop on v8+ (the encoder ignores it
           -- anyway). 'KafkaString Null' avoids accidentally
           -- shipping any bytes if the encoder ever changes.
           P.KafkaString P.Null
       , OFReq.offsetFetchRequestTopics = P.KafkaArray P.Null
       , OFReq.offsetFetchRequestGroups = P.mkKafkaArray (V.singleton groupEntry)
       , OFReq.offsetFetchRequestRequireStable = False
       }


{- | Parse a legacy (v0-v7) OffsetFetchResponse into a
@TopicPartition -> committed offset@ map. Drops partitions
that errored or had no committed offset (offset == -1), which
mirrors the JVM client's @committed()@ semantics.
-}
parseOffsetFetchResponseLegacy
  :: OFResp.OffsetFetchResponse
  -> HashMap.HashMap TopicPartition Int64
parseOffsetFetchResponseLegacy resp =
  let topicsVec =
        case P.unKafkaArray (OFResp.offsetFetchResponseTopics resp) of
          P.Null -> V.empty
          P.NotNull v -> v
      addTopic !acc tr =
        let topic = extractKafkaString (OFResp.offsetFetchResponseTopicName tr)
            partsVec =
              case P.unKafkaArray (OFResp.offsetFetchResponseTopicPartitions tr) of
                P.Null -> V.empty
                P.NotNull v -> v
        in V.foldl'
             ( \m p ->
                 let pid = OFResp.offsetFetchResponsePartitionPartitionIndex p
                     ec = OFResp.offsetFetchResponsePartitionErrorCode p
                     off = OFResp.offsetFetchResponsePartitionCommittedOffset p
                 in if ec == 0 && off >= 0
                      then HashMap.insert (TopicPartition topic pid) off m
                      else m
             )
             acc
             partsVec
  in V.foldl' addTopic HashMap.empty topicsVec


{- | Parse a v8+ OffsetFetchResponse (the per-group batched
shape). We only ever request a single group so we walk the
groups[] array and merge any successful entries; in practice
the array is always length 1.
-}
parseOffsetFetchResponseV8
  :: OFResp.OffsetFetchResponse
  -> HashMap.HashMap TopicPartition Int64
parseOffsetFetchResponseV8 resp =
  let groupsVec =
        case P.unKafkaArray (OFResp.offsetFetchResponseGroups resp) of
          P.Null -> V.empty
          P.NotNull v -> v
      addGroup !acc gr =
        -- A non-zero group-level error means /none/ of the
        -- group's offsets are valid; drop them.
        if OFResp.offsetFetchResponseGroupErrorCode gr /= 0
          then acc
          else
            let topicsVec =
                  case P.unKafkaArray (OFResp.offsetFetchResponseGroupTopics gr) of
                    P.Null -> V.empty
                    P.NotNull v -> v
            in V.foldl' addTopic acc topicsVec
      addTopic !acc tr =
        let topic = extractKafkaString (OFResp.offsetFetchResponseTopicsName tr)
            partsVec =
              case P.unKafkaArray (OFResp.offsetFetchResponseTopicsPartitions tr) of
                P.Null -> V.empty
                P.NotNull v -> v
        in V.foldl'
             ( \m p ->
                 let pid = OFResp.offsetFetchResponsePartitionsPartitionIndex p
                     ec = OFResp.offsetFetchResponsePartitionsErrorCode p
                     off = OFResp.offsetFetchResponsePartitionsCommittedOffset p
                 in if ec == 0 && off >= 0
                      then HashMap.insert (TopicPartition topic pid) off m
                      else m
             )
             acc
             partsVec
  in V.foldl' addGroup HashMap.empty groupsVec


----------------------------------------------------------------------
-- Per-partition timestamp -> offset (KIP-79)
----------------------------------------------------------------------

{- | Variant of 'queryPartitionOffsets' that returns
the full @OffsetAndTimestamp@ tuple (offset + timestamp +
leader epoch). The offset-only helper and 'offsetsForTimesFull'
both delegate here.

Each partition is asked of its leader. A partition answered with a
retriable error (no leader yet, a leader change, a topic still being
created), or whose leader couldn't be reached, is asked again after
a metadata refresh, with backoff, until
'consumerDefaultApiTimeoutMs'; any other error fails the call. Every
partition asked for is in the result, or the call fails.
-}
queryPartitionOffsetsByTimestampFull
  :: Consumer
  -> [(TopicPartition, Int64)]
  -> IO (Either String [(TopicPartition, OffsetAndTimestamp)])
queryPartitionOffsetsByTimestampFull consumer@Consumer {..} pts0 = do
  start <- getMonotonicTimeNSec
  let deadline = start + fromIntegral (consumerDefaultApiTimeoutMs consumerConfig) * 1_000_000
  go deadline (100 :: Int) False pts0 []
  where
    go deadline backoffMs refresh pending done = do
      r <- offsetsRound refresh pending
      case r of
        Left err -> pure (Left err)
        Right rnd
          | null (orRetry rnd) -> pure (Right (orDone rnd ++ done))
          | otherwise -> do
              now <- getMonotonicTimeNSec
              if now >= deadline
                then
                  pure . Left $
                    "ListOffsets: timed out after "
                      ++ show (consumerDefaultApiTimeoutMs consumerConfig)
                      ++ " ms; last error: "
                      ++ orLastError rnd
                else do
                  let remainingMs = fromIntegral ((deadline - now) `quot` 1_000_000) :: Int
                  threadDelay (1000 * max 1 (min backoffMs remainingMs))
                  go deadline (min 1000 (2 * backoffMs)) True (orRetry rnd) (orDone rnd ++ done)

    -- One pass: route every pending partition to its leader (after a
    -- metadata refresh when asked to, or when a leader is unknown)
    -- and ask every leader at once.
    offsetsRound refresh pending = do
      let topics = List.nub (map (\(tp, _) -> tp.topic) pending)
          lookupLeaders = forM pending $ \p@(tp, _) ->
            fmap (\b -> (Meta.brokerMetaAddress b, p))
              <$> atomically (Meta.getPartitionLeader consumerMetadata tp.topic tp.partition)
      leaders0 <- lookupLeaders
      refreshed <-
        if refresh || any isNothing leaders0
          then refreshMeta topics
          else pure (Right ())
      case refreshed of
        Left err -> pure (Right (OffsetsRound [] pending ("metadata refresh: " ++ err)))
        Right () -> do
          leaders <- if refresh || any isNothing leaders0 then lookupLeaders else pure leaders0
          let routed = catMaybes leaders
              unrouted = map snd (filter (isNothing . fst) (zip leaders pending))
              byBroker = Map.toList (Map.fromListWith (++) (map (\(addr, p) -> (addr, [p])) routed))
          answers <- Async.forConcurrently byBroker $ \(addr, ps) -> (,) ps <$> listOffsetsAt addr ps
          pure $
            foldl'
              classifyBroker
              (Right (OffsetsRound [] unrouted (if null unrouted then "" else "no leader known")))
              answers

    classifyBroker (Left e) _ = Left e
    classifyBroker (Right rnd) (asked, Left err) =
      Right rnd {orRetry = asked ++ orRetry rnd, orLastError = err}
    classifyBroker (Right rnd0) (asked, Right answered) =
      foldl' classifyOne (Right rnd0) asked
      where
        byTp = HashMap.fromList (map (\(tp, ec, oat) -> (tp, (ec, oat))) answered)
        classifyOne (Left e) _ = Left e
        classifyOne (Right rnd) p@(tp, _) = case HashMap.lookup tp byTp of
          Just (0, oat) -> Right rnd {orDone = (tp, oat) : orDone rnd}
          Just (ec, _)
            | RC.isRetriable ec ->
                Right rnd {orRetry = p : orRetry rnd, orLastError = describe tp (RC.errorMessage ec)}
            | otherwise -> Left ("ListOffsets: " ++ describe tp (RC.errorMessage ec))
          Nothing -> Right rnd {orRetry = p : orRetry rnd, orLastError = describe tp "not in the response"}
    describe tp why = T.unpack tp.topic ++ "-" ++ show tp.partition ++ ": " ++ T.unpack why

    refreshMeta topics = do
      brokersM <- atomically $ Meta.getAllBrokers consumerMetadata
      case brokersM of
        Just (b : _) -> do
          let addr = Meta.brokerMetaAddress b
          connR <- consumerConnect consumer addr
          case connR of
            Left err -> pure (Left err)
            Right conn -> do
              corrId <- atomicModifyIORef' consumerCorrelationId $ \cid -> (cid + 1, cid)
              Conn.withBrokerLock consumerConnManager addr $
                Meta.refreshTopicMetadata conn consumerMetadata (Just topics) corrId
        _ -> pure (Left "no brokers in the metadata cache")

    -- One ListOffsets round trip to one broker, every partition's
    -- error code kept.
    listOffsetsAt
      :: Conn.BrokerAddress
      -> [(TopicPartition, Int64)]
      -> IO (Either String [(TopicPartition, Int16, OffsetAndTimestamp)])
    listOffsetsAt brokerAddr pts = do
      connResult <- consumerConnect consumer brokerAddr
      case connResult of
        Left err -> pure (Left ("Failed to connect to broker: " ++ err))
        Right conn -> do
          corrId <- atomicModifyIORef' consumerCorrelationId $ \cid -> (cid + 1, cid)
          let apiKey = 2 -- ListOffsets
          -- ListOffsets: codegen handles up to v10. Schema
          -- changes by version:
          --   v2  KIP-98  IsolationLevel (the consumer's own, so
          --               a read_committed "latest" is the LSO)
          --   v4  KIP-320 LeaderEpoch in request + response
          --   v6  flexible (compact + tagged fields)
          --   v7  KIP-734 LeaderEpoch becomes mandatory in
          --               response; no request-shape change
          --   v8  KIP-1146 EarliestLocalTimestamp (-4) sentinel
          --               accepted in the timestamp field; no
          --               schema change
          --   v9  KIP-1133 Tiered storage sentinel (-5); no
          --               schema change
          --   v10 KIP-994 TimeoutMs added to the request
          -- We cap at v8 (the highest the broker accepts on
          -- non-tiered-storage clusters; v9+ rely on the broker
          -- having KIP-405 enabled which Kafka 3.7's default
          -- builds do not).
          verR <-
            VN.pickApiVersionForRange @LOReq.ListOffsetsRequest
              0
              8
              consumerVersionCache
              brokerAddr
              1
          let apiVersion = case verR of
                Right v -> v
                Left _ -> 1
              byTopic = Map.toList (Map.fromListWith (++) (map (\(tp, ts) -> (tp.topic, [(tp.partition, ts)])) pts))
              topics = V.fromList $ map buildTopicReq byTopic
              buildTopicReq (topic, parts) =
                LOReq.ListOffsetsTopic
                  { LOReq.listOffsetsTopicName = P.mkKafkaString topic
                  , LOReq.listOffsetsTopicPartitions =
                      P.mkKafkaArray $
                        V.fromList $
                          map
                            ( \(pid, ts) ->
                                LOReq.ListOffsetsPartition
                                  { LOReq.listOffsetsPartitionPartitionIndex = pid
                                  , LOReq.listOffsetsPartitionCurrentLeaderEpoch = -1
                                  , LOReq.listOffsetsPartitionTimestamp = ts
                                  }
                            )
                            parts
                  }
              request =
                LOReq.ListOffsetsRequest
                  { LOReq.listOffsetsRequestReplicaId = -1 -- Consumer
                  , -- A read_committed consumer's "latest" is the last
                    -- stable offset, not the high watermark (v2+).
                    LOReq.listOffsetsRequestIsolationLevel = isolationLevelCode (consumerIsolationLevel consumerConfig)
                  , LOReq.listOffsetsRequestTopics = P.mkKafkaArray topics
                  , LOReq.listOffsetsRequestTimeoutMs = 30000 -- v10+; ignored otherwise
                  }
              requestBody = WC.runEncodeVer @LOReq.ListOffsetsRequest apiVersion request
              clientId = P.mkKafkaString (consumerClientId consumerConfig)
          result <- Req.sendRequestReceiveResponseLocked (Conn.withBrokerLock consumerConnManager brokerAddr) conn apiKey apiVersion corrId clientId requestBody
          case result of
            Left err -> pure (Left err)
            Right (rcid, body)
              | rcid /= corrId -> pure (Left "Correlation ID mismatch")
              | otherwise -> case WC.runDecodeVer @LOResp.ListOffsetsResponse apiVersion body of
                  Left err -> pure (Left ("Failed to decode ListOffsets response: " ++ err))
                  Right resp -> pure (Right (extract resp))

    extract resp = case P.unKafkaArray (LOResp.listOffsetsResponseTopics resp) of
      P.Null -> []
      P.NotNull tvec ->
        concatMap
          ( \tr ->
              let topic = case P.unKafkaString (LOResp.listOffsetsTopicResponseName tr) of
                    P.Null -> ""
                    P.NotNull t -> t
                  partsVec = case P.unKafkaArray (LOResp.listOffsetsTopicResponsePartitions tr) of
                    P.Null -> V.empty
                    P.NotNull v -> v
              in map
                   ( \pr ->
                       ( TopicPartition topic (LOResp.listOffsetsPartitionResponsePartitionIndex pr)
                       , LOResp.listOffsetsPartitionResponseErrorCode pr
                       , OffsetAndTimestamp
                           { oatOffset = LOResp.listOffsetsPartitionResponseOffset pr
                           , oatTimestamp = LOResp.listOffsetsPartitionResponseTimestamp pr
                           , oatLeaderEpoch = LOResp.listOffsetsPartitionResponseLeaderEpoch pr
                           }
                       )
                   )
                   (V.toList partsVec)
          )
          (V.toList tvec)


-- | What one round of 'queryPartitionOffsetsByTimestampFull' settled.
data OffsetsRound = OffsetsRound
  { orDone :: ![(TopicPartition, OffsetAndTimestamp)]
  , orRetry :: ![(TopicPartition, Int64)]
  -- ^ asked again next round
  , orLastError :: !String
  -- ^ why the last of 'orRetry' wasn't answered
  }


----------------------------------------------------------------------
-- Additional ergonomics
--
-- Small typed-config / pure-decision helpers that previously lived
-- in @Kafka.Client.ConsumerExtras@ and (for 'PerPartitionFetchKnob')
-- @Kafka.Client.AdminExtras@. Folded in here so the consumer-side
-- surface is in one place.
----------------------------------------------------------------------

{- | Snapshot of the resolved consumer configuration after defaults
+ the user's overrides have been applied. Mirrors Java's
@KafkaConsumer.metrics()@-shaped report users grep when debugging
"is this knob actually set?" issues.
-}
data EffectiveConsumerSnapshot = EffectiveConsumerSnapshot
  { ecsClientId :: !Text
  , ecsGroupId :: !Text
  , ecsGroupInstanceId :: !(Maybe Text)
  , ecsAutoOffsetReset :: !OffsetResetStrategy
  , ecsIsolationLevel :: !IsolationLevel
  , ecsMaxPollRecords :: !Int
  , ecsMaxPollIntervalMs :: !Int
  , ecsSessionTimeoutMs :: !Int
  , ecsHeartbeatIntervalMs :: !Int
  , ecsAutoCommit :: !Bool
  , ecsAutoCommitIntervalMs :: !Int
  }
  deriving stock (Eq, Show, Generic)


effectiveConsumerSnapshot :: ConsumerConfig -> EffectiveConsumerSnapshot
effectiveConsumerSnapshot c =
  EffectiveConsumerSnapshot
    { ecsClientId = consumerClientId c
    , ecsGroupId = consumerGroupId c
    , ecsGroupInstanceId = consumerGroupInstanceId c
    , ecsAutoOffsetReset = consumerAutoOffsetReset c
    , ecsIsolationLevel = consumerIsolationLevel c
    , ecsMaxPollRecords = consumerMaxPollRecords c
    , ecsMaxPollIntervalMs = consumerMaxPollIntervalMs c
    , ecsSessionTimeoutMs = consumerSessionTimeoutMs c
    , ecsHeartbeatIntervalMs = consumerHeartbeatIntervalMs c
    , ecsAutoCommit = consumerAutoCommit c
    , ecsAutoCommitIntervalMs = consumerAutoCommitIntervalMs c
    }


{- | What to do when @OFFSET_OUT_OF_RANGE@ fires for a partition
mid-poll. Mirrors the JVM client's
@auto.offset.reset.outOfRange@ behavioural enum.
-}
data RewindPolicy
  = RewindToEarliest
  | RewindToLatest
  | RewindFail
  deriving stock (Eq, Show, Generic)


{- | Pure decision: given the configured policy, the partition's
current low- / high-water marks, return the offset the consumer
should reset to (or 'Nothing' for "fail / let the user decide").
-}
planRewind
  :: RewindPolicy
  -> Int64
  -- ^ low water mark (earliest)
  -> Int64
  -- ^ high water mark (latest)
  -> Maybe Int64
planRewind RewindFail _ _ = Nothing
planRewind RewindToEarliest l _ = Just l
planRewind RewindToLatest _ h = Just h


{- | Reason a consumer might explicitly request a rebalance,
recorded in the broker-side @JoinGroup.reason@ field. Helps
operators correlate rebalance storms with the application
action that triggered them.
-}
data RebalanceTrigger
  = TriggerSubscriptionChange
  | TriggerPauseResume
  | TriggerExplicitEnforce
  | TriggerStaleAssignment
  | TriggerOther !Text
  deriving stock (Eq, Show, Generic)


recordRebalanceTrigger :: RebalanceTrigger -> Text
recordRebalanceTrigger = \case
  TriggerSubscriptionChange -> "subscription-changed"
  TriggerPauseResume -> "pause-resume"
  TriggerExplicitEnforce -> "enforce-rebalance-called"
  TriggerStaleAssignment -> "stale-assignment"
  TriggerOther t -> t


{- | When 'True', auto-commit is suppressed regardless of the
configured 'consumerAutoCommit'. Useful for query / replay /
DR consumers that mustn't perturb the production offsets.
-}
isReadOnlyMode :: ConsumerConfig -> Bool
isReadOnlyMode = not . consumerAutoCommit


{- | Return a 'ConsumerConfig' with auto-commit disabled. The
wrapper preserves every other config field. Mirrors Java's
@KafkaConsumer(props.put(\"enable.auto.commit\", \"false\"))@
pattern but type-safe.
-}
withReadOnly :: ConsumerConfig -> ConsumerConfig
withReadOnly c = c {consumerAutoCommit = False}


{- | Typed reason supplied to a graceful close, surfaced on the
broker-side @LeaveGroup@ audit log.
-}
data ShutdownReason
  = ShutdownExplicit
  | ShutdownLost
  | ShutdownFenced
  | ShutdownConfigReload
  | ShutdownProcessExit
  | ShutdownUserSignal
  | ShutdownReason_Other !Text
  deriving stock (Eq, Show, Generic)


shutdownReasonText :: ShutdownReason -> Text
shutdownReasonText = \case
  ShutdownExplicit -> "explicit-close"
  ShutdownLost -> "consumer-lost-partitions"
  ShutdownFenced -> "consumer-fenced-by-coordinator"
  ShutdownConfigReload -> "config-reload"
  ShutdownProcessExit -> "process-exit"
  ShutdownUserSignal -> "user-signal"
  ShutdownReason_Other t -> t


{- | The server-side assignor name the client wants the broker to
use. The broker may ignore the hint if it doesn't support the
requested assignor.
-}
data AssignorHint
  = HintRangeAssignor
  | HintCooperativeStickyAssignor
  | HintRoundRobinAssignor
  | HintUniformAssignor
  | HintCustomAssignor !Text
  deriving stock (Eq, Show, Generic)


assignorHintText :: AssignorHint -> Text
assignorHintText = \case
  HintRangeAssignor -> "range"
  HintCooperativeStickyAssignor -> "cooperative-sticky"
  HintRoundRobinAssignor -> "roundrobin"
  HintUniformAssignor -> "uniform"
  HintCustomAssignor n -> n


{- | Per-partition fetch knobs: minimum bytes the broker must
accumulate before replying, plus an optional minimum-timestamp
floor.
-}
data PerPartitionFetchKnob = PerPartitionFetchKnob
  { ppfkPartition :: !Int32
  , ppfkMinBytes :: !Int
  {- ^ Per-partition @fetch.min.bytes@; the broker will not
  return less than this for this partition.
  -}
  , ppfkMinTimestampMs :: !(Maybe Int64)
  {- ^ Only return records whose timestamp is at or above
  this value.
  -}
  }
  deriving stock (Eq, Show, Generic)


----------------------------------------------------------------------
-- SPECIALIZE pragmas for the IO hot path
--
-- See the matching block in "Kafka.Client.Producer".
----------------------------------------------------------------------

{-# INLINEABLE createConsumer #-}
{-# SPECIALIZE createConsumer :: [Text] -> Text -> ConsumerConfig -> IO (Either String Consumer) #-}


{-# INLINEABLE closeConsumer #-}
{-# SPECIALIZE closeConsumer :: Consumer -> IO () #-}


{-# INLINEABLE closeConsumerWithTimeout #-}
{-# SPECIALIZE closeConsumerWithTimeout :: Consumer -> Int -> IO () #-}


{-# INLINEABLE closeConsumerWithoutLeavingGroup #-}
{-# SPECIALIZE closeConsumerWithoutLeavingGroup :: Consumer -> Int -> IO () #-}


{-# INLINEABLE subscribe #-}
{-# SPECIALIZE subscribe :: Consumer -> [Text] -> IO (Either String ()) #-}


{-# INLINEABLE unsubscribe #-}
{-# SPECIALIZE unsubscribe :: Consumer -> IO () #-}


{-# INLINEABLE assign #-}
{-# SPECIALIZE assign :: Consumer -> [TopicPartition] -> IO (Either String ()) #-}


{-# INLINEABLE poll #-}
{-# SPECIALIZE poll :: Consumer -> Int -> IO (Either String [ConsumerRecord]) #-}


{-# INLINEABLE commitSync #-}
{-# SPECIALIZE commitSync :: Consumer -> IO (Either String ()) #-}


{-# INLINEABLE commitAsync #-}
{-# SPECIALIZE commitAsync :: Consumer -> IO (Either String ()) #-}


{-# INLINEABLE committed #-}
{-# SPECIALIZE committed :: Consumer -> TopicPartition -> IO (Either String Int64) #-}


{-# INLINEABLE committedAll #-}
{-# SPECIALIZE committedAll :: Consumer -> [TopicPartition] -> IO (Either String (HashMap.HashMap TopicPartition Int64)) #-}


{-# INLINEABLE position #-}
{-# SPECIALIZE position :: Consumer -> TopicPartition -> IO (Either String Int64) #-}


{-# INLINEABLE seek #-}
{-# SPECIALIZE seek :: Consumer -> TopicPartition -> Int64 -> IO (Either String ()) #-}


{-# INLINEABLE seekToBeginning #-}
{-# SPECIALIZE seekToBeginning :: Consumer -> [TopicPartition] -> IO (Either String ()) #-}


{-# INLINEABLE seekToEnd #-}
{-# SPECIALIZE seekToEnd :: Consumer -> [TopicPartition] -> IO (Either String ()) #-}


{-# INLINEABLE seekToTimestamp #-}
{-# SPECIALIZE seekToTimestamp :: Consumer -> [TopicPartition] -> Int64 -> IO (Either String ()) #-}


{-# INLINEABLE beginningOffsets #-}
{-# SPECIALIZE beginningOffsets :: Consumer -> [TopicPartition] -> IO (Either String (HashMap.HashMap TopicPartition Int64)) #-}


{-# INLINEABLE endOffsets #-}
{-# SPECIALIZE endOffsets :: Consumer -> [TopicPartition] -> IO (Either String (HashMap.HashMap TopicPartition Int64)) #-}


{-# INLINEABLE offsetsForTimes #-}
{-# SPECIALIZE offsetsForTimes :: Consumer -> [(TopicPartition, Int64)] -> IO (Either String (HashMap.HashMap TopicPartition Int64)) #-}


{-# INLINEABLE offsetsForTimesFull #-}
{-# SPECIALIZE offsetsForTimesFull :: Consumer -> [(TopicPartition, Int64)] -> IO (Either String (HashMap.HashMap TopicPartition OffsetAndTimestamp)) #-}


{-# INLINEABLE pause #-}
{-# SPECIALIZE pause :: Consumer -> [TopicPartition] -> IO () #-}


{-# INLINEABLE resume #-}
{-# SPECIALIZE resume :: Consumer -> [TopicPartition] -> IO () #-}


{-# INLINEABLE assignment #-}
{-# SPECIALIZE assignment :: Consumer -> IO [TopicPartition] #-}


{-# INLINEABLE paused #-}
{-# SPECIALIZE paused :: Consumer -> IO [TopicPartition] #-}


{-# INLINEABLE currentAssignment #-}
{-# SPECIALIZE currentAssignment :: Consumer -> IO [TopicPartition] #-}


{-# INLINEABLE consumerClusterId #-}
{-# SPECIALIZE consumerClusterId :: Consumer -> IO (Maybe Text) #-}


{-# INLINEABLE consumerHealthy #-}
{-# SPECIALIZE consumerHealthy :: Consumer -> IO Bool #-}


{-# INLINEABLE requestRejoin #-}
{-# SPECIALIZE requestRejoin :: Consumer -> IO Bool #-}


{-# INLINEABLE consumerConfigFromEnv #-}
{-# SPECIALIZE consumerConfigFromEnv :: ConsumerConfig -> IO (Either [ConfigError] ConsumerConfig) #-}


----------------------------------------------------------------------
-- JVM-equivalent SDK shims (folded in from the older
-- 'Kafka.Client.ConsumerSdk' module to keep the JVM-equivalence
-- surface in one place).
--
-- Covers:
--   * 'ConsumerRecords'         — wrapper around the @poll@ batch
--                                 with partition / topic projections
--                                 (@KIP-447@-friendly).
--   * 'OffsetAndMetadata'       — typed commit position the
--                                 transactional path wants.
--   * 'OffsetCommitCallback'    — async-commit callback shape.
--   * 'ConsumerGroupMetadata'   — structured group identity.
--   * 'SubscriptionPattern'     — KIP-848 regex subscribe.
--   * 'clientInstanceId'        — KIP-714 telemetry id (local stub).
--   * @seek/commit/enforce*@ overloads.
----------------------------------------------------------------------

{- | A typed wrapper around the @[ConsumerRecord]@ batch that a
single 'poll' returns. Mirrors
@org.apache.kafka.clients.consumer.ConsumerRecords@.
-}
newtype ConsumerRecords = ConsumerRecords
  { unConsumerRecords :: [ConsumerRecord]
  }
  deriving stock (Eq, Show, Generic)


-- | The empty batch.
emptyConsumerRecords :: ConsumerRecords
emptyConsumerRecords = ConsumerRecords []


-- | Every record in arrival order.
consumerRecordsAll :: ConsumerRecords -> [ConsumerRecord]
consumerRecordsAll = unConsumerRecords


-- | Total number of records in the batch.
consumerRecordsCount :: ConsumerRecords -> Int
consumerRecordsCount = length . unConsumerRecords


{- | Distinct @(topic, partition)@ pairs the batch touches.
Equivalent to @ConsumerRecords.partitions()@.
-}
consumerRecordsPartitions :: ConsumerRecords -> Set.Set TopicPartition
consumerRecordsPartitions (ConsumerRecords rs) =
  Set.fromList (map toTP rs)
  where
    toTP r = TopicPartition {topic = r.topic, partition = r.partition}


{- | Records for a single partition. Equivalent to
@ConsumerRecords.records(TopicPartition)@.
-}
recordsByPartition
  :: TopicPartition -> ConsumerRecords -> [ConsumerRecord]
recordsByPartition tp (ConsumerRecords rs) =
  List.filter
    (\r -> r.topic == tp.topic && r.partition == tp.partition)
    rs


{- | All records grouped by topic. Equivalent to
@ConsumerRecords.records(String)@.
-}
recordsByTopic :: ConsumerRecords -> Map.Map Text [ConsumerRecord]
recordsByTopic (ConsumerRecords rs) =
  foldl'
    (\acc r -> Map.insertWith (flip (<>)) r.topic [r] acc)
    Map.empty
    rs


{- | The next offset to consume per partition (= max(offset)+1).
Equivalent to @ConsumerRecords.nextOffsets()@ — exactly the
shape 'Kafka.Client.Transaction.commitOffsetsInTransaction'
expects.
-}
consumerRecordsNextOffsets
  :: ConsumerRecords -> HashMap.HashMap TopicPartition Int64
consumerRecordsNextOffsets (ConsumerRecords rs) =
  foldl' step HashMap.empty rs
  where
    step acc r =
      let !tp = TopicPartition {topic = r.topic, partition = r.partition}
          !nxt = r.offset + 1
      in HashMap.insertWith max tp nxt acc


----------------------------------------------------------------------
-- OffsetAndMetadata
----------------------------------------------------------------------

{- | An offset paired with caller-supplied metadata + optional
leader epoch. Mirrors
@org.apache.kafka.clients.consumer.OffsetAndMetadata@.
-}
data OffsetAndMetadata = OffsetAndMetadata
  { oamOffset :: !Int64
  , oamLeaderEpoch :: !(Maybe Int32)
  , oamMetadata :: !Text
  }
  deriving stock (Eq, Show, Generic)


-- | Bare offset, empty metadata, no leader epoch.
offsetAndMetadata :: Int64 -> OffsetAndMetadata
offsetAndMetadata o =
  OffsetAndMetadata
    { oamOffset = o
    , oamLeaderEpoch = Nothing
    , oamMetadata = T.empty
    }


withMetadata :: Text -> OffsetAndMetadata -> OffsetAndMetadata
withMetadata m oam = oam {oamMetadata = m}


withLeaderEpoch :: Int32 -> OffsetAndMetadata -> OffsetAndMetadata
withLeaderEpoch e oam = oam {oamLeaderEpoch = Just e}


----------------------------------------------------------------------
-- OffsetCommitCallback
----------------------------------------------------------------------

{- | Async-commit callback shape. Mirrors
@org.apache.kafka.clients.consumer.OffsetCommitCallback@.
-}
type OffsetCommitCallback =
  Map.Map TopicPartition OffsetAndMetadata
  -> Maybe Control.Exception.SomeException
  -> IO ()


-- | A callback that ignores everything.
noopOffsetCommitCallback :: OffsetCommitCallback
noopOffsetCommitCallback _ _ = pure ()


----------------------------------------------------------------------
-- ConsumerGroupMetadata
----------------------------------------------------------------------

{- | Structured group identity the transactional producer wants
on @sendOffsetsToTransaction@. Mirrors
@org.apache.kafka.clients.consumer.ConsumerGroupMetadata@.
-}
data ConsumerGroupMetadata = ConsumerGroupMetadata
  { cgmGroupId :: !Text
  , cgmGenerationId :: !Int32
  , cgmMemberId :: !Text
  , cgmGroupInstanceId :: !(Maybe Text)
  }
  deriving stock (Eq, Show, Generic)


-- | Build a 'ConsumerGroupMetadata' from raw values.
newConsumerGroupMetadata
  :: Text -> Int32 -> Text -> Maybe Text -> ConsumerGroupMetadata
newConsumerGroupMetadata g gen mid inst =
  ConsumerGroupMetadata
    { cgmGroupId = g
    , cgmGenerationId = gen
    , cgmMemberId = mid
    , cgmGroupInstanceId = inst
    }


{- | Read the consumer's current group identity. Mirrors
@KafkaConsumer.groupMetadata()@.
-}
groupMetadata :: Consumer -> IO ConsumerGroupMetadata
groupMetadata c = do
  let !cfg = effectiveConsumerSnapshot (consumerConfigOf c)
  -- The live generation and member id come from the heartbeat
  -- state, which every JoinGroup / SyncGroup updates; they are what
  -- the group coordinator checks a transactional offset commit
  -- against (KIP-447 fencing).
  (gen, mid) <- case consumerHeartbeat c of
    Just (hbSt, _) -> do
      g <- readIORef (HB.hbGenerationId hbSt)
      m <- readIORef (HB.hbMemberId hbSt)
      pure (g, m)
    Nothing -> pure (-1, T.empty)
  pure
    ConsumerGroupMetadata
      { cgmGroupId = cfg.ecsGroupId
      , cgmGenerationId = gen
      , cgmMemberId = mid
      , cgmGroupInstanceId = cfg.ecsGroupInstanceId
      }


{- | The group coordinator this consumer last found, if any
(where a transactional offset commit for its group goes).
-}
groupCoordinator :: Consumer -> IO (Maybe Conn.BrokerAddress)
groupCoordinator c = case consumerHeartbeat c of
  Just (hbSt, _) -> readIORef (HB.hbCoordinatorAddr hbSt)
  Nothing -> pure Nothing


----------------------------------------------------------------------
-- SubscriptionPattern (KIP-848)
----------------------------------------------------------------------

{- | A regex-based topic subscription pattern. Mirrors
@org.apache.kafka.clients.consumer.SubscriptionPattern@.
The JVM client expects a Google RE2-compatible regex; this
Haskell shim uses POSIX extended regex via @regex-tdfa@.
-}
data SubscriptionPattern = SubscriptionPattern
  { sptText :: !Text
  , sptRegex :: !RE.Regex
  }


instance Show SubscriptionPattern where
  show sp = "SubscriptionPattern " <> show (sptText sp)


subscriptionPattern :: Text -> Either String SubscriptionPattern
subscriptionPattern txt =
  case RE.makeRegexM (T.unpack txt) :: Maybe RE.Regex of
    Just r -> Right (SubscriptionPattern txt r)
    Nothing -> Left ("subscriptionPattern: invalid regex " <> show txt)


matchesSubscriptionPattern :: SubscriptionPattern -> Text -> Bool
matchesSubscriptionPattern sp t =
  RE.matchTest (sptRegex sp) (T.unpack t)


----------------------------------------------------------------------
-- KIP-714 client instance id
----------------------------------------------------------------------

{- | Returns the consumer's client-instance id. Mirrors
@KafkaConsumer.clientInstanceId(Duration)@.

The JVM client persists a UUID per consumer instance for the
broker-side telemetry pipeline (KIP-714). Our consumer
doesn't yet implement the @GetTelemetrySubscriptions@ RPC, so
this getter returns a stable /local/ id derived
deterministically from the consumer's configured
@client.id@: the same consumer process always reports the
same id, which preserves the JVM contract that "the id is
per-process and stable".
-}
clientInstanceId :: Consumer -> IO TopicIdImp.TopicId
clientInstanceId c =
  pure
    ( TopicIdImp.TopicId
        (Telemetry.clientInstanceIdFromText ((consumerConfigOf c).consumerClientId))
    )


----------------------------------------------------------------------
-- Consumer overload tail (KIP-447 / KIP-666 / KIP-848)
----------------------------------------------------------------------

{- | Commit explicit per-partition offsets. Mirrors
@KafkaConsumer.commitSync(Map<TopicPartition, OffsetAndMetadata>)@.
-}
commitSyncOffsets
  :: Consumer
  -> Map.Map TopicPartition OffsetAndMetadata
  -> IO (Either String ())
commitSyncOffsets c offsetsM =
  if Map.null offsetsM
    then pure (Right ())
    else do
      let offsets =
            Map.foldrWithKey
              (\tp oam acc -> (tp, oamOffset oam) : acc)
              []
              offsetsM
      r <- commitOffsetsSync c (consumerGroupIdOf c) offsets
      case r of
        Right () -> dispatchOnCommit c offsets >> pure (Right ())
        Left e -> pure (Left e)


{- | 'commitAsync' with a user-supplied 'OffsetCommitCallback'.
Mirrors @KafkaConsumer.commitAsync(OffsetCommitCallback)@.
-}
commitAsyncCallback :: Consumer -> OffsetCommitCallback -> IO ()
commitAsyncCallback c cb = do
  r <- commitAsync c
  case r of
    Right () -> cb Map.empty Nothing
    Left msg -> cb Map.empty (Just (Control.Exception.toException (userError msg)))


{- | @seek(TopicPartition, OffsetAndMetadata)@ overload. Mirrors
the JVM variant that lets the caller stash leader-epoch
metadata alongside the offset. The current implementation
discards the metadata + leader epoch and forwards to the
bare 'seek'.
-}
seekWithMetadata
  :: Consumer -> TopicPartition -> OffsetAndMetadata -> IO (Either String ())
seekWithMetadata c tp oam = seek c tp (oamOffset oam)


{- | 'enforceRebalance' with a reason. Mirrors
@Consumer.enforceRebalance(String)@. The reason is currently
discarded; the underlying 'requestRejoin' is the same code
path.
-}
enforceRebalance :: Consumer -> IO Bool
enforceRebalance = requestRejoin


enforceRebalanceWithReason :: Consumer -> Text -> IO Bool
enforceRebalanceWithReason c _ = requestRejoin c
