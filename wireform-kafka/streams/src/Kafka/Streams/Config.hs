{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
Module      : Kafka.Streams.Config
Description : 'StreamsConfig' — top-level runtime configuration

Mirrors the @StreamsConfig@ keys in
@org.apache.kafka.streams.StreamsConfig@. Every field is documented
against the original key name so users porting code can grep across.
-}
module Kafka.Streams.Config (
  -- * Config
  StreamsConfig (..),
  defaultStreamsConfig,
  effectiveCommitIntervalMs,

  -- * Processing semantics
  ProcessingGuarantee (..),

  -- * Common defaults
  defaultCommitIntervalMs,
  defaultPollMs,
  defaultStateDir,

  -- * Properties-style overrides
  streamsConfigFromMap,
  StreamsConfigKey,
) where

import Data.Int (Int64)
import Data.List (foldl')
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import GHC.Generics (Generic)
import Kafka.Client.AdminClient qualified as KA
import Kafka.Client.Consumer qualified as KC
import Kafka.Client.Producer qualified as KP
import Kafka.Streams.Errors (
  DeserializationHandler,
  ProcessingExceptionHandler,
  ProductionHandler,
  logAndFail,
  logAndFailProcessing,
  logAndFailProduction,
 )
import Kafka.Streams.Processor.Assignment (TaskAssignor)


-- | What level of processing-guarantee should the runtime offer?
data ProcessingGuarantee
  = -- | default; possible duplicates on failover
    AtLeastOnceP
  | -- | KIP-447 transactional commits
    ExactlyOnceV2
  deriving stock (Eq, Show, Generic)


{- | Top-level runtime configuration.  Field names mirror the Java
camelCased property keys (with the @StreamsConfig.@ prefix
dropped):

  * 'applicationId'             — @application.id@
  * 'bootstrapServers'          — @bootstrap.servers@
  * 'numStreamThreads'          — @num.stream.threads@
  * 'numStandbyReplicas'        — @num.standby.replicas@
  * 'commitIntervalMs'          — @commit.interval.ms@
  * 'pollMs'                    — @poll.ms@
  * 'maxTaskIdleMs'             — @max.task.idle.ms@
  * 'processingGuarantee'       — @processing.guarantee@
  * 'replicationFactor'         — @replication.factor@
  * 'stateDir'                  — @state.dir@
  * 'defaultDeserHandler'       — @default.deserialization.exception.handler@
  * 'defaultProductionHandler'  — @default.production.exception.handler@
  * 'defaultProcessingHandler'  — @processing.exception.handler@
  * 'bufferedRecordsPerPartition' — @buffered.records.per.partition@

The embedded clients are configured from these; 'consumerOverrides',
'producerOverrides' and 'adminOverrides' adjust anything else on them
(the JVM's @consumer.@ / @producer.@ / @admin.@ prefixed properties).

Not here, because the runtime has no such behaviour: record caches
(every update is forwarded and logged, the JVM's behaviour with
@statestore.cache.max.bytes = 0@), standby replicas
('numStandbyReplicas' is accepted but restoring from the changelog is
the only failover path), rack-aware assignment and
@application.server@ discovery. Their JVM keys are ignored by
'streamsConfigFromMap', like any unknown key.
-}
data StreamsConfig = StreamsConfig
  { applicationId :: !Text
  , bootstrapServers :: ![Text]
  , clientId :: !Text
  , numStreamThreads :: !Int
  , numStandbyReplicas :: !Int
  , commitIntervalMs :: !(Maybe Int)
  {- ^ 'Nothing': 30 s at least once, 100 ms exactly once (the JVM
  defaults); see 'effectiveCommitIntervalMs'.
  -}
  , pollMs :: !Int
  , maxTaskIdleMs :: !Int
  , processingGuarantee :: !ProcessingGuarantee
  , replicationFactor :: !Int
  , stateDir :: !FilePath
  , defaultDeserHandler :: !DeserializationHandler
  -- ^ Default: fail (the JVM's @LogAndFailExceptionHandler@)
  , defaultProductionHandler :: !ProductionHandler
  -- ^ Default: fail. Only consulted at least once; under exactly-once a failed send aborts the transaction.
  , defaultProcessingHandler :: !ProcessingExceptionHandler
  -- ^ Default: fail (KIP-1033's @LogAndFailProcessingExceptionHandler@)
  , bufferedRecordsPerPartition :: !Int
  -- ^ A partition is paused once this many of its records wait unprocessed
  , consumerOverrides :: !(KC.ConsumerConfig -> KC.ConsumerConfig)
  , producerOverrides :: !(KP.ProducerConfig -> KP.ProducerConfig)
  , adminOverrides :: !(KA.AdminClientConfig -> KA.AdminClientConfig)
  , -- KIP-441 probing rebalances ---------------------------------
    acceptableRecoveryLag :: !Int64
  {- ^ @acceptable.recovery.lag@ — maximum changelog lag (in
  records) at which a warmup replica is considered "caught
  up" and may be promoted to active. Default 10_000.
  -}
  , maxWarmupReplicas :: !Int
  {- ^ @max.warmup.replicas@ — total warmup replicas allowed
  across the application instance. Default 2.
  -}
  , probingRebalanceIntervalMs :: !Int
  {- ^ @probing.rebalance.interval.ms@ — KIP-441 cadence for
  re-issuing rebalances to check whether warmups are
  ready. Default 600_000 (10 minutes).
  -}
  , taskAssignor :: !(Maybe TaskAssignor)
  {- ^ KIP-924 in-process 'TaskAssignor' plug-in. When set,
  the streams runtime's leader-side assignment path
  invokes this plug-in instead of the built-in
  cooperative-sticky assignor. 'Nothing' (the default)
  keeps the built-in assignor. The streams runtime
  reaches into this via
  'Kafka.Streams.Runtime.streamsRunUserAssignor', which
  constructs the 'ApplicationState' from the live
  runtime view and invokes 'taAssign' on the plug-in.
  -}
  }


-- | The commit interval in effect ('commitIntervalMs' or its guarantee's default).
effectiveCommitIntervalMs :: StreamsConfig -> Int
effectiveCommitIntervalMs cfg = case (commitIntervalMs cfg, processingGuarantee cfg) of
  (Just ms, _) -> ms
  (Nothing, ExactlyOnceV2) -> 100
  (Nothing, AtLeastOnceP) -> defaultCommitIntervalMs


defaultCommitIntervalMs :: Int
defaultCommitIntervalMs = 30_000


defaultPollMs :: Int
defaultPollMs = 100


defaultStateDir :: FilePath
defaultStateDir = "/tmp/kafka-streams"


{- | Default config matching Kafka Streams 3.x defaults. The caller
must override 'applicationId' and 'bootstrapServers'.
-}
defaultStreamsConfig :: StreamsConfig
defaultStreamsConfig =
  StreamsConfig
    { applicationId = "kafka-streams-app"
    , bootstrapServers = ["localhost:9092"]
    , clientId = "kafka-streams-client"
    , numStreamThreads = 1
    , numStandbyReplicas = 0
    , commitIntervalMs = Nothing
    , pollMs = defaultPollMs
    , maxTaskIdleMs = 0
    , processingGuarantee = AtLeastOnceP
    , replicationFactor = 1
    , stateDir = defaultStateDir
    , defaultDeserHandler = logAndFail
    , defaultProductionHandler = logAndFailProduction
    , defaultProcessingHandler = logAndFailProcessing
    , bufferedRecordsPerPartition = 1000
    , consumerOverrides = id
    , producerOverrides = id
    , adminOverrides = id
    , acceptableRecoveryLag = 10_000
    , maxWarmupReplicas = 2
    , probingRebalanceIntervalMs = 600_000
    , taskAssignor = Nothing
      }


----------------------------------------------------------------------
-- Properties-style overrides
----------------------------------------------------------------------

{- | The set of supported config keys, named to match the Java
@StreamsConfig@ constants.
-}
type StreamsConfigKey = T.Text


{- | Build a 'StreamsConfig' from the standard 'defaultStreamsConfig'
by applying @Properties@-style overrides. Mirrors how Java users
configure the runtime via @new StreamsConfig(properties)@.

Recognised keys (matching Java):

  * @application.id@
  * @bootstrap.servers@           — comma-separated host:port list
  * @client.id@
  * @num.stream.threads@
  * @num.standby.replicas@
  * @commit.interval.ms@
  * @poll.ms@
  * @max.task.idle.ms@
  * @buffered.records.per.partition@
  * @processing.guarantee@         — @"at_least_once"@ or @"exactly_once_v2"@
  * @replication.factor@
  * @state.dir@

Unknown keys are silently ignored (same as Java's default).
-}
streamsConfigFromMap
  :: Map.Map StreamsConfigKey T.Text -> StreamsConfig
streamsConfigFromMap m = foldl' step defaultStreamsConfig (Map.toAscList m)
  where
    step !cfg (k, v) = case k of
      "application.id" -> cfg {applicationId = v}
      "bootstrap.servers" ->
        cfg {bootstrapServers = T.splitOn "," v}
      "client.id" -> cfg {clientId = v}
      "num.stream.threads" ->
        maybe cfg (\n -> cfg {numStreamThreads = n}) (readT v)
      "num.standby.replicas" ->
        maybe cfg (\n -> cfg {numStandbyReplicas = n}) (readT v)
      "commit.interval.ms" ->
        maybe cfg (\n -> cfg {commitIntervalMs = Just n}) (readT v)
      "poll.ms" ->
        maybe cfg (\n -> cfg {pollMs = n}) (readT v)
      "buffered.records.per.partition" ->
        maybe cfg (\n -> cfg {bufferedRecordsPerPartition = n}) (readT v)
      "max.task.idle.ms" ->
        maybe cfg (\n -> cfg {maxTaskIdleMs = n}) (readT v)
      "processing.guarantee" -> case v of
        "at_least_once" -> cfg {processingGuarantee = AtLeastOnceP}
        "exactly_once_v2" -> cfg {processingGuarantee = ExactlyOnceV2}
        _ -> cfg
      "replication.factor" ->
        maybe cfg (\n -> cfg {replicationFactor = n}) (readT v)
      "state.dir" -> cfg {stateDir = T.unpack v}
      "acceptable.recovery.lag" ->
        maybe cfg (\n -> cfg {acceptableRecoveryLag = n}) (readT v)
      "max.warmup.replicas" ->
        maybe cfg (\n -> cfg {maxWarmupReplicas = n}) (readT v)
      "probing.rebalance.interval.ms" ->
        maybe cfg (\n -> cfg {probingRebalanceIntervalMs = n}) (readT v)
      _ -> cfg


readT :: Integral a => T.Text -> Maybe a
readT t = case TR.signed TR.decimal (T.strip t) of
  Right (n, rest) | T.null rest -> Just n
  _ -> Nothing
