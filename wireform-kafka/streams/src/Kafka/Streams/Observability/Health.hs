{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Streams.Observability.Health
Description : Liveness / readiness report for the streams runtime

A structured health document derived from the runtime's lifecycle
'StreamsStatus', its per-thread metadata, and (optionally) a lag
report. Intended to back a @\/healthz@ (liveness) and @\/ready@
(readiness) endpoint, or a CLI status command.

The mapping from runtime state to 'HealthStatus':

  * 'StreamsRunning' with lag within budget → 'Healthy', ready.
  * 'StreamsRunning' with lag over budget   → 'Degraded', not ready.
  * 'StreamsCreated' / 'StreamsClosing'     → 'Degraded', not ready.
  * 'StreamsClosed'                         → 'Unhealthy', not ready.
  * 'StreamsError'                          → 'Unhealthy', not ready.

The pure core 'healthReportFrom' takes its inputs explicitly so it
is trivially testable; 'kafkaStreamsHealth' / 'kafkaStreamsHealthWithLag'
are the IO conveniences that read live runtime state.
-}
module Kafka.Streams.Observability.Health (
  -- * Status
  HealthStatus (..),

  -- * Report
  HealthReport (..),

  -- * Configuration
  HealthConfig (..),
  defaultHealthConfig,

  -- * Construction
  healthReportFrom,
  kafkaStreamsHealth,
  kafkaStreamsHealthWithLag,

  -- * Rendering
  healthReportJson,
  renderHealthReport,
) where

import Data.Aeson ((.=))
import Data.Aeson qualified as A
import Data.Int (Int64)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Streams.Observability.Lag (
  LagReport,
  LagSeverity (..),
  classifyLag,
  lagReport,
  lagReportJson,
 )
import Kafka.Streams.Runtime (
  KafkaStreams,
  LagInfo (..),
  localTaskLags,
  LocalThreadMetadata (..),
  StreamsStatus (..),
  metadataForLocalThreads,
  streamsStatus,
 )


----------------------------------------------------------------------
-- Status
----------------------------------------------------------------------

-- | Overall health verdict, worst-wins when combining signals.
data HealthStatus = Healthy | Degraded | Unhealthy
  deriving stock (Eq, Show, Ord)


----------------------------------------------------------------------
-- Configuration
----------------------------------------------------------------------

-- | Thresholds that turn raw runtime signals into a verdict.
data HealthConfig = HealthConfig
  { healthMaxLag :: !Int64
  {- ^ Maximum acceptable per-task lag before a running instance is
  reported 'Degraded' / not ready. Only consulted when a lag
  report is supplied. Default: 10000.
  -}
  }
  deriving stock (Eq, Show)


defaultHealthConfig :: HealthConfig
defaultHealthConfig = HealthConfig {healthMaxLag = 10000}


----------------------------------------------------------------------
-- Report
----------------------------------------------------------------------

-- | A point-in-time health snapshot.
data HealthReport = HealthReport
  { healthStatus :: !HealthStatus
  , healthState :: !StreamsStatus
  , healthReady :: !Bool
  -- ^ Readiness: safe to route traffic / interactive queries.
  , healthThreads :: !Int
  , healthAssignedPartitions :: !Int
  , healthProcessedRecords :: !Int64
  , healthLag :: !(Maybe LagReport)
  , healthMessages :: ![Text]
  -- ^ Human-readable explanations for the verdict.
  }
  deriving stock (Eq, Show)


----------------------------------------------------------------------
-- Construction
----------------------------------------------------------------------

{- | Build a report from explicit inputs. Pure; the IO wrappers below
gather these from a live 'KafkaStreams'.
-}
healthReportFrom
  :: HealthConfig
  -> StreamsStatus
  -> [LocalThreadMetadata]
  -> Maybe LagReport
  -> HealthReport
healthReportFrom cfg st threads mLag =
  HealthReport
    { healthStatus = status
    , healthState = st
    , healthReady = ready
    , healthThreads = length threads
    , healthAssignedPartitions = assignedCount
    , healthProcessedRecords = processed
    , healthLag = mLag
    , healthMessages = messages
    }
  where
    assignedCount =
      List.foldl' (\acc t -> acc + length (assigned t)) 0 threads
    processed =
      List.foldl' (\acc t -> acc + processedRecs t) 0 threads
    severity = fmap (classifyLag (healthMaxLag cfg)) mLag
    (status, ready, messages) = assess st severity


-- | The core state machine: runtime lifecycle + lag severity → verdict.
assess
  :: StreamsStatus
  -> Maybe LagSeverity
  -> (HealthStatus, Bool, [Text])
assess st severity = case st of
  StreamsRunning -> case severity of
    Just LagExceeded ->
      (Degraded, False, ["running but lag exceeds threshold"])
    _ -> (Healthy, True, ["running"])
  StreamsCreated ->
    (Degraded, False, ["created but not started"])
  StreamsClosing ->
    (Degraded, False, ["closing"])
  StreamsClosed ->
    (Unhealthy, False, ["closed"])
  StreamsError msg ->
    (Unhealthy, False, ["error: " <> msg])


-- | Read live runtime state and build a health report without lag.
kafkaStreamsHealth :: HealthConfig -> KafkaStreams -> IO HealthReport
kafkaStreamsHealth cfg ks = do
  st <- streamsStatus ks
  threads <- metadataForLocalThreads ks
  pure (healthReportFrom cfg st threads Nothing)


{- | Read live runtime state and the active tasks' lag
('Kafka.Streams.Runtime.localTaskLags': restoring tasks lag, running
ones don't, standbys don't count) and build a full health report.
When the lag can't be read the report has none.
-}
kafkaStreamsHealthWithLag
  :: HealthConfig -> KafkaStreams -> IO HealthReport
kafkaStreamsHealthWithLag cfg ks = do
  st <- streamsStatus ks
  threads <- metadataForLocalThreads ks
  lags <- localTaskLags ks
  pure (healthReportFrom cfg st threads (either (const Nothing) (Just . lagReport . filter (not . lagStandby)) lags))


----------------------------------------------------------------------
-- Rendering
----------------------------------------------------------------------

-- | Render a report as a versioned JSON object.
healthReportJson :: HealthReport -> A.Value
healthReportJson rep =
  A.object
    ( [ "version" .= (1 :: Int)
      , "status" .= statusText (healthStatus rep)
      , "state" .= stateText (healthState rep)
      , "ready" .= healthReady rep
      , "threads" .= healthThreads rep
      , "assignedPartitions" .= healthAssignedPartitions rep
      , "processedRecords" .= healthProcessedRecords rep
      , "messages" .= healthMessages rep
      ]
        <> lagField
    )
  where
    lagField = case healthLag rep of
      Nothing -> []
      Just lag -> ["lag" .= lagReportJson lag]


-- | Render a report as a short human-readable block.
renderHealthReport :: HealthReport -> Text
renderHealthReport rep =
  T.unlines
    [ "status:    " <> statusText (healthStatus rep)
    , "state:     " <> stateText (healthState rep)
    , "ready:     " <> (if healthReady rep then "yes" else "no")
    , "threads:   " <> T.pack (show (healthThreads rep))
    , "assigned:  " <> T.pack (show (healthAssignedPartitions rep))
    , "processed: " <> T.pack (show (healthProcessedRecords rep))
    , "messages:  " <> T.intercalate "; " (healthMessages rep)
    ]


statusText :: HealthStatus -> Text
statusText = \case
  Healthy -> "healthy"
  Degraded -> "degraded"
  Unhealthy -> "unhealthy"


stateText :: StreamsStatus -> Text
stateText = \case
  StreamsCreated -> "CREATED"
  StreamsRunning -> "RUNNING"
  StreamsClosing -> "CLOSING"
  StreamsClosed -> "CLOSED"
  StreamsError msg -> "ERROR: " <> msg
