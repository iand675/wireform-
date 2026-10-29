{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Streams.Metrics
Description : In-process metrics registry for the Streams runtime

A lightweight metrics layer modelled on Kafka's @Metrics@ + @Sensor@
abstractions but compiled down to plain 'TVar's. The runtime pokes
counters / gauges / timers at well-known points in the engine
lifecycle; users can read snapshots via 'dumpMetrics'.

A running instance keeps its counters in the tasks themselves
(unboxed, no allocation per record) and snapshots them on demand:
'Kafka.Streams.Runtime.streamsMetrics' is a 'MetricsSource', as is
'registrySource' over a registry of your own.

The metric naming scheme mirrors the Java client:

@
"stream-processor-node-metrics:process-total"
"stream-task-metrics:commit-total"
"stream-state-metrics:put-rate"
@

...and so on. We do NOT export Prometheus / OpenTelemetry here —
the registry is a plain in-memory map; production users wire it
to whatever observability stack they prefer via a periodic
'dumpMetrics' poll.
-}
module Kafka.Streams.Metrics (
  -- * Registry
  MetricsRegistry,
  newMetricsRegistry,
  noopMetricsRegistry,

  -- * Recording
  incCounter,
  addCounter,
  setGauge,
  observeDuration,

  -- * Reading
  MetricValue (..),
  dumpMetrics,
  readCounter,
  readGauge,
  readDurationStats,
  DurationStats (..),

  -- * Common metric names
  processTotal,
  forwardTotal,
  commitTotal,
  punctuateTotal,
  storePutTotal,
  storeGetTotal,
  storeDeleteTotal,
  droppedRecordsTotal,
  threadCommitTotal,
  threadCommitFailedTotal,
  threadCommitAbortedTotal,
  taskRecoveriesTotal,
  rebalanceTotal,
  restoredRecordsTotal,
  standbyRecordsTotal,

  -- * Sources
  MetricsSource,
  registrySource,
) where

import Control.Concurrent.STM
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)


----------------------------------------------------------------------
-- Registry
----------------------------------------------------------------------

{- | A registry of named metric values. Metric names are 'Text';
duplicate names are collapsed (last write wins for gauges,
additive for counters).
-}
data MetricsRegistry = MetricsRegistry
  { mrCounters :: !(TVar (Map Text Int64))
  , mrGauges :: !(TVar (Map Text Double))
  , mrDurations :: !(TVar (Map Text DurationStats))
  }


newMetricsRegistry :: IO MetricsRegistry
newMetricsRegistry = do
  c <- newTVarIO Map.empty
  g <- newTVarIO Map.empty
  d <- newTVarIO Map.empty
  pure MetricsRegistry {mrCounters = c, mrGauges = g, mrDurations = d}


{- | A registry that silently discards all writes. Useful when the
caller doesn't care about metrics — runtimes can use this without
branching on a 'Maybe MetricsRegistry'.
-}
noopMetricsRegistry :: IO MetricsRegistry
noopMetricsRegistry = newMetricsRegistry


----------------------------------------------------------------------
-- Recording
----------------------------------------------------------------------

incCounter :: MetricsRegistry -> Text -> IO ()
incCounter r nm = addCounter r nm 1


addCounter :: MetricsRegistry -> Text -> Int64 -> IO ()
addCounter r nm n =
  atomically $
    modifyTVar' (mrCounters r) (Map.insertWith (+) nm n)


setGauge :: MetricsRegistry -> Text -> Double -> IO ()
setGauge r nm v =
  atomically $
    modifyTVar' (mrGauges r) (Map.insert nm v)


{- | Record a single observation (e.g. microseconds elapsed) for a
named timer. The registry tracks count, sum, min, and max — all
the inputs needed for an external avg / p95 / max derivation.
-}
observeDuration :: MetricsRegistry -> Text -> Int64 -> IO ()
observeDuration r nm d =
  atomically $
    modifyTVar' (mrDurations r) (Map.alter step nm)
  where
    step Nothing =
      Just
        DurationStats
          { dsCount = 1
          , dsSum = d
          , dsMin = d
          , dsMax = d
          }
    step (Just cur) =
      Just
        DurationStats
          { dsCount = dsCount cur + 1
          , dsSum = dsSum cur + d
          , dsMin = min (dsMin cur) d
          , dsMax = max (dsMax cur) d
          }


----------------------------------------------------------------------
-- Reading
----------------------------------------------------------------------

data DurationStats = DurationStats
  { dsCount :: !Int64
  , dsSum :: !Int64
  , dsMin :: !Int64
  , dsMax :: !Int64
  }
  deriving stock (Eq, Show, Generic)


data MetricValue
  = MVCounter !Int64
  | MVGauge !Double
  | MVDuration !DurationStats
  deriving stock (Eq, Show, Generic)


{- | Read every metric currently recorded as a single 'Map'. Counters,
gauges, and duration stats coexist in the same namespace.
-}
dumpMetrics :: MetricsRegistry -> IO (Map Text MetricValue)
dumpMetrics r = atomically $ do
  cs <- readTVar (mrCounters r)
  gs <- readTVar (mrGauges r)
  ds <- readTVar (mrDurations r)
  let counters = Map.map MVCounter cs
      gauges = Map.map MVGauge gs
      durs = Map.map MVDuration ds
  pure (counters `Map.union` gauges `Map.union` durs)


readCounter :: MetricsRegistry -> Text -> IO Int64
readCounter r nm = atomically $ do
  m <- readTVar (mrCounters r)
  pure (Map.findWithDefault 0 nm m)


readGauge :: MetricsRegistry -> Text -> IO (Maybe Double)
readGauge r nm = atomically $ do
  m <- readTVar (mrGauges r)
  pure (Map.lookup nm m)


readDurationStats
  :: MetricsRegistry -> Text -> IO (Maybe DurationStats)
readDurationStats r nm = atomically $ do
  m <- readTVar (mrDurations r)
  pure (Map.lookup nm m)


----------------------------------------------------------------------
-- Common names
----------------------------------------------------------------------

processTotal, forwardTotal, commitTotal, punctuateTotal :: Text
processTotal = "stream-processor-node-metrics:process-total"
forwardTotal = "stream-processor-node-metrics:forward-total"
commitTotal = "stream-task-metrics:commit-total"
punctuateTotal = "stream-task-metrics:punctuate-total"


storePutTotal, storeGetTotal, storeDeleteTotal :: Text
storePutTotal = "stream-state-metrics:put-total"
storeGetTotal = "stream-state-metrics:get-total"
storeDeleteTotal = "stream-state-metrics:delete-total"


droppedRecordsTotal :: Text
droppedRecordsTotal = "stream-task-metrics:dropped-records-total"


threadCommitTotal, threadCommitFailedTotal, threadCommitAbortedTotal :: Text
threadCommitTotal = "stream-thread-metrics:commit-total"
threadCommitFailedTotal = "stream-thread-metrics:commit-failed-total"
threadCommitAbortedTotal = "stream-thread-metrics:commit-aborted-total"


taskRecoveriesTotal, rebalanceTotal, restoredRecordsTotal, standbyRecordsTotal :: Text
taskRecoveriesTotal = "stream-thread-metrics:task-recoveries-total"
rebalanceTotal = "stream-thread-metrics:rebalance-total"
restoredRecordsTotal = "stream-state-metrics:restore-total"
standbyRecordsTotal = "stream-state-metrics:standby-update-total"


----------------------------------------------------------------------
-- Sources
----------------------------------------------------------------------

-- | A snapshot of metrics, re-read on every call.
type MetricsSource = IO (Map Text MetricValue)


registrySource :: MetricsRegistry -> MetricsSource
registrySource = dumpMetrics
