{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Streams.Examples.Ops.Observability
Description : Demo of the operational observability utilities

Walks the @Kafka.Streams.Observability.*@ utilities end to end:

  1. 'TopologyStats' over the word-count topology;
  2. a 'LagReport' from a synthetic lag snapshot;
  3. a 'HealthReport' built from synthetic runtime state;
  4. the OpenTelemetry metrics bridge — registry counters / gauges /
     durations exported through real @hs-opentelemetry-api@
     observable instruments.

For (4) we install a tiny /collecting/ 'MeterProvider' so the demo
can print the values an SDK exporter would scrape. In production you
would instead install @hs-opentelemetry-sdk@ (OTLP, or its native
Prometheus endpoint) and never write a provider by hand.
-}
module Kafka.Streams.Examples.Ops.Observability (
  runDemo,
) where

import Control.Concurrent.STM
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Kafka.Streams.Examples.Ops.Helpers (bullet, section)
import Kafka.Streams.Examples.WordCount (buildWordCountTopology)
import Kafka.Streams.Metrics (
  MetricsRegistry,
  addCounter,
  newMetricsRegistry,
  registrySource,
  observeDuration,
  setGauge,
 )
import Kafka.Streams.Observability.Health (
  defaultHealthConfig,
  healthReportFrom,
  renderHealthReport,
 )
import Kafka.Streams.Observability.Lag (
  lagReport,
  recordLagReport,
  renderLagTable,
 )
import Kafka.Streams.Observability.OpenTelemetry (
  registerStreamsMetrics,
  smrInstrumentCount,
  streamsMeter,
 )
import Kafka.Streams.Observability.TopologyStats (
  renderTopologyStats,
  topologyStats,
 )
import Kafka.Streams.Processor (TaskId (..))
import Kafka.Streams.Runtime (
  LagInfo (..),
  LocalThreadMetadata (..),
  StreamsStatus (..),
 )
import OpenTelemetry.Metric.Core (
  Meter (..),
  MeterProvider (..),
  ObservableCallbackHandle (..),
  ObservableCounter (..),
  ObservableGauge (..),
  ObservableResult (..),
  forceFlushMeterProvider,
  noopMeter,
  setGlobalMeterProvider,
 )
import OpenTelemetry.Trace.Core (FlushResult (..), ShutdownResult (..))


runDemo :: IO ()
runDemo = do
  section "ObservabilityDemo"

  -- (1) Topology structural stats -----------------------------------
  topo <- buildWordCountTopology
  bullet "Topology stats (word-count):"
  TIO.putStr (indent (renderTopologyStats (topologyStats topo)))

  -- (2) Lag report --------------------------------------------------
  let lags =
        [ LagInfo (TaskId 0 0) 980 1000 False
        , LagInfo (TaskId 0 1) 1000 1000 False
        , LagInfo (TaskId 1 0) 500 1500 False
        ]
      rep = lagReport lags
  bullet "Lag report:"
  TIO.putStr (indent (renderLagTable rep))

  -- (3) Health report -----------------------------------------------
  let threads =
        [ LocalThreadMetadata {threadId = 0, assigned = [], processedRecs = 4200}
        , LocalThreadMetadata {threadId = 1, assigned = [], processedRecs = 3900}
        ]
      health =
        healthReportFrom defaultHealthConfig StreamsRunning threads (Just rep)
  bullet "Health report (running, lag within budget):"
  TIO.putStr (indent (renderHealthReport health))

  -- (4) OpenTelemetry metrics bridge --------------------------------
  reg <- newMetricsRegistry
  seedRegistry reg
  recordLagReport reg rep

  collector <- newCollector
  setGlobalMeterProvider (collectingProvider collector)
  meter <- streamsMeter
  registration <- registerStreamsMetrics meter (registrySource reg)
  bullet
    ( "Registered "
        <> show (smrInstrumentCount registration)
        <> " OpenTelemetry observable instruments from the registry."
    )

  -- An SDK would do this on its export interval; we do it once.
  _ <- forceFlushMeterProvider (collectingProvider collector) Nothing
  collected <- readTVarIO (colValues collector)
  bullet "Collected OpenTelemetry measurements:"
  mapM_
    (\(k, v) -> bullet ("    " <> T.unpack k <> " = " <> show v))
    (Map.toAscList collected)


-- | Poke a representative slice of engine metrics into the registry.
seedRegistry :: MetricsRegistry -> IO ()
seedRegistry reg = do
  addCounter reg "stream-processor-node-metrics:process-total" 8100
  addCounter reg "stream-task-metrics:commit-total" 27
  setGauge reg "stream-thread-metrics:process-rate" 1325.5
  observeDuration reg "stream-task-metrics:process-latency" 120
  observeDuration reg "stream-task-metrics:process-latency" 340


indent :: Text -> Text
indent = T.unlines . map ("    " <>) . T.lines


----------------------------------------------------------------------
-- A minimal collecting MeterProvider (demo only)
--
-- Captures every observable callback the bridge registers, then runs
-- them on forceFlush, recording each measurement by instrument name.
----------------------------------------------------------------------

data Collector = Collector
  { colCallbacks :: !(TVar [IO ()])
  , colValues :: !(TVar (Map Text Double))
  }


newCollector :: IO Collector
newCollector = Collector <$> newTVarIO [] <*> newTVarIO Map.empty


collectingProvider :: Collector -> MeterProvider
collectingProvider col =
  MeterProvider
    { meterProviderGetMeter = \scope -> pure (collectingMeter col scope)
    , meterProviderShutdown = \_ -> pure ShutdownSuccess
    , meterProviderForceFlush = \_ -> do
        cbs <- readTVarIO (colCallbacks col)
        sequence_ cbs
        pure FlushSuccess
    }


-- The scope type is inferred from 'noopMeter' (InstrumentationLibrary);
-- left unannotated so we needn't import the internal scope type here.
collectingMeter col scope =
  (noopMeter scope)
    { meterCreateObservableCounterInt64 =
        \name _ _ _ _ -> pure (obsCounter name)
    , meterCreateObservableGaugeInt64 =
        \name _ _ _ _ -> pure (obsGaugeI64 name)
    , meterCreateObservableGaugeDouble =
        \name _ _ _ _ -> pure (obsGaugeDbl name)
    }
  where
    register name toDbl cb = do
      let res = ObservableResult $ \v _attrs ->
            atomically (modifyTVar' (colValues col) (Map.insert name (toDbl v)))
      atomically (modifyTVar' (colCallbacks col) (cb res :))
      pure (ObservableCallbackHandle (pure ()))

    obsCounter name =
      ObservableCounter
        { observableCounterRegisterCallback = register name fromIntegral
        , observableCounterInstrumentScope = scope
        , observableCounterInstrumentName = name
        , observableCounterEnabled = pure True
        }
    obsGaugeI64 name =
      ObservableGauge
        { observableGaugeRegisterCallback = register name fromIntegral
        , observableGaugeInstrumentScope = scope
        , observableGaugeInstrumentName = name
        , observableGaugeEnabled = pure True
        }
    obsGaugeDbl name =
      ObservableGauge
        { observableGaugeRegisterCallback = register name id
        , observableGaugeInstrumentScope = scope
        , observableGaugeInstrumentName = name
        , observableGaugeEnabled = pure True
        }
