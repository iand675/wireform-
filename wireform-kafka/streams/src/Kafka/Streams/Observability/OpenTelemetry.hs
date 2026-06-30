{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Streams.Observability.OpenTelemetry
Description : Bridge the in-process metrics registry to OpenTelemetry

'Kafka.Streams.Metrics' keeps a plain in-memory registry of
counters, gauges, and duration summaries that the engine pokes on
the hot path. This module exports that registry through the
OpenTelemetry metrics API (the @hs-opentelemetry-api@ 1.0 suite),
which is the framework's single channel for /all/ observability —
traces, logs, and metrics.

We deliberately depend on the API package only (not the SDK). A
library should never decide how telemetry leaves the process; the
application installs an SDK 'MeterProvider' (OTLP, or the SDK's
native scrapable Prometheus endpoint) via
'OpenTelemetry.Metric.Core.setGlobalMeterProvider'. Until it does,
the global provider is a no-op and this bridge costs nothing.

== How it works

For each metric currently in the registry we create an
/observable/ (asynchronous) instrument and register a callback
that re-reads the live value from the registry on every collection
cycle. Observable instruments are the idiomatic way to surface a
pull-based source like ours:

  * counters  → 'ObservableCounter' @Int64@;
  * gauges    → 'ObservableGauge' @Double@;
  * durations → four derived instruments — @.count@ and @.sum@
    (observable counters) plus @.min@ and @.max@ (observable
    gauges), mirroring how the JVM client decomposes a latency
    sensor.

Registry metric names use the Java convention
(@"stream-task-metrics:commit-total"@). OpenTelemetry instrument
names forbid @:@, so it is rewritten to @.@ and any other
out-of-grammar character to @_@; see 'sanitizeInstrumentName'.

Metrics are a 1.0-only API — @OpenTelemetry.Metric.Core@ did not exist
before @hs-opentelemetry-api@ 1.0. Compiled against a pre-1.0 API this
module omits the metrics bridge ('streamsMeter', 'registerStreamsMetrics',
'unregisterStreamsMetrics', 'StreamsMetricsRegistration'); only
'streamsInstrumentationScope' and 'sanitizeInstrumentName' remain.
-}
module Kafka.Streams.Observability.OpenTelemetry (
  -- * Instrumentation scope
  streamsInstrumentationScope,
#if MIN_VERSION_hs_opentelemetry_api(1,0,0)
  streamsMeter,

  -- * Registration
  StreamsMetricsRegistration (..),
  registerStreamsMetrics,
  unregisterStreamsMetrics,
#endif

  -- * Helpers (exposed for tests / reuse)
  sanitizeInstrumentName,
) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Text (Text)
import Data.Text qualified as T
import OpenTelemetry.Attributes (emptyAttributes)
import OpenTelemetry.Trace.Core (InstrumentationLibrary (..))
#if MIN_VERSION_hs_opentelemetry_api(1,0,0)
import Data.Map.Strict qualified as Map
import Kafka.Streams.Metrics (
  DurationStats (..),
  MetricValue (..),
  MetricsRegistry,
  dumpMetrics,
  readCounter,
  readDurationStats,
  readGauge,
 )
import OpenTelemetry.Metric.Core (
  Meter,
  ObservableCallbackHandle (..),
  ObservableCounter (..),
  ObservableGauge (..),
  ObservableResult (..),
  defaultAdvisoryParameters,
  getGlobalMeterProvider,
  getMeter,
  meterCreateObservableCounterInt64,
  meterCreateObservableGaugeDouble,
  meterCreateObservableGaugeInt64,
 )
#endif


----------------------------------------------------------------------
-- Instrumentation scope
----------------------------------------------------------------------

{- | The instrumentation scope every streams metric is reported
under. Mirrors the JVM @org.apache.kafka.streams@ scope.
-}
streamsInstrumentationScope :: InstrumentationLibrary
streamsInstrumentationScope =
  InstrumentationLibrary
    { libraryName = "wireform-kafka-streams"
    , libraryVersion = ""
    , librarySchemaUrl = ""
    , libraryAttributes = emptyAttributes
    }


#if MIN_VERSION_hs_opentelemetry_api(1,0,0)
{- | Obtain a 'Meter' for the streams scope from the globally
installed 'OpenTelemetry.Metric.Core.MeterProvider'. Before the
application installs an SDK provider this returns a no-op meter.
-}
streamsMeter :: IO Meter
streamsMeter = do
  mp <- getGlobalMeterProvider
  getMeter mp streamsInstrumentationScope


----------------------------------------------------------------------
-- Registration
----------------------------------------------------------------------

{- | The result of bridging a registry: the count of instruments
created and the callback handles needed to tear them down.
-}
data StreamsMetricsRegistration = StreamsMetricsRegistration
  { smrInstrumentCount :: !Int
  -- ^ Number of OpenTelemetry instruments registered.
  , smrHandles :: ![ObservableCallbackHandle]
  {- ^ One handle per registered observable callback; pass to
  'unregisterStreamsMetrics' to detach them.
  -}
  }


{- | Register OpenTelemetry observable instruments for every metric
currently present in the registry. Each instrument's callback
re-reads the live value on collection, so counts/gauges stay
current without re-registration.

Instruments are created for the metrics that exist at call time.
Call this after the topology has been built and the engine has
recorded at least once (or pre-seed the registry) so the metric
set is known. Returns a 'StreamsMetricsRegistration' whose handles
can later be passed to 'unregisterStreamsMetrics'.
-}
registerStreamsMetrics
  :: Meter -> MetricsRegistry -> IO StreamsMetricsRegistration
registerStreamsMetrics meter reg = do
  snapshot <- dumpMetrics reg
  handles <- fmap concat (traverse registerOne (Map.toList snapshot))
  pure
    StreamsMetricsRegistration
      { smrInstrumentCount = length handles
      , smrHandles = handles
      }
  where
    registerOne (rawName, value) = case value of
      MVCounter _ -> registerCounter rawName
      MVGauge _ -> registerGauge rawName
      MVDuration _ -> registerDuration rawName

    registerCounter rawName = do
      inst <-
        meterCreateObservableCounterInt64
          meter
          (sanitizeInstrumentName rawName)
          Nothing
          (Just rawName)
          defaultAdvisoryParameters
          []
      h <- observableCounterRegisterCallback inst $ \res -> do
        n <- readCounter reg rawName
        observe res n emptyAttributes
      pure [h]

    registerGauge rawName = do
      inst <-
        meterCreateObservableGaugeDouble
          meter
          (sanitizeInstrumentName rawName)
          Nothing
          (Just rawName)
          defaultAdvisoryParameters
          []
      h <- observableGaugeRegisterCallback inst $ \res -> do
        mv <- readGauge reg rawName
        case mv of
          Just v -> observe res v emptyAttributes
          Nothing -> pure ()
      pure [h]

    registerDuration rawName = do
      countH <-
        observableCounter
          (rawName <> ":count")
          Nothing
          "(count)"
          (\s -> dsCount s)
      sumH <-
        observableCounter
          (rawName <> ":sum")
          (Just "us")
          "(sum)"
          (\s -> dsSum s)
      minH <-
        observableGaugeInt
          (rawName <> ":min")
          (Just "us")
          "(min)"
          (\s -> dsMin s)
      maxH <-
        observableGaugeInt
          (rawName <> ":max")
          (Just "us")
          "(max)"
          (\s -> dsMax s)
      pure [countH, sumH, minH, maxH]
      where
        observableCounter nm unit suffix project = do
          inst <-
            meterCreateObservableCounterInt64
              meter
              (sanitizeInstrumentName nm)
              unit
              (Just (rawName <> " " <> suffix))
              defaultAdvisoryParameters
              []
          observableCounterRegisterCallback inst $ \res ->
            withDuration rawName (\s -> observe res (project s) emptyAttributes)

        observableGaugeInt nm unit suffix project = do
          inst <-
            meterCreateObservableGaugeInt64
              meter
              (sanitizeInstrumentName nm)
              unit
              (Just (rawName <> " " <> suffix))
              defaultAdvisoryParameters
              []
          observableGaugeRegisterCallback inst $ \res ->
            withDuration rawName (\s -> observe res (project s) emptyAttributes)

    withDuration rawName k = do
      ms <- readDurationStats reg rawName
      case ms of
        Just s -> k s
        Nothing -> pure ()


{- | Detach every callback created by 'registerStreamsMetrics'. After
this the instruments stop reporting (an SDK may still retain the
last value until its next collection, per OTel semantics).
-}
unregisterStreamsMetrics :: StreamsMetricsRegistration -> IO ()
unregisterStreamsMetrics =
  mapM_ unregisterObservableCallback . smrHandles
#endif


----------------------------------------------------------------------
-- Name sanitisation
----------------------------------------------------------------------

{- | Rewrite a registry metric name into a valid OpenTelemetry
instrument name. The OTel grammar is
@[A-Za-z][A-Za-z0-9_.\/-]{0,254}@:

  * @:@ becomes @.@ (Java metric groups map to OTel namespaces);
  * any other out-of-grammar character becomes @_@;
  * a non-alphabetic first character is prefixed with @x@;
  * the result is truncated to 255 characters.
-}
sanitizeInstrumentName :: Text -> Text
sanitizeInstrumentName raw =
  T.take 255 (ensureLeading (T.map replace raw))
  where
    replace c
      | c == ':' = '.'
      | isAsciiUpper c || isAsciiLower c || isDigit c = c
      | c == '_' || c == '.' || c == '-' || c == '/' = c
      | otherwise = '_'
    ensureLeading mapped =
      case T.uncons mapped of
        Nothing -> "x"
        Just (c, _) ->
          if isAsciiUpper c || isAsciiLower c
            then mapped
            else T.cons 'x' mapped
