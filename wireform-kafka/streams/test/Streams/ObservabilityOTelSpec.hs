{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Streams.ObservabilityOTelSpec
Description : Tests for the OpenTelemetry metrics bridge

Verifies that registry counters / gauges / duration summaries are
exported through real @hs-opentelemetry-api@ observable instruments.
A local /collecting/ 'MeterProvider' captures the callbacks the
bridge registers; running them simulates an SDK collection cycle and
lets us assert the values and (sanitised) instrument names that
would be exported. No SDK and no global provider are involved, so
the test is deterministic and parallel-safe.
-}
module Streams.ObservabilityOTelSpec (tests) where

import Control.Concurrent.STM
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Kafka.Streams.Metrics (
  addCounter,
  registrySource,
  newMetricsRegistry,
  observeDuration,
  setGauge,
 )
import Kafka.Streams.Observability.OpenTelemetry
import OpenTelemetry.Metric.Core (
  Meter (..),
  ObservableCallbackHandle (..),
  ObservableCounter (..),
  ObservableGauge (..),
  ObservableResult (..),
  noopMeter,
 )
import Test.Syd


tests :: Spec
tests =
  describe "ObservabilityOTel" $
    sequence_
      [ sanitize_rules
      , bridge_exports_counter_gauge_duration
      , bridge_instrument_count
      ]


sanitize_rules :: Spec
sanitize_rules =
  it "sanitizeInstrumentName follows the OTel grammar" $ do
    sanitizeInstrumentName "stream-task-metrics:commit-total"
      `shouldBe` "stream-task-metrics.commit-total"
    sanitizeInstrumentName "weird name!" `shouldBe` "weird_name_"
    sanitizeInstrumentName "9lives" `shouldBe` "x9lives"
    sanitizeInstrumentName "" `shouldBe` "x"


bridge_exports_counter_gauge_duration :: Spec
bridge_exports_counter_gauge_duration =
  it "registry values flow to OTel instruments by sanitised name" $ do
    reg <- newMetricsRegistry
    addCounter reg "stream-task-metrics:commit-total" 5
    setGauge reg "g:rate" 2.5
    observeDuration reg "lat" 100
    observeDuration reg "lat" 300

    col <- newCollector
    _ <- registerStreamsMetrics (collectingMeter col) (registrySource reg)
    runCallbacks col
    vals <- readTVarIO (colValues col)

    Map.lookup "stream-task-metrics.commit-total" vals `shouldBe` Just 5.0
    Map.lookup "g.rate" vals `shouldBe` Just 2.5
    Map.lookup "lat.count" vals `shouldBe` Just 2.0
    Map.lookup "lat.sum" vals `shouldBe` Just 400.0
    Map.lookup "lat.min" vals `shouldBe` Just 100.0
    Map.lookup "lat.max" vals `shouldBe` Just 300.0


bridge_instrument_count :: Spec
bridge_instrument_count =
  it "one instrument per counter/gauge, four per duration" $ do
    reg <- newMetricsRegistry
    addCounter reg "c" 1
    setGauge reg "g" 1.0
    observeDuration reg "d" 7
    col <- newCollector
    reg' <- registerStreamsMetrics (collectingMeter col) (registrySource reg)
    -- 1 (counter) + 1 (gauge) + 4 (duration) = 6
    smrInstrumentCount reg' `shouldBe` 6


----------------------------------------------------------------------
-- A local collecting meter (test harness only)
----------------------------------------------------------------------

data Collector = Collector
  { colCallbacks :: !(TVar [IO ()])
  , colValues :: !(TVar (Map Text Double))
  }


newCollector :: IO Collector
newCollector = Collector <$> newTVarIO [] <*> newTVarIO Map.empty


runCallbacks :: Collector -> IO ()
runCallbacks col = readTVarIO (colCallbacks col) >>= sequence_


collectingMeter :: Collector -> Meter
collectingMeter col =
  (noopMeter streamsInstrumentationScope)
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
        , observableCounterInstrumentScope = streamsInstrumentationScope
        , observableCounterInstrumentName = name
        , observableCounterEnabled = pure True
        }
    obsGaugeI64 name =
      ObservableGauge
        { observableGaugeRegisterCallback = register name fromIntegral
        , observableGaugeInstrumentScope = streamsInstrumentationScope
        , observableGaugeInstrumentName = name
        , observableGaugeEnabled = pure True
        }
    obsGaugeDbl name =
      ObservableGauge
        { observableGaugeRegisterCallback = register name id
        , observableGaugeInstrumentScope = streamsInstrumentationScope
        , observableGaugeInstrumentName = name
        , observableGaugeEnabled = pure True
        }
