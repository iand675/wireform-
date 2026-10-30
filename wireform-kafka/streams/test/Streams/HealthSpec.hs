{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Streams.HealthSpec
Description : Tests for the health/readiness report
-}
module Streams.HealthSpec (tests) where

import Kafka.Streams.Observability.Health
import Kafka.Streams.Observability.Lag (lagReport)
import Kafka.Streams.Processor (TaskId (..))
import Kafka.Streams.Runtime (LagInfo (..), LocalThreadMetadata (..), StreamsStatus (..))
import Test.Syd


tests :: Spec
tests =
  describe "Health" $
    sequence_
      [ running_within_budget_is_healthy
      , running_over_budget_is_degraded
      , error_state_is_unhealthy
      , created_state_not_ready
      , aggregates_thread_metadata
      ]


threads :: [LocalThreadMetadata]
threads =
  [ LocalThreadMetadata {threadId = 0, assigned = [], processedRecs = 100}
  , LocalThreadMetadata {threadId = 1, assigned = [], processedRecs = 250}
  ]


running_within_budget_is_healthy :: Spec
running_within_budget_is_healthy =
  it "running with lag within budget is healthy + ready" $ do
    let lag = lagReport [LagInfo (TaskId 0 0) 990 1000 False] -- behind 10
        rep = healthReportFrom defaultHealthConfig StreamsRunning threads (Just lag)
    healthStatus rep `shouldBe` Healthy
    healthReady rep `shouldBe` True


running_over_budget_is_degraded :: Spec
running_over_budget_is_degraded =
  it "running with lag over budget is degraded + not ready" $ do
    let lag = lagReport [LagInfo (TaskId 0 0) 0 1000000 False] -- behind 1e6
        rep = healthReportFrom defaultHealthConfig StreamsRunning threads (Just lag)
    healthStatus rep `shouldBe` Degraded
    healthReady rep `shouldBe` False


error_state_is_unhealthy :: Spec
error_state_is_unhealthy =
  it "error state is unhealthy + not ready" $ do
    let rep = healthReportFrom defaultHealthConfig (StreamsError "boom") threads Nothing
    healthStatus rep `shouldBe` Unhealthy
    healthReady rep `shouldBe` False


created_state_not_ready :: Spec
created_state_not_ready =
  it "created-but-not-started is degraded + not ready" $ do
    let rep = healthReportFrom defaultHealthConfig StreamsCreated threads Nothing
    healthStatus rep `shouldBe` Degraded
    healthReady rep `shouldBe` False


aggregates_thread_metadata :: Spec
aggregates_thread_metadata =
  it "thread count and processed records are summed" $ do
    let rep = healthReportFrom defaultHealthConfig StreamsRunning threads Nothing
    healthThreads rep `shouldBe` 2
    healthProcessedRecords rep `shouldBe` 350
    (healthReady rep) `shouldBe` True
