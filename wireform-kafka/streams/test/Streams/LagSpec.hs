{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Streams.LagSpec
Description : Tests for the lag report utility
-}
module Streams.LagSpec (tests) where

import Kafka.Streams.Metrics (
  newMetricsRegistry,
  readGauge,
 )
import Kafka.Streams.Observability.Lag
import Kafka.Streams.Processor (TaskId (..))
import Kafka.Streams.Runtime (LagInfo (..))
import Test.Syd


tests :: Spec
tests =
  describe "Lag" $
    sequence_
      [ task_lag_clamps_negative
      , report_aggregates
      , report_sorted_by_task
      , classify_thresholds
      , record_lag_sets_gauges
      ]


sampleLags :: [LagInfo]
sampleLags =
  [ LagInfo (TaskId 1 0) 500 1500 False -- behind 1000
  , LagInfo (TaskId 0 0) 980 1000 False -- behind 20
  , LagInfo (TaskId 0 1) 1000 1000 False -- caught up
  ]


task_lag_clamps_negative :: Spec
task_lag_clamps_negative =
  it "taskLagOf clamps a current ahead of end to zero behind" $ do
    let tl = taskLagOf (LagInfo (TaskId 0 0) 1200 1000 False)
    taskLagBehind tl `shouldBe` 0


report_aggregates :: Spec
report_aggregates =
  it "lagReport sums, maxes, and counts caught-up tasks" $ do
    let rep = lagReport sampleLags
    lagReportTaskCount rep `shouldBe` 3
    lagReportCaughtUp rep `shouldBe` 1
    lagReportTotalBehind rep `shouldBe` 1020
    lagReportMaxBehind rep `shouldBe` 1000


report_sorted_by_task :: Spec
report_sorted_by_task =
  it "lagReport sorts tasks by TaskId" $ do
    let rep = lagReport sampleLags
    map taskLagId (lagReportTasks rep)
      `shouldBe` [TaskId 0 0, TaskId 0 1, TaskId 1 0]


classify_thresholds :: Spec
classify_thresholds =
  it "classifyLag respects the threshold" $ do
    let rep = lagReport sampleLags
    classifyLag 2000 rep `shouldBe` LagWithinThreshold
    classifyLag 500 rep `shouldBe` LagExceeded
    classifyLag 0 (lagReport [LagInfo (TaskId 0 0) 10 10 False]) `shouldBe` LagCaughtUp


record_lag_sets_gauges :: Spec
record_lag_sets_gauges =
  it "recordLagReport publishes per-task and aggregate gauges" $ do
    reg <- newMetricsRegistry
    let rep = lagReport sampleLags
    recordLagReport reg rep
    mMax <- readGauge reg lagMaxGaugeName
    mTotal <- readGauge reg lagTotalGaugeName
    mTask <- readGauge reg (lagGaugeName (TaskId 1 0))
    mMax `shouldBe` Just 1000
    mTotal `shouldBe` Just 1020
    mTask `shouldBe` Just 1000
