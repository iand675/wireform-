{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Streams.Observability.Lag
Description : Aggregate per-task lag into an operational report

'Kafka.Streams.Runtime.localTaskLags' reads 'LagInfo' rows (each
local task's state against its changelogs: running, restoring or
standby). This module turns that flat list into an operational
summary — total / max lag, how many tasks
are caught up, a severity classification against a threshold — and
renders it as JSON (for a @\/lag@ endpoint) or a fixed-width table
(for a CLI / log line).

It also bridges lag into 'Kafka.Streams.Metrics' via
'recordLagReport', so lag flows out through the same
OpenTelemetry path as every other streams metric (see
"Kafka.Streams.Observability.OpenTelemetry") instead of needing a
bespoke exporter.
-}
module Kafka.Streams.Observability.Lag (
  -- * Per-task lag
  TaskLag (..),
  taskLagOf,

  -- * Report
  LagReport (..),
  lagReport,

  -- * Severity
  LagSeverity (..),
  classifyLag,

  -- * Rendering
  lagReportJson,
  renderLagTable,

  -- * Metrics bridge
  recordLagReport,
  lagGaugeName,
  lagMaxGaugeName,
  lagTotalGaugeName,
) where

import Data.Aeson ((.=))
import Data.Aeson qualified as A
import Data.Int (Int64)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Streams.Metrics (MetricsRegistry, setGauge)
import Kafka.Streams.Processor (TaskId, taskIdText)
import Kafka.Streams.Runtime (
  LagInfo (..),
 )


----------------------------------------------------------------------
-- Per-task lag
----------------------------------------------------------------------

{- | One task's lag, with the @behind@ delta precomputed. @behind@ is
clamped at zero — a current offset ahead of the reported end
offset (possible mid-fetch) reads as caught up, not negative.
-}
data TaskLag = TaskLag
  { taskLagId :: !TaskId
  , taskLagCurrent :: !Int64
  , taskLagEnd :: !Int64
  , taskLagBehind :: !Int64
  }
  deriving stock (Eq, Show)


taskLagOf :: LagInfo -> TaskLag
taskLagOf li =
  TaskLag
    { taskLagId = lagTaskId li
    , taskLagCurrent = lagCurrentOffset li
    , taskLagEnd = lagEndOffset li
    , taskLagBehind = max 0 (lagEndOffset li - lagCurrentOffset li)
    }


----------------------------------------------------------------------
-- Report
----------------------------------------------------------------------

{- | An aggregated view over every task's lag. 'lagReportTasks' is
sorted by 'TaskId' for stable rendering.
-}
data LagReport = LagReport
  { lagReportTasks :: ![TaskLag]
  , lagReportTotalBehind :: !Int64
  , lagReportMaxBehind :: !Int64
  , lagReportTaskCount :: !Int
  , lagReportCaughtUp :: !Int
  }
  deriving stock (Eq, Show)


-- | Build a 'LagReport' from raw lag rows.
lagReport :: [LagInfo] -> LagReport
lagReport infos =
  LagReport
    { lagReportTasks = tasks
    , lagReportTotalBehind = total
    , lagReportMaxBehind = mx
    , lagReportTaskCount = length tasks
    , lagReportCaughtUp = caughtUp
    }
  where
    tasks = List.sortOn taskLagId (map taskLagOf infos)
    total = List.foldl' (\acc tl -> acc + taskLagBehind tl) 0 tasks
    mx = List.foldl' (\acc tl -> max acc (taskLagBehind tl)) 0 tasks
    caughtUp =
      List.foldl'
        (\acc tl -> if taskLagBehind tl == 0 then acc + 1 else acc)
        0
        tasks


----------------------------------------------------------------------
-- Severity
----------------------------------------------------------------------

{- | A coarse health classification for a report, relative to a
caller-supplied acceptable-lag threshold.
-}
data LagSeverity
  = -- | Every task is at its end offset.
    LagCaughtUp
  | -- | Some lag, but the max is within budget.
    LagWithinThreshold
  | -- | The max lag exceeds the threshold.
    LagExceeded
  deriving stock (Eq, Show, Ord)


-- | Classify a report against a maximum acceptable per-task lag.
classifyLag :: Int64 -> LagReport -> LagSeverity
classifyLag threshold rep
  | lagReportMaxBehind rep == 0 = LagCaughtUp
  | lagReportMaxBehind rep <= threshold = LagWithinThreshold
  | otherwise = LagExceeded


----------------------------------------------------------------------
-- Rendering
----------------------------------------------------------------------

-- | Render a report as a versioned JSON object.
lagReportJson :: LagReport -> A.Value
lagReportJson rep =
  A.object
    [ "version" .= (1 :: Int)
    , "taskCount" .= lagReportTaskCount rep
    , "caughtUp" .= lagReportCaughtUp rep
    , "totalBehind" .= lagReportTotalBehind rep
    , "maxBehind" .= lagReportMaxBehind rep
    , "tasks" .= map taskLagJson (lagReportTasks rep)
    ]


taskLagJson :: TaskLag -> A.Value
taskLagJson tl =
  A.object
    [ "task" .= taskIdText (taskLagId tl)
    , "currentOffset" .= taskLagCurrent tl
    , "endOffset" .= taskLagEnd tl
    , "behind" .= taskLagBehind tl
    ]


{- | Render a report as a fixed-width text table with a trailing
summary line. Suitable for a CLI or a single structured log entry.
-}
renderLagTable :: LagReport -> Text
renderLagTable rep =
  T.unlines (header : separator : map row (lagReportTasks rep) <> [summary])
  where
    header = pad 12 "TASK" <> pad 14 "CURRENT" <> pad 14 "END" <> "BEHIND"
    separator = T.replicate 48 "-"
    row tl =
      pad 12 (taskIdText (taskLagId tl))
        <> pad 14 (T.pack (show (taskLagCurrent tl)))
        <> pad 14 (T.pack (show (taskLagEnd tl)))
        <> T.pack (show (taskLagBehind tl))
    summary =
      "tasks="
        <> T.pack (show (lagReportTaskCount rep))
        <> " caughtUp="
        <> T.pack (show (lagReportCaughtUp rep))
        <> " maxBehind="
        <> T.pack (show (lagReportMaxBehind rep))
        <> " totalBehind="
        <> T.pack (show (lagReportTotalBehind rep))
    pad n t = T.justifyLeft n ' ' t


----------------------------------------------------------------------
-- Metrics bridge
----------------------------------------------------------------------

{- | Gauge name for a single task's lag, in the JVM
@stream-task-metrics@ namespace.
-}
lagGaugeName :: TaskId -> Text
lagGaugeName tid = "stream-task-metrics:records-lag:" <> taskIdText tid


-- | Gauge name for the maximum per-task lag.
lagMaxGaugeName :: Text
lagMaxGaugeName = "stream-task-metrics:records-lag-max"


-- | Gauge name for the summed lag across tasks.
lagTotalGaugeName :: Text
lagTotalGaugeName = "stream-task-metrics:records-lag-total"


{- | Publish a report into the metrics registry as gauges (one per
task plus the max / total aggregates). Once registered with
"Kafka.Streams.Observability.OpenTelemetry", these surface as OTel
observable gauges automatically.
-}
recordLagReport :: MetricsRegistry -> LagReport -> IO ()
recordLagReport reg rep = do
  mapM_ recordTask (lagReportTasks rep)
  setGauge reg lagMaxGaugeName (fromIntegral (lagReportMaxBehind rep))
  setGauge reg lagTotalGaugeName (fromIntegral (lagReportTotalBehind rep))
  where
    recordTask tl =
      setGauge
        reg
        (lagGaugeName (taskLagId tl))
        (fromIntegral (taskLagBehind tl))
