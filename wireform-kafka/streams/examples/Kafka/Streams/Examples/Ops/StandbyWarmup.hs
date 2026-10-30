{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Streams.Examples.Ops.StandbyWarmup
Description : Warm failover with standbys, and warmups with probing rebalances

Part 1: two instances of a counting application with
@num.standby.replicas = 1@. Each runs one task and keeps the other's
stores current as a standby ('localTaskLags' shows its lag reach 0).
When one instance goes away the other takes its task over and restores
next to nothing.

Part 2 (KIP-441): one instance has been counting for a while when a
second, empty one joins. Moving a task to it would mean a full
restore, so the assignor leaves both tasks where they are caught up
and gives the newcomer a warmup. At the next probing rebalance
(@probing.rebalance.interval.ms@) the warmup has caught up
(@acceptable.recovery.lag@) and the task moves.
-}
module Kafka.Streams.Examples.Ops.StandbyWarmup (
  runDemo,
) where

import Control.Monad (forM_)
import Data.Text qualified as T
import Kafka.Streams.Examples.Ops.Helpers
import Kafka.Streams.Imperative
import Kafka.Streams.Metrics (restoredRecordsTotal, standbyRecordsTotal)
import Kafka.Streams.Runtime.MockBroker


produceVisits :: MockBroker -> Int -> IO ()
produceVisits broker n =
  forM_ [1 .. n] $ \i -> mockProduce broker "in" (Just (bytes ("user-" <> T.pack (show (i `mod` 10))))) (Just "visit") 0


showLags :: String -> KafkaStreams -> IO ()
showLags who ks = do
  r <- localTaskLags ks
  case r of
    Left e -> bullet ("    " <> who <> ": " <> T.unpack e)
    Right lags ->
      forM_ lags $ \l ->
        bullet
          ( "    "
              <> who
              <> (if l.lagStandby then " standby " else " active  ")
              <> taskLabel l.lagTaskId
              <> ": "
              <> show l.lagCurrentOffset
              <> " / "
              <> show l.lagEndOffset
          )


runDemo :: IO ()
runDemo = do
  section "StandbyWarmupDemo"
  topo <- countTopo

  bullet "Part 1: standbys"
  withDemoDirs 2 $ \[d1, d2] -> do
    broker <- newMockBroker (Just 1)
    mockCreateTopic broker "in" 2
    let cfg d = (demoConfig "ops-standby" d) {numStandbyReplicas = 1}
    a <- startInstance broker (cfg d1) topo
    b <- startInstance broker (cfg d2) topo
    _ <- waitUntil [a, b] ((\x y -> x == 1 && y == 1) <$> (length <$> ownedPartitions a) <*> (length <$> ownedPartitions b))
    produceVisits broker 400
    _ <- waitUntil [a, b] ((== 400) . length <$> outputValues broker "out")
    _ <- waitUntil [a, b] (either (const False) (all (\l -> l.lagCurrentOffset == l.lagEndOffset)) <$> localTaskLags a)
    bullet "  task state (position / changelog end):"
    showLags "a" a
    showLags "b" b
    before <- counterOf a restoredRecordsTotal
    bullet "  b goes away; a takes its task over"
    stopInstance b
    _ <- waitUntil [a] ((== 2) . length <$> ownedPartitions a)
    after <- counterOf a restoredRecordsTotal
    bullet ("  a restored " <> show (after - before) <> " changelog records for it (its standby had the rest)")
    stopInstance a

  bullet "Part 2: warmups and a probing rebalance"
  withDemoDirs 2 $ \[d1, d2] -> do
    broker <- newMockBroker (Just 1)
    mockCreateTopic broker "in" 2
    let cfg d = (demoConfig "ops-warmup" d) {acceptableRecoveryLag = 10, probingRebalanceIntervalMs = 500}
    a <- startInstance broker (cfg d1) topo
    produceVisits broker 400
    _ <- waitUntil [a] ((== 400) . length <$> outputValues broker "out")
    b <- startInstance broker (cfg d2) topo
    _ <- waitUntil [a, b] (either (const False) (not . null) <$> localTaskLags b)
    bullet "  b joined with no state: a keeps both tasks, b warms one up"
    assignmentsOf [("a", a), ("b", b)] >>= printAssignments "  right after b joined"
    showLags "b" b
    moved <- waitUntil [a, b] ((== 1) . length <$> ownedPartitions b)
    assignmentsOf [("a", a), ("b", b)] >>= printAssignments ("  after the probing rebalance (moved: " <> show moved <> ")")
    warm <- counterOf b standbyRecordsTotal
    restored <- counterOf b restoredRecordsTotal
    bullet ("  b applied " <> show warm <> " records as a warmup and restored " <> show restored <> " once active")
    mapM_ stopInstance [b, a]
