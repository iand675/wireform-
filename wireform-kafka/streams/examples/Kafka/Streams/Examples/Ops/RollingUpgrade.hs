{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Streams.Examples.Ops.RollingUpgrade
Description : Rolling deploy: replace instances one at a time, keep traffic flowing

A Kubernetes-style rolling deploy of a stateful (counting) application
with three replicas, @maxUnavailable: 1@. Each step stops one instance,
sends a batch while it is away (its tasks move to the others, warm,
because every task has a standby), and starts its replacement on the
same state directory.

The replacement comes back with no in-memory state, so the assignor
keeps its tasks on caught-up instances and gives it warmups
(@acceptable.recovery.lag@); at the next probing rebalance the tasks
move back to it, already restored. Exactly once, so the final counts
are exact.
-}
module Kafka.Streams.Examples.Ops.RollingUpgrade (
  runDemo,
) where

import Control.Monad (foldM, forM, forM_)
import Data.IORef
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Streams.Examples.Ops.Helpers
import Kafka.Streams.Imperative
import Kafka.Streams.Metrics (restoredRecordsTotal, standbyRecordsTotal)
import Kafka.Streams.Runtime.Driver (DriverRecord (..))
import Kafka.Streams.Runtime.MockBroker


keys :: [Text]
keys = map (\i -> "user-" <> T.pack (show i)) [0 .. 59 :: Int]


-- | The latest count per key in @out@.
latestCounts :: MockBroker -> IO (Map.Map Text Int64)
latestCounts broker = do
  rs <- mockVisibleRecords broker "out"
  pure . Map.fromList $
    concatMap
      ( \r -> case (r.drKey, r.drValue) of
          (Just k, Just v) -> either (const []) (\n -> [(unbytes k, n)]) (deserialize int64Serde v)
          _ -> []
      )
      rs


runDemo :: IO ()
runDemo = withDemoDirs 3 $ \dirs -> do
  section "RollingUpgradeDemo"
  broker <- newMockBroker (Just 1)
  mockCreateTopic broker "in" 6
  topo <- countTopo
  let cfg d =
        (demoConfig "ops-rolling" d)
          { numStandbyReplicas = 1
          , processingGuarantee = ExactlyOnceV2
          , acceptableRecoveryLag = 5
          , probingRebalanceIntervalMs = 200
          }
      label i = T.pack ("i" <> show i)
      sendRound r = forM_ keys $ \k -> mockProduce broker "in" (Just (bytes k)) (Just "visit") r
      allCounted n = all (== n) . (\m -> map (\k -> Map.findWithDefault 0 k m) keys) <$> latestCounts broker
  started <- forM (zip [0 :: Int ..] dirs) $ \(i, d) -> (,) (label i) <$> startInstance broker (cfg d) topo
  ref <- newIORef started
  let current = readIORef ref
      everyone = map snd <$> current
  _ <- everyone >>= \ss -> waitUntil ss (everyoneOwns ss)
  current >>= assignmentsOf >>= printAssignments "Initial assignment (3 replicas)"

  rounds <- foldM
    ( \n (i, d) -> do
        bullet ("Step " <> show i <> ": replacing " <> T.unpack (label i))
        old <- (Map.! label i) . Map.fromList <$> current
        stopInstance old
        -- traffic keeps flowing while it is away
        sendRound (fromIntegral n)
        others <- filter ((/= label i) . fst) <$> current
        _ <- waitUntil (map snd others) (allCounted (n + 1))
        new <- startInstance broker (cfg d) topo
        modifyIORef' ref (map (\(l, s) -> if l == label i then (l, new) else (l, s)))
        -- warmups first, then the probing rebalance hands its tasks back
        ss <- everyone
        _ <- waitUntil ss ((>= 2) . length <$> ownedPartitions new)
        restored <- counterOf new restoredRecordsTotal
        warmedUp <- counterOf new standbyRecordsTotal
        owned <- length <$> ownedPartitions new
        bullet
          ( "  "
              <> T.unpack (label i)
              <> " is back with "
              <> show owned
              <> " partitions: it applied "
              <> show warmedUp
              <> " changelog records as a warmup / standby, and restored only "
              <> show restored
              <> " once active"
          )
        pure (n + 1)
    )
    (0 :: Int64)
    (zip [0 :: Int ..] dirs)

  current >>= assignmentsOf >>= printAssignments "After the rollout"
  sendRound rounds
  ss <- everyone
  done <- waitUntil ss (allCounted (rounds + 1))
  bullet ("Every key counted exactly " <> show (rounds + 1) <> " times: " <> show done)
  mapM_ stopInstance ss
