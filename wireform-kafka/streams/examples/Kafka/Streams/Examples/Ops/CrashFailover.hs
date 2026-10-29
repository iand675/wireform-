{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Streams.Examples.Ops.CrashFailover
Description : One instance dies; its peer inherits its tasks

Two instances share a 4-partition input. After the first batch @i0@
goes away; the group rebalances and @i1@ takes all four tasks over,
starting each from the last committed offset, so the second batch (and
anything @i0@ hadn't committed) flows through @i1@. Nothing is lost
(at least once: something may be delivered twice).

Equivalent to a pod dying mid-flight in a Kafka Streams StatefulSet.
-}
module Kafka.Streams.Examples.Ops.CrashFailover (
  runDemo,
) where

import Data.List qualified as L
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Streams.Examples.Ops.Helpers
import Kafka.Streams.Imperative
import Kafka.Streams.Runtime.MockBroker


runDemo :: IO ()
runDemo = withDemoDirs 2 $ \[d0, d1] -> do
  section "CrashFailoverDemo"
  broker <- newMockBroker (Just 1)
  mockCreateTopic broker "in" 4
  topo <- passthroughTopo
  i0 <- startInstance broker (demoConfig "ops-crash" d0) topo
  i1 <- startInstance broker (demoConfig "ops-crash" d1) topo
  let both = [("i0", i0), ("i1", i1)]
  _ <- waitUntil [i0, i1] (everyoneOwns [i0, i1])
  assignmentsOf both >>= printAssignments "Before the crash"

  let batch1, batch2 :: [Text]
      batch1 = map (\p -> T.pack ("a" <> show p)) [0 .. 3 :: Int]
      batch2 = map (\p -> T.pack ("b" <> show p)) [0 .. 3 :: Int]
  mapM_ (\(p, v) -> sendTo broker "in" p v) (zip [0 ..] batch1)
  _ <- waitUntil [i0, i1] ((>= 4) . length <$> outputValues broker "out")

  bullet "i0 goes away..."
  stopInstance i0
  _ <- waitUntil [i1] ((== 4) . length <$> ownedPartitions i1)
  assignmentsOf [("i1", i1)] >>= printAssignments "After the crash"

  mapM_ (\(p, v) -> sendTo broker "in" p v) (zip [0 ..] batch2)
  _ <- waitUntil [i1] (Set.isSubsetOf (Set.fromList batch2) . Set.fromList <$> outputValues broker "out")
  delivered <- Set.fromList <$> outputValues broker "out"
  let expected = Set.fromList (batch1 <> batch2)
  bullet ("Records observed after failover: " <> show (Set.size delivered))
  bullet ("Missing: " <> show (L.sort (Set.toList (expected `Set.difference` delivered))))
  stopInstance i1
