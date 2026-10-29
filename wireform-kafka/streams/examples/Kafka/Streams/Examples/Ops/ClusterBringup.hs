{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Streams.Examples.Ops.ClusterBringup
Description : Start a multi-instance cluster on the in-memory broker

Three instances of one application (the real runtime, each on its own
driver over one 'Kafka.Streams.Runtime.MockBroker') subscribe to a
6-partition input topic. The demo prints the partitions each instance
owns after the group settles, then drives one record per partition
through the cluster and confirms each lands in @out@ exactly once.

This is the analogue of three replicas of a Kafka Streams application
coming up cold against a fresh broker.
-}
module Kafka.Streams.Examples.Ops.ClusterBringup (
  runDemo,
) where

import Control.Monad (forM, forM_)
import Data.List (sort)
import Data.Text qualified as T
import Kafka.Streams.Examples.Ops.Helpers
import Kafka.Streams.Imperative
import Kafka.Streams.Runtime.MockBroker


runDemo :: IO ()
runDemo = withDemoDirs 3 $ \dirs -> do
  section "ClusterBringupDemo"
  broker <- newMockBroker (Just 1)
  mockCreateTopic broker "in" 6
  topo <- passthroughTopo
  instances <- forM (zip [0 :: Int ..] dirs) $ \(i, d) ->
    (,) (T.pack ("i" <> show i)) <$> startInstance broker (demoConfig "ops-bringup" d) topo
  let streams = map snd instances
  settled <- waitUntil streams (everyoneOwns streams)
  bullet ("Group settled: " <> show settled)
  assignmentsOf instances >>= printAssignments "Assignment"
  -- one record per partition
  forM_ [0 .. 5 :: Int] $ \p ->
    sendTo broker "in" p (T.pack ("rec-p" <> show p))
  _ <- waitUntil streams ((== 6) . length <$> outputValues broker "out")
  outs <- outputValues broker "out"
  bullet ("Delivered (each exactly once): " <> show (sort outs))
  mapM_ stopInstance streams
