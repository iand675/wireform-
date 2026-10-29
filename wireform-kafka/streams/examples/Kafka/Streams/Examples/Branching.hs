{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Streams.Examples.Branching
Description : Split a stream by predicate (KIP-418)

The KIP-418 'F.split' operator takes a list of named
predicates and produces one output stream per predicate, plus
(optionally) a default branch for records no predicate
matched.

Java:

@
Map<String, KStream<K,V>> branches =
  stream.split(Named.as("ROUTE-"))
        .branch((k, v) -> v.contains("error"), Branched.as("errors"))
        .branch((k, v) -> v.contains("warn"),  Branched.as("warns"))
        .defaultBranch(Branched.as("info"));
@

Haskell (free-arrow). 'F.split' lands a
@'Data.Map.Strict.Map' Text (KStream k v)@ on the wire; the
routing fragment below destructures it and sinks each named
branch to its own topic.
-}
module Kafka.Streams.Examples.Branching (
  runDemo,
  branchingTopology,
  buildBranchingTopology,
) where

import Control.Category ((>>>))
import Data.ByteString.Char8 qualified as BSC
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Void (Void)
import Kafka.Streams
import Kafka.Streams.KStream qualified as KS
import Kafka.Streams.Topology qualified as Topo
import Kafka.Streams.Topology.Free qualified as F


branchingTopology :: F.Topology Void ()
branchingTopology =
  F.source "logs"
    >>> F.split
      [ F.splitBranch
          "errors"
          (\r -> "error" `T.isInfixOf` recordValue r)
      , F.splitBranch
          "warns"
          (\r -> "warn" `T.isInfixOf` recordValue r)
      ]
      (Just "info")
    >>> F.liftIO_
      "route-branches"
      ( \_b branches -> do
          let sinkBranch name topic =
                case Map.lookup name branches of
                  Just s ->
                    KS.toTopic
                      (topicName topic)
                      (produced textSerde textSerde)
                      s
                  Nothing ->
                    error $ "missing branch " <> T.unpack name
          sinkBranch "errors" "logs-errors"
          sinkBranch "warns" "logs-warns"
          sinkBranch "info" "logs-info"
      )


buildBranchingTopology :: IO Topo.Topology
buildBranchingTopology = F.buildTopologyFrom branchingTopology


runDemo :: IO ()
runDemo = do
  putStrLn "=== BranchingDemo ==="
  topo <- buildBranchingTopology
  driver <- newDriver topo "branching-app"

  let send v =
        pipeInput
          driver
          (topicName "logs")
          Nothing
          (BSC.pack (T.unpack v))
          (Timestamp 0)
          0
  mapM_
    send
    [ "error: connection lost"
    , "warn: slow query"
    , "info: heartbeat"
    , "error: disk full"
    , "all good"
    ]

  let tap topic = do
        rs <- readOutput driver (topicName topic)
        putStrLn
          ( "  "
              <> T.unpack topic
              <> " ("
              <> show (length rs)
              <> "):"
          )
        mapM_ (\cr -> putStrLn ("    " <> BSC.unpack (crValueBytes cr))) rs
  mapM_ tap ["logs-errors", "logs-warns", "logs-info"]
  closeDriver driver
