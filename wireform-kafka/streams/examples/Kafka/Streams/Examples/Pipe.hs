{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Streams.Examples.Pipe
Description : "Hello world" — copy from one topic to another

Mirror of @org.apache.kafka.streams.examples.pipe.PipeDemo@.
The simplest possible streams app: subscribe to @streams-plaintext-input@,
write everything to @streams-pipe-output@.

This demo runs identically against the in-process
'Kafka.Streams.Driver.TopologyTestDriver' and against a real
broker. Pick the mode at runtime with @--broker host:port@ on
the command line, or by setting @WIREFORM_KAFKA_BROKER@.
-}
module Kafka.Streams.Examples.Pipe (
  runDemo,
  pipeTopology,
  buildPipeTopology,
) where

import Control.Category ((>>>))
import Data.ByteString.Char8 qualified as BSC
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Kafka.Streams
import Kafka.Streams.Examples.Runner
import Kafka.Streams.Topology qualified as Topo
import Kafka.Streams.Topology.Free qualified as F


{- | The pipe topology as a first-class 'F.Topology' value. No
'IO' — composition is pure data, ready to be inspected or
optimised before compilation.
-}
pipeTopology :: F.Topology Void ()
pipeTopology =
  F.source @Text @Text "streams-plaintext-input"
    >>> F.sink "streams-pipe-output"


{- | Build the imperative 'Topology' graph from the
'pipeTopology' AST.
-}
buildPipeTopology :: IO Topo.Topology
buildPipeTopology = F.buildTopologyFrom pipeTopology


runDemo :: RunMode -> IO ()
runDemo mode = do
  putStrLn "=== PipeDemo ==="
  let inTopic = topicName "streams-plaintext-input"
      outTopic = topicName "streams-pipe-output"
  withDemoDriver
    mode
    "pipe-demo-app"
    buildPipeTopology
    [DemoTopic inTopic 1]
    [DemoTopic outTopic 1]
    $ \dd -> do
      let send (k, v) =
            ddSend
              dd
              inTopic
              (Just (BSC.pack (T.unpack k)))
              (BSC.pack (T.unpack v))
              (Timestamp 0)
              0
      mapM_
        send
        [ ("k1" :: Text, "all streams lead to kafka" :: Text)
        , ("k2", "hello kafka streams")
        , ("k3", "join kafka summit")
        ]
      -- Give the broker a moment to flush; in-memory is sync.
      ddAdvance dd (Timestamp 0)
      out <- ddRead dd outTopic
      putStrLn
        ( "Records published to streams-pipe-output ("
            <> show (length out)
            <> "):"
        )
      mapM_ printRec out
  where
    printRec :: CollectedRecord -> IO ()
    printRec cr =
      putStrLn $
        "  "
          <> show (crKey cr)
          <> " -> "
          <> BSC.unpack (crValueBytes cr)
