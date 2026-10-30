{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Streams.Examples.LineSplit
Description : Stateless flatMap — split lines into words

Mirror of @org.apache.kafka.streams.examples.wordcount.LineSplitDemo@.
Reads lines from one topic, splits each line on whitespace, writes
one record per word to another topic.

Java:

@
builder.<String,String>stream("streams-plaintext-input")
       .concatMapValues(value -> Arrays.asList(value.split("\\W+")))
       .to("streams-linesplit-output");
@

Haskell (written as a "Kafka.Streams.Topology.Free" value):
-}
module Kafka.Streams.Examples.LineSplit (
  runDemo,
  lineSplitTopology,
  buildLineSplitTopology,
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


lineSplitTopology :: F.Topology Void ()
lineSplitTopology =
  F.source @Text @Text "streams-plaintext-input"
    >>> F.concatMapValues (T.words :: Text -> [Text])
    >>> F.sink "streams-linesplit-output"


buildLineSplitTopology :: IO Topo.Topology
buildLineSplitTopology = F.buildTopologyFrom lineSplitTopology


runDemo :: RunMode -> IO ()
runDemo mode = do
  putStrLn "=== LineSplitDemo ==="
  let inTopic = topicName "streams-plaintext-input"
      outTopic = topicName "streams-linesplit-output"
  withDemoDriver
    mode
    "line-split-app"
    buildLineSplitTopology
    [DemoTopic inTopic 1]
    [DemoTopic outTopic 1]
    $ \dd -> do
      mapM_
        (sendLine dd inTopic)
        [ "all streams lead to kafka"
        , "hello kafka streams"
        , "join kafka summit"
        ]
      ddAdvance dd (Timestamp 0)
      out <- ddRead dd outTopic
      putStrLn ("Words emitted (" <> show (length out) <> "):")
      mapM_ (\cr -> putStrLn ("  " <> BSC.unpack (crValueBytes cr))) out
  where
    sendLine d inTopic line =
      ddSend
        d
        inTopic
        Nothing
        (BSC.pack (T.unpack line))
        (Timestamp 0)
        0
