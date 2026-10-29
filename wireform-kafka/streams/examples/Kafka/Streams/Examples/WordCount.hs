{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Kafka.Streams.Examples.WordCount
Description : Classic word-count — concatMapValues + groupBy + count

Mirror of @org.apache.kafka.streams.examples.wordcount.WordCountDemo@,
the canonical "hello world" of stateful streams processing.

Java:

@
KStream<String, String> source = builder.stream("streams-plaintext-input");
source.concatMapValues(v -> Arrays.asList(v.toLowerCase().split("\\W+")))
      .groupBy((key, word) -> word)
      .count(Materialized.as("counts-store"))
      .toStream()
      .to("streams-wordcount-output", Produced.with(stringSerde, longSerde));
@

Haskell, written against the free-arrow 'F.Topology'. The
pipeline reads as a chain of 'Control.Category.(>>>)'-composed
combinators and only at the very edge — 'F.buildTopologyFrom' —
becomes an imperative graph.
-}
module Kafka.Streams.Examples.WordCount (
  runDemo,
  wordCountTopology,
  buildWordCountTopology,
) where

import Control.Category ((>>>))
import Data.ByteString.Char8 qualified as BSC
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Kafka.Streams
import Kafka.Streams.Examples.Runner
import Kafka.Streams.Materialized qualified as Mat
import Kafka.Streams.Topology qualified as Topo
import Kafka.Streams.Topology.Free qualified as F


wordCountTopology :: F.Topology Void ()
wordCountTopology =
  F.source @Text @Text "streams-plaintext-input"
    >>> F.concatMapValues (T.words . T.toLower :: Text -> [Text])
    >>> F.groupBy
      (\r -> recordValue r)
    >>> F.count countMat
    >>> F.toStream
    >>> F.sink "streams-wordcount-output"
  where
    countMat :: Materialized Text Int64
    countMat =
      Mat.withValueSerde int64Serde $
        Mat.withKeySerde textSerde $
          Mat.materializedAs (storeName "counts-store")


buildWordCountTopology :: IO Topo.Topology
buildWordCountTopology = F.buildTopologyFrom wordCountTopology


runDemo :: RunMode -> IO ()
runDemo mode = do
  putStrLn "=== WordCountDemo ==="
  let inTopic = topicName "streams-plaintext-input"
      outTopic = topicName "streams-wordcount-output"
  withDemoDriver
    mode
    "word-count-app"
    buildWordCountTopology
    [DemoTopic inTopic 1]
    [DemoTopic outTopic 1]
    $ \dd -> do
      mapM_
        (sendLine dd inTopic)
        [ "all streams lead to kafka"
        , "hello kafka streams"
        , "join kafka summit"
        , "kafka streams kafka summit"
        ]
      ddAdvance dd (Timestamp 0)
      out <- ddRead dd outTopic
      putStrLn ("Word-count updates emitted (" <> show (length out) <> "):")
      mapM_ printRec out
  where
    sendLine d inTopic line =
      ddSend
        d
        inTopic
        Nothing
        (BSC.pack (T.unpack line))
        (Timestamp 0)
        0

    printRec :: CollectedRecord -> IO ()
    printRec cr =
      let wd = case crKey cr of
            Just k -> BSC.unpack k
            Nothing -> "<no-key>"
          cnt = case deserialize int64Serde (crValueBytes cr) :: Either Text Int64 of
            Right n -> show n
            Left err -> "?(" <> T.unpack err <> ")"
      in putStrLn ("  " <> wd <> " = " <> cnt)
