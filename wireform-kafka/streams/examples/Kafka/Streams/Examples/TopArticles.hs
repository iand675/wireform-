{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Kafka.Streams.Examples.TopArticles
Description : Hopping-window count by industry

Mirror of @org.apache.kafka.streams.examples.wordcount.TopArticlesDemo@.
For each article-view event, count views per @(industry, article)@
in a hopping window of 1 hour advancing every 5 minutes. The output
is a stream of (industry, article, count) updates that downstream
consumers can rank.

Java (paraphrased):

@
KStream<String, Article> views = builder.stream("article-views");
views.groupBy((k, a) -> a.industry + "|" + a.article)
     .windowedBy(TimeWindows.of(Duration.ofHours(1))
                            .advanceBy(Duration.ofMinutes(5)))
     .count(Materialized.as("article-counts"))
     .toStream()
     .to("article-counts-stream");
@

Haskell (free-arrow). The windowed handle returned by
'F.countWindowed' is bridged into a @KStream (WindowedKey k) v@
with 'F.liftIO_' + 'Suppress.streamFromWindowedHandle', then the
window envelope is stripped via 'F.selectKey' before sinking.
-}
module Kafka.Streams.Examples.TopArticles (
  runDemo,
  topArticlesTopology,
  buildTopArticlesTopology,
) where

import Control.Category ((>>>))
import Data.ByteString.Char8 qualified as BSC
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Kafka.Streams
import Kafka.Streams.Materialized qualified as Mat
import Kafka.Streams.Topology qualified as Topo
import Kafka.Streams.Topology.Free qualified as F


topArticlesTopology :: F.Topology Void ()
topArticlesTopology =
  F.source @Text @Text "article-views"
    >>> F.groupBy (\r -> recordValue r)
    >>> F.windowedByTime (hoppingWindows (minutes 60) (minutes 5))
    >>> F.countWindowed countMat
    >>> F.streamFromWindowed
    >>> F.selectKey
      ( \r -> case recordKey r of
          Just (WindowedKey k _) -> k
          Nothing -> ""
      )
    >>> F.sink "article-counts-stream"
  where
    countMat :: Materialized Text Int64
    countMat =
      Mat.withValueSerde int64Serde $
        Mat.withKeySerde textSerde $
          Mat.materializedAs (storeName "article-counts")


buildTopArticlesTopology :: IO Topo.Topology
buildTopArticlesTopology = F.buildTopologyFrom topArticlesTopology


runDemo :: IO ()
runDemo = do
  putStrLn "=== TopArticlesDemo ==="
  topo <- buildTopArticlesTopology
  driver <- newDriver topo "top-articles-app"

  let view value tsMs =
        pipeInput
          driver
          (topicName "article-views")
          Nothing
          (BSC.pack (T.unpack value))
          (Timestamp tsMs)
          0
      m :: Int64 -> Int64
      m n = n * 60 * 1000

  -- Within the same 1-hour window:
  view "tech|haskell-rules" (m 0)
  view "tech|haskell-rules" (m 10)
  view "tech|haskell-rules" (m 30)
  view "tech|kafka-tips" (m 5)
  view "tech|kafka-tips" (m 25)
  view "finance|rates-explained" (m 15)
  view "finance|rates-explained" (m 50)

  out <- readOutput driver (topicName "article-counts-stream")
  let topByKey =
        foldr
          ( \cr acc ->
              let k = case crKey cr of
                    Just b -> BSC.unpack b
                    Nothing -> "<no-key>"
                  n = case deserialize int64Serde (crValueBytes cr) :: Either Text Int64 of
                    Right x -> x
                    Left _ -> 0
              in case lookup k acc of
                   Just prev | prev >= n -> acc
                   _ -> (k, n) : Prelude.filter ((/= k) . fst) acc
          )
          []
          out
  putStrLn
    ( "Top counts per industry|article ("
        <> show (length topByKey)
        <> " keys, "
        <> show (length out)
        <> " raw updates):"
    )
  mapM_
    (\(k, n) -> putStrLn ("  " <> k <> " = " <> show n))
    (reverse topByKey)
  closeDriver driver
