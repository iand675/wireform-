{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Kafka.Streams.Examples.FraudDetection
Description : Session windows — bursty-activity fraud detection

Mirror of the canonical "session window" demo: count user
activity per session (gap-based clustering of events) and flag
sessions with above-threshold counts.

Java (paraphrased):

@
KStream<String, String> activity = builder.stream("user-activity");
activity.groupByKey()
        .windowedBy(SessionWindows.with(Duration.ofMinutes(5)))
        .count()
        .toStream()
        .filter((k, v) -> v != null && v >= 10)
        .to("suspicious-sessions");
@

Haskell (free-arrow). The session-windowed handle isn't a
'KStream' yet — we pin one on 'swthNode' inside 'F.liftIO_' so
the rest of the pipeline composes through the regular Free
combinators.
-}
module Kafka.Streams.Examples.FraudDetection (
  runDemo,
  fraudTopology,
  buildFraudTopology,
) where

import Control.Category ((>>>))
import Data.ByteString.Char8 qualified as BSC
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Kafka.Streams
import Kafka.Streams.Materialized qualified as Mat
import Kafka.Streams.Topology qualified as Topo
import Kafka.Streams.Topology.Free qualified as F


fraudTopology :: F.Topology Void ()
fraudTopology =
  F.source @Text @Text "user-activity"
    >>> F.groupByKey
    >>> F.windowedBySession (sessionWindows (minutes 5))
    >>> F.countSessionWindowed countMat
    >>> F.streamFromSessionWindowed
    -- a session's count; Nothing when it merged into a later one
    >>> F.filter (\r -> maybe False (>= 10) (recordValue r))
    >>> F.mapValues (fromMaybe 0)
    >>> F.sink "suspicious-sessions"
  where
    countMat :: Materialized Text Int64
    countMat =
      Mat.withValueSerde int64Serde $
        Mat.withKeySerde textSerde $
          Mat.materializedAs (storeName "session-counts")


buildFraudTopology :: IO Topo.Topology
buildFraudTopology = F.buildTopologyFrom fraudTopology


runDemo :: IO ()
runDemo = do
  putStrLn "=== FraudDetectionDemo ==="
  topo <- buildFraudTopology
  driver <- newDriver topo "fraud-app"

  let act u tsMs =
        pipeInput
          driver
          (topicName "user-activity")
          (Just (BSC.pack (T.unpack u)))
          (BSC.pack "click")
          (Timestamp tsMs)
          0
      m :: Int64 -> Int64
      m n = n * 60 * 1000

  -- alice: 12 clicks within 5 minutes -> single session, flagged.
  mapM_ (act "alice") (map (\i -> i * 5_000) [0 .. 11])
  -- bob: 3 clicks 10 minutes apart -> three single-event sessions,
  -- none flagged.
  act "bob" (m 0)
  act "bob" (m 10)
  act "bob" (m 20)

  advanceDriverStreamTime driver (Timestamp (m 30))

  out <- readOutput driver (topicName "suspicious-sessions")
  putStrLn ("Suspicious-session updates (" <> show (length out) <> "):")
  mapM_ printRec out
  closeDriver driver
  where
    printRec cr =
      let k = case crKey cr of
            Just b -> BSC.unpack b
            Nothing -> "<no-key>"
          v = case deserialize int64Serde (crValueBytes cr) :: Either Text Int64 of
            Right n -> show n
            Left err -> "?(" <> T.unpack err <> ")"
      in putStrLn ("  " <> k <> " : " <> v <> " events")
