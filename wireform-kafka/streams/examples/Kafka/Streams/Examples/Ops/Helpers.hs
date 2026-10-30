{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Streams.Examples.Ops.Helpers
Description : Shared bits for the operational demos

Every @Ops@ demo runs a deliberately-trivial topology so the
interesting thing on screen is the /operational/ behaviour
(partition rebalance, crash failover, rolling upgrade, standby
warmup, etc.) and not whatever happens to be in the topology
DAG. This module exposes the shared topology builders and a
couple of formatting helpers so each demo stays short.
-}
module Kafka.Streams.Examples.Ops.Helpers (
  passthroughTopo,
  countTopo,
  bytes,
  unbytes,
  ts0,
  section,
  bullet,
  printAssignments,

  -- * Instances on the in-memory broker
  demoConfig,
  withDemoDirs,
  startInstance,
  stopInstance,
  waitUntil,
  assignmentsOf,
  outputValues,
  sendTo,
  counterOf,
  taskLabel,
  everyoneOwns,
) where

import Data.ByteString.Char8 qualified as BSC
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Client.Consumer qualified as KC
import Kafka.Streams.Imperative
import Kafka.Streams.Processor (TaskId (..))
import Kafka.Streams.Metrics (MetricValue (..))
import Kafka.Streams.Runtime.Driver (DriverRecord (..))
import Kafka.Streams.Runtime.MockBroker
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)


{- | One-source one-sink passthrough: @in -> out@. The Riffle docs
call this "the boring topology"; it's enough to make every
subscribed instance own at least one input partition.
-}
passthroughTopo :: IO TopologyValid
passthroughTopo = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  toTopic (topicName "out") (produced textSerde textSerde) s
  topo <- buildTopology b
  case validateTopology topo of
    Left err -> error (show err)
    Right v -> pure v


-- | Counts per key, @in -> counts -> out@: state worth keeping warm.
countTopo :: IO TopologyValid
countTopo = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  counts <- countStream (materializedAs (storeName "counts")) (groupByKey (grouped textSerde textSerde) s)
  out <- toKStreamFromKTable counts
  toTopic (topicName "out") (produced textSerde int64Serde) out
  topo <- buildTopology b
  either (error . show) pure (validateTopology topo)


bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack


unbytes :: BSC.ByteString -> Text
unbytes = T.pack . BSC.unpack


ts0 :: Timestamp
ts0 = Timestamp 0


-- | @=== title ===@ banner that mirrors the existing DSL demos.
section :: String -> IO ()
section t = putStrLn ("=== " <> t <> " ===")


-- | Indented bullet line used for human-readable progress output.
bullet :: String -> IO ()
bullet s = putStrLn ("  " <> s)


-- | @label -> [topic:partition]@ for each instance.
printAssignments :: String -> [(Text, [KC.TopicPartition])] -> IO ()
printAssignments heading asg = do
  bullet (heading <> ":")
  mapM_
    (\(label, parts) -> bullet ("    " <> T.unpack label <> " -> " <> show (map (\tp -> T.unpack tp.topic <> ":" <> show tp.partition) parts)))
    asg


----------------------------------------------------------------------
-- Instances on the in-memory broker
----------------------------------------------------------------------

-- | A fast loop and frequent commits, state under @dir@.
demoConfig :: Text -> FilePath -> StreamsConfig
demoConfig app dir =
  defaultStreamsConfig
    { applicationId = app
    , bootstrapServers = ["mock:0"]
    , pollMs = 2
    , commitIntervalMs = Just 20
    , stateDir = dir
    }


-- | @n@ fresh state directories.
withDemoDirs :: Int -> ([FilePath] -> IO a) -> IO a
withDemoDirs n k = withSystemTempDirectory "ops-demo" $ \root ->
  k (map (\i -> root </> show i) [1 .. n])


-- | Start an instance of @topo@ against @broker@ (the real runtime, on the in-memory driver).
startInstance :: MockBroker -> StreamsConfig -> TopologyValid -> IO KafkaStreams
startInstance broker cfg topo = do
  ks <- newKafkaStreams cfg topo
  mockDriverFor broker cfg >>= startKafkaStreamsWith ks
  pure ks


stopInstance :: KafkaStreams -> IO ()
stopInstance ks = closeKafkaStreams ks >> awaitState ks StreamsClosed


{- | Wait (on the instances' loop ticks, no sleeping) until @cond@
holds; 'False' if it still doesn't after a bounded number of ticks.
-}
waitUntil :: [KafkaStreams] -> IO Bool -> IO Bool
waitUntil kss cond = go (8_000 :: Int)
  where
    go 0 = cond
    go n = do
      ok <- cond
      if ok
        then pure True
        else do
          sts <- mapM streamsStatus kss
          case filter (\(_, st) -> st == StreamsRunning || st == StreamsRebalancing) (zip kss sts) of
            [] -> cond
            ((ks, _) : _) -> awaitTicks ks 1 >> go (n - 1)


assignmentsOf :: [(Text, KafkaStreams)] -> IO [(Text, [KC.TopicPartition])]
assignmentsOf = traverse (\(l, ks) -> (,) l <$> ownedPartitions ks)


-- | The committed values in @topic@ (what a read_committed consumer sees), as text.
outputValues :: MockBroker -> Text -> IO [Text]
outputValues broker topic = mapMaybe (fmap unbytes . (.drValue)) <$> mockVisibleRecords broker topic


-- | Append @value@ (no key) to partition @p@ of @topic@.
sendTo :: MockBroker -> Text -> Int -> Text -> IO ()
sendTo broker topic p value =
  either error (const (pure ())) =<< mockProduceTo broker topic (Just (fromIntegral p)) Nothing (Just (bytes value)) 0 []


-- | @\<subtopology\>_\<partition\>@, as task directories are named.
taskLabel :: TaskId -> String
taskLabel (TaskId s p) = show s <> "_" <> show p


-- | Every instance owns at least one partition (the group has settled over all of them).
everyoneOwns :: [KafkaStreams] -> IO Bool
everyoneOwns kss = all (not . null) <$> traverse ownedPartitions kss


-- | A counter of 'streamsMetrics' (0 when absent).
counterOf :: KafkaStreams -> Text -> IO Int64
counterOf ks nm = do
  m <- streamsMetrics ks
  pure $ case Map.lookup nm m of
    Just (MVCounter n) -> n
    _ -> 0
