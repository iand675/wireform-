{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | End-to-end tests using 'TopologyTestDriver'. These tests exercise
the entire engine — sources, processors, sinks, state stores — without
a broker.
-}
module Streams.DriverSpec (tests) where

import Data.ByteString.Char8 qualified as BSC
import Data.IORef
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Streams.Imperative
import Test.Syd


tests :: Spec
tests =
  describe "Driver" $
    sequence_
      [ source_to_sink_passthrough
      , filter_then_sink
      , map_values
      , flatmap_values
      , merge_two_streams
      , branch_three_ways
      , peek_observes_records
      , map_keys_then_sink
      ]


----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack


unbytes :: BSC.ByteString -> Text
unbytes = T.pack . BSC.unpack


t0 :: Timestamp
t0 = Timestamp 0


----------------------------------------------------------------------
-- 1. plain source -> sink
----------------------------------------------------------------------

source_to_sink_passthrough :: Spec
source_to_sink_passthrough = it "source -> sink passthrough" $ do
  b <- newStreamsBuilder
  s <-
    streamFromTopic
      b
      (topicName "in")
      (consumed textSerde textSerde)
  toTopic (topicName "out") (produced textSerde textSerde) s
  topo <- buildTopology b
  driver <- newDriver topo "test-app"

  pipeInput driver (topicName "in") (Just (bytes "k1")) (bytes "v1") t0 0
  pipeInput driver (topicName "in") (Just (bytes "k2")) (bytes "v2") t0 0
  out <- readOutput driver (topicName "out")
  map (fmap unbytes . crKey) out `shouldBe` [Just "k1", Just "k2"]
  map (unbytes . crValueBytes) out `shouldBe` ["v1", "v2"]
  closeDriver driver


----------------------------------------------------------------------
-- 2. filter
----------------------------------------------------------------------

filter_then_sink :: Spec
filter_then_sink = it "filter drops records" $ do
  b <- newStreamsBuilder
  s <-
    streamFromTopic
      b
      (topicName "in")
      (consumed textSerde textSerde)
  s' <- filterStream (\r -> recordValue r /= "skip") s
  toTopic (topicName "out") (produced textSerde textSerde) s'
  topo <- buildTopology b
  driver <- newDriver topo "test-app"

  pipeInput driver (topicName "in") Nothing (bytes "keep1") t0 0
  pipeInput driver (topicName "in") Nothing (bytes "skip") t0 0
  pipeInput driver (topicName "in") Nothing (bytes "keep2") t0 0

  out <- readOutput driver (topicName "out")
  map (unbytes . crValueBytes) out `shouldBe` ["keep1", "keep2"]
  closeDriver driver


----------------------------------------------------------------------
-- 3. mapValues
----------------------------------------------------------------------

map_values :: Spec
map_values = it "mapValues transforms each value" $ do
  b <- newStreamsBuilder
  s <-
    streamFromTopic
      b
      (topicName "in")
      (consumed textSerde textSerde)
  s' <- mapValues T.toUpper s
  toTopic (topicName "out") (produced textSerde textSerde) s'
  topo <- buildTopology b
  driver <- newDriver topo "test-app"

  pipeInput driver (topicName "in") Nothing (bytes "hello") t0 0
  pipeInput driver (topicName "in") Nothing (bytes "world") t0 0

  out <- readOutput driver (topicName "out")
  map (unbytes . crValueBytes) out `shouldBe` ["HELLO", "WORLD"]
  closeDriver driver


----------------------------------------------------------------------
-- 4. concatMapValues
----------------------------------------------------------------------

flatmap_values :: Spec
flatmap_values = it "concatMapValues splits each record" $ do
  b <- newStreamsBuilder
  s <-
    streamFromTopic
      b
      (topicName "in")
      (consumed textSerde textSerde)
  s' <- concatMapValues (T.words) s
  toTopic (topicName "out") (produced textSerde textSerde) s'
  topo <- buildTopology b
  driver <- newDriver topo "test-app"

  pipeInput driver (topicName "in") Nothing (bytes "hello world how") t0 0
  pipeInput driver (topicName "in") Nothing (bytes "are you") t0 0

  out <- readOutput driver (topicName "out")
  map (unbytes . crValueBytes) out `shouldBe` ["hello", "world", "how", "are", "you"]
  closeDriver driver


----------------------------------------------------------------------
-- 5. mergeStreams
----------------------------------------------------------------------

merge_two_streams :: Spec
merge_two_streams = it "merge interleaves both streams" $ do
  b <- newStreamsBuilder
  s1 <- streamFromTopic b (topicName "in1") (consumed textSerde textSerde)
  s2 <- streamFromTopic b (topicName "in2") (consumed textSerde textSerde)
  merged <- mergeStreams s1 s2
  toTopic (topicName "out") (produced textSerde textSerde) merged
  topo <- buildTopology b
  driver <- newDriver topo "test-app"

  pipeInput driver (topicName "in1") Nothing (bytes "from-1a") t0 0
  pipeInput driver (topicName "in2") Nothing (bytes "from-2a") t0 0
  pipeInput driver (topicName "in1") Nothing (bytes "from-1b") t0 0

  out <- readOutput driver (topicName "out")
  map (unbytes . crValueBytes) out `shouldBe` ["from-1a", "from-2a", "from-1b"]
  closeDriver driver


----------------------------------------------------------------------
-- 6. branchStream
----------------------------------------------------------------------

branch_three_ways :: Spec
branch_three_ways = it "branch routes by predicate" $ do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  branches <-
    branchStream
      [ \r -> T.isPrefixOf "a" (recordValue r)
      , \r -> T.isPrefixOf "b" (recordValue r)
      , \_ -> True
      ]
      s
  case branches of
    [a, bb, other] -> do
      toTopic (topicName "out-a") (produced textSerde textSerde) a
      toTopic (topicName "out-b") (produced textSerde textSerde) bb
      toTopic (topicName "out-other") (produced textSerde textSerde) other
    _ -> error "expected 3 branches"
  topo <- buildTopology b
  driver <- newDriver topo "test-app"

  mapM_
    (\v -> pipeInput driver (topicName "in") Nothing (bytes v) t0 0)
    ["alpha", "bravo", "charlie", "able", "banana", "delta"]

  outA <- readOutput driver (topicName "out-a")
  outB <- readOutput driver (topicName "out-b")
  outO <- readOutput driver (topicName "out-other")

  map (unbytes . crValueBytes) outA `shouldBe` ["alpha", "able"]
  map (unbytes . crValueBytes) outB `shouldBe` ["bravo", "banana"]
  map (unbytes . crValueBytes) outO `shouldBe` ["charlie", "delta"]
  closeDriver driver


----------------------------------------------------------------------
-- 7. peekStream
----------------------------------------------------------------------

peek_observes_records :: Spec
peek_observes_records = it "peek runs side-effect, passes through" $ do
  observed <- newIORef ([] :: [Text])
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  s' <-
    peekStream
      (\r -> modifyIORef' observed (recordValue r :))
      s
  toTopic (topicName "out") (produced textSerde textSerde) s'
  topo <- buildTopology b
  driver <- newDriver topo "test-app"

  mapM_
    (\v -> pipeInput driver (topicName "in") Nothing (bytes v) t0 0)
    ["a", "b", "c"]

  out <- readOutput driver (topicName "out")
  map (unbytes . crValueBytes) out `shouldBe` ["a", "b", "c"]
  obs <- readIORef observed
  reverse obs `shouldBe` ["a", "b", "c"]
  closeDriver driver


----------------------------------------------------------------------
-- 8. mapKeyValue
----------------------------------------------------------------------

map_keys_then_sink :: Spec
map_keys_then_sink = it "mapKeyValue rewrites both" $ do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  -- 'Int' has no default 'HasSerde' (the built-in is Int64);
  -- supply an Int serde explicitly via mapKeyValueWith.
  let intSerde =
        Kafka.Streams.Imperative.imap
          (fromIntegral @Int @Int64)
          (fromIntegral @Int64 @Int)
          int64Serde
  s' <-
    mapKeyValueWith
      textSerde
      intSerde
      (\k v -> (T.reverse k, T.length v))
      s
  toTopic
    (topicName "out")
    (produced textSerde intSerde)
    s'
  topo <- buildTopology b
  driver <- newDriver topo "test-app"

  pipeInput driver (topicName "in") (Just (bytes "abc")) (bytes "hello") t0 0
  pipeInput driver (topicName "in") (Just (bytes "xy")) (bytes "hi") t0 0

  out <- readOutput driver (topicName "out")
  let kvs = map (\cr -> (fmap unbytes (crKey cr), crValueBytes cr)) out
  map fst kvs `shouldBe` [Just "cba", Just "yx"]
  -- Output value is Int64 big-endian; just check length matches.
  map (BSC.length . snd) kvs `shouldBe` [8, 8]
  closeDriver driver
