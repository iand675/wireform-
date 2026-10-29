{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Streams.AggregationSpec (tests) where

import Data.ByteString.Char8 qualified as BSC
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Streams.Imperative
import Test.Syd


tests :: Spec
tests =
  describe "Aggregation" $
    sequence_
      [ count_per_key
      , reduce_per_key
      , aggregate_with_init
      , windowed_count
      , windowed_count_drops_late_record_past_grace
      , windowed_count_accepts_late_record_within_grace
      ]


bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack


i64Bytes :: Int64 -> BSC.ByteString
i64Bytes = serialize int64Serde


t :: Int64 -> Timestamp
t = Timestamp


count_per_key :: Spec
count_per_key = it "count per key writes monotone counts" $ do
  b <- newStreamsBuilder
  src <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  let g = grouped textSerde textSerde
  let kgs = groupByKey g src
  table <- countStream materialized kgs
  -- Drive until the test driver ends.
  topo <- buildTopology b
  driver <- newDriver topo "agg-app"

  pipeInput driver (topicName "in") (Just (bytes "a")) (bytes "1") (t 0) 0
  pipeInput driver (topicName "in") (Just (bytes "a")) (bytes "2") (t 1) 0
  pipeInput driver (topicName "in") (Just (bytes "b")) (bytes "1") (t 2) 0
  pipeInput driver (topicName "in") (Just (bytes "a")) (bytes "3") (t 3) 0

  mStore <- getKeyValueStore @Text @Int64 driver (ktableStore table)
  case mStore of
    Nothing -> error "store missing"
    Just kvs -> do
      ca <- kvsGet kvs "a"
      cb <- kvsGet kvs "b"
      ca `shouldBe` Just 3
      cb `shouldBe` Just 1
  closeDriver driver


reduce_per_key :: Spec
reduce_per_key = it "reduce sums values per key" $ do
  b <- newStreamsBuilder
  src <-
    streamFromTopic
      b
      (topicName "in")
      (consumed textSerde int64Serde)
  let g = grouped textSerde int64Serde
      kgs = groupByKey g src
  table <- reduceStream (+) materialized kgs
  topo <- buildTopology b
  driver <- newDriver topo "reduce-app"

  pipeInput driver (topicName "in") (Just (bytes "a")) (i64Bytes 5) (t 0) 0
  pipeInput driver (topicName "in") (Just (bytes "a")) (i64Bytes 10) (t 1) 0
  pipeInput driver (topicName "in") (Just (bytes "b")) (i64Bytes 1) (t 2) 0
  pipeInput driver (topicName "in") (Just (bytes "a")) (i64Bytes 2) (t 3) 0
  pipeInput driver (topicName "in") (Just (bytes "b")) (i64Bytes 100) (t 4) 0

  mStore <- getKeyValueStore @Text @Int64 driver (ktableStore table)
  case mStore of
    Nothing -> error "store missing"
    Just kvs -> do
      kvsGet kvs "a" >>= (`shouldBe` Just 17)
      kvsGet kvs "b" >>= (`shouldBe` Just 101)
  closeDriver driver


aggregate_with_init :: Spec
aggregate_with_init = it "aggregate with non-trivial init" $ do
  b <- newStreamsBuilder
  src <-
    streamFromTopic
      b
      (topicName "in")
      (consumed textSerde int64Serde)
  let g = grouped textSerde int64Serde
      kgs = groupByKey g src
  table <-
    aggregateStream
      (pure (1 :: Int64)) -- start at 1, multiply by each value
      (\_ v acc -> acc * v)
      materialized
      kgs
  topo <- buildTopology b
  driver <- newDriver topo "agg-init-app"

  pipeInput driver (topicName "in") (Just (bytes "x")) (i64Bytes 2) (t 0) 0
  pipeInput driver (topicName "in") (Just (bytes "x")) (i64Bytes 3) (t 1) 0
  pipeInput driver (topicName "in") (Just (bytes "x")) (i64Bytes 5) (t 2) 0

  mStore <- getKeyValueStore @Text @Int64 driver (ktableStore table)
  case mStore of
    Nothing -> error "store missing"
    Just kvs -> kvsGet kvs "x" >>= (`shouldBe` Just 30)
  closeDriver driver


windowed_count :: Spec
windowed_count = it "tumbling windowed count" $ do
  b <- newStreamsBuilder
  src <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  let g = grouped textSerde textSerde
      kgs = groupByKey g src
      twks = windowedByTime (tumblingWindows (millis 100)) kgs
  table <- countWindowed materialized twks
  topo <- buildTopology b
  driver <- newDriver topo "wcount-app"

  -- 3 records in window [0, 100)
  pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v1") (t 10) 0
  pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v2") (t 50) 0
  pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v3") (t 99) 0
  -- 2 records in window [100, 200)
  pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v4") (t 150) 0
  pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v5") (t 199) 0

  mStore <- getWindowStore @Text @Int64 driver (wthStore table)
  case mStore of
    Nothing -> error "window store missing"
    Just ws -> do
      wsFetch ws "k" (Timestamp 0) >>= (`shouldBe` Just 3)
      wsFetch ws "k" (Timestamp 100) >>= (`shouldBe` Just 2)
  closeDriver driver


windowed_count_drops_late_record_past_grace :: Spec
windowed_count_drops_late_record_past_grace =
  it "records past windowEnd + grace are dropped from windowed agg" $ do
    b <- newStreamsBuilder
    src <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
    let g = grouped textSerde textSerde
        kgs = groupByKey g src
        ws =
          withWindowsRetention
            (millis 100_000)
            (withGracePeriod (millis 50) (tumblingWindows (millis 100)))
        twks = windowedByTime ws kgs
    table <- countWindowed materialized twks
    topo <- buildTopology b
    driver <- newDriver topo "wcount-grace-app"

    -- Window [0,100): two on-time records.
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "a") (t 10) 0
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "b") (t 50) 0
    -- Advance stream time well past 100 + 50 = 150.
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "z") (t 1000) 0
    -- Late record targeted at the [0,100) window: ts=80. Window end is 100;
    -- 100 + 50 = 150 < 1000, so the record is dropped.
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "late") (t 80) 0

    mStore <- getWindowStore @Text @Int64 driver (wthStore table)
    case mStore of
      Just ws_ -> wsFetch ws_ "k" (Timestamp 0) >>= (`shouldBe` Just 2)
      Nothing -> error "store missing"
    closeDriver driver


windowed_count_accepts_late_record_within_grace :: Spec
windowed_count_accepts_late_record_within_grace =
  it "records within the grace period are still aggregated" $ do
    b <- newStreamsBuilder
    src <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
    let g = grouped textSerde textSerde
        kgs = groupByKey g src
        ws = withGracePeriod (millis 200) (tumblingWindows (millis 100))
        twks = windowedByTime ws kgs
    table <- countWindowed materialized twks
    topo <- buildTopology b
    driver <- newDriver topo "wcount-grace2-app"

    -- Window [0,100): one on-time record.
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "a") (t 50) 0
    -- Advance stream time to 250 (window end is 100, grace=200, so 300 is the cutoff).
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "z") (t 250) 0
    -- Late record at ts=80 still within grace (window-end 100 + grace 200 = 300, cur 250 < 300).
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "late") (t 80) 0

    mStore <- getWindowStore @Text @Int64 driver (wthStore table)
    case mStore of
      Just ws_ -> wsFetch ws_ "k" (Timestamp 0) >>= (`shouldBe` Just 2)
      Nothing -> error "store missing"
    closeDriver driver
