{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Streams.CogroupSpec (tests) where

import Data.ByteString.Char8 qualified as BSC
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Streams.Imperative
import Test.Syd


bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack


i64Bytes :: Int64 -> BSC.ByteString
i64Bytes = serialize int64Serde


t :: Integer -> Timestamp
t = Timestamp . fromIntegral


tests :: Spec
tests =
  describe "Cogroup" $
    sequence_
      [ cogroup_two_sources_share_state
      , cogroup_three_sources
      ]


cogroup_two_sources_share_state :: Spec
cogroup_two_sources_share_state =
  it "cogroup of two text streams shares the aggregator state" $ do
    b <- newStreamsBuilder
    s1 <- streamFromTopic b (topicName "in1") (consumed textSerde textSerde)
    s2 <- streamFromTopic b (topicName "in2") (consumed textSerde textSerde)
    let g = grouped textSerde textSerde
        kg1 = groupByKey g s1
        kg2 = groupByKey g s2
    let cgs0 = cogroup kg1 (\_ v acc -> acc <> "/" <> v)
        cgs = addCogrouped cgs0 kg2 (\_ v acc -> acc <> "+" <> v)
    table <- aggregateCogrouped (pure (T.pack "")) materialized cgs
    topo <- buildTopology b
    driver <- newDriver topo "cog-app"

    pipeInput driver (topicName "in1") (Just (bytes "k")) (bytes "a") (t 0) 0
    pipeInput driver (topicName "in2") (Just (bytes "k")) (bytes "b") (t 1) 0
    pipeInput driver (topicName "in1") (Just (bytes "k")) (bytes "c") (t 2) 0

    Just kvs <- getKeyValueStore @Text @Text driver (ktableStore table)
    -- Expected: "" -> /a -> /a+b -> /a+b/c
    kvsGet kvs "k" >>= (`shouldBe` Just "/a+b/c")
    closeDriver driver


cogroup_three_sources :: Spec
cogroup_three_sources =
  it "cogroup with three int streams sums into one Int64" $ do
    b <- newStreamsBuilder
    s1 <- streamFromTopic b (topicName "x") (consumed textSerde int64Serde)
    s2 <- streamFromTopic b (topicName "y") (consumed textSerde int64Serde)
    s3 <- streamFromTopic b (topicName "z") (consumed textSerde int64Serde)
    let g = grouped textSerde int64Serde
        kg1 = groupByKey g s1
        kg2 = groupByKey g s2
        kg3 = groupByKey g s3
    let cgs =
          addCogrouped
            ( addCogrouped
                (cogroup kg1 (\_ v a -> a + v))
                kg2
                (\_ v a -> a + 10 * v)
            )
            kg3
            (\_ v a -> a + 100 * v)
    table <- aggregateCogrouped (pure (0 :: Int64)) materialized cgs
    topo <- buildTopology b
    driver <- newDriver topo "cog-app"

    pipeInput driver (topicName "x") (Just (bytes "k")) (i64Bytes 1) (t 0) 0
    pipeInput driver (topicName "y") (Just (bytes "k")) (i64Bytes 2) (t 1) 0
    pipeInput driver (topicName "z") (Just (bytes "k")) (i64Bytes 3) (t 2) 0
    pipeInput driver (topicName "x") (Just (bytes "k")) (i64Bytes 4) (t 3) 0

    Just kvs <- getKeyValueStore @Text @Int64 driver (ktableStore table)
    -- 0 + 1 (x) + 20 (y) + 300 (z) + 4 (x) = 325
    kvsGet kvs "k" >>= (`shouldBe` Just 325)
    closeDriver driver
