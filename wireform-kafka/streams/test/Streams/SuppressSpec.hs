{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Streams.SuppressSpec (tests) where

import Data.ByteString.Char8 qualified as BSC
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Streams.Imperative
import Test.Syd


bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack


t :: Integer -> Timestamp
t = Timestamp . fromIntegral


tests :: Spec
tests =
  describe "Suppress" $
    sequence_
      [ suppress_time_limit_debounces
      , suppress_time_limit_emits_after_limit
      , suppress_time_limit_first_seen_is_sticky
      ]


suppress_time_limit_debounces :: Spec
suppress_time_limit_debounces =
  it "suppressUntilTimeLimit holds updates within the window" $ do
    b <- newStreamsBuilder
    src <-
      streamFromTopic
        b
        (topicName "in")
        (consumed textSerde textSerde)
    suppressed <- suppressUntilTimeLimit (millis 100) src
    toTopic (topicName "out") (produced textSerde textSerde) suppressed
    topo <- buildTopology b
    driver <- newDriver topo "supp-app"

    -- Three updates inside 100ms: only the latest survives, but
    -- nothing is emitted yet (we're still inside the limit window).
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v1") (t 0) 0
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v2") (t 50) 0
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v3") (t 99) 0

    out0 <- readOutput driver (topicName "out")
    -- Within the 100ms window from t=0, no emission.
    map (T.pack . BSC.unpack . crValueBytes) out0 `shouldBe` []
    closeDriver driver


suppress_time_limit_emits_after_limit :: Spec
suppress_time_limit_emits_after_limit =
  it "suppressUntilTimeLimit emits once stream-time crosses the limit" $ do
    b <- newStreamsBuilder
    src <-
      streamFromTopic
        b
        (topicName "in")
        (consumed textSerde textSerde)
    suppressed <- suppressUntilTimeLimit (millis 100) src
    toTopic (topicName "out") (produced textSerde textSerde) suppressed
    topo <- buildTopology b
    driver <- newDriver topo "supp-app"

    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "first") (t 0) 0
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "second") (t 50) 0
    -- This record is at t=200, past the limit of the key's first
    -- buffered record (t=0): as on the JVM it is buffered first, so the
    -- key's latest value, "third", goes out, at its own timestamp.
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "third") (t 200) 0

    out <- readOutput driver (topicName "out")
    map (T.pack . BSC.unpack . crValueBytes) out `shouldBe` ["third"]
    map crTimestamp out `shouldBe` [t 200]
    closeDriver driver


suppress_time_limit_first_seen_is_sticky :: Spec
suppress_time_limit_first_seen_is_sticky =
  it "first-seen timestamp anchors the per-key debounce window" $ do
    b <- newStreamsBuilder
    src <-
      streamFromTopic
        b
        (topicName "in")
        (consumed textSerde textSerde)
    suppressed <- suppressUntilTimeLimit (millis 200) src
    toTopic (topicName "out") (produced textSerde textSerde) suppressed
    topo <- buildTopology b
    driver <- newDriver topo "supp-app"

    -- Key A's first record is at t=0; B's first is at t=50. The
    -- limit is 200ms, so A flushes at t>=200 and B flushes at
    -- t>=250.
    pipeInput driver (topicName "in") (Just (bytes "A")) (bytes "a1") (t 0) 0
    pipeInput driver (topicName "in") (Just (bytes "B")) (bytes "b1") (t 50) 0
    pipeInput driver (topicName "in") (Just (bytes "A")) (bytes "a2") (t 100) 0
    pipeInput driver (topicName "in") (Just (bytes "B")) (bytes "b2") (t 150) 0
    -- At t=210 A (waiting since 0) is due, with its latest value; B
    -- (waiting since 50) is not.
    pipeInput driver (topicName "in") (Just (bytes "A")) (bytes "a3") (t 210) 0
    out1 <- readOutput driver (topicName "out")
    map (T.pack . BSC.unpack . crValueBytes) out1 `shouldBe` ["a3"]

    -- At t=300 B is due; A starts a new wait with nothing buffered.
    pipeInput driver (topicName "in") (Just (bytes "B")) (bytes "b3") (t 300) 0
    out2 <- readOutput driver (topicName "out")
    map (T.pack . BSC.unpack . crValueBytes) out2 `shouldBe` ["b3"]
    closeDriver driver
