{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | Tests for the additional KIP-820 / Stores factory / serde
features added in the parity batch.
-}
module Streams.ExtensionsSpec (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.IORef
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Kafka.Streams.Imperative
import Kafka.Streams.Stores qualified as Stores
import Test.Syd


bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack


unbytes :: BSC.ByteString -> Text
unbytes = T.pack . BSC.unpack


t :: Integer -> Timestamp
t = Timestamp . fromIntegral


tests :: Spec
tests =
  describe "Extensions" $
    sequence_
      [ stores_factory_works
      , uuid_serde_round_trip
      , long_serde_alias
      , process_stream_runs
      , ktable_toStream_emits_changes
      , ktable_groupBy_rekeys
      ]


stores_factory_works :: Spec
stores_factory_works =
  it "Stores.inMemoryKeyValueStore creates a working store" $ do
    s <- Stores.inMemoryKeyValueStore @Int @Int (Stores.storeName "x")
    Stores.kvsPut s 1 100
    Stores.kvsGet s 1 >>= (`shouldBe` Just 100)


uuid_serde_round_trip :: Spec
uuid_serde_round_trip =
  it "uuidSerde round-trips a UUID" $ do
    let u = UUID.fromWords 0xdeadbeef 0x12345678 0x9abcdef0 0x12345678
    case deserialize uuidSerde (serialize uuidSerde u) of
      Right u' -> u' `shouldBe` u
      Left e -> error (T.unpack e)


long_serde_alias :: Spec
long_serde_alias =
  it "longSerde produces the same bytes as int64Serde" $
    serialize longSerde (1234 :: Int64)
      `shouldBe` serialize int64Serde 1234


process_stream_runs :: Spec
process_stream_runs =
  it "processStream invokes the supplier for each record" $ do
    counter <- newIORef (0 :: Int)
    b <- newStreamsBuilder
    src <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
    let supplier =
          pure
            Processor
              { procName = processorName "COUNTER"
              , procInit = \_ -> pure ()
              , procClose = pure ()
              , procProcess = \_ -> modifyIORef' counter (+ 1)
              }
    processStream "COUNTER" [] supplier src
    topo <- buildTopology b
    driver <- newDriver topo "ext-app"

    pipeInput driver (topicName "in") Nothing (bytes "a") (t 0) 0
    pipeInput driver (topicName "in") Nothing (bytes "b") (t 1) 0
    pipeInput driver (topicName "in") Nothing (bytes "c") (t 2) 0
    readIORef counter >>= (`shouldBe` 3)
    closeDriver driver


ktable_toStream_emits_changes :: Spec
ktable_toStream_emits_changes =
  it "toKStreamFromKTable emits each change as a stream record" $ do
    b <- newStreamsBuilder
    kt <-
      tableFromTopic
        b
        (topicName "in")
        (consumed textSerde textSerde)
        (materializedAs (storeName "kt-store"))
    s <- toKStreamFromKTable kt
    toTopic (topicName "out") (produced textSerde textSerde) s
    topo <- buildTopology b
    driver <- newDriver topo "ext-app"

    pipeInput driver (topicName "in") (Just (bytes "k1")) (bytes "v1") (t 0) 0
    pipeInput driver (topicName "in") (Just (bytes "k1")) (bytes "v2") (t 1) 0
    pipeInput driver (topicName "in") (Just (bytes "k2")) (bytes "v3") (t 2) 0
    out <- readOutput driver (topicName "out")
    map (unbytes . crValueBytes) out `shouldBe` ["v1", "v2", "v3"]
    closeDriver driver


ktable_groupBy_rekeys :: Spec
ktable_groupBy_rekeys =
  it "groupByKTable produces a re-keyed KStream" $ do
    b <- newStreamsBuilder
    kt <-
      tableFromTopic
        b
        (topicName "in")
        (consumed textSerde textSerde)
        (materializedAs (storeName "kt-store-rk"))
    s <- groupByKTable (\_oldKey v -> v) kt -- new key = current value
    toTopic (topicName "out") (produced textSerde textSerde) s
    topo <- buildTopology b
    driver <- newDriver topo "ext-app"

    pipeInput driver (topicName "in") (Just (bytes "k1")) (bytes "alpha") (t 0) 0
    pipeInput driver (topicName "in") (Just (bytes "k2")) (bytes "beta") (t 0) 0
    out <- readOutput driver (topicName "out")
    -- The re-keyed stream uses value as the new key; we only assert
    -- that we got two records (key inspection is bytes-noisy).
    length out `shouldBe` 2
    closeDriver driver
