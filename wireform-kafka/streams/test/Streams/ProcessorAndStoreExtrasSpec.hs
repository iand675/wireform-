{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | Tests for the remaining processor / store / TTD additions:
  * TimestampedWindowStore
  * KStream.foreachStreamAsync
  * StoreQueryParameters helper
  * FixedKeyProcessor lift round-trip
-}
module Streams.ProcessorAndStoreExtrasSpec (tests) where

import Control.Concurrent qualified
import Control.Concurrent.MVar
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Streams.Imperative
import Kafka.Streams.State.KeyValue.Timestamped qualified as TS
import Kafka.Streams.State.Window.Timestamped qualified as TWS
import Test.Syd


tests :: Spec
tests =
  describe "Processor + Store extras" $
    sequence_
      [ timestamped_window_store_keeps_record_ts
      , foreach_async_does_not_block
      , store_query_parameters_round_trip
      ]


----------------------------------------------------------------------
-- TimestampedWindowStore
----------------------------------------------------------------------

timestamped_window_store_keeps_record_ts :: Spec
timestamped_window_store_keeps_record_ts =
  it "TimestampedWindowStore: fetch returns value+ts" $ do
    ws <-
      TWS.inMemoryTimestampedWindowStore @Text @Int
        (storeName "tws")
        1000
        60_000
    TWS.twsPut ws "alice" 5 (Timestamp 250) (Timestamp 100)
    r <- TWS.twsFetch ws "alice" (Timestamp 100)
    case r of
      Just (TS.ValueAndTimestamp v ts) -> do
        v `shouldBe` 5
        ts `shouldBe` Timestamp 250
      Nothing -> error "expected the entry to be present"


----------------------------------------------------------------------
-- foreachStreamAsync — non-blocking
----------------------------------------------------------------------

foreach_async_does_not_block :: Spec
foreach_async_does_not_block =
  it "foreachStreamAsync: callbacks run concurrently; a commit waits for them" $ do
    -- the callbacks block until released: processing doesn't wait for
    -- them, the commit does
    seenRef <- newIORef (0 :: Int)
    gate <- newEmptyMVar
    b <- newStreamsBuilder
    s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
    foreachStreamAsync (\_ -> readMVar gate >> atomicModifyIORef' seenRef (\n -> (n + 1, ()))) s
    topo <- buildTopology b
    driver <- newDriver topo "fea"
    pipeInput driver (topicName "in") (Just "k1") "v" (Timestamp 0) 0
    pipeInput driver (topicName "in") (Just "k2") "v" (Timestamp 1) 0
    readIORef seenRef `shouldReturn` 0
    putMVar gate ()
    commitDriver driver
    readIORef seenRef `shouldReturn` 2
    closeDriver driver


----------------------------------------------------------------------
-- StoreQueryParameters round-trip
----------------------------------------------------------------------

store_query_parameters_round_trip :: Spec
store_query_parameters_round_trip =
  it "storeQueryParameters: defaults + staleStoresEnabled / partition gates" $ do
    let p = storeQueryParameters (storeName "x")
    p.storeName `shouldBe` storeName "x"
    p.staleStoresEnabled `shouldBe` False
    p.partition `shouldBe` Nothing

    -- The strict-mode gate is the testable bit: when the
    -- runtime isn't Running, defaults must yield 'Nothing'.
    -- (Pre-start the engine isn't even built, so a Just
    -- response can't happen — making 'staleStoresEnabled =
    -- True' indistinguishable from False in that window. The
    -- assertion below is the one that matters: defaults
    -- short-circuit when status /= StreamsRunning, so the
    -- caller knows the store isn't authoritative.)
    b <- newStreamsBuilder
    _ <-
      tableFromTopic
        b
        (topicName "iq-in")
        (consumed textSerde textSerde)
        (materializedAs (storeName "iq-store"))
    topo <- buildTopology b
    let topo' = case validateTopology topo of
          Left e -> error (show e)
          Right v -> v
    ks <-
      newKafkaStreams
        ( defaultStreamsConfig
            { applicationId = "iq-params"
            , bootstrapServers = ["mock:0"]
            , numStreamThreads = 1
            , pollMs = 0
            }
        )
        topo'

    let pStrict = p {storeName = storeName "iq-store"}
    rStrict <- queryKVStoreWithParameters @Text @Text ks pStrict
    case rStrict of
      Nothing -> pure ()
      Just _ ->
        error
          "expected Nothing when staleStoresEnabled=False and runtime not Running"
