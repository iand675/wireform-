{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Streams.JoinSpec (tests) where

import Data.ByteString.Char8 qualified as BSC
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Streams.Imperative
import Kafka.Streams.Joined (symmetricJoinWindows)
import Test.Syd
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.List (sort, sortOn)
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Kafka.Streams.Joined (JoinWindows (..))
import Test.Syd.Hedgehog ()


tests :: Spec
tests =
  describe "Joins" $
    sequence_
      [ kstream_ktable_inner
      , kstream_ktable_left
      , kstream_ktable_table_updates_propagate
      , -- Stream-stream window joins
        kstream_kstream_inner_within_window
      , kstream_kstream_inner_outside_window
      , kstream_kstream_left_unmatched_emits_nothing
      , kstream_kstream_outer_emits_both_sides
      , -- Table-table joins
        ktable_ktable_inner_join
      , ktable_ktable_left_join
      , ktable_ktable_outer_join
      , -- GlobalKTable joins
        kstream_global_ktable_inner
      , kstream_global_ktable_left_no_match_emits_nothing
      , -- Foreign-key KTable-KTable joins
        fk_join_inner_basic
      , fk_join_changing_fk_unsubscribes
      , fk_join_right_update_re_emits
      , fk_left_join_emits_when_no_right
      , -- Window joins against a model
        it "window inner join: every pair within the windows, in any arrival order (model)" prop_inner_join_model
        , it "window left/outer join: matches, then the unmatched once their window closes (model)" prop_outer_join_model
      ]


bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack


unbytes :: BSC.ByteString -> Text
unbytes = T.pack . BSC.unpack


t :: Integer -> Timestamp
t = Timestamp . fromIntegral


kstream_ktable_inner :: Spec
kstream_ktable_inner =
  it "KStream-KTable inner join drops unmatched stream records" $ do
    b <- newStreamsBuilder
    -- KTable side
    tab <-
      tableFromTopic
        b
        (topicName "users")
        (consumed textSerde textSerde)
        (materializedAs (storeName "users-store"))
    -- KStream side
    s <-
      streamFromTopic
        b
        (topicName "events")
        (consumed textSerde textSerde)
    joined <-
      joinKStreamKTable
        (\ev usr -> usr <> ":" <> ev)
        (Kafka.Streams.Imperative.joined textSerde textSerde textSerde)
        s
        tab
    toTopic (topicName "out") (produced textSerde textSerde) joined
    topo <- buildTopology b
    driver <- newDriver topo "join-app"

    -- Populate table.
    pipeInput driver (topicName "users") (Just (bytes "u1")) (bytes "alice") (t 0) 0
    pipeInput driver (topicName "users") (Just (bytes "u2")) (bytes "bob") (t 0) 0
    -- Stream events.
    pipeInput driver (topicName "events") (Just (bytes "u1")) (bytes "click") (t 1) 0
    pipeInput driver (topicName "events") (Just (bytes "u3")) (bytes "ignore") (t 1) 0
    pipeInput driver (topicName "events") (Just (bytes "u2")) (bytes "scroll") (t 2) 0

    out <- readOutput driver (topicName "out")
    map (unbytes . crValueBytes) out `shouldBe` ["alice:click", "bob:scroll"]
    closeDriver driver


kstream_ktable_left :: Spec
kstream_ktable_left =
  it "KStream-KTable left join always emits" $ do
    b <- newStreamsBuilder
    tab <-
      tableFromTopic
        b
        (topicName "users")
        (consumed textSerde textSerde)
        (materializedAs (storeName "users-store-l"))
    s <-
      streamFromTopic
        b
        (topicName "events")
        (consumed textSerde textSerde)
    j <-
      leftJoinKStreamKTable
        ( \ev mu -> case mu of
            Just u -> u <> ":" <> ev
            Nothing -> "<unknown>:" <> ev
        )
        (Kafka.Streams.Imperative.joined textSerde textSerde textSerde)
        s
        tab
    toTopic (topicName "out") (produced textSerde textSerde) j
    topo <- buildTopology b
    driver <- newDriver topo "ljoin-app"

    pipeInput driver (topicName "users") (Just (bytes "u1")) (bytes "alice") (t 0) 0
    pipeInput driver (topicName "events") (Just (bytes "u1")) (bytes "x") (t 1) 0
    pipeInput driver (topicName "events") (Just (bytes "u2")) (bytes "y") (t 1) 0

    out <- readOutput driver (topicName "out")
    map (unbytes . crValueBytes) out `shouldBe` ["alice:x", "<unknown>:y"]
    closeDriver driver


kstream_ktable_table_updates_propagate :: Spec
kstream_ktable_table_updates_propagate =
  it "KStream-KTable: table updates change subsequent join results" $ do
    b <- newStreamsBuilder
    tab <-
      tableFromTopic
        b
        (topicName "users")
        (consumed textSerde textSerde)
        (materializedAs (storeName "users-store-u"))
    s <-
      streamFromTopic
        b
        (topicName "events")
        (consumed textSerde textSerde)
    j <-
      joinKStreamKTable
        (\ev u -> u <> ":" <> ev)
        (Kafka.Streams.Imperative.joined textSerde textSerde textSerde)
        s
        tab
    toTopic (topicName "out") (produced textSerde textSerde) j
    topo <- buildTopology b
    driver <- newDriver topo "ujoin-app"

    pipeInput driver (topicName "users") (Just (bytes "u1")) (bytes "alice") (t 0) 0
    pipeInput driver (topicName "events") (Just (bytes "u1")) (bytes "x") (t 1) 0
    pipeInput driver (topicName "users") (Just (bytes "u1")) (bytes "ALICE") (t 2) 0
    pipeInput driver (topicName "events") (Just (bytes "u1")) (bytes "y") (t 3) 0

    out <- readOutput driver (topicName "out")
    map (unbytes . crValueBytes) out `shouldBe` ["alice:x", "ALICE:y"]
    closeDriver driver


----------------------------------------------------------------------
-- KStream-KStream window join tests
----------------------------------------------------------------------

-- A stream-stream join with both sides under the same key, where the
-- window covers the gap.
kstream_kstream_inner_within_window :: Spec
kstream_kstream_inner_within_window =
  it "KStream-KStream inner join: matches within window" $ do
    b <- newStreamsBuilder
    sl <-
      streamFromTopic
        b
        (topicName "left")
        (consumed textSerde textSerde)
    sr <-
      streamFromTopic
        b
        (topicName "right")
        (consumed textSerde textSerde)
    j <-
      joinKStreamKStream
        (\l r -> l <> "+" <> r)
        (symmetricJoinWindows (millis 100))
        (Kafka.Streams.Imperative.joined textSerde textSerde textSerde)
        sl
        sr
    toTopic (topicName "out") (produced textSerde textSerde) j
    topo <- buildTopology b
    driver <- newDriver topo "kskj-app"

    pipeInput driver (topicName "left") (Just (bytes "k")) (bytes "L1") (t 100) 0
    pipeInput driver (topicName "right") (Just (bytes "k")) (bytes "R1") (t 150) 0
    -- left at 200 should still match R1 (within +/-100 of 150, and 100 too)
    pipeInput driver (topicName "left") (Just (bytes "k")) (bytes "L2") (t 200) 0

    out <- readOutput driver (topicName "out")
    -- Order: L1 buffers left-side; R1 arrives, scans left, finds L1
    -- → "L1+R1"; L2 arrives, scans right, finds R1 → "L2+R1".
    map (unbytes . crValueBytes) out `shouldBe` ["L1+R1", "L2+R1"]
    closeDriver driver


-- Records outside the window must NOT match.
kstream_kstream_inner_outside_window :: Spec
kstream_kstream_inner_outside_window =
  it "KStream-KStream inner join: drops matches outside window" $ do
    b <- newStreamsBuilder
    sl <-
      streamFromTopic
        b
        (topicName "left")
        (consumed textSerde textSerde)
    sr <-
      streamFromTopic
        b
        (topicName "right")
        (consumed textSerde textSerde)
    j <-
      joinKStreamKStream
        (\l r -> l <> "+" <> r)
        (symmetricJoinWindows (millis 50))
        (Kafka.Streams.Imperative.joined textSerde textSerde textSerde)
        sl
        sr
    toTopic (topicName "out") (produced textSerde textSerde) j
    topo <- buildTopology b
    driver <- newDriver topo "kskj-app"

    pipeInput driver (topicName "left") (Just (bytes "k")) (bytes "L1") (t 0) 0
    -- Right at 200 is way outside the 50ms window of L1.
    pipeInput driver (topicName "right") (Just (bytes "k")) (bytes "R1") (t 200) 0

    out <- readOutput driver (topicName "out")
    length out `shouldBe` 0
    closeDriver driver


kstream_kstream_left_unmatched_emits_nothing :: Spec
kstream_kstream_left_unmatched_emits_nothing =
  it "KStream-KStream left join: unmatched left records emit Nothing" $ do
    b <- newStreamsBuilder
    sl <-
      streamFromTopic
        b
        (topicName "left")
        (consumed textSerde textSerde)
    sr <-
      streamFromTopic
        b
        (topicName "right")
        (consumed textSerde textSerde)
    j <-
      leftJoinKStreamKStream
        ( \l mr -> case mr of
            Just r -> l <> "+" <> r
            Nothing -> l <> "+<none>"
        )
        (symmetricJoinWindows (millis 50))
        (Kafka.Streams.Imperative.joined textSerde textSerde textSerde)
        sl
        sr
    toTopic (topicName "out") (produced textSerde textSerde) j
    topo <- buildTopology b
    driver <- newDriver topo "kskj-app"

    -- L1 has no right match yet: it is held, not emitted.
    pipeInput driver (topicName "left") (Just (bytes "k")) (bytes "L1") (t 0) 0
    -- R1 within window → finds L1 in the left store, emits "L1+R1";
    -- L1 is matched, so it never comes out unmatched.
    pipeInput driver (topicName "right") (Just (bytes "k")) (bytes "R1") (t 20) 0
    -- L2 within window of R1 → emits "L2+R1".
    pipeInput driver (topicName "left") (Just (bytes "k")) (bytes "L2") (t 30) 0
    -- L3 outside window of R1: held until its window (200 + 50) closes.
    pipeInput driver (topicName "left") (Just (bytes "k")) (bytes "L3") (t 200) 0
    out <- readOutput driver (topicName "out")
    map (unbytes . crValueBytes) out `shouldBe` ["L1+R1", "L2+R1"]
    -- a joined record carries the later timestamp
    map crTimestamp out `shouldBe` [t 20, t 30]
    -- stream time passes 250: L3 comes out unmatched, before L4 is joined
    pipeInput driver (topicName "left") (Just (bytes "k")) (bytes "L4") (t 251) 0
    out' <- readOutput driver (topicName "out")
    map (unbytes . crValueBytes) out' `shouldBe` ["L3+<none>"]
    map crTimestamp out' `shouldBe` [t 200]
    closeDriver driver


kstream_kstream_outer_emits_both_sides :: Spec
kstream_kstream_outer_emits_both_sides =
  it "KStream-KStream outer join: unmatched on either side emits Nothing" $ do
    b <- newStreamsBuilder
    sl <-
      streamFromTopic
        b
        (topicName "left")
        (consumed textSerde textSerde)
    sr <-
      streamFromTopic
        b
        (topicName "right")
        (consumed textSerde textSerde)
    j <-
      outerJoinKStreamKStream
        ( \ml mr ->
            let l = maybe "<>" id ml
                r = maybe "<>" id mr
            in l <> "/" <> r
        )
        (symmetricJoinWindows (millis 50))
        (Kafka.Streams.Imperative.joined textSerde textSerde textSerde)
        sl
        sr
    toTopic (topicName "out") (produced textSerde textSerde) j
    topo <- buildTopology b
    driver <- newDriver topo "ksko-app"

    pipeInput driver (topicName "left") (Just (bytes "k")) (bytes "L1") (t 0) 0
    -- Right at t=200: outside L1's 50ms window. Stream time passing 50
    -- closes L1's window: it comes out unmatched; R1 is held.
    pipeInput driver (topicName "right") (Just (bytes "k")) (bytes "R1") (t 200) 0
    out <- readOutput driver (topicName "out")
    map (unbytes . crValueBytes) out `shouldBe` ["L1/<>"]
    -- and R1 once stream time passes 250
    pipeInput driver (topicName "left") (Just (bytes "k")) (bytes "L2") (t 300) 0
    out' <- readOutput driver (topicName "out")
    map (unbytes . crValueBytes) out' `shouldBe` ["<>/R1"]
    closeDriver driver


----------------------------------------------------------------------
-- KTable-KTable join tests
----------------------------------------------------------------------

ktable_ktable_inner_join :: Spec
ktable_ktable_inner_join =
  it "KTable-KTable inner join: only emits when both sides have a value" $ do
    b <- newStreamsBuilder
    tl <-
      tableFromTopic
        b
        (topicName "left")
        (consumed textSerde textSerde)
        (materializedAs (storeName "left-store"))
    tr <-
      tableFromTopic
        b
        (topicName "right")
        (consumed textSerde textSerde)
        (materializedAs (storeName "right-store"))
    out <-
      joinKTableKTable
        (\l r -> l <> "+" <> r)
        (materializedAs (storeName "join-store"))
        tl
        tr
    topo <- buildTopology b
    driver <- newDriver topo "ktkt-app"

    -- Left only: no output yet (no matching right)
    pipeInput driver (topicName "left") (Just (bytes "k")) (bytes "L1") (t 0) 0
    s1 <- getKeyValueStore @Text @Text driver (ktableStore out)
    case s1 of
      Just kvs -> kvsGet kvs "k" >>= (`shouldBe` Nothing)
      Nothing -> error "out store missing"

    -- Right arrives: now both sides have values, output emitted.
    pipeInput driver (topicName "right") (Just (bytes "k")) (bytes "R1") (t 1) 0
    s2 <- getKeyValueStore @Text @Text driver (ktableStore out)
    case s2 of
      Just kvs -> kvsGet kvs "k" >>= (`shouldBe` Just "L1+R1")
      Nothing -> error "out store missing"

    -- Left updated: re-emits.
    pipeInput driver (topicName "left") (Just (bytes "k")) (bytes "L2") (t 2) 0
    s3 <- getKeyValueStore @Text @Text driver (ktableStore out)
    case s3 of
      Just kvs -> kvsGet kvs "k" >>= (`shouldBe` Just "L2+R1")
      Nothing -> error "out store missing"
    closeDriver driver


ktable_ktable_left_join :: Spec
ktable_ktable_left_join =
  it "KTable-KTable left join: emits whenever left has a value" $ do
    b <- newStreamsBuilder
    tl <-
      tableFromTopic
        b
        (topicName "left")
        (consumed textSerde textSerde)
        (materializedAs (storeName "left-store-l"))
    tr <-
      tableFromTopic
        b
        (topicName "right")
        (consumed textSerde textSerde)
        (materializedAs (storeName "right-store-l"))
    out <-
      leftJoinKTableKTable
        ( \l mr -> case mr of
            Just r -> l <> "+" <> r
            Nothing -> l <> "+<>"
        )
        (materializedAs (storeName "join-store-l"))
        tl
        tr
    topo <- buildTopology b
    driver <- newDriver topo "ktkt-l-app"

    -- Left without right: emits with Nothing.
    pipeInput driver (topicName "left") (Just (bytes "k")) (bytes "L1") (t 0) 0
    s1 <- getKeyValueStore @Text @Text driver (ktableStore out)
    case s1 of
      Just kvs -> kvsGet kvs "k" >>= (`shouldBe` Just "L1+<>")
      Nothing -> error "out store missing"

    -- Right arrives.
    pipeInput driver (topicName "right") (Just (bytes "k")) (bytes "R1") (t 1) 0
    s2 <- getKeyValueStore @Text @Text driver (ktableStore out)
    case s2 of
      Just kvs -> kvsGet kvs "k" >>= (`shouldBe` Just "L1+R1")
      Nothing -> error "out store missing"
    closeDriver driver


ktable_ktable_outer_join :: Spec
ktable_ktable_outer_join =
  it "KTable-KTable outer join: emits whenever either side has a value" $ do
    b <- newStreamsBuilder
    tl <-
      tableFromTopic
        b
        (topicName "left")
        (consumed textSerde textSerde)
        (materializedAs (storeName "left-store-o"))
    tr <-
      tableFromTopic
        b
        (topicName "right")
        (consumed textSerde textSerde)
        (materializedAs (storeName "right-store-o"))
    out <-
      outerJoinKTableKTable
        ( \ml mr ->
            let l = maybe "<>" id ml
                r = maybe "<>" id mr
            in l <> "/" <> r
        )
        (materializedAs (storeName "join-store-o"))
        tl
        tr
    topo <- buildTopology b
    driver <- newDriver topo "ktkt-o-app"

    -- Right without left first.
    pipeInput driver (topicName "right") (Just (bytes "k")) (bytes "R1") (t 0) 0
    s1 <- getKeyValueStore @Text @Text driver (ktableStore out)
    case s1 of
      Just kvs -> kvsGet kvs "k" >>= (`shouldBe` Just "<>/R1")
      Nothing -> error "out store missing"

    -- Left arrives.
    pipeInput driver (topicName "left") (Just (bytes "k")) (bytes "L1") (t 1) 0
    s2 <- getKeyValueStore @Text @Text driver (ktableStore out)
    case s2 of
      Just kvs -> kvsGet kvs "k" >>= (`shouldBe` Just "L1/R1")
      Nothing -> error "out store missing"
    closeDriver driver


----------------------------------------------------------------------
-- KStream-GlobalKTable join tests
----------------------------------------------------------------------

kstream_global_ktable_inner :: Spec
kstream_global_ktable_inner =
  it "KStream-GlobalKTable inner join: stream key mapped to global key" $ do
    b <- newStreamsBuilder
    -- Global table keyed by "user-id" (independent of stream keys).
    g <-
      globalTable
        b
        (topicName "users")
        (consumed textSerde textSerde)
        (materializedAs (storeName "g-store"))
    -- Stream of events keyed by event-id, value contains "user-id|action".
    s <-
      streamFromTopic
        b
        (topicName "events")
        (consumed textSerde textSerde)
    j <-
      joinKStreamGlobalKTable
        ( \_eventKey v ->
            -- look up by user-id: the part before "|"
            T.takeWhile (/= '|') v
        )
        (\v userName -> userName <> ":" <> T.dropWhile (== '|') (T.dropWhile (/= '|') v))
        s
        g
    toTopic (topicName "out") (produced textSerde textSerde) j
    topo <- buildTopology b
    driver <- newDriver topo "kgkt-app"

    pipeInput driver (topicName "users") (Just (bytes "u1")) (bytes "alice") (t 0) 0
    pipeInput driver (topicName "users") (Just (bytes "u2")) (bytes "bob") (t 0) 0

    pipeInput driver (topicName "events") (Just (bytes "e1")) (bytes "u1|click") (t 1) 0
    pipeInput driver (topicName "events") (Just (bytes "e2")) (bytes "uX|miss") (t 2) 0
    pipeInput driver (topicName "events") (Just (bytes "e3")) (bytes "u2|scroll") (t 3) 0

    out <- readOutput driver (topicName "out")
    map (unbytes . crValueBytes) out `shouldBe` ["alice:click", "bob:scroll"]
    closeDriver driver


kstream_global_ktable_left_no_match_emits_nothing :: Spec
kstream_global_ktable_left_no_match_emits_nothing =
  it "KStream-GlobalKTable left join: emits even when global has no match" $ do
    b <- newStreamsBuilder
    g <-
      globalTable
        b
        (topicName "users")
        (consumed textSerde textSerde)
        (materializedAs (storeName "g2-store"))
    s <-
      streamFromTopic
        b
        (topicName "events")
        (consumed textSerde textSerde)
    j <-
      leftJoinKStreamGlobalKTable
        (\_e v -> T.takeWhile (/= '|') v)
        ( \v mUser ->
            let user = maybe "<unknown>" id mUser
                rest = T.dropWhile (== '|') (T.dropWhile (/= '|') v)
            in user <> ":" <> rest
        )
        s
        g
    toTopic (topicName "out") (produced textSerde textSerde) j
    topo <- buildTopology b
    driver <- newDriver topo "kgkt-l-app"

    pipeInput driver (topicName "users") (Just (bytes "u1")) (bytes "alice") (t 0) 0
    pipeInput driver (topicName "events") (Just (bytes "e1")) (bytes "u1|x") (t 1) 0
    pipeInput driver (topicName "events") (Just (bytes "e2")) (bytes "u9|y") (t 2) 0

    out <- readOutput driver (topicName "out")
    map (unbytes . crValueBytes) out `shouldBe` ["alice:x", "<unknown>:y"]
    closeDriver driver


----------------------------------------------------------------------
-- KTable-KTable foreign-key join tests
----------------------------------------------------------------------

fk_join_inner_basic :: Spec
fk_join_inner_basic =
  it "FK join: left value's fk-extracted lookup hits right table" $ do
    b <- newStreamsBuilder
    -- Left: order id -> "user_id|amount"
    tl <-
      tableFromTopic
        b
        (topicName "orders")
        (consumed textSerde textSerde)
        (materializedAs (storeName "orders-store"))
    -- Right: user id -> name
    tr <-
      tableFromTopic
        b
        (topicName "users")
        (consumed textSerde textSerde)
        (materializedAs (storeName "users-store"))
    out <-
      foreignKeyJoinKTable
        (\v -> T.takeWhile (/= '|') v) -- extract user id
        ( \v userName ->
            userName <> ":" <> T.dropWhile (== '|') (T.dropWhile (/= '|') v)
        )
        (materializedAs (storeName "fk-out"))
        tl
        tr
    topo <- buildTopology b
    driver <- newDriver topo "fk-app"

    -- Right table loaded first.
    pipeInput driver (topicName "users") (Just (bytes "u1")) (bytes "alice") (t 0) 0
    pipeInput driver (topicName "users") (Just (bytes "u2")) (bytes "bob") (t 0) 0
    -- Left table updates.
    pipeInput driver (topicName "orders") (Just (bytes "o1")) (bytes "u1|10") (t 1) 0
    pipeInput driver (topicName "orders") (Just (bytes "o2")) (bytes "u2|20") (t 1) 0

    Just rs <- queryEngineStore @Text @Text (driverEngine driver) (ktableStore out)
    rs.roKvGet "o1" >>= (`shouldBe` Just "alice:10")
    rs.roKvGet "o2" >>= (`shouldBe` Just "bob:20")
    closeDriver driver


fk_join_changing_fk_unsubscribes :: Spec
fk_join_changing_fk_unsubscribes =
  it "FK join: changing the foreign key on a left record unsubscribes" $ do
    b <- newStreamsBuilder
    tl <-
      tableFromTopic
        b
        (topicName "orders")
        (consumed textSerde textSerde)
        (materializedAs (storeName "orders-store-2"))
    tr <-
      tableFromTopic
        b
        (topicName "users")
        (consumed textSerde textSerde)
        (materializedAs (storeName "users-store-2"))
    out <-
      foreignKeyJoinKTable
        (\v -> T.takeWhile (/= '|') v)
        (\v u -> u <> ":" <> T.dropWhile (== '|') (T.dropWhile (/= '|') v))
        (materializedAs (storeName "fk-out-2"))
        tl
        tr
    topo <- buildTopology b
    driver <- newDriver topo "fk-app"

    pipeInput driver (topicName "users") (Just (bytes "u1")) (bytes "alice") (t 0) 0
    pipeInput driver (topicName "users") (Just (bytes "u2")) (bytes "bob") (t 0) 0
    -- o1 originally points at u1.
    pipeInput driver (topicName "orders") (Just (bytes "o1")) (bytes "u1|x") (t 1) 0
    -- Now o1 points at u2. Should unsubscribe from u1 and emit "bob:x".
    pipeInput driver (topicName "orders") (Just (bytes "o1")) (bytes "u2|x") (t 2) 0
    -- Updating u1 should NOT re-emit for o1 anymore (only the original
    -- subscription window would fire).
    pipeInput driver (topicName "users") (Just (bytes "u1")) (bytes "ALICE2") (t 3) 0

    Just rs <- queryEngineStore @Text @Text (driverEngine driver) (ktableStore out)
    rs.roKvGet "o1" >>= (`shouldBe` Just "bob:x")
    closeDriver driver


fk_join_right_update_re_emits :: Spec
fk_join_right_update_re_emits =
  it "FK join: updating a right table value re-emits all subscribed left rows" $ do
    b <- newStreamsBuilder
    tl <-
      tableFromTopic
        b
        (topicName "orders")
        (consumed textSerde textSerde)
        (materializedAs (storeName "orders-store-3"))
    tr <-
      tableFromTopic
        b
        (topicName "users")
        (consumed textSerde textSerde)
        (materializedAs (storeName "users-store-3"))
    out <-
      foreignKeyJoinKTable
        (\v -> T.takeWhile (/= '|') v)
        (\v u -> u <> ":" <> T.dropWhile (== '|') (T.dropWhile (/= '|') v))
        (materializedAs (storeName "fk-out-3"))
        tl
        tr
    topo <- buildTopology b
    driver <- newDriver topo "fk-app"

    pipeInput driver (topicName "users") (Just (bytes "u1")) (bytes "alice") (t 0) 0
    -- Two orders both reference u1.
    pipeInput driver (topicName "orders") (Just (bytes "o1")) (bytes "u1|10") (t 1) 0
    pipeInput driver (topicName "orders") (Just (bytes "o2")) (bytes "u1|20") (t 1) 0
    -- Update u1's name.
    pipeInput driver (topicName "users") (Just (bytes "u1")) (bytes "ALICE") (t 2) 0

    Just rs <- queryEngineStore @Text @Text (driverEngine driver) (ktableStore out)
    rs.roKvGet "o1" >>= (`shouldBe` Just "ALICE:10")
    rs.roKvGet "o2" >>= (`shouldBe` Just "ALICE:20")
    closeDriver driver


fk_left_join_emits_when_no_right :: Spec
fk_left_join_emits_when_no_right =
  it "FK left join: emits even when the right table has no row for fk" $ do
    b <- newStreamsBuilder
    tl <-
      tableFromTopic
        b
        (topicName "orders")
        (consumed textSerde textSerde)
        (materializedAs (storeName "orders-store-l"))
    tr <-
      tableFromTopic
        b
        (topicName "users")
        (consumed textSerde textSerde)
        (materializedAs (storeName "users-store-l"))
    out <-
      leftForeignKeyJoinKTable
        (\v -> T.takeWhile (/= '|') v)
        ( \v mu -> case mu of
            Just u -> u <> ":" <> T.dropWhile (== '|') (T.dropWhile (/= '|') v)
            Nothing -> "<>:" <> T.dropWhile (== '|') (T.dropWhile (/= '|') v)
        )
        (materializedAs (storeName "fk-out-l"))
        tl
        tr
    topo <- buildTopology b
    driver <- newDriver topo "fk-l-app"

    -- Left first, no right yet.
    pipeInput driver (topicName "orders") (Just (bytes "o1")) (bytes "u1|x") (t 0) 0
    Just rs <- queryEngineStore @Text @Text (driverEngine driver) (ktableStore out)
    rs.roKvGet "o1" >>= (`shouldBe` Just "<>:x")
    -- Now right arrives, re-emit.
    pipeInput driver (topicName "users") (Just (bytes "u1")) (bytes "alice") (t 1) 0
    rs.roKvGet "o1" >>= (`shouldBe` Just "alice:x")
    closeDriver driver


-- | One input record: side ('True' left), key, timestamp, value.
data In = In {inLeft :: Bool, inKey :: Text, inTs :: Integer, inVal :: Text}
  deriving stock (Show)


genInputs :: H.Gen [In]
genInputs = do
  n <- Gen.int (Range.linear 0 25)
  forM [1 .. n] $ \i -> do
    left <- Gen.bool
    k <- Gen.element ["a", "b"]
    -- few timestamps, so duplicates (same key and timestamp) happen
    ts <- Gen.integral (Range.linear 0 60)
    pure (In left k ts ((if left then "L" else "R") <> T.pack (show i)))


-- | Run a window join of kind @kind@ over the inputs, in order; the outputs as (key, value, timestamp).
runJoin :: Text -> JoinWindows -> [In] -> IO [(Text, Text, Integer)]
runJoin kind jw ins = do
  b <- newStreamsBuilder
  sl <- streamFromTopic b (topicName "left") (consumed textSerde textSerde)
  sr <- streamFromTopic b (topicName "right") (consumed textSerde textSerde)
  let jn = Kafka.Streams.Imperative.joined textSerde textSerde textSerde
      side = maybe "-" id
  j <- case kind of
    "inner" -> joinKStreamKStream (\l r -> l <> "|" <> r) jw jn sl sr
    "left" -> leftJoinKStreamKStream (\l r -> l <> "|" <> side r) jw jn sl sr
    _ -> outerJoinKStreamKStream (\l r -> side l <> "|" <> side r) jw jn sl sr
  toTopic (topicName "out") (produced textSerde textSerde) j
  topo <- buildTopology b
  driver <- newDriver topo "join-model"
  forM_ ins $ \i ->
    pipeInput driver (topicName (if inLeft i then "left" else "right")) (Just (bytes (inKey i))) (bytes (inVal i)) (t (inTs i)) 0
  out <- readOutput driver (topicName "out")
  closeDriver driver
  pure (map (\cr -> (maybe "" unbytes (crKey cr), unbytes (crValueBytes cr), toInteger (unTimestamp (crTimestamp cr)))) out)


-- | Every left/right pair a window join matches: a left record at @t@ meets the right ones in @[t - before, t + after]@.
pairs :: JoinWindows -> [In] -> [(Text, Text, Integer)]
pairs jw ins =
  concatMap
    ( \l ->
        map
          (\r -> (inKey l, inVal l <> "|" <> inVal r, max (inTs l) (inTs r)))
          ( filter
              ( \r ->
                  not (inLeft r)
                    && inKey r == inKey l
                    && inTs r >= inTs l - toInteger jw.beforeMs
                    && inTs r <= inTs l + toInteger jw.afterMs
              )
              ins
          )
    )
    (filter inLeft ins)


prop_inner_join_model :: H.Property
prop_inner_join_model = H.property $ do
  ins <- H.forAll genInputs
  before <- H.forAll (Gen.integral (Range.linear 0 15))
  after <- H.forAll (Gen.integral (Range.linear 0 15))
  -- enough grace that nothing arrives too late
  let jw = JoinWindows before after 1000
  out <- liftIO (runJoin "inner" jw ins)
  sort out H.=== sort (pairs jw ins)


prop_outer_join_model :: H.Property
prop_outer_join_model = H.property $ do
  ins0 <- H.forAll genInputs
  kind <- H.forAll (Gen.element ["left", "outer"])
  w <- H.forAll (Gen.integral (Range.linear 0 15))
  grace <- H.forAll (Gen.integral (Range.linear 0 10))
  -- in timestamp order, so nothing is late and nothing waits on a record
  -- that comes after its window closed; then one far-off record on
  -- another key closes every window
  let ins = sortOnTs ins0
      flush = In True "zz" 100_000 "F"
      jw = JoinWindows w w grace
      matched = pairs jw ins
      hasMatch x = any (\y -> inLeft y /= inLeft x && inKey y == inKey x && abs (inTs y - inTs x) <= toInteger w) ins
      unmatched =
        map
          (\x -> (inKey x, if inLeft x then inVal x <> "|-" else "-|" <> inVal x, inTs x))
          (filter (\x -> not (hasMatch x) && (inLeft x || kind == "outer")) ins)
  out <- liftIO (runJoin kind jw (ins <> [flush]))
  sort out H.=== sort (matched <> unmatched)
  where
    sortOnTs = map snd . sortOn fst . zipWith (\i x -> ((inTs x, i :: Int), x)) [0 ..]
