{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | Tests for KIP-150-era KTable.groupBy() / KGroupedTable
aggregations. The critical invariant the JVM contract
expresses (and we mirror): on a KTable update the
/subtractor/ runs first so the prior value's contribution
is removed from the aggregate before the adder folds in the
new value. Without the subtractor an update would
double-count.
-}
module Streams.KGroupedTableSpec (tests) where

import Data.ByteString.Char8 qualified as BSC
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Streams.Imperative
import Kafka.Streams.KGroupedTable
import Test.Syd


tests :: Spec
tests =
  describe "KGroupedTable (KIP-150)" $
    sequence_
      [ count_updates_subtract_prior
      , reduce_subtracts_then_adds
      , aggregate_handles_first_record_specially
      , filter_not_table_drops_matches
      ]


bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack


i64 :: Int64 -> BSC.ByteString
i64 = serialize int64Serde


ts :: Int64 -> Timestamp
ts = Timestamp


----------------------------------------------------------------------
-- 1. count: every update to the same key keeps the count at 1
----------------------------------------------------------------------

count_updates_subtract_prior :: Spec
count_updates_subtract_prior =
  it "count: re-keying a KTable update doesn't double-count" $ do
    b <- newStreamsBuilder
    -- A KTable keyed by orderId, value = customerId. Group by
    -- customerId, then count orders per customer. Updating
    -- order's customerId from A to B must give A=0 / B=1, not
    -- A=1 / B=1.
    table <-
      tableFromTopic
        b
        (topicName "orders")
        (consumed textSerde textSerde)
        (materializedAs (storeName "orders-store"))
    let g = grouped textSerde textSerde
    kgt <-
          groupTableBy
            (\_orderId customer -> (customer, customer))
            g
            table
    counts <-
      countKGroupedTable
        (materializedAs (storeName "counts-store"))
        kgt
    topo <- buildTopology b
    driver <- newDriver topo "kgt-count"

    -- o1 -> A (count A = 1)
    pipeInput
      driver
      (topicName "orders")
      (Just (bytes "o1"))
      (bytes "A")
      (ts 0)
      0
    -- o2 -> B (count B = 1)
    pipeInput
      driver
      (topicName "orders")
      (Just (bytes "o2"))
      (bytes "B")
      (ts 1)
      0
    -- o1 reassigned from A to B (count A = 0, count B = 2)
    pipeInput
      driver
      (topicName "orders")
      (Just (bytes "o1"))
      (bytes "B")
      (ts 2)
      0

    mStore <- getKeyValueStore @Text @Int64 driver (ktableStore counts)
    case mStore of
      Just kvs -> do
        kvsGet kvs "A" >>= (`shouldBe` Just 0)
        kvsGet kvs "B" >>= (`shouldBe` Just 2)
      Nothing -> error "store missing"
    closeDriver driver


----------------------------------------------------------------------
-- 2. reduce: subtractor undoes prior, adder applies new
----------------------------------------------------------------------

reduce_subtracts_then_adds :: Spec
reduce_subtracts_then_adds =
  it "reduce: sum-by-key updates correctly under value changes" $ do
    b <- newStreamsBuilder
    -- order -> amount. Group by a constant key "TOTAL".
    table <-
      tableFromTopic
        b
        (topicName "amounts")
        (consumed textSerde int64Serde)
        (materializedAs (storeName "amounts-store"))
    let g = grouped textSerde int64Serde
    kgt <- groupTableBy (\_orderId amt -> ("TOTAL", amt)) g table
    totals <-
      reduceKGroupedTable
        (+)
        (-)
        (materializedAs (storeName "sum-store"))
        kgt
    topo <- buildTopology b
    driver <- newDriver topo "kgt-reduce"

    pipeInput
      driver
      (topicName "amounts")
      (Just (bytes "o1"))
      (i64 10)
      (ts 0)
      0
    pipeInput
      driver
      (topicName "amounts")
      (Just (bytes "o2"))
      (i64 5)
      (ts 1)
      0
    -- update o1 from 10 to 100: sum must go 15 -> 105 (not 115)
    pipeInput
      driver
      (topicName "amounts")
      (Just (bytes "o1"))
      (i64 100)
      (ts 2)
      0
    -- update o2 from 5 to 1: sum must go 105 -> 101
    pipeInput
      driver
      (topicName "amounts")
      (Just (bytes "o2"))
      (i64 1)
      (ts 3)
      0

    mStore <- getKeyValueStore @Text @Int64 driver (ktableStore totals)
    case mStore of
      Just kvs -> do
        kvsGet kvs "TOTAL" >>= (`shouldBe` Just 101)
      Nothing -> error "store missing"
    closeDriver driver


----------------------------------------------------------------------
-- 3. aggregate: first-record path goes through adder against initial
----------------------------------------------------------------------

aggregate_handles_first_record_specially :: Spec
aggregate_handles_first_record_specially =
  it "aggregate: first record per key uses initialiser as the seed" $ do
    b <- newStreamsBuilder
    table <-
      tableFromTopic
        b
        (topicName "scores")
        (consumed textSerde int64Serde)
        (materializedAs (storeName "scores-store"))
    let g = grouped textSerde int64Serde
    kgt <- groupTableBy (\_p score -> ("HIGH", score)) g table
    highs <-
      aggregateKGroupedTable
        (pure (0 :: Int64))
        (\_ v acc -> max acc v) -- adder = max
        (\_ _ acc -> acc) -- subtractor: max can't undo
        -- a previous value perfectly,
        -- so we leave it as a no-op
        -- (real apps would store the
        -- whole multiset)
        (materializedAs (storeName "high-store"))
        kgt
    topo <- buildTopology b
    driver <- newDriver topo "kgt-agg"

    pipeInput
      driver
      (topicName "scores")
      (Just (bytes "alice"))
      (i64 5)
      (ts 0)
      0
    pipeInput
      driver
      (topicName "scores")
      (Just (bytes "bob"))
      (i64 8)
      (ts 1)
      0
    pipeInput
      driver
      (topicName "scores")
      (Just (bytes "carol"))
      (i64 12)
      (ts 2)
      0

    mStore <- getKeyValueStore @Text @Int64 driver (ktableStore highs)
    case mStore of
      Just kvs -> do
        kvsGet kvs "HIGH" >>= (`shouldBe` Just 12)
      Nothing -> error "store missing"
    closeDriver driver


----------------------------------------------------------------------
-- 4. filterNotTable: KTable.filterNot
----------------------------------------------------------------------

filter_not_table_drops_matches :: Spec
filter_not_table_drops_matches =
  it "KTable.filterNot: records matching the predicate are tombstoned" $ do
    b <- newStreamsBuilder
    src <-
      tableFromTopic
        b
        (topicName "in")
        (consumed textSerde textSerde)
        (materializedAs (storeName "src-store"))
    -- Keep everything whose value is NOT "skip".
    kept <-
      filterNotTable
        (\r -> recordValue r == "skip")
        (materializedAs (storeName "kept-store"))
        src
    topo <- buildTopology b
    driver <- newDriver topo "kt-filternot"

    pipeInput
      driver
      (topicName "in")
      (Just (bytes "k1"))
      (bytes "ok")
      (ts 0)
      0
    pipeInput
      driver
      (topicName "in")
      (Just (bytes "k2"))
      (bytes "skip")
      (ts 1)
      0
    pipeInput
      driver
      (topicName "in")
      (Just (bytes "k3"))
      (bytes "yes")
      (ts 2)
      0

    mStore <- getKeyValueStore @Text @Text driver (ktableStore kept)
    case mStore of
      Just kvs -> do
        kvsGet kvs "k1" >>= (`shouldBe` Just "ok")
        kvsGet kvs "k2" >>= (`shouldBe` Nothing)
        kvsGet kvs "k3" >>= (`shouldBe` Just "yes")
      Nothing -> error "store missing"
    closeDriver driver
