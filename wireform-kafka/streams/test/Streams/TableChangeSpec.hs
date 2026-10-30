{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | Tables forward changes (new and old value), so deletes and
retractions reach every derived table and output:

  * a source tombstone and a filter retraction delete the row
    downstream, and reach the output as a tombstone;
  * a table-table join sees updates from both sides;
  * re-grouped table aggregations move a row between groups and drop it
    on delete;
  * the foreign-key join and table re-grouping run across tables
    partitioned differently, on the broker-backed runtime.
-}
module Streams.TableChangeSpec (tests) where

import Control.Exception (finally)
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Streams.Imperative
import Kafka.Streams.Observability.OrphanTopics (detectOrphans, isInternalTopicName)
import Kafka.Streams.Topology (topologyValidGraph)
import Kafka.Streams.Runtime.MockBroker
import Streams.RuntimeHarness
import Test.Syd


tests :: Spec
tests =
  describe "Table changes" $ do
    it "a source tombstone deletes the row downstream and leaves a tombstone in the output" source_tombstone_flows
    it "a row a filter stops accepting is deleted downstream" filter_retracts
    it "a table-table join sees updates from both sides" join_both_sides
    it "a re-grouped count moves a row between groups and drops it on delete" regrouped_count
    it "foreign-key join across differently partitioned tables (runtime)" fk_join_runtime
    it "table re-grouping across partitions (runtime)" regroup_runtime


-- | Latest value per key read off an output topic, 'Nothing' for a tombstone.
latestOutput :: TopologyTestDriver -> TopicName -> IO (Map.Map (Maybe ByteString) (Maybe ByteString))
latestOutput d t = do
  rs <- readOutput d t
  pure (Map.fromList (map (\cr -> (crKey cr, crValue cr)) rs))


source_tombstone_flows :: IO ()
source_tombstone_flows = do
  b <- newStreamsBuilder
  t <- tableFromTopic b (topicName "in") (consumed textSerde textSerde) (materializedAs (storeName "src"))
  up <- mapValuesTable T.toUpper (materializedAs (storeName "up")) t
  out <- toChangelogStream up
  toTopicNullable (topicName "out") (produced textSerde textSerde) out
  d <- buildTopology b >>= (`newDriver` "tomb")
  let tin = createInputTopic d (topicName "in") textSerde textSerde
  pipeKV tin (Just "a") "x"
  pipeKV tin (Just "b") "y"
  pipeTombstone tin "a"
  latestOutput d (topicName "out") >>= (`shouldBe` Map.fromList [(Just "a", Nothing), (Just "b", Just "Y")])
  Just up' <- getKeyValueStore @Text @Text d (storeName "up")
  kvsGet up' "a" >>= (`shouldBe` Nothing)
  closeDriver d


filter_retracts :: IO ()
filter_retracts = do
  b <- newStreamsBuilder
  t <- tableFromTopic b (topicName "in") (consumed textSerde textSerde) (materializedAs (storeName "src"))
  long <- filterTable (\r -> T.length (recordValue r) >= 3) (materializedAs (storeName "long")) t
  marked <- mapValuesTable (<> "!") (materializedAs (storeName "marked")) long
  out <- toChangelogStream marked
  toTopicNullable (topicName "out") (produced textSerde textSerde) out
  d <- buildTopology b >>= (`newDriver` "filter")
  let tin = createInputTopic d (topicName "in") textSerde textSerde
  pipeKV tin (Just "k") "long"
  pipeKV tin (Just "k") "ab"
  Just m <- getKeyValueStore @Text @Text d (storeName "marked")
  kvsGet m "k" >>= (`shouldBe` Nothing)
  map crValue <$> readOutput d (topicName "out") >>= (`shouldBe` [Just "long!", Nothing])
  closeDriver d


join_both_sides :: IO ()
join_both_sides = do
  b <- newStreamsBuilder
  l <- tableFromTopic b (topicName "left") (consumed textSerde textSerde) (materializedAs (storeName "l"))
  r <- tableFromTopic b (topicName "right") (consumed textSerde textSerde) (materializedAs (storeName "r"))
  j <- joinKTableKTable (\a c -> a <> "+" <> c :: Text) ((materializedAs (storeName "j")) {matValueSerde = Just textSerde}) l r
  out <- toChangelogStream j
  toTopicNullable (topicName "out") (produced textSerde textSerde) out
  d <- buildTopology b >>= (`newDriver` "join")
  let ltin = createInputTopic d (topicName "left") textSerde textSerde
      rtin = createInputTopic d (topicName "right") textSerde textSerde
  pipeKV ltin (Just "k") "l1"
  pipeKV rtin (Just "k") "r1" -- right side completes the row
  pipeKV rtin (Just "k") "r2" -- and updates it
  pipeTombstone ltin "k" -- the left delete removes it
  map crValue <$> readOutput d (topicName "out") >>= (`shouldBe` [Just "l1+r1", Just "l1+r2", Nothing])
  closeDriver d


regrouped_count :: IO ()
regrouped_count = do
  b <- newStreamsBuilder
  orders <- tableFromTopic b (topicName "orders") (consumed textSerde textSerde) (materializedAs (storeName "orders"))
  byCustomer <- groupTableBy (\_ c -> (c, c)) (grouped textSerde textSerde) orders
  counts <- countKGroupedTable (materializedAs (storeName "per-customer")) byCustomer
  _ <- toKStreamFromKTable counts
  d <- buildTopology b >>= (`newDriver` "regroup")
  let tin = createInputTopic d (topicName "orders") textSerde textSerde
  pipeKV tin (Just "o1") "A"
  pipeKV tin (Just "o2") "A"
  pipeKV tin (Just "o1") "B" -- o1 moves from A to B
  pipeTombstone tin "o2" -- o2 is gone
  Just st <- getKeyValueStore @Text @Int64 d (storeName "per-customer")
  kvsGet st "A" >>= (`shouldBe` Just 0)
  kvsGet st "B" >>= (`shouldBe` Just 1)
  closeDriver d


fk_join_runtime :: IO ()
fk_join_runtime = withStateDirs 1 $ \[dir] -> do
  broker <- newMockBroker (Just 1)
  -- orders keyed by order id, customers keyed by customer id: different partitionings
  mockCreateTopic broker "orders" 4
  mockCreateTopic broker "customers" 3
  b <- newStreamsBuilder
  orders <- tableFromTopic b (topicName "orders") (consumed textSerde textSerde) (materializedAs (storeName "orders"))
  customers <- tableFromTopic b (topicName "customers") (consumed textSerde textSerde) (materializedAs (storeName "customers"))
  -- an order's value is "<customer>:<item>"
  enriched <-
    foreignKeyJoinKTable
      (T.takeWhile (/= ':'))
      (\o c -> o <> "@" <> c :: Text)
      (materializedAs (storeName "enriched"))
      orders
      customers
  out <- toChangelogStream enriched
  toTopicNullable (topicName "out") (produced textSerde textSerde) out
  topo <- buildTopology b >>= either (error . show) pure . validateTopology
  forM_ [("c1", "Ada"), ("c2", "Bob"), ("c3", "Cy")] $ \(c, n) -> mockProduce broker "customers" (Just c) (Just n) 0
  forM_ [1 .. 12 :: Int] $ \i ->
    mockProduce broker "orders" (Just (textBytes ("o" <> T.pack (show i)))) (Just (textBytes ("c" <> T.pack (show (i `mod` 3 + 1)) <> ":item"))) 0
  ks <- startOn broker (testConfig "fk" dir) topo
  ( do
      let expect i = textBytes ("c" <> T.pack (show (i `mod` 3 + 1)) <> ":item@" <> ["Ada", "Bob", "Cy"] !! (i `mod` 3))
          joinedAll = do
            latest <- latestBy <$> visible broker "out"
            pure (all (\i -> Map.lookup (textBytes ("o" <> T.pack (show i))) latest == Just (Just (expect i))) [1 .. 12 :: Int])
      eventually [ks] "every order enriched" joinedAll
      -- the subscription topic is co-partitioned with customers, the response topic with orders
      topics <- mockTopics broker
      mapM (mockPartitionCount broker) (filter ("-subscription-registration" `T.isSuffixOf`) topics) >>= (`shouldBe` [Just 3])
      mapM (mockPartitionCount broker) (filter ("-subscription-response" `T.isSuffixOf`) topics) >>= (`shouldBe` [Just 4])
      -- every internal topic the runtime created is one the orphan detector expects
      detectOrphans (topologyValidGraph topo) "fk" (map topicName topics) `shouldBe` []
      -- a right-side update reaches its subscribers; an FK change moves an order; a delete removes it
      mockProduce broker "customers" (Just "c1") (Just "Ann") 1
      mockProduce broker "orders" (Just "o1") (Just "c3:item") 1
      mockProduce broker "orders" (Just "o2") Nothing 1
      eventually [ks] "updates applied" $ do
        latest <- latestBy <$> visible broker "out"
        pure
          ( Map.lookup "o3" latest == Just (Just "c1:item@Ann")
              && Map.lookup "o1" latest == Just (Just "c3:item@Cy")
              && Map.lookup "o2" latest == Just Nothing
          )
    )
    `finally` (closeKafkaStreams ks >> awaitState ks StreamsClosed)


regroup_runtime :: IO ()
regroup_runtime = withStateDirs 1 $ \[dir] -> do
  broker <- newMockBroker (Just 1)
  mockCreateTopic broker "orders" 4
  b <- newStreamsBuilder
  orders <- tableFromTopic b (topicName "orders") (consumed textSerde textSerde) (materializedAs (storeName "orders"))
  byCustomer <- groupTableBy (\_ c -> (c, c)) (grouped textSerde textSerde) orders
  counts <- countKGroupedTable (materializedAs (storeName "per-customer")) byCustomer
  out <- toKStreamFromKTable counts
  toTopic (topicName "out") (produced textSerde int64Serde) out
  topo <- buildTopology b >>= either (error . show) pure . validateTopology
  ks <- startOn broker (testConfig "rg" dir) topo
  ( do
      forM_ [1 .. 20 :: Int] $ \i ->
        mockProduce broker "orders" (Just (textBytes ("o" <> T.pack (show i)))) (Just (if even i then "even" else "odd")) 0
      eventually [ks] "10 and 10" $ do
        latest <- latestBy <$> visible broker "out"
        pure (Map.lookup "even" latest == Just (Just (i64 10)) && Map.lookup "odd" latest == Just (Just (i64 10)))
      topics <- mockTopics broker
      filter (isInternalTopicName "rg" . topicName) topics `shouldSatisfy` (not . null)
      detectOrphans (topologyValidGraph topo) "rg" (map topicName topics) `shouldBe` []
      -- move four orders from odd to even, delete two evens
      forM_ [1, 3, 5, 7 :: Int] $ \i -> mockProduce broker "orders" (Just (textBytes ("o" <> T.pack (show i)))) (Just "even") 1
      forM_ [2, 4 :: Int] $ \i -> mockProduce broker "orders" (Just (textBytes ("o" <> T.pack (show i)))) Nothing 1
      eventually [ks] "12 and 6" $ do
        latest <- latestBy <$> visible broker "out"
        pure (Map.lookup "even" latest == Just (Just (i64 12)) && Map.lookup "odd" latest == Just (Just (i64 6)))
    )
    `finally` (closeKafkaStreams ks >> awaitState ks StreamsClosed)
