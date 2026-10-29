{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | The broker-backed runtime end to end, on the in-memory broker:

  * tasks per (subtopology, partition), split across instances by the
    group, with state handed over through changelogs;
  * restore after a restart with empty local state;
  * repartition topics for re-keyed aggregations;
  * exactly-once: an aborted transaction leaves no trace in the
    committed output and the tasks recover from their changelogs;
  * global tables bootstrapped before processing;
  * table tombstones, failure handling, and the start-up checks.
-}
module Streams.TaskRuntimeSpec (tests) where

import Control.Exception (finally)
import Control.Monad (forM_, when)
import Data.Int (Int64)
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Streams.Imperative
import Kafka.Streams.InteractiveQueries (ReadOnlyKeyValueStore (..), queryKVStore)
import Kafka.Streams.Runtime.Driver (DriverRecord (..))
import Kafka.Streams.Runtime.MockBroker
import Streams.RuntimeHarness
import Test.Syd


tests :: Spec
tests =
  describe "Task runtime" $ do
    it "aggregations of one grouping share its repartition topic" shared_repartition
    it "splits tasks across instances and hands state over through changelogs" two_instances_share_counts
    it "restores state from the changelog after a restart with empty local state" restores_after_restart
    it "repartitions a re-keyed aggregation through an internal topic" repartitions_rekeyed
    it "exactly-once: an aborted transaction leaves no trace in committed output" eos_abort_recovers
    it "joins against a global table bootstrapped before processing" global_table_join
    it "a table tombstone deletes the key from the store" table_tombstone
    it "refuses a stateful subtopology over non-co-partitioned topics" not_copartitioned
    it "processing failures follow the processing and uncaught-exception handlers" processing_handlers


shared_repartition :: IO ()
shared_repartition = withStateDirs 1 $ \[d] -> do
  broker <- newMockBroker (Just 1)
  mockCreateTopic broker "in" 2
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  -- re-keyed by value, so both aggregations need a repartition
  byValue <- selectKey (\r -> recordValue r) s
  let g = groupByKey (grouped textSerde textSerde) {groupedName = Just "by-value"} byValue
  c1 <- countStream (materializedAs (storeName "count-a")) g
  c2 <- countStream (materializedAs (storeName "count-b")) g
  out1 <- toKStreamFromKTable c1
  out2 <- toKStreamFromKTable c2
  toTopic (topicName "out-a") (produced textSerde int64Serde) out1
  toTopic (topicName "out-b") (produced textSerde int64Serde) out2
  topo <- buildTopology b >>= either (error . show) pure . validateTopology
  ks <- startOn broker (testConfig "shared" d) topo
  ( do
      forM_ [1 .. 10 :: Int] $ \i -> mockProduce broker "in" (Just (textBytes (T.pack (show i)))) (Just (if even i then "x" else "y")) 0
      let both n = do
            a <- latestBy <$> visible broker "out-a"
            bb <- latestBy <$> visible broker "out-b"
            pure (all (\m -> Map.lookup "x" m == Just (Just (i64 n)) && Map.lookup "y" m == Just (Just (i64 n))) [a, bb])
      eventually [ks] "both counts at 5" (both 5)
      _ <- awaitTicks ks 30
      both 5 `shouldReturn` True
      -- one repartition topic, read once
      topics <- mockTopics broker
      filter ("-repartition" `T.isSuffixOf`) topics `shouldBe` ["shared-by-value-repartition"]
    )
    `finally` closeAll [ks]


two_instances_share_counts :: IO ()
two_instances_share_counts = withStateDirs 2 $ \[d1, d2] -> do
  broker <- newMockBroker (Just 1)
  mockCreateTopic broker "in" 4
  topo <- wordCount
  a <- startOn broker (testConfig "wc" d1) topo
  ( do
      produceWords broker 3
      eventually [a] "first instance counts to 3" (countsReach broker 3)
      cl <- mockPartitionCount broker "wc-counts-changelog"
      cl `shouldBe` Just 4
      b <- startOn broker (testConfig "wc" d2) topo
      ( do
          eventually [a, b] "both instances own partitions" $ do
            pa <- ownedPartitions a
            pb <- ownedPartitions b
            pure (not (null pa) && not (null pb) && length pa + length pb == 4)
          produceWords broker 2
          eventually [a, b] "counts continue to 5 across the hand-over" (countsReach broker 5)
          -- nothing was double counted on the way
          outs <- visible broker "out"
          let perKey k = mapMaybe (\r -> if r.drKey == Just (textBytes k) then decodeI64 <$> r.drValue else Nothing) outs
          forM_ keys $ \k -> maximum (perKey k) `shouldBe` 5
        )
        `finally` closeAll [b]
    )
    `finally` closeAll [a]


restores_after_restart :: IO ()
restores_after_restart = withStateDirs 2 $ \[d1, d2] -> do
  broker <- newMockBroker (Just 1)
  mockCreateTopic broker "in" 2
  topo <- wordCount
  a <- startOn broker (testConfig "rs" d1) topo
  ( do
      produceWords broker 2
      eventually [a] "counts reach 2" (countsReach broker 2)
    )
    `finally` closeAll [a]
  -- a new instance with no local state picks up where the first left off
  b <- startOn broker (testConfig "rs" d2) topo
  ( do
      produceWords broker 1
      eventually [b] "counts continue to 3 from the changelog" (countsReach broker 3)
    )
    `finally` closeAll [b]


repartitions_rekeyed :: IO ()
repartitions_rekeyed = withStateDirs 1 $ \[d] -> do
  broker <- newMockBroker (Just 1)
  mockCreateTopic broker "in" 3
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  byValue <- selectKey (\r -> recordValue r) s
  counts <- countStream (materializedAs (storeName "by-value")) (groupByKey (grouped textSerde textSerde) byValue)
  out <- toKStreamFromKTable counts
  toTopic (topicName "out") (produced textSerde int64Serde) out
  topo <- buildTopology b >>= either (error . show) pure . validateTopology
  ks <- startOn broker (testConfig "rp" d) topo
  ( do
      forM_ [0 .. 29 :: Int] $ \i ->
        mockProduce broker "in" (Just (textBytes ("key" <> T.pack (show i)))) (Just (textBytes (if even i then "even" else "odd"))) 0
      eventually [ks] "values counted after the repartition" $ do
        latest <- latestBy <$> visible broker "out"
        pure (Map.lookup "even" latest == Just (Just (i64 15)) && Map.lookup "odd" latest == Just (Just (i64 15)))
      topics <- mockTopics broker
      let repartition = filter ("-repartition" `T.isSuffixOf`) topics
      length repartition `shouldBe` 1
      -- it gets the writing subtopology's partition count
      mapM (mockPartitionCount broker) repartition >>= (`shouldBe` [Just 3])
    )
    `finally` closeAll [ks]


eos_abort_recovers :: IO ()
eos_abort_recovers = withStateDirs 1 $ \[d] -> do
  broker <- newMockBroker (Just 1)
  mockCreateTopic broker "in" 2
  topo <- wordCount
  let cfg = (testConfig "eos" d) {processingGuarantee = ExactlyOnceV2}
  ks <- startOn broker cfg topo
  ( do
      produceWords broker 3
      eventually [ks] "counts reach 3" (countsReach broker 3)
      -- the next two commits fail: their transactions are aborted, and
      -- the tasks must come back from the committed changelog
      mockFailTransactionCommits broker 2
      produceWords broker 1
      eventually [ks] "counts reach 4" (countsReach broker 4)
      -- committed output per key is exactly 1, 2, 3, 4: nothing from
      -- the aborted transactions, no count twice
      outs <- visible broker "out"
      forM_ keys $ \k -> do
        let seen = mapMaybe (\r -> if r.drKey == Just (textBytes k) then decodeI64 <$> r.drValue else Nothing) outs
        seen `shouldBe` [1, 2, 3, 4]
      -- and the aborted writes really were written (then aborted)
      everything <- mockRecords broker "out"
      when (length everything <= length outs) $ expectationFailure "expected aborted records in the log"
    )
    `finally` closeAll [ks]


global_table_join :: IO ()
global_table_join = withStateDirs 1 $ \[d] -> do
  broker <- newMockBroker (Just 1)
  mockCreateTopic broker "lookup" 1
  mockCreateTopic broker "in" 2
  forM_ ["red", "blue"] $ \c -> mockProduce broker "lookup" (Just (textBytes c)) (Just (textBytes (T.toUpper c))) 0
  b <- newStreamsBuilder
  gt <- globalTable b (topicName "lookup") (consumed textSerde textSerde) (materializedAs (storeName "colours"))
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  j <- joinKStreamGlobalKTable (\_ v -> v) (\v g -> v <> "=" <> g :: Text) s gt
  toTopic (topicName "out") (produced textSerde textSerde) j
  topo <- buildTopology b >>= either (error . show) pure . validateTopology
  ks <- startOn broker (testConfig "gt" d) topo
  ( do
      forM_ [0 .. 9 :: Int] $ \i ->
        mockProduce broker "in" (Just (textBytes (T.pack (show i)))) (Just (if even i then "red" else "blue")) 0
      eventually [ks] "every record joined" $ do
        outs <- visible broker "out"
        pure (length outs == 10)
      outs <- visible broker "out"
      map (.drValue) outs `shouldSatisfy` all (`elem` [Just "red=RED", Just "blue=BLUE"])
    )
    `finally` closeAll [ks]


table_tombstone :: IO ()
table_tombstone = withStateDirs 1 $ \[d] -> do
  broker <- newMockBroker (Just 1)
  mockCreateTopic broker "tab" 2
  b <- newStreamsBuilder
  _ <- tableFromTopic b (topicName "tab") (consumed textSerde textSerde) (materializedAs (storeName "tab-store"))
  topo <- buildTopology b >>= either (error . show) pure . validateTopology
  ks <- startOn broker (testConfig "tomb" d) topo
  ( do
      mockProduce broker "tab" (Just "a") (Just "1") 0
      mockProduce broker "tab" (Just "b") (Just "2") 0
      let lookupKey k = do
            ms <- queryKVStore @Text @Text ks (storeName "tab-store")
            maybe (pure Nothing) (\st -> st.roKvGet k) ms
      eventually [ks] "both keys in the store" $ (&&) <$> (isJust <$> lookupKey "a") <*> (isJust <$> lookupKey "b")
      mockProduce broker "tab" (Just "a") Nothing 1
      eventually [ks] "the tombstone deleted a" $ (== Nothing) <$> lookupKey "a"
      lookupKey "b" >>= (`shouldBe` Just "2")
    )
    `finally` closeAll [ks]


not_copartitioned :: IO ()
not_copartitioned = withStateDirs 1 $ \[d] -> do
  broker <- newMockBroker (Just 1)
  mockCreateTopic broker "left" 2
  mockCreateTopic broker "right" 3
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "left") (consumed textSerde textSerde)
  t <- tableFromTopic b (topicName "right") (consumed textSerde textSerde) (materializedAs (storeName "right-store"))
  j <- joinKStreamKTable (\l r -> l <> r :: Text) (joined textSerde textSerde textSerde) s t
  toTopic (topicName "out") (produced textSerde textSerde) j
  topo <- buildTopology b >>= either (error . show) pure . validateTopology
  ks <- startOn broker (testConfig "cp" d) topo
  st <- streamsStatus ks
  case st of
    StreamsError e -> T.unpack e `shouldSatisfy` ("co-partitioned" `isInfixOf`)
    other -> closeAll [ks] >> expectationFailure ("expected an error, got " <> show other)


processing_handlers :: IO ()
processing_handlers = withStateDirs 2 $ \[d1, d2] -> do
  broker <- newMockBroker (Just 1)
  mockCreateTopic broker "in" 1
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  m <- mapValues (\v -> if v == "boom" then error "boom" else T.toUpper v :: Text) s
  toTopic (topicName "out") (produced textSerde textSerde) m
  topo <- buildTopology b >>= either (error . show) pure . validateTopology
  forM_ ["a", "boom", "b"] $ \v -> mockProduce broker "in" Nothing (Just v) 0
  -- continue: the failing record is skipped
  cont <- newKafkaStreams (testConfig "ph-continue" d1) topo
  setProcessingExceptionHandler cont logAndContinueProcessing
  mockDriverFor broker (testConfig "ph-continue" d1) >>= startKafkaStreamsWith cont
  ( do
      eventually [cont] "the good records arrive" ((== 2) . length <$> visible broker "out")
      map (.drValue) <$> visible broker "out" >>= (`shouldBe` [Just "A", Just "B"])
    )
    `finally` closeAll [cont]
  -- fail (the default) + shut the client down
  failing <- newKafkaStreams (testConfig "ph-fail" d2) topo
  setUncaughtExceptionHandler failing shutdownClientOnException
  mockDriverFor broker (testConfig "ph-fail" d2) >>= startKafkaStreamsWith failing
  eventually [failing] "the instance fails" $ do
    st <- streamsStatus failing
    pure (case st of StreamsError _ -> True; _ -> False)
  closeAll [failing]

