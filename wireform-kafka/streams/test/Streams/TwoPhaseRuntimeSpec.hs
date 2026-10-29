{-# LANGUAGE OverloadedStrings #-}

{- | Two-phase sinks committing with the runtime ("Kafka.Streams.Runtime.Commit"),
on the in-memory broker:

  * under exactly-once, rows of an aborted transaction are never
    committed, and every row is committed once;
  * a rewind after a processing failure drops what was staged, so the
    reprocessed rows are not committed twice;
  * at start, prepared cycles recorded as committed are committed, the
    rest aborted.
-}
module Streams.TwoPhaseRuntimeSpec (tests) where

import Control.Exception (finally)
import Control.Monad (forM_)
import Data.IORef
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Streams.Imperative
import Kafka.Streams.Runtime.MockBroker
import Kafka.Streams.Sinks.TwoPhase
import Streams.RuntimeHarness
import Test.Syd


tests :: Spec
tests =
  describe "Two-phase sinks on the runtime" $ do
    it "exactly once: aborted transactions commit nothing, every row commits once" eos_rows_once
    it "a rewind drops staged rows: reprocessed rows commit once" rewind_discards_staged
    it "at start, recorded cycles commit and the rest abort" startup_recovery


-- | @in@ staged into a fresh in-memory sink, and a pass-through to @out@
-- that fails (once) on @failOn@.
sinkTopology :: Maybe Text -> IO (TopologyValid, InMemorySinkState (Record Text Text), TwoPhaseSink (Record Text Text))
sinkTopology failOn = do
  (sink, st) <- inMemoryTwoPhaseSink "mem"
  armed <- newIORef True
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  compileSinkTwoPhase () sink s
  m <-
    mapValuesM
      ( \v -> do
          boom <- if Just v == failOn then atomicModifyIORef' armed (\a -> (False, a)) else pure False
          if boom then ioError (userError "transient") else pure v
      )
      s
  toTopic (topicName "out") (produced textSerde textSerde) m
  topo <- buildTopology b >>= either (error . show) pure . validateTopology
  pure (topo, st, sink)


startWith :: MockBroker -> StreamsConfig -> TopologyValid -> TwoPhaseSink r -> IO KafkaStreams
startWith broker cfg topo sink = do
  ks <- newKafkaStreams cfg topo
  addCommitParticipant ks (twoPhaseParticipant sink)
  mockDriverFor broker cfg >>= startKafkaStreamsWith ks
  pure ks


committedValues :: InMemorySinkState (Record Text Text) -> IO [Text]
committedValues st = map recordValue <$> readCommittedRows st


produceAll :: MockBroker -> [Text] -> IO ()
produceAll broker = mapM_ (\v -> mockProduce broker "in" Nothing (Just (textBytes v)) 0)


closeIt :: KafkaStreams -> IO ()
closeIt ks = closeKafkaStreams ks >> awaitState ks StreamsClosed


eos_rows_once :: IO ()
eos_rows_once = withStateDirs 1 $ \[d] -> do
  broker <- newMockBroker (Just 1)
  mockCreateTopic broker "in" 2
  (topo, st, sink) <- sinkTopology Nothing
  ks <- startWith broker ((testConfig "tp-eos" d) {processingGuarantee = ExactlyOnceV2}) topo sink
  ( do
      produceAll broker ["a", "b", "c"]
      eventually [ks] "the first rows committed" ((== 3) . length <$> committedValues st)
      mockFailTransactionCommits broker 2
      produceAll broker ["d", "e", "f"]
      eventually [ks] "every row committed" ((>= 6) . length <$> committedValues st)
      _ <- awaitTicks ks 20
      sort <$> committedValues st `shouldReturn` ["a", "b", "c", "d", "e", "f"]
      readPreparedRows st `shouldReturn` Map.empty
    )
    `finally` closeIt ks


rewind_discards_staged :: IO ()
rewind_discards_staged = withStateDirs 1 $ \[d] -> do
  broker <- newMockBroker (Just 1)
  mockCreateTopic broker "in" 1
  (topo, st, sink) <- sinkTopology (Just "boom")
  -- one batch: "a" and "b" are staged before "boom" fails, and are
  -- reprocessed after the rewind
  produceAll broker ["a", "b", "boom", "c"]
  ks <- startWith broker ((testConfig "tp-rewind" d) {processingGuarantee = ExactlyOnceV2}) topo sink
  ( do
      eventually [ks] "every row committed" ((>= 4) . length <$> committedValues st)
      _ <- awaitTicks ks 20
      committedValues st `shouldReturn` ["a", "b", "boom", "c"]
      streamsStatus ks `shouldReturn` StreamsRunning
    )
    `finally` closeIt ks


startup_recovery :: IO ()
startup_recovery = withStateDirs 1 $ \[d] -> do
  broker <- newMockBroker (Just 1)
  mockCreateTopic broker "in" 1
  (topo, st, sink) <- sinkTopology Nothing
  -- a previous instance prepared cycles 3 and 4, and recorded 3 as committed
  forM_ [("old/3", "x"), ("old/4", "y")] $ \(cid, v) -> tpsPrepare sink (SinkTxnId cid) [mkRecord Nothing v (Timestamp 0)]
  mockProduce broker "tp-recover-twophase-commits" (Just "old") (Just "3") 0
  ks <- startWith broker (testConfig "tp-recover" d) topo sink
  ( do
      awaitState ks StreamsRunning
      committedValues st `shouldReturn` ["x"]
      readPreparedRows st `shouldReturn` Map.empty
    )
    `finally` closeIt ks
