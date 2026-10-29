{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Operational behaviour of the broker-backed runtime, on the
in-memory broker: stream-thread count, pause / resume, close and
leaving the group, the production and uncaught-exception handlers,
the handler defaults, rebalance listeners and probing rebalances.
-}
module Streams.RuntimeBehaviourSpec (tests) where

import Control.Exception (ErrorCall (..), finally, toException)
import Control.Monad (forM_, replicateM_)
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Client.RebalanceListener (RebalanceListener (..), noopRebalanceListener)
import Kafka.Streams.Imperative
import Kafka.Streams.Runtime.Driver (DriverRecord (..))
import Kafka.Streams.Runtime.MockBroker
import Streams.RuntimeHarness
import Test.Syd


tests :: Spec
tests =
  describe "Runtime behaviour" $ do
    describe "stream threads" $ do
      it "add and remove change the count; never below one" thread_counts
      it "records flow through every partition with several threads, across add / remove" threads_process_everything
    describe "lifecycle" $ do
      it "pause stops processing until resume" pause_resume
      it "close leaves the group unless asked not to" close_leave_group
    describe "handlers" $ do
      it "defaults fail: deserialization, processing and production" handler_defaults
      it "a failed send with the default handlers is retried after recovery: nothing lost" send_failure_recovers
      it "a failed send with a continue production handler is dropped" send_failure_continue
      it "replace-thread recovers from a transient processing failure" replace_thread_recovers
    describe "rebalance" $ do
      it "the rebalance listener hears assignments" rebalance_listener
      it "a ready warmup triggers a probing rebalance; interval 0 disables it" probing_rebalance


upcase :: IO TopologyValid
upcase = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  u <- mapValues (T.toUpper :: Text -> Text) s
  toTopic (topicName "out") (produced textSerde textSerde) u
  buildTopology b >>= either (error . show) pure . validateTopology


produceN :: MockBroker -> Int -> IO ()
produceN broker n = forM_ [1 .. n] $ \i ->
  mockProduce broker "in" (Just (textBytes (T.pack (show i)))) (Just "v") 0


outputCount :: MockBroker -> IO Int
outputCount broker = length <$> visible broker "out"


closeIt :: KafkaStreams -> IO ()
closeIt ks = closeKafkaStreams ks >> awaitState ks StreamsClosed


thread_counts :: IO ()
thread_counts = withStateDirs 1 $ \[d] -> do
  topo <- upcase
  ks <- newKafkaStreams ((testConfig "threads" d) {numStreamThreads = 2}) topo
  streamThreadCount ks `shouldReturn` 2
  addStreamThread ks `shouldReturn` Just 3
  addStreamThread ks `shouldReturn` Just 4
  replicateM_ 3 (removeStreamThread ks)
  streamThreadCount ks `shouldReturn` 1
  removeStreamThread ks `shouldReturn` Nothing


threads_process_everything :: IO ()
threads_process_everything = withStateDirs 1 $ \[d] -> do
  broker <- newMockBroker (Just 1)
  mockCreateTopic broker "in" 4
  topo <- upcase
  ks <- startOn broker ((testConfig "threads" d) {numStreamThreads = 4}) topo
  ( do
      produceN broker 40
      eventually [ks] "40 outputs" ((== 40) <$> outputCount broker)
      meta <- metadataForLocalThreads ks
      length meta `shouldBe` 4
      sum (map processedRecs meta) `shouldBe` 40
      _ <- addStreamThread ks
      produceN broker 10
      eventually [ks] "50 outputs" ((== 50) <$> outputCount broker)
      _ <- removeStreamThread ks
      _ <- removeStreamThread ks
      produceN broker 10
      eventually [ks] "60 outputs" ((== 60) <$> outputCount broker)
    )
    `finally` closeIt ks


pause_resume :: IO ()
pause_resume = withStateDirs 1 $ \[d] -> do
  broker <- newMockBroker (Just 1)
  mockCreateTopic broker "in" 1
  topo <- upcase
  ks <- startOn broker (testConfig "pause" d) topo
  ( do
      produceN broker 3
      eventually [ks] "3 outputs" ((== 3) <$> outputCount broker)
      pauseKafkaStreams ks
      isPausedKafkaStreams ks `shouldReturn` True
      _ <- awaitTicks ks 2
      produceN broker 3
      _ <- awaitTicks ks 50
      outputCount broker `shouldReturn` 3
      resumeKafkaStreams ks
      eventually [ks] "6 outputs after resume" ((== 6) <$> outputCount broker)
    )
    `finally` closeIt ks


close_leave_group :: IO ()
close_leave_group = withStateDirs 2 $ \[d1, d2] -> do
  broker <- newMockBroker (Just 1)
  mockCreateTopic broker "in" 1
  topo <- upcase
  a <- startOn broker (testConfig "leave" d1) topo
  awaitState a StreamsRunning
  mockGroupMembers broker "leave" >>= (`shouldSatisfy` ((== 1) . length))
  closeKafkaStreamsWith a defaultCloseOptions
  mockGroupMembers broker "leave" `shouldReturn` []
  b <- startOn broker (testConfig "leave" d2) topo
  awaitState b StreamsRunning
  closeKafkaStreamsWith b defaultCloseOptions {leaveGroup = False}
  streamsStatus b `shouldReturn` StreamsClosed


handler_defaults :: IO ()
handler_defaults = do
  let cfg = defaultStreamsConfig
  runDeserializationHandler (defaultDeserHandler cfg) (DeserializationException "t" 0 0 Nothing "" "bad")
    `shouldReturn` DeserFailFast
  runProcessingExceptionHandler (defaultProcessingHandler cfg) (ProcessingException "t" 0 0 "n" "bad")
    `shouldReturn` ProcessingFail
  runProductionHandler (defaultProductionHandler cfg) (ProductionException "t" "bad")
    `shouldReturn` ProdFailFast
  resp <- runStreamsUncaughtExceptionHandler replaceThreadOnException (toException (ErrorCall "x"))
  resp `shouldBe` ReplaceThread


send_failure_recovers :: IO ()
send_failure_recovers = withStateDirs 1 $ \[d] -> do
  broker <- newMockBroker (Just 1)
  mockCreateTopic broker "in" 1
  mockCreateTopic broker "out" 1
  topo <- upcase
  mockFailSends broker "out" 1
  ks <- startOn broker (testConfig "send-fail" d) topo
  ( do
      produceN broker 5
      -- the failed send stops the thread; it is replaced, rewinds to
      -- the last commit and sends again
      eventually [ks] "every input has an output" $ do
        outs <- visible broker "out"
        pure (all (\i -> any ((== Just (textBytes (T.pack (show i)))) . (.drKey)) outs) [1 .. 5 :: Int])
      streamsStatus ks `shouldReturn` StreamsRunning
    )
    `finally` closeIt ks


send_failure_continue :: IO ()
send_failure_continue = withStateDirs 1 $ \[d] -> do
  broker <- newMockBroker (Just 1)
  mockCreateTopic broker "in" 1
  mockCreateTopic broker "out" 1
  topo <- upcase
  mockFailSends broker "out" 1
  ks <- newKafkaStreams (testConfig "send-continue" d) topo
  setProductionExceptionHandler ks logAndContinueProduction
  mockDriverFor broker (testConfig "send-continue" d) >>= startKafkaStreamsWith ks
  ( do
      produceN broker 5
      eventually [ks] "the other four outputs" ((== 4) <$> outputCount broker)
      _ <- awaitTicks ks 20
      outputCount broker `shouldReturn` 4
    )
    `finally` closeIt ks


replace_thread_recovers :: IO ()
replace_thread_recovers = withStateDirs 1 $ \[d] -> do
  broker <- newMockBroker (Just 1)
  mockCreateTopic broker "in" 1
  failOnce <- newIORef True
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  -- a processor whose first "boom" fails, and later ones don't
  m <-
    mapValuesM
      ( \v -> do
          first <- if v == "boom" then atomicModifyIORef' failOnce (\f -> (False, f)) else pure False
          if first then ioError (userError "transient") else pure (T.toUpper v :: Text)
      )
      s
  toTopic (topicName "out") (produced textSerde textSerde) m
  topo <- buildTopology b >>= either (error . show) pure . validateTopology
  forM_ ["a", "boom", "c"] $ \v -> mockProduce broker "in" Nothing (Just v) 0
  ks <- startOn broker (testConfig "replace" d) topo
  ( do
      -- at least once: records processed before the failure may be sent twice
      eventually [ks] "every value made it" $ do
        vs <- map (.drValue) <$> visible broker "out"
        pure (all (`elem` vs) [Just "A", Just "BOOM", Just "C"])
      streamsStatus ks `shouldReturn` StreamsRunning
    )
    `finally` closeIt ks


rebalance_listener :: IO ()
rebalance_listener = withStateDirs 1 $ \[d] -> do
  broker <- newMockBroker (Just 1)
  mockCreateTopic broker "in" 3
  topo <- upcase
  heard <- newIORef []
  ks <- newKafkaStreams (testConfig "listen" d) topo
  setRebalanceListener ks noopRebalanceListener {rlOnAssigned = \tps -> modifyIORef' heard (<> tps)}
  mockDriverFor broker (testConfig "listen" d) >>= startKafkaStreamsWith ks
  ( do
      awaitState ks StreamsRunning
      readIORef heard >>= (`shouldBe` 3) . length
      ownedPartitions ks >>= (`shouldBe` 3) . length
    )
    `finally` closeIt ks


probing_rebalance :: IO ()
probing_rebalance = withStateDirs 2 $ \[d1, d2] -> do
  broker <- newMockBroker (Just 1)
  mockCreateTopic broker "in" 1
  topo <- upcase
  -- interval 0: never
  off <- startOn broker ((testConfig "probe-off" d1) {probingRebalanceIntervalMs = 0}) topo
  ( do
      awaitState off StreamsRunning
      g0 <- mockGeneration broker "probe-off"
      reportWarmupLag off (TaskId 0 0) 0
      _ <- awaitTicks off 30
      mockGeneration broker "probe-off" `shouldReturn` g0
    )
    `finally` closeIt off
  -- a short interval and a ready warmup: the group rebalances
  on <- startOn broker ((testConfig "probe-on" d2) {probingRebalanceIntervalMs = 1}) topo
  ( do
      awaitState on StreamsRunning
      g0 <- mockGeneration broker "probe-on"
      reportWarmupLag on (TaskId 0 0) 0
      eventually [on] "a new generation" ((> g0) <$> mockGeneration broker "probe-on")
    )
    `finally` closeIt on
