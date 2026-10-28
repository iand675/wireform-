{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | What the broker-backed runtime carries between the wire and the
topology, driven through 'newMockDriver':

  * Record headers survive the trip in (consumer record -> topology)
    and out (sink -> 'sdProducerSend'), on the single-thread and the
    worker-pool loop, along with the record timestamp.
  * A sink's 'StreamPartitioner' sees the topic's partition count from
    'sdPartitionsFor' and its answer becomes the sent record's
    partition; an answer outside @[0, n)@ is a processing exception.
  * Under exactly-once, @state.dir@ is wiped on start unless the
    previous run closed cleanly, and a clean close leaves the marker
    that keeps it.
-}
module Streams.RuntimeRecordFidelitySpec (tests) where

import Control.Exception (finally)
import Data.ByteString.Char8 qualified as BSC
import Data.IORef
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Client.Consumer qualified as KC
import Kafka.Streams.Imperative
import Kafka.Streams.Runtime.NativeDriver (
  MockDriverHandle,
  MockSend (..),
  StreamDriver,
  mockDriverDrainSends,
  mockDriverInjectPoll,
  mockDriverSetPartitions,
  newMockDriver,
 )
import Kafka.Streams.Runtime.StateDir (
  cleanShutdownMarker,
  prepareStateDir,
 )
import System.Directory (
  createDirectoryIfMissing,
  doesFileExist,
  listDirectory,
 )
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Syd


tests :: Spec
tests =
  describe "Runtime record fidelity" $ do
    describe "headers" $ do
      headers_round_trip 1
      headers_round_trip 3
    describe "StreamPartitioner" $ do
      partitioner_picks_partition 1
      partitioner_picks_partition 3
      partitioner_out_of_range_is_processing_error
      no_partitioner_leaves_partition_to_producer
    describe "state.dir under exactly-once" $ do
      unclean_eos_start_wipes
      clean_close_keeps_state
      at_least_once_never_wipes


----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack


upcaseTopo :: Produced Text Text -> IO TopologyValid
upcaseTopo p = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  upper <- mapValues (T.toUpper :: Text -> Text) s
  toTopic (topicName "out") p upper
  topo <- buildTopology b
  case validateTopology topo of
    Left err -> error (show err)
    Right v -> pure v


inRecord :: Int64 -> Text -> [(Text, BSC.ByteString)] -> KC.ConsumerRecord
inRecord off k hdrs =
  KC.ConsumerRecord
    { topic = "in"
    , partition = 0
    , offset = off
    , timestamp = 1_700_000_000_000 + off
    , key = Just (bytes k)
    , value = bytes "v"
    , headers = hdrs
    }


testCfg :: Int -> FilePath -> StreamsConfig
testCfg threads dir =
  defaultStreamsConfig
    { applicationId = "fidelity-app"
    , bootstrapServers = ["mock:0"]
    , numStreamThreads = threads
    , pollMs = 0
    , stateDir = dir
    }


-- | Start @ks@ on @drv@, run @act@, and always close it.
withRunning :: KafkaStreams -> StreamDriver -> IO a -> IO a
withRunning ks drv act = do
  startKafkaStreamsWith ks drv
  awaitState ks StreamsRunning
  act `finally` (closeKafkaStreams ks >> awaitState ks StreamsClosed)


-- | Collect sends until @n@ have arrived.
awaitSends :: KafkaStreams -> MockDriverHandle -> Int -> IO [MockSend]
awaitSends ks h n = do
  acc <- newIORef []
  let loop :: Int -> IO ()
      loop 0 = expectationFailure ("timed out waiting for " <> show n <> " sends")
      loop k = do
        ss <- mockDriverDrainSends h
        modifyIORef' acc (++ ss)
        got <- length <$> readIORef acc
        if got >= n then pure () else awaitTicks ks 1 >> loop (k - 1)
  loop 5000
  readIORef acc


----------------------------------------------------------------------
-- Headers
----------------------------------------------------------------------

headers_round_trip :: Int -> Spec
headers_round_trip threads =
  it ("source headers and timestamp reach the produced record (threads=" <> show threads <> ")") $
    withSystemTempDirectory "fidelity" $ \dir -> do
      topo <- upcaseTopo (produced textSerde textSerde)
      ks <- newKafkaStreams (testCfg threads dir) topo
      (drv, h) <- newMockDriver
      let hdrs = [("commit-ts", bytes "42"), ("source", bytes "mbe"), ("source", bytes "dup")]
      mockDriverInjectPoll h [inRecord 7 "k" hdrs]
      sends <- withRunning ks drv (awaitSends ks h 1)
      map mockSendHeaders sends `shouldBe` [hdrs]
      map mockSendTimestamp sends `shouldBe` [Just 1_700_000_000_007]
      map mockSendValue sends `shouldBe` [bytes "V"]


----------------------------------------------------------------------
-- StreamPartitioner
----------------------------------------------------------------------

-- | Routes key @"pN"@ to partition @N@; anything else to the last one.
byKeySuffix :: StreamPartitioner Text Text
byKeySuffix = StreamPartitioner $ \_topic mk _v n ->
  pure $ case T.stripPrefix "p" =<< mk of
    Just d | [(i, "")] <- reads (T.unpack d) -> Just i
    _ -> Just (n - 1)


partitioner_picks_partition :: Int -> Spec
partitioner_picks_partition threads =
  it ("the sink sends each record to the partition its StreamPartitioner picks (threads=" <> show threads <> ")") $
    withSystemTempDirectory "fidelity" $ \dir -> do
      topo <- upcaseTopo (withStreamPartitioner byKeySuffix (produced textSerde textSerde))
      ks <- newKafkaStreams (testCfg threads dir) topo
      (drv, h) <- newMockDriver
      mockDriverSetPartitions h "out" 6
      mockDriverInjectPoll h [inRecord 0 "p2" [], inRecord 1 "p4" [], inRecord 2 "other" []]
      sends <- withRunning ks drv (awaitSends ks h 3)
      map (\s -> (s.mockSendKey, s.mockSendPartition)) sends
        `shouldMatchList` [ (Just (bytes "p2"), Just 2)
                          , (Just (bytes "p4"), Just 4)
                          , (Just (bytes "other"), Just 5)
                          ]


partitioner_out_of_range_is_processing_error :: Spec
partitioner_out_of_range_is_processing_error =
  it "a partition outside [0, n) goes to the processing-exception handler and is not sent" $
    withSystemTempDirectory "fidelity" $ \dir -> do
      topo <- upcaseTopo (withStreamPartitioner byKeySuffix (produced textSerde textSerde))
      ks <- newKafkaStreams (testCfg 1 dir) topo
      seen <- newIORef []
      setProcessingExceptionHandler ks $
        ProcessingExceptionHandler $ \e -> do
          modifyIORef' seen (e.offset :)
          pure ProcessingContinue
      (drv, h) <- newMockDriver
      mockDriverSetPartitions h "out" 3
      -- offset 0 asks for partition 9 of 3; offset 1 is fine.
      mockDriverInjectPoll h [inRecord 0 "p9" [], inRecord 1 "p1" []]
      sends <- withRunning ks drv (awaitSends ks h 1)
      map mockSendPartition sends `shouldBe` [Just 1]
      readIORef seen `shouldReturn` [0]


no_partitioner_leaves_partition_to_producer :: Spec
no_partitioner_leaves_partition_to_producer =
  it "without a StreamPartitioner no partition is set and no metadata is needed" $
    withSystemTempDirectory "fidelity" $ \dir -> do
      topo <- upcaseTopo (produced textSerde textSerde)
      ks <- newKafkaStreams (testCfg 1 dir) topo
      -- No 'mockDriverSetPartitions': a lookup would fail.
      (drv, h) <- newMockDriver
      mockDriverInjectPoll h [inRecord 0 "p1" []]
      sends <- withRunning ks drv (awaitSends ks h 1)
      map mockSendPartition sends `shouldBe` [Nothing]


----------------------------------------------------------------------
-- state.dir
----------------------------------------------------------------------

-- | A stand-in for a store directory left by a previous run.
seedStore :: FilePath -> IO FilePath
seedStore dir = do
  let store = dir </> "counts" </> "0_0"
  createDirectoryIfMissing True store
  writeFile (store </> "000001.sst") "stale"
  pure (store </> "000001.sst")


eosRun :: FilePath -> IO ()
eosRun dir = do
  topo <- upcaseTopo (produced textSerde textSerde)
  ks <-
    newKafkaStreams
      ((testCfg 1 dir) {processingGuarantee = ExactlyOnceV2})
      topo
  (drv, h) <- newMockDriver
  mockDriverInjectPoll h [inRecord 0 "k" []]
  _ <- withRunning ks drv (awaitSends ks h 1)
  pure ()


unclean_eos_start_wipes :: Spec
unclean_eos_start_wipes =
  it "an exactly-once start without a clean-shutdown marker wipes state.dir" $
    withSystemTempDirectory "fidelity" $ \dir -> do
      sst <- seedStore dir
      eosRun dir
      doesFileExist sst `shouldReturn` False
      -- Only the marker from this run's clean close remains.
      listDirectory dir `shouldReturn` [cleanShutdownMarker]


clean_close_keeps_state :: Spec
clean_close_keeps_state =
  it "a clean exactly-once close leaves the marker, so the next start keeps state" $
    withSystemTempDirectory "fidelity" $ \dir -> do
      -- First run: nothing on disk yet, closes cleanly.
      eosRun dir
      doesFileExist (dir </> cleanShutdownMarker) `shouldReturn` True
      -- State written between runs (as a store would) must survive
      -- the second start, and the marker is consumed by it.
      sst <- seedStore dir
      prepareStateDir ExactlyOnceV2 dir `shouldReturn` False
      doesFileExist sst `shouldReturn` True
      doesFileExist (dir </> cleanShutdownMarker) `shouldReturn` False
      -- A start that is never followed by a clean close (a crash)
      -- leaves no marker, so the start after it wipes.
      prepareStateDir ExactlyOnceV2 dir `shouldReturn` True
      doesFileExist sst `shouldReturn` False


at_least_once_never_wipes :: Spec
at_least_once_never_wipes =
  it "at-least-once keeps state.dir even without a marker" $
    withSystemTempDirectory "fidelity" $ \dir -> do
      sst <- seedStore dir
      prepareStateDir AtLeastOnceP dir `shouldReturn` False
      doesFileExist sst `shouldReturn` True
