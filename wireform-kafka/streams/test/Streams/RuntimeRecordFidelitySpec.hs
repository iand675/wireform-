{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | What the broker-backed runtime carries between the wire and the
topology, on the in-memory broker:

  * Record headers survive the trip in (consumer record -> topology)
    and out (sink -> producer record), along with the record
    timestamp, with one and several stream threads.
  * A sink's 'StreamPartitioner' sees the topic's partition count and
    its answer becomes the sent record's partition; an answer outside
    @[0, n)@ is a processing exception.
  * Under exactly-once, the application's state directory is wiped on
    start unless the previous run closed cleanly, and a clean close
    leaves the marker that keeps it.
-}
module Streams.RuntimeRecordFidelitySpec (tests) where

import Control.Exception (finally)
import Data.ByteString.Char8 qualified as BSC
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Kafka.Streams.Imperative
import Kafka.Streams.Runtime.Driver (DriverRecord (..))
import Kafka.Streams.Runtime.MockBroker
import Kafka.Streams.Runtime.StateDir (
  cleanShutdownMarker,
  prepareStateDir,
 )
import Streams.RuntimeHarness
import System.Directory (
  createDirectoryIfMissing,
  doesFileExist,
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


cfgFor :: Int -> FilePath -> StreamsConfig
cfgFor threads dir = (testConfig "fidelity-app" dir) {numStreamThreads = threads}


-- | Run @topo@ until @n@ records reach @out@; return them.
runUntil :: MockBroker -> StreamsConfig -> TopologyValid -> (KafkaStreams -> IO ()) -> Int -> IO [DriverRecord]
runUntil broker cfg topo setUp n = do
  ks <- newKafkaStreams cfg topo
  setUp ks
  mockDriverFor broker cfg >>= startKafkaStreamsWith ks
  ( do
      eventually [ks] (show n <> " output records") ((>= n) . length <$> mockRecords broker "out")
      mockRecords broker "out"
    )
    `finally` (closeKafkaStreams ks >> awaitState ks StreamsClosed)


----------------------------------------------------------------------
-- Headers
----------------------------------------------------------------------

headers_round_trip :: Int -> Spec
headers_round_trip threads =
  it ("source headers and timestamp reach the produced record (threads=" <> show threads <> ")") $
    withSystemTempDirectory "fidelity" $ \dir -> do
      broker <- newMockBroker (Just 1)
      mockCreateTopic broker "in" 2
      topo <- upcaseTopo (produced textSerde textSerde)
      let hdrs = [("commit-ts", bytes "42"), ("source", bytes "mbe"), ("source", bytes "dup")]
      _ <- mockProduceTo broker "in" (Just 1) (Just (bytes "k")) (Just (bytes "v")) 1_700_000_000_007 hdrs
      sends <- runUntil broker (cfgFor threads dir) topo (\_ -> pure ()) 1
      map (.drHeaders) sends `shouldBe` [hdrs]
      map (.drTimestamp) sends `shouldBe` [1_700_000_000_007]
      map (.drValue) sends `shouldBe` [Just (bytes "V")]


----------------------------------------------------------------------
-- StreamPartitioner
----------------------------------------------------------------------

-- | Routes key @"pN"@ to partition @N@; anything else to the last one.
byKeySuffix :: StreamPartitioner Text Text
byKeySuffix = StreamPartitioner $ \_topic mk _v n ->
  pure $ case TR.decimal <$> (T.stripPrefix "p" =<< mk) of
    Just (Right (i, "")) -> Just i
    _ -> Just (n - 1)


partitioner_picks_partition :: Int -> Spec
partitioner_picks_partition threads =
  it ("the sink sends each record to the partition its StreamPartitioner picks (threads=" <> show threads <> ")") $
    withSystemTempDirectory "fidelity" $ \dir -> do
      broker <- newMockBroker (Just 1)
      mockCreateTopic broker "in" 2
      mockCreateTopic broker "out" 6
      topo <- upcaseTopo (withStreamPartitioner byKeySuffix (produced textSerde textSerde))
      mapM_ (\k -> mockProduce broker "in" (Just (bytes k)) (Just "v") 0) ["p2", "p4", "other"]
      sends <- runUntil broker (cfgFor threads dir) topo (\_ -> pure ()) 3
      map (\r -> (r.drKey, r.drPartition)) sends
        `shouldMatchList` [ (Just (bytes "p2"), 2)
                          , (Just (bytes "p4"), 4)
                          , (Just (bytes "other"), 5)
                          ]


partitioner_out_of_range_is_processing_error :: Spec
partitioner_out_of_range_is_processing_error =
  it "a partition outside [0, n) goes to the processing-exception handler and is not sent" $
    withSystemTempDirectory "fidelity" $ \dir -> do
      broker <- newMockBroker (Just 1)
      mockCreateTopic broker "in" 1
      mockCreateTopic broker "out" 3
      topo <- upcaseTopo (withStreamPartitioner byKeySuffix (produced textSerde textSerde))
      seen <- newIORef []
      -- offset 0 asks for partition 9 of 3; offset 1 is fine.
      mapM_ (\k -> mockProduce broker "in" (Just (bytes k)) (Just "v") 0) ["p9", "p1"]
      let handler ks =
            setProcessingExceptionHandler ks $
              ProcessingExceptionHandler $ \e -> do
                modifyIORef' seen (e.offset :)
                pure ProcessingContinue
      sends <- runUntil broker (cfgFor 1 dir) topo handler 1
      map (.drPartition) sends `shouldBe` [1]
      readIORef seen `shouldReturn` [0]


no_partitioner_leaves_partition_to_producer :: Spec
no_partitioner_leaves_partition_to_producer =
  it "without a StreamPartitioner the producer partitions by key" $
    withSystemTempDirectory "fidelity" $ \dir -> do
      broker <- newMockBroker (Just 1)
      mockCreateTopic broker "in" 1
      mockCreateTopic broker "out" 4
      topo <- upcaseTopo (produced textSerde textSerde)
      mapM_ (\k -> mockProduce broker "in" (Just (bytes k)) (Just "v") 0) ["a", "b", "c", "d"]
      sends <- runUntil broker (cfgFor 1 dir) topo (\_ -> pure ()) 4
      -- the same key lands where the producer's own partitioner puts it
      mockCreateTopic broker "check" 4
      mapM_ (\k -> mockProduce broker "check" (Just (bytes k)) (Just "v") 0) ["a", "b", "c", "d"]
      expected <- mockRecords broker "check"
      map (\r -> (r.drKey, r.drPartition)) sends `shouldMatchList` map (\r -> (r.drKey, r.drPartition)) expected


----------------------------------------------------------------------
-- state.dir
----------------------------------------------------------------------

appDirOf :: FilePath -> FilePath
appDirOf dir = dir </> "fidelity-app"


-- | A stand-in for a store directory left by a previous run.
seedStore :: FilePath -> IO FilePath
seedStore dir = do
  let store = appDirOf dir </> "0_0"
  createDirectoryIfMissing True store
  writeFile (store </> "000001.sst") "stale"
  pure (store </> "000001.sst")


eosRun :: FilePath -> IO ()
eosRun dir = do
  broker <- newMockBroker (Just 1)
  mockCreateTopic broker "in" 1
  topo <- upcaseTopo (produced textSerde textSerde)
  mockProduce broker "in" (Just "k") (Just "v") 0
  _ <- runUntil broker ((cfgFor 1 dir) {processingGuarantee = ExactlyOnceV2}) topo (\_ -> pure ()) 1
  pure ()


unclean_eos_start_wipes :: Spec
unclean_eos_start_wipes =
  it "an exactly-once start without a clean-shutdown marker wipes state.dir" $
    withSystemTempDirectory "fidelity" $ \dir -> do
      sst <- seedStore dir
      eosRun dir
      doesFileExist sst `shouldReturn` False
      -- and this run's clean close left the marker
      doesFileExist (appDirOf dir </> cleanShutdownMarker) `shouldReturn` True


clean_close_keeps_state :: Spec
clean_close_keeps_state =
  it "a clean exactly-once close leaves the marker, so the next start keeps state" $
    withSystemTempDirectory "fidelity" $ \dir -> do
      -- First run: nothing on disk yet, closes cleanly.
      eosRun dir
      doesFileExist (appDirOf dir </> cleanShutdownMarker) `shouldReturn` True
      -- State written between runs (as a store would) must survive
      -- the second start, and the marker is consumed by it.
      sst <- seedStore dir
      prepareStateDir ExactlyOnceV2 (appDirOf dir) `shouldReturn` False
      doesFileExist sst `shouldReturn` True
      doesFileExist (appDirOf dir </> cleanShutdownMarker) `shouldReturn` False
      -- A start that is never followed by a clean close (a crash)
      -- leaves no marker, so the start after it wipes.
      prepareStateDir ExactlyOnceV2 (appDirOf dir) `shouldReturn` True
      doesFileExist sst `shouldReturn` False


at_least_once_never_wipes :: Spec
at_least_once_never_wipes =
  it "at-least-once keeps state.dir even without a marker" $
    withSystemTempDirectory "fidelity" $ \dir -> do
      sst <- seedStore dir
      prepareStateDir AtLeastOnceP (appDirOf dir) `shouldReturn` False
      doesFileExist sst `shouldReturn` True
