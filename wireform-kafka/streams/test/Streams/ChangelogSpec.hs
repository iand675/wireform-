{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | Change-logged stores: every write reaches the changelog, and
replaying the changelog onto an empty store rebuilds the same state.
Also the pieces the runtime builds tasks from: an engine logging to
its own changelog partition, table-source tombstones, and the
subtopology split.
-}
module Streams.ChangelogSpec (tests) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BSC
import Data.IORef
import Data.Int (Int64)
import Data.List (foldl')
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Kafka.Streams.Imperative hiding (Subtopology (..))
import Kafka.Streams.Internal.Changelog
import Kafka.Streams.Internal.Engine (
  ChangelogTarget (..),
  EngineOptions (..),
  StoreEntry (..),
  buildEngineWith,
  defaultEngineOptions,
  feedSource,
  storeByName,
 )
import Kafka.Streams.Internal.RecordCollector (
  CollectedRecord (..),
  collectorTake,
  inMemoryCollector,
 )
import Kafka.Streams.State.KeyValue.InMemory (inMemoryKeyValueStore)
import Kafka.Streams.State.Session.InMemory (inMemorySessionStore)
import Kafka.Streams.State.Store (StoreSerdes (..))
import Kafka.Streams.State.Window.InMemory (inMemoryWindowStore)
import Kafka.Streams.Topology.Subtopology (Subtopology (..), SubtopologyPlan (..), planSubtopologies)
import Test.Syd
import Test.Syd.Hedgehog ()
import Unsafe.Coerce (unsafeCoerce)


tests :: Spec
tests =
  describe "Changelog" $ do
    it "replaying a key-value changelog rebuilds the store" prop_kv_replay
    it "replaying a window changelog rebuilds the store" window_replay
    it "replaying a session changelog rebuilds puts and removes" session_replay
    it "an engine logs aggregation state to its task's changelog partition" engine_logs_to_partition
    it "a table-source tombstone deletes the key" table_tombstone_deletes
    describe "subtopologies" $ do
      it "a repartition splits the graph in two" repartition_splits
      it "a global table is outside every subtopology" global_table_is_separate


textSD :: StoreSerdes Text Text
textSD = StoreSerdes textSerde textSerde


-- | A sink that appends to an IORef (in send order).
recordingSink :: IO (IORef [(ByteString, Maybe ByteString)], ChangelogSink)
recordingSink = do
  ref <- newIORef []
  pure (ref, \k v -> modifyIORef' ref ((k, v) :))


replayInto :: (ByteString -> Maybe ByteString -> IO (Either Text ())) -> [(ByteString, Maybe ByteString)] -> IO ()
replayInto apply rs = forM_ rs $ \(k, v) -> apply k v >>= either (error . T.unpack) pure


data Op = Put Text Text | PutIfAbsent Text Text | Delete Text
  deriving stock (Show)


prop_kv_replay :: H.Property
prop_kv_replay = H.property $ do
  let key = Gen.element ["a", "b", "c", "d"]
      val = Gen.text (Range.linear 0 4) Gen.alpha
  ops <-
    H.forAll $
      Gen.list
        (Range.linear 0 60)
        (Gen.choice [Put <$> key <*> val, PutIfAbsent <$> key <*> val, Delete <$> key])
  (live, replayed) <- H.evalIO $ do
    inner <- inMemoryKeyValueStore @Text @Text (storeName "kv")
    (ref, sink) <- recordingSink
    let st = logKeyValueStore textSD sink inner
    forM_ ops $ \case
      Put k v -> kvsPut st k v
      PutIfAbsent k v -> () <$ kvsPutIfAbsent st k v
      Delete k -> () <$ kvsDelete st k
    fresh <- inMemoryKeyValueStore @Text @Text (storeName "kv")
    readIORef ref >>= replayInto (restoreKeyValue textSD fresh) . reverse
    (,) <$> (kvsAll inner >>= kvIteratorToList) <*> (kvsAll fresh >>= kvIteratorToList)
  replayed H.=== live
  -- and the model agrees with both
  let model = foldl' step Map.empty ops
      step m = \case
        Put k v -> Map.insert k v m
        PutIfAbsent k v -> Map.insertWith (\_ old -> old) k v m
        Delete k -> Map.delete k m
  live H.=== Map.toList model


window_replay :: IO ()
window_replay = do
  inner <- inMemoryWindowStore @Text @Text (storeName "w") 10 1000
  (ref, sink) <- recordingSink
  let st = logWindowStore textSD sink inner
  wsPut st "a" "1" (Timestamp 0)
  wsPut st "a" "2" (Timestamp 10)
  wsPut st "b" "3" (Timestamp 10)
  wsPut st "a" "4" (Timestamp 0)
  fresh <- inMemoryWindowStore @Text @Text (storeName "w") 10 1000
  readIORef ref >>= replayInto (restoreWindow textSD fresh) . reverse
  live <- wsAll inner >>= kvIteratorToList
  got <- wsAll fresh >>= kvIteratorToList
  got `shouldBe` live
  length got `shouldBe` 3


session_replay :: IO ()
session_replay = do
  inner <- inMemorySessionStore @Text @Text (storeName "s") 1000
  (ref, sink) <- recordingSink
  let st = logSessionStore textSD sink inner
      sk k a e = SessionKey k (Timestamp a) (Timestamp e)
  ssPut st (sk "a" 0 5) "x"
  ssPut st (sk "a" 20 25) "y"
  ssRemove st (sk "a" 0 5)
  ssPut st (sk "a" 0 30) "xy"
  fresh <- inMemorySessionStore @Text @Text (storeName "s") 1000
  readIORef ref >>= replayInto (restoreSession textSD fresh) . reverse
  live <- ssFindSessions inner "a" (Timestamp 0) (Timestamp 100) >>= kvIteratorToList
  got <- ssFindSessions fresh "a" (Timestamp 0) (Timestamp 100) >>= kvIteratorToList
  got `shouldBe` live
  map fst got `shouldBe` [sk "a" 20 25, sk "a" 0 30]


bytes :: Text -> ByteString
bytes = BSC.pack . T.unpack


countTopology :: IO TopologyValid
countTopology = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  let g = groupByKey (grouped textSerde textSerde) s
  _ <- countStream (materializedAs (storeName "counts")) g
  topo <- buildTopology b
  either (error . show) pure (validateTopology topo)


engine_logs_to_partition :: IO ()
engine_logs_to_partition = do
  v <- countTopology
  collector <- inMemoryCollector
  let clTopic = topicName "app-counts-changelog"
      opts =
        defaultEngineOptions
          { eoChangelog = \sn ->
              if sn == storeName "counts" then Just (ChangelogTarget clTopic 3) else Nothing
          }
  e <- buildEngineWith opts v (TaskId 0 3) "app" collector logAndContinue
  forM_ (zip [0 ..] ["a", "b", "a"]) $ \(off, k) ->
    feedSource e (topicName "in") (Just (bytes k)) (bytes "v") (Timestamp (100 + off)) 3 off
  logged <- collectorTake collector clTopic
  map crPartition logged `shouldBe` [Just 3, Just 3, Just 3]
  map crKey logged `shouldBe` map (Just . bytes) ["a", "b", "a"]
  -- Replaying onto a fresh, unlogged engine's store gives the same counts.
  e2 <- buildEngineWith defaultEngineOptions v (TaskId 0 3) "app" collector logAndContinue
  Just se <- storeByName e2 (storeName "counts")
  Just restore <- pure (storeEntryRestore se)
  forM_ logged $ \cr -> maybe (error "keyless changelog record") (\k -> restore k (crValue cr)) (crKey cr)
  case storeEntryAny se of
    AnyKeyValueStore kvs -> do
      let typed = unsafeCoerce kvs :: KeyValueStore Text Int64
      kvsGet typed "a" >>= (`shouldBe` Just 2)
      kvsGet typed "b" >>= (`shouldBe` Just 1)
    _ -> expectationFailure "counts is not a key-value store"


table_tombstone_deletes :: IO ()
table_tombstone_deletes = do
  b <- newStreamsBuilder
  kt <- tableFromTopic b (topicName "in") (consumed textSerde textSerde) (materializedAs (storeName "tab"))
  topo <- buildTopology b
  d <- newDriver topo "tomb-app"
  let tin = createInputTopic d (topicName "in") textSerde textSerde
  pipeKV tin (Just "a") "1"
  pipeKV tin (Just "b") "2"
  pipeTombstone tin "a"
  Just kvs <- getKeyValueStore @Text @Text d (ktableStore kt)
  kvsGet kvs "a" >>= (`shouldBe` Nothing)
  kvsGet kvs "b" >>= (`shouldBe` Just "2")
  closeDriver d


repartition_splits :: IO ()
repartition_splits = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  rekeyed <- selectKey (\r -> recordValue r) s
  let g = groupByKey (grouped textSerde textSerde) rekeyed
  _ <- countStream (materializedAs (storeName "by-value")) g
  topo <- buildTopology b
  let plan = planSubtopologies topo
      subs = spSubtopologies plan
  length subs `shouldBe` 2
  case subs of
    [first, second] -> do
      subSourceTopics first `shouldBe` [topicName "in"]
      subStores first `shouldBe` []
      subStores second `shouldBe` [storeName "by-value"]
      -- the repartition topic links them: written by one, read by the other
      subSinkTopics first `shouldBe` subSourceTopics second
      Map.keys (topoInternalTopics topo) `shouldBe` subSourceTopics second
    _ -> expectationFailure "expected two subtopologies"


global_table_is_separate :: IO ()
global_table_is_separate = do
  b <- newStreamsBuilder
  gt <- globalTable b (topicName "lookup") (consumed textSerde textSerde) (materializedAs (storeName "g"))
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  j <- joinKStreamGlobalKTable (\_ v -> v) (\v g -> v <> g :: Text) s gt
  toTopic (topicName "out") (produced textSerde textSerde) j
  topo <- buildTopology b
  let plan = planSubtopologies topo
  spGlobalStores plan `shouldBe` [storeName "g"]
  spGlobalTopics plan `shouldBe` [topicName "lookup"]
  length (spSubtopologies plan) `shouldBe` 1
  forM_ (spSubtopologies plan) $ \st ->
    Set.disjoint (subNodes st) (spGlobalNodes plan) `shouldBe` True
