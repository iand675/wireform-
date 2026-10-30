{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | The record cache in front of a key-value store, on its own.
module Streams.CacheSpec (tests) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.IORef
import Data.Map.Strict qualified as Map
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Kafka.Streams.Serde (Serde (..), int64Serde, textSerde)
import Kafka.Streams.State.KeyValue.Caching
import Kafka.Streams.State.KeyValue.InMemory (inMemoryKeyValueStore)
import Kafka.Streams.State.Window.InMemory (inMemoryWindowStore)
import Kafka.Streams.State.Session.InMemory (inMemorySessionStore)
import Kafka.Streams.Serde.Windowed (sessionKeySerde)
import Kafka.Streams.Serde.Windowed (windowedSerde)
import Kafka.Streams.State.Store
import Kafka.Streams.Time (Timestamp (..))
import Kafka.Streams.Types (emptyHeaders)
import Data.Int (Int64)
import Data.Text (Text)
import Test.Syd
import Test.Syd.Hedgehog ()
import Control.Exception (finally)
import Kafka.Streams.Imperative qualified as I
import Kafka.Streams.Runtime.Driver (DriverRecord (..))
import Kafka.Streams.Runtime.MockBroker
import Streams.RuntimeHarness


tests :: Spec
tests =
  describe "Record cache" $ do
    it "forwards one change per key per flush, old value from the store" dedups
    it "reads (get, range, IQ) see the buffered writes" reads_through
    it "a key added and deleted between flushes is neither written nor forwarded" add_then_delete
    it "flushes in the order of the last write" flush_order
    it "logs flushed entries from the bytes it serialised" logs_bytes
    it "flushes the store being written once its task is over budget" over_budget
    it "is the store plus one change per flushed key (model)" prop_model
    it "at runtime: a hot key is forwarded and logged once per commit" runtime_dedups
    it "at runtime: withCachingDisabled forwards every update" runtime_uncached
    it "a window cache folds a window's updates and reads through" window_cache
    it "at runtime: a hot window is forwarded once per commit" runtime_windowed
    it "a session cache folds merges, and a merged-away unflushed session never surfaces" session_cache
    it "at runtime: a growing session is forwarded once per commit" runtime_session


data Fixture = Fixture
  { fxCache :: CachedStore Text Int64
  , fxInner :: KeyValueStore Text Int64
  , fxForwarded :: IORef [(Text, Maybe Int64, Maybe Int64, Timestamp)]
  , fxLogged :: IORef [(ByteString, Maybe ByteString)]
  , fxBudget :: CacheBudget
  }


fixture :: Int -> IO Fixture
fixture limit = do
  inner <- inMemoryKeyValueStore @Text @Int64 (storeName "s")
  budget <- newCacheBudget (pure limit)
  logRef <- newIORef []
  cs <- cachedKeyValueStore budget (Just (StoreSerdes textSerde int64Serde, \k v -> modifyIORef' logRef (<> [(k, v)]))) inner
  fwd <- newIORef []
  forM_ (kvsCache (csStore cs)) $ \c ->
    scOnFlush c (\k new old ts _ -> modifyIORef' fwd (<> [(k, new, old, ts)]))
  pure (Fixture cs inner fwd logRef budget)


write :: Fixture -> Text -> Maybe Int64 -> Int64 -> IO ()
write fx k v ts = forM_ (kvsCache (csStore fx.fxCache)) $ \c -> scWrite c k v (Timestamp ts) emptyHeaders


dedups :: IO ()
dedups = do
  fx <- fixture 1_000_000
  kvsPut fx.fxInner "k" 7
  mapM_ (\v -> write fx "k" (Just v) v) [1 .. 5]
  readIORef fx.fxForwarded `shouldReturn` []
  kvsGet fx.fxInner "k" `shouldReturn` Just 7
  csFlush fx.fxCache `shouldReturn` True
  readIORef fx.fxForwarded `shouldReturn` [("k", Just 5, Just 7, Timestamp 5)]
  kvsGet fx.fxInner "k" `shouldReturn` Just 5
  csFlush fx.fxCache `shouldReturn` False
  cacheBytesUsed fx.fxBudget `shouldReturn` 0


reads_through :: IO ()
reads_through = do
  fx <- fixture 1_000_000
  let st = csStore fx.fxCache
  mapM_ (\(k, v) -> kvsPut fx.fxInner k v) [("c", 30), ("d", 40), ("e", 50)]
  write fx "a" (Just 1) 0
  write fx "c" (Just 999) 0
  write fx "e" Nothing 0
  kvsGet st "c" `shouldReturn` Just 999
  kvsGet st "e" `shouldReturn` Nothing
  (kvsRange st "a" "z" >>= kvIteratorToList) `shouldReturn` [("a", 1), ("c", 999), ("d", 40)]
  (kvsReverseAll st >>= kvIteratorToList) `shouldReturn` [("d", 40), ("c", 999), ("a", 1)]
  (kvsRange st "b" "d" >>= kvIteratorToList) `shouldReturn` [("c", 999), ("d", 40)]


add_then_delete :: IO ()
add_then_delete = do
  fx <- fixture 1_000_000
  write fx "k" (Just 1) 0
  write fx "k" Nothing 0
  _ <- csFlush fx.fxCache
  readIORef fx.fxForwarded `shouldReturn` []
  readIORef fx.fxLogged `shouldReturn` []
  -- but deleting a row the store has is forwarded and logged
  kvsPut fx.fxInner "j" 3
  write fx "j" Nothing 9
  _ <- csFlush fx.fxCache
  readIORef fx.fxForwarded `shouldReturn` [("j", Nothing, Just 3, Timestamp 9)]
  readIORef fx.fxLogged `shouldReturn` [("j", Nothing)]
  kvsGet fx.fxInner "j" `shouldReturn` Nothing


flush_order :: IO ()
flush_order = do
  fx <- fixture 1_000_000
  write fx "a" (Just 1) 0
  write fx "b" (Just 2) 0
  write fx "a" (Just 3) 0
  write fx "c" (Just 4) 0
  _ <- csFlush fx.fxCache
  map (\(k, _, _, _) -> k) <$> readIORef fx.fxForwarded `shouldReturn` ["b", "a", "c"]


logs_bytes :: IO ()
logs_bytes = do
  fx <- fixture 1_000_000
  write fx "k" (Just 1) 0
  write fx "k" (Just 2) 0
  _ <- csFlush fx.fxCache
  readIORef fx.fxLogged `shouldReturn` [(serialize textSerde "k", Just (serialize int64Serde 2))]


over_budget :: IO ()
over_budget = do
  -- room for about two entries
  let entry = 1 + 8 + entryOverhead
  fx <- fixture (2 * entry + 1)
  write fx "a" (Just 1) 0
  write fx "b" (Just 2) 0
  readIORef fx.fxForwarded `shouldReturn` []
  cacheBytesUsed fx.fxBudget `shouldReturn` 2 * entry
  -- overwriting doesn't grow it
  write fx "a" (Just 3) 0
  readIORef fx.fxForwarded `shouldReturn` []
  write fx "c" (Just 4) 0
  map (\(k, new, _, _) -> (k, new)) <$> readIORef fx.fxForwarded `shouldReturn` [("b", Just 2), ("a", Just 3), ("c", Just 4)]
  cacheBytesUsed fx.fxBudget `shouldReturn` 0


data Op = Put !Text !Int64 | Del !Text | Flush
  deriving stock (Show)


prop_model :: H.Property
prop_model = H.property $ do
  let key = Gen.element ["a", "b", "c", "d"]
  ops <-
    H.forAll $
      Gen.list
        (Range.linear 0 60)
        (Gen.frequency [(5, Put <$> key <*> Gen.int64 (Range.linear 0 9)), (2, Del <$> key), (1, pure Flush)])
  limit <- H.forAll (Gen.element [1_000_000, 150])
  (reads_, flushed, forwarded, final) <- liftIO $ do
    fx <- fixture limit
    readsRef <- newIORef []
    forM_ ops $ \op -> do
      case op of
        Put k v -> write fx k (Just v) 0
        Del k -> write fx k Nothing 0
        Flush -> () <$ csFlush fx.fxCache
      got <- mapM (kvsGet (csStore fx.fxCache)) ["a", "b", "c", "d"]
      modifyIORef' readsRef (<> [got])
    _ <- csFlush fx.fxCache
    inner <- kvsAll fx.fxInner >>= kvIteratorToList
    fwd <- readIORef fx.fxForwarded
    used <- cacheBytesUsed fx.fxBudget
    (,,,) <$> readIORef readsRef <*> pure inner <*> pure fwd <*> pure used
  let step m = \case
        Put k v -> Map.insert k v m
        Del k -> Map.delete k m
        Flush -> m
      models = drop 1 (scanl step Map.empty ops)
      expectedReads = map (\m -> map (`Map.lookup` m) ["a", "b", "c", "d"]) models
      model = foldl step Map.empty ops
  -- reads always see the latest write
  reads_ H.=== expectedReads
  -- after the last flush the store is the model
  flushed H.=== Map.toAscList model
  final H.=== 0
  -- the forwarded changes replay to the model, each old value being
  -- what the key held after its previous change, and never a no-op
  let replay (m, ok) (k, new, old, _) =
        ( maybe (Map.delete k) (Map.insert k) new m
        , ok && Map.lookup k m == old && not (null new && null old)
        )
      (replayed, consistent) = foldl replay (Map.empty, True) forwarded
  replayed H.=== model
  H.assert consistent


-- | Counts per key of @in@ to @out@, its store cached unless @uncached@.
counting :: Bool -> IO I.TopologyValid
counting uncached = do
  b <- I.newStreamsBuilder
  st <- I.streamFromTopic b (I.topicName "in") (I.consumed textSerde textSerde)
  let m = I.materializedAs (storeName "counts")
  counts <- I.countStream (if uncached then I.withCachingDisabled m else m) (I.groupByKey (I.grouped textSerde textSerde) st)
  out <- I.toKStreamFromKTable counts
  I.toTopic (I.topicName "out") (I.produced textSerde int64Serde) out
  I.buildTopology b >>= either (error . show) pure . I.validateTopology


-- | Feed 200 records over 2 keys in one go; how many reached @out@ and the changelog.
hotKeys :: Bool -> IO (Int, Int)
hotKeys uncached = withStateDirs 1 $ \[d] -> do
  broker <- newMockBroker (Just 1)
  mockCreateTopic broker "in" 1
  topo <- counting uncached
  -- a long commit interval: everything lands between two commits
  ks <- startOn broker ((testConfig "cache" d) {I.commitIntervalMs = Just 60_000}) topo
  ( do
      eventually [ks] "assigned" (not . null <$> I.ownedPartitions ks)
      forM_ [1 .. 100 :: Int] $ \_ -> forM_ ["a", "b"] $ \k ->
        mockProduce broker "in" (Just (textBytes k)) (Just "x") 0
      -- the records are processed, then a clean close commits
      _ <- I.awaitTicks ks 50
      closeAll [ks]
      outs <- visible broker "out"
      logged <- visible broker "cache-counts-changelog"
      let latest = Map.map (fmap decodeI64) (latestBy outs)
      latest `shouldBe` Map.fromList [(textBytes "a", Just 100), (textBytes "b", Just 100)]
      pure (length outs, length (map (.drKey) logged))
    )
    `finally` closeAll [ks]


runtime_dedups :: IO ()
runtime_dedups = do
  (outs, logged) <- hotKeys False
  outs `shouldSatisfy` (< 20)
  logged `shouldSatisfy` (< 20)


runtime_uncached :: IO ()
runtime_uncached = hotKeys True `shouldReturn` (200, 200)


window_cache :: IO ()
window_cache = do
  inner <- inMemoryWindowStore @Text @Int64 (storeName "w") 100 1000
  budget <- newCacheBudget (pure 1_000_000)
  logRef <- newIORef []
  cw <- cachedWindowStore budget (Just (StoreSerdes textSerde int64Serde, \k v -> modifyIORef' logRef (<> [(k, v)]))) inner
  fwd <- newIORef []
  let st = cwStore cw
  forM_ (wsCache st) $ \c -> wcOnFlush c (\k start v ts _ -> modifyIORef' fwd (<> [(k, start, v, ts)]))
  let w k start v ts = forM_ (wsCache st) $ \c -> wcWrite c k v (Timestamp start) (Timestamp ts) emptyHeaders
  w "a" 0 1 5
  w "a" 0 2 7
  w "a" 100 9 150
  w "b" 0 4 8
  -- reads see the buffered windows
  wsFetch st "a" (Timestamp 0) `shouldReturn` Just 2
  (wsFetchRange st "a" (Timestamp 0) (Timestamp 100) >>= kvIteratorToList) `shouldReturn` [(Timestamp 0, 2), (Timestamp 100, 9)]
  (wsFetchAllRange st (Timestamp 0) (Timestamp 0) >>= kvIteratorToList) `shouldReturn` [(WindowedKey "a" (Timestamp 0), 2), (WindowedKey "b" (Timestamp 0), 4)]
  wsFetch inner "a" (Timestamp 0) `shouldReturn` Nothing
  readIORef fwd `shouldReturn` []
  cwFlush cw `shouldReturn` True
  -- one update per window, in last-write order, at the last write's timestamp
  readIORef fwd `shouldReturn` [("a", Timestamp 0, 2, Timestamp 7), ("a", Timestamp 100, 9, Timestamp 150), ("b", Timestamp 0, 4, Timestamp 8)]
  length <$> readIORef logRef `shouldReturn` 3
  wsFetch inner "a" (Timestamp 0) `shouldReturn` Just 2
  cacheBytesUsed budget `shouldReturn` 0


-- | 100 records in one window of one key; how many reached @out@.
runtime_windowed :: IO ()
runtime_windowed = withStateDirs 1 $ \[d] -> do
  broker <- newMockBroker (Just 1)
  mockCreateTopic broker "in" 1
  b <- I.newStreamsBuilder
  st0 <- I.streamFromTopic b (I.topicName "in") (I.consumed textSerde textSerde)
  h <- I.countWindowed (I.withValueSerde int64Serde I.materialized) (I.windowedByTime (I.tumblingWindows (I.millis 1000)) (I.groupByKey (I.grouped textSerde textSerde) st0))
  st <- I.streamFromWindowedHandle h textSerde int64Serde
  I.toTopic (I.topicName "out") (I.produced (windowedSerde textSerde) int64Serde) st
  topo <- I.buildTopology b >>= either (error . show) pure . I.validateTopology
  ks <- startOn broker ((testConfig "wcache" d) {I.commitIntervalMs = Just 60_000}) topo
  ( do
      eventually [ks] "assigned" (not . null <$> I.ownedPartitions ks)
      forM_ [1 .. 100 :: Int] $ \i -> mockProduce broker "in" (Just (textBytes "k")) (Just "x") (fromIntegral i)
      _ <- I.awaitTicks ks 50
      closeAll [ks]
      outs <- visible broker "out"
      map (fmap decodeI64 . (.drValue)) outs `shouldSatisfy` (\vs -> length vs < 10 && lastMaybe vs == Just (Just 100))
    )
    `finally` closeAll [ks]
  where
    lastMaybe = foldl (\_ x -> Just x) Nothing


session_cache :: IO ()
session_cache = do
  inner <- inMemorySessionStore @Text @Int64 (storeName "s") 10_000
  budget <- newCacheBudget (pure 1_000_000)
  logRef <- newIORef []
  cs <- cachedSessionStore budget (Just (StoreSerdes textSerde int64Serde, \k v -> modifyIORef' logRef (<> [(k, v)]))) inner
  fwd <- newIORef []
  let st = csStoreS cs
      sess a b0 = SessionKey "k" (Timestamp a) (Timestamp b0)
      w sk v = forM_ (ssCache st) $ \c -> scsWrite c sk v (skEnd sk) emptyHeaders
  forM_ (ssCache st) $ \c -> scsOnFlush c (\sk new old _ _ -> modifyIORef' fwd (<> [(sk, new, old)]))
  -- a session, then merged into a longer one before any flush
  w (sess 0 0) (Just 1)
  w (sess 0 0) Nothing
  w (sess 0 50) (Just 2)
  (ssFindSessions st "k" (Timestamp 0) (Timestamp 100) >>= kvIteratorToList) `shouldReturn` [(sess 0 50, 2)]
  _ <- csFlushS cs
  readIORef fwd `shouldReturn` [(sess 0 50, Just 2, Nothing)]
  length <$> readIORef logRef `shouldReturn` 1
  -- merging a flushed session retracts it
  w (sess 0 50) Nothing
  w (sess 0 90) (Just 3)
  (ssFindSessions st "k" (Timestamp 0) (Timestamp 100) >>= kvIteratorToList) `shouldReturn` [(sess 0 90, 3)]
  ssFetchSession st (sess 0 50) `shouldReturn` Nothing
  _ <- csFlushS cs
  drop 1 <$> readIORef fwd `shouldReturn` [(sess 0 50, Nothing, Just 2), (sess 0 90, Just 3, Nothing)]
  drop 1 . map snd <$> readIORef logRef `shouldReturn` [Nothing, Just (serialize int64Serde 3)]
  (ssFindAllSessions inner (Timestamp 0) (Timestamp 100) >>= kvIteratorToList) `shouldReturn` [(sess 0 90, 3)]
  cacheBytesUsed budget `shouldReturn` 0


-- | 100 records extending one session; what reached @out@.
runtime_session :: IO ()
runtime_session = withStateDirs 1 $ \[d] -> do
  broker <- newMockBroker (Just 1)
  mockCreateTopic broker "in" 1
  b <- I.newStreamsBuilder
  st0 <- I.streamFromTopic b (I.topicName "in") (I.consumed textSerde textSerde)
  h <- I.countSessionWindowed (I.withValueSerde int64Serde I.materialized) (I.windowedBySession (I.sessionWindows (I.millis 1000)) (I.groupByKey (I.grouped textSerde textSerde) st0))
  st <- I.streamFromSessionWindowedHandle h textSerde int64Serde
  I.toTopicNullable (I.topicName "out") (I.produced (sessionKeySerde textSerde) int64Serde) st
  topo <- I.buildTopology b >>= either (error . show) pure . I.validateTopology
  ks <- startOn broker ((testConfig "scache" d) {I.commitIntervalMs = Just 60_000}) topo
  ( do
      eventually [ks] "assigned" (not . null <$> I.ownedPartitions ks)
      forM_ [1 .. 100 :: Int] $ \i -> mockProduce broker "in" (Just (textBytes "k")) (Just "x") (fromIntegral i)
      _ <- I.awaitTicks ks 50
      closeAll [ks]
      outs <- visible broker "out"
      -- no retraction of a session that never left the cache, and the last count is 100
      map (fmap decodeI64 . (.drValue)) outs `shouldSatisfy` (\vs -> length vs < 10 && all (/= Nothing) vs && lastMaybe vs == Just (Just 100))
    )
    `finally` closeAll [ks]
  where
    lastMaybe = foldl (\_ x -> Just x) Nothing
