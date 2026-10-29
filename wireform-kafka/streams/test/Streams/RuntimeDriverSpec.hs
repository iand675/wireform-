{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | Tests for the 'KafkaStreams' runtime driven through a
'StreamDriver' rather than against a real broker. The runtime
consumes the same 'StreamDriver' record-of-IO that
'newNativeDriver' returns; this spec uses 'newMockDriver' to
assert end-to-end behaviour deterministically.

We cover:

  * Source records pumped into the mock driver flow through
    the topology and produce sink records the mock driver
    captures.
  * Subscribe / commit / close IO actions fire in the
    expected order.
  * Pause halts engine feeding without halting polling.
  * EOS-V2 commit cycles route through the bound
    'EOSCoordinator' (begin → commitOffsets → commit).
-}
module Streams.RuntimeDriverSpec (tests) where

import Control.Concurrent qualified
import Data.ByteString.Char8 qualified as BSC
import Data.IORef
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Client.Consumer qualified as KC
import Kafka.Client.RebalanceListener qualified as RBL
import Kafka.Streams.Imperative
import Kafka.Streams.Runtime.EOS (
  EOSCoordinator (..),
 )
import Kafka.Streams.Runtime.NativeDriver (
  MockSend (..),
  MockTxnEvent (..),
  RebalanceEvent (..),
  mockDriverCommitCount,
  mockDriverDrainSends,
  mockDriverInjectPoll,
  mockDriverInjectRebalance,
  mockDriverTxnLog,
  newMockDriver,
 )
import Test.Syd


tests :: Spec
tests =
  describe "Runtime <-> StreamDriver" $
    sequence_
      [ records_pumped_through_topology
      , pause_stops_engine_but_not_polling
      , commit_cycle_invokes_eos_coordinator
      , multi_thread_runtime_dispatches_by_partition
      , multi_thread_runtime_drains_collectors_at_commit
      , rebalance_assigned_updates_owned_partitions
      , rebalance_revoked_moves_to_standby_grace
      , rebalance_lost_drops_without_grace
      , rebalance_reassignment_promotes_standby
      , two_instances_handoff_partitions_via_rebalance
      ]


bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack


unbytes :: BSC.ByteString -> Text
unbytes = T.pack . BSC.unpack


----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

{- | Build a one-source / one-sink topology that uppercases its
value. Used as the lightweight workload for the runtime tests.
-}
buildUpcaseTopo :: IO TopologyValid
buildUpcaseTopo = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  upper <- mapValues (T.toUpper :: Text -> Text) s
  toTopic (topicName "out") (produced textSerde textSerde) upper
  topo <- buildTopology b
  case validateTopology topo of
    Left err -> error (show err)
    Right v -> pure v


consumerRecord :: Text -> Text -> Text -> Int64ish -> Int64ish -> KC.ConsumerRecord
consumerRecord topic k v off ts =
  KC.ConsumerRecord
    { topic = topic
    , partition = 0
    , offset = fromIntegral (unI64 off)
    , timestamp = fromIntegral (unI64 ts)
    , key = Just (bytes k)
    , value = Just (bytes v)
    , headers = []
    }


newtype Int64ish = Int64ish {unI64 :: Integer}


----------------------------------------------------------------------
-- 1. Records flow through the topology
----------------------------------------------------------------------

records_pumped_through_topology :: Spec
records_pumped_through_topology =
  it "records injected via the mock driver produce sink records" $ do
    topo <- buildUpcaseTopo
    let cfg =
          defaultStreamsConfig
            { applicationId = "rt-app-1"
            , bootstrapServers = ["mock:0"]
            , numStreamThreads = 1
            , pollMs = 0
            }
    ks <- newKafkaStreams cfg topo
    (drv, h) <- newMockDriver

    -- Push two records the runtime will see on the next poll.
    mockDriverInjectPoll
      h
      [ consumerRecord "in" "k1" "hello" (Int64ish 0) (Int64ish 100)
      , consumerRecord "in" "k2" "world" (Int64ish 1) (Int64ish 101)
      ]

    startKafkaStreamsWith ks drv
    awaitState ks StreamsRunning

    -- Wait deterministically: the runtime keeps polling, the
    -- queue is finite, and on every pass the commit count goes
    -- up. Once we've seen at least one commit AND the captured
    -- sends contain both records we know the batch was processed.
    sentRef <- newIORef []
    waitForKs ks 5000 $ do
      sends <- mockDriverDrainSends h
      modifyIORef' sentRef (++ sends)
      acc <- readIORef sentRef
      pure (length acc == 2 && all ((== "out") . mockSendTopic) acc)

    finalSends <- readIORef sentRef
    map (unbytes . mockSendValue) finalSends `shouldBe` ["HELLO", "WORLD"]

    closeKafkaStreams ks
    awaitState ks StreamsClosed


----------------------------------------------------------------------
-- 2. Pause stops feeding the engine but keeps polling
----------------------------------------------------------------------

pause_stops_engine_but_not_polling :: Spec
pause_stops_engine_but_not_polling =
  it "pause halts engine forwarding but keeps polling" $ do
    topo <- buildUpcaseTopo
    let cfg =
          defaultStreamsConfig
            { applicationId = "rt-app-2"
            , bootstrapServers = ["mock:0"]
            , numStreamThreads = 1
            , pollMs = 0
            }
    ks <- newKafkaStreams cfg topo
    (drv, h) <- newMockDriver

    startKafkaStreamsWith ks drv
    awaitState ks StreamsRunning

    pauseKafkaStreams ks
    isPausedKafkaStreams ks >>= (`shouldBe` True)

    -- Inject records while paused; nothing should reach the sink.
    mockDriverInjectPoll
      h
      [ consumerRecord "in" "k1" "while-paused" (Int64ish 0) (Int64ish 100)
      ]

    -- Drain a few times so the loop has run; the runtime is still
    -- polling so the queue empties, but no sends fire.
    let drainPolls 0 = pure ()
        drainPolls n = do
          mockDriverInjectPoll h []
          drainPolls (n - 1)
    drainPolls 10

    sendsWhilePaused <- mockDriverDrainSends h
    sendsWhilePaused `shouldBe` []

    -- Resume and inject again. Now records should flow.
    resumeKafkaStreams ks
    mockDriverInjectPoll
      h
      [ consumerRecord "in" "k2" "after-resume" (Int64ish 1) (Int64ish 101)
      ]
    waitForKs ks 5000 $ do
      ss <- mockDriverDrainSends h
      pure (any (\s -> mockSendValue s == bytes "AFTER-RESUME") ss)

    closeKafkaStreams ks
    awaitState ks StreamsClosed


----------------------------------------------------------------------
-- 3. EOS-V2 commit cycle routes through the coordinator
----------------------------------------------------------------------

commit_cycle_invokes_eos_coordinator :: Spec
commit_cycle_invokes_eos_coordinator =
  it "EOS commit cycle: runtime drives begin → commitOffsets → commit through the coordinator" $ do
    topo <- buildUpcaseTopo
    let cfg =
          defaultStreamsConfig
            { applicationId = "rt-app-3"
            , bootstrapServers = ["mock:0"]
            , numStreamThreads = 1
            , pollMs = 0
            , processingGuarantee = ExactlyOnceV2
            }
    ks <- newKafkaStreams cfg topo
    (drv, h) <- newMockDriver

    -- Recording coordinator: every callback appends a tag.
    callsRef <- newIORef ([] :: [Text])
    let log_ s = modifyIORef' callsRef (s :)
    let coord =
          EOSCoordinator
            { initTxn = log_ "init" *> pure (Right ())
            , beginTxn = log_ "begin" *> pure (Right ())
            , commitTxn = log_ "commit" *> pure (Right ())
            , abortTxn = log_ "abort" *> pure (Right ())
            , commitOffsets = \_ _ ->
                log_ "commitOffsets" *> pure (Right ())
            , storeCommit = log_ "storeCommit" *> pure (Right ())
            , storeAbort = log_ "storeAbort" *> pure (Right ())
            , preCommit2PC = pure (Right ())
            , commit2PC = pure (Right ())
            , abort2PC = pure ()
            }
    applyEOSCoordinator ks coord

    mockDriverInjectPoll
      h
      [ consumerRecord "in" "k1" "x" (Int64ish 0) (Int64ish 100)
      ]
    startKafkaStreamsWith ks drv
    awaitState ks StreamsRunning

    -- Wait for at least one full begin/commitOffsets/commit
    -- cycle to fire by observing the coordinator log.
    waitForKs ks 5000 $ do
      cs <- reverse <$> readIORef callsRef
      pure (take 3 cs == ["begin", "commitOffsets", "commit"])

    closeKafkaStreams ks
    awaitState ks StreamsClosed

    cs <- reverse <$> readIORef callsRef
    -- The first three entries in order must be the begin /
    -- commitOffsets / commit triple.
    take 3 cs `shouldBe` ["begin", "commitOffsets", "commit"]

    -- Ensure the runtime did NOT drive the producer's transaction
    -- hooks itself (those go through the coordinator under the
    -- AtLeastOnce defaults of the mock driver).
    txn <- mockDriverTxnLog h
    txn `shouldBe` []

    -- And it did call the regular consumer commit at least once
    -- (the runtime issues sdConsumerCommit after every successful
    -- commit cycle).
    cnt <- mockDriverCommitCount h
    (cnt >= 1) `shouldBe` True


----------------------------------------------------------------------
-- 4. Multi-thread runtime: per-partition dispatch
----------------------------------------------------------------------

multi_thread_runtime_dispatches_by_partition :: Spec
multi_thread_runtime_dispatches_by_partition =
  it "numStreamThreads=4: every partition's records reach the sink" $ do
    topo <- buildUpcaseTopo
    let cfg =
          defaultStreamsConfig
            { applicationId = "rt-multi-1"
            , bootstrapServers = ["mock:0"]
            , numStreamThreads = 4
            , pollMs = 0
            }
    ks <- newKafkaStreams cfg topo
    (drv, h) <- newMockDriver

    let recOn p =
          consumerRecordPart
            "in"
            p
            ("k" <> textShow p)
            "hello"
            (Int64ish (fromIntegral p))
            (Int64ish 100)
    -- 8 records spread across 4 partitions (2 per partition).
    mockDriverInjectPoll
      h
      [ recOn 0
      , recOn 1
      , recOn 2
      , recOn 3
      , recOn 0
      , recOn 1
      , recOn 2
      , recOn 3
      ]

    startKafkaStreamsWith ks drv
    awaitState ks StreamsRunning

    sentRef <- newIORef []
    waitForKs ks 5000 $ do
      sends <- mockDriverDrainSends h
      modifyIORef' sentRef (++ sends)
      acc <- readIORef sentRef
      pure (length acc == 8 && all ((== "out") . mockSendTopic) acc)

    finalSends <- readIORef sentRef
    -- Order across workers isn't guaranteed (parallel), but
    -- every send must be the upper-cased value and there must
    -- be exactly 8 of them.
    map (unbytes . mockSendValue) finalSends
      `shouldBe` replicate 8 "HELLO"

    closeKafkaStreams ks
    awaitState ks StreamsClosed


----------------------------------------------------------------------
-- 5. Multi-thread runtime: collector drain + IQ federation
----------------------------------------------------------------------

multi_thread_runtime_drains_collectors_at_commit :: Spec
multi_thread_runtime_drains_collectors_at_commit =
  it "numStreamThreads=2: per-key counts are partition-local + federated read returns the union" $ do
    topo <- buildCountTopo
    let cfg =
          defaultStreamsConfig
            { applicationId = "rt-multi-2"
            , bootstrapServers = ["mock:0"]
            , numStreamThreads = 2
            , pollMs = 0
            }
    ks <- newKafkaStreams cfg topo
    (drv, h) <- newMockDriver

    -- Three distinct keys on three distinct partitions; the
    -- (topic, partition) hash decides which worker each lands
    -- on, and we don't care which one — only that every key
    -- shows up in the federated IQ view with the right count.
    mockDriverInjectPoll
      h
      [ consumerRecordPart "in" 0 "alice" "1" (Int64ish 0) (Int64ish 100)
      , consumerRecordPart "in" 1 "bob" "1" (Int64ish 1) (Int64ish 100)
      , consumerRecordPart "in" 2 "carol" "1" (Int64ish 2) (Int64ish 100)
      , consumerRecordPart "in" 0 "alice" "1" (Int64ish 3) (Int64ish 100)
      , consumerRecordPart "in" 0 "alice" "1" (Int64ish 4) (Int64ish 100)
      ]

    startKafkaStreamsWith ks drv
    awaitState ks StreamsRunning

    -- Wait until the federated count shows the expected totals.
    waitForKs ks 5000 $ do
      mIQ <- queryKVStore @Text @Int64 ks (storeName "counts")
      case mIQ of
        Nothing -> pure False
        Just kvs -> do
          a <- kvs.roKvGet "alice"
          b <- kvs.roKvGet "bob"
          c <- kvs.roKvGet "carol"
          pure (a == Just 3 && b == Just 1 && c == Just 1)

    closeKafkaStreams ks
    awaitState ks StreamsClosed


----------------------------------------------------------------------
-- Helpers shared by multi-thread tests
----------------------------------------------------------------------

textShow :: Show a => a -> Text
textShow = T.pack . show


-- | Build a count-by-key topology. Used by the IQ federation test.
buildCountTopo :: IO TopologyValid
buildCountTopo = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  let g = grouped textSerde textSerde
      kgs = groupByKey g s
  _ <- countStream (materializedAs (storeName "counts")) kgs
  topo <- buildTopology b
  case validateTopology topo of
    Left err -> error (show err)
    Right v -> pure v


consumerRecordPart
  :: Text
  -> Int
  -> Text
  -> Text
  -> Int64ish
  -> Int64ish
  -> KC.ConsumerRecord
consumerRecordPart topic part k v off ts =
  KC.ConsumerRecord
    { topic = topic
    , partition = fromIntegral part
    , offset = fromIntegral (unI64 off)
    , timestamp = fromIntegral (unI64 ts)
    , key = Just (bytes k)
    , value = Just (bytes v)
    , headers = []
    }


----------------------------------------------------------------------
-- 6. Multi-instance rebalance handling (KIP-415/429/441/869)
----------------------------------------------------------------------

{- | Build a 'KafkaStreams' running the upcase topology with the
supplied 'taskTimeoutMs' (which the runtime uses as the
standby grace window).
-}
buildRebalanceFixture
  :: Int
  -- ^ task.timeout.ms
  -> IO (KafkaStreams, IORef [(Text, [KC.TopicPartition])])
buildRebalanceFixture graceMs = do
  topo <- buildUpcaseTopo
  let cfg =
        defaultStreamsConfig
          { applicationId = "rt-rebal"
          , bootstrapServers = ["mock:0"]
          , numStreamThreads = 1
          , pollMs = 0
          , taskTimeoutMs = graceMs
          }
  ks <- newKafkaStreams cfg topo
  log_ <- newIORef ([] :: [(Text, [KC.TopicPartition])])
  let rec_ tag tps = modifyIORef' log_ ((tag, tps) :)
  setRebalanceListener
    ks
    RBL.RebalanceListener
      { RBL.rlOnAssigned = rec_ "assigned"
      , RBL.rlOnRevoked = rec_ "revoked"
      , RBL.rlOnLost = rec_ "lost"
      }
  pure (ks, log_)


rebalance_assigned_updates_owned_partitions :: Spec
rebalance_assigned_updates_owned_partitions =
  it "RebalanceAssigned: ksOwned gains the new tps; listener fires" $ do
    (ks, log_) <- buildRebalanceFixture 0
    (drv, h) <- newMockDriver
    let tp0 = KC.TopicPartition "in" 0
        tp1 = KC.TopicPartition "in" 1
    mockDriverInjectRebalance h (RebalanceAssigned [tp0, tp1])
    startKafkaStreamsWith ks drv
    awaitState ks StreamsRunning

    waitForKs ks 5000 $ do
      owned <- ownedPartitions ks
      pure (length owned == 2)

    owned <- ownedPartitions ks
    -- Order is not deterministic across HashSet; compare as sets.
    map (\(KC.TopicPartition t p) -> (t, p)) (sortBy_ owned)
      `shouldBe` [("in", 0), ("in", 1)]

    entries <- reverse <$> readIORef log_
    map fst entries `shouldBe` ["assigned"]
    map snd entries `shouldBe` [[tp0, tp1]]

    closeKafkaStreams ks
    awaitState ks StreamsClosed


rebalance_revoked_moves_to_standby_grace :: Spec
rebalance_revoked_moves_to_standby_grace =
  it "RebalanceRevoked with non-zero grace: tp moves to ksStandbys" $ do
    (ks, _log) <- buildRebalanceFixture 60_000
    (drv, h) <- newMockDriver
    let tp = KC.TopicPartition "in" 0
    mockDriverInjectRebalance h (RebalanceAssigned [tp])
    mockDriverInjectRebalance h (RebalanceRevoked [tp])
    startKafkaStreamsWith ks drv
    awaitState ks StreamsRunning

    -- After draining both events: owned must be empty AND
    -- the revoked tp must appear in the standby map with a
    -- deadline a non-trivial time in the future.
    waitForKs ks 5000 $ do
      owned <- ownedPartitions ks
      stby <- standbyTasks ks
      pure (null owned && Map.member tp stby)

    owned <- ownedPartitions ks
    owned `shouldBe` []
    stby <- standbyTasks ks
    -- The deadline should be a real timestamp, > 0.
    case Map.lookup tp stby of
      Just dl -> (if (dl > 0) then pure () else expectationFailure ("standby deadline > 0; got " <> show dl))
      Nothing -> error "expected standby entry"

    closeKafkaStreams ks
    awaitState ks StreamsClosed


rebalance_lost_drops_without_grace :: Spec
rebalance_lost_drops_without_grace =
  it "RebalanceLost: tp drops immediately; no standby entry; listener fires" $ do
    (ks, log_) <- buildRebalanceFixture 60_000
    (drv, h) <- newMockDriver
    let tp = KC.TopicPartition "in" 0
    mockDriverInjectRebalance h (RebalanceAssigned [tp])
    mockDriverInjectRebalance h (RebalanceLost [tp])
    startKafkaStreamsWith ks drv
    awaitState ks StreamsRunning

    waitForKs ks 5000 $ do
      owned <- ownedPartitions ks
      stby <- standbyTasks ks
      tags <- map fst <$> readIORef log_
      pure
        ( null owned
            && Map.null stby
            && "lost" `elem` tags
        )

    -- The listener saw both assigned then lost, in order.
    entries <- reverse <$> readIORef log_
    map fst entries `shouldBe` ["assigned", "lost"]

    closeKafkaStreams ks
    awaitState ks StreamsClosed


rebalance_reassignment_promotes_standby :: Spec
rebalance_reassignment_promotes_standby =
  it "Re-assignment of a standby tp clears the standby and re-adds to owned" $ do
    (ks, _log) <- buildRebalanceFixture 60_000
    (drv, h) <- newMockDriver
    let tp = KC.TopicPartition "in" 0
    -- assign -> revoke (standby) -> re-assign (promote back to active)
    mockDriverInjectRebalance h (RebalanceAssigned [tp])
    mockDriverInjectRebalance h (RebalanceRevoked [tp])
    mockDriverInjectRebalance h (RebalanceAssigned [tp])
    startKafkaStreamsWith ks drv
    awaitState ks StreamsRunning

    waitForKs ks 5000 $ do
      owned <- ownedPartitions ks
      stby <- standbyTasks ks
      pure (tp `elem` owned && Map.null stby)

    owned <- ownedPartitions ks
    owned `shouldBe` [tp]
    stby <- standbyTasks ks
    Map.null stby `shouldBe` True

    closeKafkaStreams ks
    awaitState ks StreamsClosed


----------------------------------------------------------------------
-- 7. Two-instance integration: partition handoff via rebalance
----------------------------------------------------------------------

{- | Two KafkaStreams instances joined to the same application
(consumer group). They each get a slice of partitions; a
simulated rebalance migrates partitions from one to the
other; we verify that records produced after the migration
land in the new owner's state store, while records produced
before remain queryable on the previous owner during its
KIP-869 standby grace.

This test doesn't talk to a broker; it drives two mock
drivers in parallel. It's the runtime-side equivalent of
bringing up two streams app instances in the same group and
pulling one offline.
-}
two_instances_handoff_partitions_via_rebalance :: Spec
two_instances_handoff_partitions_via_rebalance =
  it "two instances: revoke from A and assign to B; subsequent records land on B" $ do
    -- Same topology + same applicationId on both instances.
    topoA <- buildCountTopo
    topoB <- buildCountTopo
    let cfg =
          defaultStreamsConfig
            { applicationId = "two-inst"
            , bootstrapServers = ["mock:0"]
            , numStreamThreads = 1
            , pollMs = 0
            , taskTimeoutMs = 60_000 -- grace window so A keeps standby
            }
    ksA <- newKafkaStreams cfg topoA
    ksB <- newKafkaStreams cfg topoB
    (drvA, hA) <- newMockDriver
    (drvB, hB) <- newMockDriver

    let tp0 = KC.TopicPartition "in" 0
        tp1 = KC.TopicPartition "in" 1
        tp2 = KC.TopicPartition "in" 2
        tp3 = KC.TopicPartition "in" 3

    -- Phase 1: A starts alone, owns every partition.
    mockDriverInjectRebalance
      hA
      (RebalanceAssigned [tp0, tp1, tp2, tp3])
    startKafkaStreamsWith ksA drvA
    awaitState ksA StreamsRunning

    -- Pump a few records across every partition into A.
    mockDriverInjectPoll
      hA
      [ consumerRecordPart "in" 0 "alice" "1" (Int64ish 0) (Int64ish 100)
      , consumerRecordPart "in" 0 "alice" "1" (Int64ish 1) (Int64ish 101)
      , consumerRecordPart "in" 1 "bob" "1" (Int64ish 2) (Int64ish 102)
      , consumerRecordPart "in" 2 "carol" "1" (Int64ish 3) (Int64ish 103)
      , consumerRecordPart "in" 3 "dave" "1" (Int64ish 4) (Int64ish 104)
      ]

    waitForKs ksA 5000 $ do
      mIQ <- queryKVStore @Text @Int64 ksA (storeName "counts")
      case mIQ of
        Nothing -> pure False
        Just kvs -> do
          a <- kvs.roKvGet "alice"
          b <- kvs.roKvGet "bob"
          c <- kvs.roKvGet "carol"
          d <- kvs.roKvGet "dave"
          pure
            ( a == Just 2
                && b == Just 1
                && c == Just 1
                && d == Just 1
            )

    -- Phase 2: B joins. Rebalance migrates partitions 2 and 3
    -- from A to B. A's revocation enters standby grace because
    -- taskTimeoutMs > 0; A's IQ keeps returning the pre-handoff
    -- counts for those keys for the duration of the grace.
    mockDriverInjectRebalance hA (RebalanceRevoked [tp2, tp3])
    mockDriverInjectRebalance hB (RebalanceAssigned [tp2, tp3])
    startKafkaStreamsWith ksB drvB
    awaitState ksB StreamsRunning

    waitForKs ksA 5000 $ do
      ownedA <- ownedPartitions ksA
      ownedB <- ownedPartitions ksB
      stbyA <- standbyTasks ksA
      pure
        ( length ownedA == 2
            && length ownedB == 2
            && Map.member tp2 stbyA
            && Map.member tp3 stbyA
        )

    -- Pump new records on the migrated partitions into B; B's
    -- IQ store gains the counts, A's stays untouched.
    mockDriverInjectPoll
      hB
      [ consumerRecordPart "in" 2 "carol" "1" (Int64ish 10) (Int64ish 200)
      , consumerRecordPart "in" 2 "carol" "1" (Int64ish 11) (Int64ish 201)
      , consumerRecordPart "in" 3 "dave" "1" (Int64ish 12) (Int64ish 202)
      ]

    waitForKs ksB 5000 $ do
      mIqB <- queryKVStore @Text @Int64 ksB (storeName "counts")
      case mIqB of
        Nothing -> pure False
        Just kvs -> do
          c <- kvs.roKvGet "carol"
          d <- kvs.roKvGet "dave"
          pure (c == Just 2 && d == Just 1)

    -- A is no longer fed records on partitions 2 / 3 so its
    -- counts for carol/dave are still the original pre-handoff
    -- ones — that's the KIP-869 grace-period IQ continuity
    -- guarantee.
    Just iqA <- queryKVStore @Text @Int64 ksA (storeName "counts")
    aliceA <- iqA.roKvGet "alice"
    carolA <- iqA.roKvGet "carol"
    aliceA `shouldBe` Just 2 -- A's own partition, still being fed
    carolA `shouldBe` Just 1 -- standby on A, frozen at handoff
    Just iqB <- queryKVStore @Text @Int64 ksB (storeName "counts")
    carolB <- iqB.roKvGet "carol"
    daveB <- iqB.roKvGet "dave"
    carolB `shouldBe` Just 2 -- new owner, post-handoff records counted
    daveB `shouldBe` Just 1

    closeKafkaStreams ksA
    closeKafkaStreams ksB
    awaitState ksA StreamsClosed
    awaitState ksB StreamsClosed


-- | Stable sort over TopicPartition for the assigned-tps test.
sortBy_ :: [KC.TopicPartition] -> [KC.TopicPartition]
sortBy_ = sortOn (\(KC.TopicPartition t p) -> (t, p))
  where
    sortOn f = sortBy (\a b -> compare (f a) (f b))
    sortBy cmp = foldr insert' []
      where
        insert' x [] = [x]
        insert' x (y : ys)
          | cmp x y == GT = y : insert' x ys
          | otherwise = x : y : ys


----------------------------------------------------------------------
-- Wait helper
----------------------------------------------------------------------

{- | Block on an IO predicate, coordinating with the runtime's
own progress signal ('awaitTicks') so we don't busy-spin or
guess at delays. Strategy: re-check the predicate, and if it
isn't true yet, block via STM until the runtime has run at
least one more event-loop iteration, then re-check. Bounded
by @maxIters@ ticks total so a stuck test fails loudly
instead of hanging.

The @maxIters@ budget here is generous (a few thousand
ticks) because the predicate only re-runs after an actual
engine iteration completed.
-}
waitForKs :: KafkaStreams -> Int -> IO Bool -> IO ()
waitForKs _ 0 _ = error "waitForKs: timed out"
waitForKs ks n act = do
  ok <- act
  if ok
    then pure ()
    else do
      _ <- awaitTicks ks 1
      waitForKs ks (n - 1) act


{- | Legacy yield-based wait kept for tests that don't have a
'KafkaStreams' handle to block on (e.g. the foreign-key DSL
tests). The bound @n@ is interpreted as a soft cap; on a
quiet machine each iteration is essentially free, on a busy
one each yield hands off so the engine thread can run.
-}
waitFor :: Int -> IO Bool -> IO ()
waitFor 0 _ = error "waitFor: timed out"
waitFor n act = do
  ok <- act
  if ok
    then pure ()
    else do
      Control.Concurrent.yield
      waitFor (n - 1) act
