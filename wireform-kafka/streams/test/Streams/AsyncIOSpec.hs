{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for the Riffle async-I\/O operator family
-- ('Kafka.Streams.AsyncIO').
--
-- The suite is layered as:
--
--   * Baseline determinism: the operator behaves the way the
--     synchronous-DSL does for the happy path.
--   * EOS pre-commit drain: the engine drains in-flight work
--     before 'commitDriver' returns.
--   * Chaos: Hedgehog-driven permutations of completion order,
--     random failure injection, randomised buffer / worker
--     sizing, retry / timeout edge cases, concurrent commits,
--     lifecycle (close mid-IO), and stress.
--
-- Coordination is via STM TVars and MVars — never 'threadDelay'
-- — so the tests are deterministic against the bounded worker
-- pool and reproduce identically under @--hedgehog-seed@.
module Streams.AsyncIOSpec (tests) where

import Control.Concurrent (yield)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
  ( TVar
  , atomically
  , isEmptyTMVar
  , modifyTVar'
  , newEmptyTMVarIO
  , newTVarIO
  , putTMVar
  , readTVar
  , readTVarIO
  , retry
  , takeTMVar
  )
import qualified Control.Category as Cat
import qualified Control.Exception as Exception
import Control.Monad (forM_, replicateM)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as List
import qualified Data.Text as T
import Data.Text (Text)
import Data.Void (Void)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import qualified Kafka.Streams.Topology.Free as F

import Kafka.Streams.Imperative
import Kafka.Streams.AsyncIO
  ( AsyncDrainTrigger (..)
  , AsyncFailurePolicy (..)
  , AsyncIOConfig (..)
  , AsyncOutputMode (..)
  , AsyncRetryStrategy (..)
  , asyncMapValues
  , defaultAsyncIOConfig
  )
import qualified Kafka.Streams.Time as Time

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack

unbytes :: BSC.ByteString -> Text
unbytes = T.pack . BSC.unpack

t0 :: Timestamp
t0 = Timestamp 0

----------------------------------------------------------------------
-- Test harness
----------------------------------------------------------------------

-- | Build a topology @in -> asyncMapValues cfg f -> out@ wired with
-- text serdes. Returns the driver plus a @waitForDeposits :: Int -> IO ()@
-- coordination helper. The helper uses an 'aioOnDeposit'-bumped
-- 'TVar' so the test can wait for /durable/ deposits without
-- racing the user IO.
buildAsyncDriver
  :: AsyncIOConfig
  -> (Text -> IO Text)
  -> IO (TopologyTestDriver, Int -> IO ())
buildAsyncDriver cfg0 f = do
  deposits <- newTVarIO (0 :: Int)
  let cfg = cfg0 { aioOnDeposit = atomically (modifyTVar' deposits (+ 1)) }
  b <- newStreamsBuilder
  s <- streamFromTopic b "in" (consumed textSerde textSerde)
  s' <- asyncMapValues cfg f s
  toTopic "out" (produced textSerde textSerde) s'
  topo <- buildTopology b
  driver <- newDriver topo "asyncio-test"
  let waitForDeposits n = atomically $ do
        c <- readTVar deposits
        if c >= n then pure () else retry
  pure (driver, waitForDeposits)

-- | Tight wall-clock advance that exceeds the default punctuator
-- interval. The drain-on-punctuator fires when the engine sees
-- the wall-clock cross the next-fire boundary.
fireDrainPunctuator :: TopologyTestDriver -> IO ()
fireDrainPunctuator d = advanceWallClockTime d 250

----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

tests :: TestTree
tests = testGroup "AsyncIO"
  [ testGroup "Baseline"
      [ ordered_preserves_input_order
      , drain_via_punctuator
      , drop_and_continue_skips_failure
      , log_and_continue_skips_failure
      , fail_task_surfaces_exception
      , unordered_emits_in_completion_order
      , topology_free_async_map_values_smoke
      , fusion_hoists_pure_into_async
      ]
  , testGroup "EOS commit drain"
      [ commit_drains_in_flight_work
      , commit_waits_for_slow_user_io
      , commit_drains_unordered
      ]
  , testGroup "Chaos: permutations"
      [ prop_ordered_under_completion_permutation
      , prop_unordered_is_permutation_of_input
      , prop_per_key_ordering_preserved
      , prop_identical_drivers_produce_identical_output
      ]
  , testGroup "Chaos: failures"
      [ prop_drop_and_continue_keeps_survivors_in_order
      , prop_custom_failure_handler_runs_for_each_failure
      , custom_failure_handler_blocking_blocks_drain
      , custom_failure_handler_throwing_doesnt_lose_other_records
      ]
  , testGroup "Chaos: retry & timeout"
      [ retry_fixed_eventually_succeeds
      , retry_fixed_exhausts_then_applies_policy
      , retry_backoff_recovers
      , timeout_fires_for_blocked_io
      ]
  , testGroup "Chaos: concurrency"
      [ backpressure_no_deadlock_buffer_one
      , backpressure_blocks_pipe_when_buffer_full
      , pipe_then_commit_pipe_then_commit_preserves_all
      , drain_during_active_workload
      , buffer_capacity_one_serialises_records
      ]
  , testGroup "Chaos: lifecycle"
      [ close_after_construction_clean
      , close_with_idle_workers_clean
      , close_with_workers_blocked_mid_io_clean
      , double_commit_after_drain_idempotent
      , observer_hook_failure_surfaces_but_other_records_continue
      ]
  , testGroup "Chaos: stress"
      [ stress_many_records_few_workers_ordered
      , stress_many_workers_few_records
      , stress_mixed_success_and_failure
      ]
  ]

----------------------------------------------------------------------
-- Ordered: 5 records, completes in input order
----------------------------------------------------------------------

ordered_preserves_input_order :: TestTree
ordered_preserves_input_order =
  testCase "ordered output preserves input order across the worker pool" $ do
    let cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 8
          , aioWorkers        = 4
          , aioOutputMode     = OrderedOutput
          }
        f v = pure (T.toUpper v)
    (driver, waitFor) <- buildAsyncDriver cfg f

    let inputs = ["alpha", "bravo", "charlie", "delta", "echo"]
    mapM_ (\v -> pipeInput driver "in"
                  Nothing (bytes v) t0 0) inputs

    waitFor (length inputs)
    fireDrainPunctuator driver

    out <- readOutput driver "out"
    map (unbytes . crValue) out @?= map T.toUpper inputs
    closeDriver driver

----------------------------------------------------------------------
-- Drain via punctuator (no more input arrives)
----------------------------------------------------------------------

drain_via_punctuator :: TestTree
drain_via_punctuator =
  testCase "drain-on-punctuator emits completed work without further input" $ do
    let cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 4
          , aioWorkers        = 2
          , aioOutputMode     = OrderedOutput
          , aioDrainTrigger   = DrainOnEntryAndPunctuator (Time.millis 25)
          }
        f v = pure (T.toUpper v)
    (driver, waitFor) <- buildAsyncDriver cfg f

    -- One input; without the punctuator fire, the single completed
    -- result would stay in the reorder buffer until the next
    -- pipeInput.
    pipeInput driver "in" Nothing (bytes "once") t0 0
    waitFor 1

    fireDrainPunctuator driver

    out <- readOutput driver "out"
    map (unbytes . crValue) out @?= ["ONCE"]
    closeDriver driver

----------------------------------------------------------------------
-- DropAndContinue: failing record is skipped silently
----------------------------------------------------------------------

drop_and_continue_skips_failure :: TestTree
drop_and_continue_skips_failure =
  testCase "DropAndContinue skips the failing slot, keeps the rest" $ do
    let cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 8
          , aioWorkers        = 2
          , aioOutputMode     = OrderedOutput
          , aioOnFailure      = DropAndContinue
          }
        f v
          | v == "boom" = Exception.throwIO (Exception.ErrorCall "synthetic")
          | otherwise   = pure (T.toUpper v)
    (driver, waitFor) <- buildAsyncDriver cfg f

    let inputs = ["a", "boom", "b", "c"]
    mapM_ (\v -> pipeInput driver "in"
                  Nothing (bytes v) t0 0) inputs
    waitFor (length inputs)

    fireDrainPunctuator driver

    out <- readOutput driver "out"
    -- "boom" drops, the surviving three preserve input order.
    map (unbytes . crValue) out @?= ["A", "B", "C"]
    closeDriver driver

----------------------------------------------------------------------
-- LogAndContinue: same as DropAndContinue but logged
----------------------------------------------------------------------

log_and_continue_skips_failure :: TestTree
log_and_continue_skips_failure =
  testCase "LogAndContinue behaves like DropAndContinue downstream" $ do
    let cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 4
          , aioWorkers        = 1
          , aioOutputMode     = OrderedOutput
          , aioOnFailure      = LogAndContinue
          }
        f v
          | v == "x"  = Exception.throwIO (Exception.ErrorCall "synthetic-log")
          | otherwise = pure (T.toUpper v)
    (driver, waitFor) <- buildAsyncDriver cfg f

    mapM_ (\v -> pipeInput driver "in"
                  Nothing (bytes v) t0 0) ["k", "x", "q"]
    waitFor 3
    fireDrainPunctuator driver

    out <- readOutput driver "out"
    map (unbytes . crValue) out @?= ["K", "Q"]
    closeDriver driver

----------------------------------------------------------------------
-- FailTask: exception surfaces on the next drain
----------------------------------------------------------------------

fail_task_surfaces_exception :: TestTree
fail_task_surfaces_exception =
  testCase "FailTask re-throws on the stream thread at the next drain" $ do
    let cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 4
          , aioWorkers        = 1
          , aioOutputMode     = OrderedOutput
          , aioOnFailure      = FailTask
          }
        f v = Exception.throwIO (Exception.ErrorCall ("fail:" <> T.unpack v))
    (driver, waitFor) <- buildAsyncDriver cfg f

    -- First pipeInput enqueues; its drain on entry has nothing to
    -- flush. The worker fails and sets the failure TVar (the
    -- failure path also fires the deposit hook).
    pipeInput driver "in" Nothing (bytes "die") t0 0
    waitFor 1

    -- The next drain (whether from a pipeInput or the punctuator)
    -- re-throws on the stream thread.
    result <- Exception.try (fireDrainPunctuator driver)
                :: IO (Either Exception.SomeException ())
    case result of
      Left _  -> pure ()
      Right _ ->
        assertBool "expected FailTask to re-throw on drain" False

    closeDriver driver

----------------------------------------------------------------------
-- Unordered: results emitted in completion order
----------------------------------------------------------------------

-- | Block each worker on a per-record MVar. The test signals them
-- in reverse to force out-of-order completion, then verifies the
-- output ordering matches the signal order.
unordered_emits_in_completion_order :: TestTree
unordered_emits_in_completion_order =
  testCase "unordered output emits in completion order" $ do
    gateA <- newEmptyMVar
    gateB <- newEmptyMVar
    gateC <- newEmptyMVar

    let pickGate "a" = gateA
        pickGate "b" = gateB
        pickGate "c" = gateC
        pickGate _   = error "unexpected key"

        cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 8
          , aioWorkers        = 3  -- one worker per record
          , aioOutputMode     = UnorderedOutput
          }
        f v = do
          takeMVar (pickGate v)
          pure (T.toUpper v)

    (driver, waitFor) <- buildAsyncDriver cfg f

    mapM_ (\v -> pipeInput driver "in"
                  Nothing (bytes v) t0 0) ["a", "b", "c"]

    -- Release in c, a, b order. The deposit hook observes the
    -- /durable/ enqueue onto the unordered queue, so by the
    -- time waitFor returns we know the deposit landed before
    -- the next gate release.
    putMVar gateC ()
    waitFor 1
    putMVar gateA ()
    waitFor 2
    putMVar gateB ()
    waitFor 3

    fireDrainPunctuator driver

    out <- readOutput driver "out"
    map (unbytes . crValue) out @?= ["C", "A", "B"]
    closeDriver driver

----------------------------------------------------------------------
-- Topology.Free AST: AsyncMapValues Prim compiles + runs
----------------------------------------------------------------------

topology_free_async_map_values_smoke :: TestTree
topology_free_async_map_values_smoke =
  testCase "Topology.Free asyncMapValues compiles and forwards" $ do
    deposits <- newTVarIO (0 :: Int)
    let cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 4
          , aioWorkers        = 2
          , aioOutputMode     = OrderedOutput
          , aioOnDeposit      = atomically (modifyTVar' deposits (+ 1))
          }
        topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "in"
            Cat.>>> F.asyncMapValues cfg pure
            Cat.>>> F.mapValues T.toUpper
            Cat.>>> F.sink "out"

    (_, topo) <- F.compile topology
    driver <- newDriver topo "asyncio-free-smoke"

    mapM_ (\v -> pipeInput driver "in"
                   Nothing (bytes v) t0 0) ["x", "y", "z"]
    atomically $ do
      n <- readTVar deposits
      if n >= 3 then pure () else retry
    advanceWallClockTime driver 250

    out <- readOutput driver "out"
    map (unbytes . crValue) out @?= ["X", "Y", "Z"]
    closeDriver driver

----------------------------------------------------------------------
-- AST fusion: a pure MapValues feeding an AsyncMapValues collapses
-- into a single AsyncMapValues with the composed function.
----------------------------------------------------------------------

fusion_hoists_pure_into_async :: TestTree
fusion_hoists_pure_into_async =
  testCase "optFuseSyncIntoAsync collapses MapValues >>> AsyncMapValues" $ do
    deposits <- newTVarIO (0 :: Int)
    let cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 4
          , aioWorkers        = 2
          , aioOutputMode     = OrderedOutput
          , aioOnDeposit      = atomically (modifyTVar' deposits (+ 1))
          }
        topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "in"
            Cat.>>> F.mapValues T.toUpper          -- pure prefix
            Cat.>>> F.asyncMapValues cfg pure       -- async tail
            Cat.>>> F.sink "out"

    -- With the rule enabled the optimiser should drop one node
    -- (the pure 'MapValues' folds into the async). We assert on
    -- the node-count delta to make the fusion observable
    -- without parsing the AST structure.
    let optimised   = F.optimize topology
        unoptimised = F.optimizeWith F.noOptimization topology
        nFused      = F.countNodes optimised
        nRaw        = F.countNodes unoptimised
    assertBool
      ("expected fused topology to have fewer nodes; raw="
         <> show nRaw <> " fused=" <> show nFused)
      (nFused < nRaw)

    -- And the fused topology still runs to the same output.
    (_, topo) <- F.compile topology
    driver <- newDriver topo "asyncio-fusion"
    mapM_ (\v -> pipeInput driver "in"
                  Nothing (bytes v) t0 0) ["alpha", "bravo"]
    atomically $ do
      n <- readTVar deposits
      if n >= 2 then pure () else retry
    advanceWallClockTime driver 250
    out <- readOutput driver "out"
    map (unbytes . crValue) out @?= ["ALPHA", "BRAVO"]
    closeDriver driver

----------------------------------------------------------------------
-- EOS pre-commit drain: commitDriver drains in-flight work before
-- it returns, without relying on the wall-clock punctuator.
----------------------------------------------------------------------

commit_drains_in_flight_work :: TestTree
commit_drains_in_flight_work =
  testCase "commitDriver drains async work even with no punctuator" $ do
    let cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 4
          , aioWorkers        = 2
          , aioOutputMode     = OrderedOutput
          , aioDrainTrigger   = DrainOnEntry
            -- ^ Critical: no punctuator. The only mechanism that
            -- can drain the reorder buffer is the pre-commit hook
            -- the engine fires from commitEngine.
          }
        f v = pure (T.toUpper v)
    (driver, _waitFor) <- buildAsyncDriver cfg f

    let inputs = ["alpha", "bravo", "charlie"]
    mapM_ (\v -> pipeInput driver "in"
                  Nothing (bytes v) t0 0) inputs

    -- Don't advance wall clock. commitDriver MUST drain on its
    -- own via the engine's drainPreCommit hook.
    commitDriver driver

    out <- readOutput driver "out"
    map (unbytes . crValue) out @?= map T.toUpper inputs
    closeDriver driver

-- | The user IO blocks until the test releases its MVar. Without
-- the EOS drain, commitDriver would return immediately and the
-- post-commit readOutput would miss records that the worker pool
-- hasn't deposited yet. With the drain, commitDriver blocks
-- until every in-flight result is deposited and forwarded.
--
-- Coordination is fully STM: a @started@ counter bumps /before/
-- each worker blocks on its gate, so we can wait until both
-- workers are parked before we schedule the background commit.
-- That removes any race between the test thread and the runtime
-- scheduler.
commit_waits_for_slow_user_io :: TestTree
commit_waits_for_slow_user_io =
  testCase "commitDriver blocks until slow async IO has completed" $ do
    gateA   <- newEmptyMVar
    gateB   <- newEmptyMVar
    started <- newTVarIO (0 :: Int)
    let pickGate "a" = gateA
        pickGate "b" = gateB
        pickGate _   = error "unexpected input"
        cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 4
          , aioWorkers        = 2
          , aioOutputMode     = OrderedOutput
          , aioDrainTrigger   = DrainOnEntry
          }
        f v = do
          atomically (modifyTVar' started (+ 1))
          takeMVar (pickGate v)
          pure (T.toUpper v)
    (driver, _waitFor) <- buildAsyncDriver cfg f

    pipeInput driver "in" Nothing (bytes "a") t0 0
    pipeInput driver "in" Nothing (bytes "b") t0 0

    -- Wait until both workers are parked on their gate. After
    -- this barrier we know: submitted == 2, deposited == 0, two
    -- workers are blocked. The drain MUST park.
    atomically $ do
      s <- readTVar started
      if s == 2 then pure () else retry

    -- Run commitDriver in the background; it must block until
    -- both gates are released.
    commitDone <- newEmptyTMVarIO
    _ <- Async.async $ do
      commitDriver driver
      atomically (putTMVar commitDone ())

    -- Confirm via STM that the commit hasn't completed: with
    -- both workers parked, the EOS drain has to be parked too.
    -- If the drain were skipped, commitDriver would return and
    -- this snapshot would observe a filled TMVar.
    notDoneYet <- atomically (isEmptyTMVar commitDone)
    assertBool
      "expected commitDriver to be blocked on in-flight IO"
      notDoneYet

    -- Releasing the gates lets the workers complete; commit
    -- should now finish and forward both records.
    putMVar gateA ()
    putMVar gateB ()
    atomically (takeTMVar commitDone)

    out <- readOutput driver "out"
    map (unbytes . crValue) out @?= ["A", "B"]
    closeDriver driver

-- | UnorderedOutput: the drain forwards the completed unordered
-- queue regardless of input ordering.
commit_drains_unordered :: TestTree
commit_drains_unordered =
  testCase "commit drains UnorderedOutput too" $ do
    let cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 4
          , aioWorkers        = 2
          , aioOutputMode     = UnorderedOutput
          , aioDrainTrigger   = DrainOnEntry
          }
        f v = pure (T.toUpper v)
    (driver, _waitFor) <- buildAsyncDriver cfg f

    let inputs = ["x", "y", "z"]
    mapM_ (\v -> pipeInput driver "in"
                  Nothing (bytes v) t0 0) inputs

    commitDriver driver

    out <- readOutput driver "out"
    -- Unordered: assert membership, not order. The drain
    -- guarantees all submitted records show up.
    map (unbytes . crValue) out
      `shouldContainAll` ["X", "Y", "Z"]
    closeDriver driver
  where
    shouldContainAll got expected =
      assertBool
        ("expected " <> show expected
           <> " to be a permutation of " <> show got)
        (length got == length expected
         && all (`elem` got) expected)

----------------------------------------------------------------------
-- Chaos suite helpers
----------------------------------------------------------------------

-- | Variant of 'buildAsyncDriver' that returns the deposit
-- 'TVar' so chaos tests can drive coordination directly without
-- the @waitForDeposits@ closure.
--
-- Wraps the caller's 'aioOnDeposit' so test code can install
-- its own hook (e.g. a throwing one) and still observe the
-- deposit count via the returned 'TVar'.
buildAsyncDriverState
  :: AsyncIOConfig
  -> (Text -> IO Text)
  -> IO (TopologyTestDriver, TVar Int)
buildAsyncDriverState cfg0 f = do
  depositsTV <- newTVarIO (0 :: Int)
  let userHook = aioOnDeposit cfg0
      cfg = cfg0
        { aioOnDeposit = do
            atomically (modifyTVar' depositsTV (+ 1))
            userHook
        }
  b <- newStreamsBuilder
  s <- streamFromTopic b "in" (consumed textSerde textSerde)
  s' <- asyncMapValues cfg f s
  toTopic "out" (produced textSerde textSerde) s'
  topo <- buildTopology b
  driver <- newDriver topo "asyncio-chaos"
  pure (driver, depositsTV)

waitDeposits :: TVar Int -> Int -> IO ()
waitDeposits tv target = atomically $ do
  c <- readTVar tv
  if c >= target then pure () else retry

-- | Hedgehog generator for a list of distinct 0-indexed labels
-- in some random permutation order.
genPermutation :: Int -> H.Gen [Int]
genPermutation n = Gen.shuffle [0 .. n - 1]

----------------------------------------------------------------------
-- Chaos / permutations
----------------------------------------------------------------------

-- | For any ordered-output topology, any completion-order
-- permutation of the inputs must still produce the inputs in
-- their original order downstream.
--
-- This is the headline invariant of 'OrderedOutput': the
-- operator's reorder buffer absorbs whatever order the worker
-- pool happens to deposit results in.
prop_ordered_under_completion_permutation :: TestTree
prop_ordered_under_completion_permutation =
  testProperty "ordered output preserves input order under any completion permutation" $
    H.withTests 60 $ H.property $ do
      n        <- H.forAll (Gen.int (Range.linear 1 12))
      workers  <- H.forAll (Gen.int (Range.linear 1 8))
      perm     <- H.forAll (genPermutation n)
      -- Buffer >= n so pipeInput never blocks; this test
      -- exercises the reorder buffer, not the backpressure path
      -- (backpressure has its own tests).
      let cfg = defaultAsyncIOConfig
            { aioBufferCapacity = max 1 n
            , aioWorkers        = workers
            , aioOutputMode     = OrderedOutput
            , aioDrainTrigger   = DrainOnEntry
            }
      out <- H.evalIO $ do
        gates <- replicateM n newEmptyMVar
        let pickGate i = gates !! i
            decodeIx v = read (T.unpack v) :: Int
            f v = do
              takeMVar (pickGate (decodeIx v))
              pure (T.toUpper v)
        (driver, _) <- buildAsyncDriverState cfg f
        let inputs = map (T.pack . show) [0 .. n - 1]
        -- Releaser runs concurrently so even if pipeInput
        -- blocked on the buffer (it won't, given our sizing)
        -- the gates still open.
        releaser <- Async.async $
          forM_ perm $ \i -> putMVar (gates !! i) ()
        mapM_ (\v -> pipeInput driver "in"
                      Nothing (bytes v) t0 0) inputs
        Async.wait releaser
        commitDriver driver
        records <- readOutput driver "out"
        closeDriver driver
        pure records
      let observed = map (unbytes . crValue) out
          expected = map (T.toUpper . T.pack . show) [0 .. n - 1]
      observed H.=== expected

-- | For 'UnorderedOutput', the multiset of outputs must equal
-- the multiset of inputs — regardless of completion order or
-- worker pool size.
prop_unordered_is_permutation_of_input :: TestTree
prop_unordered_is_permutation_of_input =
  testProperty "unordered output is a permutation of the inputs" $
    H.withTests 40 $ H.property $ do
      n       <- H.forAll (Gen.int (Range.linear 1 12))
      workers <- H.forAll (Gen.int (Range.linear 1 8))
      perm    <- H.forAll (genPermutation n)
      let cfg = defaultAsyncIOConfig
            { aioBufferCapacity = max 1 n
            , aioWorkers        = workers
            , aioOutputMode     = UnorderedOutput
            , aioDrainTrigger   = DrainOnEntry
            }
      out <- H.evalIO $ do
        gates <- replicateM n newEmptyMVar
        let f v = do
              takeMVar (gates !! (read (T.unpack v) :: Int))
              pure (T.toUpper v)
        (driver, _) <- buildAsyncDriverState cfg f
        let inputs = map (T.pack . show) [0 .. n - 1]
        releaser <- Async.async $
          forM_ perm $ \i -> putMVar (gates !! i) ()
        mapM_ (\v -> pipeInput driver "in"
                      Nothing (bytes v) t0 0) inputs
        Async.wait releaser
        commitDriver driver
        records <- readOutput driver "out"
        closeDriver driver
        pure records
      let observed = List.sort (map (unbytes . crValue) out)
          expected = List.sort
            (map (T.toUpper . T.pack . show) [0 .. n - 1])
      observed H.=== expected

-- | Per-key ordering is preserved in ordered mode: when the
-- same key appears multiple times, the relative order of those
-- records downstream matches their relative order upstream,
-- regardless of how the worker pool interleaves completions
-- with other keys.
prop_per_key_ordering_preserved :: TestTree
prop_per_key_ordering_preserved =
  testProperty "ordered output preserves per-key order" $
    H.withTests 40 $ H.property $ do
      n       <- H.forAll (Gen.int (Range.linear 2 12))
      workers <- H.forAll (Gen.int (Range.linear 1 6))
      perm    <- H.forAll (genPermutation n)
      keys    <- H.forAll
        (Gen.list (Range.singleton n) (Gen.element ["k1", "k2", "k3"]))
      let cfg = defaultAsyncIOConfig
            { aioBufferCapacity = max 1 n
            , aioWorkers        = workers
            , aioOutputMode     = OrderedOutput
            , aioDrainTrigger   = DrainOnEntry
            }
      let inputs = zip keys (map (T.pack . show) [0 .. n - 1])
      out <- H.evalIO $ do
        gates <- replicateM n newEmptyMVar
        let f v = do
              takeMVar (gates !! (read (T.unpack v) :: Int))
              pure v
        (driver, _) <- buildAsyncDriverState cfg f
        releaser <- Async.async $
          forM_ perm $ \i -> putMVar (gates !! i) ()
        forM_ inputs $ \(k, v) ->
          pipeInput driver "in"
            (Just (bytes k)) (bytes v) t0 0
        Async.wait releaser
        commitDriver driver
        records <- readOutput driver "out"
        closeDriver driver
        pure records
      let observed = map (\r -> ( fmap unbytes (crKey r)
                                , unbytes (crValue r))) out
          expected = map (\(k, v) -> (Just k, v)) inputs
      observed H.=== expected

-- | Two drivers given the same inputs in the same order produce
-- the same ordered outputs — the operator's behaviour is a pure
-- function of the input sequence even though its implementation
-- uses worker threads.
prop_identical_drivers_produce_identical_output :: TestTree
prop_identical_drivers_produce_identical_output =
  testProperty "two identical drivers produce identical outputs" $
    H.withTests 30 $ H.property $ do
      n       <- H.forAll (Gen.int (Range.linear 1 16))
      workers <- H.forAll (Gen.int (Range.linear 1 6))
      let cfg = defaultAsyncIOConfig
            { aioBufferCapacity = max 1 n
            , aioWorkers        = workers
            , aioOutputMode     = OrderedOutput
            , aioDrainTrigger   = DrainOnEntry
            }
          inputs = map (T.pack . show) [0 .. n - 1]
          runOnce = do
            (driver, depositsTV) <- buildAsyncDriverState cfg
              (\v -> pure (T.toUpper v))
            mapM_ (\v -> pipeInput driver "in"
                          Nothing (bytes v) t0 0) inputs
            waitDeposits depositsTV n
            commitDriver driver
            out <- readOutput driver "out"
            closeDriver driver
            pure out
      (a, b) <- H.evalIO $ do
        ra <- runOnce
        rb <- runOnce
        pure (ra, rb)
      map (unbytes . crValue) a H.=== map (unbytes . crValue) b

----------------------------------------------------------------------
-- Chaos / failures
----------------------------------------------------------------------

-- | Inject failures at random indices with 'DropAndContinue':
-- the surviving records appear downstream in their original
-- input order; failed indices contribute nothing.
prop_drop_and_continue_keeps_survivors_in_order :: TestTree
prop_drop_and_continue_keeps_survivors_in_order =
  testProperty "DropAndContinue keeps surviving records in input order" $
    H.withTests 50 $ H.property $ do
      n        <- H.forAll (Gen.int (Range.linear 1 16))
      failIxs  <- H.forAll
        (Gen.list (Range.linear 0 n) (Gen.int (Range.linear 0 (n - 1))))
      workers  <- H.forAll (Gen.int (Range.linear 1 6))
      let failSet = List.nub failIxs
          cfg = defaultAsyncIOConfig
            { aioBufferCapacity = max 1 n
            , aioWorkers        = workers
            , aioOutputMode     = OrderedOutput
            , aioOnFailure      = DropAndContinue
            , aioDrainTrigger   = DrainOnEntry
            }
      out <- H.evalIO $ do
        let f v =
              let i = read (T.unpack v) :: Int
              in if i `elem` failSet
                   then Exception.throwIO
                          (Exception.ErrorCall ("drop:" <> show i))
                   else pure (T.toUpper v)
        (driver, depositsTV) <- buildAsyncDriverState cfg f
        let inputs = map (T.pack . show) [0 .. n - 1]
        mapM_ (\v -> pipeInput driver "in"
                      Nothing (bytes v) t0 0) inputs
        waitDeposits depositsTV n
        commitDriver driver
        records <- readOutput driver "out"
        closeDriver driver
        pure records
      let observed = map (unbytes . crValue) out
          expected =
            [ T.toUpper (T.pack (show i))
            | i <- [0 .. n - 1]
            , not (i `elem` failSet)
            ]
      observed H.=== expected

-- | Every failure routes through the user's 'CustomFailure'
-- handler exactly once, even with several concurrent workers
-- and random failure indices.
prop_custom_failure_handler_runs_for_each_failure :: TestTree
prop_custom_failure_handler_runs_for_each_failure =
  testProperty "CustomFailure handler runs once per failed record" $
    H.withTests 30 $ H.property $ do
      n        <- H.forAll (Gen.int (Range.linear 1 12))
      failIxs  <- H.forAll
        (Gen.list (Range.linear 0 n) (Gen.int (Range.linear 0 (n - 1))))
      workers  <- H.forAll (Gen.int (Range.linear 1 4))
      let failSet = List.nub failIxs
          expected = length failSet
      handled <- H.evalIO (newTVarIO (0 :: Int))
      let cfg = defaultAsyncIOConfig
            { aioBufferCapacity = max 1 n
            , aioWorkers        = workers
            , aioOutputMode     = OrderedOutput
            , aioOnFailure      = CustomFailure $ \_e ->
                atomically (modifyTVar' handled (+ 1))
            , aioDrainTrigger   = DrainOnEntry
            }
      observed <- H.evalIO $ do
        let f v =
              let i = read (T.unpack v) :: Int
              in if i `elem` failSet
                   then Exception.throwIO
                          (Exception.ErrorCall ("custom:" <> show i))
                   else pure v
        (driver, depositsTV) <- buildAsyncDriverState cfg f
        let inputs = map (T.pack . show) [0 .. n - 1]
        mapM_ (\v -> pipeInput driver "in"
                      Nothing (bytes v) t0 0) inputs
        waitDeposits depositsTV n
        commitDriver driver
        c <- readTVarIO handled
        closeDriver driver
        pure c
      observed H.=== expected

-- | A 'CustomFailure' handler that blocks must block the
-- pre-commit drain too — the operator must not declare the
-- record complete until the user's handler has returned.
custom_failure_handler_blocking_blocks_drain :: TestTree
custom_failure_handler_blocking_blocks_drain =
  testCase "CustomFailure that blocks holds the drain back" $ do
    gate     <- newEmptyMVar
    let cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 4
          , aioWorkers        = 1
          , aioOutputMode     = OrderedOutput
          , aioOnFailure      = CustomFailure $ \_e -> takeMVar gate
          , aioDrainTrigger   = DrainOnEntry
          }
        f _v = Exception.throwIO (Exception.ErrorCall "synthetic")
    (driver, _) <- buildAsyncDriverState cfg f
    pipeInput driver "in" Nothing (bytes "die") t0 0

    -- Without releasing the gate, commitDriver must block — the
    -- handler hasn't acknowledged the failure yet.
    commitDone <- newEmptyTMVarIO
    _ <- Async.async $ do
      commitDriver driver
      atomically (putTMVar commitDone ())
    notDoneYet <- atomically (isEmptyTMVar commitDone)
    assertBool
      "expected drain to wait for the CustomFailure handler"
      notDoneYet

    putMVar gate ()
    atomically (takeTMVar commitDone)
    closeDriver driver

-- | A throwing 'CustomFailure' handler is swallowed inside the
-- 'try' in 'applyFailurePolicy', so the worker stays alive and
-- the failing record's slot is a skip — but the OTHER
-- successful records must still be forwarded. We use
-- 'commitDriver' to drain (rather than the punctuator) so the
-- test is independent of wall-clock interactions.
custom_failure_handler_throwing_doesnt_lose_other_records :: TestTree
custom_failure_handler_throwing_doesnt_lose_other_records =
  testCase "throwing CustomFailure doesn't lose neighbouring records" $ do
    let cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 8
          , aioWorkers        = 2
          , aioOutputMode     = OrderedOutput
          , aioOnFailure      = CustomFailure $ \_e ->
              Exception.throwIO (Exception.ErrorCall "handler-bang")
          , aioDrainTrigger   = DrainOnEntry
          }
        f v
          | v == "bang" = Exception.throwIO (Exception.ErrorCall "boom")
          | otherwise   = pure (T.toUpper v)
    (driver, _) <- buildAsyncDriverState cfg f
    mapM_ (\v -> pipeInput driver "in"
                  Nothing (bytes v) t0 0) ["a", "bang", "b"]
    -- commitDriver's EOS drain blocks until every in-flight
    -- request is resolved (success or skip) before forwarding
    -- the reorder buffer downstream. Independent of any
    -- wall-clock trigger.
    commitDriver driver
    out <- readOutput driver "out"
    let observed = map (unbytes . crValue) out
    -- Order-preserving: "A" then "B". The failing "bang" slot
    -- became a skip, so it doesn't show up.
    observed @?= ["A", "B"]
    closeDriver driver

----------------------------------------------------------------------
-- Chaos / retry & timeout
----------------------------------------------------------------------

-- | 'RetryFixed' retries until the IO eventually succeeds.
retry_fixed_eventually_succeeds :: TestTree
retry_fixed_eventually_succeeds =
  testCase "RetryFixed: transient failures recover before retries exhaust" $ do
    attempts <- newTVarIO (0 :: Int)
    let cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 2
          , aioWorkers        = 1
          , aioOutputMode     = OrderedOutput
          , aioRetry          = RetryFixed 3 (Time.millis 1)
          , aioOnFailure      = FailTask
          , aioDrainTrigger   = DrainOnEntry
          }
        f v = do
          a <- atomically $ do
            modifyTVar' attempts (+ 1)
            readTVar attempts
          if a < 3
            then Exception.throwIO (Exception.ErrorCall "transient")
            else pure (T.toUpper v)
    (driver, depositsTV) <- buildAsyncDriverState cfg f
    pipeInput driver "in" Nothing (bytes "ok") t0 0
    waitDeposits depositsTV 1
    commitDriver driver
    out <- readOutput driver "out"
    map (unbytes . crValue) out @?= ["OK"]
    -- The work fired 3 times: 2 failures + 1 success.
    readTVarIO attempts >>= (@?= 3)
    closeDriver driver

-- | 'RetryFixed' eventually exhausts attempts; the configured
-- 'aioOnFailure' policy then decides what to do — here
-- 'DropAndContinue' silently drops.
retry_fixed_exhausts_then_applies_policy :: TestTree
retry_fixed_exhausts_then_applies_policy =
  testCase "RetryFixed: exhaustion routes through the failure policy" $ do
    attempts <- newTVarIO (0 :: Int)
    let cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 2
          , aioWorkers        = 1
          , aioOutputMode     = OrderedOutput
          , aioRetry          = RetryFixed 2 (Time.millis 1)
          , aioOnFailure      = DropAndContinue
          , aioDrainTrigger   = DrainOnEntry
          }
        f _v = do
          atomically (modifyTVar' attempts (+ 1))
          Exception.throwIO (Exception.ErrorCall "permanent")
    (driver, depositsTV) <- buildAsyncDriverState cfg f
    pipeInput driver "in" Nothing (bytes "x") t0 0
    waitDeposits depositsTV 1
    commitDriver driver
    out <- readOutput driver "out"
    map (unbytes . crValue) out @?= []
    -- Initial attempt + 2 retries.
    readTVarIO attempts >>= (@?= 3)
    closeDriver driver

-- | 'RetryBackoff' uses an exponentially-growing delay. Verify
-- the operator wires the constructor through correctly and the
-- IO eventually succeeds.
retry_backoff_recovers :: TestTree
retry_backoff_recovers =
  testCase "RetryBackoff: works with a tiny initial delay" $ do
    attempts <- newTVarIO (0 :: Int)
    let cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 2
          , aioWorkers        = 1
          , aioOutputMode     = OrderedOutput
          , aioRetry          = RetryBackoff 4 (Time.millis 1) 2
          , aioOnFailure      = FailTask
          , aioDrainTrigger   = DrainOnEntry
          }
        f v = do
          a <- atomically $ do
            modifyTVar' attempts (+ 1)
            readTVar attempts
          if a < 3
            then Exception.throwIO (Exception.ErrorCall "transient-bo")
            else pure v
    (driver, depositsTV) <- buildAsyncDriverState cfg f
    pipeInput driver "in" Nothing (bytes "v") t0 0
    waitDeposits depositsTV 1
    commitDriver driver
    out <- readOutput driver "out"
    map (unbytes . crValue) out @?= ["v"]
    closeDriver driver

-- | An IO that blocks past 'aioTimeout' must be treated as a
-- timed-out failure; with 'DropAndContinue' the slot is
-- silently skipped.
timeout_fires_for_blocked_io :: TestTree
timeout_fires_for_blocked_io =
  testCase "aioTimeout converts a blocked IO into a failure" $ do
    gate <- newEmptyMVar
    let cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 2
          , aioWorkers        = 1
          , aioOutputMode     = OrderedOutput
          , aioTimeout        = Time.millis 20
          , aioRetry          = NoRetry
          , aioOnFailure      = DropAndContinue
          , aioDrainTrigger   = DrainOnEntry
          }
        f _v = do
          takeMVar gate  -- never released by the test
          pure "unreached"
    (driver, depositsTV) <- buildAsyncDriverState cfg f
    pipeInput driver "in" Nothing (bytes "x") t0 0
    -- Timeout fires on the worker thread independent of any
    -- test signal; we observe via the deposit counter that the
    -- worker dropped the slot.
    waitDeposits depositsTV 1
    commitDriver driver
    out <- readOutput driver "out"
    map (unbytes . crValue) out @?= []
    -- Release the gate so closeDriver doesn't have to send a
    -- cancellation through the racy timeout / shutdown
    -- interleaving. The runner async inside 'runOnce' has
    -- already been cancelled by the timeout path, so this is a
    -- best-effort cleanup that releases the user's 'MVar'.
    _ <- tryPutMVar gate ()
    closeDriver driver

----------------------------------------------------------------------
-- Chaos / concurrency
----------------------------------------------------------------------

-- | A buffer of 1 and a single worker: records flow through
-- serially with no deadlock. The test pumps many records;
-- pipeInput intermittently blocks on the bounded TBQueue and
-- unblocks when the worker pulls.
backpressure_no_deadlock_buffer_one :: TestTree
backpressure_no_deadlock_buffer_one =
  testCase "buffer=1, workers=1: many records flow without deadlock" $ do
    let cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 1
          , aioWorkers        = 1
          , aioOutputMode     = OrderedOutput
          , aioDrainTrigger   = DrainOnEntry
          }
        f v = pure (T.toUpper v)
    (driver, depositsTV) <- buildAsyncDriverState cfg f
    let n      = 20 :: Int
        inputs = map (T.pack . show) [0 .. n - 1]
    mapM_ (\v -> pipeInput driver "in"
                  Nothing (bytes v) t0 0) inputs
    waitDeposits depositsTV n
    commitDriver driver
    out <- readOutput driver "out"
    map (unbytes . crValue) out @?=
      map (T.toUpper . T.pack . show) [0 .. n - 1]
    closeDriver driver

-- | With buffer=2 and the worker blocked on a gate, the third
-- pipeInput must observably block until a slot frees up.
-- Asserted via STM: we schedule the third 'pipeInput' on a
-- background thread, confirm via an STM snapshot that it
-- hasn't returned, then release the gate.
backpressure_blocks_pipe_when_buffer_full :: TestTree
backpressure_blocks_pipe_when_buffer_full =
  testCase "pipeInput blocks when the bounded buffer is saturated" $ do
    gate <- newEmptyMVar
    let cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 2
          , aioWorkers        = 1
          , aioOutputMode     = OrderedOutput
          , aioDrainTrigger   = DrainOnEntry
          }
        f v = do
          takeMVar gate
          pure (T.toUpper v)
    (driver, _) <- buildAsyncDriverState cfg f

    -- First record: worker picks it up, blocks on gate.
    -- Second + third records: queued in the buffer (capacity
    -- 2). Fourth record: pipeInput MUST block.
    pipeInput driver "in" Nothing (bytes "a") t0 0
    pipeInput driver "in" Nothing (bytes "b") t0 0
    pipeInput driver "in" Nothing (bytes "c") t0 0

    pipeDone <- newEmptyTMVarIO
    _ <- Async.async $ do
      pipeInput driver "in" Nothing (bytes "d") t0 0
      atomically (putTMVar pipeDone ())

    notDoneYet <- atomically (isEmptyTMVar pipeDone)
    assertBool
      "expected pipeInput to block when the buffer is full"
      notDoneYet

    -- Release the gate four times so all four records pass
    -- through; pipeInput "d" should unblock as soon as the
    -- worker picks up one.
    putMVar gate ()
    putMVar gate ()
    putMVar gate ()
    putMVar gate ()
    atomically (takeTMVar pipeDone)

    commitDriver driver
    out <- readOutput driver "out"
    map (unbytes . crValue) out @?= ["A", "B", "C", "D"]
    closeDriver driver

-- | Multiple commit cycles partition records correctly: each
-- 'commitDriver' drains exactly its own in-flight batch.
pipe_then_commit_pipe_then_commit_preserves_all :: TestTree
pipe_then_commit_pipe_then_commit_preserves_all =
  testCase "interleaved pipe/commit cycles preserve every record" $ do
    let cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 4
          , aioWorkers        = 2
          , aioOutputMode     = OrderedOutput
          , aioDrainTrigger   = DrainOnEntry
          }
        f v = pure (T.toUpper v)
    (driver, _) <- buildAsyncDriverState cfg f

    let batches = [["a", "b"], ["c"], ["d", "e", "f"]]
    forM_ batches $ \batch -> do
      mapM_ (\v -> pipeInput driver "in"
                    Nothing (bytes v) t0 0) batch
      commitDriver driver

    out <- readOutput driver "out"
    map (unbytes . crValue) out
      @?= map T.toUpper (concat batches)
    closeDriver driver

-- | Active workload interleaved with periodic drain via
-- punctuator: the operator never loses a record across drain
-- boundaries even when work is continuously flowing.
drain_during_active_workload :: TestTree
drain_during_active_workload =
  testCase "punctuator drains interleave cleanly with active workload" $ do
    let cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 4
          , aioWorkers        = 2
          , aioOutputMode     = OrderedOutput
          , aioDrainTrigger   = DrainOnEntryAndPunctuator (Time.millis 10)
          }
        f v = pure (T.toUpper v)
    (driver, depositsTV) <- buildAsyncDriverState cfg f

    let total = 15 :: Int
    forM_ [0 .. total - 1] $ \i -> do
      pipeInput driver "in"
        Nothing (bytes (T.pack (show i))) t0 0
      -- Fire the punctuator after each record so the drain
      -- interleaves with submission.
      advanceWallClockTime driver 20

    waitDeposits depositsTV total
    commitDriver driver

    out <- readOutput driver "out"
    map (unbytes . crValue) out
      @?= map (T.toUpper . T.pack . show) [0 .. total - 1]
    closeDriver driver

-- | Buffer capacity = 1 with multiple workers: only one record
-- can be in the buffer at a time, so the workers serialise on
-- the buffer's STM read.
buffer_capacity_one_serialises_records :: TestTree
buffer_capacity_one_serialises_records =
  testCase "buffer=1 with workers=4 still flows correctly" $ do
    let cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 1
          , aioWorkers        = 4
          , aioOutputMode     = OrderedOutput
          , aioDrainTrigger   = DrainOnEntry
          }
        f v = pure (T.toUpper v)
    (driver, depositsTV) <- buildAsyncDriverState cfg f

    let n = 10 :: Int
    forM_ [0 .. n - 1] $ \i ->
      pipeInput driver "in"
        Nothing (bytes (T.pack (show i))) t0 0
    waitDeposits depositsTV n
    commitDriver driver

    out <- readOutput driver "out"
    map (unbytes . crValue) out
      @?= map (T.toUpper . T.pack . show) [0 .. n - 1]
    closeDriver driver

----------------------------------------------------------------------
-- Chaos / lifecycle
----------------------------------------------------------------------

-- | A freshly-constructed driver with no pipeInputs at all
-- closes cleanly — workers never had any work, the registry is
-- empty, no hangs.
close_after_construction_clean :: TestTree
close_after_construction_clean =
  testCase "closeDriver on a freshly-built driver returns promptly" $ do
    let cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 4
          , aioWorkers        = 2
          , aioOutputMode     = OrderedOutput
          , aioDrainTrigger   = DrainOnEntry
          }
    (driver, _) <- buildAsyncDriverState cfg pure
    -- The worker pool was spun up by 'procInit'; closing should
    -- shut them down via the shutdown flag without invoking any
    -- failure policy.
    closeDriver driver

-- | After a clean drain, closing is a no-op: the workers are
-- idle, the buffers are empty, no cancellation is needed.
close_with_idle_workers_clean :: TestTree
close_with_idle_workers_clean =
  testCase "closeDriver after a clean drain returns promptly" $ do
    let cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 4
          , aioWorkers        = 2
          , aioOutputMode     = OrderedOutput
          , aioDrainTrigger   = DrainOnEntry
          }
    (driver, depositsTV) <- buildAsyncDriverState cfg
      (\v -> pure (T.toUpper v))
    mapM_ (\v -> pipeInput driver "in"
                  Nothing (bytes v) t0 0) ["x", "y"]
    waitDeposits depositsTV 2
    commitDriver driver
    closeDriver driver

-- | Worker mid-IO when close is called: the user IO is
-- cancelled, the worker exits, close returns promptly. Without
-- the Async.cancel in 'closeProc', this hangs forever.
close_with_workers_blocked_mid_io_clean :: TestTree
close_with_workers_blocked_mid_io_clean =
  testCase "closeDriver with workers blocked on user IO completes" $ do
    gate <- newEmptyMVar
    let cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 4
          , aioWorkers        = 2
          , aioOutputMode     = OrderedOutput
          , aioOnFailure      = DropAndContinue
          , aioDrainTrigger   = DrainOnEntry
          }
        f _v = takeMVar gate >> pure "ok"
    (driver, _) <- buildAsyncDriverState cfg f
    pipeInput driver "in" Nothing (bytes "a") t0 0
    pipeInput driver "in" Nothing (bytes "b") t0 0

    -- close races with the workers being blocked on the gate.
    -- 'closeProc' must send Async.cancel so the workers
    -- observe an AsyncCancelled and exit cleanly.
    closed <- newEmptyTMVarIO
    _ <- Async.async $ do
      closeDriver driver
      atomically (putTMVar closed ())
    -- Wait for close to complete; if the fix is wrong this
    -- test hangs and tasty's per-test timeout (if configured)
    -- catches it. We at least confirm close returns without
    -- the gate ever being released.
    atomically (takeTMVar closed)

-- | Calling commit a second time after a successful first
-- commit on a quiescent operator returns promptly with no work
-- to do.
double_commit_after_drain_idempotent :: TestTree
double_commit_after_drain_idempotent =
  testCase "back-to-back commits on a quiescent operator are no-ops" $ do
    let cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 4
          , aioWorkers        = 2
          , aioOutputMode     = OrderedOutput
          , aioDrainTrigger   = DrainOnEntry
          }
    (driver, depositsTV) <- buildAsyncDriverState cfg
      (\v -> pure (T.toUpper v))
    pipeInput driver "in" Nothing (bytes "p") t0 0
    waitDeposits depositsTV 1
    commitDriver driver
    commitDriver driver   -- second commit: nothing in flight
    out <- readOutput driver "out"
    map (unbytes . crValue) out @?= ["P"]
    closeDriver driver

-- | A throwing 'aioOnDeposit' hook captures the failure in
-- 'apsFailure' but the worker keeps running so other records
-- aren't lost. The failure surfaces on the next drain.
observer_hook_failure_surfaces_but_other_records_continue :: TestTree
observer_hook_failure_surfaces_but_other_records_continue =
  testCase "throwing aioOnDeposit doesn't stop the worker" $ do
    callCount <- newTVarIO (0 :: Int)
    let cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 4
          , aioWorkers        = 1
          , aioOutputMode     = OrderedOutput
          , aioDrainTrigger   = DrainOnEntryAndPunctuator (Time.millis 25)
          , aioOnDeposit      = do
              n <- atomically $ do
                modifyTVar' callCount (+ 1)
                readTVar callCount
              if n == 2
                then Exception.throwIO
                       (Exception.ErrorCall "hook-bang")
                else pure ()
          }
        f v = pure (T.toUpper v)
    (driver, _) <- buildAsyncDriverState cfg f
    -- The deposit hook for the 2nd record throws; the worker
    -- captures the failure into its failure-TVar (slightly /after/
    -- it bumps the deposit counter), and a later drain re-throws
    -- it. With 'DrainOnEntry' that re-throw can land on a
    -- subsequent record's entry (inside 'pipeInput') OR on the
    -- commit drain — and which one wins is a scheduling race. So
    -- we tolerate the failure surfacing at /any/ drain point and,
    -- if it hasn't surfaced during the feed, drain until it does
    -- (bounded; the hook always eventually captures). This keeps
    -- the test deterministic without depending on the exact drain
    -- that re-throws.
    let isHookBang :: Exception.SomeException -> Bool
        isHookBang e = "hook-bang" `T.isInfixOf` T.pack (show e)
        -- Run @io@; swallow the expected hook failure (reporting
        -- 'True') and re-raise anything unexpected.
        tolerate :: IO () -> IO Bool
        tolerate io = do
          r <- Exception.try io :: IO (Either Exception.SomeException ())
          case r of
            Right _                  -> pure False
            Left e | isHookBang e    -> pure True
                   | otherwise       -> Exception.throwIO e
        drainUntilFailure :: Int -> IO Bool
        drainUntilFailure 0 = pure False
        drainUntilFailure k = do
          surfaced <- tolerate (commitDriver driver)
          if surfaced
            then pure True
            else yield >> drainUntilFailure (k - 1)
    fedFailures <- mapM
      (\v -> tolerate (pipeInput driver "in" Nothing (bytes v) t0 0))
      ["x", "y", "z"]
    -- Ensure the failure has surfaced exactly once and the buffers
    -- are flushed.
    surfaced <-
      if or fedFailures
        then tolerate (commitDriver driver) >> pure True
        else drainUntilFailure 10000
    assertBool
      "expected the throwing deposit hook to surface on a drain" surfaced
    -- The worker kept running, so at least the first deposit ("X")
    -- still flows downstream.
    out <- readOutput driver "out"
    let observed = map (unbytes . crValue) out
    assertBool
      ("expected the worker to keep forwarding records; got "
         <> show observed)
      (not (null observed))
    closeDriver driver

----------------------------------------------------------------------
-- Chaos / stress
----------------------------------------------------------------------

-- | Many records, few workers, ordered output: the reorder
-- buffer absorbs the worker race and downstream sees the input
-- order.
stress_many_records_few_workers_ordered :: TestTree
stress_many_records_few_workers_ordered =
  testCase "stress: 200 records / 2 workers / ordered" $ do
    let n = 200 :: Int
        cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 8
          , aioWorkers        = 2
          , aioOutputMode     = OrderedOutput
          , aioDrainTrigger   = DrainOnEntry
          }
        f v = pure (T.toUpper v)
    (driver, depositsTV) <- buildAsyncDriverState cfg f
    forM_ [0 .. n - 1] $ \i ->
      pipeInput driver "in"
        Nothing (bytes (T.pack (show i))) t0 0
    waitDeposits depositsTV n
    commitDriver driver
    out <- readOutput driver "out"
    map (unbytes . crValue) out
      @?= map (T.toUpper . T.pack . show) [0 .. n - 1]
    closeDriver driver

-- | Many workers and few records: the operator doesn't starve
-- — only as many workers as needed pick up work; the rest park
-- on the input queue.
stress_many_workers_few_records :: TestTree
stress_many_workers_few_records =
  testCase "stress: 32 workers / 4 records / no starvation" $ do
    let n   = 4 :: Int
        cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 8
          , aioWorkers        = 32
          , aioOutputMode     = OrderedOutput
          , aioDrainTrigger   = DrainOnEntry
          }
        f v = pure (T.toUpper v)
    (driver, depositsTV) <- buildAsyncDriverState cfg f
    forM_ [0 .. n - 1] $ \i ->
      pipeInput driver "in"
        Nothing (bytes (T.pack (show i))) t0 0
    waitDeposits depositsTV n
    commitDriver driver
    out <- readOutput driver "out"
    map (unbytes . crValue) out
      @?= map (T.toUpper . T.pack . show) [0 .. n - 1]
    closeDriver driver

-- | A stress workload mixing successes and failures: half the
-- records throw, the other half succeed, with random
-- interleaving. The surviving records are forwarded in input
-- order; the operator never loses or duplicates.
stress_mixed_success_and_failure :: TestTree
stress_mixed_success_and_failure =
  testCase "stress: half-failures, ordered survivor preservation" $ do
    let n = 50 :: Int
        cfg = defaultAsyncIOConfig
          { aioBufferCapacity = 16
          , aioWorkers        = 4
          , aioOutputMode     = OrderedOutput
          , aioOnFailure      = DropAndContinue
          , aioDrainTrigger   = DrainOnEntry
          }
        f v = do
          let i = read (T.unpack v) :: Int
          if even i
            then Exception.throwIO (Exception.ErrorCall ("e:" <> show i))
            else pure v
    (driver, depositsTV) <- buildAsyncDriverState cfg f
    forM_ [0 .. n - 1] $ \i ->
      pipeInput driver "in"
        Nothing (bytes (T.pack (show i))) t0 0
    waitDeposits depositsTV n
    commitDriver driver
    out <- readOutput driver "out"
    map (unbytes . crValue) out
      @?= [T.pack (show i) | i <- [0 .. n - 1], odd i]
    closeDriver driver
