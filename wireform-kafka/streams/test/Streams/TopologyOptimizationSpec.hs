{-# LANGUAGE OverloadedStrings #-}

module Streams.TopologyOptimizationSpec (tests) where

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Streams.Processor (Processor (..), processorName)
import Kafka.Streams.Serde (textSerde)
import Kafka.Streams.State.KeyValue.InMemory (inMemoryKeyValueStoreBuilder)
import Kafka.Streams.State.Store (
  LoggingConfig (..),
  StoreBuilderKV (..),
  StoreName,
  storeName,
  withLoggingDisabledKV,
  withSourceTopicChangelogKV,
 )
import Kafka.Streams.Time (recordTimestampExtractor)
import Kafka.Streams.Topology (
  AnyStoreBuilder (..),
  ChangelogPlanProblem (..),
  NodeName (..),
  OptimizationConfig (..),
  Topology,
  TopologyError (..),
  addProcessor,
  addSink,
  addSource,
  addStateStoreKV,
  childrenOf,
  defaultOptimizationConfig,
  effectiveChangelogReuse,
  emptyTopology,
  noOptimisations,
  optimizeTopology,
  sinkParents,
  topoChangelogPlan,
  topoProcessors,
  topoSinks,
  topoStores,
  validateChangelogPlan,
 )
import Kafka.Streams.Topology qualified
import Kafka.Streams.Types (TopicName, topicName)
import Control.Exception (try)
import Kafka.Streams.Errors (TopologyException (..))
import Kafka.Streams.Imperative (
  consumed,
  countStream,
  groupByKey,
  grouped,
  materializedAs,
  newStreamsBuilder,
  buildTopology,
  recordValue,
  selectKey,
  streamFromTopic,
 )
import Kafka.Streams.Grouped (Grouped (..))
import Test.Syd


tests :: Spec
tests =
  describe "Topology optimisation" $
    sequence_
      [ describe "optimizeTopology — REUSE_KTABLE_SOURCE_TOPICS" $
          sequence_
            [ it
                "populates topoChangelogPlan with the source topic"
                reuse_source_ktable_basic
            , it
                "is a no-op when the toggle is disabled"
                reuse_disabled_noop
            , it
                "skips when the source has multiple topics"
                reuse_skip_multi_topic_source
            , it
                "skips when the processor has multiple parents"
                reuse_skip_multi_parent
            , it
                "skips when the store is shared with another processor"
                reuse_skip_shared_store
            , it
                "skips when the store's logging is already disabled"
                reuse_skip_logging_disabled
            , it
                "skips when another sink writes to the source topic"
                reuse_skip_source_also_sink
            , it
                "is idempotent — applying twice == applying once"
                reuse_idempotent
            ]
      , describe "optimizeTopology — dynamic-modification safety (REUSE)" $
          sequence_
            [ it
                "adding a competing sink after optimisation, re-running clears the stale plan entry"
                reuse_dynamic_sink_invalidates
            , it
                "adding a second owner processor after optimisation, re-running clears the entry"
                reuse_dynamic_shared_store_invalidates
            , it
                "preserves user-explicit withSourceTopicChangelogKV across optimiser runs"
                reuse_dynamic_user_explicit_preserved
            , it
                "wipes topoChangelogPlan even when the toggle is disabled"
                reuse_dynamic_wipes_when_disabled
            , it
                "adds a new plan entry when the new graph creates a candidate"
                reuse_dynamic_adds_new_candidate
            ]
      , describe "MERGE_REPARTITION_TOPICS (built into the builder)" $
          sequence_
            [ it "the aggregations of one re-keyed grouping share one repartition topic" merge_builtin_shared
            , it "a repartition name used for two different streams is rejected" merge_builtin_name_clash
            , it "a topic read by two sources is rejected" validate_rejects_topic_read_twice
            ]
      , describe "validateChangelogPlan" $
          sequence_
            [ it
                "accepts an optimised topology"
                validate_accepts_optimised
            , it
                "rejects a stale plan entry after adding a competing sink"
                validate_rejects_stale_after_sink
            , it
                "rejects a stale plan entry after adding a shared owner"
                validate_rejects_stale_after_shared_owner
            , it
                "rejects an inconsistent user-declared loggingSourceTopic"
                validate_rejects_user_inconsistent
            ]
      ]


----------------------------------------------------------------------
-- Test helpers
----------------------------------------------------------------------

mkProc :: IO (Processor () ())
mkProc =
  pure
    Processor
      { procName = processorName "P"
      , procInit = \_ -> pure ()
      , procClose = pure ()
      , procProcess = \_ -> pure ()
      }


{- | A source-table-style topology: one source on @topic@ feeding a
single processor that owns a single in-memory store.
-}
sourceTableTopology
  :: TopicName
  -- ^ source topic
  -> NodeName
  -- ^ source name
  -> NodeName
  -- ^ processor name
  -> StoreName
  -- ^ store name
  -> Topology
sourceTableTopology topic srcNm procNm storeNm =
  addStateStoreKV builder [procNm] $
    addProcessor procNm [srcNm] mkProc $
      addSource srcNm [topic] textSerde textSerde recordTimestampExtractor $
        emptyTopology
  where
    -- Pin the type so the existential 'AnyStoreBuilder' inside
    -- the topology doesn't get a free type variable from the
    -- key/value parameter.
    builder :: StoreBuilderKV () ()
    builder = inMemoryKeyValueStoreBuilder storeNm


{- | The 'LoggingConfig' attached to a KV store, for before/after
assertions on the reuse rewrite.
-}
storeLogging :: StoreName -> Topology -> LoggingConfig
storeLogging sn t =
  case Map.lookup sn (topoStores t) of
    Nothing -> error ("storeLogging: missing store: " <> show sn)
    Just (AsKeyValueBuilder b) -> sbKvLogging b
    Just _ -> error "storeLogging: unexpected store-builder shape"


{- | Add a sink writing to @topic@ with @parent@ as its single
upstream node.
-}
addSinkOnTopic
  :: NodeName -> TopicName -> NodeName -> Topology -> Topology
addSinkOnTopic nm tp parent =
  addSink nm tp textSerde textSerde [parent]


----------------------------------------------------------------------
-- REUSE_KTABLE_SOURCE_TOPICS tests
----------------------------------------------------------------------

reuse_source_ktable_basic :: IO ()
reuse_source_ktable_basic = do
  let topic = topicName "src-topic"
      srcNm = NodeName "KTABLE-SOURCE-1"
      procNm = NodeName "KTABLE-SOURCE-PROCESSOR-2"
      storeNm = storeName "ktable-store"
      t = sourceTableTopology topic srcNm procNm storeNm
  -- Before: no optimisation; the side-table is empty.
  Map.lookup storeNm (topoChangelogPlan t) `shouldBe` Nothing
  effectiveChangelogReuse t storeNm `shouldBe` Nothing
  let !t' = optimizeTopology defaultOptimizationConfig t
  -- After: optimisation populated the side-table.
  Map.lookup storeNm (topoChangelogPlan t') `shouldBe` Just topic
  effectiveChangelogReuse t' storeNm `shouldBe` Just topic
  -- The store builder's LoggingConfig is NOT mutated — user
  -- declarations remain authoritative.
  loggingSourceTopic (storeLogging storeNm t') `shouldBe` Nothing
  loggingEnabled (storeLogging storeNm t') `shouldBe` True


reuse_disabled_noop :: IO ()
reuse_disabled_noop = do
  let topic = topicName "src-topic"
      srcNm = NodeName "S"
      procNm = NodeName "P"
      storeNm = storeName "store"
      t = sourceTableTopology topic srcNm procNm storeNm
      cfg = defaultOptimizationConfig {optReuseSourceKTable = False}
      !t' = optimizeTopology cfg t
  Map.lookup storeNm (topoChangelogPlan t') `shouldBe` Nothing
  effectiveChangelogReuse t' storeNm `shouldBe` Nothing


reuse_skip_multi_topic_source :: IO ()
reuse_skip_multi_topic_source = do
  let srcNm = NodeName "S"
      procNm = NodeName "P"
      storeNm = storeName "store"
      builder :: StoreBuilderKV () ()
      builder = inMemoryKeyValueStoreBuilder storeNm
      t =
        addStateStoreKV builder [procNm]
          $ addProcessor procNm [srcNm] mkProc
          $ addSource
            srcNm
            [topicName "a", topicName "b"]
            textSerde
            textSerde
            recordTimestampExtractor
          $ emptyTopology
      !t' = optimizeTopology defaultOptimizationConfig t
  effectiveChangelogReuse t' storeNm `shouldBe` Nothing


reuse_skip_multi_parent :: IO ()
reuse_skip_multi_parent = do
  let topic = topicName "tp"
      srcA = NodeName "SA"
      srcB = NodeName "SB"
      procNm = NodeName "P"
      storeNm = storeName "store"
      builder :: StoreBuilderKV () ()
      builder = inMemoryKeyValueStoreBuilder storeNm
      t =
        addStateStoreKV builder [procNm]
          $ addProcessor procNm [srcA, srcB] mkProc
          $ addSource
            srcB
            [topicName "other"]
            textSerde
            textSerde
            recordTimestampExtractor
          $ addSource
            srcA
            [topic]
            textSerde
            textSerde
            recordTimestampExtractor
          $ emptyTopology
      !t' = optimizeTopology defaultOptimizationConfig t
  effectiveChangelogReuse t' storeNm `shouldBe` Nothing


reuse_skip_shared_store :: IO ()
reuse_skip_shared_store = do
  let topic = topicName "tp"
      srcNm = NodeName "S"
      proc1 = NodeName "P1"
      proc2 = NodeName "P2"
      storeNm = storeName "store"
      builder :: StoreBuilderKV () ()
      builder = inMemoryKeyValueStoreBuilder storeNm
      t =
        addStateStoreKV builder [proc1, proc2]
          $ addProcessor proc2 [srcNm] mkProc
          $ addProcessor proc1 [srcNm] mkProc
          $ addSource
            srcNm
            [topic]
            textSerde
            textSerde
            recordTimestampExtractor
          $ emptyTopology
      !t' = optimizeTopology defaultOptimizationConfig t
  effectiveChangelogReuse t' storeNm `shouldBe` Nothing


reuse_skip_logging_disabled :: IO ()
reuse_skip_logging_disabled = do
  let topic = topicName "tp"
      srcNm = NodeName "S"
      procNm = NodeName "P"
      storeNm = storeName "store"
      builder :: StoreBuilderKV () ()
      builder = withLoggingDisabledKV (inMemoryKeyValueStoreBuilder storeNm)
      t =
        addStateStoreKV builder [procNm]
          $ addProcessor procNm [srcNm] mkProc
          $ addSource
            srcNm
            [topic]
            textSerde
            textSerde
            recordTimestampExtractor
          $ emptyTopology
      !t' = optimizeTopology defaultOptimizationConfig t
  loggingEnabled (storeLogging storeNm t') `shouldBe` False
  effectiveChangelogReuse t' storeNm `shouldBe` Nothing


reuse_skip_source_also_sink :: IO ()
reuse_skip_source_also_sink = do
  let topic = topicName "shared"
      srcNm = NodeName "S"
      procNm = NodeName "P"
      sinkNm = NodeName "SINK"
      storeNm = storeName "store"
      builder :: StoreBuilderKV () ()
      builder = inMemoryKeyValueStoreBuilder storeNm
      t =
        addSinkOnTopic sinkNm topic procNm
          $ addStateStoreKV builder [procNm]
          $ addProcessor procNm [srcNm] mkProc
          $ addSource
            srcNm
            [topic]
            textSerde
            textSerde
            recordTimestampExtractor
          $ emptyTopology
      !t' = optimizeTopology defaultOptimizationConfig t
  effectiveChangelogReuse t' storeNm `shouldBe` Nothing


reuse_idempotent :: IO ()
reuse_idempotent = do
  let topic = topicName "tp"
      srcNm = NodeName "S"
      procNm = NodeName "P"
      storeNm = storeName "store"
      t = sourceTableTopology topic srcNm procNm storeNm
      !t1 = optimizeTopology defaultOptimizationConfig t
      !t2 = optimizeTopology defaultOptimizationConfig t1
  effectiveChangelogReuse t1 storeNm `shouldBe` Just topic
  effectiveChangelogReuse t2 storeNm `shouldBe` Just topic
  topoChangelogPlan t1 `shouldBe` topoChangelogPlan t2
  Map.size (topoProcessors t1) `shouldBe` Map.size (topoProcessors t2)
  Map.size (topoSinks t1) `shouldBe` Map.size (topoSinks t2)


----------------------------------------------------------------------
-- Dynamic-modification safety (REUSE)
----------------------------------------------------------------------

{- | After 'compile' returns its 'Topo.Topology', callers should
be able to /modify/ the graph (add sinks, processors,
sources, stores) and /re-run/ the optimiser to get a
correct fresh plan. These tests pin that contract.
-}
reuse_dynamic_sink_invalidates :: IO ()
reuse_dynamic_sink_invalidates = do
  let topic = topicName "tp"
      srcNm = NodeName "S"
      procNm = NodeName "P"
      storeNm = storeName "store"
      t = sourceTableTopology topic srcNm procNm storeNm
      -- 1) Optimise: a candidate, plan entry set.
      !t1 = optimizeTopology defaultOptimizationConfig t
  effectiveChangelogReuse t1 storeNm `shouldBe` Just topic
  -- 2) Dynamically add a competing sink that writes to the
  -- same topic. The topic is no longer a clean source-only
  -- changelog target.
  let !t2 = addSinkOnTopic (NodeName "SINK") topic procNm t1
      -- 3) Re-running the optimiser detects the invalidation
      -- and clears the stale plan entry.
      !t3 = optimizeTopology defaultOptimizationConfig t2
  effectiveChangelogReuse t3 storeNm `shouldBe` Nothing


reuse_dynamic_shared_store_invalidates :: IO ()
reuse_dynamic_shared_store_invalidates = do
  let topic = topicName "tp"
      srcNm = NodeName "S"
      procNm = NodeName "P"
      storeNm = storeName "store"
      t = sourceTableTopology topic srcNm procNm storeNm
      !t1 = optimizeTopology defaultOptimizationConfig t
  effectiveChangelogReuse t1 storeNm `shouldBe` Just topic
  -- Add a second processor and connect it to the store.
  let proc2 = NodeName "P2"
      !t2 =
        Kafka.Streams.Topology.connectProcessorAndStateStores
          proc2
          [storeNm]
          $ addProcessor proc2 [srcNm] mkProc t1
      !t3 = optimizeTopology defaultOptimizationConfig t2
  effectiveChangelogReuse t3 storeNm `shouldBe` Nothing


reuse_dynamic_user_explicit_preserved :: IO ()
reuse_dynamic_user_explicit_preserved = do
  let topic = topicName "user-decl"
      srcNm = NodeName "S"
      procNm = NodeName "P"
      storeNm = storeName "user-store"
      builder :: StoreBuilderKV () ()
      builder =
        withSourceTopicChangelogKV
          topic
          (inMemoryKeyValueStoreBuilder storeNm)
      -- User-explicit declaration on the store. The processor /
      -- source pair below DOES match the graph shape — so the
      -- user's intent is consistent with the graph.
      t =
        addStateStoreKV builder [procNm]
          $ addProcessor procNm [srcNm] mkProc
          $ addSource
            srcNm
            [topic]
            textSerde
            textSerde
            recordTimestampExtractor
          $ emptyTopology
  -- 'effectiveChangelogReuse' sees the user declaration even
  -- before optimisation runs.
  effectiveChangelogReuse t storeNm `shouldBe` Just topic
  loggingSourceTopic (storeLogging storeNm t) `shouldBe` Just topic
  -- After optimisation the user declaration is preserved on the
  -- builder; the optimiser does NOT duplicate it into the plan
  -- (the plan filter excludes user-set stores).
  let !t' = optimizeTopology defaultOptimizationConfig t
  loggingSourceTopic (storeLogging storeNm t') `shouldBe` Just topic
  effectiveChangelogReuse t' storeNm `shouldBe` Just topic
  Map.lookup storeNm (topoChangelogPlan t') `shouldBe` Nothing
  -- Re-running optimisation many times leaves the user
  -- declaration alone.
  let !t'' =
        optimizeTopology
          defaultOptimizationConfig
          (optimizeTopology defaultOptimizationConfig t')
  loggingSourceTopic (storeLogging storeNm t'') `shouldBe` Just topic


reuse_dynamic_wipes_when_disabled :: IO ()
reuse_dynamic_wipes_when_disabled = do
  let topic = topicName "tp"
      srcNm = NodeName "S"
      procNm = NodeName "P"
      storeNm = storeName "store"
      t = sourceTableTopology topic srcNm procNm storeNm
      !t1 = optimizeTopology defaultOptimizationConfig t
  effectiveChangelogReuse t1 storeNm `shouldBe` Just topic
  -- User flips the toggle off and re-optimises. The plan is
  -- wiped — no stale entries linger.
  let !t2 = optimizeTopology noOptimisations t1
  Map.size (topoChangelogPlan t2) `shouldBe` 0
  effectiveChangelogReuse t2 storeNm `shouldBe` Nothing


reuse_dynamic_adds_new_candidate :: IO ()
reuse_dynamic_adds_new_candidate = do
  -- Topology starts with one source-table; after compile +
  -- optimise, user adds a second source-table-shaped
  -- processor + store on a separate topic. Re-running the
  -- optimiser captures the new candidate in the plan without
  -- disturbing the original.
  let topic1 = topicName "tp1"
      src1 = NodeName "S1"
      proc1 = NodeName "P1"
      store1 = storeName "s1"
      t = sourceTableTopology topic1 src1 proc1 store1
      !t1 = optimizeTopology defaultOptimizationConfig t
  effectiveChangelogReuse t1 store1 `shouldBe` Just topic1
  let topic2 = topicName "tp2"
      src2 = NodeName "S2"
      proc2 = NodeName "P2"
      store2 = storeName "s2"
      builder2 :: StoreBuilderKV () ()
      builder2 = inMemoryKeyValueStoreBuilder store2
      !t2 =
        addStateStoreKV builder2 [proc2] $
          addProcessor proc2 [src2] mkProc $
            addSource
              src2
              [topic2]
              textSerde
              textSerde
              recordTimestampExtractor
              t1
      !t3 = optimizeTopology defaultOptimizationConfig t2
  effectiveChangelogReuse t3 store1 `shouldBe` Just topic1
  effectiveChangelogReuse t3 store2 `shouldBe` Just topic2
  Map.size (topoChangelogPlan t3) `shouldBe` 2


----------------------------------------------------------------------
-- validateChangelogPlan tests
----------------------------------------------------------------------

validate_accepts_optimised :: IO ()
validate_accepts_optimised = do
  let topic = topicName "tp"
      srcNm = NodeName "S"
      procNm = NodeName "P"
      storeNm = storeName "store"
      t = sourceTableTopology topic srcNm procNm storeNm
      !t' = optimizeTopology defaultOptimizationConfig t
  validateChangelogPlan t' `shouldBe` Right ()


validate_rejects_stale_after_sink :: IO ()
validate_rejects_stale_after_sink = do
  let topic = topicName "tp"
      srcNm = NodeName "S"
      procNm = NodeName "P"
      storeNm = storeName "store"
      !t1 =
        optimizeTopology
          defaultOptimizationConfig
          (sourceTableTopology topic srcNm procNm storeNm)
      -- Add a competing sink without re-optimising. The plan
      -- entry is now stale.
      !t2 = addSinkOnTopic (NodeName "SINK") topic procNm t1
  validateChangelogPlan t2
    `shouldBe` Left (StaleChangelogPlan storeNm topic StaleTopicHasSink)
  -- Re-running the optimiser fixes it.
  let !t3 = optimizeTopology defaultOptimizationConfig t2
  validateChangelogPlan t3 `shouldBe` Right ()


validate_rejects_stale_after_shared_owner :: IO ()
validate_rejects_stale_after_shared_owner = do
  let topic = topicName "tp"
      srcNm = NodeName "S"
      procNm = NodeName "P"
      proc2 = NodeName "P2"
      storeNm = storeName "store"
      !t1 =
        optimizeTopology
          defaultOptimizationConfig
          (sourceTableTopology topic srcNm procNm storeNm)
      !t2 =
        Kafka.Streams.Topology.connectProcessorAndStateStores
          proc2
          [storeNm]
          $ addProcessor proc2 [srcNm] mkProc t1
  validateChangelogPlan t2
    `shouldBe` Left (StaleChangelogPlan storeNm topic StaleMultipleOwners)


validate_rejects_user_inconsistent :: IO ()
validate_rejects_user_inconsistent = do
  -- User declares "reuse topic X as changelog" but X isn't the
  -- store's owner's source. The validator catches it.
  let topic = topicName "wrong-topic"
      srcNm = NodeName "S"
      procNm = NodeName "P"
      storeNm = storeName "store"
      actual = topicName "actual-source"
      builder :: StoreBuilderKV () ()
      builder =
        withSourceTopicChangelogKV
          topic
          (inMemoryKeyValueStoreBuilder storeNm)
      t =
        addStateStoreKV builder [procNm]
          $ addProcessor procNm [srcNm] mkProc
          $ addSource
            srcNm
            [actual]
            textSerde
            textSerde
            recordTimestampExtractor
          $ emptyTopology
  validateChangelogPlan t
    `shouldBe` Left (StaleChangelogPlan storeNm topic (StaleWrongTopic actual))


merge_builtin_shared :: IO ()
merge_builtin_shared = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  byValue <- selectKey recordValue s
  let g = groupByKey (grouped textSerde textSerde) byValue
  _ <- countStream (materializedAs (storeName "a")) g
  _ <- countStream (materializedAs (storeName "b")) g
  -- a stream-stream join of the same re-keyed stream reads it too
  _ <- countStream (materializedAs (storeName "c")) (groupByKey (grouped textSerde textSerde) byValue)
  topo <- buildTopology b
  Map.size (Kafka.Streams.Topology.topoInternalTopics topo) `shouldBe` 1
  either (expectationFailure . show) (const (pure ())) (Kafka.Streams.Topology.validateTopology topo)


merge_builtin_name_clash :: IO ()
merge_builtin_name_clash = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  k1 <- selectKey recordValue s
  k2 <- selectKey (\r -> recordValue r <> "!") s
  let named = (grouped textSerde textSerde) {groupedName = Just "same"}
  _ <- countStream (materializedAs (storeName "a")) (groupByKey named k1)
  r <- try (countStream (materializedAs (storeName "b")) (groupByKey named k2))
  case r of
    Left (TopologyException _) -> pure ()
    Right _ -> expectationFailure "expected a TopologyException"


validate_rejects_topic_read_twice :: IO ()
validate_rejects_topic_read_twice = do
  b <- newStreamsBuilder
  _ <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  _ <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  topo <- buildTopology b
  case Kafka.Streams.Topology.validateTopology topo of
    Left (TopicReadTwice t _) -> t `shouldBe` topicName "in"
    other -> expectationFailure ("expected TopicReadTwice, got " <> either show (const "a valid topology") other)
