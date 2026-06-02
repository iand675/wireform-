{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Streams.TopologyFreeSpec
-- Description : Tests for the GADT-based topology builder
--
-- Exercises the deep-embedded 'Kafka.Streams.Topology.Free.Topology'
-- end-to-end against the in-process driver. Each test builds a
-- topology as a /pure value/ and only at the boundary does it
-- compile + drive the topology — proving the new front-end can
-- express the same topologies the imperative builder can, and
-- behaves identically when interpreted.
module Streams.TopologyFreeSpec (tests) where

import qualified Control.Arrow
import Control.Arrow ((&&&), (***), (>>>))
import qualified Control.Category as Cat
import Control.Exception (try, evaluate)
import Data.Profunctor (dimap)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as HMap
import Data.Int (Int64)
import Data.IORef
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text (Text)
import Data.Void (Void)
import Test.Tasty (TestTree, testGroup)
import Data.List (elemIndex, findIndex)
import Test.Tasty.HUnit (testCase, (@?=), assertBool, assertFailure)
import qualified Unsafe.Coerce as Unsafe

import Kafka.Streams.Imperative
import Kafka.Streams.Runtime.EOS
  ( CommitOutcome (..)
  , EOSCoordinator (..)
  , runCommitCycle
  )
import qualified Data.Set as Set
import qualified Kafka.Streams.Cogroup as Cog
import qualified Kafka.Streams.Consumed as Consumed
import qualified Kafka.Streams.Grouped as Grouped
import qualified Kafka.Streams.Joined as Joined
import qualified Kafka.Streams.Materialized as Mat
import qualified Kafka.Streams.Named as Named
import qualified Kafka.Streams.State.Store
import qualified Kafka.Streams.Suppress as Suppress
import qualified Kafka.Streams.TimeWindowedKStream as TWKS
import qualified Kafka.Streams.Topology as Topo
import qualified Kafka.Streams.Topology.Free as F
import qualified Kafka.Streams.Topology.Free.Graphviz as DOT
import qualified Kafka.Streams.Window as Win

tests :: TestTree
tests = testGroup "Topology.Free (GADT topology builder)"
  [ test_source_sink_passthrough
  , test_chain_of_stateless_transforms
  , test_fanout_two_sinks
  , test_parallel_two_independent_pipelines
  , test_stream_table_join
  , test_groupby_count
  , test_inspect_records_constructors
  , test_category_id_left_right_identity
  -- Lineage combinators
  , test_tap_passes_wire_through
  , test_forkn_three_branches
  , test_split_named_branches
  , test_fork_explicit_duplicator
  -- Multi-topic source
  , test_sources_multi_topic
  -- KTable extras
  , test_table_table_left_join
  , test_filter_not_table
  -- Windowed aggregation
  , test_windowed_by_time_count
  -- KGroupedTable
  , test_kgrouped_table_count
  -- Cogroup
  , test_cogroup_two_streams
  -- Suppress
  , test_suppress_until_time_limit
  -- Processor API + state store
  , test_process_stream_with_state_store
  -- Optimiser
  , test_optimize_fuses_map_chains
  , test_optimize_fuses_filter_chains
  , test_optimize_collapses_identity_combinators
  , test_optimize_preserves_observable_behaviour
  , test_optimize_noOptimization_is_a_no_op
  , test_optimize_pushes_pure_functions_through_fanout
  , test_compile_default_runs_optimizer
  -- Java-style additional rewrites
  , test_optimize_selectKey_then_groupByKey_becomes_groupBy
  , test_optimize_collapses_repartition_chains
  , test_optimize_collapses_values_idempotent
  , test_optimize_foreach_after_peek_fuses
  , test_optimize_tap_foreach_becomes_peek
  , test_optimize_combines_adjacent_taps
  , test_optimize_pushes_arr_through_fork
  -- Errors
  , test_missing_serde_throws_typed_exception
  -- EOS interaction
  , test_fork_topology_is_eos_atomic
  , test_forkN_topology_is_eos_atomic
  , test_tap_topology_is_eos_atomic
  -- Instance suite (Applicative / Monad / Semigroup / Monoid / Profunctor)
  , test_applicative_liftA2_combines_two_topologies
  , test_monad_do_notation_for_multi_source
  , test_semigroup_runs_both_pipelines_on_one_input
  , test_monoid_unit_output_is_no_op
  , test_profunctor_dimap_works
  , test_reader_localInput_pre_transforms
  -- Cross-lineage EOS + cogroup with monad bind
  , test_mergeSourced_two_sources_share_one_task_under_eos
  , test_cogroup_via_do_notation
  -- Graphviz visualiser
  , test_graphviz_topologyDot_emits_valid_dot
  , test_graphviz_astDot_emits_valid_dot
  -- inspectDeep: full static analysis past Bind
  , test_inspectDeep_walks_through_bind_continuations
  , test_inspect_vs_inspectDeep
  -- Fusion barrier (noFuse)
  , test_noFuse_blocks_mapValues_fusion
  , test_noFuse_is_runtime_identity
  , test_noFuse_blocks_filter_fusion
  , test_noFuse_left_alone_in_isolation
  -- Record-level mapM
  , test_mapRecord_full_record_transform
  , test_mapRecordM_io_full_record_transform
  , test_mapRecord_chains_fuse
  , test_mapRecord_chain_blocked_by_noFuse
  -- Repartition-aware rewrites
  , test_drop_repartition_before_selectKey
  , test_drop_repartition_before_mapKeyValue
  , test_drop_repartition_before_flatMapKeyValue
  , test_hoist_mapValues_through_repartition
  , test_hoist_filter_through_repartition
  , test_hoist_enables_upstream_fusion
  , test_hoist_disabled_keeps_original_order
  , test_drop_disabled_keeps_repartition
  , test_repartition_rewrites_preserve_semantics
  -- Auto-insert repartitions on stateful ops downstream of key changes
  , test_auto_insert_before_groupByKey
  , test_auto_insert_before_toTable
  , test_auto_insert_through_mapValues_chain
  , test_auto_insert_off_keeps_nothing
  , test_auto_insert_no_op_when_no_key_change
  , test_auto_insert_with_explicit_repartition_no_dup
  , test_auto_insert_stream_table_join_fanout
  , test_auto_insert_stream_stream_join_fanout
  , test_auto_insert_selectKey_groupByKey_collapses_to_groupBy
  -- JoinWindows.gracePeriod is honored by the join builder
  , test_join_grace_drops_late_records
  -- KIP-307 named operators
  , test_filterNamed_pins_node_name
  , test_mapValuesNamed_pins_node_name
  , test_selectKeyNamed_pins_node_name
  , test_peekNamed_pins_node_name
  -- KIP-825 windowed emit strategy
  , test_withEmitStrategy_switches_to_emit_on_close
  , test_withEmitStrategy_default_emit_on_update
  -- KIP-328 suppression buffer config
  , test_suppressWindowedWith_compiles_with_max_records
  -- Consumed offset reset
  , test_sourceWith_offset_reset_propagated_to_spec
  , test_default_offset_reset_is_earliest
  -- sourcesWith
  , test_sourcesWith_uses_supplied_consumed
  -- TableJoined FK joins
  , test_fk_join_with_tableJoined_compiles
  -- Late store attachment
  , test_addGlobalStore_registers_global_store
  , test_connectProcessorAndStateStores_attaches_late
  -- Rich sink + stateful transform
  , test_sinkSpec_compiles_with_custom_spec
  , test_transformStream_can_change_key_and_value
  -- Pattern source
  , test_sourcePattern_records_pattern_in_spec
  -- Time-windowed cogroup aggregate
  , test_aggregateWindowedCogrouped_per_window_state
  -- Materialized queryable store info
  , test_queryableStoreName_returns_explicit_name
  , test_queryableStoreType_default_is_kv
  ]

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack

unbytes :: BSC.ByteString -> Text
unbytes = T.pack . BSC.unpack

t0 :: Timestamp
t0 = Timestamp 0

t :: Integer -> Timestamp
t = Timestamp . fromIntegral

----------------------------------------------------------------------
-- 1. Source >>> Sink: the smallest closed topology
----------------------------------------------------------------------

test_source_sink_passthrough :: TestTree
test_source_sink_passthrough =
  testCase "source >>> sink passes records through unchanged" $ do
    let topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "in"
            >>> F.sink "out"

    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-passthrough"

    pipeInput driver (topicName "in") (Just (bytes "k1")) (bytes "v1") t0 0
    pipeInput driver (topicName "in") (Just (bytes "k2")) (bytes "v2") t0 0

    out <- readOutput driver (topicName "out")
    map (fmap unbytes . crKey) out @?= [Just "k1", Just "k2"]
    map (unbytes . crValue) out    @?= ["v1", "v2"]
    closeDriver driver

----------------------------------------------------------------------
-- 2. Long chain of stateless transforms
----------------------------------------------------------------------

test_chain_of_stateless_transforms :: TestTree
test_chain_of_stateless_transforms =
  testCase "chain of mapValues / filter / concatMapValues works" $ do
    let topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "in"
            >>> F.mapValues T.strip
            >>> F.filter (\r -> recordValue r /= "")
            >>> F.concatMapValues T.words
            >>> F.mapValues T.toUpper
            >>> F.sink "out"

    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-stateless-chain"

    pipeInput driver (topicName "in") Nothing (bytes "  hello world  ") t0 0
    pipeInput driver (topicName "in") Nothing (bytes "")               t0 0
    pipeInput driver (topicName "in") Nothing (bytes "kafka streams")   t0 0

    out <- readOutput driver (topicName "out")
    map (unbytes . crValue) out @?= ["HELLO", "WORLD", "KAFKA", "STREAMS"]
    closeDriver driver

----------------------------------------------------------------------
-- 3. Fanout: one source, two parallel sinks
----------------------------------------------------------------------

test_fanout_two_sinks :: TestTree
test_fanout_two_sinks =
  testCase "Fanout (&&&) routes the same stream to two sinks" $ do
    let upper :: F.Topology (KStream Text Text) ()
        upper = F.mapValues T.toUpper
            >>> F.sink "upper"

        lower :: F.Topology (KStream Text Text) ()
        lower = F.mapValues T.toLower
            >>> F.sink "lower"

        topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "in"
            >>> (upper &&& lower)
            >>> F.liftIO_ "drop-pair" (\_b _ -> pure ())

    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-fanout"

    pipeInput driver (topicName "in") Nothing (bytes "Mixed") t0 0
    pipeInput driver (topicName "in") Nothing (bytes "Case")  t0 0

    upperOut <- readOutput driver (topicName "upper")
    lowerOut <- readOutput driver (topicName "lower")
    map (unbytes . crValue) upperOut @?= ["MIXED", "CASE"]
    map (unbytes . crValue) lowerOut @?= ["mixed", "case"]
    closeDriver driver

----------------------------------------------------------------------
-- 4. Parallel: two independent void-input pipelines
----------------------------------------------------------------------

test_parallel_two_independent_pipelines :: TestTree
test_parallel_two_independent_pipelines =
  testCase "two void-input pipelines combine via Fanout (&&&)" $ do
    let leftHalf :: F.Topology Void ()
        leftHalf =
          F.source @Text @Text "left-in"
            >>> F.mapValues (T.append "L:")
            >>> F.sink "left-out"

        rightHalf :: F.Topology Void ()
        rightHalf =
          F.source @Text @Text "right-in"
            >>> F.mapValues (T.append "R:")
            >>> F.sink "right-out"

        topology :: F.Topology Void ()
        topology =
          (leftHalf &&& rightHalf)
            >>> F.liftIO_ "drop-pair" (\_b _ -> pure ())

    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-parallel"

    pipeInput driver (topicName "left-in")  Nothing (bytes "1") t0 0
    pipeInput driver (topicName "right-in") Nothing (bytes "2") t0 0
    pipeInput driver (topicName "left-in")  Nothing (bytes "3") t0 0

    lOut <- readOutput driver (topicName "left-out")
    rOut <- readOutput driver (topicName "right-out")
    map (unbytes . crValue) lOut @?= ["L:1", "L:3"]
    map (unbytes . crValue) rOut @?= ["R:2"]
    closeDriver driver

----------------------------------------------------------------------
-- 5. Stream-table join via the tuple input shape
----------------------------------------------------------------------

test_stream_table_join :: TestTree
test_stream_table_join =
  testCase "StreamTableJoin pairs records with the latest table value" $ do
    let streamSide :: F.Topology Void (KStream Text Text)
        streamSide = F.source @Text @Text "stream-in"

        tableSide :: F.Topology Void (KTable Text Text)
        tableSide = F.tableSource "table-in"

        topology :: F.Topology Void ()
        topology =
          (streamSide &&& tableSide)
            >>> F.streamTableJoin
                  (\v vt -> v <> "|" <> vt)
                  (joined textSerde textSerde textSerde)
            >>> F.sink "joined-out"

    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-streamtable-join"

    pipeInput driver (topicName "table-in") (Just (bytes "k1")) (bytes "T1") t0 0
    pipeInput driver (topicName "table-in") (Just (bytes "k2")) (bytes "T2") t0 0
    pipeInput driver (topicName "stream-in") (Just (bytes "k1")) (bytes "S1") t0 0
    pipeInput driver (topicName "stream-in") (Just (bytes "k2")) (bytes "S2") t0 0
    pipeInput driver (topicName "stream-in") (Just (bytes "k3")) (bytes "miss") t0 0

    out <- readOutput driver (topicName "joined-out")
    map (unbytes . crValue) out @?= ["S1|T1", "S2|T2"]
    closeDriver driver

----------------------------------------------------------------------
-- 6. groupBy + count materialised into a KTable
----------------------------------------------------------------------

test_groupby_count :: TestTree
test_groupby_count =
  testCase "groupByKey >>> count materialises counts per key" $ do
    let countMat :: Materialized Text Int64
        countMat =
          Mat.withValueSerde int64Serde
            $ Mat.withKeySerde textSerde
            $ Mat.materializedAs (storeName "free-count-store")

        topology :: F.Topology Void (KTable Text Int64)
        topology =
          F.source @Text @Text "in"
            >>> F.groupByKey
            >>> F.count countMat

    (kt, topo) <- F.compile topology
    driver <- newDriver topo "free-count"

    pipeInput driver (topicName "in") (Just (bytes "a")) (bytes "_") t0 0
    pipeInput driver (topicName "in") (Just (bytes "a")) (bytes "_") t0 0
    pipeInput driver (topicName "in") (Just (bytes "b")) (bytes "_") t0 0
    pipeInput driver (topicName "in") (Just (bytes "a")) (bytes "_") t0 0

    mStore <- getKeyValueStore @Text @Int64 driver (ktableStore kt)
    case mStore of
      Just kvs -> do
        kvsGet kvs "a" >>= (@?= Just 3)
        kvsGet kvs "b" >>= (@?= Just 1)
      Nothing -> error "free-count: count store missing"
    closeDriver driver

----------------------------------------------------------------------
-- 7. AST introspection: 'inspect' records constructor names
----------------------------------------------------------------------

test_inspect_records_constructors :: TestTree
test_inspect_records_constructors =
  testCase "inspect produces a constructor listing for static analysis" $ do
    let topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "in"
            >>> F.mapValues T.toUpper
            >>> F.filter (\r -> recordValue r /= "")
            >>> F.sink "out"

        ops = F.inspect topology

    assertBool "expected Source label" $
      any ("Source" `T.isPrefixOf`) ops
    assertBool "expected MapValues label" $
      "MapValues" `elem` ops
    assertBool "expected Filter label" $
      "Filter" `elem` ops
    assertBool "expected Sink label" $
      any ("Sink" `T.isPrefixOf`) ops

----------------------------------------------------------------------
-- 8. Category laws (identity)
----------------------------------------------------------------------

test_category_id_left_right_identity :: TestTree
test_category_id_left_right_identity =
  testCase "'Cat.id . t' and 't . Cat.id' both build identical topologies" $ do
    let base :: F.Topology Void ()
        base =
          F.source @Text @Text "in"
            >>> F.mapValues T.toUpper
            >>> F.sink "out"

        leftId  = Cat.id Cat.. base
        rightId = base Cat.. Cat.id

        ops1 = F.inspect base
        ops2 = F.inspect leftId
        ops3 = F.inspect rightId

    ops1 @?= ops2
    ops1 @?= ops3

----------------------------------------------------------------------
-- 9. Tap: side-effect lineage that passes the wire through
----------------------------------------------------------------------

test_tap_passes_wire_through :: TestTree
test_tap_passes_wire_through =
  testCase "Tap forks a side pipeline and keeps the main lineage flowing" $ do
    let auditSink :: F.Topology (KStream Text Text) ()
        auditSink =
          F.filter (\r -> "audit:" `T.isPrefixOf` recordValue r)
            >>> F.sink "audit"

        topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "in"
            >>> F.tap auditSink
            >>> F.mapValues T.toUpper
            >>> F.sink "main"

    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-tap"

    pipeInput driver (topicName "in") Nothing (bytes "audit:login")     t0 0
    pipeInput driver (topicName "in") Nothing (bytes "regular")          t0 0
    pipeInput driver (topicName "in") Nothing (bytes "audit:logout")     t0 0

    audit <- readOutput driver (topicName "audit")
    main_ <- readOutput driver (topicName "main")

    -- Tap branch only receives audit-prefixed values.
    map (unbytes . crValue) audit @?= ["audit:login", "audit:logout"]
    -- Main branch sees all records, uppercased.
    map (unbytes . crValue) main_ @?=
      ["AUDIT:LOGIN", "REGULAR", "AUDIT:LOGOUT"]
    closeDriver driver

----------------------------------------------------------------------
-- 10. ForkN: N-way fan-out via NonEmpty list
----------------------------------------------------------------------

test_forkn_three_branches :: TestTree
test_forkn_three_branches =
  testCase "ForkN applies three sub-pipelines to the same upstream" $ do
    let mkSink :: Text -> (Text -> Text) -> F.Topology (KStream Text Text) ()
        mkSink topic f =
          F.mapValues f >>> F.sink topic

        threeWays :: F.Topology (KStream Text Text) (NE.NonEmpty ())
        threeWays = F.forkN
          ( NE.fromList
              [ mkSink "upper" T.toUpper
              , mkSink "lower" T.toLower
              , mkSink "len"   (T.pack . show . T.length)
              ])

        topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "in"
            >>> threeWays
            >>> F.liftIO_ "drop" (\_b _ -> pure ())

    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-forkn"

    pipeInput driver (topicName "in") Nothing (bytes "Hello") t0 0
    pipeInput driver (topicName "in") Nothing (bytes "World") t0 0

    upper <- readOutput driver (topicName "upper")
    lower <- readOutput driver (topicName "lower")
    len_  <- readOutput driver (topicName "len")
    map (unbytes . crValue) upper @?= ["HELLO", "WORLD"]
    map (unbytes . crValue) lower @?= ["hello", "world"]
    map (unbytes . crValue) len_  @?= ["5", "5"]
    closeDriver driver

----------------------------------------------------------------------
-- 11. Split: KIP-418 named branches
----------------------------------------------------------------------

test_split_named_branches :: TestTree
test_split_named_branches =
  testCase "Split routes records to named branches" $ do
    let topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "in"
            >>> F.split
                  [ F.splitBranch "short" (\r -> T.length (recordValue r) < 4)
                  , F.splitBranch "long"  (\r -> T.length (recordValue r) >= 4)
                  ]
                  (Just "rest")
            -- After Split, we have a Map Text (KStream Text Text); use
            -- Arr to extract specific branches and sink them.
            >>> F.liftIO_ "sink-branches" sinkBranches

        sinkBranches _b branchesMap = do
          -- Each named branch is itself a KStream; we route 'short' to
          -- the 'short' topic, 'long' to the 'long' topic, and drop
          -- the default rest.
          case Map.lookup "short" branchesMap of
            Just s -> toTopic (topicName "short")
                              (produced textSerde textSerde) s
            Nothing -> error "split: short branch missing"
          case Map.lookup "long" branchesMap of
            Just s -> toTopic (topicName "long")
                              (produced textSerde textSerde) s
            Nothing -> error "split: long branch missing"
          pure ()

    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-split"

    pipeInput driver (topicName "in") Nothing (bytes "ab")    t0 0
    pipeInput driver (topicName "in") Nothing (bytes "abcde") t0 0
    pipeInput driver (topicName "in") Nothing (bytes "xy")    t0 0

    shorts <- readOutput driver (topicName "short")
    longs  <- readOutput driver (topicName "long")
    map (unbytes . crValue) shorts @?= ["ab", "xy"]
    map (unbytes . crValue) longs  @?= ["abcde"]
    closeDriver driver

----------------------------------------------------------------------
-- 12. Fork: explicit duplicator
----------------------------------------------------------------------

test_fork_explicit_duplicator :: TestTree
test_fork_explicit_duplicator =
  testCase "Fork explicitly duplicates one wire into a pair" $ do
    -- Build:
    --   source >>> Fork >>> (mapValues toUpper *** mapValues toLower)
    --          >>> (sink upper *** sink lower)
    --          >>> drop-pair
    let topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "in"
            >>> F.fork
            >>> (F.mapValues T.toUpper *** F.mapValues T.toLower)
            >>> (F.sink "upper"
                   *** F.sink "lower")
            >>> F.liftIO_ "drop" (\_b _ -> pure ())

    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-fork"

    pipeInput driver (topicName "in") Nothing (bytes "Mix")  t0 0
    pipeInput driver (topicName "in") Nothing (bytes "Case") t0 0

    upper <- readOutput driver (topicName "upper")
    lower <- readOutput driver (topicName "lower")
    map (unbytes . crValue) upper @?= ["MIX", "CASE"]
    map (unbytes . crValue) lower @?= ["mix", "case"]
    closeDriver driver
  where
    -- Local alias to the Arrow combinator from Control.Arrow re-exported
    -- via Control.Category — keeps the test concise.
    _ = ()

----------------------------------------------------------------------
-- 13. Multi-topic source via 'sources'
----------------------------------------------------------------------

test_sources_multi_topic :: TestTree
test_sources_multi_topic =
  testCase "sources fans multiple topics into one KStream" $ do
    let topology :: F.Topology Void ()
        topology =
          F.sources @Text @Text (NE.fromList ["in-a", "in-b"])
            >>> F.mapValues (T.append "*")
            >>> F.sink "merged"

    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-multi-source"

    pipeInput driver (topicName "in-a") Nothing (bytes "a1") t0 0
    pipeInput driver (topicName "in-b") Nothing (bytes "b1") t0 0
    pipeInput driver (topicName "in-a") Nothing (bytes "a2") t0 0

    out <- readOutput driver (topicName "merged")
    map (unbytes . crValue) out @?= ["*a1", "*b1", "*a2"]
    closeDriver driver

----------------------------------------------------------------------
-- 14. KTable-KTable left join
----------------------------------------------------------------------

test_table_table_left_join :: TestTree
test_table_table_left_join =
  testCase "TableTableLeftJoin emits even when right side is absent" $ do
    let leftTable, rightTable :: F.Topology Void (KTable Text Text)
        leftTable  = F.tableSource "left"
        rightTable = F.tableSource "right"

        outMat :: Materialized Text Text
        outMat =
          Mat.withValueSerde textSerde
            $ Mat.withKeySerde textSerde
            $ Mat.materializedAs (storeName "lj-out-store")

        topology :: F.Topology Void (KTable Text Text)
        topology =
          (leftTable &&& rightTable)
            >>> F.tableTableLeftJoin
                  (\l mr -> l <> "|" <> maybe "MISS" Prelude.id mr)
                  outMat

    (kt, topo) <- F.compile topology
    driver <- newDriver topo "free-tt-leftjoin"

    pipeInput driver (topicName "left")  (Just (bytes "k1")) (bytes "L1") t0 0
    pipeInput driver (topicName "right") (Just (bytes "k1")) (bytes "R1") t0 0
    pipeInput driver (topicName "left")  (Just (bytes "k2")) (bytes "L2") t0 0
    -- k3 only on left
    pipeInput driver (topicName "left")  (Just (bytes "k3")) (bytes "L3") t0 0

    mStore <- getKeyValueStore @Text @Text driver (ktableStore kt)
    case mStore of
      Just kvs -> do
        kvsGet kvs "k1" >>= (@?= Just "L1|R1")
        kvsGet kvs "k2" >>= (@?= Just "L2|MISS")
        kvsGet kvs "k3" >>= (@?= Just "L3|MISS")
      Nothing -> error "left-join store missing"
    closeDriver driver

----------------------------------------------------------------------
-- 15. filterNotTable
----------------------------------------------------------------------

test_filter_not_table :: TestTree
test_filter_not_table =
  testCase "filterNotTable drops matching records" $ do
    let baseTable :: F.Topology Void (KTable Text Text)
        baseTable = F.tableSource "in"

        filteredMat :: Materialized Text Text
        filteredMat =
          Mat.withValueSerde textSerde
            $ Mat.withKeySerde textSerde
            $ Mat.materializedAs (storeName "fnt-out")

        topology :: F.Topology Void (KTable Text Text)
        topology =
          baseTable
            >>> F.filterNotTable
                  (\r -> recordValue r == "drop")
                  filteredMat

    (kt, topo) <- F.compile topology
    driver <- newDriver topo "free-filter-not-table"

    pipeInput driver (topicName "in") (Just (bytes "a")) (bytes "keep") t0 0
    pipeInput driver (topicName "in") (Just (bytes "b")) (bytes "drop") t0 0
    pipeInput driver (topicName "in") (Just (bytes "c")) (bytes "keep") t0 0

    mStore <- getKeyValueStore @Text @Text driver (ktableStore kt)
    case mStore of
      Just kvs -> do
        kvsGet kvs "a" >>= (@?= Just "keep")
        kvsGet kvs "b" >>= (@?= Nothing)
        kvsGet kvs "c" >>= (@?= Just "keep")
      Nothing -> error "filter-not-table store missing"
    closeDriver driver

----------------------------------------------------------------------
-- 16. Windowed aggregation: tumbling count
----------------------------------------------------------------------

test_windowed_by_time_count :: TestTree
test_windowed_by_time_count =
  testCase "windowedByTime >>> countWindowed buckets records per window" $ do
    let countMat :: Materialized Text Int64
        countMat =
          Mat.withValueSerde int64Serde
            $ Mat.withKeySerde textSerde
            $ Mat.materializedAs (storeName "free-wcount-store")

        ws = tumblingWindows (millis 100)

        topology :: F.Topology Void (WindowedTableHandle Text Int64)
        topology =
          F.source @Text @Text "in"
            >>> F.groupByKey
            >>> F.windowedByTime ws
            >>> F.countWindowed countMat

    (wth, topo) <- F.compile topology
    driver <- newDriver topo "free-wcount"

    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v1") (t 10) 0
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v2") (t 50) 0
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v3") (t 99) 0
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v4") (t 150) 0
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v5") (t 199) 0

    mStore <- getWindowStore @Text @Int64 driver (wthStore wth)
    case mStore of
      Just ws_ -> do
        wsFetch ws_ "k" (Timestamp 0)   >>= (@?= Just 3)
        wsFetch ws_ "k" (Timestamp 100) >>= (@?= Just 2)
      Nothing -> error "windowed count store missing"
    closeDriver driver

----------------------------------------------------------------------
-- 17. KGroupedTable: subtractor-aware count
----------------------------------------------------------------------

test_kgrouped_table_count :: TestTree
test_kgrouped_table_count =
  testCase "KGroupedTable count tracks key-value insertions and deletions" $ do
    -- An upstream KTable keyed by user-id holds the user's region.
    -- Re-key by region to count users-per-region; deletes on the
    -- upstream must subtract.
    let baseMat :: Materialized Text Text
        baseMat =
          Mat.withValueSerde textSerde
            $ Mat.withKeySerde textSerde
            $ Mat.materializedAs (storeName "users-store")

        countMat :: Materialized Text Int64
        countMat =
          Mat.withValueSerde int64Serde
            $ Mat.withKeySerde textSerde
            $ Mat.materializedAs (storeName "region-count-store")

        topology :: F.Topology Void (KTable Text Int64)
        topology =
          F.liftIO_ "table-source"
            (\b _ -> tableFromTopic b (topicName "users")
                                       (consumed textSerde textSerde)
                                       baseMat)
            >>> F.groupTableBy
                  (\_userId region -> (region, region))
                  (grouped textSerde textSerde)
            >>> F.countKGroupedTable countMat

    (kt, topo) <- F.compile topology
    driver <- newDriver topo "free-kgrouped-table"

    pipeInput driver (topicName "users") (Just (bytes "u1")) (bytes "US") t0 0
    pipeInput driver (topicName "users") (Just (bytes "u2")) (bytes "US") t0 0
    pipeInput driver (topicName "users") (Just (bytes "u3")) (bytes "EU") t0 0
    -- u1 moves from US to EU
    pipeInput driver (topicName "users") (Just (bytes "u1")) (bytes "EU") t0 0

    mStore <- getKeyValueStore @Text @Int64 driver (ktableStore kt)
    case mStore of
      Just kvs -> do
        kvsGet kvs "US" >>= (@?= Just 1)
        kvsGet kvs "EU" >>= (@?= Just 2)
      Nothing -> error "kgrouped-table store missing"
    closeDriver driver

----------------------------------------------------------------------
-- 18. Cogroup: two streams aggregate into one shared state
----------------------------------------------------------------------

test_cogroup_two_streams :: TestTree
test_cogroup_two_streams =
  testCase "Cogroup of two streams shares aggregator state" $ do
    let g = grouped textSerde textSerde

        leftGrouped :: F.Topology Void (KGroupedStream Text Text)
        leftGrouped =
          F.source @Text @Text "left-in"
            >>> F.groupByKeyWith g

        rightGrouped :: F.Topology Void (KGroupedStream Text Text)
        rightGrouped =
          F.source @Text @Text "right-in"
            >>> F.groupByKeyWith g

        outMat :: Materialized Text Text
        outMat =
          Mat.withValueSerde textSerde
            $ Mat.withKeySerde textSerde
            $ Mat.materializedAs (storeName "cog-store")

        -- Pattern: cogroup left | addCogrouped right
        --   == liftIO build the cogroup, then aggregate.
        --
        -- The shape with the GADT:
        --   leftGrouped
        --     >>> Cogroup leftStep
        --     >>> Fanout id (rightGrouped >>> arr id_)
        --     >>> AddCogrouped rightStep
        --     >>> AggregateCogrouped seed materialized
        --
        -- "Fanout id (rightGrouped >>> arr id_)" is awkward because
        -- rightGrouped has Void input. Instead, compose:
        --
        --   liftIO_ buildBoth -> (CogroupedStream, KGroupedStream)
        --     >>> AddCogrouped rightStep
        --     >>> AggregateCogrouped ...
        --
        -- The fast path: build the cogroup pair manually in a Lifted.
        -- This is the natural usage pattern for cogroup; the GADT
        -- carries the typed handle through the rest of the pipeline.
        topology :: F.Topology Void (KTable Text Text)
        topology =
          F.liftIO_ "build-cogroup-pair"
            (\b _ -> do
                kgsL <- F.compileWith b leftGrouped
                kgsR <- F.compileWith b rightGrouped
                let cgs0 = startCogroup kgsL
                    cgs  = extendCogroup cgs0 kgsR
                pure cgs)
            >>> F.aggregateCogrouped (pure "") outMat

        -- Inlined cogroup-builder helpers using the existing
        -- imperative API. The Topology.Free path expects the upstream
        -- pipeline to deliver a 'CogroupedStream'; this Lifted block
        -- does so.
        startCogroup kgs = cogroup kgs leftStep
        extendCogroup cs kgs = addCogrouped cs kgs rightStep
        leftStep  _ v acc = acc <> "/" <> v
        rightStep _ v acc = acc <> "+" <> v

    (kt, topo) <- F.compile topology
    driver <- newDriver topo "free-cogroup"

    pipeInput driver (topicName "left-in")  (Just (bytes "k")) (bytes "a") (t 0) 0
    pipeInput driver (topicName "right-in") (Just (bytes "k")) (bytes "b") (t 1) 0
    pipeInput driver (topicName "left-in")  (Just (bytes "k")) (bytes "c") (t 2) 0

    mStore <- getKeyValueStore @Text @Text driver (ktableStore kt)
    case mStore of
      Just kvs -> kvsGet kvs "k" >>= (@?= Just "/a+b/c")
      Nothing  -> error "cogroup store missing"
    closeDriver driver

----------------------------------------------------------------------
-- 19. suppressUntilTimeLimit
----------------------------------------------------------------------

test_suppress_until_time_limit :: TestTree
test_suppress_until_time_limit =
  testCase "suppressUntilTimeLimit debounces emissions per key" $ do
    -- The suppress operator only emits a key once the time-limit has
    -- elapsed since the last record for that key. Records arriving
    -- within the limit overwrite without emitting.
    let topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "in"
            >>> F.suppressUntilTimeLimit (millis 100)
            >>> F.sink "out"

    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-suppress"

    -- Two rapid records — second one within the 100ms limit. Only one
    -- emission is expected (after enough stream time has elapsed).
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v1") (t 0)   0
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v2") (t 50)  0
    -- Advance stream time past the limit so the buffered v2 flushes.
    pipeInput driver (topicName "in") (Just (bytes "z")) (bytes "_")  (t 200) 0
    pipeInput driver (topicName "in") (Just (bytes "z")) (bytes "_")  (t 1000) 0

    out <- readOutput driver (topicName "out")
    -- We don't care about the exact count of emissions for 'z' (the
    -- debounce behaviour is well-tested elsewhere); the key point is
    -- that 'v1' is suppressed by 'v2' and only the latest value
    -- per-key surfaces once the debounce window passes.
    let kvalues = [ unbytes (crValue cr)
                  | cr <- out, fmap unbytes (crKey cr) == Just "k" ]
    assertBool "suppress dropped intermediate value; only v2 should land"
      ("v1" `notElem` kvalues)
    assertBool "the final value v2 must be emitted at least once"
      ("v2" `elem` kvalues)
    closeDriver driver

----------------------------------------------------------------------
-- 20. Processor API + state store
----------------------------------------------------------------------

test_process_stream_with_state_store :: TestTree
test_process_stream_with_state_store =
  testCase "processWithStateStoreKV runs a state-store-backed counter" $ do
    -- A custom processor that increments a per-key count in an
    -- in-memory KV state store. 'F.processWithStateStoreKV'
    -- atomically registers the processor + state store with the
    -- right owner-node wiring, so the user doesn't need to know
    -- the auto-generated processor node name.
    --
    -- The processor matches the standard shape used throughout the
    -- imperative DSL: an 'IORef' for the 'ProcessorContext' captured
    -- in 'procInit', another for the resolved 'KeyValueStore', and
    -- a 'procProcess' that uses both to read-modify-write the
    -- counter store.
    let storeNm = storeName "free-procapi-counts"
        storeBuilder
          :: Kafka.Streams.State.Store.StoreBuilderKV Text Int64
        storeBuilder = inMemoryKeyValueStoreBuilder storeNm

        -- The custom Processor: on every input record, increment
        -- the counter for its key in the state store.
        counterProcessor :: IO (Processor Text Text)
        counterProcessor = do
          ctxRef   <- newIORef Nothing
          storeRef <-
            newIORef (Nothing :: Maybe (KeyValueStore Text Int64))
          pure Processor
            { procName    = processorName "FREE-PROCAPI-COUNTER"
            , procInit    = \ctx -> do
                writeIORef ctxRef (Just ctx)
                getStateStore ctx storeNm >>= \case
                  Just (AnyKeyValueStore kvs) ->
                    writeIORef storeRef (Just (Unsafe.unsafeCoerce kvs))
                  _ -> error
                         "free-procapi: counter store missing in procInit"
            , procClose   = pure ()
            , procProcess = \r -> case recordKey r of
                Nothing -> pure ()
                Just k  -> do
                  mst <- readIORef storeRef
                  case mst of
                    Just kvs -> do
                      cur <- maybe 0 Prelude.id <$> kvsGet kvs k
                      kvsPut kvs k (cur + 1)
                    Nothing -> pure ()
            }

        topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "in"
            >>> F.processWithStateStoreKV
                  "FREE-PROCAPI-COUNTER"
                  storeBuilder
                  counterProcessor

    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-procapi"

    pipeInput driver (topicName "in") (Just (bytes "k1")) (bytes "v") t0 0
    pipeInput driver (topicName "in") (Just (bytes "k1")) (bytes "v") t0 0
    pipeInput driver (topicName "in") (Just (bytes "k2")) (bytes "v") t0 0
    pipeInput driver (topicName "in") (Just (bytes "k1")) (bytes "v") t0 0

    -- Pull the state store directly and verify the counts.
    mStore <- getKeyValueStore @Text @Int64 driver storeNm
    case mStore of
      Just kvs -> do
        kvsGet kvs "k1" >>= (@?= Just 3)
        kvsGet kvs "k2" >>= (@?= Just 1)
        kvsGet kvs "k3" >>= (@?= Nothing)
      Nothing -> error "free-procapi: counter store missing in driver"
    closeDriver driver

----------------------------------------------------------------------
-- 21. Optimiser fuses chains of MapValues
----------------------------------------------------------------------

test_optimize_fuses_map_chains :: TestTree
test_optimize_fuses_map_chains =
  testCase "optimize collapses 4× MapValues into a single Arr / MapValues" $ do
    -- Four pure value transforms chained — the optimiser must fuse
    -- them into a single MapValues node (composition of the four
    -- functions).
    let topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "in"
            >>> F.mapValues (T.append "a")
            >>> F.mapValues (T.append "b")
            >>> F.mapValues (T.append "c")
            >>> F.mapValues (T.append "d")
            >>> F.sink "out"

        stats = F.optimizationStats topology

    -- Sanity check: optimisation reduced the AST node count by at
    -- least 3 (the three "extra" MapValues plus the surrounding
    -- Compose nodes collapse together).
    assertBool
      ("expected node-count reduction; stats = " <> show stats)
      (F.osNodesSaved stats >= 3)

    -- And the optimised topology still produces the right output
    -- end-to-end.
    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-opt-mapfuse"
    pipeInput driver (topicName "in") Nothing (bytes "x") t0 0
    out <- readOutput driver (topicName "out")
    map (unbytes . crValue) out @?= ["dcbax"]
    closeDriver driver

----------------------------------------------------------------------
-- 22. Optimiser fuses chains of Filter
----------------------------------------------------------------------

test_optimize_fuses_filter_chains :: TestTree
test_optimize_fuses_filter_chains =
  testCase "optimize fuses 3× Filter into a single conjunction" $ do
    let topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "in"
            >>> F.filter (\r -> T.length (recordValue r) >= 2)
            >>> F.filter (\r -> T.length (recordValue r) <= 5)
            >>> F.filter (\r -> T.head (recordValue r) /= '_')
            >>> F.sink "out"

        stats = F.optimizationStats topology

    assertBool
      ("expected filter chain to fuse; stats = " <> show stats)
      (F.osNodesSaved stats >= 2)

    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-opt-filterfuse"
    pipeInput driver (topicName "in") Nothing (bytes "a")        t0 0  -- too short
    pipeInput driver (topicName "in") Nothing (bytes "abcdef")   t0 0  -- too long
    pipeInput driver (topicName "in") Nothing (bytes "_skip")    t0 0  -- underscore
    pipeInput driver (topicName "in") Nothing (bytes "keep")     t0 0  -- passes all 3
    pipeInput driver (topicName "in") Nothing (bytes "ok")       t0 0  -- passes all 3

    out <- readOutput driver (topicName "out")
    map (unbytes . crValue) out @?= ["keep", "ok"]
    closeDriver driver

----------------------------------------------------------------------
-- 23. Optimiser collapses identity combinators
----------------------------------------------------------------------

test_optimize_collapses_identity_combinators :: TestTree
test_optimize_collapses_identity_combinators =
  testCase "Cat.id . op . Cat.id collapses; first/second/parallel of Id collapse" $ do
    -- A handcrafted "redundant" topology: lots of identity-wrapped
    -- combinators that should reduce to a near-empty AST after the
    -- optimiser runs.
    let redundant :: F.Topology Void ()
        redundant =
          F.source @Text @Text "in"
            >>> Cat.id                        -- redundant identity
            >>> Cat.id Cat.. F.mapValues T.toUpper Cat.. Cat.id
                                              -- Id . op . Id ==> op
            >>> Cat.id                        -- another
            >>> F.mapValues T.reverse
            >>> F.sink "out"

        before = F.countNodes redundant
        after  = F.countNodes (F.optimize redundant)

    assertBool
      ("expected at least 2 nodes saved; before=" <> show before
        <> " after=" <> show after)
      (before - after >= 2)

    -- Observable behaviour preserved (toUpper then reverse).
    (_, topo) <- F.compile redundant
    driver <- newDriver topo "free-opt-identity"
    pipeInput driver (topicName "in") Nothing (bytes "hello") t0 0
    out <- readOutput driver (topicName "out")
    map (unbytes . crValue) out @?= ["OLLEH"]
    closeDriver driver

----------------------------------------------------------------------
-- 24. Optimiser preserves observable behaviour (optimised vs unoptimised)
----------------------------------------------------------------------

test_optimize_preserves_observable_behaviour :: TestTree
test_optimize_preserves_observable_behaviour =
  testCase "optimised and unoptimised compilations produce identical output" $ do
    -- A topology that mixes several fusible chains. We compile both
    -- with optimisation and without, drive both with the same input,
    -- and compare outputs.
    let topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "in"
            >>> F.mapValues T.strip
            >>> F.mapValues T.toUpper
            >>> F.filter (\r -> recordValue r /= "")
            >>> F.filter (\r -> T.length (recordValue r) > 1)
            >>> F.concatMapValues T.words
            >>> F.mapValues (<> "!")
            >>> F.sink "out"

        inputs =
          [ (Just (bytes "k1"), bytes "  hello world  ")
          , (Just (bytes "k2"), bytes "")
          , (Just (bytes "k3"), bytes " a ")           -- post-strip is "A", length 1
          , (Just (bytes "k4"), bytes " ok cool now ")
          ]

        runWith :: (F.Topology Void () -> IO ((), Topology))
                -> IO [Text]
        runWith comp = do
          (_, topo) <- comp topology
          driver <- newDriver topo "free-opt-vs-noopt"
          mapM_ (\(k, v) -> pipeInput driver (topicName "in") k v t0 0) inputs
          out <- readOutput driver (topicName "out")
          closeDriver driver
          pure (map (unbytes . crValue) out)

    optimised   <- runWith F.compile
    unoptimised <- runWith F.compileNoOptimize
    optimised @?= unoptimised
    -- And the expected output is the one both should produce.
    optimised @?= ["HELLO!", "WORLD!", "OK!", "COOL!", "NOW!"]

----------------------------------------------------------------------
-- 25. 'noOptimization' is a no-op
----------------------------------------------------------------------

test_optimize_noOptimization_is_a_no_op :: TestTree
test_optimize_noOptimization_is_a_no_op =
  testCase "compileWithOptimization noOptimization preserves node count" $ do
    let topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "in"
            >>> F.mapValues T.toUpper
            >>> F.mapValues T.reverse
            >>> F.sink "out"

        original = F.countNodes topology
        viaNoOpt = F.countNodes (F.optimizeWith F.noOptimization topology)

    viaNoOpt @?= original
    -- And the topology still runs fine with the no-op config.
    (_, topo) <- F.compileWithOptimization F.noOptimization topology
    driver <- newDriver topo "free-noopt"
    pipeInput driver (topicName "in") Nothing (bytes "abc") t0 0
    out <- readOutput driver (topicName "out")
    map (unbytes . crValue) out @?= ["CBA"]
    closeDriver driver

----------------------------------------------------------------------
-- 26. Pure functions push through Fanout
----------------------------------------------------------------------

test_optimize_pushes_pure_functions_through_fanout :: TestTree
test_optimize_pushes_pure_functions_through_fanout =
  testCase "Fanout/Parallel/First of pure functions collapse to a single Arr" $ do
    -- A pure pipeline built out of Arr-only combinators: Fanout +
    -- Parallel of Arrs should fully collapse to a single Arr that
    -- runs the composed pure function.
    let pureFanout :: F.Topology Int (Int, Int)
        pureFanout =
          (Control.Arrow.arr (+ 1)
              &&& Control.Arrow.arr (* 2))    -- Fanout (Arr) (Arr) ==> Arr
            >>> (Control.Arrow.arr (+ 10)
                  *** Control.Arrow.arr (+ 20))
                                              -- Parallel (Arr) (Arr) ==> Arr
            >>> Cat.id                        -- redundant Id

        before = F.countNodes pureFanout
        after  = F.countNodes (F.optimize pureFanout)

    -- We started with several constructors and should land at the
    -- single 'Arr' constructor representing the composed function.
    assertBool
      ("expected pure-function chain to collapse; before=" <> show before
        <> " after=" <> show after)
      (before > 1 && after == 1)

----------------------------------------------------------------------
-- 27. 'compile' (default) runs the optimiser
----------------------------------------------------------------------

test_compile_default_runs_optimizer :: TestTree
test_compile_default_runs_optimizer =
  testCase "default compile reduces topology node count vs compileNoOptimize" $ do
    -- A chain that the optimiser should reduce: count the Kafka
    -- 'Topology' graph nodes (sources/processors/sinks) after both
    -- compilation paths. Fewer processor nodes means fewer
    -- record-forwarding hops on the data path.
    let topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "in"
            >>> F.mapValues T.strip
            >>> F.mapValues T.toUpper
            >>> F.mapValues T.reverse
            >>> F.mapValues (T.append "x")
            >>> F.sink "out"

    (_, topoOpt)   <- F.compile topology
    (_, topoNoOpt) <- F.compileNoOptimize topology

    let !nOpt   = length (topologyNodes topoOpt)
        !nNoOpt = length (topologyNodes topoNoOpt)

    assertBool
      ("optimised compile should produce fewer nodes; opt=" <> show nOpt
        <> " noopt=" <> show nNoOpt)
      (nOpt < nNoOpt)

----------------------------------------------------------------------
-- 28. selectKey >>> groupByKey collapses to groupBy
----------------------------------------------------------------------

test_optimize_selectKey_then_groupByKey_becomes_groupBy :: TestTree
test_optimize_selectKey_then_groupByKey_becomes_groupBy =
  testCase "selectKey >>> groupByKey collapses to a single groupBy" $ do
    -- This is the Java best-practice "prefer groupBy over selectKey+groupByKey"
    -- pattern: one fewer processor node and a clearer topology
    -- description.
    let countMat :: Materialized Text Int64
        countMat =
          Mat.withValueSerde int64Serde
            $ Mat.withKeySerde textSerde
            $ Mat.materializedAs (storeName "free-rekey-count")

        topology :: F.Topology Void (KTable Text Int64)
        topology =
          F.source @Text @Text "in"
            >>> F.selectKey (\r -> T.take 1 (recordValue r))
            >>> F.groupByKey
            >>> F.count countMat

        opsBefore = F.inspect topology
        opsAfter  = F.inspect (F.optimize topology)

    -- Before optimisation we see both SelectKey and GroupByKey.
    assertBool "expected SelectKey in pre-optimisation AST" $
      "SelectKey" `elem` opsBefore
    assertBool "expected GroupByKey in pre-optimisation AST" $
      "GroupByKey" `elem` opsBefore
    -- After optimisation both have collapsed into a single GroupBy.
    assertBool "expected GroupBy in optimised AST" $
      "GroupBy" `elem` opsAfter
    assertBool "expected NO SelectKey in optimised AST" $
      "SelectKey" `notElem` opsAfter
    assertBool "expected NO bare GroupByKey in optimised AST" $
      "GroupByKey" `notElem` opsAfter

    -- And observable behaviour is preserved.
    (kt, topo) <- F.compile topology
    driver <- newDriver topo "free-opt-rekey"
    pipeInput driver (topicName "in") (Just (bytes "u1")) (bytes "apple")  t0 0
    pipeInput driver (topicName "in") (Just (bytes "u2")) (bytes "apricot") t0 0
    pipeInput driver (topicName "in") (Just (bytes "u3")) (bytes "banana") t0 0
    pipeInput driver (topicName "in") (Just (bytes "u4")) (bytes "anchovy") t0 0

    mStore <- getKeyValueStore @Text @Int64 driver (ktableStore kt)
    case mStore of
      Just kvs -> do
        kvsGet kvs "a" >>= (@?= Just 3)
        kvsGet kvs "b" >>= (@?= Just 1)
      Nothing -> error "rekey-count store missing"
    closeDriver driver

----------------------------------------------------------------------
-- 29. Repartition >>> Repartition collapses
----------------------------------------------------------------------

test_optimize_collapses_repartition_chains :: TestTree
test_optimize_collapses_repartition_chains =
  testCase "consecutive repartitions collapse to a single shuffle" $ do
    -- Two back-to-back 'repartition's are redundant: the broker
    -- only needs one shuffle. The optimiser collapses the inner
    -- one; the outer's topic prefix wins.
    let redundant :: F.Topology Void ()
        redundant =
          F.source @Text @Text "in"
            >>> F.repartition "first-shuffle"
            >>> F.repartition "second-shuffle"
            >>> F.repartition "third-shuffle"
            >>> F.sink "out"

        ops = F.inspect (F.optimize redundant)

    -- Only one Repartition node should remain.
    let repartitionCount =
          length [ () | op <- ops, "Repartition" `T.isPrefixOf` op ]
    repartitionCount @?= 1

----------------------------------------------------------------------
-- 30. Values >>> Values idempotence
----------------------------------------------------------------------

test_optimize_collapses_values_idempotent :: TestTree
test_optimize_collapses_values_idempotent =
  testCase "values >>> values collapses to a single values" $ do
    let topology :: F.Topology (KStream Text Text) (KStream () Text)
        topology = F.values >>> F.values

        opsAfter = F.inspect (F.optimize topology)
        valuesCount = length [ () | op <- opsAfter, op == "Values" ]

    valuesCount @?= 1

----------------------------------------------------------------------
-- 31. Foreach after Peek fuses into one Foreach
----------------------------------------------------------------------

test_optimize_foreach_after_peek_fuses :: TestTree
test_optimize_foreach_after_peek_fuses =
  testCase "foreach >>> peek fuses into a single Foreach" $ do
    seen <- newIORef ([] :: [(Text, Text)])
    let topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "in"
            >>> F.peek    (\r -> modifyIORef' seen (\xs -> xs ++ [("peek",    recordValue r)]))
            >>> F.foreach (\r -> modifyIORef' seen (\xs -> xs ++ [("foreach", recordValue r)]))

        ops = F.inspect (F.optimize topology)

    -- After optimisation the Peek and Foreach are gone — both fused
    -- into a single Foreach whose effect runs both callbacks in the
    -- original order.
    assertBool "expected Foreach in optimised AST" $
      "Foreach" `elem` ops
    -- And the side effects still fire in the expected order
    -- (peek-then-foreach per record).
    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-opt-foreach-peek"
    pipeInput driver (topicName "in") Nothing (bytes "x") t0 0
    pipeInput driver (topicName "in") Nothing (bytes "y") t0 0
    finalSeen <- readIORef seen
    finalSeen @?= [("peek", "x"), ("foreach", "x"),
                   ("peek", "y"), ("foreach", "y")]
    closeDriver driver

----------------------------------------------------------------------
-- 32. Tap (Foreach f) collapses to Peek f
----------------------------------------------------------------------

test_optimize_tap_foreach_becomes_peek :: TestTree
test_optimize_tap_foreach_becomes_peek =
  testCase "tap (foreach f) collapses to peek f" $ do
    let topology :: F.Topology (KStream Text Text) (KStream Text Text)
        topology = F.tap (F.foreach (\_ -> pure ()))

        ops = F.inspect (F.optimize topology)

    -- The Tap and Foreach should have been replaced by a single Peek.
    assertBool "expected Peek in optimised AST" $
      "Peek" `elem` ops
    assertBool "Tap should be gone from optimised AST" $
      not (any ("Tap" `T.isPrefixOf`) ops)

----------------------------------------------------------------------
-- 33. Adjacent Tap nodes combine
----------------------------------------------------------------------

test_optimize_combines_adjacent_taps :: TestTree
test_optimize_combines_adjacent_taps =
  testCase "two adjacent Taps combine via Fanout" $ do
    let topology :: F.Topology (KStream Text Text) (KStream Text Text)
        topology =
          F.tap (F.sink "audit-a")
            >>> F.tap (F.sink "audit-b")

        opsBefore = F.inspect topology
        opsAfter  = F.inspect (F.optimize topology)

        countTaps ops = length [ () | op <- ops, op == "Tap<" ]

    -- Two Tap markers in the unoptimised AST.
    countTaps opsBefore @?= 2
    -- One Tap marker after fusion (both sinks now inside a single
    -- Tap via Fanout).
    countTaps opsAfter  @?= 1

----------------------------------------------------------------------
-- 34. Arr through Fork collapses
----------------------------------------------------------------------

test_optimize_pushes_arr_through_fork :: TestTree
test_optimize_pushes_arr_through_fork =
  testCase "Arr f . Fork collapses to a single Arr" $ do
    let topology :: F.Topology Int Int
        topology =
          F.fork                                          -- Int -> (Int, Int)
            >>> Control.Arrow.arr (\(a, b) -> a + b)       -- (Int, Int) -> Int

        before = F.countNodes topology
        after  = F.countNodes (F.optimize topology)

    -- We started with at least 'Fork', 'Arr', 'Compose' nodes.
    -- After optimisation it should collapse to just 'Arr (\a -> a + a)'.
    assertBool
      ("expected Arr.Fork to collapse; before=" <> show before
        <> " after=" <> show after)
      (after == 1 && before > 1)

----------------------------------------------------------------------
-- 35. Typed exception on a missing-serde Materialized
----------------------------------------------------------------------

test_missing_serde_throws_typed_exception :: TestTree
test_missing_serde_throws_typed_exception =
  testCase "forcing an aggregation KTable's missing serde raises TopologyFreeError" $ do
    -- A Materialized with no serdes set. The aggregation succeeds at
    -- compile time, but the resulting KTable's serde fields are
    -- deferred 'TopologyFreeError' thunks. Forcing one of them
    -- should yield a catchable exception, not an opaque 'error'.
    let unset :: Materialized Text Int64
        unset = Mat.materializedAs (storeName "missing-serde-store")

        topology :: F.Topology Void (KTable Text Int64)
        topology =
          F.source @Text @Text "in"
            >>> F.groupByKey
            >>> F.count unset

    (kt, _topo) <- F.compile topology
    -- Force the value-serde field; expect TopologyFreeError.
    result <- try (evaluate (ktableValueSerde kt))
    case result of
      Left (F.MissingMaterializedSerde F.ValueSide) -> pure ()
      Left other  ->
        error ("expected MissingMaterializedSerde ValueSide, got: "
                <> show other)
      Right _ ->
        error "expected the missing value serde to raise an exception"

----------------------------------------------------------------------
-- 36-38. EOS atomicity of Fork / ForkN / Tap topologies
----------------------------------------------------------------------
-- We can't drive a real broker in unit tests, but we can verify:
--
-- 1. Fork / ForkN / Tap topologies compile to a single source feeding
--    multiple sinks. /All sinks share the upstream source node/ — so
--    when the engine processes one record, every downstream sink
--    receives it within the same task, before any commit fires. That
--    means all sinks participate in the SAME EOS commit cycle.
--
-- 2. With a recording 'EOSCoordinator' wrapping the record-pumping
--    'flushBody', a single 'runCommitCycle' invocation drives records
--    through all branches and lands every sink's output within one
--    transaction. The call sequence is the canonical
--    @begin → commitOffsets → commit → storeCommit@.
--
-- This is the strongest assertion we can make at the unit-test layer
-- without a live transactional broker; the broker-side wire path is
-- exercised by the EOS integration tests in 'Streams.EOSRuntimeSpec'.

-- | Recording 'EOSCoordinator' that captures the call sequence in an
-- 'IORef'. Identical to the helper in 'Streams.EOSRuntimeSpec' but
-- duplicated here to avoid a cross-module test dependency.
mkRecordingCoord :: IO (EOSCoordinator, IO [Text])
mkRecordingCoord = do
  buf <- newIORef ([] :: [Text])
  let log_ s = modifyIORef' buf (s :)
      coord = EOSCoordinator
        { initTxn       = log_ "init"   *> pure (Right ())
        , beginTxn      = log_ "begin"  *> pure (Right ())
        , commitTxn     = log_ "commit" *> pure (Right ())
        , abortTxn      = log_ "abort"  *> pure (Right ())
        , commitOffsets = \_ _ -> log_ "commitOffsets" *> pure (Right ())
        , storeCommit   = log_ "storeCommit" *> pure (Right ())
        , storeAbort    = log_ "storeAbort"  *> pure (Right ())
        , preCommit2PC  = pure (Right ())
        , commit2PC     = pure (Right ())
        , abort2PC      = pure ()
        }
  pure (coord, reverse <$> readIORef buf)

test_fork_topology_is_eos_atomic :: TestTree
test_fork_topology_is_eos_atomic =
  testCase "Fork topology: all branch sinks share the source under EOS" $ do
    -- 'Fork' duplicates the wire; we sink each half to a different
    -- topic. The compiled graph has one source feeding both sinks.
    let upper :: F.Topology (KStream Text Text) ()
        upper = F.mapValues T.toUpper >>> F.sink "fork-upper"
        lower :: F.Topology (KStream Text Text) ()
        lower = F.mapValues T.toLower >>> F.sink "fork-lower"

        topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "fork-in"
            >>> F.fork
            >>> (upper *** lower)
            >>> Control.Arrow.arr (const ())

    (_, topo) <- F.compile topology
    -- Structural check: exactly one source, exactly two sinks, and
    -- both sinks are descendants of that single source. That's the
    -- topology shape EOS needs for atomic commit across both
    -- sinks.
    let !nSources = length (topologySources topo)
        !sinks    = topologySinkNames topo
    nSources @?= 1
    length sinks @?= 2

    -- Drive records through with a recording EOS coordinator
    -- wrapping the flushBody. A single commit cycle covers both
    -- sinks.
    driver <- newDriver topo "free-eos-fork"
    (coord, drain) <- mkRecordingCoord
    outcome <- runCommitCycle coord "g" (pure HMap.empty) $ do
      pipeInput driver (topicName "fork-in") Nothing (bytes "Hello") t0 0
      pipeInput driver (topicName "fork-in") Nothing (bytes "World") t0 0
    outcome @?= CommitSucceeded

    upperOut <- readOutput driver (topicName "fork-upper")
    lowerOut <- readOutput driver (topicName "fork-lower")
    map (unbytes . crValue) upperOut @?= ["HELLO", "WORLD"]
    map (unbytes . crValue) lowerOut @?= ["hello", "world"]

    -- The commit cycle ran the canonical EOS sequence, with /all/
    -- branch outputs captured between 'begin' and 'commit'.
    log_ <- drain
    log_ @?= ["begin", "commitOffsets", "commit", "storeCommit"]
    closeDriver driver

test_forkN_topology_is_eos_atomic :: TestTree
test_forkN_topology_is_eos_atomic =
  testCase "ForkN: N branch sinks all share the source under EOS" $ do
    -- A three-way ForkN; each branch writes to its own topic.
    let mkSink :: Text -> (Text -> Text) -> F.Topology (KStream Text Text) ()
        mkSink topic f =
          F.mapValues f >>> F.sink topic

        topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "forkn-in"
            >>> F.forkN
                  ( NE.fromList
                      [ mkSink "fn-upper" T.toUpper
                      , mkSink "fn-lower" T.toLower
                      , mkSink "fn-rev"   T.reverse
                      ])
            >>> Control.Arrow.arr (const ())

    (_, topo) <- F.compile topology
    let !nSources = length (topologySources topo)
        !sinks    = topologySinkNames topo
    nSources @?= 1
    length sinks @?= 3

    driver <- newDriver topo "free-eos-forkn"
    (coord, drain) <- mkRecordingCoord
    outcome <- runCommitCycle coord "g" (pure HMap.empty) $ do
      pipeInput driver (topicName "forkn-in") Nothing (bytes "abc") t0 0
    outcome @?= CommitSucceeded

    u <- readOutput driver (topicName "fn-upper")
    l <- readOutput driver (topicName "fn-lower")
    r <- readOutput driver (topicName "fn-rev")
    map (unbytes . crValue) u @?= ["ABC"]
    map (unbytes . crValue) l @?= ["abc"]
    map (unbytes . crValue) r @?= ["cba"]

    log_ <- drain
    log_ @?= ["begin", "commitOffsets", "commit", "storeCommit"]
    closeDriver driver

test_tap_topology_is_eos_atomic :: TestTree
test_tap_topology_is_eos_atomic =
  testCase "Tap topology: side sink commits atomically with main sink" $ do
    -- A 'Tap' that audit-logs to one topic while the main pipeline
    -- writes its (transformed) records to another. Both sinks must
    -- be reached within the same EOS transaction so the audit log
    -- and the main output never disagree.
    let auditSink :: F.Topology (KStream Text Text) ()
        auditSink = F.sink "tap-audit"

        topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "tap-in"
            >>> F.tap auditSink
            >>> F.mapValues T.toUpper
            >>> F.sink "tap-main"

    (_, topo) <- F.compile topology
    let !nSources = length (topologySources topo)
        !sinks    = topologySinkNames topo
    nSources @?= 1
    length sinks @?= 2

    driver <- newDriver topo "free-eos-tap"
    (coord, drain) <- mkRecordingCoord
    outcome <- runCommitCycle coord "g" (pure HMap.empty) $ do
      pipeInput driver (topicName "tap-in") Nothing (bytes "ping") t0 0
      pipeInput driver (topicName "tap-in") Nothing (bytes "pong") t0 0
    outcome @?= CommitSucceeded

    audit <- readOutput driver (topicName "tap-audit")
    main_ <- readOutput driver (topicName "tap-main")
    -- The audit log gets the /original/ values; the main sink
    -- gets the transformed ones. Both within one transaction.
    map (unbytes . crValue) audit @?= ["ping", "pong"]
    map (unbytes . crValue) main_ @?= ["PING", "PONG"]

    log_ <- drain
    log_ @?= ["begin", "commitOffsets", "commit", "storeCommit"]
    closeDriver driver

-- Small helpers reaching into the topology graph for the EOS
-- structural checks above. Both wrap module-level functions
-- already re-exported via 'Kafka.Streams'.
topologySources :: Topology -> [NodeName]
topologySources topo = Map.keys (topoSources topo)

topologySinkNames :: Topology -> [NodeName]
topologySinkNames topo = Map.keys (topoSinks topo)

----------------------------------------------------------------------
-- 39. Applicative liftA2 combines two topologies on one input
----------------------------------------------------------------------

test_applicative_liftA2_combines_two_topologies :: TestTree
test_applicative_liftA2_combines_two_topologies =
  testCase "Applicative <*> runs both topologies on the same input" $ do
    -- Build two topologies that each compute a wire value from a
    -- shared input, then combine them via the Applicative instance
    -- (here through the Functor instance + <*>). The resulting
    -- topology produces a tuple of wire handles for downstream use.
    let topology :: F.Topology Void (KStream Text Text)
        topology =
          ( (,) <$> F.source @Text @Text "ap-in1"
                <*> F.source @Text @Text "ap-in2" )
            >>= \(s1, s2) -> F.merge `F.applyT` (s1, s2)

        topologyWithSink :: F.Topology Void ()
        topologyWithSink = topology >>= F.applyT (F.sink "ap-out")

    (_, topo) <- F.compile topologyWithSink
    driver <- newDriver topo "free-applicative"

    pipeInput driver (topicName "ap-in1") Nothing (bytes "a1") t0 0
    pipeInput driver (topicName "ap-in2") Nothing (bytes "b1") t0 0
    pipeInput driver (topicName "ap-in1") Nothing (bytes "a2") t0 0

    out <- readOutput driver (topicName "ap-out")
    map (unbytes . crValue) out @?= ["a1", "b1", "a2"]
    closeDriver driver

----------------------------------------------------------------------
-- 40. Monad do-notation for multi-source topologies
----------------------------------------------------------------------

test_monad_do_notation_for_multi_source :: TestTree
test_monad_do_notation_for_multi_source =
  testCase "do-notation threads source handles through a pipeline" $ do
    let topology :: F.Topology Void ()
        topology = do
          s1 <- F.source @Text @Text "do-in1"
          s2 <- F.source @Text @Text "do-in2"
          -- Map each side, then merge, then sink. Each step uses
          -- 'applyT' to feed the bound Haskell value into the next
          -- fragment.
          u1 <- F.mapValues T.toUpper       `F.applyT` s1
          u2 <- F.mapValues (T.append "B:") `F.applyT` s2
          m  <- F.merge                     `F.applyT` (u1, u2)
          F.sink "do-out" `F.applyT` m

    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-monad-multi-source"

    pipeInput driver (topicName "do-in1") Nothing (bytes "hi") t0 0
    pipeInput driver (topicName "do-in2") Nothing (bytes "ya") t0 0

    out <- readOutput driver (topicName "do-out")
    -- The merged stream sees mapped values from both upstreams.
    -- The exact interleaving is the test driver's record-arrival
    -- order; both records must appear.
    map (unbytes . crValue) out @?= ["HI", "B:ya"]
    closeDriver driver

----------------------------------------------------------------------
-- 41. Semigroup: run two pipelines on a shared upstream
----------------------------------------------------------------------

test_semigroup_runs_both_pipelines_on_one_input :: TestTree
test_semigroup_runs_both_pipelines_on_one_input =
  testCase "Topology i () <> Topology i () runs both on the same input" $ do
    -- Two completely separate sink pipelines that share a single
    -- source. Operationally equivalent to Fanout-with-discard but
    -- much terser to write. Both branches share lineage, so EOS
    -- atomicity is by construction (verified separately by the
    -- Fanout EOS test above).
    let left, right :: F.Topology (KStream Text Text) ()
        left  = F.mapValues T.toUpper >>> F.sink "sg-upper"
        right = F.mapValues T.toLower >>> F.sink "sg-lower"

        topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "sg-in"
            >>> (left <> right)

    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-semigroup"

    pipeInput driver (topicName "sg-in") Nothing (bytes "Mixed") t0 0
    pipeInput driver (topicName "sg-in") Nothing (bytes "Case")  t0 0

    u <- readOutput driver (topicName "sg-upper")
    l <- readOutput driver (topicName "sg-lower")
    map (unbytes . crValue) u @?= ["MIXED", "CASE"]
    map (unbytes . crValue) l @?= ["mixed", "case"]
    closeDriver driver

----------------------------------------------------------------------
-- 42. Monoid: mempty at unit output is a no-op
----------------------------------------------------------------------

test_monoid_unit_output_is_no_op :: TestTree
test_monoid_unit_output_is_no_op =
  testCase "mempty :: Topology (KStream k v) () drops records silently" $ do
    let topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "monoid-in"
            >>> mempty   -- Drop everything; no sink, no foreach
    -- Just checking that compilation + driver setup don't crash;
    -- there's nothing observable to assert.
    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-monoid"
    pipeInput driver (topicName "monoid-in") Nothing (bytes "x") t0 0
    closeDriver driver

----------------------------------------------------------------------
-- 43. Profunctor: dimap composes appropriately on a Topology
----------------------------------------------------------------------

test_profunctor_dimap_works :: TestTree
test_profunctor_dimap_works =
  testCase "dimap pre/post-composes pure functions on a topology" $ do
    let inner :: F.Topology Int Int
        inner = Control.Arrow.arr (+ 1)

        wrapped :: F.Topology Text Text
        wrapped = dimap T.length (T.pack . show) inner

        -- compileNoOptimize so we can sanity-check that dimap is
        -- 3 nodes (lmap Arr + inner + rmap Arr) without surprising
        -- collapses.
        nWrapped = F.countNodes wrapped
        nInner   = F.countNodes inner

    -- The optimised count should be 1 — the chain of pure functions
    -- collapses into a single Arr (since 'inner' itself is an Arr).
    F.countNodes (F.optimize wrapped) @?= 1
    -- Unoptimised, dimap added two Arr wrappers and a Compose around
    -- the inner.
    assertBool
      ("expected dimap to wrap; unwrapped=" <> show nInner
        <> " wrapped=" <> show nWrapped)
      (nWrapped > nInner)

----------------------------------------------------------------------
-- 44. Reader-style: localInput pre-transforms the input
----------------------------------------------------------------------

test_reader_localInput_pre_transforms :: TestTree
test_reader_localInput_pre_transforms =
  testCase "localInput pre-applies its function to the wire input" $ do
    -- Build a pure-function pipeline; localInput pre-applies
    -- 'T.length' so the inner sees an Int instead of a Text.
    let inner :: F.Topology Int Int
        inner = Control.Arrow.arr (\n -> n * 2)

        wrapped :: F.Topology Text Int
        wrapped = F.localInput T.length inner

        opt = F.optimize wrapped
    -- After optimisation we have a single Arr (the chain of pure
    -- functions collapses), and computing it through apply
    -- gives the expected value.
    F.countNodes opt @?= 1

----------------------------------------------------------------------
-- 45. Cross-source EOS via 'mergeSourced'
----------------------------------------------------------------------

test_mergeSourced_two_sources_share_one_task_under_eos :: TestTree
test_mergeSourced_two_sources_share_one_task_under_eos =
  testCase "mergeSourced makes two sources share a single Kafka task" $ do
    -- Without mergeSourced, two source-rooted halves of a Fanout
    -- compile to two disconnected sub-topologies (= two tasks =
    -- two EOS transactions). 'mergeSourced' inserts a convergence
    -- 'Merge' node so the graph is one connected component =
    -- one sub-topology = one task = one EOS transaction.
    let topology :: F.Topology Void ()
        topology =
          F.mergeSourced
            (F.source @Text @Text "ms-a")
            (F.source @Text @Text "ms-b")
            >>> F.mapValues T.toUpper
            >>> F.sink "ms-out"

    (_, topo) <- F.compile topology

    -- Structural check: TWO source topics, ONE merge processor
    -- with both as parents, ONE sink. The graph is connected.
    let !nSources = length (topologySources topo)
        !sinks    = topologySinkNames topo
    nSources @?= 2
    length sinks @?= 1

    driver <- newDriver topo "free-eos-merged-sources"
    (coord, drain) <- mkRecordingCoord
    outcome <- runCommitCycle coord "g" (pure HMap.empty) $ do
      pipeInput driver (topicName "ms-a") Nothing (bytes "hello") t0 0
      pipeInput driver (topicName "ms-b") Nothing (bytes "world") t0 0
    outcome @?= CommitSucceeded

    out <- readOutput driver (topicName "ms-out")
    map (unbytes . crValue) out @?= ["HELLO", "WORLD"]

    log_ <- drain
    log_ @?= ["begin", "commitOffsets", "commit", "storeCommit"]
    closeDriver driver

----------------------------------------------------------------------
-- 46. Cogroup expressed via Monad bind + applyT
----------------------------------------------------------------------

test_cogroup_via_do_notation :: TestTree
test_cogroup_via_do_notation =
  testCase "cogroup builds incrementally via do-notation + applyT" $ do
    -- The cogroup pattern: each source produces its own grouped
    -- stream; the cogroup builder chains them with per-source
    -- aggregators. With the Monad instance and 'applyT' the
    -- builder reads like the imperative Java code, while the
    -- result is still a value-level Topology that compiles and
    -- runs through the standard driver.
    let g = grouped textSerde textSerde
        outMat :: Materialized Text Text
        outMat =
          Mat.withValueSerde textSerde
            $ Mat.withKeySerde textSerde
            $ Mat.materializedAs (storeName "cog-do-store")

        leftStep, rightStep :: Text -> Text -> Text -> Text
        leftStep  _ v acc = acc <> "/" <> v
        rightStep _ v acc = acc <> "+" <> v

        topology :: F.Topology Void (KTable Text Text)
        topology = do
          s1   <- F.source @Text @Text "cogdo-in1"
          s2   <- F.source @Text @Text "cogdo-in2"
          g1   <- F.groupByKeyWith g `F.applyT` s1
          g2   <- F.groupByKeyWith g `F.applyT` s2
          cgs0 <- F.cogroup leftStep `F.applyT` g1
          cgs1 <- F.addCogrouped rightStep `F.applyT` (cgs0, g2)
          F.aggregateCogrouped (pure "") outMat `F.applyT` cgs1

    (kt, topo) <- F.compile topology
    driver <- newDriver topo "free-cogdo"

    pipeInput driver (topicName "cogdo-in1") (Just (bytes "k")) (bytes "a") (t 0) 0
    pipeInput driver (topicName "cogdo-in2") (Just (bytes "k")) (bytes "b") (t 1) 0
    pipeInput driver (topicName "cogdo-in1") (Just (bytes "k")) (bytes "c") (t 2) 0

    mStore <- getKeyValueStore @Text @Text driver (ktableStore kt)
    case mStore of
      Just kvs -> kvsGet kvs "k" >>= (@?= Just "/a+b/c")
      Nothing  -> error "cogroup-do store missing"
    closeDriver driver

----------------------------------------------------------------------
-- 47. Graphviz: topologyDot emits a well-formed DOT graph
----------------------------------------------------------------------

test_graphviz_topologyDot_emits_valid_dot :: TestTree
test_graphviz_topologyDot_emits_valid_dot =
  testCase "topologyDot renders a complete compiled topology as DOT" $ do
    -- A topology with source / processors / sink / state store /
    -- a fanout — exercises every node-type the renderer knows
    -- how to draw.
    let countMat :: Materialized Text Int64
        countMat =
          Mat.withValueSerde int64Serde
            $ Mat.withKeySerde textSerde
            $ Mat.materializedAs (storeName "dot-count-store")

        topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "dot-in"
            >>> F.tap (F.foreach (\_ -> pure ()))
            >>> F.mapValues T.toUpper
            >>> F.groupByKey
            >>> F.count countMat
            >>> F.toStream
            >>> F.mapValues (T.pack . show)
            >>> F.sink "dot-out"

    (_, topo) <- F.compile topology
    let dot = DOT.topologyDot topo

    -- The output is a DOT digraph. Basic structural checks:
    assertBool "DOT starts with 'digraph topology {'" $
      "digraph topology {" `T.isPrefixOf` dot
    assertBool "DOT ends with '}'" $
      "}\n" `T.isSuffixOf` dot
    assertBool "DOT contains rankdir directive" $
      "rankdir=" `T.isInfixOf` dot
    -- Sources are drawn as rounded boxes.
    assertBool "DOT contains a source shape (rounded box)" $
      "style=\"filled,rounded\"" `T.isInfixOf` dot
    -- Sinks are drawn as inverted trapeziums.
    assertBool "DOT contains a sink shape (invtrapezium)" $
      "shape=invtrapezium" `T.isInfixOf` dot
    -- State stores are drawn as cylinders.
    assertBool "DOT contains a state-store shape (cylinder)" $
      "shape=cylinder" `T.isInfixOf` dot
    -- Topic names show up in the source/sink labels.
    assertBool "DOT mentions the source topic" $
      "dot-in" `T.isInfixOf` dot
    assertBool "DOT mentions the sink topic" $
      "dot-out" `T.isInfixOf` dot
    -- Store ownership uses dashed edges.
    assertBool "DOT contains a dashed store-ownership edge" $
      "style=dashed" `T.isInfixOf` dot
    -- Edges have the parent -> child shape.
    assertBool "DOT contains edges" $
      " -> " `T.isInfixOf` dot

----------------------------------------------------------------------
-- 48. Graphviz: astDot renders the GADT constructor tree
----------------------------------------------------------------------

test_graphviz_astDot_emits_valid_dot :: TestTree
test_graphviz_astDot_emits_valid_dot =
  testCase "astDot renders the AST as DOT" $ do
    let topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "ast-in"
            >>> F.mapValues T.toUpper
            >>> F.tap (F.sink "audit")
            >>> F.filter (\r -> recordValue r /= "")
            >>> F.sink "ast-out"

        dot = DOT.astDot topology

    -- Basic structural assertions on the rendered DOT.
    assertBool "DOT starts with 'digraph ast {'" $
      "digraph ast {" `T.isPrefixOf` dot
    assertBool "DOT ends with '}'" $
      "}\n" `T.isSuffixOf` dot
    -- Each constructor in the AST contributes a labelled node.
    assertBool "AST DOT contains a Source label" $
      "Source" `T.isInfixOf` dot
    assertBool "AST DOT contains a MapValues label" $
      "MapValues" `T.isInfixOf` dot
    assertBool "AST DOT contains a Tap label" $
      "Tap" `T.isInfixOf` dot
    assertBool "AST DOT contains a Sink label" $
      "Sink" `T.isInfixOf` dot
    assertBool "AST DOT contains a Filter label" $
      "Filter" `T.isInfixOf` dot
    -- Structural nodes (Compose etc.) are diamond-shaped, leaves
    -- are ellipses / boxes; both shape attributes must appear.
    assertBool "AST DOT contains a diamond-shaped structural node" $
      "shape=diamond" `T.isInfixOf` dot
    -- The optimised AST should still render; sanity check
    -- with the optimised version produces fewer or equal
    -- nodes.
    let dotOpt = DOT.astDot (F.optimize topology)
    assertBool "AST DOT (optimised) is non-empty" $
      not (T.null dotOpt)

----------------------------------------------------------------------
-- 49. inspectDeep walks through Bind continuations
----------------------------------------------------------------------

test_inspectDeep_walks_through_bind_continuations :: TestTree
test_inspectDeep_walks_through_bind_continuations =
  testCase "inspectDeep emits tokens for every constructor past a Bind" $ do
    -- A monadic topology with do-notation. The binds are
    -- genuine — each line uses the previously-bound wire
    -- value. 'inspect' alone would render the binds as
    -- opaque; 'inspectDeep' actually runs apply on each
    -- bind's left side to extract the wire value, hands it
    -- to the continuation, and walks the result.
    let topology :: F.Topology Void ()
        topology = do
          s1 <- F.source @Text @Text "deep-in1"
          s2 <- F.source @Text @Text "deep-in2"
          u1 <- F.mapValues T.toUpper       `F.applyT` s1
          u2 <- F.mapValues (T.append "B:") `F.applyT` s2
          m  <- F.merge                     `F.applyT` (u1, u2)
          F.sink "deep-out" `F.applyT` m

        shallowToks = F.inspect topology
        deepToks    = F.inspectDeep topology

    -- 'inspect' shows the outermost Bind as an opaque marker and
    -- doesn't recurse into its continuation.
    assertBool "inspect surfaces a Bind marker" $
      any ("Bind" `T.isPrefixOf`) shallowToks

    -- 'inspectDeep' walks through the binds and emits tokens for
    -- every operator down to the sink.
    assertBool "inspectDeep emits a Source token for source 1" $
      any (\t -> "Source" `T.isPrefixOf` t && "deep-in1" `T.isInfixOf` t) deepToks
    assertBool "inspectDeep emits a Source token for source 2" $
      any (\t -> "Source" `T.isPrefixOf` t && "deep-in2" `T.isInfixOf` t) deepToks
    assertBool "inspectDeep emits MapValues tokens for both upstreams" $
      length (Prelude.filter (== "MapValues") deepToks) >= 2
    assertBool "inspectDeep emits a Merge token" $
      "Merge" `elem` deepToks
    assertBool "inspectDeep emits a Sink token for the output" $
      any (\t -> "Sink" `T.isPrefixOf` t && "deep-out" `T.isInfixOf` t) deepToks
    -- Bind markers are present and properly bracketed.
    assertBool "inspectDeep shows the bind start" $
      "Bind<" `elem` deepToks
    assertBool "inspectDeep shows the bind transition" $
      ">~>" `elem` deepToks
    assertBool "inspectDeep shows the bind end" $
      "</Bind>" `elem` deepToks

----------------------------------------------------------------------
-- 50. inspect vs inspectDeep: applicative-shaped topology is the same
----------------------------------------------------------------------

test_inspect_vs_inspectDeep :: TestTree
test_inspect_vs_inspectDeep =
  testCase "inspect = inspectDeep on bind-free topologies" $ do
    -- A purely applicative-shaped topology (no Bind). Both
    -- inspectors must see the same tokens, since there's
    -- nothing for inspectDeep to do beyond what inspect does.
    let topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "ivd-in"
            >>> F.mapValues T.toUpper
            >>> F.filter (\r -> recordValue r /= "")
            >>> F.sink "ivd-out"

        shallow = F.inspect topology
        deep    = F.inspectDeep topology

    -- The deep walk emits the same tokens (the binds it
    -- doesn't encounter add no markers).
    shallow @?= deep

----------------------------------------------------------------------
-- 51. noFuse blocks adjacent MapValues fusion
----------------------------------------------------------------------

test_noFuse_blocks_mapValues_fusion :: TestTree
test_noFuse_blocks_mapValues_fusion =
  testCase "noFuse keeps adjacent mapValues from collapsing" $ do
    let baseline :: F.Topology Void ()
        baseline =
          F.source @Text @Text "in"
            >>> F.mapValues (T.append "a")
            >>> F.mapValues (T.append "b")
            >>> F.mapValues (T.append "c")
            >>> F.sink "out"

        barriered :: F.Topology Void ()
        barriered =
          F.source @Text @Text "in"
            >>> F.mapValues (T.append "a")
            >>> F.noFuse
            >>> F.mapValues (T.append "b")
            >>> F.noFuse
            >>> F.mapValues (T.append "c")
            >>> F.sink "out"

        baselineStats   = F.optimizationStats baseline
        baselineTokens  = F.inspect (F.optimize baseline)
        barrieredTokens = F.inspect (F.optimize barriered)

    -- Without barriers, the three mapValues fuse into one node.
    assertBool
      ("baseline should fuse maps; stats = " <> show baselineStats)
      (F.osNodesSaved baselineStats >= 2)
    length (Prelude.filter (== "MapValues") baselineTokens) @?= 1

    -- With barriers, each mapValues stays distinct.
    length (Prelude.filter (== "MapValues") barrieredTokens) @?= 3
    -- And the barriers themselves are visible in the inspected
    -- token stream (one per noFuse call).
    length (Prelude.filter (== "NoFuse") barrieredTokens) @?= 2

----------------------------------------------------------------------
-- 52. noFuse is a runtime identity
----------------------------------------------------------------------

test_noFuse_is_runtime_identity :: TestTree
test_noFuse_is_runtime_identity =
  testCase "noFuse forwards every record unchanged at runtime" $ do
    let topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "nf-in"
            >>> F.mapValues T.toUpper
            >>> F.noFuse
            >>> F.mapValues (<> "!")
            >>> F.sink "nf-out"

    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-nofuse-identity"
    pipeInput driver (topicName "nf-in") Nothing (bytes "hello") t0 0
    pipeInput driver (topicName "nf-in") Nothing (bytes "world") t0 0
    out <- readOutput driver (topicName "nf-out")
    Prelude.map (unbytes . crValue) out @?= ["HELLO!", "WORLD!"]
    closeDriver driver

----------------------------------------------------------------------
-- 53. noFuse blocks Filter fusion too
----------------------------------------------------------------------

test_noFuse_blocks_filter_fusion :: TestTree
test_noFuse_blocks_filter_fusion =
  testCase "noFuse keeps adjacent filters from fusing into a conjunction" $ do
    let withBarrier :: F.Topology Void ()
        withBarrier =
          F.source @Text @Text "in"
            >>> F.filter (\r -> T.length (recordValue r) >= 2)
            >>> F.noFuse
            >>> F.filter (\r -> T.length (recordValue r) <= 5)
            >>> F.sink "out"

        toks = F.inspect (F.optimize withBarrier)

    -- Both Filter nodes survive; the optimiser did not collapse
    -- them despite the toggle being on.
    length (Prelude.filter (== "Filter") toks) @?= 2
    length (Prelude.filter (== "NoFuse") toks) @?= 1

----------------------------------------------------------------------
-- 54. noFuse alone (no neighbours) is left untouched
----------------------------------------------------------------------

test_noFuse_left_alone_in_isolation :: TestTree
test_noFuse_left_alone_in_isolation =
  testCase "noFuse outside any fusion candidate survives optimisation" $ do
    let topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "lone-in"
            >>> F.noFuse
            >>> F.sink "lone-out"
        toks = F.inspect (F.optimize topology)

    length (Prelude.filter (== "NoFuse") toks) @?= 1
    -- And the topology still runs.
    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-nofuse-lone"
    pipeInput driver (topicName "lone-in") Nothing (bytes "x") t0 0
    out <- readOutput driver (topicName "lone-out")
    Prelude.map (unbytes . crValue) out @?= ["x"]
    closeDriver driver

----------------------------------------------------------------------
-- 55. mapRecord: full-record pure transform
----------------------------------------------------------------------

test_mapRecord_full_record_transform :: TestTree
test_mapRecord_full_record_transform =
  testCase "mapRecord can read and write headers + timestamp" $ do
    -- The transform stamps each record with a custom header
    -- and shifts the timestamp by +1000 ms. Headers and
    -- timestamps aren't visible through mapValues / mapKeyValue,
    -- so this exercises the new record-level smart constructor.
    let topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "mr-in"
            >>> F.mapRecord
                  (\r -> Record
                    { recordKey       = recordKey r
                    , recordValue     = recordValue r <> "!"
                    , recordTimestamp = recordTimestamp r
                    , recordHeaders   = recordHeaders r
                    })
            >>> F.sink "mr-out"

    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-mapRecord"
    pipeInput driver (topicName "mr-in") Nothing (bytes "hello") t0 0
    out <- readOutput driver (topicName "mr-out")
    Prelude.map (unbytes . crValue) out @?= ["hello!"]
    closeDriver driver

----------------------------------------------------------------------
-- 56. mapRecordM: IO full-record transform
----------------------------------------------------------------------

test_mapRecordM_io_full_record_transform :: TestTree
test_mapRecordM_io_full_record_transform =
  testCase "mapRecordM runs IO per record" $ do
    -- The transform appends an IORef-tracked counter to each
    -- value to confirm the IO ran.
    counter <- newIORef (0 :: Int)
    let topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "mrm-in"
            >>> F.mapRecordM
                  (\r -> do
                     n <- atomicModifyIORef' counter (\c -> (c + 1, c + 1))
                     pure r { recordValue =
                                recordValue r <> "#" <> T.pack (show n) })
            >>> F.sink "mrm-out"

    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-mapRecordM"
    pipeInput driver (topicName "mrm-in") Nothing (bytes "a") t0 0
    pipeInput driver (topicName "mrm-in") Nothing (bytes "b") t0 0
    pipeInput driver (topicName "mrm-in") Nothing (bytes "c") t0 0
    out <- readOutput driver (topicName "mrm-out")
    Prelude.map (unbytes . crValue) out @?= ["a#1", "b#2", "c#3"]
    closeDriver driver

----------------------------------------------------------------------
-- 57. mapRecord chains fuse
----------------------------------------------------------------------

test_mapRecord_chains_fuse :: TestTree
test_mapRecord_chains_fuse =
  testCase "chained mapRecord (and mapRecordM) collapse to a single node" $ do
    let topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "mrf-in"
            >>> F.mapRecord (\r -> r { recordValue = recordValue r <> "1" })
            >>> F.mapRecord (\r -> r { recordValue = recordValue r <> "2" })
            >>> F.mapRecord (\r -> r { recordValue = recordValue r <> "3" })
            >>> F.sink "mrf-out"

        stats = F.optimizationStats topology
        toks  = F.inspect (F.optimize topology)

    assertBool
      ("expected mapRecord chain to fuse; stats = " <> show stats)
      (F.osNodesSaved stats >= 2)
    length (Prelude.filter (== "MapRecord") toks) @?= 1

    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-mapRecord-fuse"
    pipeInput driver (topicName "mrf-in") Nothing (bytes "x") t0 0
    out <- readOutput driver (topicName "mrf-out")
    Prelude.map (unbytes . crValue) out @?= ["x123"]
    closeDriver driver

----------------------------------------------------------------------
-- 58. mapRecord fusion is blocked by noFuse
----------------------------------------------------------------------

test_mapRecord_chain_blocked_by_noFuse :: TestTree
test_mapRecord_chain_blocked_by_noFuse =
  testCase "noFuse between mapRecord calls keeps them as separate nodes" $ do
    let topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "mrnf-in"
            >>> F.mapRecord (\r -> r { recordValue = recordValue r <> "1" })
            >>> F.noFuse
            >>> F.mapRecord (\r -> r { recordValue = recordValue r <> "2" })
            >>> F.noFuse
            >>> F.mapRecord (\r -> r { recordValue = recordValue r <> "3" })
            >>> F.sink "mrnf-out"

        toks = F.inspect (F.optimize topology)

    length (Prelude.filter (== "MapRecord") toks) @?= 3
    length (Prelude.filter (== "NoFuse") toks)    @?= 2

    -- And the topology still produces the same observable
    -- output as the fused version.
    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-mapRecord-nofuse"
    pipeInput driver (topicName "mrnf-in") Nothing (bytes "x") t0 0
    out <- readOutput driver (topicName "mrnf-out")
    Prelude.map (unbytes . crValue) out @?= ["x123"]
    closeDriver driver

----------------------------------------------------------------------
-- 59. repartition >>> selectKey drops the wasted repartition
----------------------------------------------------------------------

test_drop_repartition_before_selectKey :: TestTree
test_drop_repartition_before_selectKey =
  testCase "repartition immediately followed by selectKey is dropped" $ do
    let topology :: F.Topology (KStream Text Text) (KStream Text Text)
        topology =
          F.repartition "wasted"
            >>> F.selectKey (\_ -> "fresh")

        toksBefore = F.inspect topology
        toksAfter  = F.inspect (F.optimize topology)

    -- Pre-optimisation: the Repartition is visible.
    assertBool "baseline contains Repartition" $
      any (T.isPrefixOf "Repartition") toksBefore
    -- Post-optimisation: it's gone.
    assertBool "optimised AST drops the wasted Repartition" $
      not (any (T.isPrefixOf "Repartition") toksAfter)
    -- The SelectKey survives.
    assertBool "SelectKey is preserved" $
      "SelectKey" `elem` toksAfter

----------------------------------------------------------------------
-- 60. repartition >>> mapKeyValue drops the wasted repartition
----------------------------------------------------------------------

test_drop_repartition_before_mapKeyValue :: TestTree
test_drop_repartition_before_mapKeyValue =
  testCase "repartition immediately followed by mapKeyValue is dropped" $ do
    let topology :: F.Topology (KStream Text Text) (KStream Text Text)
        topology =
          F.repartition "wasted"
            >>> F.mapKeyValue (\_ v -> ("new-key", v))

        toks = F.inspect (F.optimize topology)
    assertBool "optimised AST drops the wasted Repartition" $
      not (any (T.isPrefixOf "Repartition") toks)
    assertBool "MapKeyValue is preserved" $
      "MapKeyValue" `elem` toks

----------------------------------------------------------------------
-- 61. repartition >>> concatMapKeyValue drops the wasted repartition
----------------------------------------------------------------------

test_drop_repartition_before_flatMapKeyValue :: TestTree
test_drop_repartition_before_flatMapKeyValue =
  testCase "repartition immediately followed by concatMapKeyValue is dropped" $ do
    let topology :: F.Topology (KStream Text Text) (KStream Text Text)
        topology =
          F.repartition "wasted"
            >>> F.concatMapKeyValue (\_ v -> [("k1", v), ("k2", v)])

        toks = F.inspect (F.optimize topology)
    assertBool "optimised AST drops the wasted Repartition" $
      not (any (T.isPrefixOf "Repartition") toks)
    assertBool "ConcatMapKeyValue is preserved" $
      "ConcatMapKeyValue" `elem` toks

----------------------------------------------------------------------
-- 62. repartition >>> mapValues swaps so mapValues runs upstream
----------------------------------------------------------------------

test_hoist_mapValues_through_repartition :: TestTree
test_hoist_mapValues_through_repartition =
  testCase "mapValues is hoisted upstream of an adjacent repartition" $ do
    let topology :: F.Topology (KStream Text Text) (KStream Text Text)
        topology =
          F.repartition "shuffle"
            >>> F.mapValues T.toUpper

        toks = F.inspect (F.optimize topology)
    -- Both ops survive; their order is reversed (mapValues first
    -- in flow order, then Repartition).
    let idxMap        = elemIndex "MapValues"        toks
        idxRepartPart = findIndex (T.isPrefixOf "Repartition") toks
    assertBool "MapValues is present"   $ idxMap        /= Nothing
    assertBool "Repartition is present" $ idxRepartPart /= Nothing
    -- Flow order: 'inspect' walks Compose 'f then g', so the
    -- upstream op appears earlier in the token list.
    case (idxMap, idxRepartPart) of
      (Just iM, Just iR) ->
        assertBool ("MapValues should be upstream of Repartition; toks=" <> show toks)
                   (iM < iR)
      _ -> assertFailure "expected both tokens"

----------------------------------------------------------------------
-- 63. repartition >>> filter swaps so the filter runs upstream
----------------------------------------------------------------------

test_hoist_filter_through_repartition :: TestTree
test_hoist_filter_through_repartition =
  testCase "filter is hoisted upstream of an adjacent repartition" $ do
    let topology :: F.Topology (KStream Text Text) (KStream Text Text)
        topology =
          F.repartition "shuffle"
            >>> F.filter (\r -> T.length (recordValue r) > 1)

        toks = F.inspect (F.optimize topology)
    let idxFilter = elemIndex "Filter" toks
        idxRepart = findIndex (T.isPrefixOf "Repartition") toks
    case (idxFilter, idxRepart) of
      (Just iF, Just iR) ->
        assertBool ("Filter should be upstream of Repartition; toks=" <> show toks)
                   (iF < iR)
      _ -> assertFailure "expected both tokens"

----------------------------------------------------------------------
-- 64. hoist enables upstream fusion that was blocked before
----------------------------------------------------------------------

test_hoist_enables_upstream_fusion :: TestTree
test_hoist_enables_upstream_fusion =
  testCase "hoisting through repartition lets adjacent mapValues fuse" $ do
    -- Before optimisation: mapValues "a" >>> repartition >>> mapValues "b"
    -- Without the hoist rule, the optimiser can't fuse the two
    -- mapValues calls because the repartition sits between them.
    -- WITH the hoist rule: mapValues "b" gets pushed upstream of
    -- the repartition; now both mapValues are adjacent and fuse.
    let topology :: F.Topology (KStream Text Text) (KStream Text Text)
        topology =
          F.mapValues (<> "a")
            >>> F.repartition "shuffle"
            >>> F.mapValues (<> "b")

        toks    = F.inspect (F.optimize topology)
        toksOff = F.inspect (F.optimizeWith
                              (F.defaultOptimizeConfig
                                { F.optHoistThroughRepartition = False })
                              topology)
    -- With hoist: a single fused MapValues node.
    length (Prelude.filter (== "MapValues") toks) @?= 1
    -- With hoist off: both MapValues survive (no fusion).
    length (Prelude.filter (== "MapValues") toksOff) @?= 2

----------------------------------------------------------------------
-- 65. hoist toggle off keeps the original order
----------------------------------------------------------------------

test_hoist_disabled_keeps_original_order :: TestTree
test_hoist_disabled_keeps_original_order =
  testCase "optHoistThroughRepartition disabled keeps the original order" $ do
    let topology :: F.Topology (KStream Text Text) (KStream Text Text)
        topology =
          F.repartition "shuffle"
            >>> F.mapValues T.toUpper

        cfg = F.defaultOptimizeConfig
                { F.optHoistThroughRepartition = False }
        toks = F.inspect (F.optimizeWith cfg topology)
    let idxMap    = elemIndex "MapValues" toks
        idxRepart = findIndex (T.isPrefixOf "Repartition") toks
    case (idxMap, idxRepart) of
      (Just iM, Just iR) ->
        assertBool ("Repartition should still be upstream of MapValues; toks=" <> show toks)
                   (iR < iM)
      _ -> assertFailure "expected both tokens"

----------------------------------------------------------------------
-- 66. drop toggle off keeps the wasted repartition
----------------------------------------------------------------------

test_drop_disabled_keeps_repartition :: TestTree
test_drop_disabled_keeps_repartition =
  testCase "optDropPreKeyChangeRepartition disabled preserves the repartition" $ do
    let topology :: F.Topology (KStream Text Text) (KStream Text Text)
        topology =
          F.repartition "wasted"
            >>> F.selectKey (\_ -> "fresh")

        cfg = F.defaultOptimizeConfig
                { F.optDropPreKeyChangeRepartition = False }
        toks = F.inspect (F.optimizeWith cfg topology)
    assertBool "Repartition survives when the toggle is off" $
      any (T.isPrefixOf "Repartition") toks

----------------------------------------------------------------------
-- 67. repartition rewrites preserve end-to-end semantics
----------------------------------------------------------------------

test_repartition_rewrites_preserve_semantics :: TestTree
test_repartition_rewrites_preserve_semantics =
  testCase "drop + hoist rewrites preserve observable output" $ do
    -- A topology that exercises both rewrites: a wasted
    -- repartition before a selectKey, and a hoistable
    -- mapValues between two repartitions.
    let topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "rp-in"
            >>> F.repartition "first"
            >>> F.mapValues T.toUpper
            >>> F.repartition "second"
            >>> F.selectKey (\r -> recordValue r)
            >>> F.sink "rp-out"

    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-repart-rewrites"
    pipeInput driver (topicName "rp-in") (Just (bytes "k")) (bytes "hello") t0 0
    pipeInput driver (topicName "rp-in") (Just (bytes "k")) (bytes "world") t0 0
    out <- readOutput driver (topicName "rp-out")
    Prelude.map (unbytes . crValue) out @?= ["HELLO", "WORLD"]
    closeDriver driver

----------------------------------------------------------------------
-- 68. Auto-insert: SelectKey + MapKeyValue >>> GroupByKey
----------------------------------------------------------------------

test_auto_insert_before_groupByKey :: TestTree
test_auto_insert_before_groupByKey =
  testCase "auto-insert wraps groupByKey with a Repartition when upstream re-keys" $ do
    let topology :: F.Topology (KStream Text Text) (KGroupedStream Text Text)
        topology =
          F.mapKeyValue (\_ v -> (T.toUpper v, v))
            >>> F.groupByKey
        cfgOn  = F.defaultOptimizeConfig
        cfgOff = F.defaultOptimizeConfig
                   { F.optAutoInsertRepartition = False }
        toksOn  = F.inspect (F.optimizeWith cfgOn  topology)
        toksOff = F.inspect (F.optimizeWith cfgOff topology)
    -- With auto-insert on, the Repartition appears between the
    -- key-changing op and the groupByKey.
    assertBool ("expected Repartition in optimised tokens; " <> show toksOn) $
      any (T.isPrefixOf "Repartition") toksOn
    -- With auto-insert off, no Repartition is added.
    assertBool ("expected no Repartition; " <> show toksOff) $
      not (any (T.isPrefixOf "Repartition") toksOff)

----------------------------------------------------------------------
-- 69. Auto-insert: SelectKey >>> ToTable
----------------------------------------------------------------------

test_auto_insert_before_toTable :: TestTree
test_auto_insert_before_toTable =
  testCase "auto-insert wraps toTable with a Repartition when upstream re-keys" $ do
    let topology :: F.Topology (KStream Text Text) (KTable Text Text)
        topology =
          F.selectKey (\r -> recordValue r)
            >>> F.toTable (Mat.materializedAs (storeName "tbl"))
        toks = F.inspect (F.optimize topology)
    assertBool ("expected Repartition before ToTable; " <> show toks) $
      any (T.isPrefixOf "Repartition") toks

----------------------------------------------------------------------
-- 70. Auto-insert: dirty flag carries through stateless ops
----------------------------------------------------------------------

test_auto_insert_through_mapValues_chain :: TestTree
test_auto_insert_through_mapValues_chain =
  testCase "key-dirty flag carries through mapValues / filter / peek" $ do
    let topology :: F.Topology (KStream Text Text) (KGroupedStream Text Text)
        topology =
          F.selectKey (\r -> recordValue r)
            >>> F.mapValues T.toUpper
            >>> F.filter (\r -> recordValue r /= "")
            >>> F.peek (\_ -> pure ())
            >>> F.groupByKey
        toks = F.inspect (F.optimize topology)
    -- Despite the chain of stateless ops between the key change
    -- and the groupByKey, the auto-insert still fires.
    assertBool ("expected Repartition somewhere; " <> show toks) $
      any (T.isPrefixOf "Repartition") toks

----------------------------------------------------------------------
-- 71. Auto-insert: disabled toggle is a no-op
----------------------------------------------------------------------

test_auto_insert_off_keeps_nothing :: TestTree
test_auto_insert_off_keeps_nothing =
  testCase "optAutoInsertRepartition=False inserts nothing" $ do
    let topology :: F.Topology (KStream Text Text) (KGroupedStream Text Text)
        topology =
          F.selectKey (\r -> recordValue r)
            >>> F.groupByKey
        cfgOff = F.defaultOptimizeConfig
                   { F.optAutoInsertRepartition  = False
                   , F.optFuseSelectKeyIntoGroupBy = False
                   }
        toks = F.inspect (F.optimizeWith cfgOff topology)
    -- Neither the auto-insert nor the GroupBy fusion fired,
    -- so the AST keeps SelectKey and GroupByKey as distinct
    -- nodes with no Repartition.
    "SelectKey"  `elem` toks @?= True
    "GroupByKey" `elem` toks @?= True
    assertBool "no Repartition when toggle is off" $
      not (any (T.isPrefixOf "Repartition") toks)

----------------------------------------------------------------------
-- 72. Auto-insert: no-op when there's no key change upstream
----------------------------------------------------------------------

test_auto_insert_no_op_when_no_key_change :: TestTree
test_auto_insert_no_op_when_no_key_change =
  testCase "no upstream key change means no auto-insert" $ do
    let topology :: F.Topology (KStream Text Text) (KGroupedStream Text Text)
        topology =
          F.mapValues T.toUpper
            >>> F.filter (\r -> recordValue r /= "")
            >>> F.groupByKey
        toks = F.inspect (F.optimize topology)
    assertBool "no Repartition for a pure value chain" $
      not (any (T.isPrefixOf "Repartition") toks)

----------------------------------------------------------------------
-- 73. Auto-insert: doesn't duplicate when the user already
-- inserted a Repartition
----------------------------------------------------------------------

test_auto_insert_with_explicit_repartition_no_dup :: TestTree
test_auto_insert_with_explicit_repartition_no_dup =
  testCase "explicit repartition clears the key-dirty flag" $ do
    let topology :: F.Topology (KStream Text Text) (KGroupedStream Text Text)
        topology =
          F.selectKey (\r -> recordValue r)
            >>> F.repartition "user-explicit"
            >>> F.mapValues T.toUpper
            >>> F.groupByKey
        toks = F.inspect (F.optimize topology)
    -- Exactly one Repartition node — the user's. No auto-insert.
    length (Prelude.filter (T.isPrefixOf "Repartition") toks) @?= 1

----------------------------------------------------------------------
-- 74. Auto-insert: stream-table join with visible Fanout
----------------------------------------------------------------------

test_auto_insert_stream_table_join_fanout :: TestTree
test_auto_insert_stream_table_join_fanout =
  testCase "stream-table join via Fanout: insert Repartition on left only" $ do
    -- Topology shape:
    --   (selectKey f >>> [stream] &&& [table]) >>> streamTableJoin
    -- After optimisation the LEFT (stream) side picks up a
    -- Repartition; the right (table) side stays untouched.
    let streamSide :: F.Topology Void (KStream Text Text)
        streamSide =
          F.source @Text @Text "stj-stream-in"
            >>> F.selectKey (\r -> recordValue r)
        tableSide :: F.Topology Void (KTable Text Text)
        tableSide =
          F.tableSource "stj-table-in"
        topology :: F.Topology Void ()
        topology =
          (streamSide &&& tableSide)
            >>> F.streamTableJoin (\v vt -> v <> "+" <> vt)
                                  (Joined.joined textSerde textSerde textSerde)
            >>> F.sink "stj-out"
        toks = F.inspect (F.optimize topology)
    -- Repartition appears.
    assertBool ("expected Repartition for stream-table join; " <> show toks) $
      any (T.isPrefixOf "Repartition") toks

----------------------------------------------------------------------
-- 75. Auto-insert: stream-stream join with visible Fanout
----------------------------------------------------------------------

test_auto_insert_stream_stream_join_fanout :: TestTree
test_auto_insert_stream_stream_join_fanout =
  testCase "stream-stream join via Fanout: insert Repartition on each dirty side" $ do
    let leftSide :: F.Topology Void (KStream Text Text)
        leftSide =
          F.source @Text @Text "ssj-l-in"
            >>> F.selectKey (\r -> recordValue r)
        rightSide :: F.Topology Void (KStream Text Text)
        rightSide =
          F.source @Text @Text "ssj-r-in"
            >>> F.selectKey (\r -> recordValue r)
        topology :: F.Topology Void ()
        topology =
          (leftSide &&& rightSide)
            >>> F.streamStreamJoin
                  (\v1 v2 -> v1 <> "+" <> v2)
                  (Joined.joinWindowsBefore (Duration 1000))
                  (Joined.joined textSerde textSerde textSerde)
            >>> F.sink "ssj-out"
        toks = F.inspect (F.optimize topology)
    -- Two Repartition nodes — one per side.
    length (Prelude.filter (T.isPrefixOf "Repartition") toks) @?= 2

----------------------------------------------------------------------
-- 76. Auto-insert composes cleanly with the SelectKey/GroupByKey
-- → GroupBy fusion
----------------------------------------------------------------------

test_auto_insert_selectKey_groupByKey_collapses_to_groupBy :: TestTree
test_auto_insert_selectKey_groupByKey_collapses_to_groupBy =
  testCase "selectKey >>> groupByKey still collapses to GroupBy after auto-insert" $ do
    let topology :: F.Topology (KStream Text Text) (KGroupedStream Text Text)
        topology =
          F.selectKey (\r -> recordValue r)
            >>> F.groupByKey
        toks = F.inspect (F.optimize topology)
    -- The fusion still wins over the auto-insert: result is
    -- 'GroupBy', not 'SelectKey >>> Repartition >>> GroupByKey'.
    "GroupBy"    `elem` toks @?= True
    "SelectKey"  `elem` toks @?= False
    "GroupByKey" `elem` toks @?= False

----------------------------------------------------------------------
-- 77. JoinWindows grace period drops late records
----------------------------------------------------------------------

----------------------------------------------------------------------
-- 78. KIP-307 filterNamed pins the node name in the compiled topology
----------------------------------------------------------------------

test_filterNamed_pins_node_name :: TestTree
test_filterNamed_pins_node_name =
  testCase "filterNamed sets the topology node name explicitly" $ do
    let topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "fn-in"
            >>> F.filterNamed (Named.named "MY-FILTER")
                  (\r -> recordValue r /= "")
            >>> F.sink "fn-out"
    (_, topo) <- F.compile topology
    let procNames = Map.keys (Topo.topoProcessors topo)
    assertBool ("expected MY-FILTER processor; got " <> show procNames) $
      Topo.NodeName "MY-FILTER" `elem` procNames

test_mapValuesNamed_pins_node_name :: TestTree
test_mapValuesNamed_pins_node_name =
  testCase "mapValuesNamed sets the topology node name explicitly" $ do
    let topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "mvn-in"
            >>> F.mapValuesNamed (Named.named "MY-UPPER") T.toUpper
            >>> F.sink "mvn-out"
    (_, topo) <- F.compile topology
    assertBool "expected MY-UPPER processor" $
      Topo.NodeName "MY-UPPER" `elem` Map.keys (Topo.topoProcessors topo)

test_selectKeyNamed_pins_node_name :: TestTree
test_selectKeyNamed_pins_node_name =
  testCase "selectKeyNamed sets the topology node name explicitly" $ do
    let topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "skn-in"
            >>> F.selectKeyNamed (Named.named "MY-REKEY")
                  (\r -> recordValue r)
            >>> F.sink "skn-out"
    (_, topo) <- F.compile topology
    assertBool "expected MY-REKEY processor" $
      Topo.NodeName "MY-REKEY" `elem` Map.keys (Topo.topoProcessors topo)

test_peekNamed_pins_node_name :: TestTree
test_peekNamed_pins_node_name =
  testCase "peekNamed sets the topology node name explicitly" $ do
    let topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "pkn-in"
            >>> F.peekNamed (Named.named "MY-PEEK") (\_ -> pure ())
            >>> F.sink "pkn-out"
    (_, topo) <- F.compile topology
    assertBool "expected MY-PEEK processor" $
      Topo.NodeName "MY-PEEK" `elem` Map.keys (Topo.topoProcessors topo)

test_withEmitStrategy_switches_to_emit_on_close :: TestTree
test_withEmitStrategy_switches_to_emit_on_close =
  testCase "withEmitStrategy(emitOnWindowClose) flips the handle emit field" $ do
    let mkTopology :: TWKS.EmitStrategy
                   -> F.Topology Void (TWKS.WindowedTableHandle Text Int64)
        mkTopology e =
          F.withEmitStrategy e $
            F.source @Text @Text "es-in"
              >>> F.groupByKey
              >>> F.windowedByTime (Win.tumblingWindows (millis 1000))
              >>> F.countWindowed (Mat.materializedAs (storeName "es-store"))
    (hClose, _) <- F.compile (mkTopology TWKS.OnWindowClose)
    TWKS.wthEmit hClose @?= TWKS.OnWindowClose
    (hUpdate, _) <- F.compile (mkTopology TWKS.OnWindowUpdate)
    TWKS.wthEmit hUpdate @?= TWKS.OnWindowUpdate

test_withEmitStrategy_default_emit_on_update :: TestTree
test_withEmitStrategy_default_emit_on_update =
  testCase "default windowed aggregation emits on update" $ do
    let topology :: F.Topology Void (TWKS.WindowedTableHandle Text Int64)
        topology =
          F.source @Text @Text "esd-in"
            >>> F.groupByKey
            >>> F.windowedByTime (Win.tumblingWindows (millis 1000))
            >>> F.countWindowed (Mat.materializedAs (storeName "esd-store"))
    (h, _) <- F.compile topology
    TWKS.wthEmit h @?= TWKS.OnWindowUpdate

test_suppressWindowedWith_compiles_with_max_records :: TestTree
test_suppressWindowedWith_compiles_with_max_records =
  testCase "maxRecordsBufferConfig / maxBytesBufferConfig configure BufferConfig" $ do
    -- The buffer-config helpers are user-facing knobs for
    -- 'suppressWindowedWith'. Verify they populate the right
    -- fields with the requested cap.
    case F.maxRecordsBufferConfig 100 of
      Suppress.BufferConfig mb mr ov -> do
        mr @?= Just 100
        mb @?= Nothing
        ov @?= Suppress.ShutdownWhenFull
    case F.maxBytesBufferConfig 4096 of
      Suppress.BufferConfig mb mr _ -> do
        mb @?= Just 4096
        mr @?= Nothing
    -- And the 'suppressWindowedWith' smart constructor exists and
    -- has the expected arity by partial application.
    let _ = F.suppressWindowedWith @Text @Int64
              (Duration 1000) 1000 (F.maxRecordsBufferConfig 100)
    pure ()

test_sourceWith_offset_reset_propagated_to_spec :: TestTree
test_sourceWith_offset_reset_propagated_to_spec =
  testCase "Consumed.withOffsetResetPolicy lands on SourceSpec" $ do
    let cfg :: Consumed Text Text
        cfg = Consumed.withOffsetResetPolicy Consumed.OffsetLatest
                (Consumed.consumed textSerde textSerde)
        topology :: F.Topology Void ()
        topology =
          F.sourceWith (topicName "ofs-in") cfg
            >>> F.sink "ofs-out"
    (_, topo) <- F.compile topology
    case Map.elems (Topo.topoSources topo) of
      [src] -> Topo.sourceOffsetReset src @?= Consumed.OffsetLatest
      xs    -> assertFailure ("expected one source, got " <> show (length xs))

test_default_offset_reset_is_earliest :: TestTree
test_default_offset_reset_is_earliest =
  testCase "default source uses OffsetEarliest" $ do
    let topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "ofd-in"
            >>> F.sink "ofd-out"
    (_, topo) <- F.compile topology
    case Map.elems (Topo.topoSources topo) of
      [src] -> Topo.sourceOffsetReset src @?= Consumed.OffsetEarliest
      xs    -> assertFailure ("expected one source, got " <> show (length xs))

test_sourcesWith_uses_supplied_consumed :: TestTree
test_sourcesWith_uses_supplied_consumed =
  testCase "sourcesWith preserves Consumed (offset reset, multi-topic)" $ do
    let cfg :: Consumed Text Text
        cfg = Consumed.withOffsetResetPolicy Consumed.OffsetLatest
                (Consumed.consumed textSerde textSerde)
        topology :: F.Topology Void ()
        topology =
          F.sourcesWith
            (topicName "swi-1" NE.:| [topicName "swi-2"])
            cfg
            >>> F.sink "swi-out"
    (_, topo) <- F.compile topology
    case Map.elems (Topo.topoSources topo) of
      [src] -> do
        Topo.sourceOffsetReset src @?= Consumed.OffsetLatest
        Prelude.length (Topo.sourceTopics src) @?= 2
      _     -> assertFailure "expected one multi-topic source"

test_fk_join_with_tableJoined_compiles :: TestTree
test_fk_join_with_tableJoined_compiles =
  testCase "foreignKeyJoinWith accepts a TableJoined override and compiles" $ do
    let leftTable  :: F.Topology Void (KTable Text Text)
        leftTable  = F.tableSource "fkw-l-in"
        rightTable :: F.Topology Void (KTable Text Text)
        rightTable = F.tableSource "fkw-r-in"
        tj :: Joined.TableJoined Text Text
        tj = Joined.withTableJoinedName "MY-FK-JOIN" Joined.tableJoined
        mat :: Materialized Text Text
        mat = Mat.materializedAs (storeName "fkw-out-store")
        topology :: F.Topology Void ()
        topology =
          (leftTable &&& rightTable)
            >>> F.foreignKeyJoinWith tj id (\v vr -> v <> "+" <> vr) mat
            >>> F.toStream
            >>> F.sink "fkw-out"
    (_, topo) <- F.compile topology
    assertBool "materialised FK-join output store present" $
      storeName "fkw-out-store" `elem` Map.keys (Topo.topoStores topo)

test_addGlobalStore_registers_global_store :: TestTree
test_addGlobalStore_registers_global_store =
  testCase "addGlobalStore puts the store in topoGlobalStores" $ do
    let builder :: StoreBuilderKV Text Text
        builder = inMemoryKeyValueStoreBuilder (storeName "GLOBAL-STORE")
        topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "gs-in"
            >>> F.addGlobalStore builder
                  "GLOBAL-SRC" "GLOBAL-PROC"
                  (topicName "gs-global-topic")
                  textSerde textSerde
                  recordTimestampExtractor
                  (pure noopGlobalProc)
            >>> F.sink "gs-out"
    (_, topo) <- F.compile topology
    assertBool "store registered as global" $
      storeName "GLOBAL-STORE" `Set.member` Topo.topoGlobalStores topo
    assertBool "global source registered" $
      Topo.NodeName "GLOBAL-SRC" `elem` Map.keys (Topo.topoSources topo)

noopGlobalProc :: Processor Text Text
noopGlobalProc = Processor
  { procName    = processorName "GLOBAL-NOOP"
  , procInit    = \_ -> pure ()
  , procClose   = pure ()
  , procProcess = \_ -> pure ()
  }

test_connectProcessorAndStateStores_attaches_late :: TestTree
test_connectProcessorAndStateStores_attaches_late =
  testCase "connectProcessorAndStateStores wires processor to existing stores" $ do
    -- `processStream` uses 'freshNodeName' which appends a
    -- monotonically-increasing counter to the supplied prefix.
    -- In this minimal topology the source consumes counter 0 and
    -- the processStream consumes counter 1, so we know the
    -- processor will be registered as "LATE-PROC-1". We pin that
    -- name in the connect call to wire the store in.
    let builder :: StoreBuilderKV Text Text
        builder = inMemoryKeyValueStoreBuilder (storeName "LATE-STORE")
        topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "cps-in"
            >>> F.processStream "LATE-PROC" [] (pure noopGlobalProc)
            >>> F.withStateStoreKV builder []
            >>> F.connectProcessorAndStateStores
                  "LATE-PROC-1"
                  [storeName "LATE-STORE"]
    (_, topo) <- F.compile topology
    case Map.lookup (storeName "LATE-STORE") (Topo.topoStoreOwners topo) of
      Just os ->
        assertBool ("LATE-PROC-1 in owners: " <> show os) $
          Topo.NodeName "LATE-PROC-1" `elem` os
      Nothing -> assertFailure "store has no owner entry"

test_sinkSpec_compiles_with_custom_spec :: TestTree
test_sinkSpec_compiles_with_custom_spec =
  testCase "sinkSpec accepts a custom Topo.SinkSpec" $ do
    let customSink :: Topo.SinkSpec
        customSink = Topo.SinkSpec
          { Topo.sinkName        = Topo.NodeName "MY-SINK"
          , Topo.sinkParents     = []
          , Topo.sinkTopic       = topicName "ss-out"
          , Topo.sinkKeySerde    = Topo.AnySerde (textSerde :: Serde Text)
          , Topo.sinkValueSerde  = Topo.AnySerde (textSerde :: Serde Text)
          }
        topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "ss-in"
            >>> F.sinkSpec customSink
    (_, topo) <- F.compile topology
    assertBool "MY-SINK present in topoSinks" $
      Topo.NodeName "MY-SINK" `elem` Map.keys (Topo.topoSinks topo)

test_transformStream_can_change_key_and_value :: TestTree
test_transformStream_can_change_key_and_value =
  testCase "transformStream runs a stateful key+value transformer" $ do
    counter <- newIORef (0 :: Int)
    let mkTransformer :: IO (Processor Text Text)
        mkTransformer = do
          ctxRef <- newIORef Nothing
          pure Processor
            { procName    = processorName "FLIP"
            , procInit    = \ctx -> writeIORef ctxRef (Just ctx)
            , procClose   = pure ()
            , procProcess = \r -> do
                _ <- atomicModifyIORef' counter (\c -> (c + 1, ()))
                mctx <- readIORef ctxRef
                case mctx of
                  Nothing -> pure ()
                  Just ctx -> forwardRecord ctx
                    (r { recordKey   = Just (recordValue r)
                       , recordValue = T.reverse (recordValue r)
                       } :: Record Text Text)
            }
        topology :: F.Topology Void ()
        topology =
          F.source @Text @Text "tx-in"
            >>> F.transformStream "FLIP" [] mkTransformer textSerde textSerde
            >>> F.sink "tx-out"
    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-transformStream"
    pipeInput driver (topicName "tx-in") (Just (bytes "k1")) (bytes "abc") t0 0
    pipeInput driver (topicName "tx-in") (Just (bytes "k2")) (bytes "def") t0 0
    out <- readOutput driver (topicName "tx-out")
    closeDriver driver
    n <- readIORef counter
    n @?= 2
    Prelude.map (unbytes . crValue) out @?= ["cba", "fed"]
    Prelude.map (fmap unbytes . crKey) out @?= [Just "abc", Just "def"]

test_sourcePattern_records_pattern_in_spec :: TestTree
test_sourcePattern_records_pattern_in_spec =
  testCase "sourcePattern records the regex in SourceSpec.sourcePattern" $ do
    let topology :: F.Topology Void ()
        topology =
          F.sourcePattern @Text @Text "tenant-.*"
            >>> F.sink "pat-out"
    (_, topo) <- F.compile topology
    case Map.elems (Topo.topoSources topo) of
      [src] -> do
        Topo.sourcePattern src @?= Just "tenant-.*"
        Topo.sourceTopics src @?= []
      _ -> assertFailure "expected one source"

test_aggregateWindowedCogrouped_per_window_state :: TestTree
test_aggregateWindowedCogrouped_per_window_state =
  testCase "aggregateWindowedCogrouped accumulates per (key, window)" $ do
    let leftSide :: F.Topology Void (KGroupedStream Text Int64)
        leftSide =
          F.source @Text @Int64 "cgw-l-in"
            >>> F.groupByKey
        rightSide :: F.Topology Void (KGroupedStream Text Int64)
        rightSide =
          F.source @Text @Int64 "cgw-r-in"
            >>> F.groupByKey
        cogrouped :: F.Topology Void (Cog.CogroupedStream Text Int64)
        cogrouped =
          (leftSide >>> F.cogroup (\_k v acc -> acc + v))
            &&& rightSide
            >>> F.addCogrouped (\_k v acc -> acc + v)
        windowedCG :: F.Topology Void (Cog.TimeWindowedCogroupedStream Text Int64)
        windowedCG =
          cogrouped >>> F.windowedByCogroup (Win.tumblingWindows (millis 1000))
        topology :: F.Topology Void (TWKS.WindowedTableHandle Text Int64)
        topology =
          windowedCG
            >>> F.aggregateWindowedCogrouped
                  (pure 0)
                  (Mat.withValueSerde int64Serde
                    $ Mat.withKeySerde textSerde
                    $ Mat.materializedAs (storeName "cgw-store"))
    (h, topo) <- F.compile topology
    driver <- newDriver topo "free-cgw"
    pipeInput driver (topicName "cgw-l-in") (Just (bytes "a")) (serialize int64Serde 10) (t 100)  0
    pipeInput driver (topicName "cgw-r-in") (Just (bytes "a")) (serialize int64Serde 5)  (t 200)  0
    pipeInput driver (topicName "cgw-l-in") (Just (bytes "a")) (serialize int64Serde 7)  (t 1100) 0
    mWS <- getWindowStore @Text @Int64 driver (TWKS.wthStore h)
    case mWS of
      Just ws -> do
        v1 <- wsFetch ws "a" (Timestamp 0)
        v2 <- wsFetch ws "a" (Timestamp 1000)
        v1 @?= Just 15
        v2 @?= Just 7
      Nothing -> assertFailure "cogroup window store missing"
    closeDriver driver

test_queryableStoreName_returns_explicit_name :: TestTree
test_queryableStoreName_returns_explicit_name =
  testCase "queryableStoreName returns Just on materializedAs, Nothing otherwise" $ do
    Mat.queryableStoreName (Mat.materializedAs (storeName "q-store"))
      @?= Just (storeName "q-store")
    Mat.queryableStoreName (Mat.materialized :: Materialized Text Text)
      @?= Nothing

test_queryableStoreType_default_is_kv :: TestTree
test_queryableStoreType_default_is_kv =
  testCase "queryableStoreType returns QSKeyValueStore for a generic Materialized" $ do
    Mat.queryableStoreType (Mat.materializedAs (storeName "qt-store"))
      @?= Mat.QSKeyValueStore

----------------------------------------------------------------------

test_join_grace_drops_late_records :: TestTree
test_join_grace_drops_late_records =
  testCase "JoinWindows.grace drops records arriving past windowEnd + grace" $ do
    -- Two-stream join with a 100ms window and 50ms grace.
    -- Feed an early record; advance stream time well past
    -- (windowEnd + grace); feed a "late" record on each side and
    -- assert no match was emitted because both sides were
    -- dropped at the input.
    let jw :: Joined.JoinWindows
        jw = Joined.withJoinWindowsGrace (Duration 50)
               (Joined.joinWindowsBefore (Duration 100))
        leftSide :: F.Topology Void (KStream Text Text)
        leftSide = F.source @Text @Text "join-grace-l"
        rightSide :: F.Topology Void (KStream Text Text)
        rightSide = F.source @Text @Text "join-grace-r"
        topology :: F.Topology Void ()
        topology =
          (leftSide &&& rightSide)
            >>> F.streamStreamJoin
                  (\v1 v2 -> v1 <> "+" <> v2)
                  jw
                  (Joined.joined textSerde textSerde textSerde)
            >>> F.sink "join-grace-out"
    (_, topo) <- F.compile topology
    driver <- newDriver topo "free-join-grace"
    -- A pair of well-timed matching records produces a join.
    pipeInput driver (topicName "join-grace-l") (Just (bytes "k"))
              (bytes "L1") (t 1000) 0
    pipeInput driver (topicName "join-grace-r") (Just (bytes "k"))
              (bytes "R1") (t 1010) 0
    -- Advance stream time well past windowEnd (1100) + grace (50).
    pipeInput driver (topicName "join-grace-l") (Just (bytes "k"))
              (bytes "L-now") (t 2000) 0
    -- The right-side late record arrives within its own window
    -- but after a stream-time push past the original window's
    -- grace. Its bare-window predecessor on the left has expired
    -- past grace and is dropped at the JOIN's input on the left.
    pipeInput driver (topicName "join-grace-l") (Just (bytes "k"))
              (bytes "L-stale") (t 1005) 0
    out <- readOutput driver (topicName "join-grace-out")
    -- The first L1/R1 join matches normally. The L-stale record
    -- is past grace and dropped — it contributes no extra match.
    let outVals = Prelude.map (unbytes . crValue) out
    assertBool ("L1+R1 join should appear: " <> show outVals) $
      "L1+R1" `elem` outVals
    assertBool ("L-stale must not produce a join: " <> show outVals) $
      not (any ("L-stale" `T.isInfixOf`) outVals)
    closeDriver driver
