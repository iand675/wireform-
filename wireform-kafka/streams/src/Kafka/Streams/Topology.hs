{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Kafka.Streams.Topology
Description : Low-level Topology builder

Mirrors @org.apache.kafka.streams.Topology@:

@
Topology t =
    emptyTopology
  & addSource    "src"  ["my-topic"] keySerde valSerde extractor
  & addProcessor "p1"   parents       processorSupplier
  & addStateStore       storeBuilder ["p1"]
  & addSink      "snk"  "out-topic"   keySerde valSerde ["p1"]
@

The topology is type-erased internally so we can keep all sources /
processors / sinks in a single map. Type information is preserved
at the edges (source serdes, sink serdes) and at the processor
supplier (which is given a typed 'ProcessorContext' inside the
runtime).

Validation:

  * names are unique across nodes
  * every parent reference resolves to a node
  * sources have at least one topic
  * the graph is acyclic
  * stores referenced by 'addStateStore' exist
-}
module Kafka.Streams.Topology (
  -- * Names
  NodeName (..),
  nodeName,
  unNodeName,

  -- * Topology
  Topology (..),
  emptyTopology,
  InternalTopicSpec (..),
  addInternalTopic,
  topoInternalTopics,
  addSource,
  addTableSource,
  addSourceWith,
  addProcessor,
  addProcessorWith,
  addSink,
  addSinkWith,
  addStateStore,
  addStateStoreKV,
  addStateStoreW,
  addStateStoreS,
  addGlobalStore,
  markGlobalStore,
  topoGlobalStores,
  topoChangelogPlan,
  connectProcessorAndStateStores,

  -- * Optimisations (KIP-295)
  OptimizationConfig (..),
  defaultOptimizationConfig,
  noOptimisations,
  fromOptimizationFlags,
  optimizeTopology,
  effectiveChangelogReuse,
  storeBuilderLogging,
  storeBuilderHasSerdes,

  -- * Validation
  validateTopology,
  validateChangelogPlan,
  TopologyValid,
  unsafeAssumeValid,
  topologyNodes,

  -- * Internals (used by the runtime / driver)
  SourceSpec (..),
  ProcessorSpec (..),
  SinkSpec (..),
  AnyProcessor (..),
  AnySerde (..),
  AnyStreamPartitioner (..),
  addSinkPartitioned,
  AnyTimestampExtractor (..),
  AnyStoreBuilder (..),
  storeBuilderName,
  topoSources,
  topoProcessors,
  topoSinks,
  topoStores,
  topoStoreOwners,
  topoSourceOrder,
  topoOrder,
  topologyValidGraph,
  parentsOf,
  childrenOf,

  -- * Topology-level errors
  TopologyError (..),
  ChangelogPlanProblem (..),
) where

import Control.Exception (Exception, throwIO)
import Data.Char qualified as Char
import Data.Foldable qualified as Foldable
import Data.List (foldl', nub)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Kafka.Streams.Consumed qualified as Consumed
import Kafka.Streams.Processor (
  Processor,
  ProcessorName (..),
 )
import Kafka.Streams.Produced (StreamPartitioner)
import Kafka.Streams.Serde (Serde)
import Kafka.Streams.State.Store (
  LoggingConfig (..),
  StoreBuilder (..),
  StoreBuilderKV (..),
  StoreBuilderS (..),
  StoreBuilderW (..),
  StoreName,
  unStoreName,
 )
import Kafka.Streams.State.Store qualified
import Kafka.Streams.Time (TimestampExtractor)
import Kafka.Streams.Time qualified
import Kafka.Streams.Topology.Optimization qualified
import Kafka.Streams.Types (NodeName (..), TopicName, nodeName, unNodeName)
import Kafka.Streams.Watermark qualified as Watermark


-- 'NodeName' is shared with "Kafka.Streams.Processor" so re-exported
-- from "Kafka.Streams.Types" to break the import cycle.

-- | Type-erased serde used in the topology AST.
data AnySerde where
  AnySerde :: !(Serde a) -> AnySerde


{- | Type-erased 'StreamPartitioner' carried by a 'SinkSpec'. Its
@k@ / @v@ match the sink's key / value serdes; the engine coerces
it back the same way it does the serdes.
-}
data AnyStreamPartitioner where
  AnyStreamPartitioner :: !(StreamPartitioner k v) -> AnyStreamPartitioner


-- | Type-erased timestamp extractor.
data AnyTimestampExtractor where
  AnyTimestampExtractor :: !(TimestampExtractor k v) -> AnyTimestampExtractor


{- | Type-erased processor supplier. The runtime calls 'apsBuild'
once per task to construct an instance.
-}
data AnyProcessor where
  AnyProcessor
    :: !(IO (Processor k v))
    -> AnyProcessor


{- | Type-erased store builder. Keeps the original typed builder
around so the DSL can still use it after retrieval.
-}
data AnyStoreBuilder where
  AsKeyValueBuilder :: !(StoreBuilderKV k v) -> AnyStoreBuilder
  AsWindowBuilder :: !(StoreBuilderW k v) -> AnyStoreBuilder
  AsSessionBuilder :: !(StoreBuilderS k v) -> AnyStoreBuilder
  AsRawBuilder :: !StoreBuilder -> AnyStoreBuilder


storeBuilderName :: AnyStoreBuilder -> StoreName
storeBuilderName = \case
  AsKeyValueBuilder b -> sbKvName b
  AsWindowBuilder b -> sbWName b
  AsSessionBuilder b -> sbSName b
  AsRawBuilder b -> sbName b


-- | Source node specification.
data SourceSpec = SourceSpec
  { sourceName :: !NodeName
  , sourceTopics :: ![TopicName]
  , sourceKeySerde :: !AnySerde
  , sourceValueSerde :: !AnySerde
  , sourceExtractor :: !AnyTimestampExtractor
  , sourceOffsetReset :: !Consumed.AutoOffsetReset
  {- ^ Auto-offset-reset policy. Mirrors Java's
  @Consumed.withOffsetResetPolicy@. The default 'addSource'
  entry point initialises this to 'OffsetEarliest';
  'addSourceWith' lets callers override.
  -}
  , sourcePattern :: !(Maybe Text)
  {- ^ Optional regex pattern. When 'Just', the source
  subscribes to every broker topic matching the regex
  (JVM @StreamsBuilder.stream(Pattern)@). 'sourceTopics' is
  treated as informational when a pattern is set — the
  runtime resolves topics at subscription time. 'Nothing'
  means "subscribe to the explicit 'sourceTopics' list",
  which is the only mode the in-process driver supports
  today.
  -}
  , sourceNullValues :: !Bool
  {- ^ Set on a table source: the node forwards @Maybe v@, with
  'Nothing' for a null value (a tombstone), so the table can delete
  the key. A plain stream source drops null-valued records (they have
  no @v@) and counts them as dropped.
  -}
  , sourceWatermarkStrategy :: !(Maybe Watermark.WatermarkStrategy)
  {- ^ Riffle \xc2\xa75: optional per-source watermark strategy.
  'Nothing' (the default) preserves the legacy per-task
  'StreamTime' behaviour exactly. 'Just s' opts the source
  into the cross-source watermark coordinator: the runtime
  registers the source with the coordinator at startup
  and reports every record's timestamp via
  'Kafka.Streams.Watermark.reportRecord'.
  -}
  }


-- | Processor node specification.
data ProcessorSpec = ProcessorSpec
  { processorSpecName :: !NodeName
  , processorSpecParents :: ![NodeName]
  , processorSpecSupplier :: !AnyProcessor
  , processorSpecStores :: ![StoreName]
  }


-- | Sink node specification.
data SinkSpec = SinkSpec
  { sinkName :: !NodeName
  , sinkParents :: ![NodeName]
  , sinkTopic :: !TopicName
  , sinkKeySerde :: !AnySerde
  , sinkValueSerde :: !AnySerde
  , sinkPartitioner :: !(Maybe AnyStreamPartitioner)
  {- ^ Per-record partition choice (@Produced.withStreamPartitioner@).
  'Nothing' leaves the partition to the producer's partitioner.
  -}
  }


{- | The graph itself. Maps are by node name; insertion order is kept
on the side as 'topoOrder' so the test driver / runtime can boot
nodes deterministically.
-}
data Topology = Topology
  { topoSources :: !(Map NodeName SourceSpec)
  , topoProcessors :: !(Map NodeName ProcessorSpec)
  , topoSinks :: !(Map NodeName SinkSpec)
  , topoStores :: !(Map StoreName AnyStoreBuilder)
  , topoStoreOwners :: !(Map StoreName [NodeName])
  , topoOrder :: !(Seq NodeName)
  -- ^ Insertion order across every source / processor / sink.
  , topoSourceOrder :: !(Seq NodeName)
  -- ^ Insertion order across just the sources.
  , topoChildrenIndex :: !(Map NodeName [NodeName])
  , topoGlobalStores :: !(Set StoreName)
  {- ^ Stores registered via 'addGlobalStore'. The runtime treats
  these as cluster-wide replicas and bypasses partition
  assignment for their source topics.
  -}
  , topoInternalTopics :: !(Map TopicName InternalTopicSpec)
  {- ^ Topics the topology owns (repartition topics), keyed by their
  /unprefixed/ name. The broker runtime creates them, names them
  @\<application.id\>-\<name\>@ ('prefixInternalTopics') and
  subscribes to them like any source.
  -}
  , topoChangelogPlan :: !(Map StoreName TopicName)
  {- ^ /Optimiser-derived/ KIP-295 @REUSE_KTABLE_SOURCE_TOPICS@
  decisions. Each entry @(store, topic)@ says \"the runtime
  should reuse @topic@ as the changelog for @store@ instead
  of creating a separate internal one\".

  This map is /wiped and re-derived/ on every call to
  'optimizeTopology' (with the toggle enabled), so it always
  reflects the current topology shape. Subsequent
  modifications via 'addSink' / 'addProcessor' / 'addSource' /
  'addStateStore*' invalidate prior optimisation decisions;
  re-running 'optimizeTopology' restores correctness.

  Distinct from 'Kafka.Streams.State.Store.loggingSourceTopic',
  which is the /user-explicit/ declaration set via
  'Kafka.Streams.State.Store.withSourceTopicChangelogKV'. The
  optimiser never touches that field; it's preserved across
  optimisation runs. Use 'effectiveChangelogReuse' to look up
  the combined effective changelog target for a store.
  -}
  }


emptyTopology :: Topology
emptyTopology =
  Topology
    { topoSources = Map.empty
    , topoProcessors = Map.empty
    , topoSinks = Map.empty
    , topoStores = Map.empty
    , topoStoreOwners = Map.empty
    , topoOrder = Seq.empty
    , topoSourceOrder = Seq.empty
    , topoChildrenIndex = Map.empty
    , topoGlobalStores = Set.empty
    , topoInternalTopics = Map.empty
    , topoChangelogPlan = Map.empty
    }


{- | A topic the topology itself owns. A repartition topic gets
'itsPartitions' partitions, or (when 'Nothing') as many as the
widest source topic of the subtopology that writes to it.
-}
data InternalTopicSpec = InternalTopicSpec
  { itsPartitions :: !(Maybe Int)
  }
  deriving stock (Eq, Show)


{- | Declare a repartition topic. Pair it with a sink writing to it
and a source reading from it (see
'Kafka.Streams.KStream.repartitionWith').
-}
addInternalTopic :: TopicName -> InternalTopicSpec -> Topology -> Topology
addInternalTopic tp spec t =
  t {topoInternalTopics = Map.insert tp spec (topoInternalTopics t)}


-- | Distinct error categories surfaced by 'validateTopology'.
data TopologyError
  = NodeNameTaken !NodeName
  | UnknownParent !NodeName !NodeName
  | UnknownStore !StoreName !NodeName
  | StoreAlreadyAdded !StoreName
  | EmptySourceTopics !NodeName
  | TopologyCycle ![NodeName]
  | NoSources
  | {- | 'topoChangelogPlan' or 'loggingSourceTopic' claims @store@
    can reuse @topic@ as its changelog, but the current graph
    doesn't actually support that. Typically caused by
    post-compile modifications (a new sink on the topic, a
    new co-owner of the store) that the optimiser hasn't
    been re-run over. Calling
    @'optimizeTopology' 'defaultOptimizationConfig'@ will
    repair the plan; alternatively, the caller can update the
    topology to restore the invariant.
    -}
    StaleChangelogPlan !StoreName !TopicName !ChangelogPlanProblem
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Exception)


{- | Why a changelog-plan entry is no longer valid against the
current graph state.
-}
data ChangelogPlanProblem
  = -- | The store has been removed from 'topoStores'.
    StaleNoSuchStore
  | -- | The store has no owner processor.
    StaleNoOwners
  | {- | The store now has more than one owner processor — the
    changelog source can no longer be uniquely attributed.
    -}
    StaleMultipleOwners
  | {- | The owner processor now has more than one parent — the
    changelog source is ambiguous.
    -}
    StaleOwnerHasMultipleParents
  | {- | The owner's parent is no longer a 'SourceSpec' (perhaps
    it was replaced by a processor or removed).
    -}
    StaleParentNotASource
  | -- | The source now subscribes to more than one topic.
    StaleSourceMultipleTopics
  | {- | The source subscribes to a different topic than the plan
    claims; the actual topic is supplied.
    -}
    StaleWrongTopic !TopicName
  | {- | Another sink now writes to the source topic, so it isn't
    a clean source-only changelog target anymore.
    -}
    StaleTopicHasSink
  | -- | The store's logging was turned off; reuse is moot.
    StaleLoggingDisabled
  deriving stock (Eq, Show, Generic)


----------------------------------------------------------------------
-- Builders
----------------------------------------------------------------------

{- | Add a source. Subscribes to each named topic; records flow through
the supplied serdes and timestamp extractor.
-}
addSource
  :: NodeName
  -> [TopicName]
  -> Serde k
  -> Serde v
  -> TimestampExtractor k v
  -> Topology
  -> Topology
addSource nm ts ks vs ex t =
  ensureNameFree t nm $
    insertSource
      t
      SourceSpec
        { sourceName = nm
        , sourceTopics = ts
        , sourceKeySerde = AnySerde ks
        , sourceValueSerde = AnySerde vs
        , sourceExtractor = AnyTimestampExtractor ex
        , sourceOffsetReset = Consumed.OffsetEarliest
        , sourcePattern = Nothing
        , sourceNullValues = False
        , sourceWatermarkStrategy = Nothing
        }


{- | A source for a table: like 'addSource', but the node forwards
@Maybe v@ so a null value (a tombstone) reaches the table as
'Nothing'. The extractor only sees non-null values; a tombstone keeps
the record's own timestamp.
-}
addTableSource
  :: NodeName
  -> [TopicName]
  -> Serde k
  -> Serde v
  -> TimestampExtractor k v
  -> Topology
  -> Topology
addTableSource nm ts ks vs ex t =
  ensureNameFree t nm $
    insertSource
      t
      SourceSpec
        { sourceName = nm
        , sourceTopics = ts
        , sourceKeySerde = AnySerde ks
        , sourceValueSerde = AnySerde vs
        , sourceExtractor = AnyTimestampExtractor ex
        , sourceOffsetReset = Consumed.OffsetEarliest
        , sourcePattern = Nothing
        , sourceNullValues = True
        , sourceWatermarkStrategy = Nothing
        }


{- | 'addSource' that lets the caller install a custom 'TopologyError'
via 'TopologyException'. Useful from inside the DSL where richer
error context is available.
-}
addSourceWith :: SourceSpec -> Topology -> Topology
addSourceWith spec t = ensureNameFree t (sourceName spec) (insertSource t spec)


insertSource :: Topology -> SourceSpec -> Topology
insertSource t spec =
  t
    { topoSources = Map.insert (sourceName spec) spec (topoSources t)
    , topoOrder = topoOrder t |> sourceName spec
    , topoSourceOrder = topoSourceOrder t |> sourceName spec
    }


{- | Add a processor with a list of named parents. Parents must
already exist (sources or other processors) — this is checked at
build time but only enforced strictly by 'validateTopology'.
-}
addProcessor
  :: NodeName
  -> [NodeName]
  -> IO (Processor k v)
  -> Topology
  -> Topology
addProcessor nm parents supplier =
  addProcessorWith
    ProcessorSpec
      { processorSpecName = nm
      , processorSpecParents = parents
      , processorSpecSupplier = AnyProcessor supplier
      , processorSpecStores = []
      }


-- | 'addProcessor' that takes an already-built 'ProcessorSpec'.
addProcessorWith :: ProcessorSpec -> Topology -> Topology
addProcessorWith spec t =
  ensureNameFree t (processorSpecName spec) $
    let !t' =
          t
            { topoProcessors =
                Map.insert
                  (processorSpecName spec)
                  spec
                  (topoProcessors t)
            , topoOrder = topoOrder t |> processorSpecName spec
            , topoChildrenIndex =
                foldl'
                  ( \acc p ->
                      Map.insertWith (++) p [processorSpecName spec] acc
                  )
                  (topoChildrenIndex t)
                  (processorSpecParents spec)
            }
    in t'


{- | Add a sink whose values come from one or more named parents. The
runtime serialises records using the provided serdes and emits to
the named topic.
-}
addSink
  :: NodeName
  -> TopicName
  -> Serde k
  -> Serde v
  -> [NodeName]
  -> Topology
  -> Topology
addSink nm tp ks vs parents =
  addSinkWith
    SinkSpec
      { sinkName = nm
      , sinkParents = parents
      , sinkTopic = tp
      , sinkKeySerde = AnySerde ks
      , sinkValueSerde = AnySerde vs
      , sinkPartitioner = Nothing
      }


{- | 'addSink' with a 'StreamPartitioner' that picks each record's
partition (given the topic's partition count from producer
metadata). A partitioner answering 'Nothing' for a record defers
to the producer's partitioner for that record.
-}
addSinkPartitioned
  :: NodeName
  -> TopicName
  -> Serde k
  -> Serde v
  -> StreamPartitioner k v
  -> [NodeName]
  -> Topology
  -> Topology
addSinkPartitioned nm tp ks vs sp parents =
  addSinkWith
    SinkSpec
      { sinkName = nm
      , sinkParents = parents
      , sinkTopic = tp
      , sinkKeySerde = AnySerde ks
      , sinkValueSerde = AnySerde vs
      , sinkPartitioner = Just (AnyStreamPartitioner sp)
      }


addSinkWith :: SinkSpec -> Topology -> Topology
addSinkWith spec t =
  ensureNameFree t (sinkName spec) $
    let !t' =
          t
            { topoSinks = Map.insert (sinkName spec) spec (topoSinks t)
            , topoOrder = topoOrder t |> sinkName spec
            , topoChildrenIndex =
                foldl'
                  (\acc p -> Map.insertWith (++) p [sinkName spec] acc)
                  (topoChildrenIndex t)
                  (sinkParents spec)
            }
    in t'


{- | Attach a generic store builder. Each processor in @owners@ will
have access to the store via 'getStore'.
-}
addStateStore :: StoreBuilder -> [NodeName] -> Topology -> Topology
addStateStore b owners =
  addStoreInternal (sbName b) (AsRawBuilder b) owners


{- | Typed key-value store variant. Equivalent to:

@
addStateStoreKV b ["p1"] = addStateStore (rawify b) ["p1"]
@
-}
addStateStoreKV
  :: StoreBuilderKV k v -> [NodeName] -> Topology -> Topology
addStateStoreKV b owners =
  addStoreInternal (sbKvName b) (AsKeyValueBuilder b) owners


addStateStoreW
  :: StoreBuilderW k v -> [NodeName] -> Topology -> Topology
addStateStoreW b owners =
  addStoreInternal (sbWName b) (AsWindowBuilder b) owners


addStateStoreS
  :: StoreBuilderS k v -> [NodeName] -> Topology -> Topology
addStateStoreS b owners =
  addStoreInternal (sbSName b) (AsSessionBuilder b) owners


{- | Configuration for 'optimizeTopology'. Each toggle enables one
of the KIP-295 optimisations.
-}
data OptimizationConfig = OptimizationConfig
  { optMergeRepartitionTopics :: !Bool
  {- ^ When two consecutive selectKey/groupBy operations would
  each materialise a repartition topic, merge them into one.
  Currently a no-op (the DSL doesn't auto-insert repartition
  topics yet); the field exists so callers can opt in once
  the DSL change lands.
  -}
  , optReuseSourceKTable :: !Bool
  {- ^ When a KTable's only consumer is a join, reuse the source
  topic directly instead of materialising. Currently a no-op
  (most KTables in our DSL are explicitly materialised).
  -}
  }
  deriving stock (Eq, Show)


defaultOptimizationConfig :: OptimizationConfig
defaultOptimizationConfig =
  OptimizationConfig
    { optMergeRepartitionTopics = True
    , optReuseSourceKTable = True
    }


{- | Every toggle disabled — 'optimizeTopology' becomes the
identity. Useful for golden-file tests and for callers that
want to inspect the un-rewritten 'Topology' shape.
-}
noOptimisations :: OptimizationConfig
noOptimisations =
  OptimizationConfig
    { optMergeRepartitionTopics = False
    , optReuseSourceKTable = False
    }


{- | Lift a JVM-style 'OptimizationFlags' (from
"Kafka.Streams.Topology.Optimization") into the local
'OptimizationConfig'. The third Java flag
(@flagSingleStoreSelfJoin@) currently has no Haskell-side
rewrite and is ignored.
-}
fromOptimizationFlags
  :: Kafka.Streams.Topology.Optimization.OptimizationFlags
  -> OptimizationConfig
fromOptimizationFlags f =
  OptimizationConfig
    { optMergeRepartitionTopics =
        Kafka.Streams.Topology.Optimization.flagMergeRepartitionTopics f
    , optReuseSourceKTable =
        Kafka.Streams.Topology.Optimization.flagReuseSourceTopics f
    }


{- | Mirrors @Topology.optimize(StreamsConfig)@. Applies the
requested KIP-295 optimisations and returns a (possibly)
rewritten topology.

The two rewrites are:

['optReuseSourceKTable' — @REUSE_KTABLE_SOURCE_TOPICS@]
  For every processor that has exactly one parent which is a
  single-topic 'SourceSpec' /and/ owns exactly one state store
  whose logging is enabled, mark the store's 'LoggingConfig' to
  reuse the source topic as its changelog (via
  'loggingSourceTopic'). This eliminates the separate
  @\<application-id\>-\<store-name\>-changelog@ topic the broker
  would otherwise create — the source topic itself is the
  compacted log we'd replay on restore.

  Skipped when:

    * The source has multiple topics (no single topic to reuse).
    * The processor has multiple parents (the store doesn't have
      a unique source).
    * The store is shared across multiple processors (it isn't
      owned by this lineage).
    * Logging is already disabled (no changelog to reuse).
    * Another sink in the topology writes to the source topic
      (it's not actually source-only).
    * The store is a global store (its source is already its
      changelog by construction).

['optMergeRepartitionTopics' — @MERGE_REPARTITION_TOPICS@]
  Sibling pass-through repartition processors (named
  @KSTREAM-REPARTITION-\<prefix\>-…@) that share the same
  single parent /and/ the same repartition prefix are merged
  into one. Every downstream consumer is rewired to the surviving
  node, the duplicate nodes are removed, and 'topoChildrenIndex'
  is rebuilt. The result: one shared repartition processor
  instead of N independent ones, mirroring the Java optimisation
  that collapses multiple internal repartition topics into one.

  Skipped when:

    * A repartition node has multiple parents (split lineage —
      not a sibling collision).
    * Two repartition processors share a parent but disagree on
      prefix (different broker-side topics in the JVM
      equivalent).
    * The processor owns a state store (a stateful repartition,
      which we never auto-emit but a 'liftIO_' caller could).

== Semantics

Both rewrites are observably equivalent to the un-rewritten
topology under the in-process 'TopologyTestDriver' (which
doesn't materialise broker topics — changelogs are an in-memory
store, repartition is a logical pass-through). They become
observable against a real broker, where the changelog/topic
count drops.

== Idempotence + dynamic modification

'optimizeTopology' is /reflective/: every call wipes the
optimiser's prior decisions (the 'topoChangelogPlan' map) and
re-derives them from the current graph state. This means:

  * @'optimizeTopology' cfg . 'optimizeTopology' cfg = 'optimizeTopology' cfg@
    (idempotent).
  * Subsequent modifications via 'addSink' / 'addProcessor' /
    'addSource' / 'addStateStore*' invalidate prior optimisation
    decisions; calling 'optimizeTopology' again restores
    correctness. Specifically, after adding a sink to a source
    topic that was being reused as a changelog, re-running the
    optimiser /clears/ the stale plan entry (the topic is no
    longer a clean source-only changelog).
  * 'validateChangelogPlan' (also run as part of
    'validateTopology') catches the case where the user
    modified the graph and /forgot/ to re-optimise. Stale
    entries are reported as 'StaleChangelogPlan' errors.

For 'optReuseSourceKTable' the reflective behaviour applies
cleanly: clear + re-derive on every call. For
'optMergeRepartitionTopics' the rewrite is destructive
(duplicate nodes are removed) — re-running picks up newly-added
siblings but cannot recover removed ones. See
'applyMergeRepartitionTopics' for the full contract.

User-explicit declarations via
'Kafka.Streams.State.Store.withSourceTopicChangelogKV' are
/never/ touched by the optimiser; they live in
'LoggingConfig.loggingSourceTopic' and are consulted by
'effectiveChangelogReuse' in addition to the side-table.
-}
optimizeTopology :: OptimizationConfig -> Topology -> Topology
optimizeTopology cfg = mergeRepartitions . reuseStep . wipePlan
  where
    -- Reset every optimiser-derived decision before re-deriving.
    -- This is what makes 'optimizeTopology' reflective: subsequent
    -- modifications to the topology invalidate prior decisions,
    -- and re-running the optimiser produces a fresh plan from
    -- the current graph state.
    wipePlan t = t {topoChangelogPlan = Map.empty}

    reuseStep
      | optReuseSourceKTable cfg = applyReuseSourceKTable
      | otherwise = Prelude.id
    mergeRepartitions
      | optMergeRepartitionTopics cfg = applyMergeRepartitionTopics
      | otherwise = Prelude.id


----------------------------------------------------------------------
-- KIP-295 #1 — REUSE_KTABLE_SOURCE_TOPICS
----------------------------------------------------------------------

{- | Identify every processor that fits the "source-table" shape
and populate 'topoChangelogPlan' with the resulting
@(store, source-topic)@ pairs.

The map is /wiped first/ — entries that no longer correspond
to valid candidates after dynamic modifications are
discarded. The optimiser is fully /reflective/: its output is
a pure function of the current graph state, independent of
prior optimisation history.

User-explicit 'loggingSourceTopic' declarations (set via
'Kafka.Streams.State.Store.withSourceTopicChangelogKV') are
never written to the side-table — they live in
'LoggingConfig' and the optimiser respects them as
already-decided. The 'effectiveChangelogReuse' accessor
consults both sources and returns the user declaration first,
falling back to the side-table.
-}
applyReuseSourceKTable :: Topology -> Topology
applyReuseSourceKTable t0 =
  let !t = t0 {topoChangelogPlan = Map.empty}
      !plan = Map.fromList (collectReuseCandidates t)
  in t {topoChangelogPlan = plan}


{- | Pairs of @(store, sourceTopic)@ where the store's changelog
can be redirected to the source topic.
-}
collectReuseCandidates :: Topology -> [(StoreName, TopicName)]
collectReuseCandidates t =
  let !sinkTopicSet =
        Set.fromList
          [ sinkTopic s
          | s <- Map.elems (topoSinks t)
          ]
  in [ (storeNm, topic)
     | (procNm, spec) <- Map.toList (topoProcessors t)
     , [parentNm] <- pure (processorSpecParents spec)
     , Just src <- pure (Map.lookup parentNm (topoSources t))
     , [topic] <- pure (sourceTopics src)
     , not (Set.member topic sinkTopicSet)
     , [storeNm] <- pure (processorSpecStores spec)
     , not (Set.member storeNm (topoGlobalStores t))
     , Just owners <- pure (Map.lookup storeNm (topoStoreOwners t))
     , owners == [procNm]
     , Just builder <- pure (Map.lookup storeNm (topoStores t))
     , Just logCfg <- pure (storeBuilderLogging builder)
     , loggingEnabled logCfg
     , -- A user-explicit 'loggingSourceTopic' wins; the optimiser
     -- doesn't duplicate / fight the user's declaration.
     Nothing <- pure (loggingSourceTopic logCfg)
     ]


{- | Extract the 'LoggingConfig' from an 'AnyStoreBuilder' (if it
has one — raw 'AsRawBuilder' values use the generic
'sbLogging').
-}
storeBuilderLogging :: AnyStoreBuilder -> Maybe LoggingConfig
storeBuilderLogging = \case
  AsKeyValueBuilder b -> Just (sbKvLogging b)
  AsWindowBuilder b -> Just (sbWLogging b)
  AsSessionBuilder b -> Just (sbSLogging b)
  AsRawBuilder b -> Just (sbLogging b)


-- | Whether the builder carries the serdes its changelog needs.
storeBuilderHasSerdes :: AnyStoreBuilder -> Bool
storeBuilderHasSerdes = \case
  AsKeyValueBuilder b -> isJust (sbKvSerdes b)
  AsWindowBuilder b -> isJust (sbWSerdes b)
  AsSessionBuilder b -> isJust (sbSSerdes b)
  AsRawBuilder _ -> False


{- | The effective KIP-295 changelog-reuse target for @store@, if
any. Consults both the /user-explicit/ declaration
('LoggingConfig.loggingSourceTopic') and the
/optimiser-derived/ 'topoChangelogPlan'. The user-explicit
declaration wins when both are set.

Returns 'Nothing' when the store has its own internal
changelog (the default).
-}
effectiveChangelogReuse :: Topology -> StoreName -> Maybe TopicName
effectiveChangelogReuse t sn =
  case Map.lookup sn (topoStores t) of
    Just b
      | Just lc <- storeBuilderLogging b
      , Just tp <- loggingSourceTopic lc ->
          Just tp
    _ -> Map.lookup sn (topoChangelogPlan t)


----------------------------------------------------------------------
-- KIP-295 #2 — MERGE_REPARTITION_TOPICS
----------------------------------------------------------------------

{- | Collapse sibling pass-through repartition processors that
share a single parent and the same @KSTREAM-REPARTITION-\<prefix\>@
name prefix into one. Downstream consumers are rewired to the
survivor; 'topoChildrenIndex' is rebuilt at the end.

== Dynamic-modification contract

The rewrite is /destructive/ on the topology — losing
repartition nodes are removed entirely from 'topoProcessors',
'topoOrder', and 'topoChildrenIndex'. Subsequent modifications
are still safe and correct: re-running 'applyMergeRepartitionTopics'
(via 'optimizeTopology') on the modified graph picks up the new
state. Specifically:

  * Adding a new sibling with the same prefix gets absorbed
    into the existing survivor.
  * Adding a new sibling with a /smaller/ node name swaps the
    survivor — the new node becomes the merged target, the
    old survivor folds in.
  * Adding a sibling under a different parent leaves both
    alone (different parents = different merge group).

Unlike 'applyReuseSourceKTable', this rewrite cannot be
/reverted/ — once the duplicate nodes are gone, you'd have to
rebuild the graph from the AST to recover them. That's by
design: the merge is semantically equivalent (pass-throughs
don't observe records) and the broker resource savings are the
whole point.
-}
applyMergeRepartitionTopics :: Topology -> Topology
applyMergeRepartitionTopics t =
  let !groups = repartitionSiblingGroups t
      !rename = buildRenameMap groups
      !t1 = applyRename rename t
  in t1 {topoChildrenIndex = rebuildChildrenIndex t1}
  where
    buildRenameMap :: [[NodeName]] -> Map NodeName NodeName
    buildRenameMap = foldl' addGroup Map.empty
      where
        addGroup acc grp = case grp of
          [] -> acc
          (survivor : vs) ->
            foldl' (\m loser -> Map.insert loser survivor m) acc vs


{- | Discover groups of sibling repartition processors that can be
merged. The returned list contains one entry per group; each
group is a non-empty list of 'NodeName's where the head is the
survivor and the tail is the to-be-removed duplicates.

Survivor selection is deterministic: within each group we pick
the lexicographically smallest 'NodeName'. This is independent
of insertion order, so 'applyMergeRepartitionTopics' is
idempotent under the optimiser's normal compose-with-itself
semantics.
-}
repartitionSiblingGroups :: Topology -> [[NodeName]]
repartitionSiblingGroups t =
  [ orderedGrp
  | (_parent, kids) <- Map.toList (topoChildrenIndex t)
  , let buckets = groupRepartitionsByPrefix t kids
  , (_prefix, grp) <- buckets
  , length grp > 1
  , let orderedGrp = List.sort grp
  ]


{- | Partition a child list by repartition prefix; keeps only
nodes that are eligible for merging (single-parent
pass-through repartition processors with no owned stores).
Returns @[(prefix, [nodes-in-insertion-order])]@.
-}
groupRepartitionsByPrefix
  :: Topology
  -> [NodeName]
  -> [(Text, [NodeName])]
groupRepartitionsByPrefix t kids =
  let eligible =
        [ (prefix, kid)
        | kid <- kids
        , Just spec <- [Map.lookup kid (topoProcessors t)]
        , length (processorSpecParents spec) == 1
        , null (processorSpecStores spec)
        , Just prefix <- [repartitionPrefix (unNodeName kid)]
        ]
  in Map.toList $
       Map.fromListWith
         (\new old -> old ++ new)
         [(prefix, [kid]) | (prefix, kid) <- eligible]


{- | Extract the @\<prefix\>@ part from a @KSTREAM-REPARTITION-\<prefix\>-\<id\>@
'NodeName'. Returns 'Nothing' if the name doesn't match the
repartition pattern.
-}
repartitionPrefix :: Text -> Maybe Text
repartitionPrefix nm = do
  rest <- Text.stripPrefix "KSTREAM-REPARTITION-" nm
  -- Strip the trailing @-<counter>@ that 'freshNodeName' appends.
  -- The counter is a non-empty run of digits at the end.
  let (lhs, dashCounter) = Text.breakOnEnd "-" rest
  if Text.null dashCounter || not (Text.all Char.isDigit dashCounter)
    then pure rest
    -- @lhs@ ends in '-' (because 'breakOnEnd' keeps the delimiter
    -- in the left half); strip it.
    else pure (Text.dropWhileEnd (== '-') lhs)


{- | Apply a survivor-rename map to every parent/owner reference
in the topology and drop the renamed-away nodes.
-}
applyRename :: Map NodeName NodeName -> Topology -> Topology
applyRename m t
  | Map.null m = t
  | otherwise =
      let resolve nm = Map.findWithDefault nm nm m
          renameList = map resolve
          dropLosers ns = filter (\n -> not (Map.member n m)) ns
      in t
           { topoProcessors =
               Map.map
                 (renameProcessorParents resolve)
                 ( Map.filterWithKey
                     (\k _ -> not (Map.member k m))
                     (topoProcessors t)
                 )
           , topoSinks =
               Map.map (renameSinkParents resolve) (topoSinks t)
           , topoOrder =
               Seq.filter
                 (\n -> not (Map.member n m))
                 (topoOrder t)
           , topoStoreOwners =
               Map.map (nub . renameList) (topoStoreOwners t)
           , topoChildrenIndex =
               -- Drop renamed keys; the index is rebuilt afterwards.
               Map.mapKeys
                 resolve
                 (Map.map (dropLosers . renameList) (topoChildrenIndex t))
           }
  where
    renameProcessorParents f spec =
      spec
        { processorSpecParents = nub (map f (processorSpecParents spec))
        }
    renameSinkParents f spec =
      spec
        { sinkParents = nub (map f (sinkParents spec))
        }


{- | Recompute 'topoChildrenIndex' from scratch by walking every
processor's and sink's parent list.
-}
rebuildChildrenIndex :: Topology -> Map NodeName [NodeName]
rebuildChildrenIndex t =
  Map.map reverse $
    foldl'
      ( \acc (child, parents) ->
          foldl' (\a p -> Map.insertWith (++) p [child] a) acc parents
      )
      Map.empty
      childParents
  where
    childParents :: [(NodeName, [NodeName])]
    childParents =
      [ (processorSpecName s, processorSpecParents s)
      | s <- Map.elems (topoProcessors t)
      ]
        ++ [ (sinkName s, sinkParents s)
           | s <- Map.elems (topoSinks t)
           ]


{- | Connect an existing processor to one or more existing state
stores. Mirrors @Topology.connectProcessorAndStateStores@ —
equivalent to having registered the stores at processor-add time
via the @stores@ list, useful for after-the-fact re-wiring.
-}
connectProcessorAndStateStores
  :: NodeName
  -> [StoreName]
  -> Topology
  -> Topology
connectProcessorAndStateStores procNm stores t =
  case Map.lookup procNm (topoProcessors t) of
    Nothing ->
      errorWithCtx
        ( "connectProcessorAndStateStores: unknown processor: "
            <> unNodeName procNm
        )
    Just _ ->
      let !procsNew =
            foldl'
              (\m sn -> attachToProcessor sn m procNm)
              (topoProcessors t)
              stores
          !ownersNew =
            foldl'
              (\m sn -> Map.insertWith (++) sn [procNm] m)
              (topoStoreOwners t)
              stores
      in t
           { topoProcessors = procsNew
           , topoStoreOwners = ownersNew
           }


addStoreInternal
  :: StoreName -> AnyStoreBuilder -> [NodeName] -> Topology -> Topology
addStoreInternal sn ab owners t =
  case Map.lookup sn (topoStores t) of
    Just _ -> errorWithCtx ("store already added: " <> unStoreName sn)
    Nothing ->
      let !storesNew = Map.insert sn ab (topoStores t)
          !ownersNew = Map.insertWith (++) sn owners (topoStoreOwners t)
          !procsNew = foldl' (attachToProcessor sn) (topoProcessors t) owners
      in t
           { topoStores = storesNew
           , topoStoreOwners = ownersNew
           , topoProcessors = procsNew
           }


{- | Register a /global/ state store backed by its own source +
processor. Mirrors @Topology.addGlobalStore@.

The supplied store builder is realised as a regular state store,
but the 'StoreName' is /also/ added to 'topoGlobalStores'. The
runtime treats global stores as cluster-wide replicas: every
instance subscribes to the source topic and writes to the local
copy. Within the single-task 'TopologyTestDriver' the
distinction is purely semantic — the same source/processor/store
machinery handles the data flow.

The @processorSupplier@ takes (k, v) records and updates the
store; a typical implementation just does @kvsPut store k v@.
Use 'Kafka.Streams.GlobalKTable' for the high-level
"global lookup table" API; this lower-level entry point lets
you run arbitrary processor logic on the global side.
-}
addGlobalStore
  :: StoreBuilderKV k v
  -> NodeName
  -- ^ source name
  -> NodeName
  -- ^ processor name
  -> TopicName
  -> Serde k
  -> Serde v
  -> Kafka.Streams.Time.TimestampExtractor k v
  -> AnyProcessor
  -- ^ store updater
  -> Topology
  -> Topology
addGlobalStore builder sourceNm procNm topic ks vs ex updater t0 =
  ensureNameFree t0 sourceNm $
    ensureNameFree (insertSource t0 srcSpec) procNm $
      let !t1 = insertSource t0 srcSpec
          !t2 =
            addProcessorWith
              ProcessorSpec
                { processorSpecName = procNm
                , processorSpecParents = [sourceNm]
                , processorSpecSupplier = updater
                , processorSpecStores = [Kafka.Streams.State.Store.sbKvName builder]
                }
              t1
          !t3 = addStateStoreKV builder [procNm] t2
      in t3
           { topoGlobalStores =
               Set.insert
                 (Kafka.Streams.State.Store.sbKvName builder)
                 (topoGlobalStores t3)
           }
  where
    !srcSpec =
      SourceSpec
        { sourceName = sourceNm
        , sourceTopics = [topic]
        , sourceKeySerde = AnySerde ks
        , sourceValueSerde = AnySerde vs
        , sourceExtractor = AnyTimestampExtractor ex
        , sourceOffsetReset = Consumed.OffsetEarliest
        , sourcePattern = Nothing
        , sourceNullValues = False
        , sourceWatermarkStrategy = Nothing
        }


{- | Mark an already-added store as global: one copy per instance,
fed every partition of its source topics (the runtime's global
thread), never part of a task.
-}
markGlobalStore :: StoreName -> Topology -> Topology
markGlobalStore sn t = t {topoGlobalStores = Set.insert sn (topoGlobalStores t)}


attachToProcessor
  :: StoreName
  -> Map NodeName ProcessorSpec
  -> NodeName
  -> Map NodeName ProcessorSpec
attachToProcessor sn m owner =
  Map.adjust
    ( \spec ->
        spec
          { processorSpecStores = nub (sn : processorSpecStores spec)
          }
    )
    owner
    m


-- | All node names already used by @t@.
allNames :: Topology -> Set NodeName
allNames t =
  Set.unions
    [ Map.keysSet (topoSources t)
    , Map.keysSet (topoProcessors t)
    , Map.keysSet (topoSinks t)
    ]


ensureNameFree :: Topology -> NodeName -> Topology -> Topology
ensureNameFree t nm cont =
  if Set.member nm (allNames t)
    then errorWithCtx ("node already exists: " <> unNodeName nm)
    else cont


errorWithCtx :: Text -> a
errorWithCtx msg = error ("Kafka.Streams.Topology: " <> T.unpack msg)


----------------------------------------------------------------------
-- Validation
----------------------------------------------------------------------

-- | Phantom-tagged topology proving 'validateTopology' succeeded.
newtype TopologyValid = TopologyValid {topologyValidGraph :: Topology}


{- | Bypass validation. The runtime trusts validated topologies; if you
bypass and feed an invalid graph in you can corrupt state on
failover. Don't.
-}
unsafeAssumeValid :: Topology -> TopologyValid
unsafeAssumeValid = TopologyValid


{- | Check the graph for the invariants documented at the top of the
module. Returns either the first error encountered or a phantom
proof that validation passed.
-}
validateTopology :: Topology -> Either TopologyError TopologyValid
validateTopology t = do
  let knownNodes = allNames t
  -- 1. sources non-empty + topics
  when_ (Map.null (topoSources t)) NoSources
  Map.foldlWithKey'
    ( \acc nm src ->
        acc >> when_ (null (sourceTopics src)) (EmptySourceTopics nm)
    )
    (Right ())
    (topoSources t)
  -- 2. parents resolve
  Map.foldlWithKey'
    ( \acc nm spec ->
        acc
          >> case [ p
                  | p <- processorSpecParents spec
                  , not (Set.member p knownNodes)
                  ] of
            [] -> Right ()
            (p : _) -> Left (UnknownParent nm p)
    )
    (Right ())
    (topoProcessors t)
  Map.foldlWithKey'
    ( \acc nm spec ->
        acc
          >> case [ p
                  | p <- sinkParents spec
                  , not (Set.member p knownNodes)
                  ] of
            [] -> Right ()
            (p : _) -> Left (UnknownParent nm p)
    )
    (Right ())
    (topoSinks t)
  -- 3. store ownership
  Map.foldlWithKey'
    ( \acc sn owners ->
        acc
          >> case [ o
                  | o <- owners
                  , not (Map.member o (topoProcessors t))
                  ] of
            [] -> Right ()
            (o : _) -> Left (UnknownStore sn o)
    )
    (Right ())
    (topoStoreOwners t)
  -- 4. acyclicity (the children index is a DAG iff a topo-sort exists).
  case detectCycle t of
    Just chain -> Left (TopologyCycle chain)
    Nothing -> do
      -- 5. changelog-plan consistency. Catches stale entries left
      -- over from post-optimisation modifications that the user
      -- forgot to re-optimise.
      validateChangelogPlan t
      Right (TopologyValid t)
  where
    when_ :: Bool -> TopologyError -> Either TopologyError ()
    when_ b e = if b then Left e else Right ()


{- | Validate every changelog-reuse declaration against the
current graph. Both 'topoChangelogPlan' (optimiser-derived)
and 'loggingSourceTopic' (user-explicit) are checked using
the same eligibility rules 'optimizeTopology' applies.

The check is conservative: a plan entry is accepted only when
the graph independently agrees that the store is in a
single-parent / single-source-topic / single-owner shape with
logging enabled and no competing sink. Anything else returns
'StaleChangelogPlan' with a 'ChangelogPlanProblem' explaining
which precondition was violated.

Run as part of 'validateTopology'; also exported for callers
who want to spot-check after a sequence of modifications.
-}
validateChangelogPlan :: Topology -> Either TopologyError ()
validateChangelogPlan t = do
  -- Plan-derived claims.
  Map.foldlWithKey'
    (\acc sn topic -> acc >> checkOne sn topic)
    (Right ())
    (topoChangelogPlan t)
  -- User-declared claims via 'withSourceTopicChangelogKV'. We
  -- treat them with the same eligibility rules: a user who
  -- declares "reuse topic X as changelog" must still have the
  -- graph shape that backs the claim.
  Map.foldlWithKey'
    ( \acc sn ab ->
        acc
          >> case storeBuilderLogging ab of
            Just lc
              | Just topic <- loggingSourceTopic lc -> checkOne sn topic
            _ -> Right ()
    )
    (Right ())
    (topoStores t)
  where
    sinkTopicSet :: Set TopicName
    sinkTopicSet = Set.fromList [sinkTopic s | s <- Map.elems (topoSinks t)]

    checkOne :: StoreName -> TopicName -> Either TopologyError ()
    checkOne sn topic = case Map.lookup sn (topoStores t) of
      Nothing -> bad sn topic StaleNoSuchStore
      Just builder -> case Map.lookup sn (topoStoreOwners t) of
        Nothing -> bad sn topic StaleNoOwners
        Just [] -> bad sn topic StaleNoOwners
        Just (_ : _ : _) -> bad sn topic StaleMultipleOwners
        Just [owner] -> case Map.lookup owner (topoProcessors t) of
          Nothing -> bad sn topic StaleNoOwners
          Just spec -> case processorSpecParents spec of
            [] -> bad sn topic StaleParentNotASource
            (_ : _ : _) -> bad sn topic StaleOwnerHasMultipleParents
            [parent] -> case Map.lookup parent (topoSources t) of
              Nothing -> bad sn topic StaleParentNotASource
              Just src -> case sourceTopics src of
                [] -> bad sn topic StaleSourceMultipleTopics
                (_ : _ : _) -> bad sn topic StaleSourceMultipleTopics
                [actual]
                  | actual /= topic -> bad sn topic (StaleWrongTopic actual)
                  | Set.member topic sinkTopicSet ->
                      bad sn topic StaleTopicHasSink
                  | not
                      ( maybe
                          False
                          loggingEnabled
                          (storeBuilderLogging builder)
                      ) ->
                      bad sn topic StaleLoggingDisabled
                  | otherwise -> Right ()

    bad sn topic prob = Left (StaleChangelogPlan sn topic prob)


-- | Cycle detector via DFS. Returns one offending chain if any.
detectCycle :: Topology -> Maybe [NodeName]
detectCycle t =
  go Set.empty Set.empty (Map.keys (topoChildrenIndex t))
  where
    children =
      foldr
        (\nm acc -> Map.insertWith (++) nm [] acc)
        (topoChildrenIndex t)
        (Set.toList (allNames t))

    go _seen _stack [] = Nothing
    go seen stack (n : ns)
      | Set.member n seen = go seen stack ns
      | otherwise =
          case dfs n seen stack of
            Right seen' -> go seen' stack ns
            Left cycle' -> Just cycle'

    dfs n seen stack
      | Set.member n stack = Left (Set.toList stack ++ [n])
      | Set.member n seen = Right seen
      | otherwise =
          let ks = Map.findWithDefault [] n children
              stack' = Set.insert n stack
              step e [] = e
              step (Right s) (k : rest) =
                case dfs k s stack' of
                  Right s' -> step (Right s') rest
                  err -> err
              step err _ = err
          in case step (Right seen) ks of
               Right s' -> Right (Set.insert n s')
               err -> err


-- | All node names present in a topology, in insertion order.
topologyNodes :: Topology -> [NodeName]
topologyNodes = Foldable.toList . topoOrder


-- | Reverse-lookup: who feeds @n@?
parentsOf :: Topology -> NodeName -> [NodeName]
parentsOf t n =
  case Map.lookup n (topoProcessors t) of
    Just spec -> processorSpecParents spec
    Nothing ->
      case Map.lookup n (topoSinks t) of
        Just spec -> sinkParents spec
        Nothing -> []


-- | Forward-lookup: who consumes @n@?
childrenOf :: Topology -> NodeName -> [NodeName]
childrenOf t n = Map.findWithDefault [] n (topoChildrenIndex t)
