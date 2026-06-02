{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Kafka.Streams.Topology.Free
-- Description : First-class GADT \/ free-arrow topology builder
--
-- A 'Topology' value describes a Kafka Streams topology fragment as a
-- /first-class value/. Unlike the imperative builder in
-- "Kafka.Streams.StreamsBuilder" (which mutates an 'IORef' on every
-- DSL call), a 'Topology' is a pure AST that can be inspected,
-- traversed, composed, and finally /compiled/ into the existing
-- 'Kafka.Streams.Topology.Topology' graph via 'compile'.
--
-- The type parameters @i@ and @o@ classify the /wire type/ flowing in
-- and out of the fragment. Concretely they are inhabited by the
-- handle types the DSL already uses:
--
--   * @'Kafka.Streams.KStream' k v@        — a record stream
--   * @'Kafka.Streams.KTable' k v@         — a changelog table
--   * @'Kafka.Streams.KGroupedStream' k v@ — a re-keyed stream
--   * @'Kafka.Streams.KGroupedTable' k v@  — a re-keyed table
--   * @'Kafka.Streams.GlobalKTable' k v@   — cluster-replicated table
--   * @'Kafka.Streams.TimeWindowedKStream' k v@ — time-windowed grouped stream
--   * @'Kafka.Streams.SessionWindowedKStream' k v@ — session-windowed grouped stream
--   * @'Kafka.Streams.Cogroup.CogroupedStream' k a@ — multi-source cogroup
--   * @'Data.Void.Void'@                   — an /open/ input slot
--                                             (only sources fill it)
--   * @()@                                 — a sink terminus
--   * @(a, b)@                             — two independent wires
--                                             (parallel composition)
--   * @'Either' a b@                       — a choice between two
--                                             wires
--   * @'NonEmpty' a@                       — multi-way fan-out
--   * @'Map' 'Text' ('KStream' k v)@        — named branches (KIP-418)
--
-- The DSL surface is reified as constructors. Composition uses the
-- standard category-theoretic vocabulary:
--
-- @
-- import Control.Category ((>>>))
-- import Control.Arrow    ((***), (&&&))
-- import Kafka.Streams.Topology.Free as F
--
-- normalise :: F.Topology Void ()
-- normalise =
--   F.source     \"in\"  textSerde textSerde
--     '>>>' F.mapValues T.toUpper
--     '>>>' F.filter    (\\r -> recordValue r '/=' \"\")
--     '>>>' F.sink      \"out\" textSerde textSerde
-- @
--
-- == Why a GADT?
--
-- The 'Pipeline' newtype over @Kleisli IO@ in "Kafka.Streams.Pipeline"
-- already gives us category + arrow + arrow-choice. What it doesn't
-- give is a /closed AST/ — every fragment is opaquely a function. The
-- GADT form keeps the constructors visible so the same value can be:
--
--   * pretty-printed (the upstream JVM @TopologyDescription@ idiom),
--   * statically optimised (KIP-295 repartition merging, source-KTable
--     reuse, etc — see "Kafka.Streams.Topology.Optimization"),
--   * round-tripped through the StableNames hashing in
--     "Kafka.Streams.Topology.StableNames" without forcing IO, and
--   * lawful w.r.t. the 'Category', 'Arrow', and 'ArrowChoice'
--     instances (free arrow up to the standard equations — the
--     compiler currently materialises every constructor distinctly,
--     so equational rewrites are deferred to an explicit pass).
--
-- == Lineage: 'Parallel', 'Fanout', 'Fork', 'ForkN', 'Tap', 'Split'
--
-- A /lineage/ is what the JVM topology description calls a chain of
-- nodes connected to one another in the directed graph. Two of our
-- combinators introduce a tuple shape on the wire but they mean
-- different things at the lineage level:
--
-- [@'Parallel' p q :: 'Topology' (a, c) (b, d)@]
--   Runs @p@ on the @a@-side and @q@ on the @c@-side. The two
--   subgraphs are /independent/ in the topology graph — nothing
--   connects them except the tuple shape at the call site. Use this
--   when you've brought two unrelated upstream lineages together
--   (typically before a join) and want to apply a different
--   transform to each side.
--
-- [@'Fanout' p q :: 'Topology' a (b, c)@]
--   Feeds a /single/ upstream wire to two sub-fragments. In the
--   compiled topology the upstream node has two children. Use this
--   when you want to fork one lineage into two.
--
-- For more than two siblings, see 'Fork', 'ForkN', 'Tap', and
-- 'Split':
--
-- [@'Fork' :: 'Topology' a (a, a)@]
--   Explicit duplicator. @'Fork' = 'id' '&&&' 'id'@; named for
--   clarity at call sites.
--
-- [@'ForkN' :: 'NonEmpty' ('Topology' a b) -> 'Topology' a ('NonEmpty' b)@]
--   N-way fan-out: apply each sub-pipeline to the same upstream and
--   collect the results in order. Better than nested '&&&' once you
--   have more than two branches.
--
-- [@'Tap' :: 'Topology' a () -> 'Topology' a a@]
--   Run a side-effecting sub-pipeline (typically ending in a 'Sink'
--   or 'Foreach') and pass the upstream wire through unchanged.
--   Operationally equivalent to @id '&&&' t \\>\\>\\> arr fst@ but
--   compiles cleaner and reads better.
--
-- [@'Split' :: ['SplitBranch' k v] -> 'Maybe' 'Text' -> 'Topology' ('KStream' k v) ('Map' 'Text' ('KStream' k v))@]
--   KIP-418 named branches. Each record routes to the first
--   matching branch; records that match none go to the default
--   branch if supplied, otherwise are dropped. The result is a
--   'Map' keyed by branch name, which downstream 'Arr' calls can
--   destructure.
--
-- The combinators compose, so a typical "split a source into N
-- transforms and merge a subset back together" pipeline reads
-- straightforwardly:
--
-- @
--   F.source \"in\" ks vs
--     '>>>' F.tap (F.filter isLog '>>>' F.sink \"audit\" ks vs)
--     '>>>' F.mapValues normalise
--     '>>>' F.sink \"out\" ks vs
-- @
--
-- == Lineage and exactly-once semantics
--
-- Kafka Streams partitions a topology into /sub-topologies/ — the
-- connected components of the graph — and assigns one task per
-- sub-topology per partition. EOS-v2 commits transactionally
-- /per task/, so two processors share an EOS transaction iff
-- they're in the same sub-topology.
--
-- That has implications for how our lineage combinators interact
-- with EOS:
--
--   * 'Fanout' / 'Fork' / 'ForkN' / 'Tap' / 'Split' all branch
--     from /one/ upstream, so every branch lands in the
--     /same/ sub-topology as that upstream. EOS atomicity
--     extends to every sink in every branch.
--   * 'Parallel' over two source-rooted halves (the typical
--     "two unrelated pipelines combined into one
--     @Topology Void ()@") /does not/ share lineage: the two
--     halves compile to two disconnected sub-topologies and
--     therefore run as two tasks with independent EOS
--     transactions.
--   * Use 'mergeSourced' (or any other convergence point —
--     stream-stream join, shared 'Merge', etc.) to make two
--     source-rooted halves share a task.
--   * Alternative cross-task lineage: register the same state
--     store against both halves via 'withStateStoreKV' — the
--     task assigner treats co-owned state stores as a shared
--     lineage edge.
--
-- The 'Semigroup' / 'Monoid' instances over @'Topology' i ()@
-- run two pipelines on the /same/ upstream input, so they
-- share lineage by construction (single-source, multi-sink) —
-- another way to build EOS-atomic multi-output topologies.
--
-- == On side effects
--
-- 'Foreach' is /synchronous/: the supplied callback runs on the
-- stream-processing thread for every record before the engine
-- moves on. This is intentional and matches the JVM
-- @KStream.foreach@ contract. The reasons we do /not/ ship a
-- @foreachAsync@ constructor on top:
--
--   * No backpressure — fire-and-forget asyncs accumulate when the
--     handler is slower than the poll rate; the runtime OOMs
--     before the stream thread ever felt the slowness.
--   * Silent error swallowing — discarded async handles drop
--     failures on the floor.
--   * EOS-incompatibility — the async escapes the transactional
--     commit cycle; an abort can't roll it back.
--   * Out-of-order completion within a partition — Kafka Streams
--     guarantees per-key ordering on the stream thread, but
--     @async@ destroys that guarantee.
--
-- The JVM API doesn't ship an async @foreach@ either; the canonical
-- patterns for non-blocking work are:
--
-- [\"Sink + downstream consumer\"]
--   Publish to a side topic with 'sink', then run a separate
--   consumer that processes the side topic. The intermediate
--   topic provides durability, ordering, EOS, and natural
--   backpressure.
--
-- [\"Custom processor with bounded async pool\"]
--   Reach for 'processStream' / 'processWithStateStoreKV' and
--   build your own bounded queue + worker pool inside the
--   processor. The 'Processor' lifecycle hooks
--   ('procInit' \/ 'procClose') give you a place to allocate
--   and clean up. This keeps the slow work off the stream
--   thread /and/ caps the in-flight work.
--
-- [\"liftIO_ escape hatch\"]
--   For genuinely best-effort fire-and-forget work that's
--   acceptable to lose on a restart, use 'liftIO_' with an
--   explicit 'Control.Concurrent.Async.async'. Making the
--   user reach for 'liftIO_' is the deliberate friction: the
--   ergonomic GADT path doesn't lead callers into the footgun.
--
-- The underlying imperative
-- 'Kafka.Streams.KStream.foreachStreamAsync' from the original
-- DSL is still available for callers who explicitly want it;
-- this module just doesn't promote it to a first-class
-- 'Topology' constructor.
--
-- == Monad bind for incrementally-built topologies
--
-- The 'Monad' instance lets you bind a wire value to a
-- Haskell-level name and use it across multiple downstream
-- fragments. This is especially handy for the
-- /cogroup-shaped/ Kafka Streams operations where the JVM
-- API is incremental (each @addCogrouped@ call extends a
-- builder). 'applyT' threads a bound value into a downstream
-- fragment:
--
-- @
-- topology :: F.Topology Void (KTable Text Text)
-- topology = do
--   s1  <- F.source \"in1\" textSerde textSerde
--   s2  <- F.source \"in2\" textSerde textSerde
--   g1  <- F.groupByKey grp   \`F.applyT\` s1
--   g2  <- F.groupByKey grp   \`F.applyT\` s2
--   cg0 <- F.cogroup adder1   \`F.applyT\` g1
--   cg1 <- F.addCogrouped a2  \`F.applyT\` (cg0, g2)
--   F.aggregateCogrouped (pure \"\") mat \`F.applyT\` cg1
-- @
module Kafka.Streams.Topology.Free
  ( -- * The 'Topology' free arrow
    Topology
  , Prim (..)
  , liftPrim
  , labelPrim
  , runPrim
  , SplitBranch (..)

    -- * Compilation
  --
  -- 'compile' applies a curated set of /semantics-preserving/
  -- rewrites to the topology before walking it (see
  -- "Kafka.Streams.Topology.Free.Optimize"). Use
  -- 'compileNoOptimize' for golden-file or rewrite-debugging tests
  -- where you want the AST untouched. 'compileWithOptimization'
  -- lets you pick which rewrite families fire.
  , compile
  , compileNoOptimize
  , compileWithOptimization
  , compileInBuilder
  , compileWith
  , apply

    -- * Optimisation
  --
  -- See "Kafka.Streams.Topology.Free.Optimize" for the rewrite
  -- list and configuration. 'optimize' is also re-exported from
  -- "Kafka.Streams.Topology.Free.Optimize" so callers can pick
  -- either module without changing import lists.
  , optimize
  , optimizeWith
  , OptimizeConfig (..)
  , defaultOptimizeConfig
  , noOptimization
  , countNodes
  , OptimizationStats (..)
  , optimizationStats

    -- * Constants for the type signatures
  , TBuilder
  , buildTopologyFrom

    -- * Sources
  , source
  , sourceWith
  , sources
  , sourcesWith
  , sourcePattern
  , sourcePatternWith
  , tableSource
  , tableSourceWith
  , globalTableSource
  , mergeSourced

    -- * Sinks
  , sink
  , sinkWith
  , sinkExtracted
  , sinkTwoPhase
  , through
  , throughWith

    -- * Stateless 'KStream' transforms
  , mapValues
  , mapValuesM
  , mapKeyValue
  , mapKeyValueM
  , mapRecord
  , mapRecordM
  , noFuse
    -- * Riffle: async I\/O transforms
  --
  -- See "Kafka.Streams.AsyncIO" for the contract: bounded
  -- worker pool, backpressure via a bounded in-flight queue,
  -- ordered\/unordered output, explicit failure policy, and
  -- EOS-compatible drain. Construct an 'AIO.AsyncIOConfig'
  -- (or start from 'AIO.defaultAsyncIOConfig') and reach for
  -- one of these instead of 'mapValuesM' \/ 'mapKeyValueM'
  -- whenever the per-record IO is latency-bound.
  , asyncMapValues
  , asyncMapKeyValue
  , asyncConcatMapValues
    -- ** Named operator variants (KIP-307)
  , filterNamed
  , filterNotNamed
  , mapValuesNamed
  , mapValuesMNamed
  , mapKeyValueNamed
  , mapKeyValueMNamed
  , mapRecordNamed
  , mapRecordMNamed
  , concatMapValuesNamed
  , concatMapKeyValueNamed
  , peekNamed
  , selectKeyNamed
  , valuesNamed
  , mergeNamed
  , repartitionNamed
  , sinkNamed
    -- ** Windowed emit strategy (KIP-825)
  , withEmitStrategy
  , TWKS.EmitStrategy (..)
  , TWKS.emitOnWindowUpdate
  , TWKS.emitOnWindowClose
    -- ** Configurable suppression buffers (KIP-328)
  , suppressWindowedWith
  , BufferConfig
  , maxRecordsBufferConfig
  , maxBytesBufferConfig
  , Suppress.BufferOverflowPolicy (..)
  , Suppress.unboundedBufferConfig
  , Suppress.shutDownWhenFull
  , Suppress.emitEarlyWhenFull
  , Suppress.dropOldestSilently
    -- ** Dead-letter shelf (Riffle shed policy)
  , suppressWindowedShed
  , Suppress.DeadLetterShelf (..)
    -- ** Suppressed builder vocabulary
  , Suppress.Suppressed
  , Suppress.untilWindowCloses
  , Suppress.untilTimeLimit
    -- ** Late store attachment + global stores
  , addGlobalStore
  , addReadOnlyStateStore
  , connectProcessorAndStateStores
    -- ** Rich sink + stateful transform
  , sinkSpec
  , transformStream
    -- ** TableJoined-aware FK joins (KIP-545)
  , foreignKeyJoinWith
  , leftForeignKeyJoinWith
    -- ** Time-windowed cogroup
  , windowedByCogroup
  , aggregateWindowedCogrouped
  , filter
  , filterNot
  , concatMapValues
  , concatMapKeyValue
  , peek
  , foreach
  , prints
  , selectKey
  , values

    -- * 'KStream' composition + branching
  , merge
  , mergeAll
  , branch
  , split
  , splitBranch
  , fork
  , forkN
  , tap

    -- * 'KStream' \<-\> 'KTable' conversions
  , toTable
  , toStream
  , groupByKTable
  , repartition
  , repartitionWith

    -- * Grouping + aggregation
  , groupByKey
  , groupByKeyWith
  , groupBy
  , groupByWith
  , count
  , reduce
  , aggregate

    -- * Windowed aggregation
  , windowedByTime
  , windowedBySession
  , countWindowed
  , reduceWindowed
  , aggregateWindowed
  , countSessionWindowed
  , aggregateSessionWindowed

    -- * 'KGroupedTable' (subtractor-aware aggregation)
  , groupTableBy
  , countKGroupedTable
  , reduceKGroupedTable
  , aggregateKGroupedTable

    -- * Cogroup
  , cogroup
  , addCogrouped
  , aggregateCogrouped

    -- * Joins
  , streamTableJoin
  , streamTableLeftJoin
  , streamStreamJoin
  , streamStreamLeftJoin
  , streamStreamOuterJoin
  , tableTableJoin
  , tableTableLeftJoin
  , tableTableOuterJoin
  , foreignKeyJoin
  , leftForeignKeyJoin
  , streamGlobalTableJoin
  , streamGlobalTableLeftJoin

    -- ** Two-input join helpers
    --
    -- Convenience wrappers that take both sides as
    -- @'Topology' 'Void' _@ legs and apply the corresponding
    -- join primitive. Equivalent to @left '&&&' right '>>>' …@
    -- but reads better at the call site.
  , joinStreamTable
  , leftJoinStreamTable
  , joinStreamStream
  , leftJoinStreamStream
  , outerJoinStreamStream
  , joinTableTable
  , leftJoinTableTable
  , outerJoinTableTable
  , joinForeignKey
  , leftJoinForeignKey
  , joinStreamGlobalTable
  , leftJoinStreamGlobalTable

    -- * KTable
  , filterTable
  , filterNotTable
  , mapValuesTable
  , transformValuesTable

    -- * Windowed-handle conversions
  --
  -- Adapters that take an aggregation's @*Handle@ wire and pin a
  -- 'KStream' at the aggregator's emit node so the rest of the
  -- pipeline can use the standard stream combinators.
  , streamFromWindowed
  , streamFromSessionWindowed

    -- * Suppress
  , suppressUntilTimeLimit
  , suppressWindowed

    -- * Processor API
  , processStream
  , processValuesStream
  , transformValuesStream
  , processFixedKeyValuesStream
  , processFixedKeyValuesWithStateStoreKV
  , withStateStoreKV
  , withStateStoreW
  , withStateStoreS
  , processWithStateStoreKV
  , processWithStateStoreW
  , processWithStateStoreS

    -- * Escape hatch + introspection
  , liftIO_
  , inspect
  , inspectDeep
  , applyTraced
  , prettyPrint
  , prettyPrintDeep

    -- * Reader-shaped helpers
    --
    -- 'Topology' is a profunctor (contravariant in the input,
    -- covariant in the output). Since @'Topology' = 'FreeArrow'
    -- 'Prim'@, the full 'Data.Profunctor.Profunctor' \/
    -- 'Data.Profunctor.Strong' \/ 'Data.Profunctor.Choice'
    -- hierarchy is available directly (the instances live on
    -- 'FreeArrow' in "Kafka.Streams.Topology.Free.Arrow"); use
    -- 'Data.Profunctor.lmap' \/ 'Data.Profunctor.rmap' \/
    -- 'Data.Profunctor.dimap' to re-shape a topology's wire
    -- types. The reader-shaped helpers below have no class
    -- equivalent.
  , askInput
  , localInput
  , applyT

    -- * Errors
  --
  -- Lazy errors the builder can surface when a downstream
  -- operation forces a field that wasn't fully populated
  -- upstream (the only situations where the AST is itself well-
  -- typed but partial). 'TopologyFreeError' is an 'Exception'
  -- so callers can 'Control.Exception.try' the offending action.
  , TopologyFreeError (..)
  , SerdeSide (..)
  ) where

import Prelude hiding (id, filter, (.))

import Control.Arrow ((&&&))
import Control.Category (Category (..), (>>>))
import qualified Control.Exception as Exception
import Data.Profunctor (rmap)
import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import GHC.Generics (Generic)
import qualified System.IO.Unsafe as IOUnsafe

import qualified Kafka.Streams.AsyncIO as AIO
import qualified Kafka.Streams.AsyncIO.Config as AIO
import qualified Kafka.Streams.Sinks.TwoPhase as TPS
import Kafka.Streams.Cogroup (CogroupedStream)
import qualified Kafka.Streams.Cogroup as Cog
import Kafka.Streams.Consumed
  ( Consumed
  , consumed
  , consumedExtractor
  , consumedKeySerde
  , consumedOffsetReset
  , consumedWatermark
  , consumedValueSerde
  )
import qualified Kafka.Streams.Joined as Joined
import qualified Kafka.Streams.Named as Named
import qualified Kafka.Streams.ForeignKeyJoin as FK
import Kafka.Streams.GlobalKTable (GlobalKTable)
import qualified Kafka.Streams.GlobalKTable as GT
import Kafka.Streams.Grouped (Grouped (..), grouped)
import Kafka.Streams.Joined (JoinWindows, Joined)
import qualified Kafka.Streams.KGroupedStream as KGS
import Kafka.Streams.KGroupedStream
  ( KGroupedStream
  , SessionWindowedKStream
  , TimeWindowedKStream
  )
import Kafka.Streams.KGroupedTable (KGroupedTable)
import qualified Kafka.Streams.KGroupedTable as KGT
import qualified Kafka.Streams.KStream as KS
import Kafka.Streams.KStream (KStream)
import qualified Kafka.Streams.KTable as KT
import Kafka.Streams.KTable (KTable)
import Kafka.Streams.Materialized (Materialized)
import qualified Kafka.Streams.Materialized as Mat
import Kafka.Streams.Processor (FixedKeyProcessor, Processor)
import qualified Kafka.Streams.Processor as P
import Kafka.Streams.Produced (Produced, produced)
import qualified Kafka.Streams.Repartitioned as Rep
import Kafka.Streams.Serde (HasSerde, Serde, serde)
import qualified Kafka.Streams.SessionWindowedKStream as SWKS
import Kafka.Streams.State.Store
  ( StoreBuilderKV
  , StoreBuilderS
  , StoreBuilderW
  , StoreName
  , WindowedKey
  )
import qualified Kafka.Streams.State.Store as Store
import Kafka.Streams.StreamsBuilder
  ( StreamsBuilder
  , buildTopology
  , freshNodeName
  , newStreamsBuilder
  , withTopology_
  )
import qualified Kafka.Streams.StreamsBuilder as SB
import qualified Kafka.Streams.Suppress as Suppress
import qualified Kafka.Streams.TimeWindowedKStream as TWKS
import Kafka.Streams.Time (Duration)
import qualified Kafka.Streams.Time as Time
import qualified Kafka.Streams.Topology as Topo
import qualified Kafka.Streams.Topology.Free.Arrow as FA
import Kafka.Streams.Topology.Free.Arrow (FreeArrow (..))
import Kafka.Streams.Types (Record (..), TopicName, topicName)
import qualified Kafka.Streams.Window as Win

----------------------------------------------------------------------
-- Splits
----------------------------------------------------------------------

-- | One named branch of a 'Split'. The 'Text' name appears as the
-- 'Data.Map.Strict.Map' key in the result. Mirrors Java's
-- @Branched.as(name).withPredicate(p)@.
data SplitBranch k v = SplitBranch
  { sbName :: !Text
  , sbPred :: !(Record k v -> Bool)
  }

----------------------------------------------------------------------
-- 'Topology' = 'FreeArrow' over the Kafka 'Prim' GADT
----------------------------------------------------------------------

-- | A composable Kafka Streams topology fragment from input wire
-- type @i@ to output wire type @o@.
--
-- 'Topology' is built on top of the reusable 'FreeArrow'
-- framework in "Kafka.Streams.Topology.Free.Arrow":
--
-- @
-- type 'Topology' = 'FreeArrow' 'Prim'
-- @
--
-- The /framework/ supplies all the algebraic structure —
-- 'Category', 'Arrow', 'ArrowChoice', 'Applicative', 'Monad',
-- 'Semigroup', 'Monoid', plus lineage extras ('Fork', 'ForkN',
-- 'Tap') and the bind constructor. The Kafka-specific bits live
-- in the 'Prim' GADT below: sources, sinks, joins, aggregations,
-- the Processor API, etc.
type Topology i o = FreeArrow Prim i o

-- | The Kafka Streams /primitives/: one constructor per
-- DSL operation that compiles to a node in the topology graph.
-- These are the operations specific to Kafka Streams; the
-- algebraic combinators that connect them are provided once by
-- 'FreeArrow' and reused here.
--
-- Constructors are grouped by category. Each carries enough
-- arguments to feed the corresponding imperative-DSL call when
-- the topology is compiled.
data Prim i o where
  -- ------------------------------------------------------------------
  -- KIP-418 named branches
  -- ------------------------------------------------------------------

  -- | KIP-418 named branches. Each input record routes to the first
  -- branch whose predicate matches. Records that match none go to
  -- the default branch if supplied, otherwise are dropped. The
  -- output is a 'Map' from branch name to its 'KStream'.
  Split   :: ![SplitBranch k v]
          -> !(Maybe Text)
          -> Prim (KStream k v) (Map Text (KStream k v))

  -- ------------------------------------------------------------------
  -- Sources
  -- ------------------------------------------------------------------

  Source       :: !TopicName -> !(Consumed k v)
               -> Prim Void (KStream k v)
  SourceMulti  :: !(NonEmpty TopicName) -> !(Consumed k v)
               -> Prim Void (KStream k v)
  TableSource  :: Ord k
               => !TopicName -> !(Consumed k v) -> !(Materialized k v)
               -> Prim Void (KTable k v)
  GlobalSource :: Ord k
               => !TopicName -> !(Consumed k v) -> !(Materialized k v)
               -> Prim Void (GlobalKTable k v)

  -- ------------------------------------------------------------------
  -- Sinks
  -- ------------------------------------------------------------------

  Sink         :: !TopicName -> !(Produced k v)
               -> Prim (KStream k v) ()
  SinkExtracted
               :: !(KS.TopicNameExtractor k v) -> !(Produced k v)
               -> Prim (KStream k v) ()
  Through      :: !TopicName -> !(Produced k v)
               -> Prim (KStream k v) (KStream k v)
  -- | Riffle \xc2\xa74: two-phase-commit sink. Each record is staged
  -- into the sink's per-cycle buffer via 'tpsPrepare'; the
  -- runtime's 'EOSCoordinator.preCommit2PC' \/ 'commit2PC' /
  -- 'abort2PC' hooks drive the commit cycle. Wire the sink into
  -- the coordinator with 'Kafka.Streams.Sinks.TwoPhase.withTwoPhaseSinks'.
  SinkTwoPhase :: !(TPS.TwoPhaseSink (Record k v))
               -> Prim (KStream k v) ()

  -- ------------------------------------------------------------------
  -- Stateless 'KStream' transforms
  -- ------------------------------------------------------------------

  MapValues       :: HasSerde v'
                  => (v -> v')         -> Prim (KStream k v) (KStream k v')
  MapValuesM      :: HasSerde v'
                  => (v -> IO v')      -> Prim (KStream k v) (KStream k v')
  MapKeyValue     :: (HasSerde k', HasSerde v')
                  => (k -> v -> (k', v'))
                  -> Prim (KStream k v) (KStream k' v')
  MapKeyValueM    :: (HasSerde k', HasSerde v')
                  => (k -> v -> IO (k', v'))
                  -> Prim (KStream k v) (KStream k' v')
  -- | Riffle async transform on values. Runs @v -> IO v'@ on a
  -- bounded worker pool with ordered\/unordered output and an
  -- explicit failure policy. See 'Kafka.Streams.AsyncIO' for the
  -- full contract (backpressure, EOS, ordering, retry, drain).
  AsyncMapValues       :: HasSerde v'
                       => !AIO.AsyncIOConfig
                       -> (v -> IO v')
                       -> Prim (KStream k v) (KStream k v')
  -- | Riffle async transform on key + value. May rekey;
  -- downstream key + value serdes are resolved via 'HasSerde'.
  AsyncMapKeyValue     :: (HasSerde k', HasSerde v')
                       => !AIO.AsyncIOConfig
                       -> (k -> v -> IO (k', v'))
                       -> Prim (KStream k v) (KStream k' v')
  -- | Riffle async flat-map on values. Each input record yields
  -- zero or more outputs, computed on the worker pool. Within a
  -- single record, output ordering follows the returned list; across
  -- input records, output ordering follows 'aioOutputMode'.
  AsyncConcatMapValues   :: HasSerde v'
                       => !AIO.AsyncIOConfig
                       -> (v -> IO [v'])
                       -> Prim (KStream k v) (KStream k v')
  -- | Full-record pure transform: see key, value, timestamp,
  -- and headers; produce a fresh 'Record'. Use when you need
  -- access to header / timestamp data that 'MapValues' /
  -- 'MapKeyValue' don't expose.
  MapRecord       :: (HasSerde k', HasSerde v')
                  => (Record k v -> Record k' v')
                  -> Prim (KStream k v) (KStream k' v')
  -- | Effectful full-record transform. Same backpressure caveat
  -- as 'MapValuesM' — the 'IO' runs synchronously on the
  -- stream thread.
  MapRecordM      :: (HasSerde k', HasSerde v')
                  => (Record k v -> IO (Record k' v'))
                  -> Prim (KStream k v) (KStream k' v')
  Filter          :: (Record k v -> Bool) -> Prim (KStream k v) (KStream k v)
  FilterNot       :: (Record k v -> Bool) -> Prim (KStream k v) (KStream k v)
  ConcatMapValues :: HasSerde v'
                  => (v -> [v'])       -> Prim (KStream k v) (KStream k v')
  ConcatMapKeyValue
                  :: (HasSerde k', HasSerde v')
                  => (k -> v -> [(k', v')])
                  -> Prim (KStream k v) (KStream k' v')
  Peek            :: (Record k v -> IO ()) -> Prim (KStream k v) (KStream k v)
  Foreach         :: (Record k v -> IO ()) -> Prim (KStream k v) ()
  SelectKey       :: HasSerde k'
                  => (Record k v -> k')    -> Prim (KStream k v) (KStream k' v)
  Values          :: Prim (KStream k v) (KStream () v)
  Print           :: (Show k, Show v)
                  => !Text -> !(String -> IO ())
                  -> Prim (KStream k v) ()

  -- ------------------------------------------------------------------
  -- 'KStream' composition / branching
  -- ------------------------------------------------------------------

  Merge    :: Prim (KStream k v, KStream k v) (KStream k v)
  MergeAll :: Prim [KStream k v] (KStream k v)
  Branch   :: ![Record k v -> Bool]
           -> Prim (KStream k v) [KStream k v]

  -- ------------------------------------------------------------------
  -- Conversions
  -- ------------------------------------------------------------------

  ToTableT        :: Ord k => !(Materialized k v) -> Prim (KStream k v) (KTable k v)
  ToStream        :: Prim (KTable k v) (KStream k v)
  Repartition     :: !Text                  -> Prim (KStream k v) (KStream k v)
  RepartitionWith :: !(Rep.Repartitioned k v) -> Prim (KStream k v) (KStream k v)

  -- ------------------------------------------------------------------
  -- Grouping + aggregation
  -- ------------------------------------------------------------------

  GroupByKey :: !(Grouped k v) -> Prim (KStream k v) (KGroupedStream k v)
  GroupBy    :: (Record k v -> k') -> !(Grouped k' v)
             -> Prim (KStream k v) (KGroupedStream k' v)
  Count      :: Ord k => !(Materialized k Int64)
             -> Prim (KGroupedStream k v) (KTable k Int64)
  Reduce     :: Ord k => (v -> v -> v) -> !(Materialized k v)
             -> Prim (KGroupedStream k v) (KTable k v)
  Aggregate  :: Ord k => !(IO agg) -> (k -> v -> agg -> agg)
             -> !(Materialized k agg)
             -> Prim (KGroupedStream k v) (KTable k agg)

  -- ------------------------------------------------------------------
  -- Windowed aggregation
  -- ------------------------------------------------------------------

  WindowedByTime
    :: !Win.Windows -> Prim (KGroupedStream k v) (TimeWindowedKStream k v)
  WindowedBySession
    :: !Win.SessionWindows
    -> Prim (KGroupedStream k v) (SessionWindowedKStream k v)
  CountWindowed
    :: Ord k => !(Materialized k Int64)
    -> Prim (TimeWindowedKStream k v) (TWKS.WindowedTableHandle k Int64)
  ReduceWindowed
    :: Ord k => (v -> v -> v) -> !(Materialized k v)
    -> Prim (TimeWindowedKStream k v) (TWKS.WindowedTableHandle k v)
  AggregateWindowed
    :: Ord k => !(IO agg) -> (k -> v -> agg -> agg) -> !(Materialized k agg)
    -> Prim (TimeWindowedKStream k v) (TWKS.WindowedTableHandle k agg)
  CountSessionWindowed
    :: Ord k => !(Materialized k Int64)
    -> Prim (SessionWindowedKStream k v) (SWKS.SessionWindowedTableHandle k Int64)
  AggregateSessionWindowed
    :: Ord k
    => !(IO agg) -> (k -> v -> agg -> agg) -> (k -> agg -> agg -> agg)
    -> !(Materialized k agg)
    -> Prim (SessionWindowedKStream k v) (SWKS.SessionWindowedTableHandle k agg)

  -- ------------------------------------------------------------------
  -- KGroupedTable (subtractor-aware aggregation)
  -- ------------------------------------------------------------------

  GroupTableBy
    :: (Ord k, Ord k') => (k -> v -> (k', v')) -> !(Grouped k' v')
    -> Prim (KTable k v) (KGroupedTable k' v')
  CountKGroupedTable
    :: Ord k => !(Materialized k Int64)
    -> Prim (KGroupedTable k v) (KTable k Int64)
  ReduceKGroupedTable
    :: Ord k => (v -> v -> v) -> (v -> v -> v) -> !(Materialized k v)
    -> Prim (KGroupedTable k v) (KTable k v)
  AggregateKGroupedTable
    :: Ord k => !(IO agg) -> (k -> v -> agg -> agg) -> (k -> v -> agg -> agg)
    -> !(Materialized k agg)
    -> Prim (KGroupedTable k v) (KTable k agg)

  -- ------------------------------------------------------------------
  -- Cogroup
  -- ------------------------------------------------------------------

  Cogroup            :: (k -> v -> a -> a)
                     -> Prim (KGroupedStream k v) (CogroupedStream k a)
  AddCogrouped       :: (k -> v -> a -> a)
                     -> Prim (CogroupedStream k a, KGroupedStream k v) (CogroupedStream k a)
  AggregateCogrouped :: Ord k => !(IO a) -> !(Materialized k a)
                     -> Prim (CogroupedStream k a) (KTable k a)

  -- ------------------------------------------------------------------
  -- Joins
  -- ------------------------------------------------------------------

  StreamTableJoin
    :: (Ord k, HasSerde v')
    => (v -> vt -> v') -> !(Joined k v vt)
    -> Prim (KStream k v, KTable k vt) (KStream k v')
  StreamTableLeftJoin
    :: (Ord k, HasSerde v')
    => (v -> Maybe vt -> v') -> !(Joined k v vt)
    -> Prim (KStream k v, KTable k vt) (KStream k v')

  StreamStreamJoin
    :: (Ord k, HasSerde v')
    => (v1 -> v2 -> v') -> !JoinWindows -> !(Joined k v1 v2)
    -> Prim (KStream k v1, KStream k v2) (KStream k v')
  StreamStreamLeftJoin
    :: (Ord k, HasSerde v')
    => (v1 -> Maybe v2 -> v') -> !JoinWindows -> !(Joined k v1 v2)
    -> Prim (KStream k v1, KStream k v2) (KStream k v')
  StreamStreamOuterJoin
    :: (Ord k, HasSerde v')
    => (Maybe v1 -> Maybe v2 -> v') -> !JoinWindows -> !(Joined k v1 v2)
    -> Prim (KStream k v1, KStream k v2) (KStream k v')

  TableTableJoin
    :: Ord k => (v1 -> v2 -> v') -> !(Materialized k v')
    -> Prim (KTable k v1, KTable k v2) (KTable k v')
  TableTableLeftJoin
    :: Ord k => (v1 -> Maybe v2 -> v') -> !(Materialized k v')
    -> Prim (KTable k v1, KTable k v2) (KTable k v')
  TableTableOuterJoin
    :: Ord k => (Maybe v1 -> Maybe v2 -> v') -> !(Materialized k v')
    -> Prim (KTable k v1, KTable k v2) (KTable k v')

  ForeignKeyJoin
    :: (Ord k, Ord fk, Hashable v, HasSerde v')
    => (v -> fk) -> (v -> vr -> v') -> !(Materialized k v')
    -> Prim (KTable k v, KTable fk vr) (KTable k v')
  LeftForeignKeyJoin
    :: (Ord k, Ord fk, Hashable v, HasSerde v')
    => (v -> fk) -> (v -> Maybe vr -> v') -> !(Materialized k v')
    -> Prim (KTable k v, KTable fk vr) (KTable k v')

  StreamGlobalTableJoin
    :: (Ord kg, HasSerde v')
    => (k -> v -> kg) -> (v -> vg -> v')
    -> Prim (KStream k v, GlobalKTable kg vg) (KStream k v')
  StreamGlobalTableLeftJoin
    :: (Ord kg, HasSerde v')
    => (k -> v -> kg) -> (v -> Maybe vg -> v')
    -> Prim (KStream k v, GlobalKTable kg vg) (KStream k v')

  -- ------------------------------------------------------------------
  -- KTable surface
  -- ------------------------------------------------------------------

  FilterTable
    :: Ord k => (Record k v -> Bool) -> !(Materialized k v)
    -> Prim (KTable k v) (KTable k v)
  FilterNotTable
    :: Ord k => (Record k v -> Bool) -> !(Materialized k v)
    -> Prim (KTable k v) (KTable k v)
  MapValuesTable
    :: (Ord k, HasSerde v') => (v -> v') -> !(Materialized k v')
    -> Prim (KTable k v) (KTable k v')
  TransformValuesTable
    :: Ord k => !Text -> !(IO (Processor k v)) -> ![StoreName]
    -> !(Materialized k v')
    -> Prim (KTable k v) (KTable k v')

  -- ------------------------------------------------------------------
  -- Suppress
  -- ------------------------------------------------------------------

  SuppressUntilTimeLimit
    :: Ord k => !Duration -> Prim (KStream k v) (KStream k v)
  SuppressWindowedKS
    :: Ord k => !Duration -> !Int64
    -> Prim (KStream (WindowedKey k) v) (KStream (WindowedKey k) v)

  -- ------------------------------------------------------------------
  -- Processor API
  -- ------------------------------------------------------------------

  ProcessStream
    :: !Text -> ![StoreName] -> !(IO (Processor k v))
    -> Prim (KStream k v) ()
  ProcessValuesStream
    :: !Text -> ![StoreName] -> !(IO (Processor k v)) -> !(Serde v')
    -> Prim (KStream k v) (KStream k v')
  TransformValuesStreamT
    :: !Text -> ![Topo.NodeName] -> !(IO (Processor k v)) -> !(Serde v')
    -> Prim (KStream k v) (KStream k v')

  WithStateStoreKV :: !(StoreBuilderKV k v) -> ![Topo.NodeName] -> Prim x x
  WithStateStoreW  :: !(StoreBuilderW  k v) -> ![Topo.NodeName] -> Prim x x
  WithStateStoreS  :: !(StoreBuilderS  k v) -> ![Topo.NodeName] -> Prim x x

  ProcessWithStateStoreKV
    :: !Text -> !(StoreBuilderKV stk stv) -> !(IO (Processor k v))
    -> Prim (KStream k v) ()
  ProcessWithStateStoreW
    :: !Text -> !(StoreBuilderW stk stv) -> !(IO (Processor k v))
    -> Prim (KStream k v) ()
  ProcessWithStateStoreS
    :: !Text -> !(StoreBuilderS stk stv) -> !(IO (Processor k v))
    -> Prim (KStream k v) ()

  -- | Escape hatch — splice in a hand-rolled builder action.
  Lifted :: !Text -> (StreamsBuilder -> i -> IO o) -> Prim i o

  -- ------------------------------------------------------------------
  -- Fusion barrier
  -- ------------------------------------------------------------------

  -- | A runtime-identity that the AST optimiser /will not fuse
  -- across/. Adjacent primitives on either side of 'NoFuse' are
  -- left in their own processor nodes, even when a fusion rule
  -- in 'fuseStep' would otherwise collapse them.
  --
  -- Use this to keep an expensive 'MapValuesM' (e.g. one that
  -- calls an external service) isolated in its own processor
  -- so the runtime sees it as a distinct node — useful for
  -- monitoring, tracing, and scheduling reasoning. See the
  -- 'noFuse' smart constructor.
  NoFuse :: Prim a a

----------------------------------------------------------------------
-- Algebraic structure
--
-- 'Topology' is a type alias for @'FreeArrow' 'Prim'@, so every
-- 'Category', 'Arrow', 'ArrowChoice', 'Functor', 'Applicative',
-- 'Monad', 'Semigroup', and 'Monoid' instance comes from
-- "Kafka.Streams.Topology.Free.Arrow" for free. The combinators
-- below are convenience re-exports under topology-friendly
-- names.
----------------------------------------------------------------------

-- | Lift a 'Prim' constructor into a 'Topology' node.
liftPrim :: Prim i o -> Topology i o
liftPrim = FA.lift
{-# INLINE liftPrim #-}

-- | The input wire is the \"environment\" of the topology reader
-- monad. @askInput@ is just 'Cat.id'; the alias exists so
-- do-notation reads naturally.
askInput :: Topology i i
askInput = FA.askInputFA

-- | Run a topology with the input transformed by @f@. The
-- profunctor analogue of @MonadReader@'s 'Control.Monad.Reader.local'.
localInput :: (i' -> i) -> Topology i a -> Topology i' a
localInput = FA.localInputFA

-- | Pre-feed a fixed value into a topology, producing a
-- @'Topology' i b@ from a @'Topology' a b@ regardless of the
-- outer input type @i@.
applyT :: Topology a b -> a -> Topology i b
applyT = FA.applyValueFA

----------------------------------------------------------------------
-- Compilation
----------------------------------------------------------------------

-- | Type alias just to make signatures shorter at call sites.
type TBuilder = StreamsBuilder

-- | The open-ended AST interpreter. Walks a 'Topology' bottom-up,
-- compiling each leaf primitive against the shared
-- 'StreamsBuilder' (which holds the in-progress
-- 'Topo.Topology' graph) and threading the wire value @i@ down
-- the chain.
--
-- This is the function 'compile' uses internally after running
-- the optimiser. Exposed publicly so users can splice a
-- 'Topology' fragment into a hand-rolled imperative builder
-- chain — e.g. when migrating from the imperative DSL to the
-- typed GADT incrementally.
--
-- == Contract
--
--   * The 'StreamsBuilder' is the shared builder state — every
--     leaf primitive registers nodes / sinks / stores against
--     it via 'withTopology_'.
--   * The wire value @i@ is what feeds the AST. For
--     @Topology Void o@, @i@ is 'Data.Void.Void' and the
--     interpreter discharges it with a thunk that aborts
--     ('TopologyFreeError' / 'VoidInputForced') if any
--     Source primitive tries to inspect it — which by
--     construction they don't.
--   * The result @o@ is whatever the AST produces — a
--     'KStream', a 'KTable', a tuple, '()', etc.
--
-- /Does not/ run the optimiser. Apply 'optimize' yourself
-- before calling 'apply' if you want the rewrites.
apply :: forall i o. Topology i o -> TBuilder -> i -> IO o
apply t b = FA.interpretIO (runPrim b) t

-- | Interpret a single 'Prim' against the shared 'StreamsBuilder'.
-- This is the Kafka-specific half of 'apply'; the framework
-- half (composition, parallel, fanout, lineage, monad bind) is
-- handled by 'FA.interpretIO' for free.
runPrim :: TBuilder -> Prim i o -> i -> IO o
runPrim b = go
  where
    go :: forall x y. Prim x y -> x -> IO y
    -- Split / sources (Void input never inspected by sources).
    go (Split branches mDefault) = \s -> do
      let toBranched (SplitBranch nm p) = KS.branchedFrom nm p
      KS.splitStream (Prelude.map toBranched branches) mDefault s
    go (Source topic c)         = \_void -> KS.streamFromTopic b topic c
    go (SourceMulti topics c)   = \_void -> sourceMultiCompile b topics c
    go (TableSource topic c m)  = \_void -> KT.tableFromTopic b topic c m
    go (GlobalSource topic c m) = \_void -> GT.globalTable b topic c m

    -- Sinks
    go (Sink topic p)        = KS.toTopic topic p
    go (SinkExtracted e p)   = KS.toExtracted e p
    go (Through topic p)     = KS.throughTopic topic p
    go (SinkTwoPhase sink)   = TPS.compileSinkTwoPhase b sink

    -- Stateless
    go (MapValues f)         = KS.mapValues f
    go (MapValuesM f)        = KS.mapValuesM f
    go (MapKeyValue f)       = KS.mapKeyValue f
    go (MapKeyValueM f)      = KS.mapKeyValueM f
    go (AsyncMapValues cfg f)     = AIO.asyncMapValues cfg f
    go (AsyncMapKeyValue cfg f)   = AIO.asyncMapKeyValue cfg f
    go (AsyncConcatMapValues cfg f) = AIO.asyncConcatMapValues cfg f
    go (MapRecord f)         = KS.mapRecord f
    go (MapRecordM f)        = KS.mapRecordM f
    go NoFuse                = pure
    go (Filter p)            = KS.filterStream p
    go (FilterNot p)         = KS.filterNotStream p
    go (ConcatMapValues f)     = KS.concatMapValues f
    go (ConcatMapKeyValue f)   = KS.concatMapKeyValue f
    go (Peek f)              = KS.peekStream f
    go (Foreach f)           = KS.foreachStream f
    go (SelectKey f)         = KS.selectKey f
    go Values                = KS.valuesStream
    go (Print label putLine) = KS.printToHandle label putLine

    -- Composition / branching
    go Merge                 = \(s1, s2) -> KS.mergeStreams s1 s2
    go MergeAll              = KS.mergeStreamsN
    go (Branch ps)           = KS.branchStream ps

    -- Conversions
    go (ToTableT m)            = KS.toTable m
    go ToStream                = KS.toKStreamFromKTable
    go (Repartition prefix)    = KS.repartition prefix
    go (RepartitionWith cfg)   = KS.repartitionWith cfg

    -- Grouping + aggregation
    go (GroupByKey g)        = pure . KGS.groupByKey g
    go (GroupBy f g)         = KGS.groupByStream f g
    go (Count m)             = \g -> do
      h <- KGS.countStream m g
      pure (handleToKTable m h)
    go (Reduce f m)          = \g -> do
      h <- KGS.reduceStream f m g
      pure (handleToKTable m h)
    go (Aggregate seed step m) = \g -> do
      h <- KGS.aggregateStream seed step m g
      pure (handleToKTable m h)

    -- Windowed aggregation
    go (WindowedByTime ws)     = pure . KGS.windowedByTime ws
    go (WindowedBySession sw)  = pure . KGS.windowedBySession sw
    go (CountWindowed m)       = TWKS.countWindowed m
    go (ReduceWindowed f m)    = TWKS.reduceWindowed f m
    go (AggregateWindowed seed step m)
                               = TWKS.aggregateWindowed seed step m
    go (CountSessionWindowed m)
                               = SWKS.countSessionWindowed m
    go (AggregateSessionWindowed seed step merger m)
                               = SWKS.aggregateSessionWindowed seed step merger m

    -- KGroupedTable
    go (GroupTableBy f g)      = pure . KGT.groupTableBy f g
    go (CountKGroupedTable m)  = \kgt -> do
      h <- KGT.countKGroupedTable m kgt
      pure (handleToKTable m h)
    go (ReduceKGroupedTable add sub m) = \kgt -> do
      h <- KGT.reduceKGroupedTable add sub m kgt
      pure (handleToKTable m h)
    go (AggregateKGroupedTable seed add sub m) = \kgt -> do
      h <- KGT.aggregateKGroupedTable seed add sub m kgt
      pure (handleToKTable m h)

    -- Cogroup
    go (Cogroup step)              = pure . (\g -> Cog.cogroup g step)
    go (AddCogrouped step)         = \(cs, g) -> pure (Cog.addCogrouped cs g step)
    go (AggregateCogrouped seed m) = \cs -> do
      h <- Cog.aggregateCogrouped seed m cs
      pure (handleToKTable m h)

    -- Joins
    go (StreamTableJoin j jo)
      = \(s, t') -> KS.joinKStreamKTable j jo s t'
    go (StreamTableLeftJoin j jo)
      = \(s, t') -> KS.leftJoinKStreamKTable j jo s t'
    go (StreamStreamJoin j w jo)
      = \(s1, s2) -> KS.joinKStreamKStream j w jo s1 s2
    go (StreamStreamLeftJoin j w jo)
      = \(s1, s2) -> KS.leftJoinKStreamKStream j w jo s1 s2
    go (StreamStreamOuterJoin j w jo)
      = \(s1, s2) -> KS.outerJoinKStreamKStream j w jo s1 s2
    go (TableTableJoin j m)
      = \(t1, t2) -> KT.joinKTableKTable j m t1 t2
    go (TableTableLeftJoin j m)
      = \(t1, t2) -> KT.leftJoinKTableKTable j m t1 t2
    go (TableTableOuterJoin j m)
      = \(t1, t2) -> KT.outerJoinKTableKTable j m t1 t2
    go (ForeignKeyJoin ext j m)
      = \(t1, t2) -> FK.foreignKeyJoinKTable ext j m t1 t2
    go (LeftForeignKeyJoin ext j m)
      = \(t1, t2) -> FK.leftForeignKeyJoinKTable ext j m t1 t2
    go (StreamGlobalTableJoin km j)
      = \(s, g) -> GT.joinKStreamGlobalKTable km j s g
    go (StreamGlobalTableLeftJoin km j)
      = \(s, g) -> GT.leftJoinKStreamGlobalKTable km j s g

    -- KTable surface
    go (FilterTable p m)             = KT.filterTable p m
    go (FilterNotTable p m)          = KT.filterNotTable p m
    go (MapValuesTable f m)          = KT.mapValuesTable f m
    go (TransformValuesTable _nm sup stores m)
                                     = KT.transformValuesTable sup stores m

    -- Suppress
    go (SuppressUntilTimeLimit lim)  = Suppress.suppressUntilTimeLimit lim
    go (SuppressWindowedKS grace sz) = Suppress.suppressWindowed grace sz

    -- Processor API
    go (ProcessStream nm stores sup)
                                     = KS.processStream nm stores sup
    go (ProcessValuesStream nm stores sup vs)
                                     = KS.processValuesStream nm stores sup vs
    go (TransformValuesStreamT nm stores sup vs)
                                     = KS.transformValuesStream nm stores sup vs

    go (WithStateStoreKV builder owners) = \x -> do
      withTopology_ b (Topo.addStateStoreKV builder owners)
      pure x
    go (WithStateStoreW builder owners)  = \x -> do
      withTopology_ b (Topo.addStateStoreW builder owners)
      pure x
    go (WithStateStoreS builder owners)  = \x -> do
      withTopology_ b (Topo.addStateStoreS builder owners)
      pure x

    go (ProcessWithStateStoreKV prefix builder supplier) = \s -> do
      attachProcessorWithStore
        b s prefix supplier
        (Store.sbKvName builder)
        (Topo.addStateStoreKV builder)
    go (ProcessWithStateStoreW prefix builder supplier) = \s -> do
      attachProcessorWithStore
        b s prefix supplier
        (Store.sbWName builder)
        (Topo.addStateStoreW builder)
    go (ProcessWithStateStoreS prefix builder supplier) = \s -> do
      attachProcessorWithStore
        b s prefix supplier
        (Store.sbSName builder)
        (Topo.addStateStoreS builder)

    -- Escape
    go (Lifted _ act)        = act b

-- | Attach a custom 'Processor' to a 'KStream' while atomically
-- registering the state store it owns. The fresh processor node
-- name is the only thing that gets generated; the store is
-- registered against that same node so the validator's
-- store-ownership check passes without callers needing to know
-- the generated name.
--
-- The @attachStore@ function is whichever of
-- 'Topo.addStateStoreKV' / 'Topo.addStateStoreW' /
-- 'Topo.addStateStoreS' is appropriate. The @storeNm@ is the
-- store's name (used to populate 'processorSpecStores').
attachProcessorWithStore
  :: TBuilder
  -> KStream k v
  -> Text                                            -- ^ prefix
  -> IO (Processor k v)                              -- ^ supplier
  -> StoreName                                       -- ^ store name
  -> ([Topo.NodeName] -> Topo.Topology -> Topo.Topology)
  -- ^ topology-level @addStateStore*@ for the store's type
  -> IO ()
attachProcessorWithStore b s prefix supplier storeNm attachStore = do
  nm <- freshNodeName b prefix
  withTopology_ b $ \topo ->
    let !topo1 = Topo.addProcessorWith
                   Topo.ProcessorSpec
                     { Topo.processorSpecName     = nm
                     , Topo.processorSpecParents  = [KS.kstreamParent s]
                     , Topo.processorSpecSupplier = Topo.AnyProcessor supplier
                     , Topo.processorSpecStores   = [storeNm]
                     }
                   topo
        !topo2 = attachStore [nm] topo1
     in topo2

-- | Multi-topic source helper. The existing
-- 'Kafka.Streams.KStream.streamFromTopic' takes a single 'TopicName';
-- the multi-topic variant calls the low-level
-- 'Kafka.Streams.Topology.addSource' directly and constructs the
-- same 'KStream' handle.
--
-- The runtime's source-topic set is a single list per source node, so
-- the multi-topic case is the natural shape; the single-topic
-- 'streamFromTopic' is just sugar over it. The helper takes a
-- 'NonEmpty' rather than '[]' so the empty-list case isn't even
-- representable — calling this with an empty 'NonEmpty' is a type
-- error, eliminating the partial-function pitfall.
sourceMultiCompile
  :: TBuilder
  -> NonEmpty TopicName
  -> Consumed k v
  -> IO (KStream k v)
sourceMultiCompile b ts c = do
  nm <- freshNodeName b "KSTREAM-SOURCE-MULTI"
  withTopology_ b $
    Topo.addSourceWith
      Topo.SourceSpec
        { Topo.sourceName        = nm
        , Topo.sourceTopics      = NE.toList ts
        , Topo.sourceKeySerde    = Topo.AnySerde (consumedKeySerde c)
        , Topo.sourceValueSerde  = Topo.AnySerde (consumedValueSerde c)
        , Topo.sourceExtractor   = Topo.AnyTimestampExtractor (consumedExtractor c)
        , Topo.sourceOffsetReset = consumedOffsetReset c
        , Topo.sourcePattern     = Nothing
        , Topo.sourceWatermarkStrategy = consumedWatermark c
        }
  pure (KS.KStream
          { KS.kstreamBuilder    = b
          , KS.kstreamParent     = nm
          , KS.kstreamKeySerde   = consumedKeySerde c
          , KS.kstreamValueSerde = consumedValueSerde c
          })

-- | Promote an aggregation handle to a real 'KTable'.
--
-- The 'CountedTableLocal' handle the existing DSL returns from
-- aggregations is a thin wrapper around a 'KTable' — it just
-- doesn't carry the serdes. We extract the serdes from the
-- supplied 'Materialized'. If they're absent the corresponding
-- 'KTable' field becomes a /deferred/ 'TopologyFreeError' —
-- matching the existing 'KTable' lazy-serde behaviour after
-- @mapValues@ etc. Downstream @to@\/@join@ that forces the
-- serde will raise the exception at the actual use site.
--
-- Unlike a bare 'error', the deferred thunk uses
-- 'TopologyFreeError' so callers who want to surface friendlier
-- diagnostics can catch it (via 'Control.Exception.evaluate' on
-- the thunk, or by wrapping the downstream operation in
-- 'Control.Exception.try').
handleToKTable
  :: Materialized k v
  -> KGS.CountedTableLocal k v
  -> KTable k v
handleToKTable m h = KT.KTable
  { KT.ktableNode       = KGS.ctlNode h
  , KT.ktableStore      = KGS.ctlStore h
  , KT.ktableBuilder    = KGS.ctlBuilder h
  , KT.ktableKeySerde   =
      case Mat.matKeySerde m of
        Just s  -> s
        Nothing -> Exception.throw (MissingMaterializedSerde KeySide)
  , KT.ktableValueSerde =
      case Mat.matValueSerde m of
        Just s  -> s
        Nothing -> Exception.throw (MissingMaterializedSerde ValueSide)
  }

-- | Whether the missing serde was the key or value side of the
-- 'Materialized'.
data SerdeSide = KeySide | ValueSide
  deriving stock (Eq, Show, Generic)

-- | The (small) set of partial conditions the Free topology builder
-- can land in. All currently land /lazily/: they aren't raised at
-- AST-construction time, only when a downstream operation forces
-- the affected field (typically @to@, @join@, or @repartition@
-- asking for a serde, or the runtime asking for a Void input on a
-- topology that bypassed the smart constructors).
--
-- Because 'TopologyFreeError' is an 'Exception.Exception' callers
-- can catch it via 'Control.Exception.try' \/
-- 'Control.Exception.evaluate', rather than the bare 'error' calls
-- that preceded this type. Pattern-match on the constructor to
-- decide whether the user can fix the call site
-- ('MissingMaterializedSerde') or it's a library bug
-- ('VoidInputForced').
data TopologyFreeError
  = -- | An aggregation result was promoted to a 'KTable' but the
    -- supplied 'Materialized' didn't carry the named serde. Supply
    -- one via @'Mat.withKeySerde'@ or @'Mat.withValueSerde'@ when
    -- constructing the 'Materialized', /or/ ensure the downstream
    -- operation that forced the serde gets it through @Produced@
    -- (for sinks) \/ @Joined@ (for joins) instead of relying on
    -- the @KTable@'s embedded serde.
    MissingMaterializedSerde !SerdeSide
  | -- | A 'Source' (or any closed-input) primitive inspected its
    -- 'Void' input. By construction sources never look at the
    -- void; if you see this exception, please file a bug. The
    -- 'Text' is the location label from the compile entry point.
    VoidInputForced !Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Exception.Exception)

-- | Compile a closed-input topology into the existing
-- 'Kafka.Streams.Topology.Topology' graph.
--
-- 'compile' is the canonical "AST → runnable topology" entry
-- point. It:
--
--   1. Runs the supplied AST through the default rewrite passes
--      ('defaultOptimizeConfig') — see "Kafka.Streams.Topology.Free.Optimize"
--      for the full list. The rewrites are semantics-preserving;
--      the result has fewer processor nodes but identical
--      observable behaviour.
--   2. Creates a fresh 'StreamsBuilder' and walks the optimised
--      AST, registering each leaf primitive (sources, sinks,
--      transforms, joins, aggregators, state stores) against
--      the builder.
--   3. Snapshots the builder into a 'Topo.Topology' graph and
--      returns it alongside the AST's output wire value.
--
-- The output @o@ is whatever the AST says it is — typically @()@
-- for a sink-closed pipeline, but a freshly-compiled 'KStream'
-- / 'KTable' / 'CogroupedStream' is also valid (useful when
-- you want to splice the resulting wire value into a
-- hand-rolled imperative chain afterwards).
--
-- == Variants
--
--   * 'compileNoOptimize' — same shape but skips the rewrite
--     passes. Use for golden-file tests where you need
--     node-for-node correspondence between the AST and the
--     compiled graph.
--   * 'compileWithOptimization' — explicit 'OptimizeConfig' for
--     per-pass control (toggle individual rewrite families).
--   * 'compileInBuilder' — compile against a pre-existing
--     'StreamsBuilder' for splicing into an imperative
--     pipeline.
compile :: Topology Void o -> IO (o, Topo.Topology)
compile = compileWithOptimization defaultOptimizeConfig

-- | Like 'compile' but skips the rewrite passes entirely
-- (applies 'noOptimization'). Use when:
--
--   * You need node-for-node correspondence between the
--     'Topology' AST and the compiled 'Topo.Topology' graph
--     (golden-file tests).
--   * You're debugging an unexpected optimiser interaction
--     and want to confirm the un-optimised path behaves
--     correctly.
--   * You're benchmarking the un-optimised path to
--     quantify the optimiser's win.
--
-- The /default/ should be 'compile'. Reach for
-- 'compileNoOptimize' only when one of the above applies.
compileNoOptimize :: Topology Void o -> IO (o, Topo.Topology)
compileNoOptimize = compileWithOptimization noOptimization

-- | 'compile' with an explicit 'OptimizeConfig'. Use when:
--
--   * You want to enable /one/ rewrite family for an A\/B
--     comparison (toggle individual @optFuse*@ flags).
--   * You want to /disable/ a specific rewrite family that's
--     interacting with caller-side assumptions (e.g. a
--     bespoke optimiser pass downstream that wants
--     un-fused @Filter@ nodes).
--   * You want to extend 'optMaxPasses' for an unusually
--     deep AST.
--
-- For the default everything-enabled or everything-disabled
-- cases, use 'compile' or 'compileNoOptimize'.
compileWithOptimization
  :: OptimizeConfig -> Topology Void o -> IO (o, Topo.Topology)
compileWithOptimization cfg t = do
  b <- newStreamsBuilder
  let !t' = optimizeWith cfg t
  o <- apply t' b voidInput
  topo <- buildTopology b
  -- After the AST-level rewrites have collapsed adjacent
  -- operators in the 'Topology' GADT, hand the resulting Kafka
  -- 'Topo.Topology' graph to the graph-level optimiser. Today
  -- 'Topo.optimizeTopology' is a no-op placeholder (it returns
  -- its input unchanged), but as of the KIP-295 implementation
  -- it now performs:
  --
  --   * 'optReuseSourceKTable' — for table-source-style
  --     processors (one source parent + one owned store), point
  --     the store's changelog at the source topic.
  --   * 'optMergeRepartitionTopics' — collapse sibling
  --     @KSTREAM-REPARTITION-\<prefix\>-…@ processors sharing
  --     a single parent and prefix into one shared node.
  --
  -- Both rewrites are toggleable via 'Topo.OptimizationConfig';
  -- the default config enables both. Callers that want the
  -- un-optimised graph can substitute @'Topo.optimizeTopology'
  -- 'Topo.noOptimisations'@ here.
  let !topo' = Topo.optimizeTopology Topo.defaultOptimizationConfig topo
  pure (o, topo')
  where
    -- By construction every 'Topology Void _' is rooted in one of
    -- the source constructors, and none of them inspect the input
    -- value. The thunk below is therefore unreachable code; if it
    -- ever fires it's because someone bypassed the smart
    -- constructors (e.g. via 'unsafeCoerce' or a custom 'Lifted'
    -- handler that destructures Void). We throw a typed
    -- 'TopologyFreeError' rather than a bare 'error' so callers
    -- can catch it and report at their preferred granularity.
    voidInput :: Void
    voidInput = Exception.throw (VoidInputForced "compileWithOptimization")

-- | Compile a closed-input 'Topology' against a pre-existing
-- 'StreamsBuilder'. Useful when splicing a 'Topology' into a
-- pipeline being built imperatively through
-- "Kafka.Streams.StreamsBuilder" — the imperative chain holds
-- the builder, and 'compileInBuilder' adds the AST's nodes to
-- the same builder.
--
-- /Does not/ run the optimiser — when splicing into an
-- existing imperative graph callers usually want node-for-node
-- correspondence between the 'Topology' AST and the resulting
-- builder nodes (the imperative graph has its own naming
-- conventions; fusion would change those).
--
-- Returns just the AST output value (not the topology graph)
-- because the caller is managing the builder externally.
compileInBuilder :: TBuilder -> Topology Void o -> IO o
compileInBuilder b t = apply t b voidInput
  where
    voidInput :: Void
    voidInput = Exception.throw (VoidInputForced "compileInBuilder")

-- | Deprecated alias for 'compileInBuilder'. Kept so existing
-- call sites don't break; new code should use
-- 'compileInBuilder' directly.
compileWith :: TBuilder -> Topology Void o -> IO o
compileWith = compileInBuilder

-- | Compile a closed-output topology and return only the
-- resulting 'Topo.Topology' graph, discarding the AST output
-- (which is @()@ for sink-closed pipelines).
--
-- Convenience wrapper around 'compile' for the common case of
-- "I just want the topology graph, ready to feed into a
-- 'newDriver' or 'KafkaStreams' runtime."
buildTopologyFrom :: Topology Void () -> IO Topo.Topology
buildTopologyFrom = fmap snd . compile

----------------------------------------------------------------------
-- Smart constructors
----------------------------------------------------------------------

-- * Sources --------------------------------------------------------
--
-- Every closed 'Topology' starts with at least one source: a
-- pipeline that doesn't start with a 'Source' (or one of the
-- table / global variants) can't be closed on the input side,
-- because all source primitives have @'Void'@ as their input
-- type — the only way to discharge the open @Void@ slot is to
-- pin a source there. (Composing a source on the right
-- automatically lifts the whole pipeline to @Topology Void o@.)

-- | Subscribe to a Kafka topic and emit records as a
-- 'KStream'. Uses the default 'Consumed' configuration:
-- /record-timestamp/ timestamp extractor, /earliest/
-- offset-reset, no explicit node name.
--
-- /JVM equivalent:/ @StreamsBuilder.stream(topic)@.
--
-- The 'Serde's deserialise records as the runtime polls
-- them; downstream operators see the typed @'KStream' k v@.
-- For non-default 'Consumed' (e.g. custom timestamp
-- extractor, latest offset reset) use 'sourceWith'. For
-- multi-topic sources, see 'sources'.
source
  :: forall k v. (HasSerde k, HasSerde v)
  => Text -> Topology Void (KStream k v)
source t = sourceWith (topicName t) (consumed (serde @k) (serde @v))

-- | Subscribe to a Kafka topic with a fully-specified
-- 'Consumed'.
--
-- /JVM equivalent:/ @StreamsBuilder.stream(topic, Consumed)@.
--
-- The 'Consumed' carries the key and value serdes, the
-- timestamp extractor (use 'Consumed.withTimestampExtractor'
-- to override), and the offset-reset policy. The default
-- 'Consumed' (via 'source') uses record-timestamp +
-- earliest-offset; reach for 'sourceWith' when you need to
-- override either of those, or to pin an explicit
-- source-node name via 'Consumed.withName'.
sourceWith :: TopicName -> Consumed k v -> Topology Void (KStream k v)
sourceWith t c = liftPrim (Source t c)

-- | Subscribe to N topics simultaneously, fanning them into
-- a single 'KStream'. All topics must use the same key and
-- value serdes (records from different topics are
-- indistinguishable downstream — use 'tap' / 'peek' on
-- 'recordHeaders' if you need to track which topic a record
-- came from).
--
-- /JVM equivalent:/ @StreamsBuilder.stream(Collection<String>)@.
--
-- Use this for fan-in patterns where multiple upstream
-- producers feed a single logical topic — e.g. per-region
-- topics merged into one regionless pipeline. For two
-- /different/ source-rooted pipelines that should share EOS,
-- use 'mergeSourced' instead (which keeps the value types
-- separate up to the merge point).
sources
  :: forall k v. (HasSerde k, HasSerde v)
  => NonEmpty Text -> Topology Void (KStream k v)
sources ts = liftPrim
  (SourceMulti (fmap topicName ts) (consumed (serde @k) (serde @v)))

-- | 'sources' with a fully-specified 'Consumed' (e.g. custom
-- offset-reset policy, named source node, custom timestamp
-- extractor). Mirrors the relationship between 'source' and
-- 'sourceWith'.
sourcesWith
  :: NonEmpty TopicName -> Consumed k v -> Topology Void (KStream k v)
sourcesWith ts c = liftPrim (SourceMulti ts c)

-- | Regex pattern source: subscribe to every broker topic
-- matching the supplied regex (JVM @StreamsBuilder.stream(Pattern)@).
--
-- Useful for fan-in across a dynamic set of topics that share a
-- naming convention (e.g. @per-tenant-*@). The in-process
-- 'TopologyTestDriver' doesn't enumerate broker topics, so
-- within the driver the pattern source is equivalent to a
-- no-topic source — pipe records in via the explicit
-- 'pipeInput' interface. Against a real broker the runtime
-- resolves topics at subscribe time.
sourcePattern
  :: forall k v. (HasSerde k, HasSerde v)
  => Text -> Topology Void (KStream k v)
sourcePattern pat =
  liftIO_ "sourcePattern" $ \b _void -> do
    KS.streamFromPattern b pat (consumed (serde @k) (serde @v))

-- | 'sourcePattern' with a fully-specified 'Consumed' (custom
-- offset-reset policy, named source node, custom timestamp
-- extractor).
sourcePatternWith
  :: Text -> Consumed k v -> Topology Void (KStream k v)
sourcePatternWith pat c =
  liftIO_ "sourcePattern-with" $ \b _void ->
    KS.streamFromPattern b pat c

-- | Subscribe to a Kafka topic and materialise it as a
-- 'KTable' (latest-value-per-key) backed by an in-memory
-- state store.
--
-- /JVM equivalent:/ @StreamsBuilder.table(topic)@.
--
-- Tombstone records (records with null value) delete the
-- key from the store; non-tombstones insert or update. The
-- resulting 'KTable' can be queried via the standard
-- Interactive Queries API, joined against, or converted
-- back into a 'KStream' via 'toStream'.
--
-- For finer control over the materialised store (named
-- store, custom serdes, RocksDB backend, etc.) call the
-- 'TableSource' constructor directly or fall back to
-- 'liftIO_' with the imperative @tableFromTopic@.
tableSource
  :: forall k v. (Ord k, HasSerde k, HasSerde v)
  => Text -> Topology Void (KTable k v)
tableSource t = tableSourceWith (topicName t)
  (consumed (serde @k) (serde @v))
  (Mat.withValueSerde (serde @v)
    (Mat.withKeySerde (serde @k) Mat.materialized))

-- | 'tableSource' with explicit 'Consumed' and 'Materialized'
-- records. Use this when you need finer control over the source
-- node (offset reset, timestamp extractor) or the backing store
-- (named store, custom retention, RocksDB backend).
tableSourceWith
  :: Ord k
  => TopicName -> Consumed k v -> Materialized k v
  -> Topology Void (KTable k v)
tableSourceWith t c m = liftPrim (TableSource t c m)

-- | Subscribe to a Kafka topic and materialise it as a
-- /global/ 'GlobalKTable' — a cluster-wide replicated table.
--
-- /JVM equivalent:/ @StreamsBuilder.globalTable(topic)@.
--
-- Unlike 'tableSource', a 'GlobalKTable' is /not/
-- partitioned: every Kafka Streams instance maintains its
-- own full copy of the table. That makes it ideal for
-- "lookup table" patterns — see 'streamGlobalTableJoin' /
-- 'streamGlobalTableLeftJoin' for the join shape, which
-- /doesn't/ require co-partitioning.
--
-- The memory cost is proportional to the full table size on
-- every instance; use only for small-to-moderate reference
-- data (customer details, product catalogs, feature flags)
-- rather than high-volume transactional data.
globalTableSource
  :: Ord k => Text -> Serde k -> Serde v -> Topology Void (GlobalKTable k v)
globalTableSource t ks vs =
  liftPrim (GlobalSource (topicName t) (consumed ks vs)
    (Mat.withValueSerde vs (Mat.withKeySerde ks Mat.materialized)))

-- | Merge two source-rooted streams into a single downstream lineage
-- so they share a Kafka task (and therefore an EOS transaction).
--
-- Operationally identical to:
--
-- @
-- (left '&&&' right) '>>>' 'merge'
-- @
--
-- but named for the use case it enables: cross-source
-- exactly-once atomicity. Without the convergence at 'merge',
-- the Kafka task assigner sees two disconnected sub-topologies
-- and groups them into /separate/ tasks — each with its own EOS
-- coordinator and its own commit cycle.
--
-- == When to use this
--
-- Use 'mergeSourced' whenever you want two independent source
-- topics to be processed in a single transaction. The merged
-- output is a single 'KStream' carrying records from both — feed
-- it into your downstream pipeline as usual.
--
-- For sources with /different/ value types, merge after a value
-- map that brings them to a common shape, e.g.:
--
-- @
-- mergeSourced
--   (F.source "a" ks vsA '>>>' F.mapValues 'Left')
--   (F.source "b" ks vsB '>>>' F.mapValues 'Right')
-- @
--
-- For shared-state EOS without value merging (each source still
-- writes to its own output topic but they share an EOS
-- transaction because they share a state store), use
-- 'withStateStoreKV' with the same store registered against
-- both subgraphs' processors.
mergeSourced
  :: Topology Void (KStream k v)
  -> Topology Void (KStream k v)
  -> Topology Void (KStream k v)
mergeSourced left right = Compose (liftPrim Merge) (Fanout left right)

-- * Sinks ----------------------------------------------------------
--
-- A sink closes off a 'KStream' lineage by sending its records
-- to a Kafka topic. The output wire is @()@ — there's no
-- downstream continuation. If you need to /both/ sink the
-- records and continue processing them, use 'through' (which
-- sinks and re-subscribes from the broker) or 'tap' (which
-- runs the sink as a side pipeline without leaving the
-- in-process lineage).

-- | Publish every record on the stream to the given topic.
-- The 'Serde's serialise the key and value as records are
-- produced.
--
-- /JVM equivalent:/ @KStream.to(topic, Produced.with(ks, vs))@.
--
-- This is the canonical pipeline-closing operation. Under
-- EOS the writes go through the bound transactional
-- producer and commit atomically with the task's offset
-- commit. Use 'sinkWith' if you need to override the
-- partitioner or the auto-generated sink-node name.
sink
  :: forall k v. (HasSerde k, HasSerde v)
  => Text -> Topology (KStream k v) ()
sink t = sinkWith (topicName t) (produced (serde @k) (serde @v))

-- | 'sink' with a fully-specified 'Produced'. Use this when you
-- need to override the default sink-node name, supply a custom
-- partitioner, or otherwise reach into the lower-level
-- @Produced@ configuration. For the common case of "publish
-- with these serdes", prefer 'sink'.
sinkWith :: TopicName -> Produced k v -> Topology (KStream k v) ()
sinkWith t p = liftPrim (Sink t p)

-- | Riffle \xc2\xa74: terminate the stream into a two-phase-commit sink.
-- Every record is staged via 'TPS.tpsStage'; the runtime's
-- 'Kafka.Streams.Runtime.EOS.EOSCoordinator' drives prepare /
-- commit / abort through the 5-step cycle once the sink has
-- been wired in via 'TPS.withTwoPhaseSinks'. The same
-- contract works for JDBC \/ Iceberg \/ S3 \/ HTTP adapters
-- shipped in separate packages.
sinkTwoPhase
  :: TPS.TwoPhaseSink (Record k v)
  -> Topology (KStream k v) ()
sinkTwoPhase = liftPrim . SinkTwoPhase

-- | Per-record dynamic-topic sink (KIP-303). The supplied
-- 'KS.TopicNameExtractor' is consulted for every record and
-- decides which topic that record lands in. The wire is closed
-- ('()' output).
--
-- /JVM equivalent:/ @KStream.to(TopicNameExtractor, Produced)@.
--
-- Use when you're routing records to one of many topics based
-- on record content — e.g. per-tenant topics, per-region topics
-- — without enumerating them in the topology itself.
sinkExtracted
  :: KS.TopicNameExtractor k v -> Produced k v -> Topology (KStream k v) ()
sinkExtracted e p = liftPrim (SinkExtracted e p)

-- | Publish to a topic /and/ immediately re-subscribe from it,
-- returning a fresh upstream-equivalent 'KStream' that flows
-- through the broker. Mirrors @KStream.through@.
--
-- == When to use
--
-- 'through' is the canonical way to introduce a /task boundary/:
-- the records get serialised into the topic, durably stored on
-- the broker, and re-fetched on the downstream side. Use it
-- when you want:
--
--   * a re-partition with explicit broker storage in between,
--   * a fault-tolerance boundary between expensive upstream and
--     downstream work,
--   * to expose an intermediate result as a real Kafka topic
--     that other consumers can observe.
--
-- Note that 'through' is a /sink + source pair/, so it costs
-- one round-trip through the broker. If you don't need the
-- durability, use 'repartition' (logical re-partition, broker
-- still involved but no extra read) or stay in-process.
through
  :: forall k v. (HasSerde k, HasSerde v)
  => Text -> Topology (KStream k v) (KStream k v)
through t =
  throughWith (topicName t) (produced (serde @k) (serde @v))

-- | 'through' with an explicit 'Produced' record. Use when you
-- need to override the sink-side configuration (custom
-- partitioner, sink-node name, production-exception handler).
-- The repartition topic must already exist on the broker (or
-- be auto-created); the in-process driver materialises it
-- on demand.
throughWith
  :: TopicName -> Produced k v
  -> Topology (KStream k v) (KStream k v)
throughWith t p = liftPrim (Through t p)

-- * Stateless 'KStream' transforms --------------------------------
--
-- The stateless transforms apply a pure (or @IO@) function to
-- every record and produce a fresh 'KStream' downstream. None of
-- them require a state store, none of them block on broker
-- I/O, and all are individually exposed as their own processor
-- nodes in the compiled topology (subject to the optimiser
-- fusing adjacent same-shape transforms — see
-- 'OptimizeConfig').
--
-- == Repartition implications
--
-- Transforms that change the /key/ ('mapKeyValue', 'mapKeyValueM',
-- 'concatMapKeyValue', 'selectKey') mark the stream as
-- /needing repartition/ for any subsequent stateful operation
-- (groupBy, join, aggregate). Use 'repartition' explicitly when
-- you want to force the shuffle at a particular point — Kafka's
-- broker-side partitioning is what guarantees per-key ordering,
-- so re-keying without repartitioning would put records for the
-- same key on different tasks.

-- | Apply a pure function to the value side of every record,
-- preserving the key, timestamp, and headers.
--
-- /JVM equivalent:/ @KStream.mapValues(ValueMapper)@.
--
-- Use 'mapValues' over 'mapKeyValue' whenever you don't need to
-- change the key — keeping the key stable means downstream
-- stateful operations don't trigger a repartition.
--
-- The optimiser fuses chains of 'mapValues' into one
-- @MapValues (compose ...)@ node, so writing small,
-- single-responsibility maps is cheap at run time.
mapValues :: HasSerde v' => (v -> v') -> Topology (KStream k v) (KStream k v')
mapValues = liftPrim . MapValues

-- | Effectful 'mapValues': the value-transform runs in 'IO',
-- so it can read from external resources, emit metrics, or
-- otherwise side-effect per record. The 'IO' action runs
-- /synchronously on the stream thread/ — slow handlers
-- backpressure the broker poll loop.
--
-- /JVM equivalent:/ no direct match; closest is
-- @ValueMapper@ + an out-of-band side channel. We model the
-- @IO@ in the type so the cost is visible at the call site.
--
-- For purely-stateless effects (logging, metrics) prefer
-- 'peek' (which doesn't mutate the value). For effects that
-- need exactly-once semantics, route through a state store
-- and gate on an idempotency token (the same caveat applies
-- to JVM @transformValues@).
mapValuesM
  :: HasSerde v'
  => (v -> IO v') -> Topology (KStream k v) (KStream k v')
mapValuesM = liftPrim . MapValuesM

-- | Re-key /and/ re-value every record with a pure function.
--
-- /JVM equivalent:/ @KStream.map(KeyValueMapper)@.
--
-- == Repartition warning
--
-- 'mapKeyValue' changes the record key, which means downstream
-- stateful operations (groupBy, join, aggregate) need a
-- 'repartition' to re-shuffle records onto the correct
-- partitions. The runtime doesn't auto-insert this — call
-- 'repartition' explicitly when the next stage is stateful,
-- /unless/ you've maintained the partition assignment (i.e.
-- the new key has the same hash as the old).
--
-- If you only need to change the /key/ and not the value,
-- prefer 'selectKey'. If you only need to change the value,
-- prefer 'mapValues' (avoids the repartition flag).
mapKeyValue
  :: (HasSerde k', HasSerde v')
  => (k -> v -> (k', v')) -> Topology (KStream k v) (KStream k' v')
mapKeyValue = liftPrim . MapKeyValue

-- | Effectful 'mapKeyValue'. Same trade-offs as 'mapValuesM'
-- (synchronous on the stream thread, slow handlers
-- backpressure) plus the repartition implications of
-- 'mapKeyValue'.
mapKeyValueM
  :: (HasSerde k', HasSerde v')
  => (k -> v -> IO (k', v')) -> Topology (KStream k v) (KStream k' v')
mapKeyValueM = liftPrim . MapKeyValueM

-- | Riffle async value transform.
--
-- The supplied @v -> IO v'@ runs on a bounded worker pool with
-- backpressure, EOS-correct drain, ordered\/unordered output,
-- and explicit failure policy. See 'Kafka.Streams.AsyncIO' for
-- the contract.
--
-- Differs from 'mapValuesM' by /not/ running on the stream
-- thread: latency-bound work (HTTP enrichment, DB lookups)
-- doesn't stall the engine.
--
-- == When to use this
--
--   * Per-record IO whose latency dominates throughput.
--   * Operations that benefit from parallelism but whose
--     output ordering still needs to match input ordering
--     ('AIO.OrderedOutput').
--
-- == When /not/ to use this
--
--   * Pure value transforms — use 'mapValues'.
--   * Logging / metrics — use 'peek'.
--   * Effects that must be EOS-atomic with downstream sinks —
--     keep them on-thread with 'mapValuesM'; the
--     async-operator drain is on commit-cycle boundaries, not
--     per-record.
asyncMapValues
  :: HasSerde v'
  => AIO.AsyncIOConfig
  -> (v -> IO v')
  -> Topology (KStream k v) (KStream k v')
asyncMapValues cfg = liftPrim . AsyncMapValues cfg

-- | Riffle async key+value transform. Same trade-offs as
-- 'asyncMapValues' plus the repartition implications of
-- 'mapKeyValue' (downstream stateful ops need an explicit
-- 'repartition' if the new key changes the partitioning).
asyncMapKeyValue
  :: (HasSerde k', HasSerde v')
  => AIO.AsyncIOConfig
  -> (k -> v -> IO (k', v'))
  -> Topology (KStream k v) (KStream k' v')
asyncMapKeyValue cfg = liftPrim . AsyncMapKeyValue cfg

-- | Riffle async flat-map on values. Each input record yields
-- a (possibly empty) list of outputs. Within one input record
-- the output ordering follows the returned list; across input
-- records it follows 'AIO.aioOutputMode'.
asyncConcatMapValues
  :: HasSerde v'
  => AIO.AsyncIOConfig
  -> (v -> IO [v'])
  -> Topology (KStream k v) (KStream k v')
asyncConcatMapValues cfg = liftPrim . AsyncConcatMapValues cfg

-- | Apply a pure transform to the full 'Record' — key, value,
-- timestamp, and headers all in scope. Useful when the
-- transform depends on header metadata or needs to propagate a
-- correlation-id header alongside the value transform.
--
-- /JVM equivalent:/ no single direct match — closest is
-- @KStream.transform@ with a stateless transformer. Mirrors
-- 'mapKeyValue' / 'mapValues' but with full-record visibility.
--
-- == Fusion
--
-- Chains of 'mapRecord' fuse into one @MapRecord (g . f)@ via
-- the same rule that fuses 'MapValues' chains. To opt out of
-- fusion for a particularly expensive transform, wrap with
-- 'noFuse' on either side.
mapRecord
  :: (HasSerde k', HasSerde v')
  => (Record k v -> Record k' v')
  -> Topology (KStream k v) (KStream k' v')
mapRecord = liftPrim . MapRecord

-- | Effectful 'mapRecord'. Same backpressure caveat as
-- 'mapValuesM'\/'mapKeyValueM' — the supplied 'IO' runs once
-- per record on the stream thread.
--
-- == When to reach for this over 'mapValuesM'
--
--   * The transform needs the record's headers (auth context,
--     trace IDs, schema versions) to decide how to map the
--     value.
--   * The transform needs to /set/ a header alongside the value
--     transform.
--   * The transform needs the original record timestamp to
--     compute an event-time-aware result.
--
-- For value-only IO transforms, prefer 'mapValuesM' — it's the
-- exact JVM @ValueMapper@ shape and the optimiser knows more
-- fusion rules about it.
--
-- == Pairing with 'noFuse'
--
-- Expensive per-record IO (HTTP calls, external lookups) often
-- benefits from being its own processor node so the runtime
-- can attribute latency / errors to it independently:
--
-- @
-- F.mapRecordM enrichWithHeaders
--   '>>>' F.noFuse  -- keep enrichment isolated
--   '>>>' F.mapValues normalise
-- @
mapRecordM
  :: (HasSerde k', HasSerde v')
  => (Record k v -> IO (Record k' v'))
  -> Topology (KStream k v) (KStream k' v')
mapRecordM = liftPrim . MapRecordM

-- | Opt out of operator fusion at this point in the topology.
--
-- 'noFuse' is a runtime identity (it doesn't change the wire
-- value) but acts as a /fusion barrier/: the AST optimiser
-- will not fuse adjacent primitives across it, even when
-- 'fuseStep' would otherwise collapse them.
--
-- == When to reach for it
--
-- The optimiser's fusion rules (e.g. @MapValues g . MapValues f
-- = MapValues (g . f)@) save processor nodes by collapsing
-- chains of stateless transforms. That's almost always a win,
-- but the runtime then sees /one/ node where the source had two
-- — which makes per-transform observability harder. When one of
-- the transforms is expensive (an external lookup, a heavy
-- compute step, an HTTP call), you usually want it as its own
-- node so:
--
--   * metrics and traces attribute its latency separately,
--   * @processStream@-style monitoring sees it distinctly,
--   * task assignment can reason about it independently.
--
-- 'noFuse' is the explicit "don't fuse here" marker:
--
-- @
-- F.source \"in\" ks vs
--   '>>>' F.mapValues cheap
--   '>>>' F.noFuse                          -- isolate the next transform
--   '>>>' F.mapValuesM (callExternalAPI)    -- expensive
--   '>>>' F.noFuse                          -- isolate it on both sides
--   '>>>' F.mapValues postProcess
--   '>>>' F.sink \"out\" ks vs
-- @
--
-- Without 'noFuse', the optimiser might collapse the three
-- 'mapValues' (M) calls into a single processor with the IO
-- buried inside; with it, each side stays its own node.
--
-- == Semantics
--
-- 'noFuse' compiles to a no-op pass-through processor at the
-- imperative level. At runtime it forwards every record
-- unchanged, like the pass-through processors emitted by
-- 'repartition'. The cost is one extra processor node per
-- barrier — negligible relative to the IO it's isolating.
noFuse :: Topology a a
noFuse = liftPrim NoFuse

-- | Keep records for which the predicate returns 'True'; drop
-- the rest.
--
-- /JVM equivalent:/ @KStream.filter(Predicate)@.
--
-- The predicate sees the entire 'Record', not just the value,
-- so you can filter on key, timestamp, or headers as well.
-- Chains of 'filter' fuse into a single @Filter (conj ...)@
-- node automatically.
filter :: (Record k v -> Bool) -> Topology (KStream k v) (KStream k v)
filter = liftPrim . Filter

-- | Inverse of 'filter': drop records for which the predicate
-- returns 'True'. Same fusion characteristics as 'filter'
-- (chains collapse via de Morgan).
--
-- /JVM equivalent:/ @KStream.filterNot(Predicate)@.
filterNot :: (Record k v -> Bool) -> Topology (KStream k v) (KStream k v)
filterNot = liftPrim . FilterNot

-- | Expand each record into zero or more output records,
-- changing only the value (key + timestamp + headers all
-- inherited from the input record).
--
-- /JVM equivalent:/ @KStream.concatMapValues(ValueMapper)@.
--
-- Useful for record-splitting (e.g. line → words) and
-- conditional emission (e.g. @Just x -> [x]; Nothing -> []@).
-- An empty list drops the input record entirely.
concatMapValues
  :: HasSerde v'
  => (v -> [v']) -> Topology (KStream k v) (KStream k v')
concatMapValues = liftPrim . ConcatMapValues

-- | Expand each record into zero or more output records,
-- potentially changing both key and value.
--
-- /JVM equivalent:/ @KStream.flatMap(KeyValueMapper)@.
--
-- Like 'mapKeyValue', this marks the stream as needing a
-- repartition for downstream stateful ops; insert
-- 'repartition' before any stateful stage if you're changing
-- the partitioning.
concatMapKeyValue
  :: (HasSerde k', HasSerde v')
  => (k -> v -> [(k', v')]) -> Topology (KStream k v) (KStream k' v')
concatMapKeyValue = liftPrim . ConcatMapKeyValue

-- | Run a side-effecting observer per record and pass the
-- record through unchanged. Useful for metrics, logging, or
-- debug taps that shouldn't alter the data path.
--
-- /JVM equivalent:/ @KStream.peek(ForeachAction)@.
--
-- 'peek' runs synchronously on the stream thread — same
-- backpressure caveat as 'mapValuesM' / 'foreach'. For a
-- non-data-altering "tap to a side topic", use 'tap' with a
-- sink as the side pipeline.
peek :: (Record k v -> IO ()) -> Topology (KStream k v) (KStream k v)
peek = liftPrim . Peek

-- | Terminal side-effect sink. The callback runs /synchronously/
-- on the stream-processing thread for every record. The wire
-- is closed (@()@ output) — there's no downstream
-- continuation.
--
-- /JVM equivalent:/ @KStream.foreach(ForeachAction)@.
--
-- There is /no/ @foreachAsync@ in this module on purpose. See
-- the module-level note "On side effects" for the rationale
-- and the recommended patterns for non-blocking work.
foreach :: (Record k v -> IO ()) -> Topology (KStream k v) ()
foreach = liftPrim . Foreach

-- | Re-key the stream using a function of the full 'Record'.
-- The value is preserved unchanged.
--
-- /JVM equivalent:/ @KStream.selectKey(KeyValueMapper)@.
--
-- 'selectKey' marks the stream as needing a repartition for
-- downstream stateful operations. Prefer it over
-- 'mapKeyValue' when you don't need to change the value —
-- the optimiser pairs @selectKey >>> groupByKey@ into a
-- single @groupBy@ node which matches the Apache docs'
-- recommended pattern.
selectKey
  :: HasSerde k'
  => (Record k v -> k') -> Topology (KStream k v) (KStream k' v)
selectKey = liftPrim . SelectKey

-- | Drop the key — the resulting stream is keyed by @()@.
--
-- /JVM equivalent:/ @KStream.values()@.
--
-- Idempotent — chained 'values' calls collapse into one. Use
-- when you want to disable per-key partitioning for a
-- subsequent operation that doesn't need the key (e.g. a
-- global counter via 'count').
values :: Topology (KStream k v) (KStream () v)
values = liftPrim Values

-- | Show-debugging sink: pretty-print each record via the
-- supplied line writer. The 'Text' label is prepended to
-- every printed line; @line writer@ is typically 'putStrLn',
-- 'hPutStrLn' against an open file handle, or a structured
-- logger callback.
--
-- /JVM equivalent:/ @KStream.print(Printed)@.
--
-- Named 'prints' (with an @s@) to avoid clashing with
-- 'Prelude.print'. For production-grade structured logging,
-- use 'peek' / 'foreach' against your logger; 'prints' is
-- optimised for REPL exploration and ad-hoc debugging.
prints
  :: (Show k, Show v)
  => Text -> (String -> IO ()) -> Topology (KStream k v) ()
prints lbl putLine = liftPrim (Print lbl putLine)

-- * 'KStream' composition + branching -----------------------------

-- | Binary merge of two streams of the same shape into one. The
-- output stream interleaves records from both inputs in
-- per-partition arrival order; per-key ordering is preserved
-- /within/ each input but the relative order across inputs is
-- not specified.
--
-- /JVM equivalent:/ @KStream.merge(KStream)@.
--
-- The input is a tuple, so chain with 'Fanout' / @'&&&'@ /
-- 'mergeSourced' to get the two streams in place. For more
-- than two streams use 'mergeAll'.
merge :: Topology (KStream k v, KStream k v) (KStream k v)
merge = liftPrim Merge

-- | N-ary merge: a list of streams collapses to one. The list
-- is consumed at AST-application time, so the count is
-- bounded at compile time per call site. An empty list is a
-- run-time error (matches the imperative
-- 'Kafka.Streams.KStream.mergeStreamsN' semantics).
--
-- /JVM equivalent:/ chained @merge@ calls.
mergeAll :: Topology [KStream k v] (KStream k v)
mergeAll = liftPrim MergeAll

-- | Predicate-routed split into N sub-streams.
--
-- Each input record is routed to the first sub-stream whose
-- predicate returns 'True'. Records that match no predicate
-- are dropped. The output is a /list/ of streams, one per
-- predicate, in the order the predicates were supplied.
--
-- /JVM equivalent:/ the pre-KIP-418 @KStream.branch(Predicate...)@.
--
-- For KIP-418 /named/ branches (returning a 'Data.Map.Strict.Map'
-- keyed by branch name) use 'split' instead — it's the
-- recommended modern API.
branch :: [Record k v -> Bool] -> Topology (KStream k v) [KStream k v]
branch = liftPrim . Branch

-- | KIP-418 named-branch split. Each input record is routed to
-- the first branch whose predicate matches; records that
-- match no branch go to the optional default-branch name (or
-- are dropped if 'Nothing'). The output is a 'Map' from
-- branch name to its sub-stream, so downstream code can
-- destructure by name rather than by list position.
--
-- /JVM equivalent:/ @KStream.split().branch(Branched.as("name").withPredicate(...))@.
--
-- @
-- F.source \"in\" ks vs
--   '>>>' F.split
--           [ F.splitBranch \"errors\"   (\\r -> recordValue r > threshold)
--           , F.splitBranch \"warnings\" (\\r -> recordValue r > 0)
--           ]
--           ('Just' \"info\")
--   '>>>' F.liftIO_ \"route-branches\" (\\b m -> do
--           let Just errs = Map.lookup \"errors\"   m
--               Just wrns = Map.lookup \"warnings\" m
--               Just info = Map.lookup \"info\"     m
--           -- ... sink each branch to a different topic ...
--           pure ())
-- @
split
  :: [SplitBranch k v]
  -> Maybe Text
  -> Topology (KStream k v) (Map Text (KStream k v))
split bs mDef = liftPrim (Split bs mDef)

-- | Helper for assembling a 'SplitBranch'. The 'Text' is the
-- branch name (appears as the result-'Map' key); the function
-- is the predicate.
--
-- @
-- F.splitBranch \"low\" (\\r -> recordValue r < 10)
-- @
splitBranch :: Text -> (Record k v -> Bool) -> SplitBranch k v
splitBranch = SplitBranch

-- | Explicit wire duplicator: @a -> (a, a)@.
--
-- Equivalent to @'Cat.id' '&&&' 'Cat.id'@, but named for the
-- common use case where you want to thread the same wire
-- value into two downstream paths. The optimiser pushes
-- adjacent pure functions ('Arr') through 'fork' so
-- @fork >>> arr f@ collapses to @Arr (\\a -> f (a, a))@.
--
-- See also 'forkN' for N-way duplication.
fork :: Topology a (a, a)
fork = FA.fork

-- | N-way fan-out: apply each sub-fragment to the /same/
-- upstream value and collect the results in input order.
-- Generalises 'fork' / @'&&&'@ to any positive arity.
--
-- All sub-fragments share the upstream node in the compiled
-- topology — every branch's lineage descends from the same
-- source, so the EOS task assigner groups them together
-- (every branch's sinks commit in one transaction).
--
-- @
-- forkN ( mkSink \"upper\" T.toUpper
--      'NE.:|' [ mkSink \"lower\" T.toLower
--           , mkSink \"len\"   (T.pack . show . T.length)
--           ])
-- @
forkN :: NonEmpty (Topology a b) -> Topology a (NonEmpty b)
forkN = FA.forkN

-- | Run a side-effecting sub-pipeline and pass the upstream
-- wire through unchanged.
--
-- Operationally equivalent to @'Cat.id' '&&&' t '>>>' 'arr'
-- 'fst'@ but reads better at the call site and lets the
-- optimiser apply @Tap-specific@ rewrites (e.g.
-- @'Tap' ('Foreach' f) === 'Peek' f@).
--
-- The side pipeline @t@ must be closed (return @()@) — it's
-- typically a 'Sink', 'Foreach', or a small chain ending in
-- one. Use 'tap' for "audit-log this stream without
-- disturbing the main path" patterns:
--
-- @
-- F.source \"in\" ks vs
--   '>>>' F.tap (F.filter isUrgent '>>>' F.sink \"alerts\" ks vs)
--   '>>>' F.mapValues normalise
--   '>>>' F.sink \"out\" ks vs
-- @
tap :: Topology a () -> Topology a a
tap = FA.tap

-- * Conversions ---------------------------------------------------

-- | Materialise the latest value-per-key into a state-backed
-- 'KTable'. Every input record either inserts or updates the
-- store; tombstones (records with @Nothing@-typed value
-- representations) are still inserted as such — apply
-- 'mapValues' to drop them upstream if you want delete
-- semantics.
--
-- /JVM equivalent:/ @KStream.toTable(Materialized)@.
--
-- The 'Materialized' carries the store name, optional key /
-- value serdes (downstream operations that need them will
-- raise 'MissingMaterializedSerde' if absent), and the
-- changelog config. The resulting 'KTable' shares its state
-- store with any other operation referencing the same
-- 'StoreName'.
toTable :: Ord k => Materialized k v -> Topology (KStream k v) (KTable k v)
toTable = liftPrim . ToTableT

-- | Convert a 'KTable' back to a 'KStream' carrying every
-- changelog event. The output stream emits one record for
-- every update (including tombstones) that flowed into the
-- table.
--
-- /JVM equivalent:/ @KTable.toStream@.
--
-- 'toStream' is /not/ idempotent — feeding the same KTable
-- twice produces two separate change-emitting processors and
-- the records appear on the resulting streams in (the same)
-- per-table order, but each invocation is its own node.
toStream :: Topology (KTable k v) (KStream k v)
toStream = liftPrim ToStream

-- | Force a repartition through an internal topic, using the
-- supplied prefix to name the topic. The topic is created
-- with the same partition count as the upstream and the
-- records re-keyed by the current 'KStream' key.
--
-- /JVM equivalent:/ @KStream.repartition(name)@.
--
-- == When to call this
--
-- Insert 'repartition' between a key-changing op (a
-- 'selectKey' / 'mapKeyValue' / 'concatMapKeyValue') and a
-- subsequent stateful op (a 'groupBy' / 'aggregate' / 'join')
-- to make the broker re-shuffle records so per-key ordering
-- holds.
--
-- Chained 'repartition' calls collapse to one shuffle via
-- the 'optCollapseRepartition' optimisation — the broker
-- would otherwise pay for the extra topic round-trip with
-- no benefit.
repartition :: Text -> Topology (KStream k v) (KStream k v)
repartition = liftPrim . Repartition

-- | 'repartition' with a fully-specified 'Rep.Repartitioned'
-- config: caller controls the partition count, custom
-- partitioner, and override serdes. Use this when the
-- default partition count doesn't match what downstream
-- joins or external consumers need.
--
-- /JVM equivalent:/ @KStream.repartition(Repartitioned)@.
repartitionWith
  :: Rep.Repartitioned k v -> Topology (KStream k v) (KStream k v)
repartitionWith = liftPrim . RepartitionWith

-- * Grouping + aggregation ----------------------------------------
--
-- Aggregation in Kafka Streams is a /per-key/ reduction that runs
-- inside a stateful processor backed by a state store. The
-- pattern is always three stages:
--
--   1. /Group/ the stream by some key (this stage is pure — no
--      processor node is added; it just changes the wire type to
--      'KGroupedStream' so the type system enforces that the
--      next stage is an aggregation).
--   2. /Aggregate/ via 'count', 'reduce', or 'aggregate' — this
--      adds the processor + state store and produces a 'KTable'.
--   3. Optionally consume the resulting 'KTable' downstream
--      ('toStream', joins, sinks, …).
--
-- For session-windowed or time-windowed aggregations insert a
-- 'windowedByTime' / 'windowedBySession' step between the
-- grouping and the aggregation, and use the @*Windowed@
-- aggregators that consume 'TimeWindowedKStream' /
-- 'SessionWindowedKStream'.

-- | Group records by their existing key. The wire becomes a
-- 'KGroupedStream'; the next stage must be a 'count' /
-- 'reduce' / 'aggregate' / 'windowedByTime' / 'windowedBySession'.
--
-- /JVM equivalent:/ @KStream.groupByKey(Grouped)@.
--
-- 'groupByKey' is /pure/: no processor node, no repartition
-- needed if the upstream key hasn't been altered. The
-- 'Grouped' carries the serdes the downstream aggregator
-- needs to read/write the state store.
--
-- If the upstream had a key-changing op ('selectKey',
-- 'mapKeyValue', 'concatMapKeyValue') without an explicit
-- 'repartition' in between, the broker partition layout no
-- longer matches the new key and the aggregation will silently
-- produce wrong results. Either insert 'repartition' or use
-- 'groupBy' (which the optimiser fuses with an upstream
-- 'selectKey').
groupByKey
  :: forall k v. (HasSerde k, HasSerde v)
  => Topology (KStream k v) (KGroupedStream k v)
groupByKey = groupByKeyWith (grouped (serde @k) (serde @v))

-- | 'groupByKey' with an explicit 'Grouped' record. Use this to
-- pin a named repartition topic, override either serde, or
-- attach a 'Named' label.
groupByKeyWith :: Grouped k v -> Topology (KStream k v) (KGroupedStream k v)
groupByKeyWith = liftPrim . GroupByKey

-- | Group by a derived key. The supplied function picks a new
-- key per 'Record'; the upstream value is preserved.
--
-- /JVM equivalent:/ @KStream.groupBy(KeyValueMapper, Grouped)@.
--
-- Prefer 'groupBy' over @selectKey >>> groupByKey@ —
-- the optimiser collapses the latter pair into the former, but
-- writing it directly is clearer and skips a node.
--
-- Like any key-changing op, 'groupBy' marks the stream as
-- needing a repartition for downstream stateful operations.
-- Callers usually want a 'repartition' between 'groupBy' and
-- the aggregation if the partition layout matters; in the
-- in-process test driver this is a no-op, but against a real
-- broker it's what guarantees per-key ordering.
groupBy
  :: forall k v k'. (HasSerde k', HasSerde v)
  => (Record k v -> k')
  -> Topology (KStream k v) (KGroupedStream k' v)
groupBy f = groupByWith f (grouped (serde @k') (serde @v))

-- | 'groupBy' with an explicit 'Grouped' record.
groupByWith
  :: (Record k v -> k') -> Grouped k' v
  -> Topology (KStream k v) (KGroupedStream k' v)
groupByWith f g = liftPrim (GroupBy f g)

-- | Count records per key. The resulting 'KTable' maps each
-- key to the number of records seen for it.
--
-- /JVM equivalent:/ @KGroupedStream.count(Materialized)@.
--
-- The 'Materialized' names the state store and carries the
-- key serde (the value serde is implicit — counts are
-- 'Int64'). Pass 'Mat.materializedAs' for a named store, or
-- 'Mat.materialized' for an auto-named one.
count
  :: Ord k
  => Materialized k Int64
  -> Topology (KGroupedStream k v) (KTable k Int64)
count = liftPrim . Count

-- | Combine values per key with a binary reducer of the same
-- shape. The accumulator is initialised with the first value
-- seen for each key; subsequent values are combined via the
-- supplied function.
--
-- /JVM equivalent:/ @KGroupedStream.reduce(Reducer, Materialized)@.
--
-- The reducer must be associative for the result to be
-- deterministic across replays / failovers — Kafka may
-- replay records on restart, and a non-associative reducer
-- would produce different results.
--
-- For aggregations whose accumulator type differs from the
-- input value type, use 'aggregate'.
reduce
  :: Ord k
  => (v -> v -> v) -> Materialized k v
  -> Topology (KGroupedStream k v) (KTable k v)
reduce f m = liftPrim (Reduce f m)

-- | General-shape per-key aggregator: the accumulator can be a
-- different type from the value, the initialiser runs once
-- per key (lazily, on first record), and the step function
-- folds in each new value.
--
-- /JVM equivalent:/ @KGroupedStream.aggregate(Initializer, Aggregator, Materialized)@.
--
-- The initialiser is an 'IO' to mirror Java's
-- @Initializer<A>@: callers who want a pure seed pass
-- @'pure' x@. Stateful initialisers (e.g. allocating a
-- mutable buffer) are also valid.
--
-- == Determinism note
--
-- As with 'reduce', the @(k -> v -> agg -> agg)@ step must be
-- associative if Kafka may replay records on failover —
-- otherwise different replay orderings produce different
-- final values.
aggregate
  :: Ord k
  => IO agg
  -> (k -> v -> agg -> agg)
  -> Materialized k agg
  -> Topology (KGroupedStream k v) (KTable k agg)
aggregate seed step m = liftPrim (Aggregate seed step m)

-- * Windowed aggregation ------------------------------------------
--
-- Windowed aggregations bucket records by /time/ before
-- aggregating, so the output is one value per (key, window)
-- pair instead of one value per key. There are two windowing
-- shapes:
--
--   * Time windows ('Win.Windows' / 'windowedByTime'): fixed-size
--     buckets — tumbling (no overlap), hopping (advance < size,
--     overlapping), sliding. Records fall into one or more
--     windows based on their timestamp.
--
--   * Session windows ('Win.SessionWindows' / 'windowedBySession'):
--     gap-based — a window stays open as long as records keep
--     arriving within the gap; a long enough silence closes the
--     window. Useful for activity-style aggregations.
--
-- After windowing, the wire becomes 'TimeWindowedKStream' /
-- 'SessionWindowedKStream' and the only legal next stage is one
-- of the @*Windowed@ aggregators. The result is a
-- 'TWKS.WindowedTableHandle' / 'SWKS.SessionWindowedTableHandle'
-- which downstream can read directly (via Interactive Queries)
-- or convert into a 'KStream' via the windowed-key suppress
-- machinery.

-- | Bucket the grouped stream into time windows. The 'Win.Windows'
-- carries the window size, advance interval (for hopping),
-- and retention. Used immediately upstream of 'countWindowed' /
-- 'reduceWindowed' / 'aggregateWindowed'.
--
-- /JVM equivalent:/ @KGroupedStream.windowedBy(TimeWindows)@.
--
-- Pure — no processor node, just a wire-type change.
windowedByTime
  :: Win.Windows
  -> Topology (KGroupedStream k v) (TimeWindowedKStream k v)
windowedByTime = liftPrim . WindowedByTime

-- | Bucket the grouped stream by /session/: a window stays open
-- as long as new records arrive within the configured gap.
-- When a record arrives more than the gap after the previous
-- record (for the same key), a new session window starts.
-- Adjacent sessions whose records' timestamps overlap (after
-- a late record arrives) are /merged/.
--
-- /JVM equivalent:/ @KGroupedStream.windowedBy(SessionWindows)@.
--
-- Use immediately upstream of 'countSessionWindowed' /
-- 'aggregateSessionWindowed' — the session aggregator takes a
-- /merger/ in addition to the step function, used when two
-- sessions get merged into one.
windowedBySession
  :: Win.SessionWindows
  -> Topology (KGroupedStream k v) (SessionWindowedKStream k v)
windowedBySession = liftPrim . WindowedBySession

-- | Count records per (key, window) pair. Mirrors 'count' for
-- the time-windowed case.
--
-- /JVM equivalent:/ @TimeWindowedKStream.count(Materialized)@.
countWindowed
  :: Ord k
  => Materialized k Int64
  -> Topology (TimeWindowedKStream k v) (TWKS.WindowedTableHandle k Int64)
countWindowed = liftPrim . CountWindowed

-- | Per-(key, window) reducer; the value type is unchanged.
-- See 'reduce' for the associativity caveat.
--
-- /JVM equivalent:/ @TimeWindowedKStream.reduce(Reducer, Materialized)@.
reduceWindowed
  :: Ord k
  => (v -> v -> v) -> Materialized k v
  -> Topology (TimeWindowedKStream k v) (TWKS.WindowedTableHandle k v)
reduceWindowed f m = liftPrim (ReduceWindowed f m)

-- | General-shape per-(key, window) aggregator with an
-- accumulator type that may differ from the input value.
-- Mirrors 'aggregate' for the time-windowed case.
--
-- /JVM equivalent:/ @TimeWindowedKStream.aggregate(Initializer, Aggregator, Materialized)@.
aggregateWindowed
  :: Ord k
  => IO agg -> (k -> v -> agg -> agg) -> Materialized k agg
  -> Topology (TimeWindowedKStream k v) (TWKS.WindowedTableHandle k agg)
aggregateWindowed seed step m = liftPrim (AggregateWindowed seed step m)

-- | Count records per (key, session). Mirrors 'count' for the
-- session-windowed case; no merger needed since the
-- accumulator is just @Int64@.
--
-- /JVM equivalent:/ @SessionWindowedKStream.count(Materialized)@.
countSessionWindowed
  :: Ord k
  => Materialized k Int64
  -> Topology (SessionWindowedKStream k v) (SWKS.SessionWindowedTableHandle k Int64)
countSessionWindowed = liftPrim . CountSessionWindowed

-- | Session-windowed aggregator with an explicit merger.
-- When two adjacent sessions are merged (because a late
-- record bridged them), the merger combines their two
-- accumulators into one — this is the extra argument
-- compared to the time-windowed 'aggregateWindowed'.
--
-- /JVM equivalent:/ @SessionWindowedKStream.aggregate(Initializer, Aggregator, Merger, Materialized)@.
--
-- The merger must be associative + commutative if Kafka may
-- replay records on failover.
aggregateSessionWindowed
  :: Ord k
  => IO agg
  -> (k -> v -> agg -> agg)
  -> (k -> agg -> agg -> agg)
  -> Materialized k agg
  -> Topology (SessionWindowedKStream k v) (SWKS.SessionWindowedTableHandle k agg)
aggregateSessionWindowed seed step merger m =
  liftPrim (AggregateSessionWindowed seed step merger m)

-- * KGroupedTable -------------------------------------------------
--
-- A 'KGroupedTable' aggregates a /changelog/ — the input is a
-- 'KTable' whose records are inserts/updates/deletes per key,
-- and we want to aggregate them under a derived key.
--
-- Unlike 'KGroupedStream' aggregations, a 'KGroupedTable'
-- aggregator needs BOTH an /adder/ (combine the new value
-- into the accumulator) AND a /subtractor/ (remove the
-- previous value's contribution before adding the new one).
-- Without the subtractor, every update would double-count the
-- previous contribution.
--
-- See 'groupTableBy' to enter this regime from a 'KTable'.

-- | Re-key a 'KTable' and group the resulting changelog for
-- subtractor-aware aggregation. The function picks a new
-- (key, value) per record; downstream aggregators see that
-- new key.
--
-- /JVM equivalent:/ @KTable.groupBy(KeyValueMapper, Grouped)@.
groupTableBy
  :: (Ord k, Ord k')
  => (k -> v -> (k', v')) -> Grouped k' v'
  -> Topology (KTable k v) (KGroupedTable k' v')
groupTableBy f g = liftPrim (GroupTableBy f g)

-- | Count grouped-table records per derived key. The
-- subtractor is implicit (decrement) and the adder is
-- implicit (increment), so no functions are needed beyond
-- the materialised result.
--
-- /JVM equivalent:/ @KGroupedTable.count(Materialized)@.
countKGroupedTable
  :: Ord k
  => Materialized k Int64
  -> Topology (KGroupedTable k v) (KTable k Int64)
countKGroupedTable = liftPrim . CountKGroupedTable

-- | Reduce grouped-table records per derived key with explicit
-- adder + subtractor functions. The subtractor is applied
-- first (to remove the previous value's contribution) and
-- then the adder is applied (to fold in the new value);
-- inserts skip the subtractor, deletes skip the adder.
--
-- /JVM equivalent:/ @KGroupedTable.reduce(Adder, Subtractor, Materialized)@.
--
-- Both adder and subtractor must be associative for the
-- result to be deterministic across replays.
reduceKGroupedTable
  :: Ord k
  => (v -> v -> v) -> (v -> v -> v) -> Materialized k v
  -> Topology (KGroupedTable k v) (KTable k v)
reduceKGroupedTable add sub m = liftPrim (ReduceKGroupedTable add sub m)

-- | General-shape subtractor-aware aggregator. The
-- accumulator may differ from the input value; both adder
-- and subtractor take the current accumulator, the value
-- being added/removed, and the key.
--
-- /JVM equivalent:/ @KGroupedTable.aggregate(Initializer, Adder, Subtractor, Materialized)@.
aggregateKGroupedTable
  :: Ord k
  => IO agg
  -> (k -> v -> agg -> agg)
  -> (k -> v -> agg -> agg)
  -> Materialized k agg
  -> Topology (KGroupedTable k v) (KTable k agg)
aggregateKGroupedTable seed add sub m =
  liftPrim (AggregateKGroupedTable seed add sub m)

-- * Cogroup -------------------------------------------------------
--
-- Cogroup aggregates N /independent/ grouped streams that share
-- the same key type and accumulator type. Each grouped stream
-- contributes records via its own per-source aggregator; all
-- of them update the same shared state.
--
-- The natural way to build a cogroup is /incrementally/, one
-- source at a time. With the 'Monad' instance + 'applyT' this
-- maps onto do-notation cleanly — see the module-level
-- "Monad bind for incrementally-built topologies" example.

-- | Start a cogroup with one grouped-stream source plus its
-- per-source aggregator. The resulting wire is a
-- 'CogroupedStream' that further sources can be added to via
-- 'addCogrouped'.
--
-- /JVM equivalent:/ @KGroupedStream.cogroup(Aggregator)@.
cogroup
  :: (k -> v -> a -> a)
  -> Topology (KGroupedStream k v) (CogroupedStream k a)
cogroup = liftPrim . Cogroup

-- | Extend a cogroup with another grouped-stream source. The
-- input is a tuple: bring the existing cogroup builder and
-- the new grouped stream together via 'Fanout' / @'&&&'@ /
-- 'applyT'.
--
-- /JVM equivalent:/ @CogroupedKStream.cogroup(KGroupedStream, Aggregator)@.
addCogrouped
  :: (k -> v -> a -> a)
  -> Topology (CogroupedStream k a, KGroupedStream k v) (CogroupedStream k a)
addCogrouped = liftPrim . AddCogrouped

-- | Close out a cogroup builder and emit its result as a
-- 'KTable'. The initialiser allocates the accumulator (once
-- per key, lazily); the 'Materialized' names the resulting
-- store.
--
-- /JVM equivalent:/ @CogroupedKStream.aggregate(Initializer, Materialized)@.
aggregateCogrouped
  :: Ord k
  => IO a -> Materialized k a
  -> Topology (CogroupedStream k a) (KTable k a)
aggregateCogrouped seed m = liftPrim (AggregateCogrouped seed m)

-- * Joins ---------------------------------------------------------
--
-- All join constructors take their two participants as a /tuple/
-- on the input side; bring two streams / tables together with
-- 'Fanout' / @'&&&'@ / 'mergeSourced' / @'applyT'@ so they
-- land in the tuple shape.
--
-- == Co-partitioning requirement
--
-- Kafka Streams joins require the two input topics to be
-- /co-partitioned/: same partition count, same partitioning
-- function. If you've re-keyed an input via 'selectKey' /
-- 'mapKeyValue' upstream of a join, you'll need a 'repartition'
-- before the join so the broker's partition layout matches.
-- (For stream-table joins of a 'globalTableSource', the
-- co-partitioning requirement is relaxed — the global table is
-- replicated across all tasks.)

-- | Stream-table /inner/ join: emit @joiner v vt@ whenever a
-- stream record's key matches a table entry. Stream records
-- whose key has no table match are dropped.
--
-- /JVM equivalent:/ @KStream.join(KTable, ValueJoiner, Joined)@.
streamTableJoin
  :: (Ord k, HasSerde v')
  => (v -> vt -> v') -> Joined k v vt
  -> Topology (KStream k v, KTable k vt) (KStream k v')
streamTableJoin j jo = liftPrim (StreamTableJoin j jo)

-- | Stream-table /left/ join: emit @joiner v (Just vt)@ when
-- the key matches, @joiner v Nothing@ when it doesn't. Every
-- stream record produces exactly one output; the join
-- function decides what to do with the @Nothing@ case.
--
-- /JVM equivalent:/ @KStream.leftJoin(KTable, ValueJoiner, Joined)@.
streamTableLeftJoin
  :: (Ord k, HasSerde v')
  => (v -> Maybe vt -> v') -> Joined k v vt
  -> Topology (KStream k v, KTable k vt) (KStream k v')
streamTableLeftJoin j jo = liftPrim (StreamTableLeftJoin j jo)

-- | Stream-stream /windowed inner/ join. Two stream records
-- match if their keys are equal and their timestamps are
-- within the 'JoinWindows' (asymmetric before/after offsets,
-- plus an optional grace period for late arrivals).
--
-- /JVM equivalent:/ @KStream.join(KStream, ValueJoiner, JoinWindows, StreamJoined)@.
--
-- Both sides are buffered in per-side window stores for the
-- 'JoinWindows' duration; downstream emission happens
-- per match.
streamStreamJoin
  :: (Ord k, HasSerde v')
  => (v1 -> v2 -> v')
  -> JoinWindows -> Joined k v1 v2
  -> Topology (KStream k v1, KStream k v2) (KStream k v')
streamStreamJoin j w jo = liftPrim (StreamStreamJoin j w jo)

-- | Stream-stream /windowed left/ join. Every left record
-- emits at least once: with @'Just' v2@ for each match, or
-- with @'Nothing'@ if no match is found within the join
-- window before its grace period expires. Right records
-- only contribute matches.
--
-- /JVM equivalent:/ @KStream.leftJoin(KStream, ValueJoiner, JoinWindows, StreamJoined)@.
streamStreamLeftJoin
  :: (Ord k, HasSerde v')
  => (v1 -> Maybe v2 -> v')
  -> JoinWindows -> Joined k v1 v2
  -> Topology (KStream k v1, KStream k v2) (KStream k v')
streamStreamLeftJoin j w jo = liftPrim (StreamStreamLeftJoin j w jo)

-- | Stream-stream /windowed outer/ join. Both sides emit at
-- least once; the joiner takes 'Maybe' on both sides because
-- either may be absent.
--
-- /JVM equivalent:/ @KStream.outerJoin(KStream, ValueJoiner, JoinWindows, StreamJoined)@.
streamStreamOuterJoin
  :: (Ord k, HasSerde v')
  => (Maybe v1 -> Maybe v2 -> v')
  -> JoinWindows -> Joined k v1 v2
  -> Topology (KStream k v1, KStream k v2) (KStream k v')
streamStreamOuterJoin j w jo = liftPrim (StreamStreamOuterJoin j w jo)

-- | Table-table /inner/ join. The output is a 'KTable' that
-- updates whenever either side updates. Materialised via the
-- supplied 'Materialized'.
--
-- /JVM equivalent:/ @KTable.join(KTable, ValueJoiner, Materialized)@.
tableTableJoin
  :: Ord k
  => (v1 -> v2 -> v') -> Materialized k v'
  -> Topology (KTable k v1, KTable k v2) (KTable k v')
tableTableJoin j m = liftPrim (TableTableJoin j m)

-- | Table-table /left/ join: emit even when the right side
-- has no value (joiner sees 'Nothing').
--
-- /JVM equivalent:/ @KTable.leftJoin(KTable, ValueJoiner, Materialized)@.
tableTableLeftJoin
  :: Ord k
  => (v1 -> Maybe v2 -> v') -> Materialized k v'
  -> Topology (KTable k v1, KTable k v2) (KTable k v')
tableTableLeftJoin j m = liftPrim (TableTableLeftJoin j m)

-- | Table-table /outer/ join: emit whenever either side has a
-- value (joiner sees 'Maybe' on both sides).
--
-- /JVM equivalent:/ @KTable.outerJoin(KTable, ValueJoiner, Materialized)@.
tableTableOuterJoin
  :: Ord k
  => (Maybe v1 -> Maybe v2 -> v') -> Materialized k v'
  -> Topology (KTable k v1, KTable k v2) (KTable k v')
tableTableOuterJoin j m = liftPrim (TableTableOuterJoin j m)

-- | Foreign-key KTable-KTable /inner/ join (KIP-213). The
-- /left/ table is keyed by @k@; the /right/ table is keyed by
-- @fk@. The foreign-key extractor pulls an @fk@ out of each
-- left value, used to look up the right table. Records on
-- the left whose extracted foreign key has no right entry
-- are dropped.
--
-- /JVM equivalent:/ @KTable.join(KTable, fkExtractor, ValueJoiner, Materialized)@.
--
-- The 'Hashable' constraint on @v@ is the foreign-key
-- subscription token mechanism used by the implementation to
-- detect which left rows are affected by a right update.
foreignKeyJoin
  :: (Ord k, Ord fk, Hashable v, HasSerde v')
  => (v -> fk)
  -> (v -> vr -> v')
  -> Materialized k v'
  -> Topology (KTable k v, KTable fk vr) (KTable k v')
foreignKeyJoin ext j m = liftPrim (ForeignKeyJoin ext j m)

-- | Foreign-key KTable-KTable /left/ join: emit even when the
-- right table has no entry for the extracted foreign key
-- (joiner sees 'Nothing').
--
-- /JVM equivalent:/ @KTable.leftJoin(KTable, fkExtractor, ValueJoiner, Materialized)@.
leftForeignKeyJoin
  :: (Ord k, Ord fk, Hashable v, HasSerde v')
  => (v -> fk)
  -> (v -> Maybe vr -> v')
  -> Materialized k v'
  -> Topology (KTable k v, KTable fk vr) (KTable k v')
leftForeignKeyJoin ext j m = liftPrim (LeftForeignKeyJoin ext j m)

-- | Stream-globalTable /inner/ join. The key-mapper picks a
-- foreign key out of each stream record; the global table is
-- looked up by that foreign key; the joiner produces the
-- result.
--
-- /JVM equivalent:/ @KStream.join(GlobalKTable, KeyValueMapper, ValueJoiner)@.
--
-- Unlike stream-table joins, /no co-partitioning is
-- required/: a 'GlobalKTable' is cluster-replicated, so
-- every task has the full table available locally.
streamGlobalTableJoin
  :: (Ord kg, HasSerde v')
  => (k -> v -> kg)
  -> (v -> vg -> v')
  -> Topology (KStream k v, GlobalKTable kg vg) (KStream k v')
streamGlobalTableJoin km j = liftPrim (StreamGlobalTableJoin km j)

-- | Stream-globalTable /left/ join: emit even when the
-- global-table lookup misses (joiner sees 'Nothing').
--
-- /JVM equivalent:/ @KStream.leftJoin(GlobalKTable, KeyValueMapper, ValueJoiner)@.
streamGlobalTableLeftJoin
  :: (Ord kg, HasSerde v')
  => (k -> v -> kg)
  -> (v -> Maybe vg -> v')
  -> Topology (KStream k v, GlobalKTable kg vg) (KStream k v')
streamGlobalTableLeftJoin km j = liftPrim (StreamGlobalTableLeftJoin km j)

-- * KTable surface ------------------------------------------------
--
-- These mirror the stream-side 'filter' / 'mapValues' family but
-- operate on a changelog table — the implementation maintains
-- a new materialised store of the filtered / transformed view.
-- That's why every constructor here takes a 'Materialized': the
-- output 'KTable' is its own state store.

-- | Filter a 'KTable'. Records where the predicate returns
-- 'False' are stored as tombstones (deletes) in the output
-- table — so the output is the @True@-subset view.
--
-- /JVM equivalent:/ @KTable.filter(Predicate, Materialized)@.
filterTable
  :: Ord k
  => (Record k v -> Bool) -> Materialized k v
  -> Topology (KTable k v) (KTable k v)
filterTable p m = liftPrim (FilterTable p m)

-- | Inverse of 'filterTable': records matching the predicate
-- are tombstoned out. Implemented as
-- @'filterTable' (not . p)@ but exposed as its own
-- constructor for parity with the JVM API.
--
-- /JVM equivalent:/ @KTable.filterNot(Predicate, Materialized)@.
filterNotTable
  :: Ord k
  => (Record k v -> Bool) -> Materialized k v
  -> Topology (KTable k v) (KTable k v)
filterNotTable p m = liftPrim (FilterNotTable p m)

-- | Map values in a 'KTable'. The key is preserved; the value
-- type may change. A new state store is created for the
-- mapped view.
--
-- /JVM equivalent:/ @KTable.mapValues(ValueMapper, Materialized)@.
mapValuesTable
  :: (Ord k, HasSerde v')
  => (v -> v') -> Materialized k v'
  -> Topology (KTable k v) (KTable k v')
mapValuesTable f m = liftPrim (MapValuesTable f m)

-- | Custom-processor transform of a 'KTable'. The processor
-- receives table-update records and can read/write the
-- declared 'StoreName's. Useful when 'mapValuesTable' isn't
-- enough — e.g. you need access to other state stores during
-- the value transformation.
--
-- /JVM equivalent:/ @KTable.transformValues(TransformerSupplier, Materialized, storeNames)@.
transformValuesTable
  :: Ord k
  => Text
  -> IO (Processor k v)
  -> [StoreName]
  -> Materialized k v'
  -> Topology (KTable k v) (KTable k v')
transformValuesTable nm sup stores m =
  liftPrim (TransformValuesTable nm sup stores m)

-- * Suppress ------------------------------------------------------

-- | Debounce-style suppression: hold the latest value per key
-- in a buffer for up to the supplied 'Duration' before
-- emitting. Successive updates within the time limit
-- overwrite the buffered value; the buffered value is
-- emitted once the time limit has elapsed since the first
-- record of the current window.
--
-- /JVM equivalent:/ @Suppressed.untilTimeLimit(Duration, BufferConfig)@.
--
-- Useful for reducing downstream noise from rapid updates on
-- the same key — only the latest value lands after the
-- quiet period.
suppressUntilTimeLimit
  :: Ord k => Duration -> Topology (KStream k v) (KStream k v)
suppressUntilTimeLimit = liftPrim . SuppressUntilTimeLimit

-- | Suppress all updates to a windowed key until the window
-- has closed (i.e. stream time has advanced past
-- @windowEnd + gracePeriod@). Each (window, key) pair emits
-- exactly once with its final value.
--
-- /JVM equivalent:/ @Suppressed.untilWindowCloses(BufferConfig)@.
--
-- Operates on streams keyed by 'WindowedKey' — typically the
-- output of @streamFromWindowedHandle@ or a similar windowed
-- conversion. The 'Int64' is the window size in milliseconds;
-- the 'Duration' is the grace period for late records.
suppressWindowed
  :: Ord k
  => Duration -> Int64
  -> Topology (KStream (WindowedKey k) v) (KStream (WindowedKey k) v)
suppressWindowed g sz = liftPrim (SuppressWindowedKS g sz)

-- * Processor API -------------------------------------------------
--
-- The Processor API gives the most flexibility (and the most
-- responsibility): you supply a full 'Processor' value with
-- 'procInit' / 'procProcess' / 'procClose' callbacks. The
-- callbacks have full 'IO' and can interact with declared
-- state stores via 'getStateStore'.
--
-- For the common pattern of "one processor with one state store
-- it owns exclusively" reach for 'processWithStateStoreKV' /
-- 'processWithStateStoreW' / 'processWithStateStoreS' — they
-- atomically register the processor and the store with the
-- right owner-node wiring. The lower-level 'processStream' +
-- 'withStateStoreKV' pair is for multi-owner stores and other
-- advanced cases.

-- | Attach a custom processor as a terminal sink. The processor
-- consumes records and writes only to declared stores or out
-- of band; it doesn't forward downstream (the @()@ output
-- reflects that). For a processor that DOES produce a
-- downstream value, use 'processValuesStream' or
-- 'transformValuesStream'.
--
-- /JVM equivalent:/ @KStream.process(ProcessorSupplier, storeNames)@.
--
-- The 'Text' prefix names the processor in the topology
-- graph (with a fresh suffix appended). The @['StoreName']@
-- list declares which state stores the processor will access
-- — you must separately register each named store via
-- 'withStateStoreKV' / @W@ / @S@, with this processor's
-- generated 'NodeName' as an owner. /Because the generated
-- name isn't visible/, the common case of "one processor +
-- one store" is much easier with 'processWithStateStoreKV'.
processStream
  :: Text -> [StoreName] -> IO (Processor k v)
  -> Topology (KStream k v) ()
processStream nm stores sup = liftPrim (ProcessStream nm stores sup)

-- | Attach a custom processor that produces a typed
-- downstream 'KStream'. The output value type and serde
-- come from the explicit 'Serde' argument.
--
-- /JVM equivalent:/ @KStream.processValues(ProcessorSupplier, storeNames)@.
--
-- Same store-ownership caveat as 'processStream'; for the
-- single-store common case use 'processWithStateStoreKV' or
-- its windowed / session counterparts.
processValuesStream
  :: Text -> [StoreName] -> IO (Processor k v) -> Serde v'
  -> Topology (KStream k v) (KStream k v')
processValuesStream nm stores sup vs =
  liftPrim (ProcessValuesStream nm stores sup vs)

-- | Legacy /transformValues/ wiring: same as
-- 'processValuesStream' but the store list is typed as
-- @['Topo.NodeName']@ for compatibility with the original
-- imperative DSL. Prefer 'processValuesStream' for new code.
transformValuesStream
  :: Text
  -> [Topo.NodeName]
  -> IO (Processor k v)
  -> Serde v'
  -> Topology (KStream k v) (KStream k v')
transformValuesStream nm stores sup vs =
  liftPrim (TransformValuesStreamT nm stores sup vs)

-- | Register a 'StoreBuilderKV' against the topology graph
-- and grant access to the named owner processors. Wire is
-- unchanged — compose anywhere via @'>>>'@.
--
-- The @['Topo.NodeName']@ list is the set of processors that
-- can read/write the store. Auto-generated names from
-- 'processStream' aren't easy to predict, so for the common
-- single-owner case use 'processWithStateStoreKV' instead
-- (it generates the name and registers the store atomically).
--
-- == Use cases for the standalone form
--
--   * Multi-owner stores shared across N processors (rare).
--   * Stores attached after-the-fact via
--     @connectProcessorAndStateStores@.
--   * Hand-rolled imperative wiring spliced in via
--     'liftIO_' with known node names.
withStateStoreKV
  :: StoreBuilderKV k v -> [Topo.NodeName] -> Topology x x
withStateStoreKV b owners = liftPrim (WithStateStoreKV b owners)

-- | 'withStateStoreKV' for a /window/ state store. Same
-- semantics: register the store, grant access to the named
-- owner processors, wire passes through unchanged.
withStateStoreW
  :: StoreBuilderW k v -> [Topo.NodeName] -> Topology x x
withStateStoreW b owners = liftPrim (WithStateStoreW b owners)

-- | 'withStateStoreKV' for a /session/ state store.
withStateStoreS
  :: StoreBuilderS k v -> [Topo.NodeName] -> Topology x x
withStateStoreS b owners = liftPrim (WithStateStoreS b owners)

-- | Atomically attach a custom 'Processor' /and/ its
-- 'StoreBuilderKV' to the upstream 'KStream'. The compiler
-- generates the processor's 'Topo.NodeName' from the prefix,
-- attaches the store's name to its @processorSpecStores@ list,
-- and registers the store with that generated node as its
-- owner — so callers don't need to coordinate node names manually
-- between 'processStream' and 'withStateStoreKV'.
--
-- Use this whenever your processor reads or writes a state store
-- that no other processor shares (the common case). For
-- multi-owner state stores, stay with the
-- 'processStream' + 'withStateStoreKV' split and pass an
-- explicit 'Topo.NodeName' yourself (currently requires the
-- 'liftIO_' escape; a dedicated /named/ variant is on the
-- follow-up list).
processWithStateStoreKV
  :: Text
  -> StoreBuilderKV stk stv
  -> IO (Processor k v)
  -> Topology (KStream k v) ()
processWithStateStoreKV prefix builder sup =
  liftPrim (ProcessWithStateStoreKV prefix builder sup)

processWithStateStoreW
  :: Text
  -> StoreBuilderW stk stv
  -> IO (Processor k v)
  -> Topology (KStream k v) ()
processWithStateStoreW prefix builder sup =
  liftPrim (ProcessWithStateStoreW prefix builder sup)

processWithStateStoreS
  :: Text
  -> StoreBuilderS stk stv
  -> IO (Processor k v)
  -> Topology (KStream k v) ()
processWithStateStoreS prefix builder sup =
  liftPrim (ProcessWithStateStoreS prefix builder sup)

----------------------------------------------------------------------
-- Escape hatch + introspection
----------------------------------------------------------------------

-- | Escape hatch: splice in a hand-rolled
-- 'StreamsBuilder'-mutating action as a topology fragment.
--
-- The supplied function receives the shared 'StreamsBuilder'
-- and the upstream wire value, performs whatever imperative
-- operations it likes (registering nodes, attaching stores,
-- calling into the original DSL), and returns the downstream
-- wire value.
--
-- == When to use
--
-- 'liftIO_' is the catch-all for operations that aren't yet
-- a dedicated GADT constructor. Typical cases:
--
--   * Calling into the imperative @Kafka.Streams.KStream@ /
--     @Kafka.Streams.KTable@ DSL for an operator we haven't
--     promoted yet.
--   * One-off custom processors that don't fit
--     'processStream' (e.g. needing explicit node-name
--     control).
--   * Bridging Layer-4 ('Topology') and Layer-1 ('StreamsBuilder')
--     during a gradual migration.
--
-- The supplied 'Text' is the label used by 'inspect' /
-- 'prettyPrint' so the AST remains legible. Keep it
-- descriptive (e.g. @\"custom-windowed-suppress\"@ rather
-- than @\"helper\"@); the optimiser and inspection passes
-- can't see past 'liftIO_', so the label is your only
-- breadcrumb.
--
-- == Implications
--
--   * The optimiser cannot rewrite across or into a
--     'liftIO_' boundary. If you have repeated
--     'liftIO_'-wrapped fragments, fusion opportunities are
--     lost.
--   * Whatever the action does at run time is opaque to
--     'compile' — caller-asserted correctness.
--   * EOS atomicity still holds /if/ the action only mutates
--     the shared topology graph: any processors / sinks it
--     registers go through the same transactional producer.
--     Out-of-band side channels (e.g. an action that
--     writes to a database) are NOT part of the EOS
--     transaction.
liftIO_
  :: Text
  -> (StreamsBuilder -> i -> IO o)
  -> Topology i o
liftIO_ nm act = liftPrim (Lifted nm act)

----------------------------------------------------------------------
-- KIP-307: Named operator variants
--
-- Mirror the imperative @*Named@ entry points so callers can pin
-- a topology node name on a per-operator basis. Stable node names
-- matter for stateful application upgrades — re-deploying with a
-- different node name regenerates the changelog / repartition
-- topic, which generally invalidates the existing state.
--
-- Each helper delegates to the corresponding imperative @*Named@
-- function via 'liftIO_'. The Named-vs-default split mirrors the
-- imperative API exactly; we don't push Named through the GADT
-- primitives to avoid duplicating every constructor.
----------------------------------------------------------------------

-- | 'filter' with an explicit topology node name.
filterNamed
  :: Named.Named -> (Record k v -> Bool)
  -> Topology (KStream k v) (KStream k v)
filterNamed nm p =
  liftIO_ "filter-named" (\_b s -> KS.filterStreamNamed nm p s)

-- | 'mapValues' with an explicit topology node name.
mapValuesNamed
  :: HasSerde v'
  => Named.Named -> (v -> v')
  -> Topology (KStream k v) (KStream k v')
mapValuesNamed nm f =
  liftIO_ "mapValues-named" (\_b s -> KS.mapValuesNamed nm f s)

-- | 'mapKeyValue' with an explicit topology node name.
mapKeyValueNamed
  :: (HasSerde k', HasSerde v')
  => Named.Named -> (k -> v -> (k', v'))
  -> Topology (KStream k v) (KStream k' v')
mapKeyValueNamed nm f =
  liftIO_ "mapKeyValue-named" (\_b s -> KS.mapKeyValueNamed nm f s)

-- | 'peek' with an explicit topology node name.
peekNamed
  :: Named.Named -> (Record k v -> IO ())
  -> Topology (KStream k v) (KStream k v)
peekNamed nm act =
  liftIO_ "peek-named" (\_b s -> KS.peekStreamNamed nm act s)

-- | 'selectKey' with an explicit topology node name.
selectKeyNamed
  :: HasSerde k'
  => Named.Named -> (Record k v -> k')
  -> Topology (KStream k v) (KStream k' v)
selectKeyNamed nm f =
  liftIO_ "selectKey-named" (\_b s -> KS.selectKeyNamed nm f s)

-- | 'sinkWith' with an explicit topology node name.
sinkNamed
  :: Named.Named -> TopicName -> Produced k v
  -> Topology (KStream k v) ()
sinkNamed nm tp p =
  liftIO_ "sink-named" (\_b s -> KS.toTopicNamed nm tp p s)

----------------------------------------------------------------------
-- KIP-825: configurable windowed emit strategy
----------------------------------------------------------------------

-- | Configure the emit strategy on a topology fragment that
-- produces a 'TWKS.WindowedTableHandle'. The default emit
-- strategy is 'TWKS.emitOnWindowUpdate' (every update
-- forwards downstream); use 'TWKS.emitOnWindowClose' to defer
-- emission until the window has closed past its grace period.
--
-- Operates on the /handle/ after the windowed aggregation has
-- been built, so it composes cleanly with 'aggregateWindowed',
-- 'countWindowed', and 'reduceWindowed':
--
-- @
-- F.aggregateWindowed seed step m
--   '>>>' F.withEmitStrategy F.emitOnWindowClose
-- @
withEmitStrategy
  :: TWKS.EmitStrategy
  -> Topology a (TWKS.WindowedTableHandle k v)
  -> Topology a (TWKS.WindowedTableHandle k v)
withEmitStrategy e = rmap (TWKS.withEmitStrategy e)

----------------------------------------------------------------------
-- Configurable suppression (JVM @Suppressed@ buffer config)
----------------------------------------------------------------------

-- | Re-export of 'Kafka.Streams.Suppress.BufferConfig'.
type BufferConfig = Suppress.BufferConfig

-- | 'suppressWindowed' with an explicit 'BufferConfig'. Mirrors
-- the JVM @Suppressed.untilWindowCloses(BufferConfig)@ surface:
-- the buffer can be sized by record count
-- ('Suppress.maxRecordsBufferConfig'), approximate byte budget
-- ('Suppress.maxBytesBufferConfig'), or set to overflow-emit
-- (early-emit when full) versus overflow-shutdown via the
-- 'Suppress.BufferConfig' constructor.
--
-- Use this instead of 'suppressWindowed' when you need bounded
-- memory under load — the default 'suppressWindowed' uses an
-- unbounded buffer that will keep growing until either the
-- window closes or the JVM runs out of memory.
suppressWindowedWith
  :: Ord k
  => Duration             -- ^ grace period
  -> Int64                -- ^ window size (ms)
  -> BufferConfig
  -> Topology (KStream (WindowedKey k) v) (KStream (WindowedKey k) v)
suppressWindowedWith grace sz cfg =
  liftIO_ "suppressWindowed-with"
    (\_b s -> Suppress.suppressWindowedWith grace sz cfg s)

-- | Re-export: bounded-by-record-count buffer config. Cap is a
-- soft limit on the number of buffered windowed keys before the
-- overflow policy kicks in.
maxRecordsBufferConfig :: Int -> BufferConfig
maxRecordsBufferConfig = Suppress.maxRecordsBufferConfig

-- | Re-export: bounded-by-bytes buffer config. The byte count
-- is approximate — the processor sees the unserialised value,
-- so we treat the cap as a record-count surrogate.
maxBytesBufferConfig :: Int -> BufferConfig
maxBytesBufferConfig = Suppress.maxBytesBufferConfig

----------------------------------------------------------------------
-- Late store attachment / global stores
----------------------------------------------------------------------

-- | Register a /global/ key-value store backed by its own
-- source + updater processor. Mirrors
-- 'Kafka.Streams.Topology.addGlobalStore'. The store is
-- replicated on every instance (the runtime treats global
-- stores as cluster-wide bypassing partition assignment).
--
-- Use 'globalTableSource' for the high-level "global lookup
-- table" API; this lower-level entry point lets you run
-- arbitrary updater logic. Either way, joins downstream don't
-- require co-partitioning of the source topic.
addGlobalStore
  :: StoreBuilderKV k v
  -> Text                                  -- ^ source node name
  -> Text                                  -- ^ updater processor node name
  -> TopicName
  -> Serde k
  -> Serde v
  -> Time.TimestampExtractor k v
  -> IO (Processor k v)                    -- ^ updater supplier
  -> Topology a a
addGlobalStore builder srcNm procNm topic ks vs ex updater =
  liftIO_ "addGlobalStore" $ \b x -> do
    withTopology_ b $
      Topo.addGlobalStore
        builder
        (Topo.NodeName srcNm)
        (Topo.NodeName procNm)
        topic ks vs ex
        (Topo.AnyProcessor updater)
    pure x

-- | Connect a previously-added processor to one or more
-- previously-added state stores. Mirrors
-- 'Kafka.Streams.Topology.connectProcessorAndStateStores' for
-- multi-owner / late-wiring patterns. The processor must
-- already exist; the stores must already exist. Mostly used
-- after a 'liftIO_' or 'processStream' that registered a
-- processor under a known node name.
connectProcessorAndStateStores
  :: Text             -- ^ processor node name
  -> [StoreName]
  -> Topology a a
connectProcessorAndStateStores procNm stores =
  liftIO_ "connectProcessorAndStateStores" $ \b x -> do
    withTopology_ b $
      Topo.connectProcessorAndStateStores
        (Topo.NodeName procNm) stores
    pure x

----------------------------------------------------------------------
-- Rich sink spec (custom partitioner, explicit node name)
----------------------------------------------------------------------

-- | 'sink' that accepts a full 'Topo.SinkSpec'. The richer
-- variant lets callers specify a custom partitioner / explicit
-- node name that the standard 'sinkWith' (which goes through
-- 'KS.toTopic') doesn't expose.
sinkSpec :: Topo.SinkSpec -> Topology (KStream k v) ()
sinkSpec spec =
  liftIO_ "sinkSpec" $ \b s -> do
    withTopology_ b $ \t ->
      let !t' = Topo.addSinkWith
                  (spec { Topo.sinkParents = [KS.kstreamParent s] })
                  t
       in t'
    pure ()

----------------------------------------------------------------------
-- Stateful key-changing transform
----------------------------------------------------------------------

-- | A stateful key-changing transformer — the equivalent of
-- JVM @KStream.transform(TransformerSupplier)@. The processor
-- receives every record, may consult and update declared state
-- stores, and emits zero or more typed-key/value records
-- downstream.
--
-- This is the most general primitive: it can change BOTH key
-- and value, run IO, and reach state stores attached via
-- 'withStateStoreKV' or 'connectProcessorAndStateStores'.
-- Where 'processValuesStream' is restricted to per-record
-- /value/ transforms with optional store access,
-- 'transformStream' is unrestricted.
--
-- Pair with 'withStateStoreKV' (or 'connectProcessorAndStateStores')
-- to wire in state stores; the @['StoreName']@ list goes into
-- the processor's @processorSpecStores@ and grants access.
transformStream
  :: Text                                  -- ^ name prefix
  -> [StoreName]
  -> IO (Processor k v)
  -> Serde k'
  -> Serde v'
  -> Topology (KStream k v) (KStream k' v')
transformStream prefix stores sup ks' vs' =
  liftIO_ "transformStream" $ \b s -> do
    nm <- freshNodeName b prefix
    withTopology_ b $
      Topo.addProcessorWith
        Topo.ProcessorSpec
          { Topo.processorSpecName     = nm
          , Topo.processorSpecParents  = [KS.kstreamParent s]
          , Topo.processorSpecSupplier = Topo.AnyProcessor sup
          , Topo.processorSpecStores   = stores
          }
    pure KS.KStream
      { KS.kstreamBuilder    = b
      , KS.kstreamParent     = nm
      , KS.kstreamKeySerde   = ks'
      , KS.kstreamValueSerde = vs'
      }

----------------------------------------------------------------------
-- KIP-545: TableJoined-aware FK joins
----------------------------------------------------------------------

-- | 'foreignKeyJoin' with a 'TableJoined' override
-- (naming + partitioner). See 'FK.foreignKeyJoinKTableWith' for
-- which parts of the override are honored by the in-process
-- driver today.
foreignKeyJoinWith
  :: (Ord k, Ord fk, Hashable v, HasSerde v')
  => Joined.TableJoined k fk
  -> (v -> fk)
  -> (v -> vr -> v')
  -> Materialized k v'
  -> Topology (KTable k v, KTable fk vr) (KTable k v')
foreignKeyJoinWith tj ext j m =
  liftIO_ "foreignKeyJoinWith" $ \_b (t1, t2) ->
    FK.foreignKeyJoinKTableWith tj ext j m t1 t2

-- | 'leftForeignKeyJoin' with a 'TableJoined' override.
leftForeignKeyJoinWith
  :: (Ord k, Ord fk, Hashable v, HasSerde v')
  => Joined.TableJoined k fk
  -> (v -> fk)
  -> (v -> Maybe vr -> v')
  -> Materialized k v'
  -> Topology (KTable k v, KTable fk vr) (KTable k v')
leftForeignKeyJoinWith tj ext j m =
  liftIO_ "leftForeignKeyJoinWith" $ \_b (t1, t2) ->
    FK.leftForeignKeyJoinKTableWith tj ext j m t1 t2

----------------------------------------------------------------------
-- Time-windowed cogroup
----------------------------------------------------------------------

-- | Re-export: bucket a 'CogroupedStream' into time windows.
windowedByCogroup
  :: Win.Windows
  -> Topology (Cog.CogroupedStream k a) (Cog.TimeWindowedCogroupedStream k a)
windowedByCogroup ws = FA.Arr (Cog.windowedByCogroup ws)

-- | Close out a time-windowed cogroup builder and emit the
-- result as a windowed table.
aggregateWindowedCogrouped
  :: Ord k
  => IO a -> Materialized k a
  -> Topology (Cog.TimeWindowedCogroupedStream k a) (TWKS.WindowedTableHandle k a)
aggregateWindowedCogrouped seed m =
  liftIO_ "aggregateWindowedCogrouped" $ \_b twcg ->
    Cog.aggregateWindowedCogrouped seed m twcg

----------------------------------------------------------------------
-- Windowed-handle conversions
----------------------------------------------------------------------

-- | Bridge a time-'WindowedTableHandle' into a
-- @'KStream' ('WindowedKey' k) v@. Each downstream record
-- corresponds to one update of the underlying window store; with
-- 'TWKS.emitOnWindowClose' the conversion routes through suppress
-- so each (key, window) emits exactly once at close + grace.
--
-- The supplied serdes are baked into the resulting 'KStream' so
-- downstream sinks have them available. /JVM equivalent:/
-- 'TimeWindowedKStream.toStream()'.
streamFromWindowed
  :: forall k v
   . (Ord k, Eq v, HasSerde k, HasSerde v)
  => Topology (TWKS.WindowedTableHandle k v) (KStream (WindowedKey k) v)
streamFromWindowed =
  liftIO_ "streamFromWindowed" $ \_b h ->
    -- 'Suppress.streamFromWindowedHandle' threads a key serde
    -- through purely for symmetry: the resulting 'KStream' uses
    -- an @error "WindowedKey serde unset"@ placeholder for
    -- 'kstreamKeySerde' because downstream sinks always supply a
    -- 'windowedSerde'-aware composite via 'Produced'. The class
    -- 'HasSerde k' constraint matches the rest of the Free DSL
    -- (no explicit serde threading at type-changing operators).
    Suppress.streamFromWindowedHandle h (serde @k) (serde @v)

-- | Bridge a 'SWKS.SessionWindowedTableHandle' into a
-- @'KStream' k v@ pinned at the session aggregator's emit node.
-- The aggregator forwards plain-keyed records (inner @k@, not
-- 'SWKS.SessionKey'), so the downstream stream is keyed by @k@.
streamFromSessionWindowed
  :: forall k v
   . (Ord k, HasSerde k, HasSerde v)
  => Topology (SWKS.SessionWindowedTableHandle k v) (KStream k v)
streamFromSessionWindowed =
  liftIO_ "streamFromSessionWindowed" $ \_b h ->
    Suppress.streamFromSessionWindowedHandle h (serde @k) (serde @v)

----------------------------------------------------------------------
-- Dead-letter shelf (Riffle shed policy)
----------------------------------------------------------------------

-- | KIP-1033-style overflow routing for the windowed suppress
-- buffer: when the buffer exceeds 'Suppress.dlsRecordCap', the
-- oldest entries are serialised and pushed to a configured side
-- topic instead of being dropped, emitted early, or failing the
-- task. See 'Suppress.suppressWindowedShed' for the imperative
-- equivalent.
suppressWindowedShed
  :: Ord k
  => Duration
  -> Int64
  -> Suppress.DeadLetterShelf k v
  -> Topology (KStream (WindowedKey k) v) (KStream (WindowedKey k) v)
suppressWindowedShed grace winMs shelf =
  liftIO_ "suppressWindowedShed" $ \_b s ->
    Suppress.suppressWindowedShed grace winMs shelf s

----------------------------------------------------------------------
-- KTable.groupBy (Java) lowered onto KStream
----------------------------------------------------------------------

-- | Java's @KTable.groupBy(KeyValueMapper)@ lowered: convert the
-- table to a stream, then re-key. Useful when you want to
-- re-aggregate a 'KTable' under a derived key without taking the
-- subtractor-aware 'groupTableBy' / 'KGroupedTable' path.
groupByKTable
  :: (Ord k, Ord k', HasSerde k', HasSerde v)
  => (k -> v -> k')
  -> Topology (KTable k v) (KStream k' v)
groupByKTable keyMap =
  liftIO_ "groupByKTable" $ \_b kt -> KS.groupByKTable keyMap kt

----------------------------------------------------------------------
-- Global read-only state stores (KIP-813)
----------------------------------------------------------------------

-- | Register a global, read-only state store with a custom
-- updater processor. Mirrors
-- 'Kafka.Streams.StreamsBuilder.addReadOnlyStateStore'.
--
-- The store is materialised once per Kafka Streams instance and
-- updated by the @updater@ processor in response to records on
-- the supplied @topic@. Other processors can /read/ it through
-- 'getStateStore' but should not write to it (the updater holds
-- exclusive write access).
--
-- Wire is unchanged — compose anywhere with @'>>>'@.
addReadOnlyStateStore
  :: StoreBuilderKV k v
  -> Topo.NodeName              -- ^ source node name
  -> Topo.NodeName              -- ^ updater (processor) node name
  -> TopicName                  -- ^ source topic
  -> Serde k                    -- ^ key serde of the source topic
  -> Serde v                    -- ^ value serde of the source topic
  -> Time.TimestampExtractor k v
  -> Topo.AnyProcessor          -- ^ updater body
  -> Topology x x
addReadOnlyStateStore builder srcNm procNm topic ks vs ex updater =
  liftIO_ "addReadOnlyStateStore" $ \b s -> do
    SB.addReadOnlyStateStore b builder srcNm procNm topic ks vs ex updater
    pure s

----------------------------------------------------------------------
-- KIP-820: FixedKeyProcessor
----------------------------------------------------------------------

-- | KIP-820 fixed-key processor. The processor's input and output
-- share a key, expressed in the type — useful when the JVM API
-- would have you reach for @KStream.processValues@ with a
-- key-preserving processor supplier.
--
-- This is a thin wrapper around 'processValuesStream' that
-- adapts a 'FixedKeyProcessor' to the regular 'Processor'
-- shape via 'P.liftFixedKeyProcessor'.
processFixedKeyValuesStream
  :: Text
  -> [StoreName]
  -> IO (FixedKeyProcessor k v v')
  -> Serde v'
  -> Topology (KStream k v) (KStream k v')
processFixedKeyValuesStream nm stores fkp vs =
  processValuesStream nm stores (fkp >>= P.liftFixedKeyProcessor) vs

-- | 'processWithStateStoreKV' with a 'FixedKeyProcessor'. The
-- processor is terminal (returns @()@); use a regular sink or
-- 'tap' downstream if you need to publish forwarded records.
processFixedKeyValuesWithStateStoreKV
  :: Text
  -> StoreBuilderKV stk stv
  -> IO (FixedKeyProcessor k v v')
  -> Topology (KStream k v) ()
processFixedKeyValuesWithStateStoreKV prefix b fkp =
  processWithStateStoreKV prefix b (fkp >>= P.liftFixedKeyProcessor)

----------------------------------------------------------------------
-- KIP-307: more named operator variants (symmetric set)
----------------------------------------------------------------------

-- | 'filterNot' with an explicit topology node name.
filterNotNamed
  :: Named.Named -> (Record k v -> Bool)
  -> Topology (KStream k v) (KStream k v)
filterNotNamed nm p =
  liftIO_ "filterNot-named" (\_b s -> KS.filterStreamNamed nm (not . p) s)

-- | 'mapValuesM' with an explicit topology node name.
mapValuesMNamed
  :: HasSerde v'
  => Named.Named -> (v -> IO v')
  -> Topology (KStream k v) (KStream k v')
mapValuesMNamed nm f =
  liftIO_ "mapValuesM-named" (\_b s -> KS.mapValuesMNamed nm f s)

-- | 'mapKeyValueM' with an explicit topology node name.
mapKeyValueMNamed
  :: (HasSerde k', HasSerde v')
  => Named.Named -> (k -> v -> IO (k', v'))
  -> Topology (KStream k v) (KStream k' v')
mapKeyValueMNamed nm f =
  liftIO_ "mapKeyValueM-named" (\_b s -> KS.mapKeyValueMNamed nm f s)

-- | 'mapRecord' with an explicit topology node name.
mapRecordNamed
  :: (HasSerde k', HasSerde v')
  => Named.Named -> (Record k v -> Record k' v')
  -> Topology (KStream k v) (KStream k' v')
mapRecordNamed nm f =
  liftIO_ "mapRecord-named" (\_b s -> KS.mapRecordNamed nm f s)

-- | 'mapRecordM' with an explicit topology node name.
mapRecordMNamed
  :: (HasSerde k', HasSerde v')
  => Named.Named -> (Record k v -> IO (Record k' v'))
  -> Topology (KStream k v) (KStream k' v')
mapRecordMNamed nm f =
  liftIO_ "mapRecordM-named" (\_b s -> KS.mapRecordMNamed nm f s)

-- | 'concatMapValues' with an explicit topology node name.
concatMapValuesNamed
  :: HasSerde v'
  => Named.Named -> (v -> [v'])
  -> Topology (KStream k v) (KStream k v')
concatMapValuesNamed nm f =
  liftIO_ "concatMapValues-named" (\_b s -> KS.concatMapValuesNamed nm f s)

-- | 'concatMapKeyValue' with an explicit topology node name.
concatMapKeyValueNamed
  :: (HasSerde k', HasSerde v')
  => Named.Named -> (k -> v -> [(k', v')])
  -> Topology (KStream k v) (KStream k' v')
concatMapKeyValueNamed nm f =
  liftIO_ "concatMapKeyValue-named" (\_b s -> KS.concatMapKeyValueNamed nm f s)

-- | 'values' with an explicit topology node name.
valuesNamed
  :: Named.Named
  -> Topology (KStream k v) (KStream () v)
valuesNamed nm =
  liftIO_ "values-named" (\_b s -> KS.valuesStreamNamed nm s)

-- | 'merge' with an explicit topology node name for the merge
-- processor.
mergeNamed
  :: Named.Named
  -> Topology (KStream k v, KStream k v) (KStream k v)
mergeNamed nm =
  liftIO_ "merge-named" (\_b (s1, s2) -> KS.mergeStreamsNamed nm s1 s2)

-- | 'repartition' with an explicit 'Named.Named' wrapper around
-- the generated prefix.
repartitionNamed
  :: Named.Named
  -> Topology (KStream k v) (KStream k v)
repartitionNamed nm =
  liftIO_ "repartition-named" (\_b s -> KS.repartitionNamed nm s)

----------------------------------------------------------------------
-- Two-input join helpers
--
-- Convenience wrappers: take both sides as @'Topology' 'Void' _@
-- legs and apply the corresponding join. Equivalent to
-- @left '&&&' right '>>>' joinPrim …@ but reads better at the
-- call site, especially when the two legs are themselves
-- multi-stage pipelines.
----------------------------------------------------------------------

-- | Stream-table /inner/ join from two source-rooted legs.
joinStreamTable
  :: (Ord k, HasSerde v')
  => Topology Void (KStream k v)
  -> Topology Void (KTable k vt)
  -> (v -> vt -> v')
  -> Joined k v vt
  -> Topology Void (KStream k v')
joinStreamTable left right j jo =
  (left &&& right) >>> streamTableJoin j jo

-- | Stream-table /left/ join from two source-rooted legs.
leftJoinStreamTable
  :: (Ord k, HasSerde v')
  => Topology Void (KStream k v)
  -> Topology Void (KTable k vt)
  -> (v -> Maybe vt -> v')
  -> Joined k v vt
  -> Topology Void (KStream k v')
leftJoinStreamTable left right j jo =
  (left &&& right) >>> streamTableLeftJoin j jo

-- | Stream-stream /windowed inner/ join from two source-rooted legs.
joinStreamStream
  :: (Ord k, HasSerde v')
  => Topology Void (KStream k v1)
  -> Topology Void (KStream k v2)
  -> (v1 -> v2 -> v')
  -> JoinWindows
  -> Joined k v1 v2
  -> Topology Void (KStream k v')
joinStreamStream left right j w jo =
  (left &&& right) >>> streamStreamJoin j w jo

-- | Stream-stream /windowed left/ join from two source-rooted legs.
leftJoinStreamStream
  :: (Ord k, HasSerde v')
  => Topology Void (KStream k v1)
  -> Topology Void (KStream k v2)
  -> (v1 -> Maybe v2 -> v')
  -> JoinWindows
  -> Joined k v1 v2
  -> Topology Void (KStream k v')
leftJoinStreamStream left right j w jo =
  (left &&& right) >>> streamStreamLeftJoin j w jo

-- | Stream-stream /windowed outer/ join from two source-rooted legs.
outerJoinStreamStream
  :: (Ord k, HasSerde v')
  => Topology Void (KStream k v1)
  -> Topology Void (KStream k v2)
  -> (Maybe v1 -> Maybe v2 -> v')
  -> JoinWindows
  -> Joined k v1 v2
  -> Topology Void (KStream k v')
outerJoinStreamStream left right j w jo =
  (left &&& right) >>> streamStreamOuterJoin j w jo

-- | Table-table /inner/ join from two source-rooted legs.
joinTableTable
  :: Ord k
  => Topology Void (KTable k v1)
  -> Topology Void (KTable k v2)
  -> (v1 -> v2 -> v')
  -> Materialized k v'
  -> Topology Void (KTable k v')
joinTableTable left right j m =
  (left &&& right) >>> tableTableJoin j m

-- | Table-table /left/ join from two source-rooted legs.
leftJoinTableTable
  :: Ord k
  => Topology Void (KTable k v1)
  -> Topology Void (KTable k v2)
  -> (v1 -> Maybe v2 -> v')
  -> Materialized k v'
  -> Topology Void (KTable k v')
leftJoinTableTable left right j m =
  (left &&& right) >>> tableTableLeftJoin j m

-- | Table-table /outer/ join from two source-rooted legs.
outerJoinTableTable
  :: Ord k
  => Topology Void (KTable k v1)
  -> Topology Void (KTable k v2)
  -> (Maybe v1 -> Maybe v2 -> v')
  -> Materialized k v'
  -> Topology Void (KTable k v')
outerJoinTableTable left right j m =
  (left &&& right) >>> tableTableOuterJoin j m

-- | KIP-213 foreign-key /inner/ join from two source-rooted legs.
joinForeignKey
  :: (Ord k, Ord fk, Hashable v, HasSerde v')
  => Topology Void (KTable k v)
  -> Topology Void (KTable fk vr)
  -> (v -> fk)
  -> (v -> vr -> v')
  -> Materialized k v'
  -> Topology Void (KTable k v')
joinForeignKey left right ext j m =
  (left &&& right) >>> foreignKeyJoin ext j m

-- | KIP-213 foreign-key /left/ join from two source-rooted legs.
leftJoinForeignKey
  :: (Ord k, Ord fk, Hashable v, HasSerde v')
  => Topology Void (KTable k v)
  -> Topology Void (KTable fk vr)
  -> (v -> fk)
  -> (v -> Maybe vr -> v')
  -> Materialized k v'
  -> Topology Void (KTable k v')
leftJoinForeignKey left right ext j m =
  (left &&& right) >>> leftForeignKeyJoin ext j m

-- | Stream-globalTable /inner/ join from two source-rooted legs.
joinStreamGlobalTable
  :: (Ord kg, HasSerde v')
  => Topology Void (KStream k v)
  -> Topology Void (GlobalKTable kg vg)
  -> (k -> v -> kg)
  -> (v -> vg -> v')
  -> Topology Void (KStream k v')
joinStreamGlobalTable left right km j =
  (left &&& right) >>> streamGlobalTableJoin km j

-- | Stream-globalTable /left/ join from two source-rooted legs.
leftJoinStreamGlobalTable
  :: (Ord kg, HasSerde v')
  => Topology Void (KStream k v)
  -> Topology Void (GlobalKTable kg vg)
  -> (k -> v -> kg)
  -> (v -> Maybe vg -> v')
  -> Topology Void (KStream k v')
leftJoinStreamGlobalTable left right km j =
  (left &&& right) >>> streamGlobalTableLeftJoin km j

-- | Walk the AST and collect a per-node textual label
-- listing.
--
-- The output is a list of operator-shape tokens in roughly
-- the order they'd appear in a topology-description string —
-- compose two via @'>>>'@ and the resulting 'inspect' has
-- the left side's tokens followed by the right side's
-- tokens.
--
-- /Comparable to:/ Java's
-- @org.apache.kafka.streams.TopologyDescription@. Our output
-- is a simpler tokenised form, not the full graph
-- description; it's intended for tests, golden-file
-- comparisons, and assertions on AST shape.
--
-- == What you can rely on
--
--   * Every constructor produces at least one token.
--   * Structural constructors ('Compose', 'First', 'Parallel',
--     etc.) bracket their children with angle-bracket tokens
--     so nesting is visible.
--   * The labels are stable for any given operator name in
--     the AST; 'inspect' is the basis of the introspection
--     tests in @Streams.TopologyFreeSpec@.
--
-- == What you should NOT rely on
--
--   * The exact token format is /not/ a stable public API —
--     use 'inspect' for debugging and golden-file tests, not
--     for parsing.
--   * The token list is /not/ a topological order of the
--     compiled-graph nodes; it follows the AST structure,
--     not the runtime processor graph.
--   * The continuation of a 'Bind' is opaque — 'inspect'
--     shows the left side and a single @"…>"@ marker.
inspect :: Topology i o -> [Text]
inspect = FA.inspectFA labelPrim

-- | A single textual token for a 'Prim' constructor. Used by
-- 'inspect', 'applyTraced', 'inspectDeep', 'prettyPrint', and
-- the Graphviz renderer.
labelPrim :: forall i o. Prim i o -> Text
labelPrim p0 = case p0 of
  Split bs md ->
    "Split(" <> T.intercalate "|" (Prelude.map sbName bs)
      <> maybe "" (\d -> "+default=" <> d) md <> ")"
  Source t _         -> "Source(" <> showTopic t <> ")"
  SourceMulti ts _   ->
    "SourceMulti(" <> T.intercalate "," (Prelude.map showTopic (NE.toList ts))
      <> ")"
  TableSource t _ _  -> "TableSource("  <> showTopic t <> ")"
  GlobalSource t _ _ -> "GlobalSource(" <> showTopic t <> ")"
  Sink t _           -> "Sink("         <> showTopic t <> ")"
  SinkExtracted _ _  -> "SinkExtracted"
  SinkTwoPhase sink  -> "SinkTwoPhase(" <> TPS.tpsName sink <> ")"
  Through t _        -> "Through("      <> showTopic t <> ")"
  MapValues _        -> "MapValues"
  MapValuesM _       -> "MapValuesM"
  MapKeyValue _      -> "MapKeyValue"
  MapKeyValueM _     -> "MapKeyValueM"
  AsyncMapValues cfg _     ->
    "AsyncMapValues(" <> AIO.aioName cfg <> ")"
  AsyncMapKeyValue cfg _   ->
    "AsyncMapKeyValue(" <> AIO.aioName cfg <> ")"
  AsyncConcatMapValues cfg _ ->
    "AsyncConcatMapValues(" <> AIO.aioName cfg <> ")"
  MapRecord _        -> "MapRecord"
  MapRecordM _       -> "MapRecordM"
  NoFuse             -> "NoFuse"
  Filter _           -> "Filter"
  FilterNot _        -> "FilterNot"
  ConcatMapValues _    -> "ConcatMapValues"
  ConcatMapKeyValue _  -> "ConcatMapKeyValue"
  Peek _             -> "Peek"
  Foreach _          -> "Foreach"
  SelectKey _        -> "SelectKey"
  Values             -> "Values"
  Print nm _         -> "Print(" <> nm <> ")"
  Merge              -> "Merge"
  MergeAll           -> "MergeAll"
  Branch ps          -> "Branch(" <> T.pack (show (length ps)) <> ")"
  ToTableT _         -> "ToTable"
  ToStream           -> "ToStream"
  Repartition pfx    -> "Repartition(" <> pfx <> ")"
  RepartitionWith _  -> "RepartitionWith"
  GroupByKey _       -> "GroupByKey"
  GroupBy _ _        -> "GroupBy"
  Count _            -> "Count"
  Reduce _ _         -> "Reduce"
  Aggregate _ _ _    -> "Aggregate"
  WindowedByTime _   -> "WindowedByTime"
  WindowedBySession _ -> "WindowedBySession"
  CountWindowed _    -> "CountWindowed"
  ReduceWindowed _ _ -> "ReduceWindowed"
  AggregateWindowed _ _ _ -> "AggregateWindowed"
  CountSessionWindowed _  -> "CountSessionWindowed"
  AggregateSessionWindowed _ _ _ _ -> "AggregateSessionWindowed"
  GroupTableBy _ _                 -> "GroupTableBy"
  CountKGroupedTable _             -> "CountKGroupedTable"
  ReduceKGroupedTable _ _ _        -> "ReduceKGroupedTable"
  AggregateKGroupedTable _ _ _ _   -> "AggregateKGroupedTable"
  Cogroup _                        -> "Cogroup"
  AddCogrouped _                   -> "AddCogrouped"
  AggregateCogrouped _ _           -> "AggregateCogrouped"
  StreamTableJoin _ _              -> "StreamTableJoin"
  StreamTableLeftJoin _ _          -> "StreamTableLeftJoin"
  StreamStreamJoin _ _ _           -> "StreamStreamJoin"
  StreamStreamLeftJoin _ _ _       -> "StreamStreamLeftJoin"
  StreamStreamOuterJoin _ _ _      -> "StreamStreamOuterJoin"
  TableTableJoin _ _               -> "TableTableJoin"
  TableTableLeftJoin _ _           -> "TableTableLeftJoin"
  TableTableOuterJoin _ _          -> "TableTableOuterJoin"
  ForeignKeyJoin _ _ _             -> "ForeignKeyJoin"
  LeftForeignKeyJoin _ _ _         -> "LeftForeignKeyJoin"
  StreamGlobalTableJoin _ _        -> "StreamGlobalTableJoin"
  StreamGlobalTableLeftJoin _ _    -> "StreamGlobalTableLeftJoin"
  FilterTable _ _                  -> "FilterTable"
  FilterNotTable _ _               -> "FilterNotTable"
  MapValuesTable _ _               -> "MapValuesTable"
  TransformValuesTable nm _ _ _    -> "TransformValuesTable(" <> nm <> ")"
  SuppressUntilTimeLimit _         -> "SuppressUntilTimeLimit"
  SuppressWindowedKS _ _           -> "SuppressWindowed"
  ProcessStream nm _ _             -> "ProcessStream(" <> nm <> ")"
  ProcessValuesStream nm _ _ _     -> "ProcessValuesStream(" <> nm <> ")"
  TransformValuesStreamT nm _ _ _  -> "TransformValuesStream(" <> nm <> ")"
  WithStateStoreKV b _             ->
    "WithStateStoreKV(" <> Store.unStoreName (Store.sbKvName b) <> ")"
  WithStateStoreW b _              ->
    "WithStateStoreW("  <> Store.unStoreName (Store.sbWName  b) <> ")"
  WithStateStoreS b _              ->
    "WithStateStoreS("  <> Store.unStoreName (Store.sbSName  b) <> ")"
  ProcessWithStateStoreKV nm b _   ->
    "ProcessWithStateStoreKV(" <> nm <> ","
      <> Store.unStoreName (Store.sbKvName b) <> ")"
  ProcessWithStateStoreW nm b _    ->
    "ProcessWithStateStoreW(" <> nm <> ","
      <> Store.unStoreName (Store.sbWName b) <> ")"
  ProcessWithStateStoreS nm b _    ->
    "ProcessWithStateStoreS(" <> nm <> ","
      <> Store.unStoreName (Store.sbSName b) <> ")"
  Lifted nm _                      -> "Lifted(" <> nm <> ")"
  where
    showTopic :: TopicName -> Text
    showTopic = T.pack . show

-- | Like 'inspect', but walks /through/ 'Bind' continuations by
-- actually running 'apply' on the left side to get the real wire
-- value, then handing it to the continuation. The continuation
-- sees a genuine 'KStream' / 'KTable' / etc. (with real
-- auto-generated node names, real serdes, real builder
-- handles) — so /any/ continuation is faithfully inspected,
-- including ones that read record fields.
--
-- == How it works
--
-- 'inspectDeep' spins up a throwaway 'StreamsBuilder' and walks
-- the topology in the same order 'apply' does: each leaf
-- registers its node(s) in the builder (with auto-generated
-- names) and produces a real wire value; each 'Bind' uses that
-- wire value to construct its continuation's topology, which
-- is then walked recursively. Along the way, every constructor
-- emits its 'inspect'-style token via a tracer.
--
-- The builder accumulates a fully-compiled 'Topo.Topology' as a
-- side effect — but it's discarded when 'inspectDeep' returns.
-- The only thing we keep is the token list.
--
-- /Only works for closed-input topologies/ ('Topology Void o').
-- Inspecting an open-input fragment past a Bind would need an
-- actual @i@ to feed in, which the caller hasn't supplied.
-- For open fragments, fall back to plain 'inspect' (which is
-- opaque past Bind but works for any input type).
--
-- == Cost
--
-- 'inspectDeep' /does the same work as 'compile'/ — it builds
-- a throwaway compiled topology to extract the wire values
-- past each bind. For trivial topologies this is cheap; for
-- large topologies with many sources / state stores it can
-- take real time. Prefer 'inspect' for the common case where
-- you don't have binds (or don't need to see past them).
--
-- == Tokens
--
-- The format matches 'inspect' for non-'Bind' constructors. A
-- 'Bind' is rendered as
-- @\"Bind<\" : leftTokens ++ \">~>\" : rightTokens ++ [\"\</Bind\>\"]@,
-- making the bind boundary visible in the output.
inspectDeep :: Topology Void o -> [Text]
inspectDeep t = IOUnsafe.unsafePerformIO $ do
  b <- newStreamsBuilder
  FA.inspectFADeep labelPrim (runPrim b) t voidInput
  where
    voidInput :: Void
    voidInput = Exception.throw (VoidInputForced "inspectDeep")

-- | 'apply' with a tracer callback invoked at every leaf and
-- structural-bracket point. Used internally by 'inspectDeep'.
-- The tracer receives token batches in the same order they'd
-- appear in 'inspect''s output.
applyTraced
  :: forall i o
   . ([Text] -> IO ())
  -> TBuilder
  -> Topology i o
  -> i
  -> IO o
applyTraced tracer b = FA.interpretTraced tracer labelPrim (runPrim b)


-- | Render an AST as a single-line whitespace-separated
-- description. Built by 'T.intercalate'-ing the output of
-- 'inspect'.
--
-- Useful for REPL exploration, error messages, and at-a-glance
-- comparison of AST shapes. The output is /not/ a stable
-- format — use it for debugging, not for parsing or contract
-- tests.
--
-- @
-- ghci> F.prettyPrint (F.source \"in\" textSerde textSerde
--                       '>>>' F.mapValues T.toUpper
--                       '>>>' F.sink \"out\" textSerde textSerde)
-- "Source(...) MapValues Sink(...)"
-- @
--
-- For more structured introspection (e.g. counting operator
-- occurrences, walking the AST) use 'inspect' directly — it
-- returns a list of tokens you can pattern-match on.
prettyPrint :: Topology i o -> Text
prettyPrint = T.intercalate " " . inspect

-- | 'prettyPrint' that uses 'inspectDeep' instead of 'inspect',
-- so 'Bind' continuations are walked into by actually running
-- 'apply' to extract real wire values. Only available on
-- closed-input topologies — see 'inspectDeep' for the full
-- contract.
prettyPrintDeep :: Topology Void o -> Text
prettyPrintDeep = T.intercalate " " . inspectDeep

----------------------------------------------------------------------
-- Optimisation
--
-- Walks a 'Topology' and applies a curated set of /semantics-
-- preserving/ rewrites that fuse adjacent operators, collapse
-- identity nodes, and right-associate composition into a canonical
-- form. The result is an AST that compiles to fewer topology graph
-- nodes and therefore fewer record-forwarding hops at run time.
--
-- The rewrite set covers Category\/Arrow laws (Id collapse, Arr
-- fusion, push-pure-through-First\/Second\/Parallel\/Fanout,
-- right-associate Compose), KStream operator fusion
-- (MapValues\/MapValuesM\/MapKeyValue\/ConcatMapValues\/Filter\/
-- FilterNot\/SelectKey\/Peek), and structural identity collapse
-- (Tap Id, First Id, Parallel Id Id, etc).
--
-- What we deliberately /don't/ rewrite: reordering across
-- @Filter '>>>' MapValues@ (the filter observes the pre-map
-- record), global topic-level optimisations (those live in
-- "Kafka.Streams.Topology.Optimization"), and removing observable
-- effects ('Peek', 'Foreach' are preserved verbatim).
----------------------------------------------------------------------

-- | Toggles for the rewrite passes. Each flag controls one /family/
-- of rewrites. Defaults enable everything.
--
-- == Relationship to Java's @topology.optimization@ knob
--
-- Java's Kafka Streams ships two formal optimisations, both at the
-- /topic/ level. As of the KIP-295 implementation in
-- 'Kafka.Streams.Topology.optimizeTopology' (run automatically by
-- 'compile' after the AST-level rewrites), both are now active:
--
--   * @REUSE_KTABLE_SOURCE_TOPICS@ — every 'TableSource' (or any
--     processor that owns a single store and has a single
--     single-topic source parent) has its store's logging config
--     rewritten to reuse the source topic as its changelog
--     instead of creating a separate internal one. See
--     'Kafka.Streams.Topology.optReuseSourceKTable' /
--     'Kafka.Streams.State.Store.loggingSourceTopic'.
--   * @MERGE_REPARTITION_TOPICS@ — sibling
--     @KSTREAM-REPARTITION-\<prefix\>-…@ processors that share
--     the same single parent /and/ the same repartition prefix
--     are merged into one. Two downstream stateful ops that both
--     called 'repartition' on the same upstream therefore share
--     one physical repartition node instead of duplicating the
--     shuffle. See 'Kafka.Streams.Topology.optMergeRepartitionTopics'.
--
-- The chained AST-level rewrites this module ships are /additional/
-- to those: they fuse adjacent operators (saving processor nodes /
-- record-forwarding hops) and collapse Cat\/Arrow identity
-- combinators /inside/ the AST, before the graph-level
-- 'optimizeTopology' sweeps the compiled 'Topo.Topology'.
--
-- The GADT-level rewrites this module ships are /additional/ to
-- those: they fuse adjacent operators (saving processor nodes /
-- record-forwarding hops) and collapse Cat\/Arrow identity
-- combinators. Java's DSL doesn't expose an equivalent because its
-- builder eagerly constructs the physical plan rather than holding
-- a reified AST. Operationally the result is the same shape Java
-- programmers achieve by hand-writing
--
-- @
-- stream.mapValues(f).mapValues(g)   -- two processors
-- @
--
-- as
--
-- @
-- stream.mapValues(g . f)            -- one processor
-- @
--
-- We do that fusion automatically.
data OptimizeConfig = OptimizeConfig
  { -- | Right-associate 'Compose' chains so adjacent operators
    -- surface along the right spine for the fusion pass.
    optAssociateCompose :: !Bool

    -- | Fuse adjacent 'Arr' applications and push pure functions
    -- through 'First', 'Second', 'Parallel', 'Fanout', 'Fork'.
  , optFusePureFunctions :: !Bool

    -- | Fuse adjacent 'MapValues' \/ 'MapValuesM' \/ 'MapKeyValue'.
  , optFuseMaps          :: !Bool

    -- | Fuse adjacent 'Filter' \/ 'FilterNot'.
  , optFuseFilters       :: !Bool

    -- | Fuse 'ConcatMapValues' with adjacent 'MapValues' and itself.
  , optFuseConcatMaps      :: !Bool

    -- | Fuse adjacent 'SelectKey'.
  , optFuseSelectKeys    :: !Bool

    -- | Fuse adjacent 'Peek' callbacks. Also fuses
    -- @'Foreach' '.' 'Peek'@ and collapses
    -- @'Tap' ('Foreach' f) = 'Peek' f@.
  , optFusePeeks         :: !Bool

    -- | Collapse @'Tap' 'Id'@, @'First' 'Id'@, @'Parallel' 'Id'
    -- 'Id'@, etc. to 'Id'.
  , optCollapseIdentity  :: !Bool

    -- | Java-style grouping rewrite:
    -- @'GroupByKey' g '.' 'SelectKey' f = 'GroupBy' f g@.
    -- Matches the Apache docs' guidance to prefer @groupBy@ over
    -- @selectKey + groupByKey@ for fewer nodes and clearer
    -- topology descriptions.
  , optFuseSelectKeyIntoGroupBy :: !Bool

    -- | Eliminate redundant repartitions: a second 'Repartition'
    -- after a first one with no key-changing op in between is
    -- pure waste (two shuffles produce the same data). The
    -- outer wins (its prefix is the one named in the broker
    -- topic).
  , optCollapseRepartition :: !Bool

    -- | Drop a 'Repartition' that's about to be invalidated by a
    -- /key-changing/ op. Pattern: @'SelectKey' f '.' 'Repartition' _ = 'SelectKey' f@
    -- (similarly for 'MapKeyValue' / 'MapKeyValueM' /
    -- 'ConcatMapKeyValue', plus the 'RepartitionWith' variant).
    -- Saves a wasted shuffle when callers accidentally write
    -- @repartition >>> selectKey@ instead of the other way
    -- around. The downstream stateful op will still need its
    -- own repartition (which the caller must insert, or which
    -- the runtime synthesises on the JVM side); this rewrite
    -- just removes the pointless first one.
  , optDropPreKeyChangeRepartition :: !Bool

    -- | Hoist pure stateless ops /upstream/ of an adjacent
    -- 'Repartition' so they run on smaller, pre-shuffle records
    -- — and so they can fuse with any other adjacent pure ops
    -- that are already upstream of the repartition. Concretely:
    --
    --   * @'MapValues' g '.' 'Repartition' p = 'Repartition' p '.' 'MapValues' g@
    --   * @'Filter' q '.' 'Repartition' p    = 'Repartition' p '.' 'Filter' q@
    --   * @'FilterNot' q '.' 'Repartition' p = 'Repartition' p '.' 'FilterNot' q@
    --   * @'ConcatMapValues' g '.' 'Repartition' p = 'Repartition' p '.' 'ConcatMapValues' g@
    --   * and the 'RepartitionWith' analogues.
    --
    -- Only pure key-preserving ops are hoisted — IO variants
    -- ('MapValuesM') and effect-observers ('Peek') stay put
    -- because their observable semantics depend on which
    -- partition the record is on (pre- vs. post-shuffle).
    -- Key-changing ops are explicitly /not/ hoisted (they'd
    -- invalidate the repartition).
  , optHoistThroughRepartition :: !Bool

    -- | Mirror Java Kafka Streams' implicit
    -- /auto-insert-repartition-on-stateful-op/ behaviour. When
    -- enabled, the optimiser walks the AST in flow order tracking
    -- a /key-dirty/ flag (set by 'SelectKey' / 'MapKeyValue' /
    -- 'MapKeyValueM' / 'ConcatMapKeyValue'; cleared by 'Repartition'
    -- / 'RepartitionWith') and inserts a 'Repartition' on the
    -- relevant 'KStream' edge whenever a stateful op would
    -- otherwise read mis-partitioned records:
    --
    --   * Linear flow: a 'Repartition' is inserted just before
    --     'GroupByKey', 'GroupBy', and 'ToTableT' when the
    --     upstream key is dirty.
    --   * Joins: for stream-table and stream-stream joins whose
    --     immediate upstream is a visible 'Fanout' (the
    --     '&&&'-shaped tuple-forming idiom), each half is
    --     analysed and wrapped with a 'Repartition' on the side
    --     that needs it. Stream-global-table joins are skipped
    --     (the global table is replicated, no co-partitioning
    --     required). Joins reached only through monadic bind or
    --     other opaque shapes are /not/ auto-inserted — callers
    --     using do-notation should insert 'repartition'
    --     explicitly.
    --
    -- The inserted prefix is @\"auto-rep\"@; downstream graph-level
    -- merging ('Kafka.Streams.Topology.optMergeRepartitionTopics')
    -- collapses sibling inserts so two stateful ops downstream
    -- of the same key-change share one physical repartition.
  , optAutoInsertRepartition :: !Bool

    -- | @'Values' '.' 'Values' = 'Values'@ — idempotent key
    -- drop.
  , optCollapseValues      :: !Bool

    -- | Combine adjacent 'Tap' nodes into one by 'Fanout'-ing the
    -- side pipelines:
    -- @'Tap' t1 '.' 'Tap' t2 = 'Tap' ('Fanout' t2 t1 '>>>' Arr (const ()))@.
  , optFuseTaps            :: !Bool

    -- | Riffle rule: lift an adjacent pure 'MapValues' /
    -- 'MapKeyValue' into an immediately-following
    -- 'AsyncMapValues' / 'AsyncMapKeyValue' so the pure part
    -- runs on the async worker instead of the stream thread.
    --
    -- The fusion is /one-sided/: a pure op /before/ an async op
    -- is hoisted into the async worker (correct because the
    -- worker still observes records in the same logical order
    -- the stream thread enqueued them). A pure op /after/ an
    -- async op is /not/ fused — the post-async forward happens
    -- on the stream thread, so leaving the pure op as a
    -- distinct sync node is fine and keeps the AsyncIO
    -- bookkeeping focused on the actually-async work.
    --
    -- Adjacent async-async fusion is /not/ performed: two
    -- async ops carry independent 'AsyncIOConfig' instances
    -- (different buffer budgets, retry policies, ordering
    -- modes). Collapsing them would silently change the
    -- backpressure profile.
  , optFuseSyncIntoAsync   :: !Bool

    -- | Upper bound on rewrite passes. Each iteration must reduce
    -- node count or termination is forced; this is a belt-and-
    -- braces guard against pathological inputs.
  , optMaxPasses         :: !Int
  }
  deriving stock (Show)

defaultOptimizeConfig :: OptimizeConfig
defaultOptimizeConfig = OptimizeConfig
  { optAssociateCompose             = True
  , optFusePureFunctions            = True
  , optFuseMaps                     = True
  , optFuseFilters                  = True
  , optFuseConcatMaps                 = True
  , optFuseSelectKeys               = True
  , optFusePeeks                    = True
  , optCollapseIdentity             = True
  , optFuseSelectKeyIntoGroupBy     = True
  , optCollapseRepartition          = True
  , optDropPreKeyChangeRepartition  = True
  , optHoistThroughRepartition      = True
  , optAutoInsertRepartition        = True
  , optCollapseValues               = True
  , optFuseTaps                     = True
  , optFuseSyncIntoAsync            = True
  , optMaxPasses                    = 12
  }

-- | Everything off — the AST passes through unchanged. Useful for
-- A\/B comparisons in tests.
noOptimization :: OptimizeConfig
noOptimization = OptimizeConfig
  { optAssociateCompose             = False
  , optFusePureFunctions            = False
  , optFuseMaps                     = False
  , optFuseFilters                  = False
  , optFuseConcatMaps                 = False
  , optFuseSelectKeys               = False
  , optFusePeeks                    = False
  , optCollapseIdentity             = False
  , optFuseSelectKeyIntoGroupBy     = False
  , optCollapseRepartition          = False
  , optDropPreKeyChangeRepartition  = False
  , optHoistThroughRepartition      = False
  , optAutoInsertRepartition        = False
  , optCollapseValues               = False
  , optFuseTaps                     = False
  , optFuseSyncIntoAsync            = False
  , optMaxPasses                    = 0
  }

-- | Apply the default rewrite passes ('defaultOptimizeConfig') to
-- a 'Topology' AST, returning a semantically-equivalent but
-- structurally-minimal version.
--
-- The default config enables every rewrite family in the
-- module (operator fusion, identity collapse, right-associate
-- Compose, push pure functions through Fanout/Parallel/Fork,
-- Java-aligned rewrites like @selectKey >>> groupByKey =>
-- groupBy@). Run-time semantics are preserved by construction;
-- only the node count and AST shape change.
--
-- This is the function 'compile' applies automatically before
-- walking the AST — calling 'optimize' yourself is useful when:
--
--   * comparing the pre- and post-optimisation shape via
--     'inspect' / 'countNodes' / 'optimizationStats',
--   * running the optimiser separately from compilation,
--   * caching an optimised AST for repeated compilation.
optimize :: Topology i o -> Topology i o
optimize = optimizeWith defaultOptimizeConfig

-- | 'optimize' with a custom 'OptimizeConfig'. Use to enable a
-- subset of the rewrite families (e.g. only collapse identity
-- without fusing maps), to disable all rewrites
-- ('noOptimization'), or to bump 'optMaxPasses' for
-- pathological inputs.
--
-- == Fixed-point semantics
--
-- The optimiser runs rewrite passes in a loop, each pass a
-- bottom-up traversal that applies all enabled rewrite
-- families. The loop terminates when:
--
--   * a pass produces strictly fewer nodes than the previous
--     (continue), /or/
--   * a pass produces the same (or more) nodes (stop — we've
--     reached a fixed point), /or/
--   * 'optMaxPasses' has been exhausted (stop — belt-and-
--     braces against pathological loops).
--
-- All rewrites are strictly node-count-reducing or
-- node-count-preserving, so the fixed point exists and is
-- reached in linearly-many passes.
optimizeWith :: OptimizeConfig -> Topology i o -> Topology i o
optimizeWith cfg = loop (optMaxPasses cfg)
  where
    loop :: Int -> Topology i o -> Topology i o
    loop 0 t = t
    loop n t =
      let !t' = optimizePass cfg t
          !before = countNodes t
          !after  = countNodes t'
       in if after < before then loop (n - 1) t' else t'

-- | One bottom-up rewrite pass.
--
-- The framework cases ('Compose', 'First', 'Second', etc.) are
-- handled in the same shape they were before the migration; the
-- primitive matches in 'fuseStep' / 'collapseTap' now reach
-- through the 'Lift' wrapper that 'FreeArrow' uses to embed
-- 'Prim' nodes.
--
-- Before the bottom-up fusion sweep runs, an /auto-insert/
-- pre-pass walks the AST in flow order and inserts
-- 'Repartition' primitives where Java's DSL would (KIP-295
-- auto-repartition shape). The two passes are designed to
-- cooperate: any auto-inserted Repartition that immediately
-- abuts a key change is dropped by
-- 'optDropPreKeyChangeRepartition'; any auto-inserted
-- Repartition that ends up adjacent to a stateless pure op is
-- hoisted past it by 'optHoistThroughRepartition'.
optimizePass :: OptimizeConfig -> Topology i o -> Topology i o
optimizePass cfg = go . autoInsertRepartitionPass cfg
  where
    go :: forall a b. Topology a b -> Topology a b
    go (Compose g f) = smartCompose cfg (go g) (go f)
    go (First t)     = collapseFirst cfg (go t)
    go (Second t)    = collapseSecond cfg (go t)
    go (Parallel p q)= collapseParallel cfg (go p) (go q)
    go (Fanout p q)  = collapseFanout cfg (go p) (go q)
    go (LeftT t)     = collapseLeft cfg (go t)
    go (RightT t)    = collapseRight cfg (go t)
    go (Plus p q)    = collapsePlus cfg (go p) (go q)
    go (Fanin p q)   = Fanin (go p) (go q)
    go (ForkN ts)    = ForkN (NE.map go ts)
    go (Tap t)       = collapseTap cfg (go t)
    go (Bind t k)    = Bind (go t) k
    go x             = x

-- | The auto-insert-repartition pre-pass.
--
-- Walks the AST in flow order, maintaining a /key-dirty/ flag
-- that gets set whenever a key-changing primitive runs and
-- cleared by a 'Repartition' / 'RepartitionWith'. When the walk
-- meets a stateful op whose correctness depends on partitioning
-- (a 'GroupByKey', 'GroupBy', 'ToTableT', or a join with a
-- visible 'Fanout' upstream), it inserts a 'Repartition' on
-- the relevant 'KStream' edge.
--
-- Off (a no-op) when 'optAutoInsertRepartition' is 'False'.
--
-- Tuple-input join coverage is limited to the cases where the
-- tuple is formed by a visible 'Fanout' immediately upstream of
-- the join Prim. Joins reached through 'Bind' (do-notation) are
-- opaque to this pass — callers using do-notation are expected
-- to call 'F.repartition' explicitly. Stream-global-table joins
-- are deliberately skipped (global tables are replicated, no
-- co-partitioning needed).
autoInsertRepartitionPass
  :: forall i o. OptimizeConfig -> Topology i o -> Topology i o
autoInsertRepartitionPass cfg t0
  | optAutoInsertRepartition cfg = snd (walk False t0)
  | otherwise                    = t0
  where
    walk :: forall a b. Bool -> Topology a b -> (Bool, Topology a b)
    -- Local right-associate: if the LEFT of an outer Compose is
    -- itself a Compose, rotate so the inner Compose surfaces on
    -- the right where our join patterns can see it. This is the
    -- 'Compose-associativity' axiom @(h . i) . f = h . (i . f)@
    -- applied locally so the walk sees joins regardless of how
    -- the user parenthesised the chain.
    walk d (Compose (Compose h i) f) = walk d (Compose h (Compose i f))
    walk d (Compose (Lift prim) inner) =
      case prim of
        StreamTableJoin _ _ ->
          let (_, inner') = walk d inner
           in (False, Compose (Lift prim) (insertOnLeftST inner'))
        StreamTableLeftJoin _ _ ->
          let (_, inner') = walk d inner
           in (False, Compose (Lift prim) (insertOnLeftST inner'))
        StreamStreamJoin _ _ _ ->
          let (_, inner') = walk d inner
           in (False, Compose (Lift prim) (insertOnBothSS inner'))
        StreamStreamLeftJoin _ _ _ ->
          let (_, inner') = walk d inner
           in (False, Compose (Lift prim) (insertOnBothSS inner'))
        StreamStreamOuterJoin _ _ _ ->
          let (_, inner') = walk d inner
           in (False, Compose (Lift prim) (insertOnBothSS inner'))
        _ ->
          let (d',  f') = walk d  inner
              (d'', g') = walk d' (Lift prim)
           in (d'', composeR g' f')
    -- Other Compose nodes: walk both halves with the linear flag.
    walk d (Compose g f) =
      let (d',  f') = walk d  f
          (d'', g') = walk d' g
       in (d'', Compose g' f')
    walk d (Lift p)      = walkPrim d p
    walk d (First t)     = let (_, t') = walk d t in (d, First t')
    walk d (Second t)    = let (_, t') = walk d t in (d, Second t')
    walk d (Parallel p q) =
      let (_, p') = walk d p
          (_, q') = walk d q
       in (d, Parallel p' q')
    walk d (Fanout p q)  =
      let (_, p') = walk d p
          (_, q') = walk d q
       in (d, Fanout p' q')
    walk d (LeftT t)     = let (_, t') = walk d t in (d, LeftT t')
    walk d (RightT t)    = let (_, t') = walk d t in (d, RightT t')
    walk d (Plus p q)    =
      let (_, p') = walk d p
          (_, q') = walk d q
       in (d, Plus p' q')
    walk d (Fanin p q)   =
      let (_, p') = walk d p
          (_, q') = walk d q
       in (d, Fanin p' q')
    walk d (Tap t)       = let (_, t') = walk d t in (d, Tap t')
    walk d (ForkN ts)    = (d, ForkN (NE.map (snd . walk d) ts))
    walk d (Bind t k)    = let (d', t') = walk d t in (d', Bind t' k)
    walk d t             = (d, t)

    -- Right-associating compose builder: if g is itself a
    -- Compose @(h . i)@, returns @h . (i . f)@ so the result
    -- AST stays in a canonical right-associated form. Necessary
    -- because 'walkPrim' for stateful linear ops returns a
    -- two-node @Compose Lift(prim) Lift(Repartition)@; without
    -- this helper the outer Compose would left-nest and hide
    -- the inserted Repartition from later fusion rules.
    composeR :: forall x y z. Topology y z -> Topology x y -> Topology x z
    composeR (Compose h i) f = Compose h (composeR i f)
    composeR g             f = Compose g f


    walkPrim :: forall a b. Bool -> Prim a b -> (Bool, Topology a b)
    -- Key changers — set dirty.
    walkPrim _ p@(SelectKey _)        = (True, liftPrim p)
    walkPrim _ p@(MapKeyValue _)      = (True, liftPrim p)
    walkPrim _ p@(MapKeyValueM _)     = (True, liftPrim p)
    walkPrim _ p@(ConcatMapKeyValue _)  = (True, liftPrim p)
    -- Repartitions clear dirty.
    walkPrim _ p@(Repartition _)      = (False, liftPrim p)
    walkPrim _ p@(RepartitionWith _)  = (False, liftPrim p)
    -- Stateful linear ops: when dirty, insert a Repartition just
    -- before. Stays dirty=False afterwards (the inserted
    -- Repartition restored partitioning by the current key).
    walkPrim True p@(GroupByKey _) =
      (False, Compose (liftPrim p) (liftPrim (Repartition "auto-rep")))
    walkPrim True p@(GroupBy _ _) =
      (False, Compose (liftPrim p) (liftPrim (Repartition "auto-rep")))
    walkPrim True p@(ToTableT _) =
      (False, Compose (liftPrim p) (liftPrim (Repartition "auto-rep")))
    walkPrim d p = (d, liftPrim p)

    -- Insertion helpers operating on the immediate-Fanout tuple
    -- pattern. The Fanout-visible idiom is the static
    -- @'tableSource' '&&&' 'source'@ shape — when the tuple is
    -- formed elsewhere (typically by 'Bind' in do-notation), we
    -- can't see through and the helper is conservatively a
    -- no-op.

    insertOnLeftST
      :: forall a k vL vT
       . Topology a (KStream k vL, KTable k vT)
      -> Topology a (KStream k vL, KTable k vT)
    insertOnLeftST (Fanout l r)
      | endsKeyDirty l =
          Fanout (Compose (liftPrim (Repartition "auto-rep")) l) r
    insertOnLeftST t = t

    insertOnBothSS
      :: forall a k v1 v2
       . Topology a (KStream k v1, KStream k v2)
      -> Topology a (KStream k v1, KStream k v2)
    insertOnBothSS (Fanout l r)
      | dl || dr =
          Fanout
            (if dl then Compose (liftPrim (Repartition "auto-rep")) l else l)
            (if dr then Compose (liftPrim (Repartition "auto-rep")) r else r)
      where
        dl = endsKeyDirty l
        dr = endsKeyDirty r
    insertOnBothSS t = t

-- | Conservative \"this topology re-keyed without subsequently
-- repartitioning\" test. Walks the AST in flow order; returns
-- 'True' iff a key-changing primitive appears with no subsequent
-- 'Repartition' on the same linear path. Used by the auto-insert
-- pass to decide whether a join's tuple-side needs wrapping.
endsKeyDirty :: forall a b. Topology a b -> Bool
endsKeyDirty = go False
  where
    go :: forall x y. Bool -> Topology x y -> Bool
    go d (Compose g f)              = go (go d f) g
    go _ (Lift (SelectKey _))       = True
    go _ (Lift (MapKeyValue _))     = True
    go _ (Lift (MapKeyValueM _))    = True
    go _ (Lift (ConcatMapKeyValue _)) = True
    go _ (Lift (Repartition _))     = False
    go _ (Lift (RepartitionWith _)) = False
    go d (Lift _)                   = d
    go d (First _)                  = d
    go d (Second _)                 = d
    go d (Parallel _ _)             = d
    go d (Fanout _ _)               = d
    go d (LeftT _)                  = d
    go d (RightT _)                 = d
    go d (Plus _ _)                 = d
    go d (Fanin _ _)                = d
    go d (Tap _)                    = d
    go d (ForkN _)                  = d
    go d (Bind _ _)                 = d
    go d _                          = d

----------------------------------------------------------------------
-- Identity collapses on structural combinators
----------------------------------------------------------------------

collapseFirst :: OptimizeConfig -> Topology a b -> Topology (a, c) (b, c)
collapseFirst cfg Id      | optCollapseIdentity  cfg = Id
collapseFirst cfg (Arr f) | optFusePureFunctions cfg =
  Arr (\(a, c) -> (f a, c))
collapseFirst _ t = First t

collapseSecond :: OptimizeConfig -> Topology a b -> Topology (c, a) (c, b)
collapseSecond cfg Id      | optCollapseIdentity  cfg = Id
collapseSecond cfg (Arr f) | optFusePureFunctions cfg =
  Arr (\(c, a) -> (c, f a))
collapseSecond _ t = Second t

collapseParallel
  :: OptimizeConfig
  -> Topology a b -> Topology c d -> Topology (a, c) (b, d)
collapseParallel cfg Id      Id | optCollapseIdentity  cfg = Id
collapseParallel cfg (Arr f) (Arr g) | optFusePureFunctions cfg =
  Arr (\(a, c) -> (f a, g c))
collapseParallel _ p q = Parallel p q

collapseFanout
  :: OptimizeConfig
  -> Topology a b -> Topology a c -> Topology a (b, c)
collapseFanout cfg (Arr f) (Arr g) | optFusePureFunctions cfg =
  Arr (\a -> (f a, g a))
collapseFanout _ p q = Fanout p q

collapseLeft
  :: OptimizeConfig
  -> Topology a b -> Topology (Either a c) (Either b c)
collapseLeft cfg Id | optCollapseIdentity cfg = Id
collapseLeft _ t = LeftT t

collapseRight
  :: OptimizeConfig
  -> Topology a b -> Topology (Either c a) (Either c b)
collapseRight cfg Id | optCollapseIdentity cfg = Id
collapseRight _ t = RightT t

collapsePlus
  :: OptimizeConfig
  -> Topology a b -> Topology c d -> Topology (Either a c) (Either b d)
collapsePlus cfg Id Id | optCollapseIdentity cfg = Id
collapsePlus _ p q = Plus p q

collapseTap :: OptimizeConfig -> Topology a () -> Topology a a
collapseTap cfg Id                       | optCollapseIdentity cfg = Id
-- 'Tap (Foreach f)' is exactly 'Peek f' — both run an effect on
-- every record and pass the wire through. We recognise this
-- standalone form here in addition to the @Compose@ boundary
-- form in 'fuseStep' so single-node @Tap (Foreach f)@ also
-- collapses.
collapseTap cfg (Lift (Foreach act))     | optFusePeeks cfg = liftPrim (Peek act)
collapseTap _ t = Tap t

----------------------------------------------------------------------
-- Smart 'Compose': right-associate + fuse along the right spine
----------------------------------------------------------------------

smartCompose
  :: forall a b c. OptimizeConfig -> Topology b c -> Topology a b -> Topology a c
smartCompose cfg g f =
  case fuseStep cfg g f of
    Just gf -> gf
    Nothing ->
      case f of
        -- If 'f' is itself a Compose, attempt to fuse 'g' with its
        -- head 'h' along the right spine.
        Compose h i ->
          case fuseStep cfg g h of
            Just gh -> smartCompose cfg gh i
            Nothing -> Compose g (Compose h i)
        _ -> case g of
          Compose h i | optAssociateCompose cfg ->
            -- Right-associate: (h . i) . f -> h . (i . f)
            smartCompose cfg h (smartCompose cfg i f)
          _ -> Compose g f

----------------------------------------------------------------------
-- The fuse rule table
----------------------------------------------------------------------

-- | Return the fused operator if @g '.' f@ matches one of the
-- recognised patterns. The result must be /semantically equivalent/
-- to running @f@ then @g@ in sequence; the only thing it changes is
-- the number of topology graph nodes.
fuseStep
  :: forall a b c
   . OptimizeConfig
  -> Topology b c
  -> Topology a b
  -> Maybe (Topology a c)
fuseStep cfg g f = case (g, f) of
  -- Fusion barrier — explicitly do not fuse across 'NoFuse',
  -- regardless of what the surrounding primitives are. This
  -- short-circuits the table so /no/ rule below can match a
  -- pair where either side is a barrier.
  (Lift NoFuse, _) -> Nothing
  (_, Lift NoFuse) -> Nothing

  -- Identity collapses
  (Id, _) | optCollapseIdentity cfg -> Just f
  (_, Id) | optCollapseIdentity cfg -> Just g

  -- Pure-function fusion
  (Arr g', Arr f') | optFusePureFunctions cfg -> Just (Arr (g' . f'))

  -- MapValues family
  (Lift (MapValues g'), Lift (MapValues f')) | optFuseMaps cfg ->
    Just (liftPrim (MapValues (g' . f')))
  (Lift (MapValuesM g'), Lift (MapValues f')) | optFuseMaps cfg ->
    Just (liftPrim (MapValuesM (g' . f')))
  (Lift (MapValues g'), Lift (MapValuesM f')) | optFuseMaps cfg ->
    Just (liftPrim (MapValuesM (\v -> g' <$> f' v)))
  (Lift (MapValuesM g'), Lift (MapValuesM f')) | optFuseMaps cfg ->
    Just (liftPrim (MapValuesM (\v -> f' v >>= g')))

  -- MapKeyValue family
  (Lift (MapKeyValue g'), Lift (MapKeyValue f')) | optFuseMaps cfg ->
    Just (liftPrim (MapKeyValue (\k v ->
                                   let (!k', !v') = f' k v
                                    in g' k' v')))
  (Lift (MapKeyValueM g'), Lift (MapKeyValue f')) | optFuseMaps cfg ->
    Just (liftPrim (MapKeyValueM (\k v ->
                                     let (!k', !v') = f' k v
                                      in g' k' v')))
  (Lift (MapKeyValue g'), Lift (MapKeyValueM f')) | optFuseMaps cfg ->
    Just (liftPrim (MapKeyValueM (\k v -> do
                                     (!k', !v') <- f' k v
                                     pure (g' k' v'))))
  (Lift (MapKeyValueM g'), Lift (MapKeyValueM f')) | optFuseMaps cfg ->
    Just (liftPrim (MapKeyValueM (\k v -> do
                                      (!k', !v') <- f' k v
                                      g' k' v')))

  -- Riffle: hoist a pure pre-async op into the async worker.
  --
  -- The downstream is async; the upstream is a pure
  -- 'MapValues' / 'MapKeyValue'. The worker pool will compute
  -- the composed function instead of the stream thread doing
  -- the pure half; the bookkeeping (ordering, retry, drain)
  -- is unchanged because we keep the async constructor and
  -- its 'AsyncIOConfig'.
  (Lift (AsyncMapValues aioCfg g'), Lift (MapValues f')) | optFuseSyncIntoAsync cfg ->
    Just (liftPrim (AsyncMapValues aioCfg (\v -> g' (f' v))))
  (Lift (AsyncMapKeyValue aioCfg g'), Lift (MapKeyValue f')) | optFuseSyncIntoAsync cfg ->
    Just (liftPrim (AsyncMapKeyValue aioCfg (\k v ->
      let (!k', !v') = f' k v
      in g' k' v')))
  (Lift (AsyncConcatMapValues aioCfg g'), Lift (MapValues f')) | optFuseSyncIntoAsync cfg ->
    Just (liftPrim (AsyncConcatMapValues aioCfg (\v -> g' (f' v))))

  -- MapRecord family — full-record transforms compose just like
  -- their value-only counterparts.
  (Lift (MapRecord g'), Lift (MapRecord f')) | optFuseMaps cfg ->
    Just (liftPrim (MapRecord (g' . f')))
  (Lift (MapRecordM g'), Lift (MapRecord f')) | optFuseMaps cfg ->
    Just (liftPrim (MapRecordM (g' . f')))
  (Lift (MapRecord g'), Lift (MapRecordM f')) | optFuseMaps cfg ->
    Just (liftPrim (MapRecordM (\r -> g' <$> f' r)))
  (Lift (MapRecordM g'), Lift (MapRecordM f')) | optFuseMaps cfg ->
    Just (liftPrim (MapRecordM (\r -> f' r >>= g')))

  -- FlatMap fusion
  (Lift (ConcatMapValues g'), Lift (MapValues f')) | optFuseConcatMaps cfg ->
    Just (liftPrim (ConcatMapValues (g' . f')))
  (Lift (MapValues g'), Lift (ConcatMapValues f')) | optFuseConcatMaps cfg ->
    Just (liftPrim (ConcatMapValues (fmap g' . f')))
  (Lift (ConcatMapValues g'), Lift (ConcatMapValues f')) | optFuseConcatMaps cfg ->
    Just (liftPrim (ConcatMapValues (\v -> concatMap g' (f' v))))

  -- Filter / FilterNot fusion. 'f' runs first, so the conjunction's
  -- left-hand side is the inner predicate.
  (Lift (Filter p2), Lift (Filter p1)) | optFuseFilters cfg ->
    Just (liftPrim (Filter (\r -> p1 r && p2 r)))
  (Lift (FilterNot p2), Lift (FilterNot p1)) | optFuseFilters cfg ->
    Just (liftPrim (FilterNot (\r -> p1 r || p2 r)))
  (Lift (Filter p), Lift (FilterNot q)) | optFuseFilters cfg ->
    Just (liftPrim (Filter (\r -> not (q r) && p r)))
  (Lift (FilterNot q), Lift (Filter p)) | optFuseFilters cfg ->
    Just (liftPrim (Filter (\r -> p r && not (q r))))

  -- SelectKey fusion. The outer 'SelectKey' sees the re-keyed
  -- record; we synthesise the intermediate 'Record k' v' for it.
  (Lift (SelectKey g'), Lift (SelectKey f')) | optFuseSelectKeys cfg ->
    Just (liftPrim (SelectKey (\r ->
            let !k' = f' r
                !r' = r { recordKey = Just k' }
             in g' r')))

  -- Peek fusion. 'inner' runs first, then 'outer' — both observe
  -- the same record, no order change.
  (Lift (Peek g'), Lift (Peek f')) | optFusePeeks cfg ->
    Just (liftPrim (Peek (\r -> f' r >> g' r)))

  -- Foreach after Peek: the peek's side-effect ran, then the
  -- foreach's side effect runs on the same record and drops the
  -- wire. Fuse into a single 'Foreach' that runs both in order.
  (Lift (Foreach g'), Lift (Peek f')) | optFusePeeks cfg ->
    Just (liftPrim (Foreach (\r -> f' r >> g' r)))

  -- 'GroupBy' subsumes 'SelectKey >>> GroupByKey': re-key + group
  -- in a single processor. Mirrors Java's guidance to prefer
  -- @groupBy@ over @selectKey + groupByKey@. The 'Grouped' carries
  -- the new key serde so it stays on 'GroupBy'.
  (Lift (GroupByKey g'), Lift (SelectKey f')) | optFuseSelectKeyIntoGroupBy cfg ->
    Just (liftPrim (GroupBy f' g'))
  -- Same idea, but looking through an auto-inserted Repartition
  -- between the 'SelectKey' and the 'GroupByKey'. 'GroupBy'
  -- semantically implies a repartition (the runtime emits one
  -- for it), so we can drop the explicit one here. Two sub-cases
  -- cover whether the @SelectKey@ has further upstream
  -- (@SelectKey . rest@) or is at the chain start.
  (Lift (GroupByKey g'),
    Compose (Lift (Repartition _)) (Lift (SelectKey f')))
    | optFuseSelectKeyIntoGroupBy cfg ->
      Just (liftPrim (GroupBy f' g'))
  (Lift (GroupByKey g'),
    Compose (Lift (RepartitionWith _)) (Lift (SelectKey f')))
    | optFuseSelectKeyIntoGroupBy cfg ->
      Just (liftPrim (GroupBy f' g'))
  (Lift (GroupByKey g'),
    Compose (Lift (Repartition _))
            (Compose (Lift (SelectKey f')) rest))
    | optFuseSelectKeyIntoGroupBy cfg ->
      Just (Compose (liftPrim (GroupBy f' g')) rest)
  (Lift (GroupByKey g'),
    Compose (Lift (RepartitionWith _))
            (Compose (Lift (SelectKey f')) rest))
    | optFuseSelectKeyIntoGroupBy cfg ->
      Just (Compose (liftPrim (GroupBy f' g')) rest)

  -- Repartition idempotence
  (Lift (Repartition pfx2), Lift (Repartition _)) | optCollapseRepartition cfg ->
    Just (liftPrim (Repartition pfx2))
  (Lift (RepartitionWith cfg2), Lift (Repartition _)) | optCollapseRepartition cfg ->
    Just (liftPrim (RepartitionWith cfg2))
  (Lift (Repartition pfx), Lift (RepartitionWith _)) | optCollapseRepartition cfg ->
    Just (liftPrim (Repartition pfx))
  (Lift (RepartitionWith cfg2), Lift (RepartitionWith _)) | optCollapseRepartition cfg ->
    Just (liftPrim (RepartitionWith cfg2))

  -- Drop a Repartition immediately followed by a key-changing
  -- op — the about-to-change key invalidates the shuffle the
  -- repartition just performed.
  (Lift (SelectKey f'), Lift (Repartition _))
    | optDropPreKeyChangeRepartition cfg ->
        Just (liftPrim (SelectKey f'))
  (Lift (SelectKey f'), Lift (RepartitionWith _))
    | optDropPreKeyChangeRepartition cfg ->
        Just (liftPrim (SelectKey f'))
  (Lift (MapKeyValue f'), Lift (Repartition _))
    | optDropPreKeyChangeRepartition cfg ->
        Just (liftPrim (MapKeyValue f'))
  (Lift (MapKeyValue f'), Lift (RepartitionWith _))
    | optDropPreKeyChangeRepartition cfg ->
        Just (liftPrim (MapKeyValue f'))
  (Lift (MapKeyValueM f'), Lift (Repartition _))
    | optDropPreKeyChangeRepartition cfg ->
        Just (liftPrim (MapKeyValueM f'))
  (Lift (MapKeyValueM f'), Lift (RepartitionWith _))
    | optDropPreKeyChangeRepartition cfg ->
        Just (liftPrim (MapKeyValueM f'))
  (Lift (ConcatMapKeyValue f'), Lift (Repartition _))
    | optDropPreKeyChangeRepartition cfg ->
        Just (liftPrim (ConcatMapKeyValue f'))
  (Lift (ConcatMapKeyValue f'), Lift (RepartitionWith _))
    | optDropPreKeyChangeRepartition cfg ->
        Just (liftPrim (ConcatMapKeyValue f'))

  -- Hoist pure key-preserving stateless ops /upstream/ of an
  -- adjacent Repartition. Result: the op runs before the shuffle
  -- (smaller records, more fusion opportunity with upstream
  -- siblings) and the Repartition stays as the last step before
  -- the next stateful operation.
  --
  -- We rewrite @op '.' repartition@ to @repartition '.' op@
  -- — Compose's @g . f@ order is "f then g", so swapping the
  -- argument order on the right-hand side puts the op upstream
  -- and the repartition downstream.
  -- Type-changing value transforms only hoist through the
  -- type-polymorphic 'Repartition'. They cannot hoist through
  -- 'RepartitionWith' because its 'Rep.Repartitioned' config
  -- is /value-type-specialised/ — the upstream value type
  -- doesn't match the downstream config after the swap.
  (Lift (MapValues g'), Lift (Repartition pfx))
    | optHoistThroughRepartition cfg ->
        Just (Compose (liftPrim (Repartition pfx)) (liftPrim (MapValues g')))
  (Lift (ConcatMapValues g'), Lift (Repartition pfx))
    | optHoistThroughRepartition cfg ->
        Just (Compose (liftPrim (Repartition pfx)) (liftPrim (ConcatMapValues g')))

  -- Value-type-preserving predicates hoist through both
  -- 'Repartition' /and/ 'RepartitionWith' — no type juggling
  -- is needed since the value type doesn't change on either
  -- side of the swap.
  (Lift (Filter p'), Lift (Repartition pfx))
    | optHoistThroughRepartition cfg ->
        Just (Compose (liftPrim (Repartition pfx)) (liftPrim (Filter p')))
  (Lift (Filter p'), Lift (RepartitionWith cfg'))
    | optHoistThroughRepartition cfg ->
        Just (Compose (liftPrim (RepartitionWith cfg')) (liftPrim (Filter p')))
  (Lift (FilterNot p'), Lift (Repartition pfx))
    | optHoistThroughRepartition cfg ->
        Just (Compose (liftPrim (Repartition pfx)) (liftPrim (FilterNot p')))
  (Lift (FilterNot p'), Lift (RepartitionWith cfg'))
    | optHoistThroughRepartition cfg ->
        Just (Compose (liftPrim (RepartitionWith cfg')) (liftPrim (FilterNot p')))

  -- 'Values' is idempotent on a key-less stream.
  (Lift Values, Lift Values) | optCollapseValues cfg -> Just (liftPrim Values)

  -- Adjacent 'Tap's combine into one by Fanout-ing the side
  -- pipelines.
  (Tap t1, Tap t2) | optFuseTaps cfg ->
    Just (Tap (Compose (Arr (const ())) (Fanout t2 t1)))

  -- 'Tap (Foreach f)' is exactly 'Peek f'.
  (_, _) | Just t <- tapForeachToPeek cfg g f -> Just t

  -- Push pure functions through 'Fork'.
  (Arr g', Fork) | optFusePureFunctions cfg ->
    Just (Arr (\a -> g' (a, a)))

  _ -> Nothing

-- | Detect the pattern @'Tap' ('Foreach' f)@ which is equivalent
-- to @'Peek' f@. The pattern is on the /inner/ position; the
-- outer is some unrelated combinator that the caller wants to
-- chain afterwards (it's just passed through).
--
-- We match on the @f@ argument of 'fuseStep' (i.e. the inner
-- topology) and use a tiny helper rather than a full extra
-- 'fuseStep' clause because the type juggling for the case is
-- a bit verbose.
tapForeachToPeek
  :: forall a b c
   . OptimizeConfig
  -> Topology b c
  -> Topology a b
  -> Maybe (Topology a c)
tapForeachToPeek cfg g f
  | optFusePeeks cfg
  , Tap inner <- f
  , Lift (Foreach act) <- inner
  -- Re-fuse 'g' on top of the new 'Peek' so this rewrite plays
  -- well with the surrounding pass.
  = case fuseStep cfg g (liftPrim (Peek act)) of
      Just t  -> Just t
      Nothing -> Just (Compose g (liftPrim (Peek act)))
  | otherwise = Nothing

----------------------------------------------------------------------
-- Node counting and optimization statistics
----------------------------------------------------------------------

-- | Count the constructors in a 'Topology' AST.
--
-- Every leaf (sources / sinks / transforms / aggregators /
-- 'Arr' / 'Id' / 'Lifted' / etc.) counts as 1; every structural
-- combinator ('Compose', 'First', 'Second', 'Parallel',
-- 'Fanout', 'LeftT', 'RightT', 'Plus', 'Fanin', 'Tap',
-- 'ForkN', 'Bind') also counts 1 and adds the counts of its
-- children.
--
-- This is the basis of the optimiser's fixed-point criterion
-- ('optimizeWith' loops until 'countNodes' stops decreasing)
-- and of 'optimizationStats' for visibility into how much
-- the rewriter shrank a topology.
--
-- == What it doesn't count
--
--   * The compiled Kafka 'Topo.Topology' graph nodes. AST
--     nodes and Kafka processor nodes don't have a 1:1
--     mapping — pure 'Arr' nodes compile to nothing, several
--     leaf constructors compile to multiple processors, and
--     the optimiser collapses Arr chains into one 'Arr'
--     constructor. Use 'topologyNodes' on the compiled graph
--     to count run-time nodes.
--   * The /continuation/ of a 'Bind' — the function returning
--     a continuation 'Topology' is opaque, so we count it as
--     a single placeholder.
countNodes :: Topology i o -> Int
countNodes = FA.countNodesFA

-- | Before / after node counts plus the absolute and relative
-- reduction the default rewrite passes achieve on a given AST.
--
-- Useful in tests, benchmarks, and golden-file outputs as a
-- sanity check on the optimiser — assert
-- @osNodesSaved > 0@ to confirm a representative pipeline is
-- actually being shrunk.
--
-- The fields:
--
--   * 'osBefore' — node count of the input AST (via
--     'countNodes' before rewriting).
--   * 'osAfter'  — node count after applying 'optimize'.
--   * 'osNodesSaved' — the difference, never negative
--     because every rewrite is strictly non-expanding.
--   * 'osPercent' — @100 * osNodesSaved / osBefore@; @0@ for
--     the (degenerate) empty-AST case.
data OptimizationStats = OptimizationStats
  { osBefore     :: !Int
  , osAfter      :: !Int
  , osNodesSaved :: !Int
  , osPercent    :: !Double
  }
  deriving stock (Eq, Show)

-- | Compute 'OptimizationStats' for applying the default
-- 'optimize' to @t@. Equivalent to
--
-- @
-- let before = 'countNodes' t
--     after  = 'countNodes' ('optimize' t)
-- in OptimizationStats before after (before - after) percent
-- @
--
-- but bundled into one call for ergonomics. Useful as the
-- @<>@-side of a 'show' / 'putStrLn' for quick optimiser
-- inspection at the REPL.
optimizationStats :: Topology i o -> OptimizationStats
optimizationStats t =
  let !before = countNodes t
      !after  = countNodes (optimize t)
      !saved  = before - after
      !pct    = if before == 0
                  then 0
                  else fromIntegral saved * 100 / fromIntegral before
   in OptimizationStats
        { osBefore     = before
        , osAfter      = after
        , osNodesSaved = saved
        , osPercent    = pct
        }
