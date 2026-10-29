{- |
Module      : Kafka.Streams
Description : Public API umbrella for the Kafka Streams port

Re-exports the Kafka Streams DSL in one place so user code only
needs @import Kafka.Streams@.

= Shape of the DSL

The public DSL is "Kafka.Streams.Topology.Free": a topology is a
/first-class value/ of type @'Topology' i o@, composed with the
'Control.Category.Category' and 'Control.Arrow.Arrow' operators.
Sources and sinks fix the wire shape; combinators in between
form a pure AST that can be inspected, optimised, and only at
the boundary compiled to the imperative @'Kafka.Streams.Topology'@
graph.

@
import Control.Category ((>>>))
import Kafka.Streams

topology :: Topology Void ()
topology =
  source "in"  textSerde textSerde
    >>> mapValues T.toUpper
    >>> filter   (\\r -> recordValue r /= "")
    >>> sink "out" textSerde textSerde

main :: IO ()
main = do
  topo   <- buildTopologyFrom topology
  driver <- newDriver topo "my-app"
  ...
@

Each combinator's Haddock lists the JVM-DSL method it mirrors so
the cross-reference is one click away. Two-source operations
(joins) pair their legs with 'Control.Arrow.(&&&)' or the
dedicated @join*@ helpers (e.g. 'joinStreamTable',
'joinForeignKey', 'joinStreamGlobalTable').

= Re-exported modules

The umbrella exports:

  * "Kafka.Streams.Topology.Free" — the topology DSL (primary surface).
  * The wire-handle types ('KStream', 'KTable', 'KGroupedStream',
    'KGroupedTable', 'GlobalKTable', 'CogroupedStream',
    'TimeWindowedKStream', 'SessionWindowedKStream',
    'WindowedTableHandle', 'SessionWindowedTableHandle').
  * Value / config types ("Time", "Window", "Serde",
    "Materialized", "Consumed", "Produced", "Grouped", "Joined",
    "Named", "Repartitioned", "Types", "Errors", "Config").
  * Processor API surface ("Processor", "ProcessorContext",
    "Punctuator", "FixedKeyProcessor", "FixedKeyRecord").
  * State-store builders and interfaces.
  * Driver, runtime, interactive-queries, query, discovery, and
    metrics namespaces.
  * The low-level "Kafka.Streams.Topology" graph builder
    and "Kafka.Streams.TopologyDescription" so users who need
    direct access (e.g. to register a processor by name from
    inside 'F.liftIO_') can still reach for it.

= Imperative DSL modules (no longer in the umbrella)

The imperative @'Kafka.Streams.KStream'@ / @.KTable@ /
@.KGroupedStream@ / @.KGroupedTable@ / @.GlobalKTable@ /
@.SessionWindowedKStream@ / @.TimeWindowedKStream@ / @.Cogroup@ /
@.ForeignKeyJoin@ / @.Suppress@ / @.StreamsBuilder@ /
@.Pipeline@ / @.DSL@ modules remain importable individually but
are no longer re-exported here. They power the Free DSL's
compiler and remain available for callers that need the
imperative @\KStream -> IO KStream@ shape directly.

@
import qualified Kafka.Streams.KStream as KS
@

= Stores module

"Kafka.Streams.Stores" shadows per-backend factory names (its
@Stores.persistentKeyValueStore@ \/ @Stores.inMemoryWindowStore@
shape mirrors the JVM @org.apache.kafka.streams.state.Stores@
class). Import it qualified instead:

@
import qualified Kafka.Streams.Stores as Stores
@
-}
module Kafka.Streams (
  -- * Topology DSL (Free)
  module Kafka.Streams.Topology.Free,

  -- * Wire-handle types
  KStream (..),
  KTable (..),
  KGroupedStream,
  TimeWindowedKStream,
  SessionWindowedKStream,
  KGroupedTable,
  GlobalKTable,
  CogroupedStream,
  Cog.TimeWindowedCogroupedStream,
  TWKS.WindowedTableHandle (..),
  SWKS.SessionWindowedTableHandle (..),

  -- ** Imperative-DSL handle accessors

  --
  -- The handles above export their fields; the global table's
  -- selectors are re-exported here for the same reason.
  GT.globalKTableStore,
  GT.globalKTableNode,
  GT.globalKTableBuilder,
  GT.globalKTableKeySerde,
  GT.globalKTableValueSerde,

  -- ** TopicNameExtractor (KIP-303)
  KS.TopicNameExtractor (..),

  -- ** KIP-418 named branches
  KS.Branched (..),
  KS.branchedFrom,
  KS.withFunction,
  KS.withConsumer,

  -- * StreamsBuilder (imperative escape hatch)

  --
  -- Most users don't need 'newStreamsBuilder' directly — the
  -- Free DSL hides it — but 'liftIO_' fragments that touch the
  -- imperative DSL do.
  StreamsBuilder,
  newStreamsBuilder,
  buildTopology,
  withTopology_,
  freshNodeName,
  SB.freshStoreName,

  -- * Value / config types
  module Kafka.Streams.Types,
  module Kafka.Streams.Time,
  module Kafka.Streams.Errors,
  module Kafka.Streams.Serde,
  module Kafka.Streams.Window,
  module Kafka.Streams.Metrics,
  module Kafka.Streams.Query,
  module Kafka.Streams.Discovery,
  module Kafka.Streams.Config,

  -- * Operator config types
  module Kafka.Streams.Consumed,
  module Kafka.Streams.Produced,
  module Kafka.Streams.Repartitioned,
  module Kafka.Streams.Grouped,
  module Kafka.Streams.Joined,
  module Kafka.Streams.Named,
  module Kafka.Streams.Materialized,

  -- * Processor API
  module Kafka.Streams.Processor,

  -- * Low-level topology graph

  --
  -- Most users don't need to touch the imperative graph
  -- directly — 'F.compile' walks the 'F.Topology' AST and
  -- builds it. Re-exported names below are the ones useful
  -- inside 'F.liftIO_' fragments: node-name handling and
  -- introspection. The full imperative builder API
  -- ('addProcessor', 'addStateStoreKV', etc.) is reachable
  -- via @import qualified Kafka.Streams.Topology as Topo@.
  Topo.NodeName (..),
  Topo.AnyProcessor (..),
  Topo.OptimizationConfig,
  Topo.defaultOptimizationConfig,
  Topo.noOptimisations,
  Topo.fromOptimizationFlags,
  Topo.optimizeTopology,
  Topo.validateTopology,
  Topo.TopologyValid,
  Topo.TopologyError,
  Topo.ChangelogPlanProblem,
  module Kafka.Streams.TopologyDescription,

  -- * State stores
  module Kafka.Streams.State.Store,
  module Kafka.Streams.State.KeyValue.InMemory,
  module Kafka.Streams.State.KeyValue.Persistent,
  module Kafka.Streams.State.KeyValue.Caching,
  module Kafka.Streams.State.KeyValue.Timestamped,
  module Kafka.Streams.State.KeyValue.Versioned,
  module Kafka.Streams.State.Window.InMemory,
  module Kafka.Streams.State.Session.InMemory,

  -- * Driver / Runtime / IQ
  module Kafka.Streams.Driver,
  module Kafka.Streams.Runtime,
  module Kafka.Streams.InteractiveQueries,
) where

import Kafka.Streams.Cogroup (CogroupedStream)
import Kafka.Streams.Cogroup qualified as Cog
import Kafka.Streams.Config
import Kafka.Streams.Consumed
import Kafka.Streams.Discovery
import Kafka.Streams.Driver
import Kafka.Streams.Errors
import Kafka.Streams.GlobalKTable (GlobalKTable)
import Kafka.Streams.GlobalKTable qualified as GT
import Kafka.Streams.Grouped
import Kafka.Streams.InteractiveQueries
import Kafka.Streams.Joined
import Kafka.Streams.KGroupedStream (
  KGroupedStream,
  SessionWindowedKStream,
  TimeWindowedKStream,
 )
import Kafka.Streams.KGroupedTable (KGroupedTable)
import Kafka.Streams.KStream (KStream (..))
import Kafka.Streams.KStream qualified as KS
import Kafka.Streams.KTable (KTable (..))
import Kafka.Streams.Materialized
import Kafka.Streams.Metrics
import Kafka.Streams.Named
import Kafka.Streams.Processor
import Kafka.Streams.Produced
import Kafka.Streams.Query
import Kafka.Streams.Repartitioned
import Kafka.Streams.Runtime
import Kafka.Streams.Serde
import Kafka.Streams.SessionWindowedKStream qualified as SWKS
import Kafka.Streams.State.KeyValue.Caching
import Kafka.Streams.State.KeyValue.InMemory
import Kafka.Streams.State.KeyValue.Persistent
import Kafka.Streams.State.KeyValue.Timestamped
import Kafka.Streams.State.KeyValue.Versioned
import Kafka.Streams.State.Session.InMemory
import Kafka.Streams.State.Store
import Kafka.Streams.State.Window.InMemory
import Kafka.Streams.StreamsBuilder (
  StreamsBuilder,
  buildTopology,
  freshNodeName,
  newStreamsBuilder,
  withTopology_,
 )
import Kafka.Streams.StreamsBuilder qualified as SB
import Kafka.Streams.Time
import Kafka.Streams.TimeWindowedKStream qualified as TWKS
import Kafka.Streams.Topology qualified as Topo
import Kafka.Streams.Topology.Free
import Kafka.Streams.TopologyDescription
import Kafka.Streams.Types
import Kafka.Streams.Window

