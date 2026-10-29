{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
Module      : Kafka.Streams.Topology.Optimization
Description : KIP-295 / @StreamsConfig.topology.optimization@ toggles

The Java client lets users opt into a small set of topology
rewrites with @topology.optimization@:

  * @REUSE_KTABLE_SOURCE_TOPICS@   — a table read straight off a source
    topic uses that topic as its changelog instead of a separate
    @-changelog@ topic ('Kafka.Streams.Topology.optimizeTopology').
  * @MERGE_REPARTITION_TOPICS@     — the groupings and joins of one
    re-keyed stream share one repartition topic. Here that is how the
    builder always works
    ('Kafka.Streams.StreamsBuilder.sharedRepartitionNode'), so the flag
    changes nothing.
  * @SINGLE_STORE_SELF_JOIN@       — not implemented; the flag is
    accepted and ignored.

'optimizationFlags' turns the knob into booleans and
'Kafka.Streams.Topology.fromOptimizationFlags' into the
'Kafka.Streams.Topology.OptimizationConfig' that
'Kafka.Streams.Topology.optimizeTopology' takes. The imperative
builder's topology is not optimised unless you call it; the Free DSL's
'Kafka.Streams.Topology.Free.compile' does.
-}
module Kafka.Streams.Topology.Optimization (
  TopologyOptimizationLevel (..),
  OptimizationFlags (..),
  noOptimizations,
  optimizationFlags,
  parseOptimizationLevel,
  optimizationLevelText,
) where

import Data.Text (Text)
import GHC.Generics (Generic)


{- | The user-visible @topology.optimization@ knob. Mirrors the
Java enum verbatim.
-}
data TopologyOptimizationLevel
  = OptimizeNone
  | OptimizeReuseKtableSourceTopics
  | OptimizeMergeRepartitionTopics
  | OptimizeSingleStoreSelfJoin
  | OptimizeAll
  deriving stock (Eq, Show, Generic)


{- | Internal projection of the user's knob into the set of
rewriter passes that should run.
-}
data OptimizationFlags = OptimizationFlags
  { flagReuseSourceTopics :: !Bool
  , flagMergeRepartitionTopics :: !Bool
  , flagSingleStoreSelfJoin :: !Bool
  }
  deriving stock (Eq, Show, Generic)


noOptimizations :: OptimizationFlags
noOptimizations = OptimizationFlags False False False


optimizationFlags :: TopologyOptimizationLevel -> OptimizationFlags
optimizationFlags = \case
  OptimizeNone -> noOptimizations
  OptimizeReuseKtableSourceTopics ->
    noOptimizations {flagReuseSourceTopics = True}
  OptimizeMergeRepartitionTopics ->
    noOptimizations {flagMergeRepartitionTopics = True}
  OptimizeSingleStoreSelfJoin ->
    noOptimizations {flagSingleStoreSelfJoin = True}
  OptimizeAll -> OptimizationFlags True True True


{- | Parse the textual config value users put in their
@StreamsConfig@ properties. Mirrors the Java enum's @valueOf@.
-}
parseOptimizationLevel :: Text -> Maybe TopologyOptimizationLevel
parseOptimizationLevel = \case
  "none" -> Just OptimizeNone
  "reuse.ktable.source.topics" -> Just OptimizeReuseKtableSourceTopics
  "merge.repartition.topics" -> Just OptimizeMergeRepartitionTopics
  "single.store.self.join" -> Just OptimizeSingleStoreSelfJoin
  "all" -> Just OptimizeAll
  _ -> Nothing


optimizationLevelText :: TopologyOptimizationLevel -> Text
optimizationLevelText = \case
  OptimizeNone -> "none"
  OptimizeReuseKtableSourceTopics -> "reuse.ktable.source.topics"
  OptimizeMergeRepartitionTopics -> "merge.repartition.topics"
  OptimizeSingleStoreSelfJoin -> "single.store.self.join"
  OptimizeAll -> "all"
