{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- \$ dot -Tsvg topo.dot -o topo.svg
-- @
--
-- == Two flavours
--
-- ['topologyDot']
--   Renders the compiled imperative 'Topo.Topology' graph —
--   the same shape Java's @TopologyDescription@ produces.
--   Sources are rounded blue boxes, processors are grey boxes,
--   sinks are pink inverted trapeziums, and state stores are
--   yellow cylinders with dashed owner edges. This is the view
--   that matches what the Kafka runtime sees and what task
--   assignment / repartition decisions are made from.
--
-- ['astDot']
--   Renders the 'Topology' AST as its constructor tree:
--   'Compose' as the spine, 'Fanout' / 'Parallel' / 'Fork' as
--   diamond branches, leaf primitives as ovals. Useful for
--   debugging the optimiser (does the right-associated chain
--   look the way you expected?) and for confirming that
--   rewrite passes produced the AST shape your tests
--   asserted.
--
-- == Output stability
--
-- The DOT output is intended for /visualisation/, not for
-- parsing. Node IDs in 'astDot' depend on traversal order;
-- 'topologyDot' uses the topology's own 'NodeName's so its
-- IDs are stable across compilations of the same AST. The
-- attribute set on each node (colour, shape, label) may
-- change between releases — use a small layout-renderer
-- script in your tooling if you need rendering stability.

{- |
Module      : Kafka.Streams.Topology.Free.Graphviz
Description : Graphviz DOT visualisers for topologies

Renders a 'Topology' (AST) or a compiled
'Kafka.Streams.Topology.Topology' (graph) as a
<https://graphviz.org/doc/info/lang.html DOT> string, ready to
pipe into @dot@ for visualisation:

@
ghci> topo <- F.buildTopologyFrom myTopology
ghci> Text.IO.writeFile \"topo.dot\" (topologyDot topo)
-}
module Kafka.Streams.Topology.Free.Graphviz (
  -- * Compiled topology

  --
  -- The 'topologyDot' / 'topologyDotWith' functions render the
  -- compiled imperative graph — the same shape the Kafka
  -- runtime sees. This view is /always/ fully resolved, so
  -- it's the right pick when you want to see /past/ 'Bind'
  -- continuations: just 'Kafka.Streams.Topology.Free.compile'
  -- the AST first and feed the resulting topology in.
  topologyDot,
  topologyDotWith,

  -- * AST

  --
  -- 'astDot' / 'astDotWith' render the GADT constructor tree.
  -- 'Bind' continuations are rendered as opaque octagonal
  -- markers — to see past them, use
  -- @'topologyDot' . 'snd' '<$>'
  -- 'Kafka.Streams.Topology.Free.compileNoOptimize'@.
  astDot,
  astDotWith,

  -- * Configuration
  DotConfig (..),
  defaultDotConfig,
) where

import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Kafka.Streams.AsyncIO.Config qualified as AIO
import Kafka.Streams.Sinks.TwoPhase qualified as TPS
import Kafka.Streams.State.Store qualified as Store
import Kafka.Streams.Topology qualified as Topo
import Kafka.Streams.Topology.Free (
  Prim (..),
  SplitBranch (..),
  Topology,
 )
import Kafka.Streams.Topology.Free.Arrow (FreeArrow (..))
import Kafka.Streams.Types (NodeName, TopicName, unNodeName, unTopicName)


----------------------------------------------------------------------
-- Configuration
----------------------------------------------------------------------

{- | Rendering options. Pass to 'topologyDotWith' \/ 'astDotWith';
use 'topologyDot' \/ 'astDot' for the defaults.
-}
data DotConfig = DotConfig
  { dotRankDir :: !Text
  {- ^ Layout direction. Common values: @"TB"@ (top→bottom),
  @"LR"@ (left→right), @"BT"@, @"RL"@.
  -}
  , dotShowStores :: !Bool
  {- ^ Show state stores as their own nodes with dashed
  "owns" edges from processors. Off by default for AST
  output (stores aren't part of the AST); on by default
  for compiled-topology output.
  -}
  , dotShowTopics :: !Bool
  -- ^ Show source / sink topic names in the label.
  , dotSourceColor :: !Text
  {- ^ Fill colour for source nodes (Graphviz color names
  and @#RRGGBB@ both accepted).
  -}
  , dotSinkColor :: !Text
  -- ^ Fill colour for sink nodes.
  , dotProcessorColor :: !Text
  -- ^ Fill colour for processor nodes.
  , dotStoreColor :: !Text
  -- ^ Fill colour for state-store nodes.
  , dotStructColor :: !Text
  {- ^ Fill colour for structural AST nodes ('Compose',
  'Fanout', 'Parallel', etc.). Only relevant for
  'astDot' / 'astDotWith'.
  -}
  }


{- | Sensible defaults: top-to-bottom layout, stores visible,
topic names shown, pastel colour palette.
-}
defaultDotConfig :: DotConfig
defaultDotConfig =
  DotConfig
    { dotRankDir = "TB"
    , dotShowStores = True
    , dotShowTopics = True
    , dotSourceColor = "#bcd9ff" -- light blue
    , dotSinkColor = "#ffc1cc" -- light pink
    , dotProcessorColor = "#e6e6e6" -- light grey
    , dotStoreColor = "#fff3a8" -- light yellow
    , dotStructColor = "#d6e8d6" -- pale green
    }


----------------------------------------------------------------------
-- Compiled topology -> DOT
----------------------------------------------------------------------

{- | Render a compiled 'Topo.Topology' as a Graphviz DOT graph
using the default configuration ('defaultDotConfig').

The graph contains one node per source / processor / sink
and (when 'dotShowStores' is on) one node per state store.
Edges follow the topology's @parent → child@ relationships;
state-store ownership is shown as dashed edges with an
@odot@ arrowhead.
-}
topologyDot :: Topo.Topology -> Text
topologyDot = topologyDotWith defaultDotConfig


-- | 'topologyDot' with an explicit 'DotConfig'.
topologyDotWith :: DotConfig -> Topo.Topology -> Text
topologyDotWith cfg t =
  render $
    "digraph topology {\n"
      <> "  rankdir="
      <> tb (dotRankDir cfg)
      <> ";\n"
      <> "  node [fontname=\"Helvetica\"];\n"
      <> "  // Sources\n"
      <> foldMapMap (sourceNode cfg) (Topo.topoSources t)
      <> "  // Processors\n"
      <> foldMapMap (procNode cfg) (Topo.topoProcessors t)
      <> "  // Sinks\n"
      <> foldMapMap (sinkNode cfg) (Topo.topoSinks t)
      <> ( if dotShowStores cfg
             then
               "  // State stores\n"
                 <> foldMapMap (storeNode cfg) (Topo.topoStores t)
             else mempty
         )
      <> "  // Edges (parent -> child)\n"
      <> foldMapMap
        ( \nm spec ->
            mconcat
              [edge (Topo.processorSpecParents spec) nm | _ <- [()]]
        )
        (Topo.topoProcessors t)
      <> foldMapMap
        ( \nm spec ->
            mconcat
              [edge (Topo.sinkParents spec) nm | _ <- [()]]
        )
        (Topo.topoSinks t)
      <> ( if dotShowStores cfg
             then
               "  // Store ownership\n"
                 <> foldMapMap
                   (storeEdges cfg)
                   (Topo.topoStoreOwners t)
             else mempty
         )
      <> "}\n"


sourceNode :: DotConfig -> NodeName -> Topo.SourceSpec -> TB.Builder
sourceNode cfg nm spec =
  let !label =
        if dotShowTopics cfg
          then
            "SOURCE\n"
              <> unNodeName nm
              <> "\n"
              <> T.intercalate
                ","
                (map unTopicName (Topo.sourceTopics spec))
          else "SOURCE\n" <> unNodeName nm
  in "  "
       <> nodeId nm
       <> " [shape=box, style=\"filled,rounded\", "
       <> "fillcolor=\""
       <> tb (dotSourceColor cfg)
       <> "\", "
       <> "label="
       <> dotString label
       <> "];\n"


procNode :: DotConfig -> NodeName -> Topo.ProcessorSpec -> TB.Builder
procNode cfg nm _spec =
  "  "
    <> nodeId nm
    <> " [shape=box, style=filled, "
    <> "fillcolor=\""
    <> tb (dotProcessorColor cfg)
    <> "\", "
    <> "label="
    <> dotString (unNodeName nm)
    <> "];\n"


sinkNode :: DotConfig -> NodeName -> Topo.SinkSpec -> TB.Builder
sinkNode cfg nm spec =
  let !label =
        if dotShowTopics cfg
          then
            "SINK\n"
              <> unNodeName nm
              <> "\ntopic: "
              <> unTopicName (Topo.sinkTopic spec)
          else "SINK\n" <> unNodeName nm
  in "  "
       <> nodeId nm
       <> " [shape=invtrapezium, style=filled, "
       <> "fillcolor=\""
       <> tb (dotSinkColor cfg)
       <> "\", "
       <> "label="
       <> dotString label
       <> "];\n"


storeNode
  :: DotConfig -> Store.StoreName -> Topo.AnyStoreBuilder -> TB.Builder
storeNode cfg sn b =
  let !kind = case b of
        Topo.AsKeyValueBuilder _ -> "KV"
        Topo.AsWindowBuilder _ -> "Window"
        Topo.AsSessionBuilder _ -> "Session"
        Topo.AsRawBuilder _ -> "Raw"
      !label = kind <> " store\n" <> Store.unStoreName sn
  in "  "
       <> storeId sn
       <> " [shape=cylinder, style=\"filled,dashed\", "
       <> "fillcolor=\""
       <> tb (dotStoreColor cfg)
       <> "\", "
       <> "label="
       <> dotString label
       <> "];\n"


-- | Edges from a parents list to one child.
edge :: [NodeName] -> NodeName -> TB.Builder
edge parents child =
  foldMap
    (\p -> "  " <> nodeId p <> " -> " <> nodeId child <> ";\n")
    parents


-- | Owner-processor → state-store edges (dashed, odot arrowhead).
storeEdges
  :: DotConfig -> Store.StoreName -> [NodeName] -> TB.Builder
storeEdges _cfg sn owners =
  foldMap
    ( \ow ->
        "  "
          <> nodeId ow
          <> " -> "
          <> storeId sn
          <> " [style=dashed, arrowhead=odot];\n"
    )
    owners


----------------------------------------------------------------------
-- AST -> DOT
----------------------------------------------------------------------

{- | Render a 'Topology' AST as a DOT graph using the default
configuration ('defaultDotConfig'). The graph is the
constructor tree of the AST — useful for debugging the
optimiser or for confirming the AST shape a test built.

Note: AST node IDs are traversal-order-dependent. The output
is stable for a given AST value but unrelated ASTs may
collide on the same numeric prefix.
-}
astDot :: Topology i o -> Text
astDot =
  astDotWith
    defaultDotConfig
      { dotShowStores = False -- AST has no stores to show
      }


-- | 'astDot' with an explicit 'DotConfig'.
astDotWith :: forall i o. DotConfig -> Topology i o -> Text
astDotWith cfg topo =
  let (_, builder) = walkTopAst cfg 0 topo
  in render $
       "digraph ast {\n"
         <> "  rankdir="
         <> tb (dotRankDir cfg)
         <> ";\n"
         <> "  node [fontname=\"Helvetica\"];\n"
         <> builder
         <> "}\n"


{- | Result of a walk: @(nextId, builder)@. The builder
accumulates node and edge declarations for the visited
sub-tree.
-}
type WalkResult = (Int, TB.Builder)


{- | Walk the AST starting at the supplied ID and emit the
corresponding DOT fragment. Returns the next-free ID and
the fragment.
-}
walkTopAst
  :: forall i o. DotConfig -> Int -> Topology i o -> WalkResult
walkTopAst cfg = go
  where
    go :: forall a b. Int -> Topology a b -> WalkResult
    -- Category / Arrow
    go i Id = leaf i "Id" "ellipse"
    go i (Compose g f) =
      let !me = i
          (i1, b1) = go (i + 1) f -- emit children first
          (i2, b2) = go i1 g
      in struct cfg me "Compose" [(i + 1, "L"), (i1, "R")] b1 b2 i2
    go i (Arr _) = leaf i "Arr" "ellipse"
    go i (First t) = oneChild i "First" t
    go i (Second t) = oneChild i "Second" t
    go i (Parallel p q) = twoChild i "Parallel" p q
    go i (Fanout p q) = twoChild i "Fanout" p q
    go i (LeftT t) = oneChild i "LeftT" t
    go i (RightT t) = oneChild i "RightT" t
    go i (Plus p q) = twoChild i "Plus" p q
    go i (Fanin p q) = twoChild i "Fanin" p q
    -- Lineage
    go i Fork = leaf i "Fork" "diamond"
    go i (ForkN ts) =
      let !me = i
          (iN, bN) =
            foldl
              ( \(curI, bAcc) t ->
                  let (ci, cb) = go curI t
                      !ed =
                        "  "
                          <> nodeIdInt me
                          <> " -> "
                          <> nodeIdInt curI
                          <> ";\n"
                  in (ci, bAcc <> cb <> ed)
              )
              (i + 1, mempty)
              (NE.toList ts)
          !meDef = leafNode cfg me "ForkN" "diamond"
      in (iN, meDef <> bN)
    go i (Tap t) = oneChild i "Tap" t
    -- Monad bind. Rendered as an opaque octagon with one edge to
    -- the left side. (For a fully-walked rendering of binds,
    -- compose with 'topologyDot' against the compiled topology.)
    go i (Bind t _) =
      let !me = i
          (i1, b1) = go (i + 1) t
          !meDef = leafNode cfg me "Bind\n(opaque continuation)" "octagon"
          !ed =
            "  "
              <> nodeIdInt me
              <> " -> "
              <> nodeIdInt (i + 1)
              <> " [label=\"\\>\\>=\"];\n"
      in (i1, meDef <> b1 <> ed)
    -- The Kafka primitives — dispatch via 'primNode' which returns
    -- a (label, shape) pair.
    go i (Lift p) =
      let (lab, shape) = primNode p
      in leaf i lab shape

    -- Helpers (closed over cfg).
    leaf :: Int -> Text -> Text -> WalkResult
    leaf i label shape = (i + 1, leafNode cfg i label shape)

    oneChild
      :: forall a b. Int -> Text -> Topology a b -> WalkResult
    oneChild i lab t =
      let !me = i
          (i1, b1) = go (i + 1) t
          !meDef = leafNode cfg me lab "diamond"
          !ed =
            "  "
              <> nodeIdInt me
              <> " -> "
              <> nodeIdInt (i + 1)
              <> ";\n"
      in (i1, meDef <> b1 <> ed)

    twoChild
      :: forall a b c d
       . Int -> Text -> Topology a b -> Topology c d -> WalkResult
    twoChild i lab p q =
      let !me = i
          (i1, b1) = go (i + 1) p
          (i2, b2) = go i1 q
          !meDef = leafNode cfg me lab "diamond"
          !ed1 =
            "  "
              <> nodeIdInt me
              <> " -> "
              <> nodeIdInt (i + 1)
              <> ";\n"
          !ed2 =
            "  "
              <> nodeIdInt me
              <> " -> "
              <> nodeIdInt i1
              <> ";\n"
      in (i2, meDef <> b1 <> b2 <> ed1 <> ed2)


{- | Pick a (label, Graphviz shape) pair for a 'Prim'. Used by
'walkTopAst' to render the Kafka-specific primitives; the
framework constructors are handled in line.
-}
primNode :: forall i o. Prim i o -> (Text, Text)
primNode p0 = case p0 of
  Split bs md ->
    ( "Split\n["
        <> T.intercalate "," (map sbName bs)
        <> maybe "" (\d -> "+default=" <> d) md
        <> "]"
    , "diamond"
    )
  Source tn _ -> ("Source\n" <> showTopic tn, "box")
  SourceMulti tns _ ->
    ( "SourceMulti\n"
        <> T.intercalate "," (map showTopic (NE.toList tns))
    , "box"
    )
  TableSource tn _ _ -> ("TableSource\n" <> showTopic tn, "box")
  GlobalSource tn _ _ -> ("GlobalSource\n" <> showTopic tn, "box")
  Sink tn _ -> ("Sink\n" <> showTopic tn, "invtrapezium")
  SinkExtracted _ _ -> ("SinkExtracted", "invtrapezium")
  SinkTwoPhase sink ->
    ("SinkTwoPhase\n" <> TPS.tpsName sink, "invtrapezium")
  Through tn _ -> ("Through\n" <> showTopic tn, "box")
  MapValues _ -> ("MapValues", "ellipse")
  MapValuesM _ -> ("MapValuesM", "ellipse")
  MapKeyValue _ -> ("MapKeyValue", "ellipse")
  MapKeyValueM _ -> ("MapKeyValueM", "ellipse")
  AsyncMapValues cfg _ ->
    ("AsyncMapValues\n" <> aioConfigLabel cfg, "ellipse")
  AsyncMapKeyValue cfg _ ->
    ("AsyncMapKeyValue\n" <> aioConfigLabel cfg, "ellipse")
  AsyncConcatMapValues cfg _ ->
    ("AsyncConcatMapValues\n" <> aioConfigLabel cfg, "ellipse")
  MapRecord _ -> ("MapRecord", "ellipse")
  MapRecordM _ -> ("MapRecordM", "ellipse")
  NoFuse -> ("NoFuse", "doublecircle")
  Filter _ -> ("Filter", "ellipse")
  FilterNot _ -> ("FilterNot", "ellipse")
  ConcatMapValues _ -> ("ConcatMapValues", "ellipse")
  ConcatMapKeyValue _ -> ("ConcatMapKeyValue", "ellipse")
  Peek _ -> ("Peek", "ellipse")
  Foreach _ -> ("Foreach", "invtrapezium")
  SelectKey _ -> ("SelectKey", "ellipse")
  Values -> ("Values", "ellipse")
  Print nm _ -> ("Print\n" <> nm, "invtrapezium")
  Merge -> ("Merge", "ellipse")
  MergeAll -> ("MergeAll", "ellipse")
  Branch ps ->
    ("Branch\n" <> T.pack (show (length ps)) <> " ways", "diamond")
  ToTableT _ -> ("ToTable", "box")
  ToStream -> ("ToStream", "ellipse")
  Repartition pfx -> ("Repartition\n" <> fromMaybe "auto" pfx, "box")
  RepartitionWith _ -> ("RepartitionWith", "box")
  GroupByKey _ -> ("GroupByKey", "ellipse")
  GroupBy _ _ -> ("GroupBy", "ellipse")
  Count _ -> ("Count", "box3d")
  Reduce _ _ -> ("Reduce", "box3d")
  Aggregate _ _ _ -> ("Aggregate", "box3d")
  WindowedByTime _ -> ("WindowedByTime", "ellipse")
  WindowedBySession _ -> ("WindowedBySession", "ellipse")
  CountWindowed _ -> ("CountWindowed", "box3d")
  ReduceWindowed _ _ -> ("ReduceWindowed", "box3d")
  AggregateWindowed _ _ _ -> ("AggregateWindowed", "box3d")
  CountSessionWindowed _ -> ("CountSessionWindowed", "box3d")
  AggregateSessionWindowed _ _ _ _ -> ("AggregateSessionWindowed", "box3d")
  GroupTableBy _ _ -> ("GroupTableBy", "ellipse")
  CountKGroupedTable _ -> ("CountKGroupedTable", "box3d")
  ReduceKGroupedTable _ _ _ -> ("ReduceKGroupedTable", "box3d")
  AggregateKGroupedTable _ _ _ _ -> ("AggregateKGroupedTable", "box3d")
  Cogroup _ -> ("Cogroup", "ellipse")
  AddCogrouped _ -> ("AddCogrouped", "ellipse")
  AggregateCogrouped _ _ -> ("AggregateCogrouped", "box3d")
  StreamTableJoin _ _ -> ("StreamTableJoin", "hexagon")
  StreamTableLeftJoin _ _ -> ("StreamTableLeftJoin", "hexagon")
  StreamStreamJoin _ _ _ -> ("StreamStreamJoin", "hexagon")
  StreamStreamLeftJoin _ _ _ -> ("StreamStreamLeftJoin", "hexagon")
  StreamStreamOuterJoin _ _ _ -> ("StreamStreamOuterJoin", "hexagon")
  TableTableJoin _ _ -> ("TableTableJoin", "hexagon")
  TableTableLeftJoin _ _ -> ("TableTableLeftJoin", "hexagon")
  TableTableOuterJoin _ _ -> ("TableTableOuterJoin", "hexagon")
  ForeignKeyJoin _ _ _ -> ("ForeignKeyJoin", "hexagon")
  LeftForeignKeyJoin _ _ _ -> ("LeftForeignKeyJoin", "hexagon")
  StreamGlobalTableJoin _ _ -> ("StreamGlobalTableJoin", "hexagon")
  StreamGlobalTableLeftJoin _ _ -> ("StreamGlobalTableLeftJoin", "hexagon")
  FilterTable _ _ -> ("FilterTable", "ellipse")
  FilterNotTable _ _ -> ("FilterNotTable", "ellipse")
  MapValuesTable _ _ -> ("MapValuesTable", "ellipse")
  TransformValuesTable nm _ _ _ -> ("TransformValuesTable\n" <> nm, "box")
  SuppressUntilTimeLimit _ -> ("SuppressUntilTimeLimit", "ellipse")
  SuppressWindowedKS _ _ -> ("SuppressWindowed", "ellipse")
  ProcessStream nm _ _ -> ("ProcessStream\n" <> nm, "box")
  ProcessValuesStream nm _ _ _ -> ("ProcessValuesStream\n" <> nm, "box")
  TransformValuesStreamT nm _ _ _ -> ("TransformValuesStream\n" <> nm, "box")
  WithStateStoreKV b _ ->
    ("WithStateStoreKV\n" <> Store.unStoreName (Store.sbKvName b), "cylinder")
  WithStateStoreW b _ ->
    ("WithStateStoreW\n" <> Store.unStoreName (Store.sbWName b), "cylinder")
  WithStateStoreS b _ ->
    ("WithStateStoreS\n" <> Store.unStoreName (Store.sbSName b), "cylinder")
  ProcessWithStateStoreKV nm b _ ->
    ( "ProcessWithStateStoreKV\n"
        <> nm
        <> "\n"
        <> Store.unStoreName (Store.sbKvName b)
    , "box"
    )
  ProcessWithStateStoreW nm b _ ->
    ( "ProcessWithStateStoreW\n"
        <> nm
        <> "\n"
        <> Store.unStoreName (Store.sbWName b)
    , "box"
    )
  ProcessWithStateStoreS nm b _ ->
    ( "ProcessWithStateStoreS\n"
        <> nm
        <> "\n"
        <> Store.unStoreName (Store.sbSName b)
    , "box"
    )
  Lifted nm _ -> ("Lifted\n" <> nm, "octagon")


{- | Compact textual badge for an 'AIO.AsyncIOConfig' embedded in a
DOT label: shows the operator name and the headline knobs
(buffer + workers + ordering) so the rendered graph reflects
the configured concurrency.
-}
aioConfigLabel :: AIO.AsyncIOConfig -> Text
aioConfigLabel cfg =
  AIO.aioName cfg
    <> "[buf="
    <> T.pack (show (AIO.aioBufferCapacity cfg))
    <> ",w="
    <> T.pack (show (AIO.aioWorkers cfg))
    <> ","
    <> outputModeLabel (AIO.aioOutputMode cfg)
    <> "]"
  where
    outputModeLabel AIO.OrderedOutput = "ord"
    outputModeLabel AIO.UnorderedOutput = "unord"


leafNode :: DotConfig -> Int -> Text -> Text -> TB.Builder
leafNode cfg i lab shape =
  "  "
    <> nodeIdInt i
    <> " [shape="
    <> tb shape
    <> ", style=filled, "
    <> "fillcolor=\""
    <> tb (dotStructColor cfg)
    <> "\", "
    <> "label="
    <> dotString lab
    <> "];\n"


{- | Emit a structural node and its two-child edges. Useful for
compose-like nodes where the order matters (L/R labels on
edges). Currently unused — 'twoChild' covers the common case
— but kept for symmetry.
-}
struct
  :: DotConfig
  -> Int
  -- ^ node id
  -> Text
  -- ^ label
  -> [(Int, Text)]
  -- ^ children (id, edge-label)
  -> TB.Builder
  -- ^ child fragments concatenated
  -> TB.Builder
  -- ^ child fragments concatenated (second)
  -> Int
  -> WalkResult
struct cfg me lab children fragL fragR nextI =
  let !meDef = leafNode cfg me lab "diamond"
      !edges =
        foldMap
          ( \(c, el) ->
              "  "
                <> nodeIdInt me
                <> " -> "
                <> nodeIdInt c
                <> " [label=\""
                <> tb el
                <> "\"];\n"
          )
          children
  in (nextI, meDef <> fragL <> fragR <> edges)


----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

{- | Quote and escape a string for DOT's @label="…"@ attribute.
Handles backslashes, double quotes, and newlines.
-}
dotString :: Text -> TB.Builder
dotString s =
  "\"" <> tb (T.concatMap esc s) <> "\""
  where
    esc :: Char -> Text
    esc '\\' = "\\\\"
    esc '"' = "\\\""
    esc '\n' = "\\n"
    esc c = T.singleton c


-- | Quote a 'NodeName' for use as a DOT identifier.
nodeId :: NodeName -> TB.Builder
nodeId nm = "\"" <> tb (unNodeName nm) <> "\""


-- | DOT identifier for an integer (AST node).
nodeIdInt :: Int -> TB.Builder
nodeIdInt = TB.fromString . ('n' :) . show


{- | Quote a 'Store.StoreName' for use as a DOT identifier. We
prefix with @"store:"@ so it can't collide with a 'NodeName'
that happens to share the same text.
-}
storeId :: Store.StoreName -> TB.Builder
storeId sn = "\"store:" <> tb (Store.unStoreName sn) <> "\""


-- | Show a 'TopicName' without the @TopicName "@…@"@ wrapper.
showTopic :: TopicName -> Text
showTopic = unTopicName


tb :: Text -> TB.Builder
tb = TB.fromText


render :: TB.Builder -> Text
render = TL.toStrict . TB.toLazyText


{- | 'foldMap' over a 'Map.Map' but giving the callback the
key /and/ the value. Used a lot in the topology renderer.
-}
foldMapMap :: Monoid b => (k -> a -> b) -> Map.Map k a -> b
foldMapMap f = Map.foldrWithKey (\k a !acc -> f k a <> acc) mempty
