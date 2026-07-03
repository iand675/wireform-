{- | Render a chart as Graphviz DOT.

Feed the output to @dot -Tsvg@ (or any Graphviz layout engine). The
mapping from the runtime structure ("StateMachine.Runtime"):

* Compound and parallel states become @cluster@ subgraphs (@compound=true@
  is set so edges may clip at cluster borders). The regions of a parallel
  state are drawn with dashed borders.

* Atomic states are rounded boxes, final states get @peripheries=2@,
  history pseudo-states are circles labeled @H@\/@H*@ (the real name
  rides along as an external @xlabel@).

* Each compound state (and the chart root) contains a @point@-shaped
  pseudo-node with an edge to the initial state — the statechart initial
  marker.

* Graphviz edges connect /nodes/, never clusters, so an edge touching a
  compound\/parallel state anchors at a representative leaf inside it
  (the initial state, transitively) and sets @lhead@\/@ltail@ to clip the
  arrow at the cluster border.

* Transition labels read @EVENT [guard] \/ action1,action2@ (with
  @after 3000ms@, @always@, @done(child)@, @invoke.done(id)@,
  @invoke.error(id)@, @*@). Targetless transitions are self-loops;
  root-level transitions fire from any state and render from a synthetic
  dashed @any state@ node; history default targets are dashed @default@
  edges.

Node ids are the (quote-escaped) state names themselves — DOT allows any
quoted string as an id, so no sanitization pass is needed.
-}
module StateMachine.Render.Dot (
  -- * Rendering
  dot,
  dotHighlight,
) where

import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

import StateMachine.Runtime

{-------------------------------------------------------------------------------
  Entry points
-------------------------------------------------------------------------------}

-- | The chart as a DOT digraph.
dot :: RChart -> Text
dot = dotHighlight Set.empty

-- | Like 'dot', with the given states (typically an active 'Config')
-- filled distinctly.
dotHighlight :: Set NodeName -> RChart -> Text
dotHighlight active c =
  T.unlines
    ( concat
        [ [ "digraph " <> quoted (rcName c) <> " {"
          , indent 1 <> "compound=true;"
          , indent 1 <> "rankdir=LR;"
          , indent 1 <> "labelloc=t;"
          , indent 1 <> "label=" <> quoted (rcName c) <> ";"
          , indent 1 <> "fontname=\"Helvetica\";"
          , indent 1 <> "node [fontname=\"Helvetica\", fontsize=11];"
          , indent 1 <> "edge [fontname=\"Helvetica\", fontsize=9];"
          , indent 1 <> quoted rootInit <> " [shape=point, width=0.12];"
          ]
        , concatMap (nodeLines active c 1 False) (rcTopLevel c)
        , anyStateLines c
        , [initEdge c 1 rootInit (rcInitial c)]
        , concatMap (edgesOf c) (allSources c)
        , ["}"]
        ]
    )
 where
  rootInit = freshName c "__init"

{-------------------------------------------------------------------------------
  Nodes and clusters
-------------------------------------------------------------------------------}

-- | Declarations for one node: a plain node, or a cluster subgraph with
-- its initial marker and children.
nodeLines :: Set NodeName -> RChart -> Int -> Bool -> NodeName -> [Text]
nodeLines active c depth insideParallel n =
  case maybe RAtomic rnKind (lookupNode c n) of
    RAtomic ->
      [leaf ("shape=box, style=" <> quoted (leafStyle "rounded") <> leafFill)]
    RFinal ->
      [leaf ("shape=box, peripheries=2, style=" <> quoted (leafStyle "rounded") <> leafFill)]
    RHistory kind _ ->
      [ indent depth
          <> quoted n
          <> " [shape=circle, label="
          <> quoted (historyLabel kind)
          <> ", xlabel="
          <> quoted n
          <> fillAttrs
          <> "];"
      ]
    RCompound ini ->
      cluster (childLines False <> [initPoint, initEdge c (depth + 1) (initName n) ini])
    RParallel ->
      cluster (childLines True)
 where
  leaf attrs = indent depth <> quoted n <> " [label=" <> quoted n <> ", " <> attrs <> "];"
  leafStyle base
    | Set.member n active = base <> ",filled"
    | otherwise = base
  leafFill
    | Set.member n active = ", fillcolor=\"#ffd54f\""
    | otherwise = ""
  fillAttrs
    | Set.member n active = ", style=filled, fillcolor=\"#ffd54f\""
    | otherwise = ""
  cluster body =
    concat
      [ [ indent depth <> "subgraph " <> quoted ("cluster_" <> n) <> " {"
        , indent (depth + 1) <> "label=" <> quoted n <> ";"
        , indent (depth + 1) <> "style=" <> quoted clusterStyle <> ";"
        ]
      , if Set.member n active
          then [indent (depth + 1) <> "bgcolor=\"#fff8e1\";"]
          else []
      , body
      , [indent depth <> "}"]
      ]
  clusterStyle
    | insideParallel = "rounded,dashed"
    | otherwise = "rounded"
  childLines parallel = concatMap (nodeLines active c (depth + 1) parallel) (childrenOf c n)
  initPoint = indent (depth + 1) <> quoted (initName n) <> " [shape=point, width=0.08];"
  initName name = freshName c ("__init_" <> name)

historyLabel :: HistoryKind -> Text
historyLabel = \case
  Shallow -> "H"
  Deep -> "H*"

-- | The initial-marker edge from a point pseudo-node into the initial
-- state (clipped at the cluster border when the initial state is one).
initEdge :: RChart -> Int -> Text -> NodeName -> Text
initEdge c depth point ini =
  indent depth
    <> quoted point
    <> " -> "
    <> quoted (repLeaf c ini)
    <> attrs
    <> ";"
 where
  attrs = case headAttr c ini of
    [] -> ""
    as -> " [" <> T.intercalate ", " as <> "]"

-- | The synthetic source of root-level (fire from any state) transitions.
anyStateLines :: RChart -> [Text]
anyStateLines c = case rcRootTransitions c of
  [] -> []
  _ ->
    [ indent 1
        <> quoted (anyName c)
        <> " [label=\"any state\", shape=box, style=\"rounded,dashed\"];"
    ]

anyName :: RChart -> Text
anyName = flip freshName "__any"

{-------------------------------------------------------------------------------
  Edges
-------------------------------------------------------------------------------}

-- | Transition sources: every node, then the synthetic root (rendered as
-- the @any state@ node).
allSources :: RChart -> [(NodeName, Text, [RTrans], Maybe (HistoryKind, NodeName))]
allSources c = map ofNode (Map.elems (rcNodes c)) ++ root
 where
  ofNode node = (rnName node, repLeaf c (rnName node), rnTransitions node, defaultOf node)
  defaultOf node = case rnKind node of
    RHistory kind (Just def) -> Just (kind, def)
    _ -> Nothing
  root = case rcRootTransitions c of
    [] -> []
    transitions -> [(anyName c, anyName c, transitions, Nothing)]

-- | All edges leaving one source: one per transition target, a self-loop
-- when targetless, plus the history @default@ edge.
edgesOf :: RChart -> (NodeName, Text, [RTrans], Maybe (HistoryKind, NodeName)) -> [Text]
edgesOf c (src, srcRep, transitions, historyDefault) =
  concatMap transEdges transitions ++ defaultEdges
 where
  transEdges tr = case rtTargets tr of
    [] -> [edgeTo src (transLabel tr) []]
    targets -> map (\tgt -> edgeTo tgt (transLabel tr) []) targets
  defaultEdges = case historyDefault of
    Nothing -> []
    Just (_, def) -> [edgeTo def "default" ["style=dashed"]]
  edgeTo tgt label extra =
    indent 1
      <> quoted srcRep
      <> " -> "
      <> quoted (repLeaf c tgt)
      <> " ["
      <> T.intercalate ", " (("label=" <> quoted label) : tailAttr c src ++ headAttr c tgt ++ extra)
      <> "];"

-- | @ltail@ \/ @lhead@ clip attributes when the logical endpoint is a
-- cluster (Graphviz edges must anchor at plain nodes).
tailAttr, headAttr :: RChart -> NodeName -> [Text]
tailAttr c n
  | isCluster c n = ["ltail=" <> quoted ("cluster_" <> n)]
  | otherwise = []
headAttr c n
  | isCluster c n = ["lhead=" <> quoted ("cluster_" <> n)]
  | otherwise = []

isCluster :: RChart -> NodeName -> Bool
isCluster c n = isCompound c n || isParallel c n

-- | A representative plain node inside a cluster to anchor edges at: the
-- initial state of a compound (transitively), the first region of a
-- parallel. The node itself when it is already plain.
repLeaf :: RChart -> NodeName -> NodeName
repLeaf c n = case maybe RAtomic rnKind (lookupNode c n) of
  RCompound ini -> repLeaf c ini
  RParallel -> case childrenOf c n of
    [] -> n
    (r : _) -> repLeaf c r
  _ -> n

-- | @EVENT [guard] \/ act1,act2@ — shared label shape for every trigger.
transLabel :: RTrans -> Text
transLabel tr = trigger <> guard <> actions
 where
  trigger = case rtTrigger tr of
    TOn e -> e
    TWildcard -> "*"
    TAlways -> "always"
    TAfter ms -> "after " <> T.pack (show ms) <> "ms"
    TDone s -> "done(" <> s <> ")"
    TInvokeDone i -> "invoke.done(" <> i <> ")"
    TInvokeError i -> "invoke.error(" <> i <> ")"
  guard = maybe "" (\g -> " [" <> g <> "]") (rtGuard tr)
  actions = case rtActions tr of
    [] -> ""
    as -> " / " <> T.intercalate "," as

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

-- | A pseudo-node name that cannot collide with a chart state: append
-- underscores until it is not one.
freshName :: RChart -> Text -> Text
freshName c base
  | Map.member base (rcNodes c) = freshName c (base <> "_")
  | otherwise = base

indent :: Int -> Text
indent depth = T.replicate depth "  "

-- | DOT double-quoted id\/label: escape quotes and backslashes.
quoted :: Text -> Text
quoted name = "\"" <> T.concatMap esc name <> "\""
 where
  esc = \case
    '"' -> "\\\""
    '\\' -> "\\\\"
    '\n' -> " "
    ch -> T.singleton ch
