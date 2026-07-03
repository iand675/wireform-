{- | Render a chart as a Mermaid @stateDiagram-v2@.

Paste the output into any Mermaid renderer (mermaid.live, GitHub
markdown, editor previews). The mapping from the runtime structure
("StateMachine.Runtime"):

* Compound states become composite blocks (@state X { … }@) with an
  initial marker (@[*] --> initial@) inside; the chart itself gets one at
  the top level.

* Parallel regions are separated by @--@ inside their parent block.

* Final states keep their name (they are addressable transition targets)
  and additionally get an edge to their scope's @[*]@ — Mermaid's only
  notion of "final".

* History pseudo-states have no Mermaid syntax: they render as a state
  displayed as @H@ (@H*@ for deep), carrying the real name as a
  description line, with a dashed-intent @default@ edge when an explicit
  default target exists.

* Transition labels read @EVENT [guard] \/ action1,action2@, with
  @after 3000ms@, @always@, @done(child)@, @invoke.done(id)@ \/
  @invoke.error(id)@, and @*@ for the trigger part. Targetless
  transitions render as self-loops (actions run, no exit\/entry).

* Root-level transitions fire from any state; they render from a
  synthetic @any state@ node — Mermaid has no "from the machine
  boundary" arrow.

Mermaid identifiers must be alphanumeric, so every state gets a
sanitized id and is declared via @state \"name\" as id@ — the display
name always survives verbatim. Ids are deduplicated (two names that
sanitize identically get distinct suffixes) and stable across both entry
points for a given chart.
-}
module StateMachine.Render.Mermaid (
  -- * Rendering
  mermaid,
  mermaidHighlight,
) where

import Data.Char (isAlphaNum, isDigit)
import Data.List (foldl', sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

import StateMachine.Runtime

{-------------------------------------------------------------------------------
  Entry points
-------------------------------------------------------------------------------}

-- | The chart as a Mermaid @stateDiagram-v2@.
mermaid :: RChart -> Text
mermaid = mermaidHighlight Set.empty

-- | Like 'mermaid', with the given states (typically an active 'Config')
-- highlighted via a @classDef@.
mermaidHighlight :: Set NodeName -> RChart -> Text
mermaidHighlight active c =
  T.unlines
    ( concat
        [ ["---", "title: " <> rcName c, "---", "stateDiagram-v2"]
        , [indent 1 <> "[*] --> " <> idOf ids (rcInitial c)]
        , concatMap (structureLines c ids 1) (rcTopLevel c)
        , topLevelFinalLines
        , descriptionLines c ids
        , edgeLines c ids
        , anyStateLines c ids
        , highlightLines
        ]
    )
 where
  ids = mkIds c
  topLevelFinalLines =
    map (\n -> indent 1 <> idOf ids n <> " --> [*]") (filter (isFinal c) (rcTopLevel c))
  highlightLines = case mapMaybe (\n -> Map.lookup n (idsMap ids)) (Set.toList active) of
    [] -> []
    activeIds ->
      [ indent 1 <> "classDef active fill:#ffd54f,stroke:#e65100,stroke-width:2px"
      , indent 1 <> "class " <> T.intercalate "," activeIds <> " active"
      ]

{-------------------------------------------------------------------------------
  Structure
-------------------------------------------------------------------------------}

-- | Declarations and nesting for one node: alias line, composite block,
-- initial marker, parallel separators, final edges to @[*]@.
structureLines :: RChart -> Ids -> Int -> NodeName -> [Text]
structureLines c ids depth n = case maybe RAtomic rnKind (lookupNode c n) of
  RAtomic -> [alias n]
  RFinal -> [alias n]
  RHistory kind _ ->
    [indent depth <> "state \"" <> historyLabel kind <> "\" as " <> me]
  RCompound ini ->
    concat
      [ [alias n, indent depth <> "state " <> me <> " {"]
      , [indent (depth + 1) <> "[*] --> " <> idOf ids ini]
      , concatMap (structureLines c ids (depth + 1)) kids
      , finalEdges
      , [indent depth <> "}"]
      ]
  RParallel ->
    concat
      [ [alias n, indent depth <> "state " <> me <> " {"]
      , regions
      , finalEdges
      , [indent depth <> "}"]
      ]
 where
  me = idOf ids n
  kids = childrenOf c n
  alias name = indent depth <> "state " <> quoted name <> " as " <> idOf ids name
  finalEdges =
    map
      (\f -> indent (depth + 1) <> idOf ids f <> " --> [*]")
      (filter (isFinal c) kids)
  regions = case map (structureLines c ids (depth + 1)) kids of
    [] -> []
    (r : rs) -> r ++ concatMap ((indent (depth + 1) <> "--") :) rs

historyLabel :: HistoryKind -> Text
historyLabel = \case
  Shallow -> "H"
  Deep -> "H*"

{-------------------------------------------------------------------------------
  Descriptions
-------------------------------------------------------------------------------}

-- | Per-state description lines: entry\/exit actions, invoked services,
-- and the real name of history pseudo-states (whose display name is only
-- @H@\/@H*@).
descriptionLines :: RChart -> Ids -> [Text]
descriptionLines c ids = concatMap forNode (nodesInDocOrder c)
 where
  forNode node =
    concat
      [ actionsDesc "entry" (rnEntry node)
      , actionsDesc "exit" (rnExit node)
      , map invokeDesc (rnInvokes node)
      , historyDesc
      ]
   where
    me = idOf ids (rnName node)
    actionsDesc label = \case
      [] -> []
      actions -> [indent 1 <> me <> " : " <> label <> " / " <> commaSep actions]
    invokeDesc inv =
      indent 1 <> me <> " : invoke " <> escapeLabel (riId inv <> "(" <> riSrc inv <> ")")
    historyDesc = case rnKind node of
      RHistory _ _ -> [indent 1 <> me <> " : " <> escapeLabel (rnName node)]
      _ -> []

{-------------------------------------------------------------------------------
  Edges
-------------------------------------------------------------------------------}

-- | All transition edges, flat at the top level (Mermaid state ids are
-- global, so edges need no nesting). Targetless transitions are
-- self-loops; a history default target is a @default@ edge.
edgeLines :: RChart -> Ids -> [Text]
edgeLines c ids = concatMap forNode (nodesInDocOrder c)
 where
  forNode node =
    concatMap (transEdges ids (rnName node)) (rnTransitions node) ++ defaultEdge node
  defaultEdge node = case rnKind node of
    RHistory _ (Just def) ->
      [indent 1 <> idOf ids (rnName node) <> " --> " <> idOf ids def <> " : default"]
    _ -> []

-- | Edges for one transition: one per target, self-loop when targetless.
transEdges :: Ids -> NodeName -> RTrans -> [Text]
transEdges ids src tr = case rtTargets tr of
  [] -> [edge src]
  targets -> map edge targets
 where
  edge tgt = indent 1 <> idOf ids src <> " --> " <> idOf ids tgt <> " : " <> transLabel tr

-- | Root-level (global) transitions, drawn from a synthetic node.
anyStateLines :: RChart -> Ids -> [Text]
anyStateLines c ids = case rcRootTransitions c of
  [] -> []
  transitions ->
    (indent 1 <> "state \"any state\" as " <> idsAny ids)
      : concatMap (transEdges ids' (idsAny ids)) transitions
 where
  -- The synthetic node is not a chart node: extend the mapping so
  -- 'transEdges' resolves it to itself.
  ids' = ids{idsMap = Map.insert (idsAny ids) (idsAny ids) (idsMap ids)}

-- | @EVENT [guard] \/ act1,act2@ — shared label shape for every trigger.
transLabel :: RTrans -> Text
transLabel tr = escapeLabel (trigger <> guard <> actions)
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
    as -> " / " <> commaSep as

{-------------------------------------------------------------------------------
  Identifiers
-------------------------------------------------------------------------------}

-- | The stable name → Mermaid-id mapping, plus the id reserved for the
-- synthetic @any state@ node.
data Ids = Ids
  { idsMap :: Map NodeName Text
  , idsAny :: Text
  }

mkIds :: RChart -> Ids
mkIds c = Ids{idsMap = mapping, idsAny = anyId}
 where
  (mapping, used) =
    foldl' claim (Map.empty, Set.empty) (map rnName (nodesInDocOrder c))
  (anyId, _) = fresh used (sanitizeId "__any")
  claim (m, taken) name =
    let (fid, taken') = fresh taken (sanitizeId name)
     in (Map.insert name fid m, taken')

-- | First unused candidate: the base, then @base_2@, @base_3@, …
fresh :: Set Text -> Text -> (Text, Set Text)
fresh taken base = go (1 :: Int)
 where
  go k =
    let cand = if k == 1 then base else base <> "_" <> T.pack (show k)
     in if Set.member cand taken
          then go (k + 1)
          else (cand, Set.insert cand taken)

-- | Mermaid ids must be @[A-Za-z0-9_]@ and not start with a digit.
sanitizeId :: Text -> Text
sanitizeId name = case T.uncons cleaned of
  Nothing -> "s"
  Just (ch, _) | isDigit ch -> "s_" <> cleaned
  _ -> cleaned
 where
  cleaned = T.map (\ch -> if isAlphaNum ch || ch == '_' then ch else '_') name

-- | Total for chart nodes ('mkIds' covers them all); the name itself is
-- the (never taken) fallback for foreign names.
idOf :: Ids -> NodeName -> Text
idOf ids n = Map.findWithDefault n n (idsMap ids)

{-------------------------------------------------------------------------------
  Text helpers
-------------------------------------------------------------------------------}

indent :: Int -> Text
indent depth = T.replicate depth "  "

quoted :: Text -> Text
quoted name = "\"" <> escapeLabel name <> "\""

-- | Keep labels on one line and free of the delimiters Mermaid parses.
escapeLabel :: Text -> Text
escapeLabel = T.map $ \case
  '"' -> '\''
  '\n' -> ' '
  '{' -> '('
  '}' -> ')'
  ch -> ch

commaSep :: [Text] -> Text
commaSep = T.intercalate ","

-- | Every node, preorder — the order declarations should appear in.
nodesInDocOrder :: RChart -> [RNode]
nodesInDocOrder = sortOn rnOrder . Map.elems . rcNodes
