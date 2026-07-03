{- | Render a chart as a single self-contained HTML page.

One @.html@ file, zero network dependencies: inline CSS, a few lines of
inline vanilla JS, and an SVG statechart diagram laid out in Haskell. The
page shows:

* a header with the chart name and state\/transition\/event counts;

* the statechart as SVG — a recursive box layout: atomic states are
  rounded rectangles, compound states are containers with a title bar and
  children wrapped into rows, parallel states stack their regions with
  dashed separators, history pseudo-states are @H@\/@H*@ circles, final
  states are double-bordered, and each container carries an initial
  marker (filled dot → initial child). Transition arrows are straight
  lines with event labels (parallel edges between the same pair fan out;
  self-loops and targetless transitions arc over their state). When an
  active configuration is supplied its states are filled distinctly;

* a transitions table (source, trigger, guard, targets, actions,
  internal) — hovering a row highlights its arrows in the diagram (the
  page's only JS, purely cosmetic);

* a trace timeline (one row per 'MicroTrace') when a trace is supplied;

* @\<details\>@ blocks with copy-pasteable Mermaid source
  ("StateMachine.Render.Mermaid") and XState JSON
  ("StateMachine.Render.XState").

Everything user-controlled is HTML-escaped; the page is fully
informative with JS disabled.
-}
module StateMachine.Render.Html (
  -- * Rendering
  htmlPage,
  htmlPageSimple,
) where

import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as BL
import Data.List (foldl', sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

import StateMachine.Render.Mermaid (mermaid, mermaidHighlight)
import StateMachine.Render.XState (xstateConfigText)
import StateMachine.Runtime
import StateMachine.Step (MicroTrace (..))

{-------------------------------------------------------------------------------
  Entry points
-------------------------------------------------------------------------------}

{- | The chart as a complete HTML page. The optional set of states
(typically an active 'Config') is highlighted in the diagram and listed
in the header; the trace, when non-empty, becomes a timeline table.
-}
htmlPage :: RChart -> Maybe (Set NodeName) -> [MicroTrace] -> Text
htmlPage c mActive traces =
  T.unlines
    ( concat
        [ prologue c
        , headerSection c mActive rows
        , svgSection c mActive boxes anyBox arrows
        , transitionsSection rows rowEdgeIds
        , traceSection traces
        , sourcesSection c mActive
        , epilogue
        ]
    )
 where
  boxes = placeGrid c (rcTopLevel c) (marginX + initInset, marginY)
  anyBox = anyStateBox c boxes
  rows = transitionRows c
  (arrows, rowEdgeIds) = buildArrows c boxes anyBox rows

-- | 'htmlPage' with no active configuration and no trace.
htmlPageSimple :: RChart -> Text
htmlPageSimple c = htmlPage c Nothing []

{-------------------------------------------------------------------------------
  Layout: geometry constants
-------------------------------------------------------------------------------}

-- | An absolutely positioned rectangle, in px.
data Box = Box
  { boxX :: Int
  , boxY :: Int
  , boxW :: Int
  , boxH :: Int
  }
  deriving stock (Eq)

-- Monospace text at 12px advances ~7.2px per char; 8 gives slack.
-- 'padTop' leaves headroom under a container title for self-loop arcs
-- over first-row children; the gaps leave corridors for edge labels.
charW, atomH, histD, titleH, padX, padY, padTop, initInset, hGap, vGap, regionGap, wrapAt :: Int
charW = 8
atomH = 36
histD = 30
titleH = 24
padX = 16
padY = 14
padTop = titleH + 34
initInset = 24
hGap = 64
vGap = 76
regionGap = 28
wrapAt = 3

marginX, marginY :: Int
marginX = 32
marginY = 56

textW :: Text -> Int
textW = (charW *) . T.length

leafW :: Text -> Int
leafW name = max 72 (textW name + 20)

{-------------------------------------------------------------------------------
  Layout: sizing and placement
-------------------------------------------------------------------------------}

kindOf :: RChart -> NodeName -> RNodeKind
kindOf c n = maybe RAtomic rnKind (lookupNode c n)

-- | Bottom-up size of a node's box, children included.
sizeOf :: RChart -> NodeName -> (Int, Int)
sizeOf c n = case kindOf c n of
  RAtomic -> (leafW n, atomH)
  RFinal -> (leafW n + 8, atomH + 8)
  RHistory _ _ -> (max histD (textW n), histD + 16)
  RCompound _ -> gridSize
  RParallel -> stackSize
 where
  titleW = textW n + 16
  gridSize = case chunksOf wrapAt (childrenOf c n) of
    [] -> (leafW n, atomH)
    rows ->
      let dims = map rowDim rows
          innerW = maximum (titleW : map fst dims)
          innerH = sum (map snd dims) + vGap * (length rows - 1)
       in (innerW + 2 * padX + initInset, padTop + innerH + padY)
  rowDim names =
    let sizes = map (sizeOf c) names
     in (sum (map fst sizes) + hGap * (length names - 1), maximum (atomH : map snd sizes))
  stackSize = case childrenOf c n of
    [] -> (leafW n, atomH)
    kids ->
      let dims = map (sizeOf c) kids
          innerW = maximum (titleW : map fst dims)
          innerH = sum (map snd dims) + regionGap * (length kids - 1)
       in (innerW + 2 * padX, padTop + innerH + padY)

-- | Top-down placement of a node and everything beneath it.
placeNode :: RChart -> NodeName -> (Int, Int) -> Map NodeName Box
placeNode c n (x, y) = Map.insert n (Box x y w h) children
 where
  (w, h) = sizeOf c n
  children = case kindOf c n of
    RCompound _ -> placeGrid c (childrenOf c n) (x + padX + initInset, y + padTop)
    RParallel -> placeStack c (childrenOf c n) (x + padX, y + padTop)
    _ -> Map.empty

-- | Compound (and top-level) children: rows of 'wrapAt', left-aligned,
-- tops aligned within a row.
placeGrid :: RChart -> [NodeName] -> (Int, Int) -> Map NodeName Box
placeGrid c names (x0, y0) = snd (foldl' row (y0, Map.empty) (chunksOf wrapAt names))
 where
  row (y, acc) ns =
    let rowH = maximum (atomH : map (snd . sizeOf c) ns)
        (_, acc') = foldl' (place y) (x0, acc) ns
     in (y + rowH + vGap, acc')
  place y (x, acc) name = (x + fst (sizeOf c name) + hGap, Map.union acc (placeNode c name (x, y)))

-- | Parallel regions: stacked vertically.
placeStack :: RChart -> [NodeName] -> (Int, Int) -> Map NodeName Box
placeStack c names (x0, y0) = snd (foldl' place (y0, Map.empty) names)
 where
  place (y, acc) name = (y + snd (sizeOf c name) + regionGap, Map.union acc (placeNode c name (x0, y)))

boxFor :: Map NodeName Box -> NodeName -> Box
boxFor boxes n = Map.findWithDefault (Box 0 0 0 0) n boxes

-- | The synthetic source box for root-level (fire from any state)
-- transitions, placed under everything else.
anyStateBox :: RChart -> Map NodeName Box -> Maybe Box
anyStateBox c boxes = case rcRootTransitions c of
  [] -> Nothing
  _ -> Just (Box (marginX + initInset) below (leafW anyStateLabel) atomH)
 where
  below = vGap + maximum (marginY : map (\b -> boxY b + boxH b) (Map.elems boxes))

anyStateLabel :: Text
anyStateLabel = "any state"

{-------------------------------------------------------------------------------
  Transitions and arrows
-------------------------------------------------------------------------------}

-- | One transitions-table row: display source, source key ('Nothing' =
-- the synthetic root), and the transition.
data Row = Row
  { rowSource :: Maybe NodeName
  , rowTrans :: RTrans
  }

transitionRows :: RChart -> [Row]
transitionRows c =
  concatMap ofNode (sortOn rnOrder (Map.elems (rcNodes c)))
    ++ map (Row Nothing) (rcRootTransitions c)
 where
  ofNode node = map (Row (Just (rnName node))) (rnTransitions node)

-- | A drawable arrow: id (for table-row hover), endpoint boxes, label.
data Arrow = Arrow
  { aId :: Text
  , aSrcKey :: Text
  , aTgtKey :: Text
  , aSrcBox :: Box
  , aTgtBox :: Box
  , aLabel :: Text
  , aLoop :: Bool
  , aDashed :: Bool
  }

rootKey :: Text
rootKey = "\0root"

{- | Arrows for every table row (one per target; a self-loop for
targetless transitions and self-targets) plus dashed @default@ arrows for
history pseudo-states. Returns the edge-element ids of each row alongside,
index-aligned with 'transitionRows'.
-}
buildArrows :: RChart -> Map NodeName Box -> Maybe Box -> [Row] -> ([Arrow], [[Text]])
buildArrows c boxes anyBox rows = (concat perRow ++ historyArrows, map (map aId) perRow)
 where
  perRow = zipWith rowArrows [0 :: Int ..] rows
  rowArrows k row =
    let (srcKey, srcBox) = source (rowSource row)
        tr = rowTrans row
        mk j tgt =
          Arrow
            { aId = "e" <> tshow k <> "_" <> tshow (j :: Int)
            , aSrcKey = srcKey
            , aTgtKey = fromMaybe srcKey tgt
            , aSrcBox = srcBox
            , aTgtBox = maybe srcBox (boxFor boxes) tgt
            , aLabel = transLabel tr
            , aLoop = maybe True (== srcKey) tgt
            , aDashed = False
            }
     in case rtTargets tr of
          [] -> [mk 0 Nothing]
          targets -> zipWith (\j tgt -> mk j (Just tgt)) [0 ..] targets
  source = \case
    Nothing -> (rootKey, fromMaybe (Box 0 0 0 0) anyBox)
    Just n -> (n, boxFor boxes n)
  historyArrows = zipWith historyArrow [0 :: Int ..] (mapMaybe defaulted (Map.elems (rcNodes c)))
  defaulted node = case rnKind node of
    RHistory _ (Just def) -> Just (rnName node, def)
    _ -> Nothing
  historyArrow k (h, def) =
    Arrow
      { aId = "h" <> tshow k
      , aSrcKey = h
      , aTgtKey = def
      , aSrcBox = boxFor boxes h
      , aTgtBox = boxFor boxes def
      , aLabel = "default"
      , aLoop = h == def
      , aDashed = True
      }

-- | @EVENT [guard] \/ act1,act2@ — the trigger part is shared with the
-- transitions table.
triggerLabel :: RTrigger -> Text
triggerLabel = \case
  TOn e -> e
  TWildcard -> "*"
  TAlways -> "always"
  TAfter ms -> "after " <> tshow ms <> "ms"
  TDone s -> "done(" <> s <> ")"
  TInvokeDone i -> "invoke.done(" <> i <> ")"
  TInvokeError i -> "invoke.error(" <> i <> ")"

transLabel :: RTrans -> Text
transLabel tr = triggerLabel (rtTrigger tr) <> guard <> actions
 where
  guard = maybe "" (\g -> " [" <> g <> "]") (rtGuard tr)
  actions = case rtActions tr of
    [] -> ""
    as -> " / " <> T.intercalate "," as

{-------------------------------------------------------------------------------
  SVG: nodes
-------------------------------------------------------------------------------}

svgSection ::
  RChart ->
  Maybe (Set NodeName) ->
  Map NodeName Box ->
  Maybe Box ->
  [Arrow] ->
  [Text]
svgSection c mActive boxes anyBox arrows =
  concat
    [ [ "<svg width=\"" <> tshow svgW <> "\" height=\"" <> tshow svgH <> "\""
      , "     viewBox=\"0 0 " <> tshow svgW <> " " <> tshow svgH <> "\" role=\"img\">"
      , "  <defs><marker id=\"arrow\" viewBox=\"0 0 10 10\" refX=\"9\" refY=\"5\""
      , "    markerWidth=\"7\" markerHeight=\"7\" orient=\"auto-start-reverse\">"
      , "    <path d=\"M0,0 L10,5 L0,10 z\" fill=\"#37474f\"/></marker></defs>"
      ]
    , concatMap (nodeSvg c mActive boxes) (sortOn rnOrder (Map.elems (rcNodes c)))
    , anyStateSvg
    , initialMarkers c boxes
    , arrowsSvg leafBoxes arrows
    , ["</svg>"]
    ]
 where
  allBoxes = Map.elems boxes ++ maybe [] pure anyBox
  svgW = marginX + maximum (0 : map (\b -> boxX b + boxW b) allBoxes)
  svgH = marginY + maximum (0 : map (\b -> boxY b + boxH b) allBoxes)
  leafBoxes =
    maybe [] pure anyBox
      ++ mapMaybe
        (\node -> if leafKind (rnKind node) then Map.lookup (rnName node) boxes else Nothing)
        (Map.elems (rcNodes c))
  leafKind = \case
    RAtomic -> True
    RFinal -> True
    RHistory _ _ -> True
    _ -> False
  anyStateSvg = case anyBox of
    Nothing -> []
    Just b ->
      [ "  <g class=\"node atomic\"><title>any state</title>"
      , "    " <> rect b 8 "body dashed"
      , "    " <> centeredText b (escapeHtml anyStateLabel)
      , "  </g>"
      ]

nodeSvg :: RChart -> Maybe (Set NodeName) -> Map NodeName Box -> RNode -> [Text]
nodeSvg c mActive boxes node = case rnKind node of
  RAtomic ->
    group "atomic" [rect b 8 "body", centeredText b label]
  RFinal ->
    group
      "final"
      [rect b 8 "body", rect (inset 3 b) 5 "inner", centeredText b label]
  RHistory kind _ ->
    group
      "history"
      [ "<circle class=\"body\" cx=\""
          <> tshow hx
          <> "\" cy=\""
          <> tshow hy
          <> "\" r=\"15\"/>"
      , "<text class=\"label\" text-anchor=\"middle\" x=\""
          <> tshow hx
          <> "\" y=\""
          <> tshow (hy + 4)
          <> "\">"
          <> historyGlyph kind
          <> "</text>"
      , "<text class=\"small\" text-anchor=\"middle\" x=\""
          <> tshow hx
          <> "\" y=\""
          <> tshow (boxY b + histD + 12)
          <> "\">"
          <> label
          <> "</text>"
      ]
  RCompound _ ->
    group "compound" (rect b 10 "container" : titleText)
  RParallel ->
    group "parallel" (rect b 10 "container" : titleText ++ separators)
 where
  n = rnName node
  b = boxFor boxes n
  label = escapeHtml n
  active = maybe False (Set.member n) mActive
  group cls body =
    concat
      [ ["  <g class=\"node " <> cls <> (if active then " active" else "") <> "\">"]
      , ["    <title>" <> label <> "</title>"]
      , map ("    " <>) body
      , ["  </g>"]
      ]
  titleText =
    [ "<text class=\"title\" x=\""
        <> tshow (boxX b + 10)
        <> "\" y=\""
        <> tshow (boxY b + 16)
        <> "\">"
        <> label
        <> "</text>"
    ]
  hx = boxX b + boxW b `div` 2
  hy = boxY b + histD `div` 2
  separators = mapMaybe separator (drop 1 (childrenOf c n))
  separator kid = case Map.lookup kid boxes of
    Nothing -> Nothing
    Just kb ->
      let y = boxY kb - regionGap `div` 2
       in Just
            ( "<line class=\"sep\" x1=\""
                <> tshow (boxX b + 8)
                <> "\" y1=\""
                <> tshow y
                <> "\" x2=\""
                <> tshow (boxX b + boxW b - 8)
                <> "\" y2=\""
                <> tshow y
                <> "\"/>"
            )

historyGlyph :: HistoryKind -> Text
historyGlyph = \case
  Shallow -> "H"
  Deep -> "H*"

rect :: Box -> Int -> Text -> Text
rect b r cls =
  "<rect class=\""
    <> cls
    <> "\" x=\""
    <> tshow (boxX b)
    <> "\" y=\""
    <> tshow (boxY b)
    <> "\" width=\""
    <> tshow (boxW b)
    <> "\" height=\""
    <> tshow (boxH b)
    <> "\" rx=\""
    <> tshow r
    <> "\"/>"

inset :: Int -> Box -> Box
inset d (Box x y w h) = Box (x + d) (y + d) (w - 2 * d) (h - 2 * d)

centeredText :: Box -> Text -> Text
centeredText b label =
  "<text class=\"label\" text-anchor=\"middle\" x=\""
    <> tshow (boxX b + boxW b `div` 2)
    <> "\" y=\""
    <> tshow (boxY b + boxH b `div` 2 + 4)
    <> "\">"
    <> label
    <> "</text>"

-- | Filled dot → arrow into the initial child, for the top level and
-- every compound state.
initialMarkers :: RChart -> Map NodeName Box -> [Text]
initialMarkers c boxes = concatMap marker (rcInitial c : compoundInitials)
 where
  compoundInitials = mapMaybe initialOf (Map.elems (rcNodes c))
  initialOf node = case rnKind node of
    RCompound ini -> Just ini
    _ -> Nothing
  marker ini = case Map.lookup ini boxes of
    Nothing -> []
    Just b ->
      let dy = boxY b + 14
          dx = boxX b - 16
       in [ "  <circle class=\"init\" cx=\"" <> tshow dx <> "\" cy=\"" <> tshow dy <> "\" r=\"4\"/>"
          , "  <path class=\"init\" d=\"M"
              <> tshow (dx + 4)
              <> ","
              <> tshow dy
              <> " L"
              <> tshow (boxX b - 1)
              <> ","
              <> tshow dy
              <> "\" marker-end=\"url(#arrow)\"/>"
          ]

{-------------------------------------------------------------------------------
  SVG: arrows
-------------------------------------------------------------------------------}

arrowsSvg :: [Box] -> [Arrow] -> [Text]
arrowsSvg obstacles arrows = concat (snd (foldl' one (Map.empty, []) arrows))
 where
  totals = Map.fromListWith (+) (map (\a -> (pairOf a, 1 :: Int)) arrows)
  pairOf a = (aSrcKey a, aTgtKey a)
  one (seen, acc) a =
    let occ = Map.findWithDefault 0 (pairOf a) seen
        total = Map.findWithDefault 1 (pairOf a) totals
     in (Map.insertWith (+) (pairOf a) 1 seen, acc ++ [arrowSvg obstacles a occ total])

arrowSvg :: [Box] -> Arrow -> Int -> Int -> [Text]
arrowSvg obstacles a occ total =
  concat
    [ ["  <g class=\"edge\" id=\"" <> aId a <> "\">"]
    , ["    <path class=\"line" <> dashed <> "\" d=\"" <> path <> "\" marker-end=\"url(#arrow)\"/>"]
    , [ "    <text class=\"elabel\" text-anchor=\""
          <> anchor
          <> "\" x=\""
          <> tshow lx
          <> "\" y=\""
          <> tshow ly
          <> "\">"
          <> escapeHtml (aLabel a)
          <> "</text>"
      ]
    , ["  </g>"]
    ]
 where
  dashed = if aDashed a then " dashed" else ""
  (path, (lx, ly), anchor)
    | aLoop a = loopGeometry (aSrcBox a) occ
    | otherwise = lineGeometry obstacles (aSrcBox a) (aTgtBox a) (offsetFor occ total)

-- | Symmetric fan-out for several arrows between the same ordered pair.
offsetFor :: Int -> Int -> Int
offsetFor occ total = (occ * 2 - (total - 1)) * 8 `div` 2

{- | Route one arrow between two boxes; returns the path, the label
position, and the label's @text-anchor@.

Near-horizontal edges whose straight line would cut through some other
leaf box detour below the row instead (an elbow: down, across the
corridor, up into the target's bottom edge). Everything else is a
straight border-to-border line along the dominant axis, endpoints
shifted perpendicular by the fan offset plus a direction-dependent lane
bias (so A→B and B→A never share a segment). Labels sit above
rightward lines, below leftward ones, and beside vertical ones; on long
diagonals they sit near the source (the midpoint is usually on top of
somebody else's box).
-}
lineGeometry :: [Box] -> Box -> Box -> Int -> (Text, (Int, Int), Text)
lineGeometry obstacles b1 b2 fan
  | horizontal && blocked = elbow
  | horizontal =
      let y1 = c1y + hShift
          y2 = c2y + hShift
          -- Reverse (leftward) near-horizontal edges label below their
          -- line so A→B and B→A labels take different rows; diagonals
          -- have the room to stay above.
          labelDy = if dx >= 0 || abs dy > 60 then -8 else 16
       in ( "M" <> point sx y1 <> " L" <> point tx y2
          , (at sx tx, at y1 y2 + labelDy)
          , "middle"
          )
  | otherwise =
      let (sy, ty) = if dy >= 0 then (boxY b1 + boxH b1, boxY b2) else (boxY b1, boxY b2 + boxH b2)
          vShift = fan + (if dy >= 0 then -6 else 6)
          x1 = c1x + vShift
          x2 = c2x + vShift
          -- Label to the right of the line (the left neighbour is
          -- usually the diagram edge or another box); upward labels sit
          -- near the target, clear of the elbow corridor below the row.
          lt = if dy >= 0 then 42 else 70
          alongV u v = u + ((v - u) * lt) `div` 100
       in ( "M" <> point x1 sy <> " L" <> point x2 ty
          , (alongV x1 x2 + 8, alongV sy ty)
          , "start"
          )
 where
  c1x = boxX b1 + boxW b1 `div` 2
  c1y = boxY b1 + boxH b1 `div` 2
  c2x = boxX b2 + boxW b2 `div` 2
  c2y = boxY b2 + boxH b2 `div` 2
  dx = c2x - c1x
  dy = c2y - c1y
  horizontal = abs dx >= abs dy
  (sx, tx) = if dx >= 0 then (boxX b1 + boxW b1, boxX b2) else (boxX b1, boxX b2 + boxW b2)
  hShift = fan + (if dx >= 0 then -6 else 6)
  -- Label position along the run: near the source on long edges.
  at u v = u + ((v - u) * labelT) `div` 100
   where
    labelT
      | abs dx + abs dy > 340 = 22
      | otherwise = 42
  laneY = (c1y + c2y) `div` 2 + hShift
  blocked = abs dy <= 60 && any hit obstacles
  hit b =
    b /= b1
      && b /= b2
      && laneY > boxY b
      && laneY < boxY b + boxH b
      && min sx tx < boxX b + boxW b
      && max sx tx > boxX b
  elbow =
    let xShift = fan + (if dx >= 0 then 8 else -8)
        x1 = c1x + xShift
        x2 = c2x + xShift
        bot1 = boxY b1 + boxH b1
        bot2 = boxY b2 + boxH b2
        -- Opposite-direction elbows between the same boxes take
        -- different depths, and each labels near its own source.
        yMid = max bot1 bot2 + 22 + fan + (if dx >= 0 then 0 else 16)
     in ( "M"
            <> point x1 bot1
            <> " L"
            <> point x1 yMid
            <> " L"
            <> point x2 yMid
            <> " L"
            <> point x2 bot2
        , (at x1 x2, yMid - 6)
        , "middle"
        )
  point x y = tshow x <> "," <> tshow y

-- | A cubic arc over the top edge; stacked loops rise.
loopGeometry :: Box -> Int -> (Text, (Int, Int), Text)
loopGeometry b occ =
  ( "M"
      <> tshow sx
      <> ","
      <> tshow yTopEdge
      <> " C"
      <> tshow (sx + 30)
      <> ","
      <> tshow top
      <> " "
      <> tshow (ex - 30)
      <> ","
      <> tshow top
      <> " "
      <> tshow ex
      <> ","
      <> tshow yTopEdge
  , ((sx + ex) `div` 2, apexY - 6)
  , "middle"
  )
 where
  yTopEdge = boxY b
  sx = boxX b + boxW b `div` 2 + 14
  ex = max (boxX b + 4) (sx - 28)
  top = yTopEdge - 26 - occ * 18
  apexY = (yTopEdge + 3 * top) `div` 4

{-------------------------------------------------------------------------------
  HTML: page skeleton
-------------------------------------------------------------------------------}

prologue :: RChart -> [Text]
prologue c =
  [ "<!DOCTYPE html>"
  , "<html lang=\"en\">"
  , "<head>"
  , "<meta charset=\"utf-8\">"
  , "<title>" <> escapeHtml (rcName c) <> "</title>"
  , "<style>" <> css <> "</style>"
  , "</head>"
  , "<body>"
  ]

epilogue :: [Text]
epilogue =
  [ "<script>" <> js <> "</script>"
  , "</body>"
  , "</html>"
  ]

headerSection :: RChart -> Maybe (Set NodeName) -> [Row] -> [Text]
headerSection c mActive rows =
  concat
    [ ["<h1>" <> escapeHtml (rcName c) <> "</h1>"]
    , [ "<p class=\"meta\">"
          <> tshow (Map.size (rcNodes c))
          <> " states &middot; "
          <> tshow (length rows)
          <> " transitions &middot; "
          <> tshow (length (rcEvents c))
          <> " events</p>"
      ]
    , activeLine
    ]
 where
  activeLine = case mActive of
    Nothing -> []
    Just active ->
      [ "<p class=\"meta\">active: <strong>"
          <> escapeHtml (T.intercalate ", " (Set.toList active))
          <> "</strong></p>"
      ]

{-------------------------------------------------------------------------------
  HTML: tables
-------------------------------------------------------------------------------}

transitionsSection :: [Row] -> [[Text]] -> [Text]
transitionsSection rows rowEdgeIds =
  concat
    [ ["<h2>Transitions</h2>", "<table>", "<thead><tr>"]
    , [T.concat (map th ["source", "trigger", "guard", "targets", "actions", "internal"])]
    , ["</tr></thead>", "<tbody>"]
    , zipWith rowHtml rows rowEdgeIds
    , ["</tbody>", "</table>"]
    ]
 where
  th h = "<th>" <> h <> "</th>"
  rowHtml row edgeIds =
    "<tr data-edge=\""
      <> T.unwords edgeIds
      <> "\">"
      <> T.concat
        (map
           td
           [ maybe "(root)" escapeHtml (rowSource row)
           , escapeHtml (triggerLabel (rtTrigger tr))
           , maybe dash escapeHtml (rtGuard tr)
           , listCell (rtTargets tr)
           , listCell (rtActions tr)
           , if rtInternal tr then "yes" else "no"
           ])
      <> "</tr>"
   where
    tr = rowTrans row
  td cell = "<td>" <> cell <> "</td>"
  listCell = \case
    [] -> dash
    xs -> escapeHtml (T.intercalate ", " xs)
  dash = "&mdash;"

traceSection :: [MicroTrace] -> [Text]
traceSection = \case
  [] -> []
  traces ->
    concat
      [ ["<h2>Trace</h2>", "<table>", "<thead><tr>"]
      , [T.concat (map th ["#", "event", "selected", "exited", "entered", "actions"])]
      , ["</tr></thead>", "<tbody>"]
      , zipWith rowHtml [0 :: Int ..] traces
      , ["</tbody>", "</table>"]
      ]
 where
  th h = "<th>" <> h <> "</th>"
  rowHtml k mt =
    "<tr>"
      <> T.concat
        (map
           td
           [ tshow k
           , "<code>" <> escapeHtml (jsonText (mtEvent mt)) <> "</code>"
           , listCell (map selectedText (mtSelected mt))
           , listCell (mtExited mt)
           , listCell (mtEntered mt)
           , listCell (mtActions mt)
           ])
      <> "</tr>"
  selectedText (src, ix) = src <> "#" <> tshow ix
  td cell = "<td>" <> cell <> "</td>"
  listCell = \case
    [] -> "&mdash;"
    xs -> escapeHtml (T.intercalate ", " xs)
  jsonText = TE.decodeUtf8 . BL.toStrict . encode

sourcesSection :: RChart -> Maybe (Set NodeName) -> [Text]
sourcesSection c mActive =
  [ "<details><summary>Mermaid source</summary>"
  , "<pre>" <> escapeHtml (maybe mermaid mermaidHighlight mActive c) <> "</pre>"
  , "</details>"
  , "<details><summary>XState JSON</summary>"
  , "<pre>" <> escapeHtml (xstateConfigText c) <> "</pre>"
  , "</details>"
  ]

{-------------------------------------------------------------------------------
  CSS and JS
-------------------------------------------------------------------------------}

css :: Text
css =
  T.intercalate
    "\n"
    [ "body { font-family: system-ui, -apple-system, sans-serif; margin: 24px; color: #263238; }"
    , "h1 { margin-bottom: 2px; } h2 { margin-top: 28px; }"
    , ".meta { color: #607d8b; margin-top: 2px; }"
    , "svg { border: 1px solid #eceff1; border-radius: 8px; background: #fdfefe; }"
    , "svg text { font: 12px ui-monospace, SFMono-Regular, Menlo, monospace; fill: #263238; }"
    , "svg text.title { font-weight: 600; fill: #455a64; }"
    , "svg text.small { font-size: 10px; fill: #607d8b; }"
    , "svg rect.body { fill: #ffffff; stroke: #546e7a; stroke-width: 1.2; }"
    , "svg rect.body.dashed { stroke-dasharray: 5 3; fill: #fafafa; }"
    , "svg rect.inner { fill: none; stroke: #546e7a; stroke-width: 1.2; }"
    , "svg rect.container { fill: #f4f8fa; stroke: #90a4ae; stroke-width: 1.2; }"
    , "svg circle.body { fill: #ffffff; stroke: #546e7a; stroke-width: 1.2; }"
    , "svg .node.active > rect.body, svg .node.active > circle.body { fill: #ffd54f; }"
    , "svg .node.active > rect.container { fill: #fff3d1; }"
    , "svg line.sep { stroke: #90a4ae; stroke-dasharray: 6 4; }"
    , "svg circle.init { fill: #263238; }"
    , "svg path.init { stroke: #263238; stroke-width: 1.4; fill: none; }"
    , "svg .edge path.line { stroke: #37474f; stroke-width: 1.3; fill: none; }"
    , "svg .edge path.line.dashed { stroke-dasharray: 5 3; }"
    , "svg .edge text.elabel { font-size: 11px; fill: #37474f;"
    , "  paint-order: stroke; stroke: #fdfefe; stroke-width: 3px; }"
    , "svg .edge.hl path.line { stroke: #d32f2f; stroke-width: 2.4; }"
    , "svg .edge.hl text.elabel { fill: #d32f2f; }"
    , "table { border-collapse: collapse; margin-top: 8px; }"
    , "th, td { border: 1px solid #cfd8dc; padding: 4px 10px; font-size: 13px; text-align: left; }"
    , "th { background: #eceff1; }"
    , "tr[data-edge]:hover { background: #fff3d1; }"
    , "details { margin-top: 16px; }"
    , "summary { cursor: pointer; color: #455a64; font-weight: 600; }"
    , "pre { background: #f4f8fa; border: 1px solid #eceff1; border-radius: 6px;"
    , "  padding: 12px; overflow-x: auto; font-size: 12px; }"
    , "code { font-size: 12px; }"
    ]

-- | Hovering a transitions-table row highlights its arrows. Cosmetic
-- only: the page is complete without it.
js :: Text
js =
  T.intercalate
    "\n"
    [ "(function () {"
    , "  \"use strict\";"
    , "  document.querySelectorAll(\"tr[data-edge]\").forEach(function (row) {"
    , "    var ids = row.getAttribute(\"data-edge\").split(\" \").filter(Boolean);"
    , "    function toggle(on) {"
    , "      return function () {"
    , "        ids.forEach(function (id) {"
    , "          var e = document.getElementById(id);"
    , "          if (e) { e.classList.toggle(\"hl\", on); }"
    , "        });"
    , "      };"
    , "    }"
    , "    row.addEventListener(\"mouseenter\", toggle(true));"
    , "    row.addEventListener(\"mouseleave\", toggle(false));"
    , "  });"
    , "})();"
    ]

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

-- | Minimal, total HTML escaping for text and attribute positions.
escapeHtml :: Text -> Text
escapeHtml = T.concatMap $ \case
  '&' -> "&amp;"
  '<' -> "&lt;"
  '>' -> "&gt;"
  '"' -> "&quot;"
  '\'' -> "&#39;"
  ch -> T.singleton ch

tshow :: Show a => a -> Text
tshow = T.pack . show

chunksOf :: Int -> [a] -> [[a]]
chunksOf k = go
 where
  go [] = []
  go xs =
    let (chunk, rest) = splitAt k xs
     in chunk : go rest
