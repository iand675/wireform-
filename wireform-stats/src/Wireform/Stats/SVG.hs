{- | SVG bar chart renderer for wireform-stats.

Produces light and dark variants of the same chart from one input,
using a GitHub-flavoured palette (Primer color tokens) so the
output looks at home in a README rendered on github.com.

We dogfood [wireform-xml](../../wireform-xml/) for the SVG
construction: the chart is built as 'XML.Value.Document' and
emitted via 'XML.Encode.encodePretty'. Hand-rolling string
concatenation would be smaller, but this validates that
wireform-xml is comfortable to build with and gives us schema-aware
escaping for free.
-}
module Wireform.Stats.SVG (
  -- * Chart spec
  BarChart (..),
  Series (..),
  GroupLabel,
  defaultGitHubBarChart,

  -- * Themes
  Theme (..),
  lightTheme,
  darkTheme,

  -- * Render
  renderBarChart,
  renderBarChartBoth,
  renderBarChartAdaptive,
) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Numeric (showFFloat)
import XML.Encode qualified as XE
import XML.Value (
  Attribute (..),
  Document (..),
  Node (..),
  XMLDecl (..),
  simpleName,
 )


-- ---------------------------------------------------------------------------
-- Chart spec
-- ---------------------------------------------------------------------------

{- | A grouped bar chart: one bar per (Series, GroupLabel) pair.

The same 'BarChart' renders to both a light and dark SVG via the
'Theme' parameter; pick a theme for one-off rendering or use
'renderBarChartBoth' to get both at once.
-}
data BarChart = BarChart
  { chartTitle :: !Text
  -- ^ Chart title rendered above the plot area.
  , chartSubtitle :: !(Maybe Text)
  -- ^ Optional subtitle rendered below the title (e.g. timestamp,
  --   toolchain).
  , chartUnit :: !Text
  -- ^ Unit string for the y-axis (e.g. @"ns"@, @"µs"@, @"MB/s"@).
  , chartGroups :: ![GroupLabel]
  -- ^ Group labels along the x-axis. Order is preserved.
  , chartSeries :: ![Series]
  -- ^ Series, one per legend entry. Each 'Series' carries one
  --   value per 'GroupLabel'; mismatched lengths are clipped to the
  --   shorter side and padded with @0@ otherwise.
  , chartHigherIsBetter :: !Bool
  -- ^ Used for the subtitle hint ("higher is better" /
  --   "lower is better"). Doesn't affect rendering otherwise.
  }
  deriving stock (Eq, Show)


-- | A named series of values, one per group.
data Series = Series
  { seriesName :: !Text
  , seriesValues :: ![Double]
  }
  deriving stock (Eq, Show)


type GroupLabel = Text


{- | Default empty chart with title + unit set. Useful as a starting
point with record updates.
-}
defaultGitHubBarChart :: Text -> Text -> BarChart
defaultGitHubBarChart titleT unitT =
  BarChart
    { chartTitle = titleT
    , chartSubtitle = Nothing
    , chartUnit = unitT
    , chartGroups = []
    , chartSeries = []
    , chartHigherIsBetter = False
    }


-- ---------------------------------------------------------------------------
-- Themes
-- ---------------------------------------------------------------------------

{- | Color palette for one render. Six entries cover almost every
chart we want to render in a README; if you need more series than
that, the renderer cycles modulo the palette.
-}
data Theme = Theme
  { themeBackground :: !Text
  , themeForeground :: !Text
  , themeMuted :: !Text
  , themeGrid :: !Text
  , themePalette :: ![Text]
  }
  deriving stock (Eq, Show)


{- | Light theme using GitHub Primer color tokens (the same palette
you see on github.com in light mode).
-}
lightTheme :: Theme
lightTheme =
  Theme
    { themeBackground = "#ffffff"
    , themeForeground = "#1f2328"
    , themeMuted = "#656d76"
    , themeGrid = "#d0d7de"
    , themePalette =
        [ "#0969da" -- accent.blue
        , "#cf222e" -- accent.red
        , "#1a7f37" -- accent.green
        , "#bf8700" -- accent.yellow
        , "#8250df" -- accent.purple
        , "#bc4c00" -- accent.orange
        ]
    }


-- | Dark theme using GitHub Primer dark-mode color tokens.
darkTheme :: Theme
darkTheme =
  Theme
    { themeBackground = "#0d1117"
    , themeForeground = "#e6edf3"
    , themeMuted = "#7d8590"
    , themeGrid = "#30363d"
    , themePalette =
        [ "#58a6ff" -- accent.blue
        , "#ff7b72" -- accent.red
        , "#3fb950" -- accent.green
        , "#d29922" -- accent.yellow
        , "#bc8cff" -- accent.purple
        , "#ffa657" -- accent.orange
        ]
    }


-- ---------------------------------------------------------------------------
-- Render
-- ---------------------------------------------------------------------------

-- | Render a chart to a single SVG. The caller picks the theme.
renderBarChart :: Theme -> BarChart -> ByteString
renderBarChart theme chart =
  XE.encodePretty 2 (Document (Just decl) (svgRoot theme chart))
  where
    decl = XMLDecl "1.0" (Just "UTF-8") Nothing


{- | Render a chart to both light and dark variants in one shot.
Returns @(lightSvg, darkSvg)@.
-}
renderBarChartBoth :: BarChart -> (ByteString, ByteString)
renderBarChartBoth chart =
  ( renderBarChart lightTheme chart
  , renderBarChart darkTheme chart
  )


{- | Render a chart to a single, self-contained SVG that adapts to the
reader's @prefers-color-scheme@ at view time.

The @\<picture\>@ / two-file trick the READMEs use works on
github.com but not when the SVG is inlined into a plain-markdown
Starlight page (raw @\<img src\>@ would need a base-aware URL into
@public/@, which Astro does not rewrite). Instead we stack a full
light chart and a full dark chart inside one @\<svg\>@ and toggle
visibility with an embedded @\<style\>@ media query, so the same
bytes can be dropped straight into the page with no asset wiring.

Emitted without an XML declaration so it inlines cleanly inside
HTML/markdown.
-}
renderBarChartAdaptive :: BarChart -> ByteString
renderBarChartAdaptive chart =
  XE.encodePretty 2 (Document Nothing (adaptiveRoot chart))


adaptiveRoot :: BarChart -> Node
adaptiveRoot chart =
  el
    "svg"
    [ ("xmlns", "http://www.w3.org/2000/svg")
    , ("viewBox", "0 0 " <> tshow' canvasW <> " " <> tshow' canvasH)
    , ("width", tshow' canvasW)
    , ("height", tshow' canvasH)
    , ("role", "img")
    , ("font-family", "ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Helvetica, Arial, sans-serif")
    , ("font-size", "12")
    ]
    [ el "title" [] [text_ (chartTitle chart)]
    , el "style" [] [text_ adaptiveStyle]
    , el "g" [("class", "wf-light")] (svgChildren lightTheme chart)
    , el "g" [("class", "wf-dark")] (svgChildren darkTheme chart)
    ]


{- | Show the light layer by default; swap to the dark layer when the
reader prefers a dark color scheme.
-}
adaptiveStyle :: Text
adaptiveStyle =
  ".wf-dark{display:none}"
    <> "@media (prefers-color-scheme:dark){"
    <> ".wf-light{display:none}.wf-dark{display:inline}}"


-- ---------------------------------------------------------------------------
-- Layout
-- ---------------------------------------------------------------------------

-- Chart canvas is fixed at 720 x 400 with a margin block so the
-- plot area sits at (80, 60) -> (700, 320). Bar widths derive from
-- group count and series count. The 80px band below the plot area
-- is split into two rows: the x-axis group labels sit just under
-- the axis, and the legend gets its own row near the bottom edge so
-- the two never collide (even with five groups and long series
-- names).

canvasW, canvasH :: Double
canvasW = 720
canvasH = 400


plotL, plotT, plotR, plotB :: Double
plotL = 80
plotT = 60
plotR = 700
plotB = 320


plotW, plotH :: Double
plotW = plotR - plotL
plotH = plotB - plotT


-- ---------------------------------------------------------------------------
-- SVG construction
-- ---------------------------------------------------------------------------

svgRoot :: Theme -> BarChart -> Node
svgRoot theme chart =
  el
    "svg"
    [ ("xmlns", "http://www.w3.org/2000/svg")
    , ("viewBox", "0 0 " <> tshow' canvasW <> " " <> tshow' canvasH)
    , ("width", tshow' canvasW)
    , ("height", tshow' canvasH)
    , ("font-family", "ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, \"Segoe UI\", Helvetica, Arial, sans-serif")
    , ("font-size", "12")
    ]
    (svgChildren theme chart)


{- | The ordered drawing layers for one themed chart, shared by the
single-theme 'svgRoot' and the stacked 'adaptiveRoot'.
-}
svgChildren :: Theme -> BarChart -> [Node]
svgChildren theme chart =
  [ background theme
  , title theme chart
  , subtitle theme chart
  , axes theme chart
  , yTicks theme chart
  , bars theme chart
  , barLabels theme chart
  , groupLabels theme chart
  , legend theme chart
  ]


-- | Element constructor with attribute pairs.
el :: Text -> [(Text, Text)] -> [Node] -> Node
el name attrs children =
  Element
    (simpleName name)
    (V.fromList (map (\(k, v) -> Attribute (simpleName k) v) attrs))
    (V.fromList children)


-- | Self-closing element.
el_ :: Text -> [(Text, Text)] -> Node
el_ name attrs = el name attrs []


text_ :: Text -> Node
text_ = Text


-- ---------------------------------------------------------------------------
-- Pieces
-- ---------------------------------------------------------------------------

background :: Theme -> Node
background theme =
  el_
    "rect"
    [ ("x", "0")
    , ("y", "0")
    , ("width", tshow' canvasW)
    , ("height", tshow' canvasH)
    , ("fill", themeBackground theme)
    ]


title :: Theme -> BarChart -> Node
title theme chart =
  el
    "text"
    [ ("x", tshow' (canvasW / 2))
    , ("y", "26")
    , ("text-anchor", "middle")
    , ("font-size", "15")
    , ("font-weight", "600")
    , ("fill", themeForeground theme)
    ]
    [text_ (chartTitle chart)]


subtitle :: Theme -> BarChart -> Node
subtitle theme chart =
  let direction = if chartHigherIsBetter chart then "higher is better" else "lower is better"
      pieces = [direction, chartUnit chart] ++ maybe [] (: []) (chartSubtitle chart)
      txt = T.intercalate " · " pieces
  in el
      "text"
      [ ("x", tshow' (canvasW / 2))
      , ("y", "44")
      , ("text-anchor", "middle")
      , ("font-size", "11")
      , ("fill", themeMuted theme)
      ]
      [text_ txt]


axes :: Theme -> BarChart -> Node
axes theme _ =
  el
    "g"
    [ ("stroke", themeGrid theme)
    , ("stroke-width", "1")
    ]
    [ el_
        "line"
        [ ("x1", tshow' plotL)
        , ("y1", tshow' plotB)
        , ("x2", tshow' plotR)
        , ("y2", tshow' plotB)
        ]
    , el_
        "line"
        [ ("x1", tshow' plotL)
        , ("y1", tshow' plotT)
        , ("x2", tshow' plotL)
        , ("y2", tshow' plotB)
        ]
    ]


-- Y-axis ticks at 0, 25%, 50%, 75%, 100% of the rounded-up max value.
yTicks :: Theme -> BarChart -> Node
yTicks theme chart =
  let yMax = niceMax (allValues chart)
      ticks = [0, 0.25, 0.5, 0.75, 1.0]
      tickAt p =
        let v = yMax * p
            y = plotB - p * plotH
        in [ -- gridline (skip the axis itself)
             if p > 0
              then
                el_
                  "line"
                  [ ("x1", tshow' plotL)
                  , ("y1", tshow' y)
                  , ("x2", tshow' plotR)
                  , ("y2", tshow' y)
                  , ("stroke", themeGrid theme)
                  , ("stroke-width", "1")
                  , ("stroke-dasharray", "2 3")
                  ]
              else el "g" [] []
           , -- label
             el
              "text"
              [ ("x", tshow' (plotL - 8))
              , ("y", tshow' (y + 4))
              , ("text-anchor", "end")
              , ("font-size", "10")
              , ("fill", themeMuted theme)
              ]
              [text_ (formatNumber v)]
           ]
  in el "g" [] (concatMap tickAt ticks)


bars :: Theme -> BarChart -> Node
bars theme chart =
  let yMax = niceMax (allValues chart)
      groups = chartGroups chart
      series = chartSeries chart
      nGroups = length groups
      nSeries = length series
      groupW = if nGroups == 0 then 0 else plotW / fromIntegral nGroups
      barW = if nSeries == 0 then 0 else min 64 (groupW * 0.8 / fromIntegral nSeries)
      gap = (groupW - barW * fromIntegral nSeries) / 2
      palette = themePalette theme
      barFor gi (si, s) =
        let v = nthOr 0 gi (seriesValues s)
            xLeft = plotL + fromIntegral gi * groupW + gap + fromIntegral si * barW
            barH = if yMax == 0 then 0 else (v / yMax) * plotH
            yTop = plotB - barH
            color = cycleNth palette si
        in el_
            "rect"
            [ ("x", tshow' xLeft)
            , ("y", tshow' yTop)
            , ("width", tshow' (max 0 (barW - 2)))
            , ("height", tshow' (max 0 barH))
            , ("rx", "2")
            , ("fill", color)
            ]
      everyBar = do
        gi <- [0 .. nGroups - 1]
        sIdx <- zip [0 :: Int ..] series
        pure (barFor gi sIdx)
  in el "g" [] everyBar


-- Numeric value annotations centered above each bar.
barLabels :: Theme -> BarChart -> Node
barLabels theme chart =
  let yMax = niceMax (allValues chart)
      groups = chartGroups chart
      series = chartSeries chart
      nGroups = length groups
      nSeries = length series
      groupW = if nGroups == 0 then 0 else plotW / fromIntegral nGroups
      barW = if nSeries == 0 then 0 else min 64 (groupW * 0.8 / fromIntegral nSeries)
      gap = (groupW - barW * fromIntegral nSeries) / 2
      labelFor gi (si, s) =
        let v = nthOr 0 gi (seriesValues s)
            xMid = plotL + fromIntegral gi * groupW + gap + (fromIntegral si + 0.5) * barW - 1
            barH = if yMax == 0 then 0 else (v / yMax) * plotH
            yTop = plotB - barH
        in el
            "text"
            [ ("x", tshow' xMid)
            , ("y", tshow' (yTop - 4))
            , ("text-anchor", "middle")
            , ("font-size", "10")
            , ("fill", themeForeground theme)
            ]
            [text_ (formatNumber v)]
      everyLabel = do
        gi <- [0 .. nGroups - 1]
        sIdx <- zip [0 :: Int ..] series
        pure (labelFor gi sIdx)
  in el "g" [] everyLabel


groupLabels :: Theme -> BarChart -> Node
groupLabels theme chart =
  let groups = chartGroups chart
      nGroups = length groups
      groupW = if nGroups == 0 then 0 else plotW / fromIntegral nGroups
      lab gi g =
        let xMid = plotL + (fromIntegral gi + 0.5) * groupW
        in el
            "text"
            [ ("x", tshow' xMid)
            , ("y", tshow' (plotB + 18))
            , ("text-anchor", "middle")
            , ("font-size", "11")
            , ("fill", themeForeground theme)
            ]
            [text_ g]
  in el "g" [] (zipWith lab [0 :: Int ..] groups)


legend :: Theme -> BarChart -> Node
legend theme chart =
  let series = chartSeries chart
      palette = themePalette theme
      swatchW = 12
      gapBetween = 16
      itemPx s = swatchW + 6 + fromIntegral (T.length (seriesName s)) * 7
      total = sum (map itemPx series) + fromIntegral (length series - 1) * gapBetween
      startX = canvasW / 2 - total / 2
      laidOut =
        scanl
          (+)
          startX
          (map (\s -> itemPx s + gapBetween) series)
      legendItem (si, (x, s)) =
        el
          "g"
          [("transform", "translate(" <> tshow' x <> ", " <> tshow' (canvasH - 18) <> ")")]
          [ el_
              "rect"
              [ ("x", "0")
              , ("y", "-9")
              , ("width", tshow' swatchW)
              , ("height", tshow' swatchW)
              , ("rx", "2")
              , ("fill", cycleNth palette si)
              ]
          , el
              "text"
              [ ("x", tshow' (swatchW + 6))
              , ("y", "1")
              , ("font-size", "11")
              , ("fill", themeForeground theme)
              ]
              [text_ (seriesName s)]
          ]
  in el "g" [] (map legendItem (zip [0 ..] (zip laidOut series)))


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

allValues :: BarChart -> [Double]
allValues chart = concatMap seriesValues (chartSeries chart)


-- Round up to a "nice" axis maximum (1, 2, 2.5, 5, 10 * 10^k).
niceMax :: [Double] -> Double
niceMax xs =
  let m = maximum (0 : xs)
  in if m <= 0
      then 1
      else
        let exp10 = 10 ** fromIntegral (floor (logBase 10 m) :: Int)
            frac = m / exp10
            nice
              | frac <= 1.0 = 1.0
              | frac <= 2.0 = 2.0
              | frac <= 2.5 = 2.5
              | frac <= 5.0 = 5.0
              | otherwise = 10.0
        in nice * exp10


formatNumber :: Double -> Text
formatNumber x
  | x == 0 = "0"
  | abs x >= 1000 = T.pack (showFFloat (Just 0) x "")
  | abs x >= 100 = T.pack (showFFloat (Just 0) x "")
  | abs x >= 10 = T.pack (showFFloat (Just 1) x "")
  | abs x >= 1 = T.pack (showFFloat (Just 2) x "")
  | otherwise = T.pack (showFFloat (Just 3) x "")


tshow' :: Double -> Text
tshow' x
  | x == fromIntegral (round x :: Int) = T.pack (show (round x :: Int))
  | otherwise = T.pack (showFFloat (Just 1) x "")


nthOr :: a -> Int -> [a] -> a
nthOr d _ [] = d
nthOr _ 0 (x : _) = x
nthOr d n (_ : xs) = nthOr d (n - 1) xs


cycleNth :: [a] -> Int -> a
cycleNth [] _ = error "Wireform.Stats.SVG.cycleNth: empty palette"
cycleNth xs n = xs !! (n `mod` length xs)
