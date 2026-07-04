---
title: Visualization
description: "Render any chart to Stately config, Mermaid stateDiagram-v2, Graphviz DOT, or a self-contained HTML page with an SVG diagram and trace timeline — with the live configuration highlightable."
sidebar:
  order: 7
  label: Visualization
---

Every renderer consumes the runtime chart (`ciChart impl :: RChart`). Labels are
key wire names: constructor spellings, or the names supplied to
`deriveKeyKindWith`. The renderers target editor import, documentation,
graphing, and standalone inspection, and each can highlight a live
configuration.

## Stately config

`StateMachine.Render.XState` emits a machine config for the
[Stately editor](https://stately.ai):

```haskell
import StateMachine.Render.XState (xstateConfig, xstateConfigText)

xstateConfig     :: RChart -> Data.Aeson.Value
xstateConfigText :: RChart -> Text            -- pretty-printed, 2-space

putStrLn (T.unpack (xstateConfigText (ciChart impl)))
```

Compound states become `{ initial, states }`, parallel states `type:
"parallel"`, finals `type: "final"`, history `type: "history"`; transitions
group by trigger under `on` / `always` / `after` / `invoke`. Targets are
emitted as absolute `#id` references; cross-level edges (root handlers,
history rebinds) import into a real `createMachine` without error. The output
is verified against the editor runtime.

## Mermaid

`StateMachine.Render.Mermaid` produces a `stateDiagram-v2` for READMEs and docs
sites:

```haskell
mermaid          :: RChart -> Text
mermaidHighlight :: Set NodeName -> RChart -> Text  -- highlight a live config
```

The traffic-light demo chart, rendered by the library:

```mermaid
stateDiagram-v2
  [*] --> Operational
  state "Operational" as Operational
  state Operational {
    [*] --> Green
    state "Green" as Green
    state "Yellow" as Yellow
    state "Red" as Red
    state "Walk" as Walk
    state "H" as OpHist
  }
  state "Flashing" as Flashing
  state "Off" as Off
  Off --> [*]
  OpHist : OpHist
  Operational --> Flashing : POWER_OUT
  Green --> Yellow : TIMER
  Green --> Green : PUSH / NotePedestrian
  Yellow --> Red : TIMER / CountCycle
  Red --> Green : TIMER [NoPedestrian]
  Red --> Walk : TIMER
  Walk --> Green : TIMER / ClearPedestrian
  Flashing --> OpHist : FIXED
  state "any state" as __any
  __any --> Off : DECOMMISSION
```

History appears as an `H` node, finals get an edge to `[*]`, parallel regions
are separated by `--`, and root-level handlers originate from a synthetic
`any state` node. `mermaidHighlight config` adds a `classDef active` and marks
the states in `config` — pass `config machine` to show where a live machine is.

## Graphviz DOT

`StateMachine.Render.Dot` renders a clustered digraph (compound/parallel states
as `subgraph cluster_*`, parallel regions dashed):

```haskell
dot          :: RChart -> Text
dotHighlight :: Set NodeName -> RChart -> Text

writeFile "chart.dot" (T.unpack (dot (ciChart impl)))
-- dot -Tsvg chart.dot -o chart.svg
```

## Self-contained HTML

`StateMachine.Render.Html` writes a single, offline HTML page — an SVG diagram
computed in Haskell (no layout engine, no CDN), a transitions table, and, when
you pass a step trace, a timeline of what fired:

```haskell
htmlPage       :: RChart -> Maybe (Set NodeName) -> [MicroTrace] -> Text
htmlPageSimple :: RChart -> Text

-- highlight the live config and show the last step's trace:
let page = htmlPage (ciChart impl) (Just (config machine)) (sTrace stepped)
T.writeFile "chart.html" page
```

The active-configuration argument fills the current states distinctly; the
`[MicroTrace]` (from a `step`'s `sTrace` or a simulation's `simTrace`) renders
the exit/entry/action timeline. The page works with JavaScript disabled — JS
only adds row-to-edge hover highlighting.

## Wiring it into a workflow

Because everything renders from `ciChart impl`, a practical pattern is a tiny exe or
test that regenerates the diagrams from the source of truth:

```haskell
main :: IO ()
main = do
  T.writeFile "docs/chart.mmd"  (mermaid (ciChart impl))
  BL.writeFile "docs/chart.xstate" (encode (xstateConfig (ciChart impl)))
```

The `example-traffic` demo prints Mermaid and Stately config output and writes
an HTML page highlighting the machine's live configuration after a step — run
`cabal run example-traffic` to see all four in action.

Back to the [overview](../), or the
[catalogue entry](../../packages/state-machine/).
