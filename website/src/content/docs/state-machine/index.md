---
title: State machines
description: "Typed statecharts for Haskell with type-level chart specs. Overview, hello world, and a map of the guides."
sidebar:
  order: 1
  label: Overview
---

`wireform-state-machine` is a typed statechart library inspired by
[XState](https://stately.ai/docs/xstate), with the chart specification written
as a type. The compiler checks the chart before runtime: missing
transition targets, duplicate state keys, undeclared events, incomplete handler
registries, and names used in the wrong role are compile errors. State names,
event names, guards, actions, services, invoke ids, and done-data outputs are
separate enum types promoted to kinds, so an event cannot be used where a state
is required.

The runtime implements [SCXML](https://www.w3.org/TR/scxml/) statecharts:
compound and parallel states, shallow and deep history, guarded transitions,
entry / exit / transition actions, eventless (`Always`) and delayed (`After`)
transitions, invoked services and child-chart actors with `onDone` / `onError`,
done events with typed data, wildcard and root-level handlers, internal
transitions, and typed machine output.

## Hello, statechart

A machine has four parts: a name vocabulary, a chart type, implementations for
referenced behavior, and a runner.

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
import StateMachine

-- 1. One plain enum per name role; deriveKeyKind promotes its
--    constructors to type-level chart keys.
data ToggleState = Off | On
  deriving stock (Show, Eq)
data ToggleEvent = FLIP

deriveKeyKind ''ToggleState
deriveKeyKind ''ToggleEvent

-- 2. The chart is a type over those kinds. The standalone kind signature
--    pins every name role; NoKey marks the five this chart does not use
--    (guards, actions, services, invoke ids, done-data outputs).
--    Ill-formed charts do not compile.
type Toggle :: ChartSpec ToggleState ToggleEvent NoKey NoKey NoKey NoKey NoKey
type Toggle =
  Chart "toggle" () ()
    '[ 'FLIP ::: () ]
    '[ State 'Off '[ On 'FLIP ==> To 'On ]
     , State 'On  '[ On 'FLIP ==> To 'Off ]
     ]
    'Off

-- 3. The implementation. This chart names no guards/actions/services, so
--    every registry is empty; the last argument is the machine's output.
impl :: ChartImpl IO Toggle
impl = chartImpl RNil RNil RNil RNil (const ())

-- 4. Run it.
main :: IO ()
main = do
  Right s0 <- initialize impl ()
  Right s1 <- step impl (sMachine s0) (EvExternal (mkEvent_ @'FLIP))
  print (activeKeys (sMachine s1))    -- [On]
  print (matches @'On (sMachine s1))  -- True
  -- matches @'Onn would not compile: ToggleState has no such constructor.
```

`mkEvent_ @'FLIP` is a typed, payload-less event; `matches @'On` is a
compile-checked state query. Both name a *promoted constructor*: misspell it
and it is not in scope; hand a `ToggleEvent` where a `ToggleState` belongs
and it is a kind error.

## One key, three representations

`deriveKeyKind` connects three views of every name, and `StateMachine.Key`
moves between them:

- the **type-level constructor** `'On` — what charts, registries, and
  compile-checked queries (`matches @'On`) are written with;
- the **singleton** `SOn :: SKey 'On` — runtime evidence for a type-level
  key (`skey @'On` produces it; matching an `SKey` constructor inside a
  `withKey` continuation refines types per branch, e.g. `mkEventS s payload`
  pins the payload type to that branch's event);
- the **value** `On :: ToggleState` — what runtime queries return.

```haskell
demote @'On              -- On  : reify a type-level key to its value
keyNameOf @'On           -- "On": the wire name (snapshots, traces, renders)
withKey On (\s -> …)     -- back up: s :: SKey 'On, type-level again
parseKey "On"            -- Just (SomeKey SOn) :: Maybe (SomeKey ToggleState)
keyUniverse @ToggleState -- every key of the kind, in declaration order
```

Runtime queries return enum values: `activeKeys m :: [ToggleState]` can be
pattern-matched exhaustively, `matchesKey On m` is the value-level form of
`matches`, and `availableEventKeys impl m :: [ToggleEvent]` lists enabled UI
actions. Where text does appear — snapshots, traces, rendered
charts, `config` — it is always the constructor spelling (`"On"`), produced
by `keyNameOf` and parsed back with `parseKey`.

## The shape of a program

```mermaid
flowchart LR
  spec["chart type<br/>(Spec)"] --> impl["chartImpl<br/>(Registry)"]
  impl --> run["initialize / step<br/>(pure Step)"]
  run --> io["Interpret<br/>(IO: timers, actors)"]
  run --> sim["Debug.simulate<br/>(virtual clock)"]
  run --> snap["snapshot / restore<br/>(Persist)"]
  spec --> viz["render<br/>(Stately/Mermaid/DOT/HTML)"]
```

1. **[Specify](./spec/)** the chart as a type: states, typed events,
   transitions, guards, actions, invoked services. Ill-formed charts do not
   typecheck.
2. **[Implement](./implementing/)** the names the chart mentions with
   `chartImpl`. Registration is order-insensitive and *complete* — a missing,
   duplicated, or foreign name is a compile error.
3. **[Run](./running/)** it: purely with `initialize` / `step` (timers and
   services surface as effect requests), interactively with the
   [IO interpreter](./running/#the-io-interpreter), or deterministically in
   tests with the [simulator](./testing/).
4. **[Persist](./persistence/)** it: `snapshot` / `restore` with precise errors
   and per-failure recovery when a stored snapshot no longer matches the
   current chart.
5. **[Visualize](./visualization/)** it: Stately config, Mermaid, Graphviz DOT,
   or a self-contained HTML page.

## Guides

| Page | What it covers |
| --- | --- |
| [The chart type](./spec/) | The type-level DSL: per-role key kinds, states, typed events, transitions, guards, actions, `Invoke`, history, parallel regions, root handlers, and the errors that reject bad charts. |
| [Implementing a chart](./implementing/) | `chartImpl` and the four registries; `mkGuard` / `assign` / `effect` / `raiseEvent`; the three service kinds; typed context and events inside handlers. |
| [Running a machine](./running/) | The pure `step` and its trace + effect requests; the IO interpreter (timers, subscriptions, `waitFinished`, actors); the effect-request model. |
| [Testing with the simulator](./testing/) | `simulate` with a virtual clock; scripting timer races and service outcomes deterministically; `prettyTrace`; the `unreachableStates` / `deadEnds` lints. |
| [Persistence & recovery](./persistence/) | Snapshots, the chart fingerprint, the `RestoreError` taxonomy, and `Recovery` strategies for evolving charts. |
| [Visualization](./visualization/) | Stately config, Mermaid, DOT, and self-contained HTML; highlighting the live configuration. |

## Why the types

The design separates the checked specification from the runtime interpreter:
the **specification** lives at the type level (where well-formedness and
completeness are decidable),
and the **semantics** stay a value-level interpreter (where guards, timers, and
external events actually resolve). See
[`packages/state-machine`](../packages/state-machine/) for the catalogue entry
and module map, and [`grpc-spec`](../packages/grpc-spec/) for the sibling
completeness-checking technique the registries reuse.

**Typed internal values.** Event payloads, guard/action context, invoke outputs
and errors, done-data, and parent↔child actor messages are Haskell values.
Lifecycle values ride `Data.Dynamic` and are recovered at their real type by
`invokeOutput`/`invokeError`/`doneData`; the child bridge translates typed
events both ways. Persistence is covered separately in
[Persistence & recovery](./persistence/).

Run the bundled demo end-to-end:

```bash
cabal run example-traffic
```
