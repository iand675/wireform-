---
title: State machines (statecharts)
description: "Dependently typed statecharts with the full XState feature set: type-level chart specs, completeness-checked registries, SCXML semantics, snapshots with recovery, and visualization to XState JSON, Mermaid, DOT, and HTML."
sidebar:
  order: 72
---

`wireform-state-machine` is a dependently typed statechart library — think
[XState](https://stately.ai/docs/xstate), but the machine *specification is a
type*, and the things XState checks at runtime (or not at all) are compile
errors:

- A transition targeting a state that doesn't exist, a duplicate state name,
  an event nobody declared, an `OnDone` outside an `Invoke` — **`TypeError`s**,
  each naming the chart, the offending name, and the known alternatives.
- Guards, actions, invoked services, and done-data producers register into
  **completeness-checked registries** (the same type-level permutation trick
  as [`grpc-spec`](../grpc-spec/)'s `Service`): forgetting to implement a
  guard the chart mentions, implementing it twice, or typo'ing its name is a
  compile error naming the offender.
- Events carry **typed payloads** (`mkEvent @"FETCH" url` only compiles with
  the declared payload type), context is machine-wide typed, and the machine's
  final output is typed.

The feature set is full XState/SCXML: compound and parallel states,
shallow/deep history, guarded transitions, entry/exit/transition actions,
eventless (`Always`) and delayed (`After`) transitions, invoked
services/actors (promise, callback, and child-chart) with `OnDone`/`OnError`,
done events with data, wildcard and root-level (global) handlers, internal
transitions, and typed machine output.

```haskell
type Toggle =
  Chart "toggle" () ()
    '[ "FLIP" ::: () ]
    '[ State "off" '[ On "FLIP" ==> To "on" ]
     , State "on"  '[ On "FLIP" ==> To "off" ]
     ]
    "off"

impl :: ChartImpl IO Toggle
impl = chartImpl RNil RNil RNil RNil (const ())
```

## Documentation

This is the catalogue entry. The **[State machines guide](../../state-machine/)**
is the full, multi-page treatment:

- [Overview & hello world](../../state-machine/) — the shape of a program.
- [The chart type](../../state-machine/spec/) — the full type-level DSL and the
  `TypeError`s that reject bad charts.
- [Implementing a chart](../../state-machine/implementing/) — `chartImpl`, the
  four registries, guards/actions/services.
- [Running a machine](../../state-machine/running/) — the pure `step`, effect
  requests, and the IO interpreter (timers, subscriptions, actors).
- [Testing with the simulator](../../state-machine/testing/) — the virtual
  clock, scripting timer/service races, chart lints.
- [Persistence & recovery](../../state-machine/persistence/) — snapshots,
  fingerprints, and recovery for evolving charts.
- [Visualization](../../state-machine/visualization/) — XState JSON, Mermaid,
  DOT, self-contained HTML.

## Module map

| Module | What lives there |
| --- | --- |
| `StateMachine` | umbrella re-export of the whole surface |
| `StateMachine.Spec` | the type-level chart DSL |
| `StateMachine.Validate` | well-formedness `TypeError`s (`ValidChart`) |
| `StateMachine.Reify` | demotion of the spec to the runtime chart (`KnownChart`) |
| `StateMachine.Event` | typed events + typed lifecycle channels; the external-input JSON boundary (`decodeEvent`) |
| `StateMachine.Registry` | completeness-checked guard/action/service/output registration |
| `StateMachine.Machine` | the abstract machine value, `chartImpl` |
| `StateMachine.Step` | pure SCXML macrostep semantics |
| `StateMachine.Persist` | snapshots, restore, recovery strategies |
| `StateMachine.Interpret` | the IO interpreter (timers, services, actors) |
| `StateMachine.Debug` | deterministic simulation, trace rendering, chart lints |
| `StateMachine.Render.*` | XState JSON, Mermaid, DOT, self-contained HTML |

## Try it

```bash
cabal run example-traffic
```

steps a traffic light through a pedestrian cycle, a power outage (history
restores the phase), a snapshot/restore roundtrip, a rejected stale snapshot,
a `Recovery` restart, a typed final output, and all four renderers.
