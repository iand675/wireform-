---
title: State machines (statecharts)
description: "Typed statecharts with type-level chart specs, completeness-checked registries, SCXML semantics, snapshots with recovery, and visualization to Stately config, Mermaid, DOT, and HTML."
sidebar:
  order: 72
---

`wireform-state-machine` is a typed statechart library with a chart
specification that lives at the type level. The compiler rejects errors that a
string-keyed machine would otherwise find only at runtime:

- a transition targeting a missing state, a duplicate state key, an undeclared
  event, or `OnDone` outside an `Invoke` is a `TypeError` that names the chart,
  the offending key, and the valid alternatives;
- each name role — states, events, guards, actions, services, invoke ids, and
  done-data outputs — is a separate sum type used as a kind, so an event in a
  state position is a kind error;
- guards, actions, services, and output producers live in completeness-checked
  registries, so missing, duplicate, or foreign implementation keys are compile
  errors;
- events carry typed payloads, context is a typed machine-wide value, and final
  output has its declared Haskell type.

The runtime implements SCXML statecharts: compound and parallel states,
shallow/deep history, guarded transitions, entry/exit/transition actions,
eventless (`Always`) and delayed (`After`) transitions, promise/callback/child
invocations with `OnDone`/`OnError`, done events with data, wildcard and
root-level handlers, internal transitions, and typed machine output.

```haskell
data ToggleState = Off | On
data ToggleEvent = FLIP

deriveKeyKind ''ToggleState
deriveKeyKind ''ToggleEvent

type Toggle :: ChartSpec ToggleState ToggleEvent NoKey NoKey NoKey NoKey NoKey
type Toggle =
  Chart "toggle" () ()
    '[ 'FLIP ::: () ]
    '[ State 'Off '[ On 'FLIP ==> To 'On ]
     , State 'On  '[ On 'FLIP ==> To 'Off ]
     ]
    'Off

impl :: ChartImpl IO Toggle
impl = chartImpl RNil RNil RNil RNil (const ())
```

## Documentation

This catalogue page maps the package. The **[State machines guide](../../state-machine/)**
contains the tutorial and API guide:

- [Overview & hello world](../../state-machine/) — the shape of a program.
- [The chart type](../../state-machine/spec/) — the type-level DSL and the
  `TypeError`s that reject bad charts.
- [Implementing a chart](../../state-machine/implementing/) — `chartImpl`, the
  four registries, guards/actions/services.
- [Running a machine](../../state-machine/running/) — the pure `step`, effect
  requests, and the IO interpreter (timers, subscriptions, actors).
- [Testing with the simulator](../../state-machine/testing/) — the virtual
  clock, scripting timer/service races, chart lints.
- [Persistence & recovery](../../state-machine/persistence/) — snapshots,
  fingerprints, and recovery for evolving charts.
- [Visualization](../../state-machine/visualization/) — Stately config,
  Mermaid, DOT, self-contained HTML.

## Module map

| Module | What lives there |
| --- | --- |
| `StateMachine` | umbrella re-export of the whole surface |
| `StateMachine.Key` | singleton keys: `SKey`, `KeyKind`/`KnownKey`, reify/demote, `deriveKeyKind` |
| `StateMachine.Spec` | the type-level chart DSL |
| `StateMachine.Validate` | well-formedness `TypeError`s (`ValidChart`) |
| `StateMachine.Reify` | demotion of the spec to the runtime chart (`KnownChart`) |
| `StateMachine.Event` | typed events + typed lifecycle channels; decoding for named external events (`decodeEvent`) |
| `StateMachine.Registry` | completeness-checked guard/action/service/output registration |
| `StateMachine.Machine` | the abstract machine value, `chartImpl` |
| `StateMachine.Step` | pure SCXML macrostep semantics |
| `StateMachine.Persist` | snapshots, restore, recovery strategies |
| `StateMachine.Interpret` | the IO interpreter (timers, services, actors) |
| `StateMachine.Debug` | deterministic simulation, trace rendering, chart lints |
| `StateMachine.Render.*` | Stately config, Mermaid, DOT, self-contained HTML |

## Try it

```bash
cabal run example-traffic
```

runs the traffic-light demo through a pedestrian cycle, a power outage with
history restoration, snapshot/restore, stale-snapshot rejection, `Recovery`
restart, typed final output, and all four renderers.
