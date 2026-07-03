---
title: The chart type
description: "The type-level statechart DSL: states, typed events, transitions, guards, actions, invoked services, history, parallel regions, root handlers — and the TypeErrors that reject ill-formed charts."
sidebar:
  order: 2
  label: The chart type
---

A chart is a type of kind `ChartSpec`. Writing it as a type is what lets the
compiler reject the mistakes a stringly-typed state machine catches only at
runtime — or never. This page is the reference for the DSL exported by
`StateMachine.Spec` (all re-exported from `StateMachine`).

## The two chart constructors

```haskell
type Chart     name ctx out events states initial
type ChartWith name ctx out events states initial rootFeatures
```

- `name` — a `Symbol`, the chart's id (appears in traces, snapshots, renders).
- `ctx` — the machine's context type (arbitrary; threaded through every guard
  and action).
- `out` — the output type produced when the machine reaches a **top-level**
  final state.
- `events` — the declared event list (below).
- `states` — the top-level state list.
- `initial` — the `Symbol` name of the initial top-level state.
- `rootFeatures` (only in `ChartWith`) — chart-wide handlers, e.g. an event
  every state responds to. Use `Chart` when you have none.

## Events carry typed payloads

An event is a name paired with a payload type:

```haskell
'[ "FETCH"  ::: Url
 , "CANCEL"  ::: ()
 , "RECEIVED" ::: Response
 ]
```

`mkEvent @"FETCH" someUrl` compiles only if `"FETCH"` is declared **and**
`someUrl :: Url`. A payload-less event uses `mkEvent_ @"CANCEL"`. Guards and
actions project payloads back out type-safely with `onEvent @"FETCH"` (see
[Implementing a chart](../implementing/)).

## States

State names are **globally unique** across the whole chart (the compiler
enforces it), so a bare name is a complete address — there is no path syntax.

| Constructor | Meaning |
| --- | --- |
| `State name features` | An atomic (leaf) state. |
| `Compound name initial children features` | Exactly one child active at a time, starting at `initial`. |
| `Parallel name regions features` | Every child region active simultaneously. |
| `Final name` | A final state; entering it completes the parent. |
| `FinalWith name producer` | A final state whose done event carries data from the named output producer. |
| `Hist name` | A shallow history pseudo-state. |
| `HistDeep name` | A deep history pseudo-state. |
| `HistWith name kind default` | History with an explicit kind and default target. |

`features` is a type-level list of `Feature`s: transitions, `Entry` / `Exit`
actions, and `Invoke`.

## Transitions

A transition is a *trigger* on the left of `==>`, and *targets + actions* on
the right.

```haskell
On "FETCH" ==> To "loading"                 -- event → state
On "FETCH" ==> To "loading" ! '["logStart"] -- with transition actions
On "TICK"  ?: "underLimit" ==> To "next"    -- guarded (?: names a guard)
On "PING"  ==> Stay ! '["pong"]             -- targetless: run actions, stay put
```

### Triggers

| Trigger | Fires when |
| --- | --- |
| `On "EVENT"` | The named event arrives. |
| `Wildcard` | *Any* declared named event (does not match timer/done/invoke events). |
| `Always` | Immediately, while the state is active and any guard passes (eventless). |
| `After ms` | The state has been active `ms` milliseconds. |
| `OnDoneOf "state"` | The named compound/parallel `state` completed. |
| `OnDone` / `OnError` | Inside an `Invoke` — the invocation resolved / failed. |

Attach a guard to any trigger with `?: "guardName"`. A state may list several
transitions for the same event; they are tried in declaration order and the
first whose guard passes wins.

### Targets

| Target | Meaning |
| --- | --- |
| `To "state"` | Transition to one state (external: exits and re-enters the LCCA). |
| `ToAll '["a","b"]` | Enter several states at once (targets in different parallel regions). |
| `Inside "child"` | Internal transition to a descendant — the source is not exited/re-entered. |
| `Stay` | Targetless — run actions only, no exit/entry. |

Attach transition actions with `! '["act1","act2"]`; they run between exit and
entry.

## Entry, exit, and invoked services

```haskell
State "loading"
  '[ Entry '["startSpinner"]
   , Exit  '["stopSpinner"]
   , Invoke "getUser" "httpGet"
       '[ OnDone  ==> To "success" ! '["save"] ]
       '[ OnError ==> To "failure" ]
   , On "CANCEL" ==> To "idle"
   , After 30000 ==> To "failure"
   ]
```

- `Entry` / `Exit` actions run, in order, when the state is entered / exited.
- `Invoke id service onDone onError` starts `service` on entry and cancels it on
  exit. `id` is a chart-unique invocation id; `service` is a name in the service
  registry. The `onDone` / `onError` lists use the `OnDone` / `OnError`
  triggers and see the invocation's result via `invokeOutput` / `invokeError`.

## History

History pseudo-states remember what was active in their parent when it was last
exited, so re-entering the parent via the history node restores it:

```haskell
Compound "operational" "green"
  '[ State "green"  '[ On "TICK" ==> To "yellow" ]
   , State "yellow" '[ On "TICK" ==> To "green" ]
   , Hist "opHist"          -- shallow: restores the last immediate child
   ]
   '[ On "POWER_OUT" ==> To "flashing" ]
-- elsewhere:
State "flashing" '[ On "FIXED" ==> To "opHist" ]  -- resume where we left off
```

Shallow history restores the last active immediate child; deep history
(`HistDeep`) restores the exact atomic configuration beneath the parent. Empty
history falls back to the declared default (`HistWith`) or the parent's
initial. History survives snapshotting.

## Parallel regions

A `Parallel` state's children are orthogonal regions, all active at once. One
event can fire non-conflicting transitions in several regions in the same step;
the parallel state completes (raising its done event) when *every* region has
reached a final state.

```haskell
Parallel "editing"
  '[ Compound "bold" "off"
       '[ State "off" '[ On "TOGGLE_BOLD" ==> To "on" ]
        , State "on"  '[ On "TOGGLE_BOLD" ==> To "off" ]
        ]
   , Compound "italic" "off"
       '[ State "off" '[ On "TOGGLE_ITALIC" ==> To "on" ]
        , State "on"  '[ On "TOGGLE_ITALIC" ==> To "off" ]
        ]
   ]
   '[]
```

## Root-level (global) handlers

`ChartWith`'s last argument is a feature list attached to the chart root, so a
handler fires from **any** state — unless an active state has its own handler
for that event, which shadows it:

```haskell
type Session =
  ChartWith "session" Ctx () Events States "active"
    '[ On "LOGOUT" ==> To "loggedOut" ]   -- from anywhere
```

## Ill-formed charts do not compile

The value of the type-level spec is what it *rejects*. `StateMachine.Validate`
turns each of these into a `TypeError` naming the chart and the offender, with
the list of valid names:

- a transition (or history default) target that is not a declared state;
- a duplicate state name (the reserved root name `#root` included);
- an `On "E"` for an undeclared event `E`;
- a compound whose `initial` is not one of its direct children, or that has no
  children;
- a duplicate `Invoke` id;
- `OnDone` / `OnError` used outside an `Invoke` (or a non-`OnDone` entry in an
  `onDone` list);
- an `OnDoneOf "s"` where `s` is not a compound/parallel state;
- root features containing `Exit` actions or an `After` transition (the root is
  never exited or re-entered).

For example, `On "TIMER" ==> To "nowhere"` in a chart without a `"nowhere"`
state produces roughly:

```
• Chart "traffic": transition in "green" targets unknown state "nowhere"
  Known states: '["green", "yellow", "red", ...]
```

Next: [implement the names your chart mentions](../implementing/).
