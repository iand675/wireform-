---
title: The chart type
description: "The type-level statechart DSL: states, typed events, transitions, guards, actions, invoked services, history, parallel regions, root handlers — and the TypeErrors that reject ill-formed charts."
sidebar:
  order: 2
  label: The chart type
---

A chart is a type of kind `ChartSpec`. Writing it as a type lets the compiler
reject errors that a string-keyed state machine would otherwise discover at
runtime. This page is the DSL reference; for the model behind the DSL, start
with [State machine concepts](../concepts/).

## Names are enums, used as kinds

Every name a chart mentions is a *promoted constructor* of an ordinary sum
type — one plain enum per role, equipped by a `deriveKeyKind` splice
(Template Haskell) with the singletons and instances that let it serve as a
kind (see [one key, three representations](../#one-key-three-representations)):

```haskell
{-# LANGUAGE TemplateHaskell #-}

data FetchState   = Idle | Loading | Success | Failure
data FetchEvent   = FETCH | CANCEL
data FetchGuard   = OutOfRetries
data FetchAction  = LogStart | Save | StopSpinner
data FetchService = HttpGet
data FetchInvoke  = GetUser

deriveKeyKind ''FetchState
deriveKeyKind ''FetchEvent
-- … and the other four
```

`ChartSpec` takes seven kind parameters, one per name role, in this order:

| Parameter | Keys of |
| --- | --- |
| `st` | states |
| `ev` | events |
| `g` | guards |
| `act` | actions |
| `svc` | invoked services |
| `inv` | invoke ids |
| `out` | done-data producers (`FinalWith`) |

Most code spells them once. Every chart carries one **standalone kind
signature** that pins all seven, with the uninhabited `NoKey` for roles the
chart does not use — a chart with `NoKey` guards provably mentions none, and
its guard registry is exactly `RNil`:

```haskell
type Fetch ::
  ChartSpec FetchState FetchEvent FetchGuard FetchAction FetchService FetchInvoke NoKey
type Fetch =
  Chart "fetch" FetchCtx Report
    '[ 'FETCH ::: Url, 'CANCEL ::: () ]
    '[ State 'Idle '[ On 'FETCH ==> To 'Loading ]
     , State 'Loading
         '[ Entry '[ 'LogStart ]
          , Exit  '[ 'StopSpinner ]
          , Invoke 'GetUser 'HttpGet
              '[ OnDone  ==> To 'Success ! '[ 'Save ] ]
              '[ OnError ==> To 'Failure ]
          , On 'CANCEL ==> To 'Idle
          , After 30000 ==> To 'Failure
          ]
     , State 'Failure
         '[ On 'FETCH ?: 'OutOfRetries ==> Stay
          , On 'FETCH ==> To 'Loading
          ]
     , Final 'Success
     ]
    'Idle
```

Because each role is its own kind, misusing a role is a *kind* error at the
chart definition itself: `To 'FETCH` (an event where a state belongs) or
`?: 'Save` (an action as a guard) is rejected by GHC with a kind mismatch
before chart validation even runs. The runtime *wire name* of every key —
in snapshots, traces, renders, and `config` — is the constructor spelling
as `Text` (`"Idle"`, `"FETCH"`).

This `Fetch` chart is the running example here and in
[Implementing a chart](../implementing/).

## The two chart constructors

```haskell
type Chart     name ctx out events states initial
type ChartWith name ctx out events states initial rootFeatures
```

- `name` — a `Symbol`, the chart's id (appears in traces, snapshots, renders).
  The one string in a chart; every other name is a key.
- `ctx` — the machine's context type (arbitrary; threaded through every guard
  and action).
- `out` — the output type produced when the machine reaches a **top-level**
  final state.
- `events` — the declared event list (below).
- `states` — the top-level state list.
- `initial` — the initial top-level state (a state key).
- `rootFeatures` (only in `ChartWith`) — chart-wide handlers, e.g. an event
  every state responds to. Use `Chart` when you have none.

## Events carry typed payloads

An event is a name paired with a payload type:

```haskell
'[ 'FETCH  ::: Url
 , 'CANCEL ::: ()
 ]
```

`mkEvent @'FETCH someUrl` compiles only if the chart declares `'FETCH` **and**
`someUrl :: Url`. A payload-less event uses `mkEvent_ @'CANCEL`. Guards and
actions project payloads back out type-safely with `onEvent @'FETCH` (see
[Implementing a chart](../implementing/)).

## States

State keys are **globally unique** across the whole chart (the compiler
enforces it), so a bare key is a complete address — there is no path syntax.

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
On 'FETCH  ==> To 'Loading                   -- event → state
On 'FETCH  ==> To 'Loading ! '[ 'LogStart ]  -- with transition actions
On 'FETCH  ?: 'OutOfRetries ==> Stay         -- guarded (?: names a guard)
On 'CANCEL ==> Stay ! '[ 'StopSpinner ]      -- targetless: run actions, stay put
```

### Triggers

| Trigger | Fires when |
| --- | --- |
| `On 'EVENT` | The named event arrives. |
| `Wildcard` | *Any* declared named event (does not match timer/done/invoke events). |
| `Always` | Immediately, while the state is active and any guard passes (eventless). |
| `After ms` | The state has been active `ms` milliseconds. |
| `OnDoneOf 'State` | The named compound/parallel state completed. |
| `OnDone` / `OnError` | Inside an `Invoke` — the invocation resolved / failed. |

Attach a guard to any trigger with `?: 'GuardName`. A state may list several
transitions for the same event; they are tried in declaration order and the
first whose guard passes wins.

### Targets

| Target | Meaning |
| --- | --- |
| `To 'State` | Transition to one state (external: exits and re-enters the LCCA). |
| `ToAll '[ 'A, 'B ]` | Enter several states at once (targets in different parallel regions). |
| `Inside 'Child` | Internal transition to a descendant — the source is not exited/re-entered. |
| `Stay` | Targetless — run actions only, no exit/entry. |

Attach transition actions with `! '[ 'Act1, 'Act2 ]`; they run between exit and
entry.

## Entry, exit, and invoked services

```haskell
State 'Loading
  '[ Entry '[ 'LogStart ]
   , Exit  '[ 'StopSpinner ]
   , Invoke 'GetUser 'HttpGet
       '[ OnDone  ==> To 'Success ! '[ 'Save ] ]
       '[ OnError ==> To 'Failure ]
   , On 'CANCEL ==> To 'Idle
   , After 30000 ==> To 'Failure
   ]
```

- `Entry` / `Exit` actions run, in order, when the state is entered / exited.
- `Invoke id service onDone onError` starts `service` on entry and cancels it
  on exit. `id` is a chart-unique invocation id (a key of the invoke-id kind;
  its wire spelling, `"GetUser"`, names the invocation in traces); `service`
  is a key in the service registry. The `onDone` / `onError` lists use the
  `OnDone` / `OnError` triggers and see the invocation's result via
  `invokeOutput` / `invokeError`.

## History

History pseudo-states remember what was active in their parent when it was last
exited, so re-entering the parent via the history node restores it:

```haskell
Compound 'Operational 'Green
  '[ State 'Green  '[ On 'TIMER ==> To 'Yellow ]
   , State 'Yellow '[ On 'TIMER ==> To 'Green ]
   , Hist 'OpHist          -- shallow: restores the last immediate child
   ]
   '[ On 'POWER_OUT ==> To 'Flashing ]
-- elsewhere:
State 'Flashing '[ On 'FIXED ==> To 'OpHist ]  -- resume where we left off
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
Parallel 'Editing
  '[ Compound 'Bold 'BoldOff
       '[ State 'BoldOff '[ On 'TOGGLE_BOLD ==> To 'BoldOn ]
        , State 'BoldOn  '[ On 'TOGGLE_BOLD ==> To 'BoldOff ]
        ]
   , Compound 'Italic 'ItalicOff
       '[ State 'ItalicOff '[ On 'TOGGLE_ITALIC ==> To 'ItalicOn ]
        , State 'ItalicOn  '[ On 'TOGGLE_ITALIC ==> To 'ItalicOff ]
        ]
   ]
   '[]
```

(State keys are globally unique, which is why the two regions use `'BoldOff`
/ `'ItalicOff` rather than sharing an `'Off`.)

## Root-level (global) handlers

`ChartWith`'s last argument is a feature list attached to the chart root, so a
handler fires from **any** state — unless an active state has its own handler
for that event, which shadows it:

```haskell
type Session :: ChartSpec SessionState SessionEvent NoKey NoKey NoKey NoKey NoKey
type Session =
  ChartWith "session" Ctx () Events States 'Active
    '[ On 'LOGOUT ==> To 'LoggedOut ]   -- from anywhere
```

## Ill-formed charts do not compile

The type-level spec rejects invalid charts in two layers.

The first layer is GHC kind checking: names are per-role kinds, so *role* confusion is a
kind error at the chart definition itself. `To 'FETCH` (an event where a
state belongs), `?: 'Save` (an action as a guard), an invoke id in service
position — GHC reports the kind mismatch with no help from the library.

The second layer is `StateMachine.Validate`, which reports structural chart errors as
a `TypeError` naming the chart and the offender, with the list of valid
keys:

- a transition (or history default) target no node declares — a target only
  has to *kind*-check, so a state-enum constructor the chart never mounts is
  caught here;
- a state key used by two nodes (state keys are globally unique; the
  synthetic root's runtime name `#root` cannot collide with a key —
  `deriveKeyKind` refuses to generate it as a wire name);
- an `On 'E` for an event `'E` the chart does not declare;
- a compound whose `initial` is not one of its direct children, or that has
  no children;
- a duplicate `Invoke` id;
- `OnDone` / `OnError` used outside an `Invoke` (or a non-`OnDone` entry in
  an `onDone` list);
- an `OnDoneOf 'S` where `'S` is not a compound/parallel state;
- root features containing `Exit` actions or an `After` transition (the root
  is never exited or re-entered).

For example, if the state enum has a `Broken` constructor no node declares,
`On 'TIMER ==> To 'Broken` produces roughly:

```
• Chart "traffic": transition in 'Green targets unknown state 'Broken
  Known states: '[ 'Operational, 'Green, 'Yellow, 'Red, ...]
```

Next: [implement the names your chart mentions](../implementing/).
