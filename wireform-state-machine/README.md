# wireform-state-machine

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

> [!CAUTION]
> wireform is in heavy development and has not been published to Hackage yet. APIs may change.

Typed statecharts for Haskell with a machine specification written at the type
level. The compiler
checks chart errors that a string-keyed runtime would otherwise discover late:

- **Ill-formed charts do not compile.** A transition targeting a missing
  state, a duplicate state name, an undeclared event, a compound
  state whose `initial` isn't one of its children, an `OnDone` outside an
  `Invoke` — each is a `TypeError` naming the chart, the offender, and the
  known alternatives.
- **Names are typed, per role.** States, events, guards, actions, services,
  invoke ids, and done-data outputs are each their own sum type, used as a
  *kind*: putting an event where a state belongs is a kind error, before chart validation runs.
- **Implementations are complete by construction.** Guards, actions,
  services, and done-data producers register into order-insensitive,
  completeness-checked registries (the same type-level permutation proof as
  `grpc-spec`'s `Service`): a missing, duplicated, or foreign name is a
  compile error naming it. A misspelled guard cannot reach runtime.
- **Events, context, and output are typed.** `mkEvent @'FETCH url` compiles
  only with the payload type the chart declares; guards and actions project
  payloads with `onEvent @'FETCH`; the machine's final output has its
  declared Haskell type.

The runtime implements SCXML-style statecharts: compound and parallel states, shallow/deep history, guarded
transitions, entry/exit/transition actions, eventless (`Always`) and delayed
(`After`) transitions, invoked services and child-chart actors with
`OnDone`/`OnError`, done events with data, wildcard and root-level (global)
handlers, internal transitions, typed machine output.

## A chart is a type

Declare one enum per name role and derive its singletons; the chart then
uses the *promoted constructors* as keys, pinned by one standalone kind
signature (`NoKey` marks unused roles):

```haskell
data TrafficState  = Operational | Green | Yellow | Red | Walk | OpHist | Flashing | Off
data TrafficEvent  = TIMER | PUSH | POWER_OUT | FIXED | DECOMMISSION
data TrafficGuard  = NoPedestrian
data TrafficAction = CountCycle | NotePedestrian | ClearPedestrian

deriveKeyKind ''TrafficState
deriveKeyKind ''TrafficEvent
deriveKeyKind ''TrafficGuard
deriveKeyKind ''TrafficAction

type Traffic :: ChartSpec TrafficState TrafficEvent TrafficGuard TrafficAction NoKey NoKey NoKey
type Traffic =
  ChartWith "traffic" TrafficCtx Int
    '[ 'TIMER ::: (), 'PUSH ::: (), 'POWER_OUT ::: (), 'FIXED ::: ()
     , 'DECOMMISSION ::: () ]
    '[ Compound 'Operational 'Green
         '[ State 'Green
              '[ On 'TIMER ==> To 'Yellow
               , On 'PUSH ==> Stay ! '[ 'NotePedestrian ]
               ]
          , State 'Yellow '[On 'TIMER ==> To 'Red ! '[ 'CountCycle ]]
          , State 'Red
              '[ On 'TIMER ?: 'NoPedestrian ==> To 'Green
               , On 'TIMER ==> To 'Walk
               ]
          , State 'Walk '[On 'TIMER ==> To 'Green ! '[ 'ClearPedestrian ]]
          , Hist 'OpHist
          ]
         '[On 'POWER_OUT ==> To 'Flashing]
     , State 'Flashing '[On 'FIXED ==> To 'OpHist]
     , Final 'Off
     ]
    'Operational
    '[On 'DECOMMISSION ==> To 'Off]

impl :: ChartImpl IO Traffic
impl =
  chartImpl
    (mkGuard @'NoPedestrian (\ctx _ -> not (pedestrianWaiting ctx)) :& RNil)
    ( assign @'CountCycle      (\ctx _ -> ctx{cycles = cycles ctx + 1})
        :& assign @'NotePedestrian  (\ctx _ -> ctx{pedestrianWaiting = True})
        :& assign @'ClearPedestrian (\ctx _ -> ctx{pedestrianWaiting = False})
        :& RNil )
    RNil   -- services
    RNil   -- done-data producers
    cycles -- typed output when the machine reaches a top-level final state
```

## Keys are singletons

Every key has three representations: the type-level constructor used by the
chart, a runtime singleton (`SKey`), and the ordinary value returned by queries.
`StateMachine.Key` converts between them:

```haskell
demote @'Green            -- Green  : reify a type-level key to its value
keyNameOf @'Green         -- "Green": the wire name (snapshots, traces, renders)
withKey Green (\s -> …)   -- back up: s :: SKey 'Green, type-level again
parseKey "Green"          -- Just (SomeKey SGreen): the inverse of keyName
keyUniverse @TrafficState -- every key of the kind, in declaration order
```

Runtime queries return enum values:

```haskell
activeKeys m :: [TrafficState]      -- pattern-match exhaustively
matchesKey Green m                  -- value-level membership
availableEventKeys impl m           -- [TrafficEvent] enabled in the current state
```

and inside a `withKey` continuation, matching the singleton refines the
event payload type branch by branch:

```haskell
withKey ev $ \s -> case s of
  STIMER -> mkEventS s ()   -- payload type pinned to 'TIMER's declaration
  SPUSH  -> mkEventS s ()
  …
```

Rendered by the library itself (`StateMachine.Render.Mermaid`):

```mermaid
stateDiagram-v2
  [*] --> Operational
  state Operational {
    [*] --> Green
    state "H" as OpHist
    Green --> Yellow : TIMER
    Green --> Green : PUSH / NotePedestrian
    Yellow --> Red : TIMER / CountCycle
    Red --> Green : TIMER [NoPedestrian]
    Red --> Walk : TIMER
    Walk --> Green : TIMER / ClearPedestrian
  }
  Operational --> Flashing : POWER_OUT
  Flashing --> OpHist : FIXED
  Off --> [*]
  state "any state" as __any
  __any --> Off : DECOMMISSION
```

## Running

The semantics are a pure step function (`StateMachine.Step`):
`initialize` and `step` return the new machine, a micro-step trace, and
*effect requests* (start/cancel timer, start/cancel invocation) instead of
performing IO. On top of that:

- **`StateMachine.Interpret`** executes effects for real: one driver thread
  per machine, generation-tagged timers (a timer that fired concurrently
  with its own cancellation cannot mis-fire after re-entry),
  promise/callback services, child-chart actors with parent↔child routing,
  subscriptions, `waitFinished`.
- **`StateMachine.Debug`** executes them deterministically: `simulate`
  scripts sends, virtual-clock advances (`SimAdvance 100`), and invocation
  settlements (`SimResolve`/`SimReject`) — every timer race is a repeatable
  unit test with no `threadDelay`. `prettyTrace` renders the step
  traces; `unreachableStates`/`deadEnds` lint the chart structure.

```haskell
Right s0 <- initialize impl (TrafficCtx 0 False)
Right s1 <- step impl (sMachine s0) (EvExternal (mkEvent_ @'PUSH))
matches @'Green (sMachine s1)   -- True; @'Greeen would not compile
```

## Snapshots that survive chart evolution

`snapshot` serializes a machine to plain JSON — configuration, context,
history, status, plus a structural *fingerprint* of the chart. `restore`
validates a snapshot against the **current** chart and fails precisely:

- `UnknownStates` / `IllegalConfiguration` / `BadContext` / `WrongChart` /
  `UnsupportedVersion` — each reports whether the stored fingerprint matched
  the current chart, distinguishing "stale snapshot after a deploy" from
  "corrupt data".
- Stored history that no longer fits is dropped with a `DroppedHistory` warning,
  not trusted blindly.
- `restoreWith` consults per-failure-mode `Recovery` hooks: `Restart` boots
  fresh (entry actions and invocations run), `ResumeAt` places the machine
  at a chosen configuration (initial children and parallel regions completed
  automatically). `restartRecovery` is the restart-from-fresh policy.

Timers and invocations deliberately re-arm from zero on restore; a snapshot
stores no closures and no in-flight work.

## Visualization

All renderers use the same demoted chart structure and can highlight the active
configuration:

| Renderer | Function | Use |
| --- | --- | --- |
| Stately config | `xstateConfig` / `xstateConfigText` | paste into the [Stately editor](https://stately.ai) to view and simulate the chart |
| Mermaid | `mermaid` / `mermaidHighlight` | READMEs, docs sites |
| Graphviz DOT | `dot` / `dotHighlight` | clustered digraphs |
| HTML | `htmlPage` / `htmlPageSimple` | a single self-contained page (Haskell-computed SVG diagram, transition tables, trace timeline; zero network dependencies) |

## Try it

```bash
cabal run example-traffic
```

runs the traffic light through a pedestrian cycle, a power outage whose
recovery restores the previous phase from history, a snapshot/restore
roundtrip, a rejected stale snapshot, a `Recovery` restart, a typed final
output, and all four renderers.

## Module map

| Module | What lives there |
| --- | --- |
| `StateMachine` | umbrella re-export of the whole surface |
| `StateMachine.Key` | singleton keys: `SKey`, `KeyKind`/`KnownKey`, reify/demote (`demote`, `withKey`, `parseKey`), `deriveKeyKind` |
| `StateMachine.Spec` | the type-level chart DSL (`ChartSpec` kind, `State`/`Compound`/`Parallel`/`Final`/`Hist`, `On … ==> To …`) |
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
