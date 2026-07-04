{- | Typed statecharts with the specification at the type level.

Use this module for statecharts whose chart, event payloads, implementation
registry, persistence boundary, and visualization data must agree at compile
time.

The public surface is deliberately one-way:

* build a 'ChartImpl' from a chart type with 'chartImpl';
* create a 'Machine' with 'initialize' or 'restore';
* evolve it with 'step' or run it under 'interpret';
* inspect it with 'matches', 'activeKeys', 'status', 'snapshot', and the
  renderers.

The 'Machine' constructor is not exported. An illegal runtime configuration
can only enter from the outside world through 'restore', and restore validates
it before returning a machine.

== Minimal chart

Every chart name except the chart id is a constructor of a user-defined enum.
Each enum is promoted to a kind by @DataKinds@, then 'deriveKeyKind' generates
the singleton constructors and parsing\/rendering dictionaries.

@
-- In a source file: enable GHC2021 plus TemplateHaskell.
import StateMachine

data ToggleState = Off | On
  deriving stock (Show, Eq)

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

toggleImpl :: ChartImpl IO Toggle
toggleImpl = chartImpl RNil RNil RNil RNil (const ())
@

The standalone kind signature is not decoration. It fixes the kind used for
each name role: states, events, guards, actions, services, invoke ids, and
done-data outputs. 'NoKey' marks roles the chart does not use. With that
signature in place, @To 'FLIP@ is a kind error because @'FLIP@ is an event, not
a state; a misspelled state constructor is not in scope; a state constructor
that exists but is not mounted in the chart is rejected by 'ValidChart'.

== Keys at type level, singleton level, and value level

'deriveKeyKind' gives every key three connected forms:

@
'On              -- type-level constructor used in charts and queries
SOn              -- singleton: SKey 'On
On               -- value returned by activeKeys and parseKey
@

Useful conversions:

@
demote @'On              -- On
keyNameOf @'On           -- "On"; snapshots, traces, and renders use this text
withKey On (\\s -> ...)   -- s is the matching singleton
parseKey "On"            -- Maybe (SomeKey ToggleState)
keyUniverse @ToggleState -- all keys in declaration order
@

Use type-level names when you want the compiler to check the state or event:

@
matches @'On machine
step toggleImpl machine (EvExternal (mkEvent_ @'FLIP))
@

Use value-level names when the key came from data, a UI, or storage:

@
matchesKey On machine
activeKeys machine          -- [ToggleState]
availableEventKeys impl m   -- [ToggleEvent]
@

If the value must become a typed event, pattern-match the singleton:

@
withKey evValue $ \\s ->
  case s of
    SFLIP -> mkEventS s ()
@

The branch refines the event name, so the payload type is checked against the
event declaration.

== Implementing named behavior

The chart type declares names. 'chartImpl' supplies their implementations:

@
chartImpl
  guards
  actions
  services
  outputProducers
  finalOutput
@

Each registry is an order-insensitive heterogeneous list. It must contain
exactly the names the chart mentions for that role.

@
data FetchGuard = OutOfRetries
data FetchAction = LogStart | Save
data FetchService = HttpGet

deriveKeyKind ''FetchGuard
deriveKeyKind ''FetchAction
deriveKeyKind ''FetchService

fetchImpl :: ChartImpl IO Fetch
fetchImpl =
  chartImpl
    (mkGuard @'OutOfRetries (\\ctx _ -> retries ctx >= 3) :& RNil)
    (effect @'LogStart (\\_ _ -> putStrLn "fetching") :& assign @'Save saveUser :& RNil)
    (mkService @'HttpGet fetchUser :& RNil)
    RNil
    finalReport
@

Missing, duplicate, or foreign registry entries are compile errors. Using an
action key in the guard registry is a kind error before the completeness check
runs.

Handlers receive typed context and typed events:

@
assign @'Save $ \\ctx ev ->
  case onEvent @'FETCH ev of
    Just url -> ctx{pendingUrl = Just url}
    Nothing  -> ctx
@

Invoke results, invoke errors, and done-data values are also typed; recover
them in handlers with 'invokeOutput', 'invokeError', and 'doneData'.

== Running the machine

'initialize' enters the initial configuration and returns a 'Stepped' value.
'step' processes one 'StepEvent' as a complete SCXML macrostep:

@
main :: IO ()
main = do
  Right boot <- initialize toggleImpl ()
  Right next <- step toggleImpl (sMachine boot) (EvExternal (mkEvent_ @'FLIP))
  print (activeKeys (sMachine next))   -- [On]
@

'Stepped' contains the new machine, a microstep trace, effect requests, and
cross-actor sends:

@
sMachine :: Stepped spec -> Machine spec
sEffects :: Stepped spec -> [EffectReq]
sSends   :: Stepped spec -> [SendReq spec]
sTrace   :: Stepped spec -> [MicroTrace]
@

The step function is pure with respect to timers and services: it returns
requests such as @ReqStartTimer@ and @ReqStartInvoke@ instead of running them.
The same chart can therefore run three ways:

* direct calls to 'initialize' and 'step' for pure tests;
* 'interpret' for the IO runtime with timers, services, subscriptions, and
  child-chart actors;
* 'simulate' for deterministic tests with a virtual clock and scripted service
  outcomes.

== Persistence and rendering

'snapshot' writes the active configuration, context, history, status, and a
structural chart fingerprint to JSON. 'restore' reads that JSON against the
current chart and distinguishes a stale snapshot from corrupt data with
'RestoreError' and 'RestoreWarning'. 'restoreWith' lets you restart or resume
per failure mode.

The renderers all consume the same demoted chart stored in the implementation:

* 'xstateConfig' \/ 'xstateConfigText' — Stately config;
* 'mermaid' \/ 'mermaidHighlight' — Mermaid @stateDiagram-v2@;
* 'dot' \/ 'dotHighlight' — Graphviz DOT;
* 'htmlPage' \/ 'htmlPageSimple' — a self-contained HTML diagram and trace.

== Module map

* "StateMachine.Key" — singleton keys: 'SKey', 'KeyKind', 'KnownKey',
  'deriveKeyKind', 'demote', 'withKey', 'parseKey'.
* "StateMachine.Spec" — chart DSL: states, events, transitions, history,
  invokes, root handlers.
* "StateMachine.Validate" — compile-time well-formedness checks.
* "StateMachine.Registry" — completeness-checked guard\/action\/service\/output
  registries.
* "StateMachine.Step" — pure SCXML macrostep semantics.
* "StateMachine.Interpret" — IO runtime for timers, services, actors, and
  subscriptions.
* "StateMachine.Debug" — deterministic simulator, trace pretty-printers, chart
  lints.
* "StateMachine.Persist" — snapshots, restore, recovery policies.
* "StateMachine.Render.XState", "StateMachine.Render.Mermaid",
  "StateMachine.Render.Dot", "StateMachine.Render.Html" — visualization.
-}
module StateMachine (
  -- * Singleton keys
  module StateMachine.Key,

  -- * Specification language
  module StateMachine.Spec,

  -- * Typed events
  EventVal,
  mkEvent,
  mkEventS,
  mkEvent_,
  eventName,
  matchEvent,
  StepEvent (..),
  stepEventLabel,
  onEvent,
  invokeOutput,
  invokeError,
  doneData,
  EventCodec,
  decodeEvent,

  -- * Implementation registries
  Reg (..),
  GuardE,
  mkGuard,
  ActionE,
  ActionOutcome (..),
  outcome,
  mkAction,
  assign,
  effect,
  raiseEvent,
  SendReq (..),
  SendTarget (..),
  sendSelf,
  sendChild,
  sendParent,
  ServiceE (..),
  ChildBridge (..),
  mkService,
  mkServiceCallback,
  mkServiceChart,
  OutputE,
  mkOutput,
  ChartImpl,
  ciChart,
  chartImpl,

  -- * The machine
  Machine,
  Status (..),
  config,
  context,
  status,
  isFinished,
  matches,
  matchesKey,
  activeStates,
  activeKeys,
  availableEvents,
  availableEventKeys,

  -- * Pure stepping
  initialize,
  step,
  Stepped (..),
  MicroTrace (..),
  StepFault (..),

  -- * Persistence
  Snapshot (..),
  snapshot,
  chartFingerprint,
  restore,
  Restored (..),
  RestoreWarning (..),
  RestoreError (..),
  Recovery (..),
  RecoveryAction (..),
  RestoreOutcome (..),
  noRecovery,
  restartRecovery,
  restoreWith,

  -- * Running in IO
  Interpreter,
  InterpretOptions (..),
  defaultInterpretOptions,
  interpret,
  interpretWith,
  send,
  sendNamed,
  machineView,
  waitFinished,
  halt,
  Notification (..),
  subscribe,

  -- * Deterministic simulation & debugging
  SimCommand (..),
  simResolve,
  simReject,
  SimResult (..),
  SimFailure (..),
  simulate,
  prettyTrace,
  prettyStepped,
  unreachableStates,
  deadEnds,

  -- * Visualization
  xstateConfig,
  xstateConfigText,
  mermaid,
  mermaidHighlight,
  dot,
  dotHighlight,
  htmlPage,
  htmlPageSimple,

  -- * Validation & reification
  ValidChart,
  KnownChart,
  reifyChart,
) where

import StateMachine.Debug
import StateMachine.Event
import StateMachine.Key
import StateMachine.Interpret
import StateMachine.Machine
import StateMachine.Persist
import StateMachine.Registry
import StateMachine.Reify (KnownChart, reifyChart)
import StateMachine.Render.Dot
import StateMachine.Render.Html
import StateMachine.Render.Mermaid
import StateMachine.Render.XState
import StateMachine.Spec
import StateMachine.Step
import StateMachine.Validate (ValidChart)
