{- | Dependently typed statecharts — the full XState feature set with the
machine /specification/ at the type level.

@
import StateMachine
@

== The shape of a program

1. __Specify__ the chart as a type ("StateMachine.Spec"): states
   (atomic, compound, parallel, final, history), typed events, guarded\/
   delayed\/eventless transitions, invoked services. Ill-formed charts —
   dangling targets, duplicate states, undeclared events, misplaced
   handlers — do not compile ("StateMachine.Validate").

2. __Implement__ the names the chart mentions with 'chartImpl': guards,
   actions, services, done-data producers. Registration is
   order-insensitive and /complete/ — a missing, duplicated, or foreign
   name is a compile error naming it ("StateMachine.Registry").

3. __Run__ it: purely with 'initialize' \/ 'step' (timers and services
   surface as effect requests), interactively with the IO interpreter
   ("StateMachine.Interpret"), or deterministically in tests with the
   simulator ("StateMachine.Debug").

4. __Persist__ it: 'snapshot' \/ 'restore' with precise errors and
   per-failure 'Recovery' strategies when yesterday's snapshot no longer
   fits today's chart ("StateMachine.Persist").

5. __See__ it: XState\/Stately-importable JSON, Mermaid, Graphviz DOT, or
   a self-contained HTML page ("StateMachine.Render.XState",
   "StateMachine.Render.Mermaid", "StateMachine.Render.Dot",
   "StateMachine.Render.Html").

The 'StateMachine.Machine.Machine' constructor is deliberately /not/
re-exported here: machines are created by 'initialize', evolved by
'step', and revived by 'restore' — which is why an illegal configuration
cannot exist behind this module's API.
-}
module StateMachine (
  -- * Specification language
  module StateMachine.Spec,

  -- * Typed events
  EventVal,
  mkEvent,
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
  activeStates,
  availableEvents,

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
