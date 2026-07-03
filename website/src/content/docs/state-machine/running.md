---
title: Running a machine
description: "The pure step function, its micro-step trace and effect requests; the IO interpreter with generation-tagged timers, subscriptions, waitFinished, and child-chart actors; the effect-request model that keeps semantics testable."
sidebar:
  order: 4
  label: Running a machine
---

The semantics are the SCXML / W3C statechart algorithm, implemented as a
**pure** function. Timers and invoked services never happen inside the step —
they surface as *effect requests* the caller executes. That one decision lets
the same semantics drive a real-time `IO` runtime and a deterministic
[simulator](../testing/) with no divergence.

## The pure step

```haskell
initialize :: Monad m => ChartImpl m spec -> Ctx spec        -> m (Either StepFault (Stepped spec))
step       :: Monad m => ChartImpl m spec -> Machine spec
                       -> StepEvent spec                     -> m (Either StepFault (Stepped spec))
```

`initialize` enters the initial configuration (running root and initial-state
entry actions, arming their timers/invocations). `step` processes one event as
a complete **macrostep**: it selects transitions, runs exit → transition →
entry actions, records history, raises internal events, and runs eventless
(`Always`) transitions and the raised-event queue to quiescence.

Both return `Either StepFault (Stepped spec)`:

```haskell
data Stepped spec = Stepped
  { sMachine :: Machine spec     -- the new machine
  , sEffects :: [EffectReq]      -- timers/invocations to (un)arm, in order
  , sSends   :: [SendReq spec]   -- cross-actor sends the actions requested
  , sTrace   :: [MicroTrace]     -- one entry per microstep — the debug record
  }
```

A `StepFault` is a genuinely dynamic failure — `EventlessLoop` (an
unguarded `Always` cycle, reported instead of hanging) or an `InternalFault`.
There is no "unknown state" or "missing handler" fault: those were made
impossible at compile time.

### Events

`step` takes a `StepEvent`. The one you construct is `EvExternal`; the rest are
what the algorithm feeds back internally (and what your guards/actions observe):

```haskell
EvExternal (mkEvent @"FETCH" url)   -- an external, typed event
EvExternal (mkEvent_ @"CANCEL")     -- payload-less
-- internal (raised by the engine): EvDone, EvInvokeDone, EvInvokeError, EvTimer, EvInit
```

### Querying the machine

```haskell
matches         :: forall s. (KnownSymbol s, HasState spec s) => Machine spec -> Bool
activeStates    :: Machine spec -> [NodeName]
context         :: Machine spec -> Ctx spec
status          :: Machine spec -> Status spec       -- Running | Finished out
availableEvents :: ChartImpl m spec -> Machine spec -> [Text]
```

`matches @"loading"` is compile-checked; `availableEvents` lists the named
triggers active right now (useful for UI affordances). A `Machine` can *only*
be produced by `initialize`, `step`, or [`restore`](../persistence/) — which is
why an illegal configuration cannot exist behind the API.

### Driving it by hand

```haskell
run :: ChartImpl IO Fetch -> [StepEvent Fetch] -> IO ()
run impl events = do
  Right s0 <- initialize impl initialCtx
  final <- foldM stepOne (sMachine s0) events
  print (status final)
 where
  stepOne m ev = do
    Right stepped <- step impl m ev
    pure (sMachine stepped)
```

You are responsible for the `sEffects` here (arming timers, running services)
— which is exactly what the interpreter and simulator do for you.

## The effect-request model

Each macrostep returns `sEffects :: [EffectReq]`, in order, cancels before
starts:

```haskell
ReqStartTimer  (TimerKey node delayMs docIndex)
ReqCancelTimer (TimerKey node delayMs docIndex)
ReqStartInvoke invokeId serviceName ownerNode
ReqCancelInvoke invokeId
```

To make a delay fire, feed back `EvTimer key` with the exact key from a
`ReqStartTimer`; to resolve an invocation, feed `EvInvokeDone invokeId value`.
A timer's identity is `(node, delay, document-index)`, so two `After 100`s on
one state stay distinct, and a stale timer whose state has since exited is
simply dropped by transition selection.

## The IO interpreter

`StateMachine.Interpret` executes the effect requests for real. One driver
thread owns the machine; you interact through a handle.

```haskell
interpret :: EventCodec spec => ChartImpl IO spec -> Ctx spec
          -> IO (Either StepFault (Interpreter spec))

send        :: Interpreter spec -> EventVal spec       -> IO Bool
sendNamed   :: Interpreter spec -> Text -> Value       -> IO (Either String Bool)
machineView :: Interpreter spec -> IO (Machine spec)
waitFinished:: Interpreter spec -> IO (Either StepFault (Output spec))
halt        :: Interpreter spec -> IO ()
```

```haskell
main :: IO ()
main = do
  Right sm <- interpret impl initialCtx
  _ <- send sm (mkEvent @"FETCH" someUrl)
  -- ...work happens on the driver thread; timers fire on their own...
  out <- waitFinished sm      -- blocks until Finished / fault / halt
  print out
```

- `send` returns `False` once the machine has finished, faulted, or been
  halted. `sendNamed` is the dynamic boundary — decodes a JSON payload against
  the chart's declared events (`Left` on an unknown name or bad payload).
- **Timers** are real (`optDelay` defaults to `threadDelay`) and
  **generation-tagged**: a timer that fired concurrently with its own
  cancellation cannot mis-trigger after the state re-entered.
- `waitFinished` blocks (STM) until a terminal state; `halt` cancels every live
  timer, invocation, and child actor, and is idempotent.

### Observing steps

```haskell
subscribe :: Interpreter spec -> (Notification spec -> IO ()) -> IO (IO ())
data Notification spec
  = NotifyStepped (Stepped spec)   -- a macrostep committed (trace + effects + sends)
  | NotifyFault StepFault          -- terminal
  | NotifyHalted                   -- terminal
```

`subscribe` returns an unsubscribe action. Callbacks run on the driver thread,
so keep them cheap and non-blocking (push to a queue, don't `halt` from inside
one).

### Actors and child charts

An `Invoke` of a `mkServiceChart` spawns the child on its own interpreter,
wired through the invocation's typed [`ChildBridge`](../implementing/#the-type-safe-child-bridge):
the child's `sendParent` events are translated to parent events by
`bridgeToParent`, the parent reaches the child with `sendChild "invokeId"`
events translated by `bridgeToChild`, and the child's typed `Output` becomes
the invocation's `onDone` payload (recovered with `invokeOutput @(Output child)`).
Every hop is a real typed value — no JSON. Cancelling the invoke (exiting the
owner state) halts the child.

### Configuring the runtime

```haskell
interpretWith :: EventCodec spec => InterpretOptions spec -> ChartImpl IO spec
              -> Ctx spec -> IO (Either StepFault (Interpreter spec))

data InterpretOptions spec = InterpretOptions
  { optDelay      :: Int -> IO ()               -- how to wait out `After` (default threadDelay)
  , optSendParent :: Maybe (EventVal spec -> IO ())  -- typed ToParent sink (wired for children)
  }
```

Inject `optDelay` to make timer scenarios deterministic in an integration test
(a gate you release on demand) instead of sleeping.

For pure, no-thread testing of the same semantics — including timer races —
prefer the [simulator](../testing/).
