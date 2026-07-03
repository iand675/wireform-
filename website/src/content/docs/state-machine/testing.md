---
title: Testing with the simulator
description: "simulate a chart with a virtual clock — script timer races, service outcomes, and event sequences deterministically with no threads or wall clock. prettyTrace for readable transcripts; unreachableStates / deadEnds chart lints."
sidebar:
  order: 5
  label: Testing
---

Because the [pure step](../running/#the-pure-step) turns timers and services
into effect *requests*, you can execute them on a **virtual clock** instead of
the wall clock. `StateMachine.Debug.simulate` drives a machine from a script
where time only advances when you say so and invoked services never run until
you settle them — so a timer-versus-timeout race is a deterministic, repeatable
assertion, not a flaky `threadDelay`.

## `simulate`

```haskell
simulate :: (Monad m, EventCodec spec)
         => ChartImpl m spec -> Ctx spec -> [SimCommand spec]
         -> m (Either SimFailure (SimResult spec))

data SimCommand spec
  = SimSend      (EventVal spec)   -- deliver a typed external event
  | SimSendNamed Text Value        -- deliver by name + JSON (the dynamic boundary)
  | SimAdvance   Int               -- move the virtual clock forward N ms
  | SimResolve   Text Value        -- resolve the invocation with this id (onDone)
  | SimReject    Text Value        -- fail the invocation with this id (onError)
```

The result carries everything that happened plus what is still armed:

```haskell
data SimResult spec = SimResult
  { simMachine       :: Machine spec
  , simTrace         :: [MicroTrace]      -- every microstep, init included
  , simEffectLog     :: [EffectReq]       -- full timer/invocation lifecycle
  , simPendingTimers :: [(TimerKey, Int)] -- still armed, with ms remaining
  , simActiveInvokes :: [(Text, Text)]    -- (invoke id, service) awaiting settlement
  }
```

## Timer semantics

`SimAdvance dt` moves the clock forward `dt` ms. While any armed timer is due
within the remaining window, the one with the least time left fires (ties go to
the earliest armed) and its macrostep's effects apply immediately — so a timer
armed *mid-advance* can fire in the same window if it fits. Survivors keep their
reduced remaining time, so two `SimAdvance 50` equal one `SimAdvance 100`.

```haskell
-- A state with `After 100 ==> To "warn"` and `After 250 ==> To "fail"`:
Right r <- simulate impl ctx0
  [ SimAdvance 99 ]      -- nothing fires; simPendingTimers == [(…,1),(…,151)]
matches @"working" (simMachine r)  -- still working

Right r2 <- simulate impl ctx0
  [ SimAdvance 100 ]     -- the 100ms timer fires
matches @"warn" (simMachine r2)    -- True; the 250ms timer was cancelled on exit
```

## Service outcomes

Invocations never run on their own in a simulation — starting one merely
**arms** it, which is what makes a service/timeout race scriptable. Settle it
explicitly:

```haskell
Right r <- simulate impl ctx0
  [ SimSend (mkEvent @"FETCH" url)   -- enters "loading", arms invoke "getUser"
  , SimResolve "getUser" (toJSON response)  -- onDone fires
  ]
matches @"success" (simMachine r)

-- Race the service against its timeout by advancing the clock first:
Right r2 <- simulate impl ctx0
  [ SimSend (mkEvent @"FETCH" url)
  , SimAdvance 30000                 -- After 30000 ==> To "failure" wins
  , SimResolve "getUser" v           -- SimInvokeNotActive: it was cancelled
  ]
```

`SimResolve` / `SimReject` on an invocation that is not armed is a
`SimInvokeNotActive` failure — deliberate strictness that catches a script
resolving something the machine already cancelled, or a typo'd id.

## Failure modes

```haskell
data SimFailure
  = SimStepFault StepFault      -- the step itself faulted (e.g. eventless loop)
  | SimBadEvent String          -- negative SimAdvance, or a zero-delay timer cycle
  | SimInvokeNotActive Text     -- settled an invocation that is not armed
  | SimDecodeFailure Text String -- SimSendNamed with an unknown name / bad payload
```

A machine that **finishes** mid-script stops consuming commands and still
returns `Right` — the finish already cancelled everything armed, so a script
may end with a generous `SimAdvance` without poking a finished machine.

## Readable transcripts

```haskell
prettyTrace   :: [MicroTrace] -> Text        -- what fired, exited, entered, ran
prettyStepped :: Stepped spec -> Text         -- active states + trace + effects
```

`putStrLn (T.unpack (prettyTrace (simTrace r)))` gives an aligned,
multi-line transcript — the fastest way to see why a chart did (or didn't) do
what you expected:

```
1. FETCH
   -> idle #0
   -  idle
   +  loading
   !  logStart
2. done.invoke.getUser output="ok"
   -> loading #0
   -  loading
   +  success
   !  save
```

## Static lints

Two advisory checks over the chart structure — run them in a test to catch
dead corners:

```haskell
unreachableStates :: RChart -> [NodeName]  -- states no path can reach
deadEnds          :: RChart -> [NodeName]  -- atomic non-final states with no exit
```

Get the `RChart` from an implementation with `ciChart impl`. Both are one-sided
(a flagged state is genuinely dead); an empty result is the goal:

```haskell
it "has no orphaned states" $
  unreachableStates (ciChart impl) `shouldBe` []
```

## A note on the pure vs IO test surface

The simulator is the recommended way to test *semantics* — it is pure,
deterministic, and needs no threads. Use the
[IO interpreter](../running/#the-io-interpreter) in tests only when you are
testing the *runtime* itself (real concurrency, subscriptions), and inject
`optDelay` with a gate rather than sleeping. The repo's own suite (79 tests)
drives the pure `step` directly and never uses `threadDelay`.

Next: [persist and restore machine state](../persistence/).
