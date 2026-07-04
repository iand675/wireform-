---
title: Testing with the simulator
description: "simulate a chart with a virtual clock — script timer races, service outcomes, and event sequences deterministically with no threads or wall clock. prettyTrace for readable transcripts; unreachableStates / deadEnds chart lints."
sidebar:
  order: 5
  label: Testing
---

Because the [pure step](../running/#the-pure-step) turns timers and services
into effect *requests*, execute them on a **virtual clock** instead of the wall clock.
`StateMachine.Debug.simulate` drives a machine from a script: time advances only
through `SimAdvance`, and services resolve only through explicit settlement
commands. Timer/service races become deterministic assertions, not sleeps.

## `simulate`

```haskell
simulate :: (Monad m, EventCodec spec)
         => ChartImpl m spec -> Ctx spec -> [SimCommand spec]
         -> m (Either SimFailure (SimResult spec))

data SimCommand spec
  = SimSend      (EventVal spec)   -- deliver a typed external event (mkEvent @'NAME)
  | SimSendNamed Text Value        -- deliver by name + payload at the dynamic boundary
  | SimAdvance   Int               -- move the virtual clock forward N ms
  | SimResolve   Text Dynamic      -- resolve the invocation with this id (onDone)
  | SimReject    Text Dynamic      -- fail the invocation with this id (onError)

-- typed settlement helpers — build the Dynamic for you:
simResolve :: Typeable a => Text -> a -> SimCommand spec
simReject  :: Typeable a => Text -> a -> SimCommand spec
```

The result contains the committed trace and the remaining armed effects:

```haskell
data SimResult spec = SimResult
  { simMachine       :: Machine spec
  , simTrace         :: [MicroTrace]      -- every microstep, init included
  , simEffectLog     :: [EffectReq]       -- timer/invocation lifecycle
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
-- A state with `After 100 ==> To 'Warn` and `After 250 ==> To 'Fail`:
Right r <- simulate impl ctx0
  [ SimAdvance 99 ]      -- nothing fires; simPendingTimers == [(…,1),(…,151)]
matches @'Working (simMachine r)  -- still working

Right r2 <- simulate impl ctx0
  [ SimAdvance 100 ]     -- the 100ms timer fires
matches @'Warn (simMachine r2)     -- True; the 250ms timer was cancelled on exit
```

## Service outcomes

Invocations do not run on their own in a simulation. Starting one **arms** it;
settlement remains explicit, which makes service/timeout races scriptable. Settle it
explicitly, addressed by the invoke key's wire spelling and carrying a *typed*
value recovered by `onDone` consumers with `invokeOutput`:

```haskell
Right r <- simulate impl ctx0
  [ SimSend (mkEvent @'FETCH url)   -- enters 'Loading, arms invoke "GetUser"
  , simResolve "GetUser" response   -- onDone fires
  ]
matches @'Success (simMachine r)

-- Race the service against its timeout by advancing the clock first:
Right r2 <- simulate impl ctx0
  [ SimSend (mkEvent @'FETCH url)
  , SimAdvance 30000                 -- After 30000 ==> To 'Failure wins
  , simResolve "GetUser" v           -- SimInvokeNotActive: it was cancelled
  ]
```

`SimResolve` / `SimReject` on an invocation that is not armed is a
`SimInvokeNotActive` failure — deliberate strictness that catches a script
resolving something the machine already cancelled, or a misspelled id.

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
multi-line transcript — a compact way to inspect transition selection and action order. Every name is a key's constructor spelling; event payloads
print via `Show`, lifecycle values by their `TypeRep`:

```
1. EventVal "FETCH" "https://api.example.com/users/1"
   -> Idle #0
   -  Idle
   +  Loading
   !  LogStart
2. done.invoke.GetUser <<User>>
   -> Loading #0
   -  Loading
   +  Success
   !  Save
```

## Static lints

Two advisory checks over the chart structure — run them in a test to catch
dead states:

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
`optDelay` with a gate rather than sleeping. The repo's own suite drives the
pure `step` directly and never uses `threadDelay`.

Next: [persist and restore machine state](../persistence/).
