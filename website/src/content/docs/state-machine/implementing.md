---
title: Implementing a chart
description: "chartImpl and the four completeness-checked registries: guards, actions (assign/effect/raiseEvent), invoked services (promise/callback/child-chart), and done-data producers — with typed context and events inside every handler."
sidebar:
  order: 3
  label: Implementing a chart
---

A chart type names guards, actions, services, and output producers; a
`ChartImpl` supplies them. Registration is **order-insensitive** and
**complete**: a missing name, a duplicate, or a name the chart never mentions
is a compile error naming it — the same type-level permutation check
[`grpc-spec`](../../packages/grpc-spec/) uses for RPC handlers. So a typo'd
guard cannot reach runtime, and the interpreter never has a "handler not found"
path.

## `chartImpl`

```haskell
chartImpl ::
     Reg (GuardE spec)     guardNames    -- guards
  -> Reg (ActionE m spec)  actionNames   -- actions
  -> Reg (ServiceE m spec) serviceNames  -- invoked services
  -> Reg (OutputE spec)    outputNames   -- done-data producers
  -> (Ctx spec -> Output spec)           -- machine output at a top-level final
  -> ChartImpl m spec
```

Each `Reg` is a heterogeneous list built with `:&` and terminated by `RNil`,
listing entries in **any** order. The registry is checked against the exact set
of names the chart mentions:

```haskell
impl :: ChartImpl IO Fetch
impl =
  chartImpl
    ( mkGuard @"canRetry" (\ctx _ -> retries ctx < 3)
        :& RNil )
    ( assign @"incRetries" (\ctx _ -> ctx{retries = retries ctx + 1})
        :& effect @"logStart" (\_ _ -> putStrLn "fetching…")
        :& RNil )
    ( mkService @"httpGet" doFetch
        :& RNil )
    RNil          -- no FinalWith producers
    finalReport   -- Ctx -> Output
```

Drop the guard and you get `Missing guard "canRetry"`; add a `mkGuard @"nope"`
and you get *registered a guard named "nope" that the chart does not mention*.

The monad `m` is yours: `IO`, a reader/state stack, a pure `Identity`, or the
`State` monad for testable action logs. The pure `initialize` / `step` work in
any `Monad`; only the [IO interpreter](../running/#the-io-interpreter) fixes
`m ~ IO`.

## Guards

A guard is a **pure** predicate over the context and the triggering event:

```haskell
mkGuard @"canRetry" :: (Ctx spec -> StepEvent spec -> Bool) -> ...
mkGuard @"canRetry" (\ctx _ev -> retries ctx < 3)
```

Guards must be pure — they are consulted during transition selection, possibly
several times per step. Read the event payload with `onEvent` (below) when the
decision depends on it.

## Actions

Actions run in `m` and return an `ActionOutcome`: the new context, events to
raise into the current step, and cross-actor sends. Three smart constructors
cover the common shapes:

```haskell
-- Pure context update (XState `assign`):
assign @"save" (\ctx ev -> ctx{ result = fromMaybe (result ctx) (invokeOutput @User ev) })

-- Fire-and-forget effect, context unchanged:
effect @"log" (\ctx _ -> logLine ("state changed: " <> show ctx))

-- Raise a follow-up event into the SAME macrostep (XState `raise`):
raiseEvent @"announce" (\_ _ -> mkEvent_ @"READY")
```

For full control (change context *and* raise *and* send), use `mkAction` and
build the `ActionOutcome` yourself:

```haskell
mkAction @"complete" $ \ctx ev -> do
  persist ctx
  pure (outcome ctx){ aoRaised = [mkEvent_ @"SAVED"] }
```

`ActionOutcome { aoCtx, aoRaised, aoSends }` — `outcome ctx` is the identity
(new context, nothing raised, nothing sent).

## Reading the event inside a handler

Both guards and actions receive the `StepEvent`. Project it type-safely:

```haskell
onEvent      :: forall e. StepEvent spec -> Maybe (EventPayload spec e)
invokeOutput :: Typeable a => StepEvent spec -> Maybe a  -- an onDone result, at its real type
invokeError  :: Typeable a => StepEvent spec -> Maybe a  -- an onError value
doneData     :: Typeable a => StepEvent spec -> Maybe a  -- a compound's done payload
```

`onEvent @"FETCH" ev` returns `Just url` when the step was driven by a
`"FETCH"`, and the payload has exactly the declared type — a `Url`, not a
`Value`:

```haskell
assign @"remember" $ \ctx ev ->
  case onEvent @"FETCH" ev of
    Just url -> ctx{ pending = Just url }
    Nothing  -> ctx
```

## Invoked services

An `Invoke` in the chart starts a service on state entry and cancels it on
exit. There are three service kinds; pick a name-pinning constructor:

```haskell
-- Promise: run to completion. Right resolves (onDone), Left fails (onError).
-- Output and error are ordinary typed values — no JSON.
mkService @"httpGet" $ \ctx _ev ->
  tryFetch (url ctx)          -- :: m (Either FetchError User)

-- Callback: like promise, but may send typed events back while running.
mkServiceCallback @"ticker" $ \_ctx _ev emit -> do
  forM_ [1..3] $ \_ -> emit (mkEvent_ @"TICK") >> wait
  pure (Right (3 :: Int))     -- typed output

-- Child chart: invoke another machine as an actor, bridged type-safely.
mkServiceChart @"subflow" childImpl ChildBridge
  { bridgeCtx      = \ctx _ev -> deriveChildCtx ctx
  , bridgeToChild  = \parentEv -> ...  -- Maybe (EventVal Child)
  , bridgeToParent = \childEv  -> ...  -- Maybe (EventVal Parent)
  }
```

The output and error are real Haskell types: the service returns
`m (Either err out)`, and the `onDone`/`onError` handler recovers them at
their type with `invokeOutput @out` / `invokeError @err` — the value is the
actual object, never serialized. (`out` and `err` need `Typeable`, which is
automatic.)

### The type-safe child bridge

A child chart is a different `spec` with its own event and context types, so
the bridge — the `ChildBridge parent child` you hand `mkServiceChart` — is
where the two typed worlds meet:

- `bridgeCtx` derives the child's initial context when the invocation starts.
- `bridgeToChild` translates a parent `sendChild` into a *child* event
  (`Maybe (EventVal child)`; `Nothing` drops it).
- `bridgeToParent` translates a child `sendParent` into a *parent* event.
- The child's typed `Output child` becomes the invocation's onDone payload,
  recovered with `invokeOutput @(Output child)`.

Nothing crosses the boundary as JSON; both directions are total typed
translations. (`Output child` needs `Typeable`.)

## Done-data producers

A `FinalWith name producer` state carries data on its parent's done event; the
producer computes it — as a typed value — from the context at the moment the
final state is entered:

```haskell
-- chart: Compound "job" "run" '[ ..., FinalWith "ok" "summary" ] '[]
-- registry:
mkOutput @"summary" (\ctx _ev -> report ctx)   -- report ctx :: Report
```

Downstream, an `OnDoneOf "job"` transition's action reads it with
`doneData @Report`.

## Cross-actor sends

An action's `aoSends` carries `SendReq { srTarget, srEvent }` where `srEvent`
is a /typed/ `EventVal` of the sender's chart. Build one with `sendSelf ev`
(a fresh macrostep on this machine), `sendChild "invokeId" ev` (to an invoked
child chart — translated by its `ChildBridge`), or `sendParent ev` (to the
invoking machine). The [IO interpreter](../running/#actors-and-child-charts)
routes them; the pure step just records them. No serialization.

Next: [run the machine](../running/).
