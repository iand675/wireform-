---
title: Persistence & recovery
description: "snapshot a machine to JSON and restore it later, with a chart fingerprint that distinguishes stale-after-deploy from corrupt data, a precise RestoreError taxonomy, and per-failure Recovery strategies (Restart / ResumeAt) for evolving charts."
sidebar:
  order: 6
  label: Persistence & recovery
---

A long-lived machine may outlive both its process and the chart version that
created it. `StateMachine.Persist` serializes a machine to JSON and restores it
against the current chart. Restore errors distinguish stale snapshots from
corrupt data, and recovery hooks can handle each case separately.

## Snapshotting

```haskell
snapshot :: (ToJSON (Ctx spec), ToJSON (Output spec))
         => ChartImpl m spec -> Machine spec -> Snapshot

chartFingerprint :: RChart -> Text
```

A `Snapshot` is ordinary JSON — the active configuration, the context, recorded
history, the run status, and a **structural fingerprint** of the chart:

```haskell
let snap = snapshot impl machine
BL.writeFile "state.json" (encode snap)
```

For the traffic-light demo mid-cycle, that file reads:

```json
{
  "version": 1,
  "chart": "traffic",
  "fingerprint": "8c50f1f56e3a9d42",
  "configuration": ["Operational", "Red"],
  "context": { "cycles": 1, "pedestrianWaiting": false },
  "history": {},
  "status": "running"
}
```

The names in `configuration` (and `history`) are the state keys' wire names —
their `keyName` spellings, i.e. the constructor names verbatim
(`keyNameOf @'Red == "Red"`); `parseKey` is the inverse. `"traffic"` is the
chart's display name, the one `Symbol` a chart still carries.

The fingerprint is a stable hash (FNV-1a over a canonical rendering of the
chart's states, hierarchy, transitions, and events). It changes exactly when
the chart's shape changes, and it is what lets `restore` classify failures.

Two restore rules are deliberate: a snapshot stores **no closures and no in-flight
work** — timers and invocations re-arm from zero on restore (see
`restoredEffects` below); and history is treated as an *optimization*, not
truth (stale history is dropped with a warning rather than promoted to an error).

## Restoring

```haskell
restore :: (FromJSON (Ctx spec), FromJSON (Output spec))
        => ChartImpl m spec -> Snapshot -> Either RestoreError (Restored spec)

data Restored spec = Restored
  { restoredMachine  :: Machine spec
  , restoredWarnings :: [RestoreWarning]
  , restoredEffects  :: [EffectReq]  -- re-arm requests for the live config; hand to the interpreter
  }
```

```haskell
case restore impl snap of
  Right r  -> resumeFrom (restoredMachine r) (restoredEffects r)
  Left err -> handle err
```

`restoredEffects` re-arms the timers and invocations for the restored
configuration; feed them to the interpreter exactly as you would a step's
`sEffects` (they are empty for a `Finished` machine).

### Restore errors

Every `RestoreError` carries `reFingerprintMatched` — the difference between a
stale snapshot and a broken one:

| Error | Meaning |
| --- | --- |
| `WrongChart` | The snapshot belongs to a different chart (name mismatch). |
| `UnsupportedVersion` | A snapshot format this build does not read. |
| `UnknownStates` | Configuration members that are not states of the current chart. |
| `IllegalConfiguration` | The states exist but do not form a legal configuration (e.g. two active children of one compound, a missing parallel region, a history node marked active, a missing ancestor). |
| `BadContext` / `BadOutput` | The stored context / output no longer parses. |

`UnknownStates` with `reFingerprintMatched == False` is the ordinary
"snapshot from before a deploy" case; the same error with the fingerprint
*matched* means corruption or a foreign snapshot. Non-fatal observations come
back as `RestoreWarning`s (`FingerprintChanged`, `DroppedHistory`).

## Recovery strategies

`restoreWith` consults a `Recovery` — a hook per failure mode — before giving
up:

```haskell
restoreWith :: (Monad m, FromJSON (Ctx spec), FromJSON (Output spec))
            => ChartImpl m spec -> Recovery spec -> Snapshot
            -> m (Either RestoreError (RestoreOutcome m spec))

data RestoreOutcome m spec
  = Intact             (Restored spec)             -- clean restore
  | RecoveredByRestart (Stepped spec)  RestoreError -- a Restart hook fired
  | RecoveredByResume  (Restored spec) RestoreError -- a ResumeAt hook fired
```

Each hook returns a `RecoveryAction`, or `Nothing` to fall through to the
original error:

```haskell
data RecoveryAction spec
  = Restart  (Ctx spec)              -- discard the snapshot, initialize fresh
  | ResumeAt [NodeName] (Ctx spec)   -- place the machine at these states
```

`ResumeAt` completes the configuration for you — name a compound and its
initial child is entered; name a parallel state and every region is entered.
States are named by their wire spelling; `keyNameOf @'Red` produces one with
the spelling compile-checked.

### Common policies

```haskell
-- Recover nothing (equivalent to plain `restore`):
noRecovery :: Recovery spec

-- Restart from fresh context for every failure mode:
restartRecovery :: Ctx spec -> Recovery spec
```

```haskell
out <- restoreWith impl (restartRecovery freshCtx) snap
case out of
  Right (Intact r)                 -> resume r
  Right (RecoveredByRestart s err) -> log err >> resume' s   -- booted fresh
  Right (RecoveredByResume r err)  -> log err >> resume r
  Left err                         -> giveUp err
```

For finer control, build a `Recovery` with per-mode hooks
(`onUnknownStates`, `onIllegalConfiguration`, `onBadContext`, …) — e.g. restart
on an unknown state (a removed feature) but fail loudly on a matched-fingerprint
corruption.

## Worked example

The `example-traffic` demo (`cabal run example-traffic`) performs the
round-trip: snapshot the running light, restore it, then feed it a snapshot
built for a chart that no longer has one of its states. `restore` returns
`UnknownStates { reFingerprintMatched = False }`, and
`restoreWith (restartRecovery …)` recovers by rebooting.

Next: [visualize the chart](../visualization/).
