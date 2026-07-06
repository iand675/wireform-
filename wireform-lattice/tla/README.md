# TLA+ model: the Lattice invalidation pipeline

A small, exhaustively checked model of spec §11.5 (invalidation pipeline,
including its asynchronous/out-of-band amendment), §10.6 (soft purge), and
§11.6 / §13.2 g4 (read-your-writes via snapshot tokens).

## What is modeled

One entity, one shared-cache entry, and every timing the protocol permits:

- **Two writers**: a Lattice mutation whose purge is enqueued in a
  transactional outbox *with* its commit, and an out-of-band pipeline
  (producer → topic → consumer → serving store — the "picked up off a
  Kafka topic" case).
- **An asynchronous purge relay** with at-least-once, reorderable delivery:
  purges may be delivered late, repeatedly, and in any order. Delivery
  soft-purges (marks stale); refills read current truth.
- **A client following §11.6**: after mutating, it bypasses the cache
  (`no-cache`) until it observes a snapshot token ≥ its own write.

The constant `PurgeAtIntent` selects where the out-of-band pipeline emits
its purge: at the consumer's truth-commit (conforming) or at the producer's
publish (the tempting shortcut).

## Checked claims

| Invariant | Claim |
|---|---|
| `QuiescentCoherence` | once the topic is drained and all purges delivered, a fresh cache entry holds current truth — i.e. at-least-once reordered purge delivery is *safe* |
| `ReadYourWrites` | the client never renders a version older than its own write, **in both purge modes** — read-your-writes rides the token comparison, not purge timing |
| `TokenMonotone` | a client's own-write token never runs ahead of the store |
| `TypeOK` | state-space sanity |

`LatticeInvalidation.cfg` (purge at truth-commit) passes all four.
`PurgeAtIntent.cfg` (purge at publish) is **expected to fail**: TLC produces
the minimal 5-step stale-refill race — publish (purge enqueued), purge
delivered, a read refills the cache with pre-change data, the change
commits, and no later message ever closes the window. That trace is the
argument for the spec's purge-at-truth-commit rule.

Both invariants are mutation-tested by construction: deleting the §11.6
token guard in `ClientReadHit` fails `ReadYourWrites`; moving the purge
enqueue fails `QuiescentCoherence`.

## Running

```
nix develop          # tlc is in the default dev shell
./check.sh           # runs both configs; exit 0 iff both behave as expected
```

`check.sh` asserts the conforming config passes *and* the broken config
fails with exactly the `QuiescentCoherence` counterexample, so the model's
teeth are themselves regression-checked. State spaces are tiny (~600
states); the whole run takes a couple of seconds.
