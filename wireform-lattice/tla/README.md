# TLA+ corpus: Lattice protocol rules

Small, exhaustively checked models of the Lattice spec rules that exist
because a tempting alternative races. Each conforming config must PASS;
each broken-variant config must FAIL on exactly one named invariant, and
its counterexample trace *is* the spec's argument. `check.sh` asserts both
directions, so the models' teeth are themselves regression-checked.

| Module | Spec sections | Rule defended |
|---|---|---|
| `LatticeInvalidation` | §11.5, §10.6, §11.6, §13.2 g4 | purges enqueue at truth-commit, not intent; read-your-writes rides tokens, not purge timing |
| `LatticeSnapshotFloors` | §10.2, §13.2 g2–g3 | validity floors make cross-slice divergence decidable; the consistent-cut test is sound and economical |
| `LatticePageComposition` | §6.5, §12, §13.2 g7 | composed multi-slice forms are single-snapshot via validate-and-retry |

## LatticeInvalidation — the purge pipeline

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

| Invariant | Claim |
|---|---|
| `QuiescentCoherence` | once the topic is drained and all purges delivered, a fresh cache entry holds current truth — i.e. at-least-once reordered purge delivery is *safe* |
| `ReadYourWrites` | the client never renders a version older than its own write, **in both purge modes** — read-your-writes rides the token comparison, not purge timing |
| `TokenMonotone` | a client's own-write token never runs ahead of the store |

`LatticeInvalidation.cfg` (purge at truth-commit) passes. `PurgeAtIntent.cfg`
fails: TLC produces the minimal 5-step stale-refill race — publish (purge
enqueued), purge delivered, a read refills the cache with pre-change data,
the change commits, and no later message ever closes the window.

## LatticeSnapshotFloors — cross-slice consistent cuts

The Draft 32 consistency mechanism: every response carries a per-domain
validity interval `[Lattice-Snapshot-Floor, Lattice-Snapshot]`; a page
assembly is accepted as a consistent cut iff its slices' intervals
intersect, with K-bounded `no-cache` revalidation as the repair arm
(§13.2 guarantee 3).

Modeled: one snapshot domain; two entities in **disjoint** slices (the
adversarial case — no shared entity, so `ver` conflicts can never fire);
a shared cache aging the pub slice freely; **noise writes** that advance
the token without touching the page (any busy PostgreSQL LSN / Kafka
offset); a floor index with a selectable maintenance discipline; and
optionally **lagged reads** served one token behind truth (a read replica,
a Kafka-fed materialized view).

| Invariant | Claim |
|---|---|
| `AcceptedImpliesCut` | an accepted assembly is witnessed by a single token: cross-request assembly is exactly as consistent as one response (§13.2 g3) |
| `NoiseGeneratesNoTraffic` | the §13.2 MUST NOT, verbatim: convergence traffic only ever follows a write intersecting the page's read set — unrelated commits never manufacture skew |

Configs:

- `LatticeSnapshotFloors.cfg` — conforming (interval detection, exact
  prefix-complete floors, current reads). **PASS**.
- `FloorsLaggedReads.cfg` — reads served from a lagging snapshot with
  floors prefix-complete *at that snapshot*: the replica / log-subscriber
  case. Safety unchanged; the utility invariant is deliberately not
  asserted (genuine lag can exhaust K — the `Lattice-Snapshot-Min` hint
  exists for that). **PASS**.
- `FloorsVerConflictOnly.cfg` — the pre-Draft-32 trigger (act only on a
  `ver` conflict). Disjoint slices never conflict, so skew is accepted
  blind. **FAILS `AcceptedImpliesCut`** — the motivation for floors.
- `FloorsStaleIndex.cfg` — the floor index fed asynchronously *after*
  commit visibility (the natural first implementation over an outbox
  relay). A response whose data snapshot sees a write whose index entry
  hasn't landed under-states its floor and falsely certifies the interval.
  **FAILS `AcceptedImpliesCut`** — this is §10.2's prefix-completeness
  rule: read the outbox under the same storage snapshot as the data
  (PostgreSQL/SQLite), index before visibility (in-process), index as you
  apply (Kafka materialized view), or clamp to the index watermark.
- `FloorsPointIntervals.cfg` — no floor header (floor = token), which is
  also the naive "just compare the snapshots" design. The cut test
  degenerates to token equality and pure noise manufactures skew.
  **FAILS `NoiseGeneratesNoTraffic`** — why floors exist.

A finding the model preserves (see the module header): even the conforming
design can exhaust K and degrade with all-current bodies when a fresh
*intersecting* write lands between every revalidation round. That is the
§13.2 newest-wins degrade arm working as designed under sustained
contention, not a detector defect — which is why the utility invariant
asserts exactly the spec's noise claim and no more.

## LatticePageComposition — the composed forms

§13.2 guarantee 7: a one-shot page response and every page-subscription
burst MUST be composed under a single per-domain snapshot. The reference
origin gets this from plain storage by optimistic validation: observe the
token, execute all slices, observe again, serve only if equal (retry, then
`503 lattice:snapshot-contention` / skip the burst).

| Invariant | Claim |
|---|---|
| `SinglePageSnapshot` | every served page is readable at a single token |

`LatticePageComposition.cfg` (validate-and-retry) passes.
`PageNoValidate.cfg` (serve whatever the reads returned) **fails**: a write
to each entity lands between the two slice executions and the served pair
never coexisted.

## Running

```
nix develop          # tlc is in the default dev shell
./check.sh           # runs every config; exit 0 iff all behave as expected
```

State spaces are tiny (hundreds to ~10k states); the whole run takes
seconds. Every invariant demonstrably fires in at least one broken config,
so none of them is vacuous.
