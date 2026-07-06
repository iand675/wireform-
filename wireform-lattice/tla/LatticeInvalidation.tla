------------------------- MODULE LatticeInvalidation -------------------------
(***************************************************************************)
(* A small model of the Lattice invalidation pipeline (spec section 11.5) *)
(* and its interaction with shared-cache freshness (10.6) and             *)
(* read-your-writes via snapshot tokens (11.6, 13.2 guarantee 4).         *)
(*                                                                         *)
(* The system modeled:                                                     *)
(*                                                                         *)
(*   - One serving store holding a single logical entity; `truth` is its  *)
(*     current version and doubles as the snapshot-domain token (13.1).   *)
(*   - One shared cache entry (one steady-state GET URL) that is either   *)
(*     empty, fresh, or stale (soft-purged; stale-while-revalidate).      *)
(*   - Two writers:                                                       *)
(*       * a Lattice mutation, whose commit transactionally enqueues its  *)
(*         purge (the outbox row of 11.5);                                *)
(*       * an out-of-band pipeline: a producer publishes intent to a      *)
(*         topic, a consumer later commits the change to the serving      *)
(*         store (the Kafka-consumer / batch-writer case of 11.5,         *)
(*         "Out-of-band writes").                                         *)
(*   - An asynchronous purge relay with AT-LEAST-ONCE, reorderable        *)
(*     delivery: a pending purge may be delivered repeatedly, and         *)
(*     delivery order is unconstrained.                                   *)
(*   - Readers: any principal may fill/revalidate the cache; one modeled  *)
(*     client also performs mutations and follows the 11.6 rule (bypass   *)
(*     the cache with no-cache until it sees a token >= its own write).   *)
(*                                                                         *)
(* The constant PurgeAtIntent selects where the out-of-band pipeline      *)
(* enqueues its purge:                                                     *)
(*                                                                         *)
(*   FALSE - purge enqueued atomically with the CONSUMER's commit to the  *)
(*           serving store (the spec's purge-at-truth-commit rule).       *)
(*           All invariants hold.                                          *)
(*   TRUE  - purge enqueued when the PRODUCER publishes (purge at         *)
(*           intent). TLC finds the stale-refill race: purge lands, a     *)
(*           read repopulates the cache with pre-change data, the change  *)
(*           commits afterwards, and no later message ever closes the     *)
(*           window. QuiescentCoherence is violated.                      *)
(***************************************************************************)
EXTENDS Naturals

CONSTANTS
  MaxVer,        \* how many truth changes to explore (3 is plenty)
  PurgeAtIntent  \* BOOLEAN: TRUE models the broken pipeline

ASSUME MaxVer \in Nat \ {0}
ASSUME PurgeAtIntent \in BOOLEAN

VARIABLES
  truth,      \* serving-store version = the snapshot-domain token
  pending,    \* 0|1: an out-of-band change published but not yet consumed
  purges,     \* pending purge deliveries (indistinct: one cache key)
  cacheVer,   \* version of the body held by the shared cache
  cacheState, \* "empty" | "fresh" | "stale"
  written,    \* the modeled client's own-write snapshot token (11.6)
  observed,   \* the last version that client rendered
  rywOk       \* history flag: every client read honored read-your-writes

vars == <<truth, pending, purges, cacheVer, cacheState, written, observed, rywOk>>

TypeOK ==
  /\ truth \in 0..MaxVer
  /\ pending \in 0..1
  /\ purges \in 0..(MaxVer + 1)
  /\ cacheVer \in 0..MaxVer
  /\ cacheState \in {"empty", "fresh", "stale"}
  /\ written \in 0..MaxVer
  /\ observed \in 0..MaxVer
  /\ rywOk \in BOOLEAN

Init ==
  /\ truth = 0
  /\ pending = 0
  /\ purges = 0
  /\ cacheVer = 0
  /\ cacheState = "empty"
  /\ written = 0
  /\ observed = 0
  /\ rywOk = TRUE

(***************************************************************************)
(* Writer 1: a Lattice mutation. The write-set purge is recorded in a     *)
(* transactional outbox IN THE SAME COMMIT as the effect (11.5), so the   *)
(* truth bump and the purge enqueue are one atomic step. The mutation     *)
(* response carries the new token; the client remembers it as `written`.  *)
(***************************************************************************)
Mutate ==
  /\ truth + pending < MaxVer
  /\ truth' = truth + 1
  /\ written' = truth + 1
  /\ purges' = purges + 1
  /\ UNCHANGED <<pending, cacheVer, cacheState, observed, rywOk>>

(***************************************************************************)
(* Writer 2: the out-of-band pipeline (producer -> topic -> consumer ->   *)
(* serving store). Publish is intent; Consume is truth-commit.            *)
(***************************************************************************)
Publish ==
  /\ pending = 0
  /\ truth < MaxVer
  /\ pending' = 1
  /\ purges' = IF PurgeAtIntent THEN purges + 1 ELSE purges
  /\ UNCHANGED <<truth, cacheVer, cacheState, written, observed, rywOk>>

Consume ==
  /\ pending = 1
  /\ truth' = truth + 1
  /\ pending' = 0
  /\ purges' = IF PurgeAtIntent THEN purges ELSE purges + 1
  /\ UNCHANGED <<cacheVer, cacheState, written, observed, rywOk>>

(***************************************************************************)
(* The purge relay: asynchronous, at-least-once, reorderable. Delivery    *)
(* soft-purges (marks stale, 10.6) and MAY leave the message pending      *)
(* (duplicate delivery). Purging is idempotent: re-purging a stale or    *)
(* empty entry is a no-op.                                                 *)
(***************************************************************************)
DeliverPurge ==
  /\ purges > 0
  /\ cacheState' = IF cacheState = "fresh" THEN "stale" ELSE cacheState
  /\ \E left \in {purges, purges - 1} : purges' = left
  /\ UNCHANGED <<truth, pending, cacheVer, written, observed, rywOk>>

(***************************************************************************)
(* Any principal's miss or revalidation: an empty or stale entry is       *)
(* (re)filled from the origin at current truth and becomes fresh.         *)
(***************************************************************************)
CacheFill ==
  /\ cacheState \in {"empty", "stale"}
  /\ cacheVer' = truth
  /\ cacheState' = "fresh"
  /\ UNCHANGED <<truth, pending, purges, written, observed, rywOk>>

(***************************************************************************)
(* The mutating client's read, following 11.6: after a mutation it sends  *)
(* Cache-Control: no-cache until it observes a snapshot token >= its own  *)
(* write, so a cache hit is only taken when the cached token satisfies    *)
(* the client's write. The rywOk flag records that every value the client *)
(* ever rendered respected its own writes; it is the checkable form of    *)
(* "read-your-writes is independent of purge timing".                     *)
(***************************************************************************)
ClientReadHit ==
  /\ cacheState = "fresh"
  /\ cacheVer >= written        \* the 11.6 guard: token satisfied, hit allowed
  /\ observed' = cacheVer
  /\ rywOk' = (rywOk /\ cacheVer >= written)
  /\ UNCHANGED <<truth, pending, purges, cacheVer, cacheState, written>>

ClientReadBypass ==
  /\ cacheState /= "fresh" \/ cacheVer < written
  /\ observed' = truth          \* no-cache: through the shared cache to origin
  /\ cacheVer' = truth          \* which also refreshes the shared copy (11.6)
  /\ cacheState' = "fresh"
  /\ rywOk' = (rywOk /\ truth >= written)
  /\ UNCHANGED <<truth, pending, purges, written>>

Next ==
  \/ Mutate
  \/ Publish
  \/ Consume
  \/ DeliverPurge
  \/ CacheFill
  \/ ClientReadHit
  \/ ClientReadBypass

Spec == Init /\ [][Next]_vars

(***************************************************************************)
(* Invariants                                                              *)
(***************************************************************************)

(* Quiescent coherence: once no change is in flight (topic drained) and   *)
(* every purge has been delivered, a fresh cache entry holds current      *)
(* truth. This is the claim that makes asynchronous, at-least-once,       *)
(* reordered purge delivery SAFE - and it is exactly the claim that       *)
(* purging at intent time breaks (run PurgeAtIntent.cfg for the trace).   *)
QuiescentCoherence ==
  (pending = 0 /\ purges = 0 /\ cacheState = "fresh") => cacheVer = truth

(* Read-your-writes never depended on purge latency: the 11.6 token       *)
(* comparison alone guarantees the client never renders a version older   *)
(* than its own write - in BOTH purge modes. Delete the `cacheVer >=      *)
(* written` guard in ClientReadHit and TLC fails this immediately.        *)
ReadYourWrites == rywOk

(* Tokens are monotone: a client's own-write token never runs ahead of    *)
(* the store (13.2 guarantee 4 needs this shape).                         *)
TokenMonotone == written <= truth

=============================================================================
