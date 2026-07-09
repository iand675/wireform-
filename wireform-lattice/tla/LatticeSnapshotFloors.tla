---------------------------- MODULE LatticeSnapshotFloors ----------------------------
(***************************************************************************)
(* Cross-slice consistency via validity floors (spec sections 10.2 and    *)
(* 13.2 guarantees 2-3): the slices of one logical page are fetched as    *)
(* separate HTTP responses, each carrying a per-domain validity interval  *)
(* [Lattice-Snapshot-Floor, Lattice-Snapshot]. The client accepts the     *)
(* assembly as a consistent cut iff the intervals intersect               *)
(* (max floors <= min tokens), revalidating the losing slices at most     *)
(* MaxRevalidate times before degrading.                                  *)
(*                                                                         *)
(* The system modeled:                                                     *)
(*                                                                         *)
(*   - One storage domain; `truth` is its commit counter and doubles as   *)
(*     the snapshot token (13.1).                                          *)
(*   - Two entities in disjoint slices: P (pub slice, behind a shared     *)
(*     cache) and V (priv slice, always origin-served). Disjointness is   *)
(*     the adversarial case: no shared entity means the 13.2-guarantee-3  *)
(*     `ver`-conflict corollary can never fire.                            *)
(*   - Noise writes: commits that touch neither P nor V. They advance     *)
(*     the token the way any busy database (PostgreSQL LSN, Kafka         *)
(*     offset) does, and are what makes naive token comparison useless.   *)
(*   - A floor index: last intersecting invalidation per surrogate key.   *)
(*     IndexAtCommit selects its maintenance discipline (below).          *)
(*   - A client assembling the page: pub from cache or origin, priv from  *)
(*     origin, then the 13.2 decision procedure.                          *)
(*                                                                         *)
(* Constants select the conforming design or a tempting-but-broken        *)
(* variant:                                                                *)
(*                                                                         *)
(*   Detection = "intervals"      - the 13.2 guarantee-3 cut test.        *)
(*             | "verconflict"    - drafts before 32: act only on a ver   *)
(*                                  conflict. Disjoint slices never       *)
(*                                  conflict, so every skew is accepted:  *)
(*                                  AcceptedImpliesCut is violated.       *)
(*             | "pointintervals" - no floor header (floor = token); the  *)
(*                                  cut test degenerates to token         *)
(*                                  equality, and pure noise writes       *)
(*                                  manufacture skew that is not there:   *)
(*                                  NoiseGeneratesNoTraffic is violated.  *)
(*                                  This is both the "just compare the    *)
(*                                  tokens" design and what a floor-less  *)
(*                                  origin degrades to.                   *)
(*                                                                         *)
(*   IndexAtCommit = TRUE  - floors observe every invalidation at or      *)
(*                           below the response's snapshot token: the     *)
(*                           outbox is read under the same storage        *)
(*                           snapshot as the data (PostgreSQL/SQLite), or *)
(*                           the index is updated before the write        *)
(*                           becomes visible (in-process, Kafka           *)
(*                           materialized view indexing as it applies).   *)
(*                 = FALSE - the index is fed asynchronously after commit *)
(*                           visibility (a lagging relay). A response can *)
(*                           then under-state its floor and certify an    *)
(*                           interval containing an unindexed intersect-  *)
(*                           ing write: AcceptedImpliesCut is violated.   *)
(*                           This is the 10.2 prefix-completeness rule.   *)
(*                                                                         *)
(*   LaggedReads = TRUE - reads may be served from a snapshot one token   *)
(*                        behind truth (a read replica, a Kafka consumer  *)
(*                        applying the log). Floors stay prefix-complete  *)
(*                        at the served snapshot, so safety must hold     *)
(*                        unchanged; convergence utility is what suffers, *)
(*                        which is what the Lattice-Snapshot-Min request  *)
(*                        hint exists for.                                 *)
(*                                                                         *)
(* A finding this model preserves: even the conforming design can exhaust *)
(* K and degrade while both held bodies happen to be current, when a      *)
(* fresh INTERSECTING write lands between every revalidation round (the   *)
(* last refetched interval cannot cover the other slice's later token).   *)
(* That is the 13.2 "render newest-wins and surface residual staleness"   *)
(* arm working as designed under sustained contention - not a detector    *)
(* defect - so the utility invariant below asserts exactly what the spec  *)
(* asserts: traffic requires an intersecting write, never noise alone.    *)
(***************************************************************************)
EXTENDS Naturals, Sequences, FiniteSets

CONSTANTS
  MaxVer,        \* commit-token budget (4 is plenty)
  MaxRevalidate, \* the 13.2 guarantee-3 K (default 2)
  Detection,     \* "intervals" | "verconflict" | "pointintervals"
  IndexAtCommit, \* BOOLEAN, see header
  LaggedReads    \* BOOLEAN, see header

ASSUME MaxVer \in Nat \ {0}
ASSUME MaxRevalidate \in Nat
ASSUME Detection \in {"intervals", "verconflict", "pointintervals"}
ASSUME IndexAtCommit \in BOOLEAN
ASSUME LaggedReads \in BOOLEAN

VARIABLES
  truth,     \* commit counter = snapshot token (13.1)
  pLog,      \* pLog[i] = token at which P's i-th version committed
  vLog,      \* likewise for V
  idxP,      \* async floor index: last indexed P-key invalidation token
  idxV,      \* likewise for V's key
  cache,     \* shared-cache pub entry: [ver, tok, flr]
  cphase,    \* client assembly phase: "start" | "havePub" | "decide"
  rp,        \* client-held pub response  [ver, tok, flr]
  rv,        \* client-held priv response [ver, tok, flr]
  reval,     \* revalidation rounds spent on the current assembly
  cutOk,     \* history: every accepted assembly was a consistent cut
  trafficOk  \* history: convergence traffic only ever followed an
             \* intersecting write, never noise alone

vars == <<truth, pLog, vLog, idxP, idxV, cache, cphase, rp, rv, reval, cutOk, trafficOk>>

Resp == [ver : 0..MaxVer, tok : 0..MaxVer, flr : 0..MaxVer]

TypeOK ==
  /\ truth \in 0..MaxVer
  /\ pLog \in Seq(1..MaxVer) /\ Len(pLog) <= MaxVer
  /\ vLog \in Seq(1..MaxVer) /\ Len(vLog) <= MaxVer
  /\ idxP \in 0..MaxVer /\ idxV \in 0..MaxVer
  /\ cache \in Resp /\ rp \in Resp /\ rv \in Resp
  /\ cphase \in {"start", "havePub", "decide"}
  /\ reval \in 0..MaxRevalidate
  /\ cutOk \in BOOLEAN /\ trafficOk \in BOOLEAN

MaxN(a, b) == IF a >= b THEN a ELSE b
MinN(a, b) == IF a <= b THEN a ELSE b

(* Version of an entity visible at token T: how many of its commits are <= T. *)
VerAt(log, T) == Cardinality({i \in DOMAIN log : log[i] <= T})

(* The exact floor at read snapshot R: the newest intersecting commit <= R.  *)
(* This is what "read the outbox under the same snapshot as the data" gives. *)
ExactFloor(log, R) == IF VerAt(log, R) = 0 THEN 0 ELSE log[VerAt(log, R)]

(* The floor a response actually carries, per variant. The async index is    *)
(* clamped to the read snapshot (an entry past R claims nothing about R).    *)
FloorOf(log, idx, R) ==
  CASE Detection = "pointintervals" -> R
    [] IndexAtCommit                -> ExactFloor(log, R)
    [] OTHER                        -> MinN(idx, R)

(* Snapshots a read may be served from: current, or (replica/subscriber      *)
(* lag) one token behind.                                                     *)
ReadToks == IF LaggedReads THEN {truth, MaxN(truth - 1, 0)} ELSE {truth}

FreshP(R) == [ver |-> VerAt(pLog, R), tok |-> R, flr |-> FloorOf(pLog, idxP, R)]
FreshV(R) == [ver |-> VerAt(vLog, R), tok |-> R, flr |-> FloorOf(vLog, idxV, R)]

(* The 13.2 guarantee-3 claim being checked: an accepted assembly must be    *)
(* readable as a single response - some token at which both rendered fact    *)
(* sets held simultaneously.                                                  *)
Coexists(pv, vv) == \E T \in 0..truth : VerAt(pLog, T) = pv /\ VerAt(vLog, T) = vv

(* No write has ever intersected the page's read set: any convergence        *)
(* traffic in this condition is manufactured by the detector.                *)
NoiseOnly == pLog = <<>> /\ vLog = <<>>

Init ==
  /\ truth = 0
  /\ pLog = <<>> /\ vLog = <<>>
  /\ idxP = 0 /\ idxV = 0
  /\ cache = [ver |-> 0, tok |-> 0, flr |-> 0]
  /\ cphase = "start"
  /\ rp = [ver |-> 0, tok |-> 0, flr |-> 0]
  /\ rv = [ver |-> 0, tok |-> 0, flr |-> 0]
  /\ reval = 0
  /\ cutOk = TRUE /\ trafficOk = TRUE

(***************************************************************************)
(* Writers. A write to P or V commits atomically with its outbox row       *)
(* (11.5); whether the FLOOR INDEX sees it atomically is IndexAtCommit.    *)
(* Noise commits touch neither entity - other tenants, other tables: the   *)
(* token moves, the page's floors do not.                                   *)
(***************************************************************************)
WriteP ==
  /\ truth < MaxVer
  /\ truth' = truth + 1
  /\ pLog' = Append(pLog, truth + 1)
  /\ UNCHANGED <<vLog, idxP, idxV, cache, cphase, rp, rv, reval, cutOk, trafficOk>>

WriteV ==
  /\ truth < MaxVer
  /\ truth' = truth + 1
  /\ vLog' = Append(vLog, truth + 1)
  /\ UNCHANGED <<pLog, idxP, idxV, cache, cphase, rp, rv, reval, cutOk, trafficOk>>

Noise ==
  /\ truth < MaxVer
  /\ truth' = truth + 1
  /\ UNCHANGED <<pLog, vLog, idxP, idxV, cache, cphase, rp, rv, reval, cutOk, trafficOk>>

(* The asynchronous index relay catching up (only meaningful when           *)
(* IndexAtCommit is FALSE; the conforming variant never reads idx).         *)
ApplyIndex ==
  /\ ~IndexAtCommit
  /\ (idxP /= ExactFloor(pLog, truth)) \/ (idxV /= ExactFloor(vLog, truth))
  /\ idxP' = ExactFloor(pLog, truth)
  /\ idxV' = ExactFloor(vLog, truth)
  /\ UNCHANGED <<truth, pLog, vLog, cache, cphase, rp, rv, reval, cutOk, trafficOk>>

(***************************************************************************)
(* The client's page assembly. Pub is taken from the shared cache (aged    *)
(* freely - a CDN serving within TTL) or from the origin, which also       *)
(* refills the cache; priv always goes to the origin.                      *)
(***************************************************************************)
FetchPubCached ==
  /\ cphase = "start"
  /\ rp' = cache
  /\ cphase' = "havePub"
  /\ UNCHANGED <<truth, pLog, vLog, idxP, idxV, cache, rv, reval, cutOk, trafficOk>>

FetchPubOrigin ==
  /\ cphase = "start"
  /\ \E R \in ReadToks :
       /\ rp' = FreshP(R)
       /\ cache' = FreshP(R)
  /\ cphase' = "havePub"
  /\ UNCHANGED <<truth, pLog, vLog, idxP, idxV, rv, reval, cutOk, trafficOk>>

FetchPriv ==
  /\ cphase = "havePub"
  /\ \E R \in ReadToks : rv' = FreshV(R)
  /\ cphase' = "decide"
  /\ UNCHANGED <<truth, pLog, vLog, idxP, idxV, cache, rp, reval, cutOk, trafficOk>>

(* The 13.2 guarantee-3 decision procedure, per Detection variant. With    *)
(* disjoint slices a ver conflict is impossible, so "verconflict" accepts  *)
(* everything - which is exactly its defect.                                *)
Accepts ==
  CASE Detection = "verconflict" -> TRUE
    [] OTHER -> MaxN(rp.flr, rv.flr) <= MinN(rp.tok, rv.tok)

Accept ==
  /\ cphase = "decide"
  /\ Accepts
  /\ cutOk' = (cutOk /\ Coexists(rp.ver, rv.ver))
  /\ cphase' = "start"
  /\ reval' = 0
  /\ UNCHANGED <<truth, pLog, vLog, idxP, idxV, cache, rp, rv, trafficOk>>

(* Revalidate the slices whose token lies below the page's greatest floor  *)
(* (no-cache through the shared cache: the refetch refreshes the cached    *)
(* entry too), then re-decide.                                              *)
Revalidate ==
  /\ cphase = "decide"
  /\ ~Accepts
  /\ reval < MaxRevalidate
  /\ reval' = reval + 1
  /\ trafficOk' = (trafficOk /\ ~NoiseOnly)
  /\ LET maxF == MaxN(rp.flr, rv.flr) IN
       /\ \E R \in ReadToks :
            IF rp.tok < maxF
              THEN rp' = FreshP(R) /\ cache' = FreshP(R)
              ELSE rp' = rp /\ cache' = cache
       /\ \E R \in ReadToks :
            IF rv.tok < maxF THEN rv' = FreshV(R) ELSE rv' = rv
  /\ UNCHANGED <<truth, pLog, vLog, idxP, idxV, cphase, cutOk>>

(* K exhausted: render newest-wins and surface staleness (13.2).            *)
Degrade ==
  /\ cphase = "decide"
  /\ ~Accepts
  /\ reval = MaxRevalidate
  /\ trafficOk' = (trafficOk /\ ~NoiseOnly)
  /\ cphase' = "start"
  /\ reval' = 0
  /\ UNCHANGED <<truth, pLog, vLog, idxP, idxV, cache, rp, rv, cutOk>>

Next ==
  \/ WriteP
  \/ WriteV
  \/ Noise
  \/ ApplyIndex
  \/ FetchPubCached
  \/ FetchPubOrigin
  \/ FetchPriv
  \/ Accept
  \/ Revalidate
  \/ Degrade

Spec == Init /\ [][Next]_vars

(***************************************************************************)
(* Invariants                                                               *)
(***************************************************************************)

(* 13.2 guarantee 3 soundness: an assembly the client accepted as a        *)
(* consistent cut really was one - some single token witnesses both        *)
(* rendered bodies. Violated by Detection = "verconflict" (disjoint-slice  *)
(* skew is invisible to ver conflicts) and by IndexAtCommit = FALSE (a     *)
(* floor computed from a lagging index under-states the last intersecting  *)
(* write; 10.2 prefix-completeness).                                        *)
AcceptedImpliesCut == cutOk

(* Detector economics - the 13.2 MUST NOT, verbatim: "absent an overlap    *)
(* failure, divergence is unobservable and MUST NOT generate traffic".     *)
(* Convergence traffic (revalidation or degrade) only ever follows a       *)
(* write that intersected the page's read set; unrelated commits moving    *)
(* the token (a busy LSN) never manufacture skew. Violated by              *)
(* Detection = "pointintervals", where the cut test collapses to token     *)
(* equality - which is why floors exist.                                    *)
NoiseGeneratesNoTraffic == trafficOk

=============================================================================
