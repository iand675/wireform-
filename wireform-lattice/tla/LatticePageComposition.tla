--------------------------- MODULE LatticePageComposition ---------------------------
(***************************************************************************)
(* The composed multi-slice forms (spec sections 6.5 and 12; 13.2          *)
(* guarantee 7): a one-shot page response, and every burst of a page       *)
(* subscription, MUST be composed under a single per-domain storage        *)
(* snapshot. The reference origin achieves this without storage support    *)
(* by optimistic validation: observe the snapshot token, execute every     *)
(* slice, observe the token again, and serve only when the two             *)
(* observations are equal - retrying (bounded) otherwise, then answering   *)
(* 503 lattice:snapshot-contention (pull) or skipping the burst (live).    *)
(*                                                                         *)
(* The model: two entities P and V in different slices, executed as two    *)
(* separate reads with writes free to interleave. ValidateSnapshot         *)
(* selects the conforming validate-and-retry origin (serving only         *)
(* token-stable compositions) or the tempting shortcut that serves         *)
(* whatever the two reads returned.                                        *)
(*                                                                         *)
(*   ValidateSnapshot = TRUE  - only token-stable pages are served;        *)
(*                              SinglePageSnapshot holds.                  *)
(*   ValidateSnapshot = FALSE - TLC finds the mixed-snapshot page: a       *)
(*                              write to each entity lands between the     *)
(*                              two reads, and the served (P, V) pair      *)
(*                              never coexisted at any token.              *)
(***************************************************************************)
EXTENDS Naturals, Sequences, FiniteSets

CONSTANTS
  MaxVer,           \* commit-token budget (3 is plenty)
  ValidateSnapshot  \* BOOLEAN, see header

ASSUME MaxVer \in Nat \ {0}
ASSUME ValidateSnapshot \in BOOLEAN

VARIABLES
  truth,   \* commit counter = snapshot token (13.1)
  pLog,    \* pLog[i] = token at which P's i-th version committed
  vLog,    \* likewise for V
  phase,   \* origin page execution: "idle" | "mid"
  pv0,     \* P version read by the first slice execution
  t0,      \* token observed before the first slice execution
  pageOk   \* history: every served page was a single-snapshot composition

vars == <<truth, pLog, vLog, phase, pv0, t0, pageOk>>

TypeOK ==
  /\ truth \in 0..MaxVer
  /\ pLog \in Seq(1..MaxVer) /\ Len(pLog) <= MaxVer
  /\ vLog \in Seq(1..MaxVer) /\ Len(vLog) <= MaxVer
  /\ phase \in {"idle", "mid"}
  /\ pv0 \in 0..MaxVer /\ t0 \in 0..MaxVer
  /\ pageOk \in BOOLEAN

VerAt(log, T) == Cardinality({i \in DOMAIN log : log[i] <= T})

Coexists(pv, vv) == \E T \in 0..truth : VerAt(pLog, T) = pv /\ VerAt(vLog, T) = vv

Init ==
  /\ truth = 0
  /\ pLog = <<>> /\ vLog = <<>>
  /\ phase = "idle"
  /\ pv0 = 0 /\ t0 = 0
  /\ pageOk = TRUE

WriteP ==
  /\ truth < MaxVer
  /\ truth' = truth + 1
  /\ pLog' = Append(pLog, truth + 1)
  /\ UNCHANGED <<vLog, phase, pv0, t0, pageOk>>

WriteV ==
  /\ truth < MaxVer
  /\ truth' = truth + 1
  /\ vLog' = Append(vLog, truth + 1)
  /\ UNCHANGED <<pLog, phase, pv0, t0, pageOk>>

(* First slice execution: observe the token, read P. *)
PageBegin ==
  /\ phase = "idle"
  /\ pv0' = Len(pLog)
  /\ t0' = truth
  /\ phase' = "mid"
  /\ UNCHANGED <<truth, pLog, vLog, pageOk>>

(* Second slice execution, then the serve decision. The conforming origin  *)
(* validates token stability and retries (PageAbort) on movement; the      *)
(* broken origin serves whatever it read.                                   *)
PageServe ==
  /\ phase = "mid"
  /\ (ValidateSnapshot => truth = t0)
  /\ pageOk' = (pageOk /\ Coexists(pv0, Len(vLog)))
  /\ phase' = "idle"
  /\ UNCHANGED <<truth, pLog, vLog, pv0, t0>>

(* The retry arm of the optimistic loop: the token moved, so the           *)
(* composition is discarded and re-executed from scratch. (Bounding the    *)
(* retries and answering lattice:snapshot-contention is a liveness choice  *)
(* outside this safety model.)                                              *)
PageAbort ==
  /\ phase = "mid"
  /\ truth /= t0
  /\ phase' = "idle"
  /\ UNCHANGED <<truth, pLog, vLog, pv0, t0, pageOk>>

Next ==
  \/ WriteP
  \/ WriteV
  \/ PageBegin
  \/ PageServe
  \/ PageAbort

Spec == Init /\ [][Next]_vars

(***************************************************************************)
(* 13.2 guarantee 7: every served page is readable at a single token.      *)
(* With ValidateSnapshot = FALSE, TLC's counterexample is the mixed page:  *)
(* read P, commit a P write AND a V write, read V - the served pair        *)
(* pairs a pre-write P with a post-write V that never coexisted.           *)
(***************************************************************************)
SinglePageSnapshot == pageOk

=============================================================================
