#!/usr/bin/env bash
# Model-check the Lattice TLA+ corpus: the invalidation pipeline (spec
# section 11.5), validity floors / cross-slice consistent cuts (10.2, 13.2),
# and single-snapshot page composition (6.5, 12, 13.2 guarantee 7).
#
#   ./check.sh          # runs every config; exit 0 iff all behave as expected
#
# Requires `tlc` on PATH (in the repo dev shell: `nix develop`).
#
# Conforming configs must PASS. Broken-variant configs must FAIL on exactly
# the named invariant - their counterexample traces ARE the spec's rationale
# (each config's header comment says which rule it defends). A broken config
# that stops failing is model rot and fails this script.
set -uo pipefail
cd "$(dirname "$0")"

TLC=${TLC:-tlc}
command -v "$TLC" >/dev/null || { echo "tlc not found; enter the dev shell (nix develop)"; exit 1; }

#        module                      config                        expectation
RUNS=(
  "LatticeInvalidation.tla    LatticeInvalidation.cfg      PASS -"
  "LatticeInvalidation.tla    PurgeAtIntent.cfg            FAIL QuiescentCoherence"
  "LatticeSnapshotFloors.tla  LatticeSnapshotFloors.cfg    PASS -"
  "LatticeSnapshotFloors.tla  FloorsLaggedReads.cfg        PASS -"
  "LatticeSnapshotFloors.tla  FloorsVerConflictOnly.cfg    FAIL AcceptedImpliesCut"
  "LatticeSnapshotFloors.tla  FloorsStaleIndex.cfg         FAIL AcceptedImpliesCut"
  "LatticeSnapshotFloors.tla  FloorsPointIntervals.cfg     FAIL NoiseGeneratesNoTraffic"
  "LatticePageComposition.tla LatticePageComposition.cfg   PASS -"
  "LatticePageComposition.tla PageNoValidate.cfg           FAIL SinglePageSnapshot"
)

run_tlc() {
  "$TLC" -config "$2" -metadir "$(mktemp -d)" -cleanup "$1" 2>&1
}

failures=0
i=0
total=${#RUNS[@]}
for run in "${RUNS[@]}"; do
  i=$((i + 1))
  read -r module cfg expect invariant <<<"$run"
  if [ "$expect" = "PASS" ]; then
    echo "== $i/$total $cfg: expecting PASS"
    out=$(run_tlc "$module" "$cfg"); rc=$?
    echo "$out" | grep -E "states generated" | head -1
    if [ $rc -ne 0 ]; then
      echo "FAIL: conforming config violated an invariant"
      echo "$out"
      failures=$((failures + 1))
    fi
  else
    echo "== $i/$total $cfg: expecting a $invariant violation"
    out=$(run_tlc "$module" "$cfg"); rc=$?
    if [ $rc -eq 0 ]; then
      echo "FAIL: expected a violation, but the model passed."
      echo "      (Did the broken variant stop being broken? That is a model bug.)"
      failures=$((failures + 1))
    elif ! echo "$out" | grep -q "Invariant $invariant is violated"; then
      echo "FAIL: model errored for a reason other than the expected violation:"
      echo "$out"
      failures=$((failures + 1))
    else
      echo "-- counterexample (the race, step by step):"
      echo "$out" | sed -n "/^Error: Invariant $invariant/,/^[0-9]* states generated/p" | head -60
    fi
  fi
  echo
done

if [ $failures -ne 0 ]; then
  echo "RESULT: FAIL ($failures of $total runs misbehaved)"
  exit 1
fi
echo "RESULT: PASS (all $total runs behaved as expected)"
