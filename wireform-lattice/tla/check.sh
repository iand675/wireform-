#!/usr/bin/env bash
# Model-check the Lattice invalidation pipeline (spec section 11.5).
#
#   ./check.sh          # runs both configs; exit 0 iff both behave as expected
#
# Requires `tlc` on PATH (in the repo dev shell: `nix develop`).
#
# Two runs:
#   1. LatticeInvalidation.cfg  (purge at truth-commit)  -> must PASS
#   2. PurgeAtIntent.cfg        (purge at publish time)  -> must FAIL with a
#      QuiescentCoherence counterexample: the stale-refill race the spec's
#      transactional-outbox placement exists to close. The trace is printed.
set -uo pipefail
cd "$(dirname "$0")"

TLC=${TLC:-tlc}
command -v "$TLC" >/dev/null || { echo "tlc not found; enter the dev shell (nix develop)"; exit 1; }

run_tlc() {
  local cfg=$1
  "$TLC" -config "$cfg" -metadir "$(mktemp -d)" -cleanup \
    LatticeInvalidation.tla 2>&1
}

echo "== 1/2 purge at truth-commit (transactional outbox): expecting PASS"
out1=$(run_tlc LatticeInvalidation.cfg); rc1=$?
echo "$out1" | grep -E "Model checking completed|states generated|Error" | head -4
if [ $rc1 -ne 0 ]; then
  echo "FAIL: conforming pipeline violated an invariant"; echo "$out1"; exit 1
fi

echo
echo "== 2/2 purge at intent (publish time): expecting QuiescentCoherence violation"
out2=$(run_tlc PurgeAtIntent.cfg); rc2=$?
if [ $rc2 -eq 0 ]; then
  echo "FAIL: expected the stale-refill race, but the model passed."
  echo "      (Did the broken variant stop being broken? That is a model bug.)"
  exit 1
fi
if ! echo "$out2" | grep -q "Invariant QuiescentCoherence is violated"; then
  echo "FAIL: model errored for a reason other than the expected violation:"
  echo "$out2"
  exit 1
fi
echo "-- counterexample (the race, step by step):"
echo "$out2" | sed -n '/^Error: Invariant QuiescentCoherence/,/^[0-9]* states generated/p'

echo
echo "RESULT: PASS (conforming pipeline safe; intent-time purge race exhibited)"
