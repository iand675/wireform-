#!/usr/bin/env bash
# Lattice Cloudflare-Worker CDN harness.
#
# Starts the demo Lattice origin (port 8917) and `wrangler dev --local`
# (miniflare/workerd, port 8787, emulated Cache API + KV), then runs the
# shared conformance checker through the worker in lenient/hard mode.
# Exit code = checker exit code. All children are torn down via trap.
#
# Run from the repo dev shell (`nix develop`) so node/npm/curl and a built
# `example-lattice` are available:   ./run-harness.sh
set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")"

ORIGIN_PORT=8917
WORKER_PORT=8787
ORIGIN_URL="http://127.0.0.1:${ORIGIN_PORT}"
CDN_URL="http://127.0.0.1:${WORKER_PORT}"
CHECKER=../conformance.mjs

ORIGIN_PID=""
WRANGLER_PID=""
LOG_DIR="$(mktemp -d "${TMPDIR:-/tmp}/lattice-cf-harness.XXXXXX")"

say() { printf '[cf-harness] %s\n' "$*" >&2; }

die() {
  say "ERROR: $*"
  exit 1
}

port_busy() {
  # Any listener on the port? (harness-owned ports only: 8917/8787)
  lsof -nP -iTCP:"$1" -sTCP:LISTEN -t >/dev/null 2>&1
}

kill_port_listeners() {
  # Last-resort sweep for our own descendants (workerd outlives wrangler on
  # occasion). Only ever called on ports this harness verified were free at
  # startup, so anything still listening is ours.
  local pids
  pids="$(lsof -nP -iTCP:"$1" -sTCP:LISTEN -t 2>/dev/null || true)"
  if [ -n "$pids" ]; then
    # shellcheck disable=SC2086
    kill -9 $pids 2>/dev/null || true
  fi
}

cleanup() {
  local status=$?
  trap - EXIT INT TERM
  say "tearing down (logs in $LOG_DIR)"
  if [ -n "$WRANGLER_PID" ]; then
    kill "$WRANGLER_PID" 2>/dev/null || true
    # wrangler forwards the signal to workerd; give it a moment.
    for _ in 1 2 3 4 5 6 7 8 9 10; do
      kill -0 "$WRANGLER_PID" 2>/dev/null || break
      sleep 0.3
    done
    kill -9 "$WRANGLER_PID" 2>/dev/null || true
    pkill -9 -P "$WRANGLER_PID" 2>/dev/null || true
  fi
  if [ -n "$ORIGIN_PID" ]; then
    kill "$ORIGIN_PID" 2>/dev/null || true
    wait "$ORIGIN_PID" 2>/dev/null || true
  fi
  kill_port_listeners "$WORKER_PORT"
  kill_port_listeners "$ORIGIN_PORT"
  exit "$status"
}
trap cleanup EXIT INT TERM

wait_http() {
  # wait_http URL LABEL — poll until the URL answers (any HTTP status), 60s cap.
  local url=$1 label=$2 i=0
  while ! curl -s -o /dev/null --max-time 2 "$url"; do
    i=$((i + 1))
    if [ "$i" -ge 120 ]; then
      say "--- $label log tail ---"
      tail -n 40 "$LOG_DIR"/*.log >&2 || true
      die "$label did not come up on $url within 60s"
    fi
    sleep 0.5
  done
}

# --- preflight -------------------------------------------------------------
command -v node >/dev/null || die "node not found — run from 'nix develop'"
command -v curl >/dev/null || die "curl not found — run from 'nix develop'"
[ -f "$CHECKER" ] || die "shared checker $CHECKER not found (owned by the varnish harness work)"
port_busy "$ORIGIN_PORT" && die "port $ORIGIN_PORT already in use — refusing to touch someone else's process"
port_busy "$WORKER_PORT" && die "port $WORKER_PORT already in use — refusing to touch someone else's process"

ORIGIN_BIN="$(cabal list-bin example-lattice 2>/dev/null | tail -n 1)"
[ -n "$ORIGIN_BIN" ] && [ -x "$ORIGIN_BIN" ] || die "example-lattice binary not found; build it with: cabal build example-lattice"

if [ ! -x node_modules/.bin/wrangler ]; then
  say "installing worker dev dependencies (npm install)"
  npm install --no-fund --no-audit >"$LOG_DIR/npm.log" 2>&1 || {
    tail -n 40 "$LOG_DIR/npm.log" >&2
    die "npm install failed"
  }
fi

# Stale local Cache/KV state from an earlier run would break the checker's
# cold-MISS assertions — always start from scratch.
rm -rf .wrangler/state

# --- origin ----------------------------------------------------------------
say "starting origin on :$ORIGIN_PORT"
LATTICE_PORT="$ORIGIN_PORT" \
LATTICE_DEBUG_DELAY_MS=150 \
LATTICE_PURGE_URL="$CDN_URL/_lattice/purge" \
LATTICE_PURGE_STYLE=worker \
LATTICE_PURGE_SECRET=dev-secret \
  "$ORIGIN_BIN" >"$LOG_DIR/origin.log" 2>&1 &
ORIGIN_PID=$!

# --- worker (miniflare) ------------------------------------------------------
say "starting wrangler dev --local on :$WORKER_PORT"
WRANGLER_SEND_METRICS=false \
  npx wrangler dev --local --port "$WORKER_PORT" >"$LOG_DIR/wrangler.log" 2>&1 &
WRANGLER_PID=$!

wait_http "$ORIGIN_URL/debug/requests" "origin"
wait_http "$CDN_URL/debug/requests" "worker"

# --- conformance -------------------------------------------------------------
# Miniflare's local Cache API honours max-age on stored responses; the
# harness still runs the checker in *lenient* coalescing (single-flight is
# per-isolate, and workerd may run several) and *hard* purge mode (the worker
# purges by deletion, not softpurge).
say "running conformance checker (lenient / hard)"
status=0
node "$CHECKER" \
  --cdn "$CDN_URL" \
  --origin "$ORIGIN_URL" \
  --coalesce lenient \
  --purge-mode hard || status=$?

exit "$status"
