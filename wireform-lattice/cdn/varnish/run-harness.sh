#!/usr/bin/env bash
# End-to-end Varnish conformance harness for the Lattice CDN tier.
#
# Orchestrates: example-lattice origin (with purge forwarding pointed at
# Varnish) + a containerized varnishd (./run-varnish.sh, official
# varnish:8.0 image via podman) + the shared protocol checker
# (../conformance.mjs, strict coalescing / soft purge), then a purge
# synth self-test. Exit code is the checker's; everything started here is
# torn down on exit. Run from the repo dev shell (node, curl, cabal) with
# podman available (Podman Desktop / podman machine start):
#
#   nix develop -c ./run-harness.sh
#
# Ports: origin 8917 (LATTICE_PORT), varnish 6081 (VARNISH_PORT).
set -euo pipefail

cd "$(dirname "$0")"

ORIGIN_PORT="${LATTICE_PORT:-8917}"
VARNISH_PORT="${VARNISH_PORT:-6081}"
ORIGIN_URL="http://127.0.0.1:${ORIGIN_PORT}"
CDN_URL="http://127.0.0.1:${VARNISH_PORT}"
CONTAINER="${VARNISH_CONTAINER:-lattice-varnish}"

for tool in podman node curl cabal; do
  if ! command -v "$tool" >/dev/null 2>&1; then
    echo "run-harness.sh: $tool not found — run from the repo dev shell (nix develop); podman comes from Podman Desktop" >&2
    exit 1
  fi
done

# TMPDIR (not /tmp): on macOS the podman machine mounts /Users, /private
# and /var/folders — $TMPDIR lives under /var/folders, so the rendered
# VCL bind-mount resolves inside the machine.
tmpdir="$(mktemp -d "${TMPDIR:-/tmp}/lattice-harness.XXXXXX")"
origin_pid=""
varnish_pid=""

# shellcheck disable=SC2329 # invoked via the EXIT/INT/TERM trap below
cleanup() {
  status=$?
  # Kill our children; podman-run forwards TERM to varnishd (PID 1),
  # and --rm removes the container. The rm -f below catches a container
  # that outlived a killed podman client.
  for pid in "$varnish_pid" "$origin_pid"; do
    if [ -n "$pid" ] && kill -0 "$pid" 2>/dev/null; then
      kill "$pid" 2>/dev/null || true
      wait "$pid" 2>/dev/null || true
    fi
  done
  podman rm -f "$CONTAINER" >/dev/null 2>&1 || true
  if [ "$status" -ne 0 ]; then
    echo "--- origin log (tail) ---" >&2
    tail -n 40 "$tmpdir/origin.log" >&2 2>/dev/null || true
    echo "--- varnish log (tail) ---" >&2
    tail -n 40 "$tmpdir/varnish.log" >&2 2>/dev/null || true
  fi
  rm -rf "$tmpdir"
  exit "$status"
}
trap cleanup EXIT INT TERM

# Poll a URL until it answers 200, failing loudly on timeout.
wait_for() {
  url=$1
  what=$2
  max_tries=$3
  tries=0
  until curl -sf -o /dev/null --max-time 2 "$url"; do
    tries=$((tries + 1))
    if [ "$tries" -ge "$max_tries" ]; then
      echo "run-harness.sh: $what never came up at $url" >&2
      exit 1
    fi
    sleep 0.2
  done
}

# --- origin -----------------------------------------------------------------
# The binary is normally already built; fall back to a build in the
# default builddir only when it is missing.
if ! origin_bin="$(cabal list-bin example-lattice 2>/dev/null)" || [ ! -x "$origin_bin" ]; then
  echo "run-harness.sh: building example-lattice..." >&2
  (cd ../../.. && cabal build exe:example-lattice)
  origin_bin="$(cabal list-bin example-lattice)"
fi

echo "run-harness.sh: starting origin ($origin_bin) on :$ORIGIN_PORT"
env \
  LATTICE_PORT="$ORIGIN_PORT" \
  LATTICE_DEBUG_DELAY_MS=150 \
  LATTICE_PURGE_URL="$CDN_URL/" \
  LATTICE_PURGE_STYLE=varnish \
  "$origin_bin" >"$tmpdir/origin.log" 2>&1 &
origin_pid=$!
wait_for "$ORIGIN_URL/.well-known/lattice" "origin" 150

# --- varnish ----------------------------------------------------------------
echo "run-harness.sh: starting varnish (container) on :$VARNISH_PORT"
mkdir "$tmpdir/varnish-workdir"
env \
  VARNISH_PORT="$VARNISH_PORT" \
  VARNISH_WORKDIR="$tmpdir/varnish-workdir" \
  VARNISH_CONTAINER="$CONTAINER" \
  VARNISH_BACKEND_PORT="$ORIGIN_PORT" \
  ./run-varnish.sh >"$tmpdir/varnish.log" 2>&1 &
varnish_pid=$!
# Probing /debug/requests exercises the whole chain (varnish pass ->
# origin) without touching the origin's request counter. The generous
# budget covers a first-time image pull.
wait_for "$CDN_URL/debug/requests" "varnish (proxying to origin)" 600

# --- conformance ------------------------------------------------------------
checker_status=0
node ../conformance.mjs \
  --cdn "$CDN_URL" \
  --origin "$ORIGIN_URL" \
  --coalesce strict \
  --purge-mode soft \
  || checker_status=$?

# --- purge synth self-test ---------------------------------------------------
# The checker proves purging end to end (origin -> varnish -> fresh
# content); this additionally asserts the PURGE synth reports how many
# objects a softpurge expired, and that the origin's forwarder saw 200s.
echo ""
echo "run-harness.sh: purge synth self-test"
curl -sf -o /dev/null "$CDN_URL/e/Human/1000?f=appearsIn"
purged="$(curl -sf -X PURGE -H 'xkey-softpurge: Human:1000' -o /dev/null -D - "$CDN_URL/" \
  | tr -d '\r' | awk -F': ' 'tolower($1) == "xkey-purged" { print $2 }')"
echo "  PURGE xkey-softpurge: Human:1000 -> xkey-purged: ${purged:-<missing>}"
if [ -z "$purged" ] || [ "$purged" -lt 1 ]; then
  echo "run-harness.sh: FAIL — softpurge synth did not report >=1 purged object" >&2
  exit 1
fi
echo "  origin purge forwards:"
if ! grep '\[purge\] forwarded' "$tmpdir/origin.log" | sed 's/^/    /'; then
  echo "run-harness.sh: FAIL — origin never forwarded a purge" >&2
  exit 1
fi
if ! grep '\[purge\] forwarded' "$tmpdir/origin.log" | grep -q 200; then
  echo "run-harness.sh: FAIL — no purge forward got a 200 from varnish" >&2
  exit 1
fi

exit "$checker_status"
