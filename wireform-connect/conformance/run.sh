#!/usr/bin/env bash
# Run the official connectrpc conformance suite against wireform-connect.
#
#   ./run.sh server     # wireform-connect server vs the reference client (default)
#   ./run.sh client     # wireform-connect client vs the reference server
#   ./run.sh both       # wireform-connect client vs wireform-connect server
#   ./run.sh server raw # ...append "raw" to ignore the known-failing list
#
# Downloads the pinned connectconformance runner on first use into ./bin/
# (gitignored). See README.md for status and the known-failing lists.
set -euo pipefail

VERSION=v1.0.5
MODE="${1:-server}"
RAW="${2:-}"
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$HERE/../.." && pwd)"
BIN="$HERE/bin/connectconformance"

if [[ ! -x "$BIN" ]]; then
  OS="$(uname -s)"; ARCH="$(uname -m)"
  URL="https://github.com/connectrpc/conformance/releases/download/${VERSION}/connectconformance-${VERSION}-${OS}-${ARCH}.tar.gz"
  echo "Downloading connectconformance ${VERSION} ($OS/$ARCH)..."
  mkdir -p "$HERE/bin"
  curl -fsSL "$URL" | tar -xz --directory "$HERE/bin"
fi

binOf() { ( cd "$REPO_ROOT" && cabal build "wireform-connect:$1" >/dev/null && cabal list-bin "wireform-connect:$1" ); }

case "$MODE" in
  server)
    SERVER="$(binOf conformance-server)"
    KF=(); [[ "$RAW" != raw ]] && KF=(--known-failing "@$HERE/known-failing-server.txt")
    exec "$BIN" --mode server --conf "$HERE/config-server.yaml" "${KF[@]}" -- "$SERVER" ;;
  client)
    CLIENT="$(binOf conformance-client)"
    KF=(); [[ "$RAW" != raw ]] && KF=(--known-failing "@$HERE/known-failing-client.txt")
    exec "$BIN" --mode client --conf "$HERE/config-client.yaml" "${KF[@]}" -- "$CLIENT" ;;
  both)
    CLIENT="$(binOf conformance-client)"; SERVER="$(binOf conformance-server)"
    exec "$BIN" --mode both --conf "$HERE/config-client.yaml" -- "$CLIENT" ---- "$SERVER" ;;
  *)
    echo "usage: $0 {server|client|both} [raw]" >&2; exit 2 ;;
esac
