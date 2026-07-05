#!/usr/bin/env bash
# Run Varnish 8 in a container (foreground) against ./default.vcl.
#
# Uses the official varnish:8.0 image, which bundles varnish-modules
# (including vmod_xkey) — no vmod_path wiring needed. Requires a working
# podman (on macOS: Podman Desktop, or `podman machine start`).
#
# Environment knobs:
#   VARNISH_PORT          host port to publish (default 6081)
#   VARNISH_BACKEND_HOST  origin host as seen FROM the container
#                         (default host.containers.internal — podman's
#                         host alias; use host.docker.internal for docker)
#   VARNISH_BACKEND_PORT  origin port (default 8917)
#   VARNISH_WORKDIR       where the rendered VCL is written; a fresh
#                         mktemp dir under $TMPDIR when unset (the caller
#                         owns cleanup of a directory it passes in — this
#                         script execs podman, so it cannot trap). Must
#                         be under a path the podman machine mounts
#                         (macOS: /Users, /private, /var/folders).
#   VARNISH_CONTAINER     container name (default lattice-varnish)
set -euo pipefail

cd "$(dirname "$0")"

if ! command -v podman >/dev/null 2>&1; then
  echo "run-varnish.sh: podman not found — install Podman Desktop (or podman)." >&2
  exit 1
fi
if ! podman info >/dev/null 2>&1; then
  echo "run-varnish.sh: podman is installed but its machine/socket is not ready." >&2
  echo "  start Podman Desktop (or run: podman machine start) and retry." >&2
  exit 1
fi

name="${VARNISH_CONTAINER:-lattice-varnish}"
backend_host="${VARNISH_BACKEND_HOST:-host.containers.internal}"
backend_port="${VARNISH_BACKEND_PORT:-8917}"
workdir="${VARNISH_WORKDIR:-$(mktemp -d "${TMPDIR:-/tmp}/lattice-varnish.XXXXXX")}"

# Render the VCL for this run: point the backend at the requested
# host/port (the checked-in default targets podman-from-container).
sed \
  -e "s/\.host = \".*\";/.host = \"$backend_host\";/" \
  -e "s/\.port = \".*\";/.port = \"$backend_port\";/" \
  default.vcl >"$workdir/default.vcl"

if ! podman image exists varnish:8.0; then
  echo "run-varnish.sh: pulling varnish:8.0..." >&2
  podman pull varnish:8.0 >&2
fi

# A leftover container from a crashed run would collide on the name.
podman rm -f "$name" >/dev/null 2>&1 || true

echo "run-varnish.sh: varnish:8.0 as '$name' on 127.0.0.1:${VARNISH_PORT:-6081} -> $backend_host:$backend_port" >&2

# --sysctl: the image runs varnishd as its unprivileged `varnish` user
# bound to :80 — docker allows in-container low ports by default, podman
# does not, so lower the threshold inside this container's netns.
exec podman run --rm --name "$name" \
  -p "127.0.0.1:${VARNISH_PORT:-6081}:80" \
  --sysctl net.ipv4.ip_unprivileged_port_start=0 \
  -e VARNISH_SIZE=64m \
  -v "$workdir/default.vcl:/etc/varnish/default.vcl:ro" \
  varnish:8.0
