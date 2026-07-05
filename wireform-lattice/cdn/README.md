# Lattice behind real CDNs

Lattice's whole premise is that a graph query protocol can ride the plain
HTTP caching machinery instead of reinventing it. This directory proves
that claim against two real shared caches, driven by one protocol-level
conformance checker:

- **`varnish/`** — Varnish 8 + `vmod_xkey`. The reference tier: real
  request coalescing (one origin fill for N concurrent misses), soft
  tag-purge into stale-while-revalidate grace, byte-for-byte HTTP
  semantics.
- **`cloudflare-worker/`** — a Cloudflare Worker on the Cache API + KV.
  Proves the same protocol works on a commodity edge runtime where
  surrogate-key purging is not a native primitive (the worker maintains
  its own tag index). See [its README](cloudflare-worker/README.md) for
  deployment and the documented deviations (hard purge, per-isolate
  coalescing, per-colo caches).
- **`conformance.mjs`** — the shared checker (node ≥ 18, zero
  dependencies). Speaks pure HTTP to a CDN URL and an origin URL; knows
  nothing about which CDN it is testing beyond two strictness knobs.

What the tiers demonstrate, concretely: the CDN needs **no knowledge of
the query language**. Everything it does is driven by ordinary response
headers the origin already emits (`Cache-Control`, `ETag`,
`Surrogate-Key`, `Vary`) plus one purge channel. The only "protocol
awareness" in either tier is *pass anything carrying `Authorization`* —
and even that is stock CDN hygiene.

## Running the harnesses

Both harnesses assume the repo dev shell (`nix develop` at the workspace
root), which provides `node`, `curl`, and the cabal toolchain for the
demo origin (`example-lattice`, the Star Wars dataset). The Varnish tier
additionally needs a working **podman** (on macOS: Podman Desktop, or
`podman machine start`) — Varnish runs as a container from the official
`varnish:8.0` image, which bundles varnish-modules including `vmod_xkey`.

```sh
# Varnish tier: origin :8917 (host), varnish :6081 (container, podman)
cd wireform-lattice/cdn/varnish && ./run-harness.sh

# Cloudflare Worker tier: origin :8917, wrangler dev (miniflare) :8787
cd wireform-lattice/cdn/cloudflare-worker && ./run-harness.sh
```

Each harness starts its own origin (with `LATTICE_DEBUG_DELAY_MS=150` to
make coalescing windows real, and the purge forwarder pointed at the CDN
under test), waits for both processes, runs the checker, and tears
everything down; the exit code is the checker's. The Varnish harness
additionally self-tests the `PURGE` synth: it asserts the response's
`xkey-purged` count and that the origin's forwarded purges got 200s.

The checker can also be pointed at anything by hand:

```sh
node conformance.mjs --cdn http://127.0.0.1:6081 --origin http://127.0.0.1:8917 \
  --coalesce strict --purge-mode soft [--timeout 120] [--skip 4,7]
```

`--coalesce strict` demands *exactly one* origin fill under 16-way
concurrency (Varnish's waitinglist); `lenient` accepts `< 16` fills with
identical bodies (per-isolate single-flight). `--purge-mode` annotates
step 5; both modes assert eventual freshness by polling the plain URL.

## The conformance steps

| # | step | proves |
|---|------|--------|
| 1 | `introduce` | `POST /q?intent=introduce` names the query; `Location` yields the canonical GET URL |
| 2 | `cold-miss` | cold GET via the CDN is a MISS and reaches the origin exactly once |
| 3 | `warm-hits` | repeat GETs are HITs with a stable `ETag`; the origin sees nothing |
| 4 | `coalesce` | 16 concurrent GETs of a cold URL collapse to one origin fill (strict) — the origin's 150 ms artificial latency keeps the window honest |
| 5 | `purge-on-mutation` | a mutation **through** the CDN reports `invalidated` keys (`reviews:Jedi`), the origin's purge forwarder soft-purges the CDN, and a **plain** GET (no cache busting) converges on fresh content within the SWR window — while the object's TTL (`s-maxage` > poll window) rules out natural expiry |
| 6 | `idempotent-replay` | replaying the same `Idempotency-Key` through the CDN passes through and returns `Idempotency-Replayed: true`; the review exists exactly once |
| 7 | `entity-masks` | `/e` point fetches: same mask shares one cache entry, a different mask is a different URL (no field leakage between variants), and a `ver`-pinned URL is `immutable` + HIT on repeat |
| 8 | `auth-bypass` | `Authorization`-carrying requests are served but never from (or into) the shared cache: origin count +2 |

Steps run in order and build on each other; the first failure prints an
`expected` / `got` block (with response headers) and the rest are
skipped. A summary table and `RESULT: PASS|FAIL` close the run.

## Varnish tier notes

- **VCL** (`varnish/default.vcl`, vcl 4.1): `PURGE` from the container
  host with `xkey-softpurge: k1 k2 …` soft-purges by tag (a `xkey-purge`
  header hard-purges); non-GET/HEAD, `Authorization`, and `/debug/` pass;
  all else is plain lookup. On the backend side the only protocol wiring
  is `Surrogate-Key` → `xkey` (the header the vmod reads).
- **swr → grace**: Varnish has mapped `stale-while-revalidate` to object
  grace natively since 6.0 — verified on varnishd 8.0.2 (a
  `s-maxage=300, stale-while-revalidate=60` fill logs `TTL RFC 300 60 …`).
  The VCL keeps a defensive `beresp.grace = 30s` fallback that is a no-op
  while the native mapping holds.
- **Soft purge + grace** is what makes invalidation cheap: the purged
  object keeps serving (stale) while a single background fetch refreshes
  it — the origin sees one request, not a stampede.
- `run-varnish.sh` renders `default.vcl` into a work dir (rebinding the
  backend to `$VARNISH_BACKEND_HOST`/`$VARNISH_BACKEND_PORT`; the
  checked-in default targets podman's `host.containers.internal`, use
  `host.docker.internal` under docker) and runs the container in the
  foreground with the host port published; `VARNISH_PORT`,
  `VARNISH_WORKDIR`, and `VARNISH_CONTAINER` are overridable (the
  harness owns the workdir so teardown removes it). The purgers ACL
  admits loopback plus the private/link-local ranges container runtimes
  NAT host traffic from — pin it down (or add a token) in production.
- **Version pin**: Varnish Cache rebranded to **Vinyl Cache** at v9
  (nixpkgs ships it as `vinyl-cache`, binaries renamed
  `vinyld`/`vinylstat`). This tier runs the containerized Varnish 8.0.2 —
  the last classic-branded line with a matching `xkey` vmod build. The
  native-nix path was abandoned: nixpkgs varnish 8.0.2 needs multiple
  Darwin build patches (false `ac_cv` pre-seeds, pre-2.4.7 libtool), and
  `vinyl-cache` 9.0.1 is marked broken on Darwin with no vinyl-compatible
  modules set — the container image is the supported path.

## Production notes

- **Fastly**: `Surrogate-Key` is Fastly's native tag header — the origin
  already speaks it verbatim. Point the origin's purge forwarder at
  Fastly's purge API (`POST /service/{id}/purge` with the keys, or
  `soft=1` for the SWR-preserving equivalent of `xkey.softpurge`) and
  there is nothing else to build.
- **Varnish (self-hosted)**: needs the `xkey` vmod from
  [varnish-modules](https://github.com/varnish/varnish-modules) — stock
  Varnish only purges by URL. This tier's VCL is a complete, minimal
  starting point; add an ACL/token for purges arriving from beyond
  localhost.
- **Cloudflare**: Cache-Tag purging is Enterprise-only; the worker tier
  works around it with a KV tag index, with documented deviations. On
  Enterprise, drop the KV index and pass `Surrogate-Key` through as
  `Cache-Tag` instead. Details in
  [cloudflare-worker/README.md](cloudflare-worker/README.md).
- **Any other shared cache**: the protocol's requirements are exactly
  (a) honor `Cache-Control: s-maxage` + `stale-while-revalidate`,
  (b) some tag-purge channel fed from `Surrogate-Key`, (c) pass
  `Authorization` traffic. Nothing else in these tiers is
  Lattice-specific.
