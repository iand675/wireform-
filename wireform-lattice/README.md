# wireform-lattice

An implementation of **Lattice**, a cache-native graph query protocol in the
GraphQL / JSON:API problem space, designed so that every network-relevant
property of a request — cache key, freshness, authorization variance,
dependency set, batch structure — is a static artifact derived from the
schema and the query text.

The normative specification (Draft 27) and a worked GraphQL comparison
corpus live in the docs site: [`/lattice/`](https://iand675.github.io/wireform-/lattice/)
(source: `website/src/content/docs/lattice/`).

## What's here

| Piece | Where | What |
|---|---|---|
| Schema IDL | `Lattice.IDL.Parser` / `Lattice.IDL.Print` | Surface text ↔ the semantic model (`Lattice.Schema`); canonical, content-addressed IDL documents |
| Query language | `Lattice.Query.{Parser,Validate}` | The normative §4.8 grammar + the eight static rules |
| Canonicalization | `Lattice.Canonical` | §5.1 canonical text — the query's identity — plus `queryHash` (BLAKE3-128) |
| Planner | `Lattice.Plan` | The §8.1 authorization path join (`pub`/`ctx`/`priv` slices), plan ids over pertinent declarations, budgets, `explain` |
| Wire format | `Lattice.Wire` | §9 NDJSON entity-stream records, scoped errors, surrogate keys, protocol headers |
| Origin | `Lattice.Server` (+ `.Execute`, `.Auth`) | The full HTTP origin on `wireform-http`: discovery, transport ladder (hash GET / inline GET / QUERY / POST), point fetches with masks + ver pinning, mutations with idempotency keys and write-set enforcement, per-slice caching headers + tenure |
| Backend contract | `Lattice.Backend` (+ `.Memory`) | Set-in map-out loaders (N+1 is inexpressible); an STM in-memory backend deriving pagination/cursors from the schema |
| Client | `Lattice.Client` (+ `.Store`) | Transport ladder, normalized entity store, schema-directed denormalization, live subscriptions, digest advertisement |
| Governance | `Lattice.Server.Coalesce`, `Lattice.Server.Auth` | §6.9 origin coalescing (accumulation windows, single-flight); §14.3 signed admission (Ed25519 over canonical text) |
| Live queries | `Lattice.Server.Live` | §12: SSE subscriptions over hash-form URLs, single-flight delta re-execution, reauth |
| Digests | `Lattice.Digest` | §10.4 `X-Have` + the pinned Golomb-coded set; `unchanged` markers on priv/oneshot |
| Compatibility | `Lattice.Compat`, `Lattice.Registry` | §17: change taxonomy, transitive check windows, `@break`/`@deprecated`, corpus-weighted `POST /schema/check` |
| Federation | `Lattice.Module`, `Lattice.Gateway` | §18: `extend entity` + order-insensitive fusion (in-process), and the network gateway (per-upstream `nodes` subplans, `src` tags, feed-driven purges) |
| Observability | `Lattice.Telemetry` | §19 span topology + the ten named instruments (OpenTelemetry, no-op by default) |
| Demo origin | `example/` (`cabal run example-lattice`) | The corpus Star Wars schema on port 8917, with CDN-harness hooks |
| CDN tiers | `cdn/` | Varnish (vmod_xkey) VCL + harness and a plug-and-play Cloudflare Worker, both proven by `cdn/conformance.mjs` |
| TLA+ model | `tla/` (`./check.sh`) | Model-checked §11.5 invalidation-pipeline claims: quiescent coherence under at-least-once reordered purge delivery, read-your-writes independent of purge timing, and the intent-time-purge stale-refill race as a TLC counterexample. `tlc` ships in the dev shell |

The TypeScript client (`lattice-ts/`, repo root) is a zero-runtime-dependency
Apollo-style library with React bindings whose headline is **merged
multi-root queries** — components' queries mounting in the same tick batch
into a single request. It never computes BLAKE3: the introduction handshake
teaches it its steady-state hash-form URL via `Location`.

## Quick start

```bash
cabal run example-lattice
# then:
curl http://127.0.0.1:8917/.well-known/lattice
curl -X POST 'http://127.0.0.1:8917/q?intent=introduce&slice=pub' \
  -H 'Content-Type: application/x-lattice-query' \
  --data 'query Hero { hero { name friends(first: 3) { name } } }'
# follow the Location header for the cacheable steady-state GET
curl -X POST http://127.0.0.1:8917/m/createReview \
  -H 'Content-Type: application/json' -H 'Idempotency-Key: demo-1' \
  --data '{"episode":"Jedi","stars":5,"commentary":"works"}'
```

The CDN harnesses run from the repo dev shell (varnishd + vmod_xkey and node
are provided; no docker):

```bash
cd wireform-lattice/cdn/varnish && ./run-harness.sh            # Varnish tier
cd wireform-lattice/cdn/cloudflare-worker && ./run-harness.sh  # Worker tier (miniflare)
```

## Fixtures and goldens

`test/fixtures/{starwars,blog}.lattice` pin the IDL surface;
`test/fixtures/golden/` pins canonical IDL and canonical query texts +
hashes. A golden change is a cross-implementation protocol event: regenerate
deliberately (sydtest `--golden-reset`) and record the change in the spec's
draft changelog.

## Implementation status

The docs landing page (`/lattice/`) carries the authoritative coverage
matrix. Every formerly deferred area now ships: derived fields (§3.7),
cache digests (§10.4), verb bindings (§11.7), live queries (§12), signed
admission (§14.3), the `nodes` root (§14.4), the compatibility registry
(§17), modules + federation (§18), and the OTel conventions (§19). The
matrix's "simplifications" table lists the deliberate, haddock-documented
edges that remain.
