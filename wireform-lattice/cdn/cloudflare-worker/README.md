# Lattice CDN tier — Cloudflare Worker

A **plug-and-play** shared cache for the Lattice protocol on Cloudflare
Workers. Nothing in `src/worker.ts` is specific to the Star Wars demo: point
`ORIGIN_URL` at **any conforming Lattice origin** and route traffic through
the worker. Because Lattice makes every representation-affecting input
(slice, `vc` claims, `ver` pins, field masks, variables) part of the URL, the
worker's entire cache-correctness story is *full-URL keying plus honouring
the origin's `Cache-Control`* — no protocol-specific parsing.

What the worker does:

- **Shared-caches** `GET`s under `/q`, `/e`, `/schema`, `/.well-known/lattice`
  in `caches.default`, keyed by full URL (query string included), when the
  origin says `public` + (`s-maxage>0` or `immutable`). Never caches
  `private`/`no-store` or requests carrying `Authorization`.
- **Rewrites the stored copy's** `Cache-Control` from `s-maxage` to `max-age`
  (the Cache API honours `max-age`, not `s-maxage`); the client-facing
  response keeps the origin's original headers.
- **Maintains a Surrogate-Key → URLs tag index** in Workers KV (binding
  `TAGS`, entries `tag:<key>`, deduplicated appends, 24 h TTL).
- **Purges by key**: `POST /_lattice/purge` with body `{"keys": ["k1", …]}`
  and header `X-Purge-Secret: <secret>` deletes every indexed URL from the
  cache and drops the KV entries. Responds `{"purged": <nUrls>}`.
- **Single-flights** concurrent identical-URL misses per isolate (one origin
  fill, everyone shares the buffered response).
- **Proxies everything else untouched** — mutations (`/m/…`), `QUERY`/`POST`
  requests, introduce, `/debug/…`.
- Tags every response `X-Cache: HIT | MISS | BYPASS`.

## Local harness

From the repo dev shell (`nix develop`), with `example-lattice` built and the
shared checker landed at `../conformance.mjs`:

```sh
./run-harness.sh
```

This starts the demo origin on `:8917` (purge forwarder pointed at the
worker), `wrangler dev --local` on `:8787` (miniflare/workerd emulates the
Cache API and KV, honouring `max-age` on stored copies), runs

```sh
node ../conformance.mjs --cdn http://127.0.0.1:8787 \
  --origin http://127.0.0.1:8917 --coalesce lenient --purge-mode hard
```

and tears everything down. Exit code = checker exit code.

## Deploying for real

1. Create the KV namespace and wire its id into `wrangler.toml`:

   ```sh
   npx wrangler kv namespace create TAGS
   # replace `id = "tags-dev"` under kv_namespaces with the printed id
   ```

2. Set the production origin and purge secret (secrets shadow `[vars]`;
   don't ship a real secret in `wrangler.toml`):

   ```sh
   # in wrangler.toml [vars]: ORIGIN_URL = "https://lattice-origin.example.com"
   npx wrangler secret put PURGE_SECRET
   ```

3. Deploy, and route Lattice traffic through the worker (either the printed
   `workers.dev` URL or a route/custom domain on your zone):

   ```sh
   npx wrangler deploy
   ```

4. Point the **origin's purge forwarder** at the deployed worker. The origin
   side of the contract (as implemented by `example-lattice`; any conforming
   origin needs the equivalent): on every invalidation, POST the surrogate
   keys to the worker —

   | Origin env                | Value                                            |
   | ------------------------- | ------------------------------------------------ |
   | `LATTICE_PURGE_URL`       | `https://<your-worker-route>/_lattice/purge`     |
   | `LATTICE_PURGE_STYLE`     | `worker` (POST JSON `{"keys":[…]}`)              |
   | `LATTICE_PURGE_SECRET`    | the value given to `wrangler secret put`         |

## Deviations from the Varnish tier

| Concern | Varnish (`cdn/varnish/`) | This worker | Production alternative on Cloudflare |
| --- | --- | --- | --- |
| Purge semantics | **Soft** purge (`xkey.softpurge`): objects become stale but remain servable within `stale-while-revalidate`/grace while the origin refills | **Hard** purge: `caches.default.delete` per indexed URL — next read is a full MISS | none needed; hard purge is strictly safer, just colder |
| `stale-while-revalidate` | Honoured natively (maps to grace) | Dropped when the stored copy is rewritten to `max-age` — the Cache API has no grace/SWR semantics | serve-stale needs `fetch` + `cf` options or Cache Rules on a zone |
| Request coalescing | Per-instance and absolute: N concurrent misses ⇒ exactly 1 origin fetch (checker `--coalesce strict`) | **Per-isolate** `Map<url, Promise>`: workerd may run several isolates, so N misses ⇒ *fewer than N* fetches, not exactly 1 (checker `--coalesce lenient`) | Cloudflare's own colo-level concurrency protection ("single file") on cache misses in production |
| Purge scope | The one Varnish instance = the whole cache | `caches.default` is **per-colo**: a purge clears only the colo whose worker handled the POST. In `wrangler dev --local` there is exactly one "colo", so the harness behaves like a global purge | the [Cloudflare purge API](https://developers.cloudflare.com/api/resources/cache/methods/purge/) (purge by URL/tag, zone-wide), or Enterprise **Cache-Tag** header passthrough — with real cache tags the KV index becomes unnecessary |
| Tag index | In-core xkey hashes, transactional | KV read-modify-write per key: concurrent fills across isolates can drop an append; entries expire after 24 h (matching nothing cacheable living longer than its `s-maxage` in practice — except `immutable` objects, which never need purging by design) | Enterprise Cache-Tags (no index at all) |

`vc`-bearing (claims) URLs need **no special handling** anywhere in this
worker — claims ride in the URL, so isolation falls out of full-URL keying.
That is the point of the protocol's URL contract.
