# @wireform/lattice

A zero-runtime-dependency TypeScript client for the [Lattice](../website/src/content/docs/lattice/spec.md)
cache-native graph query protocol, with Apollo-style React bindings.

```
@wireform/lattice        the protocol client: gql, LatticeClient, store, mergeQueries
@wireform/lattice/react  LatticeProvider, useLatticeQuery, useLatticeMutation
```

The library has **no runtime dependencies**. React is an optional peer
dependency used only by the `/react` entry point.

```bash
cd lattice-ts
npm install
npm run typecheck        # strict tsc over the library
npm run example:dev      # the Star Wars dashboard (see below)
```

## Queries: the `gql` tag

`gql` parses the Lattice query grammar (spec §4.8) at module scope and returns
a frozen document; malformed queries throw at import time, with line/column.
The optional type parameter types the denormalized result.

```ts
import { gql } from "@wireform/lattice";

const HERO = gql<{ hero?: Array<{ __ref: string; name?: string }> }>`
  query Hero($episode: Episode) {
    hero(episode: $episode) {
      name
      ... on Human { homePlanet friends(first: 10) { name } }
      ... on Droid { primaryFunction friends(first: 10) { name } }
    }
  }
`;
```

Everything GraphQL-shaped works the way you expect: arguments, variables with
defaults, `fragment Name on Type { ... }` + `...Name` spreads (expanded
locally at canonicalization), `... on Type { ... }` inline fragments for
interface/union targets, and `@depth(n)` for recursion.

## The client and its transport ladder

```ts
import { LatticeClient } from "@wireform/lattice";

const client = new LatticeClient({
  base: "https://api.example.com",
  claims: { org: 123 },          // optional: ctx-slice visibility claims (§8.2)
  vcAuth: "1751730000.sig",      // optional: X-Vc-Auth proof for the claims
});

const { data, manifest, errors, complete } = await client.query(HERO, { episode: "Empire" });
```

Each `query()` climbs the spec §6 ladder, cheapest viable rung first:

1. **Hash-form GET** (`GET {base}/q/{hash}?p={planId}&slice=…&{vars}`) — the
   steady state, used once the URL is known. On `404 lattice:unknown-query`
   (origin memo evicted) or `409 lattice:plan-superseded` the client forgets
   the URL and falls down a rung, which re-teaches the origin.
2. **Inline GET** (`GET {base}/q?d={b64url(deflate-raw(canonicalText))}&…`) —
   self-contained, no origin state. Used when the platform has
   `CompressionStream("deflate-raw")` and the compressed text fits the ~6KB
   URL budget. No compression dictionary is used (`dv` is omitted).
3. **POST introduce** (`POST {base}/q?intent=introduce`, body = canonical
   text, `Content-Type: application/x-lattice-query`).

After *any* response the client caches the hash-form URL from
`Location`/`Content-Location` (and the plan id from `Lattice-Plan`) per
canonical text. **The client never computes BLAKE3** — the introduction
handshake is how it learns its steady-state URL; that is the spec's design
(§6.3), and it keeps the client dependency-free.

`200` is a clean response; `207 Multi-Status` is degraded-but-usable (the body
parses normally and scoped `error` records surface in `result.errors`); other
statuses raise `LatticeHttpError` carrying the RFC 9457 problem document.

### Client-side canonicalization is a local cache key

The client canonicalizes schema-free: it expands local fragments, sorts
fields/args/variables, erases the query name, and renders with minimal
separators. Two things it *cannot* do without the schema: erase arguments
equal to schema-declared defaults, and expand schema-declared fragments. That
is fine by construction — the origin re-canonicalizes on introduction (§5.2),
so the client string only needs to be a *stable local* identity for caching
and batching. Denormalization compensates for server-side default erasure with
a same-field-name fallback (if the wire keyed `friends` where the client
computed `friends(first:10)`, and only one `friends*` key exists, it resolves).

## Merged queries: `mergeQueries`

Multiple parsed documents merge into ONE multi-root request (spec §4.3).
Shared root fields with identical canonical arguments union their selection
sets recursively:

```ts
import { gql, mergeQueries, canonicalize } from "@wireform/lattice";

const heroName = gql`query { hero { name } }`;
const heroHome = gql`query { hero { ... on Human { homePlanet } } }`;

const { merged, assignments } = mergeQueries([heroName, heroHome]);
canonicalize(merged);
// => query{hero{name ... on Human{homePlanet}}}
// assignments => [{ index: 0, roots: ["hero"] }, { index: 1, roots: ["hero"] }]
```

`assignments` records which roots each input document needs (and carries its
expanded form), so each consumer's result is sliced back out of the shared
normalized store independently.

The same root selected with **different arguments** throws `UnmergeableError`
naming the root: Lattice has no aliases, so `hero(episode: EMPIRE)` and
`hero(episode: JEDI)` cannot share one response (the manifest's `root` map
keys by bare root name, §9.2). Callers split into separate requests — which is
what the protocol wants anyway, since the two have different cache identities.
Conflicting variable declarations (same name, different type/default) are
likewise unmergeable.

## The store and denormalization

Responses are normalized entity streams, not trees (§9.1). `LatticeStore` is
a version-keyed entity map patched by every response and mutation uniformly:

- an entity record whose `ver` differs from the stored one **replaces** the
  fields it carries (unknown fields at the new version drop);
- the same `ver` **unions** fields (both responses are facts about one version);
- `tombstone` evicts; `unchanged` is a no-op; `elided` is never treated as
  nonexistence (§9.3).

`denormalize(doc, vars, store, manifest)` walks a query's selections against
the store and builds the plain tree components consume:

- to-one edges (`{$ref}`) → nested objects;
- paginated edges (`{$page}`) → `{ items, next, prev, total? }`;
- bounded collections (plain ref-string arrays) → arrays;
- missing/elided entities → `undefined`;
- parameterized fields resolve by their canonical wire key
  (`avatarUrl(size:48)`, args sorted, variables substituted) and are exposed
  under both the bare field name and the canonical key;
- every entity carries its `__ref`.

Root fields always denormalize to an **array** of entities (the client cannot
know whether a root is a `get` or a `list` without the schema); `get` roots
are single-element arrays.

## React bindings and same-tick batching

```tsx
import { LatticeProvider, useLatticeQuery, useLatticeMutation } from "@wireform/lattice/react";

function ReviewsPanel({ episode }: { episode: string }) {
  const { data, loading, error, stale, refetch } = useLatticeQuery(REVIEWS, { episode });
  const [createReview, { loading: saving }] = useLatticeMutation("createReview");
  // ...
}
```

Every `useLatticeQuery` that mounts within the same microtask tick (one
synchronous render pass, same client/claims) is collected, merged with
`mergeQueries`, and issued as **one HTTP request**; unmergeable documents fall
back to their own requests transparently. The batcher lives in
`client.queryBatched`, so it is testable without React.

Hooks subscribe to the store through `useSyncExternalStore`, keyed by the refs
their last result was assembled from — an entity update re-renders exactly the
hooks whose refs changed.

Mutations (`POST {base}/m/{name}`, optional `Idempotency-Key`) stream their
response into the same store, so entity fields are read-your-writes with zero
follow-up requests (§11.3). The response's `invalidated` record is intersected
against every cached query result's key set (`Surrogate-Key` header ∪ response
refs); intersecting results are marked stale and their active watchers refetch
with `Cache-Control: no-cache` (§11.6). `MutationResult.committed` implements
the §9.4.3 commit test: at least one `entity`/`tombstone`/`invalidated` record
arrived — a scoped error on the output selection never retracts a commit.

## Deliberately absent, per the spec

- **Aliases** (§4.1): no production exists for them in the grammar, so
  `empireHero: hero(...)` is a parse error. Replacements: separate queries
  (each with its own cache identity), or schema-declared roots combined in a
  multi-root query. This library's `mergeQueries`/batching is the ergonomic
  recovery for the common "several components, one request" case.
- **`@include` / `@skip`** (§4.7): conditional selection multiplies cache
  identities by the toggle space. Replacements, in order: separate queries
  (see `useLatticeQuery`'s `skip` option for entering a UI state), shared
  fragments across variants, or over-fetching a cheap field.
- **Client-computed query hashes**: identity is learned from the origin via
  `Location`, never computed (§6.3).
- **A schema**: this client is deliberately schema-free; everything
  schema-dependent (default erasure, slice partition, validation beyond the
  grammar) is the origin's job and arrives via re-canonicalization and plan
  discovery.

## The example app

`example/` is a Vite + React Star Wars dashboard (the corpus schema): a
`HeroCard`, a `ReviewsPanel` with a `createReview` form demonstrating
`invalidated`-driven refetch, and a `SearchPanel` dispatching
`... on Human | Droid | Starship`. All three declare their own `gql` queries;
a debug footer shows the request counter and the merged canonical text — on
first paint the three queries arrive as **one** request.

```bash
npm run example:dev                                  # expects a Lattice origin on :8917
VITE_LATTICE_BASE=http://localhost:9000 npm run example:dev   # point elsewhere
```

Without a running origin, add `?mock` to the page URL (or set
`VITE_LATTICE_MOCK=1`): the app runs against an in-browser mock origin that
honestly implements the ladder (inline `d=` decompression, introduction
memoization with `Location` grants, hash-form GETs, `Surrogate-Key` headers,
and `invalidated` records on mutation).
