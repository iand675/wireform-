---
title: wireform-lattice (Haskell)
description: "The Haskell implementation of Lattice: parse an IDL with parseSchema, compile queries with compileText and planQuery, stand up an origin over the Backend contract, and walk the demo origin with curl."
sidebar:
  order: 4
  label: Haskell guide
---

`wireform-lattice` is the reference implementation of the
[Lattice protocol](../spec/): the schema IDL parser and semantic model, the
query-language parser and canonicalizer, the authorization path-join planner,
the NDJSON entity-stream wire format, an HTTP origin built on
[`wireform-http`](../../packages/http/), and an HTTP client with a normalized
entity store.

:::note
The compiler pipeline (`parseSchema`, `compileText`, `planQuery`) and the
wire/auth vocabulary documented here are the package's stable surface. The
HTTP server, HTTP client, and demo executable are landing alongside this page;
their sections below are shaped by the `Lattice.Backend` contract and will
firm up as those modules land.
:::

## Module map

| Module | Role |
|---|---|
| `Lattice.Types` | Protocol vocabulary: names, `Ref`, visibility policies and the `Level` join-semilattice (§8.1), the §3.5 type language, `Claims` |
| `Lattice.Schema` | The semantic schema model (§3.1) plus origin budgets (§14.1) |
| `Lattice.IDL.Parser` | `parseSchema`: IDL text → `Schema`, with schema-level checks |
| `Lattice.IDL.Print` | Canonical IDL printer: the schema's published, content-addressed form |
| `Lattice.Query.AST`, `Lattice.Query.Parser` | Query documents and the normative §4.8 grammar |
| `Lattice.Query.Validate` | Compile-time validation (`CompileError`) |
| `Lattice.Canonical` | §5.1 canonicalization: `compileText`, `Compiled` |
| `Lattice.Hash` | BLAKE3 identities: `queryHash`, `schemaHash`, `planIdHash`, manifest etags |
| `Lattice.Plan` | `planQuery`: the path join, slices, plan identity, `explainJson` (§7.3, §8.1, §20.2) |
| `Lattice.Cursor` | Deterministic, session-free keyset cursors (§3.2) |
| `Lattice.Compress` | Raw DEFLATE with the optional schema-derived dictionary (§5.2) |
| `Lattice.Wire` | NDJSON records, surrogate keys, protocol header names (§9, §10.5) |
| `Lattice.Backend` | The origin backend contract: set-in, map-out loaders |
| `Lattice.Backend.Memory` | In-memory backend for demos and tests |
| `Lattice.Server` | The origin HTTP handler on wireform-http (§6, §9-§11) |
| `Lattice.Server.Auth` | `vc` claims payload + pluggable proof verification, bundled HMAC (§8.2) |
| `Lattice.Client`, `Lattice.Client.Store` | HTTP client: transport ladder + normalized entity store |

## Parsing an IDL

`Lattice.IDL.Parser.parseSchema` takes IDL text (§3.4) to the semantic model
of `Lattice.Schema`, or a list of positioned errors. Elaboration also runs the
schema-level checks: dangling type references, unregistered claims in
policies, collections whose link field is missing on the target, interface
implementors missing declared fields, write sets naming unknown collections,
and `invalidates ⊇ writes`.

```haskell
-- Runnable (imports elided).
import Lattice.IDL.Parser (SchemaError (..), parseSchema)

loadSchema :: FilePath -> IO Schema
loadSchema path = do
  idl <- Data.Text.IO.readFile path
  case parseSchema idl of
    Left errs -> fail (unlines (map (show . seMessage) errs))
    Right schema -> pure schema
```

A minimal schema, in the shape of the Star Wars corpus the demo serves:

```
entity Review by id {
  visible to all by default

  id:         ReviewId
  episode:    Episode
  stars:      W8
  commentary: Text?
  createdAt:  Timestamp

  fetch by id: public
}

list reviews of Review by episode
     ordered by createdAt desc
     page 10 max 50
     public

mutation createReview(episode: Episode, stars: W8, commentary: Text?) returns Review {
  allow       public
  writes      Review(new), reviews(episode)
  invalidates writes
  effect      transactional
}
```

`Lattice.IDL.Print` renders a `Schema` back to its canonical text. This is the
published form whose `Lattice.Hash.schemaHash` content-addresses the schema
document served at `/schema/{schemaHash}` (§7.1).

## Compiling a query

Query compilation is two stages with one intermediate:

1. `Lattice.Canonical.compileText` parses, validates (§4.8), and
   canonicalizes (§5.1) query text into a `Compiled`: the restricted canonical
   document, the canonical text (the query's identity), and its `queryHash`
   (22-character base64url of BLAKE3-128).
2. `Lattice.Plan.planQuery` compiles a `Compiled` against the schema: resolves
   every field and edge, runs the authorization path join (§8.1), derives the
   slices and the plan id (§7.3), and checks the static budgets (roots, depth,
   fan-out; §14.1).

```haskell
-- Runnable (imports elided).
import Lattice.Canonical (Compiled (..), compileText)
import Lattice.Plan (planQuery, explainJson)
import Lattice.Schema (defaultBudgets)

compile :: Schema -> Text -> Either CompileError Plan
compile schema src = do
  compiled <- compileText schema defaultBudgets src
  -- compiledText compiled : canonical text, e.g. "query{hero{friends(first:10){name} name}}"
  -- compiledHash compiled : the /q/{hash} URL segment
  planQuery schema defaultBudgets compiled
```

The `Plan` is the execution contract the server consumes; `explainJson`
renders it as the §20.2 `explain` document (path joins, slices, rounds, keys,
budgets), and `planSliceRecord` produces the dataless `slice=plan` wire record
(§6.6).

## Standing up an origin

A deployment supplies one value: a `Lattice.Backend.Backend`. The record's
shape enforces the protocol's central execution constraint. Loaders are
**set-in, map-out**: one call per `(type, round)`, never per row, so N+1 is
inexpressible. Loads are policy-free. Backends fetch rows by key and know
nothing about callers; visibility is applied at emission by the server, per
response, against the response's slice and claims.

| Field | Signature (abridged) | What it loads |
|---|---|---|
| `beSnapshot` | `IO SnapshotToken` | The storage snapshot token for the current read (§13.1). |
| `beGetRoot` | `RootName -> Map ArgName Value -> IO (Either BackendFailure (Maybe Ref))` | Resolve a `get` root to at most one entity. |
| `beListRoot` | `RootName -> Map ArgName Value -> Window -> IO (Either BackendFailure Page)` | Scan a `list` root's collection at the given grouping-key arguments and window (whole bounded set, or a keyset page). |
| `beChildren` | `TypeName -> FieldName -> [(Ref, EntityRow)] -> Window -> IO (Map Ref (Either BackendFailure Page))` | Resolve a `has many` edge **for every parent in the round at once**: the set-in, map-out loader. |
| `beLoad` | `TypeName -> [Text] -> IO (Map Text (Either BackendFailure LoadResult))` | Load entity rows by key, batched per type per round; `LoadResult` distinguishes found, absent, and tombstoned. |
| `beComputed` | `TypeName -> FieldName -> Map ArgName Value -> EntityRow -> IO (Maybe Value)` | Evaluate an argument-taking field (e.g. `avatarUrl(size: 96)`) against a loaded row; `Nothing` elides the field. |
| `beMutate` | `MutationName -> Claims -> Map ArgName Value -> IO MutationOutcome` | Run one mutation effect. A committed outcome reports `WriteFact`s; the server enforces the declared write set over them (§11.4) and derives the `invalidated` record and purge keys from them. |

Failures are values, not exceptions. `BackendFailure` (with the
`loaderTimeout`, `upstreamUnavailable`, or `internalError` vocabulary) becomes
a scoped `error` record (§9.4.2) that degrades exactly the affected entities.

`Lattice.Backend.Memory` implements the contract over in-memory maps. It is
the backend the demo origin and the test suite run on.

For the `ctx` slice, the server checks the `vc` claims payload against its
proof with a `Lattice.Server.Auth.ProofVerifier`. The bundled scheme is
HMAC-SHA256 by a shared-secret auth service (`hmacVerifier`; `hmacProof`
mints proofs for tests and demos). The payload is carried in the URL and the
cache key, while the proof is carried in the `X-Vc-Auth` header outside the
cache key, so token rotation never disturbs cached entries (§8.2).

## Running the demo origin

`example-lattice` serves the Star Wars corpus schema
(`wireform-lattice/test/fixtures/starwars.lattice`) over the memory backend:

```bash
cabal run example-lattice
```

The walkthrough below assumes the demo's default address,
`http://localhost:8080`. The executable prints its address on startup.

**Discovery** (§7.1). One small, cacheable document names the endpoints,
budgets, and the current content-addressed schema:

```bash
curl -s http://localhost:8080/.well-known/lattice
```

```json
{
  "endpoints": { "query": "/q", "mutation": "/m", "entity": "/e", "schema": "/schema" },
  "schema":    { "current": "/schema/sQ81xZ0v" },
  "admission": "open",
  "queryMediaType": "application/x-lattice-query",
  "methods":   { "introduce": ["POST"] },
  "budgets":   { "maxDepth": 12, "maxRoots": 8, "maxRounds": 8 }
}
```

**Introduce a query** (§6.4). The POST introduction rung executes the query,
memoizes it, and grants the steady-state GET URL via `Location`:

```bash
curl -si 'http://localhost:8080/q?intent=introduce' \
  -H 'Content-Type: application/x-lattice-query' \
  --data 'query Hero { hero { name friends(first: 10) { name } } }'
```

```
HTTP/1.1 200 OK
Location: /q/8f2c41a9…?p=pl_9dK2…
Lattice-Plan: pl_9dK2…
Surrogate-Key: Human:1000 …

{"kind":"manifest","query":"8f2c41a9…","plan":"pl_9dK2…","slice":"pub","root":{"hero":["Human:1000"]},"etag":"m:…"}
{"kind":"entity","id":"Human:1000","ver":"…","fields":{"name":"Luke Skywalker","friends":{"$page":{"items":[{"$ref":"Human:1002"},…]}}}}
{"kind":"entity","id":"Human:1002","ver":"…","fields":{"name":"Han Solo"}}
{"kind":"end","complete":true,"etag":"m:…"}
```

The response is the normalized entity stream of §9: a manifest naming the
root refs, one record per entity touched (each emitted once, by identity),
and an `end` record that holds the weak manifest etag.

**Steady state: hash-form GET** (§6.1). Repeat traffic uses the granted URL,
a stable cache key on any RFC 9111 cache:

```bash
curl -s 'http://localhost:8080/q/8f2c41a9…?p=pl_9dK2…&slice=pub'
```

`GET /q/{hash}/source` and `GET /q/{hash}/explain` (§7.2) return the canonical
text and the compiled plan for any memoized hash.

**Point fetch** (§6.7). Entity URLs accept a field mask (canonicalized like
any selection); pinning a `ver` makes the response immutable:

```bash
curl -s 'http://localhost:8080/e/Human/1000?f=appearsIn,name'
curl -si 'http://localhost:8080/e/Human/1000?ver=e41&f=name'   # Cache-Control: …, immutable
```

**Mutation with an idempotency key** (§11). Mutations are named,
schema-declared operations invoked as `POST /m/{name}`. The response is an
entity stream that holds post-mutation state (read-your-writes with zero
follow-up requests) and an `invalidated` record that mirrors the purge set:

```bash
curl -s http://localhost:8080/m/createReview \
  -H 'Content-Type: application/json' \
  -H 'Idempotency-Key: order-2041-review' \
  -d '{"episode":"Jedi","stars":5,"commentary":"Great!"}'
```

```json
{"kind":"manifest","mutation":"createReview","root":{"result":["Review:41"]},"etag":"m:…"}
{"kind":"entity","id":"Review:41","ver":"…","fields":{"episode":"Jedi","stars":5,"commentary":"Great!"}}
{"kind":"invalidated","keys":["Review:41","reviews:Jedi"]}
{"kind":"end","complete":true}
```

Replaying the same `Idempotency-Key` within the retention window returns the
stored response with `Idempotency-Replayed: true`; a replayed key with a
different request body is rejected `422` (§11.2).

## The Haskell client

`Lattice.Client` walks the transport ladder (hash GET first, falling back to
introduction, re-teaching evicted origins as a side effect) and feeds
`Lattice.Client.Store`, a normalized entity store keyed by `Ref`. Entity
records upsert by `ver`, `tombstone` evicts, `elided` marks
visible-but-withheld, and a mutation response's `invalidated` keys mark
intersecting cached query results stale.

```haskell
-- Illustrative: the client surface is landing alongside this page;
-- names below sketch the shape and will firm up.
withLatticeClient config $ \client -> do
  result <- runQuery client heroQuery mempty
  -- result: the root refs plus a store view of every entity the stream carried
  ...
```

The wire vocabulary the client consumes comes from `Lattice.Wire`: record
types, tolerant decoding of unknown kinds and scopes (§9.4.1), and header
names. This vocabulary is shared with the server and mirrored by [the
TypeScript client](../typescript/).
