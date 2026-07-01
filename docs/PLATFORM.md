# Platform Charter

> wireform aims to be **the .NET platform of Haskell**: a broad, internally
> **coherent** surface of independent-but-dovetailing packages. This document is
> the governance contract that makes that coherence real — the cross-cutting
> "one X" decisions every `wireform-*` package must follow, the namespace +
> versioning + entry-module conventions, and the concretely-sequenced roadmap of
> the BCL-equivalent pillars.

This is a *living* charter. The decisions in §1–§8 are fixed; §9 (the roadmap)
records what is built versus what is planned so the table never lies about the
current state.

---

## §1 Purpose & non-goals

**Purpose.** A huge integrated surface where any combination of packages composes
predictably. Coherence comes from **convention + codegen + shared seam types**,
not from a unifying layer on top: each `wireform-*` package stays independently
usable, and two packages used together dovetail because they follow the same
seams.

**Non-goals** (each is a deliberate, permanent choice):

- **No umbrella facade package.** There is no `wireform` *library*. The umbrella
  `wireform.cabal` ships only the `wireform-gen` codegen CLI plus conformance /
  profiling / example executables. Coherence is delivered through the
  entry-module convention (§4) and shared seams (§2), not through a facade that
  re-exports every format.
- **No universal codec typeclass.** There is no `Codec` / `Serialise` class
  unifying all formats. Per-format `<Format>.Class` stays. This preserves the
  repo's strict allocation discipline: hot paths must not pay dictionary-dispatch
  overhead, and a unifying class would either force a boxed `Value` round-trip
  or specialize away. Packages compose by **shared conventions + shared seam
  types**, never by a top-level unifying abstraction.
- **No unified `Value` model.** Each format's `<Format>.Value` ADT mirrors its
  own type system. Unifying them would lose precision and add allocations.
- **No bespoke observability layer.** Observability is `hs-opentelemetry-api`
  (§6), not a wireform-internal tracing/metrics/log surface.
- **Crypto is parked indefinitely.** wireform does not maintain a cryptography
  stack. Format-level encryption (Parquet modular encryption, ORC stripe
  encryption) consumes keys the *application* supplies; wireform never owns key
  management.

---

## §2 The canonical-seam table ("one X")

For each cross-cutting concern there is exactly **one** blessed module/type and
one home package. New packages must use these; do not introduce a parallel seam.

| Concern | The ONE blessed surface | Home package |
|---|---|---|
| Byte-stream transport | `Wireform.Transport` (`ReceiveTransport` / `SendTransport`) — concrete impls `Wireform.Network.*` | `wireform-core` (+ `wireform-network`) |
| Byte-level parsing | `Wireform.Parser` (`Pure` / `Stream` modes) + `Wireform.Parser.Driver` | `wireform-core` |
| Byte-level building | `Wireform.Builder` | `wireform-core` |
| Record-level pull stream | `Columnar.Stream.Iter` / `IterIO` (+ `iterChunk`, `iterScan`, `iterMergeBy`, `iterIOPrefetch`, `iterParallelMap`) | `wireform-columnar-core` |
| File / resource open | `Columnar.IO.loadFile` (mmap ≥ 64 KiB, eager below) → `openXReader :: FilePath -> IO (Either … (…, IterIO …))` | `wireform-columnar-core` + per-format reader |
| Hashing | `Wireform.Hash` (XXH64, Murmur3, etc.) | `wireform-core` |
| Base64 | `Wireform.Base64` | `wireform-core` |
| Annotation deriver vocabulary | `Wireform.Derive.Modifier` / `ModifierInfo` / `Extension` | `wireform-derive` |
| Dynamic value model | per-format `<Format>.Value` — **deliberately NOT unified** | per-format package |
| Decode error / diagnostic boundary | `Wireform.Diagnostic` *(planned — see Roadmap R1; not yet built)* | `wireform-core` |
| Observability | `hs-opentelemetry-api` `>= 1.0 && < 1.1` (API only, never the SDK) | external |
| DI / composition + resource lifetime | `fractal-layer` (`Layer`) | external |

> Where the table marks something *planned*, it is not yet built — the table
> records the target, not a fiction. `Wireform.Diagnostic` is the only planned
> row today.

---

## §3 Namespace rule

The de-facto convention is now codified:

| Namespace | Meaning |
|---|---|
| `Wireform.*` | wireform-owned **cross-cutting infrastructure**: the zero-copy parser / builder / ring / transport / hash primitives that define the performance contract (`wireform-core`, `wireform-derive`), plus the cross-format columnar facade (`Wireform.Columnar` in `wireform-columnar`). |
| `<Format>.*` | serde / format packages own their format namespace and **ship a bare `<Format>` entry module** (§4). |
| `Network.<Proto>.*` | standard wire-protocol / RPC implementations, adopting each protocol's conventional Haskell namespace: `Network.HTTP`, `Network.HTTP2`, `Network.GRPC`, `Network.WebSocket`, `Network.Connect`. |

**One sanctioned exception:** `wireform-kafka` / `wireform-kafka-protocol` use
`Kafka.*` (not `Network.Kafka.*`) for legacy `hw-kafka-client` source
compatibility. This exception is grandfathered; **do not extend it**. New
protocol packages use `Network.<Proto>.*`.

Rule for a new capability — *where does it live?*

- **Cross-cutting** (a parser/builder/transport/hash/stream/IO primitive many
  packages share) → a `Wireform.*` module in the lowest appropriate foundation
  package (`wireform-core` / `wireform-columnar-core`).
- **A format** (its own encode/decode/value/derive) → its own `wireform-<format>`
  package under `<Format>.*`, with a bare `<Format>` entry module.
- **A protocol** (an HTTP/RPC/WebSocket implementation) → `Network.<Proto>.*`
  (except the Kafka grandfather).

---

## §4 Entry-module convention

Every **serde-format** package MUST expose a single bare `<Format>` module that
re-exports its public codec surface. `Proto`, `ORC`, and `Kafka` already do;
this convention rolls the rest out. This is how independent packages dovetail
**without** a facade library — `import CBOR` is the whole everyday API.

**What the entry module re-exports** — the format's core codec surface:

- `<Format>.Class`, `<Format>.Encode`, `<Format>.Decode`, `<Format>.Value`,
  `<Format>.Derive` — always.
- Close cousins that are part of the everyday API when they exist:
  `<Format>.Wire`, `<Format>.Encoding`, `<Format>.Container`, `<Format>.Stream`.
- For CBOR only, `CBOR.Diagnostic` is the RFC-8949 *diagnostic-notation
  renderer* (not an error type — see §5) and is part of the everyday surface.

**What the entry module does NOT re-export** — specialized surfaces the user opts
into directly (mirroring how `Proto.hs` omits the parser/codegen modules):

- IDL / codegen: `*.Schema`, `*.Schema.Parse`, `*.Parser`, `*.CodeGen`,
  `*.SchemaLang`, `*.CDDL*`, `*.ISL*`, `*.IDL*`.
- Registry: `*.Registry`.
- QuasiQuoters: `*.QQ`.
- JSON bridge: `*.JSON`.
- Format-specific extras that are not the everyday codec path: `*.Protocol`,
  `*.RPC`, `*.Resolution`, `*.Fingerprint`, `*.MetaString*`, `*.TagRegistry`.
- Internal / unsafe: `*.Internal*`, `*.Unsafe`.

**Cross-format columnar work** uses `Wireform.Columnar` (in `wireform-columnar`),
not a per-format entry module.

**Columnar / table packages** (arrow / parquet / iceberg / delta / lance / hudi)
keep their granular module layout plus the `Wireform.Columnar` cross-format
facade; a per-package top-level entry module is *encouraged but optional*
(roadmap — `ORC` is the existing model).

### Re-export precedence (export-clash tie-breaker)

GHC errors ("conflicting exports") when two re-exported modules export *different
entities* under the same unqualified name — the usual case is a `<Format>.Class`
method named `encode` / `decode` colliding with the top-level
`<Format>.Encode` / `.Decode` functions. Resolve by this fixed precedence,
**highest wins**:

```
Value  >  Wire/Encoding/Container  >  Encode  >  Decode  >  Derive  >  Class
```

For the **lower-priority** module in any clash, replace its whole-module
`module X` re-export with an explicit re-export of just the names that do *not*
clash (e.g. keep `<Format>.Class`'s typeclasses `To<Format>` / `From<Format>` and
their instances, drop only the clashing `encode` / `decode` method names since
the top-level functions win). The build names the conflicting symbol; this
precedence names which side drops it. No design judgment is involved.

---

## §5 Error / diagnostic contract

**Target (implemented incrementally per Roadmap R1):** per-format decoders keep
their fast internal representation; the **public** `Either` boundary converges
on `Either Wireform.Diagnostic a`.

`Wireform.Diagnostic` (to be added to `wireform-core`):

```haskell
data DiagnosticKind
  = UnexpectedEnd
  | Malformed
  | SchemaViolation
  | UnsupportedFeature
  | Custom

data Diagnostic = Diagnostic
  { diagFormat  :: !Text          -- e.g. "CBOR", "Proto"
  , diagOffset  :: !(Maybe Word64) -- byte offset, when the decoder computes one
  , diagPath    :: ![Text]         -- field path, only schema-bearing formats populate
  , diagKind    :: !DiagnosticKind
  , diagMessage :: !Text
  }
```

with `Show` / `Exception` instances and a `prettyDiagnostic :: Diagnostic -> Text`.
Adapters `parseErrorToDiagnostic` / `chunkParseErrorToDiagnostic` fold in the
existing `Wireform.Parser.Error.ParseError` (Word64 offset) and
`Wireform.Parser.Adapter.ChunkParseError` (message + context + Word64 offset).

**Rules:**

- **Byte offset is mandatory when the decoder computes one.** Today `wireform-proto`
  tracks an offset internally and then discards it at the public boundary — that
  stops. 24 of 27 formats currently return `Either String <Value>`; the migration
  is a clean cutover per format (R1), no shims.
- **Field path is optional** and only schema-bearing formats (proto, avro,
  thrift, …) populate it. The only field-path machinery today is
  `Proto.Decode.Collect.DecodeIssue { issuePath :: [Text], … }`.
- **`CBOR.Diagnostic` is unrelated** — it is RFC-8949 diagnostic *notation*, a
  human-readable rendering of a CBOR value, not an error type. Do not confuse the
  two.
- **Value ADTs stay per-format** (§1). `Diagnostic` describes a *failure*, not a
  *value*.

---

## §6 Observability mandate

**Mandate:** `hs-opentelemetry-api` (`< 1.1`). Depend on the **API package
only, never the SDK.** Spans and instruments are no-ops until the *application*
installs an SDK provider, so library instrumentation ships unconditionally and
safely — a library that depends on the SDK would force a backend on its users.

**Version bound:** consumers straddle the pre-1.0 and 1.0 APIs behind a
`MIN_VERSION_hs_opentelemetry_api(1,0,0)` CPP seam (the propagator carrier
changed from raw `RequestHeaders` to a `TextMap` at 1.0) and so bound
`>= 0.2 && < 1.1`. The **metrics** API (`OpenTelemetry.Metric.Core`) was added
in 1.0 and does not exist pre-1.0, so a module that *uses* metrics guards the
whole bridge under the same seam and simply omits it on the old API — e.g.
`wireform-kafka`'s `Kafka.Streams.Observability.OpenTelemetry` keeps only its
metrics-free surface (`streamsInstrumentationScope`, `sanitizeInstrumentName`)
when built against 0.x. Only a component that actually *exercises* the metrics
bridge (the streams test / example) must bound `>= 1.0 && < 1.1`.

**Library rules:**

- Obtain a `Tracer` via `makeTracer provider instLib tracerOptions` from a
  provider the **application** supplies (the library never constructs a global
  provider).
- Use `OpenTelemetry.Trace.Core.inSpan''` for library-internal spans — the raw,
  no-`code.*`-attribute variant the Haddock recommends for instrumentation
  libraries. Use `inSpan'` only when the body needs the live `Span`.
- Propagate cross-process context via `OpenTelemetry.Context` +
  `OpenTelemetry.Propagator` (`inject` / `extract`).
- Metrics live in `OpenTelemetry.Metric.Core` (in the API package as of 1.0).

**Canonical reference modules:** `Kafka.Telemetry.OpenTelemetry` (producer /
consumer / transaction spans + header trace-context propagation, built directly
on `hs-opentelemetry-api`), `Network.HTTP.Client.Tracing`, and
`Network.GRPC.{Server,Client}.Otel` (server / client RPC spans + W3C
trace-context extraction).

**Fully conforming:** as of the R3 migration, `wireform-grpc`'s
`Network.GRPC.{Server,Client}.Otel` build directly on `hs-opentelemetry-api`
(`grpcTracer` yields a real `Tracer`; spans are real `OTel.Span`s). The former
hand-rolled `GrpcTracer` record is gone; no serde / RPC package now ships a
bespoke tracer surface.

---

## §7 DI / composition mandate

**Mandate:** `fractal-layer` for application wiring + resource lifetimes.

**Model:** `Layer m deps env` is approximately `deps -> m env` plus guaranteed
finalizers.

- **Builders:** `effect` (no cleanup) · `resource` (acquire + release) ·
  `bracketed` (adapt an existing `withFoo`).
- **Runners:** `withLayer` (production; releases in reverse order) · `runLayer`
  (tests / REPL).
- **Cached singletons:** `mkService` / `service`, keyed by the output `TypeRep`.
- **Composition:** `>>>` (sequential) · `&&&` (parallel via `async`) · `<|>`
  (fallback with cleanup of the failed branch).

**OTel bridge:** a `LayerInterceptor` whose hooks open `hs-opentelemetry` spans
is the sanctioned way to observe startup.

> **Caveat:** `fractal-layer` is `0.1.0.0` / experimental and requires
> `MonadUnliftIO m` + `Typeable env`. It is mandated as the standard; if it
> proves unstable during adoption (R4), the documented fallback is
> `ReaderT env IO` + `resourcet` following the same "deps → env with managed
> lifetime" shape — but that decision is deferred to R4, not now.

---

## §8 Versioning & package taxonomy

**PVP** across all packages. Restating the existing `wireform-derive` rule (see
`AGENTS.md`): any `Modifier` constructor add is an API-breaking bump — bump
`wireform-derive` to a new version, extend `ModifierInfo` + a `ConflictX`
constructor, and wire `mergeOne` / `shadowOne`.

**Package tiers:**

| Tier | Packages | Role |
|---|---|---|
| Foundation | `wireform-core`, `wireform-derive`, `wireform-columnar-core` | The zero-copy parser/builder/ring/transport/hash/stream/IO primitives + the deriver core. Define the performance contract; nearly everything depends on these. |
| Shared | `wireform-columnar` | The cross-format `Wireform.Columnar` facade over Arrow / Parquet / ORC. |
| Serde formats | `wireform-{cbor,msgpack,thrift,avro,bson,ion,edn,toml,yaml,bencode,fory,csv,ndjson,asn1,bond,flatbuffers,capnproto,proto,xml,html}` | One format per package under `<Format>.*`, each with a bare `<Format>` entry module. |
| Columnar / table | `wireform-{arrow,parquet,orc,iceberg,delta,lance,hudi}` | Granular module layout + the `Wireform.Columnar` facade. |
| Network / RPC | `wireform-{http,http1,http2,grpc,connect,kafka,kafka-protocol,websocket,network}` | `Network.<Proto>.*` (Kafka grandfathered). |
| Codegen / IDL surfaces | the `*.CodeGen` / `*.Parser` / `*.Schema` modules + `wireform-gen` CLI + per-format codegen exes | Live inside their format package; the umbrella CLI orchestrates multi-format generation. |

**Where a new capability lives** is the §3 rule: cross-cutting → `Wireform.*` in
a foundation package; a format → its own `wireform-<format>`; a protocol →
`Network.<Proto>.*`.

---

## §9 Roadmap

Each entry is concrete enough to become its own plan. **R1–R6 are endorsed and
sequenced; R7 is a candidate list, not committed work.**

### R1 — `Wireform.Diagnostic` + per-format error migration

Add `Wireform.Diagnostic` (§5) to `wireform-core` — it is the near-universal
lowest dep (`toml` / `yaml` / `delta` gain only a trivial new edge). Provide
`parseErrorToDiagnostic` / `chunkParseErrorToDiagnostic` from the existing
`Wireform.Parser.Error` / `Adapter` types. Then migrate each per-format public
decode boundary from `Either String` / `Either DecodeError` to
`Either Diagnostic a`, **Proto first** (it already has structure and an offset
to stop discarding). Breaking; clean cutover, no shims; parallelizable per
format; update each package's tests/examples.

### R2 — Streaming convergence

Give `Delta.IO` / `Lance.IO` / `Hudi.IO` the blessed open-pattern
(`Columnar.IO.loadFile` + an `openX :: FilePath -> IO (Either Diagnostic (…,
IterIO …))`, mirroring `Parquet.Read.openParquetReader`), while keeping the
existing materialized helpers. Today these three diverge (raw `BS.readFile`,
materialized table types, no `IterIO`).

### R3 — Observability conformance

**Done:** `wireform-grpc`'s `GrpcTracer` has been replaced by a direct
`hs-opentelemetry-api` `Tracer` (`grpcTracer`); library-internal spans are real
`OTel.Span`s created via `createSpan` and the redundant `noopTracer` is gone.
**Remaining:** evaluate instrumenting the currently-uninstrumented network
packages (`wireform-http1` / `http2` / `websocket` / `connect`).

### R4 — DI / composition adoption

Wire the server / runtime packages and the example executables through
`fractal-layer` `Layer`s; add the `LayerInterceptor` → OTel bridge (§7) as a
reusable helper. (This is where the `fractal-layer`-vs-`resourcet` stability
decision is made.)

### R5 — HTML templating + hot reload (Razor / Blazor analog)

Build on `wireform-html`'s existing parser + DOM + selectors + rewriter, **not**
on a blaze-style builder.

- **Prod path:** a TH splice (`templateFile` / QuasiQuoter, modeled on
  `HTML.Derive`) compiles a template into typed Haskell producing / mutating an
  `HTML.DOM` tree — bindings and URLs are type-checked at compile time.
- **Dev path:** the splice emits a `templateRuntime` that mtime-checks +
  re-parses via the existing `HTML.Parse` (shakespeare's
  `hamletFile` / `hamletFileReload` split, but DOM-native), gated by a `devel`
  cabal flag — **no `hint`, no `foreign-store`** binary-compat hazard.
- Reserve `rapid` / `ghcid` (behind `devel`) for live *application-code* reload
  only; document the restart-on-type-change rule.

No new heavy deps for the templating dev loop itself.

### R6 — Coherence cleanup

Remove the orphaned `http-semantics` from `cabal.project` (or document why it is
vendored — it currently has no wireform consumer; `wireform-http2` replaces it).
Correct `wireform-connect.cabal`'s prose: the library depends on `grpc-spec`
(not `wireform-grpc`, as the prose claims).

### R7 — Candidate pillars (pending owner prioritization; NOT yet endorsed)

Derived from a full BCL comparison in [`docs/dotnet-gap-analysis.md`](dotnet-gap-analysis.md)
(the .NET surface that wireform's mission overlaps, minus what it already
covers). That doc is the durable context; the list below is its actionable
distillate, in priority order. None is committed work; each has a starting
point. Pillars deliberately ceded to the Haskell ecosystem (general collections,
regex, filesystem, numerics) or parked (crypto) are enumerated as the
gap-analysis "out-of-scope" set — do not build them.

**Tier 1 — high alignment (compose existing strengths):**

- **Configuration + Options.** A layered, typed config system that binds through
  the existing format decoders (TOML/YAML/JSON/proto) + the deriver — turning
  wireform's format breadth into the `Microsoft.Extensions.Configuration` /
  `Options` analog. Pairs with `fractal-layer` (R4).
- **Query layer (LINQ-equivalent).** Typed query combinators over
  `Columnar.Stream.Iter` + the predicate-pushdown vocabulary, lowering to
  columnar execution — the `IQueryable`→pushdown analog for the lakehouse stack.
- **DB wire-protocol clients.** Postgres / MySQL / Redis frontend↔backend
  protocols beside `wireform-kafka` ("wire protocol" *is* the mission) — closes
  the OLTP gap without an ORM. Start with a `wireform-postgres` (v3 protocol).
- **Web framework layer.** Routing + minimal-API handlers over `wireform-http`
  (model binding via the deriver); the natural superset of **R5** (HTML
  templating).

**Tier 2 — medium alignment (the network/runtime stack needs them):**

- **Resilience pipeline.** Generalize the http retry into
  retry/circuit-breaker/hedging/timeout/rate-limiter over `Wireform.Transport`
  (the Polly + `System.Threading.RateLimiting` analog).
- **Structured-logging facade.** The one observability gap (OTel covers
  trace+metrics, not an `ILogger` equivalent): adopt the OTel logs API or bless
  `co-log` / `katip` so libraries log uniformly.
- **Structured concurrency + a blessed channel.** Evaluate `ki` for
  scopes/cancellation, plus a sanctioned channel/queue type (the
  `System.Threading.Channels` analog) the network stack standardizes on.
- **Temporal types + RFC-3339 / duration formatting.** A tz-aware time type over
  `time` + the IANA TZ database (the useful subset of globalization; full
  ICU collation/calendars stays ecosystem-owned), with no `String` round-trips.
  Proto WKT, CEL, and Iceberg already need it.
- **Unified compression surface.** One `Wireform.Compress` over the
  gzip/zstd/lz4/brotli/snappy codecs already shipped scattered, + an optional
  zip/tar archive reader/writer.
- **General record validation.** Lift `protovalidate` / `cel` from proto-only to
  "validate any derived record" — the `DataAnnotations` analog, riding the
  deriver.

**Composition pillar (high alignment; sequences after the Redis client + `ki`):**

- **Distributed / hybrid cache.** A typed cache whose **value codec is any
  wireform format** (the `IDistributedCache` / `HybridCache` "configurable
  serialization" knob is wireform's core competency) with an L2 backed by
  wireform's own **Redis (RESP) / Memcached** wire client (the DB-wire-client
  pillar). L1 leans on an existing Haskell concurrent map (`stm-containers` /
  `cache`) — wireform does **not** reinvent the in-memory LRU. The value-add is
  the abstraction + typed codec + distributed backends + **single-flight
  stampede protection** (the structured-concurrency primitive) + tag-based
  invalidation. Precedent: `wireform-http`'s RFC-9111 response cache; internal
  consumer: a `(path, version)` metadata cache for Parquet footers / Iceberg
  manifests. Gated on the DB-wire-client and concurrency pillars.
