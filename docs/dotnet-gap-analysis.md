# .NET BCL → wireform gap analysis

> Input to the [Platform Charter](PLATFORM.md) §9 **R7** ("candidate pillars").
> This document captures the comparison so it doesn't have to be re-gathered:
> what the official .NET libraries implement, what wireform covers today, and —
> the actionable part — which gaps are *in-scope* for wireform's mission versus
> deliberately ceded to the Haskell ecosystem or parked.

**Sources for the .NET side:** Microsoft Learn "Overview of core .NET libraries"
(`learn.microsoft.com/dotnet/standard/class-library-overview`, BCL `System.*`
taxonomy), the `dotnet/extensions` repo (the `Microsoft.Extensions.*`
production-app suite: DI, Logging, Configuration, Hosting, Options, Caching,
Http.Resilience), and the ASP.NET Core / EF Core / SignalR / `System.Text.Json`
/ `System.IO.Compression` docs. **wireform side:** the package inventory in
`AGENTS.md` "Module layout" + this repo.

## Framing

A 1:1 comparison misleads. .NET's surface spans GUI (WPF/MAUI), general
collections, reflection, and cryptography — most of which Haskell's
`base`/`containers`/`text`/`time`/`vector` ecosystem already owns and wireform
should **not** reinvent (and crypto is parked per the charter). The meaningful
question is narrower: *within wireform's mission — data on the wire + over the
network + the app scaffolding around it — where is .NET's "production-ready app"
surface ahead?*

Legend: ⭐ wireform exceeds .NET · ✅ covered · 🟡 partial · ❌ in-scope gap ·
⛔ out-of-scope / ecosystem-owned / parked.

## Master table

| .NET pillar | .NET surface | wireform today | Verdict |
|---|---|---|---|
| **Serialization** | `System.Text.Json`, `System.Xml`, `DataContract`, binary | ~30 formats: proto/avro/thrift/cbor/msgpack/bson/ion/edn/toml/yaml/bencode/fory/csv/ndjson/asn1/bond/flatbuffers/capnproto/xml/html + the deriver | ⭐ far exceeds |
| **Columnar / lakehouse** | *(none native)* | Parquet/Arrow/ORC + Iceberg/Delta/Hudi/Lance (time-travel, scan planning, pushdown, REST/Glue/Hadoop/SQL catalogs) | ⭐ no .NET equivalent |
| **Source-gen / metaprogramming** | Roslyn source generators, `System.Reflection(.Emit)` | TH `wireform-derive`, `wireform-gen` CLI, per-format codegen + QQ | ✅ (compile-time, reflection-free) |
| **HTTP / RPC / sockets** | `HttpClient`, Kestrel, `System.Net.Sockets`, gRPC, TLS | http/http1/http2/grpc/connect/websocket/network (sockets/io_uring/OpenSSL), hermes | ✅ strong; native **Kafka** client ⭐ |
| **Interop / FFI** | P/Invoke, marshaling, `Span` interop | `Wireform.FFI` + extensive cbits (SIMD/decode/hash); zero-copy `Parser`/`Builder`/`Ring` (`Span<T>` analog) | ✅ for its needs |
| **Validation** | `DataAnnotations` | `wireform-protovalidate` (CEL rules, refinement types) + `wireform-cel` | ✅ proto; 🟡 general |
| **Observability** | `Activity` (trace), `Diagnostics.Metrics`, `Extensions.Logging` | `hs-opentelemetry-api` mandate (http / kafka / grpc, all conforming; grpc migrated in R3) | 🟡 trace/metrics ok; **no logging facade** |
| **Compression** | `System.IO.Compression` (Deflate/GZip/Brotli/Zip), `Formats.Tar` | gzip/snappy/lz4/zstd/brotli codecs, but **scattered** per-format, behind flags | 🟡 no unified surface; no zip/tar archive |
| **Query / LINQ** | `System.Linq`, `IQueryable`→remote | `Columnar.Stream` Iter + predicate pushdown (proto-query) | 🟡 no general query layer |
| **DI / hosting / config / options** | `Extensions.{DependencyInjection,Hosting,Configuration,Options}` | `fractal-layer` mandate (R4, not yet adopted) | ❌ no config/options/hosting analog |
| **Resilience** | `Http.Resilience`/Polly (retry, circuit-breaker, hedging, timeout), `RateLimiting` | http retry (honors `Retry-After`) + RFC-9111 cache only | 🟡 retry only |
| **Caching** | `Extensions.Caching`: `IMemoryCache` / `IDistributedCache` / `HybridCache` | `wireform-http` RFC-9111 response cache only | 🟡 in-scope as a typed/distributed cache (pillar #11); generic in-memory map ecosystem-owned |
| **Concurrency / async** | TPL, `Channels`, `CancellationToken`, `Parallel`, `PeriodicTimer` | `iterIOPrefetch`/`iterParallelMap`, `Ring` (SPSC); leans on Haskell async/stm | 🟡 no blessed structured-concurrency / channel standard |
| **Date / time / globalization** | `DateTime`, `TimeSpan`, `Globalization` (ICU, calendars) | CEL `Timestamp`/`Duration` (IANA tz); proto WKT; otherwise Haskell `time` | ❌ no shared temporal/format layer |
| **Data access (OLTP)** | ADO.NET, EF Core | *(none)* — only analytical/lakehouse | ❌ no DB wire-protocol client / ORM |
| **Web framework** | MVC, minimal APIs, Razor, Blazor, SignalR | server + middleware; R5 = HTML templating on `wireform-html` (planned) | 🟡 server yes; routing/templating/realtime no |
| **Collections** | `Collections.{Generic,Concurrent,Immutable}` | Haskell `containers`/`vector`/`stm` | ⛔ ecosystem-owned |
| **Regex** | `RegularExpressions` | XPath (`XML.Path`), CSS (`HTML.Selector`), CEL regex | ⛔ general engine ecosystem-owned |
| **Filesystem** | `File`/`Directory`/`Path`/`FileSystemWatcher`/`Pipes`/`MemoryMappedFiles` | `Columnar.IO.loadFile` (mmap) ✅; rest via `directory`/`filepath` | ⛔ except mmap |
| **Numerics** | `BigInteger`, `Complex`, `Vector<T>`, `Tensor<T>`, matrices | `Wireform.Hash`/`Columnar.SIMD` (codec SIMD) ✅; `Integer` = BigInteger | ⛔ general numerics ecosystem-owned |
| **Cryptography** | `Security.Cryptography`, X509, Claims/Principal | consumes keys (Parquet/ORC AES, TLS, SASL/SCRAM, XXH64) but owns no crypto | ⛔ **parked** (charter §1) |
| **GUI / AI / Mail / DNS** | WPF/MAUI, `Extensions.AI`, `Net.Mail`, DNS | — | ⛔ out-of-scope / low value |

## Where wireform already exceeds .NET (⭐)

- **Serialization breadth** — ~30 formats vs .NET's JSON/XML/binary, all on one
  deriver + codegen spine.
- **Columnar + lakehouse** — Parquet/Arrow/ORC readers+writers and the full
  Iceberg/Delta/Hudi/Lance table-format stack. .NET has no native equivalent.
- **Native Kafka client** — pure-Haskell wire-protocol implementation (.NET wraps
  the native `librdkafka`).
- **CEL + protovalidate** — a conformant CEL engine and protobuf validation.

## In-scope gaps, prioritized

The rows wireform both *lacks* and that *extend its mission*. Ranked by how
directly they compose what already exists.

### Tier 1 — high alignment (compose existing strengths)

1. **Configuration + Options.** The standout. .NET's `Extensions.Configuration`
   layers JSON/env/cmdline/secrets into typed `IOptions`. wireform already
   decodes TOML/YAML/JSON/proto — a layered, typed config system that **binds
   through the existing deriver and format decoders** turns wireform's format
   breadth into a config capability no other Haskell stack has. Pairs with
   `fractal-layer` (R4) for hosting/lifecycle.
2. **Query layer (LINQ-equivalent).** Typed query combinators lowering to
   `Columnar.Stream` + predicate pushdown — makes the lakehouse stack
   *queryable*, not just readable. `IQueryable`→pushdown is the analog of EF's
   expression translation.
3. **DB wire-protocol clients** (Postgres / MySQL / Redis frontend↔backend
   protocols). "Wire protocol" *is* the mission — these belong beside
   `wireform-kafka`, not in an ORM. A `wireform-postgres` (Postgres v3 protocol)
   is a far better fit than an EF-Core clone and closes the biggest OLTP gap.
4. **Web framework layer.** Routing + minimal-API-style handlers over
   `wireform-http` (model binding via the deriver) + the R5 HTML-templating /
   Razor analog. Directly serves the stated Razor/Blazor interest.

### Tier 2 — medium alignment (the network/runtime stack needs them)

5. **Resilience pipeline** — generalize the http retry into
   retry/circuit-breaker/hedging/timeout/**rate-limiter** over
   `Wireform.Transport` (the Polly + `System.Threading.RateLimiting` analog).
6. **Structured-logging facade** — the one observability gap: OTel covers
   trace+metrics, but there is no `ILogger`-equivalent. Adopt the OTel logs API
   (or bless `co-log`/`katip`) so libraries log uniformly.
7. **Structured concurrency + a blessed channel** — `ki` for scopes/cancellation
   plus a sanctioned channel/queue type (the `System.Threading.Channels` analog)
   the network stack can standardize on.
8. **Temporal types + RFC-3339 / duration formatting** — the *useful subset* of
   globalization (full ICU collation/calendars stays ecosystem-owned). Proto WKT,
   CEL, and Iceberg all already need this; centralize it.
9. **Unified compression surface** — one `Wireform.Compress` over the
   gzip/zstd/lz4/brotli/snappy codecs wireform *already ships scattered*, plus an
   optional zip/tar archive reader/writer.
10. **General record validation** — lift `protovalidate`/`cel` from proto-only to
    "validate any derived record" (the `DataAnnotations` analog), riding the
    deriver.

### Composition pillar — high alignment, but sequences after #3 + #7

11. **Distributed / hybrid cache.** A typed cache abstraction whose **value codec
    is any wireform format** — the `IDistributedCache` / `HybridCache`
    "configurable serialization" knob is wireform's core competency, done typed
    via the deriver. L2 backend = wireform's own **Redis (RESP) / Memcached** wire
    client (#3); L1 leans on an existing Haskell concurrent map
    (`stm-containers` / `cache`) — wireform does **not** reinvent the in-memory
    LRU. Value-add: the abstraction + typed codec + distributed backends +
    **single-flight stampede protection** (the concurrency primitive, #7) +
    tag-based invalidation. Precedent: `wireform-http`'s RFC-9111 response cache.
    Internal consumer: a `(path, version)` metadata cache for Parquet footers /
    Iceberg manifests. Gated on #3 (Redis client) and #7 (concurrency).

## Explicitly out-of-scope (⛔ — do not build)

Ceded deliberately so the non-goals are auditable: general collections (Haskell
`containers`/`vector`/`stm`); a regex engine (domain pattern languages —
XPath/CSS/CEL — are covered); general filesystem/`Path`/watcher/pipes (`mmap`
loader excepted); general numerics / linear algebra / tensors / `Complex` /
matrices (`vector`/`massiv`/`hmatrix`); cryptography (parked, charter §1); GUI
(WPF/MAUI); mail/SMTP, a DNS resolver, and an LLM-client abstraction
(`Extensions.AI`). Note: caching is **not** in this set — only a generic
in-memory LRU map's *internals* are ecosystem-owned (`stm-containers` / `cache`);
the cache *abstraction* (typed codec + distributed backends + stampede policy) is
in-scope as composition pillar #11.

## Mapping to the charter roadmap (R7 update)

The original R7 listed four vague candidates (datetime, collections/LINQ, unified
IO, async/`ki`). This analysis sharpens them and adds the missing high-alignment
pillars. Proposed R7 set, in priority order: **Configuration/Options (#1)**,
**Query layer (#2)**, **DB wire-protocol clients (#3)**, **Resilience (#5)**,
**structured logging (#6)**, structured-concurrency/`ki` + channel (#7), temporal
types (#8), unified compression (#9), general validation (#10), and a
**distributed / hybrid cache (#11)** as a composition pillar gated on #3 + #7.
The web-framework pillar (#4) is the natural superset of the already-endorsed
**R5** (HTML templating). The old "unified IO/filesystem" item is narrowed to
*compression + config* — a filesystem clone is out-of-scope.
