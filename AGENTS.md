# wireform Development Guidelines

## Cursor Cloud specific instructions

### Toolchain on the VM

GHC and Cabal come from [ghcup](https://www.haskell.org/ghcup/) (`~/.ghcup/env`). After a fresh VM, if `ghc` is missing from `PATH`, run `source "$HOME/.ghcup/env"` (or open a login shell that sources `~/.bashrc`).

Ubuntu packages required before the first `cabal build all` (match `.github/workflows/ci.yml` plus OpenSSL for `wireform-network`):

```bash
sudo apt-get install -y build-essential libgmp-dev libffi-dev libncurses-dev \
  zlib1g-dev libnuma-dev xz-utils pkg-config protobuf-compiler \
  libsnappy-dev liblz4-dev libzstd-dev libbrotli-dev librdkafka-dev libssl-dev
```

HLint for local lint: `sudo apt-get install -y hlint` (distro build; CI uses a newer pin via `haskell-actions/setup`).

### Build and test (default workflow)

From repo root:

```bash
cabal update
cabal configure --enable-tests --enable-benchmarks
cabal build all -j2 --ghc-options="-j2"   # first build ~25–40 min on 2-core VMs
cabal test wireform-test --test-show-details=streaming
```

Subsequent builds reuse `~/.cabal/store` and are much faster.

**Hello-world executables** (no external services):

- `cabal run example-derive` — one ADT encoded to proto, CBOR, MsgPack, JSON
- `cabal run example-msgpack` — schema-less roundtrip
- `cabal run payments-pipeline -- demo` — Kafka Streams topology in `TopologyTestDriver` (no broker)

### Optional services

| Service | Start | Notes |
|---------|--------|--------|
| Kafka (integration tests) | `docker compose -f wireform-kafka/test-integration/docker-compose.yml up -d` | Then `WIREFORM_KAFKA_BROKER=localhost:9092` for relevant `cabal test` targets |
| Docs site | `cd website && npm install && npm run dev` | <http://localhost:4321/wireform-/> |
| Nix dev shell | `nix develop` or `nix develop .#ghc98` | Alternative to ghcup; provides fourmolu, prek, native libs |

### Gotchas

- **`loadProto` splice sites need `DataKinds`:** the IDL bridge emits `Proto.Schema.HasField` instances whose field-name argument is a type-level string literal, so any module containing a `$(loadProto …)` splice must enable `{-# LANGUAGE DataKinds #-}`. Forgetting it surfaces as `Illegal type: "<field>" Perhaps you intended to use DataKinds` at the splice line. (`exe:wireform-conformance-runner`, `test:wireform-proto-derive-test`, and `bench:loadproto-bench` were previously missing this and now build.) Generated enums also carry a synthetic open-enum constructor named `<Enum>''Unrecognized !Int32` (two apostrophes — see `unknownConNameFor`); reference that, not `<Enum>'Unknown`.
- **First build is slow** — use `-j2` on small cloud VMs; see [Toolchain](#toolchain).
- **Heavy optional flags** (`+python-interop`, `+dataframe-bridge`, etc.) are off by default; see the Cabal flags table in [Cabal flags worth knowing](#cabal-flags-worth-knowing).

## Code Generation Principles

**All message types must come from the code generator.** This includes well-known
types (`Timestamp`, `Duration`, `Struct`, etc.), descriptor types, and benchmark
types. Hand-written wire encode/decode instances are not permitted because they
drift from what the code generator produces and mask codegen bugs.

- Well-known types live in `src/Proto/Google/Protobuf/*.hs` and are generated
  from the `.proto` files in `proto/google/protobuf/`.
- Supplementary logic (e.g. `packAny`, RFC 3339 formatting, `TypeRegistry`)
  belongs in companion modules like `Proto.Google.Protobuf.Any.Util` or
  `Proto.JSON.WellKnown`. These import the generated types but never define
  wire-level instances.
- Benchmark comparison types must also be code-generated so that benchmarks
  measure the *actual* codegen output, not idealised hand-written decoders.

### Never hand-edit a generated file

Generated files are **output**, not source. Editing them creates silent drift
between what the codegen produces and what the repo claims it produces; the
next regen pass clobbers the edit and the change disappears. The pattern that
broke this rule before:

- a generated module needed a tweak (an extra import, a missing instance,
    a fixed comment),
- the tweak was applied directly to `<Format>/Generated/Foo.hs`,
- the codegen kept generating the old shape,
- a later regen wiped the tweak and reintroduced the original bug.

**Always make the change in the codegen** (`<Format>.CodeGen.*` /
`<Format>/codegen/`) and **regenerate**. The regen output is what gets committed.

#### Audit before committing

Before committing changes that touch any `*/Generated/*.hs` file, run a
regen + diff to make sure the source tree exactly matches what the codegen
produces. For Kafka:

```
./scripts/regen-kafka-protocol.sh /path/to/kafka/clients/src/main/resources/common/message
git diff --stat wireform-kafka/src/Kafka/Protocol/Generated/
# expect zero non-codegen diff (only what your codegen change introduced)
```

If `git diff` shows changes you did not intend, you have a hand-edit somewhere
in the source tree (or a stale Generator output). Revert the hand-edit, fold
the intent into the codegen instead, and re-regen.

#### Per-package README AUTOGEN regions

The same rule applies to the per-package `wireform-X/README.md` files:
anything between paired `<!-- BEGIN_AUTOGEN <key> -->` and
`<!-- END_AUTOGEN <key> -->` markers is owned by `wireform-stats`'s
`regen-stats` tool and rewritten on every run. Edit the surrounding
prose freely; never edit between markers. The regen-stats CI job
(`.github/workflows/regen-stats.yml`) fails the build if anything in
those regions has drifted from what the tool would produce.

Defined keys: `tests`, `coverage`, `coverage:table`,
`bench:<id>`. See [`wireform-stats/README.md`](wireform-stats/README.md)
for the schema and the regen workflow.

`regen-stats render` (and `check`) rewrites the READMEs **and** the
docs-site package pages in one pass — a `wireform-<name>` package maps
to the `<name>.md` docs page, and its `bench:<id>` regions are filled
from the same `wireform-<name>/bench-results/summary/<id>.json` files.
The only divergence is the chart: READMEs use a `<picture>` pointing
at two committed SVG files, while the docs pages inline a single
color-scheme-adaptive SVG (`renderBarChartAdaptive`) because a
plain-markdown Starlight page can't reference `public/` assets with a
base-aware URL. Never hand-edit the benchmark tables/charts in either
place — change the summary JSON (or the renderer) and re-run
`regen-stats render`. Docs pages whose performance numbers don't come
from a committed summary (e.g. `html.md`'s custom-harness throughput)
have no markers and are hand-maintained as usual.

#### Refreshing / adding benchmarks (the summary JSONs are generated)

The numbers inside `wireform-<pkg>/bench-results/summary/<id>.json` are
**measured output, not source** — treat them like any other generated
file (see "Never hand-edit a generated file"). They come from real
criterion runs, distilled by a reproducible, manifest-driven pipeline:

```
scripts/bench-manifest.json   # declares: bench target -> summary(s) + per-cell map
      │  (consumed by)
scripts/run-benchmarks.py     # build + run each target SEQUENTIALLY, then distill
      │  (calls)
scripts/distill-bench.py      # criterion --json -> refresh summary values + capturedAt
      │  (then)
regen-stats render            # summaries -> charts + READMEs + docs
```

**Always run benchmarks sequentially** — never via the parallel
subagent/`-j` paths. criterion is noise-sensitive; concurrent runs
poison each other's numbers. `run-benchmarks.py` enforces this.

To **refresh** existing numbers:

```bash
python3 scripts/run-benchmarks.py --render             # everything (slow)
python3 scripts/run-benchmarks.py --only yaml --render  # one target
```

To **add a new benchmark** (or repoint/rename an existing one):

1. Add the `BenchSummary` skeleton at
   `wireform-<pkg>/bench-results/summary/<id>.json` (id / title / unit /
   `higherIsBetter` / groups / series names / baseline / toolchain) and
   a `<!-- BEGIN_AUTOGEN bench:<id> -->…<!-- END_AUTOGEN bench:<id> -->`
   pair in the README and/or `website/src/content/docs/packages/<pkg>.md`.
2. Add an entry to `scripts/bench-manifest.json` mapping the cabal
   benchmark target to that summary. Discover the criterion
   `reportName`s with a dry run: `python3 scripts/run-benchmarks.py
   --only <target> --dry-run` — any `unmatched cells` error prints the
   available report names. Add a `"<series>|<group>=<reportName>"` entry
   to the manifest `map` for every cell that doesn't auto-match
   `<series>/<group>`.
3. Re-run `python3 scripts/run-benchmarks.py --only <target> --render`
   and commit the refreshed summary, charts, README, and docs together.

The manifest is the single source of truth for "which bench feeds which
summary"; never re-derive that mapping ad-hoc in a shell. The CI
`collect-bench` job (`.github/workflows/regen-stats.yml`, opt-in via the
`run_benchmarks` workflow_dispatch input) runs exactly this driver and
commits the result.

#### Per-format codegen entry points

| Format        | Codegen entry                                              | Regen helper                                  | Generated dir                                       |
| ------------- | ---------------------------------------------------------- | --------------------------------------------- | --------------------------------------------------- |
| `wireform-proto` | `gen-wkt` executable, sources in `wireform-proto/wkt-codegen/` | (manual: `cabal run gen-wkt`)                | `wireform-proto/src/Proto/Google/Protobuf/`         |
| `wireform-kafka` | `wireform-kafka:exe:kafka-codegen`, sources in `wireform-kafka/codegen/Kafka/Protocol/Codegen/` | `scripts/regen-kafka-protocol.sh`             | `wireform-kafka/src/Kafka/Protocol/Generated/`      |
| `wireform-kafka-protocol` | (same `kafka-codegen` output tree) | `scripts/regen-kafka-protocol.sh` | `wireform-kafka/src/Kafka/Protocol/Generated/` (sources); `wireform-kafka-protocol/wireform-kafka-protocol.cabal` lists exposed modules |

#### Kafka-specific notes

The `kafka-codegen` exe **deletes every existing `.hs` file in the output
directory** before writing fresh output (`cleanGeneratedFiles` in
`codegen-exe/Main.hs`). Consequences:

- Any module in `wireform-kafka/src/Kafka/Protocol/Generated/` whose schema
    is **not** in the supplied message-dir will be deleted by a regen.
- If `wireform-kafka.cabal` lists modules that aren't in the schema dir
    (e.g. `KIP-932` share-group messages, `StreamsGroup*` from a newer Kafka
    than what you regenerated against), the build will break after a regen
    until the cabal file is updated to match.

When importing a newer Kafka schema set, also reconcile the cabal
`exposed-modules` list in the same change: every regen-produced `.hs`
should appear there, and every entry there should map to a regen-produced
`.hs`.

Wire types live in the separate package `wireform-kafka-protocol`
(same `wireform-kafka/src` tree via `hs-source-dirs`). `wireform-kafka`
depends on it but does **not** use `reexported-modules` (keeps Haddock
clean). Client code in `wireform-kafka` imports protocol modules with
`PackageImports` (`import qualified "wireform-kafka-protocol" …`) so GHC
does not compile duplicate copies of `Kafka.Protocol.Generated.*`.
After a regen, run `scripts/split-kafka-protocol-package.py` if module
membership changed, or update `wireform-kafka-protocol.cabal` manually.

## Performance

### Performance bar (completion criterion)

A format implementation is **not complete** until its hot paths (encode,
decode, the codegen'd record path) match or beat equivalent compiled-language
implementations — Rust serde / Go / C++ / the format's own reference library.
Acceptable outcomes, in order:

1. **Equal or faster** than the reference. This is the target.
2. **Within 2× of the reference** — acceptable *only* when the gap is intrinsic
   (a GHC runtime tax with no zero-cost workaround, or a semantic the reference
   skips). Justify it in the PR: point at the GHC core / profile that explains
   the bottleneck and list what you tried. A silent 2× gap is a failure.
3. **Slower than 2×** — not acceptable. Rework until (1), or until you can
   justify (2) with evidence.

Measure with the package's `bench/` criterion target (every per-format package
has one; run it via `scripts/run-benchmarks.py --only <target>`). REPL
micro-benchmarks and "it feels fast" don't count — the committed criterion
harness does. Compare bytes-allocated (`+RTS -T` / `-s`) as well as wall time;
allocation drives GC, which drives throughput on real inputs.

### Inspecting GHC core

Low allocation is a requirement, not an aspiration — and you cannot eyeball it
from the source. Read GHC's core output frequently while implementing or
touching any hot path:

```bash
# dump core for one module to a .dump-simpl file beside the source
cabal exec -- ghc -fforce-recomp -O2 -ddump-simpl -ddump-to-file \
  -dsuppress-all wireform-cbor/src/CBOR/Decode.hs
# or attach the flags to a normal build of a package
cabal build wireform-cbor --ghc-options="-ddump-simpl -ddump-to-file -dsuppress-all"
```

What to look for on the steady-state path:

- **No heap allocation.** Each boxed `Int` / `Maybe` / `Either`, each `$f...`
  dictionary, and each closure handed to a higher-order combinator is a heap
  object. A tight loop should be `Int#` / `Word#` / unboxed tuples
  `(# ..., Int# #)` / unlifted `$w` workers — not boxed types.
- **Non-allocating workers** (`$w...` definitions taking unboxed args). A `let`
  binding a thunk, or a call to an allocating wrapper, inside the loop body
  means it allocates per iteration.
- **SpecConstr / specialisation fired** — `$s...` names mean GHC specialised
  away a dictionary or a known call shape.
- **Inlining actually fired** — a residual `... @$Type ... ($fDict ...)`
  application means you're paying dynamic dispatch instead of a statically
  resolved call.

If the core shows allocation on a hot path, fix it *before* claiming the work
done — add `{-# INLINE #-}` / `INLINE[~N]` pragmas, adopt the unboxed-sum /
`Int#` shapes in [Allocation discipline](#allocation-discipline), or restructure
so GHC can specialise. The dump is the ground truth; allocation-driven
regressions are invisible in a source review.

### Allocation discipline

- **Unboxed sums** for finite branching (success / failure / end-of-input).
  Never use boxed `Either` or `Maybe` on an internal hot path.
- **`withTag` CPS** for the decode loop tag dispatch, where continuations are
  statically known lambdas that GHC will inline.
- **Unboxed `Int#`** for offsets threaded through the decoder.
- Avoid `IORef` in benchmarks where an unboxed accumulator loop suffices.

### String / Text handling

- Never round-trip through `String`. No `T.pack (show n)`, no
  `reads (T.unpack t)`, no `T.pack . show`. Use `Data.Text.Builder` or
  direct numeric-to-Text conversion instead.
- For integer formatting, write directly to a `Builder` or use a purpose-built
  `intToText` helper.
- For parsing integers from `Text`, use `Data.Text.Read.decimal` /
  `Data.Text.Read.signed` rather than `reads . T.unpack`.

### Numeric patterns

- When you need both quotient and remainder, use `divMod` or `quotRem` in a
  single call rather than separate `div` and `mod` on the same operands.
- Prefer `quot`/`rem` over `div`/`mod` for non-negative values (avoids the
  sign-correction branch).

### Data structures

- **No plain tuples** in domain-specific return types. Define a small strict
  record with `{-# UNPACK #-}` on numeric fields. Tuples hide meaning and
  prevent GHC from unboxing nested fields.
- **GrowList is a last resort.** Each `snoc` allocates a cons cell + a
  `GrowList` node (≈48 bytes on 64-bit). Prefer:
  1. `VecBuilder` (IO-based doubling array) when inside IO/ST.
  2. `Data.Vector.create` + `MV.grow` in an ST block when the final size
     is unknown but the builder can be scoped.
  3. If stuck in a pure context (the Decoder monad), a chunked representation
     with amortised allocation (e.g. small arrays of 64 elements, chained)
     is better than a cons-per-element list.

### Decoder monad style

- The `Decoder` newtype wraps `ByteString -> Int# -> (# (# a, Int# #) | DecodeError #)`.
  All primitives (`getVarint`, `getText`, etc.) return unboxed sums.
- In hand-optimised decoders, use `withTag` + direct `runDecoder#` calls for
  each field. In generated code, the monadic `do` notation with `getTagOrU`
  is acceptable (slightly less optimal but far simpler to generate).
- Always `{-# INLINE messageDecoder #-}` on instances.

## Code style

- Do not use list comprehensions. Prefer `do` block syntax or
  higher-order functions.
- Prefer datatype-specific functions with better complexity over
  `toList` / `fromList` conversions.
- No `threadDelay` in tests.
- Keep lens usage to where the alternative would be unwieldy; comment
  complex lens expressions.
- Property-based tests via Hedgehog. Do not test things inherent to
  the language (e.g. setting a record field and reading it back).

## Toolchain

CI builds against GHC `9.6.4` and `9.8.4` (see
`.github/workflows/ci.yml`). When working from a fresh VM that
doesn't already have the Haskell toolchain installed:

- **Apt packages** (Ubuntu 24.04, root):

  ```
  apt-get install -y build-essential libgmp-dev libffi-dev libffi8 \
    libncurses-dev libtinfo6 zlib1g-dev libnuma-dev xz-utils \
    protobuf-compiler
  ```

- **Haskell toolchain via ghcup**:

  ```
  curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 sh
  source /root/.ghcup/env
  ghcup install ghc 9.6.4 --set
  ghcup install cabal 3.10.3.0 --set
  cabal update
  ```

- **Cross-language interop tests** (optional): `pip3 install pyarrow`
  for the pyarrow round-trip suites in `wireform-parquet/test/Main.hs`,
  and `pip3 install protobuf` for `python-interop`.
- **First build is slow.** `cabal build all` rebuilds the whole
  workspace (~12 min on a 2-core VM) plus the Hackage dep
  closure; subsequent builds reuse `~/.cabal/store`. Use
  `cabal build all -j2 --ghc-options="-j2"` on small VMs.

If you find yourself running this often, propose an env-setup
agent at <https://cursor.com/onboard> so the cloud-agent base
image bakes the toolchain in.

### Cabal flags worth knowing

The repo opts into a few heavyweight optional dep trees behind
flags so the default `cabal build all` stays lean:

| Flag                  | Pulls in                              | Used by                                |
| --------------------- | ------------------------------------- | -------------------------------------- |
| `+python-interop`     | `process` + a python3 runtime         | `python-interop` test-suite            |
| `+dataframe-bridge`   | `dataframe` (and its cassava/regex/zstd/zlib/granite/vector-algorithms tree) | `example-dataframe-bridge` exe         |
| `+snappy`             | `snappy-c`                            | Avro container files                   |
| `+zstd`               | `libzstd`                             | Parquet ZSTD column chunks, Arrow      |
| `+lz4`                | `liblz4`                              | Parquet LZ4_RAW column chunks, Arrow   |
| `+rest-client`        | `http-client` etc.                    | `Iceberg.Catalog.REST.Client`          |
| `+brotli`             | `libbrotli`                           | Parquet Brotli codec                   |
| `+profile`            | (none)                                | `profile-rewriter` cost-centre build   |

When adding a new optional dependency that has a heavy or
flaky-to-install transitive closure, add it behind a Cabal flag
the same way (default `False`, `manual: True`).

## Module layout

The repo is a monorepo: one umbrella package `wireform` plus 27
per-format / shared-infrastructure packages listed in `cabal.project`.
Each format owns a top-level Haskell namespace (`<Format>.*`) and
ships a bare `<Format>` entry module re-exporting its codec surface
(see the entry-module convention in `docs/PLATFORM.md` §4). The
umbrella `wireform` package ships **no library** — it provides only
the `wireform-gen` CLI plus conformance / profiling / example
executables; there is no `Wireform.<Format>` facade. Cross-package
conventions live under the `Wireform.*` namespace (in `wireform-core`,
`wireform-derive`, and `wireform-columnar-core`).

The cross-cutting platform contract — the canonical-seam table, the
namespace rule, the observability (`hs-opentelemetry-api`) and DI
(`fractal-layer`) mandates, and the sequenced roadmap — lives in
[`docs/PLATFORM.md`](PLATFORM.md) (the Platform Charter).

If you are adding or moving modules, update this section *and* the
relevant `*.cabal` `exposed-modules` list in the same change.

### Umbrella

| Package          | Top-level namespace              | Notes |
| ---------------- | -------------------------------- | ----- |
| `wireform`       | _(no library)_                  | Ships **no library** — only the `wireform-gen` multi-format codegen CLI plus the conformance / profiling / example executables. There is no `Wireform.<Format>` facade; each format package is depended on directly and exposes its own bare `<Format>` entry module (§4 of `docs/PLATFORM.md`). The cross-format columnar entry point is `Wireform.Columnar` in the separate `wireform-columnar` package. |

### Shared infrastructure (`Wireform.*` namespace)

| Package             | Exposed modules                                                                                                                                                                                                          | Purpose |
| ------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | ------- |
| `wireform-core`     | `Wireform.FFI`, `Wireform.Encode.Direct`, `Wireform.Hash`                                                                                                                                                                | Shared C-FFI primitives (`fast_decode.c`, `fast_scan.c`, `wireform_hash_simd.c`), the direct-write encode buffer, and the SIMD hashing surface. No format code. |
| `wireform-derive`   | `Wireform.Derive`, `Wireform.Derive.Backend`, `Wireform.Derive.NameStyle`, `Wireform.Derive.Modifier`, `Wireform.Derive.TypeInfo`, `Wireform.Derive.ModifierInfo`, `Wireform.Derive.Extension`, `Wireform.Derive.Aeson`  | Annotation-driven TH deriver core. `Modifier` / `ModifierInfo` are the cross-backend annotation vocabulary; `Backend` / `BackendModifier` (in `Extension`) are how a format opts in. `NameStyle` and `TypeInfo` are the rename + reification helpers used by every per-format `<Format>.Derive`. `Wireform.Derive.Aeson` is the canonical worked example deriver — it lives here (rather than a separate `wireform-derive-aeson` package) so the deriver core has a self-contained reference user. |
| `wireform-columnar`       | `Wireform.Columnar`                                                                                                                                                                                                                                                                                                                                                                                                                                                                     | The cross-format columnar facade: a single Arrow-shaped encode/decode surface over Arrow IPC, Parquet, and ORC (projection, predicate pushdown, multi-file dataset iteration, record-level helpers). Depends on `wireform-columnar-core`, `wireform-arrow`, `wireform-parquet`, `wireform-orc`. |
| `wireform-columnar-core`  | `Columnar.IO`, `Columnar.LZ4`, `Columnar.Predicate`, `Columnar.SIMD`, `Columnar.Stream`                                                                                                                                                                                                                                                                                                                                                                                                 | Format-agnostic columnar primitives shared by every columnar package: `Columnar.IO` is the mmap-aware file loader (`loadFile` defaults to mmap above 64 KiB, eager below); `Columnar.Predicate` is the `PValue`/`PColPredicate`/`Predicate` vocabulary all per-format pushdown evaluators feed into; `Columnar.Stream` is the pull-based `Iter` / `IterIO` plus combinators (`iterChunk`, `iterScan`, `iterMergeBy`, `iterIOPrefetch`, `iterParallelMap`); `Columnar.SIMD` is the SIMD-accelerated bit-unpacking / RLE kernel shared with the C side via `cbits/columnar_simd.c` + vendored `simde`. |
| `wireform-dst`            | `Sim`, `Sim.Types`, `Sim.Entropy`, `Sim.Prog`, `Sim.World`, `Sim.Fault`, `Sim.Interp`, `Sim.Coverage`, `Sim.Assert`, `Sim.Search`, `Sim.Localize`, `Sim.Workload` | Deterministic simulation-testing engine (Antithesis-style): a purely-functional distributed-system model (`Sim.World.World`) stepped by a deterministic scheduler (`Sim.Interp.step`), so snapshot/branch is a pointer copy and every failure replays byte-for-byte from a `RunInput` (`seed` + buggify overrides + decision path). `Sim.Search` is FIND (best-first guided search with novelty + a UCB1 fault bandit + a nemesis-driven look-ahead rollout); `Sim.Localize` is NAIL (content-aware ddmin minimization robust to `FireFault` index renumbering, invariant bisection, ochiai suspiciousness). The cluster-scale complement to the narrow `dejafu` systematic-concurrency tests in `wireform-http2` — it does **not** use `dejafu`/`io-sim` because O(1) snapshot/branch requires a bespoke pure-state interpreter. Bare `Sim.*` namespace (like `Columnar.*`), **not** `Wireform.*`; the entry module `Sim` re-exports only the app-facing surface (interpreter internals `step`/`World`/`Resume` stay in `Sim.Interp`/`Sim.World`). Worked example: `example-register-partition` finds a lost-acknowledged-write-under-crash bug and minimizes it to a ≤8-decision reproducer. |
| `wireform-dst-net`        | `Sim.Net.Link`, `Sim.Net.Explore` | Bridge from the `wireform-dst` fault vocabulary to the `wireform-network` transport seam: `newSimLink` builds a controllable, fault-injecting in-memory link exposing both a `DuplexTransport` pair (for Kafka / HTTP/1 / WebSocket) and raw `SendFn`/`readN` ends (for the HTTP/2 engine `Config`, hence gRPC / Connect). Because every networking layer funnels bytes through `ReceiveFn`/`SendFn`, the whole real IO stack runs over it in "simulator mode" with runtime-injectable faults (`partition`/`heal` stall, `cut` = peer death → EOF, `setDrop`, `setLatency` over `LatencyDist`, `corruptNext`), fault decisions drawn from a seeded splittable PRNG. Deliberately a **separate opt-in package** depending on `wireform-dst` + `wireform-network` so the production socket/TLS path gains no dependency and no code — zero normal-mode perf cost by construction. `Sim.Net.Explore` is the protocol-agnostic **exploration harness**: a seeded nemesis applies a timed fault schedule concurrently with any `SimLink -> IO a` workload and an oracle classifies each `Outcome` (silently-wrong result / engine crash / hang / clean error), folded across seeds by `runCampaign`. Eight test-suites: `wireform-dst-net-test` (raw-seam fault units), `http2-fault-test` (hand-designed cases against the **real vendored HTTP/2 engine** via `withConnectionOnTransport`/`runServerOnTransport` over a `Transport` built from a `LinkEnd`), `http2-explore` (randomized CI campaign — latency/cut/partition-heal checked for full safety+liveness, byte corruption/drop crash-only since h2c has no DATA integrity), `grpc-explore` (the same campaign against the **real grapesy-fork gRPC client+server** over the engine-`Config` seam, a codegen-free `RawRpc` echo), `connect-explore` (against the **real `wireform-connect` client+server** over the link's **h2c** transport, a proto echo service), `kafka-explore` (against the **real `wireform-kafka` client** — its `negotiateVersions` ApiVersions handshake — over the link's **`leDuplex`**, against a minimal raw-fn mock broker replying with a codec-encoded `ApiVersionsResponse`), `http1-explore` (against the **real `wireform-http1` client+server** over `leDuplex` — client `newConnectionFromDuplex`+`sendRequestOn`, server `runServerOnConnection`), and `websocket-explore` (against the **real `wireform-websocket` frame layer** over `leDuplex` via `newConnection`, exercising RFC 6455 masking/fragmentation/UTF-8; the HTTP/1 Upgrade handshake is covered by `http1-explore`). Two real bugs found + fixed. **(1) `wireform-http2` liveness bug**: under heavy `ServerToClient` loss then reset, the server's HEADERS frame was dropped while its DATA frame arrived, and the client's `FrameData` handler closed the stream without ever filling `siHeaders` — so `sendRequest` hung forever on the response-headers MVar (the connection-close `failOutstanding` sweep found the stream already removed). Fixed in `Network.HTTP2.Client` by rejecting DATA-before-response-HEADERS as an RFC 9113 §8.1 protocol error (plus a `chRecvDone` register-vs-teardown guard); `http2-explore`'s drop campaign now asserts liveness. **(2) `wireform-network` magic-ring double-`munmap`**: `closeDuplexTransport` was not actually idempotent despite its haddock — `destroyMagicRing` `munmap`s its base pointer unconditionally, so a re-entrant / concurrent second close would `munmap` an address a fresh ring had since been mapped at → heap corruption / SIGSEGV. The concurrent HTTP/1 + WebSocket fault campaigns (each trial opens+closes a fresh magic-ring connection pair, latency widening the teardown window) crashed hard until this was fixed by making ring teardown fire at most once, atomically (`mkDestroyOnce` in `Wireform.Network.Transport.Duplex`, guarding both `newDuplexBufTransport` and the pooled variant). This corrects the earlier mis-attribution: the fault was **not** the SimLink `leDuplex` (proven memory-safe in isolation) nor an HTTP/1 wire-parser bug, but the shared magic-ring duplex teardown — so the fix also hardens every production consumer of the fn-backed duplex. Boundary: this fault-injects the real IO stack; full byte-for-byte scheduling determinism would need the stack rewritten over an abstract monad (would erode the hot path) and is out of scope. Friends status: **HTTP/2, gRPC, Connect, Kafka, HTTP/1, and WebSocket are all wired.** HTTP/2 via the raw-fn seam; gRPC via a new engine-`Config` seam (`Network.HTTP2.Engine.Client.allocConfigForTransport` + the exactly-N `Sim.Net.Link.leReadExactN`, exposed as `Network.GRPC.Client.withConnectionVia` / `Network.GRPC.Server.Run.runServerOverConfig`); Connect over **h2c** prior-knowledge (`Network.HTTP.Server.runServerOnTransport` + `Network.HTTP.Connection.withConnectionOnTransport`, wrapped as `runConnectServerOnTransport` / `withConnectClientOnTransport`); Kafka + HTTP/1 + WebSocket over `leDuplex` (Kafka's `Connection` built in-test from the exposed `Kafka.Network.Connection.Internal.Connection(..)` ctor + a dummy socket; HTTP/1 + WebSocket via their public `newConnectionFromDuplex` / `newConnection` — all zero-production-change beyond the `wireform-network` teardown fix). |

### Per-format packages — Haskell `<Format>.*` namespace

Each per-format package conventionally exposes the same module
shape (the bare `<Format>` umbrella entry module is now mandatory for
every serde-format package — see the entry-module convention in
[`docs/PLATFORM.md`](docs/PLATFORM.md) §4; the per-format tables below
list each package's granular modules, all reachable via that single
`import <Format>`):

```
<Format>                         -- bare umbrella entry module (re-exports the codec surface)
<Format>.Class                   -- typeclass(es) for value-level codecs
<Format>.Encode / <Format>.Decode -- low-level encode / decode primitives
<Format>.Value                   -- dynamic / untyped value ADT
<Format>.Derive                  -- annotation-driven TH deriver (consumes Wireform.Derive)
<Format>.Schema | .Parser | .CodeGen | .QQ | .Registry  -- IDL surface where the format has one
<Format>.JSON                    -- self-describing-format ↔ JSON bridge
```

Derivers (`<Format>.Derive`) are structural twins: they import
`Wireform.Derive`, reify the type, walk the `ModifierInfo`, and
splice instance declarations for `<Format>.Class`. To add a new
format, the path of least resistance is "clone the nearest
existing `<Format>.Derive` and adapt the value-mapping calls".

| Package               | Exposed modules                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  | Notes |
| --------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | ----- |
| `wireform-cbor`       | `CBOR.Class`, `CBOR.Encode`, `CBOR.Decode`, `CBOR.Derive`, `CBOR.Value`, `CBOR.Diagnostic`, `CBOR.JSON`, `CBOR.QQ`, `CBOR.Stream`, `CBOR.TagRegistry`, `CBOR.CDDL`, `CBOR.CDDLSchema`, `CBOR.CDDLCodeGen`                                                                                                                                                                                                                                                                                                                                       | RFC 8949 CBOR + CDDL schema & codegen. |
| `wireform-msgpack`    | `MsgPack.Class`, `MsgPack.Encode`, `MsgPack.Decode`, `MsgPack.Derive`, `MsgPack.Value`, `MsgPack.JSON`, `MsgPack.RPC`, `MsgPack.Stream`                                                                                                                                                                                                                                                                                                                                                                                                          | MessagePack + msgpack-RPC. |
| `wireform-thrift`     | `Thrift.Class`, `Thrift.Encode`, `Thrift.Decode`, `Thrift.Derive`, `Thrift.Value`, `Thrift.Wire`, `Thrift.Schema`, `Thrift.Parser`, `Thrift.CodeGen`, `Thrift.Message`, `Thrift.Transport`, `Thrift.Registry`, `Thrift.JSON`, `Thrift.QQ`                                                                                                                                                                                                                                                                                                       | Apache Thrift binary / compact + IDL. |
| `wireform-avro`       | `Avro.Class`, `Avro.Encode`, `Avro.Decode`, `Avro.Derive`, `Avro.Value`, `Avro.Wire`, `Avro.Schema`, `Avro.Schema.Parse`, `Avro.IDL`, `Avro.IDLConvert`, `Avro.Container`, `Avro.Resolution`, `Avro.Fingerprint`, `Avro.Protocol`, `Avro.JSON`, `Avro.QQ`, `Avro.Registry`, `Avro.CodeGen`                                                                                                                                                                                                                                                       | Apache Avro + IDL + container files. |
| `wireform-bson`       | `BSON.Class`, `BSON.Encode`, `BSON.Decode`, `BSON.Derive`, `BSON.Value`                                                                                                                                                                                                                                                                                                                                                                                                                                                                          | BSON (MongoDB). |
| `wireform-ion`        | `Ion.Class`, `Ion.Encode`, `Ion.Decode`, `Ion.Derive`, `Ion.Value`, `Ion.SchemaLang`, `Ion.ISLSchema`, `Ion.ISLCodeGen`, `Ion.QQ`                                                                                                                                                                                                                                                                                                                                                                                                                | Amazon Ion + Ion Schema Language (ISL). |
| `wireform-edn`        | `EDN.Class`, `EDN.Encode`, `EDN.Decode`, `EDN.Derive`, `EDN.Value`, `EDN.JSON`                                                                                                                                                                                                                                                                                                                                                                                                                                                                  | Extensible Data Notation. |
| `wireform-toml`       | `TOML.Class`, `TOML.Encode`, `TOML.Decode`, `TOML.Derive`, `TOML.Value`                                                                                                                                                                                                                                                                                                                                                                                                                                                                          | TOML 1.0 / 1.1. Conformance against the upstream [toml-test](https://github.com/toml-lang/toml-test) suite is opt-in via `TOML_TEST_SUITE=/path/to/clone` at test time (the test binary picks up either the repo root or its `tests/` subdirectory). |
| `wireform-yaml`       | `YAML.Class`, `YAML.Encode`, `YAML.Decode`, `YAML.Derive`, `YAML.Encoding`, `YAML.JSON`, `YAML.Value`                                                                                                                                                                                                                                                                                                                                                                                                                                            | YAML 1.2 (block + flow styles, anchors / aliases, tags, block literal / folded scalars, multi-document streams). Conformance against the upstream [yaml-test-suite](https://github.com/yaml/yaml-test-suite) is opt-in via `YAML_TEST_SUITE=/path/to/clone` at test time. |
| `wireform-bencode`    | `Bencode.Class`, `Bencode.Encode`, `Bencode.Decode`, `Bencode.Derive`, `Bencode.Value`                                                                                                                                                                                                                                                                                                                                                                                                                                                          | BitTorrent bencode. |
| `wireform-fory`       | `Fory.Class`, `Fory.Encode`, `Fory.Decode`, `Fory.Derive`, `Fory.Encoding`, `Fory.IO`, `Fory.MetaString`, `Fory.MetaString.Encoder`, `Fory.MetaString.Hash`, `Fory.Options`, `Fory.Struct`, `Fory.TypeId`, `Fory.Value`                                                                                                                                                                                                                                                                                                                                       | Apache Fory (formerly Fury) xlang serialization. Wire-compatible with `pyfory` 0.17 for: `null`, `bool`, `int*`/`varint*`, `uint*`/`varuint*`, `float32`/`float64`, `string` (with LATIN-1 / UTF-8 selection), `binary`, `LIST` / `SET` (chunked `collect_flag` format incl. `TRACKING_REF`), `MAP` (chunked key-type/value-type), `NAMED_STRUCT` (with `Fory.Struct.StructSchema` registered on both sides; produces byte-identical bytes incl. the 4-byte fingerprint hash and pyfory's canonical field reordering), one-dimensional primitive arrays (`BoolArray` … `Float64Array`, byte-length payloads matching pyfory's NumPy serializer), reference tracking (`Fory.Options.eoRefTracking`; structural sharing detected via `Hashable Value`), and meta-string compression (the five LowerSpecial / LowerUpperDigitSpecial / FirstToLowerSpecial / AllToLowerSpecial / UTF-8 encodings + MurmurHash3-x64-128 hashcodes for >16-byte strings). Verified by `wireform-fory-interop` (45 / 45 cases passing). The remaining ❌ is pyfory-compatible `NAMED_COMPATIBLE_STRUCT` (schema evolution): its bit-packed `TypeDef` field-info layout is deferred. The in-package self-describing `CompatibleStructVal` round-trips fine; only cross-language interop for it is unfinished. See `Fory.Encode`'s haddock for the exact wire shape. |
| `wireform-csv`        | `CSV.Class`, `CSV.Encode`, `CSV.Decode`, `CSV.Derive`, `CSV.Value`                                                                                                                                                                                                                                                                                                                                                                                                                                                                              | CSV / tab / pipe-separated. |
| `wireform-ndjson`     | `NDJSON.Encode`, `NDJSON.Decode`, `NDJSON.Derive`                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              | Newline-delimited JSON (line framing on top of aeson). |
| `wireform-asn1`       | `ASN1.Encode`, `ASN1.Decode`, `ASN1.Derive`, `ASN1.Value`, `ASN1.Schema`, `ASN1.Parser`, `ASN1.CodeGen`, `ASN1.QQ`                                                                                                                                                                                                                                                                                                                                                                                                                              | ASN.1 BER / DER. |
| `wireform-bond`       | `Bond.Encode`, `Bond.Decode`, `Bond.Derive`, `Bond.Value`, `Bond.Schema`, `Bond.Parser`, `Bond.CodeGen`, `Bond.Registry`, `Bond.QQ`                                                                                                                                                                                                                                                                                                                                                                                                              | Microsoft Bond. |
| `wireform-flatbuffers`| `FlatBuffers.Encode`, `FlatBuffers.Decode`, `FlatBuffers.Derive`, `FlatBuffers.Value`, `FlatBuffers.Schema`, `FlatBuffers.Parser`, `FlatBuffers.CodeGen`, `FlatBuffers.Registry`, `FlatBuffers.QQ`                                                                                                                                                                                                                                                                                                                                              | Google FlatBuffers + IDL. |
| `wireform-capnproto`  | `CapnProto.Encode`, `CapnProto.Decode`, `CapnProto.Derive`, `CapnProto.Value`, `CapnProto.Schema`, `CapnProto.Parser`, `CapnProto.CodeGen`, `CapnProto.Registry`, `CapnProto.QQ`                                                                                                                                                                                                                                                                                                                                                                | Cap'n Proto + IDL. |
| `wireform-columnar`   | `Columnar.IO`, `Columnar.Predicate`, `Columnar.SIMD`, `Columnar.Stream`                                                                                                                                                                                                                                                                                                                                                                                                                                                                          | Format-agnostic columnar primitives shared by Arrow / Parquet / ORC. `Columnar.Stream` exposes pull-based `Iter` / `IterIO` + combinators (`iterChunk`, `iterScan`, `iterMergeBy`, `iterIOPrefetch`, `iterParallelMap`); `Columnar.Predicate` is the shared pushdown vocabulary; `Columnar.IO` is the mmap-aware file loader (`loadFile` / `loadFileMmap` / `loadFileEager`). |
| `wireform-arrow`      | `Arrow.Types` (incl. `schemaFingerprint` / `schemaEquivalent`), `Arrow.Column` (incl. `validateMapKeysSorted`), `Arrow.Record` (incl. `structE`/`structEMaybe` + `structD`/`structDMaybe`, `columnDWithDefault`, `subsetTable`/`projectTable`, `NameStrategy`/`applyNameStrategy`), `Arrow.Record.Generic`, `Arrow.Record.TH`, `Arrow.Derive`, `Arrow.IPC`, `Arrow.FlatBufferIPC`, `Arrow.Stream`, `Arrow.File`, `Arrow.Write`                                                                                                                     | Apache Arrow IPC / records. Test suite lives in `test-derive/` (separate from the main `test/` so the deriver fixtures isolate easily). Uses the `+zstd` and `+lz4` flags. |
| `wireform-parquet`    | `Parquet.Types`, `Parquet.Footer`, `Parquet.Read` (path + handle helpers: `loadParquetFilePath` / `openParquetReader`), `Parquet.Write`, `Parquet.Aggregate` (count(*) / count(col) / min / max from stats), `Parquet.Page`, `Parquet.PageIndex`, `Parquet.Levels`, `Parquet.LevelsEncode`, `Parquet.Nested`, `Parquet.Compress`, `Parquet.Delta`, `Parquet.DeltaEncode`, `Parquet.ByteStreamSplit`, `Parquet.BloomFilter`, `Parquet.NullPagesBitmap`, `Parquet.Encryption`, `Parquet.Thrift.Schema`, `Parquet.Arrow`, `Parquet.HighLevel`, `Parquet.Derive` | Apache Parquet (reader / writer / Thrift schema bridge). Test suite in `test-derive/`. |
| `wireform-orc`        | `ORC.Types`, `ORC.Footer`, `ORC.Stripe`, `ORC.RowIndex`, `ORC.BloomFilter` (incl. `decodeBloomFilter` + `bfCheckBytes` / `bfCheckLong`), `ORC.Encryption`, `ORC.Read` (path + handle helpers, `decompressORCStreamSized`), `ORC.Write`, `ORC.Aggregate` (count + min/max + sum from stats), `ORC.Statistics` (predicate evaluator), `ORC`, `ORC.Arrow` (incl. `streamStripesFilteredIter` / `streamStripesProjectedFilteredIter`), `ORC.Proto.Schema`, `ORC.Derive`                                                                              | Apache ORC. Test suite in `test-derive/`. |
| `wireform-iceberg`    | `Iceberg.Types`, `Iceberg.Snapshot`, `Iceberg.Manifest`, `Iceberg.ManifestMerge`, `Iceberg.Partition`, `Iceberg.Sort`, `Iceberg.Transform`, `Iceberg.Expression`, `Iceberg.Update`, `Iceberg.Validate`, `Iceberg.Read`, `Iceberg.Write`, `Iceberg.Maintenance`, `Iceberg.MetricsConfig`, `Iceberg.SchemaCompat`, `Iceberg.SchemaEvolution`, `Iceberg.SingleValue`, `Iceberg.BoundTrunc`, `Iceberg.Murmur3`, `Iceberg.Geometry`, `Iceberg.Variant{,.Parquet,.Shredding}`, `Iceberg.Puffin`, `Iceberg.Delete`, `Iceberg.DeletionVector`, `Iceberg.View`, `Iceberg.Parquet`, `Iceberg.JSON`, `Iceberg.Catalog.{Glue,Hadoop,REST,REST.Client,Sql}`, `Iceberg.Derive` | Apache Iceberg table format + catalog clients. Behind `+rest-client` flag for the HTTP client. Test suite in `test-derive/`. **Iceberg-specific** table-format interop (manifests / manifest-list / table-metadata round-tripped through pyiceberg + fastavro) lives in `wireform-iceberg/probe/Probe.hs` + `wireform-iceberg/scripts/iceberg_interop.py`; the in-process catalog (Glue / Hadoop / REST / Sql) is exercised separately by `Test.Iceberg.Catalog*` HUnit tests. |
| `wireform-delta`      | `Delta.Log` (typed actions; `parseLogLine` / `parseLogFile`; `TableSnapshot` + `applyAction` / `snapshotFromActions`; `parseDeltaSchema`; `AddStats` decoder; `LastCheckpoint`); `Delta.Checkpoint` (path-aware decoder for `*.checkpoint.parquet` rows — `add` / `remove` / `metaData` / `protocol` reconstructed including `partitionValues` / `tags` / `partitionColumns` / `configuration` / `readerFeatures` / `writerFeatures` / `deletionVector`); `Delta.IO` (`openDeltaTable` w/ checkpoint short-circuit, `openDeltaTableAt` time-travel, `historyEntries` for the @DESCRIBE HISTORY@ surface, `activeFilePaths` / `dtActiveFiles` / `partitionedActiveFiles` flat snapshot helpers). | Delta Lake transaction log reader. Interop against `deltalake` (delta-rs) covers unpartitioned, partitioned, checkpointed (v11 + post-checkpoint APPEND/OVERWRITE), partitioned + checkpointed (cross-checks `partitionValues` map + `partitionColumns` list out of the checkpoint Parquet), and time-travel + history (cross-checks `historyEntries` against `DeltaTable.history()` and `openDeltaTableAt v=2` against `DeltaTable(.., version=2)`). Out of scope so far: deletion-vector application, column mapping, V2 multi-part checkpoint format. |
| `wireform-hudi`       | `Hudi.Timeline` (`parseInstantFileName`; sort/filter helpers; `HoodieCommitMetadata` + `HoodieWriteStat` JSON; `HoodieReplaceCommitMetadata` for replacecommit instants — supersedes prior file slices via `partitionToReplaceFileIds`; `HoodieCleanMetadata` + `HoodieCleanPartitionMetadata` for clean instants; `FileSlice` / `TableState` + `applyCommit` / `applyReplaceCommit` / `applyClean` / `tableStateFromCommits`); `Hudi.Avro` (Avro container decoder for the 1.x+ instant payload format); `Hudi.IO` (`openHudiTable`, `openHudiTableAt` time-travel, `activeFiles` / `activeBaseFilePaths` flat snapshot, `tableSchemaFromCommits`).         | Apache Hudi timeline reader (Copy-on-Write). Interop against `hudi-rs` covers JSON instants, Avro 1.x+ instants, and replacecommit instants (verifies `INSERT_OVERWRITE` correctly drops the replaced fileId). Out of scope so far: MoR log-block decoding, record-level merge keys, the metadata table. |
| `wireform-lance`      | `Lance.Format` (data-file envelope + 40-byte data-file footer + 16-byte manifest footer); `Lance.IO` (`openLanceFile`, `openLanceManifest`, `openLanceDataset`, `openLanceDatasetAt` time-travel, `findManifestVersions`, `decodeManifestFileName`/`encodeManifestFileName`); `Lance.Manifest` (typed protobuf decoder + `datasetActiveDataFiles` / `datasetActiveDataFilePaths` / `datasetSchemaFields` (`LanceSchemaField` flat schema readout) / `datasetWriterVersion` / `datasetTimestampMillis`); `Lance.Pb.Lance.{File,Table}` (auto-generated by `cabal run wireform-lance:gen-lance-pb` from `proto/lance/{file,table}.proto`).        | Apache Lance file + dataset reader. Interop against `pylance` covers `--file` (40-byte footer) and `--dataset` (versions + manifest body + active fragment list + schema readout + writer version + version timestamp, all cross-checked against `lance.dataset(...).versions()` / `.schema` / `.get_fragments()`). The protobuf `ColumnMetadata` decoder for individual data files still lives downstream; this module exposes the byte ranges that decoder would consume. |
| `wireform-xml`        | `XML.Class`, `XML.Encode`, `XML.Decode`, `XML.Derive`, `XML.Value`, `XML.Schema`, `XML.SAX`, `XML.DSL`, `XML.QQ`, `XML.FastDOM`, `XML.Generic`, `XML.Incremental`, `XML.Path`, `XML.XSLT`, `XML.CodeGen`                                                                                                                                                                                                                                                                                                                                        | XML 1.0 + SAX / DOM / XSLT / XPath. |
| `wireform-html`       | `HTML.Value`, `HTML.Parse`, `HTML.Encode`, `HTML.Class`, `HTML.Derive`, `HTML.TagId`, `HTML.DOM`, `HTML.Selector`, `HTML.Rewriter`                                                                                                                                                                                                                                                                                                                                                                                                              | HTML5 parser + DOM + CSS selectors + streaming rewriter. Has its own benchmarks (`bench/HTMLBench.hs`, `bench/ProfileRewriter.hs`). |
| `wireform-grpc`       | `Network.GRPC.{Client,Server,Common,*}` — see cabal for the full list                                                                                                                                                                                                                                                                                                                                                                                                                                                                            | **Maintained fork of [`grapesy`](https://github.com/well-typed/grapesy) by Edsko de Vries** (originally vendored; now deliberately diverged — no upstream-sync constraint). Modules under `Network.GRPC.Util.*` are intentionally `other-modules` (private). Does not match wireform's `<Format>.*` shape. Handler registration: the transport-agnostic `Service`/`method`/`service` vocabulary lives in `grpc-spec` (`Network.GRPC.Spec.Service`, re-exported from `Network.GRPC.Spec`); `Network.GRPC.Server.Service.fromService` adapts it to grapesy (`[SomeRpcHandler m]`). Order-insensitive + completeness-checked (missing/duplicate/foreign methods are compile errors naming the method; explicit `methodUnimplemented` for deliberate gaps). Upstream's order-sensitive `Methods`/`Services` GADTs, `fromMethods`/`fromServices`/`simpleMethods`, and `Network.GRPC.Server.Protobuf` are **removed**; `ServiceMethods` moved to `grpc-spec`. Per-RPC escape hatch: `Network.GRPC.Server.StreamType.fromMethod` + `someRpcHandler` (see `interop/Interop/Server.hs` for a mixed typed/raw list). |
| `wireform-connect`    | `Network.Connect{,.Server,.Client,.Protocol,.Error,.Envelope,.Metadata,.Codec,.Compression,.OpenAPI}`. | Native [Connect RPC protocol](https://connectrpc.com/docs/protocol) (connectrpc.com) client + server: unary + all three streaming kinds over HTTP/1.1 **and** HTTP/2, proto + JSON codecs, unary GET, identity/gzip/br/zstd compression, the gRPC-derived error model, leading/trailing metadata, and the EndStreamResponse envelope. **Not a separate codegen** — reuses the protocol-agnostic `Protobuf serv "meth"` tags (`loadProtoServices`, from `wireform-grpc`) and message types (`loadProto`, from `wireform-proto`) unchanged; Connect is purely a new transport over `grpc-spec`'s `IsRPC`/`SupportsClientRpc`/`SupportsServerRpc`/`HasStreamingType`. `Network.Connect.OpenAPI` (`connectOpenApi`/`renderOpenApi`, also `wireform-gen openapi`) emits an OpenAPI 3.1 document for a `.proto`'s Connect services — the transport-agnostic JSON Schema walk is `Proto.JSONSchema` in `wireform-proto`, this package adds the Connect HTTP shaping (paths, GET-for-idempotent, streaming content-types + `x-connect-streaming`, the `connect.Error` envelope). Like `wireform-grpc` it owns `Network.Connect.*` (RPC framework, not a `<Format>.*` package) and is not in the umbrella. Built on `wireform-http` (server + `Network.HTTP.Connection` client). The `Proto` newtype's `ToJSON`/`FromJSON` instances live in `grpc-spec` (`Network.GRPC.Spec.RPC.Protobuf`). NOTE: full-duplex bidi required teaching `wireform-http2`'s client to send a streaming request body on a forked thread (`registerAndSend`), so `registerAndSend` returns after HEADERS and the response can be read concurrently — *and* a deferred-response client bracket (`H2.withResponseDeferred` / `Connection.withResponseDeferredOn`): a Connect server stages its response headers until the handler's first send, so a ping-pong bidi client that blocked on response HEADERS before running its continuation would deadlock; `biDiStreaming` starts the continuation immediately and `recv` awaits the response head on first call. Test suite is an in-process loopback (all kinds × both codecs) + opt-in `Test.Interop` against `demo.connectrpc.com` gated by `CONNECT_DEMO`.|
| `wireform-lattice`    | `Lattice` (umbrella), `Lattice.{Types,Schema,Value,Hash,Cursor,Compress,Canonical}`, `Lattice.IDL.{Parser,Print}`, `Lattice.Query.{AST,Parser,Validate}`, `Lattice.Plan`, `Lattice.Wire`, `Lattice.Backend{,.Memory}`, `Lattice.Server{,.Auth,.Execute,.Coalesce,.Live}`, `Lattice.Client{,.Store}`, `Lattice.Digest`, `Lattice.Telemetry`, `Lattice.Compat`, `Lattice.Registry`, `Lattice.Module`, `Lattice.Gateway`. | **Lattice**, a cache-native graph query protocol (GraphQL/JSON:API problem space; the full spec + GraphQL comparison corpus live in the docs site under `/lattice/`, source `website/src/content/docs/lattice/`). Content-addressed canonical queries (BLAKE3, `blake3` dep with SIMD flags disabled — see cabal.project), GETs in the steady state with a QUERY/POST introduction ladder, normalized NDJSON entity streams, a compile-time authorization path-join partition (`pub`/`ctx`/`priv` slices keyed by a `vc` claims payload in the URL + HMAC proof header), schema-declared collections whose grouping keys derive both read-side `Surrogate-Key` cache tags and mutation write-set invalidation, at-most-once mutation idempotency keys, and origin budgets making N+1 inexpressible (set-in map-out `Lattice.Backend`). Bare `Lattice.*` namespace (like `Sim.*`); IDL fixtures in `test/fixtures/*.lattice` pin the surface; canonical-text/IDL goldens in `test/fixtures/golden/` pin cross-implementation encodings (a legitimate canonicalization change is a protocol event — regenerate with `--golden-reset` and bump the spec). `example-lattice` serves the Star Wars corpus demo (port 8917; `/debug/requests` counter, `LATTICE_PURGE_URL`/`LATTICE_PURGE_STYLE` CDN purge forwarder, `LATTICE_DEBUG_DELAY_MS`). `cdn/` holds the **Varnish (vmod_xkey) VCL + harness** and a **plug-and-play Cloudflare Worker** cache tier (KV surrogate-key index + `/_lattice/purge`), both proven by `cdn/conformance.mjs` (coalescing, tag purging, slice isolation, idempotent replay) — run from the dev shell (varnishd + `$LATTICE_VMOD_DIR` are in it; NO docker). Formerly deferred protocol areas all ship as of Draft 27: derived fields (§3.7), cache digests (§10.4), verb bindings (§11.7), live queries over SSE (§12), signed admission (§14.3), the `nodes` root (§14.4), the compatibility registry + `POST /schema/check` (§17), schema modules/`extend entity` fusion and the federation gateway + `/invalidations` feed (§18), and the §19 OTel conventions (`Lattice.Telemetry`, no-op by default); the docs matrix's simplifications table lists the deliberate remaining edges. NOTE for aarch64: the haskell `blake3` package's vendored cbits ship no `blake3_neon.c` while auto-enabling NEON dispatch, so any >1 KiB hash jumps through a null pointer unless the pinned `cabal.project` levers are kept (`package blake3` flags + `-optc-DBLAKE3_USE_NEON=0` + `constraints: blake3 source`; the nix global-DB unit is broken the same way). The TypeScript sibling is `lattice-ts/` at the repo root: a zero-runtime-dep Apollo-style client (canonicalizer, normalized store, transport ladder that learns hash URLs from introduction `Location` — clients never compute BLAKE3) whose headline is **merged multi-root queries**: React hooks mounting in one tick batch into a single request (`mergeQueries`, no aliases needed by design). |
| `wireform-kafka`      | `Kafka` (umbrella), `Kafka.Protocol.{RecordBatch,RecordBatchWire,CRC32C,ApiVersions,VersionNegotiation}`, `Kafka.Network.{Connection, Auth.{Plain,SCRAM,SASL}}`, `Kafka.Compression.{Gzip,Lz4,Snappy,Zstd,Types}`, `Kafka.Client.{Producer,Consumer,AdminClient,Transaction,Metadata,Pipeline,Internal.*}`, `Kafka.Telemetry.OpenTelemetry`. | Pure-Haskell native client for the Apache Kafka wire protocol (TCP / TLS / SASL / compression / version negotiation / transactions / consumer groups / pipelining / OTel). Depends on `wireform-kafka-protocol` for wire/generated types (import explicitly; not re-exported). Codegen: `wireform-kafka-codegen` + `scripts/regen-kafka-protocol.sh`. C FFI in `cbits/{crc32c,snappy_ffi,lz4_ffi}.c`. Tests gated by `WIREFORM_KAFKA_BROKER=host:port`; `Protocol.Generated.{Comprehensive,KnownGood}` need `test-vectors.json`. |
| `wireform-kafka-protocol` | `Kafka.Protocol.{Primitives,Message,Wire.*}`, `Kafka.Protocol.Generated.*` (one module per API key). | Generated request/response records and wire codec. Sources under `wireform-kafka/src`; separate package so Haddock and linking stay unambiguous. Regen via `scripts/regen-kafka-protocol.sh`; keep `wireform-kafka-protocol.cabal` `exposed-modules` in sync. |
| `wireform-websocket` | `Network.WebSocket{,.Frame,.Handshake,.Connection,.Message,.Server,.Client}`. | RFC 6455 WebSocket built on `Wireform.Parser` streaming mode + `Wireform.Builder`; SHA-1 + base64 handshake via `Wireform.Base64`. Standalone TCP / TLS listener (`runWebSocketServer`); `acceptWebSocketOn{Socket,Tls}` hand-off for integrating with the `wireform-http` server's accept loop. Client connect over `ws://` and `wss://` via `Wireform.Network.TLS.OpenSSL`. |
| `wireform-protovalidate` | `Protovalidate`, `Protovalidate.{Format,Library,Rules,Constraint,Eval,Schema,Class,Proto,Violation,OpenAPI}` | [protovalidate](https://protovalidate.com/) (CEL-driven Protobuf validation) for the proto stack. Depends on `wireform-cel` + `wireform-proto`. `Protovalidate.Library` registers protovalidate's CEL extension functions (`isEmail`/`isHostname`/`isHostAndPort`/`isIp`/`isIpPrefix`/`isUri`/`isUriRef`/`isNan`/`isInf`/`unique`); `Protovalidate.Format` has the underlying pure RFC predicates; `Protovalidate.Rules` encodes the standard rules as CEL over `this`/`rules`; `Protovalidate.Eval` binds field values + rules and collects `Violation`s (nested-message + repeated paths, custom field/message CEL); `Protovalidate.Schema` reads `(buf.validate.*)` annotations off a parsed `.proto` AST (`parseProtoRules`) into `MessageRules`; `Protovalidate.Class` is the compile-once typed path (`compileValidator`/`runValidator`/`validateValue` + a `ToCel` Generic deriving so generated records validate without a `DynamicMessage` round trip); `Protovalidate.Descriptor` reads `buf.validate` rules out of a compiled `FileDescriptorProto` (extension #1159 on `FieldOptions`/`MessageOptions`, now possible because `Proto.Google.Protobuf.Descriptor` preserves unknown fields); `Protovalidate.TH.compileMessageValidator` reads a `.proto`'s rules at compile time and emits a `Value -> [Violation]` whose every predicate (standard rules inlined over `this`, plus custom `cel`) is compiled to Haskell via `CEL.TH.compileCelFn` — no runtime parse/AST-walk; `Protovalidate.Refined` reifies rules as `refined` refinement types — native predicates for length/count/comparison rules and a type-level-`Symbol` `Cel`/`CelWith` predicate that runs CEL at validation time, so well-known formats and arbitrary/custom `cel` predicates also become refinement types (`refinedFieldType` emits the `Refined (...) T` type expression a code generator would splice); `Protovalidate.Proto` still bridges schemaless `Proto.Dynamic.DynamicMessage` → CEL; `Protovalidate.OpenAPI` (`protovalidateSchemaOptions`/`fieldConstraintsFor`) maps rules onto OpenAPI 3.1 / JSON Schema validation keywords for the `wireform-gen openapi --validate` doc generator — standard keywords (`minLength`/`pattern`/`format`/`minimum`/`maximum`/`minItems`/`uniqueItems`/`required`/…) where a faithful equivalent exists, custom CEL as an `x-cel` array and everything else under `x-protovalidate` (lossless); it feeds `Proto.JSONSchema`'s `SchemaOptions` annotator hook. Advanced rules: time-relative timestamps (`lt_now`/`gt_now`/`within`) via `validateAt` (binds `now`); `map.keys`/`map.values` sub-rules (`mapKeys`/`mapValues`, reported at `field[key]`, extracted from `.proto` map fields); `enum.defined_only` (`definedOnly`); oneof `required` (`oneofRequired`, also extracted from `(buf.validate.oneof)`); `string.well_known_regex` (`wellKnownRegex`); `(buf.validate.predefined)` reusable constraints via `frPredefined` (CEL + bound `rule`). `.proto` extraction (`parseProtoRules`) resolves these from source: `enum.defined_only` (enum value numbers → `this in [...]`, scalar + `repeated.items`), `string.well_known_regex` (+`strict`), `timestamp`/`duration` `{seconds,nanos}` message-literal bounds (and `timestamp.within`), `map.keys`/`map.values`, and oneof `required`. The `Protovalidate.TH` compiled path inlines the time-literal bounds (`timestamp(..)`/`duration("..s")`) and rides custom constraints (so defined_only/well_known_regex compile too); now-relative/map-key-value/predefined stay interpreted. `Protovalidate.Descriptor` (compiled `FileDescriptorProto`) covers the standard #1159 rules. |
| `wireform-cel` | `CEL`, `CEL.{Value,Syntax,Parser,Eval,Stdlib,Environment,Error,TH}` | A conformant [Common Expression Language](https://github.com/google/cel-spec/blob/master/doc/langdef.md) parser + evaluator over a dynamic `Value` model: full grammar/lexis (incl. backtick-escaped idents), number-line numeric semantics (`1 == 1u == 1.0` with cel-go's lossy cross-type rule, NaN unordered), error-absorbing `&&`/`||`, the comprehension macros (`has`/`all`/`exists`/`exists_one`/`map`/`filter` plus the two-variable `macros2` forms `all`/`exists`/`existsOne`/`transformList`/`transformMap`), and the standard library of operators, conversions, string/regex functions, and`Timestamp`/`Duration` support (named IANA timezones via `tz`). Passes the upstream cel-spec conformance suite for all non-message core files (`pass=1124 skip=128 fail=0`; skips are protobuf-message cases). Opt-in conformance runner gated by`CEL_SPEC_DIR` (like `TOML_TEST_SUITE`). The evaluator (`CEL.Eval`) is structured as per-node combinators (`compileExpr :: Expr -> Env -> Either CelError Value`);`CEL.TH` reuses them: `[cel\|…\|]`/`compileCel` bake the parsed `Expr` as a `Lift`able constant, and`[celFn\|…\|]`/`compileCelFn` emit the program as Haskell (each node → a combinator call) with no runtime AST walk. Not yet: protobuf message values, the optional type-checker. |
|`wireform-state-machine`|`StateMachine` (umbrella), `StateMachine.{Spec,Validate,Reify,Event,Registry,Machine,Step,Persist,Interpret,Debug}`, `StateMachine.Render.{XState,Mermaid,Dot,Html}`|Dependently typed statecharts — the full XState feature set (compound + parallel states, shallow/deep history, guards, entry/exit/transition actions, eventless `Always` + delayed `After` transitions, invoked services/actors with onDone/onError, done events with typed output, machine-wide typed context + per-event typed payloads) with the chart *specification at the type level* (`StateMachine.Spec`'s `ChartSpec` kind + DSL: `State`/`Compound`/`Parallel`/`Final`/`Hist`, `On "E" ?: "guard" ==> To "s" ! '["action"]`). Ill-formed charts (dangling targets, duplicate state names, undeclared events, misplaced `OnDone`, …) are `TypeError`s (`StateMachine.Validate`); guard/action/service/output implementations register into completeness-checked, order-insensitive `Symbol`-indexed registries à la `grpc-spec`'s `Service` (missing/duplicate/foreign names are compile errors naming the offender). Semantics are the SCXML/W3C algorithm at the value level (`StateMachine.Step`, pure: timers/invokes surface as `EffectReq` requests; `StateMachine.Interpret` executes them in IO with generation-tagged timers and child-actor routing; `StateMachine.Debug.simulate` executes them deterministically for tests). `Machine` configurations are unrepresentable-if-illegal outside `StateMachine.Persist.restore`, which validates snapshots (JSON, chart-fingerprinted FNV-1a) with precise `RestoreError`s + per-failure `Recovery` strategies (`Restart`/`ResumeAt`) for stale snapshots after chart evolution. Renderers: XState v5/Stately-importable JSON, Mermaid `stateDiagram-v2`, Graphviz DOT, self-contained HTML (Haskell-computed SVG, zero network deps). Demo: `cabal run example-traffic`.|

### `wireform-proto` — bigger surface, historical layout

The protobuf package predates the per-format split and is the
largest in the repo. It owns the `Proto.*` namespace and contains
both the IDL toolchain and the generated well-known types.

```
Proto.AST                              -- .proto IDL AST
Proto.Parser, Proto.Parser.{Lexer, Resolver, Error}
                                       -- IDL parser pipeline
Proto.Wire, Proto.Wire.{Encode, Decode, Result}
                                       -- wire-format primitives (tags, varints,
                                          unboxed-sum decode results)
Proto.Encode, Proto.Encode.{Direct, Lazy, Archetype}
Proto.Decode, Proto.Decode.{Fast, Stream, Streaming, Collect}
                                       -- `Collect` is the error-accumulating
                                          diagnostic decode (`decodeCollecting`):
                                          schema-driven, collects all recoverable
                                          issues with field paths instead of
                                          failing fast
                                       -- high-level encode / decode typeclasses
                                          and the hand-tuned hot paths
Proto.SizedBuilder, Proto.VectorBuilder
                                       -- builder utilities used by encoders
Proto.CodeGen, Proto.CodeGen.{Combinators, Decode, Encode, Service, Hooks, Types}
                                       -- pure-text Haskell code generator
Proto.TH                               -- IDL → TH bridge (`loadProto`,
                                          `loadProtoWith`, `loRepConfig`)
Proto.Derive                           -- annotation-driven TH deriver: hand-written
                                          records + `ANN tag`/`wireOverride`/
                                          `customModifier` produce instances
Proto.Derive.Internal                  -- body builders shared by `Proto.Derive`
                                          and `Proto.TH` (so the IDL bridge and the
                                          annotation deriver emit identical code)
Proto.Repr                             -- per-field representation choices
                                          (`StringRep`, `BytesRep`, `RepeatedRep`,
                                          `MapRep`, `FieldRep`, `RepConfig`)
Proto.Schema                           -- runtime type metadata
Proto.Compat                           -- version-compat helpers
Proto.Lens, Proto.Inspect, Proto.Print -- lens accessors, debug printers
Proto.Annotations                      -- `Annotation`s reified by the deriver
Proto.Options, Proto.Options.Custom    -- proto file/message/field options
Proto.Extension                        -- proto2 `extend`s
Proto.Registry                         -- runtime type registry + `IsMessage` marker + `discoverRegistry` TH splice
Proto.Setup                            -- Cabal `Setup.hs` integration
Proto.QQ                               -- proto-source QuasiQuoter
Proto.Dynamic                          -- dynamic (untyped) messages
Proto.TextFormat                       -- pbtxt serialisation
Proto.TDP                              -- transparent dynamic proto support
Proto.Conformance                      -- protobuf conformance test driver
Proto.Church                           -- Church-encoded message walks
Proto.Descriptor.Convert               -- AST ↔ descriptor.proto bridge
Proto.GRPC                             -- gRPC service-method codegen
Proto.JSON, Proto.JSON.WellKnown       -- proto3 JSON mapping (canonical encoding)
Proto.JSONSchema                       -- proto → JSON Schema (draft 2020-12 /
                                          OpenAPI 3.1) walk; transport-agnostic
                                          half of schema-derived API docs
                                          (wireform-connect's Network.Connect.OpenAPI
                                          adds the Connect HTTP shaping)
Proto.Internal.{Either, Maybe}         -- strict unboxed sums for hot loops
                                          (intra-package `.Internal`; do not import
                                          from outside `wireform-proto`)
Proto.Google.Protobuf.*                -- code-generated well-known types from
                                          `proto/google/protobuf/*.proto`
Proto.Google.Protobuf.*.Util           -- supplementary logic for well-known
                                          types (`packAny`, RFC 3339 formatting,
                                          `TypeRegistry`, `FieldMask` ops, etc.)
```

Everything under `Proto.Google.Protobuf.*` is regenerated from the
.proto files in `proto/google/protobuf/` by the `gen-wkt`
executable; see "Code Generation Principles" above.

## Annotation-driven deriver vocabulary

Every per-format `Derive` module accepts the same `Modifier`
vocabulary from `wireform-derive`. Rule of thumb when adding a new
constructor to `Wireform.Derive.Modifier.Modifier`:

1. Bump `wireform-derive` to a new version (any constructor add is
   technically API-breaking under PVP).
2. Extend `Wireform.Derive.ModifierInfo.ModifierInfo` with the
   resolved field, plus a `ConflictX` constructor in `ModifierError`.
3. Wire the new field into `mergeOne` and `shadowOne`.
4. Per-format derivers consult `mi<Whatever>` and silently ignore
   the field if the modifier does not apply to their backend (e.g.
   `miMapKey` is proto-only).

Backend-specific payloads that should not pollute the core ADT use
the `Wireform.Derive.Extension.BackendModifier` typeclass — see
`XmlFieldOpt`, `HtmlFieldOpt`, and `Asn1Tag` for examples.

## HTTP wire-format libraries (`hermes`)

The `hermes/` package is the **canonical home for HTTP header
parsing and rendering** in this monorepo. It originated as a vendor of
`MercuryTechnologies/hermes` (same author) and has since diverged
substantially — it is now a maintained fork, and this copy is the source of
truth for the wireform stack (the standalone upstream lags it). When the wire
grammar of an HTTP construct needs to be touched, the change goes in
`hermes`, not in a downstream `wireform-http*` module.

### What hermes owns

| Concern | Module(s) |
| --- | --- |
| Per-header `KnownHeader` instances (parse + render + cardinality) | `Network.HTTP.Headers.{Accept, AcceptEncoding, AcceptLanguage, Age, Allow, Authorization, CacheControl, CacheStatus, Connection, ContentDisposition, ContentEncoding, ContentLength, ContentType, Cookie, Date, ETag, Expires, From, Host, IfMatch, IfModifiedSince, IfNoneMatch, IfUnmodifiedSince, KeepAlive, LastModified, Location, Origin, ProxyAuthorization, Referer, RetryAfter, Server, SetCookie, Settings, Sunset, TransferEncoding, UserAgent, Vary, WWWAuthenticate}` |
| IANA registries (codings, methods-via-Allow, header-field-name CI strings) | `Network.HTTP.{ContentCoding, ContentNegotiation}`, `Network.HTTP.Headers.HeaderFieldName` |
| Quality-weighted lists (`q=` parsing, `WeightedMediaRange`, `WeightedLanguage`) | `Network.HTTP.ContentNegotiation`, `Network.HTTP.Headers.AcceptLanguage` |
| HTTP-date (IMF-fixdate, RFC 850, asctime) | `Network.HTTP.Headers.Date` |
| Percent-decoding (RFC 3986 + the C fast path) | `Network.HTTP.URL.Decode` (+ `cbits/url_decode.c`) |
| Builder primitives shared by every header renderer | `Network.HTTP.Headers.Mason`, `Network.HTTP.Headers.Rendering.Util` |
| Parser primitives (`rfc9110Token`, `quotedString`, `weightParser`, `ows`) shared by every header parser | `Network.HTTP.Headers.Parsing.Util` |

If a header you need is in that list, **call into hermes**. Do not
hand-roll a `BS.split 0x3B` / `BS.break (== 0x2C)` parser that
duplicates a `KnownHeader` instance — those will drift, miss
quoted-string escaping, miss obs-fold, and surprise the next
person who reads the code.

### When to extend hermes vs. when to wrap it

Pick the option that matches the kind of change you're making.
**Default to extending hermes.**

1. **Wire grammar / RFC compliance change** → `hermes/`. New
   header? New parameter? Bug in the q-value parser? Tightening a
   token check? That's a `KnownHeader` change. Add (or update) the
   instance in `Network.HTTP.Headers.<Name>`, including
   `parseFromHeaders` and `renderToHeaders`. Don't redefine the
   parser in `wireform-http`.

2. **Smart-constructor / domain wrapper / IsString instance** →
   `wireform-http*`. Hermes intentionally stays close to the wire
   types (often `ShortText` or `[Word8]` shaped); the
   ergonomic API that callers actually consume — newtypes,
   `IsString`, request combinators like `withRange` or
   `ifNoneMatch`, default values — lives in
   `Network.HTTP.Client.<Topic>`.

3. **Cross-cutting client / server policy** → `wireform-http*`.
   Cache freshness (RFC 9111), redirect following, retry, cookie
   jar, content-encoding registry of decompressors, the
   middleware stack, the connection pool. Hermes parses the
   `Cache-Control` directive list; *deciding what to cache* is a
   client concern that lives in `wireform-http`.

4. **A header that hermes simply doesn't have yet** → add it to
   hermes. Mirror the closest existing instance (e.g. `RetryAfter`
   for delta-or-date shapes, `Accept` for q-weighted lists,
   `SetCookie` for attribute-bag shapes), wire the
   `KnownHeader` cardinality / direction correctly, and
   re-export through the appropriate downstream module.

When you do extend hermes, **also** check whether the new parser
should be wired into a `wireform-http` middleware (e.g. retry
honoring `Retry-After`, conditional revalidation honoring `Vary`,
proxy honoring `Proxy-Authenticate`).

### Heuristics for spotting "this should call hermes"

If you find yourself writing one of the following patterns in
`wireform-http*` or `wireform-grpc`, stop and check whether the
hermes module above already covers the same ground:

- Splitting on `0x2C` / `0x3B` to peel apart a header value.
- A bespoke `parseQuality` / `parseQ` / weight-list parser.
- A copy of the IMF-fixdate format string.
- A `case BS.elemIndex 0x3D bs of` dance to extract an `auth-param`.
- A new `data MyChallenge = MyChallenge { realm :: …, nonce :: … }`
  when `Network.HTTP.Headers.Authorization.Credentials` already
  models the same shape.
- A handwritten `case rendered of "gzip" -> …; "br" -> …` dispatch
  on `Content-Encoding` (use `Network.HTTP.ContentCoding` instead).

### Rule of thumb

> Wire grammar lives in hermes. Domain modeling and policy live
> in `wireform-http*`. If you can't make a change cleanly because
> the grammar in hermes is missing a piece, **add it to hermes
> first**, then build the wrapper.

Touching hermes is fine — it ships from this monorepo. Avoid
forking grammars across packages.

When you add or touch a `Network.HTTP.Headers.<Name>` module, give it a
module-level Haddock block that follows the **header module documentation
standard** (plain-English explainer + spec autolink + a `See also:` block of
`"Network.HTTP.Headers.*"` cross-references to related headers). The full
standard lives in [`hermes/PROJECT_GUIDE.md`](hermes/PROJECT_GUIDE.md#module-documentation-standard).
