# wireform-proto conformance harness

A Cabal test suite (`protobuf-conformance-test`) that runs the
official protobuf [conformance suite](https://github.com/protocolbuffers/protobuf/tree/main/conformance)
against `wireform-proto`'s `loadProto`-generated codecs.

## What's in here

```
wireform-proto/test-conformance/
├── README.md               # this file
├── Driver.hs               # tasty test entry point
├── Runner.hs               # the binary the upstream runner pipes to
├── Test/Conformance/
│   ├── Schema.hs           # loadProto splice for the conformance + TestAllTypes schemas
│   └── Handler.hs          # per-request dispatch logic
├── failure_list_proto3.txt # tests we knowingly don't pass (JSPB, TEXT_FORMAT, WKT JSON)
├── protos/
│   ├── conformance.proto             # vendored from protocolbuffers/protobuf
│   └── test_messages_proto3.proto    # vendored subset of TestAllTypesProto3
└── scripts/
    └── build-conformance-runner.sh   # clones + builds the upstream C++ runner
```

## How it works

1. `Test.Conformance.Schema` splices in the conformance protocol's
   schema (`ConformanceRequest` / `ConformanceResponse` /
   `WireFormat` / `FailureSet` / `TestStatus`) plus
   `TestAllTypesProto3` from the upstream test schema.

2. `Test.Conformance.Handler` decodes each `ConformanceRequest`,
   dispatches on `message_type`, runs the appropriate codec
   (`PROTOBUF` round-trip via `loadProto`-generated codecs;
   `JSON` round-trip via the `Aeson` instances the same splice
   generates), and emits a `ConformanceResponse`.

3. `Runner.hs` is the binary the upstream `conformance_test_runner`
   pipes length-prefixed requests to and reads responses from.

4. `Driver.hs` is the tasty entry point. It locates the upstream
   runner (via the `CONFORMANCE_TEST_RUNNER` env var, or the
   default `dist-newstyle/conformance/conformance_test_runner`
   path), launches it against the Haskell runner binary, and
   asserts the upstream runner exits cleanly. Without the runner
   present the test skips with instructions.

5. `failure_list_proto3.txt` lists tests we know we don't pass
   (JSPB is Google-internal; TEXT_FORMAT isn't implemented yet;
   JSON for WKT-typed payloads requires importing the
   `google.protobuf.*` schemas which `loadProto` doesn't yet do).
   The runner's `--failure_list` flag treats these as expected
   failures so the suite's net pass/fail is on regressions only.

## Setup

### One-time: build the upstream runner

```bash
bash wireform-proto/test-conformance/scripts/build-conformance-runner.sh
```

This clones `protocolbuffers/protobuf@v28.2` into
`dist-newstyle/conformance/protobuf/`, configures with cmake, and
builds the `conformance_test_runner` target. Result lands at
`dist-newstyle/conformance/conformance_test_runner`. Re-running
the script is a no-op once the binary exists; pass `--force` to
rebuild.

Requires: `git`, `cmake >= 3.13`, a C++17 compiler. The script
pulls abseil via cmake `FetchContent` so no preinstall needed.

### Run the suite

```bash
cabal test wireform-proto:protobuf-conformance-test
```

The first run builds `wireform-conformance-runner` (the Haskell
testee binary) via cabal. Each subsequent run reuses the cache.

To point at a runner installed elsewhere:

```bash
CONFORMANCE_TEST_RUNNER=/path/to/conformance_test_runner \
  cabal test wireform-proto:protobuf-conformance-test
```

To exercise the Editions and migration schemas with the upstream v34.1 runner:

```bash
cabal build wireform-proto:exe:wireform-conformance-runner
"$CONFORMANCE_TEST_RUNNER" --maximum_edition 2024 \
  "$(cabal list-bin wireform-proto:exe:wireform-conformance-runner)"
```

To use a different failure list:

```bash
CONFORMANCE_FAILURE_LIST=path/to/list.txt \
  cabal test wireform-proto:protobuf-conformance-test
```

### Adding expected failures

When the upstream runner reports new failures that we expect (e.g.
because the corresponding feature isn't implemented yet), add a
line to `failure_list_proto3.txt` with a one-line `# comment`
explaining why. Glob patterns are supported per the upstream
runner's parser.

The runner handles binary protobuf, ProtoJSON, and TextFormat for the
vendored schemas, including the well-known types. It skips JSPB requests.
