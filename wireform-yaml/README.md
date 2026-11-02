# wireform-yaml

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)


> [!CAUTION]
> wireform is in heavy development and has not been published to Hackage yet. APIs may change.

[YAML 1.2](https://yaml.org/spec/1.2.2/) for Haskell. Encode and
decode the dynamic [`YAML.Value`](src/YAML/Value.hs), derive typeclass
instances generically or via Template Haskell, work with an annotated
AST that preserves source spans / comments / scalar style,
pretty-print back to YAML, bridge to JSON, and pass the official
[yaml-test-suite](https://github.com/yaml/yaml-test-suite) when you
point the test harness at a clone of it.

YAML 1.2 covers block-style and flow-style scalars, sequences, and
mappings; anchors and aliases (with merge-key semantics and a document-wide
expansion budget); explicit
tags; the four scalar styles (plain, single-quoted, double-quoted,
literal-folded); multi-document streams; and the JSON-compatible flow
subset. wireform-yaml implements the spec close enough to read and
write the corner cases that show up in Kubernetes manifests, CI
configs, and Ansible playbooks without surprising round-trip
behavior. It also exposes a separate annotated AST for tooling that
needs to preserve source positions and trivia (comments, blank
lines, scalar style) across a load / save cycle.

This package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo and shares its allocation primitives, annotation deriver, and
testing discipline with every other format.

## Install

```cabal
build-depends:
  base,
  wireform-yaml,
  wireform-derive,    -- only if you want the cross-format annotation deriver
```

The package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo. Clone the repo and `cabal build wireform-yaml` to compile
locally. Compiling with the LLVM backend (`-fllvm`) adds compile time
but measurably improves runtime performance.

## Hello world

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}

import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Text as T
import YAML.Class (ToYAML, FromYAML, encodeYAML, decodeYAML)

data Service = Service
  { name     :: !Text
  , replicas :: !Int
  , image    :: !Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToYAML, FromYAML)

main :: IO ()
main = do
  let svc  = Service "api" 3 "ghcr.io/example/api:1.4.2"
      text = encodeYAML svc
  putStrLn (T.unpack text)
  case decodeYAML text of
    Right (decoded :: Service) -> print decoded
    Left  err                  -> putStrLn err
```

`encodeYAML svc` renders to:

```yaml
name: api
replicas: 3
image: ghcr.io/example/api:1.4.2
```

## What's in here

| Module                 | Role                                                      |
|------------------------|-----------------------------------------------------------|
| `YAML.Value`           | Dynamic untyped `Value` ADT (scalars, sequences, mappings, tags, anchors) plus the `Document` and `Stream` wrappers for multi-document inputs |
| `YAML.Encode`          | Emitter: `Value` / `Document` / `Stream` to YAML `Text` or `ByteString` |
| `YAML.Decode`          | Loader: `ByteString` / `Text` to `Value` / `Document` / `Stream`, with anchor and alias resolution |
| `YAML.Encoding`        | Builder used by `ToYAML` instances                        |
| `YAML.Pretty`          | Render `Value` / `Document` / `Stream` back to YAML text with configurable scalar style, indent, and flow / block preference (`render`, `renderBS`, `renderDocument`, `renderStream`, `defaultOptions`, `compactOptions`) |
| `YAML.Class`           | Public `ToYAML` / `FromYAML` typeclasses + `encodeYAML` / `encodeYAMLBS` / `decodeYAML` / `decodeYAMLBS` |
| `YAML.Derive`          | `deriveYAML` / `deriveToYAML` / `deriveFromYAML` Template Haskell entry points |
| `YAML.Annotated`       | Annotated AST (`AValue`, `ADocument`, `AStream`) that preserves source spans, comments, scalar style, and chomping indicators |
| `YAML.Decode.Annotated`| Loader for the annotated AST                              |
| `YAML.JSON`            | Lossy bridge to and from `aeson`'s `Value` (mapping keys are stringified, tags and anchors are dropped) |

## Encode and decode

The typeclass entry points cover both `Text` and `ByteString` I/O:

```haskell
encodeYAML   :: ToYAML   a => a          -> Text
encodeYAMLBS :: ToYAML   a => a          -> ByteString
decodeYAML   :: FromYAML a => Text       -> Either String a
decodeYAMLBS :: FromYAML a => ByteString -> Either String a
```

For dynamic values without a Haskell type to mirror them, work with
[`YAML.Value`](src/YAML/Value.hs) directly. The `Value` ADT carries
the explicit YAML tag on each node (`!!str`, `!!int`, `!!seq`,
custom tags), so type information that JSON would discard survives
the round-trip.

For multi-document YAML streams (`---` separated), use the `Document`
and `Stream` wrappers in `YAML.Value`, `decodeStream` / `decodeStreamBS`,
and `encodeStream`. Single-document entry points reject extra documents.

## Annotated AST

The annotated AST in `YAML.Annotated` is the surface to use when you
need to preserve more than the data model: source spans for error
reporting, scalar style for round-trip preservation, comments and
blank lines (trivia) for tooling that has to round-trip a file
without rewriting unrelated lines.

```haskell
import qualified YAML.Decode.Annotated as YDA
import qualified YAML.Pretty           as YP

case YDA.decodeAnnotated input of
  Right (doc, original) -> putStrLn (T.unpack (YP.renderAnnotatedDocument YP.defaultOptions doc (Just original)))
  Left  err -> putStrLn err
```

Edits preserve untouched flow-node tokens, quoting, and comments as well as
block-entry text. Changed flow collections may be laid out over multiple lines
to retain their comments. Parsed aliases keep their binding identities: changing
an anchor updates its live references, while deleting or moving a definition
materializes references that would otherwise become invalid. Editing through an
alias makes an independent copy.

Common consumers: language servers, schema-driven config editors,
formatters, and any tool that has to make a small change to a YAML
file without touching the rest.

## Annotation-driven deriving

`YAML.Derive` consumes the cross-format `Wireform.Derive.Modifier`
vocabulary from [`wireform-derive`](../wireform-derive/README.md), so
the same annotated record can produce YAML, JSON, and any other
backend's instances:

```haskell
{-# LANGUAGE TemplateHaskell #-}

import qualified YAML.Derive          as DYAML
import qualified Wireform.Derive.Aeson as DAeson
import Wireform.Derive (rename, renameStyle, KebabCase)

data DeploymentSpec = DeploymentSpec
  { deploymentReplicas       :: !Int
  , deploymentImagePullPolicy :: !Text
  } deriving stock (Show, Eq, Generic)

{-# ANN type DeploymentSpec ("DeploymentSpec" :: String) #-}
{-# ANN deploymentReplicas        (renameStyle KebabCase) #-}
{-# ANN deploymentImagePullPolicy (renameStyle KebabCase) #-}

DYAML.deriveYAML  ''DeploymentSpec
DAeson.deriveJSON ''DeploymentSpec
```

Record constructors inside sums use a mapping for `contents`, so their field
renames, defaults, optional fields, and skipped fields work just like standalone
records. Positional constructors retain scalar or sequence `contents`.

## JSON bridge

`YAML.JSON` round-trips between `YAML.Value` and `Data.Aeson.Value`.
The bridge is intentionally lossy: YAML's data model is a strict
superset of JSON's, so non-string mapping keys are stringified, and
explicit tags and anchors are dropped. See the module haddock for the
exact mapping table. Non-finite YAML floats become JSON `null`.
YAML integers use arbitrary-precision `Integer`; bounded Haskell integer
decoders reject overflow, negative unsigned values, and fractional inputs.
ByteString decoder entry points detect UTF-8, UTF-16, and UTF-32 and reject
malformed encodings. Native records and maps reject ambiguous field names after
tag metadata is removed. The ByteString serialization instance uses a text scalar
for valid UTF-8 and a sequence of byte-valued integers for arbitrary binary data.

The parser bounds nesting and logical alias expansion (10 million nodes), rejects
cyclic/forward aliases, and limits tracked indentation to 1,048,574 spaces.

## Testing

The per-format Hedgehog suite lives in `test/`:

```bash
cabal test wireform-yaml:wireform-yaml-test
```

<!-- BEGIN_AUTOGEN tests -->
_No data yet. Run `cabal test wireform-yaml:all --test-show-details=streaming --xml=dist-stats/test-results/wireform-yaml.junit.xml` to populate._
<!-- END_AUTOGEN tests -->

It covers the parser, the emitter, the typeclass instances, the
annotated AST, security-relevant edge cases (billion-laughs anchor
expansion, deep nesting), and round-trip property tests against
`Value`.

### Conformance against `yaml-test-suite`

The test binary also runs an opt-in conformance harness against the
official [yaml/yaml-test-suite](https://github.com/yaml/yaml-test-suite).
Point `YAML_TEST_SUITE` at a local clone and the harness will walk
every case it finds:

```bash
git clone --branch data https://github.com/yaml/yaml-test-suite /tmp/yaml-test-suite
YAML_TEST_SUITE=/tmp/yaml-test-suite \
  cabal test wireform-yaml:wireform-yaml-test
```

The event-only upstream fixtures `X38W` and `2JQS` contain duplicate keys.
The harness asserts that value composition rejects them; it does not treat
valid parser events as permission to construct a mapping with duplicate keys.
All available JSON companions are compared across every document. Each accepted
fixture also round-trips structurally through the fast emitter, both pretty
layouts, and annotated normalization; collection-valued keys are checked directly.

When the env var is unset the harness reports a no-op skip group so
CI stays green out of the box. A built-in mini-suite drawn from the
YAML 1.2 spec examples always runs, so core compliance is exercised
even without the external clone.

## Benchmarks

A criterion harness in [`bench/Bench.hs`](bench/Bench.hs) compares
wireform-yaml's parser and emitter against the two established Haskell
YAML libraries:

- [`HsYAML`](https://hackage.haskell.org/package/HsYAML), pure
  Haskell, prioritises spec compliance.
- [`yaml`](https://hackage.haskell.org/package/yaml), bindings to
  libyaml.

```bash
cabal bench wireform-yaml:wireform-yaml-bench
```

<!-- BEGIN_AUTOGEN bench:yaml-decode-vs-libyaml -->
<picture>
  <source media="(prefers-color-scheme: dark)" srcset="bench-results/charts/yaml-decode-vs-libyaml-dark.svg">
  <img src="bench-results/charts/yaml-decode-vs-libyaml-light.svg" alt="wireform-yaml decode vs libyaml across input sizes">
</picture>

| Operation | wireform-yaml | yaml (libyaml) |  ratio |
| :-------- | ------------: | -------------: | -----: |
| tiny      |       0.44 µs |        6.14 µs | 13.95x |
| small     |       6.73 µs |        34.6 µs |  5.15x |
| flow      |       7.51 µs |        44.2 µs |  5.88x |
| literal   |       4.71 µs |        11.2 µs |  2.38x |
| big       |       37.5 µs |         122 µs |  3.26x |

<sub>Last run 2026-11-02 14:35:47 UTC. ghc-9.10.3 on darwin-aarch64, criterion 1.6.5.</sub>
<!-- END_AUTOGEN bench:yaml-decode-vs-libyaml -->

<!-- BEGIN_AUTOGEN bench:yaml-decode-vs-hsyaml -->
<picture>
  <source media="(prefers-color-scheme: dark)" srcset="bench-results/charts/yaml-decode-vs-hsyaml-dark.svg">
  <img src="bench-results/charts/yaml-decode-vs-hsyaml-light.svg" alt="wireform-yaml decode vs HsYAML (pure Haskell) across input sizes">
</picture>

| Operation | wireform-yaml |  HsYAML |   ratio |
| :-------- | ------------: | ------: | ------: |
| tiny      |       0.44 µs |  119 µs | 270.86x |
| small     |       6.73 µs | 1068 µs | 158.75x |
| flow      |       7.51 µs | 1322 µs | 176.03x |
| literal   |       4.71 µs | 1504 µs | 319.30x |
| big       |       37.5 µs | 5027 µs | 133.95x |

<sub>Last run 2026-11-02 14:35:47 UTC. ghc-9.10.3 on darwin-aarch64, criterion 1.6.5.</sub>
<!-- END_AUTOGEN bench:yaml-decode-vs-hsyaml -->

### Escaped strings and derived records

<!-- BEGIN_AUTOGEN bench:yaml-extra-vs-libyaml -->
<picture>
  <source media="(prefers-color-scheme: dark)" srcset="bench-results/charts/yaml-extra-vs-libyaml-dark.svg">
  <img src="bench-results/charts/yaml-extra-vs-libyaml-light.svg" alt="Escaped strings and derived records vs libyaml">
</picture>

| Operation      | wireform-yaml | yaml (libyaml) | ratio |
| :------------- | ------------: | -------------: | ----: |
| escaped decode |       7.12 µs |        13.6 µs | 1.91x |
| record decode  |       1.85 µs |        11.9 µs | 6.43x |
| record encode  |       1.25 µs |        5.36 µs | 4.29x |

<sub>Last run 2026-11-02 14:35:47 UTC. ghc-9.10.3 on darwin-aarch64, criterion 1.6.5.</sub>
<!-- END_AUTOGEN bench:yaml-extra-vs-libyaml -->

The record fixture uses the actual `deriveYAML` output. Every benchmark fixture
is checked for successful decoding before timing; the HsYAML comparison forces
the parsed contents as well as the outer document list.

ASCII validation scans eight bytes at a time and skips line normalization when
there are no carriage returns. Unicode and unusual controls retain lexical
validation. Literal scalars measure their final chomped length and copy their
UTF-8 lines into one exact buffer, so discarded trailing blank lines are neither
copied nor retained. Escaped scalars likewise write directly into a bounded
UTF-8 buffer. Optimized Core shows bulk byte copies, direct byte writes, and
unboxed offset loops without per-escape result allocation.

For cross-language comparisons:

- C: [libyaml](https://github.com/yaml/libyaml). The Hackage
  [`yaml`](https://hackage.haskell.org/package/yaml) package is a
  thin binding around it, so the Haskell-vs-Haskell number above is
  already partly cross-language.
- Rust: [`serde_yaml`](https://crates.io/crates/serde_yaml) (the
  established library) and [`saphyr`](https://crates.io/crates/saphyr)
  (a newer, faster alternative).

The committed tables above are generated from Criterion measurements. The harness
also exercises floating-point boundaries and the generated native-record codec.

## License

BSD-3-Clause.

## References

- [YAML 1.2.2 specification](https://yaml.org/spec/1.2.2/)
- [yaml/yaml-test-suite](https://github.com/yaml/yaml-test-suite) (the official conformance suite)
- [YAML 1.2 core schema](https://yaml.org/spec/1.2.2/#103-core-schema)
