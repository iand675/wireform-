---
title: wireform-toml
description: "TOML 1.0 and 1.1 encoding and decoding with TH deriving, section-aware pretty printing, and datetime support."
sidebar:
  order: 23
---

`wireform-toml` reads and writes TOML configuration for Haskell services and
tools. TOML's table sections, inline tables, and array-of-tables map naturally
onto Haskell records when you derive `Generic`, while the encoder places
`[section]` headers and `[parent.child]` paths correctly on output. The parser
is validated against the upstream [toml-test](https://github.com/toml-lang/toml-test)
suite for both TOML 1.0 and 1.1.

## Key features

| Capability | Why it matters |
|------------|----------------|
| `deriveTOML` Template Haskell deriver | Load config files into typed records with `wireform-derive` annotations; Generic defaults work for simple cases |
| Section-aware pretty printing | `[database]` and nested `[database.pool]` headers land in sensible order |
| Datetime support | RFC 3339 offsets and local datetimes as first-class values |
| Inline and standard tables | Compact inline `{ key = "val" }` or full `[table]` blocks |
| Array-of-tables | `[[items]]` repeated sections for list-of-struct configs |
| toml-test conformance | Confidence that edge cases match the spec |

## Basic usage

### Typed configuration

Derive codecs with the Template Haskell deriver and round-trip with
`encodeTOML` / `decodeTOML`.

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import GHC.Generics (Generic)
import Data.Text (Text)
import TOML.Class (ToTOML, FromTOML, encodeTOML, decodeTOML)
import TOML.Derive (deriveTOML)

data Database = Database
  { dbHost :: !Text
  , dbPort :: !Int
  } deriving stock (Generic)

data AppConfig = AppConfig
  { appName  :: !Text
  , database :: !Database
  } deriving stock (Generic)

$(deriveTOML ''Database)
$(deriveTOML ''AppConfig)

loadConfig :: Text -> Either String AppConfig
loadConfig = decodeTOML
```

For simple cases with no wire-format customization, Generic defaults also
work: add `deriving Generic` and declare empty `instance ToTOML Database` and
`instance FromTOML Database` declarations (and likewise for `AppConfig`).

Nested records become TOML subtables. The encoder emits a `[database]` section
with keys under it rather than flattening everything at the top level.

### Datetimes

TOML datetimes live in `TOML.Value` as `TLocalDateTime`, `TOffsetDateTime`, and
related constructors. Deriving handles them when fields use the corresponding
Haskell types from the value module.

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import GHC.Generics (Generic)
import Data.Text (Text)
import TOML.Class (FromTOML, decodeTOML)
import TOML.Derive (deriveTOML)

data Job = Job
  { jobName     :: !Text
  , scheduledAt :: !Text
  } deriving stock (Generic)

$(deriveTOML ''Job)

parseJob :: Text -> Either String Job
parseJob = decodeTOML
```

TOML datetimes decode as `Text` in the value layer (`TDateTime`, `TDate`,
`TTime`). Map them into `time` types in application code when you need calendar
arithmetic.

### Direct encoding for large configs

`encodeTOMLDirect` routes through `toEncoding` when you want the same path the
TH deriver uses, which can avoid an extra conversion for complex nested values.

```haskell
import TOML.Class (ToTOML, encodeTOMLDirect)

writeConfig :: ToTOML a => a -> Text
writeConfig = encodeTOMLDirect
```

Use `TOML.Decode.decode` on raw text when you need the untyped `TOML.Value`
AST before mapping into application types.

## Performance

### Encode/decode

<!-- BEGIN_AUTOGEN bench:toml-encode-decode -->
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 720 400" width="720" height="400" role="img" font-family="ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, &quot;Segoe UI&quot;, Helvetica, Arial, sans-serif" font-size="12">
  <title>wireform-toml encode + decode (Person record)</title>
  <style>.wf-dark{display:none}@media (prefers-color-scheme:dark){.wf-light{display:none}.wf-dark{display:inline}}</style>
  <g class="wf-light">
    <rect x="0" y="0" width="720" height="400" fill="#ffffff"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#1f2328">wireform-toml encode + decode (Person record)</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#656d76">lower is better · ns · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5. Decode is now linear in input size (was previously O(N²) due to T.index/T.length on the full source); 100-record decode dropped from 240 ms to 335 µs.</text>
    <g stroke="#d0d7de" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#656d76">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#656d76">125000</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#656d76">250000</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#656d76">375000</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#656d76">500000</text>
    </g>
    <g>
      <rect x="171" y="319.6" width="62" height="0.4" rx="2" fill="#0969da"/>
      <rect x="235" y="318.8" width="62" height="1.2" rx="2" fill="#cf222e"/>
      <rect x="481" y="275.3" width="62" height="44.7" rx="2" fill="#0969da"/>
      <rect x="545" y="141.2" width="62" height="178.8" rx="2" fill="#cf222e"/>
    </g>
    <g>
      <text x="202" y="315.6" text-anchor="middle" font-size="10" fill="#1f2328">730</text>
      <text x="266" y="314.8" text-anchor="middle" font-size="10" fill="#1f2328">2383</text>
      <text x="512" y="271.3" text-anchor="middle" font-size="10" fill="#1f2328">86012</text>
      <text x="576" y="137.2" text-anchor="middle" font-size="10" fill="#1f2328">343876</text>
    </g>
    <g>
      <text x="235" y="338" text-anchor="middle" font-size="11" fill="#1f2328">single Person</text>
      <text x="545" y="338" text-anchor="middle" font-size="11" fill="#1f2328">[Person] x 100</text>
    </g>
    <g>
      <g transform="translate(292, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#0969da"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">encode</text>
      </g>
      <g transform="translate(368, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#cf222e"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">decode</text>
      </g>
    </g>
  </g>
  <g class="wf-dark">
    <rect x="0" y="0" width="720" height="400" fill="#0d1117"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#e6edf3">wireform-toml encode + decode (Person record)</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#7d8590">lower is better · ns · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5. Decode is now linear in input size (was previously O(N²) due to T.index/T.length on the full source); 100-record decode dropped from 240 ms to 335 µs.</text>
    <g stroke="#30363d" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#7d8590">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#7d8590">125000</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#7d8590">250000</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#7d8590">375000</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#7d8590">500000</text>
    </g>
    <g>
      <rect x="171" y="319.6" width="62" height="0.4" rx="2" fill="#58a6ff"/>
      <rect x="235" y="318.8" width="62" height="1.2" rx="2" fill="#ff7b72"/>
      <rect x="481" y="275.3" width="62" height="44.7" rx="2" fill="#58a6ff"/>
      <rect x="545" y="141.2" width="62" height="178.8" rx="2" fill="#ff7b72"/>
    </g>
    <g>
      <text x="202" y="315.6" text-anchor="middle" font-size="10" fill="#e6edf3">730</text>
      <text x="266" y="314.8" text-anchor="middle" font-size="10" fill="#e6edf3">2383</text>
      <text x="512" y="271.3" text-anchor="middle" font-size="10" fill="#e6edf3">86012</text>
      <text x="576" y="137.2" text-anchor="middle" font-size="10" fill="#e6edf3">343876</text>
    </g>
    <g>
      <text x="235" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">single Person</text>
      <text x="545" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">[Person] x 100</text>
    </g>
    <g>
      <g transform="translate(292, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#58a6ff"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">encode</text>
      </g>
      <g transform="translate(368, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#ff7b72"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">decode</text>
      </g>
    </g>
  </g>
</svg>


| Operation      |   encode |    decode |  ratio |
| :------------- | -------: | --------: | -----: |
| single Person  |   730 ns |   2383 ns |  3.26x |
| [Person] x 100 | 86012 ns | 343876 ns | 3.100x |

<sub>Last run 2026-06-27 11:35:55 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5. Decode is now linear in input size (was previously O(N²) due to T.index/T.length on the full source); 100-record decode dropped from 240 ms to 335 µs..</sub>
<!-- END_AUTOGEN bench:toml-encode-decode -->

Decode scales linearly with input size.

The chart and table above are regenerated by [`wireform-stats`](../stats/) from `wireform-toml/bench-results/summary/toml-encode-decode.json` — the same source the README chart is built from.

## Notable modules

| Module | Role |
|--------|------|
| `TOML.Class` | `ToTOML` / `FromTOML`, `encodeTOML`, `decodeTOML` |
| `TOML.Value` | AST for tables, arrays, datetimes, and inline tables |
| `TOML.Encode` | Section-aware TOML writer |
| `TOML.Decode` | Parser for TOML 1.0 / 1.1 documents |
| `TOML.Encoding` | Intermediate encoding type used by the deriver |
| `TOML.Derive` | Template Haskell deriver with `Wireform.Derive` annotations |
