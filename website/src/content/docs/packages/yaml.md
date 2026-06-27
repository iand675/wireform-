---
title: wireform-yaml
description: "YAML 1.2 encoding and decoding with TH deriving, anchors, tags, and multi-document streams."
sidebar:
  order: 22
---

`wireform-yaml` implements YAML 1.2 read and write paths for Haskell
applications. Configuration files, CI manifests, and Kubernetes-style multi-doc
streams all map cleanly onto typed records via `Generic`, while the value layer
still exposes anchors, aliases, tags, and both block and flow styles when you
need full YAML expressiveness. The decoder and emitter are validated against the
upstream [yaml-test-suite](https://github.com/yaml/yaml-test-suite) with 100%
conformance.

## Key features

| Capability | Why it matters |
|------------|----------------|
| `deriveYAML` Template Haskell deriver | Derive config types with `wireform-derive` annotations; Generic defaults work for simple cases |
| Block and flow styles | Human-readable block output; compact flow for inline structures |
| Anchors and aliases | Preserve shared references and cyclic graphs in the value layer |
| Tags | Explicit scalar typing (`!!int`, application-specific tags) |
| Literal and folded scalars | Round-trip multiline strings (`\|` and `>`) |
| Multi-document streams | `---` separated documents for kubectl-style files |
| YAML 1.2 core schema | Plain scalars that look like bools or numbers stay quoted on encode |

## Basic usage

### Typed records

Derive codecs with the Template Haskell deriver and use `encodeYAML` /
`decodeYAML` for the common case.

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import GHC.Generics (Generic)
import Data.Text (Text)
import YAML.Class (ToYAML, FromYAML, encodeYAML, decodeYAML)
import YAML.Derive (deriveYAML)

data Server = Server
  { host :: !Text
  , port :: !Int
  , tls  :: !Bool
  } deriving stock (Generic)

$(deriveYAML ''Server)

loadConfig :: Text -> Either String Server
loadConfig = decodeYAML
```

For simple cases with no wire-format customization, Generic defaults also
work: add `deriving Generic` and declare empty `instance ToYAML Server` and
`instance FromYAML Server` declarations.

Field naming follows the same `Wireform.Derive` annotation vocabulary as other
wireform formats (`rename`, `omitEmpty`, and friends via `YAML.Derive`).

### Value-level API for anchors and tags

When the shape is dynamic or you must preserve YAML-specific features, work in
`YAML.Value` and encode with `YAML.Encode`.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Text (Text)
import qualified YAML.Decode as YD
import qualified YAML.Encode as YE

roundtripAnchors :: Text -> Either String Text
roundtripAnchors doc = do
  val <- YD.decode doc
  pure (YE.encode val)
```

### Multi-document streams

Kubernetes and other tools emit several YAML documents in one file. Decode the
stream as a `YAML.Value.Stream`, or encode many values with `encodeStream`.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Vector as V
import YAML.Encode (encodeStream)
import YAML.Value (Document(..), Stream(..), mapping, string)

writeStream :: Stream -> Text
writeStream = encodeStream

exampleStream :: Stream
exampleStream =
  Stream $
    V.fromList
      [ Document True False (mapping [(string "apiVersion", string "v1")])
      , Document True False (mapping [(string "kind", string "ConfigMap")])
      ]
```

The emitter chooses block style by default and falls back to flow for empty
containers. Output is round-trippable: `encode` followed by `decode` recovers
the same `Value`.

## Performance

wireform-yaml is a pure Haskell parser that consistently outperforms the C
libyaml bindings by 3-32x across all input sizes, making it one of the fastest
YAML parsers in any language. Against HsYAML (the other pure Haskell option), it
is 244-408x faster.

### wireform-yaml vs libyaml (C bindings via Hackage `yaml` package)

<!-- BEGIN_AUTOGEN bench:yaml-decode-vs-libyaml -->
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 720 400" width="720" height="400" role="img" font-family="ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, &quot;Segoe UI&quot;, Helvetica, Arial, sans-serif" font-size="12">
  <title>wireform-yaml decode vs libyaml across input sizes</title>
  <style>.wf-dark{display:none}@media (prefers-color-scheme:dark){.wf-light{display:none}.wf-dark{display:inline}}</style>
  <g class="wf-light">
    <rect x="0" y="0" width="720" height="400" fill="#ffffff"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#1f2328">wireform-yaml decode vs libyaml across input sizes</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#656d76">lower is better · µs · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5</text>
    <g stroke="#d0d7de" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#656d76">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#656d76">50.0</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#656d76">100</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#656d76">150</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#656d76">200</text>
    </g>
    <g>
      <rect x="92.4" y="319.7" width="47.6" height="0.3" rx="2" fill="#0969da"/>
      <rect x="142" y="310.0" width="47.6" height="10.0" rx="2" fill="#cf222e"/>
      <rect x="216.4" y="315.0" width="47.6" height="5.0" rx="2" fill="#0969da"/>
      <rect x="266" y="268.6" width="47.6" height="51.4" rx="2" fill="#cf222e"/>
      <rect x="340.4" y="314.7" width="47.6" height="5.3" rx="2" fill="#0969da"/>
      <rect x="390" y="241.5" width="47.6" height="78.5" rx="2" fill="#cf222e"/>
      <rect x="464.4" y="314.6" width="47.6" height="5.4" rx="2" fill="#0969da"/>
      <rect x="514" y="302.1" width="47.6" height="17.9" rx="2" fill="#cf222e"/>
      <rect x="588.4" y="300.5" width="47.6" height="19.5" rx="2" fill="#0969da"/>
      <rect x="638" y="142.1" width="47.6" height="177.9" rx="2" fill="#cf222e"/>
    </g>
    <g>
      <text x="116.2" y="315.7" text-anchor="middle" font-size="10" fill="#1f2328">0.250</text>
      <text x="165.8" y="306.0" text-anchor="middle" font-size="10" fill="#1f2328">7.68</text>
      <text x="240.2" y="311.0" text-anchor="middle" font-size="10" fill="#1f2328">3.87</text>
      <text x="289.8" y="264.6" text-anchor="middle" font-size="10" fill="#1f2328">39.6</text>
      <text x="364.2" y="310.7" text-anchor="middle" font-size="10" fill="#1f2328">4.11</text>
      <text x="413.8" y="237.5" text-anchor="middle" font-size="10" fill="#1f2328">60.4</text>
      <text x="488.2" y="310.6" text-anchor="middle" font-size="10" fill="#1f2328">4.18</text>
      <text x="537.8" y="298.1" text-anchor="middle" font-size="10" fill="#1f2328">13.8</text>
      <text x="612.2" y="296.5" text-anchor="middle" font-size="10" fill="#1f2328">15.0</text>
      <text x="661.8" y="138.1" text-anchor="middle" font-size="10" fill="#1f2328">137</text>
    </g>
    <g>
      <text x="142" y="338" text-anchor="middle" font-size="11" fill="#1f2328">tiny</text>
      <text x="266" y="338" text-anchor="middle" font-size="11" fill="#1f2328">small</text>
      <text x="390" y="338" text-anchor="middle" font-size="11" fill="#1f2328">flow</text>
      <text x="514" y="338" text-anchor="middle" font-size="11" fill="#1f2328">literal</text>
      <text x="638" y="338" text-anchor="middle" font-size="11" fill="#1f2328">big</text>
    </g>
    <g>
      <g transform="translate(239.5, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#0969da"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">wireform-yaml</text>
      </g>
      <g transform="translate(364.5, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#cf222e"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">yaml (libyaml)</text>
      </g>
    </g>
  </g>
  <g class="wf-dark">
    <rect x="0" y="0" width="720" height="400" fill="#0d1117"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#e6edf3">wireform-yaml decode vs libyaml across input sizes</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#7d8590">lower is better · µs · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5</text>
    <g stroke="#30363d" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#7d8590">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#7d8590">50.0</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#7d8590">100</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#7d8590">150</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#7d8590">200</text>
    </g>
    <g>
      <rect x="92.4" y="319.7" width="47.6" height="0.3" rx="2" fill="#58a6ff"/>
      <rect x="142" y="310.0" width="47.6" height="10.0" rx="2" fill="#ff7b72"/>
      <rect x="216.4" y="315.0" width="47.6" height="5.0" rx="2" fill="#58a6ff"/>
      <rect x="266" y="268.6" width="47.6" height="51.4" rx="2" fill="#ff7b72"/>
      <rect x="340.4" y="314.7" width="47.6" height="5.3" rx="2" fill="#58a6ff"/>
      <rect x="390" y="241.5" width="47.6" height="78.5" rx="2" fill="#ff7b72"/>
      <rect x="464.4" y="314.6" width="47.6" height="5.4" rx="2" fill="#58a6ff"/>
      <rect x="514" y="302.1" width="47.6" height="17.9" rx="2" fill="#ff7b72"/>
      <rect x="588.4" y="300.5" width="47.6" height="19.5" rx="2" fill="#58a6ff"/>
      <rect x="638" y="142.1" width="47.6" height="177.9" rx="2" fill="#ff7b72"/>
    </g>
    <g>
      <text x="116.2" y="315.7" text-anchor="middle" font-size="10" fill="#e6edf3">0.250</text>
      <text x="165.8" y="306.0" text-anchor="middle" font-size="10" fill="#e6edf3">7.68</text>
      <text x="240.2" y="311.0" text-anchor="middle" font-size="10" fill="#e6edf3">3.87</text>
      <text x="289.8" y="264.6" text-anchor="middle" font-size="10" fill="#e6edf3">39.6</text>
      <text x="364.2" y="310.7" text-anchor="middle" font-size="10" fill="#e6edf3">4.11</text>
      <text x="413.8" y="237.5" text-anchor="middle" font-size="10" fill="#e6edf3">60.4</text>
      <text x="488.2" y="310.6" text-anchor="middle" font-size="10" fill="#e6edf3">4.18</text>
      <text x="537.8" y="298.1" text-anchor="middle" font-size="10" fill="#e6edf3">13.8</text>
      <text x="612.2" y="296.5" text-anchor="middle" font-size="10" fill="#e6edf3">15.0</text>
      <text x="661.8" y="138.1" text-anchor="middle" font-size="10" fill="#e6edf3">137</text>
    </g>
    <g>
      <text x="142" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">tiny</text>
      <text x="266" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">small</text>
      <text x="390" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">flow</text>
      <text x="514" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">literal</text>
      <text x="638" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">big</text>
    </g>
    <g>
      <g transform="translate(239.5, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#58a6ff"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">wireform-yaml</text>
      </g>
      <g transform="translate(364.5, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#ff7b72"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">yaml (libyaml)</text>
      </g>
    </g>
  </g>
</svg>


| Operation | wireform-yaml | yaml (libyaml) |  ratio |
| :-------- | ------------: | -------------: | -----: |
| tiny      |       0.25 µs |        7.68 µs | 30.72x |
| small     |       3.87 µs |        39.6 µs | 10.22x |
| flow      |       4.11 µs |        60.4 µs | 14.70x |
| literal   |       4.18 µs |        13.8 µs |  3.29x |
| big       |       15.0 µs |         137 µs |  9.12x |

<sub>Last run 2026-06-27 11:56:42 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5.</sub>
<!-- END_AUTOGEN bench:yaml-decode-vs-libyaml -->

### wireform-yaml vs HsYAML (pure Haskell)

<!-- BEGIN_AUTOGEN bench:yaml-decode-vs-hsyaml -->
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 720 400" width="720" height="400" role="img" font-family="ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, &quot;Segoe UI&quot;, Helvetica, Arial, sans-serif" font-size="12">
  <title>wireform-yaml decode vs HsYAML (pure Haskell) across input sizes</title>
  <style>.wf-dark{display:none}@media (prefers-color-scheme:dark){.wf-light{display:none}.wf-dark{display:inline}}</style>
  <g class="wf-light">
    <rect x="0" y="0" width="720" height="400" fill="#ffffff"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#1f2328">wireform-yaml decode vs HsYAML (pure Haskell) across input sizes</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#656d76">lower is better · µs · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5</text>
    <g stroke="#d0d7de" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#656d76">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#656d76">1250</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#656d76">2500</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#656d76">3750</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#656d76">5000</text>
    </g>
    <g>
      <rect x="92.4" y="320.0" width="47.6" height="0.0" rx="2" fill="#0969da"/>
      <rect x="142" y="314.4" width="47.6" height="5.6" rx="2" fill="#cf222e"/>
      <rect x="216.4" y="319.8" width="47.6" height="0.2" rx="2" fill="#0969da"/>
      <rect x="266" y="265.7" width="47.6" height="54.3" rx="2" fill="#cf222e"/>
      <rect x="340.4" y="319.8" width="47.6" height="0.2" rx="2" fill="#0969da"/>
      <rect x="390" y="253.1" width="47.6" height="66.9" rx="2" fill="#cf222e"/>
      <rect x="464.4" y="319.8" width="47.6" height="0.2" rx="2" fill="#0969da"/>
      <rect x="514" y="258.9" width="47.6" height="61.1" rx="2" fill="#cf222e"/>
      <rect x="588.4" y="319.2" width="47.6" height="0.8" rx="2" fill="#0969da"/>
      <rect x="638" y="114.0" width="47.6" height="206.0" rx="2" fill="#cf222e"/>
    </g>
    <g>
      <text x="116.2" y="316.0" text-anchor="middle" font-size="10" fill="#1f2328">0.250</text>
      <text x="165.8" y="310.4" text-anchor="middle" font-size="10" fill="#1f2328">108</text>
      <text x="240.2" y="315.8" text-anchor="middle" font-size="10" fill="#1f2328">3.87</text>
      <text x="289.8" y="261.7" text-anchor="middle" font-size="10" fill="#1f2328">1044</text>
      <text x="364.2" y="315.8" text-anchor="middle" font-size="10" fill="#1f2328">4.11</text>
      <text x="413.8" y="249.1" text-anchor="middle" font-size="10" fill="#1f2328">1286</text>
      <text x="488.2" y="315.8" text-anchor="middle" font-size="10" fill="#1f2328">4.18</text>
      <text x="537.8" y="254.9" text-anchor="middle" font-size="10" fill="#1f2328">1175</text>
      <text x="612.2" y="315.2" text-anchor="middle" font-size="10" fill="#1f2328">15.0</text>
      <text x="661.8" y="110.0" text-anchor="middle" font-size="10" fill="#1f2328">3961</text>
    </g>
    <g>
      <text x="142" y="338" text-anchor="middle" font-size="11" fill="#1f2328">tiny</text>
      <text x="266" y="338" text-anchor="middle" font-size="11" fill="#1f2328">small</text>
      <text x="390" y="338" text-anchor="middle" font-size="11" fill="#1f2328">flow</text>
      <text x="514" y="338" text-anchor="middle" font-size="11" fill="#1f2328">literal</text>
      <text x="638" y="338" text-anchor="middle" font-size="11" fill="#1f2328">big</text>
    </g>
    <g>
      <g transform="translate(267.5, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#0969da"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">wireform-yaml</text>
      </g>
      <g transform="translate(392.5, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#cf222e"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">HsYAML</text>
      </g>
    </g>
  </g>
  <g class="wf-dark">
    <rect x="0" y="0" width="720" height="400" fill="#0d1117"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#e6edf3">wireform-yaml decode vs HsYAML (pure Haskell) across input sizes</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#7d8590">lower is better · µs · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5</text>
    <g stroke="#30363d" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#7d8590">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#7d8590">1250</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#7d8590">2500</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#7d8590">3750</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#7d8590">5000</text>
    </g>
    <g>
      <rect x="92.4" y="320.0" width="47.6" height="0.0" rx="2" fill="#58a6ff"/>
      <rect x="142" y="314.4" width="47.6" height="5.6" rx="2" fill="#ff7b72"/>
      <rect x="216.4" y="319.8" width="47.6" height="0.2" rx="2" fill="#58a6ff"/>
      <rect x="266" y="265.7" width="47.6" height="54.3" rx="2" fill="#ff7b72"/>
      <rect x="340.4" y="319.8" width="47.6" height="0.2" rx="2" fill="#58a6ff"/>
      <rect x="390" y="253.1" width="47.6" height="66.9" rx="2" fill="#ff7b72"/>
      <rect x="464.4" y="319.8" width="47.6" height="0.2" rx="2" fill="#58a6ff"/>
      <rect x="514" y="258.9" width="47.6" height="61.1" rx="2" fill="#ff7b72"/>
      <rect x="588.4" y="319.2" width="47.6" height="0.8" rx="2" fill="#58a6ff"/>
      <rect x="638" y="114.0" width="47.6" height="206.0" rx="2" fill="#ff7b72"/>
    </g>
    <g>
      <text x="116.2" y="316.0" text-anchor="middle" font-size="10" fill="#e6edf3">0.250</text>
      <text x="165.8" y="310.4" text-anchor="middle" font-size="10" fill="#e6edf3">108</text>
      <text x="240.2" y="315.8" text-anchor="middle" font-size="10" fill="#e6edf3">3.87</text>
      <text x="289.8" y="261.7" text-anchor="middle" font-size="10" fill="#e6edf3">1044</text>
      <text x="364.2" y="315.8" text-anchor="middle" font-size="10" fill="#e6edf3">4.11</text>
      <text x="413.8" y="249.1" text-anchor="middle" font-size="10" fill="#e6edf3">1286</text>
      <text x="488.2" y="315.8" text-anchor="middle" font-size="10" fill="#e6edf3">4.18</text>
      <text x="537.8" y="254.9" text-anchor="middle" font-size="10" fill="#e6edf3">1175</text>
      <text x="612.2" y="315.2" text-anchor="middle" font-size="10" fill="#e6edf3">15.0</text>
      <text x="661.8" y="110.0" text-anchor="middle" font-size="10" fill="#e6edf3">3961</text>
    </g>
    <g>
      <text x="142" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">tiny</text>
      <text x="266" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">small</text>
      <text x="390" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">flow</text>
      <text x="514" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">literal</text>
      <text x="638" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">big</text>
    </g>
    <g>
      <g transform="translate(267.5, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#58a6ff"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">wireform-yaml</text>
      </g>
      <g transform="translate(392.5, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#ff7b72"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">HsYAML</text>
      </g>
    </g>
  </g>
</svg>


| Operation | wireform-yaml |  HsYAML |   ratio |
| :-------- | ------------: | ------: | ------: |
| tiny      |       0.25 µs |  108 µs | 432.84x |
| small     |       3.87 µs | 1044 µs | 269.82x |
| flow      |       4.11 µs | 1286 µs | 312.92x |
| literal   |       4.18 µs | 1175 µs | 281.21x |
| big       |       15.0 µs | 3961 µs | 263.87x |

<sub>Last run 2026-06-27 11:56:42 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5.</sub>
<!-- END_AUTOGEN bench:yaml-decode-vs-hsyaml -->

The charts and tables above are regenerated by [`wireform-stats`](../stats/) from `wireform-yaml/bench-results/summary/yaml-decode-vs-{libyaml,hsyaml}.json` — the same source the README charts are built from.

## Notable modules

| Module | Role |
|--------|------|
| `YAML.Class` | `ToYAML` / `FromYAML`, `encodeYAML`, `decodeYAML` |
| `YAML.Value` | AST for mappings, sequences, scalars, anchors, and streams |
| `YAML.Encode` | Block and flow emitter with YAML 1.2 quoting rules |
| `YAML.Decode` | Parser for documents and multi-doc streams |
| `YAML.Derive` | Template Haskell deriver wired to `Wireform.Derive` annotations |
| `YAML.JSON` | Bridge between YAML values and JSON |
