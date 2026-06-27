---
title: wireform-ndjson
description: "Newline-delimited JSON framing on aeson with streaming decode, concurrent processing, and SIMD newline scanning."
sidebar:
  order: 25
---

`wireform-ndjson` adds newline-delimited JSON framing on top of aeson. Log
aggregators, analytics pipelines, and many HTTP streaming APIs emit one JSON
value per line; this package splits those lines efficiently, parses each with
aeson, and exposes both batch and streaming APIs so memory stays flat on large
inputs. Use it when you already model records with `ToJSON`/`FromJSON` and only
need the NDJSON container format.

## Key features

| Capability | Why it matters |
|------------|----------------|
| NDJSON framing on aeson | Reuse existing JSON instances without a second schema |
| Streaming decode | `decodeStream` calls back per line with bounded memory |
| Concurrent producer/consumer | `decodeConcurrent` parses and dispatches across a `TBQueue` |
| SIMD newline scanning | `Wireform.FFI.findByteBS` finds `\n` in 16-byte chunks |
| Typed batch helpers | `decodeRecords` and `encodeRecords` for `Vector` workflows |

## Basic usage

### Encode a batch of records

Each value becomes one JSON object followed by a newline. The encoder uses
`Wireform.Builder` to avoid unnecessary intermediate buffers.

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import NDJSON.Encode (encodeRecords)

data Event = Event
  { eventId   :: !Int
  , eventName :: !Text
  } deriving stock (Generic, ToJSON)

writeLog :: Vector Event -> ByteString
writeLog = encodeRecords
```

### Stream decode with a callback

When lines arrive from a socket or a file read loop, `decodeStream` parses one
line at a time and invokes your handler. Empty lines are skipped.

```haskell
import Data.ByteString (ByteString)
import NDJSON.Decode (decodeStream)
import qualified Data.Aeson as Aeson

processLines :: ByteString -> IO (Either String ())
processLines bs =
  decodeStream bs $ \val -> do
    case Aeson.fromJSON val of
      Aeson.Success ev -> handleEvent (ev :: Event)
      Aeson.Error err  -> print err
```

### Batch decode into typed records

When the entire blob fits in memory, `decodeRecords` returns a `Vector` of
parsed rows in one pass.

```haskell
import Data.ByteString (ByteString)
import Data.Vector (Vector)
import NDJSON.Decode (decodeRecords)

loadEvents :: ByteString -> Either String (Vector Event)
loadEvents = decodeRecords
```

### Concurrent parsing

`decodeConcurrent` runs a producer thread that scans newlines and enqueues
parsed `Aeson.Value` values into a `TBQueue`, while the same call processes
each value through your callback on the consumer side. Pass the queue depth to
control how many parsed lines can buffer between producer and consumer.

```haskell
import Data.ByteString (ByteString)
import NDJSON.Decode (decodeConcurrent)
import qualified Data.Aeson as Aeson

processConcurrent :: ByteString -> Int -> IO (Either String ())
processConcurrent bs queueDepth =
  decodeConcurrent bs queueDepth $ \val -> do
    case Aeson.fromJSON val of
      Aeson.Success ev -> handleEvent (ev :: Event)
      Aeson.Error err  -> print err
```

For untyped pipelines, `NDJSON.Decode.decode` returns `Vector Aeson.Value`, and
`NDJSON.Encode.encode` accepts the same.

## Performance

### wireform-ndjson vs aeson + manual line splitting

<!-- BEGIN_AUTOGEN bench:ndjson-vs-aeson-lines -->
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 720 400" width="720" height="400" role="img" font-family="ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, &quot;Segoe UI&quot;, Helvetica, Arial, sans-serif" font-size="12">
  <title>wireform-ndjson vs aeson + manual newline splitting</title>
  <style>.wf-dark{display:none}@media (prefers-color-scheme:dark){.wf-light{display:none}.wf-dark{display:inline}}</style>
  <g class="wf-light">
    <rect x="0" y="0" width="720" height="400" fill="#ffffff"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#1f2328">wireform-ndjson vs aeson + manual newline splitting</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#656d76">lower is better · µs · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5. Today the SIMD newline scanner doesn't yet outperform `BS.split '\n'` on these inputs; both paths are within 10%.</text>
    <g stroke="#d0d7de" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#656d76">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#656d76">250</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#656d76">500</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#656d76">750</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#656d76">1000</text>
    </g>
    <g>
      <rect x="95.5" y="318.7" width="60" height="1.3" rx="2" fill="#0969da"/>
      <rect x="157.5" y="318.7" width="60" height="1.3" rx="2" fill="#cf222e"/>
      <rect x="250.5" y="185.3" width="60" height="134.7" rx="2" fill="#0969da"/>
      <rect x="312.5" y="187.9" width="60" height="132.1" rx="2" fill="#cf222e"/>
      <rect x="405.5" y="319.0" width="60" height="1.0" rx="2" fill="#0969da"/>
      <rect x="467.5" y="319.0" width="60" height="1.0" rx="2" fill="#cf222e"/>
      <rect x="560.5" y="209.2" width="60" height="110.8" rx="2" fill="#0969da"/>
      <rect x="622.5" y="220.6" width="60" height="99.4" rx="2" fill="#cf222e"/>
    </g>
    <g>
      <text x="125.5" y="314.7" text-anchor="middle" font-size="10" fill="#1f2328">5.18</text>
      <text x="187.5" y="314.7" text-anchor="middle" font-size="10" fill="#1f2328">4.95</text>
      <text x="280.5" y="181.3" text-anchor="middle" font-size="10" fill="#1f2328">518</text>
      <text x="342.5" y="183.9" text-anchor="middle" font-size="10" fill="#1f2328">508</text>
      <text x="435.5" y="315.0" text-anchor="middle" font-size="10" fill="#1f2328">3.85</text>
      <text x="497.5" y="315.0" text-anchor="middle" font-size="10" fill="#1f2328">3.84</text>
      <text x="590.5" y="205.2" text-anchor="middle" font-size="10" fill="#1f2328">426</text>
      <text x="652.5" y="216.6" text-anchor="middle" font-size="10" fill="#1f2328">382</text>
    </g>
    <g>
      <text x="157.5" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode 10 rows</text>
      <text x="312.5" y="338" text-anchor="middle" font-size="11" fill="#1f2328">encode 1000 rows</text>
      <text x="467.5" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode 10 rows</text>
      <text x="622.5" y="338" text-anchor="middle" font-size="11" fill="#1f2328">decode 1000 rows</text>
    </g>
    <g>
      <g transform="translate(236, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#0969da"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">wireform-ndjson</text>
      </g>
      <g transform="translate(375, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#cf222e"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">aeson + lines</text>
      </g>
    </g>
  </g>
  <g class="wf-dark">
    <rect x="0" y="0" width="720" height="400" fill="#0d1117"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#e6edf3">wireform-ndjson vs aeson + manual newline splitting</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#7d8590">lower is better · µs · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5. Today the SIMD newline scanner doesn't yet outperform `BS.split '\n'` on these inputs; both paths are within 10%.</text>
    <g stroke="#30363d" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#7d8590">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#7d8590">250</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#7d8590">500</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#7d8590">750</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#7d8590">1000</text>
    </g>
    <g>
      <rect x="95.5" y="318.7" width="60" height="1.3" rx="2" fill="#58a6ff"/>
      <rect x="157.5" y="318.7" width="60" height="1.3" rx="2" fill="#ff7b72"/>
      <rect x="250.5" y="185.3" width="60" height="134.7" rx="2" fill="#58a6ff"/>
      <rect x="312.5" y="187.9" width="60" height="132.1" rx="2" fill="#ff7b72"/>
      <rect x="405.5" y="319.0" width="60" height="1.0" rx="2" fill="#58a6ff"/>
      <rect x="467.5" y="319.0" width="60" height="1.0" rx="2" fill="#ff7b72"/>
      <rect x="560.5" y="209.2" width="60" height="110.8" rx="2" fill="#58a6ff"/>
      <rect x="622.5" y="220.6" width="60" height="99.4" rx="2" fill="#ff7b72"/>
    </g>
    <g>
      <text x="125.5" y="314.7" text-anchor="middle" font-size="10" fill="#e6edf3">5.18</text>
      <text x="187.5" y="314.7" text-anchor="middle" font-size="10" fill="#e6edf3">4.95</text>
      <text x="280.5" y="181.3" text-anchor="middle" font-size="10" fill="#e6edf3">518</text>
      <text x="342.5" y="183.9" text-anchor="middle" font-size="10" fill="#e6edf3">508</text>
      <text x="435.5" y="315.0" text-anchor="middle" font-size="10" fill="#e6edf3">3.85</text>
      <text x="497.5" y="315.0" text-anchor="middle" font-size="10" fill="#e6edf3">3.84</text>
      <text x="590.5" y="205.2" text-anchor="middle" font-size="10" fill="#e6edf3">426</text>
      <text x="652.5" y="216.6" text-anchor="middle" font-size="10" fill="#e6edf3">382</text>
    </g>
    <g>
      <text x="157.5" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode 10 rows</text>
      <text x="312.5" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">encode 1000 rows</text>
      <text x="467.5" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode 10 rows</text>
      <text x="622.5" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">decode 1000 rows</text>
    </g>
    <g>
      <g transform="translate(236, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#58a6ff"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">wireform-ndjson</text>
      </g>
      <g transform="translate(375, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#ff7b72"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">aeson + lines</text>
      </g>
    </g>
  </g>
</svg>


| Operation        | wireform-ndjson | aeson + lines |  ratio |
| :--------------- | --------------: | ------------: | -----: |
| encode 10 rows   |         5.18 µs |       4.95 µs |  0.96x |
| encode 1000 rows |          518 µs |        508 µs |  0.98x |
| decode 10 rows   |         3.85 µs |       3.84 µs | 0.100x |
| decode 1000 rows |          426 µs |        382 µs |  0.90x |

<sub>Last run 2026-06-27 11:35:55 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5. Today the SIMD newline scanner doesn't yet outperform `BS.split '\n'` on these inputs; both paths are within 10%..</sub>
<!-- END_AUTOGEN bench:ndjson-vs-aeson-lines -->

Performance is within ~10% of raw aeson with manual newline splitting. wireform-ndjson's value is in the typed API and proper line-framing semantics, not raw speed.

The chart and table above are regenerated by [`wireform-stats`](../stats/) from `wireform-ndjson/bench-results/summary/ndjson-vs-aeson-lines.json` — the same source the README chart is built from.

## Notable modules

| Module | Role |
|--------|------|
| `NDJSON.Decode` | `decode`, `decodeStream`, `decodeRecords`, `decodeConcurrent` |
| `NDJSON.Encode` | `encode`, `encodeRecords` |
| `NDJSON.Derive` | Helpers aligned with the wireform deriver ecosystem |
