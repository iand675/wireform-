---
title: wireform-arrow
description: "Apache Arrow IPC with schema framing, record batch encode/decode, typed record APIs, table projection, and optional zstd/lz4 compression."
sidebar:
  order: 41
---

`wireform-arrow` implements Apache Arrow IPC and the Arrow columnar data
model in Haskell. Arrow is the interchange format between analytics engines,
dataframe libraries, and columnar storage. Use this package when you need
typed record batches, schema-aware encoding, or a shared column vocabulary
that Parquet and ORC readers in wireform can target.

## Key features

- **Schema framing** and IPC message encode/decode via `Arrow.IPC`
- **Record batch** encode and decode for in-memory columnar data
- **Typed record API** with Template Haskell and Generic support
- **Table projection and subsetting** to read only the columns you need
- **Optional compression** (Zstd and LZ4 behind Cabal flags)
- **SIMD buffer validation** for record batch integrity checks
- **Streaming reader** for framed IPC streams in `Arrow.Stream`

## Basic usage

Define a record type, build a `Table`, encode a vector of records to an
Arrow IPC stream with `Arrow.Stream`, and decode it back:

```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Trades where

import Arrow.Record
import Arrow.Stream (decodeArrowStream, defaultWriteOptions, encodeArrowStream)
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Vector as V

data Trade = Trade
  { tradeSym  :: !Text
  , tradeQty  :: !Int32
  , tradeNote :: !(Maybe Text)
  }
  deriving stock (Show, Eq)

tradeTable :: Table Trade
tradeTable = table enc dec
  where
    enc =
      fieldE "sym"  tradeSym  utf8E
        <> fieldE "qty"  tradeQty  int32E
        <> fieldE "note" tradeNote (nullable utf8E)
    dec =
      Trade
        <$> columnD "sym"  utf8D
        <*> columnD "qty"  int32D
        <*> columnD "note" (nullableD utf8D)

-- | One record batch in an IPC stream. 'Left' when the columns do not
-- fit the schema (they always do for a 'Table').
encodeTrades :: V.Vector Trade -> Either String ByteString
encodeTrades trades =
  let (schema, columns) = encodeTable tradeTable trades
  in encodeArrowStream defaultWriteOptions schema [columns]

-- | Every record of every batch in the stream.
decodeTrades :: ByteString -> Either String (V.Vector Trade)
decodeTrades bytes = do
  (schema, batches) <- decodeArrowStream bytes
  V.concat <$> traverse (decodeTable tradeTable schema) batches

roundTripTrades :: V.Vector Trade -> Either String (V.Vector Trade)
roundTripTrades trades = encodeTrades trades >>= decodeTrades
```

`encodeArrowFile` / `decodeArrowFile` do the same for the Arrow file
format. The writers check every batch against the schema and return
`Left` for a mismatch, or for dictionaries they cannot write (an
emit-once dictionary larger than its index type can address).

Project column batches down to a subset of fields when the full schema is
larger than what your query needs:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Arrow.Column (ColumnArray)
import Arrow.Record (projectTable)
import Arrow.Types (Schema)
import qualified Data.Vector as V

projectSymQty :: Schema -> V.Vector ColumnArray -> Maybe (Schema, V.Vector ColumnArray)
projectSymQty schema cols =
  projectTable ["sym", "qty"] schema cols
```

`Arrow.File` reads the file format and exposes the raw record batches;
`Arrow.Stream.openStreamReader` reads a stream one batch at a time.

## Notable modules

| Module | Purpose |
|--------|---------|
| `Arrow.Types` | Schema, field, and buffer types; `schemaFingerprint` |
| `Arrow.Column` | Column array builders and validators |
| `Arrow.Record` | Typed `Table`, `Encoder`, `Decoder`, projection helpers |
| `Arrow.Record.Generic` / `Arrow.Record.TH` | Generic and TH record derivation |
| `Arrow.Derive` | Annotation-driven deriver |
| `Arrow.IPC` | IPC message framing encode/decode |
| `Arrow.Stream` | Stream and file encode / decode (with dictionaries, including nested ones), pull-based streaming reader |
| `Arrow.File` | Arrow file format readers |
| `Arrow.FlatBufferIPC` | FlatBuffer-backed IPC metadata path |
| `Arrow.Write` | `writeArrowStream` / `writeArrowFile`, column encoders, `validateColumns` |

## Compression

Enable Zstd or LZ4 with Cabal flags (`+zstd`, `+lz4`). Compressed IPC
streams follow the standard Arrow body compression layout; uncompressed
IPC remains the default for maximum interoperability.

## Performance

One record batch per workload through `Arrow.Stream`, set against
[arrow-rs](https://crates.io/crates/arrow) 58 building the same batches
from the same generators: `StreamWriter` into a `Vec<u8>` for encode,
`StreamReader` with its default validation on for decode. Inputs are
built outside the timed region. The ratio column is wireform-arrow time
over arrow-rs time, so `2.00x` means wireform-arrow takes twice as long.
Run both, sequentially, with
`python3 scripts/run-benchmarks.py --only arrow`; the harnesses are
`wireform-arrow/bench/Bench.hs` and
`interop/arrow-rs/benches/arrow_ipc.rs`.

### Encode/decode, 100k rows

<!-- BEGIN_AUTOGEN bench:arrow-encode-decode -->
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 720 400" width="720" height="400" role="img" font-family="ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, &quot;Segoe UI&quot;, Helvetica, Arial, sans-serif" font-size="12">
  <title>wireform-arrow IPC stream encode + decode (100k rows)</title>
  <style>.wf-dark{display:none}@media (prefers-color-scheme:dark){.wf-light{display:none}.wf-dark{display:inline}}</style>
  <g class="wf-light">
    <rect x="0" y="0" width="720" height="400" fill="#ffffff"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#1f2328">wireform-arrow IPC stream encode + decode (100k rows)</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#656d76">lower is better · µs · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5</text>
    <g stroke="#d0d7de" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#656d76">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#656d76">12500</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#656d76">25000</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#656d76">37500</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#656d76">50000</text>
    </g>
    <g>
      <rect x="86.9" y="311.8" width="25.6" height="8.2" rx="2" fill="#0969da"/>
      <rect x="114.4" y="319.2" width="25.6" height="0.8" rx="2" fill="#cf222e"/>
      <rect x="155.8" y="311.1" width="25.6" height="8.9" rx="2" fill="#0969da"/>
      <rect x="183.3" y="319.0" width="25.6" height="1.0" rx="2" fill="#cf222e"/>
      <rect x="224.7" y="304.4" width="25.6" height="15.6" rx="2" fill="#0969da"/>
      <rect x="252.2" y="289.0" width="25.6" height="31.0" rx="2" fill="#cf222e"/>
      <rect x="293.6" y="257.5" width="25.6" height="62.5" rx="2" fill="#0969da"/>
      <rect x="321.1" y="284.9" width="25.6" height="35.1" rx="2" fill="#cf222e"/>
      <rect x="362.4" y="239.8" width="25.6" height="80.2" rx="2" fill="#0969da"/>
      <rect x="390" y="275.0" width="25.6" height="45.0" rx="2" fill="#cf222e"/>
      <rect x="431.3" y="121.1" width="25.6" height="198.9" rx="2" fill="#0969da"/>
      <rect x="458.9" y="164.0" width="25.6" height="156.0" rx="2" fill="#cf222e"/>
      <rect x="500.2" y="240.2" width="25.6" height="79.8" rx="2" fill="#0969da"/>
      <rect x="527.8" y="313.7" width="25.6" height="6.3" rx="2" fill="#cf222e"/>
      <rect x="569.1" y="300.2" width="25.6" height="19.8" rx="2" fill="#0969da"/>
      <rect x="596.7" y="314.1" width="25.6" height="5.9" rx="2" fill="#cf222e"/>
      <rect x="638" y="311.5" width="25.6" height="8.5" rx="2" fill="#0969da"/>
      <rect x="665.6" y="318.6" width="25.6" height="1.4" rx="2" fill="#cf222e"/>
    </g>
    <g>
      <text x="99.7" y="307.8" text-anchor="middle" font-size="10" fill="#1f2328">1571</text>
      <text x="127.2" y="315.2" text-anchor="middle" font-size="10" fill="#1f2328">153</text>
      <text x="168.6" y="307.1" text-anchor="middle" font-size="10" fill="#1f2328">1707</text>
      <text x="196.1" y="315.0" text-anchor="middle" font-size="10" fill="#1f2328">189</text>
      <text x="237.4" y="300.4" text-anchor="middle" font-size="10" fill="#1f2328">3007</text>
      <text x="265" y="285.0" text-anchor="middle" font-size="10" fill="#1f2328">5963</text>
      <text x="306.3" y="253.5" text-anchor="middle" font-size="10" fill="#1f2328">12022</text>
      <text x="333.9" y="280.9" text-anchor="middle" font-size="10" fill="#1f2328">6756</text>
      <text x="375.2" y="235.8" text-anchor="middle" font-size="10" fill="#1f2328">15433</text>
      <text x="402.8" y="271.0" text-anchor="middle" font-size="10" fill="#1f2328">8658</text>
      <text x="444.1" y="117.1" text-anchor="middle" font-size="10" fill="#1f2328">38249</text>
      <text x="471.7" y="160.0" text-anchor="middle" font-size="10" fill="#1f2328">29993</text>
      <text x="513" y="236.2" text-anchor="middle" font-size="10" fill="#1f2328">15348</text>
      <text x="540.6" y="309.7" text-anchor="middle" font-size="10" fill="#1f2328">1206</text>
      <text x="581.9" y="296.2" text-anchor="middle" font-size="10" fill="#1f2328">3810</text>
      <text x="609.4" y="310.1" text-anchor="middle" font-size="10" fill="#1f2328">1133</text>
      <text x="650.8" y="307.5" text-anchor="middle" font-size="10" fill="#1f2328">1631</text>
      <text x="678.3" y="314.6" text-anchor="middle" font-size="10" fill="#1f2328">273</text>
    </g>
    <g>
      <text x="114.4" y="338" text-anchor="middle" font-size="11" fill="#1f2328">int64</text>
      <text x="183.3" y="338" text-anchor="middle" font-size="11" fill="#1f2328">double</text>
      <text x="252.2" y="338" text-anchor="middle" font-size="11" fill="#1f2328">nullable int64</text>
      <text x="321.1" y="338" text-anchor="middle" font-size="11" fill="#1f2328">utf8</text>
      <text x="390" y="338" text-anchor="middle" font-size="11" fill="#1f2328">nullable utf8</text>
      <text x="458.9" y="338" text-anchor="middle" font-size="11" fill="#1f2328">mixed 6-col</text>
      <text x="527.8" y="338" text-anchor="middle" font-size="11" fill="#1f2328">list&lt;int32&gt;</text>
      <text x="596.7" y="338" text-anchor="middle" font-size="11" fill="#1f2328">struct&lt;int32,double,bool&gt;</text>
      <text x="665.6" y="338" text-anchor="middle" font-size="11" fill="#1f2328">dictionary&lt;utf8&gt;</text>
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
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#e6edf3">wireform-arrow IPC stream encode + decode (100k rows)</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#7d8590">lower is better · µs · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5</text>
    <g stroke="#30363d" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#7d8590">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#7d8590">12500</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#7d8590">25000</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#7d8590">37500</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#7d8590">50000</text>
    </g>
    <g>
      <rect x="86.9" y="311.8" width="25.6" height="8.2" rx="2" fill="#58a6ff"/>
      <rect x="114.4" y="319.2" width="25.6" height="0.8" rx="2" fill="#ff7b72"/>
      <rect x="155.8" y="311.1" width="25.6" height="8.9" rx="2" fill="#58a6ff"/>
      <rect x="183.3" y="319.0" width="25.6" height="1.0" rx="2" fill="#ff7b72"/>
      <rect x="224.7" y="304.4" width="25.6" height="15.6" rx="2" fill="#58a6ff"/>
      <rect x="252.2" y="289.0" width="25.6" height="31.0" rx="2" fill="#ff7b72"/>
      <rect x="293.6" y="257.5" width="25.6" height="62.5" rx="2" fill="#58a6ff"/>
      <rect x="321.1" y="284.9" width="25.6" height="35.1" rx="2" fill="#ff7b72"/>
      <rect x="362.4" y="239.8" width="25.6" height="80.2" rx="2" fill="#58a6ff"/>
      <rect x="390" y="275.0" width="25.6" height="45.0" rx="2" fill="#ff7b72"/>
      <rect x="431.3" y="121.1" width="25.6" height="198.9" rx="2" fill="#58a6ff"/>
      <rect x="458.9" y="164.0" width="25.6" height="156.0" rx="2" fill="#ff7b72"/>
      <rect x="500.2" y="240.2" width="25.6" height="79.8" rx="2" fill="#58a6ff"/>
      <rect x="527.8" y="313.7" width="25.6" height="6.3" rx="2" fill="#ff7b72"/>
      <rect x="569.1" y="300.2" width="25.6" height="19.8" rx="2" fill="#58a6ff"/>
      <rect x="596.7" y="314.1" width="25.6" height="5.9" rx="2" fill="#ff7b72"/>
      <rect x="638" y="311.5" width="25.6" height="8.5" rx="2" fill="#58a6ff"/>
      <rect x="665.6" y="318.6" width="25.6" height="1.4" rx="2" fill="#ff7b72"/>
    </g>
    <g>
      <text x="99.7" y="307.8" text-anchor="middle" font-size="10" fill="#e6edf3">1571</text>
      <text x="127.2" y="315.2" text-anchor="middle" font-size="10" fill="#e6edf3">153</text>
      <text x="168.6" y="307.1" text-anchor="middle" font-size="10" fill="#e6edf3">1707</text>
      <text x="196.1" y="315.0" text-anchor="middle" font-size="10" fill="#e6edf3">189</text>
      <text x="237.4" y="300.4" text-anchor="middle" font-size="10" fill="#e6edf3">3007</text>
      <text x="265" y="285.0" text-anchor="middle" font-size="10" fill="#e6edf3">5963</text>
      <text x="306.3" y="253.5" text-anchor="middle" font-size="10" fill="#e6edf3">12022</text>
      <text x="333.9" y="280.9" text-anchor="middle" font-size="10" fill="#e6edf3">6756</text>
      <text x="375.2" y="235.8" text-anchor="middle" font-size="10" fill="#e6edf3">15433</text>
      <text x="402.8" y="271.0" text-anchor="middle" font-size="10" fill="#e6edf3">8658</text>
      <text x="444.1" y="117.1" text-anchor="middle" font-size="10" fill="#e6edf3">38249</text>
      <text x="471.7" y="160.0" text-anchor="middle" font-size="10" fill="#e6edf3">29993</text>
      <text x="513" y="236.2" text-anchor="middle" font-size="10" fill="#e6edf3">15348</text>
      <text x="540.6" y="309.7" text-anchor="middle" font-size="10" fill="#e6edf3">1206</text>
      <text x="581.9" y="296.2" text-anchor="middle" font-size="10" fill="#e6edf3">3810</text>
      <text x="609.4" y="310.1" text-anchor="middle" font-size="10" fill="#e6edf3">1133</text>
      <text x="650.8" y="307.5" text-anchor="middle" font-size="10" fill="#e6edf3">1631</text>
      <text x="678.3" y="314.6" text-anchor="middle" font-size="10" fill="#e6edf3">273</text>
    </g>
    <g>
      <text x="114.4" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">int64</text>
      <text x="183.3" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">double</text>
      <text x="252.2" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">nullable int64</text>
      <text x="321.1" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">utf8</text>
      <text x="390" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">nullable utf8</text>
      <text x="458.9" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">mixed 6-col</text>
      <text x="527.8" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">list&lt;int32&gt;</text>
      <text x="596.7" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">struct&lt;int32,double,bool&gt;</text>
      <text x="665.6" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">dictionary&lt;utf8&gt;</text>
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


| Operation                 |   encode |   decode | ratio |
| :------------------------ | -------: | -------: | ----: |
| int64                     |  1571 µs |   153 µs | 0.10x |
| double                    |  1707 µs |   189 µs | 0.11x |
| nullable int64            |  3007 µs |  5963 µs | 1.98x |
| utf8                      | 12022 µs |  6756 µs | 0.56x |
| nullable utf8             | 15433 µs |  8658 µs | 0.56x |
| mixed 6-col               | 38249 µs | 29993 µs | 0.78x |
| list<int32>               | 15348 µs |  1206 µs | 0.08x |
| struct<int32,double,bool> |  3810 µs |  1133 µs | 0.30x |
| dictionary<utf8>          |  1631 µs |   273 µs | 0.17x |

<sub>Last run 2026-10-07 23:32:59 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5.</sub>
<!-- END_AUTOGEN bench:arrow-encode-decode -->

### Encode/decode, 100-row batch

<!-- BEGIN_AUTOGEN bench:arrow-encode-decode-small -->
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 720 400" width="720" height="400" role="img" font-family="ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, &quot;Segoe UI&quot;, Helvetica, Arial, sans-serif" font-size="12">
  <title>wireform-arrow IPC stream encode + decode (100-row batch)</title>
  <style>.wf-dark{display:none}@media (prefers-color-scheme:dark){.wf-light{display:none}.wf-dark{display:inline}}</style>
  <g class="wf-light">
    <rect x="0" y="0" width="720" height="400" fill="#ffffff"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#1f2328">wireform-arrow IPC stream encode + decode (100-row batch)</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#656d76">lower is better · µs · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5</text>
    <g stroke="#d0d7de" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#656d76">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#656d76">5.00</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#656d76">10.0</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#656d76">15.0</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#656d76">20.0</text>
    </g>
    <g>
      <rect x="86.9" y="267.2" width="25.6" height="52.8" rx="2" fill="#0969da"/>
      <rect x="114.4" y="310.1" width="25.6" height="9.9" rx="2" fill="#cf222e"/>
      <rect x="155.8" y="265.8" width="25.6" height="54.2" rx="2" fill="#0969da"/>
      <rect x="183.3" y="309.7" width="25.6" height="10.3" rx="2" fill="#cf222e"/>
      <rect x="224.7" y="252.9" width="25.6" height="67.1" rx="2" fill="#0969da"/>
      <rect x="252.2" y="295.3" width="25.6" height="24.7" rx="2" fill="#cf222e"/>
      <rect x="293.6" y="244.0" width="25.6" height="76.0" rx="2" fill="#0969da"/>
      <rect x="321.1" y="268.9" width="25.6" height="51.1" rx="2" fill="#cf222e"/>
      <rect x="362.4" y="227.4" width="25.6" height="92.6" rx="2" fill="#0969da"/>
      <rect x="390" y="266.3" width="25.6" height="53.7" rx="2" fill="#cf222e"/>
      <rect x="431.3" y="69.1" width="25.6" height="250.9" rx="2" fill="#0969da"/>
      <rect x="458.9" y="169.7" width="25.6" height="150.3" rx="2" fill="#cf222e"/>
      <rect x="500.2" y="234.2" width="25.6" height="85.8" rx="2" fill="#0969da"/>
      <rect x="527.8" y="292.6" width="25.6" height="27.4" rx="2" fill="#cf222e"/>
      <rect x="569.1" y="204.3" width="25.6" height="115.7" rx="2" fill="#0969da"/>
      <rect x="596.7" y="292.2" width="25.6" height="27.8" rx="2" fill="#cf222e"/>
      <rect x="638" y="217.8" width="25.6" height="102.2" rx="2" fill="#0969da"/>
      <rect x="665.6" y="294.8" width="25.6" height="25.2" rx="2" fill="#cf222e"/>
    </g>
    <g>
      <text x="99.7" y="263.2" text-anchor="middle" font-size="10" fill="#1f2328">4.06</text>
      <text x="127.2" y="306.1" text-anchor="middle" font-size="10" fill="#1f2328">0.760</text>
      <text x="168.6" y="261.8" text-anchor="middle" font-size="10" fill="#1f2328">4.17</text>
      <text x="196.1" y="305.7" text-anchor="middle" font-size="10" fill="#1f2328">0.790</text>
      <text x="237.4" y="248.9" text-anchor="middle" font-size="10" fill="#1f2328">5.16</text>
      <text x="265" y="291.3" text-anchor="middle" font-size="10" fill="#1f2328">1.90</text>
      <text x="306.3" y="240.0" text-anchor="middle" font-size="10" fill="#1f2328">5.85</text>
      <text x="333.9" y="264.9" text-anchor="middle" font-size="10" fill="#1f2328">3.93</text>
      <text x="375.2" y="223.4" text-anchor="middle" font-size="10" fill="#1f2328">7.12</text>
      <text x="402.8" y="262.3" text-anchor="middle" font-size="10" fill="#1f2328">4.13</text>
      <text x="444.1" y="65.1" text-anchor="middle" font-size="10" fill="#1f2328">19.3</text>
      <text x="471.7" y="165.7" text-anchor="middle" font-size="10" fill="#1f2328">11.6</text>
      <text x="513" y="230.2" text-anchor="middle" font-size="10" fill="#1f2328">6.60</text>
      <text x="540.6" y="288.6" text-anchor="middle" font-size="10" fill="#1f2328">2.11</text>
      <text x="581.9" y="200.3" text-anchor="middle" font-size="10" fill="#1f2328">8.90</text>
      <text x="609.4" y="288.2" text-anchor="middle" font-size="10" fill="#1f2328">2.14</text>
      <text x="650.8" y="213.8" text-anchor="middle" font-size="10" fill="#1f2328">7.86</text>
      <text x="678.3" y="290.8" text-anchor="middle" font-size="10" fill="#1f2328">1.94</text>
    </g>
    <g>
      <text x="114.4" y="338" text-anchor="middle" font-size="11" fill="#1f2328">int64</text>
      <text x="183.3" y="338" text-anchor="middle" font-size="11" fill="#1f2328">double</text>
      <text x="252.2" y="338" text-anchor="middle" font-size="11" fill="#1f2328">nullable int64</text>
      <text x="321.1" y="338" text-anchor="middle" font-size="11" fill="#1f2328">utf8</text>
      <text x="390" y="338" text-anchor="middle" font-size="11" fill="#1f2328">nullable utf8</text>
      <text x="458.9" y="338" text-anchor="middle" font-size="11" fill="#1f2328">mixed 6-col</text>
      <text x="527.8" y="338" text-anchor="middle" font-size="11" fill="#1f2328">list&lt;int32&gt;</text>
      <text x="596.7" y="338" text-anchor="middle" font-size="11" fill="#1f2328">struct&lt;int32,double,bool&gt;</text>
      <text x="665.6" y="338" text-anchor="middle" font-size="11" fill="#1f2328">dictionary&lt;utf8&gt;</text>
    </g>
    <g>
      <g transform="translate(215, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#0969da"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">encode (100 rows)</text>
      </g>
      <g transform="translate(368, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#cf222e"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">decode (100 rows)</text>
      </g>
    </g>
  </g>
  <g class="wf-dark">
    <rect x="0" y="0" width="720" height="400" fill="#0d1117"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#e6edf3">wireform-arrow IPC stream encode + decode (100-row batch)</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#7d8590">lower is better · µs · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5</text>
    <g stroke="#30363d" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#7d8590">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#7d8590">5.00</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#7d8590">10.0</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#7d8590">15.0</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#7d8590">20.0</text>
    </g>
    <g>
      <rect x="86.9" y="267.2" width="25.6" height="52.8" rx="2" fill="#58a6ff"/>
      <rect x="114.4" y="310.1" width="25.6" height="9.9" rx="2" fill="#ff7b72"/>
      <rect x="155.8" y="265.8" width="25.6" height="54.2" rx="2" fill="#58a6ff"/>
      <rect x="183.3" y="309.7" width="25.6" height="10.3" rx="2" fill="#ff7b72"/>
      <rect x="224.7" y="252.9" width="25.6" height="67.1" rx="2" fill="#58a6ff"/>
      <rect x="252.2" y="295.3" width="25.6" height="24.7" rx="2" fill="#ff7b72"/>
      <rect x="293.6" y="244.0" width="25.6" height="76.0" rx="2" fill="#58a6ff"/>
      <rect x="321.1" y="268.9" width="25.6" height="51.1" rx="2" fill="#ff7b72"/>
      <rect x="362.4" y="227.4" width="25.6" height="92.6" rx="2" fill="#58a6ff"/>
      <rect x="390" y="266.3" width="25.6" height="53.7" rx="2" fill="#ff7b72"/>
      <rect x="431.3" y="69.1" width="25.6" height="250.9" rx="2" fill="#58a6ff"/>
      <rect x="458.9" y="169.7" width="25.6" height="150.3" rx="2" fill="#ff7b72"/>
      <rect x="500.2" y="234.2" width="25.6" height="85.8" rx="2" fill="#58a6ff"/>
      <rect x="527.8" y="292.6" width="25.6" height="27.4" rx="2" fill="#ff7b72"/>
      <rect x="569.1" y="204.3" width="25.6" height="115.7" rx="2" fill="#58a6ff"/>
      <rect x="596.7" y="292.2" width="25.6" height="27.8" rx="2" fill="#ff7b72"/>
      <rect x="638" y="217.8" width="25.6" height="102.2" rx="2" fill="#58a6ff"/>
      <rect x="665.6" y="294.8" width="25.6" height="25.2" rx="2" fill="#ff7b72"/>
    </g>
    <g>
      <text x="99.7" y="263.2" text-anchor="middle" font-size="10" fill="#e6edf3">4.06</text>
      <text x="127.2" y="306.1" text-anchor="middle" font-size="10" fill="#e6edf3">0.760</text>
      <text x="168.6" y="261.8" text-anchor="middle" font-size="10" fill="#e6edf3">4.17</text>
      <text x="196.1" y="305.7" text-anchor="middle" font-size="10" fill="#e6edf3">0.790</text>
      <text x="237.4" y="248.9" text-anchor="middle" font-size="10" fill="#e6edf3">5.16</text>
      <text x="265" y="291.3" text-anchor="middle" font-size="10" fill="#e6edf3">1.90</text>
      <text x="306.3" y="240.0" text-anchor="middle" font-size="10" fill="#e6edf3">5.85</text>
      <text x="333.9" y="264.9" text-anchor="middle" font-size="10" fill="#e6edf3">3.93</text>
      <text x="375.2" y="223.4" text-anchor="middle" font-size="10" fill="#e6edf3">7.12</text>
      <text x="402.8" y="262.3" text-anchor="middle" font-size="10" fill="#e6edf3">4.13</text>
      <text x="444.1" y="65.1" text-anchor="middle" font-size="10" fill="#e6edf3">19.3</text>
      <text x="471.7" y="165.7" text-anchor="middle" font-size="10" fill="#e6edf3">11.6</text>
      <text x="513" y="230.2" text-anchor="middle" font-size="10" fill="#e6edf3">6.60</text>
      <text x="540.6" y="288.6" text-anchor="middle" font-size="10" fill="#e6edf3">2.11</text>
      <text x="581.9" y="200.3" text-anchor="middle" font-size="10" fill="#e6edf3">8.90</text>
      <text x="609.4" y="288.2" text-anchor="middle" font-size="10" fill="#e6edf3">2.14</text>
      <text x="650.8" y="213.8" text-anchor="middle" font-size="10" fill="#e6edf3">7.86</text>
      <text x="678.3" y="290.8" text-anchor="middle" font-size="10" fill="#e6edf3">1.94</text>
    </g>
    <g>
      <text x="114.4" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">int64</text>
      <text x="183.3" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">double</text>
      <text x="252.2" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">nullable int64</text>
      <text x="321.1" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">utf8</text>
      <text x="390" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">nullable utf8</text>
      <text x="458.9" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">mixed 6-col</text>
      <text x="527.8" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">list&lt;int32&gt;</text>
      <text x="596.7" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">struct&lt;int32,double,bool&gt;</text>
      <text x="665.6" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">dictionary&lt;utf8&gt;</text>
    </g>
    <g>
      <g transform="translate(215, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#58a6ff"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">encode (100 rows)</text>
      </g>
      <g transform="translate(368, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#ff7b72"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">decode (100 rows)</text>
      </g>
    </g>
  </g>
</svg>


| Operation                 | encode (100 rows) | decode (100 rows) | ratio |
| :------------------------ | ----------------: | ----------------: | ----: |
| int64                     |           4.06 µs |           0.76 µs | 0.19x |
| double                    |           4.17 µs |           0.79 µs | 0.19x |
| nullable int64            |           5.16 µs |           1.90 µs | 0.37x |
| utf8                      |           5.85 µs |           3.93 µs | 0.67x |
| nullable utf8             |           7.12 µs |           4.13 µs | 0.58x |
| mixed 6-col               |           19.3 µs |           11.6 µs | 0.60x |
| list<int32>               |           6.60 µs |           2.11 µs | 0.32x |
| struct<int32,double,bool> |           8.90 µs |           2.14 µs | 0.24x |
| dictionary<utf8>          |           7.86 µs |           1.94 µs | 0.25x |

<sub>Last run 2026-10-07 23:32:59 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5.</sub>
<!-- END_AUTOGEN bench:arrow-encode-decode-small -->

### Entry points, mixed 6-column table

<!-- BEGIN_AUTOGEN bench:arrow-api-paths -->
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 720 400" width="720" height="400" role="img" font-family="ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, &quot;Segoe UI&quot;, Helvetica, Arial, sans-serif" font-size="12">
  <title>wireform-arrow entry points, mixed 6-column table (100k rows)</title>
  <style>.wf-dark{display:none}@media (prefers-color-scheme:dark){.wf-light{display:none}.wf-dark{display:inline}}</style>
  <g class="wf-light">
    <rect x="0" y="0" width="720" height="400" fill="#ffffff"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#1f2328">wireform-arrow entry points, mixed 6-column table (100k rows)</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#656d76">lower is better · µs · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5</text>
    <g stroke="#d0d7de" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#656d76">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#656d76">25000</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#656d76">50000</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#656d76">75000</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#656d76">100000</text>
    </g>
    <g>
      <rect x="87.8" y="223.4" width="60" height="96.6" rx="2" fill="#0969da"/>
      <rect x="165.2" y="221.8" width="60" height="98.2" rx="2" fill="#0969da"/>
      <rect x="242.8" y="247.7" width="60" height="72.3" rx="2" fill="#0969da"/>
      <rect x="320.2" y="224.7" width="60" height="95.3" rx="2" fill="#0969da"/>
      <rect x="397.8" y="246.7" width="60" height="73.3" rx="2" fill="#0969da"/>
      <rect x="475.2" y="244.4" width="60" height="75.6" rx="2" fill="#0969da"/>
      <rect x="552.8" y="195.1" width="60" height="124.9" rx="2" fill="#0969da"/>
      <rect x="630.2" y="160.6" width="60" height="159.4" rx="2" fill="#0969da"/>
    </g>
    <g>
      <text x="117.8" y="219.4" text-anchor="middle" font-size="10" fill="#1f2328">37139</text>
      <text x="195.2" y="217.8" text-anchor="middle" font-size="10" fill="#1f2328">37771</text>
      <text x="272.8" y="243.7" text-anchor="middle" font-size="10" fill="#1f2328">27818</text>
      <text x="350.2" y="220.7" text-anchor="middle" font-size="10" fill="#1f2328">36653</text>
      <text x="427.8" y="242.7" text-anchor="middle" font-size="10" fill="#1f2328">28210</text>
      <text x="505.2" y="240.4" text-anchor="middle" font-size="10" fill="#1f2328">29074</text>
      <text x="582.8" y="191.1" text-anchor="middle" font-size="10" fill="#1f2328">48048</text>
      <text x="660.2" y="156.6" text-anchor="middle" font-size="10" fill="#1f2328">61321</text>
    </g>
    <g>
      <text x="118.8" y="338" text-anchor="middle" font-size="11" fill="#1f2328">stream encode (Arrow.Stream)</text>
      <text x="196.2" y="338" text-anchor="middle" font-size="11" fill="#1f2328">stream encode (Arrow.Write)</text>
      <text x="273.8" y="338" text-anchor="middle" font-size="11" fill="#1f2328">stream decode (Arrow.Stream)</text>
      <text x="351.2" y="338" text-anchor="middle" font-size="11" fill="#1f2328">file encode (Arrow.Stream)</text>
      <text x="428.8" y="338" text-anchor="middle" font-size="11" fill="#1f2328">file decode (Arrow.Stream)</text>
      <text x="506.2" y="338" text-anchor="middle" font-size="11" fill="#1f2328">file read (Arrow.File)</text>
      <text x="583.8" y="338" text-anchor="middle" font-size="11" fill="#1f2328">typed encode (Arrow.Record)</text>
      <text x="661.2" y="338" text-anchor="middle" font-size="11" fill="#1f2328">typed decode (Arrow.Record)</text>
    </g>
    <g>
      <g transform="translate(312.5, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#0969da"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">mixed 6-col</text>
      </g>
    </g>
  </g>
  <g class="wf-dark">
    <rect x="0" y="0" width="720" height="400" fill="#0d1117"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#e6edf3">wireform-arrow entry points, mixed 6-column table (100k rows)</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#7d8590">lower is better · µs · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5</text>
    <g stroke="#30363d" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#7d8590">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#7d8590">25000</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#7d8590">50000</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#7d8590">75000</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#7d8590">100000</text>
    </g>
    <g>
      <rect x="87.8" y="223.4" width="60" height="96.6" rx="2" fill="#58a6ff"/>
      <rect x="165.2" y="221.8" width="60" height="98.2" rx="2" fill="#58a6ff"/>
      <rect x="242.8" y="247.7" width="60" height="72.3" rx="2" fill="#58a6ff"/>
      <rect x="320.2" y="224.7" width="60" height="95.3" rx="2" fill="#58a6ff"/>
      <rect x="397.8" y="246.7" width="60" height="73.3" rx="2" fill="#58a6ff"/>
      <rect x="475.2" y="244.4" width="60" height="75.6" rx="2" fill="#58a6ff"/>
      <rect x="552.8" y="195.1" width="60" height="124.9" rx="2" fill="#58a6ff"/>
      <rect x="630.2" y="160.6" width="60" height="159.4" rx="2" fill="#58a6ff"/>
    </g>
    <g>
      <text x="117.8" y="219.4" text-anchor="middle" font-size="10" fill="#e6edf3">37139</text>
      <text x="195.2" y="217.8" text-anchor="middle" font-size="10" fill="#e6edf3">37771</text>
      <text x="272.8" y="243.7" text-anchor="middle" font-size="10" fill="#e6edf3">27818</text>
      <text x="350.2" y="220.7" text-anchor="middle" font-size="10" fill="#e6edf3">36653</text>
      <text x="427.8" y="242.7" text-anchor="middle" font-size="10" fill="#e6edf3">28210</text>
      <text x="505.2" y="240.4" text-anchor="middle" font-size="10" fill="#e6edf3">29074</text>
      <text x="582.8" y="191.1" text-anchor="middle" font-size="10" fill="#e6edf3">48048</text>
      <text x="660.2" y="156.6" text-anchor="middle" font-size="10" fill="#e6edf3">61321</text>
    </g>
    <g>
      <text x="118.8" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">stream encode (Arrow.Stream)</text>
      <text x="196.2" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">stream encode (Arrow.Write)</text>
      <text x="273.8" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">stream decode (Arrow.Stream)</text>
      <text x="351.2" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">file encode (Arrow.Stream)</text>
      <text x="428.8" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">file decode (Arrow.Stream)</text>
      <text x="506.2" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">file read (Arrow.File)</text>
      <text x="583.8" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">typed encode (Arrow.Record)</text>
      <text x="661.2" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">typed decode (Arrow.Record)</text>
    </g>
    <g>
      <g transform="translate(312.5, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#58a6ff"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">mixed 6-col</text>
      </g>
    </g>
  </g>
</svg>


| Operation                    | mixed 6-col | ratio |
| :--------------------------- | ----------: | ----: |
| stream encode (Arrow.Stream) |    37139 µs |     - |
| stream encode (Arrow.Write)  |    37771 µs |     - |
| stream decode (Arrow.Stream) |    27818 µs |     - |
| file encode (Arrow.Stream)   |    36653 µs |     - |
| file decode (Arrow.Stream)   |    28210 µs |     - |
| file read (Arrow.File)       |    29074 µs |     - |
| typed encode (Arrow.Record)  |    48048 µs |     - |
| typed decode (Arrow.Record)  |    61321 µs |     - |

<sub>Last run 2026-10-07 23:32:59 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5.</sub>
<!-- END_AUTOGEN bench:arrow-api-paths -->

The entry-point rows map to the closest arrow-rs path: `StreamWriter`
for both stream writers, `FileReader` for both file readers, and for the
typed rows a `Vec` of row structs turned into one array per field (or
read back into owned row structs), since arrow-rs has no record
deriving. Comparisons against pyarrow and arrow-cpp are not yet
measured.
