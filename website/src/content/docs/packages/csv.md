---
title: wireform-csv
description: "CSV, TSV, and pipe-separated encoding and decoding with TH deriving, streaming rows, and SIMD scanning."
sidebar:
  order: 24
---

`wireform-csv` handles delimiter-separated tabular data in Haskell. Spreadsheet
exports, log pipelines, and ETL jobs often arrive as CSV or TSV; this package
parses them with RFC 4180 semantics, configurable delimiters, and
SIMD-accelerated byte scanning. Derive `ToCSV`/`FromCSV` for typed rows, or use
the streaming API when files are too large to load at once.

## Key features

| Capability | Why it matters |
|------------|----------------|
| `deriveCSV` Template Haskell deriver | Map header rows to Haskell records with `wireform-derive` annotations; Generic defaults work for simple cases |
| Configurable delimiters | CSV (`,`), TSV (`\t`), pipe, or custom separators |
| Quoting and escaping | RFC 4180 quoted fields with embedded delimiters |
| Streaming row callbacks | `decodeStream` processes one row at a time with constant memory |
| SIMD newline and delimiter scan | `Wireform.FFI.findByteBS` accelerates field boundaries |
| Header row handling | Skip or capture the first row via `CSVConfig` |

## Basic usage

### Typed rows

Define a record, derive codecs with the Template Haskell deriver, and decode
an entire file into a `Vector` of rows.

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.ByteString (ByteString)
import CSV.Class (ToCSV, FromCSV)
import CSV.Derive (deriveCSV)
import CSV.Decode (decodeRecords)
import CSV.Encode (encodeRecords)
import CSV.Value (defaultCSV)

data Row = Row
  { name  :: !Text
  , email :: !Text
  , score :: !Int
  } deriving stock (Generic)

$(deriveCSV ''Row)

loadRows :: ByteString -> Either String (Vector Row)
loadRows bs = decodeRecords defaultCSV bs
```

For simple cases with no wire-format customization, Generic defaults also
work: add `deriving Generic` and declare empty `instance ToCSV Row` and
`instance FromCSV Row` declarations.

Use `defaultTSV` from `CSV.Value` when the input is tab-separated.

### Custom delimiter configuration

Build a `CSVConfig` when the file uses non-standard separators or omits a
header row.

```haskell
import CSV.Value (CSVConfig(..), CSVDocument(..), defaultCSV)
import CSV.Decode (decode)

pipeConfig :: CSVConfig
pipeConfig = defaultCSV
  { csvDelimiter = '|'
  , csvHasHeader = True
  }

parsePipeFile :: ByteString -> Either String CSVDocument
parsePipeFile = decode pipeConfig
```

### Streaming decode

For large inputs, `decodeStream` invokes a callback per row instead of
allocating a vector of the entire file.

```haskell
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import CSV.Decode (decodeStream)
import CSV.Value (defaultCSV)

streamRows :: ByteString -> (Vector Text -> IO ()) -> IO (Either String ())
streamRows bs handleRow = decodeStream defaultCSV bs handleRow

printEachRow :: ByteString -> IO ()
printEachRow bs =
  void $ streamRows bs $ \row ->
    print (V.toList row)
```

Each callback receives a `Vector Text` of fields for one row. Combine with
`fromCSVRow` inside the callback when you want typed values row by row.

## Performance

### Encode/decode

<!-- BEGIN_AUTOGEN bench:csv-encode-decode -->
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 720 400" width="720" height="400" role="img" font-family="ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, &quot;Segoe UI&quot;, Helvetica, Arial, sans-serif" font-size="12">
  <title>wireform-csv encode + decode (Sale record)</title>
  <style>.wf-dark{display:none}@media (prefers-color-scheme:dark){.wf-light{display:none}.wf-dark{display:inline}}</style>
  <g class="wf-light">
    <rect x="0" y="0" width="720" height="400" fill="#ffffff"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#1f2328">wireform-csv encode + decode (Sale record)</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#656d76">lower is better · µs · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5</text>
    <g stroke="#d0d7de" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#656d76">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#656d76">500</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#656d76">1000</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#656d76">1500</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#d0d7de" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#656d76">2000</text>
    </g>
    <g>
      <rect x="171" y="319.3" width="62" height="0.7" rx="2" fill="#0969da"/>
      <rect x="235" y="318.6" width="62" height="1.4" rx="2" fill="#cf222e"/>
      <rect x="481" y="231.0" width="62" height="89.0" rx="2" fill="#0969da"/>
      <rect x="545" y="133.9" width="62" height="186.1" rx="2" fill="#cf222e"/>
    </g>
    <g>
      <text x="202" y="315.3" text-anchor="middle" font-size="10" fill="#1f2328">5.20</text>
      <text x="266" y="314.6" text-anchor="middle" font-size="10" fill="#1f2328">10.8</text>
      <text x="512" y="227.0" text-anchor="middle" font-size="10" fill="#1f2328">685</text>
      <text x="576" y="129.9" text-anchor="middle" font-size="10" fill="#1f2328">1432</text>
    </g>
    <g>
      <text x="235" y="338" text-anchor="middle" font-size="11" fill="#1f2328">10 rows</text>
      <text x="545" y="338" text-anchor="middle" font-size="11" fill="#1f2328">1000 rows</text>
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
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#e6edf3">wireform-csv encode + decode (Sale record)</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#7d8590">lower is better · µs · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5</text>
    <g stroke="#30363d" stroke-width="1">
      <line x1="80" y1="320" x2="700" y2="320"/>
      <line x1="80" y1="60" x2="80" y2="320"/>
    </g>
    <g>
      <g/>
      <text x="72" y="324" text-anchor="end" font-size="10" fill="#7d8590">0</text>
      <line x1="80" y1="255" x2="700" y2="255" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="259" text-anchor="end" font-size="10" fill="#7d8590">500</text>
      <line x1="80" y1="190" x2="700" y2="190" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="194" text-anchor="end" font-size="10" fill="#7d8590">1000</text>
      <line x1="80" y1="125" x2="700" y2="125" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="129" text-anchor="end" font-size="10" fill="#7d8590">1500</text>
      <line x1="80" y1="60" x2="700" y2="60" stroke="#30363d" stroke-width="1" stroke-dasharray="2 3"/>
      <text x="72" y="64" text-anchor="end" font-size="10" fill="#7d8590">2000</text>
    </g>
    <g>
      <rect x="171" y="319.3" width="62" height="0.7" rx="2" fill="#58a6ff"/>
      <rect x="235" y="318.6" width="62" height="1.4" rx="2" fill="#ff7b72"/>
      <rect x="481" y="231.0" width="62" height="89.0" rx="2" fill="#58a6ff"/>
      <rect x="545" y="133.9" width="62" height="186.1" rx="2" fill="#ff7b72"/>
    </g>
    <g>
      <text x="202" y="315.3" text-anchor="middle" font-size="10" fill="#e6edf3">5.20</text>
      <text x="266" y="314.6" text-anchor="middle" font-size="10" fill="#e6edf3">10.8</text>
      <text x="512" y="227.0" text-anchor="middle" font-size="10" fill="#e6edf3">685</text>
      <text x="576" y="129.9" text-anchor="middle" font-size="10" fill="#e6edf3">1432</text>
    </g>
    <g>
      <text x="235" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">10 rows</text>
      <text x="545" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">1000 rows</text>
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


| Operation |  encode |  decode | ratio |
| :-------- | ------: | ------: | ----: |
| 10 rows   | 5.20 µs | 10.8 µs | 2.08x |
| 1000 rows |  685 µs | 1432 µs | 2.09x |

<sub>Last run 2026-06-27 11:35:54 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5.</sub>
<!-- END_AUTOGEN bench:csv-encode-decode -->

The chart and table above are regenerated by [`wireform-stats`](../stats/) from `wireform-csv/bench-results/summary/csv-encode-decode.json` — the same source the README chart is built from.

## Notable modules

| Module | Role |
|--------|------|
| `CSV.Class` | `ToCSV` / `FromCSV` and Generic helpers |
| `CSV.Value` | `CSVDocument`, `CSVConfig`, `defaultCSV`, `defaultTSV` |
| `CSV.Decode` | `decode`, `decodeStream`, `decodeRecords` |
| `CSV.Encode` | `encode`, `encodeRecords` |
| `CSV.Derive` | Template Haskell deriver |
