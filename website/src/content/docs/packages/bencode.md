---
title: wireform-bencode
description: "BitTorrent Bencode encoding and decoding with TH deriving, sorted dictionary keys, and wireform-derive annotations."
sidebar:
  order: 15
---

`wireform-bencode` implements Bencode, the encoding used by BitTorrent for
`.torrent` files, DHT messages, and peer wire protocols. Bencode supports
byte strings, integers, lists, and dictionaries with a deliberately small
grammar. Use this package when you parse or produce BitTorrent metadata,
validate info hashes, or implement peer-facing tooling that must match the
on-wire Bencode layout exactly.

## Key features

- **Template Haskell deriving** via `deriveBencode` from `Bencode.Derive`, with
  `wireform-derive` annotations; Generic defaults (empty instances) work for
  simple uncustomized records
- **Sorted dictionary keys** enforced on encode and validated on decode, as
  required by BEP-3 for stable info hashes
- **Simple wire grammar** of strings, integers, lists, and dictionaries
- **Dynamic values** via the untyped `Value` ADT for `.torrent` inspection
- **Direct encoding** for buffer-oriented writes

## Basic usage

Model a metadata record and derive Bencode codecs. Record fields encode as
dictionary keys (field names as byte strings):

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module TorrentInfo where

import Bencode.Class (ToBencode, FromBencode, encodeBencode, decodeBencode)
import Bencode.Derive (deriveBencode)
import GHC.Generics (Generic)
import Data.Text (Text)

data FileInfo = FileInfo
  { fileLength :: !Int
  , filePath   :: !Text
  }
  deriving stock (Show, Eq, Generic)

data Info = Info
  { infoName     :: !Text
  , infoPieceLen :: !Int
  , infoFiles    :: ![FileInfo]
  }
  deriving stock (Show, Eq, Generic)

$(deriveBencode ''FileInfo)
$(deriveBencode ''Info)

encodeInfo :: Info -> ByteString
encodeInfo info = encodeBencode info

decodeInfo :: ByteString -> Either String Info
decodeInfo bs = decodeBencode bs
```

For simple records with no custom wire naming, Generic defaults also work:
declare empty `instance ToBencode FileInfo` / `FromBencode FileInfo` (and the
same for `Info`) after `deriving stock (Show, Eq, Generic)`. Field names go to
the wire verbatim and annotations are not supported.

The encoder sorts dictionary keys by raw byte order before writing. You can
pass key/value pairs in any order; the wire output is always canonical for
hashing:

```haskell
import Bencode.Encoding (dictFromList, int, encodingToByteString)
import Data.ByteString.Char8 qualified as BS8

canonicalDict :: ByteString
canonicalDict =
  encodingToByteString
    ( dictFromList
        [ (BS8.pack "zebra", int 1)
        , (BS8.pack "alpha", int 2)
        ]
    )
```

For ad hoc `.torrent` parsing, use the dynamic ADT and walk the structure:

```haskell
import Bencode.Value qualified as B
import Bencode.Decode (decode)
import Data.ByteString.Char8 qualified as BS8
import Data.Vector qualified as V

infoLength :: B.Value -> Maybe Integer
infoLength val =
  case val of
    B.BDict pairs ->
      case V.find ((== BS8.pack "length") . fst) (V.toList pairs) of
        Just (_, B.BInteger n) -> Just n
        _                      -> Nothing
    _ -> Nothing
```

## Performance

### Encode/decode

<!-- BEGIN_AUTOGEN bench:bencode-encode-decode -->
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 720 400" width="720" height="400" role="img" font-family="ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, &quot;Segoe UI&quot;, Helvetica, Arial, sans-serif" font-size="12">
  <title>wireform-bencode encode + decode (TorrentInfo record)</title>
  <style>.wf-dark{display:none}@media (prefers-color-scheme:dark){.wf-light{display:none}.wf-dark{display:inline}}</style>
  <g class="wf-light">
    <rect x="0" y="0" width="720" height="400" fill="#ffffff"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#1f2328">wireform-bencode encode + decode (TorrentInfo record)</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#656d76">lower is better · ns · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5</text>
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
      <rect x="171" y="317.5" width="62" height="2.5" rx="2" fill="#0969da"/>
      <rect x="235" y="314.9" width="62" height="5.1" rx="2" fill="#cf222e"/>
      <rect x="481" y="235.7" width="62" height="84.3" rx="2" fill="#0969da"/>
      <rect x="545" y="104.4" width="62" height="215.6" rx="2" fill="#cf222e"/>
    </g>
    <g>
      <text x="202" y="313.5" text-anchor="middle" font-size="10" fill="#1f2328">953</text>
      <text x="266" y="310.9" text-anchor="middle" font-size="10" fill="#1f2328">1946</text>
      <text x="512" y="231.7" text-anchor="middle" font-size="10" fill="#1f2328">32422</text>
      <text x="576" y="100.4" text-anchor="middle" font-size="10" fill="#1f2328">82927</text>
    </g>
    <g>
      <text x="235" y="338" text-anchor="middle" font-size="11" fill="#1f2328">single-file metainfo</text>
      <text x="545" y="338" text-anchor="middle" font-size="11" fill="#1f2328">100-file metainfo</text>
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
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#e6edf3">wireform-bencode encode + decode (TorrentInfo record)</text>
    <text x="360" y="44" text-anchor="middle" font-size="11" fill="#7d8590">lower is better · ns · ghc-9.8.4 on darwin-aarch64, criterion 1.6.5</text>
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
      <rect x="171" y="317.5" width="62" height="2.5" rx="2" fill="#58a6ff"/>
      <rect x="235" y="314.9" width="62" height="5.1" rx="2" fill="#ff7b72"/>
      <rect x="481" y="235.7" width="62" height="84.3" rx="2" fill="#58a6ff"/>
      <rect x="545" y="104.4" width="62" height="215.6" rx="2" fill="#ff7b72"/>
    </g>
    <g>
      <text x="202" y="313.5" text-anchor="middle" font-size="10" fill="#e6edf3">953</text>
      <text x="266" y="310.9" text-anchor="middle" font-size="10" fill="#e6edf3">1946</text>
      <text x="512" y="231.7" text-anchor="middle" font-size="10" fill="#e6edf3">32422</text>
      <text x="576" y="100.4" text-anchor="middle" font-size="10" fill="#e6edf3">82927</text>
    </g>
    <g>
      <text x="235" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">single-file metainfo</text>
      <text x="545" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">100-file metainfo</text>
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


| Operation            |   encode |   decode | ratio |
| :------------------- | -------: | -------: | ----: |
| single-file metainfo |   953 ns |  1946 ns | 2.04x |
| 100-file metainfo    | 32422 ns | 82927 ns | 2.56x |

<sub>Last run 2026-06-27 11:24:28 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5.</sub>
<!-- END_AUTOGEN bench:bencode-encode-decode -->

Bencode is a simple text-ish format (integers as decimal strings, byte strings length-prefixed). Encode is allocation-lean; decode is dominated by dictionary key sorting.

The chart and table above are regenerated by [`wireform-stats`](../stats/) from `wireform-bencode/bench-results/summary/bencode-encode-decode.json` — the same source the README chart is built from.

## Notable modules

| Module | Purpose |
|--------|---------|
| `Bencode.Class` | `ToBencode` / `FromBencode`, `encodeBencode`, `decodeBencode` |
| `Bencode.Encode` / `Bencode.Decode` | Low-level encode and decode with sorted-key enforcement |
| `Bencode.Encoding` | Composable encoding builder for dictionaries and lists |
| `Bencode.Value` | Dynamic untyped `Value` ADT |
| `Bencode.Derive` | Template Haskell deriver with `wireform-derive` annotations |
