---
title: wireform-xml
description: "Full XML pipeline: SAX and DOM parsing, XPath queries, XSLT transforms, XSD codegen, and Template Haskell deriving."
sidebar:
  order: 20
---

`wireform-xml` is wireform's XML 1.0 package. It covers the full lifecycle from
byte scanning through tree construction, querying, transformation, and typed
serialization. Reach for it when you need predictable, fast XML handling in
Haskell without pulling in a heavyweight foreign parser, or when you want one
library that can stream large documents, build a queryable DOM, and derive
`ToXML`/`FromXML` from your existing types.

## Key features

| Capability | Module | Why it matters |
|------------|--------|----------------|
| SAX event parser | `XML.SAX` | Constant-memory streaming over large files |
| Zero-copy DOM | `XML.FastDOM` | Sub-millisecond scans when you only need slices into the source bytes |
| Allocating tree DOM | `XML.Decode`, `XML.Value` | Mutable-free tree for XPath, XSLT, and typed decoding |
| XPath queries | `XML.Path` | Navigate and filter without hand-rolling recursive walks |
| XSLT 1.0 | `XML.XSLT` | Apply stylesheets for report generation and legacy integrations |
| XSD codegen | `XML.CodeGen`, `XML.QQ` | Generate Haskell types from schema at compile time or via CLI |
| Incremental parsing | `XML.Incremental` | Feed chunks as they arrive on a socket or from disk |
| Template Haskell deriving | `XML.Class`, `XML.Derive` | `deriveXML` with wireform-derive annotations; Generic defaults for simple cases |
| C SIMD scanner | `cbits/fast_xml.c` | Vectorized scanning on text-heavy documents |

## Basic usage

### SAX: stream events without building a tree

SAX fits pipelines where you count elements, extract a few fields, or forward
events downstream. Memory stays bounded because the parser never materializes a
full document.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.IORef (newIORef, readIORef, modifyIORef')
import Data.ByteString (ByteString)
import XML.SAX (SAXEvent(..), parseSAXStream)

countElements :: ByteString -> IO Int
countElements bs = do
  ref <- newIORef (0 :: Int)
  _ <- parseSAXStream bs $ \ev -> case ev of
    StartElement _ _ -> modifyIORef' ref (+ 1)
    _                -> pure ()
  readIORef ref
```

### DOM: parse once, query many times

When you need random access or repeated queries, build a tree with `decode` and
walk it with XPath-style helpers.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.ByteString (ByteString)
import XML.Decode (decode)
import XML.Path (queryPath, textContent)
import XML.Value (docRoot)

extractTitles :: ByteString -> Either String (Vector Text)
extractTitles bs = do
  doc <- decode bs
  let books = queryPath ["catalog", "book"] (docRoot doc)
  pure $ V.map textContent books
```

For read-only workloads where string data should stay in the original buffer,
`parseFast` from `XML.FastDOM` returns span-based nodes and avoids `Text`
allocation during the scan.

### XPath: locate nodes by path

`XML.Path` implements a practical XPath subset: child and descendant axes,
attribute predicates, indexing, and wildcards. Parse a path string once, then
reuse it across many documents.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.ByteString (ByteString)
import XML.Decode (decode)
import XML.Path (parsePath, query, textContent)
import XML.Value (docRoot)

findBySku :: ByteString -> Text -> Either String (Maybe Text)
findBySku bs sku = do
  doc <- decode bs
  path <- parsePath ("inventory/item[@sku='" <> sku <> "']/name")
  case V.uncons (query path (docRoot doc)) of
    Nothing        -> Right Nothing
    Just (node, _) -> Right (Just (textContent node))
```

### Typed records

For application-level messages, derive `ToXML` and `FromXML` with the Template
Haskell deriver and round-trip with `encodeXML` / `decodeXML`.

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import GHC.Generics (Generic)
import Data.Text (Text)
import XML.Class (ToXML, FromXML, encodeXML, decodeXML)
import XML.Derive (deriveXML)

data Book = Book
  { title  :: !Text
  , author :: !Text
  , year   :: !Int
  } deriving stock (Generic)

$(deriveXML ''Book)

roundtrip :: Book -> Either String Book
roundtrip book = decodeXML (encodeXML book)
```

For simple cases with no wire-format customization, Generic defaults also
work: add `deriving Generic` and declare empty `instance ToXML Book` and
`instance FromXML Book` declarations.

For schema-driven types, use the `[xsd| ... |]` quasiquoter or
`wireform-gen xsd` to generate modules from XSD at compile time or in CI.

## Performance

### DOM parse (medium document, ~25 KB)

<!-- BEGIN_AUTOGEN bench:dom-parse-medium -->
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 720 400" width="720" height="400" role="img" font-family="ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, &quot;Segoe UI&quot;, Helvetica, Arial, sans-serif" font-size="12">
  <title>wireform-xml DOM parse vs Hackage XML libraries (medium document)</title>
  <style>.wf-dark{display:none}@media (prefers-color-scheme:dark){.wf-light{display:none}.wf-dark{display:inline}}</style>
  <g class="wf-light">
    <rect x="0" y="0" width="720" height="400" fill="#ffffff"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#1f2328">wireform-xml DOM parse vs Hackage XML libraries (medium document)</text>
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
      <rect x="262" y="316.0" width="62" height="4.0" rx="2" fill="#0969da"/>
      <rect x="326" y="313.2" width="62" height="6.8" rx="2" fill="#cf222e"/>
      <rect x="390" y="294.8" width="62" height="25.2" rx="2" fill="#1a7f37"/>
      <rect x="454" y="108.7" width="62" height="211.3" rx="2" fill="#bf8700"/>
    </g>
    <g>
      <text x="293" y="312.0" text-anchor="middle" font-size="10" fill="#1f2328">30.7</text>
      <text x="357" y="309.2" text-anchor="middle" font-size="10" fill="#1f2328">52.0</text>
      <text x="421" y="290.8" text-anchor="middle" font-size="10" fill="#1f2328">194</text>
      <text x="485" y="104.7" text-anchor="middle" font-size="10" fill="#1f2328">1626</text>
    </g>
    <g>
      <text x="390" y="338" text-anchor="middle" font-size="11" fill="#1f2328">medium document</text>
    </g>
    <g>
      <g transform="translate(37.5, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#0969da"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">hexml (C bindings)</text>
      </g>
      <g transform="translate(197.5, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#cf222e"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">wireform-xml (FastDOM)</text>
      </g>
      <g transform="translate(385.5, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#1a7f37"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">wireform-xml (typed DOM)</text>
      </g>
      <g transform="translate(587.5, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#bf8700"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">xml-conduit</text>
      </g>
    </g>
  </g>
  <g class="wf-dark">
    <rect x="0" y="0" width="720" height="400" fill="#0d1117"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#e6edf3">wireform-xml DOM parse vs Hackage XML libraries (medium document)</text>
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
      <rect x="262" y="316.0" width="62" height="4.0" rx="2" fill="#58a6ff"/>
      <rect x="326" y="313.2" width="62" height="6.8" rx="2" fill="#ff7b72"/>
      <rect x="390" y="294.8" width="62" height="25.2" rx="2" fill="#3fb950"/>
      <rect x="454" y="108.7" width="62" height="211.3" rx="2" fill="#d29922"/>
    </g>
    <g>
      <text x="293" y="312.0" text-anchor="middle" font-size="10" fill="#e6edf3">30.7</text>
      <text x="357" y="309.2" text-anchor="middle" font-size="10" fill="#e6edf3">52.0</text>
      <text x="421" y="290.8" text-anchor="middle" font-size="10" fill="#e6edf3">194</text>
      <text x="485" y="104.7" text-anchor="middle" font-size="10" fill="#e6edf3">1626</text>
    </g>
    <g>
      <text x="390" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">medium document</text>
    </g>
    <g>
      <g transform="translate(37.5, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#58a6ff"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">hexml (C bindings)</text>
      </g>
      <g transform="translate(197.5, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#ff7b72"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">wireform-xml (FastDOM)</text>
      </g>
      <g transform="translate(385.5, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#3fb950"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">wireform-xml (typed DOM)</text>
      </g>
      <g transform="translate(587.5, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#d29922"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">xml-conduit</text>
      </g>
    </g>
  </g>
</svg>


| Operation       | hexml (C bindings) | wireform-xml (FastDOM) | wireform-xml (typed DOM) | xml-conduit | ratio |
| :-------------- | -----------------: | ---------------------: | -----------------------: | ----------: | ----: |
| medium document |            30.7 µs |                52.0 µs |                   194 µs |     1626 µs | 0.27x |

<sub>Last run 2026-06-27 11:56:42 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5.</sub>
<!-- END_AUTOGEN bench:dom-parse-medium -->

### SAX parse (medium document)

<!-- BEGIN_AUTOGEN bench:sax-parse-medium -->
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 720 400" width="720" height="400" role="img" font-family="ui-sans-serif, system-ui, -apple-system, BlinkMacSystemFont, &quot;Segoe UI&quot;, Helvetica, Arial, sans-serif" font-size="12">
  <title>wireform-xml SAX parse vs xeno (medium document)</title>
  <style>.wf-dark{display:none}@media (prefers-color-scheme:dark){.wf-light{display:none}.wf-dark{display:inline}}</style>
  <g class="wf-light">
    <rect x="0" y="0" width="720" height="400" fill="#ffffff"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#1f2328">wireform-xml SAX parse vs xeno (medium document)</text>
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
      <rect x="326" y="260.5" width="62" height="59.5" rx="2" fill="#0969da"/>
      <rect x="390" y="126.0" width="62" height="194.0" rx="2" fill="#cf222e"/>
    </g>
    <g>
      <text x="357" y="256.5" text-anchor="middle" font-size="10" fill="#1f2328">45.8</text>
      <text x="421" y="122.0" text-anchor="middle" font-size="10" fill="#1f2328">149</text>
    </g>
    <g>
      <text x="390" y="338" text-anchor="middle" font-size="11" fill="#1f2328">medium document</text>
    </g>
    <g>
      <g transform="translate(278, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#0969da"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">xeno</text>
      </g>
      <g transform="translate(340, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#cf222e"/>
        <text x="18" y="1" font-size="11" fill="#1f2328">wireform-xml</text>
      </g>
    </g>
  </g>
  <g class="wf-dark">
    <rect x="0" y="0" width="720" height="400" fill="#0d1117"/>
    <text x="360" y="26" text-anchor="middle" font-size="15" font-weight="600" fill="#e6edf3">wireform-xml SAX parse vs xeno (medium document)</text>
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
      <rect x="326" y="260.5" width="62" height="59.5" rx="2" fill="#58a6ff"/>
      <rect x="390" y="126.0" width="62" height="194.0" rx="2" fill="#ff7b72"/>
    </g>
    <g>
      <text x="357" y="256.5" text-anchor="middle" font-size="10" fill="#e6edf3">45.8</text>
      <text x="421" y="122.0" text-anchor="middle" font-size="10" fill="#e6edf3">149</text>
    </g>
    <g>
      <text x="390" y="338" text-anchor="middle" font-size="11" fill="#e6edf3">medium document</text>
    </g>
    <g>
      <g transform="translate(278, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#58a6ff"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">xeno</text>
      </g>
      <g transform="translate(340, 382)">
        <rect x="0" y="-9" width="12" height="12" rx="2" fill="#ff7b72"/>
        <text x="18" y="1" font-size="11" fill="#e6edf3">wireform-xml</text>
      </g>
    </g>
  </g>
</svg>


| Operation       |    xeno | wireform-xml | ratio |
| :-------------- | ------: | -----------: | ----: |
| medium document | 45.8 µs |       149 µs | 1.00x |

<sub>Last run 2026-06-27 11:56:42 UTC. ghc-9.8.4 on darwin-aarch64, criterion 1.6.5.</sub>
<!-- END_AUTOGEN bench:sax-parse-medium -->

wireform-xml's FastDOM is within 2x of hexml (which wraps the C pugixml library) and 30x faster than xml-conduit. The typed DOM trades some speed for a richer, fully-materialised tree. SAX parsing is slower than xeno's hand-tuned pull parser, but wireform-xml's SAX path builds a richer event stream with namespace handling.

The charts and tables above are regenerated by [`wireform-stats`](../stats/) from `wireform-xml/bench-results/summary/{dom,sax}-parse-medium.json` — the same source the README charts are built from.

## Notable modules

| Module | Role |
|--------|------|
| `XML.SAX` | Event parser with `parseSAX`, `parseSAXStream`, and `foldSAX` |
| `XML.FastDOM` | Zero-copy DOM with span-based string access |
| `XML.Decode` | SAX-to-DOM builder producing `XML.Value.Document` |
| `XML.Path` | XPath-lite cursor API over `Node` values |
| `XML.XSLT` | XSLT 1.0 stylesheet application |
| `XML.CodeGen` / `XML.QQ` | XSD-to-Haskell codegen (CLI and Template Haskell) |
| `XML.Incremental` | Chunk-fed parser for concurrent or streaming input |
| `XML.Class` / `XML.Derive` | `ToXML` / `FromXML` typeclasses and TH deriver |
| `XML.JSON` | Bridge between XML values and JSON for tooling |
