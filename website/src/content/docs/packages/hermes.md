---
title: hermes
description: "The canonical home for HTTP header parsing and rendering in the wireform monorepo: a KnownHeader codec for every IANA-registered header field, the IANA registries, foundational IETF value parsers, and shared wire-grammar primitives."
sidebar:
  order: 57
---

`hermes` is the foundation library for HTTP wire-format grammar in this
monorepo. Its focus is **HTTP header parsing and rendering**, and it is the
*canonical home* for that grammar: when the wire grammar of an HTTP construct
needs to change, the change goes in `hermes`, not in a downstream
[`wireform-http`](../http/) or [`wireform-grpc`](../grpc/) module.

Named after the messenger of the Greek gods, hermes models each header as a
`KnownHeader` instance carrying a parser, a renderer, and a cardinality, so
callers parse and render headers through one consistent surface instead of
hand-rolling `BS.split` / `BS.break` ad-hoc parsers that drift from the RFCs.

## Coverage

Every IANA-registered HTTP header field has a typed codec, plus the widely
deployed de-facto fields that are not in the registry. As of this writing that
is **241 `KnownHeader` instances** across the full permanent, provisional,
deprecated, and obsoleted IANA categories, plus 19 de-facto fields
(`X-Forwarded-*`, `X-Request-ID`, `DNT`, `Save-Data`, …).

Headers are grouped into families, each in its own module
`Network.HTTP.Headers.<Name>`:

| Family | Representative fields |
|---|---|
| **Content negotiation** | `Accept`, `Accept-{Charset,Encoding,Language,Patch,Post,Datetime}`, `Vary`, `Content-{Type,Encoding,Language,Location,Disposition}`, `MIME-Version`, the RFC 2295 transparent-negotiation set |
| **Conditionals & validators** | `ETag`, `If-{Match,NoneMatch,ModifiedSince,UnmodifiedSince,Range}`, `Last-Modified`, `Date`, `Range`, `Accept-Ranges`, `Content-Range` |
| **Authentication** | `Authorization`, `WWW-Authenticate`, `Proxy-{Authenticate,Authorization}`, `Authentication-Info`, `Client-Cert` / `Cert-Not-{Before,After}`, `DPoP`, `Replay-Nonce`, `OSCORE`, token binding |
| **Caching** | `Cache-Control`, `Age`, `Expires`, `Cache-Status`, `CDN-{Cache-Control,Loop}`, `Surrogate-{Capability,Control}`, `Pragma`, `Warning` |
| **Cookies** | `Cookie`, `Set-Cookie`, `Clear-Site-Data`, and the obsolete `Cookie2` / `Set-Cookie2` |
| **CORS & cross-origin isolation** | `Access-Control-*`, `Origin`, `Cross-Origin-{Embedder,Opener,Resource}-Policy` (+ report-only), `Origin-Agent-Cluster` |
| **Security** | `Content-Security-Policy` (+ report-only), `Permissions-Policy`, `Strict-Transport-Security`, `X-{Content-Type-Options,Frame-Options,XSS-Protection,…}`, `Public-Key-Pins`, `Expect-CT`, `DNT` |
| **Message framing & routing** | `Connection`, `TE`, `Trailer`, `Upgrade`, `Via`, `Max-Forwards`, `Transfer-Encoding`, `Content-Length`, `Forwarded`, `X-Forwarded-*`, `X-Real-IP`, `Proxy-Status`, `Host` |
| **Body integrity** | `Content-Digest`, `Repr-Digest`, `Want-{Content,Repr}-Digest`, `Signature`, `Signature-Input`, `Accept-Signature`, and the obsolete `Digest` / `Content-MD5` |
| **WebSocket handshake** | `Sec-WebSocket-{Key,Accept,Protocol,Version,Extensions}`, `ALPN` |
| **Observability** | `traceparent`, `tracestate`, `Server-Timing`, `Timing-Allow-Origin`, `NEL`, `Reporting-Endpoints`, `X-{Request-ID,Correlation-ID,Trace-ID,Request-Start}` |
| **Web push** | `TTL`, `Topic`, `Urgency` |
| **WebDAV / CalDAV / OData** | `DAV`, `Depth`, `Destination`, `If`, `Lock-Token`, `Overwrite`, `Timeout`, `Schedule-{Reply,Tag}`, `OData-{Version,MaxVersion,EntityId,…}`, `Prefer`, `Preference-Applied`, `Repeatability-*` |
| **Client hints & privacy** | `Accept-CH`, `Sec-Purpose`, `Sec-GPC`, `Save-Data` |
| **Server / client identity** | `Server`, `User-Agent`, `From`, `Referer`, `Location`, `Sunset`, `Retry-After`, `Allow`, `Link`, `Alt-Svc`, `Alt-Used`, `Priority`, `Expect`, `Early-Data`, `Refresh`, `Last-Event-ID` |
| **Historic / obsolete** | `P3P`, `PEP`, `Man` / `Opt`, `Protocol-*`, `Method-Check`, and the rest of the obsoleted registry, captured faithfully for legacy interop |

Depth is chosen per header: a **structured parser/renderer** where the grammar
is live and useful (CORS, CSP, `Forwarded`, `Link`, `Alt-Svc`, `Priority`,
digests, signatures, trace context, WebDAV, …), and a **faithful
raw-preserving newtype** where the value is obsolete or host-opaque, so the
field round-trips without fabricating a dead grammar.

## Foundational IETF value parsers

Several headers compose smaller IETF value grammars, exposed as their own
modules so the rest of the stack can reuse them:

- **`Network.Mailbox`** — RFC 5322 addresses (`addr-spec`, `mailbox`, group
  `address`, with display names, domain literals, and quoted local parts).
- **`Network.IPAddress`** — IPv4 (RFC 791), IPv6 (RFC 4291, including `::`
  zero-compression and IPv4-mapped tails), and `IPvFuture`, with RFC 5952
  canonical rendering.
- **`Network.TLS.Extensions`** — the RFC 7301 ALPN protocol-ID registry
  (`h2`, `http/1.1`, `acme-tls/1`, …), verified against the live IANA table.

## What it owns

- **Per-header parse + render**: one `KnownHeader` instance per field in
  `Network.HTTP.Headers.<Name>`, covering both `parseFromHeaders` and
  `renderToHeaders`.
- **IANA registries**: content codings, HTTP methods, status codes, HTTP
  versions, and the full case-insensitive header field-name registry
  (`Network.HTTP.Headers.HeaderFieldName`).
- **Quality-weighted lists**: `q=` parsing for content negotiation
  (`WeightedMediaRange`, `WeightedLanguage`).
- **HTTP-date**: IMF-fixdate, RFC 850, and asctime formats.
- **Percent-decoding**: RFC 3986 decoding with a C fast path
  (`cbits/url_decode.c`).
- **Shared primitives**: builder and parser helpers reused by every header
  codec (token, quoted-string, weight, OWS, RFC 8941 structured fields).

## Test coverage

Every header ships a parse/render unit test, and the structured headers ship a
Hedgehog **round-trip property** (generate a value, render it, parse it back,
assert equality) so render→parse is total over each grammar. The
`hermes-tests` suite currently runs ~660 examples across 61 per-family modules.

The property generators are constrained to each header's valid grammar, and
the pure parsers are total over those bounded inputs, so the suite disables
sydtest's per-test wall-clock timeout (it fired spuriously on this sub-second
suite — see `Main.hs`).

## Using a header

Each header module exposes a standalone `<name>Parser` and `render<Name>`
around its `KnownHeader` instance, so you can work at either the value level or
the `HeaderMap` level:

```haskell
import Network.HTTP.Headers.CacheControl
  ( cacheControlHeaderParser, renderCacheControl )
import Network.HTTP.Headers.Parsing.Util (runParser)

-- parse a value
case runParser cacheControlHeaderParser "no-cache, max-age=0" of
  OK cc "" -> pure cc   -- cc :: CacheControl
  _        -> error "bad Cache-Control"

-- render it back
renderCacheControl cc   -- :: M.Builder
```

```haskell
import Network.HTTP.Headers
  (HeaderMap, headerMapFromList, lookupHeader, setHeader)
import qualified Network.HTTP.Headers.ContentType as CT

-- round-trip a header through the typed surface
let hm = setHeader (CT.ContentType mt) (headerMapFromList []) :: HeaderMap
case lookupHeader hm :: Either String (Maybe CT.ContentType) of
  Right (Just ct) -> pure ct
  _               -> error "missing"
```

## Notable modules

All of these are exposed modules in `hermes.cabal`:

| Module | Purpose |
|--------|---------|
| `Network.HTTP.Headers` | Header collection type and the `KnownHeader` surface |
| `Network.HTTP.Headers.HeaderFieldName` | Case-insensitive header field-name registry (every IANA field) |
| `Network.HTTP.ContentCoding` | IANA content-coding registry (`gzip`, `br`, …) |
| `Network.HTTP.ContentNegotiation` | Quality-weighted content negotiation (`q=`) |
| `Network.HTTP.URL.Decode` | RFC 3986 percent-decoding (with `cbits/url_decode.c`) |
| `Network.HTTP.Headers.Mason` | Builder primitives shared by every header renderer |
| `Network.HTTP.Headers.Parsing.Util` | Token / quoted-string / weight / OWS / RFC 8941 parser primitives |
| `Network.HTTP.Headers.Rendering.Util` | Shared rendering helpers |
| `Network.HTTP.Methods` / `Network.HTTP.Status` / `Network.HTTP.Versions` | HTTP method, status, and version vocabularies |
| `Network.HTTP.Path` / `Network.HTTP.QueryParameters` | Request-target path and query handling |
| `Network.Mailbox` / `Network.IPAddress` / `Network.TLS.Extensions` | Foundational IETF value parsers (see above) |

## Provenance: a maintained fork

`hermes` is originally by Ian Duncan
([`MercuryTechnologies/hermes`](https://github.com/MercuryTechnologies/hermes),
BSD-3-Clause). The copy in this monorepo is the active line of development and
has **diverged substantially** from that upstream base — the comprehensive
header coverage, the foundational IETF value parsers, the structured-field
helpers, and the per-family test suite are all wireform-side additions — so it
is effectively a **fork**: this copy is the source of truth for the wireform
stack, and the standalone upstream now lags it rather than the other way
around. Wire-grammar work happens here. In this repo it depends on
[`wireform-core`](../core/) and ships a small C fast path
(`cbits/url_decode.c`) for percent-decoding.

## When to extend hermes vs. wrap it

The monorepo guidelines draw a sharp line. **Default to extending hermes.**

1. **Wire grammar / RFC compliance change → hermes.** A new header, a new
   parameter, a bug in the q-value parser, or a tighter token check is a
   `KnownHeader` change. Add or update the instance in
   `Network.HTTP.Headers.<Name>`, including `parseFromHeaders` and
   `renderToHeaders`. Do not redefine the parser downstream.
2. **Smart constructors / domain wrappers / `IsString` instances →
   `wireform-http*`.** Hermes stays close to the wire types (often `ShortText`
   or `[Word8]` shaped); the ergonomic newtypes and request combinators live in
   `Network.HTTP.Client.<Topic>`.
3. **Cross-cutting client / server policy → `wireform-http*`.** Cache freshness
   (RFC 9111), redirect following, retry, the cookie jar, and the middleware
   stack are client concerns. Hermes parses the directive list; *deciding what
   to cache* lives in [`wireform-http`](../http/).
4. **A header hermes doesn't have yet → add it to hermes.** Mirror the closest
   existing instance (`RetryAfter` for delta-or-date shapes, `Accept` for
   q-weighted lists, `SetCookie` for attribute-bag shapes) and wire the
   cardinality / direction correctly. Give it a module-level Haddock following
   the standard in [`hermes/PROJECT_GUIDE.md`](https://github.com/iand675/wireform-/blob/main/hermes/PROJECT_GUIDE.md#module-documentation-standard).

### Signs you should be calling hermes

If you find yourself writing one of these in `wireform-http*` or
[`wireform-grpc`](../grpc/), check whether hermes already covers it:

- splitting on `0x2C` / `0x3B` to peel apart a header value;
- a bespoke `parseQuality` / `parseQ` / weight-list parser;
- a copy of the IMF-fixdate format string;
- a `case BS.elemIndex 0x3D bs of` dance to extract an `auth-param`;
- a new challenge record when
  `Network.HTTP.Headers.Authorization.Credentials` already models it;
- a handwritten dispatch on `Content-Encoding` (use
  `Network.HTTP.ContentCoding` instead).

> Wire grammar lives in hermes. Domain modeling and policy live in
> `wireform-http*`. If a change is blocked because the grammar in hermes is
> missing a piece, add it to hermes first, then build the wrapper.
