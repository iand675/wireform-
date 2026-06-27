---
title: hermes
description: "The canonical home for HTTP header parsing and rendering in the wireform monorepo: per-header codecs, IANA registries, and shared wire-grammar primitives."
sidebar:
  order: 57
---

`hermes` is a foundation library that provides HTTP wire-format primitives.
Its current focus is **HTTP header parsing and rendering**. It is the
*canonical home* for that grammar in this monorepo: when the wire grammar of an
HTTP construct needs to change, the change goes in `hermes`, not in a
downstream [`wireform-http`](../http/) or [`wireform-grpc`](../grpc/) module.

Named after the messenger of the Greek gods, hermes models each header as a
`KnownHeader` instance carrying a parser, a renderer, and a cardinality, so
that callers parse and render headers through one consistent surface instead of
hand-rolling `BS.split` / `BS.break` ad-hoc parsers that drift from the RFCs.

## Provenance / vendored

`hermes` is **vendored from
[`MercuryTechnologies/hermes`](https://github.com/MercuryTechnologies/hermes)**
(author Ian Duncan, Mercury Technologies, BSD-3-Clause) and rebranded under the
wireform umbrella. It has not been forked in spirit: per the monorepo
guidelines, wire-grammar changes are made here and flow back, rather than being
duplicated downstream. In this repo it depends on
[`wireform-core`](../core/) and ships a small C fast path
(`cbits/url_decode.c`) for percent-decoding.

## What it owns

- **Per-header parse + render**: one `KnownHeader` instance per header field
  in `Network.HTTP.Headers.<Name>`, covering both `parseFromHeaders` and
  `renderToHeaders`.
- **IANA registries**: content codings, methods (via `Allow`), and
  case-insensitive header field-name strings.
- **Quality-weighted lists**: `q=` parsing for content negotiation
  (`WeightedMediaRange`, `WeightedLanguage`).
- **HTTP-date**: IMF-fixdate, RFC 850, and asctime formats.
- **Percent-decoding**: RFC 3986 decoding with a C fast path.
- **Shared primitives**: builder and parser helpers reused by every header
  codec (token, quoted-string, weight, OWS parsing).

## Notable modules

All of these are exposed modules in `hermes.cabal`:

| Module | Purpose |
|--------|---------|
| `Network.HTTP.Headers` | Header collection type and the `KnownHeader` surface |
| `Network.HTTP.ContentCoding` | IANA content-coding registry (`gzip`, `br`, …) |
| `Network.HTTP.ContentNegotiation` | Quality-weighted content negotiation (`q=`) |
| `Network.HTTP.URL.Decode` | RFC 3986 percent-decoding (with `cbits/url_decode.c`) |
| `Network.HTTP.Headers.Mason` | Builder primitives shared by every header renderer |
| `Network.HTTP.Headers.Parsing.Util` | Token / quoted-string / weight / OWS parser primitives |
| `Network.HTTP.Headers.Rendering.Util` | Shared rendering helpers |
| `Network.HTTP.Headers.HeaderFieldName` | Case-insensitive header field-name registry |
| `Network.HTTP.Methods` / `Network.HTTP.Status` / `Network.HTTP.Versions` | HTTP method, status, and version vocabularies |
| `Network.HTTP.Path` / `Network.HTTP.QueryParameters` | Request-target path and query handling |
| `Network.IPAddress` / `Network.Mailbox` / `Network.TLS.Extensions` | Supporting wire types |

### Per-header codecs

`Network.HTTP.Headers.<Name>` defines the `KnownHeader` instance for each
field. The exposed header modules include `Accept`, `AcceptCharset`,
`AcceptEncoding`, `AcceptLanguage`, `AcceptRanges`, `Age`, `Allow`,
`Authorization`, `CacheControl`, `CacheStatus`, `Connection`,
`ContentDisposition`, `ContentEncoding`, `ContentLength`, `ContentRange`,
`ContentType`, `Cookie`, `Date`, `ETag`, `Expires`, `From`, `Host`, `IfMatch`,
`IfModifiedSince`, `IfNoneMatch`, `IfUnmodifiedSince`, `KeepAlive`,
`LastModified`, `Location`, `Origin`, `Range`, `PingFrom`, `PingTo`,
`ProxyAuthenticate`, `ProxyAuthorization`, `Referer`, `RetryAfter`, `Server`,
`SetCookie`, `Settings`, `Sunset`, `TransferEncoding`, `UserAgent`, `Vary`, and
`WWWAuthenticate`.

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
   cardinality / direction correctly.

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
