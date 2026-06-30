---
title: Connect RPC wire protocol
description: "The on-the-wire shapes wireform-connect produces and consumes: content-type matrix, unary vs streaming framing, the 5-byte envelope and EndStreamResponse, unary GET, the error model and code↔HTTP table, metadata encoding, and compression negotiation. For interop and curl debugging."
sidebar:
  order: 5
  label: Wire protocol
---

This page documents what `wireform-connect` puts on the wire, so you can
interoperate with other Connect implementations and debug calls with `curl`. It
follows the [Connect protocol reference](https://connectrpc.com/docs/protocol);
`wireform-connect` is verified against `demo.connectrpc.com` for both codecs.

## Content types

The content-type is computed from the codec and the streaming kind — four exact
wire strings:

| Exchange | Codec | Content-Type |
|---|---|---|
| Unary | Protobuf | `application/proto` |
| Unary | JSON | `application/json` |
| Streaming | Protobuf | `application/connect+proto` |
| Streaming | JSON | `application/connect+json` |

On the server, the `application/` prefix is matched case-insensitively (HTTP
media types are case-insensitive); the codec suffix must match exactly. Anything
else yields **415 Unsupported Media Type**. Note this is a *different* set from
gRPC's `application/grpc+proto` — Connect ignores the content-type baked into
each service tag and uses its own.

## Unary framing: bare bodies

Unary requests and responses are **bare** message bodies — no length prefix, no
envelope. A unary JSON call is just an HTTP POST with a JSON body:

```bash
curl -X POST https://srv/connectrpc.eliza.v1.ElizaService/Say \
  -H 'content-type: application/json' \
  -H 'connect-protocol-version: 1' \
  -d '{"sentence":"Hi"}'
# → {"sentence":"Hello, Hi"}
```

`connect-protocol-version: 1` is required on unary requests. The path is always
`/<fully.qualified.Service>/<Method>`.

## Streaming framing: the envelope

Streaming exchanges (server / client / bidirectional) wrap **every** message in
a 5-byte prefix followed by the payload:

```
+------+------+------+------+------+----------------+
| flag |           length (4 bytes BE)              | payload ...   |
+------+------+------+------+------+----------------+
```

- **flag** — one byte, two defined bits:
  - bit 0 (`0x01`) = the payload is compressed
  - bit 1 (`0x02`) = end-of-stream (this is the final frame)
- **length** — the payload length, 4 bytes, big-endian.
- **payload** — the (optionally compressed) message bytes.

> **Not gRPC-Web.** gRPC-Web marks trailers with the *most significant* bit;
> Connect marks end-of-stream with bit 1 (the *second* bit). Different wire
> shape — do not cross them.

The **final frame** of a streaming response is the `EndStreamResponse`: its flag
byte has the end-stream bit set, and its payload is a JSON object:

```json
{ "error"?: { "code": "...", "message"?: "...", "details"?: [...] },
  "metadata"?: { "key": ["value", ...] } }
```

`error` is present **iff** the RPC failed; a successful stream omits it. A
streaming response is always **HTTP 200** — failures ride the `EndStreamResponse`
`error`, not the status line. The decoder rejects malformed error payloads
(`{"error": null}`, `{"error": {}}`, `{"error": {"code": null}}`).

This envelope shape is structurally the gRPC length-prefix but with a different
flag byte (gRPC's single-bit compressed flag cannot represent Connect's
end-stream semantics), so it is reimplemented in `Network.Connect.Envelope`
rather than bending grpc-spec's prefix.

## Unary GET

A side-effect-free unary call may be a `GET` with the request encoded as query
parameters:

| Param | Value |
|---|---|
| `connect` | `v1` |
| `encoding` | `proto` or `json` |
| `base64` | `1` if the message is binary/compressed, else `0` |
| `compression` | the content-coding used (`identity` if none) |
| `message` | the (optionally base64url-unpadded) request payload |

```bash
curl 'https://srv/connectrpc.eliza.v1.ElizaService/Say?connect=v1&encoding=json&base64=0&compression=identity&message=%7B%22sentence%22%3A%22Hi%22%7D'
```

JSON messages go in verbatim (`base64=0`); proto messages are base64url-unpadded
(`base64=1`). GET responses are cacheable by any HTTP intermediary.

## Error model

Connect reuses gRPC's sixteen status codes verbatim (`ConnectCode` is
`GrpcError`). A failed **unary** call is a non-2xx HTTP status with a JSON
`Error` body:

```json
{ "code": "not_found",
  "message": "user 42 does not exist",
  "details": [ { "type": "google.rpc.ErrorInfo", "value": "CgYKB...=", "debug": {...} } ] }
```

`message` is omitted when absent; `details` is omitted when empty. Each detail is
a Protobuf message: `type` is its fully-qualified name, `value` is the raw binary
Protobuf base64-encoded (standard, unpadded on encode; padded or unpadded
accepted on decode), and `debug` is an optional human-readable JSON rendering.

### Code ↔ HTTP status

The status for an outbound error (server) and the code inferred from a status
when the body is missing/garbled (client):

| Code | HTTP | Code | HTTP |
|---|---|---|---|
| `canceled` | 499 | `unimplemented` | 501 |
| `unknown` | 500 | `internal` | 500 |
| `invalid_argument` | 400 | `unavailable` | 503 |
| `deadline_exceeded` | 504 | `data_loss` | 500 |
| `not_found` | 404 | `unauthenticated` | 401 |
| `already_exists` | 409 | `aborted` | 409 |
| `permission_denied` | 403 | `out_of_range` | 400 |
| `resource_exhausted` | 429 | `failed_precondition` | 400 |

A failed **streaming** call carries the same `Error` object in the
`EndStreamResponse` rather than using the HTTP status.

## Metadata

Custom metadata maps to HTTP headers, with two encoding rules:

- **ASCII** values go in verbatim; **binary** values use a key with a `-bin`
  suffix and a base64 value (unpadded on encode; padded or unpadded accepted on
  decode).
- Header names reserved by Connect (`connect-*`, `content-*`, etc.) are not
  custom metadata — they're filtered out on parse.

Where trailing metadata lands depends on the call kind:

- **Unary** trailing metadata is `trailer-`-prefixed and placed in the **same
  header block** as the response (Connect's unary trailers don't use HTTP
  trailers).
- **Streaming** trailing metadata rides the `EndStreamResponse` `metadata`
  object (a map of header name → array of wire-encoded strings).

Leading metadata is always ordinary request/response headers.

## Compression

Connect supports `identity`, `gzip`, `br` (Brotli), and `zstd` — a different set
from grpc-spec's `gzip`/`deflate`/`snappy`, so a focused local enum is used.

- **Unary** compression uses the standard `content-encoding` /
  `accept-encoding` headers.
- **Streaming** compression uses `connect-content-encoding` /
  `connect-accept-encoding` headers **and** the envelope's compressed bit (each
  frame declares whether its payload is compressed).

The server negotiates the **first** client-preferred coding (in
`connect-accept-encoding` order) that it supports, else `identity`. If the client
requests a coding the server doesn't support, the server responds
`unimplemented` with a message listing the codings it *does* support.
