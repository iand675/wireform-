# wireform-connect conformance harness

Cross-language interop verification for `wireform-connect` using the **official
[connectrpc/conformance](https://github.com/connectrpc/conformance) suite**
(pinned `v1.0.5`). The suite drives a reference Connect client (written in
connect-go) against our server-under-test and checks every reply on the wire.

## Layout

| Path | What |
|---|---|
| `proto/connectrpc/conformance/v1/*.proto` | Upstream conformance protos, vendored verbatim (v1.0.5). |
| `gen/Connect/Conformance/Proto.hs` | `loadProto` splices for all four protos (message + enum types). |
| `gen/Connect/Conformance/Service.hs` | `loadProtoServices` splice → the `ConformanceService` RPC tags. |
| `gen/Connect/Conformance/Support.hs` | Shared helpers: stdin/stdout size-delimited framing, conformance-enum↔Connect mappings, `Header`↔`CustomMetadata`, `Any` pack/unpack. |
| `server/Main.hs` | The `--mode server` program (exe `conformance-server`): implements `ConformanceService` over `Network.Connect.Server`. |
| `client/Main.hs` | The `--mode client` program (exe `conformance-client`): issues RPCs with `Network.Connect.Client`. |
| `config-server.yaml` / `config-client.yaml` | Features advertised: Connect protocol, proto+json, identity/gzip/br/zstd, HTTP/1.1+2, no TLS. |
| `known-failing-server.txt` / `known-failing-client.txt` | Test patterns not yet passing (currently empty — see below). |
| `run.sh` | Downloads the runner and runs server / client / both mode. |
| `bin/` | Downloaded `connectconformance` binary (gitignored). |

## Running

```bash
wireform-connect/conformance/run.sh         # server mode, known-failing applied
wireform-connect/conformance/run.sh client  # client mode
wireform-connect/conformance/run.sh server raw   # show every failure (no known-failing)
```

## Status

**Full conformance** against connectconformance v1.0.5 — both modes, raw
(empty known-failing lists):

- **Server mode: 1411 / 1411** — wireform-connect as the server-under-test
  vs the reference connect-go client.
- **Client mode: 1603 / 1603** — wireform-connect as the client-under-test
  vs the reference connect-go server.

Both cover all RPC kinds (unary, client-/server-/bidi-streaming, full- and
half-duplex), proto + JSON codecs, identity/gzip/br/zstd compression,
HTTP/1.1 and HTTP/2 (h2c), the gRPC-derived error model, leading/trailing
metadata (incl. duplicates), Connect GET, the EndStreamResponse envelope,
client/server cancellation, and the Connect "unexpected response" protocol
checks.

Getting here surfaced and fixed real library + protocol bugs, including:

- **`wireform-proto`**: `loadProto` follows proto `import`s (cross-file enum
  refs); `oneof` field size no longer double-counts the tag; nested-message
  `protoMessageName` includes the parent (correct `Any` type URLs).
- **`wireform-connect` server**: streaming responses emit staged leading
  metadata; unary errors carry staged headers/trailers; trailing metadata
  preserves duplicates; `zstd` decompression uses the lazy/streaming API.
- **`wireform-connect` client response validation**: unary `Content-Type` is
  validated (unrecognised type → `unknown`, wrong codec → `internal`);
  streaming frames compressed without a negotiated encoding → `internal`;
  client-streaming responses must carry exactly one message (else
  `unimplemented`).
- **`wireform-connect` client cancellation**: `after_num_responses`,
  `before_close_send`, and `after_close_send` are honoured; full-duplex bidi
  receives one response per request before cancelling, reporting the received
  payloads plus `CODE_CANCELED`.
- **`wireform-http2`**: the HPACK encoder/send-ordering fix (encode atomic with
  the send under one lock; monotonic stream-id allocation —
  `Network.HTTP2.Connection.encodeAndSendHeaderBlock`), which also cleared the
  HTTP/2 streaming flakiness that previously tripped connect-go's flood
  protection.

The known-failing lists are now empty. If the HTTP/2 "empty-DATA flood"
flakiness ever resurfaces under load, prefer `--known-flaky` over re-adding
entries.

## Regenerating the vendored protos

```bash
base=https://raw.githubusercontent.com/connectrpc/conformance/v1.0.5/proto/connectrpc/conformance/v1
for f in config service client_compat server_compat; do
  curl -fsSL "$base/$f.proto" -o "proto/connectrpc/conformance/v1/$f.proto"
done
```
