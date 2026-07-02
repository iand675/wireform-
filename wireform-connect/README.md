# wireform-connect

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

> [!CAUTION]
> wireform is in heavy development and has not been published to Hackage yet. APIs may change.

A native Haskell implementation of the [Connect RPC
protocol](https://connectrpc.com/docs/protocol): a client **and** server for
unary and all three streaming RPC kinds, over **both HTTP/1.1 and HTTP/2**,
with **binary Protobuf and JSON** codecs, **GET** support for side-effect-free
unary calls, **identity/gzip/br/zstd** compression, the **gRPC-derived
error-code model**, leading/trailing **metadata**, and the streaming
**EndStreamResponse** envelope.

This package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo. Like [`wireform-grpc`](../wireform-grpc/), it is an RPC framework, so
it owns the `Network.Connect.*` namespace rather than wireform's per-format
`<Format>.*` convention, and it is not re-exported through the umbrella
`wireform` package.

## Connect is a transport over the existing service tags

The Connect runtime reuses the **same protocol-agnostic service description**
that gRPC uses. A generated Protobuf service — the `Protobuf serv "meth"` tags
that [`loadProtoServices`](../wireform-grpc/src/Network/GRPC/Protobuf/TH.hs)
emits, together with the message types from
[`loadProto`](../wireform-proto/src/Proto/TH.hs) — drives Connect **unchanged**.
There is no separate Connect codegen: Connect is purely a new transport over the
existing `IsRPC` / `SupportsClientRpc` / `SupportsServerRpc` / `HasStreamingType`
abstraction in [`grpc-spec`](../grpc-spec/). The `application/grpc+proto`
content-type baked into each tag is ignored; Connect computes its own content
types (`application/proto`, `application/json`, `application/connect+proto`,
`application/connect+json`) from the codec and streaming kind.

The proto3-JSON path works because the `Proto` newtype carries `ToJSON` /
`FromJSON` instances (defined in `grpc-spec`, delegating to the message's
generated aeson instances), so a method's `Input` / `Output` serialize in
either codec without unwrapping.

## Hello world

Generate the service tags + message types once (in a module with
`{-# LANGUAGE DataKinds #-}`):

```haskell
{-# LANGUAGE DataKinds, TemplateHaskell, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
module Eliza where

import Network.GRPC.Protobuf.TH (loadProtoServices)
import Proto.TH (loadProto)

$(loadProto "proto/eliza.proto")
$(loadProtoServices "proto/eliza.proto")
```

A server. Handlers are registered with the transport-agnostic `Service`
vocabulary: one `method` per RPC (the handler shape is inferred from the
method's streaming kind), order-insensitive, and completeness-checked at
compile time — forgetting a method, listing one twice, or listing a foreign
method is a type error naming the method:

```haskell
import Network.Connect.Server
import Network.HTTP.Server (defaultServerConfig, ServerConfig (..))
import Network.HTTP.VersionRange (preferHttp2)
import Network.GRPC.Spec (Proto (..))
import Eliza

main :: IO ()
main =
  runConnectServer defaultConnectServerConfig serverCfg (connectHandlers eliza)
  where
    serverCfg = defaultServerConfig { serverPort = "8080", serverVersionRange = preferHttp2 }

eliza :: Service ElizaService ConnectServerM
eliza =
  service
    (  method @Say       say
    :& method @Introduce introduce
    :& method @Converse  converse
    :& Done
    )
  where
    say (Proto req) =
      pure (Proto defaultSayResponse { sayResponseSentence = "Hello, " <> sayRequestSentence req })
    -- introduce / converse: handlers using the recv / send callbacks
```

The same `Service` value — written polymorphically, e.g.
`MonadIO m => Service ElizaService m` — can be served over gRPC with
`wireform-grpc`'s `Network.GRPC.Server.Service.fromService`. Declare
deliberately-unsupported methods with `methodUnimplemented @Tag`.

A client:

```haskell
import Network.Connect.Client
import Network.Connect.Protocol (Codec (..))
import Network.GRPC.Spec (Proto (..))
import Data.Proxy (Proxy (..))
import Eliza

main :: IO ()
main = do
  let ccfg    = defaultConnectClientConfig { cccCodec = CodecProto }
      connCfg = defaultConnectionConfig { connectionHost = "demo.connectrpc.com", connectionPort = "443" }
  withConnectClient ccfg connCfg { connectionTls = Just (defaultTlsConnectionConfig "demo.connectrpc.com") } $ \cl -> do
    Proto resp <- nonStreaming cl (Proxy @Say) (Proto defaultSayRequest { sayRequestSentence = "Hi" })
    print (sayResponseSentence resp)
```

## What's in here

| Module | Role |
|---|---|
| `Network.Connect.Server` | `runConnectServer`, `connectHandlers` + the `service`/`method` registration vocabulary (re-exported from `grpc-spec`), `ConnectServerM` + metadata accessors |
| `Network.Connect.Client` | `withConnectClient` + `nonStreaming`/`nonStreamingGet`/`serverStreaming`/`clientStreaming`/`biDiStreaming` |
| `Network.Connect.Protocol` | Codecs, the content-type matrix, reserved header names, GET query parameters |
| `Network.Connect.Error` | `ConnectError`/`ConnectException`, the code↔name + code↔HTTP-status tables, the JSON error envelope |
| `Network.Connect.Envelope` | The streaming frame (1 flag byte + 4-byte length) + `EndStreamResponse` |
| `Network.Connect.Metadata` | `CustomMetadata` ↔ HTTP headers (ASCII / `-bin` base64 / `trailer-` prefix) |
| `Network.Connect.Codec` | proto / JSON message-body (de)serialization |
| `Network.Connect.Compression` | `identity` / `gzip` / `br` / `zstd` + accept-encoding negotiation |
| `Network.Connect.OpenAPI` | `connectOpenApi` / `renderOpenApi` — generate an OpenAPI 3.1 document for a `.proto`'s Connect services |

## OpenAPI generation

Because Connect speaks ordinary HTTP with a JSON codec, a Connect service can
be described by an [OpenAPI 3.1](https://spec.openapis.org/oas/v3.1.0)
document. `Network.Connect.OpenAPI` generates one straight from a parsed
`.proto`, or via the CLI:

```bash
wireform-gen openapi -i proto/eliza.proto --title Eliza --api-version 1.0.0 > eliza.openapi.json
```

The transport-agnostic JSON Schema walk lives in `Proto.JSONSchema`
(`wireform-proto`); this package wraps those schemas in the Connect wire
conventions: one `/pkg.Service/Method` path per method (`post`, plus a
cacheable `get` for `idempotency_level = NO_SIDE_EFFECTS` unary methods),
`application/json` bodies for unary calls and `application/connect+json` for
streaming (tagged with an `x-connect-streaming` extension), and a shared
`connect.Error` envelope on every operation's `default` response.

OpenAPI describes the JSON codec, so the emitted schemas match the
proto3-canonical-JSON bytes exactly — lowerCamelCase field names, 64-bit
integers as strings, `bytes` as base64, well-known types inlined
(`Timestamp` → `date-time` string), enums as string enums, maps as
`additionalProperties`.

### Carrying protovalidate rules through

If your `.proto` uses [protovalidate](https://protovalidate.com/)
(`buf.validate`) field/message rules, `wireform-gen openapi --validate` folds
them into the schema as JSON Schema validation keywords:

```bash
wireform-gen openapi -i proto/user.proto --validate > user.openapi.json
```

Rules with a faithful JSON Schema equivalent become standard keywords
(`string.min_len`→`minLength`, `string.pattern`→`pattern`,
`string.email`→`format: email`, numeric `gte`/`lte`→`minimum`/`maximum`,
`repeated.min_items`→`minItems`, `repeated.unique`→`uniqueItems`,
`required`→the object's `required` list, …). Everything without a clean
analogue is preserved losslessly: custom CEL (`(buf.validate.field).cel` /
`(buf.validate.message).cel`) as an `x-cel` array of `{id, message,
expression}`, and the remaining rules under an `x-protovalidate` object — so a
validation-aware tool sees the full rule set while a generic OpenAPI consumer
still gets the standard keywords. The mapping lives in
`Protovalidate.OpenAPI` (in
[`wireform-protovalidate`](../wireform-protovalidate/)).

### Composable annotators + deprecation

`--validate` is one *annotator*, not a hardcoded pass. The seam is composable:
`Proto.JSONSchema.SchemaOptions` and the Connect-layer
`Network.Connect.OpenAPI.Annotators` are `Monoid`s, so independent annotators
stack with `<>`. Built in:

- **deprecation** (always on via the CLI): the standard proto `deprecated`
  option → OpenAPI `deprecated: true` on messages, fields, enums, and
  operations (`deprecationSchemaOptions` / `deprecationAnnotators`);
- **protovalidate** (`--validate`): `Protovalidate.OpenAPI.protovalidateSchemaOptions`.

Programmatically, compose your own and hand them to `connectOpenApiAnnotated`:

```haskell
connectOpenApiAnnotated
  (deprecationAnnotators files <> schemaAnnotators (protovalidateSchemaOptions rules))
  opts [target] schemaFiles
```

`Annotators` also carries an **operation-level** hook (`serviceFqn ->
methodName -> [Pair]`) for method-level keywords (deprecation, tags, auth,
custom `x-` extensions). A worked end-to-end example — all RPC kinds,
well-known types, protovalidate rules, and `deprecated`, plus its generated
document — is checked in at
[`examples/openapi/`](../examples/openapi/).

## Testing

```bash
cabal test wireform-connect:wireform-connect-test
```

The suite is an in-process loopback (server + client over an ephemeral port)
covering every RPC kind for both codecs, plus unary GET, the error path, gzip
compression, and metadata propagation; it also unit-tests the wire vocabulary
(content-type matrix, error tables, envelope framing, metadata round-trips,
compression).

Opt-in interop against the public demo server is gated by an environment
variable:

```bash
CONNECT_DEMO=1 cabal test wireform-connect:wireform-connect-test
```

runs `Test.Interop` against `demo.connectrpc.com`'s `connectrpc.eliza.v1.ElizaService`
over TLS HTTP/2 for both codecs (skipped silently otherwise).

## License

BSD-3-Clause.

## References

- [Connect protocol reference](https://connectrpc.com/docs/protocol)
- [`grpc-spec`](../grpc-spec/) — the shared RPC abstraction Connect reuses
- [`wireform-http`](../wireform-http/) — the HTTP/1.1 + HTTP/2 transport
