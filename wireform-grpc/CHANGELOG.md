# Revision history for grapesy

## Unreleased

* **Replace the handler-registration API.** The order-sensitive
  `Methods`/`Services` GADTs and their consumers (`fromMethods`,
  `fromServices`, `simpleMethods`, `hoistMethods`, `hoistServices`) and the
  `Network.GRPC.Server.Protobuf` module (`ProtobufMethodsOf` etc.) are
  removed. Protobuf services are now implemented with the transport-agnostic
  `Service` vocabulary from `grpc-spec` (`Network.GRPC.Spec.Service`:
  `service`, `method`, `methodUnimplemented`) and served with
  `Network.GRPC.Server.Service.fromService`. Registration is
  order-insensitive and completeness-checked (missing/duplicate/foreign
  methods are compile-time errors naming the method), and the same `Service`
  value can be served over Connect (`wireform-connect`). `ServiceMethods`
  moved to `grpc-spec` (re-exported from `Network.GRPC.Spec`).
  `Network.GRPC.Server.StreamType` keeps the per-RPC `mk*Streaming` builders
  and `fromMethod` as the raw/custom-format escape hatch.

* Add `Network.GRPC.Protobuf.TH` (`loadProtoServices` /
  `loadProtoServicesWith`): Template Haskell generation of the gRPC service
  bindings (the `Protobuf`-tagged RPC types plus their `IsRPC` /
  `SupportsClientRpc` / `SupportsServerRpc` / `HasStreamingType` /
  `ServiceMethods` instances) directly from a `.proto` file, complementing
  `wireform-proto`'s `loadProto` (which generates only the message types).
  Previously this glue had to be hand-written per method.

## 1.1.1 -- 2025-10-09

* Support `openConnection/closeConnection`

## 1.1.0 -- 2025-07-17

* User-specified actions on connection/disconnect/reconnect [#280]
  - Overhaul `ReconnectPolicy` to allow making `ReconnectDecision`s after
    running some `IO` action.
  - Introduce `ReconnectDecision`, which specifies the `ReconnectTo` target, the
    `OnConnection` action to run upon reconnection, and the next
    `ReconnectPolicy`.
  - Add `OnConnection` actions to `ConnParams` and `ReconnectDecision` so that
    users can track whether a connection is actually connected.
* `Network.GRPC.Common.Protobuf` does not import from `Data.ProtoLens.Labels`,
  to avoid problems with `lens` [#283, Leonid Onokhov].
  NOTE: Users who want to rely on this will also need
  https://github.com/google/proto-lens/pull/515.
* Lower bound on `http2-tls` changed to 0.4.9 [#289]

## 1.0.1 -- 2025-04-01

* Support unix sockets [#275, Sjoerd Visscher]

## 1.0.0 -- 2025-01-22

* First released version.
