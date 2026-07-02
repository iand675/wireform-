# Revision history for grpc-spec

## Unreleased

* Add `Network.GRPC.Spec.Service` (re-exported from `Network.GRPC.Spec`):
  the transport-agnostic service-implementation vocabulary — `Service`,
  `service`, `method`, `methodUnimplemented`, `Handlers`/`MethodOf`,
  `HandlerOf`, plus the `ServiceMethods` family (moved here from
  `wireform-grpc`'s `Network.GRPC.Server.Protobuf`). One `Service` value can
  be served over gRPC (`wireform-grpc`) and Connect (`wireform-connect`).
  Registration is order-insensitive and completeness-checked at the type
  level with named-method error messages.

## 1.0.0 -- 2025-01-22

* First released version.
