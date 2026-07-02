# OpenAPI generation — worked example

An end-to-end example of `wireform-gen openapi`: a `.proto` describing Connect
RPC services, and the OpenAPI 3.1 document generated from it.

## Files

- [`catalog.proto`](./catalog.proto) — a small catalog API exercising every
  feature the generator carries through: all four RPC kinds, a cacheable GET
  (side-effect-free unary), a `google.protobuf.Timestamp`, enums / maps /
  oneofs / a nested message, [protovalidate](https://protovalidate.com/)
  (`buf.validate`) rules, and `deprecated` on a field, an enum, and an RPC.
- [`catalog.openapi.json`](./catalog.openapi.json) — the generated document
  (checked in so it's linkable/diffable).

## Regenerate

From the repo root:

```bash
cabal run wireform-gen -- openapi \
  -i examples/openapi/catalog.proto \
  --title "Catalog API" --api-version 1.0.0 \
  --server https://api.example.com \
  --validate \
  -o examples/openapi/catalog.openapi.json
```

`--validate` folds the `buf.validate` rules in; deprecation is always carried
through. Drop `--validate` to see the base document without protovalidate
keywords.

## What to look for in the output

| proto | OpenAPI |
|---|---|
| `GetProduct` (`idempotency_level = NO_SIDE_EFFECTS`) | both a `get` (with the 5 Connect query params) and a `post` |
| `ListProducts` / `ImportProducts` / `Watch` (streaming) | `application/connect+json` body + `x-connect-streaming: server`/`client`/`bidi` |
| `CreateProductRequest.name` (`string.min_len`/`max_len`) | `minLength` / `maxLength` |
| `CreateProductRequest.price_cents` (`int64.gte = 0`) | `{ "type": "string", "format": "int64", "minimum": 0 }` |
| `CreateProductRequest.slug` (`string.pattern`) | `pattern` |
| `GetProductRequest.product_id` (`string.uuid`) | `format: uuid` |
| `tags` (`repeated.max_items`/`unique`) | `maxItems` / `uniqueItems` |
| `Product.created_at` (`google.protobuf.Timestamp`) | inlined `{ "type": "string", "format": "date-time" }` |
| `Product.sku` (`[deprecated = true]`) | `deprecated: true` on the property |
| `Category` (enum) | `{ "type": "string", "enum": [...] }` in declared order |
| `LegacyTier` (`option deprecated = true`) | `deprecated: true` on the enum component |
| `SearchLegacy` (`option deprecated = true`) | `deprecated: true` on the operation |
| every RPC | a `default` response → `#/components/schemas/connect.Error` |

## How it composes

Deprecation and protovalidate are two independent, composable *annotators*
(both `Monoid`-combined via `<>`), not hardcoded passes — see the
[`wireform-connect` README](../../wireform-connect/README.md#composable-annotators--deprecation).
The transport-agnostic JSON Schema walk is `Proto.JSONSchema`
(`wireform-proto`); the Connect HTTP shaping is `Network.Connect.OpenAPI`
(`wireform-connect`); the protovalidate mapping is `Protovalidate.OpenAPI`
(`wireform-protovalidate`).
