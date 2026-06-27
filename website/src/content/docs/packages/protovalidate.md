---
title: protovalidate
description: "CEL-driven Protobuf message validation: standard buf.validate rules plus arbitrary custom CEL, with interpreted, compile-time, and refinement-type paths."
sidebar:
  order: 71
---

`wireform-protovalidate` implements
[protovalidate](https://protovalidate.com/) for the wireform Protocol Buffers
stack: protobuf message validation driven by CEL. It is the companion to
[`wireform-proto`](../proto/) and is built on [`wireform-cel`](../cel/).

protovalidate expresses validation rules as CEL, both the standard
`buf.validate` annotations and arbitrary custom logic:

```proto
message User {
  string id         = 1 [(buf.validate.field).string.uuid = true];
  uint32 age        = 2 [(buf.validate.field).uint32.lte = 150];
  string email      = 3 [(buf.validate.field).string.email = true];
  string first_name = 4 [(buf.validate.field).string.max_len = 64];

  option (buf.validate.message).cel = {
    id: "first_name_requires_last_name"
    message: "last_name must be present if first_name is present"
    expression: "!has(this.first_name) || has(this.last_name)"
  };
}
```

## Key features

- **`Protovalidate.Library`**: the protovalidate CEL extension library
  registered onto a `CEL.Env`: `isEmail`, `isHostname`, `isHostAndPort`,
  `isIp`/`isIpPrefix`, `isUri`/`isUriRef`, `isNan`/`isInf`, and `unique`.
- **`Protovalidate.Format`**: the underlying pure `Text -> Bool` predicates
  (RFC-1034 hostnames, RFC-5321 mailboxes, IPv4/IPv6 + CIDR, host:port,
  RFC-3986 URIs).
- **`Protovalidate.Rules`**: the standard rules expressed as CEL (exactly as
  reference protovalidate does: each rule is a CEL expression over `this` and
  `rules`), plus a builder vocabulary.
- **`Protovalidate.Eval`**. The engine: bind each field value to `this` and
  its rule message to `rules`, evaluate the applicable standard + custom
  constraints, and collect `Violation`s (including nested-message and
  repeated-element paths).
- **`Protovalidate.Proto`**: a bridge that turns a `wireform-proto`
  `DynamicMessage` into the CEL value the engine consumes, given a small field
  schema.

## Quick start

A message is represented as a CEL map from field name to value. `validate`
uses the standard protovalidate CEL environment (base CEL plus the extension
library); `validateIn` lets you supply your own base environment.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Protovalidate
import CEL (Value (..), celMapFromList)

userRules :: MessageRules
userRules = messageRules
  [ ("id",         fieldRules KString [uuid])
  , ("age",        fieldRules KUint32 [lteV (VUInt 150)])
  , ("email",      fieldRules KString [email])
  , ("first_name", fieldRules KString [maxLen 64])
  ]
  [ either (error . show) id $
      mkConstraint "first_name_requires_last_name"
                   "last_name must be present if first_name is present"
                   "!has(this.first_name) || has(this.last_name)"
  ]

main :: IO ()
main = do
  let user = VMap (celMapFromList
        [ (VString "id",    VString "not-a-uuid")
        , (VString "age",   VUInt 200)
        , (VString "email", VString "alice@example.com")
        ])
  mapM_ print (validate user userRules)
  -- Violation {fieldPath = "id",  constraintId = "string.uuid", ...}
  -- Violation {fieldPath = "age", constraintId = "uint32.lte",  ...}
```

## Standard rules and custom CEL

The package implements the CEL-driven core of protovalidate. Both the standard
annotations and arbitrary custom logic are CEL, so they run through the same
engine. Covered standard rules include:

- numeric (`float`/`double`/`int*`/`uint*`/`sint*`/`fixed*`/`sfixed*`):
  `const`, `lt`/`lte`/`gt`/`gte`, `in`/`not_in`, and `finite` (float/double);
- `bool`: `const`;
- `string`: `const`, the length family
  (`len`/`min_len`/`max_len`/`min_bytes`/`max_bytes`/`len_bytes`),
  `pattern`/`prefix`/`suffix`/`contains`/`not_contains`, `in`/`not_in`, and the
  well-known formats (`email`, `hostname`, `ip`/`ipv4`/`ipv6` and the
  prefix/with-prefixlen variants, `uri`/`uri_ref`, `address`, `host_and_port`,
  `uuid`/`tuuid`);
- `bytes`: `const`, `len`/`min_len`/`max_len`, `prefix`/`suffix`/`contains`,
  `in`/`not_in`, `ip`/`ipv4`/`ipv6`;
- `timestamp`/`duration`: `const`, `lt`/`lte`/`gt`/`gte`, `in`/`not_in`, plus
  the time-relative timestamp rules `lt_now`/`gt_now`/`within`;
- `repeated`: `min_items`/`max_items`/`unique` (+ per-element `items` rules);
- `map`: `min_pairs`/`max_pairs`, plus per-key `map.keys` and per-value
  `map.values` sub-rules (built with `mapKeys`/`mapValues`); violations are
  reported at `field[key]`;
- `enum.defined_only` via `definedOnly`; oneof `required` via `oneofRequired`;
  `string.well_known_regex` via `wellKnownRegex`; `(buf.validate.predefined)`
  reusable constraints via `frPredefined`;
- field `required` and `ignore` (skip-on-empty); field- and message-level
  custom `cel`; nested-message recursion.

### Time-relative timestamps

`lt_now`/`gt_now`/`within` reference a `now` binding, so the engine needs a
clock. Use `validateAt :: Timestamp -> Value -> MessageRules -> [Violation]`,
which binds `now` to the supplied timestamp; the plain `validate` leaves `now`
unbound (those rules then surface as evaluation errors).

## Reading rules from annotations

Instead of writing `MessageRules` by hand, read them straight from a
`buf.validate`-annotated `.proto` (via `wireform-proto`'s IDL parser):

```haskell
case parseProtoRules protoSource of
  Right rulesByMessage ->
    let Just userRules = lookup "User" rulesByMessage
     in validate userMsg userRules
  Left err -> error (show err)
```

`parseProtoRules` understands the scalar/numeric/bool/bytes/enum/duration/
timestamp rules, `repeated` (incl. `repeated.items.*`) and `map` rules,
`required` / `ignore`, field- and message-level `cel`, and nested-message
validation. It also resolves the advanced rules from source:
`enum.defined_only` (declared enum value numbers → `this in [...]`),
`string.well_known_regex` (+ `strict`), `timestamp`/`duration` message-literal
bounds (and `timestamp.within`), and oneof `required`.

### …or from a compiled descriptor

protovalidate stores its rules as option *extensions*: extension #1159 on
`google.protobuf.FieldOptions` / `MessageOptions`. Because `wireform-proto`'s
`descriptor.proto` preserves unknown fields, those extension bytes survive
decoding, so rules can be read straight from a `FileDescriptorProto` (e.g. a
`protoc`-produced `FileDescriptorSet`):

```haskell
case messageRulesFromDescriptor fileDescriptorProto "acme.user.v1.User" of
  Right userRules -> validate userMsg userRules
  Left err        -> error (show err)
```

`fileRulesFromDescriptor` returns rules for every message in the file. The
`.proto` AST is the source of truth for the advanced extraction above; the
descriptor path covers the standard #1159 rules.

## Compiled vs. interpreted paths

The interpreted engine (`validate`/`validateIn`/`validateAt`) parses and walks
CEL at validation time. Two faster paths bake the work up front:

- **Compile-once typed validation.** `compileValidator :: MessageRules ->
  Validator` captures the CEL expressions and base environment once; the
  resulting `Validator` is reused across many messages. `ToCel` converts a
  typed Haskell record directly into CEL, so validation never decodes to a
  schemaless `DynamicMessage`:

  ```haskell
  data User = User { id :: Text, age :: Word32, email :: Text }
    deriving stock (Generic) deriving anyclass (ToCel)

  userValidator :: Validator
  userValidator = compileValidator userRules

  check :: User -> [Violation]
  check = validateValue userValidator
  ```

- **Compile-time validators.** `Protovalidate.TH.compileMessageValidator`
  reads a `.proto`'s `buf.validate` rules at compile time and emits a
  `Value -> [Violation]` in which every predicate, the standard rules (inlined
  to self-contained CEL, including `timestamp(..)`/`duration("..s")` literal
  bounds) and any custom `(buf.validate.field).cel`, is compiled to Haskell
  via `CEL.TH`. No runtime parsing, no AST walk:

  ```haskell
  {-# LANGUAGE TemplateHaskell #-}
  import Protovalidate
  import Protovalidate.TH (compileMessageValidator)
  import MyProtoSource (userProto)   -- a separate module (TH stage restriction)

  validateUser :: Value -> [Violation]
  validateUser = $(compileMessageValidator userProto "User")
  ```

  The compiled path covers the message's own fields + message-level CEL; the
  now-relative, map-key/value, and predefined rules still run through the
  interpreted engine, and nested-message/repeated recursion is not emitted yet.

## Refinement types

`Protovalidate.Refined` reifies rules as
[`refined`](https://hackage.haskell.org/package/refined) refinement types, so a
field's constraints can show up in its type. `refinedFieldType` turns a
`FieldRules` into the type expression a code generator would emit:

```haskell
refinedFieldType (fieldRules KString [minLen 3, maxLen 64])
-- Just "Refined (And (MinLen 3) (MaxLen 64)) Text"
```

Length/count/comparison rules become **native** `refined` predicates
(`MinLen`, `MaxLen`, `LenEq`, `Gt`, `Gte`, `Lt`, `Lte`, `ConstEq`). Everything
else (well-known string formats, regex patterns, and arbitrary custom `cel`)
becomes a **CEL-backed** `Cel` predicate that carries the CEL source at the
type level and runs it at validation time (memoized per expression text):

```haskell
refinedFieldType (fieldRules KString [email])
-- Just "Refined (Cel \"this.isEmail()\") Text"

refine "a@b.com" :: Either RefineException (Refined (Cel "this.isEmail()") Text)  -- Right
```

`CelWith tag expr` (with a `CelEnvironment tag` instance) runs in a
caller-supplied environment, so custom CEL *functions* can back a refinement
predicate too.

## Notable modules

| Module | Purpose |
|--------|---------|
| `Protovalidate` | Umbrella re-export: `validate`, `validateIn`, `validateAt`, `messageRules`, `fieldRules`, `mkConstraint`, the standard rule builders |
| `Protovalidate.Library` | protovalidate CEL extension functions onto a `CEL.Env` |
| `Protovalidate.Format` | Pure RFC predicates (hostname, mailbox, IP/CIDR, host:port, URI) |
| `Protovalidate.Rules` | Standard rules as CEL + builder vocabulary |
| `Protovalidate.Constraint` | The `Constraint` type and `mkConstraint` |
| `Protovalidate.Eval` | The violation-collecting engine |
| `Protovalidate.Violation` | The `Violation` result type |
| `Protovalidate.Schema` | `parseProtoRules` annotation extraction from `.proto` source |
| `Protovalidate.Descriptor` | `messageRulesFromDescriptor` / `fileRulesFromDescriptor` from a compiled `FileDescriptorProto` |
| `Protovalidate.Proto` | `DynamicMessage` → CEL bridge (`dynamicMessageToCel`) |
| `Protovalidate.Class` | Compile-once typed path: `compileValidator`, `validateValue`, `ToCel` |
| `Protovalidate.TH` | `compileMessageValidator` compile-time validators |
| `Protovalidate.Refined` | `refinedFieldType` + `refined` refinement-type predicates |
