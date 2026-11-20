# wireform-protovalidate

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

> [!CAUTION]
> wireform is in heavy development and has not been published to Hackage yet. APIs may change.

[protovalidate](https://protovalidate.com/) for the wireform Protocol Buffers
stack: protobuf message validation driven by CEL. It is the companion to
[`wireform-proto`](../wireform-proto) and is built on
[`wireform-cel`](../wireform-cel).

protovalidate expresses validation rules as CEL — both the standard
annotations and arbitrary custom logic:

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

This package provides the pieces needed to enforce those rules in Haskell:

- **`Protovalidate.Library`** — the protovalidate CEL extension library
  registered onto a `CEL.Env`: `isEmail`, `isHostname`, `isHostAndPort`,
  `isIp`/`isIpPrefix`, `isUri`/`isUriRef`, `isNan`/`isInf`, and `unique`.
- **`Protovalidate.Format`** — the underlying pure `Text -> Bool` predicates
  (RFC-1034 hostnames, RFC-5321 mailboxes, IPv4/IPv6 + CIDR, host:port,
  RFC-3986 URIs).
- **`Protovalidate.Rules`** — the standard rules expressed as CEL (exactly as
  reference protovalidate does: each rule is a CEL expression over `this` and
  `rules`), plus a builder vocabulary.
- **`Protovalidate.Eval`** — the engine: bind each field value to `this` and
  its rule message to `rules`, evaluate the applicable standard + custom
  constraints, and collect `Violation`s (including nested-message and
  repeated-element paths).
- **`Protovalidate.Proto`** — a bridge that turns a `wireform-proto`
  `DynamicMessage` into the CEL value the engine consumes, given a small
  field schema.

## Example

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

A message is represented as a CEL map from field name to value. `validate`
uses the standard protovalidate CEL environment (base CEL plus the extension
library); `validateIn` lets you supply your own base environment.

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
validation. (`fileMessageRules` / `extractMessageRules` work on an
already-parsed `ProtoFile` / `MessageDef`.)

### …or from a compiled descriptor

protovalidate stores its rules as option *extensions* — extension #1159 on
`google.protobuf.FieldOptions` / `MessageOptions`. Because `wireform-proto`'s
`descriptor.proto` now preserves unknown fields, those extension bytes survive
decoding, so rules can be read straight from a `FileDescriptorProto` (e.g. a
`protoc`-produced `FileDescriptorSet`):

```haskell
case messageRulesFromDescriptor fileDescriptorProto "acme.user.v1.User" of
  Right userRules -> validate userMsg userRules
  Left err        -> error (show err)
```

`fileRulesFromDescriptor` returns rules for every message in the file. Custom
`cel`, `required`, and `ignore` are always read; the standard rule sets are
mapped for the common kinds (string / numeric / bool / bytes / repeated / map)
using the buf.validate v1 field numbers.

## Rules → OpenAPI / JSON Schema

`Protovalidate.OpenAPI` folds extracted rules into the OpenAPI 3.1 generator
(see [`wireform-connect`](../wireform-connect/)) as JSON Schema validation
keywords, so a `.proto`'s constraints carry through to the generated API doc:

```haskell
import Protovalidate.Schema (fileMessageRules)
import Protovalidate.OpenAPI (protovalidateSchemaOptions)
import Network.Connect.OpenAPI (connectOpenApiWith, defaultOpenApiOptions)

-- sopts :: SchemaOptions  (the Proto.JSONSchema annotator hook)
let sopts = protovalidateSchemaOptions (either (const []) id (fileMessageRules pf))
 in connectOpenApiWith sopts defaultOpenApiOptions [pf] [pf]
```

Or straight from the CLI: `wireform-gen openapi -i x.proto --validate`.

Rules with a faithful JSON Schema equivalent become standard keywords
(`string.min_len`→`minLength`, `pattern`→`pattern`, `string.email`→`format`,
numeric `gte`/`lte`→`minimum`/`maximum`, `repeated.min_items`→`minItems`,
`repeated.unique`→`uniqueItems`, `map.min_pairs`→`minProperties`,
`required`→the object's `required` list). Custom CEL becomes an `x-cel` array
of `{id, message, expression}`; everything else is preserved losslessly under
an `x-protovalidate` object. `fieldConstraintsFor` exposes the per-field
mapping directly.

## Compile-time validators

`Protovalidate.TH.compileMessageValidator` reads a `.proto`'s `buf.validate`
rules at compile time and emits a `Value -> [Violation]`. It uses the same
rule selection, environment bindings, and traversal as runtime validation.
Standard and custom predicates compile from their CEL ASTs to Haskell via
`CEL.TH`, with rule values embedded at their original precision.

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Protovalidate
import Protovalidate.TH (compileMessageValidator)
import MyProtoSource (userProto)   -- a separate module (TH stage restriction)

validateUser :: Value -> [Violation]
validateUser = $(compileMessageValidator userProto "User")
```

`messageValidatorE` also accepts a parsed `MessageRules` directly, including
predefined rules and nested, repeated, and map rules.

## Validating typed messages (no dynamic round trip)

`compileValidator` compiles a `MessageRules` once — the CEL expressions and
base environment are captured up front — and the resulting `Validator` can be
reused across many messages. `ToCel` converts a typed Haskell record directly
into CEL, so validation never decodes to a schemaless `DynamicMessage`:

```haskell
data User = User { id :: Text, age :: Word32, email :: Text }
  deriving stock (Generic) deriving anyclass (ToCel)

userValidator :: Validator
userValidator = compileValidator userRules

check :: User -> [Violation]
check = validateValue userValidator
```

`Protovalidate.Proto.dynamicMessageToCel` remains available for when you only
have a schemaless `wireform-proto` `DynamicMessage`.
Generic record conversion omits `Nothing` fields. A present value such as
`Just VNull` still counts as present.

## Refinement types

`Protovalidate.Refined` reifies rules as
[`refined`](https://hackage.haskell.org/package/refined) refinement types, so a
field's constraints can show up in its type. `refinedFieldType` turns a
`FieldRules` into the type expression a code generator would emit:

```haskell
refinedFieldType (fieldRules KString [minLen 3, maxLen 64])
-- Just "Refined (And (MinLen 3) (MaxLen 64)) Text"
```

Two flavors of predicate are produced:

- **Native predicates** for length/count/comparison rules, via the aliases
  `MinLen`, `MaxLen`, `LenEq`, `Gt`, `Gte`, `Lt`, `Lte`, `ConstEq` (type-level
  naturals). These are ordinary `refined` predicates:

  ```haskell
  refine "abc" :: Either RefineException (Refined (MinLen 3) Text)  -- Right
  ```

- **CEL-backed predicates** for everything else — the well-known string
  formats, regex patterns, and arbitrary `(buf.validate.field).cel`
  expressions — via the `Cel` predicate, which carries the CEL source at the
  type level and runs it at validation time. This is how **custom predicates
  also become refinement types**:

  ```haskell
  refinedFieldType (fieldRules KString [email])
  -- Just "Refined (Cel \"this.isEmail()\") Text"

  refine "a@b.com" :: Either RefineException (Refined (Cel "this.isEmail()") Text)  -- Right
  ```

  `CelWith tag expr` (with a `CelEnvironment tag` instance) runs in a
  caller-supplied environment, so custom CEL *functions* can back a refinement
  predicate too.

  The `Cel`/`CelWith` predicates compile their CEL at most once per process
  (memoized by the expression text), not on every `refine`. For CEL known at
  compile time, prefer `CEL.TH`'s `[cel| … |]`, which compiles it at compile
  time into a baked-in constant.

## Scope

This package implements the CEL-driven core of protovalidate: the extension
function library, the standard rules as CEL, the violation-collecting engine
(with custom field- and message-level CEL), annotation extraction from `.proto`
sources, and a compile-once typed validation path.

Covered standard rules:

- numeric (`float`/`double`/`int*`/`uint*`/`sint*`/`fixed*`/`sfixed*`): `const`,
  `lt`/`lte`/`gt`/`gte`, `in`/`not_in`, and `finite` (float/double);
- `bool`: `const`;
- `string`: `const`, `len`/`min_len`/`max_len`/`min_bytes`/`max_bytes`/`len_bytes`,
  `pattern`/`prefix`/`suffix`/`contains`/`not_contains`, `in`/`not_in`, and the
  well-known formats `email`/`hostname`/`ip`/`ipv4`/`ipv6`/`ip_prefix`/
  `ipv4_prefix`/`ipv6_prefix`/`ip_with_prefixlen`/`ipv4_with_prefixlen`/
  `ipv6_with_prefixlen`/`uri`/`uri_ref`/`address`/`host_and_port`/`uuid`/`tuuid`;
- `bytes`: `const`, `len`/`min_len`/`max_len`, `prefix`/`suffix`/`contains`,
  `in`/`not_in`, `ip`/`ipv4`/`ipv6`;
- numeric `timestamp`/`duration`: `const`, `lt`/`lte`/`gt`/`gte`, `in`/`not_in`;
  plus the time-relative timestamp rules `lt_now`/`gt_now`/`within` (see below);
- `repeated`: `min_items`/`max_items`/`unique` (+ per-element `items` rules);
- `map`: `min_pairs`/`max_pairs`, plus per-key `map.keys` and per-value
  `map.values` sub-rules (built with `mapKeys`/`mapValues`, or extracted from a
  `.proto` map field). Violations are reported at `field[key]`;
- `enum.defined_only` via the `definedOnly` builder (you supply the enum's
  declared numbers);
- oneof `required` via the `oneofRequired` builder (a message-level
  `has(this.f1) || …` constraint), also extracted from `(buf.validate.oneof)`;
- `string.well_known_regex` via the `wellKnownRegex` builder (HTTP header
  name/value, RE2/POSIX-compatible);
- `(buf.validate.predefined)` reusable constraints via `frPredefined`: a CEL
  constraint plus the `rule` value bound for it (so the expression can reference
  both `this` and `rule`);
- field `required` and `ignore` (skip-on-empty); field- and message-level
  custom `cel`; nested-message recursion.

### Time-relative timestamps

`lt_now`/`gt_now`/`within` reference a `now` binding, so the engine cannot
evaluate them without a clock. Use `validateAt :: Timestamp -> Value ->
MessageRules -> [Violation]`, which binds `now` to the supplied timestamp; the
plain `validate` leaves `now` unbound (those rules then surface as evaluation
errors). `within` compares `this - now` and `now - this` against the configured
`VDuration`.

`.proto` extraction (`parseProtoRules`) now covers the advanced rules too:

- `enum.defined_only` resolves the enum's declared value numbers from the file
  (top-level or nested) and emits `this in [...]` — scalar and `repeated.items`;
- `string.well_known_regex` resolves the `KnownRegex` enum (+ `strict`), scalar
  and `repeated.items`;
- `timestamp`/`duration` `const`/`lt`/`lte`/`gt`/`gte` (and `timestamp.within`)
  bounds are decoded from their `{seconds:.., nanos:..}` message literals;
- `map.keys`/`map.values` sub-rules and oneof `required`.
- Field rules use protobuf presence. Omitted implicit fields are validated
  at their defaults, and `required` rejects implicit defaults and empty
  collections. An explicitly present optional or oneof field can satisfy
  `required` with its default value.
- Source and descriptor validators expose a schema-backed protobuf message
  to CEL. Selecting an unset field returns its protobuf default, including
  declared proto2 defaults. The `has()` macro still tests presence.
  Nested message shapes come from the supplied file.

Remaining caveats:

- time-relative timestamp rules need a `now` binding. Use `validateAt` to
  supply it.
- The compiled-`FileDescriptorProto` path (`Protovalidate.Descriptor`) decodes
  supported scalar rules from option extension #1159, including packed
  membership lists and timestamp and duration bounds. It also decodes
  collection subrules and required protobuf oneofs. Both source and
  descriptor validation follow same-file message references, including
  recursive references through collections.
- Both loaders accept `cel_expression` shorthand on messages, fields, and
  collection members. The expression serves as the rule ID.
- Both loaders enforce `(buf.validate.message).oneof` groups using protobuf
  presence, including for repeated and map fields. Group members skip unset
  values by default and can override that behavior with `ignore`.
- Both loaders support `enum.defined_only` and strict or loose HTTP header
  regexes on scalar fields and collection members. Enum rules require the
  enum definition in the input file.

## Building and testing

```
cabal build wireform-protovalidate
cabal test  wireform-protovalidate
```

`Test.Protovalidate.Format` exercises the format predicates;
`Test.Protovalidate.Validation` runs end-to-end validation (standard rules,
custom CEL, message-level CEL, nested messages, repeated fields).
