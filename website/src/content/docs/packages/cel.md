---
title: CEL (Common Expression Language)
description: "A conformant Haskell implementation of Google's Common Expression Language: lexer, parser, evaluator, comprehension macros, standard library, and compile-time compilation."
sidebar:
  order: 70
---

`wireform-cel` is a conformant Haskell implementation of Google's
[Common Expression Language (CEL)](https://github.com/google/cel-spec/blob/master/doc/langdef.md).
CEL is a non-Turing-complete, side-effect-free, strongly- and
dynamically-typed expression language used for policy, validation, and
filtering: IAM conditions, Envoy/xDS, protobuf field validation, and more.

The package provides the lexer, a recursive-descent parser, the dynamic
runtime `Value` model, and an evaluator with the full standard library of
operators, conversions, functions, and comprehension macros.

Unlike most packages in the wireform monorepo, `wireform-cel` depends only on
Hackage libraries (no other wireform format packages beyond `wireform-core`),
so it builds standalone. It is the engine that
[`wireform-protovalidate`](../protovalidate/) is built on.

## Key features

- **Complete grammar**: ternary `?:`, `||`/`&&`, relations and `in`,
  arithmetic, unary `-`/`!`, member selection / indexing / calls, list and map
  literals, and `Name{...}` message-literal syntax. Full literal lexis covers
  decimal/hex integers, the `u` unsigned suffix, floats, single/double and
  triple-quoted strings, raw (`r"..."`) strings, bytes (`b"..."`) literals, and
  the entire escape-sequence set with surrogate/range validation (including
  backtick-escaped identifiers).
- **Number-line numeric semantics**: `1 == 1u == 1.0` with cross-type
  ordering, `NaN` that is never equal and always unordered, heterogeneous
  equality, and overflow-checked arithmetic.
- **Error-absorbing logic**: commutative `&&`/`||` that absorb errors per the
  CEL spec.
- **Comprehension macros**: `has`, `all`, `exists`, `exists_one`, `map`
  (3- and 4-argument), and `filter`, plus the two-variable `macros2` forms
  `all` / `exists` / `existsOne` / `transformList` / `transformMap`, with
  comprehension scoping.
- **Standard library**: `size`, `type`, `dyn`, the conversions
  (`int`/`uint`/`double`/`string`/`bool`/`bytes`/`duration`/`timestamp`), the
  string functions (`contains`, `startsWith`, `endsWith`, `matches`), and the
  `Timestamp` / `Duration` accessors (`getFullYear`, `getMonth`, `getDate`,
  `getHours`, …, `getDayOfWeek`, `getDayOfYear`). Named IANA timezones are
  supported via the `tz` library, alongside `UTC` and fixed `±HH:MM` offsets.

## Quick start

```haskell
{-# LANGUAGE OverloadedStrings #-}
import CEL

main :: IO ()
main = do
  -- Self-contained expressions:
  print (run emptyEnv "1 + 2 * 3")               -- Right (VInt 7)
  print (run emptyEnv "[1, 2, 3].map(x, x * x)") -- Right (VList [VInt 1,VInt 4,VInt 9])
  print (run emptyEnv "'foobar'.matches('o+b')") -- Right (VBool True)

  -- Bind variables into the environment:
  let env = bindAll [("user", VMap (celMapFromList [("age", VInt 30)]))] emptyEnv
  print (run env "user.age >= 18")               -- Right (VBool True)
```

`compile` parses to an `Expr` you can evaluate repeatedly with `evaluate`;
`run` is the parse-and-evaluate convenience. Errors are returned as
`Left CelError`.

## Compile-time compilation

For CEL that is known at compile time there is no need to parse at runtime.
`CEL.TH` offers two levels; a CEL syntax error becomes a compile error in
both:

- `[cel| … |]` / `compileCel` parse at compile time and splice the resulting
  `Expr` as a baked-in constant. No runtime parse; `evaluate` walks the AST
  once.
- `[celFn| … |]` / `compileCelFn` go further and emit the program as
  **Haskell**: every CEL node becomes a direct call to a `CEL.Eval`
  combinator, producing an `Env -> Either CelError Value` closure with no AST
  walk and no per-node dispatch at runtime. GHC optimizes it like any other
  Haskell.

```haskell
{-# LANGUAGE QuasiQuotes #-}
import CEL
import CEL.TH (cel, celFn)

program :: Expr
program = [cel| [1, 2, 3].map(x, x * x) |]   -- parsed at compile time

-- fully compiled to Haskell; reads variables from the environment:
predicate :: Env -> Either CelError Value
predicate = [celFn| this.size() >= 3 && this.startsWith('x') |]
```

The runtime evaluator is built from the same per-node combinators, so
`compileExpr` (a reusable `Expr -> Env -> Either CelError Value`) and the
compile-time path share one definition of the language semantics.

## Conformance

The default test suite (`test/`) pairs `Test.CEL.Conformance` (example-based
tests taken from the worked examples in the language definition) with
`Test.CEL.Properties` (Hedgehog properties for arithmetic, ordering, and
`size`).

An opt-in suite (`wireform-cel-conformance`) runs the official
[`cel-spec`](https://github.com/google/cel-spec)
`tests/simple/testdata/*.textproto` suite, following the monorepo's
`TOML_TEST_SUITE` / `YAML_TEST_SUITE` pattern. Point `CEL_SPEC_DIR` at a
checkout and run it:

```bash
git clone https://github.com/google/cel-spec
CEL_SPEC_DIR=$PWD/cel-spec cabal test wireform-cel:wireform-cel-conformance
```

Over the core language files (everything except the extension libraries, the
protobuf-enum file, and the type-deduction/unknown-tracking files) the current
result is:

```
TOTAL  pass=1124  skip=128  fail=0
```

All 128 skips are protocol-buffer-message cases (construction, field access,
wrapper/`Any`/`Struct` conversions). Protobuf **message** values and the
optional static type-checking phase are the documented gaps; CEL is
dynamically typed, so evaluation does not depend on the type-checker.

## Notable modules

| Module | Purpose |
|--------|---------|
| `CEL` | Umbrella module: `run`, `compile`, `evaluate`, `emptyEnv`, `bind`, `bindAll`, `Value`, `CelError` |
| `CEL.Value` | The dynamic runtime `Value` model (`VInt`, `VUInt`, `VDouble`, `VString`, `VList`, `VMap`, …) and `celMapFromList` |
| `CEL.Syntax` | The `Expr` AST |
| `CEL.Parser` | Lexer + recursive-descent parser |
| `CEL.Eval` | Per-node combinator evaluator (`compileExpr`) |
| `CEL.Stdlib` | Standard-library operators, conversions, string/regex functions, timestamp/duration accessors (named IANA timezones via `tz`) |
| `CEL.Environment` | The `Env` of variable bindings |
| `CEL.Error` | The `CelError` type |
| `CEL.TH` | Compile-time compilation: `[cel\| … \|]` / `compileCel` (baked `Expr`) and `[celFn\| … \|]` / `compileCelFn` (compile-to-Haskell) |
