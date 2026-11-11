# wireform-cel

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

> [!CAUTION]
> wireform is in heavy development and has not been published to Hackage yet. APIs may change.

A conformant Haskell implementation of Google's
[Common Expression Language (CEL)](https://github.com/google/cel-spec/blob/master/doc/langdef.md).
CEL is a non-Turing-complete, side-effect-free, strongly- and
dynamically-typed expression language used for policy, validation, and
filtering (IAM conditions, Envoy/xDS, protobuf field validation, …).

This package provides the lexer, recursive-descent parser, the dynamic
runtime [`Value`](src/CEL/Value.hs) model, and an evaluator with the full
standard library of operators, conversions, functions, and comprehension
macros.

Unlike most other packages in the
[wireform](https://github.com/iand675/wireform-) monorepo, `wireform-cel`
depends only on Hackage libraries, so it builds standalone.

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

### Compile-time compilation

For static CEL (known at compile time) there's no need to parse at runtime.
`CEL.TH` offers two levels (a CEL syntax error becomes a compile error in both):

- `[cel| … |]` / `compileCel` parse at compile time and splice the resulting
  `Expr` as a baked-in constant — no runtime parse; `evaluate` walks the AST
  once.
- `[celFn| … |]` / `compileCelFn` go further and emit the program as
  **Haskell**: every CEL node becomes a direct call to a `CEL.Eval`
  combinator, producing an `Env -> Either CelError Value` closure with no AST
  walk and no per-node dispatch at runtime — GHC optimizes it like any other
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

The runtime evaluator is built from the same combinators, so `compileExpr`
(reusable `Expr -> Env -> Either CelError Value`) and the compile-time path
share one definition of the language semantics.

## What's supported

- **Syntax**: the complete grammar — ternary `?:`, `||`/`&&`, relations and
  `in`, arithmetic, unary `-`/`!`, member selection / indexing / calls, list
  and map literals, and `Name{...}` message-literal syntax. Full literal
  lexis: decimal/hex integers, `u` unsigned suffix, floats, single/double and
  triple-quoted strings, raw (`r"..."`) strings, bytes (`b"..."`) literals,
  and the entire escape-sequence set with surrogate/range validation.
- **Values**: `int` (64-bit signed), `uint` (64-bit unsigned), `double`,
  `bool`, `string`, `bytes`, `null`, `list`, `map`, `type`, and the abstract
  `google.protobuf.Timestamp` / `google.protobuf.Duration`. Protobuf scalar
  wrapper literals evaluate to CEL primitives. Timestamp and duration
  literals follow the upstream conformance fixtures. Protobuf JSON literals
  (`Value`, `Struct`, `ListValue`) unpack to CEL values, maps, and lists.
- **Semantics**: the numeric number-line model (`1 == 1u == 1.0`,
  cross-type ordering), `NaN` that is never equal and always unordered,
  heterogeneous equality, overflow-checked arithmetic, and the commutative,
  error-absorbing `&&`/`||` operators.
- **Macros**: `has`, `all`, `exists`, `exists_one`, `map` (3- and 4-argument),
  and `filter`, with comprehension scoping.
- **Standard library**: `size`, `type`, `dyn`, the conversions
  (`int`/`uint`/`double`/`string`/`bool`/`bytes`/`duration`/`timestamp`), the
  string functions (`contains`, `startsWith`, `endsWith`, `matches`), and the
  timestamp/duration accessors (`getFullYear`, `getMonth`, `getDate`,
  `getHours`, `getMinutes`, `getSeconds`, `getMilliseconds`, `getDayOfWeek`,
  `getDayOfYear`, …). Timestamp accessors support UTC, fixed offsets, and
  named IANA timezones.

## Not yet supported

- General protocol-buffer **message** values (construction and field access).
- The optional static **type-checking** phase. CEL is dynamically typed;
  evaluation does not depend on it.

## Building and testing

`matches` uses [RE2](https://github.com/google/re2). The build needs the RE2
headers and library, discoverable as `re2` through `pkg-config`. Install
`libre2-dev` and `pkg-config` on Debian or Ubuntu, or use the repository's
`nix develop` shell.

```
cabal build wireform-cel
cabal test  wireform-cel
```

The default test suite (`test/`) has two parts: `Test.CEL.Conformance`
(example-based tests taken directly from the worked examples in the language
definition) and `Test.CEL.Properties` (Hedgehog properties for arithmetic,
ordering, and `size`).

### Upstream conformance suite

A second, opt-in test suite (`wireform-cel-conformance`) runs the official
[`cel-spec`](https://github.com/google/cel-spec) `tests/simple/testdata/*.textproto`
suite (matching the monorepo's `TOML_TEST_SUITE` / `YAML_TEST_SUITE` pattern).
Point `CEL_SPEC_DIR` at a checkout and run it:

```
git clone https://github.com/google/cel-spec
CEL_SPEC_DIR=$PWD/cel-spec cabal test wireform-cel:wireform-cel-conformance
```

The runner parses the textproto fixtures and compares each evaluation to its
expected value or error. An omitted result matcher expects boolean `true`.
It loads the checkout's proto2 and proto3 schemas to construct and decode
protobuf message fixtures, including declared proto2 extensions.
The runner lists excluded files for unsupported features such as the
`string_ext`, `math_ext`, `lists_ext`, and `sets_ext` libraries.

Result with cel-spec v0.26.0, including all 39 enum and 94 timestamp and duration cases:

```
TOTAL  pass=1890  skip=0  fail=0
```

These totals include binary `google.protobuf.Any` decoding and equality.
The runner reports excluded files separately.
