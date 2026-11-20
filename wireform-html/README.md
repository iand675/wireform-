# wireform-html

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)


> [!CAUTION]
> wireform is in heavy development and has not been published to Hackage yet. APIs may change.

HTML5 for Haskell. A spec-compliant tokenizer and tree builder
([`HTML.Parse`](src/HTML/Parse.hs)), a typed DOM
([`HTML.DOM`](src/HTML/DOM.hs)), CSS selectors with `querySelector` /
`querySelectorAll` ([`HTML.Selector`](src/HTML/Selector.hs)), a
streaming HTML rewriter modeled after Cloudflare's `lol-html`
([`HTML.Rewriter`](src/HTML/Rewriter.hs)), an annotation-driven
Template Haskell deriver
([`HTML.Derive`](src/HTML/Derive.hs)), and a SIMD-accelerated
serializer that escapes only the bytes that need escaping. Backed
by a hand-tuned C scanning layer in
[`cbits/html_scan.c`](cbits/html_scan.c) using vendored simde for
SSE2 / AVX2 / NEON portability.

HTML5 is a parser specification more than it is a markup language:
the spec defines a tokenizer state machine and a tree-construction
algorithm so precise that the entire web's worth of malformed,
unclosed, mis-nested input has a single canonical interpretation.
This package implements that algorithm faithfully (the html5lib-test
tree-construction suite passes 1779 / 1779), then layers the working
surfaces on top: a regular DOM for inspection, CSS selectors for
querying, and a streaming rewriter for transformation.

This package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo and shares its allocation primitives, annotation deriver, and
testing discipline with every other format.

Named references use the complete WHATWG table (2,231 semicolon and legacy
forms), including multi-codepoint replacements and attribute-context rules.
The pinned source is `data/entities.json`; regenerate offline with
`python3 scripts/regen-html-entities.py` from the repository root, or pass
`--refresh` to fetch the official table before regenerating.

## Install

```cabal
build-depends:
  base,
  wireform-html,
  wireform-derive,    -- only if you want the cross-format annotation deriver
```

The package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo. Clone the repo and `cabal build wireform-html` to compile
locally. The C scanner builds with `-O3 -march=native` and the
vendored simde headers under `cbits/simde` cover SSE2 / AVX2 / NEON
portability automatically. Compiling Haskell with the LLVM backend
(`-fllvm`) is a substantial improvement on this package in
particular: about 38% throughput on the parser hot loops on
aarch64.

The package requires `text >= 2.1` because `HTML.Selector` pattern-
matches the underlying `ByteArray` constructor, which only became a
re-export of `Data.Array.Byte.ByteArray` in `text` 2.1.

## Hello world

Parse a small HTML document, run a CSS selector, print the matched
text:

```haskell
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Char8 as BS8
import qualified HTML.DOM as DOM

main :: IO ()
main = do
  let bytes = BS8.pack
        "<html><body>\
        \<h1 id=\"hero\">Hello, wireform</h1>\
        \<ul><li>one</li><li>two</li></ul>\
        \</body></html>"
      doc = DOM.parseDocument bytes
      hero = DOM.querySelector (DOM.documentElement doc) "h1#hero"
      items = DOM.querySelectorAll (DOM.documentElement doc) "ul > li"
  case hero of
    Just node -> TIO.putStrLn (DOM.nodeTextContent node)
    Nothing   -> putStrLn "no hero"
  mapM_ (TIO.putStrLn . DOM.nodeTextContent) items
```

For typed records, `HTML.Class` provides the same `ToHTML` /
`FromHTML` shape as the other format packages, with `encodeHTMLTyped`
/ `decodeHTMLTyped` as the entry points.

## What's in here

| Module           | Role                                                      |
|------------------|-----------------------------------------------------------|
| `HTML.Value`     | Dynamic HTML AST: `HTMLNode` (element / text / comment / doctype), `HTMLDocument`, attributes, `Doctype` |
| `HTML.Parse`     | HTML5 tokenizer and tree builder. `parseHTML`, `parseHTMLFragment`, `parseHTMLNodes`. Plus low-level: `tokenizeBS`, `tokenizeBSIO`, the incremental `TreeBuilder` interface (`newTreeBuilder`, `processToken`, `finishDocument`), and the raw event tokenizer (`tokenizeRawEventsIO`). |
| `HTML.Encode`    | SIMD-accelerated serializer. Bulk-copies clean byte ranges and only branches at escapable positions. |
| `HTML.Encoding`  | The `Encoding` builder type used by `ToHTML` instances    |
| `HTML.Class`     | Public `ToHTML` / `FromHTML` typeclasses + `encodeHTMLTyped` / `encodeHTMLTypedDirect` / `decodeHTMLTyped` |
| `HTML.Derive`    | `deriveHTML` Template Haskell entry point                 |
| `HTML.TagId`     | The `TagId` enumeration: a finite set of HTML tag names interned at the C layer for `O(1)` `case` dispatch instead of `Text` equality |
| `HTML.DOM`       | Typed DOM API: `parseDocument`, `Document`, `Node`, `documentElement`, `documentDoctype`, `nodeTextContent`, `querySelector`, `querySelectorAll` (DOM-spec compliant). Streaming: `streamHTML`, `streamHTMLEvents`, `newStreamParser`, `feedChunk`, `finishStream`. |
| `HTML.Selector` | CSS selector parser and matcher. `parseSelector :: Text -> Either SelectorError Selector`, plus the `Selector` AST (`ComplexSelector`, `Combinator`, `CompoundSelector`, `TypeSel`, `SubSel`). Covers the level 4 selector subset that's actually implementable against a static DOM. |
| `HTML.Rewriter` | Streaming, allocation-light HTML rewriter modeled after [`lol-html`](https://github.com/cloudflare/lol-html). Builder DSL for selector-keyed handlers (`onElement`, `onText`, `onComment`, `onDoctype`, `onEndTag`); mutable handles for in-place edits (`setTagName`, `setElemAttr`, `removeElemAttr`, `beforeElement`, `appendToElement`, `replaceElement`, `removeChildren`, `setInnerContent`, `replaceTextChunk`, ...); push-based runner (`rewrite`, `feedRewriter`, `finishRewriter`). |

## Parser

The parser implements the full HTML5 tree-construction algorithm:
every insertion mode, foster-parenting, the adoption agency
algorithm, foreign content (SVG / MathML), template contents, raw-text
elements (`<script>` / `<style>` / `<plaintext>`), RCDATA elements
(`<title>` / `<textarea>`), and the implicit-tag-closing rules.

```haskell
import qualified HTML.Parse as P

let doc = P.parseHTML bytes
```

Conformance is verified against the upstream
[html5lib-tests](https://github.com/html5lib/html5lib-tests) tree-
construction suite: 1779 / 1779 tests pass.

For incremental parsing of streamed input:

```haskell
import qualified HTML.DOM as DOM

p <- DOM.newStreamParser
_ <- DOM.feedChunk p chunk1
_ <- DOM.feedChunk p chunk2
DOM.finishStream p
```

## DOM and CSS selectors

`HTML.DOM` exposes a typed DOM (`Document`, `Node`, `HTMLDocument`,
`HTMLNode`) plus the working query surface borrowed from the WHATWG
DOM spec:

```haskell
querySelector    :: Node -> Text -> Maybe Node
querySelectorAll :: Node -> Text -> [Node]
```

Both accept any selector `HTML.Selector.parseSelector` can parse.
The implementation walks descendants in document order, returning the
first match (or all matches), with selector matching driven by the
`HTML.Selector` matcher.

`HTML.Selector` parses the level-4 CSS selector grammar that's
meaningful against a static DOM: type / class / ID selectors,
attribute selectors with operators (`[name="x"]`, `[name~="x"]`,
`[name|="x"]`, `[name^="x"]`, `[name$="x"]`, `[name*="x"]`),
descendant / child / sibling combinators, the negation pseudo-class
(`:not`), `:is` / `:where` / `:matches`, the structural pseudo-classes
(`:nth-child`, `:nth-of-type`, `:first-child`, `:last-child`, etc.),
and the link / form pseudo-classes that have a static interpretation.
Stateful pseudo-classes (`:hover`, `:focus`) are parsed but never
match against a non-interactive DOM, which is the right behavior.

```haskell
import qualified HTML.Selector as Sel

case Sel.parseSelector "ul.menu > li:nth-child(odd) > a[href^=\"/docs\"]" of
  Right sel -> ...
  Left  err -> ...
```

## Streaming HTML rewriter

`HTML.Rewriter` is the working surface most production HTML
processing needs: register handlers keyed by CSS selector, feed bytes
in, get rewritten bytes out, never materialise a full DOM.

The shape mirrors Cloudflare's [`lol-html`](https://github.com/cloudflare/lol-html):

```haskell
import qualified HTML.Rewriter as R

rw <- R.buildRewriter $ do
  R.onElement "a[href]" $ \el -> do
    href <- R.getElemAttr el "href"
    case href of
      Just h  -> R.setElemAttr el "href" (rewriteLink h)
      Nothing -> pure ()
  R.onText "title" $ \chunk -> do
    txt <- R.getTextContent chunk
    R.replaceTextChunk chunk (T.toUpper txt) R.HTMLContent

R.rewrite rw inputBytes
```

Selectors used as rewriter keys are restricted to the subset that can
be evaluated incrementally during tokenization, without backtracking
or full-document context. `HTML.Selector.isRewriterCompatible` tells
you up front whether a parsed selector qualifies; `lol-html` uses the
same restriction for the same reason.

For chunked input the push-based interface is:

```haskell
st <- R.newRewriterState rw outputCallback
mapM_ (R.feedRewriter st) chunks
R.finishRewriter st
```

## Annotation-driven deriving

`HTML.Derive` consumes the cross-format `Wireform.Derive.Modifier`
vocabulary from [`wireform-derive`](../wireform-derive/README.md). HTML
also needs a per-field "is this an attribute or a child node?" choice,
which lives under the `HtmlFieldOpt` `BackendModifier` extension:

```haskell
{-# LANGUAGE TemplateHaskell #-}

import qualified HTML.Derive as DHTML
import Wireform.Derive (extension)
import HTML.Derive (HtmlFieldOpt (..))

data Card = Card
  { cardId    :: !Text
  , cardTitle :: !Text
  } deriving stock (Show, Eq, Generic)

{-# ANN type Card ("Card" :: String) #-}
{-# ANN cardId    (extension AsAttr) #-}
{-# ANN cardTitle (extension AsChild) #-}

DHTML.deriveHTML ''Card
```

## Performance

The serializer scans output text in 16-byte SIMD chunks, bulk-copying
clean ranges and only branching at characters that need entity
escaping (`<`, `>`, `&`, `"`). `HTML.Parse`'s tokenizer uses the same
SIMD scanners (under `cbits/html_scan.c`) for text, attribute, and
raw-text inputs. Tag names go through `HTML.TagId`, a static
enumeration interned at the C layer so `case` dispatch on a
recognized tag is integer-fast instead of `Text`-equality slow.

Current numbers (29.5 KB document, single thread, GHC 9.8 with
`-fllvm` on aarch64):

- Full parse + tree construction: about 400 MB/s.
- Tree build only: about 470 MB/s.
- Tokenize only: about 1.5 GB/s.

Conformance: 1779 / 1779 html5lib tree-construction tests pass.

## Testing

The per-format Hedgehog + HUnit suite lives in `test/`:

```bash
cabal test wireform-html:wireform-html-derive-test
```

The umbrella package's `test-interop/` directory has the larger
corpus-driven suites:

- `html5lib-test`: the upstream html5lib-tests tree-construction
  suite (1779 cases).
- `html-dom-test`: DOM-API conformance.
- `wpt-selector-test`: Web Platform Tests CSS selector matcher
  cases.
- `html-rewriter-test`: rewriter round-trip cases drawn from
  `lol-html`'s test fixtures.

Run them from the repo root:

```bash
cabal test html5lib-test html-dom-test wpt-selector-test html-rewriter-test
```

## Benchmarks

A criterion / standalone harness in
[`bench/HTMLBench.hs`](../bench/HTMLBench.hs) (in the umbrella package)
exercises tokenizer, tree builder, and serializer hot loops:

```bash
cabal bench html-bench
```

The comparison currently lives against itself across input shapes
(small / medium / large pages, attribute-heavy / text-heavy /
script-heavy). For external comparisons:

- Haskell:
  [`html-conduit`](https://hackage.haskell.org/package/html-conduit)
  and [`tagsoup`](https://hackage.haskell.org/package/tagsoup) are
  the established Hackage HTML libraries; both are based on a
  permissive parser, not the WHATWG algorithm.
- Rust: [`html5ever`](https://crates.io/crates/html5ever) (the
  Servo / Mozilla reference implementation; arguably the canonical
  HTML5 implementation outside browsers) and
  [`lol-html`](https://github.com/cloudflare/lol-html) (the streaming
  rewriter `HTML.Rewriter` is modeled after).
- C++: Google's [`gumbo-parser`](https://github.com/google/gumbo-parser).

There's also a standalone profiling executable in
[`bench/ProfileRewriter.hs`](../bench/ProfileRewriter.hs) for cost-
centre analysis of the rewriter under `+RTS -p`. Build with the
`+profile` flag in the umbrella `wireform.cabal`.

> Numbers TBD: run `cabal bench html-bench` and drop a results table in.

## License

BSD-3-Clause. The vendored [simde](https://github.com/simd-everywhere/simde)
headers under `cbits/simde/` carry their own MIT license.

## References

- [HTML Standard (WHATWG)](https://html.spec.whatwg.org/) (the living spec)
- [HTML5 tree-construction algorithm](https://html.spec.whatwg.org/multipage/parsing.html#tree-construction)
- [Selectors Level 4](https://www.w3.org/TR/selectors-4/)
- [DOM Standard (WHATWG)](https://dom.spec.whatwg.org/) (`querySelector` semantics)
- [`lol-html` design notes](https://blog.cloudflare.com/html-parsing-1/) (the streaming rewriter shape)
- [html5lib-tests](https://github.com/html5lib/html5lib-tests) (the conformance corpus)
