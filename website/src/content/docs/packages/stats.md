---
title: wireform-stats
description: "Internal monorepo tooling that regenerates per-package README AUTOGEN regions from in-tree test, coverage, and benchmark data."
sidebar:
  order: 60
---

`wireform-stats` is internal tooling for the wireform monorepo. It
walks the per-package `wireform-*/README.md` files, finds the AUTOGEN
marker regions, and rewrites the body of each from in-tree test,
coverage, and benchmark data. It renders both markdown tables and SVG
bar charts (light + dark variants) so the READMEs stay useful without a
JS-rendered build step.

It is **not** a Hackage release. It lives in the monorepo because it
dogfoods [`wireform-xml`](../xml/) (the SVG charts are emitted via the
XML encoder, since SVG is XML) and runs against in-tree benchmark and
test outputs.

## Marker grammar

Every managed README region is wrapped in a paired HTML comment:

```markdown
<!-- BEGIN_AUTOGEN <key> -->
... content owned by the regen tool ...
<!-- END_AUTOGEN <key> -->
```

The key is a free-form identifier. Anything outside the markers is
hand-edited and never touched; anything inside is owned by the regen
tool and replaced wholesale on every run. (See the "Per-package README
AUTOGEN regions" section of `AGENTS.md` for the repo-wide rule and the
CI gate that enforces it.)

Defined keys:

| Key | Body |
|-----|------|
| `tests` | One-line summary: total / passing / failures / errors / skipped / wall time |
| `coverage` | One-line summary: top-level expressions, alternatives, and declaration percentages |
| `coverage:table` | Per-module expressions-used table (same source as `coverage`) |
| `bench:<id>` | A `<picture>` element referencing two SVGs (light + dark) plus a markdown table and caption |

Adding a new benchmark means dropping a `BenchSummary` JSON into
`wireform-<pkg>/bench-results/summary/<id>.json` and a matching
`<!-- BEGIN_AUTOGEN bench:<id> --><!-- END_AUTOGEN bench:<id> -->`
pair somewhere in the README; the regen tool figures out the rest.

## Docs site wiring

The same `bench:<id>` markers also drive **this docs site**, so the
per-package pages and the READMEs never disagree about benchmark
numbers. `regen-stats render` (and `check`) walks
`website/src/content/docs/packages/<name>.md` alongside
`wireform-<name>/README.md` and rewrites any `bench:<id>` regions it
finds there from the very same `bench-results/summary/<id>.json`
files. A package `wireform-<name>` maps to the `<name>.md` docs page
(e.g. `wireform-cbor` -> `cbor.md`); pages without a matching
summary (such as `html.md`, whose throughput numbers come from a
custom harness rather than criterion) are left untouched.

The one difference is the chart. The READMEs use a `<picture>`
element pointing at two committed SVG files (light + dark), which
works on github.com. A plain-markdown Starlight page can't reference
those files without a base-aware URL into `public/`, so the docs
renderer instead **inlines a single, self-contained SVG** that
stacks a light and a dark chart and toggles between them with an
embedded `@media (prefers-color-scheme: …)` rule. The bytes drop
straight into the page with no asset wiring and adapt to the
reader's theme at view time. That renderer is
`Wireform.Stats.SVG.renderBarChartAdaptive`.

## Notable modules

| Module | Role |
|--------|------|
| `Wireform.Stats.Marker` | Marker grammar + region rewriter |
| `Wireform.Stats.SVG` | SVG bar chart renderer with light / dark themes (uses `wireform-xml` for the DOM) |
| `Wireform.Stats.Table` | Markdown table renderer with column alignment |
| `Wireform.Stats.Bench` | Benchmark types: criterion JSON parser, `BenchSummary` JSON I/O, distillation and render-input conversion |
| `Wireform.Stats.Test` | JUnit XML parser (consumes tasty's `--xml=...` output via `wireform-xml`) |
| `Wireform.Stats.Coverage` | `hpc report --per-module` text parser |
| `Wireform.Stats.Shields` | shields.io endpoint badge JSON emitter (tests + coverage badges) |

## The `regen-stats` executable

The package ships a `regen-stats` executable with subcommands `render`,
`render-bench-charts`, `badges`, and `check`. Run it with `--help` for
the full surface. A typical refresh:

```bash
# Re-render the README SVG charts from the committed benchmark summaries.
cabal run wireform-stats:exe:regen-stats -- render-bench-charts

# Stitch everything into the per-package READMEs *and* this docs site.
cabal run wireform-stats:exe:regen-stats -- render

# Refresh the shields.io endpoint badge JSON files.
cabal run wireform-stats:exe:regen-stats -- badges
```

`render` rewrites both the READMEs and the docs-site package pages in
one pass; the docs-site charts are inlined at render time, so
`render-bench-charts` only needs to run for the README's two-file
SVGs. The `--docs-dir` flag points the docs pass at a different
directory if the site ever moves.

Distilling each criterion JSON into a `BenchSummary` is intentionally
manual: criterion's output is wider than the README needs, and you
almost always want to eyeball the numbers before committing them. Once
you have a summary you trust, every subsequent step is deterministic and
re-runnable.

## CI gate

A workflow runs `regen-stats check` on every PR and fails the build if
any README's **or docs page's** AUTOGEN regions are stale relative to
what the tool would produce from the in-tree summary JSON files. A
separate job collects
test and coverage data on every PR and pushes a stats commit back to the
PR branch when the rendered diff is non-empty. The benchmark step is
opt-in (criterion is too noisy on shared CI runners).
