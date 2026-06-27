# wireform-stats

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

> [!CAUTION]
> wireform is in heavy development and has not been published to Hackage yet. APIs may change.

Internal monorepo tooling. Walks the per-package
`wireform-*/README.md` files **and the Astro docs site**
(`website/src/content/docs/packages/*.md`), finds AUTOGEN marker
regions, and rewrites the body of each from in-tree test, coverage,
and benchmark data. Renders both markdown tables and SVG bar charts
(light + dark variants) so the README stays useful without a
JS-rendered build step. The docs site shares the same `bench:<id>`
markers and summary JSON, so the two never drift; its charts are
inlined as a single color-scheme-adaptive SVG
(`Wireform.Stats.SVG.renderBarChartAdaptive`) rather than a
two-file `<picture>`, since a plain Starlight page can't reference
`public/` assets with a base-aware URL.

Not a Hackage release. Lives in the monorepo because it dogfoods
[`wireform-xml`](../wireform-xml/) (the SVG charts are emitted via
`XML.Encode.encodePretty`) and runs against in-tree benchmark and
test outputs.

## Marker grammar

Every managed README region is wrapped in a paired HTML comment:

```markdown
<!-- BEGIN_AUTOGEN <key> -->
... content owned by the regen tool ...
<!-- END_AUTOGEN <key> -->
```

The key is a free-form identifier. Anything outside the markers is
hand-edited and never touched. Anything inside is owned by the regen
tool and replaced wholesale on every run.

Defined keys:

| Key                       | Body                                                                                                  |
|---------------------------|-------------------------------------------------------------------------------------------------------|
| `tests`                   | One-line summary: total / passing / failures / errors / skipped / wall time. From `dist-stats/test-results/<pkg>.junit.xml`. |
| `coverage`                | One-line summary: top-level expressions, alternatives, top-level declarations percentages. From `dist-stats/coverage/<pkg>.hpc.txt`. |
| `coverage:table`          | Per-module expressions-used table. Same source as `coverage`.                                         |
| `bench:<id>`              | A `<picture>` element referencing two SVGs (light + dark) plus a markdown table + caption. From `wireform-<pkg>/bench-results/summary/<id>.json`. |

Adding a new benchmark means dropping a `BenchSummary` JSON into
`wireform-<pkg>/bench-results/summary/<id>.json` and a matching
`<!-- BEGIN_AUTOGEN bench:<id> --><!-- END_AUTOGEN bench:<id> -->`
pair somewhere in the README (and/or the matching
`website/src/content/docs/packages/<pkg>.md` page). The regen tool
figures out the rest.

## Workflow

### Benchmarks (the manifest-driven path)

The benchmark summaries under `wireform-<pkg>/bench-results/summary/`
are **measured output, regenerated from real criterion runs** by a
reproducible, manifest-driven pipeline — not hand-typed numbers.
[`scripts/bench-manifest.json`](../scripts/bench-manifest.json) maps
each cabal benchmark target to the summary file(s) it feeds (plus any
per-cell report-name overrides), and
[`scripts/run-benchmarks.py`](../scripts/run-benchmarks.py) drives the
whole thing:

```bash
# Refresh EVERYTHING: build + run each bench sequentially, distill the
# criterion JSON back into the summaries, re-render charts + READMEs +
# docs, then `regen-stats check`. Slow; this is the canonical refresh.
python3 scripts/run-benchmarks.py --render

# Just one target (build + run + distill + render):
python3 scripts/run-benchmarks.py --only yaml --render

# Re-distill from criterion JSON already on disk (no rerun) -- use this
# to validate a manifest `map` edit:
python3 scripts/run-benchmarks.py --distill-only --dry-run
```

Benchmarks run **sequentially by construction**: criterion is
noise-sensitive and parallel runs poison each other's numbers, so the
driver never parallelises. Under the hood it calls
[`scripts/distill-bench.py`](../scripts/distill-bench.py), which parses
criterion's `--json` output and rewrites each summary's values (and
`capturedAt`) **without changing its structure**. To add or repoint a
benchmark, edit the manifest — see "Refreshing / adding benchmarks" in
the repo-root `AGENTS.md`.

### Tests + coverage

```bash
bash scripts/collect-stats.sh tests        # cabal test all -> JUnit XML
bash scripts/collect-stats.sh coverage     # cabal test all --enable-coverage -> hpc report
cabal run wireform-stats:exe:regen-stats -- render    # READMEs + docs
cabal run wireform-stats:exe:regen-stats -- badges    # shields.io endpoint JSON
```

`render` rewrites the per-package READMEs **and** the docs-site
package pages in a single pass: every `bench:<id>` region in
`website/src/content/docs/packages/<pkg>.md` is filled from the same
summary JSON, with the chart inlined as a self-contained adaptive SVG.
Use `--docs-dir` to point the docs pass elsewhere. (`render-bench-charts`
separately re-renders the README's two-file light/dark SVGs.)

## CI gate

[`.github/workflows/regen-stats.yml`](../.github/workflows/regen-stats.yml)
runs `regen-stats check` on every PR, fails the build if any
README's **or docs-site page's** AUTOGEN regions are stale relative
to what the regen tool would produce from in-tree summary JSON files. A separate job runs
`collect-stats.sh tests` + `collect-stats.sh coverage` on every PR
and pushes a stats commit back to the PR branch when the rendered
diff is non-empty. The benchmark step is opt-in via
`workflow_dispatch` with `run_benchmarks: true`, since criterion is
too noisy on shared CI runners; that job runs
`scripts/run-benchmarks.py --render` (build + run + distill + render +
check) and commits the refreshed summaries, charts, READMEs, and docs.

## What's in here

| Module                     | Role                                                                                  |
|----------------------------|---------------------------------------------------------------------------------------|
| `Wireform.Stats.Marker`    | Marker grammar + region rewriter.                                                     |
| `Wireform.Stats.SVG`       | SVG bar chart renderer with light / dark themes (uses `wireform-xml` for the DOM).    |
| `Wireform.Stats.Table`     | Markdown table renderer with column alignment.                                        |
| `Wireform.Stats.Bench`     | Benchmark types: criterion JSON parser, `BenchSummary` JSON I/O, distillation helpers, conversion to render inputs. |
| `Wireform.Stats.Test`      | JUnit XML parser (consumes tasty's `--xml=...` output via `wireform-xml`).            |
| `Wireform.Stats.Coverage`  | `hpc report --per-module` text parser.                                                |
| `Wireform.Stats.Shields`   | shields.io endpoint badge JSON emitter (tests + coverage badges).                     |

The executable is `regen-stats`; subcommands `render`,
`render-bench-charts`, `badges`, `check`. Run it with `--help` for
the full surface.

## Testing

```bash
cabal run wireform-stats:test:wireform-stats-test
```

Property-based and unit tests cover the marker round-trip, SVG
emission, JSON round-trip for `BenchSummary`, JUnit parsing, and HPC
parsing. No external deps required at test time.

## License

BSD-3-Clause.
