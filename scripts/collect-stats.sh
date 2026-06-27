#!/usr/bin/env bash
# collect-stats.sh -- run the per-package test / coverage / benchmark
# collectors that wireform-stats's regen-stats consumes.
#
# NOTE: for benchmarks, prefer scripts/run-benchmarks.py -- it reads
# scripts/bench-manifest.json and runs the full reproducible pipeline
# (build + run sequentially + distill criterion JSON back into the
# committed summaries + re-render). The `bench` / `bench-all` modes
# below only dump raw criterion JSON and are kept for ad-hoc probing.
#
# Three modes, intentionally split so the slow benchmark step isn't
# coupled to the fast test + coverage step:
#
#   collect-stats.sh tests       -- cabal test all --enable-coverage,
#                                   capture JUnit XML + HPC reports.
#   collect-stats.sh bench <pkg> -- cabal bench <pkg>:<bench> --benchmark-options=...
#                                   for one named package's benchmark.
#   collect-stats.sh all         -- tests + every benchmark in tree.
#
# The stats land under dist-stats/ (gitignored). Bench summaries are
# the canonical hand-off to the regen tool and live under each
# package's bench-results/summary/ (committed). This script only
# writes the raw outputs; the operator commits the distilled summary
# JSON after eyeballing the numbers.

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
STATS_DIR="${ROOT}/dist-stats"
TEST_RESULTS="${STATS_DIR}/test-results"
COVERAGE_DIR="${STATS_DIR}/coverage"
BENCH_RAW_DIR="${STATS_DIR}/bench-raw"

mkdir -p "${TEST_RESULTS}" "${COVERAGE_DIR}" "${BENCH_RAW_DIR}"

cabal_run() {
	if command -v nix >/dev/null 2>&1 && [[ -f "${ROOT}/flake.nix" ]]; then
		(cd "${ROOT}" && nix develop --command cabal "$@")
	else
		(cd "${ROOT}" && cabal "$@")
	fi
}

# Discover every per-package test suite by scraping cabal files for
# 'test-suite' stanzas. Cheap; doesn't shell out to cabal-install.
discover_test_suites() {
	for cabal in "${ROOT}"/wireform-*/wireform-*.cabal; do
		pkg=$(basename "$(dirname "${cabal}")")
		awk -v pkg="${pkg}" '
      /^test-suite[[:space:]]+/ {
        suite=$2
        print pkg ":" suite
      }' "${cabal}"
	done
}

discover_benchmarks() {
	for cabal in "${ROOT}"/wireform-*/wireform-*.cabal; do
		pkg=$(basename "$(dirname "${cabal}")")
		awk -v pkg="${pkg}" '
      /^benchmark[[:space:]]+/ {
        bn=$2
        print pkg ":" bn
      }' "${cabal}"
	done
}

run_tests() {
	echo "==> Discovering test suites..."
	while IFS= read -r target; do
		pkg="${target%%:*}"
		suite="${target##*:}"
		out="${TEST_RESULTS}/${pkg}.junit.xml"
		echo "==> Running ${target} -> ${out}"
		# tasty supports --xml=PATH for per-suite output. Multi-suite
		# packages will overwrite each other; aggregate via separate file
		# per (pkg, suite) and merge if it ever matters.
		if ! cabal_run test --enable-tests "${pkg}:${suite}" \
			--test-show-details=streaming \
			--test-options="--xml=${out}"; then
			echo "    (test suite failed; partial xml may still exist)"
		fi
	done < <(discover_test_suites)
}

run_coverage() {
	echo "==> Running tests with coverage enabled..."
	cabal_run test all --enable-tests --enable-coverage --test-show-details=streaming ||
		echo "    (some test suites failed; coverage may still be collected)"
	echo "==> Capturing per-package hpc report..."
	for pkg_dir in "${ROOT}"/wireform-*/; do
		pkg=$(basename "${pkg_dir}")
		# Look for a tix file under dist-newstyle for this package.
		tix=$(find "${ROOT}/dist-newstyle" -path "*/${pkg}-*/t/*.tix" 2>/dev/null | head -1)
		if [[ -n "${tix}" ]]; then
			out="${COVERAGE_DIR}/${pkg}.hpc.txt"
			echo "==> hpc report ${tix} -> ${out}"
			hpc report "${tix}" --per-module >"${out}" || true
		fi
	done
}

run_bench() {
	local target="$1"
	if [[ -z "${target}" ]]; then
		echo "Usage: collect-stats.sh bench <pkg>:<benchname>" >&2
		exit 2
	fi
	pkg="${target%%:*}"
	bn="${target##*:}"
	out="${BENCH_RAW_DIR}/${pkg}-${bn}.json"
	echo "==> Running benchmark ${target} -> ${out}"
	cabal_run bench --enable-benchmarks "${pkg}:${bn}" \
		--benchmark-options="--json ${out}"
	echo
	echo "    Raw criterion JSON written to ${out}."
	echo "    Distill it into a BenchSummary at"
	echo "      ${pkg}/bench-results/summary/<id>.json"
	echo "    then run:"
	echo "      cabal run wireform-stats:exe:regen-stats -- render-bench-charts"
	echo "      cabal run wireform-stats:exe:regen-stats -- render"
}

run_all_benches() {
	while IFS= read -r target; do
		run_bench "${target}"
	done < <(discover_benchmarks)
}

cmd="${1:-}"
case "${cmd}" in
tests) run_tests ;;
coverage) run_coverage ;;
bench) run_bench "${2:-}" ;;
bench-all) run_all_benches ;;
all)
	run_tests
	run_coverage
	run_all_benches
	;;
*)
	cat <<EOF
Usage: collect-stats.sh <command>

Commands:
  tests              Run every per-package test suite, capture JUnit XML
                     to dist-stats/test-results/<pkg>.junit.xml.
  coverage           Run all tests with coverage enabled, capture per-
                     package hpc report --per-module to
                     dist-stats/coverage/<pkg>.hpc.txt.
  bench <pkg>:<bn>   Run one named benchmark, capture criterion JSON to
                     dist-stats/bench-raw/<pkg>-<bn>.json.
  bench-all          Run every per-package benchmark (slow).
  all                tests + coverage + bench-all (very slow).

After running, regenerate the per-package READMEs:
  cabal run wireform-stats:exe:regen-stats -- render
  cabal run wireform-stats:exe:regen-stats -- badges

The bench summaries that drive the per-package README charts live at:
  wireform-<pkg>/bench-results/summary/<id>.json
and are committed to the repo. This script writes the raw criterion
JSON into dist-stats/bench-raw/; distilling that into a summary is a
manual step (so the operator can sanity-check the numbers before
they land in a README).
EOF
	exit 1
	;;
esac
