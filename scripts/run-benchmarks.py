#!/usr/bin/env python3
"""Reproducible benchmark runner for the wireform-stats pipeline.

Reads scripts/bench-manifest.json and, for every declared benchmark target,
**sequentially** (never in parallel -- criterion is noise-sensitive and
parallel runs poison each other's numbers):

  1. builds the benchmark (unless --no-build / --distill-only),
  2. runs it with `--json` into the manifest's rawDir (unless --distill-only),
  3. distills the criterion JSON back into each committed BenchSummary via
     scripts/distill-bench.py, applying the per-cell `map` overrides.

A manifest entry is a cabal benchmark (the default, `"kind": "cabal"`) or a
criterion.rs benchmark (`"kind": "cargo"`, with `cargoManifest` and
`cargoBench`), which supplies external baseline series such as arrow-rs.
A cargo bench writes criterion.rs output under `<rawDir>/<raw>-criterion`
(via CRITERION_HOME), which is collected into the same `--json` shape the
Haskell criterion emits, at `<rawDir>/<raw>.json`. A summary entry may list
the `series` its bench fills; a summary fed by several benches (one series
each) is then distilled once per bench, and every series of every summary
must be owned by exactly one bench.

With --render it then re-renders the charts + READMEs + docs and runs
`regen-stats check`, so one command reproduces the whole "fresh numbers in
every README and docs page" state from scratch.

This is the canonical way to redo a benchmark refresh; the CI workflow
.github/workflows/regen-stats.yml calls it behind a manual `run_benchmarks`
dispatch (criterion is too noisy for shared runners on every PR).

Examples:
  # Full refresh of everything (slow; build + run + distill + render):
  python3 scripts/run-benchmarks.py --render

  # Just the per-package codec benches, benches already built:
  python3 scripts/run-benchmarks.py --no-build --only encode-decode

  # Re-distill from criterion JSON already on disk (no rerun) -- handy for
  # validating manifest `map` edits:
  python3 scripts/run-benchmarks.py --distill-only
"""

from __future__ import annotations

import argparse
import json
import os
import shutil
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
MANIFEST = os.path.join(HERE, "bench-manifest.json")
DISTILL = os.path.join(HERE, "distill-bench.py")


def read_manifest(path: str) -> dict:
    try:
        with open(path) as fh:
            return json.load(fh)
    except (OSError, json.JSONDecodeError) as exc:
        raise SystemExit(f"{path}: cannot read manifest: {exc}")


def run(cmd: list[str], **kw) -> int:
    print("    $ " + " ".join(cmd))
    try:
        return subprocess.call(cmd, **kw)
    except OSError as exc:
        print(f"    cannot exec {cmd[0]}: {exc}", file=sys.stderr)
        return 127


def cabal(args: list[str]) -> list[str]:
    # Prefer a nix dev shell when present (matches collect-stats.sh), else
    # plain cabal on PATH. Already inside the dev shell (direnv or
    # `nix develop`): run cabal directly. A nested `nix develop` re-evaluates
    # the git flake, which ignores untracked files and rebuilds every
    # workspace package through nix.
    if os.environ.get("IN_NIX_SHELL"):
        return ["cabal", *args]
    if os.path.exists(os.path.join(ROOT, "flake.nix")) and which("nix"):
        return ["nix", "develop", "--command", "cabal", *args]
    return ["cabal", *args]


def which(prog: str) -> bool:
    try:
        for p in os.environ.get("PATH", "").split(os.pathsep):
            if p and os.access(os.path.join(p, prog), os.X_OK):
                return True
    except OSError:
        return False
    return False


def collect_criterion_rs(home: str, out_path: str) -> int:
    """Fold criterion.rs output under `home` into one criterion --json doc.

    criterion.rs writes `<home>/<group>/<function>/new/benchmark.json`
    (carrying the unsanitised `full_id`, `"<group>/<function>"`) next to
    `new/estimates.json` (mean in nanoseconds). The doc written to
    `out_path` has the `["criterion", <version>, [<report>, ...]]` shape
    distill-bench.py reads, so both toolchains distill the same way.
    Returns the number of reports collected.
    """
    reports = []
    for dirpath, _dirs, files in os.walk(home):
        if os.path.basename(dirpath) != "new" or "benchmark.json" not in files:
            continue
        try:
            with open(os.path.join(dirpath, "benchmark.json")) as fh:
                name = json.load(fh)["full_id"]
            with open(os.path.join(dirpath, "estimates.json")) as fh:
                mean_ns = json.load(fh)["mean"]["point_estimate"]
        except (OSError, json.JSONDecodeError, KeyError, TypeError) as exc:
            raise SystemExit(f"{dirpath}: unexpected criterion.rs output: {exc}")
        reports.append({"reportName": name, "reportAnalysis": {"anMean": {"estPoint": mean_ns / 1e9}}})
    reports.sort(key=lambda r: r["reportName"])
    try:
        with open(out_path, "w") as fh:
            json.dump(["criterion", "criterion.rs", reports], fh, indent=1)
            fh.write("\n")
    except OSError as exc:
        raise SystemExit(f"{out_path}: cannot write: {exc}")
    return len(reports)


def series_ownership_errors(all_benches: list[dict], paths: set[str]) -> list[str]:
    """Every series of each summary in `paths` must be filled by exactly one
    bench. A summary entry without `series` owns every series of its file."""
    owners: dict[str, list[tuple[str, list[str] | None]]] = {}
    for b in all_benches:
        for s in b["summaries"]:
            owners.setdefault(s["path"], []).append((b["target"], s.get("series")))
    errors = []
    for path in sorted(paths):
        try:
            with open(os.path.join(ROOT, path)) as fh:
                names = [s["name"] for s in json.load(fh)["series"]]
        except (OSError, json.JSONDecodeError, KeyError, TypeError) as exc:
            errors.append(f"{path}: cannot read series: {exc}")
            continue
        for name in names:
            by = [t for t, owned in owners.get(path, []) if owned is None or name in owned]
            if len(by) != 1:
                errors.append(f"{path}: series {name!r} filled by {len(by)} benches ({', '.join(by) or 'none'})")
        for t, owned in owners.get(path, []):
            for name in owned or []:
                if name not in names:
                    errors.append(f"{path}: {t} names unknown series {name!r}")
    return errors


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--manifest", default=MANIFEST)
    ap.add_argument("--only", default=None, help="substring filter on the bench target")
    ap.add_argument("--no-build", action="store_true", help="skip the build step")
    ap.add_argument("--distill-only", action="store_true", help="skip build+run; distill existing raw JSON")
    ap.add_argument("--no-strict", action="store_true", help="don't fail on unmatched summary cells")
    ap.add_argument("--dry-run", action="store_true", help="distill in dry-run (don't write summaries)")
    ap.add_argument("--render", action="store_true", help="render charts+READMEs+docs and run check at the end")
    args = ap.parse_args()

    manifest = read_manifest(args.manifest)
    raw_dir = os.path.join(ROOT, manifest.get("rawDir", "dist-stats/bench-raw"))
    try:
        os.makedirs(raw_dir, exist_ok=True)
    except OSError as exc:
        raise SystemExit(f"{raw_dir}: cannot create raw dir: {exc}")

    all_benches = manifest["benches"]
    benches = all_benches
    if args.only:
        benches = [b for b in benches if args.only in b["target"]]
    if not benches:
        raise SystemExit("no benches selected")

    errors = series_ownership_errors(all_benches, {s["path"] for b in benches for s in b["summaries"]})
    if errors:
        raise SystemExit("manifest series ownership:\n  " + "\n  ".join(errors))

    failures: list[str] = []
    for b in benches:
        target = b["target"]
        kind = b.get("kind", "cabal")
        raw_path = os.path.join(raw_dir, b["raw"] + ".json")
        print(f"==> {target}")

        if kind == "cabal":
            if not args.distill_only:
                if not args.no_build:
                    if run(cabal(["build", target]), cwd=ROOT) != 0:
                        failures.append(f"build {target}")
                        continue
                # criterion writes relative to the package cwd, so pass an abs path.
                rc = run(
                    cabal(["bench", target, f"--benchmark-options=--json {raw_path}"]),
                    cwd=ROOT,
                )
                if rc != 0:
                    failures.append(f"run {target}")
                    continue
        elif kind == "cargo":
            cargo = ["cargo", "bench", "--manifest-path", os.path.join(ROOT, b["cargoManifest"]), "--bench", b["cargoBench"]]
            home = os.path.join(raw_dir, b["raw"] + "-criterion")
            if not args.distill_only:
                if not args.no_build:
                    if run([*cargo, "--no-run"], cwd=ROOT) != 0:
                        failures.append(f"build {target}")
                        continue
                # A fresh CRITERION_HOME so a dropped bench can't leave a
                # stale result behind.
                shutil.rmtree(home, ignore_errors=True)
                rc = run([*cargo, "--", "--noplot"], cwd=ROOT, env={**os.environ, "CRITERION_HOME": home})
                if rc != 0:
                    failures.append(f"run {target}")
                    continue
            if os.path.isdir(home):
                n = collect_criterion_rs(home, raw_path)
                print(f"    collected {n} criterion.rs report(s) from {home}")
        else:
            failures.append(f"{target}: unknown kind {kind!r}")
            continue

        if not os.path.exists(raw_path):
            failures.append(f"missing raw {raw_path}")
            continue

        for s in b["summaries"]:
            cmd = ["python3", DISTILL, os.path.join(ROOT, s["path"]), raw_path]
            for m in s.get("map", []):
                cmd += ["--map", m]
            for name in s.get("series", []):
                cmd += ["--series", name]
            if not args.no_strict:
                cmd.append("--strict")
            if args.dry_run:
                cmd.append("--dry-run")
            if run(cmd, cwd=ROOT) != 0:
                failures.append(f"distill {s['path']}")

    if args.render and not failures and not args.dry_run:
        print("==> render charts + READMEs + docs")
        run(cabal(["run", "-v0", "wireform-stats:exe:regen-stats", "--", "render-bench-charts"]), cwd=ROOT)
        run(cabal(["run", "-v0", "wireform-stats:exe:regen-stats", "--", "render"]), cwd=ROOT)
        print("==> check")
        rc = run(cabal(["run", "-v0", "wireform-stats:exe:regen-stats", "--", "check"]), cwd=ROOT)
        if rc != 0:
            failures.append("regen-stats check")

    print()
    if failures:
        print("FAILURES:")
        for f in failures:
            print("  - " + f)
        return 1
    print(f"OK: processed {len(benches)} bench target(s)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
