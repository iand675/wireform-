#!/usr/bin/env python3
"""Reproducible benchmark runner for the wireform-stats pipeline.

Reads scripts/bench-manifest.json and, for every declared benchmark target,
**sequentially** (never in parallel -- criterion is noise-sensitive and
parallel runs poison each other's numbers):

  1. builds the benchmark (unless --no-build / --distill-only),
  2. runs it with `--json` into the manifest's rawDir (unless --distill-only),
  3. distills the criterion JSON back into each committed BenchSummary via
     scripts/distill-bench.py, applying the per-cell `map` overrides.

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
    # plain cabal on PATH.
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


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--manifest", default=MANIFEST)
    ap.add_argument("--only", default=None, help="substring filter on the cabal target")
    ap.add_argument("--no-build", action="store_true", help="skip the cabal build step")
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

    benches = manifest["benches"]
    if args.only:
        benches = [b for b in benches if args.only in b["target"]]
    if not benches:
        raise SystemExit("no benches selected")

    failures: list[str] = []
    for b in benches:
        target = b["target"]
        raw_path = os.path.join(raw_dir, b["raw"] + ".json")
        print(f"==> {target}")

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

        if not os.path.exists(raw_path):
            failures.append(f"missing raw {raw_path}")
            continue

        for s in b["summaries"]:
            cmd = ["python3", DISTILL, os.path.join(ROOT, s["path"]), raw_path]
            for m in s.get("map", []):
                cmd += ["--map", m]
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
