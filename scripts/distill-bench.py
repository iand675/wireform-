#!/usr/bin/env python3
"""Refresh a wireform-stats BenchSummary JSON from a criterion --json run.

The committed `wireform-<pkg>/bench-results/summary/<id>.json` files define
the *shape* of each comparison (id / title / unit / groups / series names /
baseline / toolchain). This tool keeps that shape exactly and only rewrites
the per-cell numbers (and `capturedAt`) from a fresh criterion run, so the
charts + tables that `regen-stats` produces reflect real, just-measured data.

Criterion's `--json` output is `["criterion", <version>, [<report>, ...]]`
where each report has a `reportName` (conventionally `"<bgroup>/<bench>"`)
and `reportAnalysis.anMean.estPoint` (mean seconds).

Each summary cell is `series[name].values[groupIndex]`. We locate the
matching criterion report by trying, in order:

  1. an explicit `--map "<series>|<group>=<reportName>"` override,
  2. "<series>/<group>",
  3. "<group>/<series>",
  4. a bare "<group>" or "<series>" (single-axis benches).

Anything still unmatched is reported and left at its previous value unless
`--strict` is passed, in which case the run fails. This is intentional: a
few summaries (cbor/msgpack inside the umbrella `format-bench`, the xml/yaml
comparison benches) use bespoke report names and are matched with `--map`.
"""

from __future__ import annotations

import argparse
import json
import sys
from datetime import datetime, timezone

UNIT_PER_SECOND = {
    "Nanos": 1e9,
    "Micros": 1e6,
    "Millis": 1e3,
    "Seconds": 1.0,
}


def read_json(path: str):
    try:
        with open(path) as fh:
            return json.load(fh)
    except (OSError, json.JSONDecodeError) as exc:
        raise SystemExit(f"{path}: cannot read JSON: {exc}")


def write_json(path: str, obj) -> None:
    try:
        with open(path, "w") as fh:
            json.dump(obj, fh, indent=2, ensure_ascii=False)
            fh.write("\n")
    except OSError as exc:
        raise SystemExit(f"{path}: cannot write: {exc}")


def load_reports(criterion_path: str) -> dict[str, float]:
    doc = read_json(criterion_path)
    if not (isinstance(doc, list) and len(doc) == 3):
        raise SystemExit(f"{criterion_path}: not a criterion --json document")
    reports = doc[2]
    out: dict[str, float] = {}
    try:
        for r in reports:
            out[r["reportName"]] = r["reportAnalysis"]["anMean"]["estPoint"]
    except (KeyError, TypeError) as exc:
        raise SystemExit(f"{criterion_path}: unexpected criterion shape: {exc}")
    return out


def round_like(x: float) -> float:
    # Match the existing summaries' 2-decimal precision.
    return round(x, 2)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("summary", help="path to bench-results/summary/<id>.json")
    ap.add_argument("criterion", help="path to a criterion --json output")
    ap.add_argument(
        "--map",
        action="append",
        default=[],
        metavar="SERIES|GROUP=REPORTNAME",
        help="explicit cell -> criterion report-name override; repeatable",
    )
    ap.add_argument("--strict", action="store_true", help="fail on any unmatched cell")
    ap.add_argument("--dry-run", action="store_true", help="print, do not write")
    args = ap.parse_args()

    overrides: dict[tuple[str, str], str] = {}
    for m in args.map:
        key, _, name = m.partition("=")
        s, _, g = key.partition("|")
        overrides[(s, g)] = name

    reports = load_reports(args.criterion)
    summary = read_json(args.summary)

    unit = summary["unit"]
    if unit not in UNIT_PER_SECOND:
        raise SystemExit(f"{args.summary}: unit {unit!r} is not time-based; distill by hand")
    factor = UNIT_PER_SECOND[unit]
    groups = summary["groups"]

    unmatched: list[str] = []
    changes: list[str] = []
    for series in summary["series"]:
        sname = series["name"]
        new_vals = list(series["values"])
        for gi, g in enumerate(groups):
            candidates = []
            if (sname, g) in overrides:
                candidates.append(overrides[(sname, g)])
            candidates += [f"{sname}/{g}", f"{g}/{sname}", g, sname]
            mean_s = None
            for c in candidates:
                if c in reports:
                    mean_s = reports[c]
                    break
            if mean_s is None:
                unmatched.append(f"{sname} | {g}")
                continue
            old = new_vals[gi] if gi < len(new_vals) else None
            new = round_like(mean_s * factor)
            if gi < len(new_vals):
                new_vals[gi] = new
            else:
                new_vals.append(new)
            if old != new:
                changes.append(f"  {sname} | {g}: {old} -> {new} {unit}")
        series["values"] = new_vals

    if unmatched:
        msg = f"{args.summary}: unmatched cells: " + "; ".join(unmatched)
        if args.strict:
            print(msg, file=sys.stderr)
            print("available reports: " + ", ".join(sorted(reports)), file=sys.stderr)
            return 2
        print("WARN " + msg, file=sys.stderr)

    summary["capturedAt"] = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")

    print(f"{args.summary}: {len(changes)} cell(s) changed")
    for c in changes:
        print(c)

    if not args.dry_run:
        write_json(args.summary, summary)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
