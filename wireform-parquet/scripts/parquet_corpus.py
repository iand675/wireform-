#!/usr/bin/env python3
"""Compare Wireform with pyarrow on Apache's parquet-testing/data corpus.

Pass a built wireform-parquet-reverse-probe binary and a checkout of
https://github.com/apache/parquet-testing. Every discovered fixture gets a
result, including explicit reference rejections and resource-limit skips.
A successful decoder call alone is insufficient: values must also agree.
"""
from __future__ import annotations

import argparse
from collections import Counter
from dataclasses import dataclass
import json
import math
from pathlib import Path
import subprocess
import sys
import tempfile

import pyarrow as pa
import pyarrow.ipc as ipc
import pyarrow.parquet as pq
from parquet_alp_interop import compare_alp_table
from parquet_timestamp_interop import physical_timestamp_copy

# These normal corpus entries deliberately violate integrity/schema rules.
# Other reference rejections (new encodings or wide timestamps) remain unknown,
# and must not be reported as evidence that a file is malformed.
KNOWN_INVALID = {
    "data/datapage_v1-corrupt-checksum.parquet",
    "data/rle-dict-uncompressed-corrupt-checksum.parquet",
    "data/incorrect_map_schema.parquet",
}


@dataclass
class Difference:
    status: str
    detail: str


def same_value(left, right):
    if isinstance(left, float) and isinstance(right, float):
        return left == right or (math.isnan(left) and math.isnan(right))
    if isinstance(left, dict) and isinstance(right, dict):
        return left.keys() == right.keys() and all(same_value(left[k], right[k]) for k in left)
    if isinstance(left, (list, tuple)) and isinstance(right, (list, tuple)):
        return len(left) == len(right) and all(same_value(a, b) for a, b in zip(left, right))
    return left == right


def compare(actual, expected):
    if actual.column_names != expected.column_names:
        return Difference("schema-mismatch", f"column names differ: {actual.column_names} != {expected.column_names}")
    if actual.num_rows != expected.num_rows:
        return Difference("value-mismatch", f"row counts differ: {actual.num_rows} != {expected.num_rows}")
    for name in expected.column_names:
        left, right = actual[name], expected[name]
        compatible_strings = (
            (pa.types.is_string(left.type) or pa.types.is_large_string(left.type))
            and (pa.types.is_string(right.type) or pa.types.is_large_string(right.type))
        )
        compatible_binary = (
            (pa.types.is_binary(left.type) or pa.types.is_large_binary(left.type))
            and (pa.types.is_binary(right.type) or pa.types.is_large_binary(right.type))
        )
        if left.type != right.type and not (compatible_strings or compatible_binary):
            return Difference("schema-mismatch", f"{name}: logical types differ: {left.type} != {right.type}")
        if left.equals(right):
            continue
        for row, (a, b) in enumerate(zip(left.to_pylist(), right.to_pylist())):
            if not same_value(a, b):
                return Difference("value-mismatch", f"{name}, row {row}: {a!r} != {b!r}")
    return None


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--corpus", type=Path, required=True)
    parser.add_argument("--probe", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--max-uncompressed-mib", type=int, default=128)
    parser.add_argument("--timeout", type=float, default=30)
    parser.add_argument("--include-large-map", action="store_true",
                        help="also verify the >2GiB map through LargeUtf8 projection and one-row C++ buffer hashes (several GiB RAM)")
    parser.add_argument("--include-bad-data", action="store_true",
                        help="also probe adversarial fixtures, even when the reference rejects them")
    parser.add_argument("--max-heap-mib", type=int, default=512,
                        help="GHC heap cap for each probe process (requires its -rtsopts build)")
    args = parser.parse_args()
    paths = sorted((args.corpus / "data").rglob("*.parquet"))
    if args.include_bad_data:
        paths += sorted((args.corpus / "bad_data").rglob("*.parquet"))
    if args.max_heap_mib <= 0:
        parser.error("--max-heap-mib must be positive")
    preflight = subprocess.run([str(args.probe.resolve()), "--help", "+RTS",
                               f"-M{args.max_heap_mib}m", "-RTS"], capture_output=True, text=True)
    if preflight.returncode:
        parser.error("probe cannot enable its memory cap: " + preflight.stderr.strip())
    if not paths:
        parser.error("no corpus Parquet fixtures found")
    results = []
    with tempfile.TemporaryDirectory(prefix="wireform-parquet-corpus-") as directory:
        work = Path(directory)
        for number, source in enumerate(paths):
            result = {"file": str(source.relative_to(args.corpus))}
            bad_data = (args.corpus / "bad_data") in source.parents
            expected_invalid = bad_data or result["file"] in KNOWN_INVALID
            expected = None
            try:
                metadata = pq.ParquetFile(source).metadata
                size = sum(metadata.row_group(r).total_byte_size for r in range(metadata.num_row_groups))
                if args.include_large_map and source.name == "large_string_map.brotli.parquet":
                    large_report = work / "large-map-result.json"
                    large = subprocess.run(
                        [sys.executable, str(Path(__file__).with_name("parquet_large_map_interop.py")),
                         "--reader", str(args.probe.resolve()), "--fixture", str(source.resolve()),
                         "--output", str(large_report)], capture_output=True, text=True, timeout=360)
                    if large.returncode == 0:
                        evidence = json.loads(large_report.read_text())
                        assert evidence["nativeAndReferenceAgree"] and evidence["failures"] == 0 and evidence["skips"] == 0
                        result.update(status="passed", largeOffsetProjection=evidence)
                    else:
                        result.update(status="decode-failed", detail=(large.stdout + large.stderr)[-4000:])
                    results.append(result)
                    print(result, flush=True)
                    continue
                if size > args.max_uncompressed_mib * 1024 * 1024:
                    result.update(status="resource-limit", detail=f"{size} uncompressed bytes")
                    results.append(result)
                    print(result, flush=True)
                    continue
                alp_fixture = source.name == "alp_extended.zstd.parquet"
                physical_timestamp = work / f"physical-timestamps-{number}.parquet"
                timestamp_storage = physical_timestamp_copy(source, physical_timestamp)
                if timestamp_storage:
                    result["timestampPhysicalOracle"] = "original pages; annotation/type-only adaptation decoded by C++"
                expected = pq.read_table(physical_timestamp if timestamp_storage else source, page_checksum_verification=True,
                                         columns=["float_plain", "double_plain"] if alp_fixture else None)
            except Exception as error:
                result["referenceError"] = str(error)
            root = work / str(number)
            root.mkdir()
            (root / source.name).symlink_to(source.resolve())
            output = root / "decoded"
            try:
                process = subprocess.run([str(args.probe.resolve()), str(root), "--arrow-output", str(output),
                                          "+RTS", f"-M{args.max_heap_mib}m", "-RTS"],
                                         capture_output=True, text=True, timeout=args.timeout)
                if process.returncode:
                    detail = (process.stdout + process.stderr)[-4000:]
                    resource_failure = any(message in detail.lower() for message in
                                           ("heap exhausted", "heap overflow", "heap limit")) or process.returncode < 0
                    status = ("resource-failed" if resource_failure else "harness-failed"
                              if "  FAIL " not in detail else "invalid-rejected"
                              if expected_invalid and expected is None else "reference-rejected"
                              if expected is None else "decode-failed")
                    result.update(status=status, detail=detail)
                elif expected is None:
                    result.update(status="invalid-accepted" if expected_invalid else "unverified-accepted",
                                  detail="reference rejected this fixture; no reference values are available")
                else:
                    with ipc.open_stream(output / (source.name + ".arrows")) as reader:
                        actual = reader.read_all()
                    if alp_fixture:
                        result["bitwisePlainOracle"] = compare_alp_table(actual, expected)
                        difference = None
                    else:
                        difference = compare(actual, expected)
                    result.update(status=difference.status if difference else "passed")
                    if difference:
                        result["detail"] = difference.detail
                    elif bad_data and source.parent.name == "variants":
                        # These files deliberately store unannotated binary
                        # pairs. Raw reads must preserve their bytes; opt in
                        # to Variant semantics in a separate production API.
                        semantic = subprocess.run(
                            [str(args.probe.resolve()), "--variant-payloads", str(source.resolve()),
                             "+RTS", f"-M{args.max_heap_mib}m", "-RTS"],
                            capture_output=True, text=True, timeout=args.timeout)
                        message = semantic.stdout + semantic.stderr
                        if semantic.returncode == 1 and "Parquet.Variant:" in message:
                            result.update(status="variant-payload-rejected", rawStorage="passed",
                                          variantPayload="resource-rejected" if "limit exceeded" in message else "invalid-rejected",
                                          detail=message.strip())
                        else:
                            result.update(status="variant-semantic-failed", detail=message.strip(),
                                          semanticReturnCode=semantic.returncode)
            except subprocess.TimeoutExpired:
                result.update(status="timeout")
            except Exception as error:
                result.update(status="comparison-failed", detail=str(error))
            results.append(result)
            print(result, flush=True)
    counts = dict(Counter(r["status"] for r in results))
    revision = subprocess.run(["git", "-C", str(args.corpus), "rev-parse", "HEAD"],
                              capture_output=True, text=True, check=True).stdout.strip()
    report = {"corpusRevision": revision, "pyarrowVersion": pa.__version__,
              "counts": counts, "results": results}
    args.output.write_text(json.dumps(report, indent=2) + "\n")
    print(counts)
    return int(any(r["status"] not in ("passed", "reference-rejected", "invalid-rejected", "variant-payload-rejected") for r in results))


if __name__ == "__main__":
    raise SystemExit(main())
