#!/usr/bin/env python3
"""Bit-compare the official ALP fixture with its independently written PLAIN columns.

PyArrow need only decode the two PLAIN columns. This still provides an oracle
when its Parquet reader does not implement ALP. Float values are compared as
buffer bits, preserving negative zero and NaN signs/payloads.
"""
from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path
import struct
import subprocess
import tempfile

import pyarrow as pa
import pyarrow.ipc as ipc
import pyarrow.parquet as pq


def check_column(actual, expected, width):
    actual = actual.combine_chunks()
    expected = expected.combine_chunks()
    if actual.type != expected.type or len(actual) != len(expected):
        raise AssertionError("ALP column type or length differs from PLAIN")
    left = memoryview(actual.buffers()[1])
    right = memoryview(expected.buffers()[1])
    word = struct.Struct("<I" if width == 4 else "<Q")
    checked = 0
    for row in range(len(expected)):
        if actual[row].is_valid != expected[row].is_valid:
            raise AssertionError(f"validity differs at row {row}")
        if not expected[row].is_valid:
            continue
        a = word.unpack_from(left, (actual.offset + row) * width)[0]
        b = word.unpack_from(right, (expected.offset + row) * width)[0]
        if a != b:
            raise AssertionError(f"IEEE bits differ at row {row}: {a:#x} != {b:#x}")
        checked += 1
    return {"rows": len(expected), "nonnullBitComparisons": checked,
            "nulls": expected.null_count}


def compare_alp_table(actual, expected):
    names = {"float_plain", "float_alp_32", "float_alp_1024", "float_alp_4096",
             "double_plain", "double_alp_32", "double_alp_1024", "double_alp_4096"}
    if set(actual.column_names) != names:
        raise AssertionError("unexpected ALP fixture columns")
    results = {}
    for kind, width, data_type in [("float", 4, pa.float32()), ("double", 8, pa.float64())]:
        reference = expected[kind + "_plain"]
        if reference.type != data_type:
            raise AssertionError("unexpected reference physical type")
        for suffix in ["plain", "alp_32", "alp_1024", "alp_4096"]:
            name = f"{kind}_{suffix}"
            results[name] = check_column(actual[name], reference, width)
    return results


def check_oracle():
    for width, data_type, pairs in [
        (4, pa.float32(), [(0, 0x80000000), (0x7fc0dead, 0x7fc0deae)]),
        (8, pa.float64(), [(0, 0x8000000000000000), (0x7ff800deadbeef00, 0x7ff800deadbeef01)]),
    ]:
        pack = struct.Struct("<I" if width == 4 else "<Q")
        for left, right in pairs:
            a = pa.chunked_array([pa.Array.from_buffers(data_type, 1, [None, pa.py_buffer(pack.pack(left))])])
            b = pa.chunked_array([pa.Array.from_buffers(data_type, 1, [None, pa.py_buffer(pack.pack(right))])])
            try:
                check_column(a, b, width)
            except AssertionError:
                continue
            raise AssertionError("bitwise oracle accepted a sign/payload mismatch")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--corpus", type=Path, required=True)
    parser.add_argument("--probe", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    check_oracle()
    source = args.corpus / "data/alp_extended.zstd.parquet"
    expected = pq.read_table(source, columns=["float_plain", "double_plain"],
                             page_checksum_verification=True)
    results = {}
    with tempfile.TemporaryDirectory(prefix="wireform-parquet-alp-") as directory:
        work = Path(directory)
        inputs = work / "inputs"
        inputs.mkdir()
        (inputs / source.name).symlink_to(source.resolve())
        output = work / "decoded"
        process = subprocess.run([str(args.probe.resolve()), str(inputs),
                                  "--arrow-output", str(output), "+RTS", "-M512m", "-RTS"],
                                 capture_output=True, text=True, timeout=60)
        if process.returncode:
            raise RuntimeError(process.stdout + process.stderr)
        with ipc.open_stream(output / (source.name + ".arrows")) as reader:
            actual = reader.read_all()
        results = compare_alp_table(actual, expected)
    report = {"fixture": str(source), "fixtureSha256": hashlib.sha256(source.read_bytes()).hexdigest(),
              "oracle": "PyArrow selective PLAIN decoding; IEEE buffer bits and nulls compared",
              "pyarrowVersion": pa.__version__, "columns": results, "passed": True}
    args.output.write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(report, indent=2))


if __name__ == "__main__":
    main()
