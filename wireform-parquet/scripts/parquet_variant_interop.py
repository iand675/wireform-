#!/usr/bin/env python3
"""Variant binary semantics and unshredded Parquet interoperability.

The official malformed fixtures are unannotated binary column pairs. Check
their raw bytes separately from explicit Variant payload interpretation.
"""
import argparse
import json
from pathlib import Path
import shutil
import struct
import subprocess
import tempfile

import pyarrow as pa
import pyarrow.parquet as pq
from thrift.Thrift import TType
from parquet_file_interop import footer
from parquet_lzo_interop import Node, encode


def binary_cases():
    empty = bytes([1, 0, 0])
    cases = {}

    def primitive(name, tag, payload=b""):
        cases[name] = (empty, bytes([tag << 2]) + payload)

    primitive("null", 0)
    primitive("true", 1)
    primitive("false", 2)
    primitive("int8", 3, struct.pack("<b", -128))
    primitive("int16", 4, struct.pack("<h", -32768))
    primitive("int32", 5, struct.pack("<i", -(2**31)))
    primitive("int64", 6, struct.pack("<q", -(2**63)))
    primitive("double-nan-bits", 7, struct.pack("<Q", 0x7FF8123456789012))
    primitive("double-negative-zero", 7, struct.pack("<Q", 1 << 63))
    primitive("decimal4", 8, bytes([9]) + struct.pack("<i", -999999999))
    primitive("decimal8", 9, bytes([18]) + struct.pack("<q", -999999999999999999))
    primitive("decimal16", 10, bytes([38]) + (-(10**38-1)).to_bytes(16, "little", signed=True))
    primitive("date", 11, struct.pack("<i", -40000))
    primitive("timestamp", 12, struct.pack("<q", -1))
    primitive("timestamp-ntz", 13, struct.pack("<q", -1))
    primitive("float-nan-bits", 14, struct.pack("<I", 0x7FC12345))
    primitive("float-negative-zero", 14, struct.pack("<I", 1 << 31))
    binary = bytes(range(256))
    primitive("binary", 15, struct.pack("<I", len(binary)) + binary)
    text = ("λ🦊" * 20).encode()
    primitive("string", 16, struct.pack("<I", len(text)) + text)
    primitive("time", 17, struct.pack("<q", 86399999999))
    primitive("timestamp-nanos", 18, struct.pack("<q", 2**63-1))
    primitive("timestamp-ntz-nanos", 19, struct.pack("<q", -(2**63)))
    primitive("uuid", 20, bytes(range(16)))
    cases["short-string"] = (empty, bytes([9]) + "λ".encode())
    cases["array"] = (empty, bytes([3, 2, 0, 1, 3, 0, 12, 255]))
    cases["array-reserved"] = (empty, bytes([227, 1, 0, 1, 0]))
    cases["large-small-array"] = (empty, bytes([19, 1, 0, 0, 0, 0, 1, 0]))
    metadata = bytes([1, 2, 0, 1, 2, 97, 98])
    value = bytes([2, 2, 0, 1, 2, 0, 4, 12, 2, 12, 1])
    cases["object-reordered-values"] = (metadata, value)
    cases["object-reserved"] = (bytes([33]) + metadata[1:], bytes([130]) + value[1:])
    cases["unsorted-nonunique-dictionary"] = (bytes([1, 3, 0, 1, 2, 3, 98, 97, 97]), bytes([2, 1, 2, 0, 1, 0]))
    for width in range(1, 5):
        def word(n):
            return n.to_bytes(width, "little")
        metadata = bytes([1 + (width-1)*64]) + word(1) + word(0) + word(1) + b"a"
        value = bytes([2 + (width-1)*4 + (width-1)*16, 1]) + word(0) + word(0) + word(1) + b"\0"
        cases[f"width-{width}"] = (metadata, value)
    return cases


def annotate_variant(source, target, reverse=False):
    prefix, metadata = footer(source)
    elements = metadata.value[2].value
    group = elements[1]
    group.value[10] = Node(TType.STRUCT, {16: Node(TType.STRUCT, {1: Node(TType.BYTE, 1)})})
    if reverse:
        assert len(elements) == 4
        elements[2], elements[3] = elements[3], elements[2]
        for row_group in metadata.value[4].value:
            row_group.value[1].value.reverse()
    raw = encode(metadata)
    target.write_bytes(prefix + raw + struct.pack("<I", len(raw)) + b"PAR1")


def check_annotation(path):
    _, metadata = footer(path)
    elements = metadata.value[2].value
    assert len(elements) == 4
    assert elements[1].value[10].value[16].value[1].value == 1
    assert elements[1].value[5].value == 2
    for element in elements[2:]:
        assert element.value[1].value == 6  # BYTE_ARRAY
        assert element.value[3].value == 0  # REQUIRED
    assert {e.value[4].value for e in elements[2:]} == {b"metadata", b"value"}


def run(command, expected=0):
    result = subprocess.run(list(map(str, command)), capture_output=True, text=True, timeout=60)
    assert result.returncode == expected, (command, result.returncode, result.stdout, result.stderr)
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--writer", required=True, type=Path)
    parser.add_argument("--reader", required=True, type=Path)
    parser.add_argument("--reference", required=True, type=Path)
    parser.add_argument("--corpus", type=Path)
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    cases = binary_cases()
    with tempfile.TemporaryDirectory(prefix="parquet-variant-") as directory:
        root = Path(directory)
        inputs, native, compare = (root / name for name in ("inputs", "native", "compare"))
        inputs.mkdir()
        compare.mkdir()
        for name, (metadata, value) in cases.items():
            (inputs / (name + ".meta")).write_bytes(metadata)
            (inputs / (name + ".value")).write_bytes(value)
            shutil.copyfile(inputs / (name + ".meta"), compare / (name + ".meta"))
            shutil.copyfile(inputs / (name + ".value"), compare / (name + ".value"))
        run([args.writer, "--variant-from-binary", inputs, native])
        for name in cases:
            path = native / (name + ".parquet")
            check_annotation(path)
            physical = pq.read_table(path).to_pylist()
            assert physical[1] == {"v": None}
            assert physical[2]["v"]["value"] == b"\0"
            exported = root / (name + "-export")
            run([args.reader, "--variant-export", path, "v", exported])
            assert (exported / "0-1.null").exists()
            for ext in ("meta", "value"):
                assert (exported / ("0-0." + ext)).read_bytes() == physical[0]["v"]["metadata" if ext == "meta" else "value"]
                shutil.copyfile(exported / ("0-0." + ext), compare / (name + "-native." + ext))
        # Foreign Parquet pages are written by PyArrow; only the logical
        # annotation (and one child's schema/descriptor order) is Thrift-edited.
        foreign_checks = 0
        for reverse in (False, True):
            name = "object-reordered-values"
            metadata, value = cases[name]
            schema = pa.schema([pa.field("v", pa.struct([pa.field("metadata", pa.binary(), False), pa.field("value", pa.binary(), False)]), True)])
            table = pa.Table.from_pylist([{"v": {"metadata": metadata, "value": value}}, {"v": None}, {"v": {"metadata": b"\1\0\0", "value": b"\0"}}], schema=schema)
            raw, annotated = root / "foreign-raw.parquet", root / f"foreign-{reverse}.parquet"
            pq.write_table(table, raw)
            annotate_variant(raw, annotated, reverse)
            check_annotation(annotated)
            exported = root / f"foreign-export-{reverse}"
            run([args.reader, "--variant-export", annotated, "v", exported])
            assert (exported / "0-1.null").exists()
            for ext in ("meta", "value"):
                shutil.copyfile(exported / ("0-0." + ext), compare / (name + f"-foreign-{reverse}." + ext))
            foreign_checks += 1
        run([args.reference, compare])
        semantic_checks = 0
        for name in cases:
            assert (compare / (name + ".rust-status")).read_text().startswith("OK")
            for ext in ("rust-meta", "rust-value"):
                assert (compare / (name + "." + ext)).read_bytes() == (compare / (name + "-native." + ext)).read_bytes(), name
            semantic_checks += 1
        for reverse in (False, True):
            for ext in ("rust-meta", "rust-value"):
                assert (compare / ("object-reordered-values." + ext)).read_bytes() == (compare / (f"object-reordered-values-foreign-{reverse}." + ext)).read_bytes()
        malformed = 0
        if args.corpus:
            for path in sorted((args.corpus / "bad_data/variants").glob("*.parquet")):
                # First prove ordinary raw reads still succeed.
                work = root / ("raw-" + path.stem)
                work.mkdir()
                (work / path.name).symlink_to(path.resolve())
                run([args.reader, work])
                rejected = subprocess.run([str(args.reader), "--variant-payloads", str(path)], capture_output=True, text=True, timeout=60)
                assert rejected.returncode == 1 and "Parquet.Variant:" in rejected.stdout, (path, rejected)
                malformed += 1
            assert malformed == 14
        report = {"status": "passed", "nativeSemanticRoundtrips": semantic_checks,
                  "independentAnnotatedParquetFiles": foreign_checks, "apacheThriftAnnotationChecks": len(cases)+foreign_checks,
                  "physicalPyArrowComparisons": len(cases), "ordinaryMalformedFixtureReadsPreserved": malformed,
                  "explicitVariantPayloadRejections": malformed,
                  "referenceLimitations": "Rust rejects decimal4/8 scale 38 despite binary spec [0,38]; covered by native regression, not claimed as Rust passes.",
                  "scope": "Binary Variant semantics and unshredded files; typed shredding/canonical Arrow extension integration remain separate release gates."}
        if args.output:
            args.output.write_text(json.dumps(report, indent=2) + "\n")
        print(json.dumps(report, indent=2))


if __name__ == "__main__":
    main()
