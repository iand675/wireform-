#!/usr/bin/env python3
"""Verify integer, floating and binary statistics with Apache Thrift and PyArrow (pip install thrift pyarrow).

The generic Thrift reader deliberately uses Apache's compact codec and the
published field IDs, independently of Wireform's metadata reader/field names.
"""

import argparse
import math
import struct
import subprocess
import tempfile
from pathlib import Path

import pyarrow as pa
import pyarrow.parquet as pq
from thrift.Thrift import TType
from thrift.protocol.TCompactProtocol import TCompactProtocol
from thrift.transport.TTransport import TMemoryBuffer


def read_value(protocol, kind):
    scalar = {
        TType.BOOL: protocol.readBool, TType.BYTE: protocol.readByte,
        TType.I16: protocol.readI16, TType.I32: protocol.readI32,
        TType.I64: protocol.readI64, TType.DOUBLE: protocol.readDouble,
        TType.STRING: protocol.readBinary,
    }
    if kind in scalar:
        return scalar[kind]()
    if kind == TType.STRUCT:
        protocol.readStructBegin()
        fields = {}
        while True:
            _, field_type, field_id = protocol.readFieldBegin()
            if field_type == TType.STOP:
                break
            fields[field_id] = read_value(protocol, field_type)
            protocol.readFieldEnd()
        protocol.readStructEnd()
        return fields
    if kind in (TType.LIST, TType.SET):
        begin = protocol.readListBegin if kind == TType.LIST else protocol.readSetBegin
        end = protocol.readListEnd if kind == TType.LIST else protocol.readSetEnd
        item_type, length = begin()
        result = []
        for _ in range(length):
            result.append(read_value(protocol, item_type))
        end()
        return result
    if kind == TType.MAP:
        key_type, value_type, length = protocol.readMapBegin()
        result = {}
        for _ in range(length):
            key = read_value(protocol, key_type)
            result[key] = read_value(protocol, value_type)
        protocol.readMapEnd()
        return result
    raise ValueError(f"unsupported Thrift value kind {kind}")


def verify_other_statistics(root):
    initial = [b"name_0", b"name_7", b"name_19"] * 14
    initial = initial[:40]
    short = [b"", b"\0", b"\0\0", b"\1", b"\1\0", b"\xff", b"\xff\0", b"abcdefgh"]
    cases = {
        "int32-empty.parquet": ([], pa.int32()),
        "int32-bounds.parquet": ([2**31-1, -2**31, 0, -1, 1, 2**31-1], pa.int32()),
        "int32-descending.parquet": ([19, 7, 0, -5, -23], pa.int32()),
        "int64-empty.parquet": ([], pa.int64()),
        "int64-bounds.parquet": ([2**63-1, -2**63, 0, -1, 1, 2**54+1, 2**63-1], pa.int64()),
        "int64-descending.parquet": ([19, 7, 0, -5, -23], pa.int64()),
        "binary-boundaries.parquet": ([b"\x80", b"\x7f", b"", b"\xff"], pa.binary()),
        "utf8-boundaries.parquet": (["é", "z", "", "🙂"], pa.string()),
        "binary-empty.parquet": ([], pa.binary()),
        "binary-short-prefix.parquet": (short, pa.binary()),
        "binary-zero-prefix.parquet": ([b"\0" * n for n in range(17)], pa.binary()),
        "binary-late-min.parquet": (initial + [b"\0" * 9, b"m", b"z"], pa.binary()),
        "binary-late-max.parquet": (initial + [b"\xff" * 9, b"a", b"m"], pa.binary()),
        "binary-long-first.parquet": ([b"a" * 32 + bytes([key]) for key in (255, 0, 128, 1, 127)], pa.binary()),
        "binary-short-slice.parquet": (short, pa.binary()),
    }
    for name, (values, arrow_type) in cases.items():
        path = root / name
        raw = path.read_bytes()
        length = int.from_bytes(raw[-8:-4], "little")
        metadata = read_value(TCompactProtocol(TMemoryBuffer(raw[-8-length:-8])), TType.STRUCT)
        stats = metadata[4][0][1][0][3][12]
        assert stats[3] == 0 and 9 not in stats, (name, "counts", stats)
        assert metadata[7] == [{1: {}}], (name, "TYPE_ORDER")
        legacy = pa.types.is_integer(arrow_type)
        if not values:
            assert all(field not in stats for field in (1, 2, 5, 6)), (name, stats)
        else:
            low, high = min(values), max(values)
            if legacy:
                code = "<i" if arrow_type == pa.int32() else "<q"
                expected_low, expected_high = struct.pack(code, low), struct.pack(code, high)
                assert stats[2] == expected_low and stats[1] == expected_high, (name, "legacy", stats)
            else:
                expected_low = low.encode("utf-8") if isinstance(low, str) else low
                expected_high = high.encode("utf-8") if isinstance(high, str) else high
                assert 1 not in stats and 2 not in stats, (name, "binary legacy bounds", stats)
            assert stats[6] == expected_low and stats[5] == expected_high, (name, "modern", stats)
            assert stats[7] is True and stats[8] is True, (name, "exact", stats)
        file = pq.ParquetFile(path)
        actual = file.read()["x"]
        assert actual.type == arrow_type and actual.to_pylist() == values, (name, "values")
        reference = file.metadata.row_group(0).column(0).statistics
        assert reference.has_min_max == bool(values), (name, "reference bounds")
        if values:
            assert (reference.min, reference.max) == (min(values), max(values)), (name, reference)
        print(f"PASS {name}")
    return set(cases)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--probe", type=Path, required=True)
    args = parser.parse_args()
    cases = {
        "mixed": ([math.nan, -2, 3, math.nan], 2, (-2, 3)),
        "nan": ([math.nan, math.nan], 2, None),
        "positive-zero": ([0.0], 0, (-0.0, 0.0)),
        "negative-zero": ([-0.0], 0, (-0.0, 0.0)),
        "infinities": ([-math.inf, math.inf, math.nan], 1, (-math.inf, math.inf)),
        "empty": ([], 0, None),
    }
    with tempfile.TemporaryDirectory(prefix="wireform-parquet-statistics-") as tmp:
        root = Path(tmp)
        subprocess.run([str(args.probe.resolve()), "--statistics", str(root)], check=True)
        index = read_value(TCompactProtocol(TMemoryBuffer((root / "statistics-index.bin").read_bytes())), TType.STRUCT)
        assert index[8] == [1, 0, 0] and index[5] == [0, 0, 4], index
        checked_names = set()
        for precision, arrow_type, binary_type in [("float", pa.float32(), "<f"), ("double", pa.float64(), "<d")]:
            for name, (expected_values, nan_count, bounds) in cases.items():
                path = root / f"{precision}-{name}.parquet"
                raw = path.read_bytes()
                footer_length = int.from_bytes(raw[-8:-4], "little")
                metadata = read_value(TCompactProtocol(TMemoryBuffer(raw[-8-footer_length:-8])), TType.STRUCT)
                # FileMetaData.row_groups[0].columns[0].meta_data.statistics
                stats = metadata[4][0][1][0][3][12]
                assert stats[9] == nan_count, (path.name, "nan_count", stats)
                assert stats[3] == 0, (path.name, "null_count", stats)
                assert metadata[7] == [{1: {}}], (path.name, "TYPE_ORDER")
                if bounds is not None:
                    assert stats[7] is True and stats[8] is True, (path.name, "exact bounds")
                if bounds is None:
                    assert all(field not in stats for field in (1, 2, 5, 6)), (path.name, stats)
                else:
                    low, high = bounds
                    for field, value in [(2, low), (6, low), (1, high), (5, high)]:
                        assert stats[field] == struct.pack(binary_type, value), (path.name, field, stats)
                file = pq.ParquetFile(path)
                column = file.metadata.row_group(0).column(0)
                assert column.has_offset_index, path.name
                # TYPE_ORDER forbids a column index when a page has only NaNs.
                assert column.has_column_index == (name != "nan"), path.name
                actual = file.read()["x"]
                assert actual.type == arrow_type and len(actual) == len(expected_values), path.name
                for value, expected in zip(actual.to_pylist(), expected_values):
                    if math.isnan(expected):
                        assert math.isnan(value), path.name
                    else:
                        assert value == expected and math.copysign(1, value) == math.copysign(1, expected), path.name
                reference_stats = file.metadata.row_group(0).column(0).statistics
                assert reference_stats.has_min_max == (bounds is not None), path.name
                if bounds is not None:
                    assert (reference_stats.min, reference_stats.max) == bounds, path.name
                checked_names.add(path.name)
                print(f"PASS {path.name}")
        checked_names.update(verify_other_statistics(root))
        actual_names = {path.name for path in root.glob("*.parquet")}
        assert actual_names == checked_names, ("unverified or missing statistics fixtures", actual_names ^ checked_names)
        print(f"{len(checked_names)} statistics fixtures verified with Apache Thrift and PyArrow {pa.__version__}")


if __name__ == "__main__":
    main()
