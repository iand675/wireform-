#!/usr/bin/env python3
"""Reverse-direction Parquet interop driver.

Generates Parquet files with each of pyarrow, duckdb, polars
(emitting different encodings, compression codecs, and shapes)
then invokes a wireform-side reader probe and reports which
files wireform-parquet successfully decoded vs failed.

Goal: catalogue exactly which engine-produced shapes we can't
read today.

Usage:
    python3 wireform-parquet/scripts/parquet_reverse_interop.py
"""

from __future__ import annotations

import json
import os
import subprocess
import sys
import tempfile
from decimal import Decimal, localcontext
from pathlib import Path
from typing import Any

import pyarrow as pa
import pyarrow.ipc as ipc
import pyarrow.parquet as pq
import duckdb
import polars as pl

ROOT = Path(__file__).resolve().parent.parent

def write_pyarrow(out: Path) -> list[Path]:
    """Generate a variety of pyarrow Parquet files."""
    written = []

    def write(name: str, table: pa.Table, **kwargs):
        path = out / name
        kwargs.setdefault("write_page_checksum", True)
        pq.write_table(table, path, **kwargs)
        written.append(path)

    # 1. Plain int32 + int64 + double + utf8 + bool
    t1 = pa.table({
        "i32":  pa.array([1, 2, 3], pa.int32()),
        "i64":  pa.array([10, 20, 30], pa.int64()),
        "f64":  pa.array([1.5, 2.5, 3.5], pa.float64()),
        "s":    pa.array(["a", "b", "c"], pa.string()),
        "b":    pa.array([True, False, True], pa.bool_()),
    })
    write("pyarrow_basic_v1.parquet", t1, version="1.0")
    write("pyarrow_basic_v2.parquet", t1, version="2.6", data_page_version="2.0")

    # 2. With compression
    write("pyarrow_snappy.parquet", t1, compression="snappy")
    write("pyarrow_zstd.parquet",   t1, compression="zstd")
    write("pyarrow_gzip.parquet",   t1, compression="gzip")

    # 3. Nullable columns
    t2 = pa.table({
        "id":    pa.array([1, None, 3], pa.int64()),
        "name":  pa.array(["alice", None, "carol"], pa.string()),
    })
    write("pyarrow_nullable.parquet", t2)

    # 4. Dictionary encoding
    write("pyarrow_dict.parquet",
          pa.table({"x": pa.array(["a", "b", "a", "c", "b", "a"], pa.string())}),
          use_dictionary=True)

    # 5. Multiple row groups
    big = pa.table({"x": pa.array(list(range(100)), pa.int64())})
    write("pyarrow_multi_rg.parquet", big, row_group_size=25)

    # Logical integer widths and unsigned values above the signed maximum.
    integers = pa.table({
        "i8": pa.array([-128, None, 127], pa.int8()),
        "i16": pa.array([-32768, None, 32767], pa.int16()),
        "u8": pa.array([0, None, 255], pa.uint8()),
        "u16": pa.array([0, None, 65535], pa.uint16()),
        "u32": pa.array([0, None, 2**32 - 1], pa.uint32()),
        "u64": pa.array([0, None, 2**64 - 1], pa.uint64()),
    })
    write("pyarrow_integer_widths.parquet", integers)

    # Annotated fixed-width values: preserve physical widths, signed decimal
    # bytes, scales, half-float payloads and nulls across page encodings.
    fixed = pa.table({
        f"w{width}": pa.array([
            bytes(width), None, bytes([255]) * width,
            bytes((lane * 37 + width) % 256 for lane in range(width)),
        ], pa.binary(width))
        for width in (1, 2, 7, 12, 16, 32)
    })
    half = pa.table({"half": pa.array([0.0, -0.0, None, 1.5, -65504.0, float("inf")], pa.float16())})
    with localcontext() as context:
        context.prec = 80
        decimals = pa.table({
            f"d{precision}_{scale}": pa.array([
                Decimal(value).scaleb(-scale) if value is not None else None
                for value in (0, None, -(10**precision - 1), 10**precision - 1, -1, 1)
            ], pa.decimal128(precision, scale) if precision <= 38 else pa.decimal256(precision, scale))
            for precision, scale in ((2, 0), (9, 2), (18, 6), (38, 10), (76, 30))
        })
    for version in ("1.0", "2.0"):
        for dictionary in (False, True):
            settings = dict(data_page_version=version, use_dictionary=dictionary,
                            compression="zstd", max_rows_per_page=2, write_batch_size=2)
            suffix = f"v{version}_dict{int(dictionary)}"
            write(f"pyarrow_fixed_{suffix}.parquet", fixed, **settings)
            write(f"pyarrow_half_{suffix}.parquet", half, **settings)
            write(f"pyarrow_decimal_{suffix}.parquet", decimals, **settings)
    write("pyarrow_decimal_integer.parquet", decimals, store_decimal_as_integer=True,
          use_dictionary=False)
    write("pyarrow_fixed_split.parquet", fixed, use_dictionary=False,
          column_encoding="BYTE_STREAM_SPLIT")
    write("pyarrow_fixed_delta.parquet", fixed, use_dictionary=False,
          column_encoding="DELTA_BYTE_ARRAY")
    write("pyarrow_half_split.parquet", half, use_dictionary=False,
          column_encoding="BYTE_STREAM_SPLIT")
    split_integers = pa.table({
        "i32": pa.array([-(2**31), None, 2**31-1, 0], pa.int32()),
        "i64": pa.array([-(2**63), None, 2**63-1, 0], pa.int64()),
    })
    write("pyarrow_integer_split.parquet", split_integers, use_dictionary=False,
          column_encoding="BYTE_STREAM_SPLIT")

    required_int = pa.field("same", pa.int64(), nullable=False)
    nested = pa.table({
        "left": pa.array([
            None, {"same": 1, "items": None}, {"same": 2, "items": []},
            {"same": 3, "items": [None, 4]}, {"same": 4, "items": [5]},
            {"same": 5, "items": list(range(97))},
        ], pa.struct([required_int, pa.field("items", pa.list_(pa.int32()))])),
        "right": pa.array([
            {"same": "a", "price": Decimal("-12.345")}, None,
            {"same": None, "price": None}, {"same": "b", "price": Decimal("0.001")},
            {"same": "c", "price": Decimal("999999999.999")}, None,
        ], pa.struct([("same", pa.string()), ("price", pa.decimal128(12, 3))])),
        "lists": pa.array([
            None, [], [None], [[]], [[None, "x"], [], None, ["y"]], [["z"]],
        ], pa.list_(pa.list_(pa.string()))),
        "maps": pa.array([
            None, [], [("a", None)], [("b", {"same": None, "items": []})],
            [("c", {"same": 3, "items": [b"x", None]}), ("d", None)],
            [("e", {"same": 4, "items": None})],
        ], pa.map_(pa.string(), pa.struct([("same", pa.int64()), ("items", pa.list_(pa.binary()))]))),
        "nulls": pa.array([None, [], [None], [None, None], [], None], pa.list_(pa.null())),
    })
    for version in ("1.0", "2.0"):
        for dictionary in (False, True):
            write(f"pyarrow_nested_v{version}_dict{int(dictionary)}.parquet", nested,
                  data_page_version=version, use_dictionary=dictionary,
                  max_rows_per_page=2, write_batch_size=2, compression="zstd")
    write("pyarrow_json_extension.parquet", pa.table({
        "doc": pa.array(['{"a":1}', None, '[true,null,"x"]'], pa.json_()),
    }))

    return written


def write_duckdb(out: Path) -> list[Path]:
    written = []
    con = duckdb.connect()
    # Use COPY to write a Parquet file from a SQL query
    base = "(SELECT * FROM (VALUES (1::INT, 10::BIGINT, 1.5::DOUBLE, 'a', TRUE), (2, 20, 2.5, 'b', FALSE), (3, 30, 3.5, 'c', TRUE)) AS t(i32, i64, f64, s, b))"
    p = out / "duckdb_basic.parquet"
    con.execute(f"COPY {base} TO '{p}' (FORMAT 'parquet')")
    written.append(p)
    p = out / "duckdb_snappy.parquet"
    con.execute(f"COPY {base} TO '{p}' (FORMAT 'parquet', COMPRESSION 'snappy')")
    written.append(p)
    p = out / "duckdb_zstd.parquet"
    con.execute(f"COPY {base} TO '{p}' (FORMAT 'parquet', COMPRESSION 'zstd')")
    written.append(p)
    return written


def write_polars(out: Path) -> list[Path]:
    written = []
    df = pl.DataFrame({
        "i32": pl.Series("i32", [1, 2, 3], dtype=pl.Int32),
        "i64": pl.Series("i64", [10, 20, 30], dtype=pl.Int64),
        "f64": pl.Series("f64", [1.5, 2.5, 3.5], dtype=pl.Float64),
        "s":   pl.Series("s",   ["a", "b", "c"]),
        "b":   pl.Series("b",   [True, False, True]),
    })
    p = out / "polars_basic.parquet"
    df.write_parquet(p)
    written.append(p)
    p = out / "polars_zstd.parquet"
    df.write_parquet(p, compression="zstd")
    written.append(p)
    # Polars's "lz4" maps to the legacy CompressionCodec.LZ4
    # (codec 5) which is the Hadoop-frame variant. Modern
    # writers use LZ4_RAW (codec 7); we test that via the
    # 'lz4_raw' name which both pyarrow and polars accept.
    p = out / "polars_lz4_raw.parquet"
    try:
        df.write_parquet(p, compression="lz4_raw")
        written.append(p)
    except Exception:
        pass
    p = out / "pyarrow_lz4_raw.parquet"
    try:
        pq.write_table(
            pa.table({
                "i32": pa.array([1, 2, 3], pa.int32()),
                "s":   pa.array(["a", "b", "c"], pa.string()),
            }),
            p,
            compression="lz4_raw",
        )
        written.append(p)
    except Exception:
        pass
    return written


def main() -> int:
    out = Path(tempfile.mkdtemp(prefix="parquet-reverse-"))
    print(f"== generating engine-emitted Parquet files in {out}")

    all_files = []
    all_files += write_pyarrow(out)
    all_files += write_duckdb(out)
    all_files += write_polars(out)
    print(f"== {len(all_files)} files written")

    # Run the wireform-side reader probe.
    print(f"\n== reading with wireform-parquet")
    decoded = out / "decoded"
    probe = os.environ.get("WIREFORM_PARQUET_REVERSE_PROBE")
    command = [probe] if probe else ["cabal", "run", "wireform-parquet:wireform-parquet-reverse-probe", "--"]
    result = subprocess.run(
        [*command, str(out), "--arrow-output", str(decoded)],
        cwd=ROOT.parent, capture_output=True, text=True
    )
    print(result.stdout)
    if result.returncode != 0:
        print("STDERR:", result.stderr)
    failures = 0
    for source in all_files:
        output = decoded / (source.name + ".arrows")
        try:
            with ipc.open_stream(output) as reader:
                actual = reader.read_all()
            expected = pq.read_table(source)
            # Parquet STRING/BYTE_ARRAY does not encode Arrow offset width;
            # Polars' Arrow schema metadata can restore large_string, while
            # the native bridge legitimately constructs 32-bit offsets.
            expected = expected.cast(pa.schema([
                pa.field(field.name,
                         pa.string() if pa.types.is_large_string(field.type)
                         else pa.binary() if pa.types.is_large_binary(field.type)
                         else field.type,
                         nullable=field.nullable)
                for field in expected.schema
            ]))
            if not actual.equals(expected):
                raise AssertionError(
                    f"decoded values differ\nexpected: {expected}\nactual: {actual}"
                )
            print(f"  VALUES OK {source.name}")
        except Exception as exc:
            failures += 1
            print(f"  VALUES FAIL {source.name}: {exc}")
    return 1 if failures or result.returncode else 0


if __name__ == "__main__":
    sys.exit(main())
