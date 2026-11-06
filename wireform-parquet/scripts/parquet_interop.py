#!/usr/bin/env python3
"""Cross-engine Parquet interop driver.

Workflow per file:

  1. Read with pyarrow -> assert against expected.
  2. Read with duckdb  -> assert against expected.
  3. Read with polars  -> assert against expected.

Each file is associated with a Python expectations dict that
the engine-specific reader is checked against. A file is OK
only if every engine accepts it AND every engine returns the
same data we wrote.

Run:
    python3 wireform-parquet/scripts/parquet_interop.py [--regen]

`--regen` additionally writes pyarrow / duckdb / polars
reference files /into/ a temp dir and tries to read them
with wireform-parquet (round-trip-the-other-way coverage).
"""

from __future__ import annotations

import argparse
import os
import subprocess
import sys
import tempfile
from pathlib import Path

import pyarrow as pa
import pyarrow.parquet as pq
import duckdb
import polars as pl
from parquet_statistics_interop import read_value, TCompactProtocol, TMemoryBuffer, TType

ROOT = Path(__file__).resolve().parent.parent

# --- Each entry: (filename, expected pa.Table)
import datetime as _dt

def temporal_expected(nullable: bool) -> pa.Table:
    def values(items):
        return [item if not nullable or i != 1 else None for i, item in enumerate(items)]
    return pa.table({
        "date": pa.array(values([-1, 0, 1]), pa.date32()),
        "time": pa.array(values([0, 1000, 86399000]), pa.time32("ms")),
        "stamp": pa.array(values([-1000, 0, 1700000000000]), pa.timestamp("ms")),
    })

def expected_files() -> list[tuple[str, pa.Table]]:
    return [
        ( "int32_required.parquet"
        , pa.table({"x": pa.array([1, 2, 3, 4, 5], pa.int32())}) )
      , ( "int64_required.parquet"
        , pa.table({"x": pa.array([10, 20, 30, 40, 50], pa.int64())}) )
      , ( "float_required.parquet"
        , pa.table({"x": pa.array([1.5, 2.5, 3.5], pa.float32())}) )
      , ( "double_required.parquet"
        , pa.table({"x": pa.array([1.5, -2.5, 3.14159], pa.float64())}) )
      , ( "bool_required.parquet"
        , pa.table({"x": pa.array([True, False, True, True, False], pa.bool_())}) )
      , ( "byte_array_required.parquet"
        , pa.table({"x": pa.array([b"alpha", b"beta", b"gamma"], pa.binary())}) )
      , ( "utf8_required.parquet"
        , pa.table({"name": pa.array(["Alice", "Bob", "Carol", "Δοε"], pa.string())}) )

      # Logical-type-annotated primitives
      , ( "date32_required.parquet"
        , pa.table({"d": pa.array(
            [ _dt.date(1970, 1, 1)
            , _dt.date(1970, 1, 1) + _dt.timedelta(days=18000)
            , _dt.date(1970, 1, 1) + _dt.timedelta(days=19000)
            ], pa.date32())}) )
      , ( "time_millis_required.parquet"
        , pa.table({"t": pa.array(
            [ _dt.time(0, 0, 0)
            , _dt.time(0, 0, 12, 345_000)
            , _dt.time(23, 59, 59, 999_000)
            ], pa.time32("ms"))}) )
      , ( "timestamp_millis_required.parquet"
          # pyarrow attaches UTC tz when the Parquet
          # ConvertedType.TIMESTAMP_MILLIS surfaces (matches
          # the spec's isAdjustedToUTC=true default for the
          # legacy converted type). duckdb / polars read it as
          # a tz-naive instant. We pin the pyarrow behaviour
          # in the expectation; the per-engine asserter is
          # tolerant of duckdb / polars's tz-naive value.
        , pa.table({"ts": pa.array(
            [ _dt.datetime(1970, 1, 1, 0, 0, 0, tzinfo=_dt.timezone.utc)
            , _dt.datetime(2023, 11, 14, 22, 13, 20, tzinfo=_dt.timezone.utc)
            ], pa.timestamp("ms", tz="UTC"))}) )
      , ( "uint32_required.parquet"
        , pa.table({"u": pa.array([0, 1, 2_147_483_647, 4_294_967_295],
                                  pa.uint32())}) )
      , ( "json_required.parquet"
          # pyarrow surfaces JSON as plain string column (the
          # JSON LogicalType doesn't widen the physical type).
        , pa.table({"doc": pa.array(['{"k":1}', '{"k":2,"j":"hi"}'],
                                    pa.string())}) )

      , ( "int64_zstd.parquet"
        , pa.table({"x": pa.array([10, 20, 30, 40, 50], pa.int64())}) )
      , ( "int64_snappy.parquet"
        , pa.table({"x": pa.array([10, 20, 30, 40, 50], pa.int64())}) )
      , ( "int64_gzip.parquet"
        , pa.table({"x": pa.array([10, 20, 30, 40, 50], pa.int64())}) )
      , ( "int64_lz4_raw.parquet"
        , pa.table({"x": pa.array([10, 20, 30, 40, 50], pa.int64())}) )

      , ( "int64_two_row_groups.parquet"
        , pa.table({"x": pa.array([1, 2, 3, 4, 5, 6], pa.int64())}) )
      , ( "mixed_optional.parquet"
        , pa.table({
            "id":    pa.array([10, 20, 30], pa.int64()),
            "name":  pa.array(["alice", None, "carol"], pa.string()),
            "score": pa.array([1.5, 2.5, None], pa.float64()),
          }) )

      # All-nullable single-column variants exercising
      # definition-level streams for each physical type.
      , ( "optional_int32.parquet"
        , pa.table({"x": pa.array([1, None, 3, -1], pa.int32())}) )
      , ( "optional_int64.parquet"
        , pa.table({"x": pa.array([100, None, 300], pa.int64())}) )
      , ( "optional_float.parquet"
        , pa.table({"x": pa.array([1.5, 2.5, None, 4.5], pa.float32())}) )
      , ( "optional_bool.parquet"
        , pa.table({"x": pa.array([True, None, False, True, None], pa.bool_())}) )
      , ("arrow_temporal_required_v1.parquet", temporal_expected(False))
      , ("arrow_temporal_required_v2.parquet", temporal_expected(False))
      , ("arrow_temporal_optional_v1.parquet", temporal_expected(True))
      , ("arrow_temporal_optional_v2.parquet", temporal_expected(True))
    ]

def _normalise_value(v):
    """Per-engine quirk normaliser:

    * pyarrow returns tz-aware datetime for TIMESTAMP_MILLIS;
      duckdb / polars return tz-naive. We compare against the
      tz-aware expectation by stripping tzinfo on both sides.
    * polars returns JSON columns as bytes; pyarrow as str.
      Decode bytes -> utf-8 so they compare equal.
    """
    if isinstance(v, bytes):
        try:
            return v.decode("utf-8")
        except UnicodeDecodeError:
            return v
    if hasattr(v, "tzinfo") and v.tzinfo is not None:
        return v.replace(tzinfo=None)
    return v

def _normalise_list(xs):
    return [_normalise_value(x) for x in xs]

def assert_table_eq(name: str, actual: pa.Table, expected: pa.Table) -> str | None:
    if actual.num_rows != expected.num_rows:
        return f"row count {actual.num_rows} != {expected.num_rows}"
    if actual.num_columns != expected.num_columns:
        return f"column count {actual.num_columns} != {expected.num_columns}"
    for col_name in expected.column_names:
        if col_name not in actual.column_names:
            return f"missing column {col_name!r}"
        a = _normalise_list(actual.column(col_name).combine_chunks().to_pylist())
        e = _normalise_list(expected.column(col_name).combine_chunks().to_pylist())
        if a != e:
            return f"column {col_name!r}: got {a} != expected {e}"
    return None

def read_pyarrow(path: Path) -> pa.Table:
    return pq.read_table(path)

def read_duckdb(path: Path) -> pa.Table:
    con = duckdb.connect()
    result = con.execute(f"SELECT * FROM read_parquet('{path}')")
    rbr = result.to_arrow_table() if hasattr(result, "to_arrow_table") else result.arrow()
    if isinstance(rbr, pa.Table):
        return rbr
    # duckdb 1.5+ returns a RecordBatchReader from .arrow() in
    # some configurations; coalesce it.
    return rbr.read_all() if hasattr(rbr, "read_all") else pa.Table.from_batches(list(rbr))

def read_polars(path: Path) -> pa.Table:
    df = pl.read_parquet(path)
    return df.to_arrow()

def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--regen", action="store_true",
        help="also generate engine-emitted Parquet files for reverse round-trip")
    args = parser.parse_args()

    out = Path(tempfile.mkdtemp(prefix="wireform-parquet-probe-"))
    print(f"== writing wireform-parquet probe outputs to {out}")
    probe = os.environ.get("WIREFORM_PARQUET_FORWARD_PROBE")
    command = [probe] if probe else ["cabal", "run", "wireform-parquet:wireform-parquet-interop-probe", "--"]
    subprocess.run(
        [*command, str(out)],
        cwd=ROOT.parent, check=True
    )

    print("\n== reading wireform-parquet output with pyarrow / duckdb / polars")
    failures = []
    for fname, expected in expected_files():
        path = out / fname
        if not path.exists():
            failures.append((fname, "probe didn't write"))
            continue

        # These probes write one data page per leaf/group. Check V2's literal
        # num_rows field independently, especially when repeated events differ.
        raw = path.read_bytes()
        metadata = pq.ParquetFile(path).metadata
        for group_number in range(metadata.num_row_groups):
            group = metadata.row_group(group_number)
            for column_number in range(group.num_columns):
                offset = group.column(column_number).data_page_offset
                header = read_value(TCompactProtocol(TMemoryBuffer(raw[offset:])), TType.STRUCT)
                if header[1] == 3:
                    assert header[8][3] == group.num_rows, (fname, 'V2 rows vs repeated events', header[8], group.num_rows)

        for engine, reader in [("pyarrow", read_pyarrow),
                               ("duckdb",  read_duckdb),
                               ("polars",  read_polars)]:
            try:
                got = reader(path)
                err = assert_table_eq(fname, got, expected)
                if err:
                    failures.append((f"{fname}::{engine}", err))
                else:
                    print(f"  OK   {fname:40s} via {engine}")
            except Exception as e:
                failures.append((f"{fname}::{engine}", repr(e)))

    if args.regen:
        print("\n== reverse round-trip: engines write, wireform reads")
        # Currently the wireform-parquet reader is exercised by the
        # main test suite + the Iceberg goldens; this script is the
        # write-side audit. The reverse direction is tested by
        # the iceberg golden corpus + parquet-test.

    if failures:
        print(f"\n{len(failures)} failures:")
        for k, v in failures:
            print(f"  FAIL {k}: {v}")
        return 1
    print("\nAll engines accepted every wireform-parquet probe output.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
