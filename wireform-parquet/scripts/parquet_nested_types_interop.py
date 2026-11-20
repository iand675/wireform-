#!/usr/bin/env python3
"""Check typed nested output against independent Arrow models and writers.

PyArrow must preserve exact logical types and values, including nanoseconds.
DuckDB and Polars compare the wireform file with a PyArrow-written reference
of that same table, allowing each engine its own canonical Arrow types.
"""
from __future__ import annotations

import argparse
from pathlib import Path
import subprocess
import tempfile

import duckdb
import polars as pl
import pyarrow as pa
import pyarrow.parquet as pq


def catalogue():
    cases = {
        "i8": (pa.int8(), [-128, -1, 0, 127]),
        "i16": (pa.int16(), [-32768, -1, 0, 32767]),
        "i32": (pa.int32(), [-(2**31), -1, 0, 2**31 - 1]),
        "i64": (pa.int64(), [-(2**63), -1, 0, 2**63 - 1]),
        "u8": (pa.uint8(), [0, 127, 128, 255]),
        "u16": (pa.uint16(), [0, 32767, 32768, 65535]),
        "u32": (pa.uint32(), [0, 2**31 - 1, 2**31, 2**32 - 1]),
        "u64": (pa.uint64(), [0, 2**63 - 1, 2**63, 2**64 - 1]),
        "date32": (pa.date32(), [-1, 0, 1, 20000]),
        "date64": (pa.date32(), [-1, 0, 1, 2]),
        "time_s": (pa.time32("ms"), [0, 1000, 43200000, 86399000]),
        "time_ms": (pa.time32("ms"), [0, 1, 43200000, 86399999]),
        "time_us": (pa.time64("us"), [0, 1, 43200000000, 86399999999]),
        "time_ns": (pa.time64("ns"), [0, 1, 43200000000000, 86399999999999]),
        "bool": (pa.bool_(), [True, False, True, True]),
    }
    for zone_label, zone in [("local", None), ("utc", "UTC")]:
        for unit in ["s", "ms", "us", "ns"]:
            data = [-1, 0, 1, 1700000000]
            if unit == "s":
                data = list(map(lambda value: value * 1000, data))
            cases[f"timestamp_{unit}_{zone_label}"] = (
                pa.timestamp("ms" if unit == "s" else unit, tz=zone), data
            )
    for label, (typ, data) in cases.items():
        values = pa.array(data, type=typ)
        yield label + "_scalar", table(values, nullable=False)
        parent = pa.StructArray.from_arrays(
            [values], fields=[pa.field("v", typ, nullable=False)],
            mask=pa.array([False, True, False, False]),
        )
        yield label + "_struct", table(parent, nullable=True)
        element = pa.field("element", typ, nullable=False)
        lists = pa.ListArray.from_arrays(
            pa.array([0, 2, 2, 3], type=pa.int32()), values.slice(1, 3),
            type=pa.list_(element),
        )
        yield label + "_list", table(lists, nullable=False)
        maps = pa.MapArray.from_arrays(
            pa.array([0, 2, 2, 3], type=pa.int32()),
            pa.array([2, 3, 4], type=pa.uint32()), values.slice(1, 3),
            type=pa.map_(pa.field("key", pa.uint32(), nullable=False),
                         pa.field("value", typ, nullable=False)),
        )
        yield label + "_map", table(maps, nullable=False)
    for count in range(66):
        values = pa.array(list(map(lambda i: None if i % 4 == 0 else i % 3 == 0,
                                   range(count))), type=pa.bool_())
        yield f"bool_tail_{count}", table(values, nullable=True)
    parent = pa.StructArray.from_arrays(
        # A filler beneath the null parent is logically absent. PyArrow's
        # writer rejects a physical null in a required child even there;
        # the probe still supplies that hidden null/poison to wireform.
        [pa.array([-1, 0, 1], type=pa.date32())],
        fields=[pa.field("date", pa.date32(), nullable=False)],
        mask=pa.array([False, True, False]),
    )
    yield "poison_struct", table(parent, nullable=True)
    lists = pa.ListArray.from_arrays(
        pa.array([0, 2], type=pa.int32()), pa.array([0, 1000], type=pa.time32("ms")),
        type=pa.list_(pa.field("element", pa.time32("ms"), nullable=False)),
    )
    yield "poison_list", table(lists, nullable=False)
    maps = pa.MapArray.from_arrays(
        pa.array([0, 0, 1], type=pa.int32()), pa.array([2], type=pa.int32()),
        pa.array([1], type=pa.date32()), mask=pa.array([True, False]),
        type=pa.map_(pa.field("key", pa.int32(), nullable=False),
                     pa.field("value", pa.date32(), nullable=False)),
    )
    yield "poison_map", table(maps, nullable=True)
    maps = pa.MapArray.from_arrays(
        pa.array([0, 2, 2, 3], type=pa.int32()), pa.array([1, 2, 3], type=pa.uint32()),
        pa.array([2**64 - 1, None, 2**63], type=pa.uint64()),
        type=pa.map_(pa.field("key", pa.uint32(), nullable=False),
                     pa.field("value", pa.uint64(), nullable=True)),
    )
    yield "map_optional_values", table(maps, nullable=False)
    nested = pa.array(
        [[[1, None], None, [], [2]], None, [], [[], [None]]],
        type=pa.list_(pa.field("element", pa.list_(pa.field("element", pa.int32(), nullable=True)), nullable=True)),
    )
    yield "list_of_lists", table(nested, nullable=True)
    legacy = {
        "i32": (pa.int32(), [-(2**31), -1, 0, 2**31 - 1]),
        "i64": (pa.int64(), [-(2**63), -1, 0, 2**63 - 1]),
        "double": (pa.float64(), [-1.5, 0, 1.5, 1e100]),
        "bool": (pa.bool_(), [True, False, True, True]),
        "binary": (pa.binary(), [b"", b"abc", b"\x00\xff", b"tail"]),
    }
    for label, (typ, data) in legacy.items():
        values = pa.array(
            [[data[0], None, data[1]], None, [], [data[2], data[3]]],
            type=pa.list_(pa.field("element", typ, nullable=True)),
        )
        yield "legacy_" + label, table(values, nullable=True)


def table(values, *, nullable):
    return pa.Table.from_arrays([values], schema=pa.schema([pa.field("v", values.type, nullable=nullable)]))


def assert_equal(label, actual, expected):
    if not actual.equals(expected, check_metadata=False):
        raise AssertionError(f"{label}: logical values/types differ\nactual: {actual}\nexpected: {expected}")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--probe", required=True)
    parser.add_argument("--output-dir")
    args = parser.parse_args()
    with tempfile.TemporaryDirectory(prefix="wireform-parquet-nested-types-") as temporary:
        root = Path(args.output_dir or temporary)
        outputs = root / "actual"
        references = root / "reference"
        outputs.mkdir(parents=True, exist_ok=True)
        references.mkdir(parents=True, exist_ok=True)
        subprocess.run([args.probe, "--nested-types", str(outputs)], check=True)
        expected = dict(catalogue())
        produced = {path.stem for path in outputs.glob("*.parquet")}
        if produced != expected.keys():
            raise AssertionError(f"catalogue mismatch: missing={expected.keys()-produced}, extra={produced-expected.keys()}")
        checks = 0
        with duckdb.connect() as connection:
            for name, model in expected.items():
                actual_path = outputs / f"{name}.parquet"
                parquet_file = pq.ParquetFile(actual_path)
                for column in range(parquet_file.metadata.num_columns):
                    schema_column = parquet_file.schema.column(column)
                    encodings = parquet_file.metadata.row_group(0).column(column).encodings
                    if (schema_column.max_definition_level or schema_column.max_repetition_level) and "RLE" not in encodings:
                        raise AssertionError(f"{name}: column {column} omits level RLE from encodings")
                assert_equal(name + ": PyArrow", pq.read_table(actual_path), model)
                reference_path = references / f"{name}.parquet"
                pq.write_table(model, reference_path, store_schema=False, use_dictionary=False,
                               data_page_version="2.0", version="2.6")
                assert_equal(name + ": DuckDB",
                             connection.read_parquet(str(actual_path)).to_arrow_table(),
                             connection.read_parquet(str(reference_path)).to_arrow_table())
                assert_equal(name + ": Polars", pl.read_parquet(actual_path).to_arrow(),
                             pl.read_parquet(reference_path).to_arrow())
                checks += 3
                print(f"PASS {name}: PyArrow exact + DuckDB/Polars reference compatibility", flush=True)
        print(f"PASS {len(expected)} nested files / {checks} engine checks; "
              f"pyarrow={pa.__version__}, duckdb={duckdb.__version__}, polars={pl.__version__}")


if __name__ == "__main__":
    main()
