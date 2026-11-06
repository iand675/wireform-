#!/usr/bin/env python3
"""Check whole-file benchmark values/layouts and prove rejection controls.

Call before measurements. Positive fixtures come independently from wireform,
parquet-rs and Arrow C++. Negative fixtures retain100000rows but change values,
schema, compression, page version, dictionary mode or page/group boundaries.
"""
from __future__ import annotations

import argparse
from pathlib import Path
import shutil
import subprocess
import tempfile

import pyarrow as pa
import pyarrow.parquet as pq

from parquet_bench_compare import CODECS, N_ROWS, make_dataset, write_table


def verify(binary, directory, success=True, message=None):
    result = subprocess.run([str(binary), "--verify-fixtures", str(directory)],
                            capture_output=True, text=True)
    if success:
        assert result.returncode == 0, (directory.name, result.stdout, result.stderr)
    else:
        assert result.returncode != 0, (directory.name, "unexpected verification success")
        assert "benchmark fixture:" in result.stderr, (directory.name, result.stderr)
        if message:
            assert message in result.stderr, (directory.name, result.stderr)


def reference_write(table, path, **overrides):
    options = dict(compression="NONE", use_dictionary=False, column_encoding="PLAIN", data_page_version="2.0",
                   write_page_index=False, store_schema=False, row_group_size=N_ROWS,
                   data_page_size=8 * 1024 * 1024, max_rows_per_page=N_ROWS)
    options.update(overrides)
    if options["use_dictionary"]:
        options.pop("column_encoding", None)
    pq.write_table(table, path, **options)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--wireform-bench", type=Path, required=True)
    parser.add_argument("--rust-bench", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path)
    args = parser.parse_args()
    native, rust = args.wireform_bench.resolve(), args.rust_bench.resolve()
    temporary = tempfile.TemporaryDirectory(prefix="wireform-parquet-file-preflight-")
    root = args.output_dir or Path(temporary.name)
    root.mkdir(parents=True, exist_ok=True)
    expected = make_dataset()
    for label, binary in (("native", native), ("rust", rust)):
        subprocess.run([str(binary), "--write-fixtures", str(root / label)], check=True)
    cpp = root / "cpp"
    cpp.mkdir(exist_ok=True)
    for codec in CODECS:
        (cpp / f"{codec}.parquet").write_bytes(write_table(expected, codec).to_pybytes())
    for label in ("native", "rust", "cpp"):
        directory = root / label
        verify(native, directory)
        for codec in CODECS:
            actual = pq.read_table(directory / f"{codec}.parquet", use_threads=False, pre_buffer=False)
            actual.validate(full=True)
            assert actual.equals(expected), (label, codec, "full values/schema differ")
        print("PASS full values/schema/layout:", label)
    changed_id = pa.concat_arrays([expected.column(0).combine_chunks().slice(0, N_ROWS - 1), pa.array([-999999], type=pa.int64())])
    changed_bool = pa.concat_arrays([expected.column(3).combine_chunks().slice(0, N_ROWS - 1), pa.array([True], type=pa.bool_())])
    changed_schema = pa.schema([pa.field("id", pa.int64(), nullable=True), *list(expected.schema)[1:]])
    cases = [
        ("last-id", expected.set_column(0, expected.schema.field(0), changed_id), {}, "decoded values differ"),
        ("last-bool", expected.set_column(3, expected.schema.field(3), changed_bool), {}, "decoded values differ"),
        ("nullable-schema", pa.Table.from_arrays(expected.columns, schema=changed_schema), {}, "schema differs"),
        ("codec", expected, {"compression": "snappy"}, "codec differs"),
        ("dictionary", expected, {"use_dictionary": True}, "disable dictionary"),
        ("v1-pages", expected, {"data_page_version": "1.0"}, "V2 data pages"),
        ("row-groups", expected, {"row_group_size": 20000}, None),
        ("multiple-pages", expected, {"max_rows_per_page": 8192}, None),
        ("page-index", expected, {"write_page_index": True}, "disable page indexes"),
    ]
    for label, table, options, message in cases:
        destination = root / ("wrong-" + label)
        shutil.copytree(root / "native", destination, dirs_exist_ok=True)
        reference_write(table, destination / "uncompressed.parquet", **options)
        verify(native, destination, success=False, message=message)
        print("PASS rejection:", label)
    print("PASS:9 independently-written valid fixtures and9 rejection controls;400000values checked per file")


if __name__ == "__main__":
    main()
