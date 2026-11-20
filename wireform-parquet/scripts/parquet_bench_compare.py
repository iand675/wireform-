#!/usr/bin/env python3
"""Compare the committed Criterion workload with Apache Arrow C++ via pyarrow.

Run the manifest-driven parquet-throughput benchmark first, then export its
inputs with the benchmark binary's --write-fixtures DIR mode. This driver
checks the file codec and every value before timing, uses the identical bytes
for reads, and matches the writer's encoding, nullability, and page settings.
All timings run sequentially; run with no competing benchmarks or builds.
"""

from __future__ import annotations

import argparse
import json
from pathlib import Path
import statistics
import subprocess
import tempfile
import time

import pyarrow as pa
import pyarrow.parquet as pq

N_ROWS = 100_000
CODECS = ("uncompressed", "snappy", "zstd")


def make_dataset() -> pa.Table:
    schema = pa.schema([
        pa.field("id", pa.int64(), nullable=False),
        pa.field("score", pa.float64(), nullable=False),
        pa.field("name", pa.string(), nullable=False),
        pa.field("active", pa.bool_(), nullable=False),
    ])
    return pa.Table.from_arrays([
        pa.array(range(N_ROWS), pa.int64()),
        pa.array([i * 0.5 for i in range(N_ROWS)], pa.float64()),
        pa.array([f"name_{i % 1000}" for i in range(N_ROWS)], pa.string()),
        pa.array([i % 2 == 0 for i in range(N_ROWS)], pa.bool_()),
    ], schema=schema)


def load_criterion(path: Path) -> dict[str, float]:
    doc = json.loads(path.read_text())
    return {
        report["reportName"]: report["reportAnalysis"]["anMean"]["estPoint"]
        for report in doc[2]
    }


def measure(action, iterations: int) -> float:
    for _ in range(3):
        action()
    times = []
    for _ in range(iterations):
        start = time.perf_counter()
        action()
        times.append(time.perf_counter() - start)
    return statistics.mean(times)


def write_table(table: pa.Table, codec: str) -> pa.Buffer:
    sink = pa.BufferOutputStream()
    kwargs = {"compression_level": 3} if codec == "zstd" else {}
    pq.write_table(
        table, sink, compression="none" if codec == "uncompressed" else codec,
        use_dictionary=False, column_encoding="PLAIN", data_page_version="2.0", write_page_index=False, store_schema=False,
        row_group_size=N_ROWS, max_rows_per_page=N_ROWS, data_page_size=8 * 1024 * 1024, **kwargs,
    )
    return sink.getvalue()


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--criterion-json", type=Path,
                        default=Path("dist-stats/bench-raw/parquet-throughput.json"))
    parser.add_argument("--fixtures", type=Path, required=True)
    parser.add_argument("--iterations", type=int, default=25)
    parser.add_argument("--output", type=Path)
    parser.add_argument("--parquet-rs-json", type=Path,
                        help="optional manifest-generated parquet-rs-throughput.json")
    parser.add_argument("--wireform-bench", type=Path,
                        help="benchmark binary: also verify C++ output layout and all values before timing")
    args = parser.parse_args()
    if args.iterations < 1:
        parser.error("--iterations must be positive")
    reference = load_criterion(args.criterion_json)
    parquet_rs = load_criterion(args.parquet_rs_json) if args.parquet_rs_json else None
    expected = make_dataset()
    results = []
    # Validate reference writer output, not just wireform's input to reads.
    # Native layout verification runs before any clock starts.
    with tempfile.TemporaryDirectory(prefix="wireform-cpp-bench-check-") as tmp:
        for codec in CODECS:
            encoded = write_table(expected, codec)
            decoded = pq.read_table(pa.BufferReader(encoded), use_threads=False, pre_buffer=False)
            if not decoded.equals(expected):
                raise AssertionError(f"C++ {codec} writer output differs from the full model")
            (Path(tmp) / f"{codec}.parquet").write_bytes(encoded.to_pybytes())
        if args.wireform_bench:
            subprocess.run([str(args.wireform_bench.resolve()), "--verify-fixtures", tmp], check=True)
    for codec in CODECS:
        blob = pa.py_buffer((args.fixtures / f"{codec}.parquet").read_bytes())
        metadata = pq.ParquetFile(pa.BufferReader(blob)).metadata
        actual_codec = metadata.row_group(0).column(0).compression.lower()
        if actual_codec != codec:
            raise AssertionError(f"{codec} fixture actually uses {actual_codec}")
        if not pq.read_table(pa.BufferReader(blob), use_threads=False, pre_buffer=False).equals(expected):
            raise AssertionError(f"{codec} fixture has incorrect data")
        for operation in ("write", "read"):
            if operation == "write":
                key = f"write {N_ROWS} rows x 4 cols/{codec}"
                action = lambda: write_table(expected, codec)
            else:
                key = f"read {N_ROWS} rows x 4 cols ({codec})/decode"
                action = lambda: pq.read_table(pa.BufferReader(blob), use_threads=False,
                                               pre_buffer=False)
            wireform_seconds = reference[key]  # Missing evidence must fail.
            arrow_seconds = measure(action, args.iterations)
            result = {
                "operation": operation, "codec": codec,
                "wireformSeconds": wireform_seconds, "pyarrowSeconds": arrow_seconds,
                "wireformOverPyarrow": wireform_seconds / arrow_seconds,
            }
            if parquet_rs is not None:
                result.update(parquetRsSeconds=parquet_rs[key],
                              wireformOverParquetRs=wireform_seconds / parquet_rs[key])
            results.append(result)
            print(f"{operation:5} {codec:12} wireform={wireform_seconds * 1000:9.3f} ms "
                  f"pyarrow={arrow_seconds * 1000:9.3f} ms "
                  f"ratio={result['wireformOverPyarrow']:.2f}x")
            if parquet_rs is not None:
                print(f"      parquet-rs={parquet_rs[key] * 1000:9.3f} ms "
                      f"wireform/parquet-rs={result['wireformOverParquetRs']:.2f}x")
    report = {"pyarrowVersion": pa.__version__, "rows": N_ROWS,
              "iterations": args.iterations,
              "workload": {"rowGroups": 1, "rowsPerPage": N_ROWS,
                           "columnsRequired": True, "encoding": "PLAIN",
                           "dataPageVersion": "2.0", "pageIndex": False,
                           "readOutput": "materialized Arrow batches",
                           "statistics": {"wireform": "chunk", "parquetRs": "chunk",
                                          "pyarrow": "page and chunk"}},
              "results": results}
    if args.output:
        args.output.write_text(json.dumps(report, indent=2) + "\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
