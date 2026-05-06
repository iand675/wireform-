#!/usr/bin/env python3
"""End-to-end Parquet write + read throughput comparison
between wireform-parquet and pyarrow on the same synthetic
dataset shape (4 columns: int64, double, utf8, bool).

For each row count + codec:

  * Generate the dataset in pyarrow + wireform-parquet
    formats.
  * Time pyarrow's pq.write_table to bytes (via BytesIO).
  * Time wireform's encode (via the throughput benchmark
    binary; this script just reads its CSV output).
  * Time pyarrow's pq.read_table from those bytes.
  * Print a side-by-side table.

Run:

    cabal bench wireform-parquet:parquet-throughput \\
      --benchmark-options='--csv /tmp/wireform_throughput.csv \\
                            --time-limit 3'
    python3 wireform-parquet/scripts/parquet_bench_compare.py
"""

from __future__ import annotations

import csv
import io
import os
import time
from pathlib import Path

import pyarrow as pa
import pyarrow.parquet as pq

N_ROWS = 100_000
ITERS = 5

def make_dataset(n: int) -> pa.Table:
    return pa.table({
        "id":     pa.array(range(n), pa.int64()),
        "score":  pa.array([i * 0.5 for i in range(n)], pa.float64()),
        "name":   pa.array([f"name_{i % 1000}" for i in range(n)], pa.string()),
        "active": pa.array([i % 2 == 0 for i in range(n)], pa.bool_()),
    })

def time_pyarrow_write(table: pa.Table, codec: str | None) -> tuple[float, int]:
    """Returns (best wall-clock seconds, file size in bytes)."""
    buf = pa.BufferOutputStream()
    pq.write_table(table, buf, compression=codec or "none")
    blob = buf.getvalue().to_pybytes()
    nbytes = len(blob)

    # Now time it cleanly (no allocation in the loop).
    times = []
    for _ in range(ITERS):
        t0 = time.perf_counter()
        buf2 = pa.BufferOutputStream()
        pq.write_table(table, buf2, compression=codec or "none")
        _ = buf2.getvalue()
        times.append(time.perf_counter() - t0)
    return (min(times), nbytes)

def time_pyarrow_read(blob: bytes, use_threads: bool = False) -> float:
    times = []
    for _ in range(ITERS):
        t0 = time.perf_counter()
        _ = pq.read_table(pa.BufferReader(blob), use_threads=use_threads)
        times.append(time.perf_counter() - t0)
    return min(times)

def load_wireform_csv(path: Path) -> dict[str, float]:
    """Parse the criterion CSV and return {benchmark name: mean seconds}."""
    out = {}
    if not path.exists():
        return out
    # Criterion concatenates one CSV header per benchmark
    # group, so 'csv' module needs to handle repeated headers.
    with path.open() as f:
        for row in csv.reader(f):
            if not row or row[0] == "Name":
                continue
            try:
                out[row[0]] = float(row[1])
            except (IndexError, ValueError):
                continue
    return out

def main() -> int:
    table = make_dataset(N_ROWS)

    # Time pyarrow on the same shape
    rows_per_sec = lambda secs: N_ROWS / secs if secs > 0 else 0
    mb_per_sec = lambda secs, sz: (sz / 1024 / 1024) / secs if secs > 0 else 0

    pa_wu_t, pa_wu_sz = time_pyarrow_write(table, None)
    pa_ws_t, pa_ws_sz = time_pyarrow_write(table, "snappy")
    pa_wz_t, pa_wz_sz = time_pyarrow_write(table, "zstd")

    blob_u = (lambda: (pa.BufferOutputStream().getvalue(), None))()  # placeholder
    # Build the readable blobs once
    buf = pa.BufferOutputStream()
    pq.write_table(table, buf, compression="none")
    blob_u = buf.getvalue().to_pybytes()
    buf = pa.BufferOutputStream()
    pq.write_table(table, buf, compression="snappy")
    blob_s = buf.getvalue().to_pybytes()
    buf = pa.BufferOutputStream()
    pq.write_table(table, buf, compression="zstd")
    blob_z = buf.getvalue().to_pybytes()

    pa_ru_t = time_pyarrow_read(blob_u)
    pa_rs_t = time_pyarrow_read(blob_s)
    pa_rz_t = time_pyarrow_read(blob_z)

    wf = load_wireform_csv(Path("/tmp/wireform_throughput.csv"))

    def wfval(key):
        return wf.get(f"{key} {N_ROWS} rows x 4 cols/{ codec(key) }".replace(" )", ")"))

    print(f"\n{N_ROWS:,}-row Parquet (4 cols: int64, double, utf8, bool)\n")
    print(f"{'workload':<30} {'wireform':>16} {'pyarrow':>16} {'ratio':>8}")
    print(f"{'-' * 75}")

    # write
    for codec in ("uncompressed", "snappy", "zstd"):
        wf_key = f"write {N_ROWS} rows x 4 cols/{codec}"
        wf_t = wf.get(wf_key)
        pa_t = {"uncompressed": pa_wu_t, "snappy": pa_ws_t, "zstd": pa_wz_t}[codec]
        if wf_t is None:
            print(f"  write {codec:<22} {'(no data)':>16} {pa_t*1000:>13.1f} ms  {'?':>8}")
            continue
        ratio = wf_t / pa_t
        print(
          f"  write {codec:<22}"
          f" {wf_t*1000:>13.1f} ms"
          f" {pa_t*1000:>13.1f} ms"
          f" {ratio:>7.2f}x"
        )

    print()
    # read
    for codec in ("uncompressed", "snappy", "zstd"):
        wf_key = f"read {N_ROWS} rows x 4 cols ({codec})/decode"
        wf_t = wf.get(wf_key)
        pa_t = {"uncompressed": pa_ru_t, "snappy": pa_rs_t, "zstd": pa_rz_t}[codec]
        if wf_t is None:
            print(f"  read  {codec:<22} {'(no data)':>16} {pa_t*1000:>13.1f} ms  {'?':>8}")
            continue
        ratio = wf_t / pa_t
        print(
          f"  read  {codec:<22}"
          f" {wf_t*1000:>13.1f} ms"
          f" {pa_t*1000:>13.1f} ms"
          f" {ratio:>7.2f}x"
        )

    print(f"\nFile sizes (bytes): uncompressed={pa_wu_sz:,}, snappy={pa_ws_sz:,}, zstd={pa_wz_sz:,}")
    print(f"\nThroughput summary (rows/sec):")
    if (wfu := wf.get(f"write {N_ROWS} rows x 4 cols/uncompressed")):
        print(f"  wireform write uncompressed: {rows_per_sec(wfu):>12,.0f} rows/s")
    print(  f"  pyarrow  write uncompressed: {rows_per_sec(pa_wu_t):>12,.0f} rows/s")
    if (wfru := wf.get(f"read {N_ROWS} rows x 4 cols (uncompressed)/decode")):
        print(f"  wireform read  uncompressed: {rows_per_sec(wfru):>12,.0f} rows/s")
    print(  f"  pyarrow  read  uncompressed: {rows_per_sec(pa_ru_t):>12,.0f} rows/s")
    return 0

def codec(name):
    if "uncompressed" in name: return "uncompressed"
    if "snappy" in name: return "snappy"
    if "zstd" in name: return "zstd"
    return "?"

if __name__ == "__main__":
    raise SystemExit(main())
