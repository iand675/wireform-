#!/usr/bin/env python3
"""End-to-end Parquet write + read throughput comparison
between wireform-parquet and pyarrow on the same synthetic
dataset shape (4 columns: int64, double, utf8, bool).

For a fair comparison we measure both libraries in
single-threaded and multi-threaded modes:

  * wireform with `+RTS -N1 -RTS`   (single capability)
  * wireform with `+RTS -N -RTS`    (use all capabilities)
  * pyarrow  with use_threads=False (single-threaded)
  * pyarrow  with use_threads=True  (default; uses CPU pool)

Run:

    cabal bench --enable-benchmarks wireform-parquet:parquet-throughput \\
      --benchmark-options='--csv /tmp/wf_n1.csv --time-limit 4'
    cabal bench --enable-benchmarks wireform-parquet:parquet-throughput \\
      --benchmark-options='--csv /tmp/wf_n4.csv --time-limit 4 +RTS -N -RTS'
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
    buf = pa.BufferOutputStream()
    pq.write_table(table, buf, compression=codec or "none")
    blob = buf.getvalue().to_pybytes()
    nbytes = len(blob)
    times = []
    for _ in range(ITERS):
        t0 = time.perf_counter()
        buf2 = pa.BufferOutputStream()
        pq.write_table(table, buf2, compression=codec or "none")
        _ = buf2.getvalue()
        times.append(time.perf_counter() - t0)
    return (min(times), nbytes)

def time_pyarrow_read(blob: bytes, use_threads: bool) -> float:
    times = []
    for _ in range(ITERS):
        t0 = time.perf_counter()
        _ = pq.read_table(pa.BufferReader(blob), use_threads=use_threads)
        times.append(time.perf_counter() - t0)
    return min(times)

def load_wireform_csv(path: Path) -> dict[str, float]:
    """Parse a criterion CSV and return {benchmark name: mean seconds}."""
    out = {}
    if not path.exists():
        return out
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

    pa_wu_t, pa_wu_sz = time_pyarrow_write(table, None)
    pa_ws_t, pa_ws_sz = time_pyarrow_write(table, "snappy")
    pa_wz_t, pa_wz_sz = time_pyarrow_write(table, "zstd")

    buf = pa.BufferOutputStream()
    pq.write_table(table, buf, compression="none")
    blob_u = buf.getvalue().to_pybytes()
    buf = pa.BufferOutputStream()
    pq.write_table(table, buf, compression="snappy")
    blob_s = buf.getvalue().to_pybytes()
    buf = pa.BufferOutputStream()
    pq.write_table(table, buf, compression="zstd")
    blob_z = buf.getvalue().to_pybytes()

    pa_ru_t  = time_pyarrow_read(blob_u, use_threads=False)
    pa_rs_t  = time_pyarrow_read(blob_s, use_threads=False)
    pa_rz_t  = time_pyarrow_read(blob_z, use_threads=False)
    pa_ru_tT = time_pyarrow_read(blob_u, use_threads=True)
    pa_rs_tT = time_pyarrow_read(blob_s, use_threads=True)
    pa_rz_tT = time_pyarrow_read(blob_z, use_threads=True)

    wf_n1 = load_wireform_csv(Path("/tmp/wf_n1.csv"))
    wf_n4 = load_wireform_csv(Path("/tmp/wf_n4.csv"))
    if not wf_n1:
        # Fall back to legacy CSV path so old workflows keep working.
        wf_n1 = load_wireform_csv(Path("/tmp/wireform_throughput.csv"))

    print(f"\n{N_ROWS:,}-row Parquet (4 cols: int64, double, utf8, bool)\n")
    print("Single-threaded comparison "
          "(wireform +RTS -N1, pyarrow use_threads=False)")
    print(f"{'workload':<28} {'wireform':>12} {'pyarrow':>12} {'ratio':>8}")
    print("-" * 65)
    for codec in ("uncompressed", "snappy", "zstd"):
        wf_t = wf_n1.get(f"write {N_ROWS} rows x 4 cols/{codec}")
        pa_t = {"uncompressed": pa_wu_t, "snappy": pa_ws_t, "zstd": pa_wz_t}[codec]
        if wf_t is not None:
            tag = "faster" if wf_t / pa_t < 1 else "slower"
            print(f"  write {codec:<20} {wf_t*1000:>9.1f} ms"
                  f" {pa_t*1000:>9.1f} ms  {wf_t/pa_t:>5.2f}x  ({tag})")
    print()
    for codec in ("uncompressed", "snappy", "zstd"):
        wf_t = wf_n1.get(f"read {N_ROWS} rows x 4 cols ({codec})/decode")
        pa_t = {"uncompressed": pa_ru_t, "snappy": pa_rs_t, "zstd": pa_rz_t}[codec]
        if wf_t is not None:
            tag = "faster" if wf_t / pa_t < 1 else "slower"
            print(f"  read  {codec:<20} {wf_t*1000:>9.1f} ms"
                  f" {pa_t*1000:>9.1f} ms  {wf_t/pa_t:>5.2f}x  ({tag})")

    print()
    if wf_n4:
        print("Multi-threaded comparison "
              "(wireform parMap +RTS -N, pyarrow use_threads=True)")
        print(f"{'workload':<28} {'wireform':>12} {'pyarrow':>12} {'ratio':>8}")
        print("-" * 65)
        for codec in ("uncompressed", "snappy", "zstd"):
            wf_t = wf_n4.get(f"write {N_ROWS} rows x 4 cols/{codec}")
            pa_t = {"uncompressed": pa_wu_t, "snappy": pa_ws_t, "zstd": pa_wz_t}[codec]
            if wf_t is not None:
                tag = "faster" if wf_t / pa_t < 1 else "slower"
                print(f"  write {codec:<20} {wf_t*1000:>9.1f} ms"
                      f" {pa_t*1000:>9.1f} ms  {wf_t/pa_t:>5.2f}x  ({tag})")
        print()
        for codec in ("uncompressed", "snappy", "zstd"):
            wf_t = wf_n4.get(f"read {N_ROWS} rows x 4 cols ({codec})/decode-par")
            pa_t = {"uncompressed": pa_ru_tT, "snappy": pa_rs_tT, "zstd": pa_rz_tT}[codec]
            if wf_t is not None:
                tag = "faster" if wf_t / pa_t < 1 else "slower"
                print(f"  read  {codec:<20} {wf_t*1000:>9.1f} ms"
                      f" {pa_t*1000:>9.1f} ms  {wf_t/pa_t:>5.2f}x  ({tag})")

    print(f"\nFile sizes (bytes): uncompressed={pa_wu_sz:,} "
          f"· snappy={pa_ws_sz:,} · zstd={pa_wz_sz:,}")
    return 0

if __name__ == "__main__":
    raise SystemExit(main())
