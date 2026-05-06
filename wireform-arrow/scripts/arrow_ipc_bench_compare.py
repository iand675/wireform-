#!/usr/bin/env python3
"""Compare wireform-arrow Arrow IPC stream throughput against
pyarrow on the same shape.

Run:

    cabal bench --enable-benchmarks wireform-arrow:arrow-ipc-throughput \\
      --benchmark-options='--csv /tmp/wf_arrow.csv --time-limit 4'
    python3 wireform-arrow/scripts/arrow_ipc_bench_compare.py
"""

from __future__ import annotations

import csv
import io
import time
from pathlib import Path

import pyarrow as pa
import pyarrow.ipc as ipc

N_ROWS = 100_000
ITERS = 5

def make_table(n: int) -> pa.Table:
    return pa.table({
        "id":     pa.array(range(n), pa.int64()),
        "score":  pa.array([i * 0.5 for i in range(n)], pa.float64()),
        "name":   pa.array([f"name_{i % 1000}" for i in range(n)], pa.string()),
        "active": pa.array([i % 2 == 0 for i in range(n)], pa.bool_()),
    })

def time_pyarrow_write(table: pa.Table) -> tuple[float, int]:
    sink = pa.BufferOutputStream()
    writer = ipc.new_stream(sink, table.schema)
    writer.write_table(table)
    writer.close()
    blob = sink.getvalue().to_pybytes()
    nbytes = len(blob)
    times = []
    for _ in range(ITERS):
        t0 = time.perf_counter()
        sink2 = pa.BufferOutputStream()
        writer2 = ipc.new_stream(sink2, table.schema)
        writer2.write_table(table)
        writer2.close()
        _ = sink2.getvalue()
        times.append(time.perf_counter() - t0)
    return (min(times), nbytes)

def time_pyarrow_read(blob: bytes) -> float:
    times = []
    for _ in range(ITERS):
        t0 = time.perf_counter()
        reader = ipc.open_stream(pa.BufferReader(blob))
        _ = reader.read_all()
        times.append(time.perf_counter() - t0)
    return min(times)

def load_wireform_csv(path: Path) -> dict[str, float]:
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
    table = make_table(N_ROWS)
    pa_w_t, pa_w_sz = time_pyarrow_write(table)
    sink = pa.BufferOutputStream()
    writer = ipc.new_stream(sink, table.schema)
    writer.write_table(table)
    writer.close()
    blob = sink.getvalue().to_pybytes()
    pa_r_t = time_pyarrow_read(blob)

    wf = load_wireform_csv(Path("/tmp/wf_arrow.csv"))

    print(f"\n{N_ROWS:,}-row Arrow IPC stream (4 cols: int64, double, utf8, bool)\n")
    print(f"{'workload':<28} {'wireform':>12} {'pyarrow':>12} {'ratio':>8}")
    print("-" * 65)

    wf_w = wf.get(f"write 100k x 4 cols (Arrow IPC stream)/encode")
    wf_r = wf.get(f"read 100k x 4 cols (Arrow IPC stream)/decode")

    for label, wf_t, pa_t in [
        ("write", wf_w, pa_w_t),
        ("read",  wf_r, pa_r_t),
    ]:
        if wf_t is None:
            print(f"  {label:<26} {'(no wf data)':>12}")
            continue
        ratio = wf_t / pa_t
        tag = "faster" if ratio < 1 else "slower"
        print(f"  {label:<26} {wf_t*1000:>9.1f} ms"
              f" {pa_t*1000:>9.1f} ms  {ratio:>5.2f}x  ({tag})")

    print(f"\nFile size: {pa_w_sz:,} bytes")
    return 0

if __name__ == "__main__":
    raise SystemExit(main())
