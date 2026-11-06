#!/usr/bin/env python3
"""Read the official >2GiB map fixture through64-bit native Arrow offsets.

The reference reads one row per batch. Hash buffer spans directly, never
materializing gigabyte Python strings or a second full IPC copy on disk.
Native and reference processes run sequentially to bound peak memory.
"""
import argparse
import ast
import hashlib
import json
from pathlib import Path
import struct
import subprocess
import pyarrow as pa
import pyarrow.parquet as pq


def reference(path):
    digest = hashlib.sha256()
    key_offsets = [0]
    values = []
    rows = 0
    for batch in pq.ParquetFile(path).iter_batches(batch_size=1, use_threads=False):
        batch.validate(full=True)
        assert batch.num_rows == 1 and batch.num_columns == 1
        mapping = batch.column(0)
        assert pa.types.is_map(mapping.type) and mapping.null_count == 0
        assert mapping.offsets.to_pylist() == [0, 1]
        keys = mapping.keys
        assert pa.types.is_string(keys.type) and keys.null_count == 0 and len(keys) == 1
        start, end = struct.unpack_from('<ii', memoryview(keys.buffers()[1]), keys.offset * 4)
        span = memoryview(keys.buffers()[2])[start:end]
        digest.update(span)
        key_offsets.append(key_offsets[-1] + len(span))
        values.extend(mapping.items.to_pylist())
        rows += 1
    return {'rows': rows, 'map_nulls': 0, 'map_offsets': list(range(rows + 1)), 'key_nulls': 0, 'key_offsets': key_offsets, 'key_bytes': key_offsets[-1], 'key_sha256': digest.hexdigest(), 'value_nulls': 0, 'values': values}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--reader', type=Path, required=True)
    parser.add_argument('--fixture', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--max-heap-mib', type=int, default=6144)
    args = parser.parse_args()
    assert args.fixture.stat().st_size < 10000
    native = subprocess.run([str(args.reader), '--large-map-summary', str(args.fixture), '+RTS', f'-M{args.max_heap_mib}m', '-RTS'], capture_output=True, text=True, timeout=300)
    assert native.returncode == 0, (native.returncode, native.stdout, native.stderr)
    actual = {}
    for line in native.stdout.splitlines():
        key, value = line.split('=', 1)
        actual[key] = value if key == 'key_sha256' else ast.literal_eval(value)
    expected = reference(args.fixture)
    assert actual == expected, (actual, expected)
    assert actual['rows'] == 2 and actual['key_offsets'] == [0, 2**30, 2**31] and actual['values'] == [1, 1]
    report = {'pyarrow': pa.__version__, 'fixture': args.fixture.name, 'nativeProjection': 'LargeUtf8 map keys, standard Map offsets', 'decodedKeyBytes': actual['key_bytes'], 'rows': actual['rows'], 'keySHA256': actual['key_sha256'], 'nativeAndReferenceAgree': True, 'failures': 0, 'skips': 0}
    args.output.write_text(json.dumps(report, indent=2) + '\n')
    print(json.dumps(report, indent=2))


if __name__ == '__main__':
    main()
