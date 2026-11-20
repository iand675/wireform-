#!/usr/bin/env python3
"""Check dictionary byte keys, slice boundaries and larger payloads independently."""
import argparse
from pathlib import Path
import subprocess
import tempfile

import duckdb
import polars as pl
import pyarrow as pa
import pyarrow.ipc as ipc
import pyarrow.parquet as pq

from parquet_dictionary_ieee_interop import dictionary_file
from parquet_writer_options_interop import thrift_at


def fixtures():
    def short_values(maximum):
        return list(map(lambda size: bytes(size), range(maximum + 1))) + list(map(
            lambda size: bytes(range(size)), range(maximum + 1)))
    random = []
    value = 0x123456789abcdef0
    for _ in range(257):
        random.append(value)
        value = (value * 6364136223846793005 + 1442695040888963407) & (2**64 - 1)
    sparse = list(map(lambda word: word.to_bytes(8, 'little'), random[:129]))
    wide = []
    for index, word in enumerate(random[:129]):
        width = (64, 256, 4096)[index % 3]
        wide.append(word.to_bytes(8, 'little') + b'a' * (width - 8))
    common_ends = list(map(lambda word: b'a' * 24 + word.to_bytes(8, 'little') + b'a' * 32,
                           random[:129]))
    late = list(map(lambda word: word.to_bytes(8, 'little') + b'a' * 56, random[:129]))
    late.append(random[0].to_bytes(8, 'little') + b'a' * 16 + b'b' * 8 + b'a' * 32)
    late.extend(map(lambda word: word.to_bytes(8, 'little') + b'a' * 56, random[129:]))
    late.append(random[0].to_bytes(8, 'little') + b'a' * 32 + b'c' * 8 + b'a' * 16)
    cases = {'short': short_values(8), 'tagged-short': short_values(7), 'mixed': short_values(40),
             'sparse8': sparse, 'wide': wide, 'common-ends': common_ends,
             'late-promotion': late}
    for name, cardinality in [('rle-binary', 2), ('rle-mixed', 16), ('rle-wide', 257)]:
        values = list(range(cardinality))
        for key, count in enumerate((7, 8, 9, 15, 16, 17, 23, 24, 31, 32, 33, 63, 64, 65)):
            values.extend([key % cardinality] * count)
            values.extend((0, 1, 0))
        cases[name] = list(map(lambda value: value.to_bytes(8, 'little'), values))
    for name, values in cases.items():
        cases[name] = values + list(reversed(values))
    return cases


def run_kinds(body):
    """Inspect run framing independently of Wireform and the external engines."""
    width, offset, kinds = body[0], 1, set()
    assert 0 <= width <= 32
    while offset < len(body):
        header, shift = 0, 0
        while True:
            byte = body[offset]
            offset += 1
            header |= (byte & 127) << shift
            if byte < 128:
                break
            shift += 7
            assert shift < 35
        assert header > 0
        packed = bool(header & 1)
        count = header >> 1
        assert 0 < count * (8 if packed else 1) <= 2**31 - 1
        offset += count * width if packed else (width + 7) // 8
        assert offset <= len(body)
        kinds.add(packed)
    assert offset == len(body)
    return kinds


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--probe', type=Path, required=True)
    parser.add_argument('--reader', type=Path)
    args = parser.parse_args()
    with tempfile.TemporaryDirectory(prefix='wireform-parquet-dict-bytes-') as directory:
        root = Path(directory)
        subprocess.run([str(args.probe.resolve()), '--dictionary-bytes', str(root)], check=True)
        cases = fixtures()
        for name, values in cases.items():
            expected = pa.array(values, pa.binary())
            table = pa.Table.from_arrays([expected], schema=pa.schema([
                pa.field('x', pa.binary(), nullable=False)]))
            seed = root / 'seed.parquet'
            pq.write_table(table, seed, compression='NONE', use_dictionary=False,
                           write_statistics=False)
            dictionary = (root / f'{name}.dictionary.bin').read_bytes()
            data = (root / f'{name}.data.bin').read_bytes()
            header, _ = thrift_at(dictionary, 0)
            assert header[1] == 2 and header[7][2] == 0
            if name.startswith('rle-'):
                _, payload_start = thrift_at(data, 0)
                assert run_kinds(data[payload_start:]) == {False, True}, name
            path = root / f'{name}.parquet'
            dictionary_file(seed, path, dictionary, data)
            assert pq.read_table(path, use_threads=False)['x'].to_pylist() == values
            assert pl.read_parquet(path)['x'].to_list() == values
            connection = duckdb.connect(config={'threads': 1})
            try:
                actual = connection.execute('SELECT x FROM read_parquet(?)', [str(path)]).fetchall()
                assert list(map(lambda row: row[0], actual)) == values
            finally:
                connection.close()
            print(f'PASS {name}: exact byte arrays in PyArrow, DuckDB and Polars')
        seed.unlink()
        if args.reader:
            decoded = root / 'decoded'
            subprocess.run([str(args.reader.resolve()), str(root), '--arrow-output', str(decoded)], check=True)
            for name, values in cases.items():
                with ipc.open_stream(decoded / (name + '.parquet.arrows')) as reader:
                    actual = reader.read_all()['x'].to_pylist()
                assert actual == values
            print(f'PASS Wireform Arrow output: exact bytes in all {len(cases)} dictionary files')


if __name__ == '__main__':
    main()
