#!/usr/bin/env python3
"""Check dictionary NaNs, signed zero and IEEE payloads with reference readers."""
import argparse
import math
from pathlib import Path
import subprocess
import tempfile

import duckdb
import polars as pl
import pyarrow as pa
import pyarrow.ipc as ipc
import pyarrow.parquet as pq

from parquet_aggregate_interop import TType, column_metadata, footer, groups
from parquet_delta_interop import encoded_struct
from parquet_writer_options_interop import thrift_at


def fixture(bits):
    if bits == 32:
        values = [0x3f800000, 0x7fc01234, 0xffa05678, 0x7fc01234, 0,
                  0x80000000, 0, 0x80000000, 0x7f800000, 0xff800000, 1, 0xffffffff]
        state, multiplier, increment = 0x12345678, 1664525, 1013904223
    else:
        values = [0x3ff0000000000000, 0x7ff8123456789abc, 0xfff056789abcdef0,
                  0x7ff8123456789abc, 0, 0x8000000000000000, 0,
                  0x8000000000000000, 0x7ff0000000000000, 0xfff0000000000000,
                  1, 0xffffffffffffffff]
        state, multiplier, increment = (0x123456789abcdef0,
                                       6364136223846793005, 1442695040888963407)
    for _ in range(129):
        values.append(state)
        state = (state * multiplier + increment) & (2**bits - 1)
    return values + list(reversed(values))


def check_semantics(actual, expected):
    assert len(actual) == len(expected)
    for left, right in zip(actual, expected, strict=True):
        if math.isnan(right):
            assert math.isnan(left)
        else:
            assert left == right
            if right == 0:
                assert math.copysign(1, left) == math.copysign(1, right)


def dictionary_file(seed, path, dictionary, data):
    _, metadata = footer(seed)
    column = column_metadata(metadata)
    column[2] = TType.LIST, (TType.I32, [(TType.I32, 0), (TType.I32, 8)])
    column[6] = column[7] = TType.I64, len(dictionary) + len(data)
    column[9] = TType.I64, 4 + len(dictionary)
    column[11] = TType.I64, 4
    column.pop(13, None)
    group = groups(metadata)[0][1]
    group[2] = group[6] = TType.I64, len(dictionary) + len(data)
    encoded_footer = encoded_struct(metadata[1])
    path.write_bytes(b'PAR1' + dictionary + data + encoded_footer
                     + len(encoded_footer).to_bytes(4, 'little') + b'PAR1')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--probe', type=Path, required=True)
    parser.add_argument('--reader', type=Path)
    args = parser.parse_args()
    with tempfile.TemporaryDirectory(prefix='wireform-parquet-dict-ieee-') as directory:
        root = Path(directory)
        subprocess.run([str(args.probe.resolve()), '--dictionary-ieee', str(root)], check=True)
        expected_bits = {}
        for bits in (32, 64):
            values = fixture(bits)
            uint_type = getattr(pa, f'uint{bits}')()
            float_type = getattr(pa, f'float{bits}')()
            expected = pa.array(values, uint_type).view(float_type)
            seed = root / 'seed.parquet'
            table = pa.Table.from_arrays([expected], schema=pa.schema([
                pa.field('x', float_type, nullable=False)]))
            pq.write_table(table, seed, compression='NONE',
                           use_dictionary=False, write_statistics=False)
            dictionary = (root / f'f{bits}.dictionary.bin').read_bytes()
            data = (root / f'f{bits}.data.bin').read_bytes()
            header, _ = thrift_at(dictionary, 0)
            assert header[1] == 2 and header[7][2] == 0, 'dictionary page must use PLAIN'
            path = root / f'f{bits}.parquet'
            dictionary_file(seed, path, dictionary, data)
            actual = pq.read_table(path, use_threads=False)['x'].combine_chunks()
            assert actual.type == float_type and actual.view(uint_type).to_pylist() == values
            polars = pl.read_parquet(path)['x'].to_arrow()
            assert polars.type == float_type
            check_semantics(polars.to_pylist(), expected.to_pylist())
            connection = duckdb.connect(config={'threads': 1})
            try:
                result = connection.execute('SELECT x FROM read_parquet(?)', [str(path)]).to_arrow_table()['x'].combine_chunks()
                assert result.type == float_type
                check_semantics(result.to_pylist(), expected.to_pylist())
            finally:
                connection.close()
            expected_bits[path.name] = bits, values
            print(f'PASS f{bits}: PyArrow exact IEEE bits; Polars/DuckDB NaNs, infinities, signed zero and finite values')
        seed.unlink()
        if args.reader:
            decoded = root / 'decoded'
            subprocess.run([str(args.reader.resolve()), str(root), '--arrow-output', str(decoded)], check=True)
            for name, (bits, values) in expected_bits.items():
                with ipc.open_stream(decoded / (name + '.arrows')) as reader:
                    actual = reader.read_all()['x'].combine_chunks()
                assert actual.view(getattr(pa, f'uint{bits}')()).to_pylist() == values
            print('PASS Wireform Arrow output: exact IEEE bits in both dictionary files')


if __name__ == '__main__':
    main()
