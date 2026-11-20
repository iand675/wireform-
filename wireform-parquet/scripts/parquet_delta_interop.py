#!/usr/bin/env python3
"""Check real integer delta encoder bytes with three independent readers.

Apache Thrift supplies the page framing. Expected signed and unsigned values
come from separate Python boundary/LCG fixtures, never Wireform's decoder.
"""

import argparse
from pathlib import Path
import subprocess
import tempfile

import duckdb
import polars as pl
import pyarrow as pa
import pyarrow.ipc as ipc
import pyarrow.parquet as pq

from parquet_aggregate_interop import (
    TCompactProtocol, TMemoryBuffer, TType, column_metadata, footer, groups,
    write_node,
)


def datasets(bits):
    low, high = -(2**(bits - 1)), 2**(bits - 1) - 1
    cases = {
        'minimum': [low],
        'maximum': [high],
        'constant-129': [low] * 129,
        'mixed-sign-delta': [0, low, low + 1],
        'wrap-up': [low, high, low],
        'wrap-down': [high, low, high],
        'step-up': [low, high],
        'step-down': [high, low],
        'edges-257': ([low, low + 1, high, high - 1, 0, -1, 1] * 37)[:257],
    }
    random_values = []
    state = 0x12345678 if bits == 32 else 0x123456789abcdef0
    multiplier = 1664525 if bits == 32 else 6364136223846793005
    increment = 1013904223 if bits == 32 else 1442695040888963407
    mask = 2**bits - 1
    for _ in range(513):
        random_values.append(state if state < 2**(bits - 1) else state - 2**bits)
        state = (state * multiplier + increment) & mask
    cases['random-513'] = random_values
    for count in (31, 32, 33, 127, 128, 129, 130):
        cases[f'alternating-{count}'] = ([low, high] * ((count + 1) // 2))[:count]
    for width in range(1, bits + 1):
        mask = 2**width - 1
        count = 2 + width % 31
        offsets = [0, 2**(width - 1), mask] + [104729 & mask, 7 & mask] * 16
        rows = [0]
        for offset in offsets[:count]:
            rows.append((rows[-1] + low + offset - low) % (2**bits) + low)
        cases[f'bitwidth-{width}'] = rows
    return cases


def encoded_struct(fields):
    transport = TMemoryBuffer()
    write_node(TCompactProtocol(transport), (TType.STRUCT, fields))
    return transport.getvalue()


def framed_file(seed, destination, payload, count, version):
    _, metadata = footer(seed)
    if version == 1:
        page = {1: (TType.I32, count), 2: (TType.I32, 5),
                3: (TType.I32, 3), 4: (TType.I32, 3)}
        header = {1: (TType.I32, 0), 5: (TType.STRUCT, page)}
    else:
        page = {1: (TType.I32, count), 2: (TType.I32, 0),
                3: (TType.I32, count), 4: (TType.I32, 5),
                5: (TType.I32, 0), 6: (TType.I32, 0),
                7: (TType.BOOL, False)}
        header = {1: (TType.I32, 3), 8: (TType.STRUCT, page)}
    header[2] = TType.I32, len(payload)
    header[3] = TType.I32, len(payload)
    chunk = encoded_struct(header) + payload
    column = column_metadata(metadata)
    column[2] = TType.LIST, (TType.I32, [(TType.I32, 5), (TType.I32, 3)])
    column[6] = TType.I64, len(chunk)
    column[7] = TType.I64, len(chunk)
    column[9] = TType.I64, 4
    column.pop(11, None)  # No dictionary page.
    column.pop(13, None)  # Discard the seed's PLAIN encoding statistics.
    group = groups(metadata)[0][1]
    group[2] = TType.I64, len(chunk)
    group[6] = TType.I64, len(chunk)
    encoded_footer = encoded_struct(metadata[1])
    destination.write_bytes(b'PAR1' + chunk + encoded_footer
                            + len(encoded_footer).to_bytes(4, 'little') + b'PAR1')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--probe', required=True, type=Path)
    parser.add_argument('--reader', type=Path)
    args = parser.parse_args()
    checked = 0
    expected_tables = {}
    with tempfile.TemporaryDirectory(prefix='wireform-parquet-delta-') as directory:
        root = Path(directory)
        connection = duckdb.connect(config={'threads': 1})
        try:
            for bits in (32, 64):
                cases = datasets(bits)
                raw = root / f'raw{bits}'
                subprocess.run([str(args.probe.resolve()), f'--delta-int{bits}', str(raw)], check=True)
                assert len(list(raw.glob('*.bin'))) == len(cases)
                for name, signed_values in cases.items():
                    payload = (raw / (name + '.bin')).read_bytes()
                    for unsigned in (False, True):
                        typ = getattr(pa, f'uint{bits}' if unsigned else f'int{bits}')()
                        expected = list(map(lambda value: value & (2**bits - 1), signed_values)) if unsigned else signed_values
                        table = pa.Table.from_arrays([pa.array(expected, typ)],
                            schema=pa.schema([pa.field('x', typ, nullable=False)]))
                        seed = root / 'seed.parquet'
                        pq.write_table(table, seed, version='2.6', compression='NONE',
                                       use_dictionary=False, write_statistics=False)
                        for version in (1, 2):
                            path = root / f'{name}-{"u" if unsigned else "i"}{bits}-v{version}.parquet'
                            framed_file(seed, path, payload, len(expected), version)
                            try:
                                actual = pq.read_table(path, use_threads=False)
                            except Exception as error:
                                raise AssertionError(f'{path.name}: PyArrow could not decode encoder output') from error
                            assert actual.equals(table), (path.name, 'PyArrow values/types', actual.to_pydict(), table.to_pydict())
                            rows = connection.execute('SELECT x FROM read_parquet(?)', [str(path)]).fetchall()
                            assert list(map(lambda row: row[0], rows)) == expected, (path.name, 'DuckDB values')
                            actual = pl.read_parquet(path)
                            assert actual.schema['x'] == getattr(pl, f'UInt{bits}' if unsigned else f'Int{bits}'), path.name
                            assert actual['x'].to_list() == expected, (path.name, 'Polars values')
                            checked += 1
                            expected_tables[path.name] = table
                            print('PASS', path.name)
            if args.reader:
                seed.unlink()
                decoded = root / 'decoded'
                subprocess.run([str(args.reader.resolve()), str(root), '--arrow-output', str(decoded)], check=True)
                for name, expected_table in expected_tables.items():
                    with ipc.open_stream(decoded / (name + '.arrows')) as reader:
                        actual = reader.read_all()
                    assert actual.equals(expected_table), (name, 'Wireform values/types')
        finally:
            connection.close()
    print(f'{checked} integer delta files verified by PyArrow, DuckDB and Polars')
    if args.reader:
        print(f'{checked} integer delta files also matched Wireform Arrow output')


if __name__ == '__main__':
    main()
