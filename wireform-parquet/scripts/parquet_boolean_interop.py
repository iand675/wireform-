#!/usr/bin/env python3
"""Check packed BOOLEAN PLAIN/RLE/dictionary pages against independent readers.

The Boolean payloads and hybrid streams below are built directly from the
published encoding grammar, independently of wireform's writer. DuckDB
validates all cases; PyArrow validates PLAIN/RLE and reports its Boolean
dictionary limitation explicitly. wireform output is compared through IPC.
"""
import argparse
import itertools
from pathlib import Path
import subprocess
import tempfile

import duckdb
import pyarrow as pa
import pyarrow.ipc as ipc
import pyarrow.parquet as pq
from parquet_aggregate_interop import TType, column_metadata, footer, groups
from parquet_delta_interop import encoded_struct


def varint(value):
    out = bytearray()
    while value >= 128:
        out.append((value & 127) | 128)
        value >>= 7
    out.append(value)
    return bytes(out)


def bits(values):
    out = bytearray((len(values) + 7) // 8)
    for index, value in enumerate(values):
        if value:
            byte_index, bit_index = divmod(index, 8)
            out[byte_index] |= 1 << bit_index
    if values and len(values) % 8:
        out[-1] |= (255 << (len(values) % 8)) & 255  # Nonzero unused padding.
    return bytes(out)


def hybrid(values):
    out = bytearray()
    for value, run in itertools.groupby(values):
        length = sum(map(lambda _: 1, run))
        out.extend(varint(length << 1))
        out.append(int(value))
    return bytes(out)


def length_prefix(data):
    return len(data).to_bytes(4, 'little') + data


def page(version, encoding, nullable, values):
    present = list(filter(lambda x: x is not None, values))
    definitions = list(map(lambda x: x is not None, values))
    levels = hybrid(definitions) if nullable else b''
    if encoding == 0:
        payload = bits(present)
    elif encoding == 3:
        payload = length_prefix(hybrid(present))
    else:
        payload = b'\x01' + hybrid(present)  # Entry 0=False, entry 1=True.
    if version == 1:
        body = (length_prefix(levels) if nullable else b'') + payload
        header = {1: (TType.I32, 0), 5: (TType.STRUCT, {
            1: (TType.I32, len(values)), 2: (TType.I32, encoding),
            3: (TType.I32, 3), 4: (TType.I32, 3)})}
    else:
        body = levels + payload
        header = {1: (TType.I32, 3), 8: (TType.STRUCT, {
            1: (TType.I32, len(values)), 2: (TType.I32, len(values)-len(present)),
            3: (TType.I32, len(values)), 4: (TType.I32, encoding),
            5: (TType.I32, len(levels)), 6: (TType.I32, 0),
            7: (TType.BOOL, False)})}
    header[2] = header[3] = TType.I32, len(body)
    return encoded_struct(header) + body


def write_case(root, name, version, encoding, nullable, sizes):
    values = []
    pages = []
    for size in sizes:
        start = len(values)
        part = []
        for index in range(start, start + size):
            part.append(None if nullable and index % 5 == 0 else bool((index // 19) % 2))
        pages.append(page(version, encoding, nullable, part))
        values.extend(part)
    expected = pa.Table.from_arrays([pa.array(values, type=pa.bool_())], schema=pa.schema([pa.field('x', pa.bool_(), nullable=nullable)]))
    seed = root / 'seed.parquet'
    pq.write_table(expected, seed, compression='NONE', use_dictionary=False, write_statistics=False)
    _, metadata = footer(seed)
    dictionary = b''
    if encoding in (2, 8):
        dictionary = encoded_struct({1: (TType.I32, 2), 2: (TType.I32, 1), 3: (TType.I32, 1), 7: (TType.STRUCT, {1: (TType.I32, 2), 2: (TType.I32, 0)})}) + b'\x02'
    content = dictionary + b''.join(pages)
    column = column_metadata(metadata)
    encodings = {encoding}
    if dictionary:
        encodings.add(0)
    if nullable:
        encodings.add(3)
    column[2] = TType.LIST, (TType.I32, list(map(lambda value: (TType.I32, value), sorted(encodings))))
    column[6] = column[7] = TType.I64, len(content)
    column[9] = TType.I64, 4 + len(dictionary)
    column.pop(13, None)
    if dictionary:
        column[11] = TType.I64, 4
    else:
        column.pop(11, None)
    group = groups(metadata)[0][1]
    group[2] = group[6] = TType.I64, len(content)
    encoded = encoded_struct(metadata[1])
    path = root / (name + '.parquet')
    path.write_bytes(b'PAR1' + content + encoded + len(encoded).to_bytes(4, 'little') + b'PAR1')
    return path, expected


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--reader', type=Path, required=True)
    args = parser.parse_args()
    with tempfile.TemporaryDirectory(prefix='wireform-boolean-interop-') as directory:
        root = Path(directory)
        inputs = root / 'inputs'
        inputs.mkdir()
        cases = []
        for version, encoding, nullable, size in itertools.product((1, 2), (0, 3, 8, 2), (False, True), (1, 7, 8, 9, 65, 1025)):
            name = f'v{version}-enc{encoding}-null{int(nullable)}-n{size}'
            cases.append(write_case(inputs, name, version, encoding, nullable, (size, size+1, 2)))
        (inputs / 'seed.parquet').unlink()
        for version, nullable, size in itertools.product(('1.0', '2.0'), (False, True), (7, 129)):
            vals = []
            for index in range(size):
                vals.append(None if nullable and index % 5 == 0 else bool((index // 19) % 2))
            expected = pa.Table.from_arrays([pa.array(vals, type=pa.bool_())], schema=pa.schema([pa.field('x', pa.bool_(), nullable=nullable)]))
            path = inputs / f'pyarrow-rle-v{version}-null{int(nullable)}-n{size}.parquet'
            pq.write_table(expected, path, compression='NONE', use_dictionary=False, column_encoding={'x': 'RLE'}, data_page_version=version, max_rows_per_page=17, write_batch_size=17)
            cases.append((path, expected))
        con = duckdb.connect(config={'threads': 1})
        limitations = 0
        try:
            for path, expected in cases:
                actual = list(map(lambda row: row[0], con.execute('SELECT x FROM read_parquet(?)', [str(path)]).fetchall()))
                assert actual == expected['x'].to_pylist(), path.name
                try:
                    assert pq.read_table(path).equals(expected), path.name
                except (pa.ArrowNotImplementedError, OSError) as error:
                    assert '-enc2-' in path.name or '-enc8-' in path.name, str(error)
                    assert 'boolean' in str(error).lower() and 'not implemented' in str(error).lower(), str(error)
                    limitations += 1
        finally:
            con.close()
        output = root / 'ipc'
        subprocess.run([str(args.reader.resolve()), str(inputs), '--arrow-output', str(output)], check=True)
        for path, expected in cases:
            with ipc.open_stream(output / (path.name + '.arrows')) as reader:
                actual = reader.read_all()
            assert actual.equals(expected), path.name
        print(f'{len(cases)} BOOLEAN fixtures verified against DuckDB and wireform IPC; {len(cases)-limitations} also verified by PyArrow; {limitations} PyArrow Boolean-dictionary limitations')


if __name__ == '__main__':
    main()
