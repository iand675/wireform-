#!/usr/bin/env python3
"""Compare indexed reads with independently decoded PyArrow page ranges.

Apache Thrift reads the offset/column indexes and page encoding tags. Expected
values come from the original Arrow arrays, not from Wireform's full reader.
"""

import argparse
from dataclasses import dataclass
from decimal import Decimal, localcontext
from pathlib import Path
import struct
import subprocess
import tempfile

import pyarrow as pa
import pyarrow.ipc as ipc
import pyarrow.parquet as pq
from parquet_aggregate_interop import footer, groups, read_node, TCompactProtocol, TMemoryBuffer, TType

ROWS = 4096


@dataclass
class Dataset:
    name: str
    typ: object
    values: list
    literal: str
    pivot: object
    decode: object


def datasets():
    signed = lambda raw: int.from_bytes(raw, 'little', signed=True)
    unsigned = lambda raw: int.from_bytes(raw, 'little')
    decimal = lambda raw: int.from_bytes(raw, 'big', signed=True)
    with localcontext() as context:
        context.prec = 90
        return [
            Dataset('int32', pa.int32(), list(range(-2048, 2048)), '0', 0, signed),
            Dataset('int64', pa.int64(), [2**40+i for i in range(ROWS)], str(2**40+2048), 2**40+2048, signed),
            Dataset('uint32', pa.uint32(), [2**31+i for i in range(ROWS)], str(2**31+2048), 2**31+2048, unsigned),
            Dataset('uint64', pa.uint64(), [2**63+i for i in range(ROWS)], str(2**63+2048), 2**63+2048, unsigned),
            Dataset('int8', pa.int8(), [i//32-64 for i in range(ROWS)], '0', 0, signed),
            Dataset('float32', pa.float32(), [(i-2048)*0.5 for i in range(ROWS)], '0', 0, lambda raw: struct.unpack('<f', raw)[0]),
            Dataset('float64', pa.float64(), [(i-2048)*0.5 for i in range(ROWS)], '0', 0, lambda raw: struct.unpack('<d', raw)[0]),
            Dataset('float16', pa.float16(), list(range(-2048, 2048)), '0', 0, lambda raw: struct.unpack('<e', raw)[0]),
            Dataset('bool', pa.bool_(), [i >= 2048 for i in range(ROWS)], 'true', True, lambda raw: bool(raw[0])),
            Dataset('utf8', pa.string(), [f'item-{i//32:03d}-é' for i in range(ROWS)], 'item-064-é', 'item-064-é', lambda raw: raw.decode()),
            Dataset('binary', pa.binary(), [(i//32).to_bytes(2, 'big')+b'\xff\x00' for i in range(ROWS)], '0040ff00', b'\x00\x40\xff\x00', lambda raw: raw),
            Dataset('fixed', pa.binary(4), [i.to_bytes(4, 'big') for i in range(ROWS)], '00000800', b'\x00\x00\x08\x00', lambda raw: raw),
            Dataset('decimal9', pa.decimal128(9, 2), [Decimal(i-2048).scaleb(-2) for i in range(ROWS)], '0', 0, decimal),
            Dataset('decimal30', pa.decimal128(30, 7), [Decimal(10**25+i).scaleb(-7) for i in range(ROWS)], str(Decimal(10**25+2048).scaleb(-7)), 10**25+2048, decimal),
            Dataset('decimal50', pa.decimal256(50, 20), [Decimal(10**45+i).scaleb(-20) for i in range(ROWS)], str(Decimal(10**45+2048).scaleb(-20)), 10**45+2048, decimal),
            Dataset('date32', pa.date32(), list(range(-2048, 2048)), '0', 0, signed),
            Dataset('time64', pa.time64('us'), list(range(ROWS)), '2048', 2048, signed),
            Dataset('timestamp', pa.timestamp('ns'), [1_700_000_000_000_000_000+i for i in range(ROWS)], '1700000000000002048', 1_700_000_000_000_002_048, signed),
        ]


def struct_at(raw, offset):
    return read_node(TCompactProtocol(TMemoryBuffer(raw[offset:])), TType.STRUCT)[1]


def index_info(path, dataset):
    raw = path.read_bytes()
    metadata = footer(path)[1]
    chunk = groups(metadata)[0][1][1][1][1][0][1]
    column = chunk[3][1]
    offsets = struct_at(raw, chunk[4][1])[1][1][1]
    bounds = struct_at(raw, chunk[6][1])
    nulls = [entry[1] for entry in bounds[1][1][1]]
    maxima = [entry[1] for entry in bounds[3][1][1]]
    locations = [(entry[1][1][1], entry[1][2][1], entry[1][3][1]) for entry in offsets]
    keep = [not null and dataset.decode(high) >= dataset.pivot for null, high in zip(nulls, maxima)]
    assert len(locations) == len(keep) and len(locations) > 1, path.name
    encodings = []
    for retained, (offset, _, _) in zip(keep, locations):
        if retained:
            header = struct_at(raw, offset)
            encodings.append(header[5][1][2][1] if header[1][1] == 0 else header[8][1][4][1])
    return raw, column, locations, keep, encodings


def check(reader, path, dataset, array, output):
    raw, column, locations, keep, encodings = index_info(path, dataset)
    parts = []
    for index, (retained, (_, _, start)) in enumerate(zip(keep, locations)):
        if retained:
            end = locations[index+1][2] if index+1 < len(locations) else len(array)
            parts.append(array.slice(start, end-start))
    expected_array = pa.concat_arrays(parts) if parts else array.slice(0, 0)
    expected = pa.Table.from_arrays([expected_array], schema=pa.schema([pa.field('x', array.type, nullable=True)]))
    result = subprocess.run([str(reader), '--prune', str(path), dataset.literal, str(output)],
                            capture_output=True, text=True)
    assert result.returncode == 0, (path.name, result.stdout, result.stderr)
    assert result.stdout.strip() == f'pages=Just ({sum(keep)},{len(keep)})', (path.name, result.stdout)
    with ipc.open_stream(output) as stream:
        actual = stream.read_all()
    assert actual.schema.field(0).type == expected.schema.field(0).type, (path.name, actual.schema, expected.schema)
    assert actual.equals(expected), (path.name, actual.to_pydict(), expected.to_pydict())
    return raw, column, locations, keep, encodings


def check_sparse(reader, root):
    """Null rows must not duplicate the first long string or binary value.

    The source holds only a few MiB. Gathering value0 for every null would
    manufacture GiB of output and incorrectly reject a valid input file.
    """
    nested = root / 'sparse-nested'
    nested.mkdir()
    expected_nested = {}
    for name, typ, large, low, high, literal, decode in [
        ('utf8', pa.string(), 'm' * (1024 * 1024), 'a', 'z', 'a', lambda raw: raw.decode()),
        ('binary', pa.binary(), b'm' * (1024 * 1024), b'a', b'z', '61', lambda raw: raw),
    ]:
        block = [large] + [None] * 4096 + [low, high]
        array = pa.array(block + block, typ)
        table = pa.table({'x': array})
        path = root / f'sparse-{name}.parquet'
        pq.write_table(table, path, use_dictionary=False, compression='NONE', data_page_version='2.0',
                       write_page_index=True, data_page_size=4096, write_batch_size=len(block),
                       row_group_size=len(array))
        dataset = Dataset(name, typ, [], literal, low, decode)
        check(reader, path, dataset, array, root / 'sparse.arrows')
        # Repeated nullable elements exercise the generic nested assembler,
        # whose output rows are selected from definition/repetition events.
        list_type = pa.list_(pa.field('element', typ, nullable=True))
        list_table = pa.table({'x': pa.array([block], list_type)})
        nested_path = nested / f'{name}.parquet'
        pq.write_table(list_table, nested_path, use_dictionary=False, compression='NONE',
                       data_page_version='2.0', data_page_size=2 * 1024 * 1024,
                       write_batch_size=len(block), row_group_size=1)
        expected_nested[nested_path.name] = list_table
        print('PASS sparse nullable', name, 'indexed selection', flush=True)
    output = root / 'sparse-nested-output'
    result = subprocess.run([str(reader), str(nested), '--arrow-output', str(output)],
                            capture_output=True, text=True)
    assert result.returncode == 0, result.stdout + result.stderr
    for name, expected in expected_nested.items():
        with ipc.open_stream(output / (name + '.arrows')) as stream:
            actual = stream.read_all()
        assert actual.equals(expected), (name, actual.schema, expected.schema)
        print('PASS sparse nullable nested', name, flush=True)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--reader', type=Path, required=True)
    args = parser.parse_args()
    reader = args.reader.resolve()
    checked = dictionary_selections = 0
    poison_sources = {}
    with tempfile.TemporaryDirectory(prefix='wireform-parquet-selection-') as directory:
        root = Path(directory)
        output = root / 'selected.arrows'
        for dataset in datasets():
            for nullable in (False, True):
                values = [None if nullable and (i < 64 or i % 17 == 0) else value for i, value in enumerate(dataset.values)]
                array = pa.array(values, dataset.typ)
                table = pa.Table.from_arrays([array], schema=pa.schema([pa.field('x', dataset.typ, nullable=nullable)]))
                for version in ('1.0', '2.0'):
                    for codec in ('NONE', 'SNAPPY', 'ZSTD'):
                        for mode in ('plain', 'dictionary', 'fallback'):
                            name = f'{dataset.name}-{nullable}-{version}-{codec}-{mode}'
                            path = root / f'{name}.parquet'
                            pq.write_table(table, path, data_page_version=version, compression=codec,
                                           use_dictionary=mode != 'plain', dictionary_pagesize_limit=512 if mode == 'fallback' else 2**20,
                                           data_page_size=128, write_batch_size=64, row_group_size=ROWS,
                                           write_page_index=True, write_page_checksum=True)
                            info = check(reader, path, dataset, array, output)
                            checked += 1
                            dictionary_selections += any(encoding in (2, 8) for encoding in info[-1])
                            if dataset.name == 'int32' and not nullable and version == '2.0' and codec == 'NONE':
                                poison_sources[mode] = path, dataset, array, info
                            if checked % 36 == 0:
                                print('PASS', dataset.name, 'nullable='+str(nullable), flush=True)
        assert dictionary_selections > 0
        # Poisoning a skipped page proves the public path really skips it.
        source, dataset, array, info = poison_sources['dictionary']
        raw, column, locations, keep, _ = info
        skipped = next(location for location, retained in zip(locations, keep) if not retained)
        poisoned = bytearray(raw)
        poisoned[skipped[0]:skipped[0]+skipped[1]] = b'\xff' * skipped[1]
        target = root / 'poisoned-skipped-page.parquet'
        target.write_bytes(poisoned)
        # Use the unchanged indexes; parsing the poisoned header is intentionally forbidden.
        result = subprocess.run([str(reader), '--prune', str(target), dataset.literal, str(output)], capture_output=True, text=True)
        assert result.returncode == 0, result.stdout + result.stderr
        with ipc.open_stream(output) as stream:
            poisoned_result = stream.read_all()
        check(reader, source, dataset, array, output)
        with ipc.open_stream(output) as stream:
            assert poisoned_result.equals(stream.read_all())
        # Dictionary fallback pages can retain only PLAIN pages. Their unused
        # dictionary must not be decoded or checksummed.
        source, dataset, array, info = poison_sources['fallback']
        raw, column, _, keep, encodings = info
        assert any(keep) and all(encoding not in (2, 8) for encoding in encodings)
        dictionary_start, data_start = column[11][1], column[9][1]
        poisoned = bytearray(raw)
        poisoned[dictionary_start:data_start] = b'\xff' * (data_start-dictionary_start)
        target = root / 'poisoned-unused-dictionary.parquet'
        target.write_bytes(poisoned)
        result = subprocess.run([str(reader), '--prune', str(target), dataset.literal, str(output)], capture_output=True, text=True)
        assert result.returncode == 0, result.stdout + result.stderr
        with ipc.open_stream(output) as stream:
            poisoned_result = stream.read_all()
        check(reader, source, dataset, array, output)
        with ipc.open_stream(output) as stream:
            assert poisoned_result.equals(stream.read_all())
        check_sparse(reader, root)
    print(f'{checked} indexed files matched independently selected Arrow rows; {dictionary_selections} selections required dictionaries; skipped-page and unused-dictionary poison checks and four sparse byte/text regressions passed')


if __name__ == '__main__':
    main()
