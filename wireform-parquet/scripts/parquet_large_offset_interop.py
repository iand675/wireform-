#!/usr/bin/env python3
"""Public LargeUtf8/LargeBinary projections against unmodified Arrow C++."""
import argparse
import json
from pathlib import Path
import subprocess
import tempfile
import pyarrow as pa
import pyarrow.ipc as ipc
import pyarrow.parquet as pq


def widen(field):
    typ = field.type
    if pa.types.is_string(typ):
        typ = pa.large_string()
    elif pa.types.is_binary(typ):
        typ = pa.large_binary()
    elif pa.types.is_struct(typ):
        typ = pa.struct(list(map(widen, typ)))
    elif pa.types.is_list(typ):
        typ = pa.large_list(widen(typ.value_field))
    elif pa.types.is_map(typ):
        typ = pa.map_(widen(typ.key_field), widen(typ.item_field), keys_sorted=typ.keys_sorted)
    return pa.field(field.name, typ, nullable=field.nullable, metadata=field.metadata)


def project(reader, source, schema, output):
    request = output.with_suffix('.schema.arrows')
    with ipc.new_stream(request, schema):
        pass
    return subprocess.run(list(map(str, [reader, '--project', source, request, output])), capture_output=True, text=True, timeout=60)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--reader', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--codecs', nargs='+', default=['NONE', 'gzip', 'snappy', 'zstd', 'lz4', 'brotli'])
    args = parser.parse_args()
    cases = 0
    rejected = 0
    with tempfile.TemporaryDirectory(prefix='large-offset-interop-') as directory:
        root = Path(directory)
        required_schema = pa.schema([pa.field('text', pa.string(), nullable=False), pa.field('bytes', pa.binary(), nullable=False)])
        tables = {
            'required': pa.Table.from_arrays([pa.array(['alpha', '', 'κόσμε'] * 9), pa.array([b'\x00\xff', b'', b'bytes'] * 9)], schema=required_schema),
            'nullable': pa.table({'text': pa.array(['alpha', None, '', 'κόσμε'] * 9), 'bytes': pa.array([b'\x00\xff', None, b'', b'bytes'] * 9)}),
            'nested': pa.table({'values': pa.array([None, [], [None, 'alpha', 'κόσμε'], ['']] * 9, pa.list_(pa.string())), 'record': pa.array([None, {'x': b'\xff', 'y': 'κόσμε'}, {'x': None, 'y': None}] * 12, pa.struct([('x', pa.binary()), ('y', pa.string())]))}),
            'map': pa.table({'mapping': pa.array([None, [], [('alpha', [b'\x00\xff', None])], [('κόσμε', []), ('', None)]] * 9, pa.map_(pa.string(), pa.list_(pa.binary())))}),
            'empty': pa.table({'text': pa.array([], pa.string()), 'mapping': pa.array([], pa.map_(pa.binary(), pa.string()))}),
        }
        for codec in args.codecs:
            for version in ['1.0', '2.0']:
                for dictionary in [False, True]:
                    for name, table in tables.items():
                        stem = f'{codec}-{version}-{dictionary}-{name}'
                        source = root / (stem + '.parquet')
                        output = root / (stem + '.arrows')
                        pq.write_table(table, source, compression=codec, data_page_version=version, use_dictionary=dictionary, row_group_size=11, write_batch_size=3, max_rows_per_page=3, write_page_index=True, write_page_checksum=True)
                        target = pa.schema(list(map(widen, table.schema)))
                        result = project(args.reader, source, target, output)
                        assert result.returncode == 0, (stem, result.stdout, result.stderr)
                        with ipc.open_stream(output) as reader:
                            actual = reader.read_all()
                        actual.validate(full=True)
                        expected = table.cast(target)
                        assert actual.equals(expected, check_metadata=False), (stem, actual, expected)
                        cases += 1
        # Offset widening never changes UTF-8 into opaque bytes, or vice versa.
        for source_type, target_type, values in [(pa.string(), pa.large_binary(), ['alpha']), (pa.binary(), pa.large_string(), [b'alpha'])]:
            source = root / f'invalid-{rejected}.parquet'
            pq.write_table(pa.table({'v': pa.array(values, source_type)}), source, compression='NONE')
            result = project(args.reader, source, pa.schema([('v', target_type)]), root / f'invalid-{rejected}.arrows')
            assert result.returncode > 0 and 'IncompatibleType' in result.stdout, result
            rejected += 1
        report = {'pyarrow': pa.__version__, 'cases': cases, 'incompatibleRejected': rejected, 'failures': 0, 'skips': 0}
        args.output.write_text(json.dumps(report, indent=2) + '\n')
        print(json.dumps(report, indent=2))


if __name__ == '__main__':
    main()
