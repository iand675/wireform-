#!/usr/bin/env python3
"""Check generated nested blooms with unmodified Arrow C++ and liblzo2."""
import argparse
import hashlib
import json
from pathlib import Path
import struct
import subprocess
import tempfile

import parquet_lzo_interop as lzo


def values():
    return [
        list(map(lambda x: struct.pack('<i', x).hex(), [-7, 0, 42])),
        list(map(lambda x: struct.pack('<q', x).hex(), [-1099511627776, 1099511627776])),
        list(map(lambda x: struct.pack('<I', x).hex(), [0x7fc00001, 0x80000000, 0, 0x7f800000])),
        list(map(lambda x: struct.pack('<d', x).hex(), [-0.0, 0.0, float('inf'), -3.5])),
        list(map(lambda x: bytes([x % 2]).hex(), range(17))),
        list(map(lambda x: x.encode().hex(), ['', 'alpha', 'λ\0β'])),
        ['', b'abc'.hex(), bytes([0, 255, 4, 0, 0, 0]).hex()],
        [(b'\0' * 9).hex(), (b'\xff' * 9).hex()],
        [bytes(range(16)).hex()],
        [bytes(range(12)).hex()],
        [(b'\xff' * 12).hex()],
        [(b'\xff' * 16).hex(), (b'\0' * 16).hex()],
        [(b'\xff' * 32).hex()],
        [], ['01'], ['00'] * 8,
    ]


def metadata(path):
    data = path.read_bytes()
    length = int.from_bytes(data[-8:-4], 'little')
    start = len(data) - 8 - length
    footer = lzo.decode(data[start:-8]).node
    return {'bytes': data, 'footer': footer, 'footerStart': start}


def bloom_bytes(path):
    record = metadata(path)
    group = record['footer'].value[4].value[0]
    columns = group.value[1].value
    assert len(columns) == 2, path
    cm = columns[0].value[3].value
    offset, size = cm[14].value, cm[15].value
    assert offset >= 4 and size > 0 and offset + size <= record['footerStart'], path
    assert 14 not in columns[1].value[3].value, ('unrequested bloom', path)
    return record['bytes'][offset:offset + size]


def strip_auxiliary(source, target):
    record = metadata(source)
    for group in record['footer'].value[4].value:
        for chunk in group.value[1].value:
            for field in (4, 5, 6, 7):
                chunk.value.pop(field, None)
            column = chunk.value[3].value
            column.pop(14, None)
            column.pop(15, None)
    footer = lzo.encode(record['footer'])
    target.write_bytes(record['bytes'][:record['footerStart']] + footer + struct.pack('<I', len(footer)) + b'PAR1')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--fixtures', type=Path, required=True)
    parser.add_argument('--reference', type=Path, required=True)
    parser.add_argument('--lzo-reference', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    files = sorted(args.fixtures.glob('nested-generated-bloom-*.parquet'))
    assert len(files) == 224, ('full seven-codec/two-page/16-model coverage', len(files))
    expected = values()
    records = []
    cpp_queries = 0
    operations = 0
    with tempfile.TemporaryDirectory(prefix='nested-bloom-interop-') as temp:
        scratch = Path(temp)
        for path in files:
            index, codec, version = path.stem.removeprefix('nested-generated-bloom-').split('-')
            case = int(index)
            canonical = args.fixtures / f'nested-generated-bloom-{case}-Uncompressed-{version}.parquet'
            assert bloom_bytes(path) == bloom_bytes(canonical), ('codec-independent bloom bytes', path)
            readable = path
            if codec == 'LZO':
                stripped, readable = scratch / 'stripped.parquet', scratch / 'decoded.parquet'
                strip_auxiliary(path, stripped)
                operations += lzo.transcode(stripped, readable, 0, args.lzo_reference, scratch,
                                            require_source_codec=3, expected_version=1 if version == 'PageV1' else 2)
            proc = subprocess.run([str(args.reference), str(readable), '-', 'plaintext'], capture_output=True, text=True)
            assert proc.returncode == 0, (path, proc.stderr)
            header, *columns = map(json.loads, proc.stdout.splitlines())
            assert header['rowGroups'] == 1 and len(columns) == 2, path
            assert columns[0]['pathParts'] == ['values.with.dot', 'list', 'element'], path
            assert columns[0]['values'] == expected[case], ('physical values', path, columns[0]['values'])
            assert columns[1]['values'] == list(map(lambda x: struct.pack('<i', x).hex(), [1, 2, 3])), path
            assert columns[1]['bloomBytes'] == 0, path
            if codec != 'LZO':
                assert columns[0]['bloomBytes'] > 0, path
                assert columns[0]['bloomValuesChecked'] == len(expected[case]), path
                cpp_queries += len(expected[case])
            else:
                # Unmodified C++ lacks LZO. liblzo2 decodes only compression;
                # C++ then proves exact physical values. Original bloom bytes
                # equal the matching uncompressed fixture checked above.
                assert columns[0]['bloomBytes'] == 0, path
            records.append({'file': path.name, 'physicalValuesMatchIndependentModel': True,
                            'bloomMatchesUncompressedControl': True,
                            'cppDirectBloomProbes': len(expected[case]) if codec != 'LZO' else 0,
                            'lzoValuesViaIndependentLiblzo2': codec == 'LZO'})
    proof = {'files': records, 'fileCount': len(records), 'cppBloomValueQueries': cpp_queries,
             'independentLzoPageOperations': operations,
             'referenceSHA256': hashlib.sha256(args.reference.read_bytes()).hexdigest(),
             'allPassed': True}
    args.output.write_text(json.dumps(proof, indent=2) + '\n')
    print(f'PASS: {len(records)} files, {cpp_queries} C++ membership probes, {operations} liblzo2 page operations')


if __name__ == '__main__':
    main()
