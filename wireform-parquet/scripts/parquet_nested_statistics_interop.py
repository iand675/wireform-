#!/usr/bin/env python3
"""Validate nested bounds, orderings and NaN counts with independent readers.

Literal metadata is decoded with Apache Thrift. Arrow C++ supplies physical
values and checks bloom membership; Python supplies independent scalar/order
models. LZO changes compression only through unmodified liblzo2.
"""
import argparse
import hashlib
import json
import math
from pathlib import Path
import struct
import subprocess
import tempfile

import pyarrow as pa
import pyarrow.parquet as pq
import parquet_lzo_interop as lzo
import parquet_nested_bloom_interop as bloom


def floating_width(kind):
    return {'LtFloat16': 2, 'LtFloat': 4, 'LtDouble': 8}.get(kind)


def expected_bounds(model):
    values = list(map(bytes.fromhex, model['values']))
    if not values:
        return {'min': None, 'max': None, 'nans': 0 if floating_width(model['type']) else None}
    kind = model['type']
    width = floating_width(kind)
    if width:
        fmt = {2: '<e', 4: '<f', 8: '<d'}[width]
        numeric = list(filter(lambda value: not math.isnan(struct.unpack(fmt, value)[0]), values))
        nans = len(values) - len(numeric)
        if numeric:
            def key(value):
                number = struct.unpack(fmt, value)[0]
                zero_sign = 0 if int.from_bytes(value, 'little') >> (width * 8 - 1) else 1
                return (number, zero_sign if number == 0 else 0)
            ordered = sorted(numeric, key=key)
        else:
            # IEEE totalOrder orders negative quiet NaNs before negative
            # signaling NaNs, with reversed payload order; positive NaNs use
            # the opposite ordering. Keep the original bytes throughout.
            def nan_key(value):
                bits = int.from_bytes(value, 'little')
                mantissa_bits = {2: 10, 4: 23, 8: 52}[width]
                negative = bool(bits >> (width * 8 - 1))
                quiet = (bits >> (mantissa_bits - 1)) & 1
                payload = bits & ((1 << (mantissa_bits - 1)) - 1)
                return (0, -quiet, -payload) if negative else (1, quiet, payload)
            ordered = sorted(values, key=nan_key)
        return {'min': ordered[0].hex(), 'max': ordered[-1].hex(), 'nans': nans}
    if model['order'] == 'Int96TimestampOrder':
        def key(value):
            nanos, day = struct.unpack('<qi', value)
            return (day, nanos)
    elif kind.startswith('LtTimestamp96'):
        key = lambda value: int.from_bytes(value, 'little', signed=True)
    elif kind.startswith('LtDecimal'):
        precision = int(kind.split()[1])
        key = lambda value: int.from_bytes(value, 'little' if precision <= 18 else 'big', signed=True)
    elif kind.startswith('LtInteger'):
        signed = kind.split()[-1] == 'True'
        key = lambda value: int.from_bytes(value, 'little', signed=signed)
    elif kind in ('LtInt32', 'LtInt64'):
        key = lambda value: int.from_bytes(value, 'little', signed=True)
    else:
        key = lambda value: value
    ordered = sorted(values, key=key)
    return {'min': ordered[0].hex(), 'max': ordered[-1].hex(), 'nans': None}


def check_metadata(path, model, indexed):
    record = bloom.metadata(path)
    footer = record['footer'].value
    orders = footer[7].value
    assert len(orders) == 1, path
    expected_order = {'TypeDefinedOrder': 1, 'IEEE754TotalOrder': 2, 'Int96TimestampOrder': 3}[model['order']]
    assert set(orders[0].value) == {expected_order}, ('column ordering', path)
    group = footer[4].value[0].value
    assert group[3].value == 3, path
    chunk = group[1].value[0].value
    column = chunk[3].value
    assert column[5].value == len(model['values']) + 3, path
    stats = column[12].value
    assert stats[3].value == 3, ('null count', path)
    assert 1 not in stats and 2 not in stats, ('deprecated bounds must stay absent', path)
    for field, name in [(6, 'min'), (5, 'max')]:
        if model[name] is None:
            assert field not in stats, (path, name)
        else:
            assert stats[field].value.hex() == model[name], (path, name, stats[field].value.hex(), model[name])
    for field in (7, 8):
        assert (stats[field].value if field in stats else None) == (True if model['min'] is not None else None), (path, 'exact bound marker')
    assert (stats[9].value if 9 in stats else None) == model['nans'], ('NaN count', path)
    assert (6 in chunk and 7 in chunk and 4 in chunk and 5 in chunk) == indexed, path
    if indexed:
        ci = lzo.decode(record['bytes'][chunk[6].value:chunk[6].value+chunk[7].value]).node.value
        assert list(map(lambda entry: entry.value, ci[1].value)) == [not model['values']], ('null page', path)
        assert list(map(lambda entry: entry.value, ci[2].value)) == [bytes.fromhex(model['min']) if model['min'] is not None else b''], ('page minimum', path)
        assert list(map(lambda entry: entry.value, ci[3].value)) == [bytes.fromhex(model['max']) if model['max'] is not None else b''], ('page maximum', path)
        assert list(map(lambda entry: entry.value, ci[5].value)) == [3], ('page null count', path)
        assert (list(map(lambda entry: entry.value, ci[8].value)) if 8 in ci else None) == ([model['nans']] if model['nans'] is not None else None), ('page NaN count', path)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--fixtures', type=Path, required=True)
    parser.add_argument('--reference', type=Path, required=True)
    parser.add_argument('--lzo-reference', type=Path, required=True)
    parser.add_argument('--half-output', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    models = json.loads((args.fixtures/'nested-stats-models.json').read_text())
    named = {}
    for model in models:
        actual = expected_bounds(model)
        assert actual == {name: model[name] for name in ('min', 'max', 'nans')}, ('independent model', model['name'], actual)
        named[model['name']] = model
    files = sorted(args.fixtures.glob('nested-stats-*.parquet'))
    assert len(files) == len(models)*7*2*2, ('full codec/page/index matrix', len(files))
    records = []
    probes = operations = 0
    with tempfile.TemporaryDirectory(prefix='nested-statistics-interop-') as temp:
        scratch = Path(temp)
        for path in files:
            name, codec, version, indexed = path.stem.removeprefix('nested-stats-').rsplit('-', 3)
            model = named[name]
            indexed = indexed == 'True'
            check_metadata(path, model, indexed)
            def stored_bloom(source):
                record = bloom.metadata(source)
                cm = record['footer'].value[4].value[0].value[1].value[0].value[3].value
                offset, size = cm[14].value, cm[15].value
                return record['bytes'][offset:offset+size]
            control = args.fixtures / f'nested-stats-{name}-Uncompressed-{version}-{indexed}.parquet'
            assert stored_bloom(path) == stored_bloom(control), ('codec-independent bloom bytes', path)
            readable = path
            if codec == 'LZO':
                stripped, readable = scratch/'stripped.parquet', scratch/'decoded.parquet'
                bloom.strip_auxiliary(path, stripped)
                operations += lzo.transcode(stripped, readable, 0, args.lzo_reference, scratch,
                                            require_source_codec=3, expected_version=1 if version == 'PageV1' else 2)
            proc = subprocess.run([str(args.reference),str(readable),'-','plaintext'],capture_output=True,text=True)
            assert proc.returncode == 0, (path, proc.stderr)
            header, column = map(json.loads,proc.stdout.splitlines())
            assert header['rows'] == 3 and header['columns'] == 1, path
            assert column['values'] == model['values'], ('physical values',path,column['values'],model['values'])
            if codec != 'LZO':
                assert column['bloomValuesChecked'] == len(model['values']), path
                assert column['columnIndex'] == indexed, path
                if indexed:
                    assert column['pageMaxima'] == [model['max'] or ''], path
                probes += len(model['values'])
            records.append({'file':path.name,'literalMetadataAndOrderMatch':True,'independentScalarModelMatches':True,'cppValuesMatch':True,'lzoCompressionOnly':codec=='LZO'})
        half_files = sorted(args.half_output.glob('*/*.parquet'))
        assert len(half_files) == 14, len(half_files)
        for path in half_files:
            readable = path
            if path.parent.name.startswith('LZO-'):
                stripped, readable = scratch/'half-stripped.parquet',scratch/'half-decoded.parquet'
                bloom.strip_auxiliary(path,stripped)
                operations += lzo.transcode(stripped,readable,0,args.lzo_reference,scratch,require_source_codec=3,expected_version=1 if 'PageV1' in path.parent.name else 2)
            table = pq.read_table(readable)
            assert table.schema.field(0).type == pa.float16(), (path,table.schema)
            assert table.column(0).to_pylist() == [1.0,None,-0.0], path
            raw = table.column(0).combine_chunks().buffers()[1].to_pybytes()
            assert raw[:2] == b'\x00\x3c' and raw[4:6] == b'\x00\x80', ('half bits',path,raw.hex())
    proof={'files':records,'fileCount':len(records),'models':len(models),'cppBloomProbes':probes,'liblzo2PageOperations':operations,'halfArrowFiles':14,'referenceSHA256':hashlib.sha256(args.reference.read_bytes()).hexdigest(),'allPassed':True}
    args.output.write_text(json.dumps(proof,indent=2)+'\n')
    print(f'PASS: {len(records)} nested statistics files, {len(models)} independent scalar models, {probes} C++ probes,14 Arrow binary16 round-trips')


if __name__=='__main__':main()
