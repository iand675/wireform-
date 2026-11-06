#!/usr/bin/env python3
"""Exercise exact aggregates using PyArrow files and Apache Thrift mutations.

The typed footer tree retains the original Thrift wire kinds, so rewriting a
flag cannot silently change another field's type. Page bytes remain untouched.
"""

import argparse
from pathlib import Path
import struct
import subprocess
import tempfile

import duckdb
import polars as pl
import pyarrow as pa
import pyarrow.parquet as pq
from thrift.Thrift import TType
from thrift.protocol.TCompactProtocol import TCompactProtocol
from thrift.transport.TTransport import TMemoryBuffer


def read_node(protocol, kind):
    if kind == TType.STRUCT:
        protocol.readStructBegin()
        fields = {}
        while True:
            _, field_kind, field_id = protocol.readFieldBegin()
            if field_kind == TType.STOP:
                break
            fields[field_id] = read_node(protocol, field_kind)
            protocol.readFieldEnd()
        protocol.readStructEnd()
        return kind, fields
    if kind == TType.LIST:
        item_kind, count = protocol.readListBegin()
        items = [read_node(protocol, item_kind) for _ in range(count)]
        protocol.readListEnd()
        return kind, (item_kind, items)
    scalar = {
        TType.BOOL: protocol.readBool, TType.BYTE: protocol.readByte,
        TType.I16: protocol.readI16, TType.I32: protocol.readI32,
        TType.I64: protocol.readI64, TType.DOUBLE: protocol.readDouble,
        TType.STRING: protocol.readBinary,
    }
    return kind, scalar[kind]()


def write_node(protocol, node):
    kind, value = node
    if kind == TType.STRUCT:
        protocol.writeStructBegin('')
        entries = value.items() if isinstance(value, dict) else value
        for field_id, child in sorted(entries):
            protocol.writeFieldBegin('', child[0], field_id)
            write_node(protocol, child)
            protocol.writeFieldEnd()
        protocol.writeFieldStop()
        protocol.writeStructEnd()
    elif kind == TType.LIST:
        item_kind, items = value
        protocol.writeListBegin(item_kind, len(items))
        for child in items:
            write_node(protocol, child)
        protocol.writeListEnd()
    else:
        scalar = {
            TType.BOOL: protocol.writeBool, TType.BYTE: protocol.writeByte,
            TType.I16: protocol.writeI16, TType.I32: protocol.writeI32,
            TType.I64: protocol.writeI64, TType.DOUBLE: protocol.writeDouble,
            TType.STRING: protocol.writeBinary,
        }
        scalar[kind](value)


def footer(path):
    raw = path.read_bytes()
    size = int.from_bytes(raw[-8:-4], 'little')
    start = len(raw) - 8 - size
    node = read_node(TCompactProtocol(TMemoryBuffer(raw[start:-8])), TType.STRUCT)
    return raw[:start], node


def groups(node):
    return node[1][4][1][1]


def column_metadata(node, index=0):
    return groups(node)[index][1][1][1][1][0][1][3][1]


def stats(node, index=0):
    # FileMetaData.row_groups[index].columns[0].meta_data.statistics
    return column_metadata(node, index)[12][1]


def duplicate_exactness(node):
    fields = stats(node)
    fields.pop(8, None)
    entries = [*fields.items(), (8, (TType.BOOL, False)), (8, (TType.BOOL, True))]
    column_metadata(node)[12] = TType.STRUCT, entries


def rewrite(source, destination, edit):
    prefix, node = footer(source)
    edit(node)
    transport = TMemoryBuffer()
    write_node(TCompactProtocol(transport), node)
    encoded = transport.getvalue()
    destination.write_bytes(prefix + encoded + len(encoded).to_bytes(4, 'little') + b'PAR1')


def check(probe, path, **expected):
    result = subprocess.run([str(probe), '--aggregate', str(path)],
                            check=True, capture_output=True, text=True)
    actual = dict(line.split('=', 1) for line in result.stdout.splitlines())
    for key, value in expected.items():
        assert actual[key] == value, (path.name, key, actual, expected)
    print('PASS', path.name)


def bounds(node, lower, upper, min_exact, max_exact):
    fields = stats(node)
    for tag in (1, 2, 7, 8):
        fields.pop(tag, None)
    fields[6], fields[5] = (TType.STRING, lower), (TType.STRING, upper)
    if max_exact is not None:
        fields[7] = TType.BOOL, max_exact
    if min_exact is not None:
        fields[8] = TType.BOOL, min_exact


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--reader', type=Path, required=True)
    parser.add_argument('--writer', type=Path, required=True)
    args = parser.parse_args()
    reader, writer = args.reader.resolve(), args.writer.resolve()
    with tempfile.TemporaryDirectory(prefix='wireform-parquet-aggregate-') as directory:
        root = Path(directory)
        original = root / 'original.parquet'
        table = pa.table({'x': pa.array(['Baker', 'Barn'], pa.string())})
        pq.write_table(table, original)
        for name, low, high, min_exact, max_exact, minimum, maximum in [
            ('compact', b'B', b'C', False, False, 'Nothing', 'Nothing'),
            ('unknown', b'B', b'C', None, None, 'Nothing', 'Nothing'),
            ('exact', b'Baker', b'Barn', True, True,
             'Just (PVText "Baker")', 'Just (PVText "Barn")'),
            ('minimum-only', b'Baker', b'C', True, False, 'Just (PVText "Baker")', 'Nothing'),
            ('maximum-only', b'B', b'Barn', False, True, 'Nothing', 'Just (PVText "Barn")'),
        ]:
            target = root / f'{name}.parquet'
            rewrite(original, target, lambda node: bounds(node, low, high, min_exact, max_exact))
            assert pq.read_table(target).equals(table), name
            check(reader, target, rows='Just 2', non_null='Just 2', minimum=minimum, maximum=maximum)

        integers = root / 'integers.parquet'
        pq.write_table(pa.table({'x': pa.array([7, 9], pa.int64())}), integers)
        legacy = root / 'legacy-exact.parquet'

        def legacy_bounds(node):
            bounds(node, struct.pack('<q', 0), struct.pack('<q', 10), False, False)
            stats(node)[2] = TType.STRING, struct.pack('<q', 7)
            stats(node)[1] = TType.STRING, struct.pack('<q', 9)

        rewrite(integers, legacy, legacy_bounds)
        check(reader, legacy, minimum='Just (PVInt64 7)', maximum='Just (PVInt64 9)')

        all_null = root / 'null-group.parquet'
        pq.write_table(pa.table({'x': pa.array([None, None, 'Baker', 'Barn'], pa.string())}),
                       all_null, row_group_size=2)
        target = root / 'null-group-exact.parquet'

        def exact_second_group(node):
            stats(node, 1)[7] = TType.BOOL, True
            stats(node, 1)[8] = TType.BOOL, True

        rewrite(all_null, target, exact_second_group)
        check(reader, target, rows='Just 4', non_null='Just 2',
              minimum='Just (PVText "Baker")', maximum='Just (PVText "Barn")')

        nested = root / 'nullable-ancestor.parquet'
        child = pa.field('x', pa.int32(), nullable=False)
        array = pa.array([None, {'x': 7}, {'x': 9}], pa.struct([child]))
        pq.write_table(pa.table({'record': array}), nested)
        check(reader, nested, rows='Just 3', non_null='Just 2')

        repeated = root / 'repeated.parquet'
        pq.write_table(pa.table({'x': pa.array([None, [], [7, 9], [None]], pa.list_(pa.int32()))}), repeated)
        check(reader, repeated, rows='Just 4', non_null='Nothing')

        empty = root / 'empty.parquet'
        pq.write_table(pa.table({'x': pa.array([], pa.string())}), empty)
        check(reader, empty, rows='Just 0', non_null='Just 0', minimum='Nothing', maximum='Nothing')

        for name, edit, message in [
            ('negative-count', lambda node: node[1].__setitem__(3, (TType.I64, -1)), 'row count'),
            ('wrong-flag-type', lambda node: stats(node).__setitem__(7, (TType.I32, 1)), 'exactness flag'),
            ('duplicate-flag', duplicate_exactness, 'exactness flag'),
        ]:
            target = root / f'{name}.parquet'
            rewrite(original, target, edit)
            result = subprocess.run([str(reader), '--aggregate', str(target)], capture_output=True, text=True)
            assert result.returncode != 0 and message in result.stdout, (name, result)
            print('PASS', name, 'rejected')

        stale = root / 'stale-total.parquet'
        rewrite(original, stale, lambda node: node[1].__setitem__(3, (TType.I64, 0)))
        assert pq.read_table(stale).equals(table)
        check(reader, stale, rows='Nothing', non_null='Nothing', minimum='Nothing', maximum='Nothing')

        generated = root / 'generated'
        generated.mkdir()
        subprocess.run([str(writer), '--statistics', str(generated)], check=True)
        for name, values, low, high in [
            ('binary', [b'\x80', b'\x7f', b'', b'\xff'], b'', b'\xff'),
            ('utf8', ['é', 'z', '', '🙂'], b'', '🙂'.encode()),
        ]:
            path = generated / f'{name}-boundaries.parquet'
            fields = stats(footer(path)[1])
            assert 1 not in fields and 2 not in fields, (name, 'legacy byte bounds')
            assert fields[6][1] == low and fields[5][1] == high, (name, 'modern byte bounds')
            assert fields[7] == (TType.BOOL, True) and fields[8] == (TType.BOOL, True)
            assert pq.read_table(path)['x'].to_pylist() == values, name
            assert pl.read_parquet(path)['x'].to_list() == values, name
            connection = duckdb.connect()
            try:
                actual = connection.execute('SELECT x FROM read_parquet(?)', [str(path)]).fetchall()
                assert actual == [(value,) for value in values], name
            finally:
                connection.close()
            print('PASS', name, 'writer bounds and values in three engines')
    print('Independent exact/compact bounds, null groups, nested counts, invalid metadata and binary boundaries passed')


if __name__ == '__main__':
    main()
