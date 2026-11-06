#!/usr/bin/env python3
"""Check decimal values, signed bounds, indexes and filtering in three engines."""
import argparse
from decimal import Decimal
from pathlib import Path
import subprocess
import tempfile

import duckdb
import polars as pl
import pyarrow.parquet as pq
from parquet_statistics_interop import read_value, TCompactProtocol, TMemoryBuffer, TType

GROUPS = [[1,127,128,255,256,1000],[-1000,-256,-129,-128,-1],[-999999,0,999999]]


def read_struct(raw, offset):
    return read_value(TCompactProtocol(TMemoryBuffer(raw[offset:])), TType.STRUCT)


def physical(integer, storage):
    if storage in ('i32', 'i64'):
        return integer.to_bytes(4 if storage == 'i32' else 8, 'little', signed=True)
    # The fixture intentionally sign-extends -1 to test equivalent encodings.
    if integer == -1:
        return b'\xff\xff'
    width = max(1, (integer.bit_length() + 8) // 8)
    while True:
        try:
            encoded = integer.to_bytes(width, 'big', signed=True)
        except OverflowError:
            width += 1
            continue
        if width > 1:
            try:
                integer.to_bytes(width - 1, 'big', signed=True)
            except OverflowError:
                return encoded
            width -= 1
        else:
            return encoded


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--probe', required=True, type=Path)
    args = parser.parse_args()
    with tempfile.TemporaryDirectory(prefix='wireform-parquet-decimal-') as directory:
        root = Path(directory)
        subprocess.run([str(args.probe.resolve()), '--decimal', str(root)], check=True)
        paths = sorted(root.glob('*.parquet'))
        assert len(paths) == 24, len(paths)
        for path in paths:
            storage, nullability, version, codec = path.stem.split('-')
            nullable = nullability == 'optional'
            expected_groups = [[Decimal(n).scaleb(-2) for n in numbers] for numbers in GROUPS]
            if nullable:
                expected_groups = [[None, *values, None] for values in expected_groups]
            expected = [value for group in expected_groups for value in group]
            table = pq.read_table(path)
            assert table['x'].to_pylist() == expected, path.name
            assert table.schema.field('x').nullable == nullable, path.name
            polars = pl.read_parquet(path)
            assert polars['x'].to_list() == expected, path.name
            raw = path.read_bytes()
            footer_size = int.from_bytes(raw[-8:-4], 'little')
            footer = read_struct(raw, len(raw) - 8 - footer_size)
            leaf = footer[2][1]
            assert leaf[6] == 5 and leaf[7] == 2 and leaf[8] == 6, (path.name, leaf)
            metadata = pq.ParquetFile(path).metadata
            for number, group in enumerate(footer[4]):
                chunk = group[1][0]
                stats = chunk[3][12]
                assert stats[7] is True and stats[8] is True, (path.name, "exact bounds")
                low, high = min(GROUPS[number]), max(GROUPS[number])
                assert (stats[6], stats[5]) == (physical(low, storage), physical(high, storage)), (path.name, stats)
                assert stats[3] == (2 if nullable else 0), path.name
                if storage == 'bytes':
                    assert 1 not in stats and 2 not in stats, path.name
                column_index = read_struct(raw, chunk[6])
                assert column_index[2] == [stats[6]] and column_index[3] == [stats[5]], path.name
                reference = metadata.row_group(number).column(0).statistics
                assert (reference.min, reference.max) == (Decimal(low).scaleb(-2), Decimal(high).scaleb(-2)), path.name
            connection = duckdb.connect()
            try:
                assert connection.execute('SELECT x FROM read_parquet(?)', [str(path)]).fetchall() == [(v,) for v in expected], path.name
                # These literals cross sign and byte-width boundaries. Incorrect
                # lexicographic bounds would discard row groups containing them.
                for operator, literal in [('=', Decimal('1.28')),('=', Decimal('-1.29')),
                                          ('=', Decimal('-0.01')),('>=', Decimal('0.01')),
                                          ('<', Decimal('0')),('>', Decimal('9999'))]:
                    comparison = {'=': lambda a,b:a==b, '>=':lambda a,b:a>=b,
                                  '<':lambda a,b:a<b, '>':lambda a,b:a>b}[operator]
                    wanted = [v for v in expected if v is not None and comparison(v,literal)]
                    arrow_operator = '==' if operator == '=' else operator
                    assert pq.read_table(path, filters=[('x',arrow_operator,literal)])['x'].to_pylist() == wanted, (path.name,operator,literal,'pyarrow')
                    assert connection.execute(f'SELECT x FROM read_parquet(?) WHERE x {operator} ?', [str(path),literal]).fetchall() == [(v,) for v in wanted], (path.name,operator,literal,'duckdb')
                    expression = {'=':pl.col('x')==literal, '>=':pl.col('x')>=literal,
                                  '<':pl.col('x')<literal, '>':pl.col('x')>literal}[operator]
                    assert pl.scan_parquet(path).filter(expression).collect()['x'].to_list() == wanted, (path.name,operator,literal,'polars')
            finally:
                connection.close()
            print('PASS', path.name)
        print('24 decimal files: values, signed bounds, page indexes and six pushed filters verified with Apache Thrift, PyArrow, DuckDB and Polars')


if __name__ == '__main__':
    main()
