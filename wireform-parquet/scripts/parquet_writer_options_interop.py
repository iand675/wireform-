#!/usr/bin/env python3
"""Verify writer settings and page indexes with Apache Thrift and three engines."""
import argparse
from pathlib import Path
import subprocess
import tempfile

import duckdb
import polars as pl
import pyarrow as pa
import pyarrow.parquet as pq
from parquet_statistics_interop import read_value, TCompactProtocol, TMemoryBuffer, TType


def thrift_at(raw, offset):
    transport = TMemoryBuffer(raw[offset:])
    value = read_value(TCompactProtocol(transport), TType.STRUCT)
    return value, transport.cstringio_buf.tell()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--probe', required=True, type=Path)
    args = parser.parse_args()
    with tempfile.TemporaryDirectory(prefix='wireform-parquet-options-') as directory:
        root = Path(directory)
        subprocess.run([str(args.probe.resolve()), '--writer-options', str(root)], check=True)
        files = sorted(root.glob('*.parquet'))
        assert len(files) == 32, len(files)
        for path in files:
            shape, version, codec, index_mode = path.stem.split('-')
            nullable, indexed = shape == 'mixed', index_mode == 'index'
            expected = pa.Table.from_arrays([pa.array([1,2,3] * 2, pa.int32()),
                pa.array(([10,None,30] if nullable else [10,20,30]) * 2, pa.int64())],
                schema=pa.schema([pa.field('r', pa.int32(), nullable=False),
                                  pa.field('o', pa.int64(), nullable=nullable)]))
            assert pq.read_table(path).equals(expected), path.name
            polars = pl.read_parquet(path).to_arrow()
            assert polars.schema.types == expected.schema.types and polars.cast(expected.schema).equals(expected), path.name
            con = duckdb.connect()
            try:
                duck = con.execute('SELECT * FROM read_parquet(?)', [str(path)]).to_arrow_table()
                assert duck.schema.types == expected.schema.types and duck.cast(expected.schema).equals(expected), path.name
            finally:
                con.close()
            raw = path.read_bytes()
            footer_size = int.from_bytes(raw[-8:-4], 'little')
            footer_start = len(raw) - 8 - footer_size
            footer, _ = thrift_at(raw, footer_start)
            metadata = pq.ParquetFile(path).metadata
            assert metadata.num_row_groups == 2
            page_ends = []
            for rg, group in enumerate(footer[4]):
                assert group[2] == sum(chunk[3][6] for chunk in group[1]), (path.name, 'row-group uncompressed size')
                for col, chunk in enumerate(group[1]):
                    assert chunk[2] == 0, (path.name, 'deprecated file_offset must be zero')
                    column = chunk[3]
                    assert column[12][7] is True and column[12][8] is True, (path.name, "exact bounds")
                    reference = metadata.row_group(rg).column(col)
                    assert reference.compression.lower() == codec, (path.name, reference.compression)
                    offset = column[9]
                    header, header_length = thrift_at(raw, offset)
                    assert header[1] == (0 if version == 'v1' else 3), path.name
                    page_end = offset + header_length + header[3]
                    page_ends.append(page_end)
                    assert page_end - offset == column[7], (path.name, 'compressed size')
                    assert header_length + header[2] == column[6], (path.name, 'uncompressed size')
                    assert (14 in column and 15 in column) == (col == 1), (path.name, 'bloom')
                    assert reference.has_offset_index == indexed and reference.has_column_index == indexed, path.name
                    assert all((field in chunk) == indexed for field in (4,5,6,7)), (path.name, chunk)
                    if not indexed:
                        continue
                    # ColumnChunk IDs: offset index=(4,5), column index=(6,7).
                    offset_index, used = thrift_at(raw, chunk[4])
                    assert used == chunk[5] and offset_index[1] == [{1: offset, 2: page_end-offset, 3: 0}], path.name
                    column_index, used = thrift_at(raw, chunk[6])
                    assert used == chunk[7] and column_index[1] == [False], path.name
                    assert column_index[5] == [int(nullable and col == 1)], path.name
                    width = 4 if col == 0 else 8
                    assert column_index[2] == [(1 if col == 0 else 10).to_bytes(width, 'little', signed=True)], path.name
                    assert column_index[3] == [(3 if col == 0 else 30).to_bytes(width, 'little', signed=True)], path.name
                    assert page_end <= chunk[4] < chunk[6] < footer_start, path.name
            assert max(page_ends) <= footer_start
            print('PASS', path.name)
        print('32 files: values verified in PyArrow, DuckDB and Polars; page versions, codecs, blooms and indexes independently checked')


if __name__ == '__main__':
    main()
