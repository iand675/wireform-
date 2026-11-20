#!/usr/bin/env python3
"""Generate shared PyArrow input and independent expected page-selection values.

Both benchmark harnesses parse the footer and page indexes on every iteration,
retain whole pages whose maximum meets the predicate, and materialize the same
rows. File IO and this fixture generation are outside the timed region.
"""

import argparse
from dataclasses import dataclass
from pathlib import Path

import pyarrow as pa
import pyarrow.parquet as pq

from parquet_selection_interop import Dataset, index_info

ROWS = 131072


@dataclass(frozen=True)
class Plan:
    name: str
    text: bool
    nullable: bool
    dictionary: bool
    codec: str
    rows: int = ROWS
    width: int = 0
    batch: int = 1024


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('directory', type=Path)
    parser.add_argument('--extended', action='store_true', help='also measure 4 KiB strings across many pages')
    args = parser.parse_args()
    root = args.directory
    root.mkdir(parents=True, exist_ok=True)
    cases = [
        Plan('int-required-plain', False, False, False, 'NONE'),
        Plan('int-required-dictionary', False, False, True, 'ZSTD'),
        Plan('int-nullable-plain', False, True, False, 'NONE'),
        Plan('int-nullable-dictionary', False, True, True, 'ZSTD'),
        Plan('text-required-plain', True, False, False, 'SNAPPY'),
        Plan('text-nullable-dictionary', True, True, True, 'ZSTD'),
    ]
    if args.extended:
        for nullable in (False, True):
            for dictionary in (False, True):
                name = f'text-{"nullable" if nullable else "required"}-{"dictionary" if dictionary else "plain"}-4k'
                cases.append(Plan(name, True, nullable, dictionary, 'ZSTD' if dictionary else 'NONE', rows=4096, width=4096, batch=16))
    plans = []
    for plan in cases:
        def value_at(i):
            if plan.width:
                prefix = f'long_{i//32:05d}_'
                return prefix + 'x' * (plan.width - len(prefix))
            if plan.text:
                return f'name_{i//32:05d}'
            return i//32 if plan.dictionary else i

        values = [None if plan.nullable and i % 17 == 0 else value_at(i) for i in range(plan.rows)]
        typ = pa.string() if plan.text else pa.int64()
        table = pa.Table.from_arrays([pa.array(values, typ)], schema=pa.schema([pa.field('x', typ, plan.nullable)]))
        path = root / f'{plan.name}.parquet'
        pq.write_table(table, path, compression=plan.codec, data_page_version='2.0',
                       use_dictionary=plan.dictionary, dictionary_pagesize_limit=2**20,
                       # Dictionary indices are much smaller than the wide
                       # strings; use a smaller target to retain several pages.
                       data_page_size=128 if plan.width and plan.dictionary else 4096,
                       write_batch_size=plan.batch, row_group_size=plan.rows,
                       write_page_index=True, write_page_checksum=False)
        pivot = value_at(plan.rows//2)
        decode = (lambda raw: raw.decode()) if plan.text else (lambda raw: int.from_bytes(raw, 'little', signed=True))
        dataset = Dataset(plan.name, typ, values, str(pivot), pivot, decode)
        _, _, locations, keep, encodings = index_info(path, dataset)
        assert any(keep) and not all(keep) and len(locations) > 1
        if plan.dictionary:
            assert any(encoding in (2, 8) for encoding in encodings)
        parts = []
        for index, ((_, _, start), retained) in enumerate(zip(locations, keep)):
            if retained:
                end = locations[index+1][2] if index+1 < len(locations) else plan.rows
                parts.append(table.slice(start, end-start))
        expected = pa.concat_tables(parts)
        pq.write_table(expected, root / f'{plan.name}-expected.parquet', compression='NONE', use_dictionary=False)
        plans.append(f'{plan.name} {"text" if plan.text else "int"} {pivot}')
        print(f'{plan.name}: {sum(keep)}/{len(keep)} pages, {expected.num_rows}/{plan.rows} rows')
    (root / 'selection-plan.txt').write_text('\n'.join(plans)+'\n')


if __name__ == '__main__':
    main()
