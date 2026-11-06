#!/usr/bin/env python3
"""Check logical integer domains against independently annotated physical values."""
import argparse
from pathlib import Path
import struct
import tempfile
import json
import pyarrow as pa
import pyarrow.ipc as ipc
import pyarrow.parquet as pq
from parquet_variant_interop import footer, run
from parquet_lzo_interop import encode


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--reader', required=True, type=Path)
    parser.add_argument('--output', required=True, type=Path)
    args = parser.parse_args()
    results = []
    with tempfile.TemporaryDirectory(prefix='parquet-narrow-integers-') as folder:
        root = Path(folder)
        def write(path, typ, values, nested, nullable):
            array = pa.array(values, type=typ)
            field = pa.field('a', typ, nullable=nullable)
            if nested:
                array = pa.StructArray.from_arrays([array], fields=[field])
                field = pa.field('v', array.type, nullable=False)
            pq.write_table(pa.Table.from_arrays([array], schema=pa.schema([field])), path, compression='NONE', use_dictionary=False, store_schema=False)
        for signed in (True, False):
            for width in (8, 16):
                typ = getattr(pa, ('int' if signed else 'uint') + str(width))()
                low = -(2 ** (width-1)) if signed else 0
                high = 2 ** (width - (1 if signed else 0)) - 1
                prototype = root/'prototype'
                write(prototype, typ, [0], False, False)
                _, model = footer(prototype)
                annotation = model.value[2].value[-1].value
                for nested in (False, True):
                    for nullable in (False, True):
                        for label, values, valid in [('bounds', [low, high, 0, 1] + ([None] if nullable else []), True), ('underflow', [low-1], False), ('overflow', [high+1], False)]:
                            name = f'{typ}-{nested}-{nullable}-{label}'
                            inputs, output = root/name, root/(name+'-arrow')
                            inputs.mkdir()
                            path = inputs/'case.parquet'
                            write(path, pa.int32(), values, nested, nullable)
                            prefix, meta = footer(path)
                            leaf = meta.value[2].value[-1].value
                            for key in (6, 10):
                                if key in annotation:
                                    leaf[key] = annotation[key]
                            raw = encode(meta)
                            path.write_bytes(prefix+raw+struct.pack('<I',len(raw))+b'PAR1')
                            result = run([args.reader, inputs, '--arrow-output', output], expected=0 if valid else 1)
                            if valid:
                                table = ipc.open_stream(output/'case.parquet.arrows').read_all()
                                actual = table.column(0).combine_chunks()
                                if nested:
                                    actual = actual.field('a')
                                assert actual.type == typ and actual.to_pylist() == values, (name, actual)
                            else:
                                assert 'annotated value exceeds its physical/logical range' in result.stdout, (name, result.stdout, result.stderr)
                            results.append({'case':name,'invalidRejected':not valid})
    report={'cases':len(results),'validBoundaryCases':sum(not x['invalidRejected'] for x in results),'invalidRejected':sum(x['invalidRejected'] for x in results),'failures':0,'skips':0,'results':results}
    args.output.write_text(json.dumps(report,indent=2)+'\n')
    print(json.dumps({k:v for k,v in report.items() if k!='results'}))


if __name__ == '__main__':
    main()
