#!/usr/bin/env python3
"""Independently verify native decimal/fixed/UUID files through PyArrow.

Input files are emitted by NestedPrimitiveRegression with
WIREFORM_PARQUET_PRIMITIVE_OUTPUT set. Missing fixtures and unsupported
reference behavior fail; this driver does not skip cases.
"""
import decimal
import json
from pathlib import Path
import sys
import pyarrow as pa
import pyarrow.parquet as pq


def decimal_value(raw, scale):
    with decimal.localcontext() as context:
        context.prec = 100
        return decimal.Decimal(raw).scaleb(-scale)


def check(directory):
    checks = 0
    for precision in range(1, 77):
        path = directory / f"decimal-{precision}.parquet"
        table = pq.read_table(path)
        table.validate(full=True)
        datatype = table.schema.field(0).type
        assert datatype.precision == precision and datatype.scale == precision, path
        bound = 10 ** precision - 1
        assert table.column(0).to_pylist() == [decimal_value(-bound, precision), None, decimal_value(0, precision), decimal_value(bound, precision)], path
        physical = pq.ParquetFile(path).schema.column(0).physical_type
        assert physical == ("INT32" if precision <= 9 else "INT64" if precision <= 18 else "FIXED_LEN_BYTE_ARRAY"), (path, physical)
        checks += 1
    for width in (1, 2, 3, 16, 31, 32, 64):
        path = directory / f"fixed-{width}.parquet"
        table = pq.read_table(path)
        table.validate(full=True)
        assert table.column(0).to_pylist() == [bytes(range(width)), None, bytes([255]) * width], path
        assert pa.types.is_fixed_size_binary(table.schema.field(0).type), path
        assert table.schema.field(0).type.byte_width == width, path
        checks += 1
    path = directory / "uuid.parquet"
    table = pq.read_table(path)
    table.validate(full=True)
    value = table.column(0).to_pylist()[0]
    assert (value.bytes if hasattr(value, "bytes") else value) == bytes(range(16)), path
    assert pq.ParquetFile(path).schema.column(0).logical_type.type == "UUID", path
    checks += 1
    for bits, precision in ((32, 9), (64, 18), (128, 38), (256, 76)):
        wanted = [decimal_value(-(10 ** precision - 1), 2), decimal_value(0, 2), decimal_value(123, 2)]
        for shape in ("flat", "list"):
            path = directory / f"arrow-{shape}-{bits}.parquet"
            table = pq.read_table(path)
            table.validate(full=True)
            assert table.column(0).to_pylist() == (wanted if shape == "flat" else [wanted[:1], wanted[1:]]), path
            checks += 1
    return {"pyarrow": pa.__version__, "semantic_file_checks": checks, "failures": 0, "skips": 0}


if __name__ == "__main__":
    assert len(sys.argv) == 2, "usage: parquet_primitive_interop.py FIXTURE_DIRECTORY"
    print(json.dumps(check(Path(sys.argv[1])), indent=2))
