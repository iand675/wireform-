#!/usr/bin/env python3
"""Foreign-written native Variant files, exact typed values and Rust unshredding.

The shared Arrow fixtures are lowered to the native storage types specified
by Parquet. Only footer annotations missing from PyArrow's writer are added;
all encoded data pages are produced by PyArrow without alteration.
"""
import argparse
import json
from pathlib import Path
import struct
import tempfile

import pyarrow as pa
import pyarrow.compute as pc
import pyarrow.ipc as ipc
import pyarrow.parquet as pq
from thrift.Thrift import TType

from parquet_lzo_interop import Node, encode
from parquet_variant_interop import annotate_variant, footer
from parquet_variant_typed_interop import EMPTY, generate_cases, outputs, run


def native_storage(field, array, visible=None):
    """Normalize physical storage without narrowing through Python scalars."""
    if visible is None:
        visible = pa.array([True] * len(array), type=pa.bool_())
    nulls = array.is_null()
    present = pc.and_(visible, pc.invert(nulls))
    if not field.nullable:
        assert not pc.any(pc.and_(visible, nulls)).as_py(), field
    typ = array.type
    if pa.types.is_dictionary(typ):
        values = array.dictionary
        if pa.types.is_binary_view(values.type):
            values = values.cast(pa.binary())
        decoded = pc.take(values, array.indices)
        return native_storage(field.with_type(decoded.type), decoded, visible)
    if pa.types.is_run_end_encoded(typ):
        values = array.values
        if pa.types.is_binary_view(values.type):
            values = values.cast(pa.binary())
        encoded = pa.RunEndEncodedArray.from_arrays(array.run_ends, values)
        decoded = pc.run_end_decode(encoded)
        return native_storage(field.with_type(decoded.type), decoded, visible)
    if pa.types.is_struct(typ):
        fields, children = [], []
        for index, child_field in enumerate(typ):
            child_field, child = native_storage(child_field, array.field(index), present)
            fields.append(child_field)
            children.append(child)
        # Required wrappers below a null ancestor carry arbitrary storage;
        # some writers reject the masked null bitmap before seeing the parent.
        result = pa.StructArray.from_arrays(children, fields=fields, mask=nulls if field.nullable else None)
    elif pa.types.is_list(typ) or pa.types.is_large_list(typ) or pa.types.is_list_view(typ):
        child = array.values
        offsets = array.offsets
        if pa.types.is_list_view(typ):
            indices, normalized = [], [0]
            for start, size in zip(offsets.to_pylist(), array.sizes.to_pylist()):
                indices.extend(range(start, start + size))
                normalized.append(len(indices))
            child = pc.take(child, pa.array(indices, type=pa.int64()))
            offsets = pa.array(normalized, type=pa.int32())
        else:
            offsets = offsets.cast(pa.int32(), safe=True)
        child_field, child = native_storage(typ.value_field, child)
        result = pa.ListArray.from_arrays(offsets, child, type=pa.list_(child_field), mask=nulls)
    elif pa.types.is_binary(typ) or pa.types.is_large_binary(typ) or pa.types.is_binary_view(typ):
        result = array.cast(pa.binary())
    elif pa.types.is_string(typ) or pa.types.is_large_string(typ) or pa.types.is_string_view(typ):
        result = array.cast(pa.string())
    elif field.metadata and field.metadata.get(b"ARROW:extension:name") == b"arrow.uuid":
        result = pa.ExtensionArray.from_storage(pa.uuid(), array)
    else:
        result = array
    return field.with_type(result.type), result


def annotations(source, target):
    annotate_variant(source, target)
    prefix, meta = footer(target)
    for item in meta.value[2].value:
        logical = item.value.get(10)
        if logical and 7 in logical.value:  # TIME: Arrow time64(us) has no zone
            logical.value[7].value[1] = Node(TType.BOOL, False)
    raw = encode(meta)
    target.write_bytes(prefix + raw + struct.pack("<I", len(raw)) + b"PAR1")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--reader", type=Path, required=True)
    parser.add_argument("--reference", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--dictionary-v2", action="store_true", help="dictionary-encoded Snappy data pages v2")
    args = parser.parse_args()
    results = []
    with tempfile.TemporaryDirectory(prefix="parquet-native-variant-") as folder:
        root = Path(folder)

        def fixture(name, fields, arrays, expected, mask=None, invalid=False, reference_error=None, reference_expected=None):
            shape_note = None
            # Empty typed objects have no physical leaf with which to record
            # presence. Test the exact semantic object via its unshredded form;
            # zero-leaf typed native layouts are separately rejected/zero-row tested.
            if name in ("empty-object", "zero-rows"):
                size = len(expected)
                fields = [pa.field("metadata", pa.binary(), False), pa.field("value", pa.binary(), False)]
                arrays = [pa.array([EMPTY] * size, type=pa.binary()), pa.array([bytes([2, 0, 0])] * size, type=pa.binary())]
                shape_note = "unshredded empty object; zero-leaf presence is unobservable"
            column = pa.StructArray.from_arrays(arrays, fields=fields, mask=mask)
            field, column = native_storage(pa.field("v", column.type), column)
            raw, path = root / (name + ".raw"), root / (name + ".parquet")
            pq.write_table(pa.Table.from_arrays([column], schema=pa.schema([field])), raw, compression="SNAPPY" if args.dictionary_v2 else "NONE", data_page_version="2.0" if args.dictionary_v2 else "1.0", store_decimal_as_integer=True, store_schema=False, use_dictionary=args.dictionary_v2)
            annotations(raw, path)
            native = root / (name + "-values")
            actual = run([args.reader, "--variant-export", path, "v", native])
            if invalid:
                assert actual.returncode != 0, (name, actual.stdout, actual.stderr)
            else:
                assert actual.returncode == 0, (name, actual.stdout, actual.stderr)
                for i in range(len(expected)):
                    for suffix in ("meta", "value", "null"):
                        original = native / f"0-{i}.{suffix}"
                        if original.exists():
                            original.rename(native / f"{i:06}.{suffix}")
                outputs(native, expected)
                # Infer the Arrow schema from the real native footer, then
                # send that schema and data to the independent Rust kernel.
                inputs, exported = root / (name + "-input"), root / (name + "-arrow")
                inputs.mkdir()
                path.rename(inputs / "case.parquet")
                run([args.reader, inputs, "--arrow-output", exported], expected=0)
                table = ipc.open_stream(exported / "case.parquet.arrows").read_all()
                arrow = root / (name + "-inferred.arrow")
                with ipc.new_file(arrow, table.schema) as writer:
                    writer.write_table(table)
                reference = root / (name + "-rust")
                rust = run([args.reference, arrow, reference])
                if reference_error:
                    assert rust.returncode != 0 and reference_error in rust.stderr, (name, rust.returncode, rust.stderr)
                else:
                    assert rust.returncode == 0, (name, rust.stdout, rust.stderr)
                    outputs(reference, reference_expected if reference_expected is not None else expected)
            results.append({"name": name, "rows": len(expected), "invalidRejected": invalid, "storageNormalization": shape_note, "referenceLimitation": reference_error, "referenceSemanticDifference": reference_expected is not None})

        generate_cases(fixture)
        malformed = []
        def rejected(name, typ, *, integer_decimal=True, edit=None):
            fields = [pa.field("metadata", pa.binary(), False), pa.field("typed_value", typ)]
            column = pa.StructArray.from_arrays([pa.array([], type=pa.binary()), pa.array([], type=typ)], fields=fields)
            raw, path = root / (name + ".raw"), root / (name + ".parquet")
            pq.write_table(pa.table({"v": column}), raw, compression="NONE", store_decimal_as_integer=integer_decimal, store_schema=False)
            annotations(raw, path)
            if edit:
                prefix, meta = footer(path)
                edit(meta.value[2].value)
                data = encode(meta)
                path.write_bytes(prefix + data + struct.pack("<I", len(data)) + b"PAR1")
            result = run([args.reader, "--variant-export", path, "v", root / (name + "-out")])
            assert result.returncode != 0, (name, result.stdout, result.stderr)
            malformed.append(name)
        for typ in [pa.uint8(), pa.uint16(), pa.uint32(), pa.uint64(), pa.float16(), pa.binary(16), pa.timestamp("ms"), pa.time32("ms")]:
            rejected("forbidden-" + str(typ), typ)
        rejected("small-decimal-fixed-carrier", pa.decimal32(9, 0), integer_decimal=False)
        rejected("medium-decimal-fixed-carrier", pa.decimal64(18, 2), integer_decimal=False)
        rejected("metadata-optional", pa.int32(), edit=lambda schema: schema[2].value.__setitem__(3, Node(TType.I32, 1)))
        rejected("typed-required", pa.int32(), edit=lambda schema: schema[3].value.__setitem__(3, Node(TType.I32, 0)))
        rejected("missing-metadata-name", pa.int32(), edit=lambda schema: schema[2].value.__setitem__(4, Node(TType.STRING, b"other")))
        list_type = pa.list_(pa.field("element", pa.struct([pa.field("typed_value", pa.int32())]), False))
        rejected("list-element-optional", list_type, edit=lambda schema: schema[5].value.__setitem__(3, Node(TType.I32, 1)))
        rejected("list-wrapper-optional", list_type, edit=lambda schema: schema[4].value.__setitem__(3, Node(TType.I32, 1)))
        rejected("list-outer-required", list_type, edit=lambda schema: schema[3].value.__setitem__(3, Node(TType.I32, 0)))
    report = {"pyarrow": pa.__version__, "cases": len(results), "rows": sum(case["rows"] for case in results), "failures": 0, "skips": 0, "results": results, "malformedSchemasRejected": malformed}
    args.output.write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps({key: value for key, value in report.items() if key != "results"}))


if __name__ == "__main__":
    main()
