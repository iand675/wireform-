#!/usr/bin/env python3
"""Canonical typed Variant reconstruction against raw encodings and arrow-rs.

All Haskell outputs are compared with independent, type-preserving expected
values. Reference limits/differences are asserted and reported explicitly;
they never relax the Haskell expectation or silently skip a native case.
"""
import argparse
import decimal
import json
from pathlib import Path
import struct
import subprocess
import tempfile

import pyarrow as pa
import pyarrow.ipc as ipc


NULL = ("null",)
EMPTY = bytes([1, 0, 0])


def dictionary(names):
    names = sorted(set(names), key=lambda text: text.encode())
    payload = bytearray()
    offsets = [0]
    for name in names:
        payload.extend(name.encode())
        offsets.append(len(payload))
    width = max(1, (max(len(names),len(payload)).bit_length()+7)//8)
    result = bytearray([1 + ((width-1)<<6)])
    result.extend(len(names).to_bytes(width,"little"))
    for offset in offsets:
        result.extend(offset.to_bytes(width,"little"))
    result.extend(payload)
    return bytes(result), names


def object_bytes(names, fields):
    keys = sorted(fields, key=lambda text: text.encode())
    ids, offsets = [], [0]
    body = bytearray()
    for key in keys:
        ids.append(names.index(key))
        body.extend(fields[key])
        offsets.append(len(body))
    iw = max(1, (max(ids,default=0).bit_length()+7)//8)
    ow = max(1, (len(body).bit_length()+7)//8)
    large = len(keys)>=256
    header = 2 | (((iw-1)<<2 | (ow-1) | (16 if large else 0))<<2)
    result=bytearray([header])
    result.extend(len(keys).to_bytes(4 if large else 1,"little"))
    for index in ids:
        result.extend(index.to_bytes(iw,"little"))
    for offset in offsets:
        result.extend(offset.to_bytes(ow,"little"))
    result.extend(body)
    return bytes(result)


def decode(metadata, data):
    def word(buf, start, width):
        assert start >= 0 and start + width <= len(buf)
        return int.from_bytes(buf[start:start + width], "little")
    width = (metadata[0] >> 6) + 1
    count = word(metadata, 1, width)
    offset_start = 1 + width
    start = offset_start + (count + 1) * width
    names = []
    for i in range(count):
        lo = word(metadata, offset_start + i * width, width)
        hi = word(metadata, offset_start + (i + 1) * width, width)
        names.append(metadata[start + lo:start + hi].decode())

    def value(buf):
        assert buf
        kind, header = buf[0] & 3, buf[0] >> 2
        if kind == 1:
            assert len(buf) == header + 1
            return ("string", buf[1:].decode())
        if kind == 0:
            if header == 0:
                assert len(buf) == 1
                return NULL
            if header in (1, 2):
                assert len(buf) == 1
                return ("bool", header == 1)
            integer_types = {3: ("int8", 1), 4: ("int16", 2), 5: ("int32", 4), 6: ("int64", 8), 11: ("date", 4), 12: ("timestamp", 8), 13: ("timestamp-ntz", 8), 17: ("time", 8), 18: ("timestamp-nanos", 8), 19: ("timestamp-ntz-nanos", 8)}
            if header in integer_types:
                tag, size = integer_types[header]
                assert len(buf) == size + 1
                return (tag, int.from_bytes(buf[1:], "little", signed=True))
            if header in (7, 14):
                size = 8 if header == 7 else 4
                assert len(buf) == size + 1
                return ("double-bits" if header == 7 else "float-bits", int.from_bytes(buf[1:], "little"))
            if header in (8, 9, 10):
                size = {8: 4, 9: 8, 10: 16}[header]
                assert len(buf) == size + 2
                return (f"decimal{size}", buf[1], int.from_bytes(buf[2:], "little", signed=True))
            if header == 20:
                assert len(buf) == 17
                return ("uuid", buf[1:])
            assert header in (15, 16)
            n = word(buf, 1, 4)
            assert len(buf) == 5 + n
            return ("binary", buf[5:]) if header == 15 else ("string", buf[5:].decode())
        if kind == 2:
            ow, iw, cw = (header & 3) + 1, ((header >> 2) & 3) + 1, 4 if header & 16 else 1
            n = word(buf, 1, cw)
            ids = 1 + cw
            offsets = ids + n * iw
            body = offsets + (n + 1) * ow
            result = {}
            for i in range(n):
                name = names[word(buf, ids + i * iw, iw)]
                assert name not in result
                lo = word(buf, offsets + i * ow, ow)
                hi = word(buf, offsets + (i + 1) * ow, ow)
                result[name] = value(buf[body + lo:body + hi])
            assert body + word(buf, offsets + n * ow, ow) == len(buf)
            return result
        ow, cw = (header & 3) + 1, 4 if header & 4 else 1
        n = word(buf, 1, cw)
        offsets = 1 + cw
        body = offsets + (n + 1) * ow
        result = []
        for i in range(n):
            lo = word(buf, offsets + i * ow, ow)
            hi = word(buf, offsets + (i + 1) * ow, ow)
            result.append(value(buf[body + lo:body + hi]))
        assert body + word(buf, offsets + n * ow, ow) == len(buf)
        return result
    return value(data)


def run(args, expected=None):
    result = subprocess.run(list(map(str, args)), capture_output=True, text=True, timeout=60)
    if expected is not None:
        assert result.returncode == expected, (args, result.returncode, result.stdout, result.stderr)
    return result


def outputs(folder, expected):
    actual = []
    for i, value in enumerate(expected):
        stem = folder / f"{i:06}"
        if value is None:
            assert stem.with_suffix(".null").is_file()
            assert not stem.with_suffix(".value").exists()
            actual.append(None)
        else:
            assert not stem.with_suffix(".null").exists()
            actual.append(decode(stem.with_suffix(".meta").read_bytes(), stem.with_suffix(".value").read_bytes()))
    assert actual == expected, (folder, actual, expected)


def generate_cases(fixture):
    def primitive(name, typ, array, expected, reference_error=None, reference_expected=None, field_metadata=None):
        field = pa.field("typed_value", typ, metadata=field_metadata)
        fixture(name, [pa.field("metadata", pa.binary(), False), field], [pa.array([EMPTY] * len(array)), array], expected, reference_error=reference_error, reference_expected=reference_expected)

    primitive("bool", pa.bool_(), pa.array([True, False, None]), [("bool", True), ("bool", False), NULL])
    for width in (8, 16, 32, 64):
        typ = getattr(pa, f"int{width}")()
        values = [-(2 ** (width - 1)), 2 ** (width - 1) - 1, 0]
        expected = list(map(lambda n: (f"int{width}", n), values))
        primitive(f"int{width}", typ, pa.array(values, type=typ), expected)
    for size, typ, bits in [(4, pa.float32(), [0x80000000, 0, 1, 0x7FC12345, 0x7F812345, 0x7F800000, 0xFF800000]), (8, pa.float64(), [0x8000000000000000, 0, 1, 0x7FF8123456789012, 0x7FF0123456789012, 0x7FF0000000000000, 0xFFF0000000000000])]:
        payload = bytearray()
        for raw in bits:
            payload.extend(raw.to_bytes(size, "little"))
        array = pa.Array.from_buffers(typ, len(bits), [None, pa.py_buffer(payload)])
        primitive(f"float{size * 8}", typ, array, list(map(lambda n: ("float-bits" if size == 4 else "double-bits", n), bits)))
    for precision in range(1, 39):
        size, make = (4, pa.decimal32) if precision <= 9 else (8, pa.decimal64) if precision <= 18 else (16, pa.decimal128)
        for scale in (0, precision):
            values = [-(10 ** precision - 1), 0, 10 ** precision - 1]
            decimal_values = []
            with decimal.localcontext() as context:
                context.prec = 100
                for value in values:
                    decimal_values.append(decimal.Decimal(value).scaleb(-scale))
            typ = make(precision, scale)
            primitive(f"decimal-{precision}-{scale}", typ, pa.array(decimal_values, type=typ), list(map(lambda n: (f"decimal{size}", scale, n), values)))
    for typ, label, values in [(pa.date32(), "date", [-40000, 0, 40000]), (pa.time64("us"), "time", [0, 1, 86399999999])]:
        raw = pa.array(values, type=pa.int32() if label == "date" else pa.int64()).cast(typ)
        primitive(label, typ, raw, list(map(lambda n: (label, n), values)))
    extreme = [-(2 ** 31), 2 ** 31 - 1]
    primitive("date-full-domain", pa.date32(), pa.array(extreme, type=pa.int32()).cast(pa.date32()), list(map(lambda n: ("date", n), extreme)), reference_error="Invalid Date32 value")
    for timezone in (None, "UTC"):
        for unit in ("us", "ns"):
            typ = pa.timestamp(unit, tz=timezone)
            label = "timestamp" if timezone else "timestamp-ntz"
            if unit == "ns":
                label += "-nanos"
            values = [-1, 0, 1]
            expected = list(map(lambda n: (label, n), values))
            narrower = None
            if unit == "ns":
                narrower = [(label, -1), ("timestamp" if timezone else "timestamp-ntz", 0), (label, 1)]
            primitive(label, typ, pa.array(values, type=pa.int64()).cast(typ), expected, reference_expected=narrower)
            extreme = [-(2 ** 63), 2 ** 63 - 1]
            primitive(label + "-full-domain", typ, pa.array(extreme, type=pa.int64()).cast(typ), list(map(lambda n: (label, n), extreme)), reference_error="Invalid timestamp microsecond value" if unit == "us" else None)
    for typ in (pa.binary(), pa.large_binary(), pa.binary_view()):
        values = [b"", bytes(range(256)), b"long enough for a referenced view"]
        primitive(str(typ), typ, pa.array(values, type=typ), list(map(lambda value: ("binary", value), values)))
    for typ in (pa.string(), pa.large_string(), pa.string_view()):
        values = ["", "λ🦊", "a long string outside an inline view"]
        primitive(str(typ), typ, pa.array(values, type=typ), list(map(lambda value: ("string", value), values)))
    uuid = bytes(range(16))
    primitive("uuid", pa.binary(16), pa.array([uuid], type=pa.binary(16)), [("uuid", uuid)], field_metadata={b"ARROW:extension:name": b"arrow.uuid", b"ARROW:extension:metadata": b""})
    # Partial object with both fully shredded and fallback-only names.
    metadata, names = dictionary(["a", "rest"])
    wrapper_fields = [pa.field("typed_value", pa.int64()), pa.field("value", pa.binary())]
    child = pa.StructArray.from_arrays([pa.array([34, None, None], type=pa.int64()), pa.array([None, b"\0", None])], fields=wrapper_fields)
    typed = pa.StructArray.from_arrays([child], fields=[pa.field("a", child.type, False)])
    fallback = object_bytes(names, {"rest": bytes([12, 9])})
    fields = [pa.field("typed_value", typed.type), pa.field("value", pa.binary()), pa.field("metadata", pa.binary(), False)]
    fixture("partial-object", fields, [typed, pa.array([fallback, None, None]), pa.array([metadata] * 3)], [{"a": ("int64", 34), "rest": ("int8", 9)}, {"a": NULL}, {}])
    fixture("overlapping-object", fields, [typed, pa.array([object_bytes(names, {"a": bytes([12, 9])}), None, None]), pa.array([metadata] * 3)], [], invalid=True)
    fixture("primitive-conflict", [pa.field("metadata", pa.binary(), False), pa.field("value", pa.binary()), pa.field("typed_value", pa.int64())], [pa.array([EMPTY]), pa.array([b"\0"]), pa.array([34], type=pa.int64())], [], invalid=True)
    # Missing required elements are materialized as nulls, never dropped.
    for label in ("list", "large-list", "list-view"):
        if label == "list":
            array = pa.ListArray.from_arrays(pa.array([0, 3], type=pa.int32()), child, type=pa.list_(pa.field("element", child.type, False)))
        elif label == "large-list":
            array = pa.LargeListArray.from_arrays(pa.array([0, 3], type=pa.int64()), child, type=pa.large_list(pa.field("element", child.type, False)))
        else:
            array = pa.ListViewArray.from_arrays(pa.array([0], type=pa.int32()), pa.array([3], type=pa.int32()), child, type=pa.list_view(pa.field("element", child.type, False)))
        fixture(label, [pa.field("metadata", pa.binary(), False), pa.field("typed_value", array.type)], [pa.array([metadata]), array], [[("int64", 34), NULL, NULL]])
    # An array of partially shredded objects nested in a shredded
    # object. Poisoned children below a null typed object stay masked.
    metadata, names = dictionary(["arr", "rest", "x", "y"])
    x_fields = [pa.field("typed_value", pa.int32()), pa.field("value", pa.binary())]
    x = pa.StructArray.from_arrays([pa.array([3, 999, None, None], type=pa.int32()), pa.array([None, b"poison", b"\0", None])], fields=x_fields, mask=pa.array([False, True, False, False]))
    objects = pa.StructArray.from_arrays([x], fields=[pa.field("x", x.type, False)], mask=pa.array([False, True, False, False]))
    fragment = object_bytes(names, {"y": bytes([13]) + b"abc"})
    element_fields = [pa.field("value", pa.binary()), pa.field("typed_value", objects.type)]
    elements = pa.StructArray.from_arrays([pa.array([fragment, b"\0", None, None]), objects], fields=element_fields)
    array = pa.ListArray.from_arrays(pa.array([0, 4], type=pa.int32()), elements, type=pa.list_(pa.field("element", elements.type, False)))
    arr = pa.StructArray.from_arrays([array], fields=[pa.field("typed_value", array.type)])
    typed = pa.StructArray.from_arrays([arr], fields=[pa.field("arr", arr.type, False)])
    remainder = object_bytes(names, {"rest": bytes([12, 9])})
    fixture("recursive-object-array-partial", [pa.field("metadata", pa.binary(), False), pa.field("typed_value", typed.type), pa.field("value", pa.binary())], [pa.array([metadata]), typed, pa.array([remainder])], [{"arr": [{"x": ("int32", 3), "y": ("string", "abc")}, NULL, {"x": NULL}, {}], "rest": ("int8", 9)}])
    fixture("missing-shared-name", [pa.field("metadata", pa.binary(), False), pa.field("typed_value", typed.type)], [pa.array([EMPTY]), typed], [], invalid=True)
    # All binary metadata profiles, every legal dictionary key width and
    # signed run-end width. A root null masks a poisoned metadata value.
    for typ in (pa.binary(), pa.large_binary(), pa.binary_view()):
        metadata_array = pa.array([EMPTY, b"poison"], type=typ)
        for signed in (True, False):
            for width in (8, 16, 32, 64):
                key_type = getattr(pa, ("int" if signed else "uint") + str(width))()
                encoded = pa.DictionaryArray.from_arrays(pa.array([0, 1], type=key_type), metadata_array)
                fixture(f"metadata-{typ}-{key_type}", [pa.field("metadata", encoded.type, False), pa.field("typed_value", pa.int64())], [encoded, pa.array([7, 999], type=pa.int64())], [("int64", 7), None], mask=pa.array([False, True]))
        for width in (16, 32, 64):
            ends = pa.array([1, 2], type=getattr(pa, f"int{width}")())
            encoded = pa.RunEndEncodedArray.from_arrays(ends, metadata_array)
            fixture(f"metadata-{typ}-ree{width}", [pa.field("metadata", encoded.type, False), pa.field("typed_value", pa.int64())], [encoded, pa.array([7, 999], type=pa.int64())], [("int64", 7), None], mask=pa.array([False, True]))
    # Unequal object ID/offset widths: these used to be swapped in
    # this driver's independent parser while all-small fixtures hid it.
    metadata,names=dictionary(["a","long"])
    text="x"*300
    payload=bytes([64])+len(text).to_bytes(4,"little")+text.encode()
    a=pa.StructArray.from_arrays([pa.array([7],type=pa.int64())],fields=[pa.field("typed_value",pa.int64())])
    typed=pa.StructArray.from_arrays([a],fields=[pa.field("a",a.type,False)])
    fixture("wide-offset-narrow-id",[pa.field("metadata",pa.binary(),False),pa.field("value",pa.binary()),pa.field("typed_value",typed.type)],[pa.array([metadata]),pa.array([object_bytes(names,{"long":payload})]),typed],[{"a":("int64",7),"long":("string",text)}])
    names=[]
    for i in range(256):names.append(f"k{i:03}")
    metadata,keys=dictionary(names+["inner","zzzz"])
    scalar=pa.StructArray.from_arrays([pa.array([b"\0"])],fields=[pa.field("value",pa.binary())])
    z=pa.StructArray.from_arrays([scalar],fields=[pa.field("zzzz",scalar.type,False)])
    inner=pa.StructArray.from_arrays([z],fields=[pa.field("typed_value",z.type)])
    arrays,fields,expected=[],[],{"inner":{"zzzz":NULL}}
    for name in names:
        arrays.append(scalar);fields.append(pa.field(name,scalar.type,False));expected[name]=NULL
    arrays.append(inner);fields.append(pa.field("inner",inner.type,False))
    typed=pa.StructArray.from_arrays(arrays,fields=fields)
    fixture("wide-id-narrow-offset-nested",[pa.field("metadata",pa.binary(),False),pa.field("typed_value",typed.type)],[pa.array([metadata]),typed],[expected])
    # Zero rows still compile typed-only/empty-object shapes, and an empty
    # typed object is a present object rather than a missing value.
    empty_object = pa.StructArray.from_arrays([], fields=[], mask=pa.array([False]))
    fixture("empty-object", [pa.field("metadata", pa.binary(), False), pa.field("typed_value", empty_object.type)], [pa.array([EMPTY]), empty_object], [{}])
    empty = pa.StructArray.from_arrays([], fields=[], mask=pa.array([], type=pa.bool_()))
    fixture("zero-rows", [pa.field("metadata", pa.binary(), False), pa.field("typed_value", empty.type)], [pa.array([], type=pa.binary()), empty], [])


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--reader", required=True, type=Path)
    parser.add_argument("--reference", required=True, type=Path)
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    results = []
    with tempfile.TemporaryDirectory(prefix="parquet-typed-variant-") as directory:
        root = Path(directory)

        def fixture(name, fields, arrays, expected, mask=None, invalid=False, reference_error=None, reference_expected=None):
            column = pa.StructArray.from_arrays(arrays, fields=fields, mask=mask)
            field = pa.field("v", column.type, nullable=True, metadata={b"ARROW:extension:name": b"arrow.parquet.variant", b"ARROW:extension:metadata": b""})
            schema = pa.schema([field])
            path = root / (name + ".arrow")
            with ipc.new_file(path, schema) as writer:
                writer.write_batch(pa.record_batch([column], schema=schema))
            native, reference = root / (name + "-native"), root / (name + "-rust")
            actual = run([args.reader, "--variant-arrow-export", path, native])
            rust = run([args.reference, path, reference])
            if invalid:
                assert actual.returncode != 0 and rust.returncode != 0, (name, actual, rust)
            else:
                assert actual.returncode == 0, (name, actual.stderr)
                outputs(native, expected)
                if reference_error:
                    assert rust.returncode != 0 and reference_error in rust.stderr, (name, rust.returncode, rust.stderr)
                else:
                    assert rust.returncode == 0, (name, rust.stderr)
                    outputs(reference, reference_expected if reference_expected is not None else expected)
            results.append({"name": name, "rows": len(expected), "invalidRejected": invalid, "referenceLimitation": reference_error, "referenceSemanticDifference": reference_expected is not None})

        generate_cases(fixture)
    report = {"pyarrow": pa.__version__, "cases": len(results), "rows": sum(item["rows"] for item in results), "failures": 0, "skips": 0, "results": results}
    if args.output:
        args.output.write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps({key: value for key, value in report.items() if key != "results"}, indent=2))


if __name__ == "__main__":
    main()
