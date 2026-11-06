#!/usr/bin/env python3
"""Lossless timestamp interop with independent Apache Thrift and PyArrow.

PyArrow cannot interpret modern signed96 TIMESTAMP and its ordinary INT96
projection can overflow. The physical oracle changes only schema/type metadata
in a temporary copy to FixedSizeBinary(12), then asks unmodified C++ to decode
the original pages, dictionaries, compression and levels. The body is asserted
byte-identical. Python integers independently verify full-width semantics.
"""
import argparse
import json
from pathlib import Path
import struct
import subprocess
import tempfile

import pyarrow as pa
import pyarrow.ipc as ipc
import pyarrow.parquet as pq
from thrift.Thrift import TType
from parquet_lzo_interop import Node, encode
from parquet_metadata_interop import parse

NAME = b"ARROW:extension:name"
PARAMETERS = b"ARROW:extension:metadata"
SIGNED = b"wireform.parquet.timestamp96"
JULIAN = b"wireform.parquet.int96"
UNITS = {"millis": (1, "ms"), "micros": (2, "us"), "nanos": (3, "ns")}


def timestamp_leaves(footer):
    leaves = [node for node in footer.value[2].value if 1 in node.value]
    result = []
    for ordinal, node in enumerate(leaves):
        physical = node.value[1].value
        logical = node.value.get(10)
        timestamp = logical.value.get(8) if logical else None
        if physical == 3:
            result.append((ordinal, node, "julian", None, None))
        elif physical == 7 and node.value.get(2) and node.value[2].value == 12 and timestamp:
            parameters = timestamp.value
            unit = next(iter(parameters[2].value))
            result.append((ordinal, node, "signed", bool(parameters[1].value), {1:"millis", 2:"micros", 3:"nanos"}[unit]))
    return result


def write_footer(path, prefix, footer):
    encoded = encode(footer)
    path.write_bytes(prefix + encoded + struct.pack("<I", len(encoded)) + b"PAR1")


def physical_timestamp_copy(source, target):
    """Return changed leaf descriptors, or [] without writing for ordinary files."""
    parsed = parse(source)
    changed = timestamp_leaves(parsed.footer)
    if not changed:
        return []
    legacy = set()
    for ordinal, node, kind, _, _ in changed:
        node.value[1] = Node(TType.I32, 7)
        node.value[2] = Node(TType.I32, 12)
        node.value.pop(10, None)
        node.value.pop(6, None)
        if kind == "julian":
            legacy.add(ordinal)
    for group in parsed.footer.value[4].value:
        for ordinal, chunk in enumerate(group.value[1].value):
            if ordinal in legacy:
                chunk.value[3].value[1] = Node(TType.I32, 7)
    # Stored Arrow schema may otherwise force the original timestamp type.
    if 5 in parsed.footer.value:
        parsed.footer.value[5].value[:] = [entry for entry in parsed.footer.value[5].value if entry.value[1].value != b"ARROW:schema"]
    parsed.footer.value.pop(7, None)
    write_footer(target, parsed.prefix, parsed.footer)
    assert parse(target).prefix == parsed.prefix
    return [(ordinal, kind, adjusted, unit) for ordinal, _, kind, adjusted, unit in changed]


def annotate(path, encoding, adjusted=False, unit="nanos"):
    parsed = parse(path)
    leaves = [node for node in parsed.footer.value[2].value if 1 in node.value]
    assert len(leaves) == 1 and leaves[0].value[1].value == 7 and leaves[0].value[2].value == 12
    leaf = leaves[0]
    if encoding == "signed":
        leaf.value[10] = Node(TType.STRUCT, {8: Node(TType.STRUCT, {
            1: Node(TType.BOOL, adjusted),
            2: Node(TType.STRUCT, {UNITS[unit][0]: Node(TType.STRUCT, {})})})})
    else:
        leaf.value[1] = Node(TType.I32, 3)
        leaf.value.pop(2, None)
        for group in parsed.footer.value[4].value:
            group.value[1].value[0].value[3].value[1] = Node(TType.I32, 3)
    parsed.footer.value.pop(7, None)
    write_footer(path, parsed.prefix, parsed.footer)


def extension_field(name, encoding, adjusted=False, unit="nanos", nullable=True):
    metadata = {NAME: SIGNED if encoding == "signed" else JULIAN,
                PARAMETERS: (unit + (":utc" if adjusted else ":local")).encode() if encoding == "signed" else b""}
    return pa.field(name, pa.binary(12), nullable=nullable, metadata=metadata)


def shape_table(values, shape, field):
    if shape == "flat":
        field = field.with_name("v")
        data = values
    elif shape == "list":
        field = pa.field("v", pa.list_(field.with_name("element")))
        data = [values[:2], None, [], values[2:], [None]]
    else:
        field = pa.field("v", pa.struct([field.with_name("t")]))
        data = [{"t": value} for value in values] + [None]
    return pa.Table.from_arrays([pa.array(data, type=field.type)], schema=pa.schema([field]))


def leaf_fields(field):
    if pa.types.is_struct(field.type):
        for child in field.type:
            yield from leaf_fields(child)
    elif pa.types.is_list(field.type) or pa.types.is_large_list(field.type):
        yield from leaf_fields(field.type.value_field)
    else:
        yield field


def verify_extension(table, descriptors):
    fields = [leaf for field in table.schema for leaf in leaf_fields(field)]
    for ordinal, kind, adjusted, unit in descriptors:
        wanted = extension_field("", kind, adjusted, unit)
        actual = fields[ordinal]
        assert actual.type == pa.binary(12)
        assert actual.metadata[NAME] == wanted.metadata[NAME]
        assert actual.metadata[PARAMETERS] == wanted.metadata[PARAMETERS]


def checked(command):
    process = subprocess.run(list(map(str, command)), capture_output=True, text=True)
    assert process.returncode == 0, (command, process.stdout, process.stderr)


def read_native(reader, path, output):
    directory = output / "source"
    directory.mkdir(parents=True)
    (directory / path.name).symlink_to(path.resolve())
    dest = output / "decoded"
    checked([reader, directory, "--arrow-output", dest])
    with ipc.open_stream(dest / (path.name + ".arrows")) as stream:
        return stream.read_all()


def verify_file(reader, path, root):
    root.mkdir(parents=True)
    physical = root / "physical.parquet"
    descriptors = physical_timestamp_copy(path, physical)
    assert descriptors
    expected = pq.read_table(physical, page_checksum_verification=True)
    expected.validate(full=True)
    actual = read_native(reader, path, root / "native")
    actual.validate(full=True)
    verify_extension(actual, descriptors)
    assert actual.equals(expected, check_metadata=False), (path, actual, expected)
    return actual, descriptors


def project(reader, path, target, root, expected=None):
    schema_path, result = root / "target.arrows", root / "projected.arrows"
    with ipc.new_stream(schema_path, target) as stream:
        pass
    process = subprocess.run([str(reader), "--project", str(path), str(schema_path), str(result)], capture_output=True, text=True)
    if expected is None:
        assert process.returncode != 0 and not result.exists(), (path, process.stdout, process.stderr)
        return
    assert process.returncode == 0, (path, process.stdout, process.stderr)
    with ipc.open_stream(result) as stream:
        actual = stream.read_all()
    assert actual.column(0).cast(pa.int64()).to_pylist() == expected


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--reader", type=Path, required=True)
    parser.add_argument("--writer", type=Path, required=True)
    parser.add_argument("--native-fixtures", type=Path, required=True)
    parser.add_argument("--corpus", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    wide_counts = [-(1<<95), -(1<<63)-1, -(1<<63), -1, 0, (1<<63)-1, 1<<63, (1<<95)-1, None]
    narrow_counts = [-(1<<63), -1, 0, (1<<63)-1, None]
    legacy = [struct.pack("<Qi", nanos, day) for day, nanos in [(-(1<<31),0), (-1,86399999999999), (2440588,0), ((1<<31)-1,86399999999999)]] + [None, struct.pack("<qi", -(1<<63), 2440588), struct.pack("<qi", (1<<63)-1, 2440588)]
    cases = []
    for unit in UNITS:
        for adjusted in (False, True):
            for profile in ("plain-v1", "dictionary-v2"):
                for shape in ("flat", "list", "struct"):
                    cases.append((f"{unit}-{adjusted}-{profile}-{shape}", "signed", unit, adjusted, profile, shape, wide_counts, False))
                cases.append((f"{unit}-{adjusted}-{profile}-narrow", "signed", unit, adjusted, profile, "flat", narrow_counts, True))
    for profile in ("plain-v1", "dictionary-v2"):
        for shape in ("flat", "list", "struct"):
            cases.append((f"julian-{profile}-{shape}", "julian", "nanos", False, profile, shape, legacy, False))
    with tempfile.TemporaryDirectory(prefix="timestamp-interop-") as directory:
        work = Path(directory)
        rows = files = projections = 0
        for number, (name, kind, unit, adjusted, profile, shape, values, narrows) in enumerate(cases):
            root = work / str(number)
            root.mkdir()
            payloads = [value.to_bytes(12, "little", signed=True) if value is not None else None for value in values] if kind == "signed" else values
            field = extension_field("v", kind, adjusted, unit)
            table = shape_table(payloads, shape, field)
            original = root / (name + ".parquet")
            pq.write_table(table, original, compression="SNAPPY" if profile == "dictionary-v2" else "NONE", use_dictionary=profile == "dictionary-v2", data_page_version="2.0" if profile == "dictionary-v2" else "1.0", row_group_size=3, write_page_checksum=True)
            annotate(original, kind, adjusted, unit)
            actual, descriptors = verify_file(args.reader, original, root / "forward")
            assert actual.equals(table, check_metadata=False)
            rows += actual.num_rows
            source = root / "arrow"
            source.mkdir()
            with ipc.new_file(source / "source.arrow", actual.schema) as sink:
                sink.write_table(actual)
            dest = root / "rewritten"
            checked([args.writer, "--nested-from-arrow", source, dest])
            copied, copied_descriptors = verify_file(args.reader, dest / "source.parquet", root / "reverse")
            assert copied.equals(actual, check_metadata=False)
            assert copied_descriptors == descriptors
            files += 2
            if shape == "flat":
                target = pa.schema([pa.field("v", pa.timestamp(UNITS[unit][1], tz="UTC" if adjusted else None))])
                project(args.reader, original, target, root, values if narrows else None)
                projections += 1
        # Native Haskell-produced extrema and nulls, independently decoded by C++.
        native = sorted(args.native_fixtures.glob("timestamp96-*.parquet")) + [args.native_fixtures / "int96-full.parquet"]
        assert len(native) == 7 and all(path.is_file() for path in native)
        for index, path in enumerate(native):
            actual, descriptors = verify_file(args.reader, path, work / f"haskell-{index}")
            files += 1
            rows += actual.num_rows
            payloads = actual.column(0).to_pylist()
            if path.name.startswith("timestamp96"):
                assert [int.from_bytes(value,"little",signed=True) if value is not None else None for value in payloads] == wide_counts
            else:
                assert payloads == legacy[:3] + [None, legacy[3]] + legacy[5:]
        # Published Apache fixtures: use the documented integer values, avoiding
        # datetime ranges and the overflowing default PyArrow INT96 projection.
        for name in ("flba12_timestamp.parquet", "int96_from_spark.parquet", "int96_timestamp_order.parquet"):
            path = args.corpus / "data" / name
            actual, descriptors = verify_file(args.reader, path, work / name)
            files += 1
            rows += actual.num_rows
            if name == "flba12_timestamp.parquet":
                seconds = [0,1,-1,9223372036,253402300799,-62135596800]
                for ordinal, _, _, unit in descriptors:
                    scale = {"millis":1000,"micros":1000000,"nanos":1000000000}[unit]
                    assert [int.from_bytes(value,"little",signed=True) for value in actual.column(ordinal).to_pylist()] == [value*scale for value in seconds]
            if name == "int96_from_spark.parquet":
                expected = [1704141296123456,1704070800000000,253402225200000000,1735599600000000,None,9089380393200000000]
                target = pa.schema([pa.field("a", pa.timestamp("us"))])
                payloads = actual.column(0).to_pylist()
                exact = [None if value is None else ((struct.unpack("<qi", value)[1]-2440588)*86400000000000 + struct.unpack("<qi", value)[0]) for value in payloads]
                assert [None if value is None else value//1000 for value in exact[:5]] == expected[:5]
                assert exact[-1]//1000 < -(1<<63)
                project(args.reader, path, target, work / name)
                legacy_difference = {"fixture":name, "exactSignedDayAndNanosMicroseconds":str(exact[-1]//1000), "documentedMicroseconds":str(expected[-1]), "pyarrowMicroseconds":str(pq.read_table(path,coerce_int96_timestamp_unit="us").column(0).cast(pa.int64()).to_pylist()[-1]), "behavior":"original12bytes preserved; overflowing Int64 projection rejected"}
                projections += 1
        report = {"cases":len(cases)+10, "nativeFiles":files, "rows":rows, "checkedProjections":projections, "pyarrow":pa.__version__, "physicalOracle":"original body byte-identical; Thrift annotation/type-only adaptation then unmodified C++ decoding", "legacyReferenceDifferences":[legacy_difference], "failures":0, "skips":0}
        args.output.write_text(json.dumps(report,indent=2)+"\n")
        print(json.dumps(report,indent=2))

if __name__ == "__main__":
    main()
