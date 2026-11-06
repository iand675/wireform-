#!/usr/bin/env python3
"""Check nested schema projection against independent PyArrow files and models.

The target is delivered as a schema-only Arrow IPC stream. Positive checks
compare output schema and values, not just successful decoding. Cases vary
page versions, dictionary encoding, compression, storage wrapper names and
row-group boundaries (including null-only groups).
"""
from __future__ import annotations

import argparse
from pathlib import Path
import subprocess
import tempfile

import pyarrow as pa
import pyarrow.parquet as pq

from parquet_aggregate_interop import groups, rewrite


def field(name, typ, nullable=True):
    return pa.field(name, typ, nullable=nullable)


def schema(typ, nullable=True):
    return pa.schema([field("v", typ, nullable)])


def catalogue():
    source_list = pa.list_(field("source_item", pa.int32()))
    source_struct = pa.struct([
        field("unused", pa.int32(), False), field("small", pa.int32()),
        field("float", pa.float32()),
    ])
    selected = pa.struct([field("float", pa.float64()), field("small", pa.int64(), False)])
    records = [
        {"unused": 9, "small": 1, "float": 1.5}, None,
        {"unused": 7, "small": 2, "float": None},
        {"unused": 5, "small": 3, "float": 4.5},
    ]
    lists = [[1, None, 2], None, [], [3]]
    yield "list", schema(source_list), lists, schema(pa.list_(field("custom", pa.int64())))
    yield "large-list", schema(source_list), lists, schema(pa.large_list(field("custom", pa.int64())))
    yield "struct", schema(source_struct), records, schema(selected)
    yield "empty-struct", schema(source_struct), records, schema(pa.struct([]))
    yield "list-struct", schema(pa.list_(field("source_item", source_struct))), [records[:3], None, [], records[3:]], schema(pa.large_list(field("requested_item", selected)))
    source_map = pa.map_(field("key", pa.int32(), False), field("value", pa.float32()))
    target_map = pa.map_(field("k", pa.int64(), False), field("x", pa.float64()))
    yield "map", schema(source_map), [[(1, 1.5), (2, None)], None, [], [(3, 3.5)]], schema(target_map)
    yield "lists-of-lists", schema(pa.list_(field("outer", source_list))), [[[1], None, [], [2, None]], None, [], [[3]]], schema(pa.large_list(field("out", pa.large_list(field("in", pa.int64())))))
    yield "list-of-maps", schema(pa.list_(field("source", source_map))), [[[(1, 1.5)], None, []], None, [], [[(2, None)]]], schema(pa.large_list(field("target", target_map)))
    yield "map-of-structs", schema(pa.map_(field("key", pa.int32(), False), field("value", source_struct))), [[(1, records[0]), (2, None)], None, [], [(3, records[2])]], schema(pa.map_(field("id", pa.int64(), False), field("record", selected)))
    yield "struct-list", schema(pa.struct([field("unused", pa.int32(), False), field("items", source_list)])), [{"unused": 1, "items": [1, None]}, None, {"unused": 2, "items": []}, {"unused": 3, "items": None}], schema(pa.struct([field("items", pa.large_list(field("renamed", pa.int64())))]))


def project_value(source, target, value):
    """A logical model independent of Dremel levels and wireform's reader."""
    if value is None:
        if not target.nullable:
            raise ValueError("visible null in required target")
        return None
    actual, requested = source.type, target.type
    if pa.types.is_struct(actual):
        result = {}
        for child in requested:
            result[child.name] = project_value(actual.field(child.name), child, value[child.name])
        return result
    if pa.types.is_list(actual) or pa.types.is_large_list(actual):
        result = []
        for item in value:
            result.append(project_value(actual.value_field, requested.value_field, item))
        return result
    if pa.types.is_map(actual):
        result = []
        for key, item in value:
            result.append((project_value(actual.key_field, requested.key_field, key),
                           project_value(actual.item_field, requested.item_field, item)))
        return result
    if actual != requested and not (
        actual == pa.int32() and requested == pa.int64()
        or actual == pa.float32() and requested == pa.float64()
    ):
        raise ValueError("incompatible leaf projection")
    return value


def write_target(path, target):
    with pa.OSFile(str(path), "wb") as sink:
        with pa.ipc.new_stream(sink, target):
            pass


def projected(probe, source, target_path, output, expected, success=True):
    result = subprocess.run([str(probe), "--project", str(source), str(target_path), str(output)],
                            text=True, capture_output=True)
    if not success:
        assert result.returncode != 0, (source.name, "unexpected projection success")
        assert "IncompatibleType" in result.stdout or "MissingColumn" in result.stdout, result.stdout
        return
    assert result.returncode == 0, (source.name, result.stdout, result.stderr)
    actual = pa.ipc.open_stream(output).read_all()
    actual.validate(full=True)
    assert actual.schema.equals(expected.schema, check_metadata=True), (source.name, actual.schema, expected.schema)
    assert actual.equals(expected), (source.name, actual.to_pylist(), expected.to_pylist())


def remove_first_metadata(node):
    for group in groups(node):
        # RowGroup.columns[0].meta_data is optional in the Thrift schema.
        columns = group[1][1][1][1]
        columns[0][1].pop(3, None)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--reader", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path)
    args = parser.parse_args()
    probe = args.reader.resolve()
    temporary = tempfile.TemporaryDirectory(prefix="wireform-parquet-projection-")
    root = args.output_dir or Path(temporary.name)
    root.mkdir(parents=True, exist_ok=True)
    successes = rejections = poisoned = reference = 0
    for label, native, values, target in catalogue():
        source_table = pa.Table.from_pylist(list(map(lambda value: {"v": value}, values)), schema=native)
        target_path = root / (label + ".schema.arrows")
        write_target(target_path, target)
        # PyArrow's own IPC roundtrip canonicalizes map key/value names.
        # Compare the schema actually delivered by the reference IPC reader.
        target = pa.ipc.open_stream(target_path).schema
        expected_rows = []
        for value in values:
            expected_rows.append({"v": project_value(native.field(0), target.field(0), value)})
        expected = pa.Table.from_pylist(expected_rows, schema=target)
        # C++ performs the same safe widening and wrapper-name conversion.
        # It conservatively rejects nullable->required children even when all
        # null children are hidden beneath null parents; the model above checks
        # that additional wireform case independently.
        for version, dictionary, codec, group_size, compliant in [
            ("1.0", False, "NONE", 4, False),
            ("2.0", True, "snappy", 1, True),
            ("2.0", False, "zstd", 2, False),
        ]:
            name = f"{label}-{version}-{dictionary}-{codec}"
            source = root / (name + ".parquet")
            pq.write_table(source_table, source, data_page_version=version,
                           use_dictionary=dictionary, compression=codec,
                           row_group_size=group_size, store_schema=False,
                           use_compliant_nested_type=compliant)
            projected(probe, source, target_path, root / (name + ".arrows"), expected)
            successes += 1
            if label in ("list", "large-list", "map", "lists-of-lists", "list-of-maps", "struct-list"):
                actual = pq.read_table(source, schema=target)
                assert actual.equals(expected), (name, "PyArrow projection differs from independent model")
                reference += 1
            if label in ("struct", "struct-list"):
                broken = root / (name + "-omitted-broken.parquet")
                rewrite(source, broken, remove_first_metadata)
                projected(probe, broken, target_path, root / (name + "-omitted-broken.arrows"), expected)
                poisoned += 1
        print("PASS", label)
    negative_source = root / "list-1.0-False-NONE.parquet"
    for label, target in [
        ("required-element", schema(pa.list_(field("item", pa.int64(), False)))),
        ("required-parent", schema(pa.list_(field("item", pa.int64())), False)),
        ("incompatible-leaf", schema(pa.list_(field("item", pa.float64())))),
        ("missing-parent", pa.schema([field("missing", pa.list_(field("item", pa.int64())))])),
    ]:
        path = root / (label + ".schema.arrows")
        write_target(path, target)
        projected(probe, negative_source, path, root / (label + ".arrows"), None, success=False)
        rejections += 1
    print(f"PASS: {successes} nested projections, {poisoned} omitted broken columns, {rejections} invalid requests; {reference} C++ cross-checks")


if __name__ == "__main__":
    main()
