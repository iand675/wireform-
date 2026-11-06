#!/usr/bin/env python3
"""Zero-leaf FILE schemas: independent footer fixtures and physical value oracle.

PyArrow cannot interpret FILE itself; Apache Thrift checks the annotation,
and PyArrow reads a copy with only FILE annotations removed. Upstream Rust
currently rejects zero-field writes and drops the FILE on read, so its
limitation is reported separately rather than used as the expected result.
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
from thrift.Thrift import TType as T
from parquet_lzo_interop import Node, encode
from parquet_file_interop import footer
from parquet_file_arrow_interop import file_infos, read_stream


def node(kind, value):
    return Node(kind, value)


def element(name, repetition=None, children=None, file=False):
    fields = {4: node(T.STRING, name.encode())}
    if repetition is not None:
        fields[3] = node(T.I32, repetition)
    if children is not None:
        fields[5] = node(T.I32, children)
    if file:
        fields[10] = node(T.STRUCT, {19: node(T.STRUCT, {})})
    return node(T.STRUCT, fields)


def write_footer(path, prefix, metadata):
    raw = encode(metadata)
    path.write_bytes(prefix + raw + struct.pack("<I", len(raw)) + b"PAR1")


def physical_copy(path, output):
    prefix, metadata = footer(path)
    found = 0
    for field in metadata.value[2].value:
        logical = field.value.get(10)
        if logical is not None and 19 in logical.value:
            assert field.value[5].value == 0, path
            assert 1 not in field.value, path
            del field.value[10]
            found += 1
    assert found == 1, path
    write_footer(output, prefix, metadata)
    return pq.read_table(output)


def project_physical_fields(path, output):
    """Remove only zero-leaf FILE nodes; physical column bytes stay identical.

    This lets C++ independently check the surviving parent/list boundaries
    when its schema converter cannot handle a nested empty group.
    """
    prefix, metadata = footer(path)
    fields = metadata.value[2].value
    def walk(index, root=False):
        field = fields[index]
        index += 1
        logical = field.value.get(10)
        if logical is not None and 19 in logical.value:
            assert field.value[5].value == 0
            return [], index
        if 1 in field.value:
            return [field], index
        children = []
        count = 0
        for _ in range(field.value[5].value):
            nodes, index = walk(index)
            if nodes:
                count += 1
                children.extend(nodes)
        if not root and count == 0:
            return [], index
        field.value[5] = node(T.I32, count)
        return [field] + children, index
    projected, end = walk(0, root=True)
    assert end == len(fields)
    metadata.value[2].value = projected
    write_footer(output, prefix, metadata)
    return pq.read_table(output)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--writer", required=True, type=Path)
    parser.add_argument("--reader", required=True, type=Path)
    parser.add_argument("--reference", required=True, type=Path)
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    metadata = {b"ARROW:extension:name": b"arrow.parquet.file", b"ARROW:extension:metadata": b"", b"PARQUET:field_id": b"27"}
    optional = pa.field("empty", pa.struct([]), nullable=True, metadata=metadata)
    required = optional.with_nullable(False)
    anchor = pa.field("anchor", pa.int32(), nullable=False)
    cases = {}
    def case(name, field, rows):
        cases[name] = pa.Table.from_arrays([pa.array(rows, field.type)], schema=pa.schema([field]))
    case("root-nulls", optional.with_name("f"), [None] * 3)
    case("root-empty-optional", optional.with_name("f"), [])
    case("root-empty-required", required.with_name("f"), [])
    cases["root-with-anchor"] = pa.Table.from_arrays([pa.array([None]*3, optional.type), pa.array([10,20,30], pa.int32())], schema=pa.schema([optional.with_name("f"), anchor]))
    case("required-parent", pa.field("f", pa.struct([optional]), nullable=False), [{"empty": None}] * 3)
    for fields in ([optional, anchor], [anchor, optional]):
        name = "optional-parent-first" if fields[0].name == "empty" else "optional-parent-last"
        case(name, pa.field("f", pa.struct(fields)), [None, {"empty": None, "anchor": 10}, {"empty": None, "anchor": 20}])
    entry = pa.field("element", pa.struct([optional, anchor]), nullable=False)
    case("list-anchored", pa.field("f", pa.list_(entry)), [None, [], [{"empty": None, "anchor": value} for value in range(5)]])
    case("map-value", pa.field("f", pa.map_(pa.int32(), optional.with_name("value"))), [None, [], [(1, None), (2, None)]])
    case("map-key-empty", pa.field("f", pa.map_(required.with_name("key"), pa.int32())), [None, [], []])
    case("zero-row-list", pa.field("f", pa.list_(optional.with_name("element"))), [])
    reference_limitations = {}
    physical_compared = []
    boundary_compared = []
    with tempfile.TemporaryDirectory(prefix="parquet-file-zero-") as directory:
        root = Path(directory)
        incoming, native, exported, foreign, foreign_export = (root/name for name in ("incoming", "native", "exported", "foreign", "foreign-export"))
        incoming.mkdir()
        foreign.mkdir()
        for name, table in cases.items():
            with ipc.new_file(str(incoming/(name+".arrow")), table.schema) as writer:
                writer.write_table(table)
        subprocess.run([str(args.writer), "--nested-from-arrow", str(incoming), str(native)], check=True)
        subprocess.run([str(args.reader), str(native), "--arrow-output", str(exported)], check=True)
        for name, table in cases.items():
            actual = read_stream(exported/(name+".parquet.arrows"))
            assert actual.to_pydict() == table.to_pydict(), (name, actual.to_pydict(), table.to_pydict())
            assert file_infos(actual.schema) == file_infos(table.schema), name
            path = native/(name+".parquet")
            _, parsed = footer(path)
            assert parsed.value[3].value == table.num_rows
            for group in parsed.value[4].value:
                assert group.value[3].value == table.num_rows
            try:
                physical = physical_copy(path, root/(name+"-physical.parquet"))
            except (OSError, pa.ArrowException) as error:
                reference_limitations[name] = str(error)
            else:
                if physical.to_pydict() == table.to_pydict():
                    physical_compared.append(name)
                else:
                    reference_limitations[name] = {"expected": table.to_pydict(), "physicalPyArrow": physical.to_pydict()}
        # These baseline cases independently pin the no-data, null-value and
        # anchored-row interpretation, regardless of container limitations.
        assert {"root-nulls", "root-empty-optional", "root-empty-required", "root-with-anchor"} <= set(physical_compared), reference_limitations
        boundary_expected = {
            "optional-parent-first": {"f": [None, {"anchor": 10}, {"anchor": 20}]},
            "optional-parent-last": {"f": [None, {"anchor": 10}, {"anchor": 20}]},
            "list-anchored": {"f": [None, [], [{"anchor": value} for value in range(5)]]},
        }
        for name, expected in boundary_expected.items():
            actual = project_physical_fields(native/(name+".parquet"), root/(name+"-projected.parquet"))
            assert actual.to_pydict() == expected, (name, actual.to_pydict(), expected)
            boundary_compared.append(name)
        # Independently frame a positive-row FILE with no physical columns.
        for repetition in (0, 1):
            schema = Node(T.LIST, [element("schema", children=1), element("f", repetition=repetition, children=0, file=True)], T.STRUCT)
            group = node(T.STRUCT, {1: Node(T.LIST, [], T.STRUCT), 2: node(T.I64, 0), 3: node(T.I64, 3)})
            parsed = node(T.STRUCT, {1: node(T.I32, 1), 2: schema, 3: node(T.I64, 3), 4: Node(T.LIST, [group], T.STRUCT)})
            write_footer(foreign/(f"foreign-{repetition}.parquet"), b"PAR1", parsed)
        subprocess.run([str(args.reader), str(foreign), "--arrow-output", str(foreign_export)], check=True)
        for path in foreign.glob("*.parquet"):
            actual = read_stream(foreign_export/(path.name+".arrows"))
            assert actual.to_pydict() == {"f": [None]*3}
            assert file_infos(actual.schema)
        # Keep the independent Rust failure explicit: treating a dropped field
        # as successful FILE interoperability would hide the very regression.
        rust_input = root/"rust-input"
        rust_input.mkdir()
        (rust_input/"nulls.arrow").write_bytes((incoming/"root-nulls.arrow").read_bytes())
        result = subprocess.run([str(args.reference), "from-arrow", str(rust_input), str(root/"rust")], capture_output=True, text=True)
        assert result.returncode != 0 and "does not support writing empty structs" in result.stderr
    report = {"status": "passed", "nativeRoundtrips": len(cases), "independentForeignFiles": 2,
              "physicalPyArrowComparisons": physical_compared, "physicalReferenceLimitations": reference_limitations,
              "independentParentListBoundaryComparisons": boundary_compared,
              "rustWriting": "rejects empty structs; not a zero-leaf FILE oracle",
              "sourceOracles": "Apache Thrift FILE group0/row counts; PyArrow physical empty groups; published FILE invalid-reference null policy"}
    if args.output:
        args.output.write_text(json.dumps(report, indent=2)+"\n")
    print(json.dumps(report, indent=2))


if __name__ == "__main__":
    main()
