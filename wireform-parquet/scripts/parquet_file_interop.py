#!/usr/bin/env python3
"""Independently frame FILE logical groups using Apache Thrift and PyArrow."""
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
from parquet_lzo_interop import Node, decode, encode


FIELD_TYPES = {
    "uri": pa.string(), "offset": pa.int64(), "size": pa.int64(),
    "content_type": pa.string(), "checksum": pa.string(), "inline": pa.binary(),
}
URI = "../images/a%20b.png?raw=1#part"


def footer(path):
    data = path.read_bytes()
    length = struct.unpack("<I", data[-8:-4])[0]
    parsed = decode(data[-8-length:-8])
    assert parsed.consumed == length
    return data[:-8-length], parsed.node


def set_file_annotation(path):
    prefix, metadata = footer(path)
    elements = metadata.value[2].value
    group = elements[1]
    assert group.value[4].value == b"f" and 1 not in group.value
    group.value[10] = Node(TType.STRUCT, {19: Node(TType.STRUCT, {})})
    raw = encode(metadata)
    path.write_bytes(prefix + raw + struct.pack("<I", len(raw)) + b"PAR1")


def check_annotations(path, expected_ids):
    _, metadata = footer(path)
    elements = metadata.value[2].value
    groups = [e for e in elements if 10 in e.value and 19 in e.value[10].value]
    assert len(groups) == 1, path
    assert groups[0].value[10].value[19].value == {}, path
    fields = {e.value[4].value.decode(): e for e in elements if 1 in e.value}
    for name, field_id in expected_ids.items():
        element = fields[name]
        assert element.value[3].value == 1, (path, name, "not OPTIONAL")
        assert (element.value[9].value if 9 in element.value else None) == field_id


def physical_copy(source, output):
    """PyArrow 26 rejects FILE; remove only that annotation for its value oracle.

    The original annotated files are independently inspected with Apache Thrift
    and decoded by the native reader. This is not a PyArrow FILE semantic test.
    """
    prefix, metadata = footer(source)
    for element in metadata.value[2].value:
        annotation = element.value.get(10)
        if annotation is not None and 19 in annotation.value:
            assert len(annotation.value) == 1
            del element.value[10]
    raw = encode(metadata)
    output.write_bytes(prefix + raw + struct.pack("<I", len(raw)) + b"PAR1")
    assert output.read_bytes()[:len(prefix)] == prefix
    return pq.read_table(output)


def read_export(path):
    with ipc.open_stream(str(path)) as reader:
        return reader.read_all()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--writer", required=True, type=Path)
    parser.add_argument("--reader", required=True, type=Path)
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    names = list(FIELD_TYPES)
    with tempfile.TemporaryDirectory(prefix="parquet-file-") as directory:
        root = Path(directory)
        native, foreign, exported = (root / name for name in ("native", "foreign", "exported"))
        foreign.mkdir()
        physical = root / "physical"
        physical.mkdir()
        subprocess.run([str(args.writer), "--file", str(native)], check=True)
        full_rows = [None, {"inline": b""}, {"uri": URI},
                     {"uri": URI, "offset": 0, "size": len(b"whole inline payload"), "inline": b"whole inline payload"},
                     {"uri": URI, "offset": 2**63-1, "size": 2**63-1},
                     {"inline": b"", "uri": "", "content_type": "", "checksum": ""}]
        def complete(rows, fields):
            return [None if row is None else {name: row.get(name) for name in fields} for row in rows]
        expected = {
            "full": {"f": complete(full_rows, names)},
            "reordered": {"f": complete(full_rows, list(reversed(names)))},
            "inline": {"f": [None, {"inline": b""}, {"inline": b"bytes"}]},
            "uri": {"f": [None, {"uri": URI}, {"uri": "uri.parquet"}]},
            "metadata-only": {"f": [None, None]},
            "list": {"files": [None, [], [None, complete([{"uri": URI}], names)[0], complete([{"inline": b""}], names)[0]]]},
        }
        for name, values in expected.items():
            path = native / (name + ".parquet")
            ids = {field: index + 10 for index, field in enumerate(names)} if name in ("full", "reordered", "list") else {field: None for field in next(iter(values.values()))[-1] or []}
            if name == "metadata-only": ids = {"content_type": None}
            check_annotations(path, ids)
            table = physical_copy(path, physical / (name + ".parquet"))
            assert table.to_pydict() == values, (name, table.to_pydict(), values)
        subprocess.run([str(args.reader), str(native), "--arrow-output", str(root / "native-arrow")], check=True)
        for name, values in expected.items():
            assert read_export(root / "native-arrow" / (name + ".parquet.arrows")).to_pydict() == values, name
        expected_foreign = {}
        # Every nonempty field subset. Reverse selected names to ensure the
        # reader cannot interpret a field by its fixed ordinal position.
        for mask in range(1, 64):
            selected = [name for index, name in enumerate(names) if mask & (1 << index)]
            selected.reverse()
            fields = [pa.field(name, FIELD_TYPES[name], nullable=True,
                               metadata={b"PARQUET:field_id": str(names.index(name) + 40).encode()}) for name in selected]
            value = {}
            if "inline" in selected: value["inline"] = b""
            if "uri" in selected:
                value["uri"] = URI
                if "size" in selected: value["size"] = 0
                if "offset" in selected and "size" in selected: value["offset"] = 0
            if "content_type" in selected: value["content_type"] = "application/octet-stream"
            if "checksum" in selected: value["checksum"] = "future:opaque:a"
            rows = [None, value if ("inline" in selected or "uri" in selected) else None, None]
            typ = pa.struct(fields)
            table = pa.Table.from_arrays([pa.array(rows, typ)], names=["f"])
            path = foreign / (f"subset-{mask:02}.parquet")
            pq.write_table(table, path, compression=("NONE", "ZSTD", "SNAPPY")[mask % 3],
                           data_page_version="2.0" if mask % 2 else "1.0", use_dictionary=bool(mask % 2),
                           store_schema=False, write_page_checksum=True)
            set_file_annotation(path)
            check_annotations(path, {name: names.index(name) + 40 for name in selected})
            assert physical_copy(path, physical / path.name).to_pydict() == table.to_pydict()
            expected_foreign[path.stem] = table.to_pydict()
        subprocess.run([str(args.writer), "--file-check", str(foreign)], check=True)
        subprocess.run([str(args.reader), str(foreign), "--arrow-output", str(exported)], check=True)
        for name, values in expected_foreign.items():
            actual = read_export(exported / (name + ".parquet.arrows")).to_pydict()
            assert actual == values, (name, actual, values)
        # A recognized FILE annotation must not hide malformed child layouts.
        invalid = root / "invalid"
        invalid.mkdir()
        template = foreign / "subset-01.parquet"
        prefix, metadata = footer(template)
        metadata.value[2].value[2].value[4] = Node(TType.STRING, b"URI")
        raw = encode(metadata)
        (invalid / "wrong-case.parquet").write_bytes(prefix + raw + struct.pack("<I", len(raw)) + b"PAR1")
        result = subprocess.run([str(args.reader), str(invalid)], capture_output=True, text=True)
        assert result.returncode != 0 and "FILE field" in result.stdout + result.stderr
        try:
            pq.read_table(native / "full.parquet")
        except OSError as error:
            assert "Logical type Undefined cannot be applied to group node" in str(error)
            reference_status = "PyArrow rejects unknown FILE tag; only unannotated physical values compared"
        else:
            reference_status = "PyArrow reads annotated FILE values"
    report = {"status": "passed", "nativeFiles": 6, "foreignSubsets": 63,
              "annotationOracle": "independent Apache Thrift tag19, exact names, OPTIONAL children and IDs",
              "valueOracle": "PyArrow physical storage with only FILE annotation removed",
              "nativeReader": "annotated FILE values and typed resolution",
              "pyarrowVersion": pa.__version__, "pyarrowFileSupport": reference_status,
              "emptyFieldSubset": "not covered: zero-leaf group support remains a separate release gate"}
    if args.output: args.output.write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(report, indent=2))


if __name__ == "__main__":
    main()
