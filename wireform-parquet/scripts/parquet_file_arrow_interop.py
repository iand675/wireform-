#!/usr/bin/env python3
"""FILE extension interoperability with a pinned upstream arrow-rs reader/writer.

The reference must enable parquet/file_experimental. PyArrow is used only for
Arrow IPC fixture construction and comparison, not to interpret Parquet FILE.
"""
import argparse
import hashlib
import json
from pathlib import Path
import subprocess
import shutil
import tempfile

import pyarrow as pa
import pyarrow.ipc as ipc
from parquet_file_interop import check_annotations, footer


NAMES = ("uri", "offset", "size", "content_type", "checksum", "inline")
EXTENSION = b"arrow.parquet.file"


def run(*args):
    subprocess.run(list(map(str, args)), check=True)


def file_info(field):
    metadata = field.metadata or {}
    assert metadata[b"ARROW:extension:name"] == EXTENSION, field
    assert metadata.get(b"ARROW:extension:metadata", b"") == b"", field
    ident = metadata.get(b"PARQUET:field_id")
    children = {child.name: (child.metadata or {}).get(b"PARQUET:field_id") for child in field.type}
    return ident, children


def file_infos(schema):
    result = {}
    def visit(field, path):
        if (field.metadata or {}).get(b"ARROW:extension:name") == EXTENSION:
            result[path] = file_info(field)
        if pa.types.is_struct(field.type):
            for child in field.type:
                visit(child, path + "." + child.name)
        elif pa.types.is_list(field.type):
            visit(field.type.value_field, path + "." + field.type.value_field.name)
    for field in schema:
        visit(field, field.name)
    return result


def check_file(path, schema):
    info = file_infos(schema)
    assert len(info) == 1
    parent_id, children = next(iter(info.values()))
    check_annotations(path, {name: int(ident) for name, ident in children.items()})
    _, metadata = footer(path)
    groups = [element for element in metadata.value[2].value if 10 in element.value and 19 in element.value[10].value]
    assert len(groups) == 1 and groups[0].value[9].value == int(parent_id)


def read_file(path):
    with ipc.open_file(str(path)) as reader:
        return reader.read_all()


def read_stream(path):
    with ipc.open_stream(str(path)) as reader:
        return reader.read_all()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--writer", required=True, type=Path)
    parser.add_argument("--reader", required=True, type=Path)
    parser.add_argument("--reference", required=True, type=Path)
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    profiles = {
        "ordinary": (pa.string(), pa.binary()),
        "large": (pa.large_string(), pa.large_binary()),
        "view": (pa.string_view(), pa.binary_view()),
    }
    with tempfile.TemporaryDirectory(prefix="parquet-file-arrow-") as directory:
        root = Path(directory)
        incoming = root / "incoming"
        incoming.mkdir()
        expected = {}
        for profile, (string_type, binary_type) in profiles.items():
            for mask in range(1, 64):
                selected = [name for index, name in enumerate(NAMES) if mask & (1 << index)]
                selected.reverse()
                fields = []
                for name in selected:
                    ty = binary_type if name == "inline" else pa.int64() if name in ("offset", "size") else string_type
                    fields.append(pa.field(name, ty, nullable=True, metadata={b"PARQUET:field_id": str(NAMES.index(name) + 21).encode()}))
                metadata = {b"ARROW:extension:name": EXTENSION, b"PARQUET:field_id": b"-91"}
                if mask % 2:
                    metadata[b"ARROW:extension:metadata"] = b""
                field = pa.field("f", pa.struct(fields), nullable=True, metadata=metadata)
                rows = [None]
                for index in range(3):
                    if "uri" not in selected and "inline" not in selected:
                        rows.append(None)
                        continue
                    value = {}
                    payload = b"" if index == 0 else bytes(range(80))
                    if "inline" in selected:
                        value["inline"] = payload
                    has_uri = "uri" in selected and not ("inline" in selected and index == 2)
                    if "uri" in selected:
                        value["uri"] = "../assets/a%20b-long-name.bin?raw=1#part" if has_uri else ""
                    if has_uri:
                        if "size" in selected:
                            value["size"] = len(payload) if "inline" in selected else index * 17
                        if "offset" in selected and "size" in selected:
                            value["offset"] = 0 if index == 0 else 2**63-1
                    if "content_type" in selected:
                        value["content_type"] = "" if index == 0 else "application/octet-stream"
                    if "checksum" in selected:
                        value["checksum"] = "MD5:" + hashlib.md5(payload).hexdigest() if "inline" in selected else "ETAG:opaque:token"
                    rows.append(value)
                table = pa.Table.from_arrays([pa.array(rows, field.type)], schema=pa.schema([field]))
                name = f"{profile}-{mask:02}"
                expected[name] = table
                with ipc.new_file(str(incoming / (name + ".arrow")), table.schema) as writer:
                    for batch in table.to_batches(max_chunksize=2):
                        writer.write_batch(batch)
        full = expected["view-63"]
        values = full.column(0).to_pylist()
        element = full.schema.field(0).with_name("element")
        list_field = pa.field("files", pa.list_(element))
        struct_field = pa.field("wrapper", pa.struct([full.schema.field(0), pa.field("side", pa.int32())]))
        for name, field, rows in (
            ("nested-list", list_field, [None, [], [None, values[1]], [values[2], values[3]]]),
            ("nested-struct", struct_field, [None, {"f": values[1], "side": 7}, {"f": None, "side": 9}, {"f": values[3], "side": None}]),
        ):
            table = pa.Table.from_arrays([pa.array(rows, field.type)], schema=pa.schema([field]))
            expected[name] = table
            with ipc.new_file(str(incoming / (name + ".arrow")), table.schema) as writer:
                for batch in table.to_batches(max_chunksize=2):
                    writer.write_batch(batch)
        native, rust, rust_arrow, native_arrow = (root / name for name in ("native", "rust", "rust-arrow", "native-arrow"))
        run(args.writer, "--nested-from-arrow", incoming, native)
        run(args.reference, "from-arrow", incoming, rust)
        run(args.reference, "to-arrow", native, rust_arrow)
        run(args.reader, rust, "--arrow-output", native_arrow)
        for source in (native, rust):
            flat = root / (source.name + "-flat")
            flat.mkdir()
            for path in source.glob("*.parquet"):
                if not path.name.startswith("nested-"):
                    shutil.copyfile(path, flat / path.name)
            run(args.writer, "--file-check", flat)
        for name, table in expected.items():
            for folder in (native, rust):
                check_file(folder / (name + ".parquet"), table.schema)
            for actual in (read_file(rust_arrow / (name + ".arrow")), read_stream(native_arrow / (name + ".parquet.arrows"))):
                assert actual.to_pydict() == table.to_pydict(), name
                assert file_infos(actual.schema) == file_infos(table.schema), name
        # Exercise inferred FILE metadata as input to both writers, including
        # child/group IDs rather than relying on the original embedded schema.
        restored_native, restored_rust = root / "restored-native", root / "restored-rust"
        run(args.writer, "--nested-from-arrow", rust_arrow, restored_native)
        run(args.reference, "from-stream", native_arrow, restored_rust)
        for name, table in expected.items():
            for folder in (restored_native, restored_rust):
                check_file(folder / (name + ".parquet"), table.schema)
        run(args.reader, restored_rust, "--arrow-output", root / "restored-arrow")
        run(args.reference, "to-arrow", restored_native, root / "restored-rust-arrow")
        for name, table in expected.items():
            for actual in (read_stream(root / "restored-arrow" / (name + ".parquet.arrows")), read_file(root / "restored-rust-arrow" / (name + ".arrow"))):
                assert actual.to_pydict() == table.to_pydict(), name
                assert file_infos(actual.schema) == file_infos(table.schema), name
        # Nonempty future extension metadata must not silently become a plain
        # struct in the native writer.
        invalid = root / "invalid"
        invalid.mkdir()
        table = expected["ordinary-01"]
        field = table.schema.field(0).with_metadata({b"ARROW:extension:name": EXTENSION, b"ARROW:extension:metadata": b"v2"})
        table = pa.Table.from_arrays(table.columns, schema=pa.schema([field]))
        with ipc.new_file(str(invalid / "invalid.arrow"), table.schema) as writer:
            writer.write_table(table)
        result = subprocess.run([str(args.writer), "--nested-from-arrow", str(invalid), str(root / "rejected")], capture_output=True, text=True)
        assert result.returncode != 0 and "FILE extension" in result.stdout + result.stderr
    report = {"status": "passed", "fieldSubsets": 63, "storageProfiles": list(profiles), "flatInputs": 189, "nestedInputs": 2, "inputFiles": len(expected),
              "crossReaderComparisons": len(expected) * 4, "annotationChecks": len(expected) * 4,
              "reference": "Pinned upstream arrow-rs with file_experimental; skips embedded Arrow metadata on read",
              "referenceBinarySHA256": hashlib.sha256(args.reference.read_bytes()).hexdigest(),
              "covered": "FILE identity, empty metadata, optional named fields, child/group IDs, values/nulls, multi-batch IPC, ordinary/large/view arrays, both readers/writers and roundtrips"}
    if args.output:
        args.output.write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(report, indent=2))


if __name__ == "__main__":
    main()
