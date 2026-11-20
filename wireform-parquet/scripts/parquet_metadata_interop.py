#!/usr/bin/env python3
"""Check file metadata with independent Apache Thrift and PyArrow codecs."""
import argparse
from dataclasses import dataclass
from pathlib import Path
import struct
import subprocess
import tempfile

import pyarrow as pa
import pyarrow.parquet as pq
from thrift.Thrift import TType
from parquet_lzo_interop import Node, decode, encode


@dataclass
class Entry:
    key: bytes
    value: bytes | None


@dataclass
class ParsedFile:
    prefix: bytes
    footer: Node


def parse(path):
    data = path.read_bytes()
    assert data[:4] == data[-4:] == b"PAR1"
    length = struct.unpack("<I", data[-8:-4])[0]
    result = decode(data[-8-length:-8])
    assert result.consumed == length
    return ParsedFile(data[:-8-length], result.node)


def metadata(path):
    footer = parse(path).footer
    if 5 not in footer.value:
        return None
    field = footer.value[5]
    assert field.kind == TType.LIST and field.element_kind == TType.STRUCT
    result = []
    for item in field.value:
        assert item.kind == TType.STRUCT and item.value[1].kind == TType.STRING
        value = item.value.get(2)
        assert value is None or value.kind == TType.STRING
        result.append(Entry(item.value[1].value, value.value if value is not None else None))
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--probe", required=True, type=Path)
    args = parser.parse_args()
    with tempfile.TemporaryDirectory(prefix="parquet-metadata-") as directory:
        root = Path(directory)
        source, copied = root / "source", root / "copied"
        source.mkdir()
        subprocess.run([str(args.probe), "--metadata", str(source)], check=True)
        expected = [Entry(b"same", None), Entry(b"same", b""), Entry(b"same", b"last"),
                    Entry("κλειδί".encode(), "世界".encode()), Entry(b"opaque", b"\xff\x00\x80")]
        assert metadata(source / "absent.parquet") is None
        assert metadata(source / "empty.parquet") == []
        assert metadata(source / "entries.parquet") == expected
        schema = pa.schema([pa.field("x", pa.int32())], metadata={b"custom": b"\xff\x00", "name".encode(): "世界".encode()})
        table = pa.Table.from_arrays([pa.array([-2**31, 0, 2**31-1], pa.int32())], schema=schema)
        foreign = source / "foreign.parquet"
        pq.write_table(table, foreign, compression="NONE", write_page_index=False, write_page_checksum=True)
        assert any(entry.key == b"ARROW:schema" for entry in metadata(foreign))
        parsed = parse(foreign)
        fields = parsed.footer.value
        fields[5].value.extend([
            Node(TType.STRUCT, {1: Node(TType.STRING, b"duplicate")}),
            Node(TType.STRUCT, {1: Node(TType.STRING, b"duplicate"), 2: Node(TType.STRING, b"")}),
            Node(TType.STRUCT, {1: Node(TType.STRING, b"duplicate"), 2: Node(TType.STRING, b"last")})])
        raw = encode(parsed.footer)
        (source / "foreign-duplicate.parquet").write_bytes(parsed.prefix + raw + struct.pack("<I", len(raw)) + b"PAR1")
        files = sorted(source.glob("*.parquet"))
        assert len(files) == 5
        before = {path.name: metadata(path) for path in files}
        subprocess.run([str(args.probe), "--copy-footers", str(source), str(copied)], check=True)
        assert {p.name for p in copied.glob("*.parquet")} == set(before)
        for path in files:
            assert pq.read_table(path).column("x").to_pylist() == [-2**31, 0, 2**31-1], path.name
            output = copied / path.name
            assert metadata(output) == before[path.name], path.name
            assert pq.read_table(output).column("x").to_pylist() == [-2**31, 0, 2**31-1], path.name
    print("PASS: 3 native metadata states and5 footer copies; Apache Thrift +PyArrow values,order,duplicates,absent/empty/opaquevalues andARROW:schema")


if __name__ == "__main__":
    main()
