#!/usr/bin/env python3
"""Independent raw-LZO Parquet interop using Apache Thrift, liblzo2 and PyArrow.

The codec is unavailable in ordinary PyArrow builds. Transcode only page
compression with the independent reference library, preserving logical data,
levels, encodings and statistics. PyArrow then supplies the value oracle.
"""
import argparse
from dataclasses import dataclass
from pathlib import Path
import struct
import subprocess
import tempfile
import zlib

import pyarrow as pa
import pyarrow.ipc as ipc
import pyarrow.parquet as pq
from thrift.Thrift import TType
from thrift.protocol.TCompactProtocol import TCompactProtocol
from thrift.transport.TTransport import TMemoryBuffer


@dataclass
class Node:
    kind: int
    value: object
    element_kind: int = 0
    key_kind: int = 0
    value_kind: int = 0


@dataclass
class Entry:
    key: Node
    value: Node


@dataclass
class Decoded:
    node: Node
    consumed: int


def read_node(protocol, kind):
    if kind == TType.STRUCT:
        protocol.readStructBegin()
        fields = {}
        while True:
            _, field_kind, field_id = protocol.readFieldBegin()
            if field_kind == TType.STOP:
                break
            assert field_id not in fields, "duplicate metadata field"
            fields[field_id] = read_node(protocol, field_kind)
            protocol.readFieldEnd()
        protocol.readStructEnd()
        return Node(kind, fields)
    if kind in (TType.LIST, TType.SET):
        element, count = (protocol.readListBegin() if kind == TType.LIST else protocol.readSetBegin())
        items = []
        for _ in range(count):
            items.append(read_node(protocol, element))
        (protocol.readListEnd if kind == TType.LIST else protocol.readSetEnd)()
        return Node(kind, items, element_kind=element)
    if kind == TType.MAP:
        key, value, count = protocol.readMapBegin()
        entries = []
        for _ in range(count):
            entries.append(Entry(read_node(protocol, key), read_node(protocol, value)))
        protocol.readMapEnd()
        return Node(kind, entries, key_kind=key, value_kind=value)
    readers = {TType.BOOL: protocol.readBool, TType.BYTE: protocol.readByte,
               TType.I16: protocol.readI16, TType.I32: protocol.readI32,
               TType.I64: protocol.readI64, TType.DOUBLE: protocol.readDouble,
               TType.STRING: protocol.readBinary}
    return Node(kind, readers[kind]())


def write_node(protocol, node):
    if node.kind == TType.STRUCT:
        protocol.writeStructBegin("metadata")
        for field_id, value in node.value.items():
            protocol.writeFieldBegin("field", value.kind, field_id)
            write_node(protocol, value)
            protocol.writeFieldEnd()
        protocol.writeFieldStop()
        protocol.writeStructEnd()
    elif node.kind in (TType.LIST, TType.SET):
        (protocol.writeListBegin if node.kind == TType.LIST else protocol.writeSetBegin)(node.element_kind, len(node.value))
        for item in node.value:
            write_node(protocol, item)
        (protocol.writeListEnd if node.kind == TType.LIST else protocol.writeSetEnd)()
    elif node.kind == TType.MAP:
        protocol.writeMapBegin(node.key_kind, node.value_kind, len(node.value))
        for item in node.value:
            write_node(protocol, item.key)
            write_node(protocol, item.value)
        protocol.writeMapEnd()
    else:
        writers = {TType.BOOL: protocol.writeBool, TType.BYTE: protocol.writeByte,
                   TType.I16: protocol.writeI16, TType.I32: protocol.writeI32,
                   TType.I64: protocol.writeI64, TType.DOUBLE: protocol.writeDouble,
                   TType.STRING: protocol.writeBinary}
        writers[node.kind](node.value)


def decode(data):
    transport = TMemoryBuffer(data)
    node = read_node(TCompactProtocol(transport), TType.STRUCT)
    return Decoded(node, transport.cstringio_buf.tell())


def encode(node):
    transport = TMemoryBuffer()
    write_node(TCompactProtocol(transport), node)
    return transport.getvalue()


def signed_crc(body):
    value = zlib.crc32(body)
    return value if value < 2**31 else value - 2**32


def transcode(source, target, codec, oracle, scratch, v2_uncompressed=False,
              require_source_codec=None, expected_version=None):
    data = source.read_bytes()
    assert data[:4] == data[-4:] == b"PAR1"
    length = struct.unpack("<I", data[-8:-4])[0]
    parsed = decode(data[-8-length:-8])
    footer = parsed.node
    assert parsed.consumed == length
    result = bytearray(b"PAR1")
    calls = 0

    codec_names = {1: "snappy", 2: "gzip", 4: "brotli", 6: "zstd", 7: "lz4_raw"}

    def convert(body, old_codec, expected, output_codec=codec):
        nonlocal calls
        if old_codec == 0 or (expected == 0 and not body):
            raw = body
        elif old_codec == 3:
            src, dst = scratch / "compressed", scratch / "plain"
            src.write_bytes(body)
            subprocess.run([str(oracle), "--reference-decompress", str(expected), str(src), str(dst)], check=True)
            raw = dst.read_bytes()
            calls += 1
        else:
            raw = bytes(pa.Codec(codec_names[old_codec]).decompress(body, expected))
        assert len(raw) == expected
        if output_codec == 0:
            return raw
        if output_codec != 3:
            return bytes(pa.Codec(codec_names[output_codec]).compress(raw))
        src, dst = scratch / "plain", scratch / "compressed"
        src.write_bytes(raw)
        subprocess.run([str(oracle), "--reference-compress", str(src), str(dst)], check=True)
        calls += 1
        return dst.read_bytes()

    for group in footer.value[4].value:
        group_start = len(result)
        total_raw, total_packed = 0, 0
        for chunk in group.value[1].value:
            assert not {1, 4, 5, 6, 7}.intersection(chunk.value), "external chunks/indexes excluded from fixture"
            column = chunk.value[3].value
            old_codec = column[4].value
            assert old_codec in (0, 1, 2, 3, 4, 6, 7)
            if require_source_codec is not None and column[5].value > 0:
                assert old_codec == require_source_codec, (source, old_codec)
            offsets = [column[9].value]
            if 11 in column:
                offsets.append(column[11].value)
            # PyArrow's empty dictionary column has a real dictionary page
            # and a zero data-page offset because no data page was written.
            assert all(offset >= 4 or (offset == 0 and column[5].value == 0)
                       for offset in offsets), (source, offsets)
            start = min((offset for offset in offsets if offset >= 4), default=0)
            assert start != 0 or column[7].value == 0, (source, column)
            stop = start + column[7].value
            cursor = start
            new_start = len(result)
            raw_size = 0
            first_data, first_dictionary = None, None
            while cursor < stop:
                parsed = decode(data[cursor:stop])
                header, header_size = parsed.node, parsed.consumed
                fields = header.value
                assert {1, 2, 3}.issubset(fields), (source.name, start, stop, cursor, fields)
                kind, expected, size = fields[1].value, fields[2].value, fields[3].value
                if kind in (0, 3) and expected_version is not None:
                    values = fields[5 if kind == 0 else 8].value[1].value
                    if values > 0:
                        assert kind == (0 if expected_version == 1 else 3), (source, kind)
                body = data[cursor+header_size:cursor+header_size+size]
                assert len(body) == size
                if 4 in fields:
                    assert fields[4].value == signed_crc(body)
                if kind == 3:
                    page = fields[8].value
                    level_bytes = page[5].value + page[6].value
                    assert 0 <= level_bytes <= len(body)
                    values = body[level_bytes:]
                    previous = old_codec if page.get(7, Node(TType.BOOL, True)).value else 0
                    if codec != 0 and v2_uncompressed:
                        converted = convert(values, previous, expected-level_bytes, 0)
                        page[7] = Node(TType.BOOL, False)
                    else:
                        converted = convert(values, previous, expected-level_bytes)
                        page[7] = Node(TType.BOOL, codec != 0)
                    new_body = body[:level_bytes] + converted
                else:
                    assert kind in (0, 2), (source, kind)
                    new_body = convert(body, old_codec, expected)
                position = len(result)
                if kind == 2:
                    assert first_dictionary is None
                    first_dictionary = position
                elif first_data is None:
                    first_data = position
                fields[3] = Node(TType.I32, len(new_body))
                if 4 in fields:
                    fields[4] = Node(TType.I32, signed_crc(new_body))
                serialized = encode(header)
                result.extend(serialized)
                result.extend(new_body)
                raw_size += len(serialized) + expected
                cursor += header_size + size
            assert cursor == stop
            column[4] = Node(TType.I32, codec)
            column[6] = Node(TType.I64, raw_size)
            column[7] = Node(TType.I64, len(result)-new_start)
            column[9] = Node(TType.I64, first_data if first_data is not None else 0)
            if first_dictionary is not None:
                column[11] = Node(TType.I64, first_dictionary)
            else:
                column.pop(11, None)
            assert 10 not in column and 14 not in column, "index pages/bloom filters excluded from fixture"
            chunk.value[2] = Node(TType.I64, new_start)
            total_raw += raw_size
            total_packed += len(result)-new_start
        group.value[2] = Node(TType.I64, total_raw)
        if 5 in group.value:
            group.value[5] = Node(TType.I64, group_start)
        group.value[6] = Node(TType.I64, total_packed)
    serialized = encode(footer)
    result.extend(serialized)
    result.extend(struct.pack("<I", len(serialized)))
    result.extend(b"PAR1")
    target.write_bytes(result)
    return calls


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--writer", required=True, type=Path)
    parser.add_argument("--reader", required=True, type=Path)
    parser.add_argument("--oracle", required=True, type=Path)
    args = parser.parse_args()
    with tempfile.TemporaryDirectory(prefix="parquet-lzo-interop-") as directory:
        root = Path(directory)
        controls, incoming, outgoing, decoded, arrow, scratch = (root / name for name in ("controls", "incoming", "outgoing", "decoded", "arrow", "scratch"))
        for path in (controls, incoming, outgoing, decoded, arrow, scratch):
            path.mkdir()
        tables = {
            "scalars": pa.table({"i32": pa.array([-2**31,None,0,2**31-1,-1]*6,pa.int32()),
                                 "flag": pa.array([True,None,False,False,True]*6),
                                 "bytes": pa.array([b"a",None,b"",b"\x00\xff",b"abc"*30]*6,pa.binary())}),
            "nested": pa.table({"items": pa.array([None,[],[None,1,2],[3]]*6,pa.list_(pa.int32())),
                                "obj": pa.array([None,{"x":1,"s":"a"},{"x":None,"s":None},{"x":-3,"s":""}]*6,pa.struct([("x",pa.int64()),("s",pa.string())]))}),
            "allnull": pa.table({"x": pa.array([None]*16,pa.int32())}),
            "empty": pa.table({"x": pa.array([],pa.int32())}),
        }
        expected, calls = {}, 0
        for family, table in tables.items():
            for version in ("1.0", "2.0"):
                for dictionary in (False, True):
                    name = f"{family}-v{version}-dict{int(dictionary)}"
                    plain = controls / (name + ".parquet")
                    pq.write_table(table, plain, compression="NONE", data_page_version=version,
                                   use_dictionary=dictionary, row_group_size=11, max_rows_per_page=5,
                                   write_batch_size=5, write_page_index=False, write_page_checksum=True)
                    # First independently check metadata/header rewriting without codec work.
                    copy = decoded / (name + ".parquet")
                    transcode(plain, copy, 0, args.oracle, scratch, require_source_codec=0,
                              expected_version=1 if version == "1.0" else 2)
                    assert pq.read_table(copy).to_pylist() == table.to_pylist(), name
                    packed = incoming / (name + ".parquet")
                    calls += transcode(plain, packed, 3, args.oracle, scratch, require_source_codec=0)
                    expected[packed.stem] = table.to_pylist()
                    if version == "2.0":
                        variant = incoming / (name + "-raw-values.parquet")
                        calls += transcode(plain, variant, 3, args.oracle, scratch, v2_uncompressed=True)
                        expected[variant.stem] = table.to_pylist()
        subprocess.run([str(args.reader), str(incoming), "--arrow-output", str(arrow), "+RTS", "-M128m", "-RTS"], check=True)
        for name, rows in expected.items():
            with pa.memory_map(str(arrow / (name + ".parquet.arrows")), "r") as source:
                table = ipc.open_stream(source).read_all()
            assert table.to_pylist() == rows, name
        subprocess.run([str(args.writer), "--lzo", str(outgoing)], check=True)
        assert len(list(outgoing.glob("*.parquet"))) == 8
        for source in outgoing.glob("*.parquet"):
            target = decoded / ("native-" + source.name)
            calls += transcode(source, target, 0, args.oracle, scratch, require_source_codec=3,
                               expected_version=1 if source.stem.startswith("v1") else 2)
            table = pq.read_table(target)
            nullable = "nullable" in source.stem
            if "empty" in source.stem:
                rows = []
            elif "all-null" in source.stem:
                rows = [{"i32":None,"flag":None,"bytes":None}]*200
            else:
                integers = [-2**31,None,0,2**31-1,None] if nullable else [-2**31,-1,0,1,2**31-1]
                flags = [True,None,False,True,None] if nullable else [True,False,False,True,True]
                binary = [b"alpha",None,b"",b"\xff",None] if nullable else [b"alpha",b"",b"\x00",b"\xff",b"a"*128]
                rows = []
                for i in range(200):
                    rows.append({"i32":integers[i%5],"flag":flags[i%5],"bytes":binary[i%5]})
            assert table.to_pylist() == rows, source.name
        print(f"PASS: {len(expected)} independent LZO input files,8 native output files,{calls} liblzo2 page operations; V1/V2,dictionaries,nesting,null/empty,CRC andrawV2values")


if __name__ == "__main__":
    main()
