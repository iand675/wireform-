#!/usr/bin/env python3
"""Regenerate/check wire goldens using Apache's Python thrift package (0.25.0).

Run from the package directory: python scripts/protocol_golden.py --check
Use --write after intentionally changing fixtures. The Haskell suite consumes
these independent bytes without requiring Python at test time.
"""
import argparse
from importlib.metadata import version
from pathlib import Path
from uuid import UUID

from thrift.Thrift import TType
from thrift.protocol import TBinaryProtocol, TCompactProtocol
from thrift.transport import TTransport


def struct(fields):
    return TType.STRUCT, fields


def sequence(kind, element_type, values):
    return kind, (element_type, values)


def fixtures():
    yield "scalars", struct([
        (1, (TType.BOOL, False)), (2, (TType.BOOL, True)),
        (3, (TType.BYTE, -128)), (4, (TType.I16, -32768)),
        (5, (TType.I32, -2147483648)), (6, (TType.I64, -9223372036854775808)),
        (7, (TType.DOUBLE, -123.5)), (8, (TType.STRING, "é雪\0")),
        (9, (TType.STRING, bytes([0, 255, 128, 1]))),
        (10, (TType.UUID, UUID(bytes=bytes(range(16))))),
    ])
    yield "field-ids", struct([
        (32767, (TType.I32, 1)), (-32768, (TType.I32, 2)), (0, (TType.BOOL, False)),
    ])
    yield "container-booleans", struct([
        (1, sequence(TType.LIST, TType.BOOL, [False, True, False])),
        (2, sequence(TType.SET, TType.BOOL, [False, True])),
        (3, (TType.MAP, (TType.BOOL, TType.BOOL, [(False, True), (True, False)]))),
    ])
    for count in (0, 14, 15, 128):
        yield f"list-{count}", struct([
            (1, sequence(TType.LIST, TType.I32, list(range(-64, count - 64))))
        ])
    yield "nested", struct([
        (1, sequence(TType.LIST, TType.STRUCT, [
            [(1, (TType.STRING, "first")), (2, (TType.BOOL, False))],
            [(1, (TType.STRING, "雪")), (2, (TType.BOOL, True))],
        ])),
        (2, (TType.MAP, (TType.STRING, TType.LIST, [
            ("a", (TType.BOOL, [False, True])), ("é", (TType.BOOL, [])),
        ]))),
        (3, (TType.MAP, (TType.STOP, TType.STOP, []))),
    ])


def write(protocol, typ, value):
    if typ == TType.STRUCT:
        protocol.writeStructBegin("fixture")
        for fid, (field_type, field_value) in sorted(value):
            protocol.writeFieldBegin("field", field_type, fid)
            write(protocol, field_type, field_value)
            protocol.writeFieldEnd()
        protocol.writeFieldStop()
        protocol.writeStructEnd()
    elif typ in (TType.LIST, TType.SET):
        element_type, values = value
        begin = protocol.writeListBegin if typ == TType.LIST else protocol.writeSetBegin
        end = protocol.writeListEnd if typ == TType.LIST else protocol.writeSetEnd
        begin(element_type, len(values))
        for element in values:
            write(protocol, element_type, element)
        end()
    elif typ == TType.MAP:
        kt, vt, entries = value
        protocol.writeMapBegin(kt, vt, len(entries))
        for key, element in entries:
            write(protocol, kt, key)
            write(protocol, vt, element)
        protocol.writeMapEnd()
    elif typ == TType.STRING:
        if isinstance(value, str):
            protocol.writeString(value)
        else:
            protocol.writeBinary(value)
    else:
        method = {
            TType.BOOL: protocol.writeBool, TType.BYTE: protocol.writeByte,
            TType.I16: protocol.writeI16, TType.I32: protocol.writeI32,
            TType.I64: protocol.writeI64, TType.DOUBLE: protocol.writeDouble,
            TType.UUID: protocol.writeUuid,
        }[typ]
        method(value)


def rendered():
    lines = ["# Apache Python thrift 0.25.0; regenerate with scripts/protocol_golden.py --write",
             "# fixture\tbinary hex\tcompact hex"]
    for name, (typ, value) in fixtures():
        encodings = []
        for implementation in (TBinaryProtocol.TBinaryProtocol, TCompactProtocol.TCompactProtocol):
            transport = TTransport.TMemoryBuffer()
            write(implementation(transport), typ, value)
            encodings.append(transport.getvalue().hex())
        lines.append("\t".join([name, *encodings]))
    return "\n".join(lines) + "\n"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument("--check", action="store_true")
    mode.add_argument("--write", action="store_true")
    args = parser.parse_args()
    path = Path(__file__).resolve().parents[1] / "test/fixtures/protocol-encodings.tsv"
    if version("thrift") != "0.25.0":
        raise SystemExit("Install Apache Python thrift==0.25.0 to reproduce these fixtures")
    expected = rendered()
    if args.write:
        path.write_text(expected)
    elif path.read_text() != expected:
        raise SystemExit("Apache Thrift protocol fixtures differ; inspect before regenerating")
    print("PASS: 8 fixtures x Binary/Compact match Apache Thrift")


if __name__ == "__main__":
    main()
