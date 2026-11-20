#!/usr/bin/env python3
"""Extract matched raw ALP benchmark pages from Apache's official fixture.

Both readers consume the same decompressed page bytes. Expected IEEE bytes
come from independently decoded PLAIN columns; extraction and validation are
outside measurements. No Python float conversion touches NaN payloads.
"""
import argparse
import hashlib
import json
from pathlib import Path
import struct

import pyarrow as pa
import pyarrow.parquet as pq
from parquet_statistics_interop import read_value, TCompactProtocol, TMemoryBuffer, TType


def header_at(raw, offset):
    transport = TMemoryBuffer(raw[offset:])
    header = read_value(TCompactProtocol(transport), TType.STRUCT)
    return header, offset + transport.cstringio_buf.tell()


def plain_bits(array, width):
    array = array.combine_chunks()
    data = memoryview(array.buffers()[1])
    output = bytearray()
    for row in range(len(array)):
        if array[row].is_valid:
            start = (array.offset + row) * width
            output.extend(data[start:start + width])
    return bytes(output)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--corpus", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args()
    source = args.corpus / "data/alp_extended.zstd.parquet"
    raw = source.read_bytes()
    reference = pq.ParquetFile(source)
    root = args.output_dir
    root.mkdir(parents=True, exist_ok=True)
    records, provenance = [], []
    for rg in range(reference.metadata.num_row_groups):
        plain = reference.read_row_group(rg, columns=["float_plain", "double_plain"])
        group = reference.metadata.row_group(rg)
        for col in range(group.num_columns):
            meta = group.column(col)
            name = meta.path_in_schema
            if "_alp_" not in name:
                continue
            kind, _, vector_size = name.split("_")
            width = 4 if kind == "float" else 8
            schema = reference.schema.column(col)
            assert schema.max_repetition_level == 0 and schema.max_definition_level in (0, 1)
            assert meta.compression in ("ZSTD", "UNCOMPRESSED") and not meta.has_dictionary_page
            offset, row, page = meta.data_page_offset, 0, 0
            while row < group.num_rows:
                header, body = header_at(raw, offset)
                assert header[1] == 0 and header[5][2] == 10, (rg, name, header)
                count = header[5][1]
                decoded = raw[body:body + header[3]]
                if meta.compression == "ZSTD":
                    decoded = bytes(pa.Codec("zstd").decompress(decoded, header[2]))
                assert len(decoded) == header[2]
                values_start = 0
                if schema.max_definition_level:
                    values_start = 4 + struct.unpack_from("<I", decoded)[0]
                payload = decoded[values_start:]
                expected = plain_bits(plain[kind + "_plain"].slice(row, count), width)
                nonnull = len(expected) // width
                assert len(payload) >= 7 and struct.unpack_from("<i", payload, 3)[0] == nonnull
                assert (1 << payload[2]) == int(vector_size)
                stem = f"{kind}-rg{rg}-v{vector_size}-p{page}"
                (root / (stem + ".alp")).write_bytes(payload)
                (root / (stem + ".plain")).write_bytes(expected)
                label = f"{kind.upper()} rg{rg} vector{vector_size} {nonnull} values"
                records.append(f"{stem}\t{width}\t{nonnull}\t{label}")
                provenance.append({"name": label, "file": stem, "width": width, "count": nonnull,
                                   "encodedSha256": hashlib.sha256(payload).hexdigest(),
                                   "plainSha256": hashlib.sha256(expected).hexdigest()})
                row += count
                page += 1
                offset = body + header[3]
            assert row == group.num_rows
    assert len(records) == 30, len(records)
    (root / "cases.tsv").write_text("\n".join(records) + "\n")
    (root / "provenance.json").write_text(json.dumps({"source": str(source),
        "sourceSha256": hashlib.sha256(raw).hexdigest(), "pyarrowVersion": pa.__version__,
        "oracle": "selective PLAIN decode; exact buffer bits from nonnull rows", "cases": provenance}, indent=2) + "\n")
    print("Extracted 30 official ALP pages and independent PLAIN bit oracles")


if __name__ == "__main__":
    main()
