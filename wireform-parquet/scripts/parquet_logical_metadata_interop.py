#!/usr/bin/env python3
"""Check geospatial metadata with Apache Thrift and PyArrow independently.

The writer must preserve CRS/interpolation fields, including their absence,
while retaining valid WKB pages and omitting unordered min/max statistics.
"""

import argparse
import struct
import subprocess
import tempfile
from pathlib import Path

import pyarrow.parquet as pq
from thrift.Thrift import TType
from thrift.protocol.TCompactProtocol import TCompactProtocol
from thrift.transport.TTransport import TMemoryBuffer

from parquet_statistics_interop import read_value


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--probe", type=Path, required=True)
    args = parser.parse_args()
    cases = {
        "geometry-default": (17, {}),
        "geometry-custom": (17, {1: b"EPSG:3857"}),
        "geography-default": (18, {}),
    }
    for code, name in enumerate(("spherical", "vincenty", "thomas", "andoyer", "karney")):
        cases["geography-" + name] = (18, {1: b"EPSG:4326", 2: code})
    point = struct.pack("<BIdd", 1, 1, 1.5, 2.5)
    with tempfile.TemporaryDirectory(prefix="parquet-logical-metadata-") as directory:
        root = Path(directory)
        subprocess.run([str(args.probe.resolve()), "--logical-metadata", str(root)], check=True)
        assert {p.stem for p in root.glob("*.parquet")} == set(cases)
        for name, (tag, parameters) in cases.items():
            path = root / (name + ".parquet")
            data = path.read_bytes()
            assert data[:4] == data[-4:] == b"PAR1"
            length = struct.unpack("<I", data[-8:-4])[0]
            protocol = TCompactProtocol(TMemoryBuffer(data[-8 - length:-8]))
            footer = read_value(protocol, TType.STRUCT)
            assert footer[2][1][10] == {tag: parameters}, (name, footer[2][1])
            statistics = footer[4][0][1][0][3].get(12, {})
            assert not {1, 2, 5, 6}.intersection(statistics), (name, statistics)
            metadata = pq.read_metadata(path)
            kind = "GEOMETRY" if tag == 17 else "GEOGRAPHY"
            assert kind in str(metadata.schema.column(0).logical_type).upper()
            assert pq.read_table(path, arrow_extensions_enabled=False).to_pylist() == [{"geometry": point}]
    print(f"PASS: {len(cases)} geospatial metadata cases (Apache Thrift + PyArrow)")


if __name__ == "__main__":
    main()
