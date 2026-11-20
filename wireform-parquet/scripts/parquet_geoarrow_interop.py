#!/usr/bin/env python3
"""Check Parquet geospatial/GeoArrow metadata and WKB values independently."""
import argparse
from dataclasses import dataclass
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


@dataclass
class Case:
    name: str
    crs: object
    edges: str | None = None
    hint: str | None = None
    large: bool = False


def arrow_metadata(field):
    metadata = field.metadata or {}
    assert metadata[b"ARROW:extension:name"] == b"geoarrow.wkb", field
    return json.loads(metadata[b"ARROW:extension:metadata"])


def logical(path):
    data = path.read_bytes()
    count = struct.unpack("<I", data[-8:-4])[0]
    parsed = decode(data[-8-count:-8])
    assert parsed.consumed == count
    return parsed.node.value[2].value[1].value[10].value


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--writer", type=Path, required=True)
    parser.add_argument("--reader", type=Path, required=True)
    args = parser.parse_args()
    projjson = {"type":"GeographicCRS","name":"WGS 84","datum":{"type":"GeodeticReferenceFrame","name":"World Geodetic System 1984","ellipsoid":{"name":"WGS 84","semi_major_axis":6378137,"inverse_flattening":298.257223563}},"coordinate_system":{"subtype":"ellipsoidal","axis":[{"name":"Longitude","abbreviation":"lon","direction":"east","unit":"degree"},{"name":"Latitude","abbreviation":"lat","direction":"north","unit":"degree"}]}}
    cases = [Case("planar","EPSG:3857"),Case("authority","OGC:CRS84",hint="authority_code"),Case("projjson",projjson,hint="projjson"),Case("srid","42",hint="srid"),Case("large","EPSG:3857",large=True)]
    for edges in ("spherical","vincenty","thomas","andoyer","karney"):
        cases.append(Case("geography-"+edges,"EPSG:4326",edges))
    points = [struct.pack("<BIdd",1,1,1.5,2.5),None,struct.pack("<BIdd",1,1,-10.5,20.5)]
    with tempfile.TemporaryDirectory(prefix="parquet-geoarrow-") as directory:
        root = Path(directory)
        incoming, files, native, out = (root/name for name in ("incoming","files","native","out"))
        incoming.mkdir()
        for case in cases:
            metadata = {"crs":case.crs}
            if case.edges is not None: metadata["edges"] = case.edges
            if case.hint is not None: metadata["crs_type"] = case.hint
            storage = pa.large_binary() if case.large else pa.binary()
            field = pa.field("g",storage,metadata={b"ARROW:extension:name":b"geoarrow.wkb",b"ARROW:extension:metadata":json.dumps(metadata).encode()})
            table = pa.Table.from_arrays([pa.array(points,storage)],schema=pa.schema([field]))
            with ipc.new_file(str(incoming/(case.name+".arrow")),table.schema) as writer:writer.write_table(table)
        subprocess.run([str(args.writer),"--from-arrow",str(incoming),str(files)],check=True)
        assert len(list(files.glob("*.parquet"))) == len(cases)
        for case in cases:
            path = files/(case.name+".parquet")
            annotation = logical(path)
            tag = 18 if case.edges is not None else 17
            assert set(annotation) == {tag}, (case.name,annotation)
            parameters = annotation[tag].value
            raw_crs = parameters[1].value.decode()
            if isinstance(case.crs,dict):assert json.loads(raw_crs)==case.crs
            else:assert raw_crs == ("srid:"+case.crs if case.hint=="srid" else case.crs)
            if case.edges is None:assert 2 not in parameters
            else:assert parameters[2].value == ("spherical","vincenty","thomas","andoyer","karney").index(case.edges)
            assert pq.read_table(path,arrow_extensions_enabled=False).column("g").to_pylist() == points
        subprocess.run([str(args.writer),"--logical-metadata",str(native)],check=True)
        reference = native/"geometry-custom.parquet"
        data = reference.read_bytes();count = struct.unpack("<I",data[-8:-4])[0];footer = decode(data[-8-count:-8]).node
        footer.value[2].value[1].value[10].value[17].value[1] = Node(TType.STRING,b"projjson:crs-definition")
        footer.value[5] = Node(TType.LIST,[Node(TType.STRUCT,{1:Node(TType.STRING,b"crs-definition"),2:Node(TType.STRING,json.dumps(projjson).encode())})],element_kind=TType.STRUCT)
        raw = encode(footer);(native/"geometry-reference.parquet").write_bytes(data[:-8-count]+raw+struct.pack("<I",len(raw))+b"PAR1")
        subprocess.run([str(args.reader),str(native),"--arrow-output",str(out)],check=True)
        for source in native.glob("*.parquet"):
            with pa.memory_map(str(out/(source.name+".arrows")),"r") as data:
                table = ipc.open_stream(data).read_all()
            metadata = arrow_metadata(table.schema.field(0));name = source.stem
            if name=="geometry-reference":assert metadata["crs"]==projjson
            elif name=="geometry-custom":assert metadata["crs"]=="EPSG:3857"
            elif name.endswith("default"):assert metadata["crs"]=="OGC:CRS84"
            else:assert metadata["crs"]=="EPSG:4326"
            if name.startswith("geometry-"):assert "edges" not in metadata
            else:assert metadata["edges"] == ("spherical" if name=="geography-default" else name.removeprefix("geography-"))
            assert table.column(0).to_pylist()==[points[0]]
        bad, rejected = root/"bad",root/"rejected";bad.mkdir()
        field = pa.field("g",pa.binary(),metadata={b"ARROW:extension:name":b"geoarrow.wkb",b"ARROW:extension:metadata":b"{}"})
        table = pa.Table.from_arrays([pa.array(points,pa.binary())],schema=pa.schema([field]))
        with ipc.new_file(str(bad/"unknown-crs.arrow"),table.schema) as writer:writer.write_table(table)
        result = subprocess.run([str(args.writer),"--from-arrow",str(bad),str(rejected)],capture_output=True,text=True)
        assert result.returncode!=0 and "unknown GeoArrow CRS" in result.stderr
        assert not list(rejected.glob("*.parquet"))
    print(f"PASS: {len(cases)} independent Arrow inputs,9 native/reference Parquet inputs,all5 edges,PROJJSON/SRID/default CRS,large binary,nulls,WKB andunknown-CRS rejection")


if __name__=="__main__":main()
