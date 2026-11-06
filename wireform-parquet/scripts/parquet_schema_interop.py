#!/usr/bin/env python3
"""Schema-shape boundaries using independent Apache Thrift serialization.

C++ writes the valid scalar/struct controls. Malformed cases follow the
published SchemaElement requirements; permissive reference readers need not
reject them for the native checked boundary to reject them correctly.
"""
import argparse
import json
from pathlib import Path
import struct
import subprocess
import tempfile
import pyarrow as pa
import pyarrow.parquet as pq
from thrift.Thrift import TType
from parquet_lzo_interop import Node,encode
from parquet_metadata_interop import parse


def write(path,parsed):
    raw=encode(parsed.footer)
    path.write_bytes(parsed.prefix+raw+struct.pack("<I",len(raw))+b"PAR1")


def probe(reader,path,succeeds,root):
    directory=root/path.stem;directory.mkdir();(directory/path.name).symlink_to(path)
    result=subprocess.run([str(reader),str(directory)],capture_output=True,text=True,timeout=120)
    assert (result.returncode==0 if succeeds else result.returncode>0 and "FAIL" in result.stdout),(path,result.stdout,result.stderr)


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--reader",type=Path,required=True)
    parser.add_argument("--output",type=Path,required=True)
    args=parser.parse_args()
    with tempfile.TemporaryDirectory(prefix="schema-boundary-") as directory:
        root=Path(directory)
        scalar=root/"scalar.parquet";nested=root/"nested.parquet"
        pq.write_table(pa.table({"x":pa.array([],pa.int32())}),scalar)
        pq.write_table(pa.table({"g":pa.array([],pa.struct([("x",pa.int32())]))}),nested)
        controls=2;rejected=0
        for path in (scalar,nested):probe(args.reader,path,True,root)
        for index,marker in enumerate((None,0,1,2)):
            parsed=parse(nested)
            if marker is None:parsed.footer.value[2].value[0].value.pop(3,None)
            else:parsed.footer.value[2].value[0].value[3]=Node(TType.I32,marker)
            path=root/f"root-marker-{index}.parquet";write(path,parsed);probe(args.reader,path,True,root);controls+=1
        cases=[
            (scalar,"missing-leaf-repetition",1,3,None),
            (nested,"missing-group-repetition",1,3,None),
            (nested,"missing-nested-repetition",2,3,None),
            (scalar,"primitive-count-zero",1,5,0),
            (scalar,"primitive-count-positive",1,5,1),
            (scalar,"primitive-count-negative",1,5,-1),
            (nested,"missing-group-count",1,5,None),
            (scalar,"negative-root-count",0,5,-1),
            (scalar,"oversized-root-count",0,5,2**31-1),
            (nested,"negative-group-count",1,5,-1),
            (scalar,"unknown-repetition-enum",1,3,99),
            (scalar,"unknown-physical-enum",1,1,99),
        ]
        for seed,name,element,key,value in cases:
            parsed=parse(seed);fields=parsed.footer.value[2].value[element].value
            if value is None:fields.pop(key,None)
            else:fields[key]=Node(TType.I32,value)
            path=root/(name+".parquet");write(path,parsed);probe(args.reader,path,False,root);rejected+=1
        # A legal empty schema needs an explicit root child count of zero.
        # Remove that field only after verifying the canonical native control.
        parsed=parse(scalar);parsed.prefix=b"PAR1";fields=parsed.footer.value
        fields[2].value=fields[2].value[:1];fields[2].value[0].value[5]=Node(TType.I32,0)
        fields[3]=Node(TType.I64,0);fields[4].value=[];fields.pop(5,None);fields.pop(7,None)
        control=root/"empty-root-control.parquet";write(control,parsed);probe(args.reader,control,True,root);controls+=1
        fields[2].value[0].value.pop(5)
        bad=root/"missing-empty-root-count.parquet";write(bad,parsed);probe(args.reader,bad,False,root);rejected+=1
        report={"pyarrow":pa.__version__,"controls":controls,"malformedRejected":rejected,"failures":0,"skips":0}
        args.output.write_text(json.dumps(report,indent=2)+"\n");print(json.dumps(report,indent=2))

if __name__=="__main__":main()
