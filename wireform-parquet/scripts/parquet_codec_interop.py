#!/usr/bin/env python3
"""All enabled codecs: independent C++ pages, native writer pages and errors.

LZO/Hadoop LZ4 have their own independent interop drivers. This suite covers
UNCOMPRESSED, GZIP, SNAPPY, ZSTD, LZ4_RAW and BROTLI, including dictionaries,
both page versions and explicit uncompressed V2 value sections.
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
from thrift.Thrift import TType
from parquet_lzo_interop import Node, decode, encode, transcode, signed_crc
from parquet_metadata_interop import parse

CODECS = {"uncompressed":0,"snappy":1,"gzip":2,"brotli":4,"zstd":6,"lz4":7}


def run(command, succeeds=True):
    result = subprocess.run(list(map(str, command)),capture_output=True,text=True,timeout=120)
    if succeeds:
        assert result.returncode == 0, (command,result.stdout,result.stderr)
    else:
        assert result.returncode > 0 and "FAIL" in result.stdout, (command,result.stdout,result.stderr)
    return result


def read_output(directory, name):
    with ipc.open_stream(directory / (name + ".arrows")) as reader:
        table = reader.read_all()
    table.validate(full=True)
    return table


def same_type(left, right):
    if pa.types.is_list(left) and pa.types.is_list(right):
        return left.value_field.nullable == right.value_field.nullable and same_type(left.value_type,right.value_type)
    if pa.types.is_struct(left) and pa.types.is_struct(right):
        return len(left)==len(right) and all(a.name==b.name and a.nullable==b.nullable and same_type(a.type,b.type) for a,b in zip(left,right))
    return left == right


def same_table(actual, expected):
    # Standard Parquet LIST wrapper names are positional; preserve all logical
    # types, nullable declarations, struct names and actual values.
    return actual.column_names==expected.column_names and all(a.nullable==b.nullable and same_type(a.type,b.type) for a,b in zip(actual.schema,expected.schema)) and actual.to_pylist()==expected.to_pylist()


def native_expected(shape):
    nullable = shape != "required"
    schema = pa.schema([pa.field("i",pa.int32(),nullable),pa.field("s",pa.string(),nullable),pa.field("b",pa.bool_(),nullable),pa.field("d",pa.float64(),nullable)])
    if shape == "empty":
        values=[[],[],[],[]]
    elif shape == "allnull":
        values=list(map(lambda _: [None]*160, schema))
    elif shape == "mixed":
        values=[[-2**31,None,0,2**31-1,None]*32,["κόσμε",None,"","alpha",None]*32,[True,None,False,True,None]*32,[-1.5,None,0,1.25,None]*32]
    else:
        values=[[-2**31,-1,0,2**31-1,1]*32,["κόσμε","beta","","alpha","omega"]*32,[True,False,False,True,False]*32,[-1.5,-1,0,1.25,2]*32]
    return pa.Table.from_arrays(list(map(lambda pair: pa.array(pair[0],type=pair[1].type),zip(values,schema))),schema=schema)


def corrupt_page(source,target,mode,codec):
    parsed=parse(source)
    metadata=parsed.footer
    column=metadata.value[4].value[0].value[1].value[0].value[3].value
    offset=column[9].value
    assert offset == 4 and 11 not in column
    decoded=decode(parsed.prefix[offset:]);header=decoded.node
    body=parsed.prefix[offset+decoded.consumed:offset+decoded.consumed+header.value[3].value]
    if mode == "large-size":
        header.value[2].value += 1
    elif mode == "small-size":
        header.value[2].value -= 1
    elif mode == "truncated":
        body=body[:-1]
    elif mode == "trailing":
        body += b"trailing"
    else:
        assert mode in ("null-raw","null-compressed")
        body=b"\x06\x00invalid codec bytes"
        header.value[1]=Node(TType.I32,3)
        header.value.pop(5,None)
        header.value[8]=Node(TType.STRUCT,{1:Node(TType.I32,3),2:Node(TType.I32,3),3:Node(TType.I32,3),4:Node(TType.I32,0),5:Node(TType.I32,2),6:Node(TType.I32,0),7:Node(TType.BOOL,mode=="null-compressed")})
        header.value[2]=Node(TType.I32,2)
        if 12 in column:
            column[12]=Node(TType.STRUCT,{3:Node(TType.I64,3)})
    header.value[3]=Node(TType.I32,len(body))
    header.value[4]=Node(TType.I32,signed_crc(body))
    encoded=encode(header)
    column[6]=Node(TType.I64,len(encoded)+header.value[2].value)
    column[7]=Node(TType.I64,len(encoded)+len(body))
    column[4]=Node(TType.I32,codec)
    group=metadata.value[4].value[0].value
    group[2]=Node(TType.I64,column[6].value)
    group[6]=Node(TType.I64,column[7].value)
    footer=encode(metadata)
    target.write_bytes(b"PAR1"+encoded+body+footer+struct.pack("<I",len(footer))+b"PAR1")


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--reader",type=Path,required=True)
    parser.add_argument("--writer",type=Path,required=True)
    parser.add_argument("--output",type=Path,required=True)
    parser.add_argument("--expect-codecs",nargs="+",default=list(CODECS))
    args=parser.parse_args()
    codecs=args.expect_codecs
    assert set(codecs)<=CODECS.keys()
    for name in codecs:
        if name != "uncompressed":
            assert pa.Codec.is_available("lz4_raw" if name=="lz4" else name),name
    with tempfile.TemporaryDirectory(prefix="codec-interop-") as directory:
        work=Path(directory)
        incoming,decoded,native,scratch=(work/name for name in ("incoming","decoded","native","scratch"))
        for path in (incoming,decoded,native,scratch):path.mkdir()
        tables={
            "scalar":pa.table({"i":pa.array([-2**31,None,0,2**31-1,-1]*8,pa.int32()),"s":pa.array(["κόσμε",None,"","alpha","omega"]*8),"b":pa.array([True,None,False,True,False]*8)}),
            "nested":pa.table({"items":pa.array([None,[],[None,1,2],[3]]*8,pa.list_(pa.int32())),"s":pa.array([None,{"x":1},{"x":None},{"x":-1}]*8,pa.struct([("x",pa.int64())]))}),
            "allnull":pa.table({"x":pa.array([None]*40,pa.int32())}),
            "empty":pa.table({"x":pa.array([],pa.int32())}),
        }
        expected={}
        for codec in codecs:
            for family,table in tables.items():
                for version in ("1.0","2.0"):
                    for dictionary in (False,True):
                        name=f"{codec}-{family}-v{version}-dict{int(dictionary)}"
                        path=incoming/(name+".parquet")
                        pq.write_table(table,path,compression="NONE" if codec=="uncompressed" else codec,data_page_version=version,use_dictionary=dictionary,row_group_size=13,write_batch_size=5,max_rows_per_page=5,write_page_index=False,write_page_checksum=True)
                        expected[path.name]=table
                        if version=="2.0":
                            raw=incoming/(name+"-raw-values.parquet")
                            transcode(path,raw,CODECS[codec],None,scratch,v2_uncompressed=True,require_source_codec=CODECS[codec],expected_version=2)
                            assert pq.read_table(raw).equals(table,check_metadata=False)
                            expected[raw.name]=table
        run([args.reader,incoming,"--arrow-output",decoded])
        for name,table in expected.items():
            actual=read_output(decoded,name)
            assert same_table(actual,table),(name,actual,table)
        run([args.writer,"--codec-profiles",native])
        paths=sorted(native.glob("*.parquet"))
        assert len(paths)==len(codecs)*16,(len(paths),len(codecs)*16)
        for path in paths:
            codec,version,shape,indexed=path.stem.split("-")
            assert codec in codecs
            wanted=native_expected(shape)
            actual=pq.read_table(path)
            actual.validate(full=True)
            assert actual.equals(wanted,check_metadata=False),(path.name,actual,wanted)
            metadata=parse(path).footer
            for group in metadata.value[4].value:
                for chunk in group.value[1].value:
                    assert chunk.value[3].value[4].value==CODECS[codec]
        invalid=work/"invalid";invalid.mkdir()
        rejected=0
        for codec in codecs:
            seed=work/(codec+"-seed.parquet")
            pq.write_table(pa.table({"x":pa.array([1,2,3],pa.int32())}),seed,compression="NONE" if codec=="uncompressed" else codec,use_dictionary=False,write_page_index=False,write_page_checksum=True)
            for mode in ("large-size","small-size","truncated","trailing","null-raw","null-compressed"):
                path=invalid/(codec+"-"+mode+".parquet")
                corrupt_page(seed,path,mode,CODECS[codec])
                # Isolate each failure; an invalid file must produce a handled
                # decoder failure, not a signal, timeout or partial output.
                one=work/("case-"+str(rejected));one.mkdir();(one/path.name).symlink_to(path)
                run([args.reader,one],False)
                rejected+=1
        report={"pyarrow":pa.__version__,"codecs":codecs,"incomingFiles":len(expected),"nativeWriterFiles":len(paths),"malformedRejected":rejected,"failures":0,"skips":0}
        args.output.write_text(json.dumps(report,indent=2)+"\n");print(json.dumps(report,indent=2))

if __name__=="__main__":main()
