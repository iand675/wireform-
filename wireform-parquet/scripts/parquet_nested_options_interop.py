#!/usr/bin/env python3
"""Independent nested compression/page-version/index checks.

Requires all optional codecs enabled. LZO pages are decoded through an
unmodified liblzo2 oracle, then read by PyArrow, DuckDB and Polars; the
ordinary PyArrow build does not include LZO. Index metadata is verified on
the original files before that codec-only conversion.
"""
import argparse
from pathlib import Path
import hashlib,json,struct,subprocess
import duckdb
import polars as pl
import pyarrow as pa
import pyarrow.parquet as pq
import parquet_nested_types_interop as typed
import parquet_lzo_interop as lzo


def without_indexes(source,target):
    data=source.read_bytes();length=int.from_bytes(data[-8:-4],'little');start=len(data)-8-length
    footer=lzo.decode(data[start:-8]).node
    for group in footer.value[4].value:
        for chunk in group.value[1].value:
            for field in (4,5,6,7):chunk.value.pop(field,None)
    encoded=lzo.encode(footer)
    target.write_bytes(data[:start]+encoded+struct.pack('<I',len(encoded))+b'PAR1')


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--probe',type=Path,required=True)
    parser.add_argument('--lzo-reference',type=Path,required=True)
    parser.add_argument('--output-dir',type=Path,required=True)
    parser.add_argument('--aux-fixtures',type=Path,required=True)
    parser.add_argument('--reference',type=Path,required=True)
    args=parser.parse_args();root=args.output_dir;root.mkdir(parents=True,exist_ok=True)
    inputs=root/'inputs';outputs=root/'outputs';references=root/'references';scratch=root/'scratch'
    for folder in (inputs,references,scratch):folder.mkdir(exist_ok=True)
    expected=dict(typed.catalogue())
    extra=pa.Table.from_pylist([
        {'s':None,'l':None,'b':b''},
        {'s':{'x':None,'t':'日本語'},'l':[],'b':None},
        {'s':{'x':-7,'t':None},'l':[None,True,False],'b':b'\x00\xff'},
    ],schema=pa.schema([('s',pa.struct([('x',pa.int32()),('t',pa.string())])),('l',pa.list_(pa.bool_())),('b',pa.binary())]))
    expected['mixed_nested']=extra
    expected['all_null']=pa.table({'v':pa.array([None,None,None],type=pa.int32())})
    for name,table in expected.items():
        with pa.OSFile(str(inputs/(name+'.arrow')),'wb') as sink:
            with pa.ipc.new_file(sink,table.schema) as stream:stream.write_table(table)
        pq.write_table(table,references/(name+'.parquet'))
    subprocess.run([str(args.probe),'--nested-options-from-arrow',str(inputs),str(outputs)],check=True)
    aux_results=[]
    for name in ('nested-bloom-with-options','nested-all-null-with-options'):
        path=args.aux_fixtures/(name+'.parquet');output=root/(name+'.arrow')
        proc=subprocess.run([str(args.reference),str(path),str(output),'plaintext'],capture_output=True,text=True)
        assert proc.returncode==0,(name,proc.stderr)
        header,*columns=map(json.loads,proc.stdout.splitlines())
        assert header['rowGroups']==1 and len(columns)==1,name
        assert columns[0]['bloomBytes']>0 and columns[0]['offsetPages']==1,name
        if 'all-null' in name:
            assert columns[0]['columnIndex'] and columns[0]['nullPages']==[True],name
            expected_aux=pa.table({'v':pa.array([None,None,None],type=pa.int32())})
        else:
            expected_aux=pa.table({'v':pa.array([None,[],[42,None,-7],[9]],type=pa.list_(pa.field('element',pa.int32())))})
        typed.assert_equal(name,pa.ipc.open_stream(output).read_all(),expected_aux)
        aux_results.append({'file':name,'cppValuesIndexBloomPass':True})
    results=[];codec_names={'Uncompressed':'UNCOMPRESSED','GZip':'GZIP','Snappy':'SNAPPY','ZSTD':'ZSTD','LZ4Raw':'LZ4','LZO':'LZO','Brotli':'BROTLI'}
    with duckdb.connect() as db:
        for codec,metadata_codec in codec_names.items():
            for version in (1,2):
                directory=outputs/(codec+'-PageV'+str(version));files=sorted(directory.glob('*.parquet'))
                assert set(map(lambda p:p.stem,files))==set(expected),(codec,version,'case coverage')
                operations=0
                for path in files:
                    metadata=pq.ParquetFile(path).metadata
                    assert metadata.num_row_groups==1,path
                    group=metadata.row_group(0)
                    assert group.total_byte_size==sum(group.column(i).total_uncompressed_size for i in range(group.num_columns)),path
                    data=path.read_bytes();footer_length=int.from_bytes(data[-8:-4],'little')
                    footer=lzo.decode(data[-8-footer_length:-8]).node
                    chunks=footer.value[4].value[0].value[1].value
                    for index in range(group.num_columns):
                        column=group.column(index)
                        header=lzo.decode(data[column.data_page_offset:])
                        assert header.node.value[1].value==(0 if version==1 else 3),(path,'page version')
                        assert header.consumed+header.node.value[3].value==column.total_compressed_size,(path,'compressed size')
                        assert header.consumed+header.node.value[2].value==column.total_uncompressed_size,(path,'uncompressed size')
                        chunk=chunks[index].value
                        offset_index=lzo.decode(data[chunk[4].value:chunk[4].value+chunk[5].value])
                        assert offset_index.consumed==chunk[5].value,(path,'offset index size')
                        pages=offset_index.node.value[1].value
                        assert len(pages)==1,(path,'page count')
                        page=pages[0].value
                        assert page[1].value==column.data_page_offset and page[2].value==column.total_compressed_size and page[3].value==0,(path,'offset index locations')
                        assert column.compression==metadata_codec,(path,column.compression)
                        assert column.has_offset_index,(path,'offset index missing')
                    readable=path
                    if codec=='LZO':
                        stripped=scratch/'stripped.parquet';readable=scratch/'decoded.parquet'
                        without_indexes(path,stripped)
                        operations+=lzo.transcode(stripped,readable,0,args.lzo_reference,scratch,require_source_codec=3,expected_version=version)
                    actual=pq.ParquetFile(readable).read(use_threads=False)
                    typed.assert_equal(str(path),actual,expected[path.stem])
                    reference=references/(path.stem+'.parquet')
                    own=db.execute('SELECT * FROM read_parquet(?)',[str(readable)]).to_arrow_table()
                    ref=db.execute('SELECT * FROM read_parquet(?)',[str(reference)]).to_arrow_table()
                    typed.assert_equal('DuckDB '+str(path),own,ref)
                    typed.assert_equal('Polars '+str(path),pl.read_parquet(readable).to_arrow(),pl.read_parquet(reference).to_arrow())
                results.append({'codec':codec,'pageVersion':version,'files':len(files),'engines':['PyArrow','DuckDB','Polars'],'independentLzoPageOperations':operations,'passed':True})
                (root/'results.json').write_text(json.dumps({'probeSHA256':hashlib.sha256(args.probe.read_bytes()).hexdigest(),'pyarrowVersion':pa.__version__,'auxiliaryFixtures':aux_results,'groups':results},indent=2)+'\n')
                print(codec,version,len(files),'PASS',flush=True)
    print('PASS',sum(x['files'] for x in results),'nested option files across three independent readers',flush=True)


if __name__=='__main__':main()
