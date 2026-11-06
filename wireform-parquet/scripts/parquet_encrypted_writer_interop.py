#!/usr/bin/env python3
"""Check writer encryption using Apache Thrift, independent AES and Arrow C++.

Only public fixture keys are used. For LZO, authenticate/decrypt every module
with cryptography, compare the compressed bytes to plaintext controls, then
decode control compression with unmodified liblzo2 before Arrow C++ reads it.
"""
import argparse
import hashlib
import json
from pathlib import Path
import struct
import subprocess
import tempfile

import pyarrow as pa
from cryptography.hazmat.primitives.ciphers import Cipher, algorithms, modes
from cryptography.hazmat.primitives.ciphers.aead import AESGCM

import parquet_lzo_interop as thrift
from parquet_nested_bloom_interop import strip_auxiliary


def canonical(node):
    # Compact Protocol requires a struct root, including for scalar/list comparisons.
    return thrift.encode(thrift.Node(thrift.TType.STRUCT, {1: node}))


def framed(data, start):
    assert 0 <= start <= len(data)-4
    size = int.from_bytes(data[start:start+4], 'little')
    assert size >= 12 and size <= len(data)-start-4
    return data[start+4:start+4+size], start+4+size


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--fixtures', type=Path, required=True)
    parser.add_argument('--reference', type=Path, required=True)
    parser.add_argument('--lzo-reference', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    root = args.fixtures / 'encryption-writer'
    files = sorted(root.glob('feature-*.parquet'))
    assert len(files) == 672, ('complete 7-codec/3-width/2-algorithm/2-mode/2-key-route/2-page/2-layout grid',len(files))
    nonces = set()
    module_count = 0
    cpp_queries = 0
    lzo_calls = 0
    records = []
    controls = {}
    with tempfile.TemporaryDirectory(prefix='encrypted-writer-oracle-') as directory:
        scratch = Path(directory)

        def cpp(path, mode, output):
            proc = subprocess.run([str(args.reference),str(path),str(output),mode],capture_output=True,text=True)
            assert proc.returncode == 0,(path.name,proc.stderr)
            head,*columns = map(json.loads,proc.stdout.splitlines())
            return head,columns

        for control in sorted(root.glob('plain-*.parquet')):
            _,shape,codec,version = control.stem.split('-')
            readable = control
            if codec == 'LZO':
                stripped = scratch / 'stripped.parquet'
                readable = scratch / 'lzo-control.parquet'
                strip_auxiliary(control,stripped)
                lzo_calls += thrift.transcode(stripped,readable,0,args.lzo_reference,scratch,require_source_codec=3,expected_version=1 if version=='PageV1' else 2)
            arrow = scratch/'control.arrow'
            head,columns = cpp(readable,'plaintext',arrow)
            controls[(shape,codec,version)] = pa.ipc.open_stream(arrow).read_all()
            assert head['rows'] == (3 if shape=='nested' else 6)
        for shape in ('mixed','nested'):
            for codec in ('Uncompressed','GZip','Snappy','ZSTD','LZ4Raw','LZO','Brotli'):
                for version in ('PageV1','PageV2'):
                    assert controls[(shape,codec,version)].equals(controls[(shape,'Uncompressed',version)],check_metadata=False)

        for path in files:
            _,shape,width,algorithm,mode,route,codec,version = path.stem.split('-')
            width = int(width)
            keys = {'kf':bytes([0x42])*width,'kc1':bytes([0x43])*width,'kc2':bytes([0x44])*width}
            raw = path.read_bytes()
            assert raw[:4] == raw[-4:] == (b'PARE' if mode=='EncryptedFooter' else b'PAR1')
            length = int.from_bytes(raw[-8:-4],'little')
            assert length <= len(raw)-12
            tail = raw[-8-length:-8]
            first = thrift.decode(tail)
            if mode == 'EncryptedFooter':
                crypto = first.node.value
                encoded_algorithm = crypto[1].value
                assert crypto[2].value == b'kf'
            else:
                encoded_algorithm = first.node.value[8].value
                assert first.node.value[9].value == b'kf'
            assert list(encoded_algorithm) == [1 if algorithm=='AesGcmV1' else 2]
            context = next(iter(encoded_algorithm.values())).value
            assert context[1].value == b'writer-regression'
            file_id = context[2].value
            assert len(file_id)>0

            def aad(kind,rg=0,col=0):
                return b'writer-regression'+file_id+bytes([kind])+(b'' if kind==0 else struct.pack('<hh',rg,col))+(b'\0\0' if kind in (2,4) else b'')

            def unique(key,nonce):
                nonlocal module_count
                assert len(nonce)==12 and (key,nonce) not in nonces,(path.name,'reused nonce')
                nonces.add((key,nonce));module_count+=1

            def gcm(key,kind,payload,rg=0,col=0):
                assert len(payload)>=28
                unique(key,payload[:12])
                return AESGCM(key).decrypt(payload[:12],payload[12:],aad(kind,rg,col))

            def module(key,kind,buffer,offset=0,rg=0,col=0):
                payload,end = framed(buffer,offset)
                return gcm(key,kind,payload,rg,col),end

            if mode == 'EncryptedFooter':
                plain,end = module(keys['kf'],0,tail,first.consumed)
                assert end == len(tail)
                decoded = thrift.decode(plain)
                assert decoded.consumed == len(plain)
                footer = decoded.node
            else:
                assert len(tail)-first.consumed == 28
                nonce,tag = tail[first.consumed:first.consumed+12],tail[-16:]
                unique(keys['kf'],nonce)
                assert AESGCM(keys['kf']).encrypt(nonce,tail[:first.consumed],aad(0))[-16:] == tag
                footer = first.node
            control = root/f'plain-{shape}-{codec}-{version}.parquet'
            source = control.read_bytes()
            source_length = int.from_bytes(source[-8:-4],'little')
            source_footer = thrift.decode(source[-8-source_length:-8]).node
            assert canonical(footer.value[2])==canonical(source_footer.value[2])
            assert footer.value[3].value==source_footer.value[3].value
            assert len(footer.value[4].value)==len(source_footer.value[4].value)
            for rg,(group,source_group) in enumerate(zip(footer.value[4].value,source_footer.value[4].value)):
                assert group.value[7].value==rg
                for col,(chunk,source_chunk) in enumerate(zip(group.value[1].value,source_group.value[1].value)):
                    original=source_chunk.value[3].value
                    key=keys['kf'] if route=='footer' else keys['kc1' if col%2==0 else 'kc2']
                    encrypted=col!=0
                    if encrypted:
                        crypto=chunk.value[8].value
                        assert list(crypto)==([1] if route=='footer' else [2])
                        if route=='split':
                            assert canonical(crypto[2].value[1])==canonical(original[3])
                            assert crypto[2].value[2].value==('kc1' if col%2==0 else 'kc2').encode()
                        if mode=='SignedPlaintextFooter' or route=='split':
                            metadata,end=module(key,1,chunk.value[9].value,rg=rg,col=col)
                            assert end==len(chunk.value[9].value)
                            cm=thrift.decode(metadata).node.value
                            if mode=='SignedPlaintextFooter':
                                assert 3 in chunk.value and 12 not in chunk.value[3].value
                            else:
                                assert 3 not in chunk.value
                        else:
                            assert 9 not in chunk.value
                            cm=chunk.value[3].value
                    else:
                        assert 8 not in chunk.value and 9 not in chunk.value
                        cm=chunk.value[3].value
                    for field in (1,2,3,4,5,12):
                        assert (field in cm)==(field in original)
                        if field in original:assert canonical(cm[field])==canonical(original[field]),(path.name,col,field)
                    old_start=original[9].value
                    old_header=thrift.decode(source[old_start:])
                    old_body=source[old_start+old_header.consumed:old_start+original[7].value]
                    start=cm[9].value
                    if encrypted:
                        header_bytes,after_header=module(key,4,raw,start,rg,col)
                        header=thrift.decode(header_bytes).node
                        body_payload,end=framed(raw,after_header)
                        if algorithm=='AesGcmV1':
                            body=gcm(key,2,body_payload,rg,col)
                        else:
                            unique(key,body_payload[:12])
                            decryptor=Cipher(algorithms.AES(key),modes.CTR(body_payload[:12]+b'\0\0\0\1')).decryptor()
                            body=decryptor.update(body_payload[12:])+decryptor.finalize()
                        assert header.value[3].value==end-after_header
                    else:
                        decoded=thrift.decode(raw[start:]);header=decoded.node
                        after_header=start+decoded.consumed
                        end=after_header+header.value[3].value
                        body=raw[after_header:end]
                    assert body==old_body,(path.name,col,'compressed page body')
                    assert cm[7].value==end-start
                    assert cm[6].value==original[6].value+(after_header-start)-old_header.consumed
                    for field in (1,2,5,8):
                        if field in old_header.node.value:assert canonical(header.value[field])==canonical(old_header.node.value[field])
                    for offset_field,length_field,kind in ((4,5,7),(6,7,6)):
                        assert (offset_field in chunk.value)==(offset_field in source_chunk.value)
                        if offset_field not in chunk.value:continue
                        offset,size=chunk.value[offset_field].value,chunk.value[length_field].value
                        data=raw[offset:offset+size]
                        if encrypted:
                            data,used=module(key,kind,data,rg=rg,col=col);assert used==size
                        old_offset,old_size=source_chunk.value[offset_field].value,source_chunk.value[length_field].value
                        old_data=source[old_offset:old_offset+old_size]
                        if kind==6:
                            assert data==old_data
                        else:
                            oi,old_oi=thrift.decode(data).node,thrift.decode(old_data).node
                            page=oi.value[1].value[0].value
                            assert page[1].value==start and page[2].value==end-start and page[3].value==0
                            page[1],page[2]=old_oi.value[1].value[0].value[1],old_oi.value[1].value[0].value[2]
                            assert canonical(oi)==canonical(old_oi)
                    assert (14 in cm)==(14 in original)
                    if 14 in cm:
                        offset=cm[14].value
                        old=source[original[14].value:original[14].value+original[15].value]
                        parsed=thrift.decode(old)
                        if encrypted:
                            bloom_header,after=module(key,8,raw,offset,rg,col)
                            bloom_bits,after_bits=module(key,9,raw,after,rg,col)
                            assert bloom_header==old[:parsed.consumed] and bloom_bits==old[parsed.consumed:]
                            assert cm[15].value==after_bits-offset
                        else:
                            assert raw[offset:offset+cm[15].value]==old
            if codec!='LZO':
                arrow=scratch/'decoded.arrow'
                head,columns=cpp(path,f'writer-{width}',arrow)
                assert pa.ipc.open_stream(arrow).read_all().equals(controls[(shape,codec,version)],check_metadata=False),(path.name,'C++ values')
                for column in columns:
                    assert column['encrypted']==(column['column']!=0)
                    cpp_queries+=column['bloomValuesChecked']
                direct=True
            else:
                direct=False
            records.append({'file':path.name,'independentCryptoAndLiteralMetadataMatch':True,'allModulePlaintextsMatchControls':True,'cppEncryptedValuesIndexesAndBlooms':direct,'lzoViaIndependentControl':codec=='LZO'})
    proof={'fileCount':len(records),'files':records,'allPassed':True,'uniqueKeyNoncePairs':len(nonces),'authenticatedOrCtrModules':module_count,'cppEncryptedFiles':sum(r['cppEncryptedValuesIndexesAndBlooms'] for r in records),'cppBloomQueries':cpp_queries,'liblzo2ControlOperations':lzo_calls,'controls':len(controls),'referenceSHA256':hashlib.sha256(args.reference.read_bytes()).hexdigest()}
    args.output.write_text(json.dumps(proof,indent=2)+'\n')
    print(f'PASS: {len(records)} encrypted files, {module_count} independent module checks, {proof["cppEncryptedFiles"]} direct C++ reads, {cpp_queries} bloom probes',flush=True)


if __name__=='__main__':
    main()
