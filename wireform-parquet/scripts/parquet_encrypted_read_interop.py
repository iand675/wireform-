#!/usr/bin/env python3
"""Check complete native encrypted reads against unmodified Arrow C++.

Compares every physical value and level, Arrow values (INT96 is checked as raw
physical bytes), indexes and bloom filters with the official encrypted corpus.
Exercises explicit/metadata key routing, projections without unrelated keys,
and whole-page predicates with damaged skipped/retained headers. All keys are
public fixture data and external material uses a local mock KMS.
"""

import argparse
import hashlib
import json
import math
from pathlib import Path
import struct
import subprocess

import pyarrow as pa
import pyarrow.ipc as ipc

def same(a,b):
    if isinstance(a,float) and isinstance(b,float):return a==b or math.isnan(a) and math.isnan(b)
    if isinstance(a,list) and isinstance(b,list):return len(a)==len(b) and all(same(x,y) for x,y in zip(a,b))
    if isinstance(a,dict) and isinstance(b,dict):return a.keys()==b.keys() and all(same(a[k],b[k]) for k in a)
    return a==b


def selected_checks(r, root, reader):
    results = []
    def run(name,case,config,path,expected=None,fail=False):
        dest=r/'selection'/name/case;dest.parent.mkdir(parents=True,exist_ok=True)
        keys=dest.with_suffix('.keys.json');keys.write_text(json.dumps(config,indent=2)+'\n')
        proc=subprocess.run([str(reader),'--encrypted-read',str(path),str(keys),str(dest)+'.arrow'],capture_output=True,text=True,timeout=60)
        Path(str(dest)+'.stdout').write_text(proc.stdout);Path(str(dest)+'.stderr').write_text(proc.stderr)
        if fail:assert proc.returncode!=0,(name,case)
        else:
            assert proc.returncode==0,(name,case,proc.stderr)
            actual=ipc.open_stream(str(dest)+'.arrow').read_all()
            assert actual.column_names==expected.column_names,(name,case,'columns')
            assert same(actual.to_pylist(),expected.to_pylist()),(name,case,'values')
        record={'fixture':name,'case':case,'exit':proc.returncode,'expectedFailure':fail,'passed':True};results.append(record)
        (r/'selection-results.json').write_text(json.dumps(results,indent=2)+'\n')
        print(json.dumps(record),flush=True)

    for name in json.loads((r/'oracle-fixtures.json').read_text()):
        oracle=r/'oracle'/name;header,*columns=map(json.loads,Path(str(oracle)+'.jsonl').read_text().splitlines())
        config=json.loads(Path(str(oracle)+'.keys.json').read_text());table=ipc.open_stream(str(oracle)+'.arrow').read_all();path=root/name
        eligible=filter(lambda c: c['encrypted'] and c['columnIndex'] and c['type'] in [1,2,4,5] and c['maxRepetition']==0 and len(c['pathParts'])==1, columns)
        seen = set()
        for column in eligible:
            identity = (column['column'], tuple(column['pathParts']))
            if identity in seen:
                continue
            seen.add(identity)
            related = sorted((item for item in columns if item['column'] == column['column']), key=lambda item: item['rowGroup'])
            def decode(hex_string):
                raw=bytes.fromhex(hex_string)
                return int.from_bytes(raw,'little',signed=True) if column['type'] in [1,2] else struct.unpack('<f' if column['type']==4 else '<d',raw)[0]
            maxima = []
            for item in related:
                for value, null in zip(item['pageMaxima'], item['nullPages']):
                    if not null:
                        maxima.append(decode(value))
            assert maxima and all(math.isfinite(v) for v in maxima)
            thresholds=[('all',min(maxima)-1),('none',max(maxima)+1)]
            if len(set(maxima))>1:thresholds.append(('partial',sorted(set(maxima))[len(set(maxima))//2]))
            field=column['pathParts'][0]
            for label,threshold in thresholds:
                case=f"column-{column['column']}-{label}";plan={**config,'projection':[field],'prune':str(threshold)}
                plans = []
                parts = []
                base = 0
                for item in related:
                    kept = list(map(lambda pair: not pair[1] and decode(pair[0]) >= threshold, zip(item['pageMaxima'], item['nullPages'])))
                    plans.append((item, kept))
                    for index, (offset, size, start) in enumerate(item['pageLocations']):
                        if kept[index]:
                            end = item['pageLocations'][index+1][2] if index+1 < len(kept) else item['events']
                            parts.append(table.select([field]).slice(base+start, end-start))
                    base += item['events']
                expected = pa.concat_tables(parts) if parts else table.select([field]).slice(0, 0)
                run(name, case, plan, path, expected)
                skipped, retained = [], []
                for item, kept in plans:
                    for index, retain in enumerate(kept):
                        (retained if retain else skipped).append((item, index))
                if skipped:
                    altered = bytearray(path.read_bytes())
                    for item, index in skipped:
                        altered[item['pageLocations'][index][0]] ^= 1
                    changed = r/'selection-damaged'/name/(case+'.parquet.encrypted')
                    changed.parent.mkdir(parents=True, exist_ok=True)
                    changed.write_bytes(altered)
                    run(name, case+'-damaged-skipped', plan, changed, expected)
                if retained:
                    altered = bytearray(path.read_bytes())
                    item, index = retained[0]
                    altered[item['pageLocations'][index][0]] ^= 1
                    changed = r/'selection-damaged'/name/(case+'-retained.parquet.encrypted')
                    changed.parent.mkdir(parents=True, exist_ok=True)
                    changed.write_bytes(altered)
                    run(name, case+'-damaged-retained', plan, changed, fail=True)
        plaintext=next((c for c in columns if not c['encrypted'] and len(c['pathParts'])==1 and c['type']!=3 and all(not item['encrypted'] for item in columns if item['column']==c['column'])),None)
        encrypted=next((c for c in columns if c['encrypted'] and len(c['pathParts'])==1 and c['keyMetadataHex'] and c['keyMetadataHex']!=header['footerKeyMetadataHex']),None)
        if plaintext and encrypted:
            without_keys={**config,'columns':[],'projection':[plaintext['pathParts'][0]]}
            expected=table.select([plaintext['pathParts'][0]])
            run(name,'project-public-without-column-keys',without_keys,path,expected)
            missing={**without_keys,'projection':[encrypted['pathParts'][0]]}
            run(name,'missing-requested-column-key',missing,path,fail=True)
    return results


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--reference", type=Path, required=True)
    parser.add_argument("--reader", type=Path, required=True)
    parser.add_argument("--corpus", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    r = args.output.resolve().with_suffix(".artifacts")
    r.mkdir(parents=True, exist_ok=True)
    root = args.corpus.resolve() / "data"
    reader = args.reader.resolve()
    names = []
    for path in sorted(root.rglob("*.parquet.encrypted")):
        name = path.relative_to(root)
        dest = r / "oracle" / name
        dest.parent.mkdir(parents=True, exist_ok=True)
        process = subprocess.run([str(args.reference.resolve()), str(path), str(dest)+".arrow"],
                                 capture_output=True, text=True, timeout=60)
        if process.returncode:
            raise AssertionError(f"reference rejected {name}: {process.stderr}")
        Path(str(dest)+".jsonl").write_text(process.stdout)
        header, *columns = map(json.loads, process.stdout.splitlines())
        assert all(column["pathParts"] for column in columns), name
        keys = {"footer": header["publicFixtureFooterKeyHex"], "footerMetadata": header["footerKeyMetadataHex"],
                "prefix": "746573746572" if header["prefixRequired"] else header["storedPrefixHex"],
                "mode": "metadata", "columns": []}
        for column in columns:
            if column["encrypted"]:
                keys["columns"].append({"path": column["pathParts"], "key": column["publicFixtureColumnKeyHex"],
                                        "metadata": column["keyMetadataHex"]})
        Path(str(dest)+".keys.json").write_text(json.dumps(keys, indent=2)+"\n")
        names.append(str(name))
    if len(names) < 14:
        raise AssertionError(f"incomplete encrypted corpus: {len(names)} fixtures")
    (r/"oracle-fixtures.json").write_text(json.dumps(names, indent=2)+"\n")
    results = []
    fields = ['rowGroup','column','path','type','maxDefinition','maxRepetition','columnIndex','offsetPages','bloomBytes','events','present','definitions','repetitions','values']
    for name in json.loads((r/'oracle-fixtures.json').read_text()):
        oracle=r/'oracle'/name
        reference=list(map(json.loads, Path(str(oracle)+'.jsonl').read_text().splitlines()))
        for mode in ['metadata','explicit']:
            dest=r/f'native-{mode}'/name;dest.parent.mkdir(parents=True,exist_ok=True)
            config=json.loads(Path(str(oracle)+'.keys.json').read_text());config['mode']=mode
            keyfile=Path(str(dest)+'.keys.json');keyfile.write_text(json.dumps(config,indent=2)+'\n')
            result=subprocess.run([str(reader),'--encrypted-read',str(root/name),str(keyfile),str(dest)+'.arrow'],capture_output=True,text=True,timeout=60)
            Path(str(dest)+'.stdout').write_text(result.stdout);Path(str(dest)+'.stderr').write_text(result.stderr)
            record={'fixture':name,'mode':mode,'exit':result.returncode,'stderr':result.stderr.strip()}
            if result.returncode==0:
                try:
                    actual=list(map(json.loads, result.stdout.splitlines()))
                    assert all(actual[0][key]==reference[0][key] for key in ['rows','rowGroups','columns'])
                    assert len(actual)==len(reference)
                    for left,right in zip(actual[1:],reference[1:]):
                        for key in fields:assert left[key]==right[key],(key,left['column'],str(left[key])[:120],str(right[key])[:120])
                    expected=ipc.open_stream(str(oracle)+'.arrow').read_all();native=ipc.open_stream(str(dest)+'.arrow').read_all()
                    assert native.num_rows==expected.num_rows
                    for field in expected.column_names:
                        assert same(native[field].to_pylist(),expected[field].to_pylist()),('Arrow value mismatch',field)
                    record.update(physicalValuesMatch=True,arrowValuesMatch=True)
                except Exception as error:record['comparisonFailure']=str(error)
            results.append(record);print(json.dumps(record),flush=True)
            (r/'first-encrypted-results.json').write_text(json.dumps(results,indent=2)+'\n')
    assert len(results) == 2 * len(names)
    assert all(result.get("physicalValuesMatch") and result.get("arrowValuesMatch") for result in results), results
    selected_results = selected_checks(r, root, reader)
    revision = subprocess.check_output(["git", "-C", str(args.corpus.resolve()), "rev-parse", "HEAD"], text=True).strip()
    digest = lambda path: hashlib.sha256(Path(path).read_bytes()).hexdigest()
    args.output.write_text(json.dumps({
        "scope": "Complete native physical/Arrow reads plus encrypted index/bloom and selective/projection checks",
        "fullNativeEncryptedReadsValidated": True, "corpusRevision": revision,
        "pyarrowVersion": pa.__version__, "referenceSHA256": digest(args.reference), "readerSHA256": digest(reader),
        "referenceSourceSHA256": digest(Path(__file__).with_name("parquet_encrypted_reference.cc")),
        "fullReads": results, "selectiveChecks": selected_results,
    }, indent=2)+"\n")
    print(f"Passed {len(results)} complete encrypted reads and {len(selected_results)} selective/projection checks", flush=True)


if __name__ == "__main__":
    main()
