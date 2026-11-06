#!/usr/bin/env python3
"""Canonical typed Arrow -> native Parquet, checked by PyArrow and Rust.

Every source metadata blob and supplied fallback fragment must survive
verbatim. Native-only fallback slots encode implicit root/array nulls.
"""
import argparse
import json
from pathlib import Path
import tempfile
import struct

from parquet_variant_typed_interop import EMPTY
from parquet_variant_interop import footer

import pyarrow as pa
import pyarrow.ipc as ipc
import pyarrow.parquet as pq

from parquet_variant_typed_interop import generate_cases, outputs, run


def child(array, name):
    return array.field(name) if array.type.get_field_index(name) >= 0 else None


def binary_at(array):
    return None if array is None or not array[0].is_valid else array[0].as_py()


def check_fragments(source, native, required=True):
    assert len(source) == len(native) == 1
    if not source[0].is_valid:
        assert not native[0].is_valid
        return
    assert native[0].is_valid
    metadata = child(source, 'metadata')
    if metadata is not None:
        assert binary_at(metadata) == binary_at(child(native, 'metadata'))
    fallback, typed = child(source, 'value'), child(source, 'typed_value')
    present = typed is not None and typed[0].is_valid
    expected = binary_at(fallback)
    if expected is None and not present and required:
        expected = b'\x00'
    assert binary_at(child(native, 'value')) == expected
    if typed is None:
        return
    actual = child(native, 'typed_value')
    assert actual is not None and actual[0].is_valid == present
    if not present:
        return
    if pa.types.is_struct(typed.type):
        for field in typed.type:
            check_fragments(typed.field(field.name), actual.field(field.name), False)
    elif pa.types.is_list(typed.type) or pa.types.is_large_list(typed.type) or pa.types.is_list_view(typed.type):
        expected_values, actual_values = typed[0].values, actual[0].values
        assert len(expected_values) == len(actual_values)
        for i in range(len(expected_values)):
            check_fragments(expected_values.slice(i, 1), actual_values.slice(i, 1), True)


def check_schema(source, actual, root=True):
    assert pa.types.is_struct(actual.type)
    if root:
        assert actual.metadata and actual.metadata.get(b'ARROW:extension:name') == b'arrow.parquet.variant'
    expected_names = set(source.type.names) | {'value'}
    assert set(actual.type.names) == expected_names, (source, actual)
    for field in source.type:
        target = actual.type.field(field.name)
        if field.name in ('metadata', 'value'):
            assert target.type == pa.binary()
        elif field.name == 'typed_value':
            source_type, target_type = field.type, target.type
            if pa.types.is_struct(source_type):
                assert pa.types.is_struct(target_type) and source_type.names == target_type.names
                for child_field in source_type:
                    check_schema(child_field, target_type.field(child_field.name), False)
            elif pa.types.is_list(source_type) or pa.types.is_large_list(source_type) or pa.types.is_list_view(source_type):
                assert pa.types.is_list(target_type) and not target_type.value_field.nullable
                check_schema(source_type.value_field, target_type.value_field, False)
            else:
                if pa.types.is_large_binary(source_type) or pa.types.is_binary_view(source_type):
                    source_type = pa.binary()
                if pa.types.is_large_string(source_type) or pa.types.is_string_view(source_type):
                    source_type = pa.string()
                uuid_extension = isinstance(target_type, pa.BaseExtensionType) and target_type.extension_name == 'arrow.uuid'
                if uuid_extension:
                    target_type = target_type.storage_type
                assert source_type == target_type, (field, target)
                if field.metadata and field.metadata.get(b'ARROW:extension:name') == b'arrow.uuid':
                    assert uuid_extension or (target.metadata and target.metadata[b'ARROW:extension:name'] == b'arrow.uuid')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--writer', type=Path, required=True)
    parser.add_argument('--reader', type=Path, required=True)
    parser.add_argument('--reference', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--semantic', action='store_true', help='shred decoded Variant values back to their declared shape')
    args = parser.parse_args()
    results = []
    with tempfile.TemporaryDirectory(prefix='parquet-variant-write-') as directory:
        root = Path(directory)
        def fixture(name, fields, arrays, expected, mask=None, invalid=False, reference_error=None, reference_expected=None, root_metadata_extra=None, expected_ids=None):
            inputs, written = root/(name+'-input'), root/(name+'-written')
            inputs.mkdir()
            source = pa.StructArray.from_arrays(arrays, fields=fields, mask=mask)
            extension = {b'ARROW:extension:name':b'arrow.parquet.variant',b'ARROW:extension:metadata':b''}
            extension.update(root_metadata_extra or {})
            field = pa.field('v', source.type, nullable=True, metadata=extension)
            schema = pa.schema([field])
            with ipc.new_file(inputs/'case.arrow', schema) as output:
                output.write_batch(pa.record_batch([source], schema=schema))
            result = run([args.writer, '--variant-shred-from-arrow' if args.semantic else '--nested-from-arrow', inputs, written])
            unobservable = name == 'empty-object'
            pyarrow_limitation = None
            if invalid or unobservable:
                assert result.returncode != 0, (name, result.stdout, result.stderr)
                if unobservable:
                    assert 'no physical leaf' in result.stderr, result.stderr
                assert not (written/'case.parquet').exists()
            else:
                assert result.returncode == 0, (name, result.stdout, result.stderr)
                path = written/'case.parquet'
                if expected_ids is not None:
                    _, native_metadata = footer(path)
                    actual_ids = [(node.value[4].value, node.value[9].value) for node in native_metadata.value[2].value if 9 in node.value]
                    assert actual_ids == expected_ids, (name, actual_ids, expected_ids)
                if name == 'zero-rows':
                    try:
                        pq.read_table(path)
                    except pa.ArrowInvalid as error:
                        assert 'Parquet non-leaf node has no children' in str(error)
                        pyarrow_limitation = str(error)
                    else:
                        raise AssertionError('PyArrow zero-leaf limitation changed; refresh this explicit expectation')
                else:
                    physical = pq.read_table(path).column(0).combine_chunks()
                    assert len(physical) == len(source)
                    for row in range(len(source)):
                        if args.semantic:
                            if source[row].is_valid:
                                assert binary_at(child(source.slice(row, 1), 'metadata')) == binary_at(child(physical.slice(row, 1), 'metadata'))
                        else:
                            check_fragments(source.slice(row, 1), physical.slice(row, 1))
                values = root/(name+'-values')
                run([args.reader, '--variant-export', path, 'v', values], expected=0)
                for row in range(len(expected)):
                    for suffix in ('meta','value','null'):
                        old = values/f'0-{row}.{suffix}'
                        if old.exists():
                            old.rename(values/f'{row:06}.{suffix}')
                outputs(values, expected)
                exported = root/(name+'-arrow')
                run([args.reader, written, '--arrow-output', exported], expected=0)
                table = ipc.open_stream(exported/'case.parquet.arrows').read_all()
                check_schema(field, table.schema.field('v'))
                reference_input = root/(name+'-inferred.arrow')
                with ipc.new_file(reference_input, table.schema) as output:
                    output.write_table(table)
                reference = root/(name+'-rust')
                rust = run([args.reference, reference_input, reference])
                if reference_error:
                    assert rust.returncode != 0 and reference_error in rust.stderr, (name, rust.returncode, rust.stderr)
                else:
                    assert rust.returncode == 0, (name, rust.stdout, rust.stderr)
                    outputs(reference, reference_expected if reference_expected is not None else expected)
            results.append({'name':name,'rows':len(expected),'invalidRejected':invalid,'unobservableStorageRejected':unobservable,'pyarrowLimitation':pyarrow_limitation,'referenceLimitation':reference_error,'referenceSemanticDifference':reference_expected is not None})
        generate_cases(fixture)
        def fallback_case(name, typ, encoded, expected):
            fields = [pa.field('metadata', pa.binary(), False), pa.field('value', pa.binary()), pa.field('typed_value', typ)]
            fixture(name, fields, [pa.array([EMPTY]*len(encoded)), pa.array(encoded, type=pa.binary()), pa.array([None]*len(encoded), type=typ)], expected)
        fallback_case('exact-signed-integer-tags', pa.int64(), [bytes([12,1]),bytes([16])+struct.pack('<h',1),bytes([20])+struct.pack('<i',1),bytes([24])+struct.pack('<q',1)], [('int8',1),('int16',1),('int32',1),('int64',1)])
        fallback_case('decimal-scale-and-precision-fallback',pa.decimal32(8,2),[bytes([32,3])+struct.pack('<i',123),bytes([32,2])+struct.pack('<i',100000000)],[('decimal4',3,123),('decimal4',2,100000000)])
        fallback_case('decimal-width-fallback',pa.decimal64(10,2),[bytes([32,2])+struct.pack('<i',1)],[('decimal4',2,1)])
        fallback_case('timestamp-unit-and-zone-fallback',pa.timestamp('us',tz='UTC'),[bytes([72])+struct.pack('<q',1),bytes([52])+struct.pack('<q',1)],[('timestamp-nanos',1),('timestamp-ntz',1)])

        element_type = pa.struct([pa.field('typed_value',pa.int32())])
        element = pa.StructArray.from_arrays([pa.array([None], type=pa.int32())], fields=list(element_type))
        array_type = pa.list_(pa.field('element',element_type,False))
        array = pa.ListArray.from_arrays(pa.array([0,0,0,0,1],type=pa.int32()),element,type=array_type,mask=pa.array([False,True,True,False]))
        fixture('empty-null-and-missing-arrays',[pa.field('metadata',pa.binary(),False),pa.field('value',pa.binary()),pa.field('typed_value',array_type)],[pa.array([EMPTY]*4),pa.array([None,b'\x00',None,None],type=pa.binary()),array],[[],('null',),('null',),[('null',)]])
        ids=lambda number:{b'PARQUET:field_id':str(number).encode()}
        identified_element_type=pa.struct([pa.field('typed_value',pa.int32(),metadata=ids(14))])
        identified_element=pa.StructArray.from_arrays([pa.array([1],type=pa.int32())],fields=list(identified_element_type))
        identified_array_type=pa.list_(pa.field('element',identified_element_type,False,metadata=ids(13)))
        identified_array=pa.ListArray.from_arrays(pa.array([0,1],type=pa.int32()),identified_element,type=identified_array_type)
        fixture('nested-field-ids',[pa.field('metadata',pa.binary(),False,metadata=ids(11)),pa.field('typed_value',identified_array_type,metadata=ids(12))],[pa.array([EMPTY]),identified_array],[[('int32',1)]],root_metadata_extra=ids(10),expected_ids=[(b'v',10),(b'metadata',11),(b'typed_value',12),(b'element',13),(b'typed_value',14)])
        fixture('visible-required-typed-null',[pa.field('metadata',pa.binary(),False),pa.field('typed_value',pa.int32(),False)],[pa.array([EMPTY]),pa.array([None],type=pa.int32())],[],invalid=True)
    report={'semantic':args.semantic,'pyarrow':pa.__version__,'cases':len(results),'nativeFiles':sum(not case['invalidRejected'] and not case['unobservableStorageRejected'] for case in results),'rows':sum(case['rows'] for case in results if not case['unobservableStorageRejected']),'failures':0,'skips':0,'results':results}
    args.output.write_text(json.dumps(report,indent=2)+'\n')
    print(json.dumps({key:value for key,value in report.items() if key!='results'}))


if __name__ == '__main__':
    main()
