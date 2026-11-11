//! Same official encoded pages and independent PLAIN bits as AlpThroughput.hs.
//! Every iteration constructs a fresh decoder and output vector.
use bytes::Bytes;
use criterion::{criterion_group, criterion_main, Criterion};
use parquet::basic::{Encoding, Type};
use parquet::data_type::{DoubleType, FloatType};
use parquet::decoding::get_decoder;
use parquet::schema::types::{ColumnDescPtr, ColumnDescriptor, ColumnPath, Type as SchemaType};
use std::{env, fs, hint::black_box, path::PathBuf, sync::Arc, time::Duration};

fn descriptor(physical: Type) -> ColumnDescPtr {
    Arc::new(ColumnDescriptor::new(
        Arc::new(SchemaType::primitive_type_builder("value", physical).build().unwrap()),
        0, 0, ColumnPath::new(vec!["value".into()]),
    ))
}

fn decode32(encoded: Bytes, n: usize, desc: &ColumnDescPtr) -> Vec<f32> {
    let mut decoder = get_decoder::<FloatType>(desc.clone(), Encoding::ALP).unwrap();
    decoder.set_data(encoded, n).unwrap();
    let mut output = vec![0.0; n];
    assert_eq!(decoder.get(&mut output).unwrap(), n);
    output
}

fn decode64(encoded: Bytes, n: usize, desc: &ColumnDescPtr) -> Vec<f64> {
    let mut decoder = get_decoder::<DoubleType>(desc.clone(), Encoding::ALP).unwrap();
    decoder.set_data(encoded, n).unwrap();
    let mut output = vec![0.0; n];
    assert_eq!(decoder.get(&mut output).unwrap(), n);
    output
}

fn benchmarks(criterion: &mut Criterion) {
    let root = PathBuf::from(env::var("WIREFORM_PARQUET_ALP_FIXTURES").expect("set ALP fixtures directory"));
    let plan = fs::read_to_string(root.join("cases.tsv")).unwrap();
    let desc32 = descriptor(Type::FLOAT);
    let desc64 = descriptor(Type::DOUBLE);
    let mut group = criterion.benchmark_group("ALP");
    group.warm_up_time(Duration::from_secs(1));
    group.measurement_time(Duration::from_secs(2));
    for line in plan.lines() {
        let fields: Vec<_> = line.split('\t').collect();
        assert_eq!(fields.len(), 4);
        let width: usize = fields[1].parse().unwrap();
        let n: usize = fields[2].parse().unwrap();
        let encoded = Bytes::from(fs::read(root.join(format!("{}.alp", fields[0]))).unwrap());
        let expected = fs::read(root.join(format!("{}.plain", fields[0]))).unwrap();
        assert_eq!(expected.len(), n * width);
        if width == 4 {
            let output = decode32(encoded.clone(), n, &desc32);
            for (value, bits) in output.iter().zip(expected.chunks_exact(4)) {
                assert_eq!(value.to_bits(), u32::from_le_bytes(bits.try_into().unwrap()), "{}", fields[3]);
            }
            group.bench_function(format!("{}/decode", fields[3]), |bench| {
                bench.iter(|| black_box(decode32(black_box(encoded.clone()), n, &desc32)))
            });
        } else {
            assert_eq!(width, 8);
            let output = decode64(encoded.clone(), n, &desc64);
            for (value, bits) in output.iter().zip(expected.chunks_exact(8)) {
                assert_eq!(value.to_bits(), u64::from_le_bytes(bits.try_into().unwrap()), "{}", fields[3]);
            }
            group.bench_function(format!("{}/decode", fields[3]), |bench| {
                bench.iter(|| black_box(decode64(black_box(encoded.clone()), n, &desc64)))
            });
        }
    }
    group.finish();
}

criterion_group!(benches, benchmarks);
criterion_main!(benches);
