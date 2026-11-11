//! Matches ByteStreamSplitThroughput.hs, including allocation on each iteration.
//! Independent byte and IEEE-bit models run before each case is timed.
use bytes::Bytes;
use criterion::{criterion_group, criterion_main, Criterion};
use parquet::basic::{Encoding, Type};
use parquet::data_type::{DoubleType, FloatType};
use parquet::decoding::get_decoder;
use parquet::encoding::get_encoder;
use parquet::schema::types::{ColumnDescPtr, ColumnDescriptor, ColumnPath, Type as SchemaType};
use std::hint::black_box;
use std::sync::Arc;

fn descriptor(physical: Type) -> ColumnDescPtr {
    Arc::new(ColumnDescriptor::new(
        Arc::new(SchemaType::primitive_type_builder("value", physical).build().unwrap()),
        0,
        0,
        ColumnPath::new(vec!["value".into()]),
    ))
}

fn word32(i: usize) -> u32 {
    match i {
        0 => 0,
        1 => 0x80000000,
        2 => 0x7f800000,
        3 => 0xff800000,
        4 => 0x7fc12345,
        5 => 0x7f812345,
        6 => 1,
        7 => 0x7f7fffff,
        _ => (i as u32).wrapping_mul(1664525).wrapping_add(1013904223),
    }
}

fn word64(i: usize) -> u64 {
    match i {
        0 => 0,
        1 => 0x8000000000000000,
        2 => 0x7ff0000000000000,
        3 => 0xfff0000000000000,
        4 => 0x7ff8123456789abc,
        5 => 0x7ff0123456789abc,
        6 => 1,
        7 => 0x7fefffffffffffff,
        _ => (i as u64)
            .wrapping_mul(6364136223846793005)
            .wrapping_add(1442695040888963407),
    }
}

fn encode32(values: &[f32], desc: &ColumnDescPtr) -> Bytes {
    let mut encoder = get_encoder::<FloatType>(Encoding::BYTE_STREAM_SPLIT, desc).unwrap();
    encoder.put(values).unwrap();
    encoder.flush_buffer().unwrap()
}

fn encode64(values: &[f64], desc: &ColumnDescPtr) -> Bytes {
    let mut encoder = get_encoder::<DoubleType>(Encoding::BYTE_STREAM_SPLIT, desc).unwrap();
    encoder.put(values).unwrap();
    encoder.flush_buffer().unwrap()
}

fn decode32(encoded: Bytes, n: usize, desc: &ColumnDescPtr) -> Vec<f32> {
    let mut decoder = get_decoder::<FloatType>(desc.clone(), Encoding::BYTE_STREAM_SPLIT).unwrap();
    decoder.set_data(encoded, n).unwrap();
    let mut output = vec![0.0; n];
    assert_eq!(decoder.get(&mut output).unwrap(), n);
    output
}

fn decode64(encoded: Bytes, n: usize, desc: &ColumnDescPtr) -> Vec<f64> {
    let mut decoder = get_decoder::<DoubleType>(desc.clone(), Encoding::BYTE_STREAM_SPLIT).unwrap();
    decoder.set_data(encoded, n).unwrap();
    let mut output = vec![0.0; n];
    assert_eq!(decoder.get(&mut output).unwrap(), n);
    output
}

fn benchmarks(criterion: &mut Criterion) {
    let desc32 = descriptor(Type::FLOAT);
    let desc64 = descriptor(Type::DOUBLE);
    let mut group = criterion.benchmark_group("byte stream split");
    for n in [17, 1024, 65536] {
        let values: Vec<f32> = (0..n).map(|i| f32::from_bits(word32(i))).collect();
        let encoded = encode32(&values, &desc32);
        {
            let model: Vec<u8> = (0..4)
                .flat_map(|lane| (0..n).map(move |i| (word32(i) >> (lane * 8)) as u8))
                .collect();
            assert_eq!(encoded.as_ref(), model);
            let decoded = decode32(encoded.clone(), n, &desc32);
            assert!(decoded.iter().enumerate().all(|(i, value)| value.to_bits() == word32(i)));
        }
        group.bench_function(format!("FLOAT {n} values/encode"), |bench| {
            bench.iter(|| black_box(encode32(black_box(&values), &desc32)))
        });
        group.bench_function(format!("FLOAT {n} values/decode"), |bench| {
            bench.iter(|| black_box(decode32(black_box(encoded.clone()), n, &desc32)))
        });
    }
    for n in [17, 1024, 65536] {
        let values: Vec<f64> = (0..n).map(|i| f64::from_bits(word64(i))).collect();
        let encoded = encode64(&values, &desc64);
        {
            let model: Vec<u8> = (0..8)
                .flat_map(|lane| (0..n).map(move |i| (word64(i) >> (lane * 8)) as u8))
                .collect();
            assert_eq!(encoded.as_ref(), model);
            let decoded = decode64(encoded.clone(), n, &desc64);
            assert!(decoded.iter().enumerate().all(|(i, value)| value.to_bits() == word64(i)));
        }
        group.bench_function(format!("DOUBLE {n} values/encode"), |bench| {
            bench.iter(|| black_box(encode64(black_box(&values), &desc64)))
        });
        group.bench_function(format!("DOUBLE {n} values/decode"), |bench| {
            bench.iter(|| black_box(decode64(black_box(encoded.clone()), n, &desc64)))
        });
    }
    group.finish();
}

criterion_group!(benches, benchmarks);
criterion_main!(benches);
