//! Raw DELTA_BINARY_PACKED encoding, matching DeltaThroughput.hs inputs.
//! Includes fresh encoder allocation, put, and flush on every iteration.
use criterion::{criterion_group, criterion_main, Criterion};
use parquet::data_type::{Int32Type, Int64Type};
use parquet::decoding::{Decoder, DeltaBitPackDecoder};
use parquet::encoding::{DeltaBitPackEncoder, Encoder};
use std::hint::black_box;

const ROWS: usize = 65536;

fn encode32(values: &[i32]) -> bytes::Bytes {
    let mut encoder = DeltaBitPackEncoder::<Int32Type>::new();
    encoder.put(values).expect("encode INT32");
    encoder.flush_buffer().expect("flush INT32")
}

fn encode64(values: &[i64]) -> bytes::Bytes {
    let mut encoder = DeltaBitPackEncoder::<Int64Type>::new();
    encoder.put(values).expect("encode INT64");
    encoder.flush_buffer().expect("flush INT64")
}

fn benchmarks(criterion: &mut Criterion) {
    let bounded: Vec<u32> = (0..ROWS)
        .map(|i| (i as u32).wrapping_mul(1103515245).wrapping_add(12345) & 0xffff)
        .collect();
    let mut state32 = 0x12345678_u32;
    let full32: Vec<i32> = (0..ROWS)
        .map(|_| {
            let value = state32 as i32;
            state32 = state32.wrapping_mul(1664525).wrapping_add(1013904223);
            value
        })
        .collect();
    let mut state64 = 0x123456789abcdef0_u64;
    let full64: Vec<i64> = (0..ROWS)
        .map(|_| {
            let value = state64 as i64;
            state64 = state64
                .wrapping_mul(6364136223846793005)
                .wrapping_add(1442695040888963407);
            value
        })
        .collect();
    let cases32 = [
        ("INT32 ascending", (0..ROWS as i32).collect::<Vec<_>>()),
        ("INT32 bounded", bounded.iter().map(|&x| x as i32).collect()),
        ("INT32 full", full32),
        (
            "INT32 wrapping",
            (0..ROWS)
                .map(|i| if i % 2 == 0 { i32::MIN } else { i32::MAX })
                .collect(),
        ),
    ];
    let cases64 = [
        ("INT64 ascending", (0..ROWS as i64).collect::<Vec<_>>()),
        ("INT64 bounded", bounded.iter().map(|&x| x as i64).collect()),
        ("INT64 full", full64),
        (
            "INT64 wrapping",
            (0..ROWS)
                .map(|i| if i % 2 == 0 { i64::MIN } else { i64::MAX })
                .collect(),
        ),
    ];
    let mut group = criterion.benchmark_group("delta encode");
    for (name, values) in &cases32 {
        let mut decoder = DeltaBitPackDecoder::<Int32Type>::new();
        decoder
            .set_data(encode32(values), ROWS)
            .expect("INT32 input");
        let mut decoded = vec![0; ROWS];
        assert_eq!(decoder.get(&mut decoded).expect("INT32 decode"), ROWS);
        assert_eq!(decoded, *values);
        group.bench_function(*name, |bench| {
            bench.iter(|| black_box(encode32(black_box(values))))
        });
    }
    for (name, values) in &cases64 {
        let mut decoder = DeltaBitPackDecoder::<Int64Type>::new();
        decoder
            .set_data(encode64(values), ROWS)
            .expect("INT64 input");
        let mut decoded = vec![0; ROWS];
        assert_eq!(decoder.get(&mut decoded).expect("INT64 decode"), ROWS);
        assert_eq!(decoded, *values);
        group.bench_function(*name, |bench| {
            bench.iter(|| black_box(encode64(black_box(values))))
        });
    }
    group.finish();
}

criterion_group!(benches, benchmarks);
criterion_main!(benches);
