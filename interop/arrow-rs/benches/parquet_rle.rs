//! Matched raw hybrid streams for RleThroughput.hs. Fresh allocation and
//! final flush are included. parquet-rs adaptively mixes RLE/packed runs;
//! Both encoders mix repeated and packed runs; their run thresholds differ.
use criterion::{criterion_group, criterion_main, Criterion};
use parquet::encodings::rle::{RleDecoder, RleEncoder};
use std::hint::black_box;
use std::time::Duration;

const ROWS: usize = 65536;

fn encode(width: u8, values: &[u32]) -> Vec<u8> {
    let mut encoder = RleEncoder::new(width, RleEncoder::max_buffer_size(width, values.len()));
    for &value in values {
        encoder.put(value as u64);
    }
    encoder.consume()
}

fn benchmarks(criterion: &mut Criterion) {
    let mut group = criterion.benchmark_group("hybrid encode");
    for width in [0_u8, 1, 2, 4, 8, 9, 16, 24, 31, 32] {
        let mask = ((1_u64 << width) - 1) as u32;
        let patterns: &[&str] = if width == 0 {
            &["constant"]
        } else {
            &[
                "constant",
                "cycle",
                "random",
                "blocks8",
                "blocks24",
                "blocks64",
                "prefix",
                "misaligned64",
            ]
        };
        for &pattern in patterns {
            let values: Vec<u32> = (0..ROWS as u32)
                .map(|i| {
                    (match pattern {
                        "constant" => mask,
                        "cycle" => i,
                        "random" => i.wrapping_mul(1664525).wrapping_add(1013904223),
                        "blocks8" => i / 8,
                        "blocks24" => i / 24,
                        "blocks64" => i / 64,
                        "prefix" => {
                            if i == ROWS as u32 - 1 {
                                mask
                            } else {
                                0
                            }
                        }
                        "misaligned64" => {
                            if i < 3 {
                                i
                            } else {
                                (i - 3) / 64
                            }
                        }
                        _ => unreachable!(),
                    }) & mask
                })
                .collect();
            let encoded = encode(width, &values);
            let mut decoder = RleDecoder::new(width);
            decoder
                .set_data(encoded.clone().into())
                .expect("hybrid input");
            let mut decoded = vec![0_u32; ROWS];
            assert_eq!(
                decoder.get_batch(&mut decoded).expect("hybrid decode"),
                ROWS
            );
            assert_eq!(decoded, values);
            let name = format!("width {width}/{pattern} 65536 values");
            eprintln!("hybrid-size {name}: {}", encoded.len());
            group.bench_function(name, |bench| {
                bench.iter(|| black_box(encode(width, black_box(&values))))
            });
        }
    }
    group.finish();
}

criterion_group! {
    name = benches;
    config = Criterion::default()
        .warm_up_time(Duration::from_secs(1))
        .measurement_time(Duration::from_secs(2));
    targets = benchmarks
}
criterion_main!(benches);
