//! Fresh dictionary construction, matching DictionaryThroughput.hs.
use criterion::{criterion_group, criterion_main, Criterion};
use parquet::data_type::{
    ByteArray, ByteArrayType, DataType, DoubleType, FloatType, Int32Type, Int64Type,
};
use parquet::decoding::{Decoder, DictDecoder, PlainDecoder};
use parquet::encoding::{DictEncoder, Encoder};
use parquet::schema::types::{ColumnDescriptor, ColumnPath, Type as SchemaType};
use std::hint::black_box;
use std::sync::Arc;

const ROWS: usize = 8192;

fn run<T: DataType>(criterion: &mut Criterion, label: &str, cardinality: usize, values: Vec<T::T>)
where
    T::T: PartialEq + std::fmt::Debug,
{
    let ty = SchemaType::primitive_type_builder("x", T::get_physical_type())
        .build()
        .unwrap();
    let desc = Arc::new(ColumnDescriptor::new(
        Arc::new(ty),
        0,
        0,
        ColumnPath::new(vec!["x".into()]),
    ));
    // Validation-only buffers are dropped before timing each fresh encoder.
    {
        let mut encoder = DictEncoder::<T>::new(desc.clone());
        encoder.put(&values).unwrap();
        assert_eq!(encoder.num_entries(), cardinality);
        let mut plain = PlainDecoder::<T>::new(-1);
        plain
            .set_data(encoder.write_dict().unwrap(), cardinality)
            .unwrap();
        let mut decoder = DictDecoder::<T>::new();
        decoder.set_dict(Box::new(plain)).unwrap();
        decoder
            .set_data(encoder.write_indices().unwrap(), ROWS)
            .unwrap();
        let mut decoded = vec![T::T::default(); ROWS];
        assert_eq!(decoder.get(&mut decoded).unwrap(), ROWS);
        assert_eq!(decoded, values);
    }
    criterion.bench_function(
        &format!("dictionary build/{label} {cardinality} unique"),
        |bench| {
            bench.iter(|| {
                let mut encoder = DictEncoder::<T>::new(desc.clone());
                encoder.put(black_box(&values)).unwrap();
                black_box(encoder)
            });
        },
    );
}

fn benchmarks(criterion: &mut Criterion) {
    for cardinality in [8, 1024, ROWS] {
        let keys: Vec<usize> = (0..ROWS).map(|i| (i * 104729) % cardinality).collect();
        run::<Int32Type>(
            criterion,
            "INT32",
            cardinality,
            keys.iter().map(|&key| key as i32).collect(),
        );
        run::<Int64Type>(
            criterion,
            "INT64",
            cardinality,
            keys.iter().map(|&key| key as i64).collect(),
        );
        run::<FloatType>(
            criterion,
            "FLOAT",
            cardinality,
            keys.iter()
                .map(|&key| f32::from_bits(0x3f000000 | key as u32))
                .collect(),
        );
        run::<DoubleType>(
            criterion,
            "DOUBLE",
            cardinality,
            keys.iter()
                .map(|&key| f64::from_bits(0x3fe0000000000000 | key as u64))
                .collect(),
        );
        run::<ByteArrayType>(
            criterion,
            "BYTE_ARRAY",
            cardinality,
            keys.iter()
                .map(|&key| ByteArray::from((key as u64).to_le_bytes().to_vec()))
                .collect(),
        );
        run::<Int32Type>(
            criterion,
            "INT32 sparse",
            cardinality,
            keys.iter()
                .map(|&key| (key as u32).wrapping_mul(1664525).wrapping_add(1013904223) as i32)
                .collect(),
        );
        run::<Int64Type>(
            criterion,
            "INT64 sparse",
            cardinality,
            keys.iter()
                .map(|&key| {
                    (key as u64)
                        .wrapping_mul(6364136223846793005)
                        .wrapping_add(1442695040888963407) as i64
                })
                .collect(),
        );
        run::<FloatType>(
            criterion,
            "FLOAT sparse",
            cardinality,
            keys.iter()
                .map(|&key| {
                    f32::from_bits(
                        0x3f000000
                            | ((key as u32).wrapping_mul(1664525).wrapping_add(1013904223)
                                & 0x007fffff),
                    )
                })
                .collect(),
        );
        run::<DoubleType>(
            criterion,
            "DOUBLE sparse",
            cardinality,
            keys.iter()
                .map(|&key| {
                    f64::from_bits(
                        0x3fe0000000000000
                            | ((key as u64)
                                .wrapping_mul(6364136223846793005)
                                .wrapping_add(1442695040888963407)
                                & 0x000fffffffffffff),
                    )
                })
                .collect(),
        );
        run::<ByteArrayType>(
            criterion,
            "BYTE_ARRAY short text",
            cardinality,
            keys.iter()
                .map(|&key| ByteArray::from(key.to_string().into_bytes()))
                .collect(),
        );
        run::<ByteArrayType>(
            criterion,
            "BYTE_ARRAY common ends multiple words 64 bytes",
            cardinality,
            keys.iter()
                .map(|&key| {
                    let mut value = vec![97; 16];
                    value.extend_from_slice(&((key as u64) % 4).to_le_bytes());
                    value.extend_from_slice(&((key as u64) / 4).to_le_bytes());
                    value.resize(64, 97);
                    ByteArray::from(value)
                })
                .collect(),
        );
        run::<ByteArrayType>(
            criterion,
            "BYTE_ARRAY common ends 64 bytes",
            cardinality,
            keys.iter()
                .map(|&key| {
                    let mut value = vec![97; 24];
                    value.extend_from_slice(&(key as u64).to_le_bytes());
                    value.resize(64, 97);
                    ByteArray::from(value)
                })
                .collect(),
        );
        for width in [64, 4096] {
            run::<ByteArrayType>(
                criterion,
                &format!("BYTE_ARRAY {width} bytes"),
                cardinality,
                keys.iter()
                    .map(|&key| {
                        let mut value = (key as u64).to_le_bytes().to_vec();
                        value.resize(width, 97);
                        ByteArray::from(value)
                    })
                    .collect(),
            );
        }
    }
}
criterion_group! {
    name = benches;
    config = Criterion::default().warm_up_time(std::time::Duration::from_secs(1)).measurement_time(std::time::Duration::from_secs(2));
    targets = benchmarks
}
criterion_main!(benches);
