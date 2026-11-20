use arrow_array::{
    builder::{Int64Builder, ListBuilder},
    ArrayRef, Int64Array, RecordBatch,
};
use arrow_schema::{DataType, Field, Schema};
use criterion::{criterion_group, BenchmarkId, Criterion};
use parquet::{
    arrow::ArrowWriter,
    basic::{Compression, Encoding},
    encryption::encrypt::FileEncryptionProperties,
    file::properties::{EnabledStatistics, WriterProperties, WriterVersion},
};
use std::{hint::black_box, path::PathBuf, sync::Arc};

fn batch(n: usize, nested: bool) -> RecordBatch {
    let public = Arc::new(Int64Array::from_iter_values((0..n).map(|i| i as i64))) as ArrayRef;
    let (typ, secret): (DataType, ArrayRef) = if nested {
        let field = Arc::new(Field::new("element", DataType::Int64, true));
        let mut builder = ListBuilder::new(Int64Builder::new()).with_field(field.clone());
        for i in 0..n {
            if i % 11 == 0 {
                builder.append(false);
            } else if i % 7 == 0 {
                builder.append(true);
            } else {
                for j in 0..3 {
                    if j == 0 && i % 5 == 0 {
                        builder.values().append_null();
                    } else {
                        builder.values().append_value((i * 3 + j) as i64);
                    }
                }
                builder.append(true);
            }
        }
        (DataType::List(field), Arc::new(builder.finish()))
    } else {
        (
            DataType::Int64,
            Arc::new(Int64Array::from_iter((0..n).map(|i| {
                if i % 5 == 0 {
                    None
                } else {
                    Some((i * 3) as i64)
                }
            }))),
        )
    };
    let schema = Arc::new(Schema::new(vec![
        Field::new("public", DataType::Int64, false),
        Field::new("secret", typ, true),
    ]));
    RecordBatch::try_new(schema, vec![public, secret]).unwrap()
}

fn encode(batch: &RecordBatch, nested: bool, signed: bool) -> Vec<u8> {
    let key_path = if nested {
        "secret.list.element"
    } else {
        "secret"
    };
    let encryption = FileEncryptionProperties::builder(vec![0x42; 16])
        .with_footer_key_metadata(b"kf".to_vec())
        .with_column_key_and_metadata(key_path, vec![0x44; 16], b"kc2".to_vec())
        .with_aad_prefix(b"writer-regression".to_vec())
        .with_aad_prefix_storage(true)
        .with_plaintext_footer(signed)
        .build()
        .unwrap();
    let props = WriterProperties::builder()
        .set_writer_version(WriterVersion::PARQUET_2_0)
        .set_encoding(Encoding::PLAIN)
        .set_dictionary_enabled(false)
        .set_compression(Compression::UNCOMPRESSED)
        .set_statistics_enabled(EnabledStatistics::Page)
        .set_offset_index_disabled(false)
        .set_write_page_header_statistics(false)
        .set_data_page_size_limit(16 * 1024 * 1024)
        .set_data_page_row_count_limit(1024 * 1024)
        .with_file_encryption_properties(encryption)
        .build();
    let mut writer = ArrowWriter::try_new(Vec::new(), batch.schema(), Some(props)).unwrap();
    writer.write(batch).unwrap();
    writer.into_inner().unwrap()
}

fn benches(c: &mut Criterion) {
    let fixtures = std::env::var_os("WIREFORM_ENCRYPTION_REFERENCE_FIXTURES").map(PathBuf::from);
    if let Some(root) = &fixtures {
        std::fs::create_dir_all(root).unwrap();
    }
    for n in [64, 8192] {
        for nested in [false, true] {
            let shape = if nested { "nested" } else { "mixed" };
            let batch = batch(n, nested);
            let mut group = c.benchmark_group(format!("{shape}/{n}"));
            let seconds = std::env::var("WIREFORM_ENCRYPTION_BENCH_SECONDS")
                .map(|value| value.parse::<u64>().expect("measurement seconds"))
                .unwrap_or(2);
            let samples = std::env::var("WIREFORM_ENCRYPTION_BENCH_SAMPLES")
                .map(|value| value.parse::<usize>().expect("sample count"))
                .unwrap_or(30);
            group
                .sample_size(samples)
                .warm_up_time(std::time::Duration::from_secs(1))
                .measurement_time(std::time::Duration::from_secs(seconds));
            for signed in [false, true] {
                let mode = if signed { "signed" } else { "encrypted" };
                if let Some(root) = &fixtures {
                    std::fs::write(
                        root.join(format!("{shape}-{n}-{mode}.parquet")),
                        encode(&batch, nested, signed),
                    )
                    .unwrap();
                }
                group.bench_with_input(BenchmarkId::from_parameter(mode), &signed, |b, &signed| {
                    b.iter(|| black_box(encode(black_box(&batch), nested, signed)))
                });
            }
            group.finish();
        }
    }
}
criterion_group!(encryption, benches);
#[cfg(feature = "allocation-counts")]
mod allocation {
    use std::alloc::{GlobalAlloc, Layout, System};
    use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering::Relaxed};
    pub static ENABLED: AtomicBool = AtomicBool::new(false);
    pub static BYTES: AtomicUsize = AtomicUsize::new(0);
    pub struct Counting;
    unsafe impl GlobalAlloc for Counting {
        unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
            if ENABLED.load(Relaxed) {
                BYTES.fetch_add(layout.size(), Relaxed);
            }
            unsafe { System.alloc(layout) }
        }
        unsafe fn dealloc(&self, p: *mut u8, layout: Layout) {
            unsafe { System.dealloc(p, layout) }
        }
        unsafe fn realloc(&self, p: *mut u8, layout: Layout, n: usize) -> *mut u8 {
            if ENABLED.load(Relaxed) {
                BYTES.fetch_add(n, Relaxed);
            }
            unsafe { System.realloc(p, layout, n) }
        }
    }
}
#[cfg(feature = "allocation-counts")]
#[global_allocator]
static ALLOCATOR: allocation::Counting = allocation::Counting;

fn main() {
    #[cfg(feature = "allocation-counts")]
    {
        use std::sync::atomic::Ordering::Relaxed;
        let mut records = Vec::new();
        for n in [64, 8192] {
            for nested in [false, true] {
                for signed in [false, true] {
                    let input = batch(n, nested);
                    black_box(encode(&input, nested, signed));
                    allocation::BYTES.store(0, Relaxed);
                    allocation::ENABLED.store(true, Relaxed);
                    for _ in 0..10 {
                        black_box(encode(black_box(&input), nested, signed));
                    }
                    allocation::ENABLED.store(false, Relaxed);
                    let name = format!(
                        "{}/{}/{}",
                        if nested { "nested" } else { "mixed" },
                        n,
                        if signed { "signed" } else { "encrypted" }
                    );
                    records.push(format!(
                        "{{\"name\":\"{}\",\"requestedAllocatedBytesPerFile\":{}}}",
                        name,
                        allocation::BYTES.load(Relaxed) / 10
                    ));
                }
            }
        }
        std::fs::write(
            std::env::var("WIREFORM_ENCRYPTION_ALLOCATION_REPORT").unwrap(),
            format!("[{}]\n", records.join(",")),
        )
        .unwrap();
    }
    #[cfg(not(feature = "allocation-counts"))]
    {
        encryption();
        Criterion::default().configure_from_args().final_summary();
    }
}
