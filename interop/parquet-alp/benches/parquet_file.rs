//! Parquet counterpart of wireform-parquet/bench/Throughput.hs.
//! Set WIREFORM_PARQUET_BENCH_FIXTURES to that binary's --write-fixtures
//! directory. Reads use the identical Wireform bytes; writes use the same
//! four required columns, PLAIN encoding, V2 pages, codecs and statistics.

use std::hint::black_box;
use std::path::PathBuf;
use std::sync::Arc;

use arrow_array::{ArrayRef, BooleanArray, Float64Array, Int64Array, RecordBatch, StringArray};
use arrow_schema::{DataType, Field, Schema};
use bytes::Bytes;
use criterion::{criterion_group, Criterion};
use parquet::arrow::arrow_reader::ParquetRecordBatchReaderBuilder;
use parquet::arrow::arrow_writer::{ArrowWriter, ArrowWriterOptions};
use parquet::basic::{Compression, Encoding, ZstdLevel};
use parquet::file::properties::{EnabledStatistics, WriterProperties, WriterVersion};

const ROWS: usize = 100_000;

fn dataset() -> RecordBatch {
    let schema = Arc::new(Schema::new(vec![
        Field::new("id", DataType::Int64, false),
        Field::new("score", DataType::Float64, false),
        Field::new("name", DataType::Utf8, false),
        Field::new("active", DataType::Boolean, false),
    ]));
    let columns: Vec<ArrayRef> = vec![
        Arc::new(Int64Array::from_iter_values((0..ROWS).map(|i| i as i64))),
        Arc::new(Float64Array::from_iter_values(
            (0..ROWS).map(|i| i as f64 * 0.5),
        )),
        Arc::new(StringArray::from_iter_values(
            (0..ROWS).map(|i| format!("name_{}", i % 1000)),
        )),
        Arc::new(BooleanArray::from_iter((0..ROWS).map(|i| Some(i % 2 == 0)))),
    ];
    RecordBatch::try_new(schema, columns).expect("dataset")
}

fn write(batch: &RecordBatch, properties: Arc<WriterProperties>) -> Vec<u8> {
    let options = ArrowWriterOptions::new()
        .with_properties(properties.as_ref().clone())
        .with_skip_arrow_metadata(true);
    let mut writer =
        ArrowWriter::try_new_with_options(Vec::new(), batch.schema(), options).expect("writer");
    writer.write(batch).expect("write");
    writer.into_inner().expect("finish")
}

fn read(bytes: Bytes) -> Vec<RecordBatch> {
    ParquetRecordBatchReaderBuilder::try_new(bytes)
        .expect("reader")
        .with_batch_size(ROWS)
        .build()
        .expect("build")
        .collect::<Result<Vec<_>, _>>()
        .expect("decode")
}

struct CodecCase {
    name: &'static str,
    compression: Compression,
}

fn codec_cases() -> [CodecCase; 3] {
    [
        CodecCase {
            name: "uncompressed",
            compression: Compression::UNCOMPRESSED,
        },
        CodecCase {
            name: "snappy",
            compression: Compression::SNAPPY,
        },
        CodecCase {
            name: "zstd",
            compression: Compression::ZSTD(ZstdLevel::try_new(3).unwrap()),
        },
    ]
}

fn properties(compression: Compression) -> Arc<WriterProperties> {
    Arc::new(
        WriterProperties::builder()
            .set_writer_version(WriterVersion::PARQUET_2_0)
            .set_encoding(Encoding::PLAIN)
            .set_dictionary_enabled(false)
            .set_statistics_enabled(EnabledStatistics::Chunk)
            .set_offset_index_disabled(true)
            .set_max_row_group_row_count(Some(ROWS))
            .set_data_page_row_count_limit(ROWS)
            .set_data_page_size_limit(8 * 1024 * 1024)
            .set_compression(compression)
            .build(),
    )
}

fn write_fixtures(root: PathBuf) {
    std::fs::create_dir_all(&root).expect("fixture directory");
    let batch = dataset();
    for case in codec_cases() {
        let bytes = write(&batch, properties(case.compression));
        assert_eq!(read(Bytes::from(bytes.clone())), vec![batch.clone()]);
        std::fs::write(root.join(format!("{}.parquet", case.name)), bytes).expect("fixture");
    }
}

fn benchmarks(criterion: &mut Criterion) {
    let root = PathBuf::from(std::env::var_os("WIREFORM_PARQUET_BENCH_FIXTURES").expect(
        "set WIREFORM_PARQUET_BENCH_FIXTURES to parquet-throughput --write-fixtures output",
    ));
    let batch = dataset();
    for case in codec_cases() {
        let name = case.name;
        let compression = case.compression;
        let properties = properties(compression);
        let bytes =
            Bytes::from(std::fs::read(root.join(format!("{name}.parquet"))).expect("fixture"));
        // Validate every value outside timing. A successful decode alone
        // does not establish that this benchmark is doing equivalent work.
        assert_eq!(read(bytes.clone()), vec![batch.clone()]);
        assert_eq!(
            read(Bytes::from(write(&batch, properties.clone()))),
            vec![batch.clone()]
        );
        criterion.bench_function(&format!("write {ROWS} rows x 4 cols/{name}"), |b| {
            b.iter(|| black_box(write(black_box(&batch), properties.clone())))
        });
        criterion.bench_function(&format!("read {ROWS} rows x 4 cols ({name})/decode"), |b| {
            b.iter(|| black_box(read(black_box(bytes.clone()))))
        });
    }
}

criterion_group!(benches, benchmarks);

fn main() {
    let args: Vec<_> = std::env::args().collect();
    if args.len() == 3 && args[1] == "--write-fixtures" {
        write_fixtures(PathBuf::from(&args[2]));
        return;
    }
    benches();
    Criterion::default().configure_from_args().final_summary();
}
