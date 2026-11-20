//! Same page predicates, bytes and complete selected values as the Haskell
//! parquet-selection-throughput harness. Footer/index parsing, predicate
//! evaluation and materialization are timed; fixture IO and validation are not.

use std::hint::black_box;
use std::path::PathBuf;

use arrow::array::RecordBatch;
use bytes::Bytes;
use criterion::{criterion_group, criterion_main, Criterion};
use parquet::arrow::arrow_reader::{
    ArrowReaderOptions, ParquetRecordBatchReaderBuilder, RowSelection,
};
use parquet::file::metadata::PageIndexPolicy;
use parquet::file::page_index::column_index::ColumnIndexMetaData;

const ROWS: usize = 131_072;

enum Pivot {
    Int(i64),
    Text(Vec<u8>),
}

fn selected(bytes: Bytes, pivot: &Pivot) -> Vec<RecordBatch> {
    let options = ArrowReaderOptions::new().with_page_index_policy(PageIndexPolicy::Required);
    let builder = ParquetRecordBatchReaderBuilder::try_new_with_options(bytes, options)
        .expect("indexed reader");
    let metadata = builder.metadata();
    let rows = usize::try_from(metadata.file_metadata().num_rows()).expect("row count");
    let index = &metadata.column_index().expect("column index")[0][0];
    let offsets = metadata.offset_index().expect("offset index")[0][0].page_locations();
    let keep: Vec<bool> = match (index, pivot) {
        (ColumnIndexMetaData::INT64(index), Pivot::Int(pivot)) => index
            .max_values_iter()
            .map(|maximum| maximum.is_some_and(|value| value >= pivot))
            .collect(),
        (ColumnIndexMetaData::BYTE_ARRAY(index), Pivot::Text(pivot)) => index
            .max_values_iter()
            .map(|maximum| maximum.is_some_and(|value| value >= pivot.as_slice()))
            .collect(),
        _ => panic!("selection index/predicate type mismatch"),
    };
    assert_eq!(keep.len(), offsets.len());
    let ranges = offsets.iter().enumerate().filter_map(|(i, page)| {
        let start = page.first_row_index as usize;
        let end = offsets
            .get(i + 1)
            .map_or(rows, |next| next.first_row_index as usize);
        keep[i].then_some(start..end)
    });
    let selection = RowSelection::from_consecutive_ranges(ranges, rows);
    builder
        .with_batch_size(ROWS)
        .with_row_selection(selection)
        .build()
        .expect("build selection")
        .collect::<Result<Vec<_>, _>>()
        .expect("selected values")
}

fn benchmarks(criterion: &mut Criterion) {
    let root = PathBuf::from(
        std::env::var_os("WIREFORM_PARQUET_SELECTION_FIXTURES")
            .expect("set WIREFORM_PARQUET_SELECTION_FIXTURES"),
    );
    let plan = std::fs::read_to_string(root.join("selection-plan.txt")).expect("plan");
    for line in plan.lines() {
        let parts: Vec<_> = line.split_whitespace().collect();
        assert_eq!(parts.len(), 3);
        let name = parts[0];
        let pivot = match parts[1] {
            "int" => Pivot::Int(parts[2].parse().expect("integer pivot")),
            "text" => Pivot::Text(parts[2].as_bytes().to_vec()),
            _ => panic!("invalid pivot type"),
        };
        let bytes =
            Bytes::from(std::fs::read(root.join(format!("{name}.parquet"))).expect("fixture"));
        let expected_bytes = Bytes::from(
            std::fs::read(root.join(format!("{name}-expected.parquet"))).expect("expected"),
        );
        let expected = ParquetRecordBatchReaderBuilder::try_new(expected_bytes)
            .expect("expected reader")
            .with_batch_size(ROWS)
            .build()
            .expect("expected build")
            .collect::<Result<Vec<_>, _>>()
            .expect("expected values");
        assert_eq!(selected(bytes.clone(), &pivot), expected);
        criterion.bench_function(&format!("selected pages/{name}"), |b| {
            b.iter(|| black_box(selected(black_box(bytes.clone()), black_box(&pivot))))
        });
    }
}

criterion_group!(benches, benchmarks);
criterion_main!(benches);
