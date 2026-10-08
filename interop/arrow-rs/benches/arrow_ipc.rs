//! arrow-rs reference benchmark mirroring `wireform-arrow/bench/Bench.hs`.
//!
//! Every workload, row count, generator and operation matches the Haskell
//! harness so `scripts/run-benchmarks.py` can put both libraries side by side
//! in the `arrow-encode-decode*` and `arrow-api-paths` summaries. Report names
//! follow the Haskell ones (`encode/<workload>`, `decode (100 rows)/<workload>`,
//! ...) so `scripts/bench-manifest.json` maps both series with the same
//! patterns.
//!
//! * encode: `StreamWriter` into a `Vec<u8>` (schema, batch, EOS), the same
//!   message sequence as `encodeArrowStream`.
//! * decode: `StreamReader` over the bytes, collecting every batch, with the
//!   reader's default validation on.
//! * `decode + to_vec`: decode, then copy every column into owned Rust
//!   values (`Vec<Option<T>>`, `String`, nested `Vec`s; dictionaries
//!   expanded), the counterpart of `decode + toVector` in Bench.hs.
//! * `decode skip_validation*`: the same read with validation off. Not mapped
//!   into any summary; recorded for information only.
//!
//! arrow-rs has no lazy (chunked, aliasing) stream writer, so the Haskell
//! `encode lazy` rows are compared against `encode` (`StreamWriter`) in the
//! manifest.
//!
//! Inputs (record batches, encoded bytes, rows) are built before timing, so
//! only the codec call is measured.
//!
//! Run: `cargo bench --manifest-path interop/arrow-rs/Cargo.toml --bench arrow_ipc`.

use std::hint::black_box;
use std::io::Cursor;
use std::sync::Arc;

use arrow::array::{
    Array, ArrayRef, AsArray, BooleanArray, DictionaryArray, Float64Array, Int32Array,
    Int64Array, ListArray, RecordBatch, StringArray, StructArray,
};
use arrow::buffer::OffsetBuffer;
use arrow::datatypes::{DataType, Field, Fields, Float64Type, Int32Type, Int64Type, Schema};
use arrow::ipc::reader::{FileReader, StreamReader};
use arrow::ipc::writer::{FileWriter, StreamWriter};
use criterion::{criterion_group, criterion_main, Criterion};

const BIG_ROWS: usize = 100_000;
const SMALL_ROWS: usize = 100;

/// Same pool as `labelPool` in Bench.hs: 16 labels, 4 to 11 bytes.
const LABEL_POOL: [&str; 16] = [
    "alpha",
    "bravo",
    "charlie",
    "delta",
    "echo",
    "foxtrot",
    "golf",
    "hotel",
    "india",
    "juliett",
    "kilo",
    "lima",
    "mike",
    "november",
    "oscar",
    "papa-quebec",
];

fn label(i: usize) -> &'static str {
    LABEL_POOL[i % LABEL_POOL.len()]
}

fn int64_col(n: usize) -> ArrayRef {
    Arc::new(Int64Array::from_iter_values(
        (0..n).map(|i| i as i64 * 7919),
    ))
}

fn double_col(n: usize) -> ArrayRef {
    Arc::new(Float64Array::from_iter_values(
        (0..n).map(|i| i as f64 * 1.25),
    ))
}

/// Every tenth row is null, as in `nullableInt64Col`.
fn nullable_int64_col(n: usize) -> ArrayRef {
    Arc::new(Int64Array::from_iter(
        (0..n).map(|i| if i % 10 == 0 { None } else { Some(i as i64) }),
    ))
}

fn utf8_col(n: usize) -> ArrayRef {
    Arc::new(StringArray::from_iter_values((0..n).map(label)))
}

fn nullable_utf8_col(n: usize) -> ArrayRef {
    Arc::new(StringArray::from_iter(
        (0..n).map(|i| if i % 10 == 0 { None } else { Some(label(i)) }),
    ))
}

fn bool_col(n: usize) -> ArrayRef {
    Arc::new(BooleanArray::from_iter((0..n).map(|i| Some(i % 2 == 0))))
}

struct Workload {
    name: &'static str,
    batch: RecordBatch,
}

fn single(name: &'static str, field: Field, col: ArrayRef) -> Workload {
    let schema = Arc::new(Schema::new(vec![field]));
    Workload {
        name,
        batch: RecordBatch::try_new(schema, vec![col]).expect(name),
    }
}

fn mixed(n: usize) -> Workload {
    let schema = Arc::new(Schema::new(vec![
        Field::new("tradeId", DataType::Int64, false),
        Field::new("tradePrice", DataType::Float64, false),
        Field::new("tradeQty", DataType::Int64, true),
        Field::new("tradeSymbol", DataType::Utf8, false),
        Field::new("tradeNote", DataType::Utf8, true),
        Field::new("tradeSettled", DataType::Boolean, false),
    ]));
    let cols = vec![
        int64_col(n),
        double_col(n),
        nullable_int64_col(n),
        utf8_col(n),
        nullable_utf8_col(n),
        bool_col(n),
    ];
    Workload {
        name: "mixed 6-col",
        batch: RecordBatch::try_new(schema, cols).expect("mixed 6-col"),
    }
}

/// Four int32 elements per row.
fn list_int32(n: usize) -> Workload {
    let item = Arc::new(Field::new("item", DataType::Int32, false));
    let offsets = OffsetBuffer::new((0..=n).map(|i| (i * 4) as i32).collect());
    let values = Arc::new(Int32Array::from_iter_values((0..n * 4).map(|i| i as i32)));
    let list = ListArray::new(item.clone(), offsets, values, None);
    single(
        "list<int32>",
        Field::new("v", DataType::List(item), false),
        Arc::new(list),
    )
}

fn struct3(n: usize) -> Workload {
    let fields = Fields::from(vec![
        Field::new("a", DataType::Int32, false),
        Field::new("b", DataType::Float64, false),
        Field::new("c", DataType::Boolean, false),
    ]);
    let a: ArrayRef = Arc::new(Int32Array::from_iter_values((0..n).map(|i| i as i32)));
    let arr = StructArray::new(fields.clone(), vec![a, double_col(n), bool_col(n)], None);
    single(
        "struct<int32,double,bool>",
        Field::new("v", DataType::Struct(fields), false),
        Arc::new(arr),
    )
}

/// Sixteen distinct values, int32 indices cycling over them.
fn dict_utf8(n: usize) -> Workload {
    let keys = Int32Array::from_iter_values((0..n).map(|i| (i % LABEL_POOL.len()) as i32));
    let values = Arc::new(StringArray::from_iter_values(LABEL_POOL));
    let dict = DictionaryArray::<Int32Type>::try_new(keys, values).expect("dictionary<utf8>");
    single(
        "dictionary<utf8>",
        Field::new(
            "v",
            DataType::Dictionary(Box::new(DataType::Int32), Box::new(DataType::Utf8)),
            false,
        ),
        Arc::new(dict),
    )
}

fn workloads(n: usize) -> Vec<Workload> {
    vec![
        single("int64", Field::new("v", DataType::Int64, false), int64_col(n)),
        single("double", Field::new("v", DataType::Float64, false), double_col(n)),
        single(
            "nullable int64",
            Field::new("v", DataType::Int64, true),
            nullable_int64_col(n),
        ),
        single("utf8", Field::new("v", DataType::Utf8, false), utf8_col(n)),
        single(
            "nullable utf8",
            Field::new("v", DataType::Utf8, true),
            nullable_utf8_col(n),
        ),
        mixed(n),
        list_int32(n),
        struct3(n),
        dict_utf8(n),
    ]
}

fn encode_stream(batch: &RecordBatch) -> Vec<u8> {
    let mut w = StreamWriter::try_new(Vec::new(), &batch.schema()).expect("stream writer");
    w.write(batch).expect("stream write");
    w.finish().expect("stream finish");
    w.into_inner().expect("stream into_inner")
}

fn decode_stream(bytes: &[u8]) -> Vec<RecordBatch> {
    StreamReader::try_new(Cursor::new(bytes), None)
        .expect("stream reader")
        .collect::<Result<Vec<_>, _>>()
        .expect("stream decode")
}

fn decode_stream_unvalidated(bytes: &[u8]) -> Vec<RecordBatch> {
    let reader = StreamReader::try_new(Cursor::new(bytes), None).expect("stream reader");
    // SAFETY: the bytes come from `encode_stream` in this process and were
    // decoded with validation on during setup.
    let reader = unsafe { reader.with_skip_validation(true) };
    reader
        .collect::<Result<Vec<_>, _>>()
        .expect("stream decode")
}

fn encode_file(batch: &RecordBatch) -> Vec<u8> {
    let mut w = FileWriter::try_new(Vec::new(), &batch.schema()).expect("file writer");
    w.write(batch).expect("file write");
    w.finish().expect("file finish");
    w.into_inner().expect("file into_inner")
}

fn decode_file(bytes: &[u8]) -> Vec<RecordBatch> {
    FileReader::try_new(Cursor::new(bytes), None)
        .expect("file reader")
        .collect::<Result<Vec<_>, _>>()
        .expect("file decode")
}

/// A decoded column copied into owned Rust values, one variant per shape
/// the workloads use (mirrors `Boxed` in Bench.hs).
#[allow(dead_code)]
enum Owned {
    I32(Vec<Option<i32>>),
    I64(Vec<Option<i64>>),
    F64(Vec<Option<f64>>),
    Str(Vec<Option<String>>),
    Bool(Vec<Option<bool>>),
    ListI32(Vec<Option<Vec<Option<i32>>>>),
    Struct(Vec<Owned>),
}

fn to_owned_column(a: &dyn Array) -> Owned {
    match a.data_type() {
        DataType::Int32 => Owned::I32(a.as_primitive::<Int32Type>().iter().collect()),
        DataType::Int64 => Owned::I64(a.as_primitive::<Int64Type>().iter().collect()),
        DataType::Float64 => Owned::F64(a.as_primitive::<Float64Type>().iter().collect()),
        DataType::Utf8 => Owned::Str(a.as_string::<i32>().iter().map(|s| s.map(str::to_owned)).collect()),
        DataType::Boolean => Owned::Bool(a.as_boolean().iter().collect()),
        DataType::List(_) => {
            let l = a.as_list::<i32>();
            Owned::ListI32(
                l.iter()
                    .map(|row| row.map(|r| r.as_primitive::<Int32Type>().iter().collect()))
                    .collect(),
            )
        }
        DataType::Struct(_) => {
            Owned::Struct(a.as_struct().columns().iter().map(|c| to_owned_column(c.as_ref())).collect())
        }
        DataType::Dictionary(_, _) => {
            let d = a.as_dictionary::<Int32Type>();
            let vals = d.values().as_string::<i32>();
            Owned::Str(
                d.keys()
                    .iter()
                    .map(|k| k.map(|k| vals.value(k as usize).to_owned()))
                    .collect(),
            )
        }
        t => panic!("to_owned_column: unsupported {t}"),
    }
}

fn decode_to_vec(bytes: &[u8]) -> Vec<Vec<Owned>> {
    decode_stream(bytes)
        .iter()
        .map(|b| b.columns().iter().map(|c| to_owned_column(c.as_ref())).collect())
        .collect()
}

/// Encode once and check the bytes decode back to the input, so the timed
/// loop never measures an error path or a mismatched workload.
fn checked_stream_bytes(w: &Workload) -> Vec<u8> {
    let bytes = encode_stream(&w.batch);
    let back = decode_stream(&bytes);
    assert!(back.len() == 1 && back[0] == w.batch, "{}: round trip", w.name);
    bytes
}

fn codec_groups(c: &mut Criterion, suffix: &str, n: usize) {
    let ws = workloads(n);
    let mut g = c.benchmark_group(format!("encode{suffix}"));
    for w in &ws {
        g.bench_function(w.name, |b| b.iter(|| encode_stream(black_box(&w.batch))));
    }
    g.finish();

    let inputs: Vec<(&str, Vec<u8>)> = ws.iter().map(|w| (w.name, checked_stream_bytes(w))).collect();
    let mut g = c.benchmark_group(format!("decode{suffix}"));
    for (name, bytes) in &inputs {
        g.bench_function(*name, |b| b.iter(|| decode_stream(black_box(bytes))));
    }
    g.finish();

    let mut g = c.benchmark_group(format!("decode + to_vec{suffix}"));
    for (name, bytes) in &inputs {
        g.bench_function(*name, |b| b.iter(|| decode_to_vec(black_box(bytes))));
    }
    g.finish();

    let mut g = c.benchmark_group(format!("decode skip_validation{suffix}"));
    for (name, bytes) in &inputs {
        g.bench_function(*name, |b| {
            b.iter(|| decode_stream_unvalidated(black_box(bytes)))
        });
    }
    g.finish();
}

fn codec_big(c: &mut Criterion) {
    codec_groups(c, "", BIG_ROWS);
}

fn codec_small(c: &mut Criterion) {
    codec_groups(c, &format!(" ({SMALL_ROWS} rows)"), SMALL_ROWS);
}

/// Row type matching `mixed` column for column, like `Trade` in Bench.hs.
#[derive(Clone, Debug, PartialEq)]
struct Trade {
    trade_id: i64,
    trade_price: f64,
    trade_qty: Option<i64>,
    trade_symbol: String,
    trade_note: Option<String>,
    trade_settled: bool,
}

fn trades(n: usize) -> Vec<Trade> {
    (0..n)
        .map(|i| Trade {
            trade_id: i as i64 * 7919,
            trade_price: i as f64 * 1.25,
            trade_qty: if i % 10 == 0 { None } else { Some(i as i64) },
            trade_symbol: label(i).to_owned(),
            trade_note: if i % 10 == 0 { None } else { Some(label(i).to_owned()) },
            trade_settled: i % 2 == 0,
        })
        .collect()
}

/// arrow-rs has no derive for row types; the idiomatic path is one array per
/// field built from the rows, then the stream writer.
fn typed_encode(schema: &Arc<Schema>, ts: &[Trade]) -> Vec<u8> {
    let cols: Vec<ArrayRef> = vec![
        Arc::new(Int64Array::from_iter_values(ts.iter().map(|t| t.trade_id))),
        Arc::new(Float64Array::from_iter_values(ts.iter().map(|t| t.trade_price))),
        Arc::new(Int64Array::from_iter(ts.iter().map(|t| t.trade_qty))),
        Arc::new(StringArray::from_iter_values(ts.iter().map(|t| t.trade_symbol.as_str()))),
        Arc::new(StringArray::from_iter(ts.iter().map(|t| t.trade_note.as_deref()))),
        Arc::new(BooleanArray::from_iter(ts.iter().map(|t| Some(t.trade_settled)))),
    ];
    let batch = RecordBatch::try_new(schema.clone(), cols).expect("typed batch");
    encode_stream(&batch)
}

fn column<'a, T: Array + 'static>(batch: &'a RecordBatch, i: usize) -> &'a T {
    batch
        .column(i)
        .as_any()
        .downcast_ref::<T>()
        .expect("typed decode: column type")
}

/// Stream decode, then one owned `Trade` per row.
fn typed_decode(bytes: &[u8]) -> Vec<Trade> {
    let batches = decode_stream(bytes);
    assert!(batches.len() == 1, "typed decode: expected one batch");
    let b = &batches[0];
    let ids = column::<Int64Array>(b, 0);
    let prices = column::<Float64Array>(b, 1);
    let qtys = column::<Int64Array>(b, 2);
    let symbols = column::<StringArray>(b, 3);
    let notes = column::<StringArray>(b, 4);
    let settled = column::<BooleanArray>(b, 5);
    (0..b.num_rows())
        .map(|i| Trade {
            trade_id: ids.value(i),
            trade_price: prices.value(i),
            trade_qty: qtys.is_valid(i).then(|| qtys.value(i)),
            trade_symbol: symbols.value(i).to_owned(),
            trade_note: notes.is_valid(i).then(|| notes.value(i).to_owned()),
            trade_settled: settled.value(i),
        })
        .collect()
}

fn api_paths(c: &mut Criterion) {
    let w = mixed(BIG_ROWS);
    let stream_bytes = checked_stream_bytes(&w);
    let file_bytes = encode_file(&w.batch);
    let back = decode_file(&file_bytes);
    assert!(back.len() == 1 && back[0] == w.batch, "file round trip");
    let ts = trades(BIG_ROWS);
    let schema = w.batch.schema();
    let typed_bytes = typed_encode(&schema, &ts);
    assert!(typed_decode(&typed_bytes) == ts, "typed round trip");

    c.bench_function("stream encode (StreamWriter)", |b| {
        b.iter(|| encode_stream(black_box(&w.batch)))
    });
    c.bench_function("stream decode (StreamReader)", |b| {
        b.iter(|| decode_stream(black_box(&stream_bytes)))
    });
    c.bench_function("file encode (FileWriter)", |b| {
        b.iter(|| encode_file(black_box(&w.batch)))
    });
    c.bench_function("file decode (FileReader)", |b| {
        b.iter(|| decode_file(black_box(&file_bytes)))
    });
    c.bench_function("typed encode (row structs)", |b| {
        b.iter(|| typed_encode(&schema, black_box(&ts)))
    });
    c.bench_function("typed decode (row structs)", |b| {
        b.iter(|| typed_decode(black_box(&typed_bytes)))
    });
}

criterion_group!(benches, codec_big, codec_small, api_paths);
criterion_main!(benches);
