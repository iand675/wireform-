//! Same UTF-8 payloads and lexical work as wireform-edi/bench/Bench.hs.
//! Parser output borrows unchanged fields, and owns release-unescaped fields.
//! Neither side performs envelope or directory validation in these codec rows.
use criterion::{Criterion, criterion_group, criterion_main};
use edifact_rs::{
    EdifactDeserialize, EdifactSerialize, Element, Segment, from_bytes, segments_to_bytes,
};
use std::hint::black_box;

fn sample(payload: &str, count: usize) -> Vec<Segment<'_>> {
    (0..count)
        .map(|_| {
            Segment::new(
                "FTX",
                vec![
                    Element::of(&["AAA"]),
                    Element::of(&["alpha", "", "beta"]),
                    Element::of(&[payload]),
                    Element::of(&[""]),
                    Element::of(&["end "]),
                ],
            )
        })
        .collect()
}

#[derive(Debug, EdifactDeserialize, EdifactSerialize)]
#[edifact(segment = "FTX")]
struct Ftx {
    #[edifact(element = 0)]
    first: String,
    #[edifact(element = 1)]
    second: String,
    #[edifact(element = 2)]
    third: String,
}

fn benches(c: &mut Criterion) {
    for (name, payload, count) in [
        ("plain 1", "unescaped payload", 1),
        ("plain 1000", "unescaped payload", 1000),
        ("released 1", "literal ? + : ' and 雪", 1),
        ("released 1000", "literal ? + : ' and 雪", 1000),
    ] {
        let segments = sample(payload, count);
        let encoded = segments_to_bytes(&segments).unwrap();
        let parsed = from_bytes(&encoded).collect::<Result<Vec<_>, _>>().unwrap();
        assert_eq!(parsed.len(), count);
        assert_eq!(parsed[0].elements[2].get_component(0), Some(payload));
        let mut group = c.benchmark_group(name);
        group.bench_function("encode", |b| {
            b.iter(|| black_box(segments_to_bytes(black_box(&segments)).unwrap()))
        });
        group.bench_function("decode", |b| {
            b.iter(|| {
                black_box(
                    from_bytes(black_box(&encoded))
                        .collect::<Result<Vec<_>, _>>()
                        .unwrap(),
                )
            })
        });
        group.finish();
    }
    let record = Ftx {
        first: "AAA".into(),
        second: "literal ? + : ' and 雪".into(),
        third: "end ".into(),
    };
    let encoded = edifact_rs::to_bytes(&record).unwrap();
    let mut group = c.benchmark_group("typed FTX");
    group.bench_function("encode", |b| {
        b.iter(|| black_box(edifact_rs::to_bytes(black_box(&record)).unwrap()))
    });
    group.bench_function("decode", |b| {
        b.iter(|| {
            let segments = from_bytes(black_box(&encoded))
                .collect::<Result<Vec<_>, _>>()
                .unwrap();
            black_box(Ftx::edifact_deserialize(&segments).unwrap())
        })
    });
    group.finish();
}
criterion_group!(edifact, benches);
criterion_main!(edifact);
