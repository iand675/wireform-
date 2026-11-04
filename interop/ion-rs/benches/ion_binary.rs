//! Same owned dynamic values as wireform-ion/bench/Bench.hs. Fixture parsing
//! and byte preparation happen outside the measurement. Decode materializes
//! the complete Element tree, including local symbol table resolution.
use criterion::{criterion_group, criterion_main, Criterion};
use ion_rs::{v1_0::Binary, Element};
use std::hint::black_box;

fn person(i: usize) -> String {
    format!(
        r#"{{personName:"user-{i}",personAge:{},personEmail:"user{i}@example.com"}}"#,
        20 + i % 50
    )
}

fn benches(c: &mut Criterion) {
    let single =
        Element::read_one(r#"{personName:"Alice",personAge:30,personEmail:"alice@example.com"}"#)
            .unwrap();
    let records_text = format!("[{}]", (1..=100).map(person).collect::<Vec<_>>().join(","));
    let symbols_text = format!("[{}]", (1..=100).map(|i| format!(r#"event::{{name:"user-{i}",kind:created,tags:[created,updated,created],unicode:'雪'}}"#)).collect::<Vec<_>>().join(","));
    let records = Element::read_one(records_text).unwrap();
    let symbols = Element::read_one(symbols_text).unwrap();
    for (name, value) in [
        ("Person", single),
        ("[Person] x 100", records),
        ("symbols x 100", symbols),
    ] {
        let bytes: Vec<u8> = value.encode_as(Binary).unwrap();
        assert_eq!(Element::read_one(&bytes).unwrap(), value);
        c.bench_function(&format!("encode/{name}"), |b| {
            b.iter(|| black_box(black_box(&value).encode_as(Binary).unwrap()))
        });
        c.bench_function(&format!("decode/{name}"), |b| {
            b.iter(|| black_box(Element::read_one(black_box(&bytes)).unwrap()))
        });
    }
}
criterion_group!(ion_binary, benches);
criterion_main!(ion_binary);
