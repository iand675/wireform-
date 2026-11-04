//! Actual serde-derived records, matching the TH-derived Haskell Person.
//! ion-rs currently gates its serde bridge as experimental; the stable
//! owned Element codec comparison remains in ion_binary.rs.
use criterion::{criterion_group, criterion_main, Criterion};
use ion_rs::serde::{from_ion, to_binary};
use serde::{Deserialize, Serialize};
use std::hint::black_box;

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
struct Person {
    person_name: String,
    person_age: i64,
    person_email: String,
}

fn benches(c: &mut Criterion) {
    let one = Person {
        person_name: "Alice".into(),
        person_age: 30,
        person_email: "alice@example.com".into(),
    };
    let many: Vec<Person> = (1..=100)
        .map(|i| Person {
            person_name: format!("user-{i}"),
            person_age: 20 + i % 50,
            person_email: format!("user{i}@example.com"),
        })
        .collect();
    let one_bytes = to_binary(&one).unwrap();
    let many_bytes = to_binary(&many).unwrap();
    assert_eq!(from_ion::<Person, _>(&one_bytes).unwrap(), one);
    assert_eq!(from_ion::<Vec<Person>, _>(&many_bytes).unwrap(), many);
    c.bench_function("encode/Person", |b| {
        b.iter(|| black_box(to_binary(black_box(&one)).unwrap()))
    });
    c.bench_function("decode/Person", |b| {
        b.iter(|| black_box(from_ion::<Person, _>(black_box(&one_bytes)).unwrap()))
    });
    c.bench_function("encode/[Person] x 100", |b| {
        b.iter(|| black_box(to_binary(black_box(&many)).unwrap()))
    });
    c.bench_function("decode/[Person] x 100", |b| {
        b.iter(|| black_box(from_ion::<Vec<Person>, _>(black_box(&many_bytes)).unwrap()))
    });
}
criterion_group!(ion_typed, benches);
criterion_main!(ion_typed);
