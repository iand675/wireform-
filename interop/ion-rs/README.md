# Ion reference checks

The dependency is pinned to official `ion-rs` **1.1.0** (see Cargo.lock).
The CLI materializes exactly one value and re-encodes it as Ion 1.0 binary:

```sh
cargo build --release --manifest-path interop/ion-rs/Cargo.toml
WIREFORM_ION_REFERENCE="$PWD/interop/ion-rs/target/release/wireform-ion-rs-interop" \
  cabal test wireform-ion:wireform-ion-derive-test --test-show-details=direct
```

The opt-in tests check Wireform -> Rust -> Wireform, including all system
symbols, a 300-entry local table, empty/Unicode symbols, repeated fields,
ordered annotations, the Int64 boundaries, negative zero, and every supported
scalar/container shape. Ordinary tests also include independently framed
binary fixtures for replacement/append/reset/import semantics and malformed
input. Shared-catalog resolution and types outside `Ion.Value` remain outside
this codec surface; they are described in the package README.

Run the owned dynamic-value benchmark comparison **sequentially**:

```sh
python3 scripts/run-benchmarks.py --only wireform-ion:
python3 scripts/run-benchmarks.py --only interop/ion-rs
```

On a prepared non-Nix toolchain, add `--no-nix --project-file FILE` to the
Haskell command. Both sides encode to a fresh byte buffer, and decode to a
fully materialized dynamic tree with symbol text resolved. Fixture creation
is outside the clock. The extra Haskell `encode`/`decode` rows also include
the TH-generated typed-record conversion; the Rust series compares the
explicitly named `codec encode`/`codec decode` rows.

The separate typed comparison uses a real `serde`-derived Rust `Person`
and `Vec<Person>` against the TH-derived Haskell records:

```sh
python3 scripts/run-benchmarks.py --only interop/ion-rs:bench:ion_typed
```

`ion-rs` 1.1.0 gates this bridge behind `experimental-serde`. That status
describes the Rust API, not the Ion 1.0 specification. Keep these results
separate from the stable owned-`Element` codec comparison above. Both typed
sides include record conversion and fresh binary output or materialized
records; setup and fixture generation remain outside the clock.
