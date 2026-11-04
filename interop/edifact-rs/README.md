# EDIFACT reference checks

Pins `edifact-rs` **0.18.0** (see Cargo.lock). The reference CLI fully parses
UTF-8 input and writes the segment values using default service advice.
It normalizes service characters rather than preserving the original UNA.

```sh
cargo build --release --manifest-path interop/edifact-rs/Cargo.toml
WIREFORM_EDIFACT_REFERENCE="$PWD/interop/edifact-rs/target/release/wireform-edifact-rs-reference" \
  cabal test wireform-edi:wireform-edi-derive-test --test-show-details=direct
```

Fixtures exercise the reported `??+` boundary bug, escaped separators,
Unicode, trailing whitespace, empty/composite positions, custom UNA and long
release runs. Both libraries parse Wireform's bytes, and Wireform reads the
reference's independently rewritten output.

```sh
python3 scripts/run-benchmarks.py --only edi
```

The manifest runs Haskell and Rust sequentially. Both timed inputs/outputs
are UTF-8 bytes; both dynamic decoders retain unchanged text by borrowing
or slicing, and materialize released fields. Workloads have 1 and 1,000
identical segments, with plain or release-rich payloads. The typed rows use
Haskell TH and Rust derive output for the same three-field FTX record.
Fixture preparation and round-trip checks are outside the clock. These
rows compare lexical codecs, without envelope/directory validation or
charset transcoding. No repeated-element workload is claimed.
