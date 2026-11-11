# ALP reference benchmark

This pins parquet-rs 60.1.0 for its ALP codec. The established Arrow 58 interop
project remains available for comparisons recorded against that version.

Extract the official Apache parquet-testing fixture, then run the two targets
sequentially through the repository's benchmark driver:

```sh
python3 wireform-parquet/scripts/parquet_alp_fixtures.py \
  --corpus /path/to/parquet-testing \
  --output-dir wireform-parquet/dist-stats/bench-fixtures/alp
python3 scripts/run-benchmarks.py --only parquet-alp-
```

Both harnesses decode the same 30 raw pages and construct fresh output for
every iteration. Setup checks every non-null value's IEEE bits against the
file's independent PLAIN columns before timing. Fixture generation, file IO,
decompression and expected-value checks are outside measurements. The cases
cover five row groups, FLOAT/DOUBLE, and vector sizes 32, 1024 and 4096.

The same crate also compiles the existing byte-stream-split, dictionary and
whole-file control harnesses against 60.1.0. The whole-file target requires
`--features file-bench`; its input fixture and validation protocol match the
established Arrow 58 harness.
The `parquet60-` targets in `scripts/bench-manifest.json` run these controls
against 60.1.0 without changing the historical Arrow 58 targets.
