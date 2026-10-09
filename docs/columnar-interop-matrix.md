# Columnar interop matrix

What's tested against what, as of this PR. Each row is one
direction (wireform → engine, or engine → wireform); a "✓ N/M"
cell means "N of M test files in this matrix pass through the
listed engine without modification".

## Apache Parquet

|                                  | pyarrow 24 | duckdb 1.5 | polars 1.40 | arrow-rs 53 |
| -------------------------------- | :--------: | :--------: | :---------: | :---------: |
| **wireform → engine** (10 files) |  ✓ 10/10   |  ✓ 10/10   |   ✓ 10/10   |   ✓ 10/10   |
| **engine → wireform** (14 files) |  ✓ 14/14   |   ✓ 3/3    |    ✓ 3/3    |     n/a     |

The wireform → engine side covers: required Int32 / Int64 /
Float / Double / Bool / ByteArray / UTF-8, ZSTD-compressed,
two-row-group, mixed required + nullable. The engine → wireform
side covers each engine's defaults plus their preferred
codecs (Snappy / ZSTD / GZip / LZ4_RAW / dictionary / V1 page /
V2 page / multi-row-group).

Drivers:

- `wireform-parquet/scripts/parquet_interop.py` (forward)
- `wireform-parquet/scripts/parquet_reverse_interop.py` (reverse)
- `interop/arrow-rs/target/release/read_parquet` (Rust forward)

## Apache ORC

|                              | pyarrow 24 | duckdb 1.5 |
| ---------------------------- | :--------: | :--------: |
| **wireform → engine** (5)    |    ✓ 5/5   |  ⚠ skipped |
| **engine → wireform** (8)    |    ✓ 8/8   |     n/a    |

DuckDB's ORC support is a community extension not bundled by
default in 1.5.x; polars doesn't ship a stable ORC reader.
pyarrow.orc wraps the official Apache C++ ORC reader so it
exercises the spec faithfully.

The reverse-direction matrix covers Snappy / ZSTD / Zlib
compression plus every primitive type our writer emits.

Drivers:

- `wireform-orc/scripts/orc_interop.py` (forward)
- `wireform-orc/scripts/orc_reverse_interop.py` (reverse)

## Apache Iceberg

|                                                | pyiceberg 0.11 + fastavro |
| ---------------------------------------------- | :-----------------------: |
| **wireform → engine** (3 metadata files)       |          ✓ 3/3            |

The 3 files cover the three Iceberg metadata file types:

* `manifest_v2.avro` — manifest file (Avro container of
  `manifest_entry` records, full v2 statistics including
  `column_sizes`, `value_counts`, `null_value_counts`,
  `lower_bounds`, `upper_bounds`).
* `manifest_list_v2.avro` — manifest list (Avro container of
  `manifest_file` records).
* `table_metadata_v2.json` — `TableMetadata` JSON pyiceberg
  parses through `TableMetadataUtil.parse_raw`, surfacing
  `format_version`, `table_uuid`, `location`,
  `current_snapshot_id`, schemas, and snapshot list.

Reverse-direction (read pyiceberg-emitted metadata into
wireform) is the next bite-sized follow-up; the writer-side
matrix above is the higher-impact one because it validates
that wireform-emitted Iceberg tables are consumable by every
downstream Iceberg client.

Drivers:

- `wireform-iceberg/scripts/iceberg_interop.py`

## Apache Arrow IPC

|                              | pyarrow 24 | arrow-rs 58 |
| ---------------------------- | :--------: | :---------: |
| **wireform → engine** (26)   |   ✓ 26/26  |   ✓ 26/26   |

The 26 files cover every primitive integer width
(int8/16/32/64, uint8/16/32/64), Float / Double, Binary /
FixedSizeBinary, Date / Time / Timestamp / Duration,
Decimal128, List<int32>, Struct, dictionary-encoded UTF-8,
view types (Utf8View / ListView), RunEndEncoded, ZSTD body
compression, and the file-format (.arrow) variant.

ListView / LargeListView used to fail against arrow-rs 53 with
`Type ListView not supported` in `arrow-ipc/src/convert.rs`; that
gap was closed in arrow-rs by [PR
\#9006](https://github.com/apache/arrow-rs/pull/9006) (merged
2025-12-18, shipped in the 57.x cycle and round-tripped through
the Parquet writer in [\#9344](https://github.com/apache/arrow-rs/pull/9344)
in the 58.0 release, 2026-02-19). The interop pin is now
`arrow = "58"` and the columnar harness expects every file —
including `ours_listview.arrows` — to read clean.

(History note: `utf8view` flushed out a wireform alignment bug
arrow-rs caught — see commit "FlatBuffers / Arrow IPC: fix i64
vector alignment for arrow-rs".)

Drivers:

- `cabal test wireform-arrow:wireform-arrow-pyarrow-interop` (both directions against pyarrow; case matrix in `wireform-arrow/test-interop/`)
- `interop/arrow-rs/target/release/read_arrow_ipc`

## Apache Delta Lake (table format)

|                                          | deltalake (delta-rs) |
| ---------------------------------------- | :------------------: |
| **wireform → engine** (commit JSON)      |       ✓ 5/5          |
| **wireform → engine** (checkpoint Parquet) |     ✓ 2/2          |
| **time travel + history**                |       ✓ 1/1          |

The wireform-delta probe opens a Delta table via
`Delta.IO.openDeltaTable`. When a `*.checkpoint.parquet`
file is present the snapshot at that version is decoded
directly via `Delta.Checkpoint.decodeCheckpointFile` (no
JSON walk through every prior commit), then JSON commits
with version > checkpoint version are replayed on top.

The Python driver builds four real Delta tables with
`deltalake.write_deltalake`:

  * an **unpartitioned** table with a write / append /
    overwrite history (only one add survives);
  * a **partitioned-by-region** table with two writes;
  * a **checkpointed** table — 12 appends + an explicit
    `DeltaTable.create_checkpoint()` at v11, then APPEND v12
    + OVERWRITE v13. This forces both code paths: the
    checkpoint Parquet seeds the snapshot at v11, then the
    post-checkpoint commits replay through the JSON walker.
  * a **partitioned + checkpointed** table — 12 appends to a
    region-partitioned table + checkpoint at v11. Verifies
    the typed `map<string, string>` partitionValues leaf and
    the `list<string>` `metaData.partitionColumns` leaf out of
    the checkpoint Parquet match what `DeltaTable.metadata()`
    reports.

For every case the probe's `version`, `active_files`,
`protocol`, `metadata.partition_columns`,
`metadata.schema_field_names`, `last_checkpoint.version`,
and `checkpoint_parquet_version` are cross-checked against
`DeltaTable.*`. For the checkpointed cases the probe
additionally surfaces the *standalone* checkpoint-Parquet
snapshot (`checkpoint_active_files`, `checkpoint_protocol`,
`checkpoint_metadata`) and the driver verifies it produces
the same view of the table at v11 that the JSON walker
would, including the partitionValues map per active file
and the partitionColumns list on the metaData row.

Driver: `wireform-delta/scripts/delta_interop.py`.

## Apache Hudi (timeline)

|                                       | hudi-rs (Python)     |
| ------------------------------------- | :------------------: |
| **wireform → engine** (commit JSON)   |       ✓ 2/2          |

The wireform-hudi probe parses every completed
`<instantTime>.commit` JSON under `.hoodie/`, folds it into a
per-partition / per-fileId `FileSlice` map, and writes a JSON
summary. The Python driver hand-builds two `COPY_ON_WRITE`
tables on disk (one unpartitioned with two commits where the
second supersedes the first's base file; one partitioned by
`region` with a UPSERT in one partition), then asserts that
hudi-rs's `HudiTable.get_file_slices()` and wireform's view
agree on every `(partition_path, file_id, base_file)` tuple.

Hudi-rs is read-only — writing real Hudi tables requires
Spark / hudi-java — so the driver constructs the on-disk layout
directly. The point of the round-trip is to verify wireform's
JSON commit decoder and `tableStateFromCommits` fold produce the
same active set the canonical reader does.

Driver: `wireform-hudi/scripts/hudi_interop.py`.

## Apache Lance (file footer + dataset)

|                                       | pylance              |
| ------------------------------------- | :------------------: |
| **engine → wireform** (file footer)   |       ✓ 1/1          |
| **engine → wireform** (dataset)       |       ✓ 1/1          |

Two interop modes:

* `--file`: probe a single `.lance` data file. Emits the typed
  `LanceFooter` (CMO offset, GBO offset, num columns, num
  global buffers, version, plus the per-column /
  per-global-buffer (position, size) tables). The driver
  asserts every field matches an independent struct-unpack
  decode of the trailing 40 bytes, that `num_columns` matches
  `len(LanceDataset.schema)`, and that the column slice table
  is in-range.

* `--dataset`: probe a `.lance/` directory.
  `Lance.IO.openLanceDataset` enumerates every
  `_versions/<n>.manifest` (decoding the `2^64 − 1 − v`
  filename convention back to the user-visible version),
  parses the active manifest's distinct 16-byte
  `LanceManifestFooter`, and lists every `data/*.lance`
  fragment. The driver writes two append commits with pylance
  and asserts the probe's `latest_version`, `versions[]`,
  `latest_manifest_footer.{manifest_position, major_version,
  minor_version}`, and `data_file_names` all match what
  `lance.dataset(...).versions()` and a directory listing
  report.

Driver: `wireform-lance/scripts/lance_interop.py`.

## Replication

```bash
# system deps
apt install liblz4-dev libsnappy-dev libzstd-dev   # ubuntu
brew install lz4 snappy zstd                       # macos
pip install pyarrow duckdb polars
pip install pyiceberg fastavro                     # Iceberg
pip install deltalake hudi pylance                 # Delta / Hudi / Lance

# build
cabal build all
( cd interop/arrow-rs && cargo build --release )

# Parquet (forward + reverse)
python3 wireform-parquet/scripts/parquet_interop.py
python3 wireform-parquet/scripts/parquet_reverse_interop.py

# ORC (forward + reverse)
python3 wireform-orc/scripts/orc_interop.py
python3 wireform-orc/scripts/orc_reverse_interop.py

# Arrow IPC
WIREFORM_ARROW_REQUIRE_PYARROW=1 cabal test wireform-arrow:wireform-arrow-pyarrow-interop --enable-tests

# Iceberg / Delta / Hudi / Lance (table-format readers)
python3 wireform-iceberg/scripts/iceberg_interop.py
python3 wireform-delta/scripts/delta_interop.py
python3 wireform-hudi/scripts/hudi_interop.py
python3 wireform-lance/scripts/lance_interop.py

# Rust side: feed the wireform probe outputs to arrow-rs / parquet-rs
mkdir -p /tmp/wf-pq /tmp/wf-arrow
cabal run wireform-parquet:wireform-parquet-interop-probe -- /tmp/wf-pq
cabal run wireform-arrow:test:wireform-arrow-pyarrow-interop --enable-tests -- --write /tmp/wf-arrow
./interop/arrow-rs/target/release/read_parquet /tmp/wf-pq
./interop/arrow-rs/target/release/read_arrow_ipc /tmp/wf-arrow
```

## Bugs surfaced (and fixed) by the probes

Each one was a real bug in wireform that no internal test
exercised; the probes caught them by feeding our output into
the Apache reference readers.

1. **`utf8_required.parquet` was rejected by every reader**.
   Cause: the probe used a bare `ByteString` literal for the
   row containing `"Δοε"`. ByteString's `IsString` truncates
   each `Char` to its low 8 bits, silently mangling
   non-ASCII strings. (Probe-side bug.)
2. **Default builds couldn't read what every other engine
   writes by default.** The `snappy` / `zstd` / `lz4` Cabal
   flags were `default: False`. pyarrow defaults to Snappy,
   duckdb to Snappy, polars to ZSTD. Flipped to `default:
   True` on Parquet; added the same flags to ORC (which was
   missing snappy + zstd entirely).
3. **`ColumnMetaData.dictionary_page_offset` was wired to
   the wrong thrift field number** (10 instead of 11; field
   10 is `index_page_offset`). Result: the offset was always
   `Nothing` after decode, so `columnChunkSlice` missed the
   dictionary page and any RLE_DICTIONARY data page failed
   with "RLE_DICTIONARY page before dictionary page".
4. **Dictionary page header rejected legacy
   `PLAIN_DICTIONARY` encoding.** pyarrow's V1 writer emits
   dictionary pages with `PLAIN_DICTIONARY = 2`; both `PLAIN`
   and `PLAIN_DICTIONARY` are spec-valid for the dict page
   itself.
5. **BOOLEAN with `RLE` encoding (encoding 3) wasn't
   supported.** V2 BOOLEAN columns use this encoding;
   previously the dispatcher returned "unsupported encoding
   3".
6. **LZ4_RAW + ORC LZ4 went through a wire-incompatible
   `lz4` Hackage package.** That package framed every block
   in an 8-byte size header that no other Parquet / ORC
   reader expects. Replaced with direct FFI bindings to
   `liblz4` (`Columnar.LZ4`).
7. **ORC `StripeFooter.columns` field was always empty.**
   Every official ORC reader rejects this with "bad number
   of ColumnEncodings in StripeFooter: expected=N, actual=0".
   Fixed to emit one `ColumnEncoding` per column (DIRECT for
   Struct / Map / List / Union; DIRECT_V2 otherwise).
8. **ORC postscript magic was required to be `"ORC"`.** The
   field is optional in the spec; pyarrow / arrow-cpp omit
   it because the leading "ORC" file header is canonical.
9. **ORC footer + stripe footers weren't decompressed.** The
   spec wraps both in the file's compression envelope; we
   were reading the raw bytes and the protobuf decoder
   choked on the high-bit byte from the compressed stream.
10. **Arrow IPC `[long]` vectors had wrong element
    alignment.** The variadic-buffer-counts vector for view
    types put i64 elements at file position `≡ 4 mod 8` —
    arrow-rs's verifier rejected it. Generic `prepForObject`
    accounted for the u32 length prefix incorrectly. Fixed
    with a vector-aware `prepForVector` helper.
