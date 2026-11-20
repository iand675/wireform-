# wireform-delta

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)


> [!CAUTION]
> wireform is in heavy development and has not been published to Hackage yet. APIs may change.

[Delta Lake](https://delta.io/) transaction-log reader for Haskell.
Parse JSON commit files ([`Delta.Log`](src/Delta/Log.hs)),
read Parquet checkpoint files
([`Delta.Checkpoint`](src/Delta/Checkpoint.hs)),
materialise the active-file snapshot at any point in the table's
history, walk the time-travel API
([`Delta.IO`](src/Delta/IO.hs)),
and answer the queries `DESCRIBE HISTORY` answers.

Delta Lake is the table format Databricks open-sourced after several
years of using it internally. Same shape as Iceberg and Hudi: a
metadata layer on top of Parquet data files, ACID semantics through
an append-only log, snapshot isolation, time travel, schema
evolution. The log lives under `_delta_log/` as a sequence of
`NNNNNNNNNNNNNNNNNNNN.json` commit files (one per transaction);
periodic `NNNNNNNNNNNNNNNNNNNN.checkpoint.parquet` files snapshot
the cumulative state so a reader doesn't have to replay every
commit since the table was created.

This package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo and shares its allocation primitives, annotation deriver, and
testing discipline with every other format.

## Install

```cabal
build-depends:
  base,
  wireform-delta,
  wireform-parquet,    -- transitive: checkpoint files are Parquet
```

The package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo. Clone the repo and `cabal build wireform-delta` to compile
locally.

## Hello world

Open a Delta table and list its currently-active data files:

```haskell
import qualified Delta.IO as DT
import qualified Data.Text.IO as T

main :: IO ()
main = do
  result <- DT.openDeltaTable "/path/to/delta-table"
  case result of
    Right table -> mapM_ T.putStrLn (DT.activeFilePaths table)
    Left  err   -> putStrLn err
```

For time travel, `openDeltaTableAt` takes a version number and returns
the snapshot the table was in at that version. `historyEntries`
returns the list of available JSON commits newest first, the same surface
`DESCRIBE HISTORY` exposes in the Spark / Delta integration.

## What's in here

| Module             | Role                                                      |
|--------------------|-----------------------------------------------------------|
| `Delta.Log`        | JSON commit-file actions (`AddAction`, `RemoveAction`, `MetaData`, `Protocol`, `CommitInfo`, `Cdc`, `Txn`, `DomainMetadata`); `parseLogLine`, `parseLogFile`; the `TableSnapshot` aggregate (`applyAction`, `snapshotFromActions`); `parseDeltaSchema`; `AddStats` (per-file column statistics); `LastCheckpoint` (the `_last_checkpoint` pointer file) |
| `Delta.Checkpoint` | Parquet checkpoint reader. Reconstructs `add` / `remove` / `metaData` / `protocol` records from the columnar layout, including `partitionValues`, `tags`, `partitionColumns`, `configuration`, `readerFeatures`, `writerFeatures`, and `deletionVector` |
| `Delta.DeletionVector` | Checked reader for inline and on-disk Roaring deletion vectors |
| `Delta.IO`         | `openDeltaTable` (with checkpoint recovery), version and timestamp time travel, `validateVersionChecksum`, `historyEntries` (the `DESCRIBE HISTORY` surface), active-file helpers and log discovery |
| `Delta.Scan`       | `scanTable` streams logical rows from a local Delta snapshot, `tableFileStats` resolves statistics to logical names, and `openDataFile` reads a file in its physical schema |

## Reading a table

`openDeltaTable` walks the `_delta_log/` directory:

1. Find the newest readable checkpoint at or before the target version.
   This supports classic and complete multipart Parquet checkpoints and V2
   JSON or Parquet checkpoints with optional Parquet sidecars.
   Each sidecar names a `.parquet` file in the table's
   `_delta_log/_sidecars` directory. The reader resolves relative
   references from that directory and checks absolute references against it.
   V2 checkpoints that publish file counts or sidecar totals are checked
   against the actions and sidecars they contain.
   A matching `_last_checkpoint` add-file count also checks classic and
   multipart checkpoints and named V2 manifests. Try another checkpoint
   if decoding or validation fails.
2. Replay numbered JSON commits and contiguous compacted ranges
   after the checkpoint through the target version. With no readable
   checkpoint, replay from version 0 and require its initial protocol
   and table metadata. Time travel inside a compacted range requires
   numbered commits or another route to that version.
3. Return the resulting `DeltaTable`. If a segment fails, try an
   alternate contiguous route when one exists. A checkpoint or compacted
   range can also be rejected when its replayed file totals disagree with
   a checksum tied to the numbered commit by transaction ID.
4. If a numbered commit has zero bytes or ends in a truncated JSON record, try its version
   checksum (`.crc`), then replay any remaining commits. Recovery needs
   the complete live file, transaction and domain sets, and verifies
   the file count, total size, and any published histograms. The checksum
   must also agree with the complete actions that survived in the truncated commit.
   Recovery uses file-pruning statistics from surviving commit actions,
   or from earlier replayed actions whose descriptors and statistics agree
   with the checksum. For checksum-only files with deletion vectors it retains
   the physical row count, which is checked against the Parquet footer.
5. When a numbered commit replays cleanly and carries a transaction ID,
   check the file count, total size, and available deletion-vector totals
   if a checksum is present. A complete checksum with the matching ID
   can recover a commit that lost whole JSON lines. If the matching
   checksum shows a discrepancy and omits state needed for recovery,
   replay reports the discrepancy and can try another checkpoint or
   compacted range. Change Data Feed reports an error in either case
   because it needs the original events.

Call `validateVersionChecksum` on a loaded snapshot to check its protocol,
metadata, live file count, and total data size against its `.crc` file.
It also compares the transaction ID and in-commit timestamp when both
the snapshot and checksum contain them, and verifies deletion-vector
totals when supplied. When the checksum includes
`allFiles`, it checks membership using replay's URI path normalization and
compares file descriptors, including deletion vectors and row-tracking fields.
Deletion vectors are compared by their Delta descriptor fields, so a null
offset and an omitted offset agree.
It also compares published domain state and transaction versions. With
transaction retention configured, only IDs present in both states can be
compared because the writer may have expired entries at a different cutoff.
Published file-size and deleted-row histograms are checked bin by bin.
Use this check when you suspect a change
that did not alter the aggregate totals.
When both records include JSON statistics, validation compares their
contents as well.
It returns `Right False` if the checksum was never published or has been
removed, and reports malformed checksums and mismatches as errors.

An empty log directory has no committed snapshot. A committed table may
have zero active files.

The readers reject repeated keys in JSON objects, including within file
actions, statistics, and table schemas.
File reconciliation normalizes URI escapes for unreserved characters and
ignores hex digit case in escapes. It also treats literal spaces from
older writers as equivalent to `%20`. The snapshot exposes the stored
spelling of each live add action.
For incremental replay, `applyAction` keeps an index of alternate URI
spellings. If a caller supplies a file map, the index is built lazily.

`decodeAddStats` reads either the JSON statistics from the log or
structured Parquet statistics from a checkpoint or sidecar. Where a
checkpoint supplies both, the JSON field takes precedence. Bounds and
counts keep the stored physical names on column-mapped tables.
Structured Variant bounds also support Spark checkpoints whose footer
records the Variant type in its Spark schema and omits the Parquet annotation.
`tableFileStats` resolves those names and converts date bounds to midnight
for fields widened to `timestamp_ntz`. It excludes partition columns, whose
values come from `AddFile.partitionValues`. Numeric floating-point bounds enclose
the exact binary values represented by their stored decimal strings. For
fields widened from `float` to `double`, they also cover Float32 values in
older files. The function accepts historical UTC offsets with seconds and
expands millisecond-aligned timestamp maxima by one millisecond (up to the
largest representable timestamp). It omits zoned timestamp bounds that
lack an explicit offset. It also omits
intermediate null counts when the file has wide bounds. Logical statistics
omit bounds for all-null columns and bounds that have the wrong type or fall
outside the column's range. Null counts that exceed the applicable row count
are also omitted. Files with deletion vectors require `numRecords`. Tight
null counts are checked against the live row count, and wide null counts
against the physical row count.
Table scans and structured checkpoint statistics rebase Spark legacy
dates and zoned timestamps to the proleptic Gregorian calendar using the
Parquet footer. Zoned values that need conversion require a recognized
writer timezone. Structured bounds with an unknown conversion are omitted
so they cannot cause incorrect file pruning.
Filtered scans use date and timestamp bounds from either statistics
encoding. Zoned bounds need an explicit offset before they can rule out a file.
A single bound can rule out a file when the comparison only needs that endpoint.
Partition values and file bounds can rule out different branches of the
same filter before a Parquet file is opened.
Null filters on nested fields can also use file-level counts. These count
rows where the leaf or any of its parent structs is null.
An all-null count can rule out ordinary comparisons and membership tests,
including under negation. Negated null-safe equality can still match a null.
A `NOT IN` list containing null cannot match a row. For other `NOT IN`
filters, equal lower and upper bounds can rule out a file when that value
appears in the list.

`openDeltaTableAt v` short-circuits at version `v` instead of the
latest version, returning the snapshot the table was in at that
specific commit.

Pass the snapshot to `Delta.Scan.scanTable` to get its logical Arrow
schema and a `Columnar.Stream.IterIO DataBatch`. The scan follows the
snapshot's column mapping mode, injects partition values from the log,
and fills fields absent from older files with nulls, including nullable
values in Parquet maps stored with keys only. It rejects unsupported
reader features even on snapshots assembled from decoded actions. A metadata
format descriptor, when present, must name a provider. Scans require Parquet.
Table scans and file statistics require the `deletionVectors` reader and
writer feature when a file carries a vector. The scan checks the declared
physical row count against the Parquet footer before applying the vector
and skips fully deleted row groups and files.
`openDataFile` provides the
physical schema when reading a single add action.
`scanTableProjected` takes an ordered list of top-level display names. It
decodes only the needed Parquet columns and supplies the same partition
values and evolved nulls as the full scan. An empty list streams live row
counts without decoding data columns. `scanTableProjectedWithPartitionOffset`
also accepts a resolver for legacy partition timestamps.
`scanTableFiltered` takes an optional output projection and a typed
`Delta.Predicate` over the snapshot's logical names. Paths identify nested
struct fields. Comparisons, null-safe equality, membership, null tests,
`LIKE`, collection size, string length, and boolean composition use SQL
null semantics. Time and date
literals can be given as Unix-epoch microseconds and days respectively.
The scan applies deletion vectors before row evaluation. It can avoid files
whose logged partition values or usable statistics rule out a match. The API accepts a
predicate tree for callers that already parse SQL or another query language.
`scanTrackedTable` additionally returns stable row IDs and row commit
versions for snapshots with row tracking enabled. It uses materialized
values when present and generates defaults from the original physical
row positions and the file's log action.
The snapshot exposes live domain configurations through `tsDomains`.
The logical scan converts recorded historical types to the current
schema at the field or collection path where each change applies.
It also restores typed nulls from legacy Parquet files containing physical
`UNKNOWN` columns after a Delta `void` field evolves. This includes null
elements and map values inside populated collections.
The scan reads dates stored in partition values before a
change to timestamp without timezone. The `timestamp_ntz` type requires
the `timestampNtz` table feature, including inside nested types. The default
scan uses UTC for legacy partition timestamps that carry no zone.
`scanTableWithPartitionOffset` and `scanTrackedTableWithPartitionOffset`
accept a resolver for the writer's UTC offset in seconds for each file and
local time. The resolver can reject unknown or ambiguous local times.
Explicit offsets in the log preserve historical seconds.
Spark ANSI interval columns are supported as an interoperability extension.
Year-month intervals use Arrow's month interval type. Day-time intervals
use Arrow durations in microseconds. This applies to Parquet values and
Spark's interval literals in partition values.

Variant columns require the Variant table feature. Shredded files and Variant
min/max statistics require the shredding feature. Stable and legacy preview
protocols are supported. Table scans recognize legacy shredded Parquet groups
whose Variant type is declared in the Delta schema.

## Change data feed

`scanChangeDataFeed` streams an inclusive commit range. Its output schema
is the end version's logical schema with `_change_type`, `_commit_version`,
and `_commit_timestamp` appended. `Nothing` for the end version captures
the latest observed version. Explicit CDC files take precedence over add
and remove actions in the same commit. Otherwise, data-changing actions
emit inserts and deletes, including visibility changes from deletion
vector replacements. `scanChangeDataFeedFiltered` also accepts a typed
predicate over the emitted rows.

The requested range needs its original numbered commits and the data or
CDC files they reference. A checkpoint can seed the state before the
range. Schema conversion follows the end version's types and mappings.
The stream reports an error if it encounters an incompatible physical
type, a partition schema change, or a non-additive change to a mapped
schema. For a range starting at a schema change, inferred deletes
also need a compatible pre-change schema.

## Time travel and history

`Delta.IO` exposes the working surface for time-travel queries:

```haskell
openDeltaTable    :: FilePath -> IO (Either String DeltaTable)
openDeltaTableAt  :: FilePath -> Word64 -> IO (Either String DeltaTable)
openDeltaTableAtTimestamp :: FilePath -> UTCTime -> IO (Either String DeltaTable)
historyEntries    :: DeltaTable -> IO [HistoryEntry]
activeFilePaths   :: DeltaTable -> [Text]
dtActiveFiles     :: DeltaTable -> [AddAction]
```

`historyEntries` mirrors the result `DESCRIBE HISTORY <table>` returns
in the Spark / Delta integration: one entry per commit, with the
operation kind, timestamp, isolation level, and application transaction
identifier where available. Timestamps use `inCommitTimestamp` from the
enablement version onward and file modification times for older commits.
If an in-commit timestamp was truncated, history and time travel can use
the timestamp from a checksum that can recover that version's snapshot.
When a checkpoint or compacted range supplies the snapshot, the opener
recovers its timestamp only if the checksum also agrees with that state.
Timestamp-based reads select the latest commit at or before the given UTC
time, at millisecond precision. They reject times outside the available
history and gaps where the commit timestamps cannot establish the version.

## Testing

```bash
cabal test wireform-delta:delta-test
```

The Sydtest suite covers log and schema decoding, snapshot replay,
checkpoint recovery, and time travel.

### Cross-language interop with `delta-rs`

[`probe/Probe.hs`](probe/Probe.hs) round-trips against
[`delta-rs`](https://github.com/delta-io/delta-rs) (the Rust /
Python Delta Lake binding). The driver exercises:

- Unpartitioned, no checkpoint.
- Partitioned, no checkpoint.
- Checkpointed (v11), with subsequent APPEND / OVERWRITE commits.
- Partitioned + checkpointed, including null and empty-string
  partition encodings and the checkpoint's `partitionColumns`.
- Recovery after checkpoint corruption and log cleanup.
- A checkpoint followed by a compacted range, with gaps in the
  numbered commits and optional target-version provenance.
- V2 JSON and Parquet sidecar fixtures, plus complete, incomplete, and
  invalid multipart checkpoints.
- Checkpoints with only structured statistics, including nested counts,
  decimal bounds and timestamps.
- Live-row reads across Parquet row groups using inline, UUID-relative
  and absolute-path deletion vectors.
- Table scans with name and field-ID mapping, renamed nested fields,
  missing fields in old files, and supported type widening. The field-ID
  case also checks ordered projection over files with a deletion vector.
- Stable row tracking with materialized and default values, deleted row
  groups, and checkpoint-only recovery.
- Time-travel + history (cross-checks `historyEntries` against
  `DeltaTable.history()` and `openDeltaTableAt v=2` against
  `DeltaTable(.., version=2)`).
- In-commit timestamp enablement, including recovery from a delta-rs
  checkpoint after removing the older commit.

Catalog-managed commits are outside the local reader's scope.

```bash
python3 -m pip install deltalake pyarrow
python3 wireform-delta/scripts/delta_interop.py
```

## Benchmarks

The snapshot benchmark opens tables with 10,000 active files using numbered
JSON commits, compacted JSON ranges with ordered and shuffled paths, a
Parquet checkpoint, and a checkpoint with a short JSON tail. The long-log
case also lists and classifies 25,001 numbered commits through one checkpoint.
The V2 sidecar case reads a JSON checkpoint with 100 Parquet sidecars.
The row-tracking JSON case assigns row IDs and default commit versions
to all 10,000 file actions.
The history benchmark reads provenance and transaction markers from
the numbered JSON commits.
The checksum benchmarks validate an open snapshot with 10,000 files and
recover a commit with 1,000 surviving file actions, with cuts both inside
a JSON action and at a line boundary.
The incremental-actions benchmark applies 10,000 file additions one at a
time through the public snapshot helper.
Four structured-statistics cases verify the active file count and sum
physical row counts, minimums, maximums, and null counts for 10,000
checkpoint entries. Ordered and shuffled paths are measured separately
for repeated and distinct bounds.
The logical-statistics case uses the ordered checkpoint with distinct
bounds and supplies a deletion-vector descriptor per file to exercise the
reader's validation and null-count handling.
The deletion-vector cases scan 65,536 rows with every eighth row
deleted, using row groups of 8,192 and 512 rows. The 512-row case
is measured with both Roaring bitset and fragmented run containers.
A fully deleted file measures scan setup across 1,024 small row groups.
The partition-only case counts live rows without decoding data columns
or gathering their positions.
The mapped-table case scans 65,536 rows across four files and
materializes a partition value and a missing column.
The projection cases sum the same ID column across 65,536 rows, reading
either one Parquet column or all 17.
The predicate cases count matches on a column outside the output projection,
and measure file pruning using data statistics and mapped partition values.
The change-feed case counts 65,536 inserts and an equal number of deletes
from two numbered commits.
The text-partition case checks the UTF-8 bytes materialized from a
partition value across 65,536 rows.
The row-tracking cases read 65,536 physical rows and materialize both
stable metadata columns. One generates every value from the log. The
other combines stored values with generated defaults and deletion filtering.
The timestamp case checks conversion of millisecond Parquet timestamps
to Delta's microsecond representation.
The decimal case widens the precision of two Decimal128 columns and
increases the scale of one of them.
The reference command uses delta-rs on the same files and applies its
deletion mask with PyArrow for the scan.

```bash
python3 -m pip install deltalake pyarrow
python3 scripts/run-benchmarks.py --only delta-snapshot-bench
python3 wireform-delta/scripts/delta_bench.py dist-stats/delta-fixtures --reference 30
```

## License

BSD-3-Clause.

## References

- [Delta Lake protocol specification](https://github.com/delta-io/delta/blob/master/PROTOCOL.md)
- [Delta Lake project](https://delta.io/)
- [`delta-rs`](https://github.com/delta-io/delta-rs) (the Rust binding)
