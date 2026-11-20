"""Generate equivalent Delta log/checkpoint workloads and time the delta-rs reader."""

import argparse
import json
import random
import shutil
import struct
import zlib
from decimal import Decimal
from pathlib import Path
from statistics import mean, median
from time import perf_counter_ns

import pyarrow as pa
import pyarrow.compute as pc
import pyarrow.parquet as pq
from deltalake import DeltaTable, WriterProperties, write_deltalake


CHECKPOINT_STATISTICS_CASES = (
    ("checkpoint-ordered-repeated-stats", False, False, 10_000),
    ("checkpoint-shuffled-repeated-stats", False, True, 10_000),
    ("checkpoint-ordered-varied-stats", True, False, 49_995_000),
    ("checkpoint-shuffled-varied-stats", True, True, 49_995_000),
)


def add_row(index):
    return {"add": {"path": f"part-{index:08d}.parquet", "size": 256,
                    "modificationTime": 1, "dataChange": True,
                    "partitionValues": {},
                    "stats": '{"numRecords":16,"minValues":{"id":1},"maxValues":{"id":16},"nullCount":{"id":0}}'}}


def write_commit(path, actions):
    path.write_text("".join(json.dumps(action) + "\n" for action in actions))


def create_fixtures(root):
    for scenario in ("json", "compacted", "checkpoint", "checkpoint-tail", "checkpoint-structured-stats"):
        table = root / scenario
        log = table / "_delta_log"
        if not log.exists():
            config = None
            if scenario == "checkpoint-structured-stats":
                config = {"delta.checkpoint.writeStatsAsStruct": "true",
                          "delta.checkpoint.writeStatsAsJson": "false"}
            write_deltalake(str(table), pa.table({"id": [1]}), configuration=config)
        initial = log / "00000000000000000000.json"
        actions = [json.loads(line) for line in initial.read_text().splitlines()]
        write_commit(initial, (action for action in actions if "add" not in action))
        for version in range(1, 21):
            start = (version - 1) * 500
            write_commit(log / f"{version:020d}.json", map(add_row, range(start, start + 500)))
        checkpoint = log / "00000000000000000020.checkpoint.parquet"
        if scenario not in ("json", "compacted") and not checkpoint.exists():
            DeltaTable(str(table)).create_checkpoint()
        if scenario == "compacted":
            compacted = log / "00000000000000000001.00000000000000000020.compacted.json"
            write_commit(compacted, map(add_row, range(10_000)))
        tail = log / "00000000000000000021.json"
        if scenario == "checkpoint-tail":
            write_commit(tail, map(add_row, range(10_000, 10_100)))
    create_shuffled_compaction_fixture(root)
    create_checksum_fixtures(root)
    create_long_history_fixture(root / "checkpoint", root / "checkpoint-long-history")
    create_in_commit_timestamp_fixture(root / "in-commit-timestamp-lookup")
    create_sidecar_fixture(root / "checkpoint", root / "checkpoint-sidecars")
    create_row_tracking_log_fixture(root)
    create_checkpoint_statistics_fixtures(root)
    create_scan_fixture(root / "deletion-vector-scan")
    create_scan_fixture(root / "deletion-vector-small-row-groups", row_group_size=512)
    create_scan_fixture(root / "deletion-vector-fragmented-runs", row_group_size=512, fragmented_runs=True)
    create_scan_fixture(root / "deletion-vector-fully-deleted", row_group_size=64, fully_deleted=True)
    create_count_only_fixture(root / "deletion-vector-count-only")
    create_mapped_fixture(root / "mapped-table-scan")
    create_projection_fixture(root / "projection")
    create_change_feed_fixture(root / "change-feed")
    create_partition_text_fixture(root / "partition-text-scan")
    create_tracked_fixture(root / "row-tracking-scan")
    create_tracked_fixture(root / "row-tracking-mixed-scan", mixed=True)
    create_timestamp_fixture(root / "timestamp-scan")
    create_decimal_fixture(root / "decimal-widening-scan")


def create_checksum_fixtures(root):
    log = root / "json" / "_delta_log"
    initial = (log / "00000000000000000000.json").read_text()
    actions = {key: value for line in initial.splitlines() for key, value in json.loads(line).items()}
    adds = [add_row(index)["add"] for index in range(10_000)]
    checksum = {
        "metadata": actions["metaData"], "protocol": actions["protocol"],
        "numFiles": len(adds), "tableSizeBytes": sum(add["size"] for add in adds),
        "numMetadata": 1, "numProtocol": 1, "allFiles": adds,
    }
    recovery = root / "version-checksum-recovery" / "_delta_log"
    recovery.mkdir(parents=True, exist_ok=True)
    (recovery / "00000000000000000000.json").write_text(initial)
    prefix = "".join(json.dumps({"add": add}) + "\n" for add in adds[:1000])
    (recovery / "00000000000000000001.json").write_text(prefix + '{"add":{"pa')
    checksum.update(setTransactions=[], domainMetadata=[])
    (recovery / "00000000000000000001.crc").write_text(json.dumps(checksum))

    boundary = root / "version-checksum-line-boundary" / "_delta_log"
    boundary.mkdir(parents=True, exist_ok=True)
    (boundary / "00000000000000000000.json").write_text(initial)
    txn_id = "4f2dfeea-3b8f-41a1-9f14-925a1a50bb50"
    provenance = json.dumps({"commitInfo": {"txnId": txn_id}}) + "\n"
    last_commit = log / "00000000000000000020.json"
    last_commit.write_text(provenance + "".join(json.dumps({"add": add}) + "\n" for add in adds[9500:]))
    checksum["txnId"] = txn_id
    (log / "00000000000000000020.crc").write_text(json.dumps(checksum))
    (boundary / "00000000000000000001.json").write_text(provenance + prefix)
    (boundary / "00000000000000000001.crc").write_text(json.dumps(checksum))


def create_long_history_fixture(source, root):
    log = root / "_delta_log"
    log.mkdir(parents=True, exist_ok=True)
    version = 25_000
    checkpoint = log / f"{version:020d}.checkpoint.parquet"
    if checkpoint.exists():
        return
    source_log = source / "_delta_log"
    for number in range(21):
        name = f"{number:020d}.json"
        shutil.copyfile(source_log / name, log / name)
    for number in range(21, version + 1):
        write_commit(log / f"{number:020d}.json", [{"commitInfo": {"timestamp": number}}])
    (log / "_last_checkpoint").write_text(json.dumps({"version": version, "size": 10_002}))
    shutil.copyfile(source_log / "00000000000000000020.checkpoint.parquet", checkpoint)


def create_in_commit_timestamp_fixture(root):
    log = root / "_delta_log"
    log.mkdir(parents=True, exist_ok=True)
    schema = {"type": "struct", "fields": []}
    protocol = {"minReaderVersion": 1, "minWriterVersion": 7, "writerFeatures": ["inCommitTimestamp"]}
    metadata = {
        "id": "00000000-0000-0000-0000-000000000001",
        "format": {"provider": "parquet", "options": {}},
        "schemaString": json.dumps(schema),
        "partitionColumns": [],
        "configuration": {"delta.enableInCommitTimestamps": "true"},
    }
    for version in range(1001):
        actions = [{"commitInfo": {"inCommitTimestamp": 10_000 + version}}]
        if version == 0:
            actions.extend([{"protocol": protocol}, {"metaData": metadata}])
        write_commit(log / f"{version:020d}.json", actions)


def create_row_tracking_log_fixture(root):
    source = root / "json" / "_delta_log"
    target = root / "json-row-tracking" / "_delta_log"
    target.mkdir(parents=True, exist_ok=True)
    for version in range(21):
        name = f"{version:020d}.json"
        actions = [json.loads(line) for line in (source / name).read_text().splitlines()]
        next_row_id = (version - 1) * 8_000
        for action in actions:
            if "protocol" in action:
                action["protocol"].update(
                    minWriterVersion=7, writerFeatures=["rowTracking", "domainMetadata"]
                )
            if "add" in action:
                action["add"].update(baseRowId=next_row_id, defaultRowCommitVersion=version)
                next_row_id += 16
        if version:
            actions.append({"domainMetadata": {
                "domain": "delta.rowTracking",
                "configuration": json.dumps({"rowIdHighWaterMark": next_row_id - 1}),
                "removed": False,
            }})
        write_commit(target / name, actions)


def create_sidecar_fixture(source, root):
    version = 20
    log = root / "_delta_log"
    manifest = log / f"{version:020d}.checkpoint.11111111-2222-4333-8444-555555555555.json"
    if manifest.exists():
        return
    sidecars = log / "_sidecars"
    sidecars.mkdir(parents=True, exist_ok=True)
    source_log = source / "_delta_log"
    initial = (source_log / "00000000000000000000.json").read_text()
    actions = [json.loads(line) for line in initial.splitlines()]
    metadata = next(action for action in actions if "metaData" in action)
    protocol = {"minReaderVersion": 3, "minWriterVersion": 7,
                "readerFeatures": ["v2Checkpoint"], "writerFeatures": ["v2Checkpoint"]}
    adds = pq.read_table(source_log / f"{version:020d}.checkpoint.parquet", columns=["add"])
    adds = adds.filter(pc.is_valid(adds["add"]))
    commit = f"{version:020d}.json"
    shutil.copyfile(source_log / commit, log / commit)
    actions = [{"checkpointMetadata": {"version": version}}, {"protocol": protocol}, metadata]
    for offset in range(0, len(adds), 100):
        name = f"adds-{offset:05d}.parquet"
        path = sidecars / name
        pq.write_table(adds.slice(offset, 100), path, compression="zstd")
        actions.append({"sidecar": {"path": name, "sizeInBytes": path.stat().st_size, "modificationTime": 1}})
    write_commit(manifest, actions)


def create_shuffled_compaction_fixture(root):
    log = root / "shuffled-compacted" / "_delta_log"
    log.mkdir(parents=True, exist_ok=True)
    initial = "00000000000000000000.json"
    (log / initial).write_bytes((root / "compacted" / "_delta_log" / initial).read_bytes())
    order = list(range(10_000))
    random.Random(110026).shuffle(order)
    write_commit(log / "00000000000000000001.00000000000000000020.compacted.json",
                 map(add_row, order))


def create_checkpoint_statistics_fixtures(root):
    filename = "00000000000000000020.checkpoint.parquet"
    cases = tuple((varied, shuffled, root / name / "_delta_log" / filename)
                  for name, varied, shuffled, _ in CHECKPOINT_STATISTICS_CASES)
    if all(target.exists() for _, _, target in cases):
        return
    source = root / "checkpoint-structured-stats" / "_delta_log" / filename
    table = pq.read_table(source)
    rows = table.to_pylist()
    other_actions = list(filter(lambda row: row["add"] is None, rows))
    adds = sorted((row for row in rows if row["add"] is not None), key=lambda row: row["add"]["path"])
    order = list(range(len(adds)))
    random.Random(110026).shuffle(order)
    for varied in (False, True):
        if varied:
            # Keep each file's statistics the same across both path orders.
            for i, row in enumerate(adds):
                stats = row["add"]["stats_parsed"]
                stats["minValues"]["id"] = i
                stats["maxValues"]["id"] = i + 15
        for case_varied, shuffled, target in cases:
            if case_varied == varied and not target.exists():
                data = list(map(adds.__getitem__, order)) if shuffled else adds
                target.parent.mkdir(parents=True, exist_ok=True)
                pq.write_table(pa.Table.from_pylist(other_actions + data, schema=table.schema),
                               target, compression="zstd")


def create_timestamp_fixture(root):
    log = root / "_delta_log"
    log.mkdir(parents=True, exist_ok=True)
    data = root / "timestamps.parquet"
    if not data.exists():
        # Millisecond physical storage must be converted to Delta microseconds.
        values = pa.array(range(65_536), type=pa.int64()).cast(pa.timestamp("ms", "UTC"))
        pq.write_table(pa.table({"ts": values}), data, row_group_size=8192, compression="NONE")
    protocol = {"minReaderVersion": 1, "minWriterVersion": 2}
    field = {"name": "ts", "type": "timestamp", "nullable": True, "metadata": {}}
    metadata = {"id": "timestamps", "partitionColumns": [], "configuration": {},
                "format": {"provider": "parquet", "options": {}},
                "schemaString": json.dumps({"type": "struct", "fields": [field]})}
    add = {"path": data.name, "size": data.stat().st_size, "modificationTime": 0,
           "dataChange": True, "partitionValues": {}}
    write_commit(log / "00000000000000000000.json",
                 [{"protocol": protocol}, {"metaData": metadata}, {"add": add}])


def create_decimal_fixture(root):
    log = root / "_delta_log"
    log.mkdir(parents=True, exist_ok=True)
    data = root / "decimals.parquet"
    if not data.exists():
        values = pa.array([Decimal(i).scaleb(-2) for i in range(-32_768, 32_768)],
                          type=pa.decimal128(20, 2))
        pq.write_table(pa.table({"same_scale": values, "higher_scale": values}), data,
                       row_group_size=8192, compression="NONE", use_dictionary=False)
    protocol = {"minReaderVersion": 3, "minWriterVersion": 7,
                "readerFeatures": ["typeWidening"], "writerFeatures": ["typeWidening"]}

    def field(name, target):
        return {"name": name, "type": target, "nullable": True,
                "metadata": {"delta.typeChanges": [{"fromType": "decimal(20,2)", "toType": target}]}}

    fields = [field("same_scale", "decimal(38,2)"), field("higher_scale", "decimal(38,20)")]
    metadata = {"id": "decimals", "partitionColumns": [], "configuration": {"delta.enableTypeWidening": "true"},
                "format": {"provider": "parquet", "options": {}},
                "schemaString": json.dumps({"type": "struct", "fields": fields})}
    add = {"path": data.name, "size": data.stat().st_size, "modificationTime": 0,
           "dataChange": True, "partitionValues": {}}
    write_commit(log / "00000000000000000000.json", [{"protocol": protocol}, {"metaData": metadata}, {"add": add}])


def create_tracked_fixture(root, mixed=False):
    log = root / "_delta_log"
    log.mkdir(parents=True, exist_ok=True)
    commit = log / "00000000000000000000.json"
    data = root / "rows.parquet"
    if not data.exists():
        rows = range(65_536)
        columns = {"id": pa.array(rows, type=pa.int64())}
        if mixed:
            columns["_row_id"] = pa.array([None if i % 2 == 0 else i + 100_000 for i in rows], pa.int64())
            columns["_row_version"] = pa.array([None if i % 3 == 0 else 1 for i in rows], pa.int64())
        pq.write_table(pa.table(columns), data, row_group_size=8192, compression="NONE")
    protocol = {"minReaderVersion": 1, "minWriterVersion": 7,
                "writerFeatures": ["rowTracking", "domainMetadata"]}
    if mixed:
        protocol["minReaderVersion"] = 3
        protocol["readerFeatures"] = ["deletionVectors"]
        protocol["writerFeatures"].append("deletionVectors")
    schema = {"type": "struct", "fields": [
        {"name": "id", "type": "long", "nullable": True, "metadata": {}},
    ]}
    metadata = {"id": "row-tracking-benchmark", "schemaString": json.dumps(schema),
                "format": {"provider": "parquet", "options": {}},
                "partitionColumns": [], "configuration": {
                    "delta.enableRowTracking": "true",
                    "delta.rowTracking.materializedRowIdColumnName": "_row_id",
                    "delta.rowTracking.materializedRowCommitVersionColumnName": "_row_version",
                }}
    add = {"path": data.name, "size": data.stat().st_size, "modificationTime": 0,
           "dataChange": True, "partitionValues": {}, "baseRowId": 1000,
           "defaultRowCommitVersion": 0}
    if mixed:
        store_every_eighth_deleted(root, add, 65_536)
    write_commit(commit, [{"protocol": protocol}, {"metaData": metadata}, {"add": add}])


def create_mapped_fixture(root):
    log = root / "_delta_log"
    log.mkdir(parents=True, exist_ok=True)
    commit = log / "00000000000000000000.json"
    if commit.exists():
        return
    fields = [
        {"name": name, "type": ty, "nullable": True, "metadata": {
            "delta.columnMapping.physicalName": physical, "delta.columnMapping.id": ident,
        }}
        for name, ty, physical, ident in (
            ("logical_id", "long", "col_id", 1),
            ("region", "string", "col_region", 2),
            ("added_later", "long", "col_new", 3),
        )
    ]
    protocol = {"minReaderVersion": 3, "minWriterVersion": 7,
                "readerFeatures": ["columnMapping"], "writerFeatures": ["columnMapping"]}
    metadata = {"id": "mapped-scan-benchmark", "format": {"provider": "parquet", "options": {}},
                "schemaString": json.dumps({"type": "struct", "fields": fields}),
                "partitionColumns": ["region"], "configuration": {"delta.columnMapping.mode": "id"}}
    schema = pa.schema([pa.field("file_id", pa.int64(), metadata={"PARQUET:field_id": "1"})])
    actions = [{"protocol": protocol}, {"metaData": metadata}]
    for i in range(4):
        data = root / f"part-{i:04d}.parquet"
        rows = pa.Table.from_arrays([pa.array(range(i * 16_384, (i + 1) * 16_384))], schema=schema)
        pq.write_table(rows, data, row_group_size=8192, compression="NONE")
        actions.append({"add": {"path": data.name, "size": data.stat().st_size,
                                "modificationTime": 0, "dataChange": True,
                                "partitionValues": {"col_region": "europe"}}})
    write_commit(commit, actions)


def create_projection_fixture(root):
    if (root / "_delta_log" / "00000000000000000000.json").exists():
        return
    ids = pa.array(range(65_536), type=pa.int64())
    columns = {"id": ids}
    for i in range(1, 17):
        columns[f"value_{i}"] = pc.add(ids, i)
    write_deltalake(str(root), pa.table(columns), target_file_size=64 * 1024 * 1024,
                    writer_properties=WriterProperties(compression="UNCOMPRESSED"))


def create_change_feed_fixture(root):
    log = root / "_delta_log"
    log.mkdir(parents=True, exist_ok=True)
    rows = 65_536
    data = root / "rows.parquet"
    if not data.exists():
        pq.write_table(pa.table({"id": pa.array(range(rows), type=pa.int64())}), data,
                       row_group_size=8192, compression="NONE")
    metadata = {"id": "change-feed-bench", "partitionColumns": [],
                "format": {"provider": "parquet", "options": {}},
                "configuration": {"delta.enableChangeDataFeed": "true"},
                "schemaString": json.dumps({"type": "struct", "fields": [
                    {"name": "id", "type": "long", "nullable": True, "metadata": {}}]})}
    file = {"path": data.name, "size": data.stat().st_size, "modificationTime": 0,
            "dataChange": True, "partitionValues": {}}
    write_commit(log / "00000000000000000000.json", [
        {"protocol": {"minReaderVersion": 1, "minWriterVersion": 4}},
        {"metaData": metadata}, {"add": file},
    ])
    write_commit(log / "00000000000000000001.json", [{"remove": {
        "path": data.name, "dataChange": True, "extendedFileMetadata": True,
        "size": data.stat().st_size, "partitionValues": {},
    }}])


def create_partition_text_fixture(root):
    if (root / "_delta_log" / "00000000000000000000.json").exists():
        return
    values = pa.table({
        "id": pa.array(range(65_536), type=pa.int64()),
        "region": pa.repeat("région-europe", 65_536),
    })
    write_deltalake(str(root), values, partition_by=["region"], target_file_size=64 * 1024 * 1024)


def create_scan_fixture(root, row_group_size=8192, fragmented_runs=False, fully_deleted=False):
    commit = root / "_delta_log" / "00000000000000000000.json"
    if commit.exists():
        return
    rows = 65_536
    ids = pa.table({"id": pa.array(range(rows), type=pa.int64())})
    write_deltalake(str(root), ids, configuration={"delta.enableDeletionVectors": "true"})
    actions = [json.loads(line) for line in commit.read_text().splitlines()]
    add = next(action["add"] for action in actions if "add" in action)
    data = root / add["path"]
    pq.write_table(ids, data, row_group_size=row_group_size, compression="NONE")
    add["size"] = data.stat().st_size
    if fully_deleted:
        # One run covers every row, with no gather or data-page decoding needed.
        bitmap = struct.pack("<IQIIB5H", 1681511377, 1, 0, 12347, 1, 0, rows - 1, 1, 0, rows - 1)
        store_deletion_vector(root, add, bitmap, rows, rows)
    else:
        store_every_eighth_deleted(root, add, rows, fragmented_runs=fragmented_runs)
    write_commit(commit, actions)


def store_every_eighth_deleted(root, add, rows, fragmented_runs=False):
    if fragmented_runs:
        # The same rows, encoded as one Roaring run container with no offset table.
        bitmap = struct.pack("<IQIIBHHH", 1681511377, 1, 0, 12347, 1, 0, rows // 8 - 1, rows // 8)
        bitmap += b"".join(struct.pack("<HH", row, 0) for row in range(0, rows, 8))
    else:
        # One Roaring bitmap container holds the whole file's physical positions.
        bitmap = struct.pack("<IQIIIHHI", 1681511377, 1, 0, 12346, 1, 0, rows // 8 - 1, 16)
        bitmap += struct.pack("<1024Q", *([0x0101010101010101] * 1024))
    store_deletion_vector(root, add, bitmap, rows, rows // 8)


def create_count_only_fixture(root):
    commit = root / "_delta_log" / "00000000000000000000.json"
    if commit.exists():
        return
    rows = 65_536
    write_deltalake(str(root), pa.table({"id": pa.array(range(rows)), "part": pa.repeat(7, rows)}),
                    partition_by=["part"], configuration={"delta.enableDeletionVectors": "true"})
    actions = [json.loads(line) for line in commit.read_text().splitlines()]
    add = next(action["add"] for action in actions if "add" in action)
    meta = next(action["metaData"] for action in actions if "metaData" in action)
    schema = json.loads(meta["schemaString"])
    schema["fields"] = [field for field in schema["fields"] if field["name"] == "part"]
    meta["schemaString"] = json.dumps(schema)
    store_every_eighth_deleted(root, add, rows)
    write_commit(commit, actions)


def store_deletion_vector(root, add, bitmap, rows, cardinality):
    dv_file = root / "deletion_vector.bin"
    dv_file.write_bytes(b"\x01" + struct.pack(">I", len(bitmap))
                        + bitmap + struct.pack(">I", zlib.crc32(bitmap)))
    stats = json.loads(add.get("stats", "{}"))
    stats["numRecords"] = rows
    add["stats"] = json.dumps({**stats, "tightBounds": False})
    add["deletionVector"] = {
        "storageType": "p", "pathOrInlineDv": dv_file.resolve().as_uri(),
        "offset": 1, "sizeInBytes": len(bitmap), "cardinality": cardinality,
    }


def time_reference(root, repetitions):
    for scenario in ("json", "json-row-tracking", "checkpoint", "checkpoint-tail", "checkpoint-long-history",
                     "checkpoint-sidecars"):
        samples = []
        expected = 10_100 if scenario == "checkpoint-tail" else 10_000
        path = str(root / scenario)
        for _ in range(repetitions):
            start = perf_counter_ns()
            count = len(DeltaTable(path).file_uris())
            samples.append(perf_counter_ns() - start)
            if count != expected:
                raise ValueError(f"{scenario}: expected {expected} files, got {count}")
        print(f"{scenario}: {mean(samples) / 1_000_000:.3f} ms mean, "
              f"{median(samples) / 1_000_000:.3f} ms median")
    samples = []
    for _ in range(repetitions):
        start = perf_counter_ns()
        table = DeltaTable(str(root / "deletion-vector-count-only"))
        masks = pa.RecordBatchReader.from_stream(table.deletion_vectors()).read_all()
        mask = masks.column("selection_vector")[0].values
        n = pq.ParquetFile(table.file_uris()[0]).metadata.num_rows
        total = pc.sum(mask).as_py() + n - len(mask)
        samples.append(perf_counter_ns() - start)
        if total != 57_344:
            raise ValueError(f"unexpected live partition row count: {total}")
    print(f"deletion-vector-count-only: {mean(samples) / 1_000_000:.3f} ms mean, "
          f"{median(samples) / 1_000_000:.3f} ms median")
    for scenario, _, _, expected_minimums in CHECKPOINT_STATISTICS_CASES:
        samples = []
        expected = (10_000, 160_000, expected_minimums, expected_minimums + 150_000, 0)
        for _ in range(repetitions):
            start = perf_counter_ns()
            files = pa.table(DeltaTable(str(root / scenario)).get_add_actions())
            columns = (files.column("num_records"), pc.struct_field(files.column("min"), "id"),
                       pc.struct_field(files.column("max"), "id"), pc.struct_field(files.column("null_count"), "id"))
            totals = (len(files), *(pc.sum(column, skip_nulls=False).as_py() for column in columns))
            samples.append(perf_counter_ns() - start)
            if totals != expected:
                raise ValueError(f"{scenario}: unexpected checkpoint file count or statistics totals: {totals}")
        print(f"{scenario}: {mean(samples) / 1_000_000:.3f} ms mean, "
              f"{median(samples) / 1_000_000:.3f} ms median")
    for scenario in (
        "deletion-vector-scan", "deletion-vector-small-row-groups", "deletion-vector-fragmented-runs",
        "deletion-vector-fully-deleted",
    ):
        samples = []
        for _ in range(repetitions):
            start = perf_counter_ns()
            table = DeltaTable(str(root / scenario))
            data = pq.read_table(table.file_uris()[0], columns=["id"])
            masks = pa.RecordBatchReader.from_stream(table.deletion_vectors()).read_all()
            mask = masks.column("selection_vector")[0].values
            mask = pa.concat_arrays([mask, pa.repeat(True, data.num_rows - len(mask))])
            total = pc.sum(pc.filter(data.column("id"), mask)).as_py()
            samples.append(perf_counter_ns() - start)
            expected = None if scenario == "deletion-vector-fully-deleted" else 1_879_048_192
            if total != expected:
                raise ValueError(f"{scenario}: unexpected scan sum: {total}")
        print(f"{scenario}: {mean(samples) / 1_000_000:.3f} ms mean, "
              f"{median(samples) / 1_000_000:.3f} ms median")
    samples = []
    for _ in range(repetitions):
        start = perf_counter_ns()
        table = DeltaTable(str(root / "mapped-table-scan"))
        total = 0
        for path in table.file_uris():
            file = pq.ParquetFile(path)
            key = next(field.name for field in file.schema_arrow
                       if field.metadata.get(b"PARQUET:field_id") == b"1")
            ids = file.read(columns=[key]).column(key)
            projected = pa.table({"logical_id": ids, "region": pa.repeat("europe", len(ids)),
                                  "added_later": pa.nulls(len(ids), type=pa.int64())})
            total += pc.sum(projected.column("logical_id")).as_py()
        samples.append(perf_counter_ns() - start)
        if total != 2_147_450_880:
            raise ValueError(f"unexpected mapped scan sum: {total}")
    print(f"mapped-table-scan: {mean(samples) / 1_000_000:.3f} ms mean, "
          f"{median(samples) / 1_000_000:.3f} ms median")
    samples = []
    for _ in range(repetitions):
        start = perf_counter_ns()
        table = DeltaTable(str(root / "partition-text-scan")).to_pyarrow_table()
        regions = table.column("region")
        total = sum(chunk.buffers()[2].size for chunk in regions.chunks)
        if total != 65_536 * 14 or regions[-1].as_py() != "région-europe":
            raise ValueError("unexpected partition column")
        samples.append(perf_counter_ns() - start)
    print(f"partition-text-scan: {mean(samples) / 1_000_000:.3f} ms mean, "
          f"{median(samples) / 1_000_000:.3f} ms median")
    for mixed, scenario, expected in ((False, "row-tracking-scan", 4_360_437_760),
                                      (True, "row-tracking-mixed-scan", 7_059_510_613)):
        samples = []
        for _ in range(repetitions):
            start = perf_counter_ns()
            table = DeltaTable(str(root / scenario))
            data = pq.read_table(table.file_uris()[0])
            ids = data.column("id")
            row_ids = pc.add(pa.array(range(len(ids)), type=pa.int64()), 1000)
            versions = pa.repeat(0, len(ids))
            if mixed:
                row_ids = pc.coalesce(data.column("_row_id"), row_ids)
                versions = pc.coalesce(data.column("_row_version"), versions)
                masks = pa.RecordBatchReader.from_stream(table.deletion_vectors()).read_all()
                mask = masks.column("selection_vector")[0].values
                mask = pa.concat_arrays([mask, pa.repeat(True, data.num_rows - len(mask))])
                ids, row_ids, versions = (pc.filter(column, mask) for column in (ids, row_ids, versions))
            total = sum(pc.sum(column).as_py() for column in (ids, row_ids, versions))
            samples.append(perf_counter_ns() - start)
            if total != expected:
                raise ValueError(f"unexpected tracked scan sum: {total}")
        print(f"{scenario}: {mean(samples) / 1_000_000:.3f} ms mean, "
              f"{median(samples) / 1_000_000:.3f} ms median")
    samples = []
    for _ in range(repetitions):
        start = perf_counter_ns()
        table = DeltaTable(str(root / "timestamp-scan")).to_pyarrow_table()
        micros = pc.cast(pc.cast(table.column("ts"), pa.timestamp("us", "UTC")), pa.int64())
        total = pc.sum(micros).as_py()
        samples.append(perf_counter_ns() - start)
        if total != 2_147_450_880_000:
            raise ValueError(f"unexpected timestamp scan sum: {total}")
    print(f"timestamp-scan: {mean(samples) / 1_000_000:.3f} ms mean, "
          f"{median(samples) / 1_000_000:.3f} ms median")
    samples = []
    for _ in range(repetitions):
        start = perf_counter_ns()
        table = DeltaTable(str(root / "decimal-widening-scan"))
        reader = pq.ParquetFile(table.file_uris()[0])
        count = 0
        for batch in reader.iter_batches(batch_size=8192):
            same = pc.cast(batch.column(0), pa.decimal128(38, 2))
            scaled = pc.cast(batch.column(1), pa.decimal128(38, 20))
            count += batch.num_rows
            value = Decimal(count - 1 - 32_768).scaleb(-2)
            if same[-1].as_py() != value or scaled[-1].as_py() != value:
                raise ValueError("unexpected widened decimal values")
        samples.append(perf_counter_ns() - start)
        if count != 65_536:
            raise ValueError(f"unexpected decimal row count: {count}")
    print(f"decimal-widening-scan: {mean(samples) / 1_000_000:.3f} ms mean, "
          f"{median(samples) / 1_000_000:.3f} ms median")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("fixtures", type=Path)
    parser.add_argument("--reference", type=int, metavar="REPETITIONS")
    args = parser.parse_args()
    if args.reference:
        time_reference(args.fixtures, args.reference)
    else:
        create_fixtures(args.fixtures)
