#!/usr/bin/env python3
"""wireform-delta ↔ deltalake interop driver.

Builds a small Delta table on disk with the official ``deltalake``
(delta-rs) writer, then runs ``wireform-delta-interop-probe`` over
the same table and asserts that wireform's parsed view of the
transaction log matches deltalake's.

Coverage:
    * an initial WRITE commit
    * an APPEND commit
    * an OVERWRITE commit (which removes the original add and
      writes a new one)
    * a partitioned table (separate from the unpartitioned case)
    * a checkpointed table and a synthetic Parquet checkpoint with
      deletion-vector tombstones and signed transaction identifiers.

Usage:
    python3 wireform-delta/scripts/delta_interop.py
"""

from __future__ import annotations

import json
import math
import os
import struct
import subprocess
import sys
import tempfile
import zlib
from datetime import date, datetime, timezone
from decimal import Decimal
from pathlib import Path
from urllib.parse import quote

import pyarrow as pa
import pyarrow.parquet as pq
from deltalake import DeltaTable, write_deltalake

ROOT = Path(__file__).resolve().parent.parent


class Failures(list):
    def add(self, where: str, msg: str) -> None:
        self.append((where, msg))


def expect_eq(failures: Failures, where: str, label: str, got, want) -> None:
    if got != want:
        failures.add(where, f"{label} = {got!r}, want {want!r}")


def deltalake_relative_files(dt: DeltaTable, root: Path) -> list[str]:
    """The deltalake Python binding exposes file_uris() returning
    absolute paths; the wireform-delta probe carries the same
    paths the JSON commits do (relative). Strip the root prefix
    so the two views can be compared directly.
    """
    base = str(root.resolve()) + "/"
    out = []
    for u in dt.file_uris():
        # file_uris() may include a 'file://' prefix on some platforms.
        p = u[len("file://"):] if u.startswith("file://") else u
        if p.startswith(base):
            p = p[len(base):]
        out.append(p)
    return out


def run_probe(table_root: Path, version: int | None = None) -> dict:
    out_json = table_root.parent / (table_root.name + ".probe.json")
    version_args = [] if version is None else ["--at", str(version)]
    subprocess.run(
        [
            "cabal", "run",
            "wireform-delta:wireform-delta-interop-probe",
            "--",
            *version_args, str(table_root), str(out_json),
        ],
        cwd=ROOT.parent, check=True,
    )
    with out_json.open() as f:
        return json.load(f)


def scan_ids(table_root: Path, *, expect_failure: bool = False) -> list[int] | str:
    output = table_root.parent / (table_root.name + ".scan.json")
    result = subprocess.run(
        ["cabal", "run", "wireform-delta:wireform-delta-interop-probe", "--",
         "--scan-ids", str(table_root), str(output)],
        cwd=ROOT.parent, check=not expect_failure, capture_output=expect_failure,
        text=True,
    )
    if expect_failure:
        if result.returncode == 0:
            raise AssertionError("invalid deletion vector scan succeeded")
        return result.stderr
    return json.loads(output.read_text())


def case_unpartitioned(failures: Failures, root: Path) -> None:
    print(f"\n== unpartitioned table at {root}")
    write_deltalake(str(root), pa.table({
        "id":   [1, 2, 3],
        "name": ["a", "b", "c"],
    }))
    write_deltalake(str(root), pa.table({"id": [4], "name": ["d"]}), mode="append")
    write_deltalake(str(root), pa.table({"id": [99], "name": ["only"]}),
                    mode="overwrite")

    summary = run_probe(root)

    dt = DeltaTable(str(root))
    deltalake_files = sorted(deltalake_relative_files(dt, root))
    deltalake_schema = [f.name for f in dt.schema().fields]

    # 3 commits, each contributes a different action set (the first
    # carries protocol + metadata + add; the second adds; the third
    # removes the original adds and adds a new one).
    expect_eq(failures, "unpartitioned", "num_commits", summary["num_commits"], 3)
    expect_eq(failures, "unpartitioned", "version (vs deltalake)",
              summary["version"], dt.version())

    # Active files: deltalake's view of the live file set must
    # exactly match wireform's. After OVERWRITE only the third
    # add survives.
    wireform_files = sorted(f["path"] for f in summary["active_files"])
    expect_eq(failures, "unpartitioned", "active files",
              wireform_files, deltalake_files)

    expect_eq(failures, "unpartitioned", "active_file_count",
              summary["active_file_count"], len(deltalake_files))

    # Protocol min versions match what delta-rs writes today
    # (reader-1 / writer-2 for the simple unpartitioned case).
    proto = summary["protocol"]
    expect_eq(failures, "unpartitioned", "protocol present",
              proto is not None, True)
    if proto is not None:
        if not (proto["min_reader_version"] >= 1):
            failures.add("unpartitioned", f"protocol.min_reader_version = {proto['min_reader_version']}")
        if not (proto["min_writer_version"] >= 2):
            failures.add("unpartitioned", f"protocol.min_writer_version = {proto['min_writer_version']}")

    md = summary["metadata"]
    expect_eq(failures, "unpartitioned", "metadata present", md is not None, True)
    if md is not None:
        expect_eq(failures, "unpartitioned", "metadata.partition_columns",
                  md["partition_columns"], [])
        expect_eq(failures, "unpartitioned", "metadata.schema_field_names",
                  md["schema_field_names"], deltalake_schema)

    expect_eq(failures, "unpartitioned", "last_commit_operation",
              summary["last_commit_operation"], "WRITE")
    if not failures or all(f[0] != "unpartitioned" for f in failures):
        print(f"  OK   unpartitioned: {len(deltalake_files)} active files, "
              f"protocol={proto['min_reader_version']}/{proto['min_writer_version']}")


def case_partitioned(failures: Failures, root: Path) -> None:
    print(f"\n== partitioned table at {root}")
    write_deltalake(
        str(root),
        pa.table({"region": ["us", "us", "eu"], "id": [1, 2, 3]}),
        partition_by=["region"],
    )
    write_deltalake(
        str(root),
        pa.table({"region": ["eu"], "id": [4]}),
        partition_by=["region"],
        mode="append",
    )

    summary = run_probe(root)
    dt = DeltaTable(str(root))
    deltalake_files = sorted(deltalake_relative_files(dt, root))

    expect_eq(failures, "partitioned", "num_commits", summary["num_commits"], 2)
    wireform_files = sorted(f["path"] for f in summary["active_files"])
    expect_eq(failures, "partitioned", "active files",
              wireform_files, deltalake_files)

    # Every active file in our snapshot should have a region partition value.
    for f in summary["active_files"]:
        pv = f.get("partition_values") or {}
        if "region" not in pv or pv["region"] not in ("us", "eu"):
            failures.add("partitioned",
                         f"active file {f['path']} partition_values = {pv!r}")

    md = summary["metadata"]
    if md is None:
        failures.add("partitioned", "metadata missing")
    else:
        expect_eq(failures, "partitioned", "metadata.partition_columns",
                  md["partition_columns"], ["region"])

    if not failures or all(f[0] != "partitioned" for f in failures):
        print(f"  OK   partitioned: {len(deltalake_files)} active files across 2 regions")


def case_partitioned_checkpoint(failures: Failures, root: Path) -> None:
    """Cross-check the *checkpoint Parquet decoder* against
    deltalake on a partitioned table — verifies that the
    map<string, string> 'partitionValues' leaf and the
    list<string> 'metaData.partitionColumns' leaf thread through
    correctly."""
    print(f"\n== partitioned + checkpointed table at {root}")
    regions = [None, "", "eu", "us"]
    for i in range(12):
        write_deltalake(
            str(root),
            pa.table({"region": pa.array([regions[i % len(regions)]], type=pa.string()),
                      "id": [i]}),
            partition_by=["region"],
            mode="append" if i > 0 else "error",
        )
    dt = DeltaTable(str(root))
    expected_rows = sorted(dt.to_pyarrow_table().to_pylist(), key=lambda row: row["id"])
    output = root.parent / (root.name + ".table-scan.json")
    command = ["cabal", "run", "wireform-delta:wireform-delta-interop-probe", "--",
               "--scan-table", str(root), str(output)]
    subprocess.run(command, cwd=ROOT.parent, check=True)
    expect_eq(failures, "part-ckpt", "partition rows from numbered commits",
              sorted(json.loads(output.read_text())["rows"], key=lambda row: row["id"]), expected_rows)
    dt.create_checkpoint()
    expected_partitions = {
        row["path"]: row["partition"]
        for row in pa.table(dt.get_add_actions()).to_pylist()
    }
    summary = run_probe(root)

    # The post-checkpoint snapshot must match deltalake.
    deltalake_files = sorted(deltalake_relative_files(dt, root))
    wireform_files  = sorted(f["path"] for f in summary["active_files"])
    expect_eq(failures, "part-ckpt", "active files",
              wireform_files, deltalake_files)

    # The protocol maps both null and the empty string to a null partition.
    for f in summary["active_files"]:
        expect_eq(failures, "part-ckpt", f"{f['path']} partition values",
                  f["partition_values"], expected_partitions[f["path"]])
    subprocess.run(command, cwd=ROOT.parent, check=True)
    expect_eq(failures, "part-ckpt", "partition rows recovered from the checkpoint",
              sorted(json.loads(output.read_text())["rows"], key=lambda row: row["id"]), expected_rows)

    # Standalone checkpoint Parquet decoder agrees on the file
    # set AND on partition columns.
    ckpt_files = summary["checkpoint_active_files"]
    if ckpt_files is None:
        failures.add("part-ckpt", "checkpoint_active_files = null")
    else:
        expect_eq(failures, "part-ckpt", "checkpoint vs JSON files",
                  sorted(ckpt_files), deltalake_files)

    ckpt_meta = summary["checkpoint_metadata"]
    if ckpt_meta is None:
        failures.add("part-ckpt",
                     "checkpoint metadata row not surfaced")
    else:
        expect_eq(failures, "part-ckpt", "checkpoint partition_columns",
                  ckpt_meta["partition_columns"], ["region"])

    if not any(f[0] == "part-ckpt" for f in failures):
        print(f"  OK   partitioned + checkpointed: {len(deltalake_files)} files, "
              "partition columns + values + checkpoint metadata all decoded")


def case_checkpointed(failures: Failures, root: Path) -> None:
    print(f"\n== checkpointed table at {root}")

    # Twelve appends, then force a checkpoint so the
    # _delta_log/ ends up with `_last_checkpoint`,
    # NNNN.checkpoint.parquet, and a sibling NNNN.json. After
    # the checkpoint we also write *two more* commits — one
    # APPEND of an extra row and one OVERWRITE that removes
    # everything before it — so the Delta.IO short-circuit
    # actually has post-checkpoint commits to fold on top of
    # the Parquet-derived snapshot.
    for i in range(12):
        write_deltalake(str(root),
                        pa.table({"id": [i]}),
                        mode="append" if i > 0 else "error")
    dt = DeltaTable(str(root))
    dt.create_checkpoint()

    # Post-checkpoint commits: append + overwrite
    write_deltalake(str(root), pa.table({"id": [99]}), mode="append")
    write_deltalake(str(root), pa.table({"id": [101, 102]}), mode="overwrite")

    dt = DeltaTable(str(root))   # reload after additional commits
    summary = run_probe(root)

    expect_eq(failures, "checkpointed", "version",
              summary["version"], dt.version())
    expect_eq(failures, "checkpointed", "num_commits",
              summary["num_commits"], dt.version() + 1)

    # The checkpoint pointer + on-disk checkpoint Parquet are
    # at version 11 (where 'create_checkpoint()' was called).
    # The post-checkpoint commits are at version 12 (append)
    # and version 13 (overwrite), which the JSON-walk branch
    # of openDeltaTable must replay on top of the checkpoint
    # snapshot.
    lc = summary["last_checkpoint"]
    if lc is None:
        failures.add("checkpointed", "last_checkpoint pointer missing")
    else:
        expect_eq(failures, "checkpointed", "last_checkpoint.version",
                  lc["version"], 11)

    expect_eq(failures, "checkpointed", "checkpoint_parquet_version",
              summary["checkpoint_parquet_version"], 11)

    expect_eq(failures, "checkpointed", "post-checkpoint table version",
              dt.version(), 13)

    # The combined snapshot (checkpoint + later JSON commits)
    # must match deltalake's file_uris() — one row written by
    # the OVERWRITE that supersedes everything before.
    deltalake_files = sorted(deltalake_relative_files(dt, root))
    wireform_files  = sorted(f["path"] for f in summary["active_files"])
    expect_eq(failures, "checkpointed", "post-checkpoint active files",
              wireform_files, deltalake_files)

    # The standalone checkpoint Parquet decoder (without the
    # post-checkpoint JSON delta) must reproduce the v11 file
    # set as the JSON walk-only-up-to-v11 snapshot would.
    ckpt_files = summary["checkpoint_active_files"]
    if ckpt_files is None:
        failures.add("checkpointed", "checkpoint_active_files = null")
    else:
        # The pre-overwrite snapshot has 13 active files: the
        # 12 original adds + the post-checkpoint append.  Wait
        # — the checkpoint is at v11 (12 commits), so the
        # checkpoint Parquet only knows about the first 12
        # adds. We just check the count rather than the exact
        # file names since deltalake names files with random
        # uuids and we'd need to peek at the delta log to
        # reconstruct the v11 active set.
        expect_eq(failures, "checkpointed",
                  "checkpoint Parquet active file count",
                  len(ckpt_files), 12)

    ckpt_proto = summary["checkpoint_protocol"]
    if ckpt_proto is None:
        failures.add("checkpointed",
                     "checkpoint Parquet decoder lost the protocol row")
    else:
        expect_eq(failures, "checkpointed",
                  "checkpoint protocol min_reader",
                  ckpt_proto["min_reader_version"],
                  dt.protocol().min_reader_version)
        expect_eq(failures, "checkpointed",
                  "checkpoint protocol min_writer",
                  ckpt_proto["min_writer_version"],
                  dt.protocol().min_writer_version)

    ckpt_meta = summary["checkpoint_metadata"]
    if ckpt_meta is None:
        failures.add("checkpointed",
                     "checkpoint Parquet decoder lost the metaData row")
    else:
        expect_eq(failures, "checkpointed",
                  "checkpoint metadata.id",
                  ckpt_meta["id"], str(dt.metadata().id))

    if not any(f[0] == "checkpointed" for f in failures):
        print(f"  OK   checkpointed: version={dt.version()}, "
              f"checkpoint at {summary['checkpoint_parquet_version']}, "
              f"{len(deltalake_files)} active files (JSON + Parquet checkpoint agree)")


def case_time_travel_and_history(failures: Failures, root: Path) -> None:
    """Verify the new openDeltaTableAt + historyEntries surface
    matches deltalake. Builds a 5-version table with a mix of
    operations, then asks wireform for the snapshot at version
    2 and for the full history."""
    print(f"\n== time-travel + history at {root}")

    write_deltalake(str(root), pa.table({"id": [1]}))                 # v0
    write_deltalake(str(root), pa.table({"id": [2]}), mode="append")  # v1
    write_deltalake(str(root), pa.table({"id": [3]}), mode="append")  # v2
    write_deltalake(str(root), pa.table({"id": [99]}),
                    mode="overwrite")                                 # v3
    write_deltalake(str(root), pa.table({"id": [101]}),
                    mode="append")                                    # v4

    dt = DeltaTable(str(root))
    expect_eq(failures, "tt", "deltalake current version",
              dt.version(), 4)

    # ------ history at the latest version ------
    summary = run_probe(root)
    hist = summary.get("history") or []
    expect_eq(failures, "tt", "history entry count", len(hist), 5)
    # Newest-first
    versions = [e["version"] for e in hist]
    expect_eq(failures, "tt", "history versions order",
              versions, [4, 3, 2, 1, 0])
    # Operations should match what deltalake would surface.
    delta_ops = [h["operation"] for h in dt.history()]
    wf_ops    = [e["operation"] for e in hist]
    expect_eq(failures, "tt", "history operations", wf_ops, delta_ops)

    # ------ time travel: snapshot at version 2 ------
    v2 = run_probe(root, version=2)

    # deltalake's load_as_version reads the table at v2.
    dt_v2 = DeltaTable(str(root), version=2)
    expect_eq(failures, "tt", "v2 active file count",
              len(v2["active_files"]),
              len(dt_v2.file_uris()))
    expect_eq(failures, "tt", "v2 version", v2["version"], 2)

    # ------ active_relative_paths convenience ------
    if not summary["active_relative_paths"]:
        failures.add("tt", "active_relative_paths empty")
    elif sorted(summary["active_relative_paths"]) != sorted(
            f["path"] for f in summary["active_files"]):
        failures.add("tt", "active_relative_paths != active_files paths")

    if not any(f[0] == "tt" for f in failures):
        print(f"  OK   time-travel + history: {len(hist)} history rows, "
              f"v2 has {len(v2['active_files'])} active files")


def case_checkpoint_in_commit_timestamps(failures: Failures, root: Path) -> None:
    label = "checkpoint-in-commit-timestamps"
    write_deltalake(str(root), pa.table({"id": [1]}))
    log = root / "_delta_log"
    first = log / "00000000000000000000.json"
    second = log / "00000000000000000001.json"
    os.utime(first, (12, 12))
    initial = [json.loads(line) for line in first.read_text().splitlines()]
    metadata = next(action["metaData"] for action in initial if "metaData" in action)
    metadata["configuration"].update({
        "delta.enableInCommitTimestamps": "true",
        "delta.inCommitTimestampEnablementVersion": "1",
        "delta.inCommitTimestampEnablementTimestamp": "12001",
    })
    actions = [
        {"commitInfo": {"timestamp": 5, "inCommitTimestamp": 12001, "operation": "SET TBLPROPERTIES"}},
        {"protocol": {"minReaderVersion": 1, "minWriterVersion": 7, "writerFeatures": ["inCommitTimestamp"]}},
        {"metaData": metadata},
    ]
    second.write_text("".join(json.dumps(action) + "\n" for action in actions))
    os.utime(second, (99, 99))
    reference = DeltaTable(str(root))
    old = run_probe(root, version=0)
    expect_eq(failures, label, "history before enabling", old["history"][0]["timestamp"], 12000)
    replayed = run_probe(root)
    expect_eq(failures, label, "timestamp sources across enablement",
              [entry["timestamp"] for entry in replayed["history"]],
              [reference.history()[0]["inCommitTimestamp"], 12000])
    reference.create_checkpoint()
    first.unlink()
    recovered = run_probe(root, version=1)
    expect_eq(failures, label, "checkpoint active files", recovered["active_relative_paths"],
              sorted(deltalake_relative_files(reference, root)))
    expect_eq(failures, label, "history after removing pre-checkpoint commit",
              recovered["history"], replayed["history"][:1])
    expect_eq(failures, label, "exact-version provenance on checkpoint recovery",
              recovered["last_commit_operation"], replayed["last_commit_operation"])


def case_checkpoint_replay(failures: Failures, root: Path) -> None:
    log = root / "_delta_log"
    log.mkdir(parents=True)
    dv_type = pa.struct([
        ("storageType", pa.string()), ("pathOrInlineDv", pa.string()),
        ("offset", pa.int32()), ("sizeInBytes", pa.int32()),
        ("cardinality", pa.int64()),
    ])
    file_path = "p.parquet"
    old_dv = {"storageType": "p", "pathOrInlineDv": "file:///vectors",
              "offset": 1, "sizeInBytes": 32, "cardinality": 1}
    checkpoint = pa.table({
        "add": pa.array([
            {"path": file_path, "size": 1, "modificationTime": -1,
             "dataChange": True, "deletionVector": {**old_dv, "offset": 2}},
            None, None, None, None, None,
        ], type=pa.struct([
            ("path", pa.string()), ("size", pa.int64()),
            ("modificationTime", pa.int64()), ("dataChange", pa.bool_()),
            ("deletionVector", dv_type),
        ])),
        "remove": pa.array([
            None, {"path": file_path, "dataChange": True, "deletionVector": old_dv},
            None, None, None, None,
        ], type=pa.struct([
            ("path", pa.string()), ("dataChange", pa.bool_()),
            ("deletionVector", dv_type),
        ])),
        "txn": pa.array([
            None, None, {"appId": "kept", "version": -1},
            {"appId": "updated", "version": 7, "lastUpdated": 42}, None, None,
        ], type=pa.struct([
            ("appId", pa.string()), ("version", pa.int64()),
            ("lastUpdated", pa.int64()),
        ])),
        "protocol": pa.array([
            None, None, None, None, {"minReaderVersion": 3, "minWriterVersion": 7,
                                      "readerFeatures": ["deletionVectors"],
                                      "writerFeatures": ["deletionVectors"]}, None,
        ], type=pa.struct([
            ("minReaderVersion", pa.int32()), ("minWriterVersion", pa.int32()),
            ("readerFeatures", pa.list_(pa.field("element", pa.string(), False))),
            ("writerFeatures", pa.list_(pa.field("element", pa.string(), False))),
        ])),
        "metaData": pa.array([
            None, None, None, None, None,
            {"id": "checkpoint-replay", "schemaString": '{"type":"struct","fields":[]}'},
        ], type=pa.struct([("id", pa.string()), ("schemaString", pa.string())])),
    })
    pq.write_table(checkpoint, log / "00000000000000000000.checkpoint.parquet",
                   compression="NONE")
    (log / "00000000000000000001.json").write_text(
        json.dumps({"txn": {"appId": "updated", "version": 3}}) + "\n"
    )
    summary = run_probe(root)
    expect_eq(failures, "checkpoint-replay", "checkpoint active files",
              summary["checkpoint_active_files"], [file_path])
    expect_eq(failures, "checkpoint-replay", "active files after replay",
              summary["active_relative_paths"], [file_path])
    expect_eq(failures, "checkpoint-replay", "pre-epoch file timestamp",
              summary["active_files"][0]["modificationTime"], -1)
    expect_eq(failures, "checkpoint-replay", "signed transaction versions after replay",
              summary["txn_app_versions"], {"kept": -1, "updated": 3})


def case_checkpoint_recovery(failures: Failures, root: Path) -> None:
    label = "checkpoint-recovery"
    log = root / "_delta_log"
    write_deltalake(str(root), pa.table({"id": [1]}))
    DeltaTable(str(root)).create_checkpoint()
    commit_zero = log / "00000000000000000000.json"
    saved_commit = root / "saved-commit.json"
    commit_zero.rename(saved_commit)

    checkpoint_only = run_probe(root)
    expect_eq(failures, label, "checkpoint-only version", checkpoint_only["version"], 0)

    checkpoint_file = log / "00000000000000000000.checkpoint.parquet"
    saved_checkpoint = checkpoint_file.read_bytes()
    checkpoint = pq.read_table(checkpoint_file)
    pq.write_table(checkpoint.filter(pa.array([not row.is_valid for row in checkpoint["add"]])), checkpoint_file)
    out = root.parent / (root.name + ".incomplete-checkpoint.json")
    command = ["cabal", "run", "wireform-delta:wireform-delta-interop-probe", "--", str(root), str(out)]
    rejected = subprocess.run(command, cwd=ROOT.parent, capture_output=True, text=True)
    expect_eq(failures, label, "checkpoint with a missing add rejected",
              rejected.returncode != 0 and "_last_checkpoint numOfAddFiles mismatch" in rejected.stdout + rejected.stderr,
              True)
    saved_commit.rename(commit_zero)
    for version in (None, 0):
        recovered = run_probe(root, version=version)
        expect_eq(failures, label, f"files after checkpoint lost an add (version={version})",
                  recovered["active_relative_paths"], checkpoint_only["active_relative_paths"])
    checkpoint_file.write_bytes(saved_checkpoint)

    write_deltalake(str(root), pa.table({"id": [2]}), mode="append")
    dt = DeltaTable(str(root))
    expected = sorted(deltalake_relative_files(dt, root))
    initial = [json.loads(line) for line in commit_zero.read_text().splitlines()]
    commit_zero.unlink()
    commit_one = log / "00000000000000000001.json"
    actions = [json.loads(line) for line in commit_one.read_text().splitlines()]
    next(action["commitInfo"] for action in actions if "commitInfo" in action)["txnId"] = "checkpoint-recovery"
    commit_one.write_text("".join(json.dumps(action) + "\n" for action in actions))
    files = [row["add"] for row in initial + actions if "add" in row]
    protocol = next(row["protocol"] for row in initial if "protocol" in row)
    metadata = next(row["metaData"] for row in initial if "metaData" in row)
    (log / "00000000000000000001.crc").write_text(json.dumps({
        "txnId": "checkpoint-recovery", "numFiles": len(files), "tableSizeBytes": sum(file["size"] for file in files),
        "numProtocol": 1, "numMetadata": 1, "protocol": protocol, "metadata": metadata,
    }))
    newest_checkpoint = log / "00000000000000000001.checkpoint.parquet"
    newest_checkpoint.write_bytes((log / "00000000000000000000.checkpoint.parquet").read_bytes())
    verified = run_probe(root)
    expect_eq(failures, label, "stale checkpoint rejected by version checksum", verified["active_relative_paths"], expected)
    newest_checkpoint.write_bytes(b"truncated checkpoint")
    recovered = run_probe(root)
    expect_eq(failures, label, "recovered version", recovered["version"], 1)
    expect_eq(failures, label, "active files from older checkpoint and JSON tail",
              recovered["active_relative_paths"], expected)
    expect_eq(failures, label, "recovered protocol", recovered["protocol"], checkpoint_only["protocol"])
    time_travel = run_probe(root, version=0)
    expect_eq(failures, label, "as-of version", time_travel["version"], 0)
    expect_eq(failures, label, "as-of active files",
              time_travel["active_relative_paths"], checkpoint_only["active_relative_paths"])


def case_checkpoint_path_alias(failures: Failures, root: Path) -> None:
    label = "checkpoint-path-alias"
    write_deltalake(str(root), pa.table({"id": [1]}))
    log = root / "_delta_log"
    first = log / "00000000000000000000.json"
    rows = [json.loads(line) for line in first.read_text().splitlines()]
    addition = next(row["add"] for row in rows if "add" in row)
    old_path = "old name.parquet"
    (root / addition["path"]).rename(root / old_path)
    addition["path"] = old_path
    first.write_text("".join(json.dumps(row) + "\n" for row in rows))
    reference = DeltaTable(str(root))
    reference.create_checkpoint()
    checkpoint = pq.read_table(log / "00000000000000000000.checkpoint.parquet")
    expect_eq(failures, label, "legacy spelling in the Parquet checkpoint",
              [entry["path"] for entry in checkpoint["add"].to_pylist() if entry is not None], [old_path])
    first.unlink()
    expect_eq(failures, label, "checkpoint preserves the old path spelling",
              run_probe(root)["active_relative_paths"], [old_path])
    (log / "00000000000000000001.json").write_text(json.dumps({
        "remove": {"path": "old%20name.parquet", "dataChange": True},
    }) + "\n")
    expect_eq(failures, label, "encoded tombstone removes the legacy add",
              run_probe(root)["active_relative_paths"], [])


def case_nullable_add_tags(failures: Failures, root: Path) -> None:
    label = "nullable-add-tags"
    write_deltalake(str(root), pa.table({"id": [1]}))
    log = root / "_delta_log"
    commit = log / "00000000000000000000.json"
    actions = [json.loads(line) for line in commit.read_text().splitlines()]
    add = next(action["add"] for action in actions if "add" in action)
    add["tags"] = {"present": "value", "empty": "", "missing": None}
    commit.write_text("".join(json.dumps(action) + "\n" for action in actions))
    reference = DeltaTable(str(root))
    expected = sorted(deltalake_relative_files(reference, root))
    expect_eq(failures, label, "numbered commit with null tags",
              run_probe(root)["active_relative_paths"], expected)
    reference.create_checkpoint()
    commit.unlink()
    expect_eq(failures, label, "checkpoint with null tags",
              run_probe(root)["active_relative_paths"], expected)


def case_v2_checkpoint(failures: Failures, root: Path, encoding: str) -> None:
    label = f"v2-checkpoint-{encoding}"
    write_deltalake(str(root), pa.table({"id": [1, 2]}))
    write_deltalake(str(root), pa.table({"id": [3]}), mode="append")
    log = root / "_delta_log"
    commit = log / "00000000000000000000.json"
    tail = log / "00000000000000000001.json"
    rows = [json.loads(line) for line in commit.read_text().splitlines()]
    protocol = {"minReaderVersion": 3, "minWriterVersion": 7,
                "readerFeatures": ["v2Checkpoint"], "writerFeatures": ["v2Checkpoint"]}
    metadata = next(row["metaData"] for row in rows if "metaData" in row)
    metadata["configuration"]["delta.checkpointPolicy"] = "v2"
    tail_rows = [json.loads(line) for line in tail.read_text().splitlines()]
    additions = [row["add"] for row in rows + tail_rows if "add" in row]
    sidecar_dir = log / "_sidecars"
    sidecar_dir.mkdir()
    sidecar = sidecar_dir / "data files.parquet"
    second_sidecar = sidecar_dir / "append.parquet"
    stats_value = pa.struct([("id", pa.int64())])
    parsed = pa.struct([("numRecords", pa.int64()), ("minValues", stats_value),
                        ("maxValues", stats_value), ("nullCount", stats_value)])
    fields = pa.struct([
        ("path", pa.string()), ("size", pa.int64()), ("modificationTime", pa.int64()),
        ("dataChange", pa.bool_()), ("stats_parsed", parsed),
        ("partitionValues", pa.map_(pa.string(), pa.string())),
    ])
    for addition in additions:
        addition["stats_parsed"] = json.loads(addition.pop("stats"))
    first, second = additions
    pq.write_table(pa.table({"add": pa.array([first], type=fields)}), sidecar)
    pq.write_table(pa.table({"add": pa.array([second], type=fields)}), second_sidecar)
    references = (
        {"path": "data%20files.parquet", "sizeInBytes": sidecar.stat().st_size, "modificationTime": 1},
        {"path": second_sidecar.name, "sizeInBytes": second_sidecar.stat().st_size, "modificationTime": 1},
    )
    reference = references[0]
    if encoding != "json":
        checkpoint_schema = pa.schema([
            ("protocol", pa.struct([("minReaderVersion", pa.int32()),
                                    ("minWriterVersion", pa.int32()),
                                    ("readerFeatures", pa.list_(pa.field("element", pa.string(), False))),
                                    ("writerFeatures", pa.list_(pa.field("element", pa.string(), False)))])),
            ("metaData", pa.struct([
                ("id", pa.string()), ("schemaString", pa.string()),
                ("format", pa.struct([("provider", pa.string()),
                                      ("options", pa.map_(pa.string(), pa.string()))])),
                ("partitionColumns", pa.list_(pa.field("element", pa.string(), False))),
                ("configuration", pa.map_(pa.string(), pa.string())),
            ])),
            ("checkpointMetadata", pa.struct([("version", pa.int64()), ("tags", pa.map_(pa.string(), pa.string()))])),
            ("sidecar", pa.struct([("path", pa.string()), ("sizeInBytes", pa.int64()),
                                   ("modificationTime", pa.int64())])),
        ])

    def write_checkpoint(*entries, tags=None):
        checkpoint_metadata = {"version": 1}
        if tags is not None:
            checkpoint_metadata["tags"] = tags
        actions = [{"protocol": protocol}, {"metaData": metadata}, {"checkpointMetadata": checkpoint_metadata}]
        actions += [{"sidecar": entry} for entry in entries]
        if encoding == "json":
            name = "00000000000000000001.checkpoint.9c23f542-af8c-4373-bd4c-d9e85a218178.json"
            (log / name).write_text("".join(json.dumps(action) + "\n" for action in actions))
        else:
            pq.write_table(pa.Table.from_pylist(actions, schema=checkpoint_schema),
                           log / "00000000000000000001.checkpoint.parquet")

    write_checkpoint(*references)
    commit.unlink()
    tail.unlink()
    summary = run_probe(root)
    reference_files = sorted(deltalake_relative_files(DeltaTable(str(root)), root))
    expect_eq(failures, label, "active files", summary["active_relative_paths"],
              reference_files)
    expect_eq(failures, label, "checkpoint-only version", summary["version"], 1)
    expect_eq(failures, label, "reader features", summary["protocol"]["reader_features"],
              ["v2Checkpoint"])
    expect_eq(failures, label, "structured stats from both sidecars",
              sorted(file["stats"]["numRecords"] for file in summary["active_files"]), [1, 2])
    output = root.parent / (root.name + ".missing-sidecar.json")
    command = ["cabal", "run", "wireform-delta:wireform-delta-interop-probe", "--", str(root), str(output)]
    if encoding == "json":
        manifest = next(log.glob("*.checkpoint.*.json"))
        pointer = log / "_last_checkpoint"
        pointer.write_text(json.dumps({
            "version": 1, "size": 7, "numOfAddFiles": 2,
            "v2Checkpoint": {"path": manifest.name, "sizeInBytes": manifest.stat().st_size, "modificationTime": 1},
        }))
        expect_eq(failures, label, "V2 manifest matches the pointer add count",
                  run_probe(root)["active_relative_paths"], reference_files)
        write_checkpoint(reference)
        attempt = subprocess.run(command, cwd=ROOT.parent, capture_output=True, text=True)
        expect_eq(failures, label, "pointer catches a lost sidecar reference without manifest totals",
                  attempt.returncode != 0 and "_last_checkpoint numOfAddFiles mismatch" in attempt.stdout + attempt.stderr,
                  True)
        pointer.unlink()
    totals = {"sidecarSizeInBytes": str(sum(entry["sizeInBytes"] for entry in references)),
              "sidecarNumActions": "2", "numOfAddFiles": "2"}
    write_checkpoint(*references, tags=totals)
    expect_eq(failures, label, "checkpoint with valid totals", run_probe(root)["active_relative_paths"], reference_files)
    for key, value in totals.items():
        write_checkpoint(*references, tags={key: str(int(value) + 1)})
        attempt = subprocess.run(command, cwd=ROOT.parent, capture_output=True, text=True)
        expect_eq(failures, label, f"inconsistent checkpoint {key} rejected",
                  attempt.returncode != 0 and f"checkpointMetadata.tags.{key} mismatch" in attempt.stdout + attempt.stderr,
                  True)
    write_checkpoint(reference, tags=totals)
    attempt = subprocess.run(command, cwd=ROOT.parent, capture_output=True, text=True)
    expect_eq(failures, label, "manifest missing a sidecar reference rejected",
              attempt.returncode != 0 and "sidecarSizeInBytes mismatch" in attempt.stdout + attempt.stderr, True)
    commit.write_text("".join(json.dumps(row) + "\n" for row in rows))
    tail.write_text("".join(json.dumps(row) + "\n" for row in tail_rows))
    expect_eq(failures, label, "fallback to commits after a manifest loses a reference",
              run_probe(root)["active_relative_paths"], reference_files)
    commit.unlink()
    tail.unlink()
    absolute = sidecar.resolve().as_uri()
    write_checkpoint(dict(reference, path=absolute), references[1])
    expect_eq(failures, label, "absolute sidecar in this table",
              run_probe(root)["active_relative_paths"], reference_files)
    write_checkpoint(dict(reference, path="data%20files%2Eparquet"), references[1])
    expect_eq(failures, label, "encoded dot in sidecar filename",
              run_probe(root)["active_relative_paths"], reference_files)

    wrong_suffix = sidecar.with_suffix(".bin")
    wrong_suffix.write_bytes(sidecar.read_bytes())
    write_checkpoint(dict(reference, path="data%20files.bin"), references[1])
    attempt = subprocess.run(command, cwd=ROOT.parent, capture_output=True, text=True)
    expect_eq(failures, label, "sidecar filename without .parquet rejected",
              attempt.returncode != 0 and "expected a .parquet sidecar filename" in attempt.stdout + attempt.stderr,
              True)

    outside = root.parent / sidecar.name
    outside.write_bytes(sidecar.read_bytes())
    alias = sidecar_dir / "alias.parquet"
    alias.symlink_to(outside)
    for location in (outside.resolve().as_uri(), "subdir/" + reference["path"], alias.name):
        write_checkpoint(dict(reference, path=location))
        attempt = subprocess.run(command, cwd=ROOT.parent, capture_output=True, text=True)
        expect_eq(failures, label, f"invalid sidecar location {location} rejected",
                  attempt.returncode != 0 and "sidecar must reside" in attempt.stdout + attempt.stderr, True)
    write_checkpoint(reference, dict(reference, path=absolute))
    attempt = subprocess.run(command, cwd=ROOT.parent, capture_output=True, text=True)
    expect_eq(failures, label, "duplicate relative and absolute sidecar rejected",
              attempt.returncode != 0 and "duplicate sidecar path" in attempt.stdout + attempt.stderr, True)
    for size in (0, reference["sizeInBytes"] - 1, reference["sizeInBytes"] + 1):
        write_checkpoint(dict(reference, sizeInBytes=size))
        attempt = subprocess.run(command, cwd=ROOT.parent, capture_output=True, text=True)
        expect_eq(failures, label, f"wrong sidecar size {size} rejected",
                  attempt.returncode != 0 and "sidecar size mismatch" in attempt.stdout + attempt.stderr,
                  True)
    # Distinct sidecar files must not silently contribute the same live add.
    second_sidecar.write_bytes(sidecar.read_bytes())
    write_checkpoint(reference, dict(references[1], sizeInBytes=reference["sizeInBytes"]))
    attempt = subprocess.run(command, cwd=ROOT.parent, capture_output=True, text=True)
    expect_eq(failures, label, "duplicate adds across sidecars rejected",
              attempt.returncode != 0 and "duplicate add path" in attempt.stdout + attempt.stderr, True)
    # Replay must discard this add if the later sidecar is invalid.
    pq.write_table(pa.table({"add": pa.array([dict(first, path="ghost.parquet")], type=fields)}), sidecar)
    checkpoint_metadata = pa.struct([("version", pa.int64())])
    pq.write_table(pa.table({"checkpointMetadata": pa.array([{"version": 1}], type=checkpoint_metadata)}),
                   second_sidecar)
    write_checkpoint(dict(reference, sizeInBytes=sidecar.stat().st_size),
                     dict(references[1], sizeInBytes=second_sidecar.stat().st_size))
    attempt = subprocess.run(command, cwd=ROOT.parent, capture_output=True, text=True)
    expect_eq(failures, label, "metadata action in sidecar rejected",
              attempt.returncode != 0 and "sidecar contains non-file actions" in attempt.stdout + attempt.stderr, True)
    commit.write_text("".join(json.dumps(row) + "\n" for row in rows))
    tail.write_text("".join(json.dumps(row) + "\n" for row in tail_rows))
    expect_eq(failures, label, "fallback to commits after later sidecar fails",
              run_probe(root)["active_relative_paths"], reference_files)
    commit.unlink()
    tail.unlink()
    write_checkpoint(reference)
    sidecar.unlink()
    attempt = subprocess.run(command, cwd=ROOT.parent, capture_output=True, text=True)
    expect_eq(failures, label, "missing sidecar rejected", attempt.returncode != 0, True)
    expect_eq(failures, label, "missing sidecar reported",
              "data files.parquet" in attempt.stdout + attempt.stderr, True)


def case_multipart_checkpoint(failures: Failures, root: Path) -> None:
    label = "multipart-checkpoint"
    write_deltalake(str(root), pa.table({"id": [1]}))
    table = DeltaTable(str(root))
    table.create_checkpoint()
    log = root / "_delta_log"
    classic = log / "00000000000000000000.checkpoint.parquet"
    actions = pq.read_table(classic)
    classic.unlink()
    first = log / "00000000000000000000.checkpoint.0000000001.0000000002.parquet"
    second = log / "00000000000000000000.checkpoint.0000000002.0000000002.parquet"
    pq.write_table(actions.slice(0, 1), first)
    expected = sorted(deltalake_relative_files(table, root))
    incomplete = run_probe(root)
    expect_eq(failures, label, "incomplete checkpoint is ignored",
              incomplete["checkpoint_parquet_version"], None)
    pq.write_table(actions.slice(1), second)
    commit = log / "00000000000000000000.json"
    commit_bytes = commit.read_bytes()
    commit.unlink()
    summary = run_probe(root)
    reference_files = sorted(deltalake_relative_files(DeltaTable(str(root)), root))
    expect_eq(failures, label, "reference active files", reference_files, expected)
    expect_eq(failures, label, "active files", summary["active_relative_paths"], reference_files)
    expect_eq(failures, label, "checkpoint-only version", summary["version"], 0)

    # A multipart checkpoint may predate the V2 feature. It cannot represent
    # a snapshot whose own protocol enables that feature.
    fields = pa.struct([("minReaderVersion", pa.int32()), ("minWriterVersion", pa.int32()),
                        ("readerFeatures", pa.list_(pa.string())), ("writerFeatures", pa.list_(pa.string()))])
    protocol = {"minReaderVersion": 3, "minWriterVersion": 7,
                "readerFeatures": ["v2Checkpoint"], "writerFeatures": ["v2Checkpoint"]}
    pq.write_table(pa.table({"protocol": pa.array([protocol], type=fields)}), first)
    actions_without_protocol = actions.filter(actions["protocol"].is_null())
    pq.write_table(actions_without_protocol, second)
    output = root.parent / (root.name + ".bad-multipart.json")
    command = ["cabal", "run", "wireform-delta:wireform-delta-interop-probe", "--",
               str(root), str(output)]
    attempt = subprocess.run(command, cwd=ROOT.parent, capture_output=True, text=True)
    expect_eq(failures, label, "invalid multipart protocol rejected",
              attempt.returncode != 0 and "multipart checkpoint cannot enable v2Checkpoint"
              in attempt.stdout + attempt.stderr, True)
    commit.write_bytes(commit_bytes)
    recovered = run_probe(root)
    expect_eq(failures, label, "invalid multipart protocol falls back to JSON",
              recovered["active_relative_paths"], expected)


def case_local_scan_paths(failures: Failures, root: Path) -> None:
    label = "local-scan-paths"
    write_deltalake(str(root), pa.table({"id": [1, 2]}))
    commit = root / "_delta_log" / "00000000000000000000.json"
    actions = [json.loads(line) for line in commit.read_text().splitlines()]
    addition = next(action["add"] for action in actions if "add" in action)
    data = root / "a +%#é.parquet"
    (root / addition["path"]).rename(data)
    absolute = data.resolve().as_uri()
    relative = quote(data.name, safe="+")
    output = root.parent / (root.name + ".table-scan.json")
    command = ["cabal", "run", "wireform-delta:wireform-delta-interop-probe", "--",
               "--scan-table", str(root), str(output)]
    for path in (relative, "./" + relative, absolute, absolute.replace("file://", "file:", 1),
                 absolute.replace("file://", "FILE://LOCALHOST", 1)):
        addition["path"] = path
        commit.write_text("".join(json.dumps(action) + "\n" for action in actions))
        subprocess.run(command, cwd=ROOT.parent, check=True)
        expect_eq(failures, label, f"scan {path}", json.loads(output.read_text())["rows"], [{"id": 1}, {"id": 2}])
    for path in (absolute.replace("file://", "file://remote", 1), absolute + "?query",
                 relative + "?query", relative + "#fragment",
                 absolute.removeprefix("file://") + "#fragment",
                 "%2e%2e/" + relative, "%2f" + relative, relative + "%00", relative + "%zz"):
        addition["path"] = path
        commit.write_text("".join(json.dumps(action) + "\n" for action in actions))
        attempt = subprocess.run(command, cwd=ROOT.parent, capture_output=True, text=True)
        expect_eq(failures, label, f"invalid path {path} rejected",
                  attempt.returncode != 0 and "Delta.Path:" in attempt.stderr, True)


def case_invalid_checkpoint_text(failures: Failures, root: Path) -> None:
    label = "invalid-checkpoint-text"
    write_deltalake(str(root), pa.table({"id": [1]}))
    expected = sorted(deltalake_relative_files(DeltaTable(str(root)), root))
    log = root / "_delta_log"
    fields = pa.struct([("path", pa.binary())])
    bad = pa.table({"add": pa.array([{"path": b"invalid\xff"}], type=fields)})
    checkpoint = log / "00000000000000000000.checkpoint.parquet"
    pq.write_table(bad, checkpoint)
    summary = run_probe(root)
    expect_eq(failures, label, "fallback active files", summary["active_relative_paths"], expected)


def case_deletion_vector_scan(failures: Failures, root: Path) -> None:
    label = "deletion-vector-scan"
    write_deltalake(str(root), pa.table({"id": list(range(35))}),
                    configuration={"delta.enableDeletionVectors": "true"})
    log = root / "_delta_log"
    commit = log / "00000000000000000000.json"
    actions = [json.loads(line) for line in commit.read_text().splitlines()]
    add = next(action["add"] for action in actions if "add" in action)
    data = root / "id file.parquet"
    (root / add["path"]).unlink()
    rows = pa.table({"id": list(range(35))})
    with pq.ParquetWriter(data, rows.schema, compression="NONE") as writer:
        for start, count in ((0, 3), (3, 2), (5, 8), (13, 22)):
            writer.write_table(rows.slice(start, count))
    add.update(path="id%20file.parquet", size=data.stat().st_size)
    stats = json.loads(add["stats"])
    add["stats"] = json.dumps({**stats, "tightBounds": False})
    # Portable encoding of the rows from the protocol's legacy inline example.
    positions = [3, 4, 7, 11, 18, 29]
    bitmap = struct.pack("<IQIIIHHI6H", 1681511377, 1, 0, 12346, 1,
                         0, len(positions) - 1, 16, *positions)
    stored = struct.pack(">I", len(bitmap)) + bitmap + struct.pack(">I", zlib.crc32(bitmap))
    dv_file = root / "vectors" / "deletion_vector_d2c639aa-8816-431a-aaf6-d3fe2512ff61.bin"
    dv_file.parent.mkdir()
    dv_file.write_bytes(b"\x01" + stored + stored)
    descriptors = [
        {"storageType": "i", "pathOrInlineDv": "^Bg9^0rr910000000000iXQKl0rr91000f55c8Xg0@@D72lkbi5=-{L"},
        {"storageType": "u", "pathOrInlineDv": "vectors^-aqEH.-t@S}K{vb[*k^", "offset": 1 + len(stored)},
        {"storageType": "p", "pathOrInlineDv": dv_file.as_uri(), "offset": 1},
    ]
    expected = [i for i in range(35) if i not in positions]
    for descriptor in descriptors:
        storage = descriptor["storageType"]
        add["deletionVector"] = {**descriptor, "sizeInBytes": len(bitmap),
                                 "cardinality": len(positions)}
        commit.write_text("".join(json.dumps(action) + "\n" for action in actions))
        dt = DeltaTable(str(root))
        vectors = pa.RecordBatchReader.from_stream(dt.deletion_vectors()).read_all()
        mask = vectors.column("selection_vector")[0].as_py()
        actual = scan_ids(root)
        expect_eq(failures, label, f"{storage} wireform live rows", actual, expected)
        expect_eq(failures, label, f"{storage} delta-rs live rows",
                  [i for i in range(35) if i >= len(mask) or mask[i]], expected)
    # A missing offset selects the first record in delta-rs. An explicit
    # offset is a byte position and 0 points at the file's version header.
    first_record = add["deletionVector"].copy()
    del add["deletionVector"]["offset"]
    commit.write_text("".join(json.dumps(action) + "\n" for action in actions))
    expect_eq(failures, label, "absent offset selects first record", scan_ids(root), expected)
    vectors = pa.RecordBatchReader.from_stream(DeltaTable(str(root)).deletion_vectors()).read_all()
    mask = vectors.column("selection_vector")[0].as_py()
    expect_eq(failures, label, "delta-rs absent offset selects first record",
              [i for i in range(35) if i >= len(mask) or mask[i]], expected)
    add["deletionVector"]["offset"] = 0
    commit.write_text("".join(json.dumps(action) + "\n" for action in actions))
    expect_eq(failures, label, "explicit zero cannot address a record",
              "offset points at the file version" in scan_ids(root, expect_failure=True), True)
    add["deletionVector"] = first_record
    properties = next(action["metaData"]["configuration"] for action in actions if "metaData" in action)
    properties.pop("delta.enableDeletionVectors", None)
    commit.write_text("".join(json.dumps(action) + "\n" for action in actions))
    command = ["cabal", "run", "wireform-delta:wireform-delta-interop-probe", "--",
               "--scan-table", str(root), str(root.parent / (root.name + ".live.json"))]
    subprocess.run(command, cwd=ROOT.parent, check=True)
    output = Path(command[-1])
    expect_eq(failures, label, "disabled writer property still filters the vector",
              [row["id"] for row in json.loads(output.read_text())["rows"]], expected)
    protocol = next(action for action in actions if "protocol" in action)
    unsupported = [{"protocol": {"minReaderVersion": 1, "minWriterVersion": 2}} if action is protocol
                   else action for action in actions]
    commit.write_text("".join(json.dumps(action) + "\n" for action in unsupported))
    rejected = subprocess.run(command, cwd=ROOT.parent, capture_output=True, text=True)
    expect_eq(failures, label, "full table scan rejects vector without required protocol",
              rejected.returncode != 0 and "deletionVectors reader and writer feature" in rejected.stderr, True)
    commit.write_text("".join(json.dumps(action) + "\n" for action in actions))
    valid_stats = add["stats"]
    previous_vector = add["deletionVector"]
    # Removing a prior vector and adding Spark's empty inline descriptor
    # at the same path restores all physical rows. Try both source orders.
    replace = log / "00000000000000000001.json"
    replacement = {**add, "deletionVector": {
        "storageType": "i", "pathOrInlineDv": "", "sizeInBytes": 0, "cardinality": 0,
    }}
    remove = {"remove": {"path": add["path"], "dataChange": True,
                         "deletionVector": previous_vector}}
    for ordered in ([{"add": replacement}, remove], [remove, {"add": replacement}]):
        replace.write_text("".join(json.dumps(action) + "\n" for action in ordered))
        expect_eq(failures, label, "empty replacement restores all rows",
                  scan_ids(root), list(range(35)))
    DeltaTable(str(root)).create_checkpoint()
    replace.unlink()
    commit.unlink()
    expect_eq(failures, label, "empty vector survives checkpoint-only recovery",
              scan_ids(root), list(range(35)))
    (log / "00000000000000000001.checkpoint.parquet").unlink()
    for description, invalid_stats in (("missing", None), ("mismatched", '{"numRecords":34}')):
        if invalid_stats is None:
            del add["stats"]
        else:
            add["stats"] = invalid_stats
        commit.write_text("".join(json.dumps(action) + "\n" for action in actions))
        expect_eq(failures, label, f"{description} physical row count rejected",
                  "numRecords" in scan_ids(root, expect_failure=True), True)
    add["stats"] = valid_stats
    commit.write_text("".join(json.dumps(action) + "\n" for action in actions))
    dt.create_checkpoint()
    commit.unlink()
    expect_eq(failures, label, "scan after checkpoint-only recovery", scan_ids(root), expected)
    checkpoint = log / "00000000000000000000.checkpoint.parquet"
    original_checkpoint = checkpoint.read_bytes()
    actions = pq.read_table(checkpoint)
    add_type = actions.schema.field("add").type
    partial_vector = pa.struct([pa.field("cardinality", pa.int64())])
    partial_add = pa.struct([
        pa.field("deletionVector", partial_vector) if field.name == "deletionVector" else field
        for field in add_type
    ])
    adds = actions.column("add").to_pylist()
    for entry in adds:
        if entry is not None and entry["deletionVector"] is not None:
            entry["deletionVector"] = {"cardinality": None}
    column_index = actions.schema.get_field_index("add")
    malformed = actions.set_column(column_index, "add", pa.array(adds, type=partial_add))
    pq.write_table(malformed, checkpoint)
    expect_eq(failures, label, "present checkpoint vector without storageType rejected",
              "deletion vector missing storageType" in scan_ids(root, expect_failure=True), True)
    checkpoint.write_bytes(original_checkpoint)
    dv_file.write_bytes(b"\x01" + stored[:-1] + bytes([stored[-1] ^ 1]) + stored)
    expect_eq(failures, label, "corrupt stored vector rejected",
              "CRC-32 checksum mismatch" in scan_ids(root, expect_failure=True), True)


def case_structured_checkpoint_stats(failures: Failures, root: Path) -> None:
    label = "structured-checkpoint-stats"
    table = pa.table({
        "id": [1, 2, 3],
        "s": pa.array([{"x": "alpha"}, {"x": None}, None]),
        "amount": pa.array([Decimal("-12.30"), None, Decimal("5.13")], pa.decimal128(15, 2)),
        "day": pa.array([date(1969, 12, 31), date(2020, 1, 1), None]),
        "utc": pa.array([-1, 1001, None], pa.timestamp("us", "UTC")),
        "ntz": pa.array([-1, 1001, None], pa.timestamp("us")),
        "empty": pa.array([None, None, None], pa.int64()),
    })
    write_deltalake(str(root), table, configuration={
        "delta.checkpoint.writeStatsAsStruct": "true",
        "delta.checkpoint.writeStatsAsJson": "false",
    })
    log = root / "_delta_log"
    commit = log / "00000000000000000000.json"
    actions = [json.loads(line) for line in commit.read_text().splitlines()]
    add = next(action["add"] for action in actions if "add" in action)
    expected = json.loads(add["stats"])
    expected["tightBounds"] = False
    add["stats"] = json.dumps(expected)
    changed = {**expected, "numRecords": 4, "tightBounds": True,
               "minValues": {**expected["minValues"], "id": -10},
               "nullCount": {**expected["nullCount"], "id": 1}}
    # Exercise reuse of identical adjacent statistics and ensure a
    # different count, bound or tightness flag starts a new record.
    actions.remove(next(action for action in actions if "add" in action))
    expected_by_path = {}
    for i, stats in enumerate((expected, expected, changed, changed, expected)):
        path = f"file-{i:02d}.parquet"
        actions.append({"add": {**add, "path": path, "stats": json.dumps(stats)}})
        expected_by_path[path] = stats
    commit.write_text("".join(json.dumps(action) + "\n" for action in actions))
    dt = DeltaTable(str(root))
    dt.create_checkpoint()
    checkpoint = next(log.glob("*.checkpoint.parquet"))
    add_type = pq.read_schema(checkpoint).field("add").type
    expect_eq(failures, label, "no JSON stats column", add_type.get_field_index("stats"), -1)
    checkpoint_rows = pq.read_table(checkpoint)
    pq.write_table(checkpoint_rows, checkpoint, row_group_size=5)
    commit.unlink()
    stats = {f["path"]: f["stats"] for f in run_probe(root)["active_files"]}
    def without_nulls(value):
        if isinstance(value, dict):
            return {k: without_nulls(v) for k, v in value.items() if v is not None}
        return value
    expect_eq(failures, label, "bounds and counts after log cleanup",
              without_nulls(stats), without_nulls(expected_by_path))

    # A duplicate that spans row groups must fail when the batches are
    # reconciled, even though neither row group has a duplicate on its own.
    file_rows = checkpoint_rows.filter(checkpoint_rows["add"].is_valid())
    pq.write_table(pa.concat_tables([checkpoint_rows, file_rows.slice(0, 1)]),
                   checkpoint, row_group_size=checkpoint_rows.num_rows)
    output = root.parent / (root.name + ".duplicate-checkpoint-add.json")
    command = ["cabal", "run", "wireform-delta:wireform-delta-interop-probe", "--",
               str(root), str(output)]
    attempt = subprocess.run(command, cwd=ROOT.parent, capture_output=True, text=True)
    expect_eq(failures, label, "duplicate add across row groups rejected",
              attempt.returncode != 0 and "duplicate add path" in attempt.stdout + attempt.stderr,
              True)


def case_mapped_scan(failures: Failures, root: Path, mode: str, legacy_protocol: bool = False) -> None:
    label = f"mapped-scan-{root.name}"
    log = root / "_delta_log"
    log.mkdir(parents=True)

    def delta_field(name, storage, ident, dtype):
        return {"name": name, "type": dtype, "nullable": True, "metadata": {
            "delta.columnMapping.physicalName": storage, "delta.columnMapping.id": ident,
        }}

    def parquet_field(storage, ident, dtype):
        name = f"parquet_{ident}" if mode == "id" else storage
        # Name mapping ignores file IDs. Deliberately make them disagree.
        return pa.field(name, dtype, metadata={"PARQUET:field_id": str(ident if mode == "id" else ident + 100)})

    nested = {"type": "struct", "fields": [
        delta_field("renamed_child", "physical_child", 3, "long"),
        delta_field("new_child", "new_child_storage", 4, "long"),
    ]}
    list_type = {"type": "array", "elementType": nested, "containsNull": True}
    map_type = {"type": "map", "keyType": "string", "valueType": nested, "valueContainsNull": True}

    def item_type(child_id, new_id):
        return {"type": "struct", "fields": [
            delta_field("renamed_child", "physical_child", child_id, "long"),
            delta_field("new_child", "new_child_storage", new_id, "long"),
        ]}

    list_type["elementType"] = item_type(6, 7)
    map_type["valueType"] = item_type(9, 10)
    logical_fields = [
        delta_field("renamed_id", "physical_id", 1, "long"),
        delta_field("record", "physical_record", 2, nested),
        delta_field("items", "physical_items", 5, list_type),
        delta_field("lookup", "physical_lookup", 8, map_type),
        delta_field("region", "physical_region", 11, "string"),
        delta_field("new_field", "new_field_storage", 12, "long"),
        delta_field("record_with_only_new_children", "physical_old_record", 13,
                    {"type": "struct", "fields": [delta_field("new_child", "new_storage", 15, "long")]}),
    ]
    expected = []
    adds = []
    for version in (0, 1):
        scalar = parquet_field("physical_id", 1, pa.int64())
        struct_child = parquet_field("physical_child", 3, pa.int64())
        list_child = parquet_field("physical_child", 6, pa.int64())
        map_child = parquet_field("physical_child", 9, pa.int64())
        old_child = parquet_field("old_storage", 14, pa.int64())
        record = parquet_field("physical_record", 2, pa.struct([struct_child]))
        items = parquet_field("physical_items", 5, pa.list_(pa.struct([list_child])))
        lookup = parquet_field("physical_lookup", 8, pa.map_(pa.string(), pa.struct([map_child])))
        old_record = parquet_field("physical_old_record", 13, pa.struct([old_child]))
        schema = pa.schema([scalar, record, items, lookup, old_record])
        value = 11 + version
        payload = pa.Table.from_pylist([
            {scalar.name: value, record.name: {struct_child.name: 20},
             items.name: [{list_child.name: 30}, None],
             lookup.name: {"key": {map_child.name: 40}}, old_record.name: {old_child.name: 50}},
            {scalar.name: value + 100, record.name: None, items.name: [], lookup.name: {}, old_record.name: None},
        ], schema=schema)
        path = root / f"file-{version}.parquet"
        pq.write_table(payload, path, row_group_size=1)
        region = "eu" if version == 0 else None
        stats = {
            "numRecords": 2,
            "minValues": {"physical_id": value, "physical_record": {"physical_child": 20}},
            "nullCount": {"physical_id": 0, "physical_record": {"physical_child": 1},
                          "physical_old_record": {"old_storage": 1}},
        }
        additions = {"path": path.name, "size": path.stat().st_size, "modificationTime": 0,
                     "dataChange": True, "partitionValues": {"physical_region": region},
                     "stats": json.dumps(stats)}
        adds.append({"add": additions})
        expected.extend([
            {"renamed_id": value, "record": {"renamed_child": 20, "new_child": None},
             "items": [{"renamed_child": 30, "new_child": None}, None],
             "lookup": {"key": {"renamed_child": 40, "new_child": None}},
             "region": region, "new_field": None,
             "record_with_only_new_children": {"new_child": None}},
            {"renamed_id": value + 100, "record": None, "items": [], "lookup": {},
             "region": region, "new_field": None, "record_with_only_new_children": None},
        ])
    if mode == "id":
        # Delete physical row 1 from the newer file. This also checks that
        # partition and missing-field columns use the post-deletion length.
        bitmap = struct.pack("<IQIIIHHIH", 1681511377, 1, 0, 12346, 1, 0, 0, 16, 1)
        dv_file = root / "mapped-delete.bin"
        dv_file.write_bytes(b"\x01" + struct.pack(">I", len(bitmap))
                            + bitmap + struct.pack(">I", zlib.crc32(bitmap)))
        adds[1]["add"]["deletionVector"] = {
            "storageType": "p", "pathOrInlineDv": dv_file.as_uri(),
            "offset": 1, "sizeInBytes": len(bitmap), "cardinality": 1,
        }
        stats = json.loads(adds[1]["add"]["stats"])
        stats["nullCount"]["physical_region"] = 2
        adds[1]["add"]["stats"] = json.dumps({**stats, "tightBounds": False})
        expected.pop()
    metadata = {"id": "mapping-test", "format": {"provider": "parquet", "options": {}},
                "schemaString": json.dumps({"type": "struct", "fields": logical_fields}),
                "partitionColumns": ["region"], "configuration": {"delta.columnMapping.mode": mode}}
    features = ["columnMapping", "deletionVectors"] if mode == "id" else ["columnMapping"]
    protocol = {"minReaderVersion": 3, "minWriterVersion": 7,
                "readerFeatures": [] if legacy_protocol else features, "writerFeatures": features}
    original_fields = [{**logical_fields[0], "name": "original_id"}, *logical_fields[1:]]
    original = {**metadata, "schemaString": json.dumps({"type": "struct", "fields": original_fields})}
    for version, actions in enumerate((
        [{"protocol": protocol}, {"metaData": original}, adds[0]],
        [{"metaData": metadata}, adds[1]],
    )):
        (log / f"{version:020d}.json").write_text("".join(json.dumps(action) + "\n" for action in actions))
    output = root.parent / (root.name + ".table-scan.json")
    command = ["cabal", "run", "wireform-delta:wireform-delta-interop-probe", "--",
               "--scan-table", str(root), str(output)]
    subprocess.run(command, cwd=ROOT.parent, check=True)
    result = json.loads(output.read_text())
    expect_eq(failures, label, "logical rows", result["rows"], expected)
    expect_eq(failures, label, "logical stats", result["stats"][0]["minValues"],
              {"renamed_id": 11, "record": {"renamed_child": 20}})
    expect_eq(failures, label, "tight null counts keep intermediate values",
              result["stats"][0]["nullCount"]["record"], {"renamed_child": 1})
    if mode == "id":
        expect_eq(failures, label, "wide null counts omit partitions and intermediate nested values",
                  result["stats"][1]["nullCount"], {"renamed_id": 0})
        names = ["new_field", "region", "renamed_id"]
        command_projection = [*command[:-3], "--scan-projected", str(root), str(output), *names]
        subprocess.run(command_projection, cwd=ROOT.parent, check=True)
        projected = json.loads(output.read_text())
        expect_eq(failures, label, "projected column order", projected["columns"], names)
        expect_eq(failures, label, "projected rows with mapping and a deletion vector", projected["rows"],
                  [{name: row[name] for name in names} for row in expected])
        # After a metadata-only drop, the remaining columns all come from
        # the log or are missing in both files. Preserve live row counts.
        remaining = logical_fields[4:6]
        narrow = {**metadata, "schemaString": json.dumps({"type": "struct", "fields": remaining})}
        (log / "00000000000000000002.json").write_text(json.dumps({"metaData": narrow}) + "\n")
        subprocess.run(command, cwd=ROOT.parent, check=True)
        expect_eq(failures, label, "scan with no projected Parquet columns",
                  json.loads(output.read_text())["rows"],
                  [{"region": row["region"], "new_field": None} for row in expected])
        pq.write_table(pa.table({"physical_id": [99]}), root / "file-0.parquet")
        failure = subprocess.run(
            command, cwd=ROOT.parent, capture_output=True, text=True,
        )
        expect_eq(failures, label, "ID-less file rejected",
                  failure.returncode != 0 and "no field IDs" in failure.stderr, True)


def case_scan_widening(failures: Failures, root: Path) -> None:
    label = "scan-type-widening"
    log = root / "_delta_log"
    log.mkdir(parents=True)
    data = root / "old.parquet"
    pq.write_table(pa.table({
        "id": pa.array([-2, 3], pa.int16()),
        "score": pa.array([-0.1, 0.1], pa.float32()),
        "day": pa.array([date(1969, 12, 31), None]),
        "event_time": pa.array([-501, -501], pa.timestamp("us", tz="UTC")),
        "samples": pa.array([[1, 2], [3]], pa.list_(pa.int8())),
        "measurements": pa.array([[{"old": Decimal("-1.25")}], [{"new": Decimal("3.50")}]],
                                 pa.list_(pa.map_(pa.string(), pa.decimal128(6, 2)))),
        "same_scale": pa.array([Decimal("-1234567890123456789.01"), None], pa.decimal128(21, 2)),
        "higher_scale": pa.array([Decimal("184467440737095516.16"), Decimal("-184467440737095516.17")],
                                 pa.decimal128(20, 2)),
        "wide_multiplier": pa.array([Decimal("-9999999999999.9"), Decimal("0.1")], pa.decimal128(14, 1)),
        "integer_to_decimal": pa.array([-9223372036854775808, 9223372036854775807], pa.int64()),
    }), data)
    protocol = {"minReaderVersion": 3, "minWriterVersion": 7,
                "readerFeatures": ["typeWidening", "timestampNtz"],
                "writerFeatures": ["typeWidening", "timestampNtz"]}

    def field(name, new, old, path=None, to=None):
        change = {"fromType": old, "toType": to or new}
        if path:
            change["fieldPath"] = path
        return {"name": name, "type": new, "nullable": True,
                "metadata": {"delta.typeChanges": [change]}}

    fields = [
        field("id", "long", "short"),
        field("score", "double", "float"),
        field("day", "timestamp_ntz", "date"),
        field("samples", {"type": "array", "elementType": "short", "containsNull": True},
              "byte", "element", "short"),
        field("measurements", {"type": "array", "containsNull": True, "elementType": {
            "type": "map", "keyType": "string", "valueType": "decimal(10,4)", "valueContainsNull": True,
        }}, "decimal(6,2)", "element.value", "decimal(10,4)"),
        field("same_scale", "decimal(38,2)", "decimal(21,2)"),
        field("higher_scale", "decimal(38,20)", "decimal(20,2)"),
        field("wide_multiplier", "decimal(38,25)", "decimal(14,1)"),
        field("integer_to_decimal", "decimal(38,18)", "long"),
        {"name": "event_time", "type": "timestamp", "nullable": True, "metadata": {}},
    ]
    fields[0]["metadata"]["delta.typeChanges"] = [
        {"fromType": "short", "toType": "integer"},
        {"fromType": "integer", "toType": "long"},
    ]
    metadata = {"id": label, "partitionColumns": [], "configuration": {},
                "schemaString": json.dumps({"type": "struct", "fields": fields})}
    truncated = "1969-12-31T23:59:59.999Z"
    original_stats = {"numRecords": 2, "minValues": {"day": "1969-12-31", "event_time": truncated, "score": -0.1},
                      "maxValues": {"day": "1969-12-31T00:00:00.000000", "event_time": truncated, "score": 0.1}}
    addition = {"add": {"path": data.name, "size": data.stat().st_size,
                        "modificationTime": 0, "dataChange": True, "partitionValues": {},
                        "stats": json.dumps(original_stats)}}
    commit = log / "00000000000000000000.json"
    actions = [{"protocol": protocol}, {"metaData": metadata}, addition]
    commit.write_text("".join(json.dumps(action) + "\n" for action in actions))
    output = root.parent / (root.name + ".table-scan.json")
    command = ["cabal", "run", "wireform-delta:wireform-delta-interop-probe", "--",
               "--scan-table", str(root), str(output)]
    subprocess.run(command, cwd=ROOT.parent, check=True)
    score = pa.array([-0.1, 0.1], pa.float32()).to_pylist()
    expected_stats = {**original_stats,
                      "minValues": {"day": "1969-12-31 00:00:00", "event_time": truncated},
                      "maxValues": {"day": "1969-12-31 00:00:00.001", "event_time": "1970-01-01T00:00:00Z"},
                      "nullCount": {}, "tightBounds": True}
    decoded = json.loads(output.read_text())
    actual_stats = decoded["stats"][0]
    expect_eq(failures, label, "float bounds enclose widened data",
              actual_stats["minValues"]["score"] <= score[0] and actual_stats["maxValues"]["score"] >= score[1],
              True)
    for name in ("minValues", "maxValues"):
        del actual_stats[name]["score"]
    expect_eq(failures, label, "conservative logical date and timestamp bounds", actual_stats, expected_stats)
    expect_eq(failures, label, "raw log bounds retain their original encoding",
              run_probe(root)["active_files"][0]["stats"],
              {**original_stats, "nullCount": {}, "tightBounds": True})
    expect_eq(failures, label, "widened logical rows", json.loads(output.read_text())["rows"], [
        {"id": -2, "score": score[0], "day": -86400000000, "samples": [1, 2],
         "measurements": [{"old": -12500}],
         "same_scale": -123456789012345678901, "higher_scale": (2 ** 64) * (10 ** 18),
         "wide_multiplier": -(10 ** 14 - 1) * (10 ** 24),
         "integer_to_decimal": -(2 ** 63) * (10 ** 18), "event_time": -501},
        {"id": 3, "score": score[1], "day": None, "samples": [3],
         "measurements": [{"new": 35000}],
         "same_scale": None, "higher_scale": -(2 ** 64 + 1) * (10 ** 18),
         "wide_multiplier": 10 ** 24,
         "integer_to_decimal": (2 ** 63 - 1) * (10 ** 18), "event_time": -501},
    ])
    fields[0]["metadata"]["delta.typeChanges"][0]["fromType"] = "string"
    metadata["schemaString"] = json.dumps({"type": "struct", "fields": fields})
    commit.write_text("".join(json.dumps(action) + "\n" for action in actions))
    failure = subprocess.run(command, cwd=ROOT.parent, capture_output=True, text=True)
    expect_eq(failures, label, "unsupported historical type change rejected",
              failure.returncode != 0 and "unsupported type change" in failure.stderr, True)
    fields[0]["metadata"]["delta.typeChanges"][0]["fromType"] = "short"
    changes = fields[4]["metadata"]["delta.typeChanges"]
    valid_change = changes[0]
    for incorrect, problem in (
        ({**valid_change, "fieldPath": "element.key"}, "type change does not match"),
        ({**valid_change, "toType": "decimal(12,5)"}, "type change does not match"),
        ({**valid_change, "fieldPath": "element.value.element"}, "invalid delta.typeChanges fieldPath"),
    ):
        changes[0] = incorrect
        metadata["schemaString"] = json.dumps({"type": "struct", "fields": fields})
        commit.write_text("".join(json.dumps(action) + "\n" for action in actions))
        failure = subprocess.run(command, cwd=ROOT.parent, capture_output=True, text=True)
        expect_eq(failures, label, f"invalid widening metadata {incorrect} rejected",
                  failure.returncode != 0 and problem in failure.stderr, True)
    changes[0] = valid_change
    for index, history, column in (
        (0, [{"fromType": "integer", "toType": "long"}], "id"),
        (3, [], "element"),
        (4, [{"fromType": "decimal(7,2)", "toType": "decimal(10,4)",
              "fieldPath": "element.value"}], "value"),
    ):
        saved = fields[index]["metadata"]["delta.typeChanges"]
        fields[index]["metadata"]["delta.typeChanges"] = history
        metadata["schemaString"] = json.dumps({"type": "struct", "fields": fields})
        commit.write_text("".join(json.dumps(action) + "\n" for action in actions))
        failure = subprocess.run(command, cwd=ROOT.parent, capture_output=True, text=True)
        expect_eq(failures, label, f"unrecorded widening rejected for {column}",
                  failure.returncode != 0 and f"incompatible physical type for {column}" in failure.stderr,
                  True)
        fields[index]["metadata"]["delta.typeChanges"] = saved
    metadata["schemaString"] = json.dumps({"type": "struct", "fields": fields})
    intermediate = root / "intermediate.parquet"
    original_data = pq.read_table(data)
    pq.write_table(original_data.set_column(0, "id", pa.array([-2, 3], pa.int32())), intermediate)
    intermediate_add = {"add": {**addition["add"], "path": intermediate.name,
                                "size": intermediate.stat().st_size}}
    commit.write_text("".join(json.dumps(action) + "\n" for action in [*actions[:2], intermediate_add]))
    subprocess.run(command, cwd=ROOT.parent, check=True)
    expect_eq(failures, label, "recorded intermediate integer type still widens",
              [row["id"] for row in json.loads(output.read_text())["rows"]], [-2, 3])
    # Older checkpoints can still carry Date bounds. A structured timestamp
    # with submillisecond precision is already an inclusive upper bound.
    stats_type = pa.struct([
        ("numRecords", pa.int64()),
        ("minValues", pa.struct([("day", pa.date32())])),
        ("maxValues", pa.struct([("day", pa.date32()), ("event_time", pa.timestamp("us", tz="UTC")),
                                 ("score", pa.float64())])),
    ])
    added_file = {**addition["add"], "stats_parsed": {
        "numRecords": 2,
        "minValues": {"day": date(1969, 12, 31)},
        "maxValues": {"day": date(1969, 12, 31), "event_time": -501, "score": 0.1},
    }}
    del added_file["stats"]
    del added_file["partitionValues"]
    add_type = pa.struct([
        ("path", pa.string()), ("size", pa.int64()), ("modificationTime", pa.int64()),
        ("dataChange", pa.bool_()), ("stats_parsed", stats_type),
    ])
    pq.write_table(pa.table({
        "protocol": pa.array([protocol, None, None], type=pa.struct([
            ("minReaderVersion", pa.int32()), ("minWriterVersion", pa.int32()),
            ("readerFeatures", pa.list_(pa.string())), ("writerFeatures", pa.list_(pa.string())),
        ])),
        "metaData": pa.array([None, metadata, None], type=pa.struct([
            ("id", pa.string()), ("schemaString", pa.string()),
        ])),
        "add": pa.array([None, None, added_file], type=add_type),
    }), log / "00000000000000000000.checkpoint.parquet")
    commit.unlink()
    subprocess.run(command, cwd=ROOT.parent, check=True)
    actual_stats = json.loads(output.read_text())["stats"][0]
    expect_eq(failures, label, "Float32 maximum copied into Double checkpoint statistics stays conservative",
              actual_stats["maxValues"].pop("score") >= score[1], True)
    expect_eq(failures, label, "date statistics widened after checkpoint-only recovery",
              actual_stats, {**expected_stats, "minValues": {"day": "1969-12-31 00:00:00"},
                             "maxValues": {"day": "1969-12-31 00:00:00", "event_time": "1969-12-31T23:59:59.999499Z"}})
    # Spark can copy its truncated JSON bounds into structured checkpoints.
    checkpoint = log / "00000000000000000000.checkpoint.parquet"
    structured = pq.read_table(checkpoint)
    added_file["stats_parsed"]["maxValues"]["event_time"] = -1000
    double_bound = score[1] + 1e-9
    added_file["stats_parsed"]["maxValues"]["score"] = double_bound
    pq.write_table(structured.set_column(structured.schema.get_field_index("add"), "add",
                                        pa.array([None, None, added_file], type=add_type)), checkpoint)
    subprocess.run(command, cwd=ROOT.parent, check=True)
    actual_bounds = json.loads(output.read_text())["stats"][0]["maxValues"]
    expect_eq(failures, label, "millisecond-aligned structured timestamp remains conservative",
              actual_bounds["event_time"], "1970-01-01T00:00:00Z")
    expect_eq(failures, label, "Double maximum above the exact Float32 value is preserved",
              actual_bounds["score"], double_bound)


def case_scan_void_evolution(failures: Failures, root: Path) -> None:
    label = "void-evolution"
    old = pa.table({
        "id": [1, 2],
        "number": pa.nulls(2),
        "record": pa.nulls(2),
        "items": pa.array([[None, None], []], pa.list_(pa.null())),
        "lookup": pa.array([{"a": None}, None], pa.map_(pa.string(), pa.null())),
    })
    write_deltalake(str(root), old)
    log = root / "_delta_log"
    commit = log / "00000000000000000000.json"
    metadata = next(action["metaData"] for line in commit.read_text().splitlines()
                    if "metaData" in (action := json.loads(line)))
    schema = json.loads(metadata["schemaString"])
    fields = {field["name"]: field for field in schema["fields"]}
    fields["number"]["type"] = "integer"
    fields["record"]["type"] = {"type": "struct", "fields": [
        {"name": "n", "type": "long", "nullable": True, "metadata": {}},
    ]}
    fields["items"]["type"]["elementType"] = "integer"
    fields["lookup"]["type"]["valueType"] = "string"
    metadata["schemaString"] = json.dumps(schema)
    latest = root / "z-current.parquet"
    pq.write_table(pa.table({
        "id": [3],
        "number": pa.array([5], pa.int32()),
        "record": pa.array([{"n": 6}], pa.struct([("n", pa.int64())])),
        "items": pa.array([[7, None]], pa.list_(pa.int32())),
        "lookup": pa.array([{"b": "updated"}], pa.map_(pa.string(), pa.string())),
    }), latest)
    addition = {"path": latest.name, "size": latest.stat().st_size,
                "modificationTime": 0, "dataChange": True, "partitionValues": {}}
    (log / "00000000000000000001.json").write_text(
        json.dumps({"metaData": metadata}) + "\n" + json.dumps({"add": addition}) + "\n")
    output = root.parent / (root.name + ".table-scan.json")
    command = ["cabal", "run", "wireform-delta:wireform-delta-interop-probe", "--",
               "--scan-table", str(root), str(output)]
    expected = [
        {"id": 1, "number": None, "record": None, "items": [None, None], "lookup": {"a": None}},
        {"id": 2, "number": None, "record": None, "items": [], "lookup": None},
        {"id": 3, "number": 5, "record": {"n": 6}, "items": [7, None], "lookup": {"b": "updated"}},
    ]

    def scan(source):
        attempt = subprocess.run(command, cwd=ROOT.parent, capture_output=True, text=True)
        actual = (sorted(json.loads(output.read_text())["rows"], key=lambda row: row["id"])
                  if attempt.returncode == 0 else attempt.stderr)
        expect_eq(failures, label, f"legacy physical NULL columns widen after {source}", actual, expected)

    scan("JSON replay")
    DeltaTable(str(root)).create_checkpoint()
    commit.unlink()
    (log / "00000000000000000001.json").unlink()
    scan("checkpoint recovery")


def case_integer_partitions(failures: Failures, root: Path) -> None:
    label = "integer-partitions"
    widths = {"p8": 8, "p16": 16, "p32": 32, "p64": 64}
    types = (pa.int8(), pa.int16(), pa.int32(), pa.int64())
    columns = {name: pa.array([0], kind) for name, kind in zip(widths, types)}
    write_deltalake(str(root), pa.table({"id": [1], **columns}), partition_by=list(widths))
    commit = root / "_delta_log" / "00000000000000000000.json"
    actions = [json.loads(line) for line in commit.read_text().splitlines()]
    addition = next(row["add"] for row in actions if "add" in row)
    output = root.parent / (root.name + ".table-scan.json")
    command = ["cabal", "run", "wireform-delta:wireform-delta-interop-probe", "--",
               "--scan-table", str(root), str(output)]

    def scan(values):
        addition["partitionValues"] = values
        commit.write_text("".join(json.dumps(action) + "\n" for action in actions))
        return subprocess.run(command, cwd=ROOT.parent, capture_output=True, text=True)

    minimums = {name: str(-(2 ** (bits - 1))) for name, bits in widths.items()}
    maximums = {name: str(2 ** (bits - 1) - 1) for name, bits in widths.items()}
    zeros = dict.fromkeys(widths, "0")
    for values in (minimums, maximums, {"p8": "+42", "p16": "-00042", "p32": "-0", "p64": ""}):
        attempt = scan(values)
        reference = DeltaTable(str(root)).to_pyarrow_table().to_pylist()
        expect_eq(failures, label, f"valid signed integers {values}",
                  json.loads(output.read_text())["rows"] if attempt.returncode == 0 else attempt.stderr,
                  reference)
    for name, bits in widths.items():
        for value in (str(2 ** (bits - 1)), str(-(2 ** (bits - 1)) - 1)):
            attempt = scan({**zeros, name: value})
            expect_eq(failures, label, f"{name} rejects overflow {value}",
                      attempt.returncode != 0 and "invalid partition value" in attempt.stderr, True)
    for value in ("0x2a", "0o52", "(42)", " 42 ", "42x", "+", "-", "9" * 100_000):
        attempt = scan({**zeros, "p64": value})
        expect_eq(failures, label, f"malformed number {value[:32]!r} rejected",
                  attempt.returncode != 0 and "invalid partition value" in attempt.stderr, True)


def case_decimal_partitions(failures: Failures, root: Path) -> None:
    label = "decimal-partitions"
    write_deltalake(str(root), pa.table({
        "id": [1],
        "bucket": pa.array([Decimal("999.99")], pa.decimal128(5, 2)),
    }), partition_by=["bucket"])
    commit = root / "_delta_log" / "00000000000000000000.json"
    actions = [json.loads(line) for line in commit.read_text().splitlines()]
    addition = next(action["add"] for action in actions if "add" in action)
    output = root.parent / (root.name + ".table-scan.json")
    command = ["cabal", "run", "wireform-delta:wireform-delta-interop-probe", "--",
               "--scan-table", str(root), str(output)]

    def scan(value):
        addition["partitionValues"] = {"bucket": value}
        commit.write_text("".join(json.dumps(action) + "\n" for action in actions))
        return subprocess.run(command, cwd=ROOT.parent, capture_output=True, text=True)

    reference = DeltaTable(str(root)).to_pyarrow_table().column("bucket")[0].as_py()
    scan("999.99").check_returncode()
    expect_eq(failures, label, "delta-rs written partition value agrees with PyArrow",
              json.loads(output.read_text())["rows"], [{"id": 1, "bucket": int(reference.scaleb(2))}])
    for value, expected in (("-999.99", -99999), ("1.2300", 123), (".5", 50), ("+12.3e-1", 123),
                            ("1e-2", 1), ("0100e-00004", 1), ("0e999999999999999999999", 0),
                            (None, None), ("", None)):
        attempt = scan(value)
        attempt.check_returncode()
        expect_eq(failures, label, f"exact unscaled value for {value!r}",
                  json.loads(output.read_text())["rows"], [{"id": 1, "bucket": expected}])
    for value in ("1000.00", "-0.001", "0e+", "1.0.0", "1e18446744073709551616", "1e1000000000", "1e-1000000000"):
        attempt = scan(value)
        expect_eq(failures, label, f"invalid decimal {value!r} rejected",
                  attempt.returncode != 0 and "invalid partition value" in attempt.stderr, True)


def case_binary_partitions(failures: Failures, root: Path) -> None:
    label = "binary-partitions"
    values = [b"\x00\x01\xff", "café".encode(), b"\\u0041", None]
    write_deltalake(str(root), pa.table({
        "id": list(range(len(values))),
        "bucket": pa.array(values, pa.binary()),
    }), partition_by=["bucket"])
    table = DeltaTable(str(root))
    output = root.parent / (root.name + ".table-scan.json")
    command = ["cabal", "run", "wireform-delta:wireform-delta-interop-probe", "--",
               "--scan-table", str(root), str(output)]
    expected = [{"id": i, "bucket": None if data is None else list(data)}
                for i, data in enumerate(values)]

    def scan(description):
        subprocess.run(command, cwd=ROOT.parent, check=True)
        expect_eq(failures, label, description,
                  sorted(json.loads(output.read_text())["rows"], key=lambda row: row["id"]),
                  expected)

    scan("JSON replay preserves original binary values")
    table.create_checkpoint()
    commit = root / "_delta_log" / "00000000000000000000.json"
    actions = [json.loads(line) for line in commit.read_text().splitlines()]
    commit.unlink()
    scan("checkpoint recovery preserves original binary values")

    # Also exercise byte values escaped by JSON itself, with no literal
    # per-byte escape sequence left after the log's JSON parser.
    direct = [values[0], values[1], b"prefix\\u0041", None]
    for action in actions:
        if "add" in action:
            addition = action["add"]
            index = json.loads(addition["stats"])["minValues"]["id"]
            data = direct[index]
            addition["partitionValues"]["bucket"] = None if data is None else data.decode("latin-1")
    (root / "_delta_log" / "00000000000000000001.json").write_text(
        "".join(json.dumps(action) + "\n" for action in actions if "add" in action))
    expected[2]["bucket"] = list(direct[2])
    scan("single-layer JSON escapes preserve the stored byte values")


def case_floating_partitions(failures: Failures, root: Path, typ) -> None:
    label = f"floating-partitions-{typ}"
    values = [float("inf"), float("-inf"), float("nan"), -0.0, 1.25, None]
    write_deltalake(str(root), pa.table({"id": list(range(len(values))), "p": pa.array(values, typ)}),
                    partition_by=["p"])
    output = root.parent / (root.name + ".table-scan.json")
    command = ["cabal", "run", "wireform-delta:wireform-delta-interop-probe", "--",
               "--scan-table", str(root), str(output)]

    def probe():
        subprocess.run(command, cwd=ROOT.parent, check=True)
        return sorted(json.loads(output.read_text(), parse_int=float)["rows"], key=lambda row: row["id"])

    def wire_value(value):
        if value is None:
            return None
        if math.isnan(value):
            return "NaN"
        if math.isinf(value):
            return "-inf" if value < 0 else "+inf"
        if value == 0.0 and math.copysign(1, value) < 0:
            return "-0"
        return value

    def compare_reference(description):
        table = DeltaTable(str(root)).to_pyarrow_table().sort_by("id")
        expect_eq(failures, label, description, probe(),
                  [{"id": row["id"], "p": wire_value(row["p"])} for row in table.to_pylist()])

    compare_reference("writer infinities, NaN and signed zero")
    commit = root / "_delta_log" / "00000000000000000000.json"
    actions = [json.loads(line) for line in commit.read_text().splitlines()]
    additions = sorted((row["add"] for row in actions if "add" in row),
                       key=lambda row: json.loads(row["stats"])["minValues"]["id"])
    midpoint = "1.000000059604644775390625" if typ == pa.float32() else "1.00000000000000011102230246251565404236316680908203125"
    new_values = ["1e18446744073709551616", "-1e-18446744073709551616", "-0e1000000000",
                  midpoint, midpoint + "0" * 800 + "1", "0.5"]
    for addition, value in zip(additions, new_values):
        addition["partitionValues"]["p"] = value
    commit.write_text("".join(json.dumps(action) + "\n" for action in actions))
    compare_reference("large exponents and exact rounding at and above a midpoint")
    if typ == pa.float32():
        limits = ["1e-45", "7e-46", "-7e-46", "3.4028235e38", "3.4028236e38", "0"]
    else:
        limits = ["4e-324", "2e-324", "-2e-324", "1.7976931348623157e308", "1.7976931348623159e308", "0"]
    for addition, value in zip(additions, limits):
        addition["partitionValues"]["p"] = value
    commit.write_text("".join(json.dumps(action) + "\n" for action in actions))
    compare_reference("subnormal, signed underflow and overflow boundaries")


def case_timestamp_partitions(failures: Failures, root: Path) -> None:
    label = "timestamp-partitions"
    write_deltalake(str(root), pa.table({"id": [1], "p": pa.array([0], pa.timestamp("us", "UTC"))}),
                    partition_by=["p"])
    commit = root / "_delta_log" / "00000000000000000000.json"
    actions = [json.loads(line) for line in commit.read_text().splitlines()]
    addition = next(action["add"] for action in actions if "add" in action)
    output = root.parent / (root.name + ".table-scan.json")
    command = ["cabal", "run", "wireform-delta:wireform-delta-interop-probe", "--",
               "--scan-table", str(root), str(output)]

    def scan(value):
        addition["partitionValues"]["p"] = value
        commit.write_text("".join(json.dumps(action) + "\n" for action in actions))
        return subprocess.run(command, cwd=ROOT.parent, capture_output=True, text=True)

    epoch = datetime(1970, 1, 1, tzinfo=timezone.utc)
    for value in ("1969-12-31 23:59:59.999999", "1970-01-01T01:00:00.123456+01:00",
                  "1969-12-31T23:59:59.999999Z", "1970-01-01T00:00:59.123456-0030",
                  "1970-01-01T00:00:00.1234560000000Z"):
        scan(value).check_returncode()
        reference = DeltaTable(str(root)).to_pyarrow_table().column("p")[0].as_py() - epoch
        micros = (reference.days * 86400 + reference.seconds) * 1_000_000 + reference.microseconds
        expect_eq(failures, label, f"exact microseconds for {value}",
                  json.loads(output.read_text())["rows"], [{"id": 1, "p": micros}])
    # Spark 4 can include the seconds component of a historical UTC offset.
    # Use a UTC datetime oracle since delta-rs readers may reject this spelling.
    value = "0202-10-21T00:00:00.123456+00:53:28"
    scan(value).check_returncode()
    reference = datetime(202, 10, 20, 23, 6, 32, 123456, tzinfo=timezone.utc) - epoch
    micros = (reference.days * 86400 + reference.seconds) * 1_000_000 + reference.microseconds
    expect_eq(failures, label, "historical offset keeps its seconds",
              json.loads(output.read_text())["rows"], [{"id": 1, "p": micros}])
    for value in ("1969-12-31 23:59:59.9999995", "1970-01-01T00:00:00.1234560000001Z",
                  "1970-01-01T00:00:00+25:90", "1970-01-01T00:00:00+12:60",
                  "1970-01-01T00:00:00+05:30:60"):
        attempt = scan(value)
        expect_eq(failures, label, f"invalid timestamp {value} rejected",
                  attempt.returncode != 0 and "invalid partition value" in attempt.stderr, True)


def case_extended_calendar_partitions(failures: Failures, root: Path) -> None:
    label = "extended-calendar-partitions"
    days = [-719893, -719528, 2932896, 2932897]
    micros = [day * 86_400_000_000 + 123456 for day in days]
    dates = pa.array(days, pa.int32()).cast(pa.date32())
    stamps = pa.array(micros, pa.int64()).cast(pa.timestamp("us", "UTC"))
    ids = list(range(len(days)))
    write_deltalake(str(root), pa.table({"id": ids, "day": dates, "stamp": stamps}),
                    partition_by=["day", "stamp"])
    log = root / "_delta_log"
    commit = log / "00000000000000000000.json"
    actions = [json.loads(line) for line in commit.read_text().splitlines()]
    adds = [action["add"] for action in actions if "add" in action]
    expect_eq(failures, label, "delta-rs emitted a signed positive year",
              any(add["partitionValues"]["day"] == "+10000-01-01" for add in adds), True)
    output = root.parent / (root.name + ".table-scan.json")
    command = ["cabal", "run", "wireform-delta:wireform-delta-interop-probe", "--",
               "--scan-table", str(root), str(output)]
    expected = [{"id": i, "day": day, "stamp": stamp} for i, day, stamp in zip(ids, days, micros)]

    def scan(source):
        attempt = subprocess.run(command, cwd=ROOT.parent, capture_output=True, text=True)
        actual = (sorted(json.loads(output.read_text())["rows"], key=lambda row: row["id"])
                  if attempt.returncode == 0 else attempt.stderr)
        expect_eq(failures, label, f"signed calendar years in {source}", actual, expected)

    scan("delta-rs JSON")
    DeltaTable(str(root)).create_checkpoint()
    commit.unlink()
    scan("delta-rs checkpoint")

    expanded = next(add["partitionValues"] for add in adds if add["partitionValues"]["day"].startswith("+"))
    (log / "00000000000000000000.checkpoint.parquet").unlink()
    (log / "_last_checkpoint").unlink()
    expanded["stamp"] = "+10000-01-01T01:00:00.123456+01:00"
    commit.write_text("".join(json.dumps(action) + "\n" for action in actions))
    scan("ISO timestamp with an explicit offset")

    expanded["stamp"] = "+-10000-01-01T01:00:00.123456+01:00"
    commit.write_text("".join(json.dumps(action) + "\n" for action in actions))
    attempt = subprocess.run(command, cwd=ROOT.parent, capture_output=True, text=True)
    expect_eq(failures, label, "malformed signed year rejected",
              attempt.returncode != 0 and "invalid partition value for stamp" in attempt.stderr, True)


def case_widened_date_partition(failures: Failures, root: Path) -> None:
    label = "widened-date-partition"
    log = root / "_delta_log"
    log.mkdir(parents=True)
    protocol = {"minReaderVersion": 3, "minWriterVersion": 7,
                "readerFeatures": ["typeWidening", "timestampNtz"],
                "writerFeatures": ["typeWidening", "timestampNtz"]}
    partition = {"name": "day", "type": "date", "nullable": True, "metadata": {}}
    schema = {"type": "struct", "fields": [
        {"name": "id", "type": "long", "nullable": False, "metadata": {}}, partition,
    ]}
    metadata = {"id": label, "partitionColumns": ["day"], "format": {"provider": "parquet", "options": {}},
                "configuration": {"delta.enableTypeWidening": "true"},
                "schemaString": json.dumps(schema)}

    def add_file(name, ident, value):
        data = root / name
        pq.write_table(pa.table({"id": pa.array([ident], pa.int64())}), data)
        return {"add": {"path": name, "size": data.stat().st_size, "modificationTime": 0,
                        "dataChange": True, "partitionValues": {"day": value}}}

    initial = [{"protocol": protocol}, {"metaData": metadata}, add_file("a.parquet", 1, "1969-12-31"),
               add_file("d.parquet", 4, "+10000-01-01")]
    (log / "00000000000000000000.json").write_text("".join(json.dumps(action) + "\n" for action in initial))
    partition["type"] = "timestamp_ntz"
    partition["metadata"]["delta.typeChanges"] = [{"fromType": "date", "toType": "timestamp_ntz"}]
    metadata["schemaString"] = json.dumps(schema)
    actions = [{"metaData": metadata}, add_file("b.parquet", 2, "1970-01-02 00:00:00.123456"),
               add_file("c.parquet", 3, None)]
    commit = log / "00000000000000000001.json"
    commit.write_text("".join(json.dumps(action) + "\n" for action in actions))
    output = root.parent / (root.name + ".table-scan.json")
    command = ["cabal", "run", "wireform-delta:wireform-delta-interop-probe", "--",
               "--scan-table", str(root), str(output)]
    subprocess.run(command, cwd=ROOT.parent, check=True)
    expect_eq(failures, label, "old dates, new timestamps and null partitions", json.loads(output.read_text())["rows"],
              [{"id": 1, "day": -86400000000}, {"id": 2, "day": 86400123456},
               {"id": 3, "day": None}, {"id": 4, "day": 253402300800000000}])

    partition["metadata"].clear()
    metadata["schemaString"] = json.dumps(schema)
    commit.write_text("".join(json.dumps(action) + "\n" for action in actions))
    failure = subprocess.run(command, cwd=ROOT.parent, capture_output=True, text=True)
    expect_eq(failures, label, "a date without widening metadata is rejected",
              failure.returncode != 0 and "invalid partition value" in failure.stderr, True)


def case_scan_legacy_timestamps(failures: Failures, root: Path) -> None:
    label = "legacy-int96"
    log = root / "_delta_log"
    log.mkdir(parents=True)
    data = root / "legacy.parquet"
    times = pa.array([datetime(1969, 12, 31, 23, 59, 59, 999999), None,
                      datetime(1970, 1, 1, 0, 0, 0, 123456)], pa.timestamp("us"))
    pq.write_table(pa.table({"utc": times, "local": times}), data,
                   use_deprecated_int96_timestamps=True)
    protocol = {"minReaderVersion": 3, "minWriterVersion": 7,
                "readerFeatures": ["timestampNtz"], "writerFeatures": ["timestampNtz"]}
    fields = [
        {"name": "utc", "type": "timestamp", "nullable": True, "metadata": {}},
        {"name": "local", "type": "timestamp_ntz", "nullable": True, "metadata": {}},
    ]
    metadata = {"id": label, "partitionColumns": [], "configuration": {},
                "format": {"provider": "parquet", "options": {}},
                "schemaString": json.dumps({"type": "struct", "fields": fields})}
    addition = {"path": data.name, "size": data.stat().st_size, "modificationTime": 0,
                "dataChange": True, "partitionValues": {}}
    actions = [{"protocol": protocol}, {"metaData": metadata}, {"add": addition}]
    (log / "00000000000000000000.json").write_text("".join(json.dumps(action) + "\n" for action in actions))
    output = root.parent / (root.name + ".table-scan.json")
    command = ["cabal", "run", "wireform-delta:wireform-delta-interop-probe", "--",
               "--scan-table", str(root), str(output)]
    subprocess.run(command, cwd=ROOT.parent, check=True)
    expect_eq(failures, label, "UTC and local INT96 values", json.loads(output.read_text())["rows"],
              [{"utc": -1, "local": -1}, {"utc": None, "local": None},
               {"utc": 123456, "local": 123456}])
    for unit, values, expected in (
        ("ms", [-9_223_372_036_854_775, None, 9_223_372_036_854_775],
         [-9_223_372_036_854_775_000, None, 9_223_372_036_854_775_000]),
        ("ns", [-1000, None, 2000], [-1, None, 2]),
        ("ms", [9_223_372_036_854_776], None),
        ("ms", [-9_223_372_036_854_776], None),
        ("ns", [1001], None),
    ):
        utc = pa.array(values, pa.int64()).cast(pa.timestamp(unit, "UTC"))
        local = pa.array(values, pa.int64()).cast(pa.timestamp(unit))
        pq.write_table(pa.table({"utc": utc, "local": local}), data, version="2.6")
        addition["size"] = data.stat().st_size
        (log / "00000000000000000000.json").write_text("".join(json.dumps(action) + "\n" for action in actions))
        result = subprocess.run(command, cwd=ROOT.parent, capture_output=True, text=True)
        if expected is None:
            expect_eq(failures, label, f"{unit} inexact or overflowing value rejected",
                      result.returncode != 0 and "cannot be represented in microseconds" in result.stderr, True)
        else:
            result.check_returncode()
            expect_eq(failures, label, f"{unit} values converted exactly",
                      json.loads(output.read_text())["rows"],
                      [{"utc": value, "local": value} for value in expected])
    instant = pa.array([0], pa.timestamp("us", "UTC"))
    wall_time = pa.array([0], pa.timestamp("us"))
    for swapped in ("utc", "local"):
        pq.write_table(pa.table({
            "utc": wall_time if swapped == "utc" else instant,
            "local": instant if swapped == "local" else wall_time,
        }), data)
        addition["size"] = data.stat().st_size
        (log / "00000000000000000000.json").write_text("".join(json.dumps(action) + "\n" for action in actions))
        result = subprocess.run(command, cwd=ROOT.parent, capture_output=True, text=True)
        expect_eq(failures, label, f"wrong Parquet UTC flag for {swapped} rejected",
                  result.returncode != 0 and f"incompatible physical type for {swapped}" in result.stderr,
                  True)


def case_compaction_checkpoint(failures: Failures, root: Path) -> None:
    label = "compaction-checkpoint"
    write_deltalake(str(root), pa.table({"id": [0]}))
    DeltaTable(str(root)).create_checkpoint()
    write_deltalake(str(root), pa.table({"id": [1]}), mode="overwrite")
    DeltaTable(str(root)).create_checkpoint()
    write_deltalake(str(root), pa.table({"id": [2]}), mode="append")
    dt = DeltaTable(str(root))
    expected = sorted(deltalake_relative_files(dt, root))
    log = root / "_delta_log"
    first = log / "00000000000000000001.json"
    last = log / "00000000000000000002.json"
    compacted = log / "00000000000000000001.00000000000000000002.compacted.json"
    state_actions = [json.loads(line) for source in (first, last)
                     for line in source.read_text().splitlines()]
    compacted.write_text("".join(json.dumps(a) + "\n" for a in state_actions if "commitInfo" not in a))
    delta_rs_history = dt.history()
    first.unlink()
    result = run_probe(root)
    expect_eq(failures, label, "live files through checkpoint and compaction",
              result["active_relative_paths"], expected)
    expect_eq(failures, label, "latest version", result["version"], 2)
    expect_eq(failures, label, "latest provenance from numbered commit",
              result["history"][0]["operation"], delta_rs_history[0]["operation"])
    last.unlink()
    compact_only_tail = run_probe(root)
    expect_eq(failures, label, "live files using older checkpoint and compaction",
              compact_only_tail["active_relative_paths"], expected)


def case_row_tracking(failures: Failures, root: Path) -> None:
    label = "row-tracking"
    log = root / "_delta_log"
    log.mkdir(parents=True)
    columns = pa.table({
        "id": [10, 11, 12, 13, 14, 15, 16],
        "_materialized_id": pa.array([None, 700, 202, None, 800, None, None], pa.int64()),
        "_materialized_version": pa.array([None, 0, None, None, 0, None, 0], pa.int64()),
    })
    tracked = root / "01-tracked.parquet"
    plain = root / "02-defaults.parquet"
    pq.write_table(columns, tracked, row_group_size=2)
    pq.write_table(pa.table({"id": [17, 18]}), plain)
    # Rows 2 and 3 are a full deleted group. Row 0 is deleted from the
    # first group, so its surviving neighbor retains physical offset 1.
    bitmap = struct.pack("<IQIIIHHI3H", 1681511377, 1, 0, 12346, 1, 0, 2, 16, 0, 2, 3)
    dv_file = root / "deleted.bin"
    dv_file.write_bytes(b"\x01" + struct.pack(">I", len(bitmap))
                        + bitmap + struct.pack(">I", zlib.crc32(bitmap)))
    descriptor = {"storageType": "p", "pathOrInlineDv": dv_file.as_uri(), "offset": 1,
                  "sizeInBytes": len(bitmap), "cardinality": 3}
    protocol = {"minReaderVersion": 3, "minWriterVersion": 7,
                "readerFeatures": ["deletionVectors"],
                "writerFeatures": ["deletionVectors", "rowTracking", "domainMetadata"]}
    schema = {"type": "struct", "fields": [
        {"name": "id", "type": "long", "nullable": True, "metadata": {}},
    ]}
    metadata = {"id": label, "schemaString": json.dumps(schema), "partitionColumns": [],
                "format": {"provider": "parquet", "options": {}}, "configuration": {
                    "delta.enableDeletionVectors": "true",
                    "delta.enableRowTracking": "true",
                    "delta.rowTracking.materializedRowIdColumnName": "_materialized_id",
                    "delta.rowTracking.materializedRowCommitVersionColumnName": "_materialized_version",
                }}

    fully_deleted = root / "00-deleted.parquet"
    pq.write_table(pa.table({"id": [8, 9]}), fully_deleted, row_group_size=1)
    full_bitmap = struct.pack("<IQIIIHHI2H", 1681511377, 1, 0, 12346, 1, 0, 1, 16, 0, 1)
    full_dv = root / "fully-deleted.bin"

    def write_full_vector(bitmap):
        full_dv.write_bytes(b"\x01" + struct.pack(">I", len(bitmap))
                            + bitmap + struct.pack(">I", zlib.crc32(bitmap)))

    write_full_vector(full_bitmap)

    def addition(path, base, count):
        return {"path": path.name, "size": path.stat().st_size, "modificationTime": 0,
                "dataChange": True, "partitionValues": {}, "baseRowId": base,
                "defaultRowCommitVersion": 1, "stats": json.dumps({"numRecords": count})}

    first = addition(tracked, 100, 7)
    first["deletionVector"] = descriptor
    first["stats"] = json.dumps({"numRecords": 7, "tightBounds": False})
    removed = addition(fully_deleted, 50, 2)
    removed["deletionVector"] = {**descriptor, "pathOrInlineDv": full_dv.as_uri(),
                                 "sizeInBytes": len(full_bitmap), "cardinality": 2}
    initial = log / "00000000000000000000.json"
    commit = log / "00000000000000000001.json"
    def domain(name, config, removed=False):
        return {"domainMetadata": {"domain": name, "configuration": config, "removed": removed}}

    start = [{"protocol": protocol}, {"metaData": metadata},
             domain("example.keep", "before"), domain("example.remove", "discard")]
    initial.write_text("".join(json.dumps(action) + "\n" for action in start))
    actions = [{"add": removed}, {"add": first}, {"add": addition(plain, 1000, 2)},
               domain("example.keep", "after"), domain("example.remove", "discard", True),
               domain("delta.rowTracking", '{"rowIdHighWaterMark":1001}')]
    commit.write_text("".join(json.dumps(action) + "\n" for action in actions))
    output = root.parent / (root.name + ".tracked-scan.json")
    command = ["cabal", "run", "wireform-delta:wireform-delta-interop-probe", "--",
               "--scan-tracked", str(root), str(output)]
    expected = {"rows": [{"id": i} for i in (11, 14, 15, 16, 17, 18)],
                "ids": [700, 800, 105, 106, 1000, 1001],
                "versions": [0, 0, 1, 0, 1, 1]}
    subprocess.run(command, cwd=ROOT.parent, check=True)
    expect_eq(failures, label, "stable values and physical fallback positions",
              json.loads(output.read_text()), expected)
    expected_domains = {"example.keep": "after", "delta.rowTracking": '{"rowIdHighWaterMark":1001}'}
    expect_eq(failures, label, "latest live domains from JSON",
              run_probe(root)["domains"], expected_domains)
    DeltaTable(str(root)).create_checkpoint()
    commit.unlink()
    initial.unlink()
    subprocess.run(command, cwd=ROOT.parent, check=True)
    expect_eq(failures, label, "row tracking survives checkpoint-only recovery",
              json.loads(output.read_text()), expected)
    expect_eq(failures, label, "live domains from checkpoint",
              run_probe(root)["domains"], expected_domains)
    # Keep cardinality equal to the footer row count, but make one index
    # out of range. The empty-file shortcut must still reject the vector.
    write_full_vector(full_bitmap[:-2] + struct.pack("<H", 2))
    attempt = subprocess.run(command, cwd=ROOT.parent, capture_output=True, text=True)
    expect_eq(failures, label, "fully deleted file still checks bitmap range",
              attempt.returncode != 0 and "deleted row outside the Parquet file" in attempt.stderr, True)


def case_row_tracking_boundaries(failures: Failures, root: Path) -> None:
    label = "row-tracking-boundaries"
    log = root / "_delta_log"
    log.mkdir(parents=True)
    data = root / "rows.parquet"
    data_columns = {"id": [10, 11], "_row_id": pa.array([None, 7], pa.int64())}
    pq.write_table(pa.table(data_columns), data)
    protocol = {"minReaderVersion": 1, "minWriterVersion": 7,
                "writerFeatures": ["rowTracking", "domainMetadata"]}
    schema = {"type": "struct", "fields": [
        {"name": "id", "type": "long", "nullable": True, "metadata": {}},
    ]}
    metadata = {"id": label, "schemaString": json.dumps(schema), "partitionColumns": [],
                "format": {"provider": "parquet", "options": {}}, "configuration": {
                    "delta.enableRowTracking": "true",
                    "delta.rowTracking.materializedRowIdColumnName": "_row_id",
                    "delta.rowTracking.materializedRowCommitVersionColumnName": "_row_version",
                }}
    addition = {"path": data.name, "size": data.stat().st_size, "modificationTime": 0,
                "dataChange": True, "partitionValues": {}, "baseRowId": 2**63 - 1, "defaultRowCommitVersion": 0}
    commit = log / "00000000000000000000.json"
    actions = [{"protocol": protocol}, {"metaData": metadata}, {"add": addition}]
    output = root.parent / (root.name + ".tracked-scan.json")
    command = ["cabal", "run", "wireform-delta:wireform-delta-interop-probe", "--",
               "--scan-tracked", str(root), str(output)]

    def write_commit():
        commit.write_text("".join(json.dumps(action) + "\n" for action in actions))

    write_commit()
    subprocess.run(command, cwd=ROOT.parent, check=True)
    expect_eq(failures, label, "stored row ID at an overflowing default position",
              json.loads(output.read_text()), {"rows": [{"id": 10}, {"id": 11}],
                                              "ids": [2**63 - 1, 7], "versions": [0, 0]})
    data_columns["_row_id"] = pa.array([7, None], pa.int64())
    pq.write_table(pa.table(data_columns), data)
    addition["size"] = data.stat().st_size
    write_commit()
    attempt = subprocess.run(command, cwd=ROOT.parent, capture_output=True, text=True)
    expect_eq(failures, label, "an overflowing default still fails",
              attempt.returncode != 0 and "row ID exceeds" in attempt.stderr, True)

    bitmap = struct.pack("<IQIIIHHIH", 1681511377, 1, 0, 12346, 1, 0, 0, 16, 1)
    dv_file = root / "deleted.bin"
    dv_file.write_bytes(b"\x01" + struct.pack(">I", len(bitmap))
                        + bitmap + struct.pack(">I", zlib.crc32(bitmap)))
    protocol.update(minReaderVersion=3, readerFeatures=["deletionVectors"])
    protocol["writerFeatures"].append("deletionVectors")
    addition.update(stats=json.dumps({"numRecords": 3}), deletionVector={
        "storageType": "p", "pathOrInlineDv": dv_file.as_uri(), "offset": 1,
        "sizeInBytes": len(bitmap), "cardinality": 1,
    })
    data_columns["id"] = [10, 11, 12]
    data_columns["_row_id"] = pa.array([None, None, 7], pa.int64())
    data_columns["_row_version"] = pa.array([None, 1, 2], pa.int64())
    pq.write_table(pa.table(data_columns), data)
    addition["size"] = data.stat().st_size
    write_commit()
    attempt = subprocess.run(command, cwd=ROOT.parent, capture_output=True, text=True)
    expect_eq(failures, label, "selected rows use validity and stored values at physical indices",
              json.loads(output.read_text()) if attempt.returncode == 0 else attempt.stderr,
              {"rows": [{"id": 10}, {"id": 12}], "ids": [2**63 - 1, 7], "versions": [0, 2]})

    data_columns["_row_id"] = pa.array([None, 7, None], pa.int64())
    pq.write_table(pa.table(data_columns), data)
    addition["size"] = data.stat().st_size
    write_commit()
    attempt = subprocess.run(command, cwd=ROOT.parent, capture_output=True, text=True)
    expect_eq(failures, label, "an overflowing default at a live physical index still fails",
              attempt.returncode != 0 and "row ID exceeds" in attempt.stderr, True)

    # A materialized partition value can be absent from the old Parquet file,
    # so checking only projected fields would miss this reserved name.
    schema["fields"].append({"name": "_row_version", "type": "long", "nullable": True, "metadata": {}})
    metadata["schemaString"] = json.dumps(schema)
    metadata["partitionColumns"] = ["_row_version"]
    addition["partitionValues"] = {"_row_version": "99"}
    addition["baseRowId"] = 0
    write_commit()
    attempt = subprocess.run(command, cwd=ROOT.parent, capture_output=True, text=True)
    expect_eq(failures, label, "hidden and synthesized partition name collision rejected",
              attempt.returncode != 0 and "also a data column" in attempt.stderr, True)


def main() -> int:
    failures = Failures()
    out = Path(tempfile.mkdtemp(prefix="wireform-delta-probe-"))

    case_unpartitioned(failures, out / "table_unpart")
    case_partitioned (failures, out / "table_part")
    case_checkpointed(failures, out / "table_ckpt")
    case_checkpoint_replay(failures, out / "table_ckpt_replay")
    case_checkpoint_recovery(failures, out / "table_ckpt_recovery")
    case_checkpoint_path_alias(failures, out / "table_ckpt_path_alias")
    case_nullable_add_tags(failures, out / "table_nullable_tags")
    case_v2_checkpoint(failures, out / "table_ckpt_v2_json", "json")
    case_v2_checkpoint(failures, out / "table_ckpt_v2_parquet", "parquet")
    case_multipart_checkpoint(failures, out / "table_ckpt_multipart")
    case_local_scan_paths(failures, out / "table_local_scan_paths")
    case_invalid_checkpoint_text(failures, out / "table_ckpt_invalid_text")
    case_deletion_vector_scan(failures, out / "table_deletion_vector")
    case_structured_checkpoint_stats(failures, out / "table_structured_stats")
    case_mapped_scan(failures, out / "table_mapped_name", "name")
    case_mapped_scan(failures, out / "table_mapped_id", "id")
    case_mapped_scan(failures, out / "table_mapped_legacy", "name", legacy_protocol=True)
    case_scan_widening(failures, out / "table_scan_widening")
    case_scan_void_evolution(failures, out / "table_scan_void_evolution")
    case_widened_date_partition(failures, out / "table_widened_date_partition")
    case_integer_partitions(failures, out / "table_integer_partitions")
    case_decimal_partitions(failures, out / "table_decimal_partitions")
    case_binary_partitions(failures, out / "table_binary_partitions")
    case_floating_partitions(failures, out / "table_float_partitions", pa.float32())
    case_floating_partitions(failures, out / "table_double_partitions", pa.float64())
    case_timestamp_partitions(failures, out / "table_timestamp_partitions")
    case_extended_calendar_partitions(failures, out / "table_extended_calendar_partitions")
    case_scan_legacy_timestamps(failures, out / "table_scan_legacy_timestamps")
    case_compaction_checkpoint(failures, out / "table_compaction_checkpoint")
    case_row_tracking(failures, out / "table_row_tracking")
    case_row_tracking_boundaries(failures, out / "table_row_tracking_boundaries")
    case_partitioned_checkpoint(failures, out / "table_part_ckpt")
    case_time_travel_and_history(failures, out / "table_tt")
    case_checkpoint_in_commit_timestamps(failures, out / "table_in_commit_timestamps")

    print()
    if failures:
        print(f"{len(failures)} failures:")
        for k, v in failures:
            print(f"  FAIL {k}: {v}")
        return 1
    print("All wireform-delta outputs round-trip through deltalake.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
