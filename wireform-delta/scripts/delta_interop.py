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
import subprocess
import sys
import tempfile
from pathlib import Path

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


def run_probe(table_root: Path) -> dict:
    out_json = table_root.parent / (table_root.name + ".probe.json")
    subprocess.run(
        [
            "cabal", "run",
            "wireform-delta:wireform-delta-interop-probe",
            "--",
            str(table_root), str(out_json),
        ],
        cwd=ROOT.parent, check=True,
    )
    with out_json.open() as f:
        return json.load(f)


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
    for i in range(12):
        write_deltalake(
            str(root),
            pa.table({"region": ["us" if i % 2 else "eu"], "id": [i]}),
            partition_by=["region"],
            mode="append" if i > 0 else "error",
        )
    DeltaTable(str(root)).create_checkpoint()

    summary = run_probe(root)
    dt = DeltaTable(str(root))

    # The post-checkpoint snapshot must match deltalake.
    deltalake_files = sorted(deltalake_relative_files(dt, root))
    wireform_files  = sorted(f["path"] for f in summary["active_files"])
    expect_eq(failures, "part-ckpt", "active files",
              wireform_files, deltalake_files)

    # Every active file's partitionValues must be {"region": "us"|"eu"}.
    for f in summary["active_files"]:
        pv = f.get("partition_values") or {}
        if pv.get("region") not in ("us", "eu"):
            failures.add("part-ckpt",
                         f"active file {f['path']} partition_values = {pv!r}")

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
    out_v2 = root.parent / (root.name + ".v2.json")
    subprocess.run(
        [
            "cabal", "run",
            "wireform-delta:wireform-delta-interop-probe",
            "--",
            "--at", "2", str(root), str(out_v2),
        ],
        cwd=ROOT.parent, check=True,
    )
    with out_v2.open() as f:
        v2 = json.load(f)

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
            {"path": file_path, "size": 1, "modificationTime": 0,
             "dataChange": True, "deletionVector": {**old_dv, "offset": 2}},
            None, None, None,
        ], type=pa.struct([
            ("path", pa.string()), ("size", pa.int64()),
            ("modificationTime", pa.int64()), ("dataChange", pa.bool_()),
            ("deletionVector", dv_type),
        ])),
        "remove": pa.array([
            None, {"path": file_path, "dataChange": True, "deletionVector": old_dv},
            None, None,
        ], type=pa.struct([
            ("path", pa.string()), ("dataChange", pa.bool_()),
            ("deletionVector", dv_type),
        ])),
        "txn": pa.array([
            None, None, {"appId": "kept", "version": -1},
            {"appId": "updated", "version": 7, "lastUpdated": 42},
        ], type=pa.struct([
            ("appId", pa.string()), ("version", pa.int64()),
            ("lastUpdated", pa.int64()),
        ])),
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
    expect_eq(failures, "checkpoint-replay", "signed transaction versions after replay",
              summary["txn_app_versions"], {"kept": -1, "updated": 3})


def main() -> int:
    failures = Failures()
    out = Path(tempfile.mkdtemp(prefix="wireform-delta-probe-"))

    case_unpartitioned(failures, out / "table_unpart")
    case_partitioned (failures, out / "table_part")
    case_checkpointed(failures, out / "table_ckpt")
    case_checkpoint_replay(failures, out / "table_ckpt_replay")
    case_partitioned_checkpoint(failures, out / "table_part_ckpt")
    case_time_travel_and_history(failures, out / "table_tt")

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
