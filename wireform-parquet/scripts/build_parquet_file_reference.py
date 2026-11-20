#!/usr/bin/env python3
"""Build the pinned, unmodified upstream Arrow FILE interoperability reference.

Requires Python 3.12+, Cargo and Rust 1.88+. The small Rust command adapter and
generated dependency lock are shipped beside this script. No Rust dependency
is added to the production Haskell package.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tarfile
import tempfile
import urllib.request


REVISION = "cc58320c2da359b757814db5a9651397d829b601"
ARCHIVE_SHA256 = "8bf1f8eafe3651093c43dad7d905a725473b0a2928436e267526298fe987e21a"


def sha256(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument("--archive", type=Path, help="Use an existing source archive instead of downloading it")
    parser.add_argument("--cargo", default=os.environ.get("CARGO", "cargo"))
    parser.add_argument("--allow-network", action="store_true", help="Allow Cargo to fetch missing locked dependencies")
    args = parser.parse_args()
    if sys.version_info < (3, 12):
        parser.error("the reference builder requires Python 3.12 or newer")
    output = args.output.resolve()
    output.mkdir(parents=True, exist_ok=True)
    archive = args.archive.resolve() if args.archive else output / "arrow-rs.tar.gz"
    if not args.archive:
        urllib.request.urlretrieve(f"https://codeload.github.com/apache/arrow-rs/tar.gz/{REVISION}", archive)
    if sha256(archive) != ARCHIVE_SHA256:
        raise RuntimeError("upstream source archive hash differs from the pinned reference")
    # A fresh directory prevents stale or locally edited upstream sources from
    # influencing the reference; keep it for inspection after the build.
    checkout = Path(tempfile.mkdtemp(prefix="upstream-", dir=output))
    with tarfile.open(archive) as source:
        source.extractall(checkout, filter="data")
    upstream = checkout / ("arrow-rs-" + REVISION)
    project = output / "oracle"
    (project / "src").mkdir(parents=True, exist_ok=True)
    scripts = Path(__file__).resolve().parent
    shutil.copyfile(scripts / "parquet_file_reference.rs", project / "src/main.rs")
    shutil.copyfile(scripts / "parquet_file_reference.lock", project / "Cargo.lock")
    dependencies = {
        "parquet": "{ path = " + json.dumps(str(upstream / "parquet")) + ', default-features = false, features = ["arrow", "file_experimental", "snap", "zstd", "lz4"] }',
        "arrow-array": "{ path = " + json.dumps(str(upstream / "arrow-array")) + " }",
        "arrow-ipc": "{ path = " + json.dumps(str(upstream / "arrow-ipc")) + " }",
    }
    manifest = '[package]\nname = "wireform-parquet-file-reference"\nversion = "0.1.0"\nedition = "2024"\n\n[workspace]\n\n[dependencies]\n'
    manifest += "".join(name + " = " + value + "\n" for name, value in dependencies.items())
    (project / "Cargo.toml").write_text(manifest)
    command = [args.cargo, "build", "--release", "--locked", "--manifest-path", str(project / "Cargo.toml"), "--target-dir", str(output / "target")]
    if not args.allow_network:
        command.append("--offline")
    subprocess.run(command, check=True)
    binary = output / "target/release/wireform-parquet-file-reference"
    report = {"revision": REVISION, "archiveSHA256": ARCHIVE_SHA256, "upstreamSource": str(upstream),
              "adapterSHA256": sha256(project / "src/main.rs"), "lockSHA256": sha256(project / "Cargo.lock"),
              "binary": str(binary), "binarySHA256": sha256(binary), "features": ["arrow", "file_experimental", "snap", "zstd", "lz4"]}
    (output / "reference.json").write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(report, indent=2))


if __name__ == "__main__":
    main()
