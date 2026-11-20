#!/usr/bin/env python3
"""Authenticate all official encrypted-file footers against Arrow C++.

The reference reads every physical value, definition/repetition level, index,
and bloom filter. The native check covers FOOTERS ONLY; a passing result does
not claim native encrypted-column or encrypted-index support. Fixture keys are
public test data. External key material is resolved by Arrow's local mock KMS.
"""

import argparse
import hashlib
import json
from pathlib import Path
import shutil
import subprocess
import tempfile

import pyarrow as pa


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def execute(command, *, success=True):
    result = subprocess.run(list(map(str, command)), capture_output=True, text=True, timeout=60)
    if success != (result.returncode == 0):
        raise AssertionError(f"unexpected exit {result.returncode}: {command[0]}: {result.stderr}")
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--reference", type=Path, required=True)
    parser.add_argument("--reader", type=Path, required=True)
    parser.add_argument("--corpus", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    reference, reader = args.reference.resolve(), args.reader.resolve()
    root = args.corpus.resolve() / "data"
    fixtures = sorted(root.rglob("*.parquet.encrypted"))
    if len(fixtures) < 14:
        raise AssertionError(f"incomplete encrypted corpus: {len(fixtures)} fixtures, expected at least 14")
    results = []
    args.output.parent.mkdir(parents=True, exist_ok=True)
    oracle_dir = args.output.with_suffix(".reference")
    oracle_dir.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix="wireform-encrypted-footers-") as tmp:
        for fixture in fixtures:
            name = fixture.relative_to(root)
            decoded = execute([reference, fixture]).stdout
            output = oracle_dir / Path(str(name) + ".jsonl")
            output.parent.mkdir(parents=True, exist_ok=True)
            output.write_text(decoded)
            header, *columns = map(json.loads, decoded.splitlines())
            assert len(columns) == header["rowGroups"] * header["columns"], name
            for column in columns:
                assert len(column["definitions"]) == len(column["repetitions"]) == column["events"], name
                assert len(column["values"]) == column["present"], name
            key = header["publicFixtureFooterKeyHex"]
            prefix = "746573746572" if header["prefixRequired"] else header["storedPrefixHex"]

            def native(path=fixture, test_key=key, test_prefix=prefix, success=True):
                return execute([reader, "--authenticate-footer", path, test_key, test_prefix], success=success)

            metadata = json.loads(native().stdout)
            assert metadata["rows"] == header["rows"], name
            assert metadata["rowGroups"] == header["rowGroups"], name
            assert metadata["columns"] == [header["columns"]] * header["rowGroups"], name
            native(test_key="00" * len(bytes.fromhex(key)), success=False)
            native(test_prefix="6f74686572", success=False)
            native(test_prefix="-", success=not header["prefixRequired"])
            tampered = bytearray(fixture.read_bytes())
            tampered[-9] ^= 1
            changed = Path(tmp) / name
            changed.parent.mkdir(parents=True, exist_ok=True)
            changed.write_bytes(tampered)
            sidecar = fixture.with_name("_KEY_MATERIAL_FOR_" + fixture.name + ".json")
            if sidecar.exists():
                shutil.copy2(sidecar, changed.with_name(sidecar.name))
            native(path=changed, success=False)
            execute([reference, changed], success=False)
            results.append({
                "fixture": str(name), "sha256": digest(fixture),
                "rows": header["rows"], "rowGroups": header["rowGroups"], "columns": header["columns"],
                "nativeFooterMatchesReference": True, "tamperedFooterRejectedByBoth": True,
                "wrongKeyRejected": True, "wrongPrefixRejected": True,
                "omittedPrefixRequired": header["prefixRequired"],
                "referenceColumnIndexes": sum(c["columnIndex"] for c in columns),
                "referenceOffsetIndexes": sum(c["offsetPages"] > 0 for c in columns),
                "referenceBlooms": sum(c["bloomBytes"] > 0 for c in columns),
                "referenceOutputSHA256": digest(output),
            })
            print(f"PASS footer {name}", flush=True)
    args.output.write_text(json.dumps({
        "scope": "Native footer authentication only; C++ independently reads complete files",
        "fullNativeEncryptedReadsValidated": False, "pyarrowVersion": pa.__version__,
        "referenceBinarySHA256": digest(reference), "readerBinarySHA256": digest(reader),
        "referenceSourceSHA256": digest(Path(__file__).with_name("parquet_encrypted_reference.cc")),
        "fixtures": results,
    }, indent=2) + "\n")
    print(f"All {len(results)} encrypted footers authenticated; native encrypted-column reads remain unvalidated")


if __name__ == "__main__":
    main()
