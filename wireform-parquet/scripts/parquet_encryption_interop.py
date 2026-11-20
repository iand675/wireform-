#!/usr/bin/env python3
"""Read Wireform's encrypted output with Apache Arrow's independent C++ reader.

Uses a deliberately public test key and a local mock KMS. Covers both page
versions, AES128/192/256, GCM/CTR, footer-only/selective column encryption,
and two row groups. Independent C++ metadata checks prove that requested
column encryption is present, in addition to checking decoded values.
No real KMS, account, or credentials are used.
"""

import argparse
import json
import subprocess
import tempfile
from pathlib import Path

import pyarrow as pa
import pyarrow.parquet as pq
import pyarrow.parquet.encryption as pe


class TestKms(pe.KmsClient):
    def unwrap_key(self, wrapped_key, master_key_identifier):
        widths = {"test-key": 16, "test-key-16": 16, "test-key-24": 24, "test-key-32": 32}
        if master_key_identifier != "wireform-test" or wrapped_key not in widths:
            raise ValueError("unexpected test key metadata")
        return bytes([0x42]) * widths[wrapped_key]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--probe", type=Path, required=True)
    parser.add_argument("--reference", type=Path, required=True,
                        help="C++ fixture oracle built by build_parquet_encrypted_reference.py")
    args = parser.parse_args()
    factory = pe.CryptoFactory(lambda config: TestKms())
    connection = pe.KmsConnectionConfig(kms_instance_id="wireform-test")
    properties = factory.file_decryption_properties(connection)
    schema = pa.schema([pa.field("x", pa.int32(), nullable=False), pa.field("y", pa.string(), nullable=False)])
    expected = pa.table({"x": [1, 2, 3, 4, 5], "y": ["a", "b", "c", "d", "e"]}, schema=schema)
    with tempfile.TemporaryDirectory(prefix="wireform-parquet-encryption-") as tmp:
        root = Path(tmp)
        for width in (16, 24, 32):
            directory = root / str(width)
            directory.mkdir()
            metadata = directory / "test-key.json"
            metadata.write_text(json.dumps({
                "keyMaterialType": "PKMT1", "internalStorage": True,
                "isFooterKey": True, "kmsInstanceID": "wireform-test", "kmsInstanceURL": "",
                "masterKeyID": "wireform-test", "wrappedDEK": f"test-key-{width}",
                "doubleWrapping": False,
            }))
            subprocess.run([str(args.probe.resolve()), "--encrypted", str(directory), str(metadata), str(width)], check=True)
        fixtures = sorted(root.rglob("*.parquet"))
        if len(fixtures) != 24:
            raise AssertionError(f"expected 24 encryption fixtures, got {len(fixtures)}")
        for path in fixtures:
            assert path.read_bytes()[:4] == b"PARE", path.name
            file = pq.ParquetFile(path, decryption_properties=properties)
            assert file.num_row_groups == 2, path.name
            table = file.read(use_threads=False)
            if not table.equals(expected, check_metadata=False):
                raise AssertionError(f"{path.name}: unexpected decoded values/types: {table}")
            # Matching values alone cannot prove requested column encryption:
            # a writer that ignores the option still round-trips through a
            # reader with the footer key. Inspect independent C++ crypto
            # metadata and read every page/index/bloom through that library.
            arrow_output = root / (path.name + ".arrow")
            reference = subprocess.run([str(args.reference.resolve()), str(path), str(arrow_output), "native-forward"],
                                       capture_output=True, text=True, check=True)
            header, *columns = map(json.loads, reference.stdout.splitlines())
            assert header["rowGroups"] == 2, path.name
            for column in columns:
                requested = "-column" in path.name and column["column"] == 0
                assert column["encrypted"] == requested, (path.name, column["column"], "column encryption option ignored")
                assert column["columnIndex"] and column["offsetPages"] == 1, path.name
                if column["column"] == 0:
                    assert column["bloomBytes"] > 0, path.name
            independent = pa.ipc.open_stream(arrow_output).read_all()
            assert independent.equals(expected, check_metadata=False), path.name
            # Corrupt the final footer authentication-tag byte. Even files
            # using CTR for pages must authenticate their footer with GCM.
            tampered = bytearray(path.read_bytes())
            tampered[-9] ^= 1
            try:
                pq.ParquetFile(pa.BufferReader(tampered), decryption_properties=properties)
            except (OSError, pa.ArrowException):
                pass
            else:
                raise AssertionError(f"{path.name}: accepted a tampered footer")
            print(f"PASS {path.name}")
        print(f"{len(fixtures)} encrypted files verified with PyArrow {pa.__version__}")


if __name__ == "__main__":
    main()
