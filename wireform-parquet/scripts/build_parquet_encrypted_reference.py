#!/usr/bin/env python3
"""Build the fixture oracle against PyArrow 26's unmodified C++ library.

Requires a C++20 compiler and the PyArrow 26 wheel. No library source is
modified or downloaded. The adapter uses public parquet-testing keys only.
"""

import argparse
import os
from pathlib import Path
import shutil
import subprocess
import sys

import pyarrow as pa


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    if pa.__version__.split(".")[0] != "26":
        raise SystemExit("this adapter requires PyArrow 26's C++ ABI")
    if sys.platform not in ("darwin", "linux"):
        raise SystemExit("the fixture adapter currently builds on macOS and Linux")
    compiler = os.environ.get("CXX") or shutil.which("clang++") or shutil.which("c++")
    if not compiler:
        raise SystemExit("a C++20 compiler is required")
    library = Path(pa.get_library_dirs()[0])
    suffix = ".2600.dylib" if sys.platform == "darwin" else ".so.2600"
    libraries = [library / ("lib" + name + suffix) for name in ("parquet", "arrow")]
    for path in libraries:
        if not path.is_file():
            raise SystemExit(f"missing PyArrow C++ library: {path}")
    output = args.output.resolve()
    output.parent.mkdir(parents=True, exist_ok=True)
    subprocess.run([
        compiler, "-std=c++20", "-O2", "-I", pa.get_include(),
        str(Path(__file__).with_name("parquet_encrypted_reference.cc")),
        *map(str, libraries), f"-Wl,-rpath,{library}", "-o", str(output),
    ], check=True)
    print(f"Built {output} against PyArrow {pa.__version__}")


if __name__ == "__main__":
    main()
