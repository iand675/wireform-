#!/usr/bin/env python3
"""Verify a Wireform encoding of test/fixtures/codec.json against official flatc.

Requires flatc, a C++17 compiler, and FlatBuffers headers. Nothing is generated
inside the repository. The flatc fixture is generated from its JSON afresh.
"""
import argparse
import json
from pathlib import Path
import shutil
import subprocess
import tempfile

ROOT = Path(__file__).resolve().parent.parent
FIXTURES = ROOT / 'wireform-flatbuffers/test/fixtures'


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--encoded', type=Path, help='existing Wireform buffer; otherwise build a focused Haskell probe')
    parser.add_argument('--builddir', help='Cabal build output directory')
    parser.add_argument('--project-file', help='focused Cabal project used to build wireform-flatbuffers')
    parser.add_argument('--flatc', default='flatc')
    parser.add_argument('--cxx', default='c++')
    parser.add_argument('--include', type=Path, help='directory containing flatbuffers/flatbuffers.h')
    args = parser.parse_args()
    flatc = Path(shutil.which(args.flatc) or args.flatc).resolve()
    include = args.include or flatc.parent.parent / 'include'
    with tempfile.TemporaryDirectory(prefix='wireform-flatc-') as tmp:
        tmp = Path(tmp)
        subprocess.run([str(flatc), '--cpp', '--binary', '--strict-json', '-o', str(tmp), str(FIXTURES / 'codec.fbs'), str(FIXTURES / 'codec.json')], check=True)
        source = tmp / 'verify.cpp'
        source.write_text('''#include "codec_generated.h"
#include <fstream>
#include <iterator>
#include <vector>
int main(int argc, char **argv) {
  if (argc != 2) return 2;
  std::ifstream in(argv[1], std::ios::binary);
  std::vector<char> bytes((std::istreambuf_iterator<char>(in)), {});
  flatbuffers::Verifier verifier(reinterpret_cast<const uint8_t *>(bytes.data()), bytes.size());
  return Conformance::VerifyRootBuffer(verifier) ? 0 : 1;
}
''')
        subprocess.run([args.cxx, '-std=c++17', '-O2', '-I', str(include), '-I', str(tmp), str(source), '-o', str(tmp / 'verify')], check=True)
        expected = tmp / 'codec.bin'
        actual = tmp / 'wireform.bin'
        if args.encoded:
            shutil.copyfile(args.encoded, actual)
        else:
            probe = tmp / 'Interop.hs'
            probe.write_text('''{-# LANGUAGE OverloadedStrings #-}
import Data.ByteString qualified as BS
import Data.Text.IO qualified as T
import FlatBuffers.Parser
import FlatBuffers.Encode
import FlatBuffers.Decode
import System.Environment
main = do
  [fbs, input, output] <- getArgs
  source <- T.readFile fbs
  bytes <- BS.readFile input
  case do
    schema <- parseFlatBuffers source
    value <- decode schema bytes
    encode schema value
    of
      Left err -> fail err
      Right result -> BS.writeFile output result
''')
            cabal = ['cabal', 'exec']
            if args.project_file:
                cabal += ['--project-file=' + args.project_file]
            if args.builddir:
                cabal += ['--builddir=' + args.builddir]
            subprocess.run(cabal + ['--', 'ghc', '-v0', '-XGHC2021', '-package', 'wireform-flatbuffers', '-outputdir', str(tmp), '-o', str(tmp / 'interop'), str(probe)], cwd=ROOT, check=True)
            subprocess.run([str(tmp / 'interop'), str(FIXTURES / 'codec.fbs'), str(expected), str(actual)], check=True)
        for binary in [expected, actual]:
            subprocess.run([str(tmp / 'verify'), str(binary)], check=True)
            subprocess.run([str(flatc), '--json', '--strict-json', '--defaults-json', '-o', str(tmp), str(FIXTURES / 'codec.fbs'), '--', str(binary)], check=True)
        if json.loads((tmp / 'codec.json').read_text()) != json.loads((tmp / 'wireform.json').read_text()):
            raise SystemExit('decoded JSON differs')
        print('Official C++ verifier and flatc JSON comparison passed in both directions.')


if __name__ == '__main__':
    main()
