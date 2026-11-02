"""Exercise change-file transport and upload failures without a CI account.

Run with: python3 -m unittest discover -s .buildkite -p 'test_*.py'
"""

import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest


GENERATOR = Path(__file__).with_name("generate.sh").resolve()


class GenerateTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.bin = self.root / "bin"
        self.bin.mkdir()
        self.env = dict(os.environ, PATH=f"{self.bin}:{os.environ['PATH']}",
                        BUILDKITE_BRANCH="main", TEST_ROOT=str(self.root))
        self.git("init", "-q")
        self.git("config", "user.email", "test@example.invalid")
        self.git("config", "user.name", "Generator Test")
        self.git("commit", "--allow-empty", "-qm", "base")
        # Each of these was previously embedded into a Nix string literal.
        self.names = ["wireform-yaml/it's ''${not_nix}.hs", "wireform-yaml/unicode-λ.hs",
                      "wireform-yaml/new\nline.hs"]
        for name in self.names:
            path = self.root / name
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text("fixture")
        self.git("add", ".")
        self.git("commit", "-qm", "change")
        self.executable("nix", """#!/usr/bin/env python3
import json, os, pathlib, sys
root = pathlib.Path(os.environ['TEST_ROOT'])
files = pathlib.Path(os.environ['WIREFORM_CI_CHANGED_FILES']).read_text()
with (root / 'evaluations.jsonl').open('a') as output:
    output.write(json.dumps({'files': json.loads(files), 'args': sys.argv[1:]}) + '\\n')
if os.environ.get('FAIL_NIX') == 'yes':
    sys.exit(42)
print('{"steps": []}')
""")
        self.executable("buildkite-agent", """#!/usr/bin/env python3
import os, pathlib, sys
with (pathlib.Path(os.environ['TEST_ROOT']) / 'uploads').open('a') as output:
    output.write(sys.argv[-1] + '\\n')
""")

    def git(self, *args):
        subprocess.run(["git", "-c", "core.fsmonitor=false", *args], cwd=self.root,
                       check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    def executable(self, name, source):
        path = self.bin / name
        path.write_text(source)
        path.chmod(0o755)

    def run_generator(self):
        return subprocess.run(["bash", str(GENERATOR)], cwd=self.root, env=self.env,
                              stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)

    @unittest.skipUnless(shutil.which("jq"), "jq is required by the generator")
    def test_filenames_are_data_and_locked_nixpkgs_is_used(self):
        result = self.run_generator()
        self.assertEqual(result.returncode, 0, result.stderr)
        evaluations = (self.root / "evaluations.jsonl").read_text().splitlines()
        self.assertEqual(len(evaluations), 3)
        for line in evaluations:
            evaluation = json.loads(line)
            self.assertEqual(sorted(evaluation["files"]), sorted(self.names))
            expression = evaluation["args"][-1]
            self.assertIn('(builtins.getFlake ("git+file://" + toString ./.)).inputs.nixpkgs.lib', expression)
            for name in self.names:
                self.assertNotIn(name, expression)
        self.assertEqual((self.root / "uploads").read_text().splitlines(),
                         ["pipeline-main.json", "pipeline-interop.json"])

    @unittest.skipUnless(shutil.which("jq"), "jq is required by the generator")
    def test_failed_evaluation_never_uploads_a_partial_pipeline(self):
        self.env["FAIL_NIX"] = "yes"
        result = self.run_generator()
        self.assertEqual(result.returncode, 42)
        self.assertFalse((self.root / "uploads").exists())


if __name__ == "__main__":
    unittest.main()
