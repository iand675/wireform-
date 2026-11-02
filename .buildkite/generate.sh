#!/usr/bin/env bash
set -euo pipefail

# Determine the base branch for change detection.
base="${BUILDKITE_PULL_REQUEST_BASE_BRANCH:-main}"

# On the main branch itself, diff against the parent commit so we
# still get targeted builds for each push.
if [ "${BUILDKITE_BRANCH:-}" = "main" ]; then
  changed=$(git diff --name-only -z HEAD~1...HEAD | jq -Rs 'split("\u0000")[:-1]')
else
  git fetch origin "$base" --depth=50 2>/dev/null || true
  changed=$(git diff --name-only -z "origin/${base}...HEAD" | jq -Rs 'split("\u0000")[:-1]')
fi

is_main="false"
if [ "${BUILDKITE_BRANCH:-}" = "main" ]; then
  is_main="true"
fi

echo "--- :mag: Changed files"
echo "$changed" | jq -r '.[]'

# Read JSON as data rather than interpolating filenames into Nix source.
changed_file=$(mktemp)
trap 'rm -f "$changed_file"' EXIT
printf '%s\n' "$changed" > "$changed_file"
export WIREFORM_CI_CHANGED_FILES="$changed_file"

echo "--- :test_tube: Checking pipeline regressions"
nix eval --impure --json --expr '
  import ./nix/ci/tests.nix {
    lib = (builtins.getFlake ("git+file://" + toString ./.)).inputs.nixpkgs.lib;
  }
'

eval_pipeline() {
  local pipeline_file="$1"
  local extra_args="$2"

  nix eval --impure --json --expr "
    let lib = (builtins.getFlake (\"git+file://\" + toString ./.)).inputs.nixpkgs.lib;
        files = builtins.fromJSON (builtins.readFile
          (builtins.getEnv \"WIREFORM_CI_CHANGED_FILES\"));
    in import ./${pipeline_file} ({
      inherit lib;
      changedFiles = files;
    } // ${extra_args})
  "
}

echo "--- :nix: Evaluating main pipeline"
eval_pipeline "nix/ci/pipelines/main.nix" "{ isMain = ${is_main}; }" > pipeline-main.json

echo "--- :nix: Evaluating interop pipeline"
eval_pipeline "nix/ci/pipelines/interop.nix" "{}" > pipeline-interop.json

echo "--- :pipeline: Uploading main pipeline"
buildkite-agent pipeline upload pipeline-main.json

echo "--- :pipeline: Uploading interop pipeline"
buildkite-agent pipeline upload pipeline-interop.json

echo "--- :white_check_mark: Pipeline generation complete"
echo "Main pipeline steps: $(jq '.steps | length' pipeline-main.json)"
echo "Interop pipeline steps: $(jq '.steps | length' pipeline-interop.json)"
