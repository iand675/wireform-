# Pure pipeline regressions, evaluated before uploading either pipeline.
{ lib }:
let
  main = import ./pipelines/main.nix { inherit lib; changedFiles = [ "wireform-yaml/src/YAML.hs" ]; isMain = true; };
  interop = import ./pipelines/interop.nix { inherit lib; changedFiles = [ ".buildkite/pipeline.yml" ]; };
  commands = steps: lib.concatMap (step:
    if builtins.isAttrs step && step ? steps then commands step.steps
    else lib.optional (builtins.isAttrs step && step ? command) step
  ) steps;
  mainCommands = commands main.steps;
  allCommands = mainCommands ++ commands interop.steps;
  nixPlugin = "jcollie/install-nix#af1bcfc10d2a7c018b53908434439496ddf1ef6d";
  hasInstaller = step: builtins.any (p: builtins.hasAttr nixPlugin p) (step.plugins or []);
  yamlBuild = lib.findFirst (s: (s.key or "") == "build-wireform_yaml") null mainCommands;
in
assert yamlBuild != null;
assert yamlBuild.matrix.setup.ghc == [ "ghc910" "ghc912" "ghc914" ];
assert builtins.any (s: (s.key or "") == "test-wireform_yaml") mainCommands;
assert allCommands != [];
assert builtins.all hasInstaller allCommands;
{ pipelineRegressions = "passed"; }
