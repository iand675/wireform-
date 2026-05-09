# Revision history for wireform-jsonschema

## 0.1.0.0 -- Unreleased

* Initial port from `fractal-openapi` (`Fractal.JsonSchema.*` →
  `JsonSchema.*`). JSON Schema draft-04 / 06 / 07 / 2019-09 / 2020-12
  parser, validator, renderer, dialect / vocabulary registry and
  embedded metaschemas.
* Adds `JsonSchema.Class`, `JsonSchema.CodeGen` and
  `JsonSchema.Derive` so the package fits the wireform per-format
  module shape (typeclass + schema-driven codegen + annotation-driven
  TH deriver).
