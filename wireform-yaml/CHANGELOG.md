# Changelog

## 0.1.0.0

Initial experimental release.

- YAML 1.2 encoding and decoding for dynamic values, documents, and streams.
- Block and flow collections; plain, quoted, literal, and folded scalars;
  tags, anchors, aliases, and merge keys.
- UTF-8, UTF-16, and UTF-32 input decoding.
- Generic and Template Haskell derivation of `ToYAML` and `FromYAML`, with
  the shared `wireform-derive` annotation vocabulary.
- Annotated syntax trees and source-preserving scalar edits, configurable
  pretty printing, and an `aeson` bridge.
- Nesting and alias-expansion limits, duplicate-key rejection, regression
  and property tests, and an optional upstream `yaml-test-suite` harness.
- Criterion comparisons with `HsYAML` and libyaml, including literal and
  escaped-string decoding.
