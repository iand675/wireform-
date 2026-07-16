# Changelog for wireform-derive

## 0.2.0.0 -- 2026-07-16

Breaking release from 0.1.0.0.

### Highlights

* `Wireform.Derive` -- TH driver and re-export surface.
* `Wireform.Derive.Modifier` -- shared annotation vocabulary
  (`Modifier`): `rename` / `renameStyle` / `renameWith` /
  `renameIdiomatic`, `tag N`, `skip`, `defaults`, `required` /
  `optional`, `coerced`, `flatten`, `wireOverride`, `mapKey`,
  `oneof`, `extension`.
* `Wireform.Derive.ModifierInfo` -- resolved modifier shape
  consumed by per-format derivers.  `ConflictX` errors surface
  modifier conflicts cleanly.
* `Wireform.Derive.NameStyle` -- name conversion helpers
  (`SnakeCase`, `CamelCase`, `KebabCase`, `Idiomatic`, …).
* `Wireform.Derive.Backend` -- backend tags
  (`backendJSON` / `backendProto` / `backendCBOR` / …) used by
  the per-backend overrides.
* `Wireform.Derive.Extension` -- `BackendModifier` typeclass for
  typed per-backend payloads (e.g. `XmlFieldOpt`,
  `HtmlFieldOpt`, `Asn1Tag`).
* `Wireform.Derive.TypeInfo` -- TH reification helpers for the
  per-format derivers.
* `Wireform.Derive.Aeson` -- canonical worked-example deriver
  (`deriveJSON`) that targets Aeson; per-format derivers in
  sibling packages mirror the same shape.
