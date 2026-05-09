# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.2.0] - 2025-11-21

### Fixed
- Case-insensitive A-label recognition: uppercase "XN--" prefix is now properly recognized as an A-label (RFC 5891 Section 5.1)
- Case-insensitive hyphen validation: the "xn--" check in hyphen position validation now handles mixed case input
- Lowercase normalization: all domain labels are now normalized to lowercase per RFC 5891/3490 (DNS labels are case-insensitive)
- Round-trip consistency: improved Punycode decoding to accept case-insensitive A-labels during validation

### Changed
- **Performance improvement**: case normalization now happens at label construction time (`mkLabel`) instead of during processing, reducing redundant `toLower` calls
- Updated documentation to reflect case-insensitive behavior and lowercase normalization

### Technical Details
- `mkLabel` now normalizes input to lowercase before determining label type
- `processLabelToASCII` simplified to remove redundant normalization
- `processLabelToUnicode` accepts uppercase A-labels and normalizes them during processing
- All outputs are guaranteed to be lowercase per DNS case-insensitivity requirements

## [0.1.1.0] - 2025-11-21

### Added
- Initial release with IDNA2008 support
- Punycode encoding/decoding (RFC 3492)
- Full IDNA2008 validation (RFC 5890-5893)
- Comprehensive test suite with RFC test vectors

[0.1.2.0]: https://github.com/iand675/fractal/compare/idn-v0.1.1.0...idn-v0.1.2.0
[0.1.1.0]: https://github.com/iand675/fractal/releases/tag/idn-v0.1.1.0
