# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0.0] - 2025-11-19

### Added
- Initial release
- Full ECMAScript 2023 regex specification support via QuickJS libregexp
- FFI bindings to libregexp C library
- High-level Haskell API with `compile`, `match`, `matchAll`, `test` functions
- Support for all ECMAScript regex flags (g, i, m, s, u, y, d, v)
- Named capture groups support
- Unicode property escapes and full Unicode support
- Lookahead and lookbehind assertions
- Backreferences
- `regex-base` typeclass instances (RegexMaker, RegexLike, RegexContext)
- Support for standard `=~` and `=~~` operators via regex-base
- Comprehensive test suite with 40+ test cases
- Example program demonstrating common use cases
- Complete API documentation
