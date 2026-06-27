# HTTP Headers Library Guide

## Project Overview

This is a comprehensive HTTP headers library written in Haskell that provides type-safe parsing and rendering of HTTP headers according to various RFC specifications. The library is designed with a focus on correctness, performance, and type safety.

## Core Architecture

The library is organized into several key components:

### 1. Header Field Names (`HeaderFieldName.hs`)

- Central registry of all IANA-registered HTTP header field names
- Provides type-safe constants for all standard headers
- Implements case-insensitive comparison and hashing
- Uses interning for efficient memory usage and comparisons
- Headers are categorized as:
  - Permanent
  - Provisional
  - Deprecated
  - Obsoleted

### 2. Parsing (`Parsing/Util.hs`)

The parsing module provides utilities for parsing HTTP header values according to RFC specifications:

- RFC 9110 token parsing
- RFC 8941 structured field value parsing
- Support for various data types:
  - Integers
  - Decimals
  - Strings
  - Tokens
  - Binary data
  - Booleans
- Parameter parsing with type safety
- Error handling with custom error types

### 3. Rendering (`Rendering/Util.hs`)

The rendering module handles the conversion of parsed header values back to their string representation:

- RFC 8941 structured field value rendering
- Support for all value types
- Parameter rendering with configurable empty value handling
- Header size management and splitting
- Efficient string building using `Mason.Builder`

### 4. Individual Header Modules

Each standard HTTP header has its own module implementing:

- Type-safe data structures for header values
- Parsing functions
- Rendering functions
- Validation logic
- Documentation with RFC references

## Key Design Patterns

1. **Type Safety**
   - Strong type system to prevent invalid header values
   - Newtype wrappers for semantic meaning
   - Type-level guarantees for parsing and rendering

2. **Performance**
   - Efficient string handling with `ShortText` and `ByteString`
   - Interning for header names
   - Zero-copy parsing where possible
   - Builder pattern for rendering

3. **RFC Compliance**
   - Strict adherence to RFC specifications
   - Comprehensive test coverage
   - Clear documentation of RFC references

## Usage Guidelines

### Working with Header Names

```haskell
-- Use predefined constants for standard headers
import Network.HTTP.Headers.HeaderFieldName

-- Example: Using Content-Type header
contentTypeHeader = hContentType

-- Creating custom headers (with validation)
customHeader = headerFieldName "X-Custom-Header"
```

### Parsing Headers

```haskell
import Network.HTTP.Headers.Parsing.Util

-- Parse structured field values
parseHeaderValue :: ByteString -> Either String HeaderValue

-- Parse with parameters
parseHeaderWithParams :: ByteString -> Either String (HeaderValue, Parameters)
```

### Rendering Headers

```haskell
import Network.HTTP.Headers.Rendering.Util

-- Render header values
renderHeaderValue :: HeaderValue -> Builder

-- Render with parameters
renderHeaderWithParams :: HeaderValue -> Parameters -> Builder
```

## Best Practices

1. **Header Name Usage**
   - Always use the predefined constants from `HeaderFieldName.hs`
   - Avoid string literals for header names
   - Use `headerFieldName` for custom headers

2. **Parsing**
   - Use the appropriate parser for the header type
   - Handle parsing errors explicitly
   - Validate values after parsing

3. **Rendering**
   - Use the builder pattern for efficient string construction
   - Consider header size limits
   - Handle parameter rendering appropriately

4. **Type Safety**
   - Use the most specific type possible
   - Avoid unsafe conversions
   - Leverage the type system for validation

## Common Patterns

1. **Header Module Structure**
   ```haskell
   module Network.HTTP.Headers.ExampleHeader where

   -- Type definition
   data ExampleHeader = ExampleHeader { ... }

   -- Parsing
   parseExampleHeader :: ParserT st e ExampleHeader

   -- Rendering
   renderExampleHeader :: ExampleHeader -> Builder
   ```

2. **Parameter Handling**
   ```haskell
   -- Parsing parameters
   parseParams :: ParserT st e [(ST.ShortText, Maybe ItemValue)]

   -- Rendering parameters
   renderParams :: [(ST.ShortText, Maybe ItemValue)] -> Builder
   ```

## Error Handling

The library uses a combination of:

1. **Parse Errors**
   - Custom error types for parsing failures
   - Detailed error messages
   - Type-safe error handling

2. **Validation Errors**
   - Runtime checks for invalid values
   - Clear error messages
   - Type-safe error handling

## Performance Considerations

1. **Memory Usage**
   - Use `ShortText` for small strings
   - Use `ByteString` for binary data
   - Leverage interning for header names

2. **String Handling**
   - Use builders for string construction
   - Avoid unnecessary string conversions
   - Use efficient string types

3. **Parsing**
   - Use zero-copy parsing where possible
   - Avoid unnecessary allocations
   - Use efficient data structures

## Testing

1. **Unit Tests**
   - Test parsing and rendering
   - Test edge cases
   - Test error conditions

2. **Property Tests**
   - Test round-trip properties
   - Test RFC compliance
   - Test performance properties

## Contributing

When adding new headers:

1. Add the header name to `HeaderFieldName.hs`
2. Create a new module for the header
3. Implement parsing and rendering
4. Add tests
5. Document the module to the standard below

### Module documentation standard

Every header module (`Network.HTTP.Headers.<Name>`) MUST open with a
module-level Haddock block (after any `{-# LANGUAGE #-}` pragmas, before
the `module … (` line) containing, in order:

1. **Explainer** — a one-to-three sentence plain-English description: what
   the header conveys, whether it is a request and/or response header, and
   when it is used. Write for an engineer who doesn't know this specific
   header.
2. **Spec link** — a markdown autolink to the defining spec, e.g.
   `Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-10.1.1>`. Use the
   precise RFC/section (or WHATWG/W3C/OASIS) URL. For de-facto, non-IANA
   headers, say so plainly and link the best available reference.
3. **`See also:`** — 2–6 Haddock module links to the most related headers,
   written as double-quoted module names, e.g.
   `See also: "Network.HTTP.Headers.Accept", "Network.HTTP.Headers.Vary".`
   Cross-link the families that belong together (content negotiation,
   conditionals/validators, range, auth, caching, CORS / cross-origin
   isolation, integrity digests, etc.).

A detailed `== Grammar` section is encouraged for non-trivial wire formats
but is optional. Keep the block tight — explainer + spec + See also is the
floor, not a place to pad.

## References

- [RFC 9110: HTTP Semantics](https://datatracker.ietf.org/doc/html/rfc9110)
- [RFC 8941: Structured Field Values for HTTP](https://datatracker.ietf.org/doc/html/rfc8941)
- [IANA HTTP Headers Registry](https://www.iana.org/assignments/message-headers/message-headers.xhtml)
