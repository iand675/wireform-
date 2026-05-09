# ecma262-regex

[![Haskell](https://img.shields.io/badge/language-Haskell-blue.svg)](https://www.haskell.org)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

ECMAScript 262 (JavaScript) regular expressions for Haskell.

## Overview

This package provides full ECMAScript 262 (JavaScript-compatible) regular expression support for Haskell. It uses FFI bindings to [QuickJS's libregexp](https://bellard.org/quickjs/), which is fully compliant with the ECMAScript 2023 specification.

## Features

- ✅ **Full ECMAScript 2023 compliance**: All modern JavaScript regex features
- 🚀 **High performance**: Native C implementation
- 🎯 **All regex flags**: `g`, `i`, `m`, `s`, `u`, `y`, `d`, `v`
- 📦 **Named capture groups**: `(?<name>pattern)`
- 🔍 **Lookahead/lookbehind**: `(?=...)`, `(?!...)`, `(?<=...)`, `(?<!...)`
- 🌍 **Unicode support**: `\p{Property}`, `\u{...}`, Unicode-aware matching
- 🔙 **Backreferences**: `\1`, `\2`, etc.
- 📝 **Simple, type-safe API**: Idiomatic Haskell interface

## Installation

Add to your `package.yaml`:

```yaml
dependencies:
  - ecma262-regex
```

Or to your `.cabal` file:

```cabal
build-depends:
  ecma262-regex
```

## Quick Start

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Text.Regex.ECMA262
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
  -- Compile a regex
  Right regex <- compile "hello" [IgnoreCase]

  -- Test if it matches
  result <- test regex "HELLO WORLD"
  print result  -- True

  -- Find a match with details
  Just match <- match regex "Say HELLO"
  print (matchText match)  -- "HELLO"
  print (matchStart match) -- 4
```

## Using with regex-base

This library supports the standard Haskell `regex-base` interface, allowing you to use the `=~` and `=~~` operators:

```haskell
import Text.Regex.ECMA262.Base

-- Test if a string matches
"hello world" =~ "world" :: Bool  -- True

-- Extract matched text
"hello world" =~ "w\\w+" :: String  -- "world"

-- Get all matches
"one 123 two 456" =~ "\\d+" :: [[String]]  -- [["123"],["456"]]

-- Extract capture groups
"Date: 2024-03-15" =~ "(\\d{4})-(\\d{2})-(\\d{2})" :: (String, String, String, [String])
-- ("Date: ","2024-03-15","",["2024","03","15"])
```

## Examples

### Basic Pattern Matching

```haskell
import Text.Regex.ECMA262

-- Case-insensitive matching
Right regex <- compile "hello" [IgnoreCase]
test regex "HELLO" >>= print  -- True
```

### Capture Groups

```haskell
-- Extract date components
Right regex <- compile "(\\d{4})-(\\d{2})-(\\d{2})" []
Just m <- match regex "Date: 2024-03-15"

let [(_, _, year), (_, _, month), (_, _, day)] = captures m
print year   -- "2024"
print month  -- "03"
print day    -- "15"
```

### Find All Matches

```haskell
-- Extract all words
Right regex <- compile "\\b\\w+\\b" []
matches <- matchAll regex "The quick brown fox"
mapM_ (print . matchText) matches
-- "The"
-- "quick"
-- "brown"
-- "fox"
```

### Unicode Support

The library provides full Unicode support when the `Unicode` flag is used:

```haskell
import qualified Data.Text.Encoding as TE

-- Match emoji with Unicode property escapes
Right regex <- compile "\\p{Emoji}+" [Unicode]
let text = TE.encodeUtf8 "Hello 😀🎉 World"
Just m <- match regex text
print (matchText m)  -- "\240\159\152\128\240\159\152\142" (UTF-8 encoded emoji)

-- Match Unicode characters with dot
Right regex2 <- compile "." [Unicode]
let emoji = TE.encodeUtf8 "😀"
Just m2 <- match regex2 emoji
print (matchText m2)  -- Matches the full 4-byte emoji, not just one byte

-- Use Unicode escapes in patterns
Right regex3 <- compile "\\u{1F600}+" [Unicode]  -- U+1F600 = 😀
let text3 = TE.encodeUtf8 "😀😀😀"
Just m3 <- match regex3 text3
print (matchText m3)  -- "\240\159\152\128\240\159\152\128\240\159\152\128"
```

**Important notes:**
- When using the `Unicode` flag, the library operates in UTF-16 mode internally for full ECMAScript compliance
- Input text should be properly UTF-8 encoded (use `Data.Text.Encoding.encodeUtf8`)
- For emoji or Unicode characters in patterns, use Unicode escapes (e.g., `\u{1F600}`) or `compileText`
- Without the `Unicode` flag, the library uses byte mode for better performance with ASCII text

### Named Capture Groups

```haskell
-- Parse email with named groups
Right regex <- compile "(?<user>[^@]+)@(?<domain>.+)" []
Just m <- match regex "test@example.com"
names <- getGroupNames regex
print names  -- Just "user,domain"
```

### Multiline Mode

```haskell
-- Match line starts
Right regex <- compile "^line" [Multiline]
test regex "first\nline" >>= print  -- True
```

## API Overview

### Compilation

```haskell
compile :: ByteString -> [RegexFlag] -> IO (Either String Regex)
compileText :: Text -> [RegexFlag] -> IO (Either String Regex)
```

### Matching

```haskell
test :: Regex -> ByteString -> IO Bool
match :: Regex -> ByteString -> IO (Maybe Match)
matchFrom :: Int -> Regex -> ByteString -> IO (Maybe Match)
matchAll :: Regex -> ByteString -> IO [Match]
```

### Match Results

```haskell
data Match = Match
  { matchStart :: Int
  , matchEnd :: Int
  , matchText :: ByteString
  , captures :: [(Int, Int, ByteString)]
  }
```

### Flags

```haskell
data RegexFlag
  = Global        -- g: global matching
  | IgnoreCase    -- i: case-insensitive
  | Multiline     -- m: ^ and $ match line boundaries
  | DotAll        -- s: . matches newline
  | Unicode       -- u: Unicode mode
  | Sticky        -- y: match from lastIndex only
  | Indices       -- d: generate match indices
  | UnicodeSets   -- v: Unicode sets mode
```

## Supported ECMAScript Features

- Character classes: `[abc]`, `[^abc]`, `[a-z]`
- Predefined classes: `\d`, `\D`, `\w`, `\W`, `\s`, `\S`, `.`
- Anchors: `^`, `$`, `\b`, `\B`
- Quantifiers: `*`, `+`, `?`, `{n}`, `{n,}`, `{n,m}`
- Lazy quantifiers: `*?`, `+?`, `??`, `{n,m}?`
- Groups: `(...)`, `(?:...)`, `(?<name>...)`
- Alternation: `|`
- Lookahead: `(?=...)`, `(?!...)`
- Lookbehind: `(?<=...)`, `(?<!...)`
- Backreferences: `\1`, `\2`, `\k<name>`
- Unicode: `\u{...}`, `\p{Property}`, `\P{Property}`
- Escapes: `\t`, `\n`, `\r`, `\f`, `\v`, `\0`, `\xHH`, `\uHHHH`

## Performance

The library uses QuickJS's libregexp, which features:

- Linear time execution for "simple" patterns (no backreferences or complex lookahead)
- Optimized bytecode compilation
- Efficient Unicode handling
- Lock-step execution mode

## Differences from Other Haskell Regex Libraries

| Feature | ecma262-regex | regex-pcre | regex-tdfa |
|---------|---------------|------------|------------|
| ECMAScript compatibility | ✅ Full | ❌ PCRE syntax | ❌ POSIX ERE |
| Unicode properties | ✅ Full | ⚠️ Limited | ❌ No |
| Lookahead/Lookbehind | ✅ Both | ✅ Both | ❌ Lookahead only |
| Named groups | ✅ Yes | ✅ Yes | ❌ No |
| Performance | Native C | Native C | Haskell |
| Dependencies | Self-contained | Requires libpcre | Pure Haskell |

## Building

```bash
# Build the library
cabal build ecma262-regex

# Run tests
cabal test ecma262-regex

# Run example
cabal run ecma262-regex-example
```

## License

This package is licensed under the MIT License.

The underlying C library (libregexp from QuickJS) is also licensed under the MIT License:
- Copyright (c) 2017-2018 Fabrice Bellard
- Copyright (c) 2018 Charlie Gordon

See the `cbits/` directory for the original source files and license headers.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## Credits

- **QuickJS libregexp**: Fabrice Bellard and Charlie Gordon
- **Haskell bindings**: Ian Duncan

## See Also

- [ECMAScript Language Specification](https://tc39.es/ecma262/)
- [QuickJS JavaScript Engine](https://bellard.org/quickjs/)
- [Regular Expressions on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
