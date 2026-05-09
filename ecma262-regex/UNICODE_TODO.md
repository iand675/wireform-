# Unicode Support TODO

## Current Status

The library currently operates in **byte mode** (cbuf_type=0), which treats input as individual bytes rather than UTF-8 characters. This limits Unicode support.

## Limitations

### What Works
- ASCII patterns and text
- Basic Unicode characters in patterns (compiled correctly)
- Unicode flag is recognized and passed to libregexp

### What Doesn't Work
- `.` with Unicode flag matches 1 byte instead of 1 Unicode character
- `\p{Emoji}`, `\p{Letter}`, etc. (Unicode property escapes) don't match
- Unicode-aware case-insensitive matching may be incorrect
- Character class ranges with Unicode don't work properly

## Required Changes

### 1. UTF-8 to UTF-16 Conversion

libregexp expects UTF-16 input when using cbuf_type=2. We need to:

```c
// In ecma262_regex_wrapper.c
- Convert UTF-8 input to UTF-16
- Use cbuf_type = 2 for UTF-16 mode
- Convert capture positions back to UTF-8 byte offsets
```

### 2. C Wrapper Changes

```c
int ecma262_regex_exec_utf8(ecma262_regex_t *regex,
                            const uint8_t *subject_utf8,
                            int start_index_utf8,
                            int subject_len_utf8,
                            uint8_t **captures_utf8) {
    // 1. Convert UTF-8 to UTF-16
    uint16_t *subject_utf16 = utf8_to_utf16(subject_utf8, subject_len_utf8, &len_utf16);

    // 2. Convert UTF-8 byte offset to UTF-16 code unit offset
    int start_index_utf16 = utf8_offset_to_utf16(subject_utf8, start_index_utf8);

    // 3. Execute with cbuf_type = 2 (UTF-16)
    uint16_t **captures_utf16 = allocate_capture_array(...);
    int result = lre_exec(captures_utf16, regex->bytecode, subject_utf16,
                         start_index_utf16, len_utf16, 2, NULL);

    // 4. Convert capture positions from UTF-16 offsets to UTF-8 byte offsets
    convert_captures_utf16_to_utf8(captures_utf16, captures_utf8, ...);

    // 5. Clean up
    free(subject_utf16);
    free(captures_utf16);

    return result;
}
```

### 3. Helper Functions Needed

```c
// Convert UTF-8 to UTF-16
uint16_t* utf8_to_utf16(const uint8_t *utf8, int len_utf8, int *len_utf16_out);

// Convert UTF-16 to UTF-8
uint8_t* utf16_to_utf8(const uint16_t *utf16, int len_utf16, int *len_utf8_out);

// Convert byte offset from UTF-8 to UTF-16 code unit offset
int utf8_offset_to_utf16(const uint8_t *utf8, int offset_utf8);

// Convert code unit offset from UTF-16 to UTF-8 byte offset
int utf16_offset_to_utf8(const uint8_t *utf8_base, const uint16_t *utf16, int offset_utf16);
```

### 4. Haskell API Changes

Minimal changes needed - the C wrapper handles conversion transparently:

```haskell
-- Internal.hs - No changes needed, wrapper handles UTF-16 conversion
execRegex :: RegexPtr -> BS.ByteString -> Int -> IO (Maybe (Int, Int, [(Int, Int)]))
-- Input and output remain UTF-8 byte offsets
```

## Implementation Strategy

### Option 1: Always Use UTF-16 Mode
- Convert all input to UTF-16 before calling libregexp
- Provides full ECMAScript compliance
- Performance overhead for UTF-8/UTF-16 conversion

### Option 2: Conditional UTF-16 Mode
- Use byte mode (cbuf_type=0) for ASCII-only patterns
- Use UTF-16 mode (cbuf_type=2) only when Unicode flag is set
- Better performance for ASCII use cases
- More complex implementation

### Option 3: Expose Both APIs
- `execRegexByte` - current byte mode (fast, ASCII-focused)
- `execRegexUnicode` - UTF-16 mode (full Unicode support)
- Let users choose based on their needs

## Recommended Approach

**Option 2 (Conditional UTF-16 Mode)** is recommended:

1. Check if Unicode flag is present in compiled regex
2. If yes, use UTF-16 conversion path
3. If no, use current byte mode (faster for ASCII)
4. This provides both performance and correctness

## Testing Requirements

Once implemented, add tests for:
- `.` matching multi-byte UTF-8 characters with Unicode flag
- `\p{Emoji}`, `\p{Letter}`, `\p{Number}` property escapes
- Case-insensitive matching with Unicode characters
- Surrogate pair handling (characters like 𝕳 U+1D573)
- Capture group positions with mixed ASCII/Unicode text

## Effort Estimate

- UTF-8/UTF-16 conversion functions: ~100 lines of C
- Modified exec wrapper: ~50 lines of C
- Offset conversion logic: ~50 lines of C
- Testing: ~20 new test cases
- **Total: ~2-3 hours of development**

## Priority

**Medium** - The library is functional for ASCII and basic Unicode patterns. Full Unicode support is needed for:
- Emoji matching
- International text processing
- Full ECMAScript 262 compliance

For many use cases (log parsing, ASCII protocols, etc.), the current byte mode is sufficient.
