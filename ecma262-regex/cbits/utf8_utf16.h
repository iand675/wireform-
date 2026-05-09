/* UTF-8/UTF-16 conversion utilities for ecma262-regex
 * Copyright (c) 2025
 * SPDX-License-Identifier: MIT
 */

#ifndef UTF8_UTF16_H
#define UTF8_UTF16_H

#include <stdint.h>
#include <stddef.h>

/* Convert UTF-8 to UTF-16
 *
 * Parameters:
 *   utf8: Input UTF-8 string
 *   utf8_len: Length of UTF-8 string in bytes
 *   utf16_len_out: Output parameter for UTF-16 length in code units
 *
 * Returns: Allocated UTF-16 string (caller must free), or NULL on error
 */
uint16_t* utf8_to_utf16(const uint8_t *utf8, int utf8_len, int *utf16_len_out);

/* Convert UTF-8 byte offset to UTF-16 code unit offset
 *
 * Parameters:
 *   utf8: UTF-8 string base
 *   utf8_offset: Byte offset in UTF-8 string
 *
 * Returns: Corresponding code unit offset in UTF-16, or -1 on error
 */
int utf8_offset_to_utf16(const uint8_t *utf8, int utf8_offset);

/* Convert UTF-16 code unit offset to UTF-8 byte offset
 *
 * Parameters:
 *   utf8: UTF-8 string base
 *   utf8_len: Length of UTF-8 string in bytes
 *   utf16_offset: Code unit offset in UTF-16
 *
 * Returns: Corresponding byte offset in UTF-8, or -1 on error
 */
int utf16_offset_to_utf8(const uint8_t *utf8, int utf8_len, int utf16_offset);

#endif /* UTF8_UTF16_H */
