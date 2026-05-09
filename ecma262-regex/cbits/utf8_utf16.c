/* UTF-8/UTF-16 conversion utilities for ecma262-regex
 * Copyright (c) 2025
 * SPDX-License-Identifier: MIT
 */

#include "utf8_utf16.h"
#include <stdlib.h>
#include <string.h>

/* Decode one UTF-8 character and return the Unicode code point
 * Updates *pos to point after the decoded character
 * Returns the code point, or 0xFFFD (replacement character) on error
 */
static uint32_t utf8_decode(const uint8_t *utf8, int utf8_len, int *pos) {
    if (*pos >= utf8_len) return 0;

    uint8_t c = utf8[*pos];

    /* 1-byte: 0xxxxxxx */
    if ((c & 0x80) == 0) {
        (*pos)++;
        return c;
    }

    /* 2-byte: 110xxxxx 10xxxxxx */
    if ((c & 0xE0) == 0xC0) {
        if (*pos + 1 >= utf8_len) {
            (*pos)++;
            return 0xFFFD;
        }
        uint8_t c2 = utf8[*pos + 1];
        if ((c2 & 0xC0) != 0x80) {
            (*pos)++;
            return 0xFFFD;
        }
        *pos += 2;
        uint32_t codepoint = ((c & 0x1F) << 6) | (c2 & 0x3F);
        /* Check for overlong encoding */
        if (codepoint < 0x80) return 0xFFFD;
        return codepoint;
    }

    /* 3-byte: 1110xxxx 10xxxxxx 10xxxxxx */
    if ((c & 0xF0) == 0xE0) {
        if (*pos + 2 >= utf8_len) {
            (*pos)++;
            return 0xFFFD;
        }
        uint8_t c2 = utf8[*pos + 1];
        uint8_t c3 = utf8[*pos + 2];
        if ((c2 & 0xC0) != 0x80 || (c3 & 0xC0) != 0x80) {
            (*pos)++;
            return 0xFFFD;
        }
        *pos += 3;
        uint32_t codepoint = ((c & 0x0F) << 12) | ((c2 & 0x3F) << 6) | (c3 & 0x3F);
        /* Check for overlong encoding and surrogates */
        if (codepoint < 0x800 || (codepoint >= 0xD800 && codepoint <= 0xDFFF)) {
            return 0xFFFD;
        }
        return codepoint;
    }

    /* 4-byte: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx */
    if ((c & 0xF8) == 0xF0) {
        if (*pos + 3 >= utf8_len) {
            (*pos)++;
            return 0xFFFD;
        }
        uint8_t c2 = utf8[*pos + 1];
        uint8_t c3 = utf8[*pos + 2];
        uint8_t c4 = utf8[*pos + 3];
        if ((c2 & 0xC0) != 0x80 || (c3 & 0xC0) != 0x80 || (c4 & 0xC0) != 0x80) {
            (*pos)++;
            return 0xFFFD;
        }
        *pos += 4;
        uint32_t codepoint = ((c & 0x07) << 18) | ((c2 & 0x3F) << 12) |
                             ((c3 & 0x3F) << 6) | (c4 & 0x3F);
        /* Check for overlong encoding and valid range */
        if (codepoint < 0x10000 || codepoint > 0x10FFFF) {
            return 0xFFFD;
        }
        return codepoint;
    }

    /* Invalid start byte */
    (*pos)++;
    return 0xFFFD;
}

uint16_t* utf8_to_utf16(const uint8_t *utf8, int utf8_len, int *utf16_len_out) {
    if (!utf8 || utf8_len < 0 || !utf16_len_out) {
        return NULL;
    }

    /* First pass: count UTF-16 code units needed */
    int utf16_len = 0;
    int pos = 0;
    while (pos < utf8_len) {
        uint32_t codepoint = utf8_decode(utf8, utf8_len, &pos);
        if (codepoint <= 0xFFFF) {
            utf16_len += 1;
        } else {
            utf16_len += 2; /* Surrogate pair */
        }
    }

    /* Allocate UTF-16 buffer with space for null terminator */
    uint16_t *utf16 = (uint16_t*)malloc((utf16_len + 1) * sizeof(uint16_t));
    if (!utf16) {
        return NULL;
    }

    /* Second pass: convert to UTF-16 */
    pos = 0;
    int out_pos = 0;
    while (pos < utf8_len) {
        uint32_t codepoint = utf8_decode(utf8, utf8_len, &pos);

        if (codepoint <= 0xFFFF) {
            /* BMP character - single UTF-16 code unit */
            utf16[out_pos++] = (uint16_t)codepoint;
        } else {
            /* Supplementary character - surrogate pair */
            codepoint -= 0x10000;
            utf16[out_pos++] = 0xD800 + (uint16_t)(codepoint >> 10);
            utf16[out_pos++] = 0xDC00 + (uint16_t)(codepoint & 0x3FF);
        }
    }

    /* Null-terminate the UTF-16 string */
    utf16[out_pos] = 0;

    *utf16_len_out = utf16_len;
    return utf16;
}

int utf8_offset_to_utf16(const uint8_t *utf8, int utf8_offset) {
    if (!utf8 || utf8_offset < 0) {
        return -1;
    }

    int utf16_offset = 0;
    int pos = 0;

    while (pos < utf8_offset) {
        int old_pos = pos;
        uint32_t codepoint = utf8_decode(utf8, utf8_offset, &pos);

        /* If we couldn't decode, we've gone past the end */
        if (pos == old_pos) {
            break;
        }

        if (codepoint <= 0xFFFF) {
            utf16_offset += 1;
        } else {
            utf16_offset += 2; /* Surrogate pair */
        }
    }

    return utf16_offset;
}

int utf16_offset_to_utf8(const uint8_t *utf8, int utf8_len, int utf16_offset) {
    if (!utf8 || utf8_len < 0 || utf16_offset < 0) {
        return -1;
    }

    int utf16_pos = 0;
    int utf8_pos = 0;

    while (utf8_pos < utf8_len && utf16_pos < utf16_offset) {
        int old_pos = utf8_pos;
        uint32_t codepoint = utf8_decode(utf8, utf8_len, &utf8_pos);

        /* If we couldn't decode, we've gone past the end */
        if (utf8_pos == old_pos) {
            break;
        }

        if (codepoint <= 0xFFFF) {
            utf16_pos += 1;
        } else {
            utf16_pos += 2; /* Surrogate pair */
        }
    }

    return utf8_pos;
}
