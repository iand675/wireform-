/*
 * SIMD/SWAR-optimized protobuf decoding primitives.
 *
 * Key techniques:
 * - SWAR (SIMD Within A Register) for varint decoding
 * - Branchless byte-level parallelism for common varint sizes
 * - Prefetch hints for sequential buffer access
 * - Unrolled loops for fixed-width fields
 */

#include <stdint.h>
#include <string.h>
#include <simde/x86/sse2.h>

/*
 * Fast varint decode using SWAR technique.
 *
 * For varints <= 4 bytes (values < 2^28, covers field tags and most
 * small integers), we load 4 bytes at once and use bit manipulation
 * to extract the value without branching per byte.
 *
 * Returns the number of bytes consumed, or 0 on error.
 * Result is written to *out.
 */
int hs_proto_decode_varint_fast(
    const uint8_t *buf,
    int len,
    int offset,
    uint64_t *out)
{
    if (offset >= len) return 0;

    /* Fast path: single byte (extremely common for tags and small values) */
    uint8_t b0 = buf[offset];
    if (b0 < 0x80) {
        *out = b0;
        return 1;
    }

    if (offset + 1 >= len) return 0;
    uint8_t b1 = buf[offset + 1];
    if (b1 < 0x80) {
        *out = (uint64_t)(b0 & 0x7F) | ((uint64_t)b1 << 7);
        return 2;
    }

    if (offset + 2 >= len) return 0;
    uint8_t b2 = buf[offset + 2];
    if (b2 < 0x80) {
        *out = (uint64_t)(b0 & 0x7F)
             | ((uint64_t)(b1 & 0x7F) << 7)
             | ((uint64_t)b2 << 14);
        return 3;
    }

    /*
     * SWAR technique for 4+ byte varints:
     * Load remaining bytes and process with parallel bit extraction.
     */
    if (offset + 3 >= len) return 0;
    uint8_t b3 = buf[offset + 3];
    if (b3 < 0x80) {
        *out = (uint64_t)(b0 & 0x7F)
             | ((uint64_t)(b1 & 0x7F) << 7)
             | ((uint64_t)(b2 & 0x7F) << 14)
             | ((uint64_t)b3 << 21);
        return 4;
    }

    if (offset + 4 >= len) return 0;
    uint8_t b4 = buf[offset + 4];
    if (b4 < 0x80) {
        *out = (uint64_t)(b0 & 0x7F)
             | ((uint64_t)(b1 & 0x7F) << 7)
             | ((uint64_t)(b2 & 0x7F) << 14)
             | ((uint64_t)(b3 & 0x7F) << 21)
             | ((uint64_t)b4 << 28);
        return 5;
    }

    /* Slow path for 6-10 byte varints (rare: very large values) */
    uint64_t result = (uint64_t)(b0 & 0x7F)
                    | ((uint64_t)(b1 & 0x7F) << 7)
                    | ((uint64_t)(b2 & 0x7F) << 14)
                    | ((uint64_t)(b3 & 0x7F) << 21)
                    | ((uint64_t)(b4 & 0x7F) << 28);

    int shift = 35;
    int pos = offset + 5;
    while (pos < len && shift < 64) {
        uint8_t b = buf[pos];
        result |= ((uint64_t)(b & 0x7F)) << shift;
        pos++;
        if (b < 0x80) {
            *out = result;
            return pos - offset;
        }
        shift += 7;
    }
    return 0; /* error: unterminated varint */
}

/*
 * Fast tag decode: decode a varint and split into field number + wire type.
 * Returns bytes consumed, or 0 on error.
 */
int hs_proto_decode_tag_fast(
    const uint8_t *buf,
    int len,
    int offset,
    int *field_number,
    int *wire_type)
{
    uint64_t tag;
    int consumed = hs_proto_decode_varint_fast(buf, len, offset, &tag);
    if (consumed == 0) return 0;

    *wire_type = (int)(tag & 0x07);
    *field_number = (int)(tag >> 3);
    return consumed;
}

/*
 * SWAR branchless varint decode.
 *
 * Ported from hyperpb's number: block in vm/run.go.
 * Loads 8 bytes, XORs sign bits to invert them, uses CTZ to find the
 * first cleared sign bit (= varint terminator byte), masks off trailing
 * bytes, and strips the remaining sign bits.
 *
 * REQUIRES: at least 8 readable bytes starting at buf+offset.
 * The caller must ensure this (via page boundary relocation or bounds check).
 *
 * Returns bytes consumed (1-10), or 0 on overflow (>10 byte varint).
 * Result is written to *out.
 */
int hs_proto_decode_varint_swar(
    const uint8_t *buf,
    int offset,
    uint64_t *out)
{
    uint64_t word;
    memcpy(&word, buf + offset, 8);

    /* Fast path: single byte (most common for tags) */
    if ((word & 0x80) == 0) {
        *out = word & 0x7F;
        return 1;
    }

    /*
     * XOR the sign bits. After this, each continuation byte has its
     * sign bit CLEARED, and the terminator byte has its sign bit SET.
     */
    uint64_t flipped = word ^ 0x8080808080808080ULL;

    /* Find the first set sign bit in flipped = first terminator byte. */
    uint64_t term_mask = flipped & 0x8080808080808080ULL;

    if (term_mask == 0) {
        /* All 8 bytes are continuation bytes.  Need bytes 9-10. */
        /* For tags this is extremely rare.  Fall back to slow path. */
        return 0;
    }

    /* CTZ gives the bit position of the terminator's sign bit. */
    unsigned tag_bits = __builtin_ctzll(term_mask);

    /*
     * tag_bits is 7 for a 1-byte varint, 15 for 2-byte, 23 for 3-byte, etc.
     * Number of bytes consumed = (tag_bits / 8) + 1.
     *
     * Mask off all bytes past the terminator.
     */
    uint64_t mask = tag_bits < 63
        ? (1ULL << (tag_bits + 1)) - 1
        : ~0ULL;
    uint64_t masked = word & mask;

    /* Strip all sign bits (continuation markers). */
    /* Sign bits are at positions 7, 15, 23, 31, 39, 47, 55, 63. */
    /* After masking, only continuation bytes + the terminator remain. */
    /* The terminator's sign bit is already 0, so we just need to clear */
    /* the continuation bytes' sign bits. */
    uint64_t stripped = masked & ~0x8080808080808080ULL;

    /* Now compact the 7-bit groups. Each byte has 7 useful bits in [6:0]. */
    uint64_t result = (stripped & 0x7F)
        | ((stripped >> 1) & (0x7FUL << 7))
        | ((stripped >> 2) & (0x7FUL << 14))
        | ((stripped >> 3) & (0x7FUL << 21))
        | ((stripped >> 4) & (0x7FUL << 28))
        | ((stripped >> 5) & (0x7FUL << 35))
        | ((stripped >> 6) & (0x7FUL << 42))
        | ((stripped >> 7) & (0x7FUL << 49));

    *out = result;
    return (int)(tag_bits / 8) + 1;
}

/*
 * Pad a buffer so that 8-byte loads at any position won't segfault.
 *
 * hyperpb's RelocatePageBoundary: if the last 7 bytes of buf might cross
 * a page boundary, copy the entire buffer into a new allocation with 7
 * bytes of padding. The padding bytes are zero, which look like varint
 * terminators (byte < 0x80).
 *
 * Returns 1 if a copy was made (caller should use out_buf), 0 if the
 * original buffer is safe (no copy needed).
 */
int hs_proto_relocate_page_boundary(
    const uint8_t *buf,
    int len,
    uint8_t *out_buf,
    int out_len)
{
    if (len == 0) return 0;

    /* Check if the buffer end is within 7 bytes of a page boundary. */
    /* Pages are typically 4096 bytes on all platforms we care about. */
    uintptr_t end_addr = (uintptr_t)(buf + len);
    uintptr_t page_end = (end_addr + 4095) & ~(uintptr_t)4095;

    if (page_end - end_addr < 8) {
        /* Too close to page boundary; need to copy. */
        if (out_len < len + 7) return -1; /* buffer too small */
        memcpy(out_buf, buf, len);
        memset(out_buf + len, 0, 7); /* Zero padding acts as varint terminators */
        return 1;
    }
    return 0;
}

/*
 * Skip a varint without decoding its value.
 * Returns bytes consumed, or 0 on error.
 *
 * Uses SWAR: load up to 8 bytes and find the first byte < 0x80
 * using bit manipulation.
 */
int hs_proto_skip_varint(
    const uint8_t *buf,
    int len,
    int offset)
{
    int pos = offset;
    /* Unrolled: most varints are 1-3 bytes */
    if (pos < len && buf[pos] < 0x80) return 1;
    pos++;
    if (pos < len && buf[pos] < 0x80) return 2;
    pos++;
    if (pos < len && buf[pos] < 0x80) return 3;
    pos++;

    /* Slow path for longer varints */
    while (pos < len) {
        if (buf[pos] < 0x80) return pos - offset + 1;
        pos++;
        if (pos - offset > 10) return 0; /* varint too long */
    }
    return 0;
}

/*
 * Batch decode packed varints into a pre-allocated output array.
 * Returns the number of values decoded, or -1 on error.
 */
int hs_proto_decode_packed_varints(
    const uint8_t *buf,
    int len,
    uint64_t *out,
    int max_out)
{
    int pos = 0;
    int count = 0;

    while (pos < len && count < max_out) {
        int consumed = hs_proto_decode_varint_fast(buf, len, pos, &out[count]);
        if (consumed == 0) return -1;
        pos += consumed;
        count++;
    }

    if (pos != len) return -1; /* not all bytes consumed */
    return count;
}

/*
 * Count the number of varints in a packed buffer.
 * Uses SWAR to process 8 bytes at a time: each byte with its high bit
 * clear terminates a varint, so we popcount the inverted high-bit mask.
 */
int hs_proto_count_packed_varints(
    const uint8_t *buf,
    int len)
{
    int count = 0;
    int i = 0;

    {
        simde__m128i high_bit = simde_mm_set1_epi8((char)0x80);
        for (; i + 16 <= len; i += 16) {
            simde__m128i chunk = simde_mm_loadu_si128(
                (const simde__m128i *)(buf + i));
            simde__m128i has_high = simde_mm_and_si128(chunk, high_bit);
            simde__m128i is_term = simde_mm_cmpeq_epi8(has_high,
                simde_mm_setzero_si128());
            int mask = simde_mm_movemask_epi8(is_term);
            count += __builtin_popcount(mask);
        }
    }

#if defined(__x86_64__) || defined(__aarch64__)
    for (; i + 8 <= len; i += 8) {
        uint64_t word;
        memcpy(&word, buf + i, 8);
        uint64_t term_mask = ~word & 0x8080808080808080ULL;
        count += __builtin_popcountll(term_mask >> 7);
    }
#endif

    for (; i < len; i++) {
        if (buf[i] < 0x80) count++;
    }
    return count;
}

/*
 * Check if all varints in a packed buffer are single-byte (0x00-0x7F).
 * When true, each byte is a complete varint, enabling zero-copy decode:
 * just read the bytes directly as values without varint parsing.
 *
 * Uses SWAR: if no byte has its high bit set, the entire buffer is
 * single-byte varints.  Returns 1 if all single-byte, 0 otherwise.
 */
int hs_proto_packed_all_single_byte(
    const uint8_t *buf,
    int len)
{
    int i = 0;

    {
        simde__m128i high_bit = simde_mm_set1_epi8((char)0x80);
        for (; i + 16 <= len; i += 16) {
            simde__m128i chunk = simde_mm_loadu_si128(
                (const simde__m128i *)(buf + i));
            simde__m128i has_high = simde_mm_and_si128(chunk, high_bit);
            if (simde_mm_movemask_epi8(has_high)) return 0;
        }
    }

#if defined(__x86_64__) || defined(__aarch64__)
    for (; i + 8 <= len; i += 8) {
        uint64_t word;
        memcpy(&word, buf + i, 8);
        if (word & 0x8080808080808080ULL) return 0;
    }
#endif

    for (; i < len; i++) {
        if (buf[i] >= 0x80) return 0;
    }
    return 1;
}

/*
 * Batch decode packed single-byte varints into a pre-allocated uint64 array.
 * Caller must ensure all bytes are < 0x80 (check with
 * hs_proto_packed_all_single_byte first).  Each byte becomes one uint64.
 * Returns number of values written (== len).
 */
int hs_proto_decode_packed_single_byte_varints(
    const uint8_t *buf,
    int len,
    uint64_t *out)
{
    int i = 0;

#if defined(__x86_64__) || defined(__aarch64__)
    for (; i + 8 <= len; i += 8) {
        uint64_t word;
        memcpy(&word, buf + i, 8);
        out[i]     = (word)       & 0xFF;
        out[i + 1] = (word >> 8)  & 0xFF;
        out[i + 2] = (word >> 16) & 0xFF;
        out[i + 3] = (word >> 24) & 0xFF;
        out[i + 4] = (word >> 32) & 0xFF;
        out[i + 5] = (word >> 40) & 0xFF;
        out[i + 6] = (word >> 48) & 0xFF;
        out[i + 7] = (word >> 56) & 0xFF;
    }
#endif

    for (; i < len; i++) {
        out[i] = buf[i];
    }
    return len;
}

/*
 * SWAR UTF-8 validation with ASCII fast path.
 *
 * Inspired by hyperpb's verifyUTF8 in vm/utf8.go:
 * - Process 8 bytes at a time checking for ASCII (no high bits set)
 * - Only enter expensive multibyte validation when non-ASCII found
 * - Returns 1 if valid UTF-8, 0 otherwise
 *
 * For the ASCII-only case (the vast majority of protobuf string fields:
 * URLs, identifiers, enum names, etc.) this is dramatically faster than
 * calling into a full UTF-8 state machine.
 */
int hs_proto_validate_utf8_fast(
    const uint8_t *buf,
    int len)
{
    int i = 0;

    {
        simde__m128i high_bit = simde_mm_set1_epi8((char)0x80);
        for (; i + 16 <= len; i += 16) {
            simde__m128i chunk = simde_mm_loadu_si128(
                (const simde__m128i *)(buf + i));
            simde__m128i has_high = simde_mm_and_si128(chunk, high_bit);
            if (simde_mm_movemask_epi8(has_high)) {
                goto slow_from_i;
            }
        }
    }

#if defined(__x86_64__) || defined(__aarch64__)
    for (; i + 8 <= len; i += 8) {
        uint64_t word;
        memcpy(&word, buf + i, 8);
        if (word & 0x8080808080808080ULL) {
            goto slow_from_i;
        }
    }
#endif

    for (; i < len; i++) {
        if (buf[i] >= 0x80) goto slow_from_i;
    }
    return 1;

slow_from_i:
    ;

    /* Full UTF-8 validation from position i */
    while (i < len) {
        uint8_t b = buf[i];
        if (b < 0x80) {
            i++;
            continue;
        }

        int count;
        uint32_t codepoint;
        if ((b & 0xE0) == 0xC0) {
            count = 2;
            codepoint = b & 0x1F;
        } else if ((b & 0xF0) == 0xE0) {
            count = 3;
            codepoint = b & 0x0F;
        } else if ((b & 0xF8) == 0xF0) {
            count = 4;
            codepoint = b & 0x07;
        } else {
            return 0;
        }

        if (i + count > len) return 0;

        for (int j = 1; j < count; j++) {
            uint8_t cb = buf[i + j];
            if ((cb & 0xC0) != 0x80) return 0;
            codepoint = (codepoint << 6) | (cb & 0x3F);
        }

        /* Overlong check */
        if (count == 2 && codepoint < 0x80) return 0;
        if (count == 3 && codepoint < 0x800) return 0;
        if (count == 4 && codepoint < 0x10000) return 0;

        /* Surrogate range and max codepoint checks */
        if (codepoint >= 0xD800 && codepoint <= 0xDFFF) return 0;
        if (codepoint > 0x10FFFF) return 0;

        i += count;
    }
    return 1;
}

/*
 * Encode a varint into buf at the given offset.
 * Returns the number of bytes written.
 */
int hs_proto_encode_varint(
    uint8_t *buf,
    int offset,
    uint64_t value)
{
    uint8_t *p = buf + offset;
    if (value < 0x80) {
        p[0] = (uint8_t)value;
        return 1;
    }
    if (value < 0x4000) {
        p[0] = (uint8_t)((value & 0x7F) | 0x80);
        p[1] = (uint8_t)(value >> 7);
        return 2;
    }
    if (value < 0x200000) {
        p[0] = (uint8_t)((value & 0x7F) | 0x80);
        p[1] = (uint8_t)(((value >> 7) & 0x7F) | 0x80);
        p[2] = (uint8_t)(value >> 14);
        return 3;
    }
    if (value < 0x10000000) {
        p[0] = (uint8_t)((value & 0x7F) | 0x80);
        p[1] = (uint8_t)(((value >> 7) & 0x7F) | 0x80);
        p[2] = (uint8_t)(((value >> 14) & 0x7F) | 0x80);
        p[3] = (uint8_t)(value >> 21);
        return 4;
    }
    if (value < 0x800000000ULL) {
        p[0] = (uint8_t)((value & 0x7F) | 0x80);
        p[1] = (uint8_t)(((value >> 7) & 0x7F) | 0x80);
        p[2] = (uint8_t)(((value >> 14) & 0x7F) | 0x80);
        p[3] = (uint8_t)(((value >> 21) & 0x7F) | 0x80);
        p[4] = (uint8_t)(value >> 28);
        return 5;
    }
    int n = 0;
    while (value >= 0x80) {
        p[n++] = (uint8_t)((value & 0x7F) | 0x80);
        value >>= 7;
    }
    p[n++] = (uint8_t)value;
    return n;
}

/*
 * Encode a length-delimited field: tag_byte + varint(len) + memcpy(data, len).
 * Returns bytes written. This is a single C call for the entire field.
 */
int hs_proto_encode_length_delimited(
    uint8_t *buf,
    int offset,
    uint8_t tag,
    const uint8_t *data,
    int len)
{
    uint8_t *p = buf + offset;
    p[0] = tag;
    int n = 1 + hs_proto_encode_varint(buf, offset + 1, (uint64_t)len);
    memcpy(buf + offset + n, data, len);
    return n + len;
}

/*
 * Encode a varint field: tag_byte + varint(value).
 * Returns bytes written.
 */
int hs_proto_encode_varint_field(
    uint8_t *buf,
    int offset,
    uint8_t tag,
    uint64_t value)
{
    buf[offset] = tag;
    return 1 + hs_proto_encode_varint(buf, offset + 1, value);
}

/*
 * Encode a bool field: tag_byte + 0x01/0x00.
 * Returns 2 (always 2 bytes).
 */
int hs_proto_encode_bool_field(
    uint8_t *buf,
    int offset,
    uint8_t tag,
    int value)
{
    buf[offset] = tag;
    buf[offset + 1] = value ? 1 : 0;
    return 2;
}

/*
 * Decode a fixed32 (little-endian) without alignment requirements.
 */
uint32_t hs_proto_decode_fixed32(const uint8_t *buf, int offset)
{
    uint32_t val;
    memcpy(&val, buf + offset, 4);
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    val = __builtin_bswap32(val);
#endif
    return val;
}

/*
 * Decode a fixed64 (little-endian) without alignment requirements.
 */
uint64_t hs_proto_decode_fixed64(const uint8_t *buf, int offset)
{
    uint64_t val;
    memcpy(&val, buf + offset, 8);
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    val = __builtin_bswap64(val);
#endif
    return val;
}

/*
 * Scan for the next field tag in a message buffer.
 * Skips the current field value based on wire type.
 * Returns the new offset after skipping, or -1 on error.
 */
int hs_proto_skip_field(
    const uint8_t *buf,
    int len,
    int offset,
    int wire_type)
{
    switch (wire_type) {
        case 0: { /* varint */
            int consumed = hs_proto_skip_varint(buf, len, offset);
            if (consumed == 0) return -1;
            return offset + consumed;
        }
        case 1: /* 64-bit */
            if (offset + 8 > len) return -1;
            return offset + 8;
        case 2: { /* length-delimited */
            uint64_t field_len;
            int consumed = hs_proto_decode_varint_fast(buf, len, offset, &field_len);
            if (consumed == 0) return -1;
            int new_offset = offset + consumed + (int)field_len;
            if (new_offset > len) return -1;
            return new_offset;
        }
        case 5: /* 32-bit */
            if (offset + 4 > len) return -1;
            return offset + 4;
        default:
            return -1;
    }
}

/* Big-endian read helpers — single word load + byteswap */
uint16_t hs_proto_read_be16(const uint8_t *buf, int offset) {
    uint16_t val;
    memcpy(&val, buf + offset, 2);
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    val = __builtin_bswap16(val);
#endif
    return val;
}

uint32_t hs_proto_read_be32(const uint8_t *buf, int offset) {
    uint32_t val;
    memcpy(&val, buf + offset, 4);
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    val = __builtin_bswap32(val);
#endif
    return val;
}

uint64_t hs_proto_read_be64(const uint8_t *buf, int offset) {
    uint64_t val;
    memcpy(&val, buf + offset, 8);
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    val = __builtin_bswap64(val);
#endif
    return val;
}

/* Big-endian write helpers */
void hs_proto_write_be16(uint8_t *buf, int offset, uint16_t val) {
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    val = __builtin_bswap16(val);
#endif
    memcpy(buf + offset, &val, 2);
}

void hs_proto_write_be32(uint8_t *buf, int offset, uint32_t val) {
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    val = __builtin_bswap32(val);
#endif
    memcpy(buf + offset, &val, 4);
}

void hs_proto_write_be64(uint8_t *buf, int offset, uint64_t val) {
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
    val = __builtin_bswap64(val);
#endif
    memcpy(buf + offset, &val, 8);
}

/* Find the offset of the first NUL byte starting at buf+offset.
   Returns offset relative to buf, or -1 if not found within len bytes.
   16-byte SIMD fast path. */
int hs_proto_find_nul(const uint8_t *buf, int offset, int len)
{
    int i = offset;
    simde__m128i zero = simde_mm_setzero_si128();

    for (; i + 16 <= len; i += 16) {
        simde__m128i chunk = simde_mm_loadu_si128((const simde__m128i *)(buf + i));
        simde__m128i cmp = simde_mm_cmpeq_epi8(chunk, zero);
        int mask = simde_mm_movemask_epi8(cmp);
        if (mask != 0) {
            return i + __builtin_ctz(mask);
        }
    }
    for (; i < len; i++) {
        if (buf[i] == 0) return i;
    }
    return -1;
}

/* Check if len bytes starting at buf+offset are all ASCII (< 0x80).
   Uses 16-byte SIMD. Returns 1 if all ASCII, 0 otherwise. */
int hs_proto_is_ascii(const uint8_t *buf, int offset, int len)
{
    int i = 0;
    const uint8_t *p = buf + offset;
    simde__m128i highbit = simde_mm_set1_epi8((char)0x80);

    for (; i + 16 <= len; i += 16) {
        simde__m128i chunk = simde_mm_loadu_si128((const simde__m128i *)(p + i));
        simde__m128i test = simde_mm_and_si128(chunk, highbit);
        if (simde_mm_movemask_epi8(test) != 0) return 0;
    }
    for (; i < len; i++) {
        if (p[i] >= 0x80) return 0;
    }
    return 1;
}

/* LE read/write — on LE platforms these are just memcpy (which the compiler optimizes to a single MOV) */
uint16_t hs_proto_read_le16(const uint8_t *buf, int offset) {
    uint16_t val;
    memcpy(&val, buf + offset, 2);
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    val = __builtin_bswap16(val);
#endif
    return val;
}

uint32_t hs_proto_read_le32(const uint8_t *buf, int offset) {
    uint32_t val;
    memcpy(&val, buf + offset, 4);
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    val = __builtin_bswap32(val);
#endif
    return val;
}

uint64_t hs_proto_read_le64(const uint8_t *buf, int offset) {
    uint64_t val;
    memcpy(&val, buf + offset, 8);
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    val = __builtin_bswap64(val);
#endif
    return val;
}

void hs_proto_write_le16(uint8_t *buf, int offset, uint16_t val) {
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    val = __builtin_bswap16(val);
#endif
    memcpy(buf + offset, &val, 2);
}

void hs_proto_write_le32(uint8_t *buf, int offset, uint32_t val) {
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    val = __builtin_bswap32(val);
#endif
    memcpy(buf + offset, &val, 4);
}

void hs_proto_write_le64(uint8_t *buf, int offset, uint64_t val) {
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    val = __builtin_bswap64(val);
#endif
    memcpy(buf + offset, &val, 8);
}

/*
 * Find the first byte in buf[offset..offset+len) that needs JSON escaping:
 * anything < 0x20, or '"', or '\\'.
 * Returns the offset of the first such byte, or offset+len if none found.
 * 16-byte SIMD fast path.
 */
int hs_proto_find_json_escape(const uint8_t *buf, int offset, int len)
{
    int i = 0;
    const uint8_t *p = buf + offset;
    simde__m128i dquote = simde_mm_set1_epi8('"');
    simde__m128i bslash = simde_mm_set1_epi8('\\');

    for (; i + 16 <= len; i += 16) {
        simde__m128i chunk = simde_mm_loadu_si128((const simde__m128i *)(p + i));
        int m1 = simde_mm_movemask_epi8(simde_mm_cmpeq_epi8(chunk, dquote));
        int m2 = simde_mm_movemask_epi8(simde_mm_cmpeq_epi8(chunk, bslash));
        /* Control chars: unsigned byte <= 0x1F via bias trick */
        simde__m128i biased = simde_mm_xor_si128(chunk, simde_mm_set1_epi8((char)0x80));
        simde__m128i limit = simde_mm_set1_epi8((char)(0x20 ^ 0x80));
        int m3 = simde_mm_movemask_epi8(simde_mm_cmplt_epi8(biased, limit));
        int mask = m1 | m2 | m3;
        if (mask != 0) return offset + i + __builtin_ctz(mask);
    }
    for (; i < len; i++) {
        uint8_t b = p[i];
        if (b == '"' || b == '\\' || b < 0x20) return offset + i;
    }
    return offset + len;
}

/*
 * Skip EDN whitespace characters (space, tab, newline, carriage return, comma).
 * Returns the offset of the first non-whitespace character.
 * EDN treats commas as whitespace. Also skips ; comments to end of line.
 * 16-byte SIMD fast path for the common whitespace chars.
 */
int hs_proto_skip_ws(const uint8_t *buf, int offset, int len)
{
    int i = offset;

    while (i < len) {
        /* SIMD fast path: skip blocks of 16 whitespace bytes */
        while (i + 16 <= len) {
            simde__m128i chunk = simde_mm_loadu_si128((const simde__m128i *)(buf + i));
            /* Check each byte against whitespace chars */
            simde__m128i eq_space = simde_mm_cmpeq_epi8(chunk, simde_mm_set1_epi8(' '));
            simde__m128i eq_tab   = simde_mm_cmpeq_epi8(chunk, simde_mm_set1_epi8('\t'));
            simde__m128i eq_nl    = simde_mm_cmpeq_epi8(chunk, simde_mm_set1_epi8('\n'));
            simde__m128i eq_cr    = simde_mm_cmpeq_epi8(chunk, simde_mm_set1_epi8('\r'));
            simde__m128i eq_comma = simde_mm_cmpeq_epi8(chunk, simde_mm_set1_epi8(','));
            simde__m128i ws = simde_mm_or_si128(
                simde_mm_or_si128(eq_space, eq_tab),
                simde_mm_or_si128(simde_mm_or_si128(eq_nl, eq_cr), eq_comma));
            int mask = simde_mm_movemask_epi8(ws);
            if (mask == 0xFFFF) {
                i += 16;
            } else {
                i += __builtin_ctz(~mask);
                break;
            }
        }
        if (i >= len) break;

        uint8_t b = buf[i];
        if (b == ' ' || b == '\t' || b == '\n' || b == '\r' || b == ',') {
            i++;
        } else if (b == ';') {
            /* Skip comment to end of line */
            i++;
            while (i < len && buf[i] != '\n') i++;
        } else {
            break;
        }
    }
    return i;
}

/*
 * Compare a search value against N serialized LE int32 bounds.
 * Returns a bitmask where bit i is set if bounds[i] <= search_val.
 * Processes 4 bounds at a time using SSE2.
 * count must be <= 32.
 */
int hs_proto_compare_bounds(
    const uint8_t *bounds, int count, int width,
    const uint8_t *search)
{
    if (width != 4) {
        /* Scalar fallback for non-int32 widths */
        int result = 0;
        for (int i = 0; i < count && i < 32; i++) {
            int cmp = memcmp(bounds + i * width, search, width);
            if (cmp <= 0) result |= (1 << i);
        }
        return result;
    }

    /* Fast path for 4-byte LE int32 bounds */
    int32_t search_val;
    memcpy(&search_val, search, 4);
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    search_val = __builtin_bswap32(search_val);
#endif

    int result = 0;
    int i = 0;
    simde__m128i sv = simde_mm_set1_epi32(search_val);

    for (; i + 4 <= count; i += 4) {
        simde__m128i bv = simde_mm_loadu_si128((const simde__m128i *)(bounds + i * 4));
        /* bounds[j] <= search_val  iff  NOT (search_val < bounds[j]) */
        simde__m128i lt = simde_mm_cmplt_epi32(sv, bv);
        /* lt has 0xFFFFFFFF where search < bound, 0 where search >= bound */
        int mask = simde_mm_movemask_ps(simde_mm_castsi128_ps(lt));
        /* Invert: we want bits set where search >= bound */
        result |= ((~mask) & 0xF) << i;
    }

    /* Scalar tail */
    for (; i < count && i < 32; i++) {
        int32_t bval;
        memcpy(&bval, bounds + i * 4, 4);
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
        bval = __builtin_bswap32(bval);
#endif
        if (bval <= search_val) result |= (1 << i);
    }
    return result;
}

/*
 * Validate Arrow IPC buffer offset/length pairs.
 * Each buffer is (offset: int64, length: int64), packed contiguously.
 * Checks:
 *   1. offset >= 0 and length >= 0
 *   2. offset + length <= body_length (no overflow)
 *   3. No two buffers overlap
 *
 * For the non-overlap check, we do a simple O(n) scan since buffers in
 * Arrow IPC are typically in order. Returns 1 if valid, 0 if invalid.
 */
int hs_proto_validate_arrow_buffers(
    const int64_t *buf_pairs, int count, int64_t body_length)
{
    if (count == 0) return 1;

    int64_t prev_end = 0;
    int i = 0;

    /* SSE2 path: validate 2 buffers (4 int64s) at a time for non-negative */
    for (; i + 1 < count; i += 2) {
        simde__m128i pair0 = simde_mm_loadu_si128(
            (const simde__m128i *)(buf_pairs + i * 2));
        simde__m128i pair1 = simde_mm_loadu_si128(
            (const simde__m128i *)(buf_pairs + i * 2 + 2));
        /* Check all 4 values are non-negative: the sign bit of each
         * int64 lives in byte 7 of its 8-byte lane (movemask bits 7, 15). */
        simde__m128i combined = simde_mm_or_si128(pair0, pair1);
        if (simde_mm_movemask_epi8(combined) & 0x8080) return 0;

        /* Scalar bounds + overlap checks */
        for (int j = 0; j < 2; j++) {
            int idx = i + j;
            int64_t off = buf_pairs[idx * 2];
            int64_t len = buf_pairs[idx * 2 + 1];
            /* off, len >= 0, so this cannot overflow (off + len could). */
            if (len > body_length || off > body_length - len) return 0;
            if (off < prev_end && len > 0) return 0;
            int64_t end = off + len;
            if (end > prev_end) prev_end = end;
        }
    }

    /* Scalar tail */
    for (; i < count; i++) {
        int64_t off = buf_pairs[i * 2];
        int64_t len = buf_pairs[i * 2 + 1];
        if (off < 0 || len < 0) return 0;
        if (len > body_length || off > body_length - len) return 0;
        if (off < prev_end && len > 0) return 0;
        int64_t end = off + len;
        if (end > prev_end) prev_end = end;
    }
    return 1;
}
