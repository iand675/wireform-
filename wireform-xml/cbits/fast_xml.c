/*
 * SIMD-optimized XML scanning primitives.
 *
 * Uses SSE2 via simde for portable SIMD: load 16 bytes, pcmpeqb against
 * target byte(s), pmovmskb to get a bitmask, ctz to find first match.
 */

#include <stdint.h>
#include <string.h>
#include <simde/x86/sse2.h>

/*
 * Fast scan for '<' in 16-byte chunks.
 * Returns offset of first '<' at or after offset, or -1 if not found.
 */
int hs_xml_find_lt(const uint8_t *buf, int offset, int len)
{
    int i = offset;
    simde__m128i target = simde_mm_set1_epi8('<');

    /* Scalar lead-in to align to 16-byte boundary */
    while (i < len && ((uintptr_t)(buf + i) & 15) != 0) {
        if (buf[i] == '<') return i;
        i++;
    }

    for (; i + 16 <= len; i += 16) {
        simde__m128i chunk = simde_mm_load_si128((const simde__m128i *)(buf + i));
        int mask = simde_mm_movemask_epi8(simde_mm_cmpeq_epi8(chunk, target));
        if (mask != 0) return i + __builtin_ctz(mask);
    }

    for (; i < len; i++) {
        if (buf[i] == '<') return i;
    }
    return -1;
}

/*
 * Fast scan for a specific byte in 16-byte chunks.
 * Returns offset of first match at or after offset, or -1 if not found.
 */
int hs_xml_find_byte(const uint8_t *buf, int offset, int len, uint8_t target_byte)
{
    int i = offset;
    simde__m128i target = simde_mm_set1_epi8((char)target_byte);

    while (i < len && ((uintptr_t)(buf + i) & 15) != 0) {
        if (buf[i] == target_byte) return i;
        i++;
    }

    for (; i + 16 <= len; i += 16) {
        simde__m128i chunk = simde_mm_load_si128((const simde__m128i *)(buf + i));
        int mask = simde_mm_movemask_epi8(simde_mm_cmpeq_epi8(chunk, target));
        if (mask != 0) return i + __builtin_ctz(mask);
    }

    for (; i < len; i++) {
        if (buf[i] == target_byte) return i;
    }
    return -1;
}

/*
 * Scan for end of attribute value (find unescaped quote_char).
 * Returns offset of the closing quote, or -1 if not found.
 */
int hs_xml_find_attr_end(const uint8_t *buf, int offset, int len, uint8_t quote_char)
{
    return hs_xml_find_byte(buf, offset, len, quote_char);
}

/*
 * Scan for end of text content: find '<' or '&'.
 * Returns offset of first '<' or '&', or len if neither found.
 */
int hs_xml_find_text_end(const uint8_t *buf, int offset, int len)
{
    int i = offset;
    simde__m128i v_lt  = simde_mm_set1_epi8('<');
    simde__m128i v_amp = simde_mm_set1_epi8('&');

    while (i < len && ((uintptr_t)(buf + i) & 15) != 0) {
        if (buf[i] == '<' || buf[i] == '&') return i;
        i++;
    }

    for (; i + 16 <= len; i += 16) {
        simde__m128i chunk = simde_mm_load_si128((const simde__m128i *)(buf + i));
        int m1 = simde_mm_movemask_epi8(simde_mm_cmpeq_epi8(chunk, v_lt));
        int m2 = simde_mm_movemask_epi8(simde_mm_cmpeq_epi8(chunk, v_amp));
        int mask = m1 | m2;
        if (mask != 0) return i + __builtin_ctz(mask);
    }

    for (; i < len; i++) {
        if (buf[i] == '<' || buf[i] == '&') return i;
    }
    return len;
}

/*
 * Scan for end of CDATA section (find ']]>').
 * Returns offset of the first ']' of ']]>', or -1 if not found.
 */
int hs_xml_find_cdata_end(const uint8_t *buf, int offset, int len)
{
    int i = offset;
    simde__m128i v_rb = simde_mm_set1_epi8(']');

    while (i < len && ((uintptr_t)(buf + i) & 15) != 0) {
        if (buf[i] == ']' && i + 2 < len && buf[i+1] == ']' && buf[i+2] == '>') {
            return i;
        }
        i++;
    }

    for (; i + 16 <= len; i += 16) {
        simde__m128i chunk = simde_mm_load_si128((const simde__m128i *)(buf + i));
        int mask = simde_mm_movemask_epi8(simde_mm_cmpeq_epi8(chunk, v_rb));
        while (mask != 0) {
            int bit = __builtin_ctz(mask);
            int pos = i + bit;
            if (pos + 2 < len && buf[pos+1] == ']' && buf[pos+2] == '>') {
                return pos;
            }
            mask &= mask - 1;
        }
    }

    for (; i < len; i++) {
        if (buf[i] == ']' && i + 2 < len && buf[i+1] == ']' && buf[i+2] == '>') {
            return i;
        }
    }
    return -1;
}

/*
 * Scan for end of comment (find '-->').
 * Returns offset of the first '-' of '-->', or -1.
 */
int hs_xml_find_comment_end(const uint8_t *buf, int offset, int len)
{
    int i = offset;
    simde__m128i v_dash = simde_mm_set1_epi8('-');

    while (i < len && ((uintptr_t)(buf + i) & 15) != 0) {
        if (buf[i] == '-' && i + 2 < len && buf[i+1] == '-' && buf[i+2] == '>') {
            return i;
        }
        i++;
    }

    for (; i + 16 <= len; i += 16) {
        simde__m128i chunk = simde_mm_load_si128((const simde__m128i *)(buf + i));
        int mask = simde_mm_movemask_epi8(simde_mm_cmpeq_epi8(chunk, v_dash));
        while (mask != 0) {
            int bit = __builtin_ctz(mask);
            int pos = i + bit;
            if (pos + 2 < len && buf[pos+1] == '-' && buf[pos+2] == '>') {
                return pos;
            }
            mask &= mask - 1;
        }
    }

    for (; i < len; i++) {
        if (buf[i] == '-' && i + 2 < len && buf[i+1] == '-' && buf[i+2] == '>') {
            return i;
        }
    }
    return -1;
}

