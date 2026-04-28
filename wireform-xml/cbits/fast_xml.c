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
 * Scan for XML structural characters in 16-byte chunks.
 * Finds: '<', '>', '/', '=', '"', '\'', '&'
 * Writes positions of structural bytes into indices[].
 * Returns number of indices written (<= max_indices).
 */
int hs_xml_scan_structural(const uint8_t *buf, int len,
                           uint32_t *indices, int max_indices)
{
    int count = 0;
    int i = 0;

    simde__m128i v_lt   = simde_mm_set1_epi8('<');
    simde__m128i v_gt   = simde_mm_set1_epi8('>');
    simde__m128i v_sl   = simde_mm_set1_epi8('/');
    simde__m128i v_eq   = simde_mm_set1_epi8('=');
    simde__m128i v_dq   = simde_mm_set1_epi8('"');
    simde__m128i v_sq   = simde_mm_set1_epi8('\'');
    simde__m128i v_amp  = simde_mm_set1_epi8('&');

    for (; i + 16 <= len && count < max_indices; i += 16) {
        simde__m128i chunk = simde_mm_loadu_si128((const simde__m128i *)(buf + i));

        simde__m128i m1 = simde_mm_cmpeq_epi8(chunk, v_lt);
        simde__m128i m2 = simde_mm_cmpeq_epi8(chunk, v_gt);
        simde__m128i m3 = simde_mm_cmpeq_epi8(chunk, v_sl);
        simde__m128i m4 = simde_mm_cmpeq_epi8(chunk, v_eq);
        simde__m128i m5 = simde_mm_cmpeq_epi8(chunk, v_dq);
        simde__m128i m6 = simde_mm_cmpeq_epi8(chunk, v_sq);
        simde__m128i m7 = simde_mm_cmpeq_epi8(chunk, v_amp);

        simde__m128i combined = simde_mm_or_si128(
            simde_mm_or_si128(simde_mm_or_si128(m1, m2),
                              simde_mm_or_si128(m3, m4)),
            simde_mm_or_si128(simde_mm_or_si128(m5, m6), m7));

        int mask = simde_mm_movemask_epi8(combined);
        while (mask != 0 && count < max_indices) {
            int bit = __builtin_ctz(mask);
            indices[count++] = (uint32_t)(i + bit);
            mask &= mask - 1;
        }
    }

    for (; i < len && count < max_indices; i++) {
        uint8_t b = buf[i];
        if (b == '<' || b == '>' || b == '/' || b == '=' ||
            b == '"' || b == '\'' || b == '&') {
            indices[count++] = (uint32_t)i;
        }
    }
    return count;
}

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

/*
 * HTML text end: scan for '<', '&', '\0', or '\r' in 16-byte chunks.
 * Returns offset of first match, or len if none found.
 * Used by the HTML tokenizer for bulk text scanning.
 */
int hs_html_find_text_end(const uint8_t *buf, int offset, int len)
{
    int i = offset;
    simde__m128i v_lt  = simde_mm_set1_epi8('<');
    simde__m128i v_amp = simde_mm_set1_epi8('&');
    simde__m128i v_nul = simde_mm_set1_epi8('\0');
    simde__m128i v_cr  = simde_mm_set1_epi8('\r');

    while (i < len && ((uintptr_t)(buf + i) & 15) != 0) {
        uint8_t b = buf[i];
        if (b == '<' || b == '&' || b == '\0' || b == '\r') return i;
        i++;
    }

    for (; i + 16 <= len; i += 16) {
        simde__m128i chunk = simde_mm_load_si128((const simde__m128i *)(buf + i));
        int m1 = simde_mm_movemask_epi8(simde_mm_cmpeq_epi8(chunk, v_lt));
        int m2 = simde_mm_movemask_epi8(simde_mm_cmpeq_epi8(chunk, v_amp));
        int m3 = simde_mm_movemask_epi8(simde_mm_cmpeq_epi8(chunk, v_nul));
        int m4 = simde_mm_movemask_epi8(simde_mm_cmpeq_epi8(chunk, v_cr));
        int mask = m1 | m2 | m3 | m4;
        if (mask != 0) return i + __builtin_ctz(mask);
    }

    for (; i < len; i++) {
        uint8_t b = buf[i];
        if (b == '<' || b == '&' || b == '\0' || b == '\r') return i;
    }
    return len;
}

/*
 * HTML text escape: scan for '<', '>', '&' only (no quotes).
 * Returns offset of first match, or len if not found.
 */
/* Renamed to avoid colliding with wireform-html when both packages
 * are linked into the same executable. */
int hs_xml_find_text_escape(const uint8_t *buf, int offset, int len)
{
    int i = offset;
    simde__m128i v_lt  = simde_mm_set1_epi8('<');
    simde__m128i v_gt  = simde_mm_set1_epi8('>');
    simde__m128i v_amp = simde_mm_set1_epi8('&');

    while (i < len && ((uintptr_t)(buf + i) & 15) != 0) {
        uint8_t b = buf[i];
        if (b == '<' || b == '>' || b == '&') return i;
        i++;
    }

    for (; i + 16 <= len; i += 16) {
        simde__m128i chunk = simde_mm_load_si128((const simde__m128i *)(buf + i));
        int m1 = simde_mm_movemask_epi8(simde_mm_cmpeq_epi8(chunk, v_lt));
        int m2 = simde_mm_movemask_epi8(simde_mm_cmpeq_epi8(chunk, v_gt));
        int m3 = simde_mm_movemask_epi8(simde_mm_cmpeq_epi8(chunk, v_amp));
        int mask = m1 | m2 | m3;
        if (mask != 0) return i + __builtin_ctz(mask);
    }

    for (; i < len; i++) {
        uint8_t b = buf[i];
        if (b == '<' || b == '>' || b == '&') return i;
    }
    return len;
}

/*
 * HTML attr escape: scan for '"', '&', '<', '>' in attribute values.
 * Returns offset of first match, or len if not found.
 */
/* Renamed to avoid colliding with wireform-html. */
int hs_xml_find_attr_escape(const uint8_t *buf, int offset, int len)
{
    int i = offset;
    simde__m128i v_dq  = simde_mm_set1_epi8('"');
    simde__m128i v_amp = simde_mm_set1_epi8('&');
    simde__m128i v_lt  = simde_mm_set1_epi8('<');
    simde__m128i v_gt  = simde_mm_set1_epi8('>');

    while (i < len && ((uintptr_t)(buf + i) & 15) != 0) {
        uint8_t b = buf[i];
        if (b == '"' || b == '&' || b == '<' || b == '>') return i;
        i++;
    }

    for (; i + 16 <= len; i += 16) {
        simde__m128i chunk = simde_mm_load_si128((const simde__m128i *)(buf + i));
        int m1 = simde_mm_movemask_epi8(simde_mm_cmpeq_epi8(chunk, v_dq));
        int m2 = simde_mm_movemask_epi8(simde_mm_cmpeq_epi8(chunk, v_amp));
        int m3 = simde_mm_movemask_epi8(simde_mm_cmpeq_epi8(chunk, v_lt));
        int m4 = simde_mm_movemask_epi8(simde_mm_cmpeq_epi8(chunk, v_gt));
        int mask = m1 | m2 | m3 | m4;
        if (mask != 0) return i + __builtin_ctz(mask);
    }

    for (; i < len; i++) {
        uint8_t b = buf[i];
        if (b == '"' || b == '&' || b == '<' || b == '>') return i;
    }
    return len;
}

/*
 * Scan for a quote char or '&' in attribute values (16-byte SIMD).
 * Returns offset of first match, or len if not found.
 */
int hs_html_find_attr_break(const uint8_t *buf, int offset, int len, uint8_t quote_char)
{
    int i = offset;
    simde__m128i v_q   = simde_mm_set1_epi8((char)quote_char);
    simde__m128i v_amp = simde_mm_set1_epi8('&');

    while (i < len && ((uintptr_t)(buf + i) & 15) != 0) {
        uint8_t b = buf[i];
        if (b == quote_char || b == '&') return i;
        i++;
    }

    for (; i + 16 <= len; i += 16) {
        simde__m128i chunk = simde_mm_load_si128((const simde__m128i *)(buf + i));
        int m1 = simde_mm_movemask_epi8(simde_mm_cmpeq_epi8(chunk, v_q));
        int m2 = simde_mm_movemask_epi8(simde_mm_cmpeq_epi8(chunk, v_amp));
        int mask = m1 | m2;
        if (mask != 0) return i + __builtin_ctz(mask);
    }

    for (; i < len; i++) {
        uint8_t b = buf[i];
        if (b == quote_char || b == '&') return i;
    }
    return len;
}

/*
 * Fast scan for bytes that need XML escaping: '<', '>', '&', '"', '\''.
 * Returns offset of first such byte, or offset+scan_len if none found.
 */
int hs_xml_find_escape(const uint8_t *buf, int offset, int len)
{
    int i = 0;
    const uint8_t *p = buf + offset;
    simde__m128i v_lt  = simde_mm_set1_epi8('<');
    simde__m128i v_gt  = simde_mm_set1_epi8('>');
    simde__m128i v_amp = simde_mm_set1_epi8('&');
    simde__m128i v_dq  = simde_mm_set1_epi8('"');
    simde__m128i v_sq  = simde_mm_set1_epi8('\'');

    for (; i + 16 <= len; i += 16) {
        simde__m128i chunk = simde_mm_loadu_si128((const simde__m128i *)(p + i));
        int m1 = simde_mm_movemask_epi8(simde_mm_cmpeq_epi8(chunk, v_lt));
        int m2 = simde_mm_movemask_epi8(simde_mm_cmpeq_epi8(chunk, v_gt));
        int m3 = simde_mm_movemask_epi8(simde_mm_cmpeq_epi8(chunk, v_amp));
        int m4 = simde_mm_movemask_epi8(simde_mm_cmpeq_epi8(chunk, v_dq));
        int m5 = simde_mm_movemask_epi8(simde_mm_cmpeq_epi8(chunk, v_sq));
        int mask = m1 | m2 | m3 | m4 | m5;
        if (mask != 0) return offset + i + __builtin_ctz(mask);
    }

    for (; i < len; i++) {
        uint8_t b = p[i];
        if (b == '<' || b == '>' || b == '&' || b == '"' || b == '\'') {
            return offset + i;
        }
    }
    return offset + len;
}
