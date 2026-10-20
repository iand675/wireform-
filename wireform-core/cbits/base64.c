/*
 * RFC 4648 base64 encode / decode.
 *
 * Two SIMD-accelerated primitives, in line with the rest of
 * wireform-core:
 *
 *   * 'hs_base64_encode': SSSE3 (via simde) inner loop, 12 input
 *     bytes -> 16 output chars per iteration, scalar prologue /
 *     epilogue for the tail.
 *   * 'hs_base64_decode': SSE2 (via simde) pre-scan, 16 chars at a
 *     time, rejects any window containing a high-bit byte (which
 *     by construction is outside the base64 alphabet); the actual
 *     sextet extraction then runs through a tight scalar loop on
 *     a 256-entry decode table.  This is the same shape every
 *     wireform format takes for "validate-then-decode" SIMD: see
 *     'hs_proto_validate_utf8_fast' / 'hs_find_byte'.
 *
 * The encode SIMD path uses the well-known Mula / alfredklomp
 * formula (no LUT in memory; ASCII offset is derived from the
 * 6-bit value via a single PSHUFB on a 16-byte lookup).
 *
 * The decoder is kept scalar in the sextet-extraction step
 * because the typical wireform call site (SHA-1 = 28 base64
 * chars; proto3 JSON bytes fields = O(100s)) does not benefit
 * enough from a Mula-style PSHUFB classifier to be worth the
 * table-debugging cost.  Encoding -- which is on the hot path
 * for big payloads via the proto3 JSON mapping -- stays SSSE3.
 */

#include <limits.h>
#include <stdint.h>
#include <string.h>
#include <simde/x86/sse2.h>
#include <simde/x86/ssse3.h>

/* 64 symbols plus the literal's terminating NUL (never indexed). */
static const char b64_alphabet[65] =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "abcdefghijklmnopqrstuvwxyz"
  "0123456789+/";

/* Reverse-lookup table: ASCII -> 6-bit value, or 0xFF for any byte
 * outside the standard base64 alphabet.  '=' maps to 0xFF too;
 * the tail handler checks for it explicitly. */
static const uint8_t b64_dec_table[256] = {
  /* 0x00..0x2A (43 entries) */
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
  255,255,255,255,255,255,255,255,255,255,255,
  /* 0x2B '+' = 62, 0x2C ',' invalid, 0x2D '-' invalid,
   * 0x2E '.' invalid, 0x2F '/' = 63 */
  62, 255, 255, 255, 63,
  /* 0x30..0x39 '0'..'9' = 52..61 */
  52, 53, 54, 55, 56, 57, 58, 59, 60, 61,
  /* 0x3A..0x40 (7 entries) */
  255, 255, 255, 255, 255, 255, 255,
  /* 0x41..0x5A 'A'..'Z' = 0..25 */
  0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
  10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
  20, 21, 22, 23, 24, 25,
  /* 0x5B..0x60 (6 entries) */
  255, 255, 255, 255, 255, 255,
  /* 0x61..0x7A 'a'..'z' = 26..51 */
  26, 27, 28, 29, 30, 31, 32, 33, 34, 35,
  36, 37, 38, 39, 40, 41, 42, 43, 44, 45,
  46, 47, 48, 49, 50, 51,
  /* 0x7B..0xFF (133 entries) */
  255, 255, 255, 255, 255,
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
  255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
};

/* ------------------------------------------------------------------
 * Length helpers (RFC 4648 sec 4).
 *
 * Encode: 3-byte triplets pad up to the next multiple of 4 chars.
 * Decode (upper bound): every 4 chars produce up to 3 bytes;
 * trailing '=' chars subtract one byte each.  The exact decoded
 * length is reported by hs_base64_decode itself; this helper just
 * sizes the output buffer.
 *
 * Both take and return int (the FFI uses CInt).  The largest input
 * whose padded encoding still fits in an int is 3 * (INT_MAX / 4);
 * for anything longer the encoded length is not representable, so
 * hs_base64_encoded_length returns -1 and hs_base64_encode refuses
 * to run (instead of overflowing its output index).
 * ------------------------------------------------------------------ */

#define B64_MAX_ENCODE_INPUT (3 * (INT_MAX / 4))

int hs_base64_encoded_length(int in_len)
{
    if (in_len < 0) return 0;
    if (in_len > B64_MAX_ENCODE_INPUT) return -1;
    return ((in_len + 2) / 3) * 4;
}

int hs_base64_decoded_max_length(int in_len)
{
    if (in_len < 0) return 0;
    return (in_len / 4) * 3;
}

/* ------------------------------------------------------------------
 * Encode
 *
 * Returns the number of bytes written to @out, which must hold
 * hs_base64_encoded_length(in_len) bytes; always equals that length
 * (padded form).  in_len <= 0 writes nothing and returns 0;
 * in_len > 3 * (INT_MAX / 4) writes nothing and returns -1.
 * ------------------------------------------------------------------ */

static inline simde__m128i enc_reshuffle(simde__m128i in)
{
    /* Spread each 3-byte triplet into a 4-byte slot so the 6-bit
     * fields land in adjacent bytes. */
    in = simde_mm_shuffle_epi8(in, simde_mm_setr_epi8(
         1,  0,  2,  1,
         4,  3,  5,  4,
         7,  6,  8,  7,
        10,  9, 11, 10));

    simde__m128i t0 = simde_mm_and_si128(in, simde_mm_set1_epi32(0x0fc0fc00));
    simde__m128i t1 = simde_mm_mulhi_epu16(t0, simde_mm_set1_epi32(0x04000040));
    simde__m128i t2 = simde_mm_and_si128(in, simde_mm_set1_epi32(0x003f03f0));
    simde__m128i t3 = simde_mm_mullo_epi16(t2, simde_mm_set1_epi32(0x01000010));
    return simde_mm_or_si128(t1, t3);
}

static inline simde__m128i enc_translate(simde__m128i in)
{
    /* 6-bit value v -> ASCII char via a 16-entry PSHUFB lookup of
     * the offset to add to v.  Branchless. */
    simde__m128i lut = simde_mm_setr_epi8(
        65, 71, -4, -4, -4, -4, -4, -4,
        -4, -4, -4, -4, -19, -16, 0, 0);
    simde__m128i indices = simde_mm_subs_epu8(in, simde_mm_set1_epi8(51));
    simde__m128i mask    = simde_mm_cmpgt_epi8(in, simde_mm_set1_epi8(25));
    indices = simde_mm_sub_epi8(indices, mask);
    simde__m128i offsets = simde_mm_shuffle_epi8(lut, indices);
    return simde_mm_add_epi8(in, offsets);
}

int hs_base64_encode(const uint8_t *in, int in_len, uint8_t *out)
{
    int i = 0, j = 0;

    if (in_len > B64_MAX_ENCODE_INPUT) return -1;

    /* SIMD: 12 in -> 16 out per iter.  Need 16 readable input bytes
     * (we load 16 then mask out the upper 4) so guard accordingly. */
    while (in_len - i >= 16) {
        simde__m128i chunk = simde_mm_loadu_si128((const simde__m128i *)(in + i));
        simde__m128i v = enc_reshuffle(chunk);
        simde__m128i c = enc_translate(v);
        simde_mm_storeu_si128((simde__m128i *)(out + j), c);
        i += 12;
        j += 16;
    }

    /* Scalar tail: 3 in -> 4 out. */
    while (in_len - i >= 3) {
        uint32_t v = ((uint32_t)in[i]   << 16)
                   | ((uint32_t)in[i+1] <<  8)
                   |  (uint32_t)in[i+2];
        out[j+0] = (uint8_t)b64_alphabet[(v >> 18) & 0x3F];
        out[j+1] = (uint8_t)b64_alphabet[(v >> 12) & 0x3F];
        out[j+2] = (uint8_t)b64_alphabet[(v >>  6) & 0x3F];
        out[j+3] = (uint8_t)b64_alphabet[ v        & 0x3F];
        i += 3;
        j += 4;
    }

    /* RFC 4648 sec 4 padding. */
    int rem = in_len - i;
    if (rem == 1) {
        uint32_t v = (uint32_t)in[i] << 16;
        out[j+0] = (uint8_t)b64_alphabet[(v >> 18) & 0x3F];
        out[j+1] = (uint8_t)b64_alphabet[(v >> 12) & 0x3F];
        out[j+2] = (uint8_t)'=';
        out[j+3] = (uint8_t)'=';
        j += 4;
    } else if (rem == 2) {
        uint32_t v = ((uint32_t)in[i] << 16) | ((uint32_t)in[i+1] << 8);
        out[j+0] = (uint8_t)b64_alphabet[(v >> 18) & 0x3F];
        out[j+1] = (uint8_t)b64_alphabet[(v >> 12) & 0x3F];
        out[j+2] = (uint8_t)b64_alphabet[(v >>  6) & 0x3F];
        out[j+3] = (uint8_t)'=';
        j += 4;
    }
    return j;
}

/* ------------------------------------------------------------------
 * Decode
 *
 * Strict RFC 4648 decoder.  Input length must be a multiple of 4;
 * any out-of-alphabet byte (other than '=' in the trailing
 * position) produces -1.  Returns the number of output bytes
 * written on success.
 * ------------------------------------------------------------------ */

/* SSE2 fast-path probe: does this 16-byte window contain any byte
 * with the high bit set?  Any such byte is by construction outside
 * the base64 alphabet, so we can short-circuit. */
static inline int dec_window_high_bits(const uint8_t *in)
{
    simde__m128i chunk = simde_mm_loadu_si128((const simde__m128i *)in);
    return simde_mm_movemask_epi8(chunk);
}

int hs_base64_decode(const uint8_t *in, int in_len, uint8_t *out)
{
    if (in_len < 0 || (in_len & 3) != 0) return -1;
    if (in_len == 0) return 0;

    int i = 0, j = 0;
    int main_len = in_len - 4;  /* tail quartet handled separately */

    /* SIMD pre-scan + scalar decode of the main body.
     *
     * The SIMD step rejects any 16-byte window containing a byte
     * with the high bit set (cheap PMOVMSKB + branch); the scalar
     * step then decodes 4 quartets = 12 bytes from each clean
     * window.  This gives us the bulk of the SIMD win (early
     * rejection of garbage input) without the table-debugging
     * overhead of a full PSHUFB classifier. */
    while (main_len - i >= 16) {
        if (dec_window_high_bits(in + i) != 0) return -1;
        for (int k = 0; k < 16; k += 4) {
            uint8_t a = b64_dec_table[in[i+k+0]];
            uint8_t b = b64_dec_table[in[i+k+1]];
            uint8_t c = b64_dec_table[in[i+k+2]];
            uint8_t d = b64_dec_table[in[i+k+3]];
            if ((a | b | c | d) >= 64) return -1;
            uint32_t v = ((uint32_t)a << 18)
                       | ((uint32_t)b << 12)
                       | ((uint32_t)c <<  6)
                       |  (uint32_t)d;
            out[j+0] = (uint8_t)(v >> 16);
            out[j+1] = (uint8_t)(v >>  8);
            out[j+2] = (uint8_t) v;
            j += 3;
        }
        i += 16;
    }

    /* Scalar middle: remaining full quartets before the tail. */
    while (i < main_len) {
        uint8_t a = b64_dec_table[in[i+0]];
        uint8_t b = b64_dec_table[in[i+1]];
        uint8_t c = b64_dec_table[in[i+2]];
        uint8_t d = b64_dec_table[in[i+3]];
        if ((a | b | c | d) >= 64) return -1;
        uint32_t v = ((uint32_t)a << 18)
                   | ((uint32_t)b << 12)
                   | ((uint32_t)c <<  6)
                   |  (uint32_t)d;
        out[j+0] = (uint8_t)(v >> 16);
        out[j+1] = (uint8_t)(v >>  8);
        out[j+2] = (uint8_t) v;
        i += 4;
        j += 3;
    }

    /* Tail quartet: may contain '=' padding. */
    {
        uint8_t a = b64_dec_table[in[i+0]];
        uint8_t b = b64_dec_table[in[i+1]];
        if (a >= 64 || b >= 64) return -1;
        if (in[i+2] == (uint8_t)'=') {
            if (in[i+3] != (uint8_t)'=') return -1;
            uint32_t v = ((uint32_t)a << 18) | ((uint32_t)b << 12);
            out[j+0] = (uint8_t)(v >> 16);
            j += 1;
        } else if (in[i+3] == (uint8_t)'=') {
            uint8_t c = b64_dec_table[in[i+2]];
            if (c >= 64) return -1;
            uint32_t v = ((uint32_t)a << 18)
                       | ((uint32_t)b << 12)
                       | ((uint32_t)c <<  6);
            out[j+0] = (uint8_t)(v >> 16);
            out[j+1] = (uint8_t)(v >>  8);
            j += 2;
        } else {
            uint8_t c = b64_dec_table[in[i+2]];
            uint8_t d = b64_dec_table[in[i+3]];
            if (c >= 64 || d >= 64) return -1;
            uint32_t v = ((uint32_t)a << 18)
                       | ((uint32_t)b << 12)
                       | ((uint32_t)c <<  6)
                       |  (uint32_t)d;
            out[j+0] = (uint8_t)(v >> 16);
            out[j+1] = (uint8_t)(v >>  8);
            out[j+2] = (uint8_t) v;
            j += 3;
        }
        i += 4;
    }
    (void)i;
    return j;
}
