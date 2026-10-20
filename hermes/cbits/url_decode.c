/*
 * SIMD-accelerated URL percent-decoding for hermes.
 *
 * Two entry points:
 *
 *   hermes_url_scan_special(src, len, plus_is_space)
 *     - Returns the offset of the first byte that requires
 *       transformation ('%' always; '+' when plus_is_space != 0),
 *       or `len` if none is found.
 *
 *   hermes_url_decode(src, srclen, dst, plus_is_space)
 *     - Decodes the input. `dst` may equal `src` for in-place
 *       decoding. Returns the number of bytes written to `dst` on
 *       success, or a negative error code:
 *         -1  truncated escape ('%' or '%H' at EOF)
 *         -2  non-hex digit in an escape (including "%z" at EOF);
 *             the first error in input order wins
 *
 * The scan loop is the only SIMD-sensitive part. We pick the
 * widest registers available at compile time (AVX2 → SSE2 → NEON
 * → portable). The decode itself is byte-at-a-time; for typical
 * URL payloads (mostly ASCII without escapes) the SIMD scan jumps
 * over the unescaped runs in 16- or 32-byte strides, so the
 * decode work is proportional to the (small) number of escapes.
 *
 * For unescaped inputs, callers should test
 * `hermes_url_scan_special(...) == len` first and reuse the input
 * buffer with no copy; that's the fast path most query strings
 * actually hit.
 */
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#if defined(__AVX2__)
#  include <immintrin.h>
#  define HERMES_HAVE_AVX2 1
#elif defined(__SSE2__) || defined(__x86_64__) || defined(_M_X64)
#  include <emmintrin.h>
#  define HERMES_HAVE_SSE2 1
#elif defined(__ARM_NEON) || defined(__aarch64__)
#  include <arm_neon.h>
#  define HERMES_HAVE_NEON 1
#endif

static inline int hex_value(uint8_t c) {
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'a' && c <= 'f') return 10 + (c - 'a');
    if (c >= 'A' && c <= 'F') return 10 + (c - 'A');
    return -1;
}

/* Scalar tail scan, also used as the fallback on non-SIMD targets. */
static inline size_t scan_special_scalar(
    const uint8_t *src, size_t len, int plus_is_space)
{
    size_t i = 0;
    if (plus_is_space) {
        for (; i < len; i++) {
            uint8_t c = src[i];
            if (c == '%' || c == '+') return i;
        }
    } else {
        for (; i < len; i++) {
            if (src[i] == '%') return i;
        }
    }
    return len;
}

size_t hermes_url_scan_special(
    const uint8_t *src, size_t len, int plus_is_space)
{
    size_t i = 0;

#if defined(HERMES_HAVE_AVX2)
    {
        const __m256i pct  = _mm256_set1_epi8('%');
        const __m256i plus = _mm256_set1_epi8('+');
        while (len - i >= 32) {
            __m256i v = _mm256_loadu_si256((const __m256i *)(src + i));
            __m256i hit = _mm256_cmpeq_epi8(v, pct);
            if (plus_is_space) {
                hit = _mm256_or_si256(hit, _mm256_cmpeq_epi8(v, plus));
            }
            int mask = _mm256_movemask_epi8(hit);
            if (mask) return i + (size_t)__builtin_ctz((unsigned)mask);
            i += 32;
        }
    }
#endif

#if defined(HERMES_HAVE_SSE2) || defined(HERMES_HAVE_AVX2)
    /* SSE2 stride (also picks up the residue after AVX2). */
    {
        const __m128i pct  = _mm_set1_epi8('%');
        const __m128i plus = _mm_set1_epi8('+');
        while (len - i >= 16) {
            __m128i v = _mm_loadu_si128((const __m128i *)(src + i));
            __m128i hit = _mm_cmpeq_epi8(v, pct);
            if (plus_is_space) {
                hit = _mm_or_si128(hit, _mm_cmpeq_epi8(v, plus));
            }
            int mask = _mm_movemask_epi8(hit);
            if (mask) return i + (size_t)__builtin_ctz((unsigned)mask);
            i += 16;
        }
    }
#elif defined(HERMES_HAVE_NEON)
    {
        const uint8x16_t pct  = vdupq_n_u8('%');
        const uint8x16_t plus = vdupq_n_u8('+');
        while (len - i >= 16) {
            uint8x16_t v = vld1q_u8(src + i);
            uint8x16_t hit = vceqq_u8(v, pct);
            if (plus_is_space) {
                hit = vorrq_u8(hit, vceqq_u8(v, plus));
            }
            /* Reduce to a 64-bit mask: each matching byte → 0xFF. */
            uint64_t mask = vgetq_lane_u64(vreinterpretq_u64_u8(hit), 0);
            if (mask) {
                int j = __builtin_ctzll(mask) >> 3;
                return i + (size_t)j;
            }
            mask = vgetq_lane_u64(vreinterpretq_u64_u8(hit), 1);
            if (mask) {
                int j = __builtin_ctzll(mask) >> 3;
                return i + 8 + (size_t)j;
            }
            i += 16;
        }
    }
#endif

    return i + scan_special_scalar(src + i, len - i, plus_is_space);
}

ptrdiff_t hermes_url_decode(
    const uint8_t *src, size_t srclen,
    uint8_t *dst, int plus_is_space)
{
    size_t in = 0;
    size_t out = 0;
    while (in < srclen) {
        size_t hit = in + hermes_url_scan_special(src + in, srclen - in,
                                                  plus_is_space);
        size_t prefix = hit - in;
        if (prefix) {
            /* memmove tolerates dst == src + offset for in-place. */
            memmove(dst + out, src + in, prefix);
            out += prefix;
            in = hit;
        }
        if (in == srclen) break;

        uint8_t c = src[in];
        if (c == '+') {
            dst[out++] = ' ';
            in++;
        } else { /* '%' */
            if (srclen - in < 3) {
                /* Truncated, unless the one escape byte present is
                 * already not a hex digit ("%z" at EOF is -2, like
                 * "%zz"): errors are reported in input order. */
                if (srclen - in == 2 && hex_value(src[in + 1]) < 0) return -2;
                return -1;
            }
            int hi = hex_value(src[in + 1]);
            int lo = hex_value(src[in + 2]);
            if (hi < 0 || lo < 0) return -2;
            dst[out++] = (uint8_t)((hi << 4) | lo);
            in += 3;
        }
    }
    return (ptrdiff_t)out;
}
