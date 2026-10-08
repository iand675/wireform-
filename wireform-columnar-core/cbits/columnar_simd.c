/*
 * Columnar format helpers (Arrow validity / packed bools, Parquet PLAIN bool,
 * bitmap popcount). Uses SIMDe for portable SIMD on x86 and ARM.
 *
 * Mirrors the style of fast_decode.c: hot loops in C, thin Haskell FFI.
 */

#include <stdint.h>
#include <string.h>

#include <simde/x86/sse2.h>

/*
 * Population count of all bits in buf[0..len). Uses 64-bit popcount builtins
 * on the main loop; SIMDe is included for follow-on kernels (bit unpack width
 * > 1, bulk LE swizzle, etc.) in this same translation unit.
 */
int32_t hs_columnar_bitmap_popcount(const uint8_t *buf, int len)
{
    int32_t total = 0;
    int i = 0;

    for (; i + 8 <= len; i += 8) {
        uint64_t w;
        memcpy(&w, buf + i, 8);
        total += (int32_t)__builtin_popcountll(w);
    }

    for (; i + 4 <= len; i += 4) {
        uint32_t w;
        memcpy(&w, buf + i, 4);
        total += (int32_t)__builtin_popcountl((unsigned long)w);
    }

    for (; i < len; i++) {
        total += (int32_t)__builtin_popcount((unsigned int)buf[i]);
    }
    return total;
}

/*
 * Bit-expansion LUT: expand[b][k] == (b >> k) & 1 (LSB-first, Arrow / Parquet
 * PLAIN bool order). Built at compile time by the quadrupling macro ladder
 * below (1 -> 4 -> 16 -> 64 -> 256 rows); hs_columnar_unpack_lut_selfcheck()
 * verifies the result against the runtime loop this replaces.
 */
#define WF_BIT_ROW(b) \
    { ((b) >> 0) & 1, ((b) >> 1) & 1, ((b) >> 2) & 1, ((b) >> 3) & 1, \
      ((b) >> 4) & 1, ((b) >> 5) & 1, ((b) >> 6) & 1, ((b) >> 7) & 1 }
#define WF_BIT_ROWS_4(b)  WF_BIT_ROW(b),     WF_BIT_ROW((b)+1),      WF_BIT_ROW((b)+2),      WF_BIT_ROW((b)+3)
#define WF_BIT_ROWS_16(b) WF_BIT_ROWS_4(b),  WF_BIT_ROWS_4((b)+4),   WF_BIT_ROWS_4((b)+8),   WF_BIT_ROWS_4((b)+12)
#define WF_BIT_ROWS_64(b) WF_BIT_ROWS_16(b), WF_BIT_ROWS_16((b)+16), WF_BIT_ROWS_16((b)+32), WF_BIT_ROWS_16((b)+48)

static const uint8_t expand[256][8] = {
    WF_BIT_ROWS_64(0), WF_BIT_ROWS_64(64), WF_BIT_ROWS_64(128), WF_BIT_ROWS_64(192)
};

#undef WF_BIT_ROWS_64
#undef WF_BIT_ROWS_16
#undef WF_BIT_ROWS_4
#undef WF_BIT_ROW

/*
 * Rebuilds the LUT with the runtime loop that the macro ladder replaced and
 * compares the two. Returns 1 on match, 0 on mismatch. Test-only.
 */
int hs_columnar_unpack_lut_selfcheck(void)
{
    uint8_t ref[256][8];
    int b;
    for (b = 0; b < 256; b++) {
        int k;
        for (k = 0; k < 8; k++) {
            ref[b][k] = (uint8_t)((b >> k) & 1);
        }
    }
    return memcmp(ref, expand, sizeof ref) == 0;
}

/*
 * Expand packed bits to dst[i] in {0,1}. Bit order matches Arrow / Parquet
 * PLAIN bool: least-significant bit of each byte is the first logical value in
 * that byte (index i: byte i/8, bit i%8).
 */
void hs_columnar_unpack_bits_lsb(const uint8_t *src, int32_t n, uint8_t *dst)
{
    int pos = 0;
    while (pos < n) {
        int bi = pos / 8;
        uint8_t b = src[bi];
        int in_byte = pos & 7;
        int avail_in_byte = 8 - in_byte;
        int need = n - pos;
        int chunk = (need < avail_in_byte) ? need : avail_in_byte;

        if (in_byte == 0 && chunk == 8) {
            memcpy(dst + pos, expand[b], 8);
        } else {
            int k;
            for (k = 0; k < chunk; k++) {
                dst[pos + k] = expand[b][in_byte + k];
            }
        }
        pos += chunk;
    }
}

/*
 * Bulk copy with 16-byte SIMDe loads/stores (libc memcpy is often similar; this
 * keeps one place to tune for Parquet/Arrow page bodies).
 */
void hs_columnar_memcpy_fast(const uint8_t *src, uint8_t *dst, int32_t len)
{
    int i = 0;
    for (; i + 16 <= len; i += 16) {
        simde__m128i v = simde_mm_loadu_si128((const simde__m128i *)(src + i));
        simde_mm_storeu_si128((simde__m128i *)(dst + i), v);
    }
    if (i < len) {
        memcpy(dst + i, src + i, (size_t)(len - i));
    }
}
