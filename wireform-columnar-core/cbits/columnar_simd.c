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
 * Expand packed bits to dst[i] in {0,1}. Bit order matches Arrow / Parquet
 * PLAIN bool: least-significant bit of each byte is the first logical value in
 * that byte (index i: byte i/8, bit i%8).
 */
void hs_columnar_unpack_bits_lsb(const uint8_t *src, int32_t n, uint8_t *dst)
{
    static uint8_t expand[256][8];
    static int init = 0;
    if (!init) {
        int b;
        for (b = 0; b < 256; b++) {
            int k;
            for (k = 0; k < 8; k++) {
                expand[b][k] = (uint8_t)((b >> k) & 1);
            }
        }
        init = 1;
    }

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
