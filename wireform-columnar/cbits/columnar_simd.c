/*
 * Columnar format helpers (Arrow validity / packed bools, Parquet PLAIN bool,
 * bitmap popcount). Uses SIMDe for portable SIMD on x86 and ARM.
 *
 * Mirrors the style of fast_decode.c: hot loops in C, thin Haskell FFI.
 */

#include <stdint.h>
#include <string.h>

#include <simde/x86/sse2.h>
#include <simde/x86/ssse3.h>

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

/*
 * Fast ASCII validity check: returns 1 if every byte in buf is < 0x80,
 * else 0. Uses SSE2 to OR 16 bytes at a time and check the high-bit
 * mask via movemask. Roughly memory-bandwidth-bound; ~2-3x faster than
 * a Word64-chunked Haskell loop on cache-warm inputs and ~10x faster
 * than BS.all (< 0x80).
 */
int32_t hs_columnar_is_ascii(const uint8_t *buf, int32_t len)
{
    simde__m128i acc = simde_mm_setzero_si128();
    int i = 0;

    /* 64-byte chunks: 4 vectors per iter, OR them together. Lets the
     * pipeline keep more loads in flight than a tight 16-byte loop. */
    for (; i + 64 <= len; i += 64) {
        simde__m128i v0 = simde_mm_loadu_si128((const simde__m128i *)(buf + i));
        simde__m128i v1 = simde_mm_loadu_si128((const simde__m128i *)(buf + i + 16));
        simde__m128i v2 = simde_mm_loadu_si128((const simde__m128i *)(buf + i + 32));
        simde__m128i v3 = simde_mm_loadu_si128((const simde__m128i *)(buf + i + 48));
        simde__m128i v01 = simde_mm_or_si128(v0, v1);
        simde__m128i v23 = simde_mm_or_si128(v2, v3);
        acc = simde_mm_or_si128(acc, simde_mm_or_si128(v01, v23));
    }
    for (; i + 16 <= len; i += 16) {
        simde__m128i v = simde_mm_loadu_si128((const simde__m128i *)(buf + i));
        acc = simde_mm_or_si128(acc, v);
    }

    /* movemask gives one bit per source byte's MSB; non-zero => some byte
     * had bit 7 set => not ASCII. */
    int mask = simde_mm_movemask_epi8(acc) & 0xFFFF;
    if (mask != 0) {
        return 0;
    }

    /* Tail: <16 bytes left. Scalar OR. */
    uint8_t tail = 0;
    for (; i < len; i++) {
        tail |= buf[i];
    }
    return (tail < 0x80) ? 1 : 0;
}

/*
 * SIMD byte-swap memcpy.
 *
 * Used for the rare endianness-mismatched path on columnar reads (Arrow
 * Big-endian payload on a Little-endian host, or vice versa). The
 * common LE -> LE case goes straight through libc's memcpy and never
 * touches these kernels.
 *
 * Implements the swap with SSSE3 pshufb: one shuffle mask handles
 * 4 x 32-bit (or 2 x 64-bit) values per 128-bit register. Tail is
 * handled scalarly with __builtin_bswap{32,64}.
 *
 * @n_elems is the number of source elements (NOT bytes).
 */
void hs_columnar_bswap32_copy(const uint8_t *src, uint8_t *dst, int32_t n_elems)
{
    /* Reverse bytes within each 32-bit lane: 0123 4567 89AB CDEF
     *                                     -> 3210 7654 BA98 FEDC */
    const simde__m128i mask = simde_mm_set_epi8(
        12, 13, 14, 15,
         8,  9, 10, 11,
         4,  5,  6,  7,
         0,  1,  2,  3
    );

    int32_t i = 0;
    int32_t total_bytes = n_elems * 4;

    for (; i + 16 <= total_bytes; i += 16) {
        simde__m128i v = simde_mm_loadu_si128((const simde__m128i *)(src + i));
        simde__m128i s = simde_mm_shuffle_epi8(v, mask);
        simde_mm_storeu_si128((simde__m128i *)(dst + i), s);
    }
    /* Tail: 0..3 remaining 32-bit words. */
    for (; i + 4 <= total_bytes; i += 4) {
        uint32_t w;
        memcpy(&w, src + i, 4);
        w = __builtin_bswap32(w);
        memcpy(dst + i, &w, 4);
    }
}

void hs_columnar_bswap64_copy(const uint8_t *src, uint8_t *dst, int32_t n_elems)
{
    /* Reverse bytes within each 64-bit lane. */
    const simde__m128i mask = simde_mm_set_epi8(
         8,  9, 10, 11, 12, 13, 14, 15,
         0,  1,  2,  3,  4,  5,  6,  7
    );

    int32_t i = 0;
    int32_t total_bytes = n_elems * 8;

    for (; i + 16 <= total_bytes; i += 16) {
        simde__m128i v = simde_mm_loadu_si128((const simde__m128i *)(src + i));
        simde__m128i s = simde_mm_shuffle_epi8(v, mask);
        simde_mm_storeu_si128((simde__m128i *)(dst + i), s);
    }
    /* Tail: 0 or 1 remaining 64-bit word. */
    if (i + 8 <= total_bytes) {
        uint64_t w;
        memcpy(&w, src + i, 8);
        w = __builtin_bswap64(w);
        memcpy(dst + i, &w, 8);
    }
}

void hs_columnar_bswap16_copy(const uint8_t *src, uint8_t *dst, int32_t n_elems)
{
    /* Reverse bytes within each 16-bit lane. */
    const simde__m128i mask = simde_mm_set_epi8(
        14, 15, 12, 13, 10, 11,  8,  9,
         6,  7,  4,  5,  2,  3,  0,  1
    );

    int32_t i = 0;
    int32_t total_bytes = n_elems * 2;

    for (; i + 16 <= total_bytes; i += 16) {
        simde__m128i v = simde_mm_loadu_si128((const simde__m128i *)(src + i));
        simde__m128i s = simde_mm_shuffle_epi8(v, mask);
        simde_mm_storeu_si128((simde__m128i *)(dst + i), s);
    }
    for (; i + 2 <= total_bytes; i += 2) {
        uint16_t w;
        memcpy(&w, src + i, 2);
        w = (uint16_t)((w >> 8) | (w << 8));
        memcpy(dst + i, &w, 2);
    }
}
