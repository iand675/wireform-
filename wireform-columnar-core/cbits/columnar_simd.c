/*
 * Columnar format helpers (Arrow validity / packed bools, Parquet PLAIN bool,
 * bitmap popcount, Arrow buffer validators, bit copies and gathers). Uses
 * SIMDe for portable SIMD on x86 and ARM.
 *
 * Mirrors the style of fast_decode.c: hot loops in C, thin Haskell FFI.
 *
 * Every pointer argument may be arbitrarily aligned: multi-byte values are
 * always read and written through memcpy. Bitmaps are LSB-first (bit k is
 * (buf[k >> 3] >> (k & 7)) & 1) and are read a 64-bit little-endian word at
 * a time where possible, never touching bytes outside the requested range.
 *
 * Validators return -1 when the input is valid, else the index of the first
 * failing element.
 */

#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include <simde/x86/sse2.h>

/* ------------------------------------------------------------------------ */
/* Unaligned loads and stores                                               */
/* ------------------------------------------------------------------------ */

#define DEF_LD(NAME, T)                                                       \
    static inline T NAME(const void *base, size_t i)                          \
    {                                                                         \
        T v;                                                                  \
        memcpy(&v, (const uint8_t *)base + i * sizeof(T), sizeof(T));        \
        return v;                                                             \
    }

#define DEF_ST(NAME, T)                                                       \
    static inline void NAME(void *base, size_t i, T v)                        \
    {                                                                         \
        memcpy((uint8_t *)base + i * sizeof(T), &v, sizeof(T));               \
    }

DEF_LD(ld_i8, int8_t)
DEF_LD(ld_i16, int16_t)
DEF_LD(ld_i32, int32_t)
DEF_LD(ld_i64, int64_t)
DEF_LD(ld_u8, uint8_t)
DEF_LD(ld_u16, uint16_t)
DEF_LD(ld_u32, uint32_t)
DEF_LD(ld_u64, uint64_t)
DEF_ST(st_i32, int32_t)
DEF_ST(st_i64, int64_t)

static inline uint64_t ld_le64(const uint8_t *p)
{
    uint64_t v;
    memcpy(&v, p, 8);
#if defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    v = __builtin_bswap64(v);
#endif
    return v;
}

static inline void st_le64(uint8_t *p, uint64_t v)
{
#if defined(__BYTE_ORDER__) && __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    v = __builtin_bswap64(v);
#endif
    memcpy(p, &v, 8);
}

/* Store the low nbytes (0..8) bytes of v, little endian. */
static inline void st_le_bytes(uint8_t *p, uint64_t v, size_t nbytes)
{
    if (nbytes == 8) {
        st_le64(p, v);
        return;
    }
    for (size_t j = 0; j < nbytes; j++) {
        p[j] = (uint8_t)(v >> (8 * j));
    }
}

static inline size_t min_sz(size_t a, size_t b) { return a < b ? a : b; }

static inline uint64_t low_mask(size_t nbits)
{
    return nbits >= 64 ? ~UINT64_C(0) : (UINT64_C(1) << nbits) - 1;
}

/* ------------------------------------------------------------------------ */
/* Bit loads                                                                */
/* ------------------------------------------------------------------------ */

static inline unsigned get_bit(const uint8_t *buf, size_t k)
{
    return ((unsigned)buf[k >> 3] >> (k & 7)) & 1u;
}

/* The 64 bits [bitoff, bitoff + 64). Reads 8 bytes, or 9 when bitoff is not
 * byte aligned (the ninth byte holds the last bits of the range). */
static inline uint64_t load_bits64(const uint8_t *buf, size_t bitoff)
{
    const uint8_t *p = buf + (bitoff >> 3);
    unsigned s = (unsigned)(bitoff & 7);
    uint64_t v = ld_le64(p) >> s;
    if (s) {
        v |= (uint64_t)p[8] << (64 - s);
    }
    return v;
}

/* nbits (0..64) bits starting at bitoff, upper bits zero. Reads only the
 * bytes that cover the range. */
static inline uint64_t load_bits(const uint8_t *buf, size_t bitoff, size_t nbits)
{
    if (nbits == 0) {
        return 0;
    }
    if (nbits == 64) {
        return load_bits64(buf, bitoff);
    }
    const uint8_t *p = buf + (bitoff >> 3);
    unsigned s = (unsigned)(bitoff & 7);
    size_t nbytes = (s + nbits + 7) >> 3;
    uint64_t v;
    if (nbytes >= 8) {
        v = ld_le64(p) >> s;
        if (nbytes == 9) {
            v |= (uint64_t)p[8] << (64 - s);
        }
    } else {
        v = 0;
        for (size_t j = 0; j < nbytes; j++) {
            v |= (uint64_t)p[j] << (8 * j);
        }
        v >>= s;
    }
    return v & low_mask(nbits);
}

/* Validity mask for rows [i, i + m), m <= 64. valid == NULL means all set. */
static inline uint64_t valid_mask(const uint8_t *valid, size_t validoff, size_t i, size_t m)
{
    return valid ? load_bits(valid, validoff + i, m) : low_mask(m);
}

/* First row in [0, n) whose validity bit is set, or -1. */
static int64_t first_valid(size_t n, const uint8_t *valid, size_t validoff)
{
    for (size_t i = 0; i < n; i += 64) {
        uint64_t w = valid_mask(valid, validoff, i, min_sz(64, n - i));
        if (w) {
            return (int64_t)(i + (size_t)__builtin_ctzll(w));
        }
    }
    return -1;
}

/* ------------------------------------------------------------------------ */
/* Existing helpers                                                         */
/* ------------------------------------------------------------------------ */

/*
 * Population count of all bits in buf[0..len). Uses 64-bit popcount builtins
 * on the main loop.
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
 *
 * No lookup table: this used to fill a function-local `static` table on
 * first call behind a plain `int` flag. Concurrent unsafe ccalls from
 * several capabilities raced on that initialisation, and on weakly ordered
 * CPUs (aarch64) a caller could see the flag set before the table writes
 * and expand garbage. Shifting is as fast and has no shared state.
 */
void hs_columnar_unpack_bits_lsb(const uint8_t *src, int32_t n, uint8_t *dst)
{
    int32_t pos = 0;
    for (; pos + 8 <= n; pos += 8) {
        const uint8_t b = src[pos >> 3];
        dst[pos + 0] = (uint8_t)(b & 1);
        dst[pos + 1] = (uint8_t)((b >> 1) & 1);
        dst[pos + 2] = (uint8_t)((b >> 2) & 1);
        dst[pos + 3] = (uint8_t)((b >> 3) & 1);
        dst[pos + 4] = (uint8_t)((b >> 4) & 1);
        dst[pos + 5] = (uint8_t)((b >> 5) & 1);
        dst[pos + 6] = (uint8_t)((b >> 6) & 1);
        dst[pos + 7] = (uint8_t)((b >> 7) & 1);
    }
    for (; pos < n; pos++) {
        dst[pos] = (uint8_t)((src[pos >> 3] >> (pos & 7)) & 1);
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

/* ------------------------------------------------------------------------ */
/* Offsets and run ends                                                     */
/* ------------------------------------------------------------------------ */

/* Rows are scanned in blocks with a branch-free "any failure" flag; only a
 * block that contains a failure is rescanned to find the exact index. */
#define VALIDATOR_BLOCK 256

/*
 * Offsets: valid iff n == 0, or offs[0] >= 0, offs is non-decreasing and
 * offs[n-1] <= limit.
 */
#define DEF_OFFSETS(NAME, T, LD)                                              \
    int64_t NAME(const T *offs, size_t n, int64_t limit)                      \
    {                                                                         \
        if (n == 0) {                                                         \
            return -1;                                                        \
        }                                                                     \
        if (LD(offs, 0) < 0) {                                                \
            return 0;                                                         \
        }                                                                     \
        for (size_t i = 1; i < n; i += VALIDATOR_BLOCK) {                     \
            size_t m = min_sz(VALIDATOR_BLOCK, n - i);                        \
            unsigned bad = 0;                                                 \
            for (size_t j = 0; j < m; j++) {                                  \
                bad |= (unsigned)(LD(offs, i + j) < LD(offs, i + j - 1));     \
            }                                                                 \
            if (bad) {                                                        \
                for (size_t j = 0; j < m; j++) {                              \
                    if (LD(offs, i + j) < LD(offs, i + j - 1)) {              \
                        return (int64_t)(i + j);                              \
                    }                                                         \
                }                                                             \
            }                                                                 \
        }                                                                     \
        if ((int64_t)LD(offs, n - 1) > limit) {                               \
            return (int64_t)(n - 1);                                          \
        }                                                                     \
        return -1;                                                            \
    }

DEF_OFFSETS(hs_columnar_offsets_i32, int32_t, ld_i32)
DEF_OFFSETS(hs_columnar_offsets_i64, int64_t, ld_i64)

/*
 * Run ends: valid iff ends[0] > 0, ends is strictly increasing and
 * ends[n-1] >= minEnd. n == 0 is valid iff minEnd <= 0.
 */
#define DEF_RUN_ENDS(NAME, T, LD)                                             \
    int64_t NAME(const T *ends, size_t n, int64_t minEnd)                     \
    {                                                                         \
        if (n == 0) {                                                         \
            return minEnd <= 0 ? -1 : 0;                                      \
        }                                                                     \
        if (LD(ends, 0) <= 0) {                                               \
            return 0;                                                         \
        }                                                                     \
        for (size_t i = 1; i < n; i += VALIDATOR_BLOCK) {                     \
            size_t m = min_sz(VALIDATOR_BLOCK, n - i);                        \
            unsigned bad = 0;                                                 \
            for (size_t j = 0; j < m; j++) {                                  \
                bad |= (unsigned)(LD(ends, i + j) <= LD(ends, i + j - 1));    \
            }                                                                 \
            if (bad) {                                                        \
                for (size_t j = 0; j < m; j++) {                              \
                    if (LD(ends, i + j) <= LD(ends, i + j - 1)) {             \
                        return (int64_t)(i + j);                              \
                    }                                                         \
                }                                                             \
            }                                                                 \
        }                                                                     \
        if ((int64_t)LD(ends, n - 1) < minEnd) {                              \
            return (int64_t)(n - 1);                                          \
        }                                                                     \
        return -1;                                                            \
    }

DEF_RUN_ENDS(hs_columnar_run_ends_i16, int16_t, ld_i16)
DEF_RUN_ENDS(hs_columnar_run_ends_i32, int32_t, ld_i32)
DEF_RUN_ENDS(hs_columnar_run_ends_i64, int64_t, ld_i64)

/*
 * Character boundaries. Precondition: offsets already validated against len,
 * so 0 <= offs[i] <= len. An offset below len must not point at a UTF-8
 * continuation byte (0x80..0xBF).
 */
#define DEF_UTF8_BOUNDARIES(NAME, T, LD)                                      \
    int64_t NAME(const T *offs, size_t n, const uint8_t *data, int64_t len)   \
    {                                                                         \
        if (len <= 0) {                                                       \
            return -1;                                                        \
        }                                                                     \
        for (size_t i = 0; i < n; i += VALIDATOR_BLOCK) {                     \
            size_t m = min_sz(VALIDATOR_BLOCK, n - i);                        \
            unsigned bad = 0;                                                 \
            for (size_t j = 0; j < m; j++) {                                  \
                int64_t o = (int64_t)LD(offs, i + j);                         \
                unsigned in = (unsigned)(o < len);                            \
                uint8_t c = data[in ? o : 0];                                 \
                bad |= in & (unsigned)((c & 0xC0) == 0x80);                   \
            }                                                                 \
            if (bad) {                                                        \
                for (size_t j = 0; j < m; j++) {                              \
                    int64_t o = (int64_t)LD(offs, i + j);                     \
                    if (o < len && (data[o] & 0xC0) == 0x80) {                \
                        return (int64_t)(i + j);                              \
                    }                                                         \
                }                                                             \
            }                                                                 \
        }                                                                     \
        return -1;                                                            \
    }

DEF_UTF8_BOUNDARIES(hs_columnar_utf8_boundaries_i32, int32_t, ld_i32)
DEF_UTF8_BOUNDARIES(hs_columnar_utf8_boundaries_i64, int64_t, ld_i64)

/* dst[i] = src[i] + delta (wrapping). dst may equal src. */
void hs_columnar_rebase_offsets_i32(int32_t *dst, const int32_t *src, size_t n, int64_t delta)
{
    uint32_t d = (uint32_t)(uint64_t)delta;
    for (size_t i = 0; i < n; i++) {
        st_i32(dst, i, (int32_t)((uint32_t)ld_i32(src, i) + d));
    }
}

void hs_columnar_rebase_offsets_i64(int64_t *dst, const int64_t *src, size_t n, int64_t delta)
{
    for (size_t i = 0; i < n; i++) {
        st_i64(dst, i, (int64_t)((uint64_t)ld_i64(src, i) + (uint64_t)delta));
    }
}

/* ------------------------------------------------------------------------ */
/* Bitmaps                                                                  */
/* ------------------------------------------------------------------------ */

/* Number of set bits among bits [bitoff, bitoff + nbits) of buf. */
int64_t hs_columnar_popcount_bits(const uint8_t *buf, size_t bitoff, size_t nbits)
{
    if (nbits == 0) {
        return 0;
    }
    int64_t total = 0;
    const uint8_t *p = buf + (bitoff >> 3);
    unsigned s = (unsigned)(bitoff & 7);
    if (s) {
        size_t take = min_sz(8 - s, nbits);
        unsigned b = ((unsigned)p[0] >> s) & ((1u << take) - 1u);
        total += __builtin_popcount(b);
        nbits -= take;
        p++;
    }
    size_t nbytes = nbits >> 3;
    size_t i = 0;
    uint64_t t0 = 0, t1 = 0, t2 = 0, t3 = 0;
    for (; i + 32 <= nbytes; i += 32) {
        uint64_t w0, w1, w2, w3;
        memcpy(&w0, p + i, 8);
        memcpy(&w1, p + i + 8, 8);
        memcpy(&w2, p + i + 16, 8);
        memcpy(&w3, p + i + 24, 8);
        t0 += (uint64_t)__builtin_popcountll(w0);
        t1 += (uint64_t)__builtin_popcountll(w1);
        t2 += (uint64_t)__builtin_popcountll(w2);
        t3 += (uint64_t)__builtin_popcountll(w3);
    }
    total += (int64_t)(t0 + t1 + t2 + t3);
    for (; i + 8 <= nbytes; i += 8) {
        uint64_t w;
        memcpy(&w, p + i, 8);
        total += __builtin_popcountll(w);
    }
    for (; i < nbytes; i++) {
        total += __builtin_popcount((unsigned)p[i]);
    }
    unsigned rem = (unsigned)(nbits & 7);
    if (rem) {
        total += __builtin_popcount((unsigned)p[nbytes] & ((1u << rem) - 1u));
    }
    return total;
}

/*
 * Copy nbits bits from src at bit srcoff into dst at bit dstoff, preserving
 * the bits of dst outside [dstoff, dstoff + nbits). src and dst must not
 * overlap.
 */
void hs_columnar_copy_bits(uint8_t *dst, size_t dstoff, const uint8_t *src, size_t srcoff, size_t nbits)
{
    if (nbits == 0) {
        return;
    }
    uint8_t *d = dst + (dstoff >> 3);
    unsigned ds = (unsigned)(dstoff & 7);
    unsigned ss = (unsigned)(srcoff & 7);

    if (ds == ss) {
        /* Same phase: patch the head byte, memcpy the middle, patch the tail. */
        const uint8_t *s = src + (srcoff >> 3);
        if (ds) {
            size_t take = min_sz(8 - ds, nbits);
            unsigned mask = ((1u << take) - 1u) << ds;
            d[0] = (uint8_t)((d[0] & ~mask) | (s[0] & mask));
            nbits -= take;
            d++;
            s++;
        }
        size_t nb = nbits >> 3;
        memcpy(d, s, nb);
        unsigned rem = (unsigned)(nbits & 7);
        if (rem) {
            unsigned mask = (1u << rem) - 1u;
            d[nb] = (uint8_t)((d[nb] & ~mask) | (s[nb] & mask));
        }
        return;
    }

    size_t sbit = srcoff;
    if (ds) {
        /* Bring dst to a byte boundary. */
        size_t take = min_sz(8 - ds, nbits);
        unsigned v = (unsigned)load_bits(src, sbit, take);
        unsigned mask = ((1u << take) - 1u) << ds;
        d[0] = (uint8_t)((d[0] & ~mask) | ((v << ds) & mask));
        nbits -= take;
        sbit += take;
        d++;
    }
    /* dst is byte aligned and src is not (the phases differ): shift words. */
    const uint8_t *s = src + (sbit >> 3);
    unsigned k = (unsigned)(sbit & 7);
    while (nbits >= 64) {
        uint64_t v = (ld_le64(s) >> k) | ((uint64_t)s[8] << (64 - k));
        st_le64(d, v);
        d += 8;
        s += 8;
        nbits -= 64;
    }
    sbit = (size_t)k;
    if (nbits) {
        uint64_t v = load_bits(s, sbit, nbits);
        size_t full = nbits >> 3;
        st_le_bytes(d, v, full);
        unsigned rem = (unsigned)(nbits & 7);
        if (rem) {
            unsigned mask = (1u << rem) - 1u;
            unsigned last = (unsigned)(v >> (8 * full)) & mask;
            d[full] = (uint8_t)((d[full] & ~mask) | last);
        }
    }
}

/*
 * dst (from bit 0) = bits [aoff, aoff + nbits) of a AND bits [boff, boff +
 * nbits) of b. Trailing bits of the last byte are zeroed. Returns the number
 * of set bits.
 */
int64_t hs_columnar_and_bits(uint8_t *dst, const uint8_t *a, size_t aoff, const uint8_t *b, size_t boff, size_t nbits)
{
    int64_t total = 0;
    size_t i = 0;
    for (; i + 64 <= nbits; i += 64) {
        uint64_t r = load_bits64(a, aoff + i) & load_bits64(b, boff + i);
        st_le64(dst + (i >> 3), r);
        total += __builtin_popcountll(r);
    }
    if (i < nbits) {
        size_t m = nbits - i;
        uint64_t r = load_bits(a, aoff + i, m) & load_bits(b, boff + i, m);
        st_le_bytes(dst + (i >> 3), r, (m + 7) >> 3);
        total += __builtin_popcountll(r);
    }
    return total;
}

/*
 * dst bit k = src bit (srcoff + idx[k]) for k in [0, n); trailing bits of the
 * last byte are zeroed. Returns the number of set bits written.
 */
int64_t hs_columnar_gather_bits(uint8_t *dst, const uint8_t *src, size_t srcoff, const int64_t *idx, size_t n)
{
    int64_t total = 0;
    for (size_t k = 0; k < n; k += 64) {
        size_t m = min_sz(64, n - k);
        uint64_t w = 0;
        for (size_t j = 0; j < m; j++) {
            size_t bit = srcoff + (size_t)ld_i64(idx, k + j);
            w |= (uint64_t)get_bit(src, bit) << j;
        }
        st_le_bytes(dst + (k >> 3), w, (m + 7) >> 3);
        total += __builtin_popcountll(w);
    }
    return total;
}

/* ------------------------------------------------------------------------ */
/* Dictionary keys, list views, dense unions                                */
/* ------------------------------------------------------------------------ */

/*
 * For every valid row: 0 <= keys[i] < max.
 *
 * The test runs at the key's own width: key i is in range iff
 * (UT)keys[i] < lim, where lim is max clamped to the type. A negative
 * signed key casts to an unsigned value >= 2^(bits-1) >= lim, so it fails;
 * an unsigned 64-bit key >= 2^63 is >= any int64 max, so it fails too. When
 * max exceeds every value an unsigned key type can hold, all keys pass.
 *
 * Each 64-row block is first checked with a branch-free OR of the compares,
 * which the compiler vectorises (no validity, no per-row shift). Only a
 * block that contains an out-of-range value is rescanned exactly with its
 * validity mask, because null slots may hold any key.
 */
#define DEF_KEYS(NAME, T, UT, IS_SIGNED)                                      \
    static inline int NAME##_block_hit(const uint8_t *p, size_t m, UT lim)    \
    {                                                                         \
        UT hit = 0;                                                           \
        for (size_t j = 0; j < m; j++) {                                      \
            UT k;                                                             \
            memcpy(&k, p + j * sizeof(UT), sizeof(UT));                       \
            hit |= (UT)(k >= lim);                                            \
        }                                                                     \
        return hit != 0;                                                      \
    }                                                                         \
                                                                              \
    int64_t NAME(const T *keys, size_t n, const uint8_t *valid,               \
                 size_t validoff, int64_t max)                                \
    {                                                                         \
        if (max <= 0) {                                                       \
            return first_valid(n, valid, validoff);                           \
        }                                                                     \
        const unsigned bits = (unsigned)(sizeof(T) * 8);                      \
        uint64_t limv = (uint64_t)max;                                        \
        if (IS_SIGNED) {                                                      \
            const uint64_t top = (uint64_t)1 << (bits - 1);                   \
            if (limv > top) {                                                 \
                limv = top;                                                   \
            }                                                                 \
        } else if (bits < 64 && limv > (((uint64_t)1 << bits) - 1)) {         \
            return -1;                                                        \
        }                                                                     \
        const UT lim = (UT)limv;                                              \
        const uint8_t *p = (const uint8_t *)keys;                             \
        for (size_t i = 0; i < n; i += 64) {                                  \
            size_t m = min_sz(64, n - i);                                     \
            if (!NAME##_block_hit(p + i * sizeof(T), m, lim)) {               \
                continue;                                                     \
            }                                                                 \
            uint64_t vm = valid_mask(valid, validoff, i, m);                  \
            uint64_t bad = 0;                                                 \
            for (size_t j = 0; j < m; j++) {                                  \
                UT k;                                                         \
                memcpy(&k, p + (i + j) * sizeof(T), sizeof(T));               \
                bad |= (uint64_t)(k >= lim) << j;                             \
            }                                                                 \
            bad &= vm;                                                        \
            if (bad) {                                                        \
                return (int64_t)(i + (size_t)__builtin_ctzll(bad));           \
            }                                                                 \
        }                                                                     \
        return -1;                                                            \
    }

DEF_KEYS(hs_columnar_keys_in_range_i8, int8_t, uint8_t, 1)
DEF_KEYS(hs_columnar_keys_in_range_i16, int16_t, uint16_t, 1)
DEF_KEYS(hs_columnar_keys_in_range_i32, int32_t, uint32_t, 1)
DEF_KEYS(hs_columnar_keys_in_range_i64, int64_t, uint64_t, 1)
DEF_KEYS(hs_columnar_keys_in_range_u8, uint8_t, uint8_t, 0)
DEF_KEYS(hs_columnar_keys_in_range_u16, uint16_t, uint16_t, 0)
DEF_KEYS(hs_columnar_keys_in_range_u32, uint32_t, uint32_t, 0)
DEF_KEYS(hs_columnar_keys_in_range_u64, uint64_t, uint64_t, 0)

/*
 * List view: for every valid row, offs[i] >= 0, sizes[i] >= 0 and
 * offs[i] + sizes[i] <= childLen (checked without overflow).
 */
#define DEF_LIST_VIEW(NAME, T, LD)                                            \
    int64_t NAME(const T *offs, const T *sizes, size_t n,                     \
                 const uint8_t *valid, size_t validoff, int64_t childLen)     \
    {                                                                         \
        for (size_t i = 0; i < n; i += 64) {                                  \
            size_t m = min_sz(64, n - i);                                     \
            uint64_t vm = valid_mask(valid, validoff, i, m);                  \
            if (!vm) {                                                        \
                continue;                                                     \
            }                                                                 \
            uint64_t bad = 0;                                                 \
            for (size_t j = 0; j < m; j++) {                                  \
                int64_t o = (int64_t)LD(offs, i + j);                         \
                int64_t s = (int64_t)LD(sizes, i + j);                        \
                unsigned b = (unsigned)(o < 0) | (unsigned)(s < 0) |          \
                             (unsigned)(o > childLen);                        \
                /* childLen - o cannot overflow once 0 <= o <= childLen. */   \
                b |= !b && s > childLen - o;                                  \
                bad |= (uint64_t)b << j;                                      \
            }                                                                 \
            bad &= vm;                                                        \
            if (bad) {                                                        \
                return (int64_t)(i + (size_t)__builtin_ctzll(bad));           \
            }                                                                 \
        }                                                                     \
        return -1;                                                            \
    }

DEF_LIST_VIEW(hs_columnar_list_view_i32, int32_t, ld_i32)
DEF_LIST_VIEW(hs_columnar_list_view_i64, int64_t, ld_i64)

/* Dense union: 0 <= types[i] < nchildren and 0 <= offs[i] < childLens[types[i]]. */
int64_t hs_columnar_dense_union(const int8_t *types, const int32_t *offs, size_t n, const int64_t *childLens, size_t nchildren)
{
    for (size_t i = 0; i < n; i++) {
        int t = ld_i8(types, i);
        if (t < 0 || (size_t)t >= nchildren) {
            return (int64_t)i;
        }
        int64_t o = ld_i32(offs, i);
        if (o < 0 || o >= ld_i64(childLens, (size_t)t)) {
            return (int64_t)i;
        }
    }
    return -1;
}

/* ------------------------------------------------------------------------ */
/* Binary / UTF-8 views                                                     */
/* ------------------------------------------------------------------------ */

/* Strict UTF-8: rejects overlongs, surrogates, code points above U+10FFFF and
 * truncated sequences. Returns 1 when s[0..len) is valid. */
static int utf8_valid(const uint8_t *s, size_t len)
{
    size_t i = 0;
    while (i < len) {
        if (i + 8 <= len) {
            uint64_t w;
            memcpy(&w, s + i, 8);
            if (!(w & UINT64_C(0x8080808080808080))) {
                i += 8;
                continue;
            }
        }
        unsigned c = s[i];
        if (c < 0x80) {
            i++;
        } else if (c < 0xC2) {
            /* stray continuation byte, or overlong C0 / C1 lead */
            return 0;
        } else if (c < 0xE0) {
            if (i + 1 >= len || (s[i + 1] & 0xC0) != 0x80) {
                return 0;
            }
            i += 2;
        } else if (c < 0xF0) {
            if (i + 2 >= len) {
                return 0;
            }
            unsigned c1 = s[i + 1], c2 = s[i + 2];
            if ((c1 & 0xC0) != 0x80 || (c2 & 0xC0) != 0x80) {
                return 0;
            }
            if (c == 0xE0 && c1 < 0xA0) {
                return 0; /* overlong */
            }
            if (c == 0xED && c1 >= 0xA0) {
                return 0; /* surrogate D800..DFFF */
            }
            i += 3;
        } else if (c < 0xF5) {
            if (i + 3 >= len) {
                return 0;
            }
            unsigned c1 = s[i + 1], c2 = s[i + 2], c3 = s[i + 3];
            if ((c1 & 0xC0) != 0x80 || (c2 & 0xC0) != 0x80 || (c3 & 0xC0) != 0x80) {
                return 0;
            }
            if (c == 0xF0 && c1 < 0x90) {
                return 0; /* overlong */
            }
            if (c == 0xF4 && c1 >= 0x90) {
                return 0; /* above U+10FFFF */
            }
            i += 4;
        } else {
            return 0;
        }
    }
    return 1;
}

/*
 * Utf8View / BinaryView: 16 bytes per view. int32 length at byte 0; a length
 * of at most 12 is stored inline at bytes 4..4+len, otherwise bytes 4..8 are
 * a prefix, the int32 at 8 a buffer index and the int32 at 12 an offset into
 * that buffer. Only valid rows are checked.
 */
int64_t hs_columnar_view_refs(const uint8_t *views, size_t n, const uint8_t *valid, size_t validoff, const uint8_t *const *bufs, const int64_t *buflens, size_t nbufs, int utf8)
{
    for (size_t i = 0; i < n; i++) {
        if (valid && !get_bit(valid, validoff + i)) {
            continue;
        }
        const uint8_t *v = views + 16 * i;
        int32_t len = ld_i32(v, 0);
        if (len < 0) {
            return (int64_t)i;
        }
        if (len <= 12) {
            if (utf8 && !utf8_valid(v + 4, (size_t)len)) {
                return (int64_t)i;
            }
            continue;
        }
        int32_t bi = ld_i32(v + 8, 0);
        int32_t off = ld_i32(v + 12, 0);
        if (bi < 0 || (size_t)bi >= nbufs || off < 0) {
            return (int64_t)i;
        }
        if ((int64_t)off + (int64_t)len > ld_i64(buflens, (size_t)bi)) {
            return (int64_t)i;
        }
        const uint8_t *buf;
        memcpy(&buf, (const uint8_t *)bufs + (size_t)bi * sizeof(buf), sizeof(buf));
        const uint8_t *payload = buf + off;
        if (memcmp(v + 4, payload, 4) != 0) {
            return (int64_t)i;
        }
        if (utf8 && !utf8_valid(payload, (size_t)len)) {
            return (int64_t)i;
        }
    }
    return -1;
}

/* ------------------------------------------------------------------------ */
/* Gathers and takes (indices validated by the caller)                      */
/* ------------------------------------------------------------------------ */

#define DEF_GATHER(NAME, W)                                                   \
    void NAME(uint8_t *dst, const uint8_t *src, const int64_t *idx, size_t n) \
    {                                                                         \
        for (size_t k = 0; k < n; k++) {                                      \
            memcpy(dst + k * (W), src + (size_t)ld_i64(idx, k) * (W), (W));   \
        }                                                                     \
    }

DEF_GATHER(hs_columnar_gather_1, 1)
DEF_GATHER(hs_columnar_gather_2, 2)
DEF_GATHER(hs_columnar_gather_4, 4)
DEF_GATHER(hs_columnar_gather_8, 8)
DEF_GATHER(hs_columnar_gather_16, 16)

/*
 * Var-length take, pass 1: dstOffs[0] = 0 and dstOffs[k + 1] = dstOffs[k] +
 * length of row idx[k]. Returns the total length, or -1 when it does not fit
 * the offset type.
 */
int64_t hs_columnar_take_offsets_i32(int32_t *dstOffs, const int32_t *srcOffs, const int64_t *idx, size_t n)
{
    int64_t acc = 0;
    st_i32(dstOffs, 0, 0);
    for (size_t k = 0; k < n; k++) {
        size_t j = (size_t)ld_i64(idx, k);
        acc += (int64_t)ld_i32(srcOffs, j + 1) - (int64_t)ld_i32(srcOffs, j);
        if (acc > INT32_MAX) {
            return -1;
        }
        st_i32(dstOffs, k + 1, (int32_t)acc);
    }
    return acc;
}

int64_t hs_columnar_take_offsets_i64(int64_t *dstOffs, const int64_t *srcOffs, const int64_t *idx, size_t n)
{
    int64_t acc = 0;
    st_i64(dstOffs, 0, 0);
    for (size_t k = 0; k < n; k++) {
        size_t j = (size_t)ld_i64(idx, k);
        int64_t len;
        if (__builtin_sub_overflow(ld_i64(srcOffs, j + 1), ld_i64(srcOffs, j), &len) ||
            __builtin_add_overflow(acc, len, &acc)) {
            return -1;
        }
        st_i64(dstOffs, k + 1, acc);
    }
    return acc;
}

/* Pass 2: concatenate the selected rows' bytes into dst. */
#define DEF_TAKE_BYTES(NAME, T, LD)                                           \
    void NAME(uint8_t *dst, const T *srcOffs, const uint8_t *src,             \
              const int64_t *idx, size_t n)                                   \
    {                                                                         \
        size_t pos = 0;                                                       \
        for (size_t k = 0; k < n; k++) {                                      \
            size_t j = (size_t)ld_i64(idx, k);                                \
            size_t a = (size_t)LD(srcOffs, j);                                \
            size_t len = (size_t)LD(srcOffs, j + 1) - a;                      \
            memcpy(dst + pos, src + a, len);                                  \
            pos += len;                                                       \
        }                                                                     \
    }

DEF_TAKE_BYTES(hs_columnar_take_bytes_i32, int32_t, ld_i32)
DEF_TAKE_BYTES(hs_columnar_take_bytes_i64, int64_t, ld_i64)
