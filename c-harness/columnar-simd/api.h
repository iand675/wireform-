/* Kernel API as wireform-columnar-core/src/Columnar/SIMD.hs imports it, the
 * caller contracts derived from the Haskell call sites, and slow scalar
 * reference implementations the tests and fuzzers compare against.
 *
 * General contract (Columnar.SIMD module docs): no pointer is assumed to be
 * aligned; bitmaps are LSB-first (bit k is (buf[k >> 3] >> (k & 7)) & 1)
 * and bit offsets may be any value; a NULL validity pointer means every row
 * is valid, otherwise row i is valid when bit validoff + i is set and the
 * bitmap covers bits [validoff, validoff + n); validators return -1 when the
 * input is valid, else the index of the first failing element; index arrays
 * (Haskell Ptr Int, 64-bit) are trusted, i.e. already bounds checked by the
 * caller. A kernel reads only the bytes that cover the ranges it is given and
 * writes only the bytes described below. Any pointer may be NULL when the
 * range it describes is empty (an empty ByteString has a NULL pointer). */
#ifndef COLUMNAR_SIMD_API_H
#define COLUMNAR_SIMD_API_H

#include <stddef.h>
#include <stdint.h>
#include <string.h>

/* ------------------------------------------------------------------ */
/* Prototypes                                                          */
/* ------------------------------------------------------------------ */

/* bitmapPopCount: set bits in buf[0, len). Haskell passes a ByteString's
 * pointer and length (any length; 0 is short-circuited in Haskell). */
int64_t hs_columnar_bitmap_popcount(const uint8_t *buf, size_t len);

/* unpackBitsLsbUnsafe (Parquet PLAIN BOOLEAN): dst[i] = bit i of src for
 * i in [0, n). Precondition: n is a non-negative bit count (Columnar.SIMD
 * imports it as CSize), src has >= (n + 7) / 8 bytes
 * (Parquet.Read.decodePlainBool checks it), dst has n bytes. */
void hs_columnar_unpack_bits_lsb(const uint8_t *src, size_t n, uint8_t *dst);

/* memcpyFast: copy len bytes src -> dst, no overlap. Arguments are
 * (src, dst, len) in that order. Precondition: len is a non-negative byte
 * count (Columnar.SIMD imports it as CSize, so there is no 2 GiB ceiling);
 * a copy needs len bytes at src and len at dst, so in practice the length
 * is bounded by the buffers, not by the type. */
void hs_columnar_memcpy_fast(const uint8_t *src, uint8_t *dst, size_t len);

/* The two kernels above were the last with a 32-bit length parameter
 * (CInt -> int32_t), which truncated a >2 GiB source. Both touch every byte
 * they are given, so unlike the sparse-offset kernels a >2 GiB runtime case
 * cannot be cheap: assert the widths at compile time instead. */
_Static_assert(
    _Generic(&hs_columnar_unpack_bits_lsb,
             void (*)(const uint8_t *, size_t, uint8_t *): 1,
             default: 0),
    "hs_columnar_unpack_bits_lsb must take a size_t bit count");
_Static_assert(
    _Generic(&hs_columnar_memcpy_fast,
             void (*)(const uint8_t *, uint8_t *, size_t): 1,
             default: 0),
    "hs_columnar_memcpy_fast must take a size_t byte count");

/* offsetsCheck: n offsets (rows + 1). Valid iff n == 0, or offs[0] >= 0,
 * offs never decreases and offs[n-1] <= limit. Reads offs[0, n). */
int64_t hs_columnar_offsets_i32(const int32_t *offs, size_t n, int64_t limit);
int64_t hs_columnar_offsets_i64(const int64_t *offs, size_t n, int64_t limit);

/* runEndsCheck: ends[0] > 0, strictly increasing, ends[n-1] >= minEnd; an
 * empty array is valid iff minEnd <= 0 (else fails at 0). */
int64_t hs_columnar_run_ends_i16(const int16_t *ends, size_t n, int64_t minEnd);
int64_t hs_columnar_run_ends_i32(const int32_t *ends, size_t n, int64_t minEnd);
int64_t hs_columnar_run_ends_i64(const int64_t *ends, size_t n, int64_t minEnd);

/* utf8Boundaries: first i with offs[i] < len and data[offs[i]] a UTF-8
 * continuation byte. Precondition: the offsets already passed offsetsCheck
 * with limit == len (Arrow.Column.Internal.validateUtf8 runs after
 * validateOffsets), so 0 <= offs[i] <= len; data has len bytes. */
int64_t hs_columnar_utf8_boundaries_i32(const int32_t *offs, size_t n, const uint8_t *data, int64_t len);
int64_t hs_columnar_utf8_boundaries_i64(const int64_t *offs, size_t n, const uint8_t *data, int64_t len);

/* rebaseOffsets: dst[i] = src[i] + delta (wrapping two's complement);
 * dst may equal src (same pointer), otherwise no overlap. */
void hs_columnar_rebase_offsets_i32(int32_t *dst, const int32_t *src, size_t n, int64_t delta);
void hs_columnar_rebase_offsets_i64(int64_t *dst, const int64_t *src, size_t n, int64_t delta);

/* popCountBits: set bits among [bitoff, bitoff + nbits). */
int64_t hs_columnar_popcount_bits(const uint8_t *buf, size_t bitoff, size_t nbits);

/* copyBits: copy src bits [srcoff, srcoff + nbits) to dst bits [dstoff, ...),
 * preserving every other dst bit. Reads and writes only the bytes covering
 * the two ranges. src and dst must not overlap. */
void hs_columnar_copy_bits(uint8_t *dst, size_t dstoff, const uint8_t *src, size_t srcoff, size_t nbits);

/* andBits: dst bits [0, nbits) = a[aoff..] AND b[boff..]; writes exactly
 * (nbits + 7) / 8 bytes, trailing bits of the last byte zeroed. Returns the
 * set-bit count. */
int64_t hs_columnar_and_bits(uint8_t *dst, const uint8_t *a, size_t aoff, const uint8_t *b, size_t boff, size_t nbits);

/* gatherBits: dst bit k = src bit (srcoff + idx[k]); writes (n + 7) / 8
 * bytes, trailing bits zeroed; returns the set-bit count. Precondition:
 * 0 <= idx[k] and bit srcoff + idx[k] lies inside src (indices trusted). */
int64_t hs_columnar_gather_bits(uint8_t *dst, const uint8_t *src, size_t srcoff, const int64_t *idx, size_t n);

/* keysInRange: every valid key satisfies 0 <= key < max (unsigned 64-bit
 * keys >= 2^63 always fail). Reads keys[0, n) and the validity bits. */
int64_t hs_columnar_keys_in_range_i8(const int8_t *keys, size_t n, const uint8_t *valid, size_t validoff, int64_t max);
int64_t hs_columnar_keys_in_range_i16(const int16_t *keys, size_t n, const uint8_t *valid, size_t validoff, int64_t max);
int64_t hs_columnar_keys_in_range_i32(const int32_t *keys, size_t n, const uint8_t *valid, size_t validoff, int64_t max);
int64_t hs_columnar_keys_in_range_i64(const int64_t *keys, size_t n, const uint8_t *valid, size_t validoff, int64_t max);
int64_t hs_columnar_keys_in_range_u8(const uint8_t *keys, size_t n, const uint8_t *valid, size_t validoff, int64_t max);
int64_t hs_columnar_keys_in_range_u16(const uint16_t *keys, size_t n, const uint8_t *valid, size_t validoff, int64_t max);
int64_t hs_columnar_keys_in_range_u32(const uint32_t *keys, size_t n, const uint8_t *valid, size_t validoff, int64_t max);
int64_t hs_columnar_keys_in_range_u64(const uint64_t *keys, size_t n, const uint8_t *valid, size_t validoff, int64_t max);

/* listViewCheck: for every valid row, offs[i] >= 0, sizes[i] >= 0 and
 * offs[i] + sizes[i] <= childLen (no overflow). */
int64_t hs_columnar_list_view_i32(const int32_t *offs, const int32_t *sizes, size_t n, const uint8_t *valid, size_t validoff, int64_t childLen);
int64_t hs_columnar_list_view_i64(const int64_t *offs, const int64_t *sizes, size_t n, const uint8_t *valid, size_t validoff, int64_t childLen);

/* denseUnionCheck: 0 <= types[i] < nchildren and
 * 0 <= offs[i] < childLens[types[i]]. childLens has nchildren entries. */
int64_t hs_columnar_dense_union(const int8_t *types, const int32_t *offs, size_t n, const int64_t *childLens, size_t nchildren);

/* viewRefsCheck: views has 16 * n bytes (validateViews checks it). For
 * every valid row: int32 len at byte 0 is >= 0; len <= 12 is inline at bytes
 * 4..4+len; otherwise bytes 4..8 are a prefix, the int32 at 8 a buffer index
 * and the int32 at 12 an offset, and the view must name an existing buffer,
 * lie inside it (bufs[k] has buflens[k] readable bytes) and match the prefix.
 * With utf8 != 0 every payload must be strict UTF-8. */
int64_t hs_columnar_view_refs(const uint8_t *views, size_t n, const uint8_t *valid, size_t validoff, const uint8_t *const *bufs, const int64_t *buflens, size_t nbufs, int utf8);

/* gatherN: dst[k] = src[idx[k]] for N-byte elements; dst has n * N bytes;
 * indices trusted in-bounds. */
void hs_columnar_gather_1(uint8_t *dst, const uint8_t *src, const int64_t *idx, size_t n);
void hs_columnar_gather_2(uint8_t *dst, const uint8_t *src, const int64_t *idx, size_t n);
void hs_columnar_gather_4(uint8_t *dst, const uint8_t *src, const int64_t *idx, size_t n);
void hs_columnar_gather_8(uint8_t *dst, const uint8_t *src, const int64_t *idx, size_t n);
void hs_columnar_gather_16(uint8_t *dst, const uint8_t *src, const int64_t *idx, size_t n);

/* takeOffsets: dstOffs (n + 1 entries) = running sum of the selected rows'
 * lengths from 0; returns the total, or -1 when it does not fit the offset
 * type (dstOffs is then partially written). Precondition: srcOffs passed
 * offsetsCheck and every idx[k] is a row of it (idx[k] + 1 < its length). */
int64_t hs_columnar_take_offsets_i32(int32_t *dstOffs, const int32_t *srcOffs, const int64_t *idx, size_t n);
int64_t hs_columnar_take_offsets_i64(int64_t *dstOffs, const int64_t *srcOffs, const int64_t *idx, size_t n);

/* takeBytes: concatenate the selected rows' bytes; dst has the total that
 * takeOffsets returned. Same preconditions as takeOffsets, and src covers
 * srcOffs' last offset. src and dst may be NULL when every selected row is
 * empty. */
void hs_columnar_take_bytes_i32(uint8_t *dst, const int32_t *srcOffs, const uint8_t *src, const int64_t *idx, size_t n);
void hs_columnar_take_bytes_i64(uint8_t *dst, const int64_t *srcOffs, const uint8_t *src, const int64_t *idx, size_t n);

/* ------------------------------------------------------------------ */
/* Scalar references                                                   */
/* ------------------------------------------------------------------ */

static inline unsigned ref_bit(const uint8_t *b, uint64_t k)
{
    return (b[k >> 3] >> (k & 7)) & 1u;
}

static inline void ref_set_bit(uint8_t *b, uint64_t k, unsigned v)
{
    b[k >> 3] = (uint8_t)((b[k >> 3] & ~(1u << (k & 7))) | ((v & 1u) << (k & 7)));
}

static inline int64_t ref_bitmap_popcount(const uint8_t *buf, size_t len)
{
    int64_t t = 0;
    for (size_t k = 0; k < 8 * len; k++) t += ref_bit(buf, k);
    return t;
}

static inline void ref_unpack_bits_lsb(const uint8_t *src, size_t n, uint8_t *dst)
{
    for (size_t i = 0; i < n; i++) dst[i] = (uint8_t)ref_bit(src, (uint64_t)i);
}

static inline int64_t ref_popcount_bits(const uint8_t *buf, size_t off, size_t n)
{
    int64_t t = 0;
    for (size_t k = 0; k < n; k++) t += ref_bit(buf, off + k);
    return t;
}

static inline void ref_copy_bits(uint8_t *dst, size_t doff, const uint8_t *src, size_t soff, size_t n)
{
    for (size_t k = 0; k < n; k++) ref_set_bit(dst, doff + k, ref_bit(src, soff + k));
}

/* dst gets (n + 7) / 8 bytes. */
static inline int64_t ref_and_bits(uint8_t *dst, const uint8_t *a, size_t aoff, const uint8_t *b, size_t boff, size_t n)
{
    int64_t t = 0;
    memset(dst, 0, (n + 7) / 8);
    for (size_t k = 0; k < n; k++) {
        unsigned v = ref_bit(a, aoff + k) & ref_bit(b, boff + k);
        ref_set_bit(dst, k, v);
        t += v;
    }
    return t;
}

static inline int64_t ref_gather_bits(uint8_t *dst, const uint8_t *src, size_t soff, const int64_t *idx, size_t n)
{
    int64_t t = 0;
    memset(dst, 0, (n + 7) / 8);
    for (size_t k = 0; k < n; k++) {
        unsigned v = ref_bit(src, soff + (uint64_t)idx[k]);
        ref_set_bit(dst, k, v);
        t += v;
    }
    return t;
}

/* Validators over values widened to int64 (the tests widen typed arrays). */
static inline int64_t ref_offsets(const int64_t *v, size_t n, int64_t limit)
{
    if (n == 0) return -1;
    if (v[0] < 0) return 0;
    for (size_t i = 1; i < n; i++) {
        if (v[i] < v[i - 1]) return (int64_t)i;
    }
    return v[n - 1] > limit ? (int64_t)(n - 1) : -1;
}

static inline int64_t ref_run_ends(const int64_t *v, size_t n, int64_t minEnd)
{
    if (n == 0) return minEnd <= 0 ? -1 : 0;
    if (v[0] <= 0) return 0;
    for (size_t i = 1; i < n; i++) {
        if (v[i] <= v[i - 1]) return (int64_t)i;
    }
    return v[n - 1] < minEnd ? (int64_t)(n - 1) : -1;
}

static inline int64_t ref_utf8_boundaries(const int64_t *v, size_t n, const uint8_t *data, int64_t len)
{
    for (size_t i = 0; i < n; i++) {
        if (v[i] >= 0 && v[i] < len && (data[v[i]] & 0xC0) == 0x80) return (int64_t)i;
    }
    return -1;
}

static inline unsigned ref_valid(const uint8_t *valid, size_t validoff, size_t i)
{
    return valid ? ref_bit(valid, validoff + i) : 1u;
}

/* is_signed: keys hold int64 values; else uint64 values in ukeys. */
static inline int64_t ref_keys(const int64_t *skeys, const uint64_t *ukeys, size_t n,
                               const uint8_t *valid, size_t validoff, int64_t max)
{
    for (size_t i = 0; i < n; i++) {
        if (!ref_valid(valid, validoff, i)) continue;
        int ok;
        if (skeys) ok = skeys[i] >= 0 && skeys[i] < max;
        else ok = max > 0 && ukeys[i] < (uint64_t)max;
        if (!ok) return (int64_t)i;
    }
    return -1;
}

static inline int64_t ref_list_view(const int64_t *o, const int64_t *s, size_t n,
                                    const uint8_t *valid, size_t validoff, int64_t childLen)
{
    for (size_t i = 0; i < n; i++) {
        if (!ref_valid(valid, validoff, i)) continue;
        if (o[i] < 0 || s[i] < 0 || (__int128)o[i] + s[i] > childLen) return (int64_t)i;
    }
    return -1;
}

static inline int64_t ref_dense_union(const int8_t *types, const int32_t *offs, size_t n,
                                      const int64_t *childLens, size_t nchildren)
{
    for (size_t i = 0; i < n; i++) {
        if (types[i] < 0 || (size_t)types[i] >= nchildren) return (int64_t)i;
        if (offs[i] < 0 || offs[i] >= childLens[types[i]]) return (int64_t)i;
    }
    return -1;
}

/* Strict UTF-8 by decoding each code point: minimal length, no
 * surrogates, at most U+10FFFF, no truncation. */
static inline int ref_utf8_valid(const uint8_t *s, size_t n)
{
    size_t i = 0;
    while (i < n) {
        uint32_t c = s[i];
        size_t len;
        uint32_t min;
        if (c < 0x80) {
            i++;
            continue;
        } else if ((c & 0xE0) == 0xC0) {
            len = 2, c &= 0x1F, min = 0x80;
        } else if ((c & 0xF0) == 0xE0) {
            len = 3, c &= 0x0F, min = 0x800;
        } else if ((c & 0xF8) == 0xF0) {
            len = 4, c &= 0x07, min = 0x10000;
        } else {
            return 0;
        }
        if (n - i < len) return 0;
        for (size_t k = 1; k < len; k++) {
            if ((s[i + k] & 0xC0) != 0x80) return 0;
            c = (c << 6) | (s[i + k] & 0x3F);
        }
        if (c < min || c > 0x10FFFF || (c >= 0xD800 && c <= 0xDFFF)) return 0;
        i += len;
    }
    return 1;
}

static inline int32_t ref_le32(const uint8_t *p)
{
    return (int32_t)((uint32_t)p[0] | (uint32_t)p[1] << 8 | (uint32_t)p[2] << 16 | (uint32_t)p[3] << 24);
}

static inline int64_t ref_view_refs(const uint8_t *views, size_t n, const uint8_t *valid, size_t validoff,
                                    const uint8_t *const *bufs, const int64_t *buflens, size_t nbufs, int utf8)
{
    for (size_t i = 0; i < n; i++) {
        if (!ref_valid(valid, validoff, i)) continue;
        const uint8_t *v = views + 16 * i;
        int32_t len = ref_le32(v);
        if (len < 0) return (int64_t)i;
        if (len <= 12) {
            if (utf8 && !ref_utf8_valid(v + 4, (size_t)len)) return (int64_t)i;
            continue;
        }
        int32_t bi = ref_le32(v + 8), off = ref_le32(v + 12);
        if (bi < 0 || (size_t)bi >= nbufs || off < 0) return (int64_t)i;
        if ((int64_t)off + len > buflens[bi]) return (int64_t)i;
        const uint8_t *p = bufs[bi] + off;
        for (int k = 0; k < 4; k++) {
            if (p[k] != v[4 + k]) return (int64_t)i;
        }
        if (utf8 && !ref_utf8_valid(p, (size_t)len)) return (int64_t)i;
    }
    return -1;
}

static inline void ref_gather(uint8_t *dst, const uint8_t *src, const int64_t *idx, size_t n, size_t w)
{
    for (size_t k = 0; k < n; k++) {
        for (size_t j = 0; j < w; j++) dst[k * w + j] = src[(size_t)idx[k] * w + j];
    }
}

/* Offsets widened to int64; lim is INT32_MAX or INT64_MAX. dst gets n + 1
 * entries when the result is not -1. */
static inline int64_t ref_take_offsets(int64_t *dst, const int64_t *src, const int64_t *idx, size_t n, int64_t lim)
{
    __int128 acc = 0;
    dst[0] = 0;
    for (size_t k = 0; k < n; k++) {
        acc += (__int128)src[idx[k] + 1] - src[idx[k]];
        if (acc > lim) return -1;
        dst[k + 1] = (int64_t)acc;
    }
    return (int64_t)acc;
}

static inline size_t ref_take_bytes(uint8_t *dst, const int64_t *offs, const uint8_t *src, const int64_t *idx, size_t n)
{
    size_t pos = 0;
    for (size_t k = 0; k < n; k++) {
        for (int64_t j = offs[idx[k]]; j < offs[idx[k] + 1]; j++) dst[pos++] = src[j];
    }
    return pos;
}

#endif
