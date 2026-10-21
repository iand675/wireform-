/*
 * Iceberg hot-path kernels.
 *
 * - Murmur3 32-bit (seed 0) byte-compatible with org.apache.iceberg.util.BucketUtil
 * - XXH64 (XXH 0.6.x layout, byte-compatible with parquet split-block bloom)
 * - Portable Roaring 32-bit container decode + membership test for V3 deletion vectors
 *
 * Hot loops are written to use SIMDe (`simde/x86/sse2.h`, `simde/x86/sse4.1.h`)
 * so they compile to AVX2/SSE4 on x86, NEON on ARM, and scalar everywhere
 * else. The Haskell side is Wireform.Hash (wireform-core).
 *
 * Style mirrors wireform-columnar/cbits/columnar_simd.c.
 */

#include <stdint.h>
#include <string.h>
#include <stddef.h>

#include <simde/x86/sse2.h>
#include <simde/x86/sse4.1.h>

#if defined(__GNUC__) || defined(__clang__)
#  define HS_INLINE static inline __attribute__((always_inline))
#  define HS_UNUSED __attribute__((unused))
#else
#  define HS_INLINE static inline
#  define HS_UNUSED
#endif

/* ============================================================
 * Endian helpers
 * ============================================================ */

HS_INLINE uint32_t hs_load_le32(const uint8_t *p) {
    uint32_t v;
    memcpy(&v, p, 4);
    return v; /* All targets we build for are LE; SIMDe assumes the same. */
}

HS_INLINE uint64_t hs_load_le64(const uint8_t *p) {
    uint64_t v;
    memcpy(&v, p, 8);
    return v;
}

/* ============================================================
 * Murmur3 32-bit, seed = 0 (matches Iceberg BucketUtil.hash)
 * ============================================================ */

#define MURMUR_C1 0xcc9e2d51u
#define MURMUR_C2 0x1b873593u

HS_INLINE uint32_t hs_rotl32(uint32_t x, int r) {
    return (x << r) | (x >> ((32 - r) & 31));
}

/* Murmur3 x86_32, seed 0, of buf[0, len).
 *
 * len is a byte count (size_t), so a ByteString over 2 GiB is hashed in
 * full; it is never narrowed. buf may be NULL when len == 0 (an empty
 * ByteString). Reads exactly buf[0, len). i is a byte position with
 * 0 <= i <= len, and the bounds are written as len - i, which is exact
 * and cannot wrap for any size_t length. The length also enters the
 * finalizer as a uint32, as the reference algorithm specifies, so two
 * lengths that differ by a multiple of 2^32 produce the same digest. */
int32_t hs_wf_murmur3_32(const uint8_t *buf, size_t len)
{
    const uint32_t c1 = MURMUR_C1;
    const uint32_t c2 = MURMUR_C2;
    uint32_t h = 0;
    size_t i = 0;

    /* Body: 4-byte blocks. We unroll 4 lanes at a time (16 bytes per iter)
     * because the kernel is dominated by integer multiplies and rotates,
     * which run independently on the typical CPU pipeline. The bounds are
     * written as len - i (0 <= i <= len) so they are exact at any length,
     * including the old INT32_MAX overflow point and above it. */
    for (; len - i >= 16; i += 16) {
        uint32_t k0 = hs_load_le32(buf + i);
        uint32_t k1 = hs_load_le32(buf + i + 4);
        uint32_t k2 = hs_load_le32(buf + i + 8);
        uint32_t k3 = hs_load_le32(buf + i + 12);
        k0 = (hs_rotl32(k0 * c1, 15)) * c2;
        h  = (hs_rotl32(h ^ k0, 13)) * 5 + 0xe6546b64u;
        k1 = (hs_rotl32(k1 * c1, 15)) * c2;
        h  = (hs_rotl32(h ^ k1, 13)) * 5 + 0xe6546b64u;
        k2 = (hs_rotl32(k2 * c1, 15)) * c2;
        h  = (hs_rotl32(h ^ k2, 13)) * 5 + 0xe6546b64u;
        k3 = (hs_rotl32(k3 * c1, 15)) * c2;
        h  = (hs_rotl32(h ^ k3, 13)) * 5 + 0xe6546b64u;
    }
    for (; len - i >= 4; i += 4) {
        uint32_t k = hs_load_le32(buf + i);
        k = (hs_rotl32(k * c1, 15)) * c2;
        h = (hs_rotl32(h ^ k, 13)) * 5 + 0xe6546b64u;
    }

    /* Tail */
    uint32_t k1 = 0;
    size_t tail = len - i;
    if (tail == 3) {
        k1 = (uint32_t)buf[i] |
             ((uint32_t)buf[i + 1] << 8) |
             ((uint32_t)buf[i + 2] << 16);
    } else if (tail == 2) {
        k1 = (uint32_t)buf[i] | ((uint32_t)buf[i + 1] << 8);
    } else if (tail == 1) {
        k1 = (uint32_t)buf[i];
    }
    if (tail) {
        k1 = (hs_rotl32(k1 * c1, 15)) * c2;
        h ^= k1;
    }

    /* Finalize. The reference algorithm folds in the length as a uint32,
     * so this cast is the specification, not a truncation bug. */
    h ^= (uint32_t)len;
    h ^= h >> 16;
    h *= 0x85ebca6bu;
    h ^= h >> 13;
    h *= 0xc2b2ae35u;
    h ^= h >> 16;
    return (int32_t)h;
}

/* Inline 8-byte (long) bucket: avoid a tiny memcpy + function call.
 *
 * `buckets` stays int32_t on purpose. It is Iceberg's bucket[N] count from
 * table metadata, a small transform parameter rather than a buffer size;
 * the Haskell wrapper narrows N to Int32 (documented there), so a count
 * >= 2^31 arrives here as 0 or negative and returns 0 rather than dividing
 * by zero. Widening it would buy nothing a real table can reach. */
int32_t hs_wf_bucket_long(int64_t value, int32_t buckets)
{
    if (buckets <= 0) return 0;
    uint8_t buf[8];
    uint64_t u = (uint64_t)value;
    buf[0] = (uint8_t)(u);
    buf[1] = (uint8_t)(u >> 8);
    buf[2] = (uint8_t)(u >> 16);
    buf[3] = (uint8_t)(u >> 24);
    buf[4] = (uint8_t)(u >> 32);
    buf[5] = (uint8_t)(u >> 40);
    buf[6] = (uint8_t)(u >> 48);
    buf[7] = (uint8_t)(u >> 56);
    int32_t h = hs_wf_murmur3_32(buf, 8);
    uint32_t pos = (uint32_t)h & 0x7fffffffu;
    return (int32_t)(pos % (uint32_t)buckets);
}

/* ============================================================
 * XXH64, seed = 0
 *
 * Algorithm: https://github.com/Cyan4973/xxHash, layout matches the upstream
 * 0.6.x reference used by parquet-format and Iceberg manifests.
 * ============================================================ */

#define XXH_PRIME64_1 0x9E3779B185EBCA87ULL
#define XXH_PRIME64_2 0xC2B2AE3D27D4EB4FULL
#define XXH_PRIME64_3 0x165667B19E3779F9ULL
#define XXH_PRIME64_4 0x85EBCA77C2B2AE63ULL
#define XXH_PRIME64_5 0x27D4EB2F165667C5ULL

HS_INLINE uint64_t xxh_rotl64(uint64_t x, int r) {
    return (x << r) | (x >> ((64 - r) & 63));
}

HS_INLINE uint64_t xxh_round(uint64_t acc, uint64_t input) {
    acc += input * XXH_PRIME64_2;
    acc  = xxh_rotl64(acc, 31);
    acc *= XXH_PRIME64_1;
    return acc;
}

HS_INLINE uint64_t xxh_merge(uint64_t acc, uint64_t val) {
    val = xxh_round(0, val);
    acc ^= val;
    acc = acc * XXH_PRIME64_1 + XXH_PRIME64_4;
    return acc;
}

/* len is a byte count (size_t) and is neither narrowed nor sign-checked;
 * the caller passes a ByteString length, which cannot be negative. buf
 * may be NULL when len == 0 (an empty ByteString) and hashes the empty
 * input. Reads exactly buf[0, len). Offsets are tracked as indices so no
 * pointer is formed past buf + len (or from NULL). */
uint64_t hs_wf_xxh64(const uint8_t *buf, size_t len, uint64_t seed)
{
    const size_t n = len;
    size_t i = 0;
    uint64_t h64;

    if (n >= 32) {
        uint64_t v1 = seed + XXH_PRIME64_1 + XXH_PRIME64_2;
        uint64_t v2 = seed + XXH_PRIME64_2;
        uint64_t v3 = seed + 0;
        uint64_t v4 = seed - XXH_PRIME64_1;

        do {
            v1 = xxh_round(v1, hs_load_le64(buf + i));
            v2 = xxh_round(v2, hs_load_le64(buf + i + 8));
            v3 = xxh_round(v3, hs_load_le64(buf + i + 16));
            v4 = xxh_round(v4, hs_load_le64(buf + i + 24));
            i += 32;
        } while (n - i >= 32);

        h64 = xxh_rotl64(v1, 1) + xxh_rotl64(v2, 7)
            + xxh_rotl64(v3, 12) + xxh_rotl64(v4, 18);
        h64 = xxh_merge(h64, v1);
        h64 = xxh_merge(h64, v2);
        h64 = xxh_merge(h64, v3);
        h64 = xxh_merge(h64, v4);
    } else {
        h64 = seed + XXH_PRIME64_5;
    }

    h64 += (uint64_t)n;

    while (n - i >= 8) {
        uint64_t k1 = xxh_round(0, hs_load_le64(buf + i));
        h64 ^= k1;
        h64  = xxh_rotl64(h64, 27) * XXH_PRIME64_1 + XXH_PRIME64_4;
        i += 8;
    }
    if (n - i >= 4) {
        h64 ^= (uint64_t)hs_load_le32(buf + i) * XXH_PRIME64_1;
        h64  = xxh_rotl64(h64, 23) * XXH_PRIME64_2 + XXH_PRIME64_3;
        i += 4;
    }
    while (i < n) {
        h64 ^= (uint64_t)buf[i] * XXH_PRIME64_5;
        h64  = xxh_rotl64(h64, 11) * XXH_PRIME64_1;
        i++;
    }

    h64 ^= h64 >> 33;
    h64 *= XXH_PRIME64_2;
    h64 ^= h64 >> 29;
    h64 *= XXH_PRIME64_3;
    h64 ^= h64 >> 32;
    return h64;
}

/* ============================================================
 * Portable Roaring 32-bit container decode + membership
 *
 * The portable Roaring 32-bit serialised layout (used inside the Iceberg V3
 * deletion-vector blob's per-high32 buckets) is:
 *
 *   uint32_t cookie       (low 16 bits == 0x3B30)
 *   uint32_t numContainers
 *   key/cardinality table : numContainers * { uint16 key; uint16 card-1 }
 *   offsets table         : numContainers * uint32 offset
 *   container payloads    : ARRAY (uint16 lows) | BITSET (8192 bytes) ...
 *
 * The Haskell side produces only ARRAY containers when packing deletion
 * vectors, so this kernel is fast-pathed for them. BITSET containers are
 * decoded with a SIMDe popcount-for-bit-extraction loop.
 *
 * Widths: the kernel below has no byte length parameters, only element
 * counts (`cardinality`). Those stay int32_t deliberately. A container is
 * bounded by the format (ARRAY holds at most 4096 distinct 16-bit lows,
 * BITSET at most 65536), so a count that needs more than 31 bits cannot
 * arise from a real container, and the Haskell wrappers additionally
 * clamp untrusted counts to the payload size. Widening them would change
 * the Haskell vector signatures without removing a reachable truncation.
 * ============================================================ */

/* Decode an ARRAY container of @cardinality@ uint16 values starting at
 * @src@ and write them, OR'd with @hi << 16@, into @dst[0..cardinality)@.
 * Contract: src holds 2 * cardinality bytes, dst holds cardinality int32
 * (the Haskell wrapper clamps cardinality to the payload length).
 * cardinality <= 0 touches nothing. Returns the number of bytes consumed,
 * cardinality * 2 (computed modulo 2^32). */
int32_t hs_wf_roaring_decode_array(
    const uint8_t *src,
    int32_t cardinality,
    uint32_t hi,
    int32_t *dst)
{
    const size_t n = cardinality > 0 ? (size_t)cardinality : 0;
    const uint32_t base = hi << 16;
    size_t i = 0;
    /* SIMDe path: load 8 uint16 lanes, expand to 8 uint32, OR with hi.
     * 16 bytes in -> 32 bytes out per iteration. */
    simde__m128i hiv = simde_mm_set1_epi32((int32_t)base);
    for (; n - i >= 8; i += 8) {
        simde__m128i lows = simde_mm_loadu_si128((const simde__m128i *)(src + i * 2));

        /* Expand low half to 32-bit lanes */
        simde__m128i lo32 = simde_mm_unpacklo_epi16(lows, simde_mm_setzero_si128());
        simde__m128i hi32 = simde_mm_unpackhi_epi16(lows, simde_mm_setzero_si128());

        simde__m128i out0 = simde_mm_or_si128(lo32, hiv);
        simde__m128i out1 = simde_mm_or_si128(hi32, hiv);

        simde_mm_storeu_si128((simde__m128i *)(dst + i),     out0);
        simde_mm_storeu_si128((simde__m128i *)(dst + i + 4), out1);
    }
    for (; i < n; i++) {
        uint16_t lo;
        memcpy(&lo, src + i * 2, 2);
        dst[i] = (int32_t)(((uint32_t)lo) | base);
    }
    return (int32_t)(2u * (uint32_t)cardinality);
}

/* Decode a BITSET container (8192 bytes = 65536 bits) into the destination
 * buffer. Each set bit at position @b@ produces (hi << 16) | b. Returns the
 * number of integers written.
 *
 * This is the same algorithm Java's CRoaring uses in MutableRoaringArray.
 * The hot loop processes 64 bits at a time and uses
 * __builtin_ctzll to find the next set bit, which beats SIMD here on x86-64
 * because of the BMI1 BLSR instruction.
 */
int32_t hs_wf_roaring_decode_bitset(
    const uint8_t *src,
    uint32_t hi,
    int32_t *dst)
{
    const uint32_t base = hi << 16;
    int32_t out = 0;
    for (int word = 0; word < 1024; word++) {
        uint64_t w = hs_load_le64(src + word * 8);
        while (w) {
            int bit = __builtin_ctzll(w);
            dst[out++] = (int32_t)(base | ((uint32_t)(word * 64 + bit)));
            w &= w - 1;
        }
    }
    return out;
}

/* Membership test on the raw portable Roaring container payload.
 * We accept a single container at a time so the Haskell caller decides which
 * bucket (high16) to look up first.
 *
 * @kind = 1  -> BITSET: src is exactly 8192 bytes
 * @kind = anything else -> ARRAY: src is a sorted vector of @cardinality@
 *   uint16 lows (2 * cardinality bytes); cardinality <= 0 is empty.
 *   Unsorted input can yield a false negative, never a false positive or
 *   a read outside the 2 * cardinality bytes.
 *
 * @value16 is the 16-bit low half of the position to test. Returns 1 if
 * present, 0 otherwise. */
int32_t hs_wf_roaring_contains(
    int32_t kind,
    const uint8_t *src,
    int32_t cardinality,
    uint16_t value16)
{
    if (kind == 1) {
        uint32_t bit = value16;
        uint32_t word = bit >> 6;
        uint64_t w = hs_load_le64(src + word * 8);
        return (int32_t)((w >> (bit & 63)) & 1ULL);
    }

    /* SIMD-accelerated linear search for small arrays; binary search for
     * larger. Both are correct because the array is stored sorted. */
    if (cardinality <= 64) {
        simde__m128i needle = simde_mm_set1_epi16((int16_t)value16);
        int32_t i = 0;
        for (; i + 8 <= cardinality; i += 8) {
            simde__m128i v = simde_mm_loadu_si128((const simde__m128i *)(src + i * 2));
            simde__m128i eq = simde_mm_cmpeq_epi16(v, needle);
            int mask = simde_mm_movemask_epi8(eq);
            if (mask) return 1;
        }
        for (; i < cardinality; i++) {
            uint16_t v;
            memcpy(&v, src + i * 2, 2);
            if (v == value16) return 1;
            if (v > value16)  return 0;
        }
        return 0;
    }

    /* Binary search for larger arrays. */
    int32_t lo = 0, hi_ = cardinality - 1;
    while (lo <= hi_) {
        int32_t mid = lo + ((hi_ - lo) >> 1);
        uint16_t v;
        memcpy(&v, src + (size_t)mid * 2, 2);
        if (v == value16) return 1;
        if (v < value16) lo = mid + 1;
        else             hi_ = mid - 1;
    }
    return 0;
}

/* Bulk encode a sorted uint16 vector into an ARRAY container. The caller
 * already determined cardinality; we just unpack into the buffer. SIMDe
 * memcpy lanes give it a small boost over scalar. Returns bytes written.
 * cardinality <= 0 writes nothing (and keeps a NULL src/dst away from
 * memcpy, which an empty Storable vector may hand over). */
int32_t hs_wf_roaring_encode_array(
    const uint16_t *src,
    int32_t cardinality,
    uint8_t *dst)
{
    if (cardinality <= 0) return 0;
    /* aligned uint16 -> bytes is just memcpy. */
    memcpy(dst, src, (size_t)cardinality * 2);
    return (int32_t)(2u * (uint32_t)cardinality);
}

/* Encode a sorted uint16 vector into a BITSET container (8192 bytes). */
void hs_wf_roaring_encode_bitset(
    const uint16_t *src,
    int32_t cardinality,
    uint8_t *dst)
{
    /* Zero the 8 KiB target with SIMD stores. */
    simde__m128i zero = simde_mm_setzero_si128();
    for (int i = 0; i < 512; i++) {
        simde_mm_storeu_si128((simde__m128i *)(dst + i * 16), zero);
    }
    for (int32_t i = 0; i < cardinality; i++) {
        uint16_t v = src[i];
        uint32_t word = v >> 3;        /* byte index in 8 KiB buffer */
        uint32_t bit  = v & 7;          /* bit within that byte */
        dst[word] |= (uint8_t)(1u << bit);
    }
}

HS_UNUSED static const char *hs_wf_simd_version = "1";
