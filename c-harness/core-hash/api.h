/* Kernel API of wireform-core/cbits/wireform_hash_simd.c as
 * wireform-core/src/Wireform/Hash.hs imports it, the caller contracts the
 * Haskell wrappers establish, and slow reference implementations the
 * tests compare against.
 *
 * Haskell types map as: Ptr Word8 -> const uint8_t *, Int32 -> int32_t,
 * Int64 -> int64_t, Word16/32/64 -> uint16_t/uint32_t/uint64_t.
 */
#ifndef CORE_HASH_API_H
#define CORE_HASH_API_H

#include <stddef.h>
#include <stdint.h>
#include <string.h>

/* Murmur3 x86_32, seed 0, signed result (Iceberg BucketUtil / Guava).
 * Caller (murmur3_32): buf/len from unsafeUseAsCStringLen. buf is NULL for
 * an empty ByteString (Data.ByteString.empty uses nullForeignPtr). len is
 * the ByteString length narrowed to Int32, so it is negative or wrapped
 * for inputs of 2 GiB or more (Java's int-indexed BucketUtil cannot hash
 * those either). Reads exactly buf[0, len); len <= 0 reads nothing. */
int32_t hs_wf_murmur3_32(const uint8_t *buf, int32_t len);

/* Iceberg bucket[N] of a long: (murmur3(8-byte LE value) & INT_MAX) % N.
 * Caller (bucketLong) returns 0 for N <= 0 itself, but narrows N to Int32,
 * so N >= 2^31 from table metadata ("bucket[4294967296]") arrives as 0 or
 * negative. The kernel returns 0 for buckets <= 0. */
int32_t hs_wf_bucket_long(int64_t value, int32_t buckets);

/* XXH64 (xxHash 0.6.x+ layout; identical output to current libxxhash).
 * Caller (xxh64): buf NULL for an empty ByteString, len = ByteString
 * length (>= 0). Reads exactly buf[0, len); len <= 0 hashes the empty
 * input. */
uint64_t hs_wf_xxh64(const uint8_t *buf, int64_t len, uint64_t seed);

/* Roaring ARRAY container -> dst[i] = (hi << 16) | lows[i], i < cardinality.
 * Contract: src holds >= 2*cardinality bytes (little-endian uint16 lows),
 * dst holds >= cardinality int32. The Haskell wrapper (roaringDecodeArray)
 * clamps cardinality to [0, min(len/2, INT32_MAX)] because Iceberg
 * decodeDV passes a cardinality from untrusted blob bytes alongside a
 * payload that may be truncated. Returns 2*cardinality (bytes consumed),
 * computed modulo 2^32. */
int32_t hs_wf_roaring_decode_array(const uint8_t *src, int32_t cardinality,
                                   uint32_t hi, int32_t *dst);

/* Roaring BITSET container (8192 bytes, little-endian 64-bit words) ->
 * dst[k] = (hi << 16) | bit for each set bit, ascending. Contract: src
 * holds 8192 bytes (the wrapper zero-pads shorter input), dst holds 65536
 * int32. Returns the number written (the popcount). */
int32_t hs_wf_roaring_decode_bitset(const uint8_t *src, uint32_t hi,
                                    int32_t *dst);

/* Membership of value16 in one container. kind 1: src is an 8192-byte
 * BITSET (wrapper zero-pads shorter input). Any other kind: src is an
 * ARRAY of cardinality little-endian uint16, holding >= 2*cardinality
 * bytes (wrapper clamps cardinality to len/2). Arrays are sorted
 * ascending by contract; on unsorted input the result may be a false
 * negative but never a false positive or an out-of-bounds read.
 * cardinality <= 0 is the empty array. */
int32_t hs_wf_roaring_contains(int32_t kind, const uint8_t *src,
                               int32_t cardinality, uint16_t value16);

/* dst[0, 2*cardinality) = the uint16 lows as bytes (host order, LE on
 * every target). Caller (roaringEncodeArray): cardinality = vector length
 * (>= 0), dst a fresh 2*cardinality-byte buffer; src may be any pointer
 * (even NULL) when cardinality is 0. Returns 2*cardinality, or 0 when
 * cardinality <= 0. */
int32_t hs_wf_roaring_encode_array(const uint16_t *src, int32_t cardinality,
                                   uint8_t *dst);

/* dst[0, 8192) = BITSET with bit v set for every src[i]; any order,
 * duplicates allowed. Caller (roaringEncodeBitset): dst fresh 8192 bytes. */
void hs_wf_roaring_encode_bitset(const uint16_t *src, int32_t cardinality,
                                 uint8_t *dst);

/* ------------------------------------------------------------------ */
/* References                                                          */
/* ------------------------------------------------------------------ */

static inline uint32_t ref_rotl32(uint32_t x, unsigned r)
{
    return r ? (x << r) | (x >> (32 - r)) : x;
}

static inline uint64_t ref_rotl64(uint64_t x, unsigned r)
{
    return r ? (x << r) | (x >> (64 - r)) : x;
}

/* MurmurHash3_x86_32 as in SMHasher, byte-wise little-endian reads. */
static inline int32_t ref_murmur3_32(const uint8_t *data, size_t len,
                                     uint32_t seed)
{
    const uint32_t c1 = 0xcc9e2d51u, c2 = 0x1b873593u;
    uint32_t h = seed;
    size_t nblocks = len / 4;
    for (size_t b = 0; b < nblocks; b++) {
        const uint8_t *q = data + 4 * b;
        uint32_t k = (uint32_t)q[0] | ((uint32_t)q[1] << 8)
                   | ((uint32_t)q[2] << 16) | ((uint32_t)q[3] << 24);
        k *= c1;
        k = ref_rotl32(k, 15);
        k *= c2;
        h ^= k;
        h = ref_rotl32(h, 13);
        h = h * 5 + 0xe6546b64u;
    }
    uint32_t k = 0;
    const uint8_t *t = data + 4 * nblocks;
    switch (len & 3) {
    case 3: k ^= (uint32_t)t[2] << 16; /* fallthrough */
    case 2: k ^= (uint32_t)t[1] << 8;  /* fallthrough */
    case 1:
        k ^= t[0];
        k *= c1;
        k = ref_rotl32(k, 15);
        k *= c2;
        h ^= k;
    }
    h ^= (uint32_t)len;
    h ^= h >> 16;
    h *= 0x85ebca6bu;
    h ^= h >> 13;
    h *= 0xc2b2ae35u;
    h ^= h >> 16;
    return (int32_t)h;
}

static inline int32_t ref_bucket_long(int64_t value, int32_t buckets)
{
    if (buckets <= 0) return 0;
    uint8_t b[8];
    for (int i = 0; i < 8; i++) b[i] = (uint8_t)((uint64_t)value >> (8 * i));
    uint32_t h = (uint32_t)ref_murmur3_32(b, 8, 0) & 0x7fffffffu;
    return (int32_t)(h % (uint32_t)buckets);
}

/* XXH64 transcribed from the xxHash specification (doc/xxhash_spec.md),
 * byte-wise little-endian reads, stripes processed into an explicit
 * accumulator array. */
#define REF_P1 0x9E3779B185EBCA87ULL
#define REF_P2 0xC2B2AE3D27D4EB4FULL
#define REF_P3 0x165667B19E3779F9ULL
#define REF_P4 0x85EBCA77C2B2AE63ULL
#define REF_P5 0x27D4EB2F165667C5ULL

static inline uint64_t ref_le(const uint8_t *p, int n)
{
    uint64_t v = 0;
    for (int i = n - 1; i >= 0; i--) v = (v << 8) | p[i];
    return v;
}

static inline uint64_t ref_xxh_round(uint64_t acc, uint64_t lane)
{
    acc += lane * REF_P2;
    acc = ref_rotl64(acc, 31);
    return acc * REF_P1;
}

static inline uint64_t ref_xxh64(const uint8_t *in, size_t len, uint64_t seed)
{
    size_t pos = 0;
    uint64_t acc;
    if (len >= 32) {
        uint64_t a[4] = { seed + REF_P1 + REF_P2, seed + REF_P2, seed,
                          seed - REF_P1 };
        while (len - pos >= 32) {
            for (int l = 0; l < 4; l++) {
                a[l] = ref_xxh_round(a[l], ref_le(in + pos + 8 * l, 8));
            }
            pos += 32;
        }
        acc = ref_rotl64(a[0], 1) + ref_rotl64(a[1], 7)
            + ref_rotl64(a[2], 12) + ref_rotl64(a[3], 18);
        for (int l = 0; l < 4; l++) {
            acc ^= ref_xxh_round(0, a[l]);
            acc = acc * REF_P1 + REF_P4;
        }
    } else {
        acc = seed + REF_P5;
    }
    acc += (uint64_t)len;
    while (len - pos >= 8) {
        acc ^= ref_xxh_round(0, ref_le(in + pos, 8));
        acc = ref_rotl64(acc, 27) * REF_P1 + REF_P4;
        pos += 8;
    }
    if (len - pos >= 4) {
        acc ^= ref_le(in + pos, 4) * REF_P1;
        acc = ref_rotl64(acc, 23) * REF_P2 + REF_P3;
        pos += 4;
    }
    while (pos < len) {
        acc ^= in[pos] * REF_P5;
        acc = ref_rotl64(acc, 11) * REF_P1;
        pos++;
    }
    acc ^= acc >> 33;
    acc *= REF_P2;
    acc ^= acc >> 29;
    acc *= REF_P3;
    acc ^= acc >> 32;
    return acc;
}

static inline uint16_t ref_low(const uint8_t *src, size_t i)
{
    return (uint16_t)(src[2 * i] | (src[2 * i + 1] << 8));
}

static inline void ref_decode_array(const uint8_t *src, int32_t card,
                                    uint32_t hi, int32_t *dst)
{
    for (int32_t i = 0; i < card; i++) {
        dst[i] = (int32_t)((hi << 16) | ref_low(src, (size_t)i));
    }
}

static inline int32_t ref_decode_bitset(const uint8_t *src, uint32_t hi,
                                        int32_t *dst)
{
    int32_t n = 0;
    for (uint32_t b = 0; b < 65536; b++) {
        if ((src[b >> 3] >> (b & 7)) & 1) dst[n++] = (int32_t)((hi << 16) | b);
    }
    return n;
}

/* Exact set membership; for sorted arrays this is the kernel's answer. */
static inline int32_t ref_array_member(const uint8_t *src, int32_t card,
                                       uint16_t v)
{
    for (int32_t i = 0; i < card; i++) {
        if (ref_low(src, (size_t)i) == v) return 1;
    }
    return 0;
}

static inline int32_t ref_bitset_member(const uint8_t *src, uint16_t v)
{
    return (src[v >> 3] >> (v & 7)) & 1;
}

#endif
