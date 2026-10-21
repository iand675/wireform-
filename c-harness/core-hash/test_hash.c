/* Murmur3-32, Iceberg bucket and XXH64 tests for wireform_hash_simd.c.
 *
 * Correctness is pinned three ways: published vectors (Iceberg spec
 * Appendix B, xxHash), a table from independent oracles (mmh3 and
 * libxxhash, vectors.h), and the slow references in api.h, which the
 * boundary sweeps and random tests compare against on every input. */
#include "harness.h"
#include "api.h"
#include "vectors.h"
#include "vecbuf.h"

#include <limits.h>

/* Lengths are byte counts size_t end to end: the test never narrows a
 * buffer size before it reaches the kernel. */
static int32_t murmur(const uint8_t *p, size_t n)
{
    return hs_wf_murmur3_32(p, n);
}

static uint64_t xxh(const uint8_t *p, size_t n, uint64_t seed)
{
    return hs_wf_xxh64(p, n, seed);
}

static void le64(uint8_t out[8], int64_t v)
{
    for (int i = 0; i < 8; i++) out[i] = (uint8_t)((uint64_t)v >> (8 * i));
}

/* Iceberg spec, Appendix B "32-bit Hash Requirements". */
static void murmur_iceberg_spec(void)
{
    static const struct { int64_t v; int32_t h; } longs[] = {
        { 34, 2017239379 },                 /* int / long 34 */
        { 17486, -653330422 },              /* date 2017-11-16 */
        { 81068000000LL, -662762989 },      /* time 22:31:08 */
        { 1510871468000000LL, -2047944441 } /* timestamp(tz) 2017-11-16T22:31:08 */
    };
    for (size_t i = 0; i < sizeof longs / sizeof longs[0]; i++) {
        uint8_t b[8];
        le64(b, longs[i].v);
        uint8_t *p = wf_dup(b, 8);
        WF_CHECK_EQ(murmur(p, 8), longs[i].h);
        WF_CHECK_EQ(ref_murmur3_32(p, 8, 0), longs[i].h);
        wf_free(p, 8);
    }
    static const struct { const char *bytes; size_t n; int32_t h; } raw[] = {
        { "\x05\x8c", 2, -500754589 },      /* decimal(4,2) 14.20 */
        { "iceberg", 7, 1210000089 },       /* string */
        { "\xf7\x9c\x3e\x09\x67\x7c\x4b\xbd\xa4\x79\x3f\x34\x9c\xb7\x85\xe7",
          16, 1488055340 },                 /* uuid f79c3e09-677c-... */
        { "\x00\x01\x02\x03", 4, -188683207 }, /* fixed(4) / binary */
        { "", 0, 0 },
        { "Hello, world!", 13, -1070186941 },
        { "The quick brown fox jumps over the lazy dog", 43, 776992547 },
    };
    for (size_t i = 0; i < sizeof raw / sizeof raw[0]; i++) {
        uint8_t *p = wf_dup(raw[i].bytes, raw[i].n);
        WF_CHECK_EQ(murmur(p, raw[i].n), raw[i].h);
        WF_CHECK_EQ(ref_murmur3_32(p, raw[i].n, 0), raw[i].h);
        wf_free(p, raw[i].n);
    }
    /* Spec: bucket[16] of 34 is 3 (Haskell test suite, Iceberg docs). */
    WF_CHECK_EQ(hs_wf_bucket_long(34, 16), 3);
}

static void xxh64_published(void)
{
    static const struct { const char *s; uint64_t h0, h1; } v[] = {
        { "", 0xef46db3751d8e999ULL, 0xd5afba1336a3be4bULL },
        { "a", 0xd24ec4f1a98c6e5bULL, 0xdec2bc81c3cd46c6ULL },
        { "abc", 0x44bc2cf5ad770999ULL, 0xbea9ca8199328908ULL },
        { "Nobody inspects the spammish repetition", 0xfbcea83c8a378bf1ULL,
          0x43f425448d954db6ULL },
        { "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", 0x856e843298f99ad7ULL,
          0x53ac5803e608ddf7ULL },
        { "The quick brown fox jumps over the lazy dog",
          0x0b242d361fda71bcULL, 0xdf5091b6dad2c6dbULL },
    };
    for (size_t i = 0; i < sizeof v / sizeof v[0]; i++) {
        size_t n = strlen(v[i].s);
        uint8_t *p = wf_dup(v[i].s, n);
        WF_CHECK(xxh(p, n, 0) == v[i].h0);
        WF_CHECK(xxh(p, n, 1) == v[i].h1);
        WF_CHECK(ref_xxh64(p, n, 0) == v[i].h0);
        WF_CHECK(ref_xxh64(p, n, 1) == v[i].h1);
        wf_free(p, n);
    }
}

/* The oracle table, with the input flush against a guard page at either
 * end, so the kernels cannot read a byte outside [0, len). */
static void oracle_table(void)
{
    enum { MAXN = 4096 };
    uint8_t sanity[MAXN];
    uint64_t g = 2654435761ULL;
    for (size_t i = 0; i < MAXN; i++) {
        sanity[i] = (uint8_t)(g >> 56);
        g *= 11400714785074694797ULL;
    }
    wf_guarded tail = wf_guard_tail(MAXN);
    wf_guarded head = wf_guard_head(MAXN);
    for (size_t e = 0; e < sizeof vec_sanity / sizeof vec_sanity[0]; e++) {
        size_t n = vec_sanity[e].len;
        uint8_t *pt = tail.p + MAXN - n;
        memcpy(pt, sanity, n);
        memcpy(head.p, sanity, n);
        WF_CHECK_EQ(murmur(pt, n), vec_sanity[e].murmur);
        WF_CHECK_EQ(murmur(head.p, n), vec_sanity[e].murmur);
        WF_CHECK_EQ(ref_murmur3_32(pt, n, 0), vec_sanity[e].murmur);
        for (int s = 0; s < 4; s++) {
            uint64_t seed = vec_xxh_seeds[s];
            WF_CHECK(xxh(pt, n, seed) == vec_sanity[e].xxh[s]);
            WF_CHECK(xxh(head.p, n, seed) == vec_sanity[e].xxh[s]);
            WF_CHECK(ref_xxh64(pt, n, seed) == vec_sanity[e].xxh[s]);
        }
    }
    wf_guard_free(tail);
    wf_guard_free(head);
}

/* Every length across several murmur 16-byte unrolled blocks and XXH64
 * 32-byte stripes, at every start alignment mod 64. k == 0 ends flush
 * against a guard page (an overread past len faults); k > 0 shifts the
 * start alignment with a k-byte gap. The head-guarded buffer starts k
 * bytes after a guard page, catching any read before the start. */
static void boundary_sweep(void)
{
    enum { MAXN = 300, SKEW = 64 };
    wf_rng r = wf_rng_from_env();
    wf_guarded g = wf_guard_tail(MAXN + SKEW);
    wf_guarded h = wf_guard_head(MAXN + SKEW);
    wf_rng_fill(&r, g.p, g.n);
    wf_rng_fill(&r, h.p, h.n);
    for (size_t n = 0; n <= MAXN; n++) {
        for (size_t k = 0; k < SKEW; k++) {
            uint64_t seed = wf_rng_next(&r);
            const uint8_t *p = g.p + g.n - n - k;
            const uint8_t *s = h.p + k;
            WF_CHECK_EQ(murmur(p, n), ref_murmur3_32(p, n, 0));
            WF_CHECK(xxh(p, n, seed) == ref_xxh64(p, n, seed));
            if (k < 16) {
                WF_CHECK_EQ(murmur(s, n), ref_murmur3_32(s, n, 0));
                WF_CHECK(xxh(s, n, seed) == ref_xxh64(s, n, seed));
            }
        }
    }
    wf_guard_free(g);
    wf_guard_free(h);
}

/* All-0x00, all-0xFF and 0x80 (sign bit) inputs at every length. */
static void adversarial_bytes(void)
{
    enum { MAXN = 200 };
    static const uint8_t fills[] = { 0x00, 0xff, 0x80, 0x7f, 0x01 };
    wf_guarded g = wf_guard_tail(MAXN);
    for (size_t f = 0; f < sizeof fills; f++) {
        memset(g.p, fills[f], MAXN);
        for (size_t n = 0; n <= MAXN; n++) {
            const uint8_t *p = g.p + MAXN - n;
            WF_CHECK_EQ(murmur(p, n), ref_murmur3_32(p, n, 0));
            WF_CHECK(xxh(p, n, 0) == ref_xxh64(p, n, 0));
            WF_CHECK(xxh(p, n, UINT64_MAX) == ref_xxh64(p, n, UINT64_MAX));
        }
    }
    wf_guard_free(g);
}

/* Data.ByteString.empty hands the kernels a NULL pointer with length 0;
 * len == 0 must read nothing (NULL makes any read fault). Negative
 * lengths no longer exist in the interface: the C parameter is size_t and
 * the Haskell one CSize, so the old negative-length cases were removed
 * with the int32_t/int64_t parameters rather than kept as dead contract. */
static void null_and_zero_lengths(void)
{
    WF_CHECK_EQ(hs_wf_murmur3_32(NULL, 0), 0);
    WF_CHECK(hs_wf_xxh64(NULL, 0, 0) == 0xef46db3751d8e999ULL);
    WF_CHECK(hs_wf_xxh64(NULL, 0, 1) == 0xd5afba1336a3be4bULL);
    WF_CHECK(hs_wf_xxh64(NULL, 0, 7) == ref_xxh64(NULL, 0, 7));
}

static void random_differential(void)
{
    wf_rng r = wf_rng_from_env();
    long iters = wf_iters(3000);
    for (long it = 0; it < iters; it++) {
        size_t n = (size_t)wf_rng_below(&r, (it & 15) ? 160 : 5000);
        uint8_t *src = malloc(n ? n : 1);
        wf_rng_fill(&r, src, n);
        uint8_t *p = wf_dup(src, n);
        uint64_t seed = (it & 3) ? wf_rng_next(&r) : wf_rng_below(&r, 4);
        WF_CHECK_EQ(murmur(p, n), ref_murmur3_32(p, n, 0));
        WF_CHECK(xxh(p, n, seed) == ref_xxh64(p, n, seed));
        wf_free(p, n);
        free(src);
    }
}

/* Changing any single byte or the seed changes the digest (a property of
 * these functions on short inputs that catches skipped tail bytes). */
static void single_byte_sensitivity(void)
{
    enum { MAXN = 72 };
    wf_rng r = wf_rng_from_env();
    uint8_t buf[MAXN];
    wf_rng_fill(&r, buf, MAXN);
    for (size_t n = 1; n <= MAXN; n++) {
        int32_t m0 = murmur(buf, n);
        uint64_t x0 = xxh(buf, n, 0);
        WF_CHECK(xxh(buf, n, 1) != x0);
        for (size_t i = 0; i < n; i++) {
            buf[i] ^= 0x01;
            WF_CHECK(xxh(buf, n, 0) != x0);
            WF_CHECK(murmur(buf, n) != m0);
            buf[i] ^= 0x01;
        }
    }
}

static void bucket_long(void)
{
    /* buckets <= 0, including N >= 2^31 narrowed to Int32 by the Haskell
     * wrapper (4294967296 -> 0, 4294967295 -> -1): 0, never a division
     * by zero (SIGFPE on x86-64). Regression for the unchecked modulus. */
    static const int32_t bad[] = { 0, -1, -16, INT32_MIN };
    static const int64_t vals[] = { 0, 1, -1, 34, INT64_MIN, INT64_MAX };
    for (size_t b = 0; b < sizeof bad / sizeof bad[0]; b++) {
        for (size_t v = 0; v < sizeof vals / sizeof vals[0]; v++) {
            WF_CHECK_EQ(hs_wf_bucket_long(vals[v], bad[b]), 0);
        }
    }
    for (size_t v = 0; v < sizeof vals / sizeof vals[0]; v++) {
        WF_CHECK_EQ(hs_wf_bucket_long(vals[v], 1), 0);
        WF_CHECK_EQ(hs_wf_bucket_long(vals[v], INT32_MAX),
                    ref_bucket_long(vals[v], INT32_MAX));
    }
    wf_rng r = wf_rng_from_env();
    long iters = wf_iters(20000);
    for (long it = 0; it < iters; it++) {
        int64_t v = (int64_t)wf_rng_next(&r);
        if (it & 1) v = (int64_t)wf_rng_below(&r, 2000) - 1000;
        int32_t n = (it & 2) ? (int32_t)wf_rng_below(&r, 1024) + 1
                             : (int32_t)(wf_rng_next(&r) >> 33) + 1;
        int32_t got = hs_wf_bucket_long(v, n);
        WF_CHECK_EQ(got, ref_bucket_long(v, n));
        WF_CHECK(got >= 0 && got < n);
        /* bucket_long is murmur3 over the 8-byte little-endian value. */
        uint8_t b[8];
        le64(b, v);
        uint32_t h = (uint32_t)murmur(b, 8) & 0x7fffffffu;
        WF_CHECK_EQ(got, (int32_t)(h % (uint32_t)n));
    }
}

/* Lengths at and above the old int32 ceiling, over a sparse all-zero
 * mapping (vecbuf.h: 1 MiB of zero file mapped repeatedly, so the >2 GiB
 * span costs a MiB of memory and no touched anonymous pages).
 *
 * Historically the loop conditions `i + 16 <= len` / `i + 4 <= len`
 * overflowed int32 for len >= 2^31-16 (UBSan: signed integer overflow),
 * and with the overflow wrapping the block loop read past the end of the
 * input (SIGBUS against the guard). len is a size_t byte count now, so
 * these values keep their mmh3-over-bytes(n) digests while the arithmetic
 * stays exact. The 2^31 + 16 case is the regression for the old
 * Int32 truncation: as int32_t that length wrapped negative and the
 * kernel hashed zero bytes, so matching the byte-wise reference over the
 * whole span proves the new signature hashes all of it. */
static void murmur_lengths_beyond_int32(void)
{
    const size_t big = ((size_t)1 << 31) + 16; /* just over 2 GiB */
    zero_span z;
    if (!zero_span_map(&z, big)) return;
    static const struct { size_t len; int32_t h; } v[] = {
        { INT32_MAX, 1387477367 },
        { (size_t)INT32_MAX - 15, 2001921547 },
    };
    for (size_t i = 0; i < sizeof v / sizeof v[0]; i++) {
        WF_CHECK_EQ(hs_wf_murmur3_32(z.end - v[i].len, v[i].len), v[i].h);
    }
    const uint8_t *pbig = z.end - big;
    WF_CHECK_EQ(hs_wf_murmur3_32(pbig, big), ref_murmur3_32(pbig, big, 0));
    zero_span_unmap(&z);
}

int main(void)
{
    WF_RUN(murmur_iceberg_spec);
    WF_RUN(xxh64_published);
    WF_RUN(oracle_table);
    WF_RUN(boundary_sweep);
    WF_RUN(adversarial_bytes);
    WF_RUN(null_and_zero_lengths);
    WF_RUN(random_differential);
    WF_RUN(single_byte_sensitivity);
    WF_RUN(bucket_long);
    WF_RUN(murmur_lengths_beyond_int32);
    return wf_finish("core-hash/test_hash");
}
