/* Roaring container kernels of wireform_hash_simd.c: ARRAY / BITSET decode,
 * membership, encode, against the references in api.h and by round trip.
 * Payloads sit flush against guard pages; outputs are exact-size. */
#include "harness.h"
#include "api.h"
#include "vecbuf.h"

#include <limits.h>

static const uint32_t his[] = { 0, 1, 2, 0x7fff, 0x8000, 0xffff, 0x12345,
                                0xffffffffu };

static void put_lows(uint8_t *dst, const uint16_t *lows, size_t n)
{
    for (size_t i = 0; i < n; i++) {
        dst[2 * i] = (uint8_t)lows[i];
        dst[2 * i + 1] = (uint8_t)(lows[i] >> 8);
    }
}

static int cmp_u16(const void *a, const void *b)
{
    uint16_t x = *(const uint16_t *)a, y = *(const uint16_t *)b;
    return (x > y) - (x < y);
}

/* n distinct sorted lows, biased per mode towards the edges of the range. */
static void sorted_unique(wf_rng *r, uint16_t *out, size_t n, int mode)
{
    static uint8_t seen[65536];
    memset(seen, 0, sizeof seen);
    size_t k = 0;
    while (k < n) {
        uint16_t v;
        switch (mode % 4) {
        case 0: v = (uint16_t)wf_rng_next(r); break;
        case 1: v = (uint16_t)wf_rng_below(r, n * 2 + 1); break;
        case 2: v = (uint16_t)(65535 - wf_rng_below(r, n * 2 + 1)); break;
        default: v = (uint16_t)(0x7ff0 + wf_rng_below(r, n + 32)); break;
        }
        if (!seen[v]) { seen[v] = 1; out[k++] = v; }
    }
    qsort(out, n, sizeof out[0], cmp_u16);
}

/* Every cardinality across a few 8-lane SIMD blocks, every payload start
 * alignment (k-byte gap before the guard page; k == 0 is flush), each hi,
 * into an exact-size destination. */
static void decode_array_sweep(void)
{
    enum { MAXC = 80, SKEW = 16 };
    wf_rng r = wf_rng_from_env();
    wf_guarded g = wf_guard_tail(2 * MAXC + SKEW);
    int32_t want[MAXC];
    for (int32_t card = 0; card <= MAXC; card++) {
        for (size_t k = 0; k < SKEW; k++) {
            uint8_t *src = g.p + g.n - 2 * (size_t)card - k;
            wf_rng_fill(&r, src, 2 * (size_t)card);
            uint32_t hi = his[(card + k) % (sizeof his / sizeof his[0])];
            int32_t *dst = (int32_t *)vec_alloc(4 * (size_t)card);
            WF_CHECK_EQ(hs_wf_roaring_decode_array(src, card, hi, dst), 2 * card);
            ref_decode_array(src, card, hi, want);
            WF_CHECK_MEM(dst, want, 4 * (size_t)card);
            vec_free(dst, 4 * (size_t)card);
        }
    }
    wf_guard_free(g);
}

/* hi is ORed as hi << 16 (the documented contract), in both decoders,
 * and the two decoders agree on the same set for every hi. Regression:
 * decode_array used to OR hi unshifted and decode_bitset used
 * hi & 0xffff0000, so neither produced (hi << 16) | low. */
static void decode_hi_contract(void)
{
    static const uint16_t lows[] = { 0, 1, 7, 8, 9, 100, 4095, 4096, 0x7fff,
                                     0x8000, 0xfffe, 0xffff };
    enum { N = sizeof lows / sizeof lows[0] };
    uint8_t *arr = wf_alloc(2 * N);
    put_lows(arr, lows, N);
    uint8_t *bits = wf_alloc(8192);
    hs_wf_roaring_encode_bitset(lows, N, bits);
    int32_t a[N], *b = (int32_t *)vec_alloc(4 * N);
    for (size_t h = 0; h < sizeof his / sizeof his[0]; h++) {
        hs_wf_roaring_decode_array(arr, N, his[h], a);
        WF_CHECK_EQ(hs_wf_roaring_decode_bitset(bits, his[h], b), N);
        for (size_t i = 0; i < N; i++) {
            WF_CHECK_EQ((uint32_t)a[i], (his[h] << 16) | lows[i]);
            WF_CHECK_EQ(a[i], b[i]);
        }
    }
    hs_wf_roaring_decode_array(arr, 1, 1, a);
    WF_CHECK_EQ(a[0], 0x10000);
    vec_free(b, 4 * N);
    wf_free(bits, 8192);
    wf_free(arr, 2 * N);
}

/* cardinality <= 0 reads and writes nothing; returns 2*cardinality. */
static void decode_array_empty(void)
{
    static const int32_t cards[] = { 0, -1, -8, -9, INT32_MIN / 2 };
    for (size_t i = 0; i < sizeof cards / sizeof cards[0]; i++) {
        WF_CHECK_EQ(hs_wf_roaring_decode_array(NULL, cards[i], 5, NULL),
                    2 * cards[i]);
    }
}

static void decode_array_max(void)
{
    enum { C = 65536 };
    wf_rng r = wf_rng_from_env();
    uint8_t *src = wf_alloc(2 * C);
    wf_rng_fill(&r, src, 2 * C);
    int32_t *dst = (int32_t *)vec_alloc(4 * C);
    int32_t *want = malloc(4 * C);
    WF_CHECK_EQ(hs_wf_roaring_decode_array(src, C, 0xabcd, dst), 2 * C);
    ref_decode_array(src, C, 0xabcd, want);
    WF_CHECK_MEM(dst, want, 4 * (size_t)C);
    free(want);
    vec_free(dst, 4 * C);
    wf_free(src, 2 * C);
}

static void fill_bitset(wf_rng *r, uint8_t *b, int mode)
{
    switch (mode % 7) {
    case 0: memset(b, 0, 8192); break;
    case 1: memset(b, 0xff, 8192); break;
    case 2: memset(b, 0, 8192); b[wf_rng_below(r, 8192)] = (uint8_t)(1u << wf_rng_below(r, 8)); break;
    case 3: memset(b, 0x55, 8192); break;
    case 4: memset(b, 0x80, 8192); break;
    case 5:
        memset(b, 0, 8192);
        for (int i = 0; i < 300; i++) {
            uint32_t v = (uint32_t)wf_rng_below(r, 65536);
            b[v >> 3] |= (uint8_t)(1u << (v & 7));
        }
        break;
    default: wf_rng_fill(r, b, 8192); break;
    }
    b[0] |= (uint8_t)(mode & 8 ? 1 : 0);
    b[8191] |= (uint8_t)(mode & 16 ? 0x80 : 0);
}

/* Each bitset at payload offsets 0..7 from the guard page (k == 0 flush),
 * decoded into a destination of exactly popcount entries (the kernel
 * writes nothing past the last set bit even though the contract gives
 * it 65536). */
static void decode_bitset_cases(void)
{
    wf_rng r = wf_rng_from_env();
    wf_guarded g = wf_guard_tail(8192 + 8);
    int32_t *want = malloc(65536 * 4);
    long iters = wf_iters(64);
    for (long it = 0; it < iters; it++) {
        size_t k = (size_t)it % 8;
        uint8_t *src = g.p + g.n - 8192 - k;
        fill_bitset(&r, src, (int)it);
        uint32_t hi = his[it % (sizeof his / sizeof his[0])];
        int32_t n = ref_decode_bitset(src, hi, want);
        int32_t *dst = (int32_t *)vec_alloc(4 * (size_t)n);
        WF_CHECK_EQ(hs_wf_roaring_decode_bitset(src, hi, dst), n);
        WF_CHECK_MEM(dst, want, 4 * (size_t)n);
        vec_free(dst, 4 * (size_t)n);
    }
    free(want);
    wf_guard_free(g);
}

/* Probe arrays of every cardinality around the SIMD-linear / binary-search
 * switch (64) and the 8-lane blocks: every 16-bit value for small arrays,
 * every member and its neighbours plus random values for large ones. */
static void contains_array_sorted(void)
{
    static const int32_t big[] = { 127, 128, 129, 1000, 4096, 65535, 65536 };
    enum { NBIG = sizeof big / sizeof big[0] };
    wf_rng r = wf_rng_from_env();
    uint16_t *lows = malloc(65536 * 2);
    uint8_t *member = malloc(65536);
    for (int32_t ci = 0; ci <= 72 + NBIG; ci++) {
        int32_t card = ci <= 72 ? ci : big[ci - 73];
        for (int mode = 0; mode < 4; mode++) {
            if (card > 32768 && mode != 0) continue;
            sorted_unique(&r, lows, (size_t)card, mode);
            memset(member, 0, 65536);
            for (int32_t i = 0; i < card; i++) member[lows[i]] = 1;
            uint8_t *src = wf_alloc(2 * (size_t)card);
            put_lows(src, lows, (size_t)card);
            if (card <= 72) {
                for (uint32_t v = 0; v < 65536; v++) {
                    WF_CHECK_EQ(hs_wf_roaring_contains(0, src, card, (uint16_t)v),
                                member[v]);
                }
            } else {
                for (int32_t i = 0; i < card; i++) {
                    for (int d = -1; d <= 1; d++) {
                        uint16_t v = (uint16_t)(lows[i] + d);
                        WF_CHECK_EQ(hs_wf_roaring_contains(0, src, card, v),
                                    member[v]);
                    }
                }
                for (int i = 0; i < 4096; i++) {
                    uint16_t v = (uint16_t)wf_rng_next(&r);
                    WF_CHECK_EQ(hs_wf_roaring_contains(0, src, card, v), member[v]);
                }
            }
            wf_free(src, 2 * (size_t)card);
        }
    }
    free(member);
    free(lows);
}

/* Unsorted input (contract violation by a hostile blob): never a false
 * positive and never an out-of-bounds read. Duplicates in sorted input
 * are exact. Any kind other than 1 is ARRAY. */
static void contains_array_unsorted_and_kinds(void)
{
    wf_rng r = wf_rng_from_env();
    long iters = wf_iters(4000);
    for (long it = 0; it < iters; it++) {
        int32_t card = (int32_t)wf_rng_below(&r, (it & 1) ? 70 : 300);
        uint8_t *src = wf_alloc(2 * (size_t)card);
        if (it & 2) wf_rng_fill(&r, src, 2 * (size_t)card);
        else for (int32_t i = 0; i < card; i++) {
            uint16_t v = (uint16_t)wf_rng_below(&r, 64);
            src[2 * i] = (uint8_t)v;
            src[2 * i + 1] = 0;
        }
        for (int p = 0; p < 64; p++) {
            uint16_t v = (p & 1) ? (uint16_t)wf_rng_next(&r) : (uint16_t)wf_rng_below(&r, 64);
            int32_t kind = (int32_t[]){ 0, 2, -1, INT32_MIN }[p & 3];
            int32_t got = hs_wf_roaring_contains(kind, src, card, v);
            WF_CHECK(got == 0 || got == 1);
            if (got) WF_CHECK(ref_array_member(src, card, v));
        }
        /* sorted copy with duplicates: exact */
        uint16_t *s = malloc(2 * (size_t)card + 2);
        for (int32_t i = 0; i < card; i++) s[i] = ref_low(src, (size_t)i);
        qsort(s, (size_t)card, 2, cmp_u16);
        put_lows(src, s, (size_t)card);
        for (int p = 0; p < 32; p++) {
            uint16_t v = (uint16_t)wf_rng_below(&r, 70);
            WF_CHECK_EQ(hs_wf_roaring_contains(0, src, card, v),
                        ref_array_member(src, card, v));
        }
        free(s);
        wf_free(src, 2 * (size_t)card);
    }
    static const int32_t empty[] = { 0, -1, -64, -65, INT32_MIN };
    for (size_t i = 0; i < sizeof empty / sizeof empty[0]; i++) {
        WF_CHECK_EQ(hs_wf_roaring_contains(0, NULL, empty[i], 0), 0);
        WF_CHECK_EQ(hs_wf_roaring_contains(0, NULL, empty[i], 0xffff), 0);
    }
}

/* Binary search over an INT32_MAX-entry array (4 GiB of zero lows, see
 * zero_span_map). Searching for 1 walks mid up towards INT32_MAX; the old
 * `src + mid * 2` overflowed int32 once mid >= 2^30 (UB, and a read far
 * before src once it wrapped). */
static void contains_array_int32_max(void)
{
    zero_span z;
    if (!zero_span_map(&z, 2 * (size_t)INT32_MAX)) return;
    const uint8_t *src = z.end - 2 * (size_t)INT32_MAX;
    WF_CHECK_EQ(hs_wf_roaring_contains(0, src, INT32_MAX, 1), 0);
    WF_CHECK_EQ(hs_wf_roaring_contains(0, src, INT32_MAX, 0xffff), 0);
    WF_CHECK_EQ(hs_wf_roaring_contains(0, src, INT32_MAX, 0), 1);
    WF_CHECK_EQ(hs_wf_roaring_contains(0, src, (1 << 30) + 1, 1), 0);
    zero_span_unmap(&z);
}

static void contains_bitset(void)
{
    wf_rng r = wf_rng_from_env();
    wf_guarded g = wf_guard_tail(8192 + 8);
    wf_guarded h = wf_guard_head(8192 + 8);
    for (int mode = 0; mode < 32; mode++) {
        uint8_t *src = g.p + g.n - 8192 - (size_t)(mode & 7);
        uint8_t *hs = h.p + (size_t)(mode & 7);
        fill_bitset(&r, src, mode);
        memcpy(hs, src, 8192);
        for (uint32_t v = 0; v < 65536; v++) {
            int32_t want = ref_bitset_member(src, (uint16_t)v);
            WF_CHECK_EQ(hs_wf_roaring_contains(1, src, 0, (uint16_t)v), want);
            WF_CHECK_EQ(hs_wf_roaring_contains(1, hs, -5, (uint16_t)v), want);
        }
    }
    wf_guard_free(g);
    wf_guard_free(h);
}

static void encode_array_cases(void)
{
    wf_rng r = wf_rng_from_env();
    for (int32_t card = 0; card <= 200; card++) {
        uint16_t *src = (uint16_t *)vec_alloc(2 * (size_t)card);
        for (int32_t i = 0; i < card; i++) src[i] = (uint16_t)wf_rng_next(&r);
        uint8_t *dst = wf_alloc(2 * (size_t)card);
        WF_CHECK_EQ(hs_wf_roaring_encode_array(src, card, dst), 2 * card);
        uint8_t *want = malloc(2 * (size_t)card + 1);
        put_lows(want, src, (size_t)card);
        WF_CHECK_MEM(dst, want, 2 * (size_t)card);
        /* round trip through decode_array */
        int32_t *back = (int32_t *)vec_alloc(4 * (size_t)card);
        hs_wf_roaring_decode_array(dst, card, 0, back);
        for (int32_t i = 0; i < card; i++) WF_CHECK_EQ(back[i], src[i]);
        vec_free(back, 4 * (size_t)card);
        free(want);
        wf_free(dst, 2 * (size_t)card);
        vec_free(src, 2 * (size_t)card);
    }
    /* An empty Storable vector may hand over any pointer; NULL with a zero
     * length must not reach memcpy (UB). Negative lengths write nothing. */
    WF_CHECK_EQ(hs_wf_roaring_encode_array(NULL, 0, NULL), 0);
    WF_CHECK_EQ(hs_wf_roaring_encode_array(NULL, -1, NULL), 0);
    WF_CHECK_EQ(hs_wf_roaring_encode_array(NULL, INT32_MIN, NULL), 0);
}

/* encode_bitset zeroes stale destination bytes, takes any order and
 * duplicates, and round-trips through decode_bitset / contains. */
static void encode_bitset_cases(void)
{
    wf_rng r = wf_rng_from_env();
    uint8_t *want = malloc(8192);
    int32_t *dec = malloc(65536 * 4);
    long iters = wf_iters(200);
    for (long it = 0; it < iters; it++) {
        int32_t card = (int32_t)(it == 0 ? 0 : it == 1 ? 65536
                                 : (long)wf_rng_below(&r, (it & 1) ? 100 : 20000));
        uint16_t *src = (uint16_t *)vec_alloc(2 * (size_t)card);
        for (int32_t i = 0; i < card; i++) {
            src[i] = it == 1 ? (uint16_t)(65535 - i)
                   : (it & 2) ? (uint16_t)wf_rng_next(&r)
                              : (uint16_t)(65536 - 1 - wf_rng_below(&r, 50));
        }
        memset(want, 0, 8192);
        for (int32_t i = 0; i < card; i++) want[src[i] >> 3] |= (uint8_t)(1u << (src[i] & 7));
        uint8_t *dst = wf_alloc(8192);
        memset(dst, 0xa5, 8192);
        hs_wf_roaring_encode_bitset(src, card, dst);
        WF_CHECK_MEM(dst, want, 8192);
        int32_t n = hs_wf_roaring_decode_bitset(dst, 0, dec);
        int32_t m = 0;
        for (uint32_t v = 0; v < 65536; v++) {
            if ((want[v >> 3] >> (v & 7)) & 1) {
                if (m < n) WF_CHECK_EQ(dec[m], (int32_t)v);
                m++;
            }
            if ((v & 1023) == (uint32_t)(it & 1023)) {
                WF_CHECK_EQ(hs_wf_roaring_contains(1, dst, 0, (uint16_t)v),
                            (want[v >> 3] >> (v & 7)) & 1);
            }
        }
        WF_CHECK_EQ(n, m);
        if (it == 1) WF_CHECK_EQ(n, 65536);
        wf_free(dst, 8192);
        vec_free(src, 2 * (size_t)card);
    }
    hs_wf_roaring_encode_bitset(NULL, 0, want);
    for (int i = 0; i < 8192; i++) WF_CHECK_EQ(want[i], 0);
    free(dec);
    free(want);
}

int main(void)
{
    WF_RUN(decode_array_sweep);
    WF_RUN(decode_hi_contract);
    WF_RUN(decode_array_empty);
    WF_RUN(decode_array_max);
    WF_RUN(decode_bitset_cases);
    WF_RUN(contains_array_sorted);
    WF_RUN(contains_array_unsorted_and_kinds);
    WF_RUN(contains_bitset);
    WF_RUN(contains_array_int32_max);
    WF_RUN(encode_array_cases);
    WF_RUN(encode_bitset_cases);
    return wf_finish("core-hash/test_roaring");
}
