/* Unit, boundary and differential tests for the bit kernels, copies,
 * gathers and takes of wireform-columnar-core/cbits/columnar_simd.c.
 *
 * Every input and output buffer covers exactly the bytes the contract
 * allows and ends flush against a guard page (or the ASan redzone), and the
 * bit offsets sweep every phase, so reading or writing one byte too many
 * faults. */
#include "harness.h"
#include "api.h"
#include "testutil.h"

/* ------------------------------------------------------------------ */
/* hs_columnar_bitmap_popcount                                         */
/* ------------------------------------------------------------------ */

static void bitmap_popcount_vectors(void)
{
    static const uint8_t v[] = { 0x00, 0x01, 0x80, 0xFF, 0x55, 0xAA, 0x0F, 0xF0, 0x81, 0x7E, 0x33 };
    uint8_t *b = wf_dup(v, sizeof v);
    WF_CHECK_EQ(hs_columnar_bitmap_popcount(b, 0), 0);
    WF_CHECK_EQ(hs_columnar_bitmap_popcount(b, 1), 0);
    WF_CHECK_EQ(hs_columnar_bitmap_popcount(b, 4), 1 + 1 + 8);
    WF_CHECK_EQ(hs_columnar_bitmap_popcount(b, sizeof v), 0 + 1 + 1 + 8 + 4 + 4 + 4 + 4 + 2 + 6 + 4);
    wf_free(b, sizeof v);
    WF_CHECK_EQ(hs_columnar_bitmap_popcount(NULL, 0), 0);
}

/* Lengths across the 8-byte, 4-byte and byte loops, flush against a guard
 * page at both ends, for all-zero, all-one and random contents; and with
 * the end 0..63 bytes before the guard page, the gap holding 0xFF bytes
 * that would change the count if read. */
static void bitmap_popcount_boundaries(void)
{
    enum { MAXN = 140, GAPS = 64 };
    wf_rng r = wf_rng_from_env();
    wf_guarded t = wf_guard_tail(MAXN + GAPS), h = wf_guard_head(MAXN);
    for (int fill = 0; fill < 3; fill++) {
        for (size_t len = 0; len <= MAXN; len++) {
            for (size_t gap = 0; gap < GAPS; gap += (fill == 2 ? 1 : 9)) {
                uint8_t *bt = gap_of(t, len, gap, 0xFF);
                if (fill == 0) memset(bt, 0, len);
                else if (fill == 1) memset(bt, 0x7E, len);
                else wf_rng_fill(&r, bt, len);
                int64_t want = ref_bitmap_popcount(bt, len);
                WF_CHECK_EQ(hs_columnar_bitmap_popcount(bt, len), want);
                if (gap == 0) {
                    memcpy(h.p, bt, len);
                    WF_CHECK_EQ(hs_columnar_bitmap_popcount(h.p, len), want);
                }
            }
        }
    }
    wf_guard_free(t);
    wf_guard_free(h);
}

/* Regression: the count was an int32 accumulated in a signed int, so a
 * bitmap over 256 MiB overflowed (UB) and returned a wrong count; the C
 * length was a CInt, truncating ByteStrings over 2 GiB. Both are 64-bit now
 * (Columnar.SIMD imports Ptr Word8 -> CSize -> Int64). */
static void bitmap_popcount_over_int32(void)
{
    size_t len = ((size_t)1 << 28) + 11;
    wf_guarded g = wf_guard_tail(len);
    memset(g.p, 0xFF, len);
    g.p[len - 1] = 0x01;
    WF_CHECK_EQ(hs_columnar_bitmap_popcount(g.p, len), (int64_t)8 * (int64_t)len - 7);
    wf_guard_free(g);
}

/* ------------------------------------------------------------------ */
/* hs_columnar_unpack_bits_lsb                                         */
/* ------------------------------------------------------------------ */

static void unpack_bits_boundaries(void)
{
    enum { MAXN = 200, GAPS = 64 };
    wf_rng r = wf_rng_from_env();
    wf_guarded st = wf_guard_tail((MAXN + 7) / 8 + GAPS), sh = wf_guard_head((MAXN + 7) / 8);
    wf_guarded dt = wf_guard_tail(MAXN + GAPS);
    uint8_t want[MAXN];
    for (size_t n = 0; n <= MAXN; n++) {
        size_t nb = (n + 7) / 8;
        for (size_t gap = 0; gap < GAPS; gap += (n < 40 ? 1 : 7)) {
            uint8_t *src = gap_of(st, nb, gap, 0xFF);
            fill_bits(&r, src, nb);
            uint8_t *dst = gap_of(dt, n, gap, 0x5A);
            ref_unpack_bits_lsb(src, n, want);
            memset(dst, 0x77, n);
            hs_columnar_unpack_bits_lsb(src, n, dst);
            WF_CHECK_MEM(dst, want, n);
            WF_CHECK(gap_intact(dst, n, gap, 0x5A));
            if (gap == 0) {
                memcpy(sh.p, src, nb);
                memset(dst, 0x77, n);
                hs_columnar_unpack_bits_lsb(sh.p, n, dst);
                WF_CHECK_MEM(dst, want, n);
            }
        }
    }
    hs_columnar_unpack_bits_lsb(NULL, 0, NULL);
    uint8_t one = 0xA5, out[8];
    hs_columnar_unpack_bits_lsb(&one, 8, out);
    static const uint8_t exp[8] = { 1, 0, 1, 0, 0, 1, 0, 1 };
    WF_CHECK_MEM(out, exp, 8);
    wf_guard_free(st);
    wf_guard_free(sh);
    wf_guard_free(dt);
}

/* ------------------------------------------------------------------ */
/* hs_columnar_memcpy_fast                                             */
/* ------------------------------------------------------------------ */

/* Every length across one to four 16-byte blocks; both buffers flush
 * against a guard page, then each end 0..63 bytes before it (every
 * relative src/dst alignment), with a canary after a shifted dst; then
 * both starting at a guard page. */
static void memcpy_fast_boundaries(void)
{
    enum { MAXN = 70, GAPS = 64 };
    wf_rng r = wf_rng_from_env();
    wf_guarded s = wf_guard_tail(MAXN + GAPS), d = wf_guard_tail(MAXN + GAPS);
    wf_guarded sh = wf_guard_head(MAXN), dh = wf_guard_head(MAXN);
    for (size_t len = 0; len <= MAXN; len++) {
        for (unsigned sg = 0; sg < GAPS; sg += (len % 3 ? 7 : 1)) {
            for (unsigned dg = 0; dg < GAPS; dg += (len % 3 ? 5 : 3)) {
                uint8_t *src = gap_of(s, len, sg, 0xEE);
                uint8_t *dst = gap_of(d, len, dg, 0x5A);
                wf_rng_fill(&r, src, len);
                memset(dst, 0, len);
                hs_columnar_memcpy_fast(src, dst, len);
                WF_CHECK_MEM(dst, src, len);
                WF_CHECK(gap_intact(dst, len, dg, 0x5A));
            }
        }
        wf_rng_fill(&r, sh.p, len);
        hs_columnar_memcpy_fast(sh.p, dh.p, len);
        WF_CHECK_MEM(dh.p, sh.p, len);
    }
    hs_columnar_memcpy_fast(NULL, NULL, 0);
    wf_guard_free(s);
    wf_guard_free(d);
    wf_guard_free(sh);
    wf_guard_free(dh);
}

/* Both length parameters are size_t now (they were the last int32_t
 * lengths in this file). A >2 GiB case cannot be run here: unlike the
 * sparse-offset kernels, these two touch every byte they are given, so any
 * such test would allocate and dirty the whole range. api.h asserts the
 * widened parameter types at compile time instead; this test runs the same
 * loops over megabytes of guarded data, far past the boundary sweeps
 * above. */
static void bulk_kernels_large(void)
{
    enum { N = 4 << 20 };
    wf_rng r = wf_rng_from_env();
    wf_guarded s = wf_guard_tail(N), d = wf_guard_tail(N);
    wf_rng_fill(&r, s.p, N);

    memset(d.p, 0, N);
    hs_columnar_memcpy_fast(s.p, d.p, N);
    WF_CHECK_MEM(d.p, s.p, N);

    /* N bits (N / 8 source bytes) expand to N bytes. */
    uint8_t *want = malloc(N);
    ref_unpack_bits_lsb(s.p, N, want);
    memset(d.p, 0x77, N);
    hs_columnar_unpack_bits_lsb(s.p, N, d.p);
    WF_CHECK_MEM(d.p, want, N);
    free(want);

    wf_guard_free(s);
    wf_guard_free(d);
}

/* ------------------------------------------------------------------ */
/* hs_columnar_popcount_bits                                           */
/* ------------------------------------------------------------------ */

/* Every bit phase and every length across the head byte, the 32-byte
 * unrolled loop, the 8-byte loop, the byte loop and the tail bits. The
 * buffer holds exactly the bytes covering [off, off + n) and ends at a
 * guard page, or (alternately) 1..63 bytes before it with 0xFF canary bytes
 * in the gap; with off < 8 it also starts at one. */
static void popcount_bits_boundaries(void)
{
    enum { MAXOFF = 24, MAXN = 640, MAXB = (MAXOFF + MAXN + 7) / 8 };
    wf_rng r = wf_rng_from_env();
    wf_guarded t = wf_guard_tail(MAXB + 64), h = wf_guard_head(MAXB);
    for (size_t off = 0; off < MAXOFF; off++) {
        for (size_t n = 0; n <= MAXN; n++) {
            size_t nb = (off + n + 7) / 8;
            size_t gap = (n + off) % 2 ? 0 : (n * 7 + off) % 64;
            uint8_t *b = gap_of(t, nb, gap, 0xFF);
            fill_bits(&r, b, nb);
            /* bits of the last byte past the range are set: they must not count */
            if ((off + n) % 8) b[nb - 1] |= (uint8_t)(0xFF << ((off + n) % 8));
            int64_t want = ref_popcount_bits(b, off, n);
            WF_CHECK_EQ(hs_columnar_popcount_bits(b, off, n), want);
            if (off < 8) {
                memcpy(h.p, b, nb);
                WF_CHECK_EQ(hs_columnar_popcount_bits(h.p, off, n), want);
            }
        }
    }
    WF_CHECK_EQ(hs_columnar_popcount_bits(NULL, 0, 0), 0);
    WF_CHECK_EQ(hs_columnar_popcount_bits(NULL, 12345, 0), 0);
    wf_guard_free(t);
    wf_guard_free(h);
}

/* ------------------------------------------------------------------ */
/* hs_columnar_copy_bits                                               */
/* ------------------------------------------------------------------ */

static void check_copy(wf_rng *r, uint8_t *dst, size_t doff, const uint8_t *src, size_t soff, size_t n,
                       size_t dbytes, uint8_t *want)
{
    fill_bits(r, dst, dbytes);
    memcpy(want, dst, dbytes);
    ref_copy_bits(want, doff, src, soff, n);
    hs_columnar_copy_bits(dst, doff, src, soff, n);
    WF_CHECK_MEM(dst, want, dbytes);
}

/* Same-phase and shifting paths: every (dst phase, src phase) pair over
 * two bytes of offset, lengths through several 64-bit words. Both buffers
 * hold exactly the covering bytes and end at a guard page or 1..63 bytes
 * before it (canary in the gap), and dst bits outside the range must
 * survive. */
static void copy_bits_boundaries(void)
{
    enum { MAXOFF = 18, MAXN = 270, MAXB = (MAXOFF + MAXN + 7) / 8 };
    wf_rng r = wf_rng_from_env();
    wf_guarded st = wf_guard_tail(MAXB + 64), dt = wf_guard_tail(MAXB + 64);
    wf_guarded sh = wf_guard_head(MAXB), dh = wf_guard_head(MAXB);
    uint8_t want[MAXB];
    for (size_t doff = 0; doff < MAXOFF; doff++) {
        for (size_t soff = 0; soff < MAXOFF; soff++) {
            for (size_t n = 0; n <= MAXN; n++) {
                size_t sb = (soff + n + 7) / 8, db = (doff + n + 7) / 8;
                size_t sg = (n + soff) % 2 ? 0 : (n * 3 + doff) % 64;
                size_t dg = (n + doff) % 3 ? 0 : (n * 5 + soff) % 64;
                uint8_t *src = gap_of(st, sb, sg, 0xA5);
                fill_bits(&r, src, sb);
                uint8_t *dst = gap_of(dt, db, dg, 0x5A);
                check_copy(&r, dst, doff, src, soff, n, db, want);
                WF_CHECK(gap_intact(dst, db, dg, 0x5A));
                if (doff < 8 && soff < 8 && n % 7 == 0) {
                    memcpy(sh.p, src, sb);
                    check_copy(&r, dh.p, doff, sh.p, soff, n, db, want);
                }
            }
        }
    }
    hs_columnar_copy_bits(NULL, 0, NULL, 0, 0);
    hs_columnar_copy_bits(NULL, 99, NULL, 3, 0);
    wf_guard_free(st);
    wf_guard_free(dt);
    wf_guard_free(sh);
    wf_guard_free(dh);
}

/* ------------------------------------------------------------------ */
/* hs_columnar_and_bits                                                */
/* ------------------------------------------------------------------ */

static void and_bits_boundaries(void)
{
    enum { MAXOFF = 18, MAXN = 200, MAXB = (MAXOFF + MAXN + 7) / 8 };
    wf_rng r = wf_rng_from_env();
    wf_guarded at = wf_guard_tail(MAXB + 64), bt = wf_guard_tail(MAXB + 64), dt = wf_guard_tail(MAXB + 64);
    wf_guarded ah = wf_guard_head(MAXB), bh = wf_guard_head(MAXB);
    uint8_t want[MAXB];
    for (size_t aoff = 0; aoff < MAXOFF; aoff++) {
        for (size_t boff = 0; boff < MAXOFF; boff++) {
            for (size_t n = 0; n <= MAXN; n++) {
                size_t ab = (aoff + n + 7) / 8, bb = (boff + n + 7) / 8, db = (n + 7) / 8;
                size_t g = (n + aoff + boff) % 2 ? 0 : (n * 3 + aoff * 5 + boff) % 64;
                uint8_t *a = gap_of(at, ab, g, 0xFF), *b = gap_of(bt, bb, (g * 7) % 64, 0xFF);
                uint8_t *d = gap_of(dt, db, (g * 3) % 64, 0x5A);
                fill_bits(&r, a, ab);
                fill_bits(&r, b, bb);
                memset(d, 0xEE, db);
                int64_t wc = ref_and_bits(want, a, aoff, b, boff, n);
                WF_CHECK_EQ(hs_columnar_and_bits(d, a, aoff, b, boff, n), wc);
                WF_CHECK_MEM(d, want, db);
                WF_CHECK(gap_intact(d, db, (g * 3) % 64, 0x5A));
                if (aoff < 8 && boff < 8 && n % 5 == 0) {
                    memcpy(ah.p, a, ab);
                    memcpy(bh.p, b, bb);
                    memset(d, 0xEE, db);
                    WF_CHECK_EQ(hs_columnar_and_bits(d, ah.p, aoff, bh.p, boff, n), wc);
                    WF_CHECK_MEM(d, want, db);
                }
            }
        }
    }
    WF_CHECK_EQ(hs_columnar_and_bits(NULL, NULL, 5, NULL, 9, 0), 0);
    wf_guard_free(at);
    wf_guard_free(bt);
    wf_guard_free(dt);
    wf_guard_free(ah);
    wf_guard_free(bh);
}

/* ------------------------------------------------------------------ */
/* hs_columnar_gather_bits                                             */
/* ------------------------------------------------------------------ */

static void gather_bits_boundaries(void)
{
    enum { MAXN = 200, SRCBITS = 300 };
    wf_rng r = wf_rng_from_env();
    wf_guarded dt = wf_guard_tail((MAXN + 7) / 8);
    uint8_t want[(MAXN + 7) / 8];
    int64_t idx[MAXN];
    for (size_t soff = 0; soff < 20; soff++) {
        size_t sb = (soff + SRCBITS + 7) / 8;
        xbuf src;
        uint8_t tmp[(20 + SRCBITS + 7) / 8];
        fill_bits(&r, tmp, sb);
        src = xb_new(tmp, sb, 0);
        for (size_t n = 0; n <= MAXN; n++) {
            for (size_t k = 0; k < n; k++) {
                /* sorted, reversed, repeated and random index patterns */
                switch (n % 4) {
                case 0: idx[k] = (int64_t)(k % SRCBITS); break;
                case 1: idx[k] = (int64_t)(SRCBITS - 1 - k % SRCBITS); break;
                case 2: idx[k] = (int64_t)(SRCBITS - 1); break;
                default: idx[k] = (int64_t)wf_rng_below(&r, SRCBITS); break;
                }
            }
            xbuf ix = xb_new(idx, n * 8, (unsigned)(n % 8));
            size_t db = (n + 7) / 8;
            uint8_t *d = tail_of(dt, db);
            memset(d, 0xEE, db);
            int64_t wc = ref_gather_bits(want, src.p, soff, idx, n);
            WF_CHECK_EQ(hs_columnar_gather_bits(d, src.p, soff, (const int64_t *)(void *)ix.p, n), wc);
            WF_CHECK_MEM(d, want, db);
            xb_free(ix);
        }
        xb_free(src);
    }
    WF_CHECK_EQ(hs_columnar_gather_bits(NULL, NULL, 0, NULL, 0), 0);
    wf_guard_free(dt);
}

/* ------------------------------------------------------------------ */
/* Fixed-width gathers                                                 */
/* ------------------------------------------------------------------ */

typedef void (*gather_fn)(uint8_t *, const uint8_t *, const int64_t *, size_t);

static void gathers(void)
{
    static const gather_fn fns[5] = { hs_columnar_gather_1, hs_columnar_gather_2, hs_columnar_gather_4,
                                      hs_columnar_gather_8, hs_columnar_gather_16 };
    static const size_t ws[5] = { 1, 2, 4, 8, 16 };
    wf_rng r = wf_rng_from_env();
    long iters = wf_iters(400);
    for (int f = 0; f < 5; f++) {
        size_t w = ws[f];
        fns[f](NULL, NULL, NULL, 0);
        for (long it = 0; it < iters; it++) {
            size_t rows = 1 + wf_rng_below(&r, 70), n = wf_rng_below(&r, 70);
            uint8_t *tmp = malloc(rows * w);
            int64_t *idx = malloc((n ? n : 1) * 8);
            uint8_t *want = malloc((n ? n : 1) * w);
            wf_rng_fill(&r, tmp, rows * w);
            for (size_t k = 0; k < n; k++) {
                idx[k] = (int64_t)(it % 3 == 0 ? rows - 1 : wf_rng_below(&r, rows));
            }
            xbuf src = xb_new(tmp, rows * w, (unsigned)wf_rng_below(&r, 16));
            xbuf ix = xb_new(idx, n * 8, (unsigned)wf_rng_below(&r, 8));
            uint8_t *dst = wf_alloc(n * w);
            ref_gather(want, src.p, idx, n, w);
            fns[f](dst, src.p, (const int64_t *)(void *)ix.p, n);
            WF_CHECK_MEM(dst, want, n * w);
            wf_free(dst, n * w);
            xb_free(src);
            xb_free(ix);
            free(tmp);
            free(idx);
            free(want);
        }
    }
}

/* ------------------------------------------------------------------ */
/* Takes                                                               */
/* ------------------------------------------------------------------ */

/* Random validated offsets (some empty rows), random row selection; the
 * offsets, indices, source bytes and both outputs are exact-size. */
static void take_random(void)
{
    wf_rng r = wf_rng_from_env();
    long iters = wf_iters(3000);
    for (long it = 0; it < iters; it++) {
        size_t rows = wf_rng_below(&r, 40), n = rows ? wf_rng_below(&r, 60) : 0;
        int64_t offs[41], idx[60], wantOffs[61], gotOffs64[61];
        int32_t offs32[41], gotOffs32[61];
        offs[0] = (int64_t)wf_rng_below(&r, 5);
        for (size_t i = 1; i <= rows; i++) {
            offs[i] = offs[i - 1] + (wf_rng_below(&r, 3) ? (int64_t)wf_rng_below(&r, 9) : 0);
        }
        for (size_t i = 0; i <= rows; i++) offs32[i] = (int32_t)offs[i];
        for (size_t k = 0; k < n; k++) idx[k] = (int64_t)wf_rng_below(&r, rows);
        size_t dlen = (size_t)offs[rows];
        uint8_t data[400];
        wf_rng_fill(&r, data, dlen);

        xbuf ix = xb_new(idx, n * 8, (unsigned)wf_rng_below(&r, 8));
        xbuf o32 = xb_new(offs32, (rows + 1) * 4, (unsigned)wf_rng_below(&r, 4));
        xbuf o64 = xb_new(offs, (rows + 1) * 8, (unsigned)wf_rng_below(&r, 8));
        uint8_t *src = dlen ? wf_dup(data, dlen) : NULL;
        const int64_t *pix = (const int64_t *)(void *)ix.p;

        int64_t total = ref_take_offsets(wantOffs, offs, idx, n, INT32_MAX);
        uint8_t want[2400];
        size_t wlen = ref_take_bytes(want, offs, data, idx, n);
        WF_CHECK_EQ(total, (int64_t)wlen);

        uint8_t *do32 = wf_alloc((n + 1) * 4);
        WF_CHECK_EQ(hs_columnar_take_offsets_i32((int32_t *)(void *)do32, (const int32_t *)(void *)o32.p, pix, n), total);
        memcpy(gotOffs32, do32, (n + 1) * 4);
        for (size_t k = 0; k <= n; k++) WF_CHECK_EQ(gotOffs32[k], wantOffs[k]);
        wf_free(do32, (n + 1) * 4);

        uint8_t *do64 = wf_alloc((n + 1) * 8);
        WF_CHECK_EQ(hs_columnar_take_offsets_i64((int64_t *)(void *)do64, (const int64_t *)(void *)o64.p, pix, n), total);
        memcpy(gotOffs64, do64, (n + 1) * 8);
        for (size_t k = 0; k <= n; k++) WF_CHECK_EQ(gotOffs64[k], wantOffs[k]);
        wf_free(do64, (n + 1) * 8);

        /* An empty result gets a NULL destination, as an empty ByteString
         * would hand over. */
        uint8_t *dst = wlen ? wf_alloc(wlen) : NULL;
        hs_columnar_take_bytes_i32(dst, (const int32_t *)(void *)o32.p, src, pix, n);
        WF_CHECK_MEM(dst, want, wlen);
        if (wlen) memset(dst, 0, wlen);
        hs_columnar_take_bytes_i64(dst, (const int64_t *)(void *)o64.p, src, pix, n);
        WF_CHECK_MEM(dst, want, wlen);
        if (dst) wf_free(dst, wlen);
        if (src) wf_free(src, dlen);
        xb_free(ix);
        xb_free(o32);
        xb_free(o64);
    }
}

/* Regression: takeBytes on rows that are all empty, with the empty data
 * buffer and destination passed as NULL (what an empty ByteString's
 * pointer is), formed NULL + 0 and called memcpy(NULL, NULL, 0); both are
 * UB (UBSan: "applying zero offset to null pointer"). */
static void take_bytes_null_empty(void)
{
    static const int32_t o32[4] = { 0, 0, 0, 0 };
    static const int64_t o64[4] = { 0, 0, 0, 0 };
    static const int64_t idx[5] = { 0, 2, 1, 1, 0 };
    hs_columnar_take_bytes_i32(NULL, o32, NULL, idx, 5);
    hs_columnar_take_bytes_i64(NULL, o64, NULL, idx, 5);
    int32_t d32[6];
    int64_t d64[6];
    WF_CHECK_EQ(hs_columnar_take_offsets_i32(d32, o32, idx, 5), 0);
    WF_CHECK_EQ(hs_columnar_take_offsets_i64(d64, o64, idx, 5), 0);
    for (int k = 0; k < 6; k++) {
        WF_CHECK_EQ(d32[k], 0);
        WF_CHECK_EQ(d64[k], 0);
    }
    hs_columnar_take_bytes_i32(NULL, o32, NULL, NULL, 0);
    hs_columnar_take_bytes_i64(NULL, o64, NULL, NULL, 0);
}

/* Totals at and past the offset type's range. */
static void take_offsets_overflow(void)
{
    int32_t d32[4];
    int64_t d64[4];
    {
        const int32_t o[3] = { 0, INT32_MAX, INT32_MAX };
        const int64_t one[1] = { 0 }, two[2] = { 0, 1 }, three[3] = { 1, 0, 1 };
        WF_CHECK_EQ(hs_columnar_take_offsets_i32(d32, o, one, 1), INT32_MAX);
        WF_CHECK_EQ(d32[1], INT32_MAX);
        WF_CHECK_EQ(hs_columnar_take_offsets_i32(d32, o, two, 2), INT32_MAX);
        WF_CHECK_EQ(hs_columnar_take_offsets_i32(d32, o, three, 3), INT32_MAX);
        const int64_t twice[2] = { 0, 0 };
        WF_CHECK_EQ(hs_columnar_take_offsets_i32(d32, o, twice, 2), -1);
    }
    {
        const int32_t o[3] = { 0, 1, INT32_MAX };
        const int64_t sel[3] = { 1, 0, 0 };
        WF_CHECK_EQ(hs_columnar_take_offsets_i32(d32, o, sel, 2), INT32_MAX);
        WF_CHECK_EQ(hs_columnar_take_offsets_i32(d32, o, sel, 3), -1);
        const int64_t sel1[1] = { 1 };
        WF_CHECK_EQ(hs_columnar_take_offsets_i32(d32, o, sel1, 1), INT32_MAX - 1);
    }
    {
        const int64_t o[3] = { 0, INT64_MAX, INT64_MAX };
        const int64_t one[1] = { 0 }, twice[2] = { 0, 0 }, mix[3] = { 1, 0, 1 };
        WF_CHECK_EQ(hs_columnar_take_offsets_i64(d64, o, one, 1), INT64_MAX);
        WF_CHECK_EQ(hs_columnar_take_offsets_i64(d64, o, mix, 3), INT64_MAX);
        WF_CHECK_EQ(hs_columnar_take_offsets_i64(d64, o, twice, 2), -1);
    }
    {
        const int64_t o[3] = { 0, INT64_MAX / 2 + 1, INT64_MAX };
        const int64_t both[2] = { 0, 1 }, first2[2] = { 0, 0 };
        WF_CHECK_EQ(hs_columnar_take_offsets_i64(d64, o, both, 2), INT64_MAX);
        WF_CHECK_EQ(hs_columnar_take_offsets_i64(d64, o, first2, 2), -1);
    }
    /* n == 0 writes the single leading 0. */
    d32[0] = 7;
    d64[0] = 7;
    WF_CHECK_EQ(hs_columnar_take_offsets_i32(d32, NULL, NULL, 0), 0);
    WF_CHECK_EQ(hs_columnar_take_offsets_i64(d64, NULL, NULL, 0), 0);
    WF_CHECK_EQ(d32[0], 0);
    WF_CHECK_EQ(d64[0], 0);
}

/* ------------------------------------------------------------------ */
/* Offset rebasing                                                     */
/* ------------------------------------------------------------------ */

static void rebase_offsets(void)
{
    wf_rng r = wf_rng_from_env();
    static const int64_t deltas[] = { 0, 1, -1, INT32_MAX, INT32_MIN, (int64_t)1 << 32, INT64_MAX, INT64_MIN,
                                      -((int64_t)1 << 40) + 3 };
    long iters = wf_iters(200);
    for (long it = 0; it < iters; it++) {
        size_t n = wf_rng_below(&r, 80);
        int32_t a32[80], w32[80];
        int64_t a64[80], w64[80];
        wf_rng_fill(&r, (uint8_t *)a32, sizeof a32);
        wf_rng_fill(&r, (uint8_t *)a64, sizeof a64);
        if (it % 4 == 0) {
            for (size_t i = 0; i < n; i++) a32[i] = (i & 1) ? INT32_MAX : INT32_MIN;
            for (size_t i = 0; i < n; i++) a64[i] = (i & 1) ? INT64_MAX : INT64_MIN;
        }
        int64_t delta = it < (long)(sizeof deltas / sizeof deltas[0]) ? deltas[it] : (int64_t)wf_rng_next(&r);
        for (size_t i = 0; i < n; i++) {
            w32[i] = (int32_t)(uint32_t)((uint64_t)(int64_t)a32[i] + (uint64_t)delta);
            w64[i] = (int64_t)((uint64_t)a64[i] + (uint64_t)delta);
        }
        xbuf s32 = xb_new(a32, n * 4, (unsigned)wf_rng_below(&r, 4));
        xbuf s64 = xb_new(a64, n * 8, (unsigned)wf_rng_below(&r, 8));
        xbuf d32 = xb_new(NULL, n * 4, (unsigned)wf_rng_below(&r, 4));
        xbuf d64 = xb_new(NULL, n * 8, (unsigned)wf_rng_below(&r, 8));
        hs_columnar_rebase_offsets_i32((int32_t *)(void *)d32.p, (const int32_t *)(void *)s32.p, n, delta);
        hs_columnar_rebase_offsets_i64((int64_t *)(void *)d64.p, (const int64_t *)(void *)s64.p, n, delta);
        WF_CHECK_MEM(d32.p, w32, n * 4);
        WF_CHECK_MEM(d64.p, w64, n * 8);
        /* in place */
        hs_columnar_rebase_offsets_i32((int32_t *)(void *)s32.p, (const int32_t *)(void *)s32.p, n, delta);
        hs_columnar_rebase_offsets_i64((int64_t *)(void *)s64.p, (const int64_t *)(void *)s64.p, n, delta);
        WF_CHECK_MEM(s32.p, w32, n * 4);
        WF_CHECK_MEM(s64.p, w64, n * 8);
        xb_free(s32);
        xb_free(s64);
        xb_free(d32);
        xb_free(d64);
    }
    hs_columnar_rebase_offsets_i32(NULL, NULL, 0, 5);
    hs_columnar_rebase_offsets_i64(NULL, NULL, 0, 5);
}

/* ------------------------------------------------------------------ */
/* Random differential over larger ranges                              */
/* ------------------------------------------------------------------ */

static void bits_random_large(void)
{
    wf_rng r = wf_rng_from_env();
    long iters = wf_iters(300);
    for (long it = 0; it < iters; it++) {
        size_t n = wf_rng_below(&r, 5000);
        size_t aoff = wf_rng_below(&r, 1000), boff = wf_rng_below(&r, 1000);
        size_t ab = (aoff + n + 7) / 8, bb = (boff + n + 7) / 8, db = (n + 7) / 8;
        uint8_t *ta = malloc(ab + 1), *tb = malloc(bb + 1);
        fill_bits(&r, ta, ab);
        fill_bits(&r, tb, bb);
        uint8_t *a = wf_dup(ta, ab), *b = wf_dup(tb, bb);
        WF_CHECK_EQ(hs_columnar_popcount_bits(a, aoff, n), ref_popcount_bits(a, aoff, n));
        uint8_t *want = malloc(db + 1), *d = wf_alloc(db);
        WF_CHECK_EQ(hs_columnar_and_bits(d, a, aoff, b, boff, n), ref_and_bits(want, a, aoff, b, boff, n));
        WF_CHECK_MEM(d, want, db);
        wf_free(d, db);
        /* copy a's range into b's range */
        uint8_t *wb = malloc(bb + 1);
        memcpy(wb, b, bb);
        ref_copy_bits(wb, boff, a, aoff, n);
        hs_columnar_copy_bits(b, boff, a, aoff, n);
        WF_CHECK_MEM(b, wb, bb);
        WF_CHECK_EQ(hs_columnar_bitmap_popcount(a, ab), ref_bitmap_popcount(a, ab));
        free(wb);
        free(want);
        wf_free(a, ab);
        wf_free(b, bb);
        free(ta);
        free(tb);
    }
}

int main(void)
{
    WF_RUN(bitmap_popcount_vectors);
    WF_RUN(bitmap_popcount_boundaries);
    WF_RUN(bitmap_popcount_over_int32);
    WF_RUN(unpack_bits_boundaries);
    WF_RUN(memcpy_fast_boundaries);
    WF_RUN(bulk_kernels_large);
    WF_RUN(popcount_bits_boundaries);
    WF_RUN(copy_bits_boundaries);
    WF_RUN(and_bits_boundaries);
    WF_RUN(gather_bits_boundaries);
    WF_RUN(gathers);
    WF_RUN(take_random);
    WF_RUN(take_bytes_null_empty);
    WF_RUN(take_offsets_overflow);
    WF_RUN(rebase_offsets);
    WF_RUN(bits_random_large);
    return wf_finish("columnar-simd test_bits");
}
