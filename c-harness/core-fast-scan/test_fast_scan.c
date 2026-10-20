/* Unit, boundary and differential tests for wireform-core fast_scan.c. */
#include "harness.h"
#include "api.h"

/* Exhaustive over every (end gap, length, scan offset, match position) in
 * a window that covers the scalar prologue, one or two SIMD blocks and
 * the scalar epilogue. With gap 0 the buffer ends flush against a guard
 * page, so any read past `len` faults; other gaps move the end off the
 * page boundary (so the epilogue runs) and fill the gap with the target
 * byte, which a read past `len` would report as a match. Sweeping `off`
 * sweeps the scan's start alignment. */
static void find_byte_exhaustive_small(void)
{
    enum { MAXN = 80, MAXGAP = 32 };
    wf_guarded g = wf_guard_tail(MAXN + MAXGAP);
    for (int gap = 0; gap < MAXGAP; gap++) {
        for (int len = 0; len <= MAXN; len++) {
            uint8_t *b = g.p + (g.n - (size_t)len - (size_t)gap);
            memset(b, 'a', (size_t)len);
            memset(b + len, 'x', (size_t)gap);
            for (int off = 0; off <= len; off++) {
                WF_CHECK_EQ(hs_find_byte(b, off, len, 'x'), len);
            }
            for (int pos = 0; pos < len; pos++) {
                b[pos] = 'x';
                for (int off = 0; off <= len; off++) {
                    WF_CHECK_EQ(hs_find_byte(b, off, len, 'x'),
                                ref_find_byte(b, off, len, 'x'));
                }
                b[pos] = 'a';
            }
        }
    }
    wf_guard_free(g);
}

/* The first match wins even when several share one SIMD block, and every
 * byte value (including 0x00 and 0x80+, which flip the movemask sign bit)
 * is found. */
static void find_byte_all_values(void)
{
    uint8_t buf[64];
    for (int v = 0; v < 256; v++) {
        for (int i = 0; i < 64; i++) buf[i] = (uint8_t)(v + 1 + i % 7);
        for (int i = 0; i < 64; i++) {
            if (buf[i] == v) buf[i] = (uint8_t)(v + 1);
        }
        uint8_t *b = wf_dup(buf, sizeof buf);
        WF_CHECK_EQ(hs_find_byte(b, 0, 64, (uint8_t)v), 64);
        b[37] = (uint8_t)v;
        b[39] = (uint8_t)v;
        b[63] = (uint8_t)v;
        WF_CHECK_EQ(hs_find_byte(b, 0, 64, (uint8_t)v), 37);
        WF_CHECK_EQ(hs_find_byte(b, 38, 64, (uint8_t)v), 39);
        WF_CHECK_EQ(hs_find_byte(b, 40, 64, (uint8_t)v), 63);
        wf_free(b, sizeof buf);
    }
}

/* offset >= len is a miss that returns len without touching memory. */
static void find_byte_empty_ranges(void)
{
    wf_guarded g = wf_guard_tail(0);
    WF_CHECK_EQ(hs_find_byte(g.p, 0, 0, 0), 0);
    wf_guard_free(g);
    uint8_t one[1] = { 7 };
    WF_CHECK_EQ(hs_find_byte(one, 1, 1, 7), 1);
    WF_CHECK_EQ(hs_find_byte(one, 5, 1, 7), 1);
}

static void find_byte_random(void)
{
    wf_rng r = wf_rng_from_env();
    static const uint8_t alpha[] = { 'a', 'b', ',', '\n', 0, 0x80, 0xff };
    long iters = wf_iters(20000);
    for (long it = 0; it < iters; it++) {
        int len = (int)wf_rng_below(&r, 300);
        uint8_t *tmp = malloc((size_t)len + 1);
        wf_rng_fill_from(&r, tmp, (size_t)len, alpha, sizeof alpha);
        uint8_t *b = wf_dup(tmp, (size_t)len);
        int off = (int)wf_rng_below(&r, (uint64_t)len + 2);
        uint8_t t = alpha[wf_rng_below(&r, sizeof alpha)];
        WF_CHECK_EQ(hs_find_byte(b, off, len, t), ref_find_byte(b, off, len, t));
        wf_free(b, (size_t)len);
        free(tmp);
    }
}

/* Masking against the reference for every start alignment (skew) and
 * length across a few SIMD blocks; masking twice restores the input
 * (involution). With skew > 0 the bytes between the buffer end and the
 * guard page hold a canary that must survive. */
static void ws_mask_exhaustive(void)
{
    enum { MAXN = 100 };
    wf_rng r = wf_rng_from_env();
    uint8_t src[MAXN], want[MAXN];
    wf_guarded g = wf_guard_tail(MAXN + 16);
    static const uint32_t masks[] = { 0, 0xffffffffu, 0x01020304u, 0x80000001u, 0xdeadbeefu };
    static const uint8_t canary[16] = { 0xa5, 0xa5, 0xa5, 0xa5, 0xa5, 0xa5, 0xa5, 0xa5,
                                        0xa5, 0xa5, 0xa5, 0xa5, 0xa5, 0xa5, 0xa5, 0xa5 };
    for (size_t mi = 0; mi < sizeof masks / sizeof masks[0]; mi++) {
        for (int skew = 0; skew < 16; skew++) {
            for (int len = 0; len <= MAXN; len++) {
                uint8_t *b = g.p + (g.n - (size_t)len) - (size_t)skew;
                memcpy(b + len, canary, (size_t)skew);
                wf_rng_fill(&r, src, (size_t)len);
                memcpy(b, src, (size_t)len);
                memcpy(want, src, (size_t)len);
                ref_ws_mask(want, len, masks[mi]);
                hs_ws_mask(b, len, masks[mi]);
                WF_CHECK_MEM(b, want, (size_t)len);
                hs_ws_mask(b, len, masks[mi]);
                WF_CHECK_MEM(b, src, (size_t)len);
                WF_CHECK_MEM(b + len, canary, (size_t)skew);
            }
        }
    }
    wf_guard_free(g);
}

/* The mask phase follows the absolute index, so masking a prefix and
 * then the rest with a rotated key equals masking the whole buffer. This
 * is how a frame split across reads gets unmasked. */
static void ws_mask_split_rotation(void)
{
    wf_rng r = wf_rng_from_env();
    for (long it = 0; it < wf_iters(2000); it++) {
        int len = (int)wf_rng_below(&r, 200);
        int cut = (int)wf_rng_below(&r, (uint64_t)len + 1);
        uint32_t mask = (uint32_t)wf_rng_next(&r);
        uint8_t *a = malloc((size_t)len + 1), *b = malloc((size_t)len + 1);
        wf_rng_fill(&r, a, (size_t)len);
        memcpy(b, a, (size_t)len);
        hs_ws_mask(a, len, mask);
        int rot = (cut & 3) * 8;
        uint32_t rotated = rot ? (mask << rot) | (mask >> (32 - rot)) : mask;
        hs_ws_mask(b, cut, mask);
        hs_ws_mask(b + cut, len - cut, rotated);
        WF_CHECK_MEM(a, b, (size_t)len);
        free(a);
        free(b);
    }
}

int main(void)
{
    (void)wf_rng_from_env();
    WF_RUN(find_byte_exhaustive_small);
    WF_RUN(find_byte_all_values);
    WF_RUN(find_byte_empty_ranges);
    WF_RUN(find_byte_random);
    WF_RUN(ws_mask_exhaustive);
    WF_RUN(ws_mask_split_rotation);
    return wf_finish("core-fast-scan");
}
