/* Unit, boundary and differential tests for the Arrow buffer validators of
 * wireform-columnar-core/cbits/columnar_simd.c: offsets, run ends, UTF-8
 * boundaries, dictionary keys, list views, dense unions and binary/UTF-8
 * views. Every array is exact-size (flush against a guard page or the ASan
 * redzone), sizes sweep across the 64-row and 256-row blocks, and failures
 * are planted at every position near the block edges. */
#include "harness.h"
#include "api.h"
#include "testutil.h"

/* Typed array of width w (1, 2, 4, 8) from widened values. */
static void narrow(uint8_t *dst, const int64_t *v, size_t n, size_t w)
{
    for (size_t i = 0; i < n; i++) {
        uint64_t x = (uint64_t)v[i];
        for (size_t b = 0; b < w; b++) dst[i * w + b] = (uint8_t)(x >> (8 * b));
    }
}

static int64_t type_min(size_t w)
{
    return w == 8 ? INT64_MIN : -((int64_t)1 << (8 * w - 1));
}

static int64_t type_max(size_t w)
{
    return w == 8 ? INT64_MAX : ((int64_t)1 << (8 * w - 1)) - 1;
}

/* ------------------------------------------------------------------ */
/* Offsets and run ends                                                */
/* ------------------------------------------------------------------ */

static int64_t call_offsets(const uint8_t *p, size_t n, int64_t limit, size_t w)
{
    return w == 4 ? hs_columnar_offsets_i32((const int32_t *)(const void *)p, n, limit)
                  : hs_columnar_offsets_i64((const int64_t *)(const void *)p, n, limit);
}

static int64_t call_run_ends(const uint8_t *p, size_t n, int64_t minEnd, size_t w)
{
    switch (w) {
    case 2: return hs_columnar_run_ends_i16((const int16_t *)(const void *)p, n, minEnd);
    case 4: return hs_columnar_run_ends_i32((const int32_t *)(const void *)p, n, minEnd);
    default: return hs_columnar_run_ends_i64((const int64_t *)(const void *)p, n, minEnd);
    }
}

enum { VMAXN = 530 };

/* Positions worth corrupting for an array of n: all of them while small,
 * else the block edges and a few random ones. */
static int pick_position(size_t n, size_t i)
{
    if (n <= 70) return 1;
    size_t m = i % 256;
    return i < 3 || m < 2 || m > 253 || i % 64 < 1 || i % 64 > 62 || i + 3 > n;
}

static void offsets_sweep(void)
{
    wf_rng r = wf_rng_from_env();
    static const size_t ws[2] = { 4, 8 };
    int64_t v[VMAXN];
    for (int wi = 0; wi < 2; wi++) {
        size_t w = ws[wi];
        wf_guarded g = wf_guard_tail(VMAXN * w);
        for (size_t n = 0; n <= VMAXN; n++) {
            v[0] = (int64_t)wf_rng_below(&r, 3);
            for (size_t i = 1; i < n; i++) v[i] = v[i - 1] + (int64_t)wf_rng_below(&r, 4);
            uint8_t *p = tail_of(g, n * w);
            int64_t last = n ? v[n - 1] : 0;
            const int64_t limits[4] = { last, last - 1, last + 1, type_min(8) };
            for (int li = 0; li < 4; li++) {
                narrow(p, v, n, w);
                WF_CHECK_EQ(call_offsets(p, n, limits[li], w), ref_offsets(v, n, limits[li]));
            }
            for (size_t i = 0; i < n; i++) {
                if (!pick_position(n, i)) continue;
                int64_t save = v[i];
                v[i] = i == 0 ? -1 : v[i - 1] - 1 - (int64_t)wf_rng_below(&r, 2);
                narrow(p, v, n, w);
                WF_CHECK_EQ(call_offsets(p, n, last + 5, w), ref_offsets(v, n, last + 5));
                v[i] = save;
            }
        }
        /* Extremes: the type's min/max as offsets and limits. */
        int64_t ext[3] = { 0, type_max(w), type_max(w) };
        uint8_t *p = tail_of(g, 3 * w);
        narrow(p, ext, 3, w);
        WF_CHECK_EQ(call_offsets(p, 3, INT64_MAX, w), -1);
        WF_CHECK_EQ(call_offsets(p, 3, type_max(w) - 1, w), 2);
        ext[0] = type_min(w);
        narrow(p, ext, 3, w);
        WF_CHECK_EQ(call_offsets(p, 3, INT64_MAX, w), 0);
        ext[0] = 0, ext[1] = type_max(w), ext[2] = type_min(w);
        narrow(p, ext, 3, w);
        WF_CHECK_EQ(call_offsets(p, 3, INT64_MAX, w), 2);
        WF_CHECK_EQ(call_offsets(NULL, 0, INT64_MIN, w), -1);
        wf_guard_free(g);
    }
}

static void run_ends_sweep(void)
{
    wf_rng r = wf_rng_from_env();
    static const size_t ws[3] = { 2, 4, 8 };
    int64_t v[VMAXN];
    for (int wi = 0; wi < 3; wi++) {
        size_t w = ws[wi];
        wf_guarded g = wf_guard_tail(VMAXN * w);
        for (size_t n = 0; n <= VMAXN; n++) {
            v[0] = 1 + (int64_t)wf_rng_below(&r, 3);
            for (size_t i = 1; i < n; i++) v[i] = v[i - 1] + 1 + (int64_t)wf_rng_below(&r, 4);
            uint8_t *p = tail_of(g, n * w);
            int64_t last = n ? v[n - 1] : 0;
            const int64_t mins[5] = { last, last + 1, last - 1, 0, INT64_MIN };
            for (int mi = 0; mi < 5; mi++) {
                narrow(p, v, n, w);
                WF_CHECK_EQ(call_run_ends(p, n, mins[mi], w), ref_run_ends(v, n, mins[mi]));
            }
            for (size_t i = 0; i < n; i++) {
                if (!pick_position(n, i)) continue;
                int64_t save = v[i];
                v[i] = i == 0 ? -(int64_t)wf_rng_below(&r, 2) : v[i - 1] - (int64_t)wf_rng_below(&r, 2);
                narrow(p, v, n, w);
                WF_CHECK_EQ(call_run_ends(p, n, 1, w), ref_run_ends(v, n, 1));
                v[i] = save;
            }
        }
        WF_CHECK_EQ(call_run_ends(NULL, 0, 0, w), -1);
        WF_CHECK_EQ(call_run_ends(NULL, 0, -5, w), -1);
        WF_CHECK_EQ(call_run_ends(NULL, 0, 1, w), 0);
        int64_t ext[2] = { 1, type_max(w) };
        uint8_t *p = tail_of(g, 2 * w);
        narrow(p, ext, 2, w);
        WF_CHECK_EQ(call_run_ends(p, 2, type_max(w), w), -1);
        WF_CHECK_EQ(call_run_ends(p, 2, INT64_MAX, w), w == 8 ? -1 : 1);
        ext[1] = type_min(w);
        narrow(p, ext, 2, w);
        WF_CHECK_EQ(call_run_ends(p, 2, 0, w), 1);
        wf_guard_free(g);
    }
}

/* ------------------------------------------------------------------ */
/* UTF-8 boundaries                                                    */
/* ------------------------------------------------------------------ */

static void utf8_boundaries_sweep(void)
{
    wf_rng r = wf_rng_from_env();
    static const uint8_t alpha[] = { 'a', 0x7F, 0x80, 0xBF, 0xC3, 0xE2, 0xF0, 0xC0, 0xFF };
    int64_t v[VMAXN];
    for (int wi = 0; wi < 2; wi++) {
        size_t w = wi ? 8 : 4;
        wf_guarded go = wf_guard_tail(VMAXN * w), gd = wf_guard_tail(300);
        for (size_t n = 0; n <= VMAXN; n++) {
            size_t len = wf_rng_below(&r, 300);
            uint8_t *d = tail_of(gd, len);
            wf_rng_fill_from(&r, d, len, alpha, sizeof alpha);
            /* valid offsets against len: non-decreasing within [0, len] */
            int64_t o = 0;
            for (size_t i = 0; i < n; i++) {
                if (len) o += (int64_t)wf_rng_below(&r, 3);
                if (o > (int64_t)len) o = (int64_t)len;
                v[i] = o;
            }
            /* Mostly boundaries: move continuation-byte hits off unless
             * this round plants one at a chosen position. */
            size_t plant = n ? wf_rng_below(&r, n) : 0;
            for (size_t i = 0; i < n; i++) {
                if (i != plant && v[i] < (int64_t)len && (d[v[i]] & 0xC0) == 0x80 && wf_rng_below(&r, 8)) {
                    d[v[i]] = 'a';
                }
            }
            uint8_t *p = tail_of(go, n * w);
            narrow(p, v, n, w);
            int64_t want = ref_utf8_boundaries(v, n, d, (int64_t)len);
            int64_t got = w == 4 ? hs_columnar_utf8_boundaries_i32((const int32_t *)(void *)p, n, d, (int64_t)len)
                                 : hs_columnar_utf8_boundaries_i64((const int64_t *)(void *)p, n, d, (int64_t)len);
            WF_CHECK_EQ(got, want);
        }
        /* len == 0: nothing is read, any (empty-range) offsets pass. */
        int64_t zeros[3] = { 0, 0, 0 };
        uint8_t *p = tail_of(go, 3 * w);
        narrow(p, zeros, 3, w);
        WF_CHECK_EQ(w == 4 ? hs_columnar_utf8_boundaries_i32((const int32_t *)(void *)p, 3, NULL, 0)
                           : hs_columnar_utf8_boundaries_i64((const int64_t *)(void *)p, 3, NULL, 0),
                    -1);
        wf_guard_free(go);
        wf_guard_free(gd);
    }
    /* "é" split mid-character: offsets 0, 1 (inside), 2. */
    static const uint8_t e_acute[2] = { 0xC3, 0xA9 };
    const int32_t o32[3] = { 0, 1, 2 };
    const int64_t o64[3] = { 0, 2, 2 };
    WF_CHECK_EQ(hs_columnar_utf8_boundaries_i32(o32, 3, e_acute, 2), 1);
    WF_CHECK_EQ(hs_columnar_utf8_boundaries_i64(o64, 3, e_acute, 2), -1);
}

/* ------------------------------------------------------------------ */
/* Dictionary keys                                                     */
/* ------------------------------------------------------------------ */

static int64_t keys_call(int t, const void *k, size_t n, const uint8_t *valid, size_t vo, int64_t max)
{
    switch (t) {
    case 0: return hs_columnar_keys_in_range_i8(k, n, valid, vo, max);
    case 1: return hs_columnar_keys_in_range_i16(k, n, valid, vo, max);
    case 2: return hs_columnar_keys_in_range_i32(k, n, valid, vo, max);
    case 3: return hs_columnar_keys_in_range_i64(k, n, valid, vo, max);
    case 4: return hs_columnar_keys_in_range_u8(k, n, valid, vo, max);
    case 5: return hs_columnar_keys_in_range_u16(k, n, valid, vo, max);
    case 6: return hs_columnar_keys_in_range_u32(k, n, valid, vo, max);
    default: return hs_columnar_keys_in_range_u64(k, n, valid, vo, max);
    }
}

static const int64_t key_maxes[] = { INT64_MIN, -1, 0, 1, 2, 100, 127, 128, 129, 255, 256, 32767, 32768,
                                     65535, 65536, INT32_MAX, (int64_t)1 << 31, ((int64_t)1 << 32) - 1,
                                     (int64_t)1 << 32, INT64_MAX - 1, INT64_MAX };
#define NMAXES (sizeof key_maxes / sizeof key_maxes[0])

/* A key value, as raw 64 bits, near the boundaries of max and the type. */
static uint64_t pick_key(wf_rng *r, int64_t max, size_t w, int is_signed)
{
    uint64_t mask = w == 8 ? ~UINT64_C(0) : (UINT64_C(1) << (8 * w)) - 1;
    uint64_t x;
    switch (wf_rng_below(r, 9)) {
    case 0: x = 0; break;
    case 1: x = (uint64_t)max; break;
    case 2: x = (uint64_t)max - 1; break;
    case 3: x = (uint64_t)max + 1; break;
    case 4: x = ~UINT64_C(0); break;
    case 5: x = is_signed ? (uint64_t)type_max(w) : mask; break;
    case 6: x = is_signed ? (uint64_t)type_min(w) : UINT64_C(1) << (8 * w - 1); break;
    default: x = max > 1 ? wf_rng_below(r, (uint64_t)max) : 0; break;
    }
    return x & mask;
}

static void keys_sweep(void)
{
    wf_rng r = wf_rng_from_env();
    enum { KMAXN = 200 };
    int64_t sk[KMAXN];
    uint64_t uk[KMAXN];
    uint8_t raw[KMAXN * 8];
    for (int t = 0; t < 8; t++) {
        size_t w = (size_t)1 << (t & 3);
        int is_signed = t < 4;
        wf_guarded gk = wf_guard_tail(KMAXN * w), gv = wf_guard_tail((KMAXN + 24) / 8 + 1);
        for (size_t n = 0; n <= KMAXN; n++) {
            for (size_t mi = 0; mi < NMAXES; mi += (n > 70 ? 3 : 1)) {
                int64_t max = key_maxes[mi];
                /* mostly-in-range keys with a few edge values */
                for (size_t i = 0; i < n; i++) {
                    uint64_t x = wf_rng_below(&r, 6) ? (max > 1 ? wf_rng_below(&r, (uint64_t)max) : 0)
                                                     : pick_key(&r, max, w, is_signed);
                    uint64_t mask = w == 8 ? ~UINT64_C(0) : (UINT64_C(1) << (8 * w)) - 1;
                    x &= mask;
                    uk[i] = x;
                    /* sign-extend for the signed widening */
                    sk[i] = w == 8 ? (int64_t)x : (int64_t)(x << (64 - 8 * w)) >> (64 - 8 * w);
                }
                narrow(raw, sk, n, w);
                uint8_t *kp = tail_of(gk, n * w);
                memcpy(kp, raw, n * w);
                /* no validity */
                int64_t want = ref_keys(is_signed ? sk : NULL, is_signed ? NULL : uk, n, NULL, 0, max);
                WF_CHECK_EQ(keys_call(t, kp, n, NULL, 0, max), want);
                /* validity at a random phase, exact covering bytes */
                size_t vo = wf_rng_below(&r, 20);
                size_t vb = (vo + n + 7) / 8;
                uint8_t *vp = tail_of(gv, vb);
                fill_bits(&r, vp, vb);
                want = ref_keys(is_signed ? sk : NULL, is_signed ? NULL : uk, n, vp, vo, max);
                WF_CHECK_EQ(keys_call(t, kp, n, vp, vo, max), want);
            }
        }
        wf_guard_free(gk);
        wf_guard_free(gv);
    }
}

/* Spot checks from the documented semantics. */
static void keys_vectors(void)
{
    const int8_t i8[4] = { 0, 5, -1, 127 };
    WF_CHECK_EQ(hs_columnar_keys_in_range_i8(i8, 2, NULL, 0, 6), -1);
    WF_CHECK_EQ(hs_columnar_keys_in_range_i8(i8, 2, NULL, 0, 5), 1);
    WF_CHECK_EQ(hs_columnar_keys_in_range_i8(i8, 4, NULL, 0, 1000), 2);
    const uint8_t v_skip[1] = { 0x0B }; /* rows 0, 1, 3 valid */
    WF_CHECK_EQ(hs_columnar_keys_in_range_i8(i8, 4, v_skip, 0, 128), -1);
    WF_CHECK_EQ(hs_columnar_keys_in_range_i8(i8, 4, v_skip, 0, 127), 3);
    const uint8_t u8[2] = { 255, 0 };
    WF_CHECK_EQ(hs_columnar_keys_in_range_u8(u8, 2, NULL, 0, 256), -1);
    WF_CHECK_EQ(hs_columnar_keys_in_range_u8(u8, 2, NULL, 0, 255), 0);
    const uint64_t u64[2] = { 1, UINT64_C(1) << 63 };
    WF_CHECK_EQ(hs_columnar_keys_in_range_u64(u64, 2, NULL, 0, INT64_MAX), 1);
    const int64_t i64[2] = { INT64_MAX - 1, INT64_MIN };
    WF_CHECK_EQ(hs_columnar_keys_in_range_i64(i64, 1, NULL, 0, INT64_MAX), -1);
    /* max past the type's own range: signed types clamp to their top
     * (a negative key still fails), unsigned types pass every key. */
    const int16_t i16[2] = { 32767, -1 };
    WF_CHECK_EQ(hs_columnar_keys_in_range_i16(i16, 1, NULL, 0, 100000), -1);
    WF_CHECK_EQ(hs_columnar_keys_in_range_i16(i16, 2, NULL, 0, 100000), 1);
    const int32_t i32[2] = { 2147483647, -5 };
    WF_CHECK_EQ(hs_columnar_keys_in_range_i32(i32, 1, NULL, 0, 3000000000LL), -1);
    WF_CHECK_EQ(hs_columnar_keys_in_range_i32(i32, 2, NULL, 0, 3000000000LL), 1);
    const uint16_t u16[2] = { 65535, 0 };
    WF_CHECK_EQ(hs_columnar_keys_in_range_u16(u16, 2, NULL, 0, 70000), -1);
    const uint32_t u32[2] = { 4294967295u, 0 };
    WF_CHECK_EQ(hs_columnar_keys_in_range_u32(u32, 2, NULL, 0, 5000000000LL), -1);
    WF_CHECK_EQ(hs_columnar_keys_in_range_i64(i64, 2, NULL, 0, INT64_MAX), 1);
    /* max <= 0: the first valid row fails, an all-null column passes. */
    const uint8_t none[1] = { 0 }, third[1] = { 0x04 };
    WF_CHECK_EQ(hs_columnar_keys_in_range_u8(u8, 2, NULL, 0, 0), 0);
    WF_CHECK_EQ(hs_columnar_keys_in_range_u8(u8, 2, none, 0, 0), -1);
    WF_CHECK_EQ(hs_columnar_keys_in_range_i8(i8, 4, third, 0, -3), 2);
    WF_CHECK_EQ(hs_columnar_keys_in_range_i32(NULL, 0, NULL, 0, 0), -1);
}

/* ------------------------------------------------------------------ */
/* List views                                                          */
/* ------------------------------------------------------------------ */

static void list_view_random(void)
{
    wf_rng r = wf_rng_from_env();
    enum { LMAXN = 150 };
    long iters = wf_iters(1500);
    int64_t o[LMAXN], s[LMAXN];
    uint8_t ob[LMAXN * 8], sb[LMAXN * 8], vb[LMAXN / 8 + 4];
    for (long it = 0; it < iters; it++) {
        size_t w = (it & 1) ? 8 : 4;
        size_t n = wf_rng_below(&r, LMAXN);
        int64_t child = (int64_t)wf_rng_below(&r, 6) == 0 ? rng_i64_between(&r, -3, 3)
                                                           : (int64_t)wf_rng_below(&r, 1000);
        if (wf_rng_below(&r, 10) == 0) child = type_max(w);
        if (wf_rng_below(&r, 20) == 0) child = INT64_MAX;
        for (size_t i = 0; i < n; i++) {
            int64_t ci = child > 0 ? child : 1;
            o[i] = (int64_t)wf_rng_below(&r, (uint64_t)ci + 1);
            s[i] = (int64_t)wf_rng_below(&r, (uint64_t)(ci - (o[i] < ci ? o[i] : ci)) + 1);
            if (wf_rng_below(&r, 25) == 0) {
                switch (wf_rng_below(&r, 6)) {
                case 0: o[i] = -1; break;
                case 1: s[i] = -1; break;
                case 2: o[i] = type_max(w); break;
                case 3: s[i] = type_max(w); break;
                case 4: o[i] = type_min(w); break;
                default:
                    if (o[i] <= child && child - o[i] < INT64_MAX) s[i] = child - o[i] + 1;
                    break;
                }
            }
            if (w == 4) {
                o[i] = (int32_t)o[i];
                s[i] = (int32_t)s[i];
            }
        }
        narrow(ob, o, n, w);
        narrow(sb, s, n, w);
        xbuf po = xb_new(ob, n * w, (unsigned)wf_rng_below(&r, 8));
        xbuf ps = xb_new(sb, n * w, (unsigned)wf_rng_below(&r, 8));
        size_t vo = wf_rng_below(&r, 20), nvb = (vo + n + 7) / 8;
        fill_bits(&r, vb, nvb);
        xbuf pv = xb_new(vb, nvb, 0);
        const uint8_t *valid = wf_rng_below(&r, 3) ? pv.p : NULL;
        int64_t want = ref_list_view(o, s, n, valid, vo, child);
        int64_t got = w == 4 ? hs_columnar_list_view_i32((const int32_t *)(void *)po.p, (const int32_t *)(void *)ps.p,
                                                         n, valid, vo, child)
                             : hs_columnar_list_view_i64((const int64_t *)(void *)po.p, (const int64_t *)(void *)ps.p,
                                                         n, valid, vo, child);
        WF_CHECK_EQ(got, want);
        xb_free(po);
        xb_free(ps);
        xb_free(pv);
    }
    /* Sum overflow: offset + size exceeds int64 but each is in range. */
    const int64_t o2[1] = { INT64_MAX - 1 }, s2[1] = { INT64_MAX - 1 };
    WF_CHECK_EQ(hs_columnar_list_view_i64(o2, s2, 1, NULL, 0, INT64_MAX), 0);
    const int64_t o3[1] = { INT64_MAX - 1 }, s3[1] = { 1 };
    WF_CHECK_EQ(hs_columnar_list_view_i64(o3, s3, 1, NULL, 0, INT64_MAX), -1);
    WF_CHECK_EQ(hs_columnar_list_view_i64(o3, s3, 1, NULL, 0, INT64_MAX - 1), 0);
    WF_CHECK_EQ(hs_columnar_list_view_i32(NULL, NULL, 0, NULL, 0, -1), -1);
    const int32_t z[1] = { 0 };
    WF_CHECK_EQ(hs_columnar_list_view_i32(z, z, 1, NULL, 0, -1), 0);
    WF_CHECK_EQ(hs_columnar_list_view_i32(z, z, 1, NULL, 0, 0), -1);
}

/* ------------------------------------------------------------------ */
/* Dense unions                                                        */
/* ------------------------------------------------------------------ */

static void dense_union_random(void)
{
    wf_rng r = wf_rng_from_env();
    long iters = wf_iters(3000);
    int8_t t[100];
    int32_t o[100];
    int64_t cl[130];
    for (long it = 0; it < iters; it++) {
        size_t n = wf_rng_below(&r, 100), nch = wf_rng_below(&r, 130);
        for (size_t c = 0; c < nch; c++) cl[c] = wf_rng_below(&r, 8) ? (int64_t)wf_rng_below(&r, 50) : rng_i64_between(&r, -2, 0);
        for (size_t i = 0; i < n; i++) {
            int64_t ti = nch ? (int64_t)wf_rng_below(&r, nch) : 0;
            if (wf_rng_below(&r, 30) == 0) ti = (int64_t)wf_rng_below(&r, 256) - 128;
            t[i] = (int8_t)ti;
            int64_t lim = (ti >= 0 && (size_t)ti < nch && cl[ti] > 0) ? cl[ti] : 1;
            o[i] = (int32_t)wf_rng_below(&r, (uint64_t)lim);
            if (wf_rng_below(&r, 30) == 0) o[i] = wf_rng_below(&r, 2) ? -1 : INT32_MAX;
        }
        xbuf pt = xb_new(t, n, 0), po = xb_new(o, n * 4, (unsigned)wf_rng_below(&r, 4));
        xbuf pc = xb_new(cl, nch * 8, (unsigned)wf_rng_below(&r, 8));
        WF_CHECK_EQ(hs_columnar_dense_union((const int8_t *)(void *)pt.p, (const int32_t *)(void *)po.p, n,
                                            (const int64_t *)(void *)pc.p, nch),
                    ref_dense_union(t, o, n, cl, nch));
        xb_free(pt);
        xb_free(po);
        xb_free(pc);
    }
    WF_CHECK_EQ(hs_columnar_dense_union(NULL, NULL, 0, NULL, 0), -1);
}

/* ------------------------------------------------------------------ */
/* Binary / UTF-8 views                                                */
/* ------------------------------------------------------------------ */

static void put_le32(uint8_t *p, int32_t v)
{
    for (int k = 0; k < 4; k++) p[k] = (uint8_t)((uint32_t)v >> (8 * k));
}

static void inline_view(uint8_t *v, const uint8_t *s, int32_t len)
{
    memset(v, 0, 16);
    put_le32(v, len);
    memcpy(v + 4, s, (size_t)len);
}

/* One inline view per byte sequence, checked against the decoder. */
static wf_guarded g_view; /* one tail-guarded 16-byte view */

static int64_t check_inline(const uint8_t *s, int32_t len)
{
    inline_view(g_view.p, s, len);
    return hs_columnar_view_refs(g_view.p, 1, NULL, 0, NULL, NULL, 0, 1);
}

/* Every 1-, 2- and 3-byte sequence, and every 4-byte sequence over all
 * lead/second bytes with boundary trailing bytes. */
static void utf8_exhaustive_inline(void)
{
    uint8_t s[4];
    for (int a = 0; a < 256; a++) {
        s[0] = (uint8_t)a;
        WF_CHECK_EQ(check_inline(s, 1), ref_utf8_valid(s, 1) ? -1 : 0);
        for (int b = 0; b < 256; b++) {
            s[1] = (uint8_t)b;
            WF_CHECK_EQ(check_inline(s, 2), ref_utf8_valid(s, 2) ? -1 : 0);
            static const uint8_t tails[] = { 0x00, 0x41, 0x7F, 0x80, 0x8F, 0x90, 0x9F, 0xA0, 0xBF, 0xC0, 0xF4, 0xFF };
            for (size_t c = 0; c < sizeof tails; c++) {
                s[2] = tails[c];
                WF_CHECK_EQ(check_inline(s, 3), ref_utf8_valid(s, 3) ? -1 : 0);
                for (size_t d = 0; a >= 0xE0 && d < sizeof tails; d += 2) {
                    s[3] = tails[d];
                    WF_CHECK_EQ(check_inline(s, 4), ref_utf8_valid(s, 4) ? -1 : 0);
                }
            }
        }
    }
    /* Full 3-byte space for the leads with special second-byte ranges. */
    static const uint8_t leads[] = { 0xE0, 0xE1, 0xED, 0xEF };
    for (size_t l = 0; l < sizeof leads; l++) {
        s[0] = leads[l];
        for (int b = 0x70; b < 0xD0; b++) {
            for (int c = 0x70; c < 0xD0; c++) {
                s[1] = (uint8_t)b;
                s[2] = (uint8_t)c;
                WF_CHECK_EQ(check_inline(s, 3), ref_utf8_valid(s, 3) ? -1 : 0);
            }
        }
    }
}

static void utf8_vectors(void)
{
    static const struct {
        const char *s;
        int ok;
    } cases[] = {
        { "", 1 },
        { "hello", 1 },
        { "\xC3\xA9", 1 },
        { "\xE2\x82\xAC", 1 },
        { "\xF0\x9F\x98\x80", 1 },
        { "\xF4\x8F\xBF\xBF", 1 },           /* U+10FFFF */
        { "\xED\x9F\xBF", 1 },               /* U+D7FF */
        { "\xEE\x80\x80", 1 },               /* U+E000 */
        { "\xC0\x80", 0 },                   /* overlong NUL */
        { "\xC1\xBF", 0 },
        { "\xE0\x9F\xBF", 0 },               /* overlong */
        { "\xF0\x8F\xBF\xBF", 0 },           /* overlong */
        { "\xED\xA0\x80", 0 },               /* U+D800 */
        { "\xED\xBF\xBF", 0 },               /* U+DFFF */
        { "\xF4\x90\x80\x80", 0 },           /* U+110000 */
        { "\xF5\x80\x80\x80", 0 },
        { "\xFF", 0 },
        { "\x80", 0 },
        { "\xC3", 0 },                       /* truncated */
        { "\xE2\x82", 0 },
        { "\xF0\x9F\x98", 0 },
        { "abc\xC3", 0 },
        { "\xC3\x28", 0 },
        { "\xE2\x28\xA1", 0 },
        { "\xF0\x28\x8C\xBC", 0 },
        { "abcdefghijk\xC3", 0 },
    };
    for (size_t i = 0; i < sizeof cases / sizeof cases[0]; i++) {
        int32_t len = (int32_t)strlen(cases[i].s);
        WF_CHECK_EQ(ref_utf8_valid((const uint8_t *)cases[i].s, (size_t)len), cases[i].ok);
        WF_CHECK_EQ(check_inline((const uint8_t *)cases[i].s, len), cases[i].ok ? -1 : 0);
    }
}

/* Random view tables: inline and out-of-line views over exact-size data
 * buffers, with planted bad lengths, buffer indices, offsets and prefixes;
 * the payload of an in-range view may end exactly at its buffer's end. */
static void view_refs_random(void)
{
    wf_rng r = wf_rng_from_env();
    static const uint8_t alpha[] = { 'a', 'Z', 0x00, 0x7F, 0x80, 0xBF, 0xC3, 0xA9, 0xE2, 0x82, 0xAC, 0xF0, 0x9F, 0xED, 0xA0 };
    static const uint8_t ascii[] = { 'a', 'b', 'c', 'd' };
    long iters = wf_iters(2000);
    enum { NB = 5, VN = 80, BMAX = 300 };
    for (long it = 0; it < iters; it++) {
        size_t nbufs = wf_rng_below(&r, NB + 1), n = wf_rng_below(&r, VN);
        uint8_t *bufs[NB];
        int64_t lens[NB];
        int ascii_only = wf_rng_below(&r, 2);
        for (size_t b = 0; b < nbufs; b++) {
            lens[b] = (int64_t)wf_rng_below(&r, BMAX);
            uint8_t tmp[BMAX];
            if (ascii_only) wf_rng_fill_from(&r, tmp, (size_t)lens[b], ascii, sizeof ascii);
            else wf_rng_fill_from(&r, tmp, (size_t)lens[b], alpha, sizeof alpha);
            bufs[b] = lens[b] ? wf_dup(tmp, (size_t)lens[b]) : NULL;
        }
        uint8_t views[VN * 16];
        for (size_t i = 0; i < n; i++) {
            uint8_t *v = views + 16 * i;
            memset(v, 0, 16);
            if (wf_rng_below(&r, 3) == 0 || nbufs == 0) {
                int32_t len = (int32_t)wf_rng_below(&r, 13);
                uint8_t s[12];
                if (ascii_only) wf_rng_fill_from(&r, s, (size_t)len, ascii, sizeof ascii);
                else wf_rng_fill_from(&r, s, (size_t)len, alpha, sizeof alpha);
                inline_view(v, s, len);
            } else {
                size_t b = wf_rng_below(&r, nbufs);
                int32_t len, off;
                if (lens[b] >= 13) {
                    len = 13 + (int32_t)wf_rng_below(&r, (uint64_t)lens[b] - 12);
                    off = (int32_t)wf_rng_below(&r, (uint64_t)(lens[b] - len) + 1);
                    if (wf_rng_below(&r, 4) == 0) off = (int32_t)(lens[b] - len); /* flush */
                    memcpy(v + 4, bufs[b] + off, 4);
                } else {
                    len = 13 + (int32_t)wf_rng_below(&r, 5);
                    off = 0;
                }
                put_le32(v, len);
                put_le32(v + 8, (int32_t)b);
                put_le32(v + 12, off);
            }
            if (wf_rng_below(&r, 20) == 0) {
                switch (wf_rng_below(&r, 8)) {
                case 0: put_le32(v, -1 - (int32_t)wf_rng_below(&r, 3)); break;
                case 1: put_le32(v + 8, (int32_t)nbufs); break;
                case 2: put_le32(v + 8, -1); break;
                case 3: put_le32(v + 12, -1); break;
                case 4: put_le32(v + 12, INT32_MAX); break;
                case 5: v[4 + wf_rng_below(&r, 4)] ^= 1; break;
                case 6: put_le32(v, INT32_MAX); break;
                default: put_le32(v + 12, ref_le32(v + 12) + 1); break;
                }
            }
        }
        size_t vo = wf_rng_below(&r, 16), vb = (vo + n + 7) / 8;
        uint8_t vtmp[VN / 8 + 4];
        fill_bits(&r, vtmp, vb);
        uint8_t *valid = wf_rng_below(&r, 3) ? wf_dup(vtmp, vb) : NULL;
        uint8_t *pv = wf_dup(views, n * 16);
        const uint8_t **pb = (const uint8_t **)(void *)wf_dup(bufs, nbufs * sizeof(uint8_t *));
        int64_t *pl = (int64_t *)(void *)wf_dup(lens, nbufs * 8);
        for (int utf8 = 0; utf8 < 2; utf8++) {
            WF_CHECK_EQ(hs_columnar_view_refs(pv, n, valid, vo, pb, pl, nbufs, utf8),
                        ref_view_refs(pv, n, valid, vo, (const uint8_t *const *)bufs, lens, nbufs, utf8));
        }
        wf_free(pv, n * 16);
        wf_free(pb, nbufs * sizeof(uint8_t *));
        wf_free(pl, nbufs * 8);
        if (valid) wf_free(valid, vb);
        for (size_t b = 0; b < nbufs; b++) {
            if (bufs[b]) wf_free(bufs[b], (size_t)lens[b]);
        }
    }
    WF_CHECK_EQ(hs_columnar_view_refs(NULL, 0, NULL, 0, NULL, NULL, 0, 1), -1);
}

/* Long out-of-line UTF-8 payloads: every length across the 8-byte ASCII
 * fast path, with one multi-byte character at each position, the payload
 * flush against the end of its buffer. */
static void view_refs_long_utf8(void)
{
    enum { MAXL = 90 };
    wf_guarded g = wf_guard_tail(MAXL);
    for (int32_t len = 13; len <= MAXL; len++) {
        uint8_t *buf = tail_of(g, (size_t)len);
        for (int32_t pos = -1; pos < len; pos++) {
            memset(buf, 'x', (size_t)len);
            /* a 2-byte char at pos, possibly cut by the end */
            if (pos >= 0) {
                buf[pos] = 0xC3;
                if (pos + 1 < len) buf[pos + 1] = 0xA9;
            }
            uint8_t v[16];
            put_le32(v, len);
            memcpy(v + 4, buf, 4);
            put_le32(v + 8, 0);
            put_le32(v + 12, 0);
            const uint8_t *bufs[1] = { buf };
            int64_t lens[1] = { len };
            int ok = ref_utf8_valid(buf, (size_t)len);
            WF_CHECK_EQ(ok, pos + 1 < len);
            WF_CHECK_EQ(hs_columnar_view_refs(v, 1, NULL, 0, bufs, lens, 1, 1), ok ? -1 : 0);
            WF_CHECK_EQ(hs_columnar_view_refs(v, 1, NULL, 0, bufs, lens, 1, 0), -1);
        }
    }
    wf_guard_free(g);
}

/* Out-of-range and continuation offsets inside one recheck block: the
 * out-of-range entry must not mask the continuation offender in either
 * order, and the scan records the first offender across all of them. */
static void utf8_mixed_block(void)
{
    static const uint8_t data[4] = { 0x41, 0x80, 0x42, 0x83 };
    /* offender at index 1, out-of-range after it */
    const int32_t a[3] = { 0, 1, 99 };
    WF_CHECK_EQ(hs_columnar_utf8_boundaries_i32(a, 3, data, 4), 1);
    /* out-of-range first, then two offenders: the first offender wins
     * and the scan visits every entry. */
    const int32_t b[3] = { 99, 1, 3 };
    WF_CHECK_EQ(hs_columnar_utf8_boundaries_i32(b, 3, data, 4), 1);
    const int32_t c[3] = { 99, 3, 1 };
    WF_CHECK_EQ(hs_columnar_utf8_boundaries_i32(c, 3, data, 4), 1);
    /* the 64-bit instantiation is separate code; drive it too */
    const int64_t a64[3] = { 0, 1, 99 };
    WF_CHECK_EQ(hs_columnar_utf8_boundaries_i64(a64, 3, data, 4), 1);
    const int64_t b64[3] = { 99, 1, 3 };
    WF_CHECK_EQ(hs_columnar_utf8_boundaries_i64(b64, 3, data, 4), 1);
    const int64_t c64[3] = { 99, 3, 1 };
    WF_CHECK_EQ(hs_columnar_utf8_boundaries_i64(c64, 3, data, 4), 1);
    /* clean input still passes */
    const int32_t ok[2] = { 0, 2 };
    WF_CHECK_EQ(hs_columnar_utf8_boundaries_i32(ok, 2, data, 4), -1);
}

int main(void)
{
    WF_RUN(offsets_sweep);
    WF_RUN(run_ends_sweep);
    WF_RUN(utf8_mixed_block);
    WF_RUN(utf8_boundaries_sweep);
    WF_RUN(keys_vectors);
    WF_RUN(keys_sweep);
    WF_RUN(list_view_random);
    WF_RUN(dense_union_random);
    g_view = wf_guard_tail(16);
    WF_RUN(utf8_vectors);
    WF_RUN(utf8_exhaustive_inline);
    WF_RUN(view_refs_random);
    WF_RUN(view_refs_long_utf8);
    wf_guard_free(g_view);
    return wf_finish("columnar-simd test_validators");
}
