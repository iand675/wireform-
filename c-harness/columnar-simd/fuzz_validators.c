/* Differential fuzz target for the Arrow buffer validators: offsets, run
 * ends, UTF-8 boundaries, dictionary keys (all 8 key types), list views,
 * dense unions and offset rebasing, against the references in api.h.
 *
 * Input: [mode][sub][8-byte limit][validity offset][payload...]. The payload
 * is the raw little-endian array (exact-size, wf_dup) and, for the modes
 * that take one, a validity bitmap covering exactly the rows. Inputs stay
 * within each kernel's documented precondition (UTF-8 boundary offsets are
 * first made valid against the data length, union child lengths come from
 * the payload). */
#include "harness.h"
#include "api.h"

static int64_t take_i64(wf_input *in)
{
    uint64_t v = 0;
    for (int k = 0; k < 8; k++) v |= (uint64_t)wf_take_u8(in) << (8 * k);
    return (int64_t)v;
}

/* Little-endian signed value of width w at p. */
static int64_t rd(const uint8_t *p, size_t w)
{
    uint64_t v = 0;
    for (size_t b = 0; b < w; b++) v |= (uint64_t)p[b] << (8 * b);
    if (w < 8) {
        unsigned sh = (unsigned)(64 - 8 * w);
        return (int64_t)(v << sh) >> sh;
    }
    return (int64_t)v;
}

static uint64_t rdu(const uint8_t *p, size_t w)
{
    uint64_t v = 0;
    for (size_t b = 0; b < w; b++) v |= (uint64_t)p[b] << (8 * b);
    return v;
}

/* Split the remaining input into n rows of `w` bytes and an optional
 * validity bitmap (exact covering bytes at bit offset vo). */
typedef struct {
    uint8_t *arr;
    size_t n;
    uint8_t *valid;
    size_t vbytes;
} rows_t;

static rows_t take_rows(wf_input *in, size_t w, int with_valid, size_t vo)
{
    rows_t r;
    size_t avail = in->n;
    size_t n = with_valid ? (8 * avail) / (8 * w + 1) : avail / w;
    while (with_valid && n && n * w + (vo + n + 7) / 8 > avail) n--;
    /* too short for even the offset's validity bytes: no bitmap */
    if (with_valid && n * w + (vo + n + 7) / 8 > avail) with_valid = 0;
    r.n = n;
    r.arr = wf_dup(in->p, n * w);
    r.vbytes = with_valid ? (vo + n + 7) / 8 : 0;
    r.valid = with_valid ? wf_dup(in->p + n * w, r.vbytes) : NULL;
    return r;
}

static void free_rows(rows_t r, size_t w)
{
    wf_free(r.arr, r.n * w);
    if (r.valid) wf_free(r.valid, r.vbytes);
}

static int64_t *widen(const uint8_t *a, size_t n, size_t w)
{
    int64_t *v = malloc((n ? n : 1) * 8);
    for (size_t i = 0; i < n; i++) v[i] = rd(a + i * w, w);
    return v;
}

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size > (1u << 16)) return 0;
    wf_input in = { data, size };
    uint8_t mode = wf_take_u8(&in) % 7;
    uint8_t sub = wf_take_u8(&in);
    int64_t lim = take_i64(&in);
    size_t vo = wf_take_u8(&in) % 64;

    switch (mode) {
    case 0: { /* offsets */
        size_t w = (sub & 1) ? 8 : 4;
        rows_t r = take_rows(&in, w, 0, 0);
        int64_t *v = widen(r.arr, r.n, w);
        int64_t got = w == 4 ? hs_columnar_offsets_i32((const int32_t *)(void *)r.arr, r.n, lim)
                             : hs_columnar_offsets_i64((const int64_t *)(void *)r.arr, r.n, lim);
        WF_FUZZ_ASSERT(got == ref_offsets(v, r.n, lim));
        free(v);
        free_rows(r, w);
        break;
    }
    case 1: { /* run ends */
        size_t w = (size_t)2 << (sub % 3);
        rows_t r = take_rows(&in, w, 0, 0);
        int64_t *v = widen(r.arr, r.n, w);
        int64_t got = w == 2 ? hs_columnar_run_ends_i16((const int16_t *)(void *)r.arr, r.n, lim)
                    : w == 4 ? hs_columnar_run_ends_i32((const int32_t *)(void *)r.arr, r.n, lim)
                             : hs_columnar_run_ends_i64((const int64_t *)(void *)r.arr, r.n, lim);
        WF_FUZZ_ASSERT(got == ref_run_ends(v, r.n, lim));
        free(v);
        free_rows(r, w);
        break;
    }
    case 2: { /* UTF-8 boundaries: first half deltas, second half data */
        size_t w = (sub & 1) ? 8 : 4;
        size_t nd = in.n / 2, len = in.n - nd;
        const uint8_t *deltas = in.p, *bytes = in.p + nd;
        int64_t *v = malloc((nd ? nd : 1) * 8);
        uint8_t *arr = malloc((nd ? nd : 1) * w);
        int64_t o = 0;
        for (size_t i = 0; i < nd; i++) {
            o += deltas[i] & 7;
            if (o > (int64_t)len) o = (int64_t)len;
            v[i] = o;
            for (size_t b = 0; b < w; b++) arr[i * w + b] = (uint8_t)((uint64_t)o >> (8 * b));
        }
        WF_FUZZ_ASSERT(nd == 0 || ref_offsets(v, nd, (int64_t)len) == -1);
        uint8_t *po = wf_dup(arr, nd * w), *pd = wf_dup(bytes, len);
        int64_t got = w == 4 ? hs_columnar_utf8_boundaries_i32((const int32_t *)(void *)po, nd, pd, (int64_t)len)
                             : hs_columnar_utf8_boundaries_i64((const int64_t *)(void *)po, nd, pd, (int64_t)len);
        WF_FUZZ_ASSERT(got == ref_utf8_boundaries(v, nd, pd, (int64_t)len));
        wf_free(po, nd * w);
        wf_free(pd, len);
        free(v);
        free(arr);
        break;
    }
    case 3: { /* dictionary keys, type = sub % 8 */
        int t = sub % 8;
        size_t w = (size_t)1 << (t & 3);
        int with_valid = (sub >> 3) & 1;
        rows_t r = take_rows(&in, w, with_valid, vo);
        int64_t *sk = malloc((r.n ? r.n : 1) * 8);
        uint64_t *uk = malloc((r.n ? r.n : 1) * 8);
        for (size_t i = 0; i < r.n; i++) {
            sk[i] = rd(r.arr + i * w, w);
            uk[i] = rdu(r.arr + i * w, w);
        }
        /* bias max towards small and type-boundary values */
        int64_t max = lim;
        if ((sub >> 4) & 1) max = (int64_t)((uint64_t)lim & 0x1FF) - 2;
        const void *k = r.arr;
        int64_t got;
        switch (t) {
        case 0: got = hs_columnar_keys_in_range_i8(k, r.n, r.valid, vo, max); break;
        case 1: got = hs_columnar_keys_in_range_i16(k, r.n, r.valid, vo, max); break;
        case 2: got = hs_columnar_keys_in_range_i32(k, r.n, r.valid, vo, max); break;
        case 3: got = hs_columnar_keys_in_range_i64(k, r.n, r.valid, vo, max); break;
        case 4: got = hs_columnar_keys_in_range_u8(k, r.n, r.valid, vo, max); break;
        case 5: got = hs_columnar_keys_in_range_u16(k, r.n, r.valid, vo, max); break;
        case 6: got = hs_columnar_keys_in_range_u32(k, r.n, r.valid, vo, max); break;
        default: got = hs_columnar_keys_in_range_u64(k, r.n, r.valid, vo, max); break;
        }
        WF_FUZZ_ASSERT(got == ref_keys(t < 4 ? sk : NULL, t < 4 ? NULL : uk, r.n, r.valid, vo, max));
        free(sk);
        free(uk);
        free_rows(r, w);
        break;
    }
    case 4: { /* list views: rows of (offset, size) pairs */
        size_t w = (sub & 1) ? 8 : 4;
        int with_valid = (sub >> 1) & 1;
        rows_t r = take_rows(&in, 2 * w, with_valid, vo);
        int64_t *o = malloc((r.n ? r.n : 1) * 8), *s = malloc((r.n ? r.n : 1) * 8);
        uint8_t *ob = malloc((r.n ? r.n : 1) * w), *sb = malloc((r.n ? r.n : 1) * w);
        for (size_t i = 0; i < r.n; i++) {
            o[i] = rd(r.arr + 2 * w * i, w);
            s[i] = rd(r.arr + 2 * w * i + w, w);
            memcpy(ob + i * w, r.arr + 2 * w * i, w);
            memcpy(sb + i * w, r.arr + 2 * w * i + w, w);
        }
        int64_t child = (sub >> 2) & 1 ? (int64_t)((uint64_t)lim & 0xFFFF) - 3 : lim;
        uint8_t *po = wf_dup(ob, r.n * w), *ps = wf_dup(sb, r.n * w);
        int64_t got = w == 4 ? hs_columnar_list_view_i32((const int32_t *)(void *)po, (const int32_t *)(void *)ps,
                                                         r.n, r.valid, vo, child)
                             : hs_columnar_list_view_i64((const int64_t *)(void *)po, (const int64_t *)(void *)ps,
                                                         r.n, r.valid, vo, child);
        WF_FUZZ_ASSERT(got == ref_list_view(o, s, r.n, r.valid, vo, child));
        wf_free(po, r.n * w);
        wf_free(ps, r.n * w);
        free(o);
        free(s);
        free(ob);
        free(sb);
        free_rows(r, 2 * w);
        break;
    }
    case 5: { /* dense union: sub = nchildren, lengths from lim's bytes */
        size_t nch = sub % 9;
        int64_t cl[8];
        for (size_t c = 0; c < nch; c++) cl[c] = (int64_t)(int8_t)((uint64_t)lim >> (8 * c));
        if (nch == 8 && (lim & 1)) cl[0] = INT64_MAX;
        size_t n = in.n / 5;
        int8_t *t = malloc(n ? n : 1);
        int32_t *o = malloc((n ? n : 1) * 4);
        for (size_t i = 0; i < n; i++) {
            t[i] = (int8_t)in.p[5 * i];
            o[i] = (int32_t)rd(in.p + 5 * i + 1, 4);
        }
        uint8_t *pt = wf_dup(t, n), *po = wf_dup(o, n * 4), *pc = wf_dup(cl, nch * 8);
        int64_t got = hs_columnar_dense_union((const int8_t *)(void *)pt, (const int32_t *)(void *)po, n,
                                              (const int64_t *)(void *)pc, nch);
        WF_FUZZ_ASSERT(got == ref_dense_union(t, o, n, cl, nch));
        wf_free(pt, n);
        wf_free(po, n * 4);
        wf_free(pc, nch * 8);
        free(t);
        free(o);
        break;
    }
    default: { /* rebase, out of place and in place */
        size_t w = (sub & 1) ? 8 : 4;
        rows_t r = take_rows(&in, w, 0, 0);
        uint8_t *want = malloc(r.n * w + 1), *dst = wf_alloc(r.n * w);
        for (size_t i = 0; i < r.n; i++) {
            uint64_t x = rdu(r.arr + i * w, w) + (uint64_t)lim;
            for (size_t b = 0; b < w; b++) want[i * w + b] = (uint8_t)(x >> (8 * b));
        }
        if (w == 4) {
            hs_columnar_rebase_offsets_i32((int32_t *)(void *)dst, (const int32_t *)(void *)r.arr, r.n, lim);
            hs_columnar_rebase_offsets_i32((int32_t *)(void *)r.arr, (const int32_t *)(void *)r.arr, r.n, lim);
        } else {
            hs_columnar_rebase_offsets_i64((int64_t *)(void *)dst, (const int64_t *)(void *)r.arr, r.n, lim);
            hs_columnar_rebase_offsets_i64((int64_t *)(void *)r.arr, (const int64_t *)(void *)r.arr, r.n, lim);
        }
        WF_FUZZ_ASSERT(r.n == 0 || memcmp(dst, want, r.n * w) == 0);
        WF_FUZZ_ASSERT(r.n == 0 || memcmp(r.arr, want, r.n * w) == 0);
        wf_free(dst, r.n * w);
        free(want);
        free_rows(r, w);
        break;
    }
    }
    return 0;
}
