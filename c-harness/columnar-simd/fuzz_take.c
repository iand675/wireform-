/* Differential fuzz target for the gathers and the two-pass variable-length
 * take (take_offsets_i32/i64, take_bytes_i32/i64) against the references
 * in api.h.
 *
 * Input: [mode][rows][payload...].
 *   mode % 6 < 5: fixed-width gather of 1/2/4/8/16-byte elements; the
 *     payload holds `rows` source elements then one index byte per output
 *     row (taken modulo rows, as the Haskell caller has bounds-checked).
 *   mode % 6 == 5: variable-length take. mode bit 3 picks 32/64-bit offsets,
 *     bit 4 makes row lengths huge (u32 deltas, for the overflow -> -1 path),
 *     bit 5 passes the empty data buffer as NULL (an empty ByteString). The
 *     payload holds one length byte per row, then one index byte per output
 *     row. The offsets are valid by construction (non-decreasing from 0).
 * Every array and output is exact-size. */
#include "harness.h"
#include "api.h"

typedef void (*gather_fn)(uint8_t *, const uint8_t *, const int64_t *, size_t);

static void do_gather(size_t w, size_t rows, const uint8_t *p, size_t n)
{
    static const gather_fn fns[5] = { hs_columnar_gather_1, hs_columnar_gather_2, hs_columnar_gather_4,
                                      hs_columnar_gather_8, hs_columnar_gather_16 };
    size_t avail = n;
    if (rows * w > avail) rows = avail / w;
    if (rows == 0) return;
    size_t nidx = avail - rows * w;
    int64_t *idx = malloc((nidx ? nidx : 1) * 8);
    for (size_t k = 0; k < nidx; k++) idx[k] = (int64_t)(p[rows * w + k] % rows);
    uint8_t *src = wf_dup(p, rows * w), *ix = wf_dup(idx, nidx * 8);
    uint8_t *dst = wf_alloc(nidx * w), *want = malloc(nidx * w + 1);
    int f = w == 1 ? 0 : w == 2 ? 1 : w == 4 ? 2 : w == 8 ? 3 : 4;
    fns[f](dst, src, (const int64_t *)(void *)ix, nidx);
    ref_gather(want, src, idx, nidx, w);
    WF_FUZZ_ASSERT(nidx == 0 || memcmp(dst, want, nidx * w) == 0);
    wf_free(src, rows * w);
    wf_free(ix, nidx * 8);
    wf_free(dst, nidx * w);
    free(want);
    free(idx);
}

static void do_take(uint8_t mode, size_t rows, const uint8_t *p, size_t n)
{
    int wide = (mode >> 3) & 1, huge = (mode >> 4) & 1, null_empty = (mode >> 5) & 1;
    size_t per = huge ? 4 : 1;
    if (rows * per > n) rows = n / per;
    if (rows == 0) return;
    size_t nidx = n - rows * per;
    int64_t *offs = malloc((rows + 1) * 8);
    offs[0] = 0;
    for (size_t i = 0; i < rows; i++) {
        uint64_t d = huge ? (uint64_t)p[4 * i] | (uint64_t)p[4 * i + 1] << 8 | (uint64_t)p[4 * i + 2] << 16
                                | (uint64_t)p[4 * i + 3] << 24
                          : (uint64_t)(p[i] % 24);
        if (huge && wide) d <<= 31; /* 64-bit sums reach INT64_MAX */
        int64_t next;
        if (__builtin_add_overflow(offs[i], (int64_t)d, &next)) next = INT64_MAX;
        if (!wide && next > INT32_MAX) next = INT32_MAX;
        offs[i + 1] = next;
    }
    int64_t *idx = malloc((nidx ? nidx : 1) * 8);
    for (size_t k = 0; k < nidx; k++) idx[k] = (int64_t)(p[rows * per + k] % rows);

    uint8_t *ix = wf_dup(idx, nidx * 8);
    const int64_t *pix = (const int64_t *)(void *)ix;
    int64_t *wantOffs = malloc((nidx + 1) * 8);
    int64_t want = ref_take_offsets(wantOffs, offs, idx, nidx, wide ? INT64_MAX : INT32_MAX);
    int64_t got;
    size_t ow = wide ? 8 : 4;
    uint8_t *po = malloc((rows + 1) * ow);
    for (size_t i = 0; i <= rows; i++) {
        for (size_t b = 0; b < ow; b++) po[i * ow + b] = (uint8_t)((uint64_t)offs[i] >> (8 * b));
    }
    uint8_t *so = wf_dup(po, (rows + 1) * ow);
    uint8_t *dofs = wf_alloc((nidx + 1) * ow);
    if (wide) got = hs_columnar_take_offsets_i64((int64_t *)(void *)dofs, (const int64_t *)(void *)so, pix, nidx);
    else got = hs_columnar_take_offsets_i32((int32_t *)(void *)dofs, (const int32_t *)(void *)so, pix, nidx);
    WF_FUZZ_ASSERT(got == want);
    if (got >= 0) {
        for (size_t k = 0; k <= nidx; k++) {
            int64_t v;
            if (wide) {
                memcpy(&v, dofs + 8 * k, 8);
            } else {
                int32_t v32;
                memcpy(&v32, dofs + 4 * k, 4);
                v = v32;
            }
            WF_FUZZ_ASSERT(v == wantOffs[k]);
        }
    }
    /* Pass 2 only for inputs and outputs the test can materialise. */
    size_t dlen = (size_t)offs[rows];
    if (got >= 0 && dlen <= (1u << 20) && got <= (1 << 22)) {
        uint8_t *bytes = malloc(dlen + 1);
        for (size_t j = 0; j < dlen; j++) bytes[j] = (uint8_t)(j * 131 + 7);
        uint8_t *src = (dlen == 0 && null_empty) ? NULL : wf_dup(bytes, dlen);
        size_t total = (size_t)got;
        uint8_t *dst = (total == 0 && null_empty) ? NULL : wf_alloc(total);
        uint8_t *wantb = malloc(total + 1);
        WF_FUZZ_ASSERT(ref_take_bytes(wantb, offs, bytes, idx, nidx) == total);
        if (wide) hs_columnar_take_bytes_i64(dst, (const int64_t *)(void *)so, src, pix, nidx);
        else hs_columnar_take_bytes_i32(dst, (const int32_t *)(void *)so, src, pix, nidx);
        WF_FUZZ_ASSERT(total == 0 || memcmp(dst, wantb, total) == 0);
        if (dst) wf_free(dst, total);
        if (src) wf_free(src, dlen);
        free(wantb);
        free(bytes);
    }
    wf_free(ix, nidx * 8);
    wf_free(so, (rows + 1) * ow);
    wf_free(dofs, (nidx + 1) * ow);
    free(po);
    free(wantOffs);
    free(idx);
    free(offs);
}

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size > (1u << 16)) return 0;
    wf_input in = { data, size };
    uint8_t mode = wf_take_u8(&in);
    size_t rows = wf_take_u8(&in);
    if (mode % 6 < 5) do_gather((size_t)1 << (mode % 6), rows, in.p, in.n);
    else do_take(mode, rows, in.p, in.n);
    return 0;
}
