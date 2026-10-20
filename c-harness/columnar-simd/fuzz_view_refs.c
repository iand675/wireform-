/* Differential fuzz target for hs_columnar_view_refs (Arrow BinaryView /
 * Utf8View validation, including the strict UTF-8 validator) against
 * ref_view_refs and the decoding UTF-8 reference in api.h.
 *
 * Input: [flags][fixmask][validity offset][nbufs]
 *        nbufs x ([u16 length][bytes...])
 *        views (16 bytes each) [validity bytes...]
 * flags bit 0: utf8; bit 1: validity bitmap present. A view i whose bit
 * (i % 8) of fixmask is set is normalised into a structurally valid
 * out-of-line view (buffer index, offset and prefix fixed up) so the
 * payload UTF-8 path is reached; other views are taken verbatim. Every data
 * buffer is exact-size, so an out-of-line view reading past its buffer
 * faults. */
#include "harness.h"
#include "api.h"

enum { MAXBUFS = 6 };

static void put32(uint8_t *p, int32_t v)
{
    for (int k = 0; k < 4; k++) p[k] = (uint8_t)((uint32_t)v >> (8 * k));
}

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size > (1u << 16)) return 0;
    wf_input in = { data, size };
    uint8_t flags = wf_take_u8(&in), fixmask = wf_take_u8(&in);
    size_t vo = wf_take_u8(&in) % 32;
    size_t nbufs = wf_take_u8(&in) % (MAXBUFS + 1);
    int utf8 = flags & 1, with_valid = (flags >> 1) & 1;

    uint8_t *bufs[MAXBUFS] = { 0 };
    int64_t lens[MAXBUFS] = { 0 };
    for (size_t b = 0; b < nbufs; b++) {
        size_t l = (size_t)wf_take_u8(&in) | (size_t)wf_take_u8(&in) << 8;
        if (l > in.n) l = in.n;
        lens[b] = (int64_t)l;
        bufs[b] = l ? wf_dup(in.p, l) : NULL;
        in.p += l;
        in.n -= l;
    }

    size_t n = in.n / 16;
    while (with_valid && n && 16 * n + (vo + n + 7) / 8 > in.n) n--;
    if (with_valid && 16 * n + (vo + n + 7) / 8 > in.n) with_valid = 0;
    uint8_t *views = malloc(16 * n + 1);
    memcpy(views, in.p, 16 * n);
    for (size_t i = 0; i < n; i++) {
        if (!((fixmask >> (i % 8)) & 1) || nbufs == 0) continue;
        uint8_t *v = views + 16 * i;
        size_t b = (size_t)v[8] % nbufs;
        if (lens[b] < 13) continue;
        int32_t len = 13 + (int32_t)(ref_le32(v) & 0x7FFFFFFF) % (int32_t)(lens[b] - 12);
        int32_t off = (int32_t)((uint32_t)ref_le32(v + 12) % (uint32_t)(lens[b] - len + 1));
        put32(v, len);
        put32(v + 8, (int32_t)b);
        put32(v + 12, off);
        memcpy(v + 4, bufs[b] + off, 4);
    }
    size_t vb = with_valid ? (vo + n + 7) / 8 : 0;
    uint8_t *valid = with_valid ? wf_dup(in.p + 16 * n, vb) : NULL;
    uint8_t *pv = wf_dup(views, 16 * n);
    uint8_t *pb = wf_dup(bufs, nbufs * sizeof(uint8_t *));
    uint8_t *pl = wf_dup(lens, nbufs * 8);

    int64_t got = hs_columnar_view_refs(pv, n, valid, vo, (const uint8_t *const *)(void *)pb,
                                        (const int64_t *)(void *)pl, nbufs, utf8);
    int64_t want = ref_view_refs(pv, n, valid, vo, (const uint8_t *const *)bufs, lens, nbufs, utf8);
    WF_FUZZ_ASSERT(got == want);

    wf_free(pv, 16 * n);
    wf_free(pb, nbufs * sizeof(uint8_t *));
    wf_free(pl, nbufs * 8);
    if (valid) wf_free(valid, vb);
    for (size_t b = 0; b < nbufs; b++) {
        if (bufs[b]) wf_free(bufs[b], (size_t)lens[b]);
    }
    free(views);
    return 0;
}
