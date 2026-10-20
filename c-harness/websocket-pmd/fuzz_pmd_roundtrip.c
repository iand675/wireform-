/* Fuzz target: wf_pmd_deflate -> strip tail -> wf_pmd_inflate round trip.
 *
 * Input layout: [wbits][level][memlevel][step][flags] then messages as
 * [len: u16 LE][bytes]. Every compressed message must inflate back to
 * the plaintext through the shim (Haskell driver) and through a plain
 * zlib raw inflater with the negotiated window (so the deflater never
 * exceeds the agreed window, RFC 7692 sec 7.2.1). The deflate side runs
 * with fixed `step`-byte output buffers to exercise the resume path.
 * flags bit 0: reset between messages; bit 1: pass BS.empty as NULL. */
#include "harness.h"
#include "api.h"

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size < 5 || size > (1u << 20)) return 0;
    wf_input in = { data, size };
    int wb = 8 + (wf_take_u8(&in) & 7);
    int level = (int)(wf_take_u8(&in) % 12) - 1;     /* -1..10, 10 invalid */
    int memlevel = wf_take_u8(&in) % 11;             /* 0 and 10 invalid */
    size_t step = wf_take_u8(&in) & 63;
    uint8_t flags = wf_take_u8(&in);

    wf_pmd_stream *d = wf_pmd_deflate_new(wb, level, memlevel);
    wf_pmd_stream *i = wf_pmd_inflate_new(wb);
    WF_FUZZ_ASSERT(d && i);
    z_stream ref;
    memset(&ref, 0, sizeof ref);
    WF_FUZZ_ASSERT(inflateInit2(&ref, -wb) == Z_OK);

    while (in.n >= 2) {
        size_t len = wf_take_u8(&in);
        len |= (size_t)wf_take_u8(&in) << 8;
        if (len > in.n) len = in.n;
        const uint8_t *msg = in.p;
        in.p += len;
        in.n -= len;

        pmd_buf c = { 0 }, p = { 0 }, q = { 0 };
        int bad = 0;
        WF_FUZZ_ASSERT(pmd_deflate_msg(d, msg, len, step, &c, &bad) == WF_PMD_OK);
        WF_FUZZ_ASSERT(c.n >= 1);
        WF_FUZZ_ASSERT(pmd_inflate_msg(i, c.p, c.n, 0, 0, &p, &bad) == WF_PMD_OK);
        WF_FUZZ_ASSERT(!bad);
        WF_FUZZ_ASSERT(p.n == len && (len == 0 || memcmp(p.p, msg, len) == 0));
        WF_FUZZ_ASSERT(ref_inflate_msg(&ref, c.p, c.n, &q));
        WF_FUZZ_ASSERT(q.n == len && (len == 0 || memcmp(q.p, msg, len) == 0));
        free(c.p);
        free(p.p);
        free(q.p);
        if (flags & 1) {
            WF_FUZZ_ASSERT(wf_pmd_reset_deflate(d) == WF_PMD_OK);
            WF_FUZZ_ASSERT(wf_pmd_reset_inflate(i) == WF_PMD_OK);
            inflateReset(&ref);
        }
        if ((flags & 2) && len == 0) {
            /* Raw empty message as BS.empty: (NULL, 0). */
            uint8_t buf[16];
            size_t produced = 0;
            WF_FUZZ_ASSERT(wf_pmd_deflate(d, NULL, 0, buf, sizeof buf, &produced) == WF_PMD_OK);
            WF_FUZZ_ASSERT(produced >= 5 && memcmp(buf + produced - 4, pmd_tail, 4) == 0);
            pmd_buf p2 = { 0 }, q2 = { 0 };
            WF_FUZZ_ASSERT(pmd_inflate_msg(i, buf, produced - 4, 0, 0, &p2, &bad) == WF_PMD_OK);
            WF_FUZZ_ASSERT(p2.n == 0);
            WF_FUZZ_ASSERT(ref_inflate_msg(&ref, buf, produced - 4, &q2) && q2.n == 0);
            free(p2.p);
            free(q2.p);
        }
    }
    inflateEnd(&ref);
    wf_pmd_free(d);
    wf_pmd_free(i);
    return 0;
}
