/* Fuzz target: wf_pmd_inflate on a sequence of untrusted messages.
 *
 * Input layout: [wbits][step][flags] then messages as [len: u16 LE][bytes].
 * Each message goes through the Haskell-style driver (doubling buffer)
 * and, on a second stream, through fixed `step`-byte calls. Invariants:
 *   - every call stays inside its exact-size dst and reports
 *     produced <= cap; NEEDS_MORE_OUT only with a full buffer;
 *   - both drivers agree on status and bytes (capacity independence);
 *   - when a plain zlib raw inflater accepts the message, so does the
 *     shim, with identical output.
 * flags bit 0: reset all streams between messages (no context takeover). */
#include "harness.h"
#include "api.h"

enum { LIMIT = 1 << 22 };

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size < 3 || size > (1u << 20)) return 0;
    wf_input in = { data, size };
    int wb = 8 + (wf_take_u8(&in) & 7);
    size_t step = wf_take_u8(&in) & 63;
    uint8_t flags = wf_take_u8(&in);

    wf_pmd_stream *a = wf_pmd_inflate_new(wb);
    wf_pmd_stream *b = wf_pmd_inflate_new(wb);
    WF_FUZZ_ASSERT(a && b);
    z_stream ref;
    memset(&ref, 0, sizeof ref);
    WF_FUZZ_ASSERT(inflateInit2(&ref, -(wb == 8 ? 9 : wb)) == Z_OK);
    int ref_ok = 1;

    while (in.n >= 2) {
        size_t len = wf_take_u8(&in);
        len |= (size_t)wf_take_u8(&in) << 8;
        if (len > in.n) len = in.n;
        const uint8_t *msg = in.p;
        in.p += len;
        in.n -= len;

        pmd_buf pa = { 0 }, pb = { 0 }, pr = { 0 };
        int bad = 0;
        int ra = pmd_inflate_msg(a, msg, len, 0, LIMIT, &pa, &bad);
        int rb = pmd_inflate_msg(b, msg, len, step ? step : 1, LIMIT, &pb, &bad);
        WF_FUZZ_ASSERT(!bad);
        WF_FUZZ_ASSERT(ra == WF_PMD_OK || ra == WF_PMD_BAD_INPUT || ra == WF_PMD_NEEDS_MORE_OUT);
        int limited = ra == WF_PMD_NEEDS_MORE_OUT || rb == WF_PMD_NEEDS_MORE_OUT;
        if (!limited) {
            WF_FUZZ_ASSERT(ra == rb);
            WF_FUZZ_ASSERT(pa.n == pb.n);
            WF_FUZZ_ASSERT(pa.n == 0 || memcmp(pa.p, pb.p, pa.n) == 0);
        }
        if (ref_ok && !limited) {
            ref_ok = ref_inflate_msg(&ref, msg, len, &pr);
            if (ref_ok) {
                WF_FUZZ_ASSERT(ra == WF_PMD_OK);
                WF_FUZZ_ASSERT(pr.n == pa.n);
                WF_FUZZ_ASSERT(pa.n == 0 || memcmp(pa.p, pr.p, pa.n) == 0);
            }
        }
        free(pa.p);
        free(pb.p);
        free(pr.p);
        /* A failed message fails the connection. */
        if (ra != WF_PMD_OK || limited) break;
        if (flags & 1) {
            WF_FUZZ_ASSERT(wf_pmd_reset_inflate(a) == WF_PMD_OK);
            WF_FUZZ_ASSERT(wf_pmd_reset_inflate(b) == WF_PMD_OK);
            inflateReset(&ref);
            ref_ok = 1;
        }
    }
    inflateEnd(&ref);
    wf_pmd_free(a);
    wf_pmd_free(b);
    return 0;
}
