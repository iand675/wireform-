/* Differential fuzz target for the tag scanners: scan_tagname,
 * skip_attrs (quoted sections, "/>", unterminated tags) and scan_gt,
 * against the references in api.h, plus the attribute walk from the
 * offset (readTagAttrsBS's sequence of value scans).
 *
 * Input layout: [offset: 2 bytes LE][tag text...], as in fuzz_text. */
#include "harness.h"
#include "drive.h"

#define WHICH ((1u << HS_I_TAGNAME) | (1u << HS_I_GT) | (1u << HS_I_SKIP))

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size < 2 || size > (1u << 20)) return 0;
    wf_input in = { data, size };
    unsigned off16 = wf_take_u8(&in);
    off16 |= (unsigned)wf_take_u8(&in) << 8;
    ptrdiff_t len = (ptrdiff_t)in.n;
    ptrdiff_t off = (ptrdiff_t)(off16 % (size_t)(len + 1));
    uint8_t *buf = wf_dup(in.p, (size_t)len);
    WF_FUZZ_ASSERT(hs_check_chain(WHICH, buf, off, len) == 0);
    WF_FUZZ_ASSERT(hs_check_all(WHICH, buf, 0, len) == 0);
    WF_FUZZ_ASSERT(hs_walk_attrs(buf, off, len) == 0);
    wf_free(buf, (size_t)len);
    return 0;
}
