/* Differential fuzz target for the attribute-value scanners: scan_dquote,
 * scan_squote and their fused ASCII variants, skip_to_dquote,
 * skip_to_squote, scan_unquoted and find_attr_escape, against the
 * references in api.h.
 *
 * Input layout: [offset: 2 bytes LE][document...], as in fuzz_text. */
#include "harness.h"
#include "drive.h"

#define WHICH ((1u << HS_I_DQ) | (1u << HS_I_DQ_ASCII) | (1u << HS_I_SQ)          \
               | (1u << HS_I_SQ_ASCII) | (1u << HS_I_SKIP_DQ) | (1u << HS_I_SKIP_SQ) \
               | (1u << HS_I_UNQ) | (1u << HS_I_ATTR_ESC))

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
    wf_free(buf, (size_t)len);
    return 0;
}
