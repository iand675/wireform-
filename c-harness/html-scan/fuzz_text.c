/* Differential fuzz target for the text-content scanners: scan_text,
 * scan_text_ascii, is_all_ascii and find_text_escape, against the
 * references in api.h.
 *
 * Input layout: [offset: 2 bytes LE][document...]. The document is copied
 * into an exact-size buffer so a read past its end faults; the offset is
 * taken modulo len + 1, sweeping the scan's start alignment. Each scanner
 * is checked at the offset, then chained to the end the way the
 * tokenizer consumes stop bytes, and once from 0. */
#include "harness.h"
#include "drive.h"

#define WHICH ((1u << HS_I_TEXT) | (1u << HS_I_TEXT_ASCII) | (1u << HS_I_ASCII) \
               | (1u << HS_I_TEXT_ESC))

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
