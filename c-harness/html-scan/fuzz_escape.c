/* Round-trip fuzz target for the escape finders: the Encode.hs
 * escapeText / escapeAttrValue loops driven by hs_html_find_text_escape
 * and hs_html_find_attr_escape must produce the byte-at-a-time escaping
 * of the input, which must unescape back to it.
 *
 * Input: the raw string to escape, copied into an exact-size buffer. */
#include "harness.h"
#include "drive.h"

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size > (1u << 20)) return 0;
    uint8_t *buf = wf_dup(data, size);
    WF_FUZZ_ASSERT(hs_check_escape(0, buf, (ptrdiff_t)size) == 0);
    WF_FUZZ_ASSERT(hs_check_escape(1, buf, (ptrdiff_t)size) == 0);
    wf_free(buf, size);
    return 0;
}
