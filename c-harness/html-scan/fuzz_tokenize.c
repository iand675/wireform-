/* Whole-document fuzz target: walks the input the way the Parse.hs
 * tokenizer loops do (text runs via scan_text_ascii, start tags via
 * scan_tagname + attribute value scans + skip_attrs, end tags via
 * scan_tagname + scan_gt), checking every kernel call against its
 * reference and that each step makes progress inside the buffer.
 *
 * Input: an HTML document, copied into an exact-size buffer. */
#include "harness.h"
#include "drive.h"

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size > (1u << 20)) return 0;
    uint8_t *buf = wf_dup(data, size);
    WF_FUZZ_ASSERT(hs_check_tokenize(buf, (ptrdiff_t)size) == 0);
    wf_free(buf, size);
    return 0;
}
