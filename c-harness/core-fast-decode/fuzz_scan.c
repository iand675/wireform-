/* Differential fuzz target for the offset-taking byte scanners of
 * fast_decode.c: find_nul, is_ascii, find_json_escape and skip_ws
 * (EDN whitespace + ';' comments), against api.h references.
 *
 * Input layout: [offset byte][bytes...]; offset modulo len + 2. */
#include "harness.h"
#include "api.h"

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size < 1 || size > (1u << 18)) return 0;
    int len = (int)(size - 1);
    int off = data[0] % (len + 2);
    uint8_t *buf = wf_dup(data + 1, (size_t)len);

    ptrdiff_t nul = hs_proto_find_nul(buf, off, len);
    WF_FUZZ_ASSERT(nul == ref_find_nul(buf, off, len));
    ptrdiff_t ws = hs_proto_skip_ws(buf, off, len);
    WF_FUZZ_ASSERT(ws == ref_skip_ws(buf, off, len));
    if (off <= len) {
        WF_FUZZ_ASSERT(ws >= off && ws <= len);
        int n = len - off;
        WF_FUZZ_ASSERT(hs_proto_is_ascii(buf, off, n) == ref_is_ascii(buf, off, n));
        ptrdiff_t je = hs_proto_find_json_escape(buf, off, n);
        WF_FUZZ_ASSERT(je == ref_find_json_escape(buf, off, n));
        WF_FUZZ_ASSERT(je >= off && je <= len);
    }
    wf_free(buf, (size_t)len);
    return 0;
}
