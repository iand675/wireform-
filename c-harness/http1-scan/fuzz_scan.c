/* Differential fuzz target: the four scanners (find_cr, find_lf,
 * find_non_token, find_non_fieldvalue) against the RFC references.
 *
 * Input layout: [offset byte][buffer...]. The buffer goes into an
 * exact-size copy; every scanner runs from the given offset (taken modulo
 * len + 18, so offsets past len are exercised) and from every offset in
 * the first 17 bytes, which sweeps the 16-byte block phase. */
#include "harness.h"
#include "api.h"

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size < 1 || size > (1u << 20)) return 0;
    int len = (int)(size - 1);
    int off = data[0] % (len + 18);
    uint8_t *buf = wf_dup(data + 1, (size_t)len);
    for (int kind = 0; kind < SCAN_KINDS; kind++) {
        int got = scan_kernel(kind, buf, off, len);
        WF_FUZZ_ASSERT(got == scan_ref(kind, buf, off, len));
        WF_FUZZ_ASSERT(got == len || (got >= off && got < len && scan_stops(kind, buf[got])));
        for (int o = 0; o <= 16 && o <= len; o++) {
            WF_FUZZ_ASSERT(scan_kernel(kind, buf, o, len) == scan_ref(kind, buf, o, len));
        }
    }
    wf_free(buf, (size_t)len);
    return 0;
}
