/* Differential fuzz target for hs_xml_scan_structural.
 *
 * Input layout: [max lo][max hi][document...]. max_indices is the 16-bit
 * value taken modulo (len + 2), so the cap lands below, at and above the
 * number of structural bytes. The index array is exact-size (max_indices
 * uint32_t), so a write past the cap faults, and the document is an
 * exact-size copy, so a read past len faults. */
#include "harness.h"
#include "api.h"

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size < 2 || size > (1u << 20)) return 0;
    int len = (int)(size - 2);
    int max = (int)((data[0] | (unsigned)data[1] << 8) % (unsigned)(len + 2));
    uint8_t *buf = wf_dup(data + 2, (size_t)len);
    idx_buf ib = idx_alloc(max);
    uint32_t *idx = ib.p;
    uint32_t *want = malloc(((size_t)max + 1) * sizeof(uint32_t));
    int got = hs_xml_scan_structural(buf, len, idx, max);
    int exp = ref_xml_scan_structural(buf, len, want, max);
    WF_FUZZ_ASSERT(got == exp);
    WF_FUZZ_ASSERT(got >= 0 && got <= max);
    WF_FUZZ_ASSERT(got == 0 || memcmp(idx, want, (size_t)got * sizeof(uint32_t)) == 0);
    free(want);
    idx_free(ib);
    wf_free(buf, (size_t)len);
    return 0;
}
