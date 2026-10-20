/* Differential fuzz target: hs_http1_parse_hex against a count-then-decide
 * reference, from the given offset and from every offset of the input.
 *
 * Input layout: [offset byte][buffer...], exact-size copy of the buffer.
 * Checks the return code, *consumed, *out (on success or zero digits) and
 * that *out is untouched on overflow. */
#include "harness.h"
#include "api.h"

static void check(const uint8_t *buf, int off, int len)
{
    const uint64_t poison = 0x5a5a5a5a5a5a5a5aULL;
    uint64_t o1 = poison, o2 = poison;
    int c1 = -9, c2 = -9;
    int r1 = hs_http1_parse_hex(buf, off, len, &o1, &c1);
    int r2 = ref_parse_hex(buf, off, len, &o2, &c2);
    WF_FUZZ_ASSERT(r1 == r2);
    WF_FUZZ_ASSERT(c1 == c2);
    WF_FUZZ_ASSERT(o1 == o2);
    WF_FUZZ_ASSERT(c1 >= 0 && c1 <= 16);
    if (r1 < 0) WF_FUZZ_ASSERT(o1 == poison);
}

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size < 1 || size > 4096) return 0;
    int len = (int)(size - 1);
    uint8_t *buf = wf_dup(data + 1, (size_t)len);
    check(buf, data[0] % (len + 3), len);
    for (int o = 0; o <= len; o++) check(buf, o, len);
    wf_free(buf, (size_t)len);
    return 0;
}
