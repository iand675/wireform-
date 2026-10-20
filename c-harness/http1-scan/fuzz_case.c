/* Differential fuzz target: hs_http1_to_lower_ascii (into a disjoint
 * exact-size destination and in place) and hs_http1_ascii_ieq against the
 * references.
 *
 * Input layout: [split byte][a...][b...]. The bytes after the split byte
 * are cut in two halves at len / 2 (b gets the extra byte when odd and is
 * compared over the common length); ieq runs on (a, b), on (a, lower(a))
 * which must always match, and on (lower(a), lower(b)) which must agree
 * with ieq(a, b). The split byte flips the case of selected bytes of b so
 * case-only differences are common. */
#include "harness.h"
#include "api.h"

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size < 1 || size > (1u << 20)) return 0;
    uint8_t flip = data[0];
    size_t rest = size - 1;
    int n = (int)(rest / 2);
    uint8_t *a = wf_dup(data + 1, (size_t)n);
    uint8_t *b = wf_dup(data + 1 + n, (size_t)n);
    for (int i = 0; i < n; i++) {
        if (flip & (1u << (i & 7))) {
            uint8_t c = b[i];
            if ((c | 0x20) >= 'a' && (c | 0x20) <= 'z') b[i] = (uint8_t)(c ^ 0x20);
        }
    }

    uint8_t *la = wf_alloc((size_t)n), *want = malloc((size_t)n + 1);
    hs_http1_to_lower_ascii(a, la, n);
    ref_to_lower(a, want, n);
    WF_FUZZ_ASSERT(n == 0 || memcmp(la, want, (size_t)n) == 0);

    WF_FUZZ_ASSERT(hs_http1_ascii_ieq(a, b, n) == ref_ieq(a, b, n));
    WF_FUZZ_ASSERT(hs_http1_ascii_ieq(a, la, n) == 1);

    uint8_t *lb = wf_dup(b, (size_t)n);
    hs_http1_to_lower_ascii(lb, lb, n); /* in place, as lowercaseInPlace does */
    ref_to_lower(b, want, n);
    WF_FUZZ_ASSERT(n == 0 || memcmp(lb, want, (size_t)n) == 0);
    WF_FUZZ_ASSERT(hs_http1_ascii_ieq(la, lb, n) == hs_http1_ascii_ieq(a, b, n));
    WF_FUZZ_ASSERT(hs_http1_ascii_ieq(la, lb, n) == (n == 0 || memcmp(la, lb, (size_t)n) == 0));

    /* Every prefix length crosses the block / tail boundary somewhere. */
    for (int k = 0; k <= n && k <= 40; k++) {
        WF_FUZZ_ASSERT(hs_http1_ascii_ieq(a, b, k) == ref_ieq(a, b, k));
    }

    wf_free(a, (size_t)n);
    wf_free(b, (size_t)n);
    wf_free(la, (size_t)n);
    wf_free(lb, (size_t)n);
    free(want);
    return 0;
}
