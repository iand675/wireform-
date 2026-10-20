/* Differential fuzz target for hermes_url_decode and
 * hermes_url_scan_special against the scalar references in api.h.
 *
 * Input layout: [mode][src...]. mode bit 0 selects form decoding ('+' is
 * a space). The source is copied into an exact-size buffer; the kernel
 * decodes it out of place into an exact srclen-byte buffer, into a buffer
 * of exactly the decoded length, in place, and through decodeWith's
 * scan / copy-prefix / decode-suffix split. Every result must equal the
 * reference (length or error code, and bytes), the decoded length never
 * exceeds the input, and the scan must agree with the reference. */
#include "harness.h"
#include "api.h"

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size < 1 || size > (1u << 20)) return 0;
    int mode = data[0] & 1;
    size_t n = size - 1;
    uint8_t *src = wf_dup(data + 1, n);
    uint8_t *want = malloc(n + 1);
    ptrdiff_t wn = ref_url_decode(src, n, want, mode);
    WF_FUZZ_ASSERT(wn <= (ptrdiff_t)n);

    size_t first = hermes_url_scan_special(src, n, mode);
    WF_FUZZ_ASSERT(first == ref_url_scan_special(src, n, mode));

    uint8_t *dst = wf_alloc(n);
    ptrdiff_t got = hermes_url_decode(src, n, dst, mode);
    WF_FUZZ_ASSERT(got == wn);
    WF_FUZZ_ASSERT(wn < 0 || memcmp(dst, want, (size_t)wn) == 0);

    if (wn >= 0) {
        uint8_t *tight = wf_alloc((size_t)wn);
        WF_FUZZ_ASSERT(hermes_url_decode(src, n, tight, mode) == wn);
        WF_FUZZ_ASSERT(memcmp(tight, want, (size_t)wn) == 0);
        wf_free(tight, (size_t)wn);
    }

    uint8_t *inplace = wf_dup(src, n);
    got = hermes_url_decode(inplace, n, inplace, mode);
    WF_FUZZ_ASSERT(got == wn);
    WF_FUZZ_ASSERT(wn < 0 || memcmp(inplace, want, (size_t)wn) == 0);
    wf_free(inplace, n);

    /* decodeWith: copy the clean prefix, decode the suffix after it. */
    if (n > 0 && first < n) {
        memcpy(dst, src, first);
        got = hermes_url_decode(src + first, n - first, dst + first, mode);
        WF_FUZZ_ASSERT(got < 0 ? got == wn : (ptrdiff_t)first + got == wn);
        WF_FUZZ_ASSERT(got < 0 || memcmp(dst, want, (size_t)wn) == 0);
    }

    wf_free(dst, n);
    free(want);
    wf_free(src, n);
    return 0;
}
