/* Round-trip fuzz target: percent-encode arbitrary bytes with the
 * reference encoder, decode with hermes_url_decode, expect the original.
 *
 * Input layout: [mode][nchoice][choice bytes (nchoice)...][raw bytes...].
 * mode bit 0 selects form encoding ('+' escaped, ' ' as '+'); each choice
 * byte (cycled over the raw bytes) decides whether a byte that need not be
 * escaped is escaped anyway (bit 0) and the hex case (bit 1). The encoded
 * string is an exact-size buffer; the decode goes into an exact-size
 * buffer of the raw length (so the kernel cannot write one byte more than
 * it decodes) and also runs in place. */
#include "harness.h"
#include "api.h"

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size < 2 || size > (1u << 18)) return 0;
    wf_input in = { data, size };
    int mode = wf_take_u8(&in) & 1;
    size_t nchoice = wf_take_u8(&in);
    if (nchoice > in.n) nchoice = in.n;
    const uint8_t *choice = in.p;
    const uint8_t *raw = in.p + nchoice;
    size_t n = in.n - nchoice;

    uint8_t *enc = malloc(3 * n + 1);
    size_t en = ref_url_encode(raw, n, enc, mode, choice, nchoice);
    WF_FUZZ_ASSERT(en >= n && en <= 3 * n);
    uint8_t *s = wf_dup(enc, en);
    free(enc);

    uint8_t *d = wf_alloc(n);
    WF_FUZZ_ASSERT(hermes_url_decode(s, en, d, mode) == (ptrdiff_t)n);
    WF_FUZZ_ASSERT(n == 0 || memcmp(d, raw, n) == 0);
    wf_free(d, n);

    size_t first = hermes_url_scan_special(s, en, mode);
    WF_FUZZ_ASSERT(first == ref_url_scan_special(s, en, mode));

    WF_FUZZ_ASSERT(hermes_url_decode(s, en, s, mode) == (ptrdiff_t)n);
    WF_FUZZ_ASSERT(n == 0 || memcmp(s, raw, n) == 0);
    wf_free(s, en);
    return 0;
}
