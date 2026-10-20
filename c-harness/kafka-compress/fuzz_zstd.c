/* Fuzz target: wf_zstd_get_frame_content_size + wf_zstd_decompress_into
 * on untrusted zstd input.
 *
 * Input layout: [mode][cap selector][data...].
 * mode bit 0 = 0: data is untrusted. The declared size never exceeds
 *   what the input could regenerate (and is never a value the Haskell
 *   would turn into a negative Int); when a streaming zstd reference
 *   decodes the input completely, the shim agrees for any cap.
 * mode bit 0 = 1: data is plaintext compressed at level (mode >> 1) - 60. */
#include "fuzz_common.h"

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size < 2 || size > (1u << 20)) return 0;
    uint8_t mode = data[0], sel = data[1];
    size_t n = size - 2;
    uint8_t *in = wf_dup(data + 2, n);
    uint8_t *src = in;
    size_t sn = n;
    uint8_t *comp = NULL;

    if (mode & 1) {
        size_t bound = ZSTD_compressBound(n);
        uint8_t *tmp = malloc(bound);
        sn = ZSTD_compress(tmp, bound, in, n, (int)(mode >> 1) - 60);
        WF_FUZZ_ASSERT(!ZSTD_isError(sn));
        comp = wf_dup(tmp, sn);
        free(tmp);
        src = comp;
        WF_FUZZ_ASSERT(wf_zstd_get_frame_content_size(src, sn) == n);
    }
    if (sn == 0) goto out;

    size_t fcs = wf_zstd_get_frame_content_size(src, sn);
    if (fcs != ZSTD_CONTENTSIZE_UNKNOWN && fcs != ZSTD_CONTENTSIZE_ERROR) {
        WF_FUZZ_ASSERT(fcs / 131072 <= sn / 4 + 1);
        WF_FUZZ_ASSERT(fcs < ((size_t)1 << 62));
    }

    cbuf ref = { 0 };
    int ref_ok = ref_zstd(src, sn, FUZZ_LIMIT, &ref);
    if (mode & 1) WF_FUZZ_ASSERT(ref_ok == 1 && ref.n == n && (n == 0 || memcmp(ref.p, in, n) == 0));
    if (ref_ok == 1) WF_FUZZ_ASSERT(fcs != ZSTD_CONTENTSIZE_ERROR);
    size_t cap = fuzz_cap(sel, ref_ok, ref.n);
    if (cap > FUZZ_LIMIT) cap = FUZZ_LIMIT;
    uint8_t *dst = wf_alloc(cap);
    size_t prod = 7;
    int rc = wf_zstd_decompress_into(src, sn, dst, cap, &prod);
    WF_FUZZ_ASSERT(rc == WF_DECOMPRESS_OK || rc == WF_DECOMPRESS_NEEDS_MORE_OUT || rc == WF_DECOMPRESS_BAD_INPUT);
    WF_FUZZ_ASSERT(prod <= cap);
    WF_FUZZ_ASSERT(rc == WF_DECOMPRESS_OK || prod == 0);
    if (ref_ok == 1) {
        if (cap >= ref.n) {
            WF_FUZZ_ASSERT(rc == WF_DECOMPRESS_OK && prod == ref.n);
            WF_FUZZ_ASSERT(prod == 0 || memcmp(dst, ref.p, prod) == 0);
        } else {
            WF_FUZZ_ASSERT(rc == WF_DECOMPRESS_NEEDS_MORE_OUT);
        }
    }
    wf_free(dst, cap);
    free(ref.p);
out:
    if (comp) wf_free(comp, sn);
    wf_free(in, n);
    return 0;
}
