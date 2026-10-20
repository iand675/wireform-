/* Fuzz target: wf_lz4f_decompress_into (ring path) and
 * lz4_decompress_wrapper (ByteString path) on untrusted LZ4 frames.
 *
 * Input layout: [mode][cap selector][data...].
 * mode bit 0 = 0: data is an untrusted frame, checked against a
 *   streaming LZ4F reference: same bytes, OK iff the frame completes
 *   within cap, NEEDS_MORE_OUT only with a full buffer, BAD_INPUT for
 *   malformed / truncated frames.
 * mode bit 0 = 1: data is plaintext, compressed at level (mode >> 1)
 *   and decompressed both ways. */
#include "fuzz_common.h"

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size < 2 || size > (1u << 20)) return 0;
    uint8_t mode = data[0], sel = data[1];
    size_t n = size - 2;
    uint8_t *in = wf_dup(data + 2, n);

    if (mode & 1) {
        if (n == 0) goto out;
        char *c = NULL;
        size_t cn = 0;
        WF_FUZZ_ASSERT(lz4_compress_wrapper_level((const char *)in, n, &c, &cn, (int)(mode >> 1) - 20) == cn);
        uint8_t *cc = wf_dup(c, cn);
        free(c);
        size_t cap = fuzz_cap(sel, 1, n);
        uint8_t *dst = wf_alloc(cap);
        size_t prod = 0, cons = 0;
        int rc = wf_lz4f_decompress_into(cc, cn, dst, cap, &prod, &cons);
        if (cap >= n) {
            WF_FUZZ_ASSERT(rc == WF_DECOMPRESS_OK && prod == n && cons == cn);
            WF_FUZZ_ASSERT(memcmp(dst, in, n) == 0);
        } else {
            WF_FUZZ_ASSERT(rc == WF_DECOMPRESS_NEEDS_MORE_OUT && prod == cap);
            WF_FUZZ_ASSERT(cap == 0 || memcmp(dst, in, cap) == 0);
        }
        wf_free(dst, cap);
        char *o = NULL;
        size_t on = 0;
        WF_FUZZ_ASSERT(lz4_decompress_wrapper((const char *)cc, cn, &o, &on) == n);
        WF_FUZZ_ASSERT(on == n && memcmp(o, in, n) == 0);
        free(o);
        wf_free(cc, cn);
        goto out;
    }

    if (n == 0) goto out;
    cbuf ref = { 0 };
    size_t ref_cons = 0;
    int ref_ok = ref_lz4f(in, n, FUZZ_LIMIT, &ref, &ref_cons);
    size_t cap = fuzz_cap(sel, ref_ok, ref.n);
    uint8_t *dst = wf_alloc(cap);
    size_t prod = 7, cons = 7;
    int rc = wf_lz4f_decompress_into(in, n, dst, cap, &prod, &cons);
    WF_FUZZ_ASSERT(rc == WF_DECOMPRESS_OK || rc == WF_DECOMPRESS_NEEDS_MORE_OUT || rc == WF_DECOMPRESS_BAD_INPUT);
    WF_FUZZ_ASSERT(prod <= cap && cons <= n);
    WF_FUZZ_ASSERT(rc != WF_DECOMPRESS_NEEDS_MORE_OUT || prod == cap);
    if (ref_ok != -1) {
        size_t k = min_sz(prod, ref.n);
        WF_FUZZ_ASSERT(k == 0 || memcmp(dst, ref.p, k) == 0);
    }
    if (ref_ok == 1) {
        if (cap >= ref.n) {
            WF_FUZZ_ASSERT(rc == WF_DECOMPRESS_OK && prod == ref.n && cons == ref_cons);
        } else {
            WF_FUZZ_ASSERT(rc == WF_DECOMPRESS_NEEDS_MORE_OUT);
        }
    } else if (ref_ok == 0) {
        WF_FUZZ_ASSERT(rc != WF_DECOMPRESS_OK);
    }
    wf_free(dst, cap);

    if (ref_ok != -1) {
        char *o = (char *)1;
        size_t on = 0;
        size_t r = lz4_decompress_wrapper((const char *)in, n, &o, &on);
        if (ref_ok == 1 && ref.n > 0) {
            WF_FUZZ_ASSERT(r == ref.n && on == r && memcmp(o, ref.p, r) == 0);
        } else {
            WF_FUZZ_ASSERT(r == 0 && o == NULL);
        }
        free(o);
    }
    free(ref.p);
out:
    wf_free(in, n);
    return 0;
}
