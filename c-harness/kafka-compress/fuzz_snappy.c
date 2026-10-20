/* Fuzz target: the snappy shims (ring path and ByteString path).
 *
 * Input layout: [mode][data...].
 * mode bit 0 = 0: data is an untrusted snappy block. The library's own
 *   snappy_uncompress is the reference: the shims must agree with it,
 *   never reject a block the library decodes (plausibility bound), and
 *   stay inside exact-size buffers.
 * mode bit 0 = 1: data is plaintext; compress with the shim, then all
 *   three decompress paths must give it back. */
#include "fuzz_common.h"

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size < 1 || size > (1u << 20)) return 0;
    uint8_t mode = data[0];
    size_t n = size - 1;
    uint8_t *in = wf_dup(data + 1, n);

    if (mode & 1) {
        if (n == 0) goto out;
        char *c = NULL;
        size_t cn = 0;
        WF_FUZZ_ASSERT(snappy_compress_wrapper((const char *)in, n, &c, &cn) == cn && cn > 0);
        uint8_t *cc = wf_dup(c, cn);
        free(c);
        WF_FUZZ_ASSERT(snappy_get_uncompressed_length_wrapper((const char *)cc, cn) == n);
        uint8_t *dst = wf_alloc(n);
        WF_FUZZ_ASSERT(snappy_decompress_into((const char *)cc, cn, (char *)dst, n) == 1);
        WF_FUZZ_ASSERT(memcmp(dst, in, n) == 0);
        wf_free(dst, n);
        char *o = NULL;
        size_t on = 0;
        WF_FUZZ_ASSERT(snappy_decompress_wrapper((const char *)cc, cn, &o, &on) == n);
        WF_FUZZ_ASSERT(on == n && memcmp(o, in, n) == 0);
        free(o);
        wf_free(cc, cn);
        goto out;
    }

    if (n == 0) goto out;
    size_t ul = 0;
    int lib_hdr = snappy_uncompressed_length((const char *)in, n, &ul) == SNAPPY_OK;
    int lib_ok = 0;
    uint8_t *ref = NULL;
    if (lib_hdr && ul <= FUZZ_LIMIT) {
        ref = malloc(ul ? ul : 1);
        size_t rl = ul;
        lib_ok = snappy_uncompress((const char *)in, n, (char *)ref, &rl) == SNAPPY_OK;
        WF_FUZZ_ASSERT(!lib_ok || rl == ul);
    }

    size_t got = snappy_get_uncompressed_length_wrapper((const char *)in, n);
    if (!lib_hdr) WF_FUZZ_ASSERT(got == (size_t)-1);
    if (lib_ok) WF_FUZZ_ASSERT(got == ul);
    if (got != (size_t)-1) {
        WF_FUZZ_ASSERT(got == ul);
        /* The bound: nothing larger than the input can produce. */
        WF_FUZZ_ASSERT(got / 64 <= n / 3 + 1);
        if (got <= FUZZ_LIMIT) {
            uint8_t *dst = wf_alloc(got);
            int r = snappy_decompress_into((const char *)in, n, (char *)dst, got);
            WF_FUZZ_ASSERT(r == lib_ok);
            if (r) WF_FUZZ_ASSERT(got == 0 || memcmp(dst, ref, got) == 0);
            wf_free(dst, got);
        }
    }

    char *o = (char *)1;
    size_t on = 0;
    size_t r = snappy_decompress_wrapper((const char *)in, n, &o, &on);
    if (lib_ok && ul > 0) {
        WF_FUZZ_ASSERT(r == ul && on == ul && memcmp(o, ref, ul) == 0);
    } else if (ul <= FUZZ_LIMIT || got == (size_t)-1) {
        WF_FUZZ_ASSERT(r == 0 && o == NULL);
    }
    free(o == (char *)1 ? NULL : o);
    free(ref);
out:
    wf_free(in, n);
    return 0;
}
