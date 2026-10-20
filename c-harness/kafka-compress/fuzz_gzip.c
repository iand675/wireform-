/* Fuzz target: wf_gzip_inflate_into on untrusted gzip / zlib streams.
 *
 * Input layout: [mode][cap selector][data...].
 * mode bit 0 = 0: data is an untrusted stream, checked against
 *   streaming zlib: same bytes, OK iff the stream completes within cap,
 *   NEEDS_MORE_OUT only with a full buffer, BAD_INPUT otherwise.
 * mode bit 0 = 1: data is plaintext, compressed with a gzip (bit 1) or
 *   zlib wrapper at level (mode >> 2) % 10, then decompressed. */
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
        z_stream zs;
        memset(&zs, 0, sizeof zs);
        WF_FUZZ_ASSERT(deflateInit2(&zs, (mode >> 2) % 10, Z_DEFLATED, (mode & 2) ? 31 : 15, 8,
                                    Z_DEFAULT_STRATEGY) == Z_OK);
        size_t bound = deflateBound(&zs, (uLong)n) + 32;
        uint8_t *tmp = malloc(bound);
        zs.next_in = in;
        zs.avail_in = (uInt)n;
        zs.next_out = tmp;
        zs.avail_out = (uInt)bound;
        WF_FUZZ_ASSERT(deflate(&zs, Z_FINISH) == Z_STREAM_END);
        sn = bound - zs.avail_out;
        deflateEnd(&zs);
        comp = wf_dup(tmp, sn);
        free(tmp);
        src = comp;
    }
    if (sn == 0) goto out;

    cbuf ref = { 0 };
    int ref_ok = ref_gzip(src, sn, FUZZ_LIMIT, &ref);
    if (mode & 1) WF_FUZZ_ASSERT(ref_ok == 1 && ref.n == n && (n == 0 || memcmp(ref.p, in, n) == 0));
    size_t cap = fuzz_cap(sel, ref_ok, ref.n);
    uint8_t *dst = wf_alloc(cap);
    size_t prod = 7;
    int rc = wf_gzip_inflate_into(src, sn, dst, cap, &prod);
    WF_FUZZ_ASSERT(rc == WF_DECOMPRESS_OK || rc == WF_DECOMPRESS_NEEDS_MORE_OUT || rc == WF_DECOMPRESS_BAD_INPUT);
    WF_FUZZ_ASSERT(prod <= cap);
    WF_FUZZ_ASSERT(rc != WF_DECOMPRESS_NEEDS_MORE_OUT || prod == cap);
    if (ref_ok != -1) {
        size_t k = min_sz(prod, ref.n);
        WF_FUZZ_ASSERT(k == 0 || memcmp(dst, ref.p, k) == 0);
    }
    if (ref_ok == 1) {
        if (cap >= ref.n) {
            WF_FUZZ_ASSERT(rc == WF_DECOMPRESS_OK && prod == ref.n);
        } else {
            WF_FUZZ_ASSERT(rc == WF_DECOMPRESS_NEEDS_MORE_OUT);
        }
    } else if (ref_ok == 0) {
        WF_FUZZ_ASSERT(rc != WF_DECOMPRESS_OK);
        /* Truncated or corrupt with room to spare is never "grow". */
        if (ref.n < cap) WF_FUZZ_ASSERT(rc == WF_DECOMPRESS_BAD_INPUT);
    }
    wf_free(dst, cap);
    free(ref.p);
out:
    if (comp) wf_free(comp, sn);
    wf_free(in, n);
    return 0;
}
