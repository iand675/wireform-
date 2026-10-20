/* Unit, adversarial and round-trip tests for the Kafka codec shims:
 * snappy_ffi.c, lz4_ffi.c, wireform_decompress.c. */
#include "harness.h"
#include "api.h"

#include <sys/mman.h>

static void gen_data(wf_rng *r, uint8_t *p, size_t n, int kind)
{
    switch (kind % 4) {
    case 0: /* random: incompressible */
        wf_rng_fill(r, p, n);
        break;
    case 1: /* zeros: maximally compressible */
        memset(p, 0, n);
        break;
    case 2: { /* record-batch-ish text */
        static const char *w[] = { "{\"key\":", "\"value\"", ",", "kafka", "topic-", "0123", "}\n" };
        size_t i = 0;
        while (i < n) {
            const char *s = w[wf_rng_below(r, 7)];
            for (size_t k = 0; s[k] && i < n; k++) p[i++] = (uint8_t)s[k];
        }
        break;
    }
    default: /* small alphabet incl. 0x00 / 0xFF / 0x80 */
        for (size_t i = 0; i < n; i++) {
            static const uint8_t a[] = { 0x00, 0xFF, 0x80, 'a' };
            p[i] = a[wf_rng_below(r, 4)];
        }
        break;
    }
}

/* ------------------------------------------------------------------ */
/* Snappy                                                              */
/* ------------------------------------------------------------------ */

static void snappy_roundtrips(void)
{
    wf_rng r = wf_rng_from_env();
    long iters = wf_iters(400);
    for (long it = 0; it < iters; it++) {
        size_t n = it < 300 ? (size_t)it : (size_t)wf_rng_below(&r, 200000);
        if (n == 0) n = 1;
        uint8_t *plain = malloc(n);
        gen_data(&r, plain, n, (int)it);
        uint8_t *in = wf_dup(plain, n);
        char *c = NULL;
        size_t cn = 0;
        size_t ret = snappy_compress_wrapper((const char *)in, n, &c, &cn);
        WF_CHECK(ret > 0 && ret == cn);
        uint8_t *cc = wf_dup(c, cn);
        free(c);

        /* Ring path: exact sizing from the header. */
        size_t ul = snappy_get_uncompressed_length_wrapper((const char *)cc, cn);
        WF_CHECK_EQ(ul, n);
        uint8_t *dst = wf_alloc(n);
        WF_CHECK_EQ(snappy_decompress_into((const char *)cc, cn, (char *)dst, n), 1);
        WF_CHECK_MEM(dst, plain, n);
        wf_free(dst, n);
        /* One byte short: rejected, nothing written past the end. */
        dst = wf_alloc(n - 1);
        WF_CHECK_EQ(snappy_decompress_into((const char *)cc, cn, (char *)dst, n - 1), 0);
        wf_free(dst, n - 1);

        /* ByteString path. */
        char *o = NULL;
        size_t on = 0;
        WF_CHECK_EQ(snappy_decompress_wrapper((const char *)cc, cn, &o, &on), n);
        WF_CHECK_EQ(on, n);
        if (o && on == n) WF_CHECK_MEM(o, plain, n);
        free(o);

        /* Every truncation of the block fails cleanly. */
        if (it < 120) {
            for (size_t k = 0; k < cn; k++) {
                uint8_t *t = wf_dup(cc, k);
                size_t tl = snappy_get_uncompressed_length_wrapper((const char *)t, k);
                if (tl != (size_t)-1 && tl <= n) {
                    uint8_t *d = wf_alloc(tl);
                    WF_CHECK_EQ(snappy_decompress_into((const char *)t, k, (char *)d, tl), 0);
                    wf_free(d, tl);
                }
                char *o2 = NULL;
                size_t on2 = 0;
                WF_CHECK_EQ(snappy_decompress_wrapper((const char *)t, k, &o2, &on2), 0);
                WF_CHECK(o2 == NULL);
                wf_free(t, k);
            }
        }
        wf_free(cc, cn);
        wf_free(in, n);
        free(plain);
    }
}

/* Declared lengths the block cannot produce are malformed: before the
 * fix five bytes made the Haskell allocate 4 GiB (mallocByteString of
 * the declared length) before snappy rejected the block. */
static void snappy_huge_declared_length(void)
{
    static const uint8_t claim4g[] = { 0xff, 0xff, 0xff, 0xff, 0x0f };
    static const uint8_t claim4g_lit[] = { 0xff, 0xff, 0xff, 0xff, 0x0f, 0x00, 'x' };
    static const uint8_t claim_1m[] = { 0x80, 0x80, 0x40, 0x00, 'x' };
    const struct { const uint8_t *p; size_t n; } cases[] = {
        { claim4g, sizeof claim4g }, { claim4g_lit, sizeof claim4g_lit }, { claim_1m, sizeof claim_1m },
    };
    for (size_t i = 0; i < sizeof cases / sizeof cases[0]; i++) {
        uint8_t *b = wf_dup(cases[i].p, cases[i].n);
        WF_CHECK_EQ(snappy_get_uncompressed_length_wrapper((const char *)b, cases[i].n), (size_t)-1);
        char *o = (char *)1;
        size_t on = 0;
        WF_CHECK_EQ(snappy_decompress_wrapper((const char *)b, cases[i].n, &o, &on), 0);
        WF_CHECK(o == NULL);
        wf_free(b, cases[i].n);
    }
    /* Overlong varints / truncated headers. */
    static const uint8_t overlong[] = { 0x80, 0x80, 0x80, 0x80, 0x80, 0x01 };
    WF_CHECK_EQ(snappy_get_uncompressed_length_wrapper((const char *)overlong, sizeof overlong), (size_t)-1);
    WF_CHECK_EQ(snappy_get_uncompressed_length_wrapper((const char *)overlong, 2), (size_t)-1);
    WF_CHECK_EQ(snappy_get_uncompressed_length_wrapper((const char *)overlong, 0), (size_t)-1);
    WF_CHECK_EQ(snappy_get_uncompressed_length_wrapper(NULL, 3), (size_t)-1);
}

/* The plausibility bound is tight enough never to reject a valid block:
 * the densest possible stream (1 literal byte, then 3-byte copies of 64
 * bytes at offset 1) decodes, and its length is accepted. */
static void snappy_densest_stream_accepted(void)
{
    for (size_t copies = 0; copies <= 3000; copies += (copies < 20 ? 1 : 97)) {
        size_t ulen = 1 + copies * 64;
        uint8_t hdr[8];
        size_t h = 0;
        size_t v = ulen;
        while (v >= 0x80) {
            hdr[h++] = (uint8_t)(v | 0x80);
            v >>= 7;
        }
        hdr[h++] = (uint8_t)v;
        size_t n = h + 2 + copies * 3;
        uint8_t *b = malloc(n);
        memcpy(b, hdr, h);
        b[h] = 0x00; /* literal, length 1 */
        b[h + 1] = 'z';
        for (size_t k = 0; k < copies; k++) {
            uint8_t *e = b + h + 2 + k * 3;
            e[0] = (uint8_t)((63u << 2) | 2u); /* copy, 2-byte offset, len 64 */
            e[1] = 1;
            e[2] = 0;
        }
        uint8_t *in = wf_dup(b, n);
        WF_CHECK_EQ(snappy_get_uncompressed_length_wrapper((const char *)in, n), ulen);
        uint8_t *dst = wf_alloc(ulen);
        WF_CHECK_EQ(snappy_decompress_into((const char *)in, n, (char *)dst, ulen), 1);
        int allz = 1;
        for (size_t k = 0; k < ulen; k++) allz &= dst[k] == 'z';
        WF_CHECK(allz);
        char *o = NULL;
        size_t on = 0;
        WF_CHECK_EQ(snappy_decompress_wrapper((const char *)in, n, &o, &on), ulen);
        free(o);
        wf_free(dst, ulen);
        wf_free(in, n);
        free(b);
    }
}

/* A block that decodes to zero bytes ("\0"): the wrapper reports 0 and
 * leaves nothing to free (before the fix it returned 0 with a malloc(0)
 * buffer that the Haskell, seeing failure, never freed). The ring path
 * handles it as a 0-byte success. NULL arguments are rejected. */
static void snappy_empty_and_null(void)
{
    static const uint8_t empty_block[] = { 0x00 };
    uint8_t *b = wf_dup(empty_block, 1);
    char *o = (char *)1;
    size_t on = 77;
    WF_CHECK_EQ(snappy_decompress_wrapper((const char *)b, 1, &o, &on), 0);
    WF_CHECK(o == NULL);
    WF_CHECK_EQ(snappy_get_uncompressed_length_wrapper((const char *)b, 1), 0);
    uint8_t one;
    WF_CHECK_EQ(snappy_decompress_into((const char *)b, 1, (char *)&one, 0), 1);
    wf_free(b, 1);

    char *c = NULL;
    size_t cn = 0;
    WF_CHECK_EQ(snappy_compress_wrapper(NULL, 0, &c, &cn), 0);
    WF_CHECK_EQ(snappy_compress_wrapper("x", 1, NULL, &cn), 0);
    WF_CHECK_EQ(snappy_compress_wrapper("x", 1, &c, NULL), 0);
    WF_CHECK_EQ(snappy_compress_wrapper("", 0, &c, &cn), 1);
    WF_CHECK_EQ(cn, 1);
    free(c);
    WF_CHECK_EQ(snappy_decompress_wrapper(NULL, 1, &o, &on), 0);
    WF_CHECK_EQ(snappy_decompress_wrapper("x", 1, NULL, &on), 0);
    WF_CHECK_EQ(snappy_decompress_wrapper("x", 1, &o, NULL), 0);
    WF_CHECK_EQ(snappy_decompress_into(NULL, 1, (char *)&one, 1), 0);
    WF_CHECK_EQ(snappy_decompress_into("\x01\x00x", 3, NULL, 1), 0);
}

/* ------------------------------------------------------------------ */
/* LZ4 frame                                                           */
/* ------------------------------------------------------------------ */

static uint8_t *lz4_frame(const uint8_t *p, size_t n, const LZ4F_preferences_t *prefs, size_t *out_n)
{
    size_t bound = LZ4F_compressFrameBound(n, prefs);
    uint8_t *f = malloc(bound);
    size_t r = LZ4F_compressFrame(f, bound, p, n, prefs);
    if (LZ4F_isError(r)) abort();
    *out_n = r;
    return f;
}

/* wf_lz4f_decompress_into against exact, short and generous capacities;
 * lz4_decompress_wrapper against the plaintext; every compression level
 * the Haskell can pass plus out-of-range ones. */
static void lz4_roundtrips(void)
{
    wf_rng r = wf_rng_from_env();
    static const int levels[] = { -5, 0, 1, 3, 9, 12, 16, 17, 1000 };
    long iters = wf_iters(150);
    for (long it = 0; it < iters; it++) {
        size_t n = it < 40 ? (size_t)it + 1 : 1 + (size_t)wf_rng_below(&r, 300000);
        uint8_t *plain = malloc(n);
        gen_data(&r, plain, n, (int)it);
        uint8_t *in = wf_dup(plain, n);
        char *c = NULL;
        size_t cn = 0;
        int lvl = levels[it % (long)(sizeof levels / sizeof levels[0])];
        size_t ret = lz4_compress_wrapper_level((const char *)in, n, &c, &cn, lvl);
        WF_CHECK(ret > 0 && ret == cn);
        uint8_t *cc = wf_dup(c, cn);
        free(c);

        size_t caps[] = { n, n + 1, n - 1, n / 2, 1, n * 3 + 100 };
        for (size_t k = 0; k < sizeof caps / sizeof caps[0]; k++) {
            size_t cap = caps[k];
            if (cap == 0) continue;
            uint8_t *dst = wf_alloc(cap);
            size_t prod = 99, cons = 99;
            int rc = wf_lz4f_decompress_into(cc, cn, dst, cap, &prod, &cons);
            if (cap >= n) {
                WF_CHECK_EQ(rc, WF_DECOMPRESS_OK);
                WF_CHECK_EQ(prod, n);
                WF_CHECK_EQ(cons, cn);
                WF_CHECK_MEM(dst, plain, n);
            } else {
                WF_CHECK_EQ(rc, WF_DECOMPRESS_NEEDS_MORE_OUT);
                WF_CHECK_EQ(prod, cap);
                WF_CHECK_MEM(dst, plain, cap);
            }
            wf_free(dst, cap);
        }

        char *o = NULL;
        size_t on = 0;
        WF_CHECK_EQ(lz4_decompress_wrapper((const char *)cc, cn, &o, &on), n);
        WF_CHECK_EQ(on, n);
        if (o && on == n) WF_CHECK_MEM(o, plain, n);
        free(o);
        wf_free(cc, cn);
        wf_free(in, n);
        free(plain);
    }
}

/* Before the fix lz4_decompress_wrapper made one LZ4F_decompress call
 * into max(4 * input, 64 KiB) bytes and returned whatever fit as
 * success: anything compressing better than 4:1 above 64 KiB came back
 * silently truncated. */
static void lz4_wrapper_high_ratio_not_truncated(void)
{
    size_t sizes[] = { 65536, 65537, 1 << 20, (5 << 20) + 3 };
    for (size_t k = 0; k < sizeof sizes / sizeof sizes[0]; k++) {
        size_t n = sizes[k];
        uint8_t *z = calloc(1, n);
        z[n / 2] = 7;
        char *c = NULL;
        size_t cn = 0;
        WF_CHECK(lz4_compress_wrapper_level((const char *)z, n, &c, &cn, 0) > 0);
        WF_CHECK(cn * 4 < n);
        char *o = NULL;
        size_t on = 0;
        WF_CHECK_EQ(lz4_decompress_wrapper(c, cn, &o, &on), n);
        WF_CHECK_EQ(on, n);
        if (o && on == n) WF_CHECK_MEM(o, z, n);
        free(o);
        free(c);
        free(z);
    }
}

/* Truncated frames: every proper prefix. With room to spare the input
 * ran out mid-frame, so the answer is BAD_INPUT; before the fix it was
 * NEEDS_MORE_OUT, and Ring.hs doubled its buffer until allocation
 * failed. The ByteString wrapper fails too (it used to return a
 * truncated prefix as success). */
static void lz4_truncated_frames(void)
{
    wf_rng r = wf_rng_from_env();
    LZ4F_preferences_t prefs;
    memset(&prefs, 0, sizeof prefs);
    for (int variant = 0; variant < 4; variant++) {
        prefs.frameInfo.blockMode = (variant & 1) ? LZ4F_blockIndependent : LZ4F_blockLinked;
        prefs.frameInfo.contentChecksumFlag = (variant & 2) ? LZ4F_contentChecksumEnabled : LZ4F_noContentChecksum;
        prefs.frameInfo.blockChecksumFlag = (variant & 2) ? LZ4F_blockChecksumEnabled : LZ4F_noBlockChecksum;
        prefs.frameInfo.contentSize = (variant == 3) ? 3000 : 0;
        prefs.frameInfo.blockSizeID = LZ4F_max64KB;
        size_t n = 3000;
        uint8_t plain[3000];
        gen_data(&r, plain, n, 2 + variant);
        size_t fn = 0;
        uint8_t *f = lz4_frame(plain, n, &prefs, &fn);
        for (size_t k = 1; k < fn; k++) {
            uint8_t *t = wf_dup(f, k);
            uint8_t *dst = wf_alloc(n + 64);
            size_t prod = 0, cons = 0;
            WF_CHECK_EQ(wf_lz4f_decompress_into(t, k, dst, n + 64, &prod, &cons), WF_DECOMPRESS_BAD_INPUT);
            WF_CHECK(prod <= n);
            WF_CHECK_MEM(dst, plain, prod);
            wf_free(dst, n + 64);
            char *o = NULL;
            size_t on = 0;
            WF_CHECK_EQ(lz4_decompress_wrapper((const char *)t, k, &o, &on), 0);
            WF_CHECK(o == NULL);
            wf_free(t, k);
        }
        /* Exact capacity, every capacity below it. */
        uint8_t *ff = wf_dup(f, fn);
        for (size_t cap = 1; cap <= n; cap += (cap < 70 ? 1 : 131)) {
            uint8_t *dst = wf_alloc(cap);
            size_t prod = 0, cons = 0;
            int rc = wf_lz4f_decompress_into(ff, fn, dst, cap, &prod, &cons);
            WF_CHECK_EQ(rc, cap >= n ? WF_DECOMPRESS_OK : WF_DECOMPRESS_NEEDS_MORE_OUT);
            WF_CHECK_EQ(prod, cap >= n ? n : cap);
            WF_CHECK_MEM(dst, plain, prod);
            wf_free(dst, cap);
        }
        uint8_t *dst = wf_alloc(n);
        size_t prod = 0, cons = 0;
        WF_CHECK_EQ(wf_lz4f_decompress_into(ff, fn, dst, n, &prod, &cons), WF_DECOMPRESS_OK);
        WF_CHECK_EQ(cons, fn);
        wf_free(dst, n);
        /* Corrupt a byte in the middle of a checksummed frame. */
        if (variant & 2) {
            ff[fn / 2] ^= 0x40;
            dst = wf_alloc(n + 64);
            WF_CHECK_EQ(wf_lz4f_decompress_into(ff, fn, dst, n + 64, &prod, &cons), WF_DECOMPRESS_BAD_INPUT);
            wf_free(dst, n + 64);
        }
        wf_free(ff, fn);
        free(f);
    }
}

/* Linked 64 KiB blocks across a buffer growth: the wrapper restarts the
 * frame in a fresh buffer rather than realloc'ing one the decoder holds
 * pointers into. Data with long-distance repeats so blocks reference
 * their predecessors. */
static void lz4_linked_blocks_growth(void)
{
    wf_rng r = wf_rng_from_env();
    size_t n = 4 << 20;
    uint8_t *plain = malloc(n);
    wf_rng_fill(&r, plain, 1000);
    for (size_t k = 1000; k < n; k++) plain[k] = (uint8_t)(plain[k - 1000] + (k % 4096 == 0));
    LZ4F_preferences_t prefs;
    memset(&prefs, 0, sizeof prefs);
    prefs.frameInfo.blockMode = LZ4F_blockLinked;
    prefs.frameInfo.blockSizeID = LZ4F_max64KB;
    prefs.frameInfo.contentChecksumFlag = LZ4F_contentChecksumEnabled;
    size_t fn = 0;
    uint8_t *f = lz4_frame(plain, n, &prefs, &fn);
    WF_CHECK(fn * 16 < n); /* several growth restarts */
    char *o = NULL;
    size_t on = 0;
    WF_CHECK_EQ(lz4_decompress_wrapper((const char *)f, fn, &o, &on), n);
    if (o && on == n) WF_CHECK_MEM(o, plain, n);
    free(o);
    free(f);
    free(plain);
}

/* Empty frame, trailing bytes after a frame, garbage, NULL args. */
static void lz4_edges(void)
{
    LZ4F_preferences_t prefs;
    memset(&prefs, 0, sizeof prefs);
    size_t fn = 0;
    uint8_t *f = lz4_frame((const uint8_t *)"", 0, &prefs, &fn);
    uint8_t dst[16];
    size_t prod = 9, cons = 9;
    WF_CHECK_EQ(wf_lz4f_decompress_into(f, fn, dst, sizeof dst, &prod, &cons), WF_DECOMPRESS_OK);
    WF_CHECK_EQ(prod, 0);
    WF_CHECK_EQ(cons, fn);
    char *o = (char *)1;
    size_t on = 0;
    WF_CHECK_EQ(lz4_decompress_wrapper((const char *)f, fn, &o, &on), 0);
    WF_CHECK(o == NULL);

    uint8_t *tr = malloc(fn + 5);
    memcpy(tr, f, fn);
    memcpy(tr + fn, "junk!", 5);
    WF_CHECK_EQ(wf_lz4f_decompress_into(tr, fn + 5, dst, sizeof dst, &prod, &cons), WF_DECOMPRESS_OK);
    WF_CHECK_EQ(cons, fn);
    free(tr);
    free(f);

    static const uint8_t garbage[] = { 0x00, 0xff, 0x80, 0x01, 0x02, 0x03, 0x04, 0x05 };
    static const uint8_t magic_only[] = { 0x04, 0x22, 0x4d, 0x18 };
    static const uint8_t bad_flg[] = { 0x04, 0x22, 0x4d, 0x18, 0xff, 0xff, 0xff };
    const struct { const uint8_t *p; size_t n; } cases[] = {
        { garbage, sizeof garbage }, { magic_only, sizeof magic_only }, { bad_flg, sizeof bad_flg },
        { garbage, 1 },
    };
    for (size_t i = 0; i < sizeof cases / sizeof cases[0]; i++) {
        uint8_t *b = wf_dup(cases[i].p, cases[i].n);
        WF_CHECK_EQ(wf_lz4f_decompress_into(b, cases[i].n, dst, sizeof dst, NULL, NULL), WF_DECOMPRESS_BAD_INPUT);
        WF_CHECK_EQ(lz4_decompress_wrapper((const char *)b, cases[i].n, &o, &on), 0);
        wf_free(b, cases[i].n);
    }
    WF_CHECK_EQ(lz4_decompress_wrapper(NULL, 1, &o, &on), 0);
    WF_CHECK_EQ(lz4_decompress_wrapper("x", 1, NULL, &on), 0);
    WF_CHECK_EQ(lz4_decompress_wrapper("x", 1, &o, NULL), 0);
    WF_CHECK_EQ(lz4_compress_wrapper_level(NULL, 1, &o, &on, 0), 0);
    WF_CHECK_EQ(lz4_compress_wrapper_level("x", 1, NULL, &on, 0), 0);
    WF_CHECK_EQ(lz4_compress_wrapper_level("x", 1, &o, NULL, 0), 0);
    WF_CHECK(lz4_compress_wrapper("abc", 3, &o, &on) > 0);
    free(o);
}

/* ------------------------------------------------------------------ */
/* Gzip / zlib                                                         */
/* ------------------------------------------------------------------ */

static uint8_t *zlib_compress(const uint8_t *p, size_t n, int wbits, int level, size_t *out_n)
{
    z_stream zs;
    memset(&zs, 0, sizeof zs);
    if (deflateInit2(&zs, level, Z_DEFLATED, wbits, 8, Z_DEFAULT_STRATEGY) != Z_OK) abort();
    size_t bound = deflateBound(&zs, (uLong)n) + 32;
    uint8_t *o = malloc(bound);
    zs.next_in = (Bytef *)p;
    zs.avail_in = (uInt)n;
    zs.next_out = o;
    zs.avail_out = (uInt)bound;
    if (deflate(&zs, Z_FINISH) != Z_STREAM_END) abort();
    *out_n = bound - zs.avail_out;
    deflateEnd(&zs);
    return o;
}

static void gzip_roundtrips(void)
{
    wf_rng r = wf_rng_from_env();
    long iters = wf_iters(200);
    for (long it = 0; it < iters; it++) {
        size_t n = it < 50 ? (size_t)it : (size_t)wf_rng_below(&r, 300000);
        uint8_t *plain = malloc(n ? n : 1);
        gen_data(&r, plain, n, (int)it);
        int wbits = (it & 1) ? 15 + 16 : 15; /* gzip and zlib wrappers */
        size_t cn = 0;
        uint8_t *c = zlib_compress(plain, n, wbits, (int)(it % 10), &cn);
        uint8_t *cc = wf_dup(c, cn);
        free(c);
        size_t caps[] = { n, n + 1, n - 1, n / 3, 1, 2 * n + 64 };
        for (size_t k = 0; k < sizeof caps / sizeof caps[0]; k++) {
            size_t cap = caps[k];
            if (cap == 0 || cap > 2 * n + 64) continue;
            uint8_t *dst = wf_alloc(cap);
            size_t prod = 99;
            int rc = wf_gzip_inflate_into(cc, cn, dst, cap, &prod);
            if (cap >= n) {
                WF_CHECK_EQ(rc, WF_DECOMPRESS_OK);
                WF_CHECK_EQ(prod, n);
                WF_CHECK_MEM(dst, plain, n);
            } else {
                WF_CHECK_EQ(rc, WF_DECOMPRESS_NEEDS_MORE_OUT);
                WF_CHECK_EQ(prod, cap);
                WF_CHECK_MEM(dst, plain, cap);
            }
            wf_free(dst, cap);
        }
        wf_free(cc, cn);
        free(plain);
    }
}

/* Truncated gzip: with room to spare it is BAD_INPUT. Before the fix
 * zlib's Z_BUF_ERROR was reported as NEEDS_MORE_OUT regardless, so
 * gzipDecompressFromPtr doubled its buffer forever on a cut-off batch. */
static void gzip_truncated_and_corrupt(void)
{
    wf_rng r = wf_rng_from_env();
    size_t n = 5000;
    uint8_t plain[5000];
    gen_data(&r, plain, n, 2);
    size_t cn = 0;
    uint8_t *c = zlib_compress(plain, n, 31, 6, &cn);
    for (size_t k = 0; k < cn; k++) {
        uint8_t *t = wf_dup(c, k);
        uint8_t *dst = wf_alloc(n + 100);
        size_t prod = 99;
        WF_CHECK_EQ(wf_gzip_inflate_into(t, k, dst, n + 100, &prod), WF_DECOMPRESS_BAD_INPUT);
        WF_CHECK(prod <= n);
        WF_CHECK_MEM(dst, plain, prod);
        wf_free(dst, n + 100);
        wf_free(t, k);
    }
    /* Corrupt the CRC32 / ISIZE trailer and the header. */
    size_t spots[] = { cn - 1, cn - 5, cn - 8, 0, 2 };
    for (size_t k = 0; k < sizeof spots / sizeof spots[0]; k++) {
        uint8_t *t = wf_dup(c, cn);
        t[spots[k]] ^= 0x01;
        uint8_t *dst = wf_alloc(n + 100);
        size_t prod = 0;
        WF_CHECK_EQ(wf_gzip_inflate_into(t, cn, dst, n + 100, &prod), WF_DECOMPRESS_BAD_INPUT);
        wf_free(dst, n + 100);
        wf_free(t, cn);
    }
    /* A second gzip member after the first is not decoded: Kafka
     * producers write one member. Output = first member. */
    uint8_t *two = malloc(2 * cn);
    memcpy(two, c, cn);
    memcpy(two + cn, c, cn);
    uint8_t *dst = wf_alloc(3 * n);
    size_t prod = 0;
    WF_CHECK_EQ(wf_gzip_inflate_into(two, 2 * cn, dst, 3 * n, &prod), WF_DECOMPRESS_OK);
    WF_CHECK_EQ(prod, n);
    wf_free(dst, 3 * n);
    free(two);
    free(c);

    static const uint8_t garbage[] = { 0x1f, 0x8b, 0x08, 0xff, 0, 0, 0, 0, 0, 0xff, 0x06 };
    static const uint8_t raw_deflate[] = { 0xf3, 0x48, 0xcd, 0xc9, 0xc9, 0x07, 0x00 };
    static const uint8_t ff[] = { 0xff, 0xff, 0xff, 0xff };
    const struct { const uint8_t *p; size_t n; } cases[] = {
        { garbage, sizeof garbage }, { raw_deflate, sizeof raw_deflate }, { ff, sizeof ff }, { ff, 1 },
    };
    for (size_t i = 0; i < sizeof cases / sizeof cases[0]; i++) {
        uint8_t *b = wf_dup(cases[i].p, cases[i].n);
        uint8_t d[64];
        WF_CHECK_EQ(wf_gzip_inflate_into(b, cases[i].n, d, sizeof d, NULL), WF_DECOMPRESS_BAD_INPUT);
        wf_free(b, cases[i].n);
    }
    /* Empty plaintext. */
    size_t en = 0;
    uint8_t *e = zlib_compress((const uint8_t *)"", 0, 31, 6, &en);
    uint8_t d[4];
    WF_CHECK_EQ(wf_gzip_inflate_into(e, en, d, sizeof d, &prod), WF_DECOMPRESS_OK);
    WF_CHECK_EQ(prod, 0);
    free(e);
}

/* A gzip bomb (64 MiB of zeros) never writes past the cap it is given,
 * whatever that cap is; the Haskell-style doubling reaches the result. */
static void gzip_bomb_capped(void)
{
    size_t n = (size_t)64 << 20;
    uint8_t *z = calloc(1, n);
    size_t cn = 0;
    uint8_t *c = zlib_compress(z, n, 31, 9, &cn);
    WF_CHECK(cn < n / 900);
    size_t cap = 256 * 1024;
    int rc;
    size_t prod = 0;
    for (;;) {
        uint8_t *dst = wf_alloc(cap);
        rc = wf_gzip_inflate_into(c, cn, dst, cap, &prod);
        if (rc == WF_DECOMPRESS_NEEDS_MORE_OUT) WF_CHECK_EQ(prod, cap);
        wf_free(dst, cap);
        if (rc != WF_DECOMPRESS_NEEDS_MORE_OUT) break;
        cap *= 2;
    }
    WF_CHECK_EQ(rc, WF_DECOMPRESS_OK);
    WF_CHECK_EQ(prod, n);
    free(c);
    free(z);
}

/* dst_cap above 4 GiB. Before the fix the shim cast dst_cap to uInt:
 * 2^32 + 100 became 100 and produced was reported as 2^32 + 50 for a
 * 50-byte result (the Haskell then exposed 4 GiB of uninitialised
 * buffer); exactly 2^32 became 0, an endless NEEDS_MORE_OUT that
 * gzipDecompressFromPtr reaches when doubling from 256 KiB. Lazily
 * committed mapping; only the first page is touched. */
static void gzip_capacity_above_4gib(void)
{
    if (sizeof(size_t) < 8) return;
    size_t map = ((size_t)1 << 33) + 65536;
    uint8_t *big = mmap(NULL, map, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
    if (big == MAP_FAILED) {
        fprintf(stderr, "gzip_capacity_above_4gib: mmap failed, skipped\n");
        return;
    }
    uint8_t plain[50];
    for (int i = 0; i < 50; i++) plain[i] = (uint8_t)(i * 7);
    size_t cn = 0;
    uint8_t *c = zlib_compress(plain, 50, 31, 6, &cn);
    size_t caps[] = { (size_t)1 << 32, ((size_t)1 << 32) + 100, ((size_t)1 << 33) + 3 };
    for (size_t k = 0; k < sizeof caps / sizeof caps[0]; k++) {
        size_t prod = 0;
        WF_CHECK_EQ(wf_gzip_inflate_into(c, cn, big, caps[k], &prod), WF_DECOMPRESS_OK);
        WF_CHECK_EQ(prod, 50);
        WF_CHECK_MEM(big, plain, 50);
    }
    free(c);
    munmap(big, map);
}

/* ------------------------------------------------------------------ */
/* Zstd                                                                */
/* ------------------------------------------------------------------ */

static void zstd_roundtrips(void)
{
    wf_rng r = wf_rng_from_env();
    static const int levels[] = { 1, 3, -5, 9, 19 };
    long iters = wf_iters(150);
    for (long it = 0; it < iters; it++) {
        size_t n = it < 40 ? (size_t)it : (size_t)wf_rng_below(&r, 400000);
        uint8_t *plain = malloc(n ? n : 1);
        gen_data(&r, plain, n, (int)it);
        size_t bound = ZSTD_compressBound(n);
        uint8_t *c = malloc(bound);
        size_t cn = ZSTD_compress(c, bound, plain, n, levels[it % 5]);
        WF_CHECK(!ZSTD_isError(cn));
        uint8_t *cc = wf_dup(c, cn);
        free(c);
        WF_CHECK_EQ(wf_zstd_get_frame_content_size(cc, cn), n);
        size_t caps[] = { n, n + 1, n - 1, n / 2, 3 * n + 10 };
        for (size_t k = 0; k < sizeof caps / sizeof caps[0]; k++) {
            size_t cap = caps[k];
            if (cap > 3 * n + 10) continue;
            uint8_t *dst = wf_alloc(cap);
            size_t prod = 99;
            int rc = wf_zstd_decompress_into(cc, cn, dst, cap, &prod);
            if (cap >= n) {
                WF_CHECK_EQ(rc, WF_DECOMPRESS_OK);
                WF_CHECK_EQ(prod, n);
                WF_CHECK_MEM(dst, plain, n);
            } else {
                WF_CHECK_EQ(rc, WF_DECOMPRESS_NEEDS_MORE_OUT);
                WF_CHECK_EQ(prod, 0);
            }
            wf_free(dst, cap);
        }
        /* Truncations. */
        if (it < 40) {
            for (size_t k = 1; k < cn; k++) {
                uint8_t *t = wf_dup(cc, k);
                size_t cs = wf_zstd_get_frame_content_size(t, k);
                WF_CHECK(cs == n || cs == ZSTD_CONTENTSIZE_ERROR);
                uint8_t *dst = wf_alloc(n + 8);
                size_t prod = 0;
                WF_CHECK_EQ(wf_zstd_decompress_into(t, k, dst, n + 8, &prod), WF_DECOMPRESS_BAD_INPUT);
                wf_free(dst, n + 8);
                wf_free(t, k);
            }
        }
        wf_free(cc, cn);
        free(plain);
    }
}

/* Build a frame header with Frame_Content_Size_Flag = 3 (8-byte FCS),
 * Single_Segment = 0, then one RLE block of `rle` bytes as the last
 * block. */
static size_t zstd_rle_frame(uint8_t *f, uint64_t fcs, uint32_t rle)
{
    size_t i = 0;
    f[i++] = 0x28; f[i++] = 0xb5; f[i++] = 0x2f; f[i++] = 0xfd;
    f[i++] = 0xc0;         /* FCS flag 3, no checksum, no dict */
    f[i++] = 0x00;         /* Window_Descriptor: 1 KiB */
    for (int k = 0; k < 8; k++) f[i++] = (uint8_t)(fcs >> (8 * k));
    uint32_t bh = 1u | (1u << 1) | (rle << 3); /* last, RLE, size */
    f[i++] = (uint8_t)bh; f[i++] = (uint8_t)(bh >> 8); f[i++] = (uint8_t)(bh >> 16);
    f[i++] = 'q';
    return i;
}

/* Untrusted frame headers: a 19-byte frame claiming 2^63 + 5 bytes.
 * Before the fix the size was passed through; Ring.hs's zstdIntoRing
 * converts it to a negative Int that passes its capacity check and hands
 * zstd a destination capacity far beyond the ring, and
 * zstdDecompressFromPtr asks GHC to allocate it. Claims within what the
 * frame could produce still pass. */
static void zstd_huge_declared_size(void)
{
    uint8_t f[32];
    static const uint64_t lies[] = { (1ull << 63) + 5, ~0ull - 2, 1ull << 40, 1ull << 32 };
    for (size_t k = 0; k < sizeof lies / sizeof lies[0]; k++) {
        size_t fn = zstd_rle_frame(f, lies[k], 1000);
        uint8_t *b = wf_dup(f, fn);
        WF_CHECK_EQ(wf_zstd_get_frame_content_size(b, fn), ZSTD_CONTENTSIZE_ERROR);
        uint8_t dst[2000];
        size_t prod = 0;
        WF_CHECK(wf_zstd_decompress_into(b, fn, dst, sizeof dst, &prod) != WF_DECOMPRESS_OK);
        wf_free(b, fn);
    }
    /* Honest RLE frame: 100000 bytes from 19, accepted and decoded. */
    size_t fn = zstd_rle_frame(f, 100000, 100000);
    f[5] = 0x40; /* window 128 KiB so the block fits */
    uint8_t *b = wf_dup(f, fn);
    WF_CHECK_EQ(wf_zstd_get_frame_content_size(b, fn), 100000);
    uint8_t *dst = wf_alloc(100000);
    size_t prod = 0;
    WF_CHECK_EQ(wf_zstd_decompress_into(b, fn, dst, 100000, &prod), WF_DECOMPRESS_OK);
    WF_CHECK_EQ(prod, 100000);
    WF_CHECK(dst[0] == 'q' && dst[99999] == 'q');
    wf_free(dst, 100000);
    wf_free(b, fn);
    /* Claim bigger than the block it carries, but plausible: passes the
     * header check, fails in decompression. */
    fn = zstd_rle_frame(f, 200000, 1000);
    b = wf_dup(f, fn);
    WF_CHECK_EQ(wf_zstd_get_frame_content_size(b, fn), 200000);
    dst = wf_alloc(200000);
    WF_CHECK_EQ(wf_zstd_decompress_into(b, fn, dst, 200000, &prod), WF_DECOMPRESS_BAD_INPUT);
    wf_free(dst, 200000);
    wf_free(b, fn);
}

/* A skippable frame first used to report content size 0, so the
 * Haskell allocated 0 bytes, got NEEDS_MORE_OUT and then failed with
 * "output too large" (0 * 2 = 0) for a valid input. */
static void zstd_skippable_and_multiframe(void)
{
    const char *msg = "kafka record batch payload kafka record batch payload";
    size_t n = strlen(msg);
    uint8_t c[256];
    size_t cn = ZSTD_compress(c, sizeof c, msg, n, 3);
    uint8_t f[600];
    size_t i = 0;
    f[i++] = 0x50; f[i++] = 0x2a; f[i++] = 0x4d; f[i++] = 0x18; /* skippable */
    f[i++] = 3; f[i++] = 0; f[i++] = 0; f[i++] = 0;
    f[i++] = 'a'; f[i++] = 'b'; f[i++] = 'c';
    memcpy(f + i, c, cn);
    i += cn;
    uint8_t *b = wf_dup(f, i);
    WF_CHECK_EQ(wf_zstd_get_frame_content_size(b, i), ZSTD_CONTENTSIZE_UNKNOWN);
    uint8_t *dst = wf_alloc(n);
    size_t prod = 0;
    WF_CHECK_EQ(wf_zstd_decompress_into(b, i, dst, n, &prod), WF_DECOMPRESS_OK);
    WF_CHECK_EQ(prod, n);
    WF_CHECK_MEM(dst, msg, n);
    wf_free(dst, n);
    wf_free(b, i);

    /* Two data frames: the header reports the first frame's size; the
     * Haskell's NEEDS_MORE_OUT fallback finds the total. */
    memcpy(f, c, cn);
    memcpy(f + cn, c, cn);
    b = wf_dup(f, 2 * cn);
    WF_CHECK_EQ(wf_zstd_get_frame_content_size(b, 2 * cn), n);
    dst = wf_alloc(2 * n);
    WF_CHECK_EQ(wf_zstd_decompress_into(b, 2 * cn, dst, n, &prod), WF_DECOMPRESS_NEEDS_MORE_OUT);
    WF_CHECK_EQ(wf_zstd_decompress_into(b, 2 * cn, dst, 2 * n, &prod), WF_DECOMPRESS_OK);
    WF_CHECK_EQ(prod, 2 * n);
    wf_free(dst, 2 * n);
    wf_free(b, 2 * cn);

    /* Streamed frame without a content size. */
    ZSTD_CStream *cs = ZSTD_createCStream();
    ZSTD_initCStream(cs, 3);
    ZSTD_inBuffer in = { msg, n, 0 };
    ZSTD_outBuffer out = { f, sizeof f, 0 };
    ZSTD_compressStream(cs, &out, &in);
    ZSTD_endStream(cs, &out);
    ZSTD_freeCStream(cs);
    b = wf_dup(f, out.pos);
    WF_CHECK_EQ(wf_zstd_get_frame_content_size(b, out.pos), ZSTD_CONTENTSIZE_UNKNOWN);
    dst = wf_alloc(n);
    WF_CHECK_EQ(wf_zstd_decompress_into(b, out.pos, dst, n, &prod), WF_DECOMPRESS_OK);
    WF_CHECK_MEM(dst, msg, n);
    wf_free(dst, n);
    wf_free(b, out.pos);

    static const uint8_t garbage[] = { 0x28, 0xb5, 0x2f, 0xfd, 0xff, 0xff, 0xff, 0xff, 0xff };
    b = wf_dup(garbage, sizeof garbage);
    WF_CHECK_EQ(wf_zstd_get_frame_content_size(b, sizeof garbage), ZSTD_CONTENTSIZE_ERROR);
    uint8_t d[64];
    WF_CHECK_EQ(wf_zstd_decompress_into(b, sizeof garbage, d, sizeof d, &prod), WF_DECOMPRESS_BAD_INPUT);
    WF_CHECK_EQ(wf_zstd_get_frame_content_size(b, 2), ZSTD_CONTENTSIZE_ERROR);
    /* out_produced may be NULL (the Haskell always passes one). */
    WF_CHECK_EQ(wf_zstd_decompress_into(b, sizeof garbage, d, sizeof d, NULL), WF_DECOMPRESS_BAD_INPUT);
    wf_free(b, sizeof garbage);
    WF_CHECK_EQ(wf_zstd_decompress_into(c, cn, f, sizeof f, NULL), WF_DECOMPRESS_OK);
    WF_CHECK_MEM(f, msg, n);
}

int main(void)
{
    WF_RUN(snappy_roundtrips);
    WF_RUN(snappy_huge_declared_length);
    WF_RUN(snappy_densest_stream_accepted);
    WF_RUN(snappy_empty_and_null);
    WF_RUN(lz4_roundtrips);
    WF_RUN(lz4_wrapper_high_ratio_not_truncated);
    WF_RUN(lz4_truncated_frames);
    WF_RUN(lz4_linked_blocks_growth);
    WF_RUN(lz4_edges);
    WF_RUN(gzip_roundtrips);
    WF_RUN(gzip_truncated_and_corrupt);
    WF_RUN(gzip_bomb_capped);
    WF_RUN(gzip_capacity_above_4gib);
    WF_RUN(zstd_roundtrips);
    WF_RUN(zstd_huge_declared_size);
    WF_RUN(zstd_skippable_and_multiframe);
    return wf_finish("test_compress");
}
