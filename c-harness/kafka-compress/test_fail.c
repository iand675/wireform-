/*
 * Failure-path tests for the Kafka codec shims (snappy_ffi.c,
 * lz4_ffi.c, wireform_decompress.c): two classes of error paths.
 *
 * 1. Injected failures (only in the "fail" build variant, which
 *    renames the shims' allocation and library-constructor entry
 *    points to failpoint-aware fakes; see common/wf_fail.h and
 *    failfns.h). These drive the out-of-memory error paths.
 * 2. Input-driven error paths (every variant): oversized requests
 *    whose allocation cannot succeed, truncated or empty frames, and
 *    output buffers too small for the frame.
 */
#include "harness.h"
#include "api.h"
#include "wf_fail.h"

#include <stdlib.h>
#include <zstd.h>

/* ------------------------------------------------------------------ */
/* Injected failures (fail variant only)                               */
/* ------------------------------------------------------------------ */

#ifdef WF_FAIL_VARIANT

static void arm(const char *name)
{
    char var[96];
    snprintf(var, sizeof(var), "WF_FAIL_%s", name);
    setenv(var, "1", 1);
}

static void disarm(const char *name)
{
    char var[96];
    snprintf(var, sizeof(var), "WF_FAIL_%s", name);
    unsetenv(var);
}

static void fail_snappy_compress_paths(void)
{
    char *out = NULL;
    size_t out_len = 0;

    /* Allocation failure in snappy_compress_wrapper. */
    arm("malloc");
    WF_CHECK_EQ(snappy_compress_wrapper("hello", 5, &out, &out_len), (size_t)0);
    disarm("malloc");
    WF_CHECK(out == NULL);

    /* snappy_compress itself reporting an error. */
    out = NULL;
    out_len = 0;
    arm("snappy_compress");
    WF_CHECK_EQ(snappy_compress_wrapper("hello", 5, &out, &out_len), (size_t)0);
    disarm("snappy_compress");
    WF_CHECK(out == NULL);
}

static void fail_snappy_decompress_alloc(void)
{
    static const char plain[60000] = { 1 };
    char *c = NULL, *o = (char *)1;
    size_t cn = 0, on = 0;
    WF_CHECK(snappy_compress_wrapper(plain, sizeof plain, &c, &cn) > 0);

    arm("malloc");
    WF_CHECK_EQ(snappy_decompress_wrapper(c, cn, &o, &on), (size_t)0);
    disarm("malloc");
    WF_CHECK(o == NULL);
    free(c);
}

static void fail_lz4_compress_paths(void)
{
    char *out = NULL;
    size_t out_len = 0;

    arm("malloc");
    WF_CHECK_EQ(lz4_compress_wrapper("hello", 5, &out, &out_len), (size_t)0);
    disarm("malloc");
    WF_CHECK(out == NULL);

    out = NULL;
    out_len = 0;
    arm("LZ4F_compressFrame");
    WF_CHECK_EQ(lz4_compress_wrapper("hello", 5, &out, &out_len), (size_t)0);
    disarm("LZ4F_compressFrame");
    WF_CHECK(out == NULL);
}

static void fail_lz4_decompress_paths(void)
{
    static const char plain[8] = { 1, 2, 3, 4, 5, 6, 7, 8 };
    char *c = NULL, *o = NULL;
    size_t cn = 0, on = 0;
    WF_CHECK(lz4_compress_wrapper(plain, sizeof plain, &c, &cn) > 0);

    arm("malloc");
    WF_CHECK_EQ(lz4_decompress_wrapper(c, cn, &o, &on), (size_t)0);
    disarm("malloc");
    WF_CHECK(o == NULL);

    o = NULL;
    arm("LZ4F_createDecompressionContext");
    WF_CHECK_EQ(lz4_decompress_wrapper(c, cn, &o, &on), (size_t)0);
    disarm("LZ4F_createDecompressionContext");
    WF_CHECK(o == NULL);
    free(c);
}

static void fail_codec_init(void)
{
    uint8_t dst[64];
    size_t produced = 0, consumed = 0;
    const uint8_t junk[4] = { 0x04, 0x22, 0x4d, 0x18 };

    arm("inflateInit2");
    WF_CHECK_EQ(wf_gzip_inflate_into(junk, sizeof junk, dst, sizeof dst, &produced),
                WF_DECOMPRESS_INIT_FAIL);
    disarm("inflateInit2");

    arm("LZ4F_createDecompressionContext");
    WF_CHECK_EQ(wf_lz4f_decompress_into(junk, sizeof junk, dst, sizeof dst,
                                        &produced, &consumed),
                WF_DECOMPRESS_INIT_FAIL);
    disarm("LZ4F_createDecompressionContext");
}

/* A library that claims to fill an enormous output buffer without
 * writing it drives the decompress wrapper's growth guard: the buffer
 * doubles past SIZE_MAX / 2 and the wrapper stops instead of wrapping.
 * The stub allocator hands out a small real block for the huge
 * request; the fake decompressor never touches it. */
static void fail_lz4_growth_guard(void)
{
    char input[8] = { 0 };
    char *o = NULL;
    size_t on = 0;
    arm("malloc_stub");
    arm("LZ4F_decompress_full");
    WF_CHECK_EQ(lz4_decompress_wrapper(input, (size_t)1 << 61, &o, &on), (size_t)0);
    disarm("LZ4F_decompress_full");
    disarm("malloc_stub");
    WF_CHECK(o == NULL);
}

/* A decompressor stuck in place (consumes nothing, produces nothing)
 * must end the read loop rather than spin. */
static void fail_lz4_no_progress(void)
{
    static const char plain[8] = { 1 };
    char *c = NULL, *o = NULL;
    size_t cn = 0, on = 0;
    WF_CHECK(lz4_compress_wrapper(plain, sizeof plain, &c, &cn) > 0);

    arm("LZ4F_decompress_no_progress");
    WF_CHECK_EQ(lz4_decompress_wrapper(c, cn, &o, &on), (size_t)0);
    uint8_t dst[64];
    size_t produced = 0, consumed = 0;
    arm("LZ4F_decompress_no_progress");
    WF_CHECK_EQ(wf_lz4f_decompress_into((const uint8_t *)c, cn, dst, sizeof dst,
                                        &produced, &consumed),
                WF_DECOMPRESS_BAD_INPUT);
    disarm("LZ4F_decompress_no_progress");
    WF_CHECK(o == NULL);
    free(c);
}

/* A decompressor that claims one byte of output while consuming
 * nothing makes progress by the callers' accounting; the shifted real
 * decode that follows must still complete without corrupting memory. */
static void fail_lz4_partial_output(void)
{
    static const char plain[8] = { 1, 2, 3, 4, 5, 6, 7, 8 };
    char *c = NULL, *o = NULL;
    size_t cn = 0, on = 0;
    WF_CHECK(lz4_compress_wrapper(plain, sizeof plain, &c, &cn) > 0);

    arm("LZ4F_decompress_partial_out");
    size_t got = lz4_decompress_wrapper(c, cn, &o, &on);
    uint8_t dst[64];
    size_t produced = 0, consumed = 0;
    arm("LZ4F_decompress_partial_out");
    WF_CHECK_EQ(wf_lz4f_decompress_into((const uint8_t *)c, cn, dst, sizeof dst,
                                        &produced, &consumed),
                WF_DECOMPRESS_OK);
    disarm("LZ4F_decompress_partial_out");
    /* One claimed byte plus the real frame: outputs agree. */
    WF_CHECK_EQ(produced, got);
    WF_CHECK_EQ(got, sizeof plain + 1);
    free(c);
    free(o);
}

/* A decompressor that consumes one byte and produces nothing mid-frame
 * must end the read loop as a bad input, not spin or claim success. */
static void fail_lz4_partial_input(void)
{
    static const char plain[8] = { 1, 2, 3, 4, 5, 6, 7, 8 };
    char *c = NULL, *o = NULL;
    size_t cn = 0, on = 0;
    WF_CHECK(lz4_compress_wrapper(plain, sizeof plain, &c, &cn) > 0);

    arm("LZ4F_decompress_partial_in");
    WF_CHECK_EQ(lz4_decompress_wrapper(c, cn, &o, &on), (size_t)0);
    WF_CHECK(o == NULL);
    uint8_t dst[64];
    size_t produced = 0, consumed = 0;
    arm("LZ4F_decompress_partial_in");
    WF_CHECK_EQ(wf_lz4f_decompress_into((const uint8_t *)c, cn, dst, sizeof dst,
                                        &produced, &consumed),
                WF_DECOMPRESS_BAD_INPUT);
    disarm("LZ4F_decompress_partial_in");
    free(c);
}

/* The zstd classifier guards its error-name lookup against NULL even
 * though libzstd documents a never-NULL result. */
static void fail_zstd_null_name(void)
{
    static const char junk[32] = { (char)0xFF };
    uint8_t dst[64];
    size_t produced = 0;
    arm("ZSTD_getErrorName");
    WF_CHECK_EQ(wf_zstd_decompress_into((const uint8_t *)junk, sizeof junk,
                                        dst, sizeof dst, &produced),
                WF_DECOMPRESS_BAD_INPUT);
    disarm("ZSTD_getErrorName");
}

#endif /* WF_FAIL_VARIANT */

/* ------------------------------------------------------------------ */
/* Input-driven error paths (every variant)                            */
/* ------------------------------------------------------------------ */

/* Requests too large to allocate must fail cleanly before anything is
 * read or written. Skipped under the sanitizers, whose allocators
 * treat oversized requests as errors rather than returning NULL. */
static void huge_requests_fail(void)
{
#if !WF_SANITIZED_HEAP
    char in[8] = { 0 };
    char *out = (char *)1;
    size_t out_len = 1;

    WF_CHECK_EQ(snappy_compress_wrapper(in, (size_t)1 << 46, &out, &out_len),
                (size_t)0);
    WF_CHECK(out == NULL);

    out = (char *)1;
    out_len = 1;
    WF_CHECK_EQ(lz4_compress_wrapper(in, (size_t)1 << 46, &out, &out_len),
                (size_t)0);
    WF_CHECK(out == NULL);

    /* Both initial-capacity arms of lz4_decompress_wrapper: the
     * input_length * 4 arm and the clamped SIZE_MAX arm. */
    out = (char *)1;
    out_len = 1;
    WF_CHECK_EQ(lz4_decompress_wrapper(in, (size_t)1 << 44, &out, &out_len),
                (size_t)0);
    WF_CHECK(out == NULL);
    out = (char *)1;
    out_len = 1;
    WF_CHECK_EQ(lz4_decompress_wrapper(in, (size_t)1 << 63, &out, &out_len),
                (size_t)0);
    WF_CHECK(out == NULL);
#endif
}

/* Truncated, empty and under-runned frames classify as errors, never
 * as "buffer full" (which would make the Haskell caller loop). */
static void lz4_frame_error_paths(void)
{
    static const char plain[30000] = { 7 };
    char *c = NULL;
    size_t cn = 0;
    WF_CHECK(lz4_compress_wrapper(plain, sizeof plain, &c, &cn) > 32);
    uint8_t dst[40000];
    size_t produced = 0, consumed = 0;

    /* Truncated frame: input runs out mid-frame. */
    WF_CHECK_EQ(wf_lz4f_decompress_into((const uint8_t *)c, cn / 2, dst, sizeof dst,
                                        &produced, &consumed),
                WF_DECOMPRESS_BAD_INPUT);
    char *o = NULL;
    size_t on = 0;
    WF_CHECK_EQ(lz4_decompress_wrapper(c, cn / 2, &o, &on), (size_t)0);
    WF_CHECK(o == NULL);

    /* Empty input: nothing was ever started. */
    produced = 0;
    consumed = 0;
    WF_CHECK_EQ(wf_lz4f_decompress_into((const uint8_t *)c, 0, dst, sizeof dst,
                                        &produced, &consumed),
                WF_DECOMPRESS_BAD_INPUT);
    o = NULL;
    WF_CHECK_EQ(lz4_decompress_wrapper(c, 0, &o, &on), (size_t)0);

    /* Partial magic at every cut: still "input ran out", never a
     * buffer-full verdict. */
    for (size_t cut = 1; cut < 4; cut++) {
        produced = 0;
        consumed = 0;
        WF_CHECK_EQ(wf_lz4f_decompress_into((const uint8_t *)c, cut, dst, sizeof dst,
                                            &produced, &consumed),
                    WF_DECOMPRESS_BAD_INPUT);
    }

    /* Output too small for the frame: "buffer full", retryable. */
    produced = 0;
    consumed = 0;
    WF_CHECK_EQ(wf_lz4f_decompress_into((const uint8_t *)c, cn, dst, 8,
                                        &produced, &consumed),
                WF_DECOMPRESS_NEEDS_MORE_OUT);
    free(c);
}

/* A zstd frame larger than the destination reports NEEDS_MORE_OUT (so
 * the caller grows and retries) rather than a generic error. */
static void zstd_dst_too_small(void)
{
    static const char plain[1 << 20] = { 3 };
    size_t bound = ZSTD_compressBound(sizeof plain);
    uint8_t *frame = (uint8_t *)malloc(bound);
    WF_CHECK(frame != NULL);
    size_t flen = ZSTD_compress(frame, bound, plain, sizeof plain, 3);
    WF_CHECK(flen > 0);

    uint8_t small[64];
    size_t produced = 0;
    WF_CHECK_EQ(wf_zstd_decompress_into(frame, flen, small, sizeof small, &produced),
                WF_DECOMPRESS_NEEDS_MORE_OUT);

    /* A different zstd error is a plain bad input, not a grow-and-retry
     * verdict: corrupt the frame past its header. */
    frame[16] ^= 0xFF;
    frame[17] ^= 0xFF;
    produced = 0;
    WF_CHECK_EQ(wf_zstd_decompress_into(frame, flen, small, sizeof small, &produced),
                WF_DECOMPRESS_BAD_INPUT);
    free(frame);
}

int main(void)
{
#ifdef WF_FAIL_VARIANT
    WF_RUN(fail_snappy_compress_paths);
    WF_RUN(fail_snappy_decompress_alloc);
    WF_RUN(fail_lz4_compress_paths);
    WF_RUN(fail_lz4_decompress_paths);
    WF_RUN(fail_codec_init);
    WF_RUN(fail_lz4_growth_guard);
    WF_RUN(fail_lz4_no_progress);
    WF_RUN(fail_lz4_partial_output);
    WF_RUN(fail_lz4_partial_input);
    WF_RUN(fail_zstd_null_name);
#endif
    WF_RUN(huge_requests_fail);
    WF_RUN(lz4_frame_error_paths);
    WF_RUN(zstd_dst_too_small);
    return wf_finish("test_fail");
}
