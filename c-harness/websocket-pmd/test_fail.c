/*
 * Failure-path tests for the permessage-deflate shim (wf_pmd.c): two
 * classes of error paths.
 *
 * 1. Injected failures (only in the "fail" build variant, which
 *    renames the shim's allocation and zlib entry points to
 *    failpoint-aware fakes; see common/wf_fail.h and failfns.h).
 *    These drive the out-of-memory and constructor-failure paths plus
 *    the zlib error codes the message loop maps to WF_PMD_OOM /
 *    WF_PMD_BAD_INPUT / WF_PMD_INIT_FAIL.
 * 2. Input-driven edge cases (every variant): a non-NULL zero-length
 *    source, a BFINAL block followed by four bytes that are not the
 *    RFC 7692 tail, and empty-message drains that discard the count.
 */
#include "harness.h"
#include "api.h"
#include "wf_fail.h"

static const uint8_t HELLO[] = "Hello";

/* A BFINAL = 1 block ("Hello") followed by whatever the caller likes:
 * the raw call form of the sec 7.2.3.4 message. */
static const uint8_t bfinal[] = { 0xf3, 0x48, 0xcd, 0xc9, 0xc9, 0x07, 0x00 };

/* ------------------------------------------------------------------ */
/* Input-driven edge cases (every variant)                             */
/* ------------------------------------------------------------------ */

/* pmd_set_input's second arm: a non-NULL source with zero length is a
 * new (empty) message, not the resume call, so the stream's restart
 * bookkeeping is reset. The deflate side emits the sec 7.2.3.6 empty
 * stored block like the (NULL, 0) form; the inflate side has no block
 * boundary and reports BAD_INPUT without reading the source. */
static void zero_length_source(void)
{
    uint8_t byte = 0xa5;
    uint8_t buf[64];
    size_t produced = 9;

    wf_pmd_stream *d = wf_pmd_deflate_new(15, 1, 8);
    WF_CHECK(d != NULL);
    WF_CHECK_EQ(wf_pmd_deflate(d, &byte, 0, buf, sizeof buf, &produced), WF_PMD_OK);
    WF_CHECK(produced >= 5);
    WF_CHECK(memcmp(buf + produced - 4, pmd_tail, 4) == 0);
    wf_pmd_free(d);

    wf_pmd_stream *i = wf_pmd_inflate_new(15);
    WF_CHECK(i != NULL);
    produced = 9;
    WF_CHECK_EQ(wf_pmd_inflate(i, &byte, 0, buf, sizeof buf, &produced), WF_PMD_BAD_INPUT);
    WF_CHECK_EQ(produced, 0);
    wf_pmd_free(i);
}

/* The converse contract violation: a NULL source with a positive
 * length (src may be NULL only with length 0). zlib refuses the stream
 * outright (next_in == NULL while avail_in != 0) and the shim reports
 * BAD_INPUT without touching the source or the destination. */
static void null_source_with_length(void)
{
    uint8_t buf[64];
    size_t produced = 9;

    wf_pmd_stream *d = wf_pmd_deflate_new(15, 1, 8);
    WF_CHECK(d != NULL);
    WF_CHECK_EQ(wf_pmd_deflate(d, NULL, 5, buf, sizeof buf, &produced), WF_PMD_BAD_INPUT);
    WF_CHECK_EQ(produced, 0);
    wf_pmd_free(d);

    wf_pmd_stream *i = wf_pmd_inflate_new(15);
    WF_CHECK(i != NULL);
    produced = 9;
    WF_CHECK_EQ(wf_pmd_inflate(i, NULL, 5, buf, sizeof buf, &produced), WF_PMD_BAD_INPUT);
    WF_CHECK_EQ(produced, 0);
    wf_pmd_free(i);
}

/* sec 7.2.3.4: after a BFINAL block the shim leniently strips a
 * trailing 00 00 FF FF. Four bytes that are NOT the tail are left in
 * the stream (they are the start of the next block set) and the window
 * restart still happens. Here they begin an incomplete stored block,
 * so the message is BAD_INPUT but "Hello" decoded. */
static void bfinal_non_tail_suffix(void)
{
    uint8_t msg[sizeof bfinal + 4];
    memcpy(msg, bfinal, sizeof bfinal);
    memcpy(msg + sizeof bfinal, "ABCD", 4);
    wf_pmd_stream *s = wf_pmd_inflate_new(15);
    WF_CHECK(s != NULL);
    uint8_t buf[64];
    size_t produced = 9;
    WF_CHECK_EQ(wf_pmd_inflate(s, msg, sizeof msg, buf, sizeof buf, &produced), WF_PMD_BAD_INPUT);
    WF_CHECK_EQ(produced, 5);
    WF_CHECK_MEM(buf, HELLO, 5);
    wf_pmd_free(s);
}

/* An empty message whose count the caller discards (out_produced may
 * be NULL everywhere in the API): the shim writes the sec 7.2.3.6
 * empty stored block itself when deflate() reports "nothing to do",
 * which is the normal case after a completed sync flush. */
static void empty_drain_null_out(void)
{
    wf_pmd_stream *d = wf_pmd_deflate_new(15, 1, 8);
    WF_CHECK(d != NULL);
    uint8_t buf[64];
    size_t produced = 0;
    WF_CHECK_EQ(wf_pmd_deflate(d, HELLO, 5, buf, sizeof buf, &produced), WF_PMD_OK);
    WF_CHECK(produced >= 5);
    /* Two empty messages with the count discarded. */
    WF_CHECK_EQ(wf_pmd_deflate(d, NULL, 0, buf, sizeof buf, NULL), WF_PMD_OK);
    WF_CHECK_EQ(wf_pmd_deflate(d, NULL, 0, buf, sizeof buf, NULL), WF_PMD_OK);
    /* And one counted: the same 5-byte empty block. */
    WF_CHECK_EQ(wf_pmd_deflate(d, NULL, 0, buf, sizeof buf, &produced), WF_PMD_OK);
    WF_CHECK_EQ(produced, 5);
    WF_CHECK_MEM(buf, "\x00\x00\x00\xff\xff", 5);
    wf_pmd_free(d);
}

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

/* wf_pmd_inflate_new / wf_pmd_deflate_new: allocator and zlib
 * constructor failures all return NULL (the Haskell throws
 * PmdInitFailed and frees the other stream). */
static void fail_constructors(void)
{
    arm("calloc");
    WF_CHECK(wf_pmd_inflate_new(15) == NULL);
    disarm("calloc");
    arm("calloc");
    WF_CHECK(wf_pmd_deflate_new(15, 1, 8) == NULL);
    disarm("calloc");

    arm("inflateInit2");
    WF_CHECK(wf_pmd_inflate_new(15) == NULL);
    disarm("inflateInit2");

    arm("deflateInit2");
    WF_CHECK(wf_pmd_deflate_new(15, 1, 8) == NULL);
    disarm("deflateInit2");
}

/* pmd_inflate_restart: the window-copy allocation failure reports
 * Z_MEM_ERROR, which aborts the restart and maps to WF_PMD_OOM. The
 * decoded output before the restart is kept. */
static void fail_restart_malloc(void)
{
    wf_pmd_stream *s = wf_pmd_inflate_new(15);
    WF_CHECK(s != NULL);
    uint8_t buf[64];
    size_t produced = 9;
    arm("malloc");
    WF_CHECK_EQ(wf_pmd_inflate(s, bfinal, sizeof bfinal, buf, sizeof buf, &produced),
                WF_PMD_OOM);
    disarm("malloc");
    WF_CHECK_EQ(produced, 5);
    WF_CHECK_MEM(buf, HELLO, 5);
    wf_pmd_free(s);
}

/* The restart's two window-copy steps: whichever zlib entry point
 * rejects the stream aborts the restart with BAD_INPUT (at most one
 * restart per message, so the message ends there). */
static void fail_restart_steps(void)
{
    for (int step = 0; step < 2; step++) {
        const char *fn = step == 0 ? "inflateGetDictionary" : "inflateReset";
        wf_pmd_stream *s = wf_pmd_inflate_new(15);
        WF_CHECK(s != NULL);
        uint8_t buf[64];
        size_t produced = 9;
        arm(fn);
        WF_CHECK_EQ(wf_pmd_inflate(s, bfinal, sizeof bfinal, buf, sizeof buf, &produced),
                    WF_PMD_BAD_INPUT);
        disarm(fn);
        wf_pmd_free(s);
    }
}

/* wf_pmd_reset_*: a reset that zlib rejects is WF_PMD_INIT_FAIL, and
 * the stream stays usable once the reset works again. */
static void fail_resets(void)
{
    wf_pmd_stream *i = wf_pmd_inflate_new(15);
    wf_pmd_stream *d = wf_pmd_deflate_new(15, 1, 8);
    WF_CHECK(i && d);
    arm("inflateReset");
    WF_CHECK_EQ(wf_pmd_reset_inflate(i), WF_PMD_INIT_FAIL);
    disarm("inflateReset");
    arm("deflateReset");
    WF_CHECK_EQ(wf_pmd_reset_deflate(d), WF_PMD_INIT_FAIL);
    disarm("deflateReset");
    WF_CHECK_EQ(wf_pmd_reset_inflate(i), WF_PMD_OK);
    WF_CHECK_EQ(wf_pmd_reset_deflate(d), WF_PMD_OK);
    wf_pmd_free(i);
    wf_pmd_free(d);
}

/* wf_pmd_deflate's error mapping: Z_MEM_ERROR is WF_PMD_OOM, any other
 * zlib error is WF_PMD_BAD_INPUT. */
static void fail_deflate_errors(void)
{
    for (int mode = 0; mode < 2; mode++) {
        const char *fn = mode == 0 ? "deflate_zmem" : "deflate_zstream";
        wf_pmd_stream *d = wf_pmd_deflate_new(15, 1, 8);
        WF_CHECK(d != NULL);
        uint8_t buf[64];
        size_t produced = 9;
        arm(fn);
        WF_CHECK_EQ(wf_pmd_deflate(d, HELLO, 5, buf, sizeof buf, &produced),
                    mode == 0 ? WF_PMD_OOM : WF_PMD_BAD_INPUT);
        disarm(fn);
        WF_CHECK_EQ(produced, 0);
        wf_pmd_free(d);
    }
}

#endif /* WF_FAIL_VARIANT */

int main(void)
{
    WF_RUN(zero_length_source);
    WF_RUN(null_source_with_length);
    WF_RUN(bfinal_non_tail_suffix);
    WF_RUN(empty_drain_null_out);
#ifdef WF_FAIL_VARIANT
    WF_RUN(fail_constructors);
    WF_RUN(fail_restart_malloc);
    WF_RUN(fail_restart_steps);
    WF_RUN(fail_resets);
    WF_RUN(fail_deflate_errors);
#endif
    return wf_finish("test_fail");
}
