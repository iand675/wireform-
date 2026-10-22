/*
 * Failure-path tests for wireform-http1 http1_send.c: the send and
 * sendfile loops' peer-closed and interrupted-transfer arms.
 *
 * send() on a stream socket never returns 0 for a non-empty buffer, and
 * pread() only fails with EINTR while a signal handler runs, so these
 * arms are unreachable through the public API: no input drives them.
 * The "fail" build variant makes them testable by renaming send and
 * pread to failpoint-aware fakes (failfns.h/.c, common/wf_fail.h);
 * the other variants run this binary trivially (c-harness/README.md,
 * "Failure injection").
 *
 * The fakes' short mode really sends its half, so everything a shim
 * reports as sent is compared against what the peer actually received.
 */
#include "harness.h"
#include "api.h"
#include "wf_fail.h"

#ifdef WF_FAIL_VARIANT

static void arm(const char *name)
{
    char var[96];
    snprintf(var, sizeof var, "WF_FAIL_%s", name);
    setenv(var, "1", 1);
}

static void disarm(const char *name)
{
    char var[96];
    snprintf(var, sizeof var, "WF_FAIL_%s", name);
    unsetenv(var);
}

/* A send() that reports a closed peer (0 bytes) must stop the send-all
 * loop; the call reports exactly the bytes that went out before it, 0
 * when the peer closed before the first one. */
static void fail_send_all_peer_closed(void)
{
    uint8_t *src = wf_alloc(64);
    for (size_t i = 0; i < 64; i++) src[i] = (uint8_t)(i * 7);
    int sv[2];
    sx_pair(sv);
    sx_nonblock(sv[1], 1);
    uint8_t got[128];
    size_t have = 0;

    arm("send_zero");
    WF_CHECK_EQ(hs_http1_send_all(sv[0], src, 64), 0);
    disarm("send_zero");
    sx_drain(sv[1], got, &have, sizeof got);
    WF_CHECK_EQ(have, 0);

    /* Short first send, then closed: the partial count is reported and
     * exactly those bytes arrive. */
    arm("send_short");
    arm("send_zero");
    hs_ssize_t r = hs_http1_send_all(sv[0], src, 64);
    disarm("send_zero");
    disarm("send_short");
    WF_CHECK_EQ(r, 32);
    sx_drain(sv[1], got, &have, sizeof got);
    WF_CHECK_EQ(have, 32);
    WF_CHECK_MEM(got, src, 32);
    close(sv[0]);
    close(sv[1]);
    wf_free(src, 64);
}

/* send_more reports a closed peer as a plain 0-byte send; its caller
 * treats 0 and a short count alike and calls again. */
static void fail_send_more_peer_closed(void)
{
    uint8_t *src = wf_alloc(16);
    memset(src, 0x5a, 16);
    int sv[2];
    sx_pair(sv);
    arm("send_zero");
    WF_CHECK_EQ(hs_http1_send_more(sv[0], src, 16), 0);
    disarm("send_zero");
    close(sv[0]);
    close(sv[1]);
    wf_free(src, 16);
}

/* Interrupted before any progress: both calls retry internally, so the
 * caller sees the full transfer and the peer receives every byte. */
static void fail_eintr_retried(void)
{
    uint8_t *src = wf_alloc(48);
    for (size_t i = 0; i < 48; i++) src[i] = (uint8_t)i;
    int sv[2];
    sx_pair(sv);
    sx_nonblock(sv[1], 1);

    arm("send_eintr");
    WF_CHECK_EQ(hs_http1_send_more(sv[0], src, 48), 48);
    disarm("send_eintr");
    arm("send_eintr");
    WF_CHECK_EQ(hs_http1_send_all(sv[0], src, 48), 48);
    disarm("send_eintr");

    uint8_t got[128];
    size_t have = 0;
    sx_drain(sv[1], got, &have, sizeof got);
    WF_CHECK_EQ(have, 96);
    WF_CHECK_MEM(got, src, 48);
    WF_CHECK_MEM(got + 48, src, 48);
    close(sv[0]);
    close(sv[1]);
    wf_free(src, 48);
}

/* A failure after partial progress: the call returns the short count,
 * never re-sending those bytes, and the error surfaces on the next
 * call at zero progress. */
static void fail_send_all_partial_then_error(void)
{
    uint8_t *src = wf_alloc(64);
    for (size_t i = 0; i < 64; i++) src[i] = (uint8_t)(i ^ 0xa5);
    int sv[2];
    sx_pair(sv);
    sx_nonblock(sv[1], 1);
    uint8_t got[128];
    size_t have = 0;

    arm("send_short");
    arm("send_error");
    hs_ssize_t r = hs_http1_send_all(sv[0], src, 64);
    disarm("send_error");
    disarm("send_short");
    WF_CHECK_EQ(r, 32);
    sx_drain(sv[1], got, &have, sizeof got);
    WF_CHECK_EQ(have, 32);
    WF_CHECK_MEM(got, src, 32);

    arm("send_error");
    WF_CHECK_EQ(hs_http1_send_all(sv[0], src, 8), -EPIPE);
    disarm("send_error");
    close(sv[0]);
    close(sv[1]);
    wf_free(src, 64);
}

/* The portable sendfile fallback's send loop has the same peer-closed
 * arm: it stops at the bytes already out (0 when the peer closed
 * before the first one). */
static void fail_sendfile_fallback_peer_closed(void)
{
    uint8_t *src = wf_alloc(64);
    for (size_t i = 0; i < 64; i++) src[i] = (uint8_t)(i + 3);
    int f = sx_tempfile(src, 64);
    int sv[2];
    sx_pair(sv);
    sx_nonblock(sv[1], 1);
    uint8_t got[128];
    size_t have = 0;

    arm("send_zero");
    WF_CHECK_EQ(hs_http1_sendfile(sv[0], f, 0, 64), 0);
    disarm("send_zero");
    sx_drain(sv[1], got, &have, sizeof got);
    WF_CHECK_EQ(have, 0);

    arm("send_short");
    arm("send_zero");
    hs_ssize_t r = hs_http1_sendfile(sv[0], f, 0, 64);
    disarm("send_zero");
    disarm("send_short");
    WF_CHECK_EQ(r, 32);
    sx_drain(sv[1], got, &have, sizeof got);
    WF_CHECK_EQ(have, 32);
    WF_CHECK_MEM(got, src, 32);
    close(f);
    close(sv[0]);
    close(sv[1]);
    wf_free(src, 64);
}

/* An interrupted pread in the fallback is retried internally: the
 * transfer completes and the caller never sees EINTR. */
static void fail_sendfile_pread_eintr(void)
{
    uint8_t *src = wf_alloc(64);
    for (size_t i = 0; i < 64; i++) src[i] = (uint8_t)(255 - i);
    int f = sx_tempfile(src, 64);
    int sv[2];
    sx_pair(sv);
    sx_nonblock(sv[1], 1);

    arm("pread_eintr");
    WF_CHECK_EQ(hs_http1_sendfile(sv[0], f, 0, 64), 64);
    disarm("pread_eintr");

    uint8_t got[128];
    size_t have = 0;
    sx_drain(sv[1], got, &have, sizeof got);
    WF_CHECK_EQ(have, 64);
    WF_CHECK_MEM(got, src, 64);
    close(f);
    close(sv[0]);
    close(sv[1]);
    wf_free(src, 64);
}

#endif /* WF_FAIL_VARIANT */

int main(void)
{
    signal(SIGPIPE, SIG_IGN); /* what the GHC RTS does */
#ifdef WF_FAIL_VARIANT
    WF_RUN(fail_send_all_peer_closed);
    WF_RUN(fail_send_more_peer_closed);
    WF_RUN(fail_eintr_retried);
    WF_RUN(fail_send_all_partial_then_error);
    WF_RUN(fail_sendfile_fallback_peer_closed);
    WF_RUN(fail_sendfile_pread_eintr);
#endif
    return wf_finish("test_fail");
}
