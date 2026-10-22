/*
 * Failure-path tests for wireform_openssl.c: two classes of error
 * paths.
 *
 * 1. Injected failures (only in the "fail" build variant, which
 *    renames the kernel's OpenSSL entry points to failpoint-aware
 *    fakes; see common/wf_fail.h and failfns.h). These drive the
 *    SSL_CTX / SSL construction failures, the server-ALPN list copy
 *    guards, the verify-hostname guards, the private-key check, and
 *    the read/write error classification for code / queue pairings a
 *    fault BIO cannot produce.
 * 2. Input-driven edge cases (every variant): a NULL hostname for
 *    wf_ssl_set_verify_hostname, a cleared server ALPN list (the
 *    selection callback then runs with no list at all), and a NULL
 *    context for the ALPN setters.
 */
#include "harness.h"
#include "api.h"

static tls_identity id;

/* ------------------------------------------------------------------ */
/* Fixtures                                                             */
/* ------------------------------------------------------------------ */

typedef struct {
    SSL_CTX *cctx, *sctx;
    SSL *c, *s;
    int fds[2];
} conn;

static void conn_open(conn *k)
{
    memset(k, 0, sizeof *k);
    k->fds[0] = k->fds[1] = -1;
    k->cctx = wf_ssl_ctx_new_client(0);
    k->sctx = wf_ssl_ctx_new_server(id.cert, id.key);
    WF_CHECK(k->cctx && k->sctx);
    tls_socketpair(k->fds);
    k->c = wf_ssl_new_for_fd(k->cctx, k->fds[0]);
    k->s = wf_ssl_new_for_fd(k->sctx, k->fds[1]);
    WF_CHECK(k->c && k->s);
}

static void conn_close(conn *k)
{
    wf_ssl_free(k->c);
    wf_ssl_free(k->s);
    wf_ssl_ctx_free(k->cctx);
    wf_ssl_ctx_free(k->sctx);
    if (k->fds[0] >= 0) close(k->fds[0]);
    if (k->fds[1] >= 0) close(k->fds[1]);
    ERR_clear_error();
}

/* One handshake of a client offering "h2" against `sctx`; the
 * negotiated protocol must be `want` (NULL for none). The server
 * context belongs to the caller. */
static void alpn_expect(SSL_CTX *sctx, const char *want)
{
    SSL_CTX *cctx = wf_ssl_ctx_new_client(0);
    WF_CHECK(cctx != NULL);
    WF_CHECK_EQ(wf_ssl_ctx_set_alpn(cctx, (const unsigned char *)"\x02h2", 3), 0);
    conn k;
    memset(&k, 0, sizeof k);
    k.fds[0] = k.fds[1] = -1;
    k.cctx = cctx;
    k.sctx = sctx;
    tls_socketpair(k.fds);
    k.c = wf_ssl_new_for_fd(k.cctx, k.fds[0]);
    k.s = wf_ssl_new_for_fd(k.sctx, k.fds[1]);
    WF_CHECK(k.c && k.s);
    WF_CHECK_EQ(tls_handshake(k.c, k.s, NULL, NULL), 0);
    const unsigned char *p = NULL;
    unsigned n = 0;
    int rc = wf_ssl_get_alpn(k.c, &p, &n);
    if (want == NULL) {
        WF_CHECK_EQ(rc, -1);
        WF_CHECK(p == NULL);
    } else {
        WF_CHECK_EQ(rc, 0);
        WF_CHECK_EQ(n, strlen(want));
        if (p) WF_CHECK_MEM(p, want, n);
    }
    wf_ssl_free(k.c);
    wf_ssl_free(k.s);
    wf_ssl_ctx_free(k.cctx);
    close(k.fds[0]);
    close(k.fds[1]);
    ERR_clear_error();
}

/* ------------------------------------------------------------------ */
/* Input-driven edge cases (every variant)                             */
/* ------------------------------------------------------------------ */

/* wf_ssl_set_verify_hostname accepts a NULL hostname: the IP-literal
 * branch is skipped and the host expectation is cleared instead (the
 * pinning call also turns on peer verification, so the client needs
 * the identity as a trust anchor). */
static void verify_hostname_null(void)
{
    conn k;
    conn_open(&k);
    WF_CHECK_EQ(wf_ssl_ctx_load_ca_bundle(k.cctx, id.cert), WF_SSL_OK);
    WF_CHECK_EQ(wf_ssl_set_verify_hostname(k.c, NULL), WF_SSL_OK);
    WF_CHECK_EQ(tls_handshake(k.c, k.s, NULL, NULL), 0);
    conn_close(&k);
}

/* A cleared server ALPN list still installs the selection callback,
 * which then runs with no list and must not touch it (it selects
 * nothing). A NULL context is a no-op. */
static void alpn_no_list_noack(void)
{
    wf_ssl_ctx_set_alpn_select_server(NULL, (const unsigned char *)"\x02h2\0");
    SSL_CTX *sctx = wf_ssl_ctx_new_server(id.cert, id.key);
    WF_CHECK(sctx != NULL);
    wf_ssl_ctx_set_alpn_select_server(sctx, (const unsigned char *)"\x02h2\0");
    wf_ssl_ctx_set_alpn_select_server(sctx, NULL);
    alpn_expect(sctx, NULL);
    wf_ssl_ctx_free(sctx);
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

/* Both SSL_CTX constructors report NULL when OpenSSL cannot allocate
 * the context. */
static void fail_ctx_new(void)
{
    arm("SSL_CTX_new");
    WF_CHECK(wf_ssl_ctx_new_client(0) == NULL);
    disarm("SSL_CTX_new");
    arm("SSL_CTX_new");
    WF_CHECK(wf_ssl_ctx_new_server(id.cert, id.key) == NULL);
    disarm("SSL_CTX_new");
    ERR_clear_error();
}

/* wf_ssl_ctx_set_min_proto: OpenSSL refusing the version is FATAL. */
static void fail_set_min_proto(void)
{
    SSL_CTX *c = wf_ssl_ctx_new_client(0);
    WF_CHECK(c != NULL);
    arm("SSL_CTX_ctrl_set_min_proto");
    WF_CHECK_EQ(wf_ssl_ctx_set_min_proto(c, 13), WF_SSL_FATAL);
    disarm("SSL_CTX_ctrl_set_min_proto");
    WF_CHECK_EQ(wf_ssl_ctx_set_min_proto(c, 13), WF_SSL_OK);
    wf_ssl_ctx_free(c);
}

/* The private-key check (key_file_fake_ok: an OpenSSL that accepts a
 * key file without cross-checking it, as older ones did): the kernel's
 * SSL_CTX_check_private_key is what rejects the unmatched pair, in
 * both the server constructor and wf_ssl_ctx_use_client_cert. */
static void fail_check_private_key(void)
{
    arm("key_file_fake_ok");
    WF_CHECK(wf_ssl_ctx_new_server(id.cert, id.other) == NULL);
    disarm("key_file_fake_ok");

    SSL_CTX *c = wf_ssl_ctx_new_client(0);
    WF_CHECK(c != NULL);
    arm("key_file_fake_ok");
    WF_CHECK_EQ(wf_ssl_ctx_use_client_cert(c, id.cert, id.other), WF_SSL_FATAL);
    disarm("key_file_fake_ok");
    wf_ssl_ctx_free(c);
}

/* wf_ssl_ctx_set_alpn_select_server's initialization / allocation
 * guards: every failure is a silent no-op that keeps the previous list
 * (here: none), so a later handshake negotiates nothing. Must run
 * before any real one-time initialization (see main). */
static void fail_alpn_index_init(void)
{
    /* The one-time index init failing outright. */
    SSL_CTX *s1 = wf_ssl_ctx_new_server(id.cert, id.key);
    WF_CHECK(s1 != NULL);
    arm("run_once_fail");
    wf_ssl_ctx_set_alpn_select_server(s1, (const unsigned char *)"\x02h2\0");
    disarm("run_once_fail");
    alpn_expect(s1, NULL);
    wf_ssl_ctx_free(s1);

    /* The init reporting success without having run, so the index is
     * still unset. */
    SSL_CTX *s2 = wf_ssl_ctx_new_server(id.cert, id.key);
    WF_CHECK(s2 != NULL);
    arm("run_once_skip");
    wf_ssl_ctx_set_alpn_select_server(s2, (const unsigned char *)"\x02h2\0");
    disarm("run_once_skip");
    alpn_expect(s2, NULL);
    wf_ssl_ctx_free(s2);

    /* The list copy failing to allocate. */
    SSL_CTX *s3 = wf_ssl_ctx_new_server(id.cert, id.key);
    WF_CHECK(s3 != NULL);
    arm("CRYPTO_memdup");
    wf_ssl_ctx_set_alpn_select_server(s3, (const unsigned char *)"\x02h2\0");
    disarm("CRYPTO_memdup");
    alpn_expect(s3, NULL);
    wf_ssl_ctx_free(s3);

    /* Installing the copy in the context failing: the copy is freed
     * and the previous list stays. Afterwards a real call still works,
     * so the one-time init was left pending as intended. */
    SSL_CTX *s4 = wf_ssl_ctx_new_server(id.cert, id.key);
    WF_CHECK(s4 != NULL);
    arm("SSL_CTX_set_ex_data");
    wf_ssl_ctx_set_alpn_select_server(s4, (const unsigned char *)"\x02h2\0");
    disarm("SSL_CTX_set_ex_data");
    alpn_expect(s4, NULL);
    wf_ssl_ctx_set_alpn_select_server(s4, (const unsigned char *)"\x02h2\0");
    alpn_expect(s4, "h2");
    wf_ssl_ctx_free(s4);
}

/* wf_ssl_new_for_fd: SSL allocation failure and the fd bind failing
 * (the half-built SSL is freed) both report NULL. */
static void fail_new_for_fd(void)
{
    SSL_CTX *c = wf_ssl_ctx_new_client(0);
    WF_CHECK(c != NULL);
    int fds[2];
    tls_socketpair(fds);
    arm("SSL_new");
    WF_CHECK(wf_ssl_new_for_fd(c, fds[0]) == NULL);
    disarm("SSL_new");
    arm("SSL_set_fd");
    WF_CHECK(wf_ssl_new_for_fd(c, fds[0]) == NULL);
    disarm("SSL_set_fd");
    wf_ssl_ctx_free(c);
    close(fds[0]);
    close(fds[1]);
}

/* wf_ssl_set_verify_hostname's guards: no verify parameter, and the
 * host expectation failing to set (both the IP-literal and the DNS
 * name branch) are WF_SSL_FATAL. */
static void fail_verify_hostname(void)
{
    conn k;
    conn_open(&k);
    arm("SSL_get0_param");
    WF_CHECK_EQ(wf_ssl_set_verify_hostname(k.c, "localhost"), WF_SSL_FATAL);
    disarm("SSL_get0_param");
    arm("X509_VERIFY_PARAM_set1_host");
    WF_CHECK_EQ(wf_ssl_set_verify_hostname(k.c, "127.0.0.1"), WF_SSL_FATAL);
    disarm("X509_VERIFY_PARAM_set1_host");
    arm("X509_VERIFY_PARAM_set1_host");
    WF_CHECK_EQ(wf_ssl_set_verify_hostname(k.c, "localhost"), WF_SSL_FATAL);
    disarm("X509_VERIFY_PARAM_set1_host");
    conn_close(&k);
}

/* Read/write error classification with the code decoupled from the
 * error queue: an unclassifiable read failure is FATAL, a SYSCALL
 * verdict with a non-system queued error falls back to errno (0 here,
 * cleaned by the kernel) and reports EOF, one with a queued EAGAIN
 * retries, and a write that must read first parks on WF_SSL_WANT_RETRY
 * like the read side. */
static void fail_io_classification(void)
{
    conn k;
    conn_open(&k);
    WF_CHECK_EQ(tls_handshake(k.c, k.s, NULL, NULL), 0);
    uint8_t buf[64];
    size_t out = 7;

    arm("read_fail_none");
    WF_CHECK_EQ(wf_ssl_read_into(k.s, buf, sizeof buf, &out), WF_SSL_FATAL);
    disarm("read_fail_none");
    WF_CHECK_EQ(out, 0);

    arm("read_fail_sysev");
    WF_CHECK_EQ(wf_ssl_read_into(k.s, buf, sizeof buf, &out), WF_SSL_EOF);
    disarm("read_fail_sysev");
    WF_CHECK_EQ(out, 0);

    arm("read_fail_sysev_sys");
    WF_CHECK_EQ(wf_ssl_read_into(k.s, buf, sizeof buf, &out), WF_SSL_WANT_RETRY);
    disarm("read_fail_sysev_sys");
    WF_CHECK_EQ(out, 0);

    arm("write_want_read");
    WF_CHECK_EQ(wf_ssl_write_from(k.c, buf, sizeof buf, &out), WF_SSL_WANT_RETRY);
    disarm("write_want_read");
    WF_CHECK_EQ(out, 0);

    /* Oldest queued entry a library error (so SSL_get_error reports
     * SSL_ERROR_SSL), newest a transient system error (so the kernel
     * classifies the failure as a would-block): a write that would
     * block parks on writability. */
    arm("write_err_multi");
    WF_CHECK_EQ(wf_ssl_write_from(k.c, buf, sizeof buf, &out), WF_SSL_WANT_WRITE);
    disarm("write_err_multi");
    WF_CHECK_EQ(out, 0);

    conn_close(&k);
}

#endif /* WF_FAIL_VARIANT */

int main(void)
{
    tls_identity_make(&id);
#ifdef WF_FAIL_VARIANT
    /* Before anything runs the kernel's real CRYPTO_THREAD_run_once. */
    WF_RUN(fail_alpn_index_init);
#endif
    WF_RUN(verify_hostname_null);
    WF_RUN(alpn_no_list_noack);
#ifdef WF_FAIL_VARIANT
    WF_RUN(fail_ctx_new);
    WF_RUN(fail_set_min_proto);
    WF_RUN(fail_check_private_key);
    WF_RUN(fail_new_for_fd);
    WF_RUN(fail_verify_hostname);
    WF_RUN(fail_io_classification);
#endif
    tls_identity_remove(&id);
    return wf_finish("test_fail");
}
