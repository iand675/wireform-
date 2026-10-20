/* End-to-end tests for wireform_openssl.c: contexts and knobs, handshakes
 * over a socketpair (ALPN, SNI, hostname verification, mTLS, TLS 1.2 and
 * 1.3), the direct read/write path, every return-code mapping (forced
 * through a fault-injecting BIO where a real socket cannot produce it),
 * and error-queue hygiene. */
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

/* tls: 0 = default (1.3), 12 = cap both sides at TLS 1.2. */
static void conn_ctxs(conn *k, int tls)
{
    memset(k, 0, sizeof *k);
    k->fds[0] = k->fds[1] = -1;
    k->cctx = wf_ssl_ctx_new_client(0);
    k->sctx = wf_ssl_ctx_new_server(id.cert, id.key);
    WF_CHECK(k->cctx && k->sctx);
    if (tls == 12) {
        SSL_CTX_set_max_proto_version(k->cctx, TLS1_2_VERSION);
        SSL_CTX_set_max_proto_version(k->sctx, TLS1_2_VERSION);
    }
}

static void conn_ssls(conn *k)
{
    tls_socketpair(k->fds);
    k->c = wf_ssl_new_for_fd(k->cctx, k->fds[0]);
    k->s = wf_ssl_new_for_fd(k->sctx, k->fds[1]);
    WF_CHECK(k->c && k->s);
}

static int conn_open(conn *k, int tls)
{
    conn_ctxs(k, tls);
    conn_ssls(k);
    return tls_handshake(k->c, k->s, NULL, NULL);
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

/* Read from `ssl` until `n` bytes arrived or a non-OK code; reads use
 * buffers of `chunk` bytes allocated exact-size, so a write past buf_len
 * faults. Returns bytes read. */
static size_t read_all(SSL *ssl, uint8_t *dst, size_t n, size_t chunk)
{
    size_t got = 0;
    int idle = 0;
    while (got < n && idle < 1000) {
        size_t want = n - got < chunk ? n - got : chunk;
        uint8_t *buf = wf_alloc(want);
        size_t out = 12345;
        int rc = wf_ssl_read_into(ssl, buf, want, &out);
        if (rc == WF_SSL_OK) {
            WF_CHECK(out > 0 && out <= want);
            memcpy(dst + got, buf, out);
            got += out;
            idle = 0;
        } else {
            WF_CHECK_EQ(rc, WF_SSL_WANT_RETRY);
            WF_CHECK_EQ(out, 0);
            idle++;
        }
        wf_free(buf, want);
        if (rc != WF_SSL_OK && !wf_ssl_want(rc)) break;
    }
    return got;
}

/* Write all of src from `w`, pumping `r` (the peer) into dst whenever the
 * socket is full, then read whatever remains. */
static void transfer(SSL *w, SSL *r, const uint8_t *src, size_t n, uint8_t *dst,
                     size_t chunk)
{
    size_t sent = 0, got = 0;
    int spins = 0;
    while (sent < n && spins < 100000) {
        size_t len = n - sent;
        size_t out = 999;
        int rc = wf_ssl_write_from(w, src + sent, len, &out);
        if (rc == WF_SSL_OK) {
            WF_CHECK_EQ(out, len);
            sent += out;
        } else {
            /* A full socket wants write (or read, mid-renegotiation);
             * either way the loop drains the peer and retries. */
            WF_CHECK(wf_ssl_want(rc));
            WF_CHECK_EQ(out, 0);
            /* Retry with the same (buf, len) after draining the peer. */
            uint8_t *buf = wf_alloc(chunk);
            size_t o = 0;
            int rr = wf_ssl_read_into(r, buf, chunk, &o);
            if (rr == WF_SSL_OK) {
                WF_CHECK(o <= chunk);
                memcpy(dst + got, buf, o);
                got += o;
            }
            wf_free(buf, chunk);
            spins++;
        }
    }
    WF_CHECK_EQ(sent, n);
    got += read_all(r, dst + got, n - got, chunk);
    WF_CHECK_EQ(got, n);
    WF_CHECK_MEM(dst, src, n);
}

/* ------------------------------------------------------------------ */
/* Fault-injecting BIO: replaces a connected SSL's rbio or wbio          */
/* ------------------------------------------------------------------ */

enum fault_mode {
    FAULT_PLAIN,     /* return -1, no retry flag, errno and queue untouched */
    FAULT_RETRY,     /* return -1 with the retry flag (would block) */
    FAULT_EOF,       /* return 0 (orderly EOF) */
    FAULT_ERRNO,     /* set errno = fault_arg, return -1 */
    FAULT_PUSH,      /* ERR_raise(fault_lib, fault_arg), return -1 */
    FAULT_RETRY_WRITE, /* return -1 with only the retry-WRITE flag: a
                        * read that must send before it can progress (a
                        * TLS 1.3 key update), or a blocked send. */
};
static enum fault_mode fault_mode;
static int fault_arg, fault_lib;

static int fault_io(BIO *b)
{
    BIO_clear_retry_flags(b);
    switch (fault_mode) {
    case FAULT_PLAIN: return -1;
    case FAULT_RETRY: BIO_set_retry_read(b); BIO_set_retry_write(b); return -1;
    case FAULT_RETRY_WRITE: BIO_set_retry_write(b); return -1;
    case FAULT_EOF: return 0;
    case FAULT_ERRNO: errno = fault_arg; return -1;
    case FAULT_PUSH:
        /* A real BIO sets errno with a system error; OpenSSL's record
         * layer then queues its own entry carrying that errno. */
        if (fault_lib == ERR_LIB_SYS) errno = fault_arg;
        ERR_raise(fault_lib, fault_arg);
        return -1;
    }
    return -1;
}

static int fault_read(BIO *b, char *buf, int len) { (void)buf; (void)len; return fault_io(b); }
static int fault_write(BIO *b, const char *buf, int len) { (void)buf; (void)len; return fault_io(b); }
static long fault_ctrl(BIO *b, int cmd, long num, void *ptr)
{
    (void)b; (void)num; (void)ptr;
    return cmd == BIO_CTRL_FLUSH ? 1 : 0;
}
static int fault_create(BIO *b) { BIO_set_init(b, 1); return 1; }

static BIO_METHOD *fault_method(void)
{
    static BIO_METHOD *m;
    if (!m) {
        m = BIO_meth_new(BIO_get_new_index() | BIO_TYPE_SOURCE_SINK, "wf-fault");
        BIO_meth_set_read(m, fault_read);
        BIO_meth_set_write(m, fault_write);
        BIO_meth_set_ctrl(m, fault_ctrl);
        BIO_meth_set_create(m, fault_create);
    }
    return m;
}

/* A handshaken pair whose server read side (or client write side) is the
 * fault BIO. */
static void conn_fault_rbio(conn *k)
{
    WF_CHECK_EQ(conn_open(k, 0), 0);
    SSL_set0_rbio(k->s, BIO_new(fault_method()));
}

static void conn_fault_wbio(conn *k)
{
    WF_CHECK_EQ(conn_open(k, 0), 0);
    SSL_set0_wbio(k->c, BIO_new(fault_method()));
}

static int read_with(conn *k, enum fault_mode mode, int lib, int arg, int stale_errno)
{
    fault_mode = mode;
    fault_lib = lib;
    fault_arg = arg;
    uint8_t buf[64];
    size_t out = 7;
    errno = stale_errno;
    int rc = wf_ssl_read_into(k->s, buf, sizeof buf, &out);
    WF_CHECK_EQ(out, 0);
    return rc;
}

static int write_with(conn *k, enum fault_mode mode, int lib, int arg, int stale_errno)
{
    fault_mode = mode;
    fault_lib = lib;
    fault_arg = arg;
    static const uint8_t msg[32] = "hello";
    size_t out = 7;
    errno = stale_errno;
    int rc = wf_ssl_write_from(k->c, msg, sizeof msg, &out);
    WF_CHECK_EQ(out, 0);
    return rc;
}

/* ------------------------------------------------------------------ */
/* Contexts and knobs                                                   */
/* ------------------------------------------------------------------ */

static void init_is_idempotent(void)
{
    wf_ssl_init();
    wf_ssl_init();
    WF_CHECK_EQ(ERR_peek_error(), 0);
}

static void client_ctx(void)
{
    for (int v = 0; v <= 1; v++) {
        SSL_CTX *c = wf_ssl_ctx_new_client(v);
        WF_CHECK(c != NULL);
        WF_CHECK_EQ(SSL_CTX_get_min_proto_version(c), TLS1_2_VERSION);
        WF_CHECK_EQ(SSL_CTX_get_verify_mode(c), v ? SSL_VERIFY_PEER : SSL_VERIFY_NONE);
        wf_ssl_ctx_free(c);
    }
    wf_ssl_ctx_free(NULL);
    ERR_clear_error();
}

static void server_ctx(void)
{
    SSL_CTX *s = wf_ssl_ctx_new_server(id.cert, id.key);
    WF_CHECK(s != NULL);
    WF_CHECK_EQ(SSL_CTX_get_min_proto_version(s), TLS1_2_VERSION);
    wf_ssl_ctx_free(s);

    const char *bad[][2] = {
        { "/nonexistent/wf-cert.pem", id.key },
        { id.cert, "/nonexistent/wf-key.pem" },
        { id.junk, id.key },
        { id.cert, id.junk },
        { id.cert, id.other },  /* key does not match the certificate */
        { id.key, id.key },
        { id.cert, id.cert },
    };
    for (size_t i = 0; i < sizeof bad / sizeof bad[0]; i++) {
        WF_CHECK(wf_ssl_ctx_new_server(bad[i][0], bad[i][1]) == NULL);
        /* And the failure is reported through wf_ssl_last_error. */
        char buf[512];
        size_t n = wf_ssl_last_error(buf, sizeof buf);
        WF_CHECK(n > 0);
        WF_CHECK_EQ(n, strlen(buf));
        WF_CHECK_EQ(ERR_peek_error(), 0);
        ERR_clear_error();
    }
}

static void ctx_knobs(void)
{
    SSL_CTX *c = wf_ssl_ctx_new_client(0);

    WF_CHECK_EQ(wf_ssl_ctx_load_ca_bundle(NULL, id.cert), WF_SSL_FATAL);
    WF_CHECK_EQ(wf_ssl_ctx_load_ca_bundle(c, NULL), WF_SSL_FATAL);
    WF_CHECK_EQ(wf_ssl_ctx_load_ca_bundle(c, "/nonexistent/ca.pem"), WF_SSL_FATAL);
    WF_CHECK_EQ(wf_ssl_ctx_load_ca_bundle(c, id.junk), WF_SSL_FATAL);
    WF_CHECK_EQ(wf_ssl_ctx_load_ca_bundle(c, id.cert), WF_SSL_OK);
    ERR_clear_error();

    WF_CHECK_EQ(wf_ssl_ctx_use_client_cert(NULL, id.cert, id.key), WF_SSL_FATAL);
    WF_CHECK_EQ(wf_ssl_ctx_use_client_cert(c, NULL, id.key), WF_SSL_FATAL);
    WF_CHECK_EQ(wf_ssl_ctx_use_client_cert(c, id.cert, NULL), WF_SSL_FATAL);
    WF_CHECK_EQ(wf_ssl_ctx_use_client_cert(c, "/nonexistent", id.key), WF_SSL_FATAL);
    WF_CHECK_EQ(wf_ssl_ctx_use_client_cert(c, id.cert, "/nonexistent"), WF_SSL_FATAL);
    WF_CHECK_EQ(wf_ssl_ctx_use_client_cert(c, id.cert, id.other), WF_SSL_FATAL);
    ERR_clear_error();
    SSL_CTX *c2 = wf_ssl_ctx_new_client(0);
    WF_CHECK_EQ(wf_ssl_ctx_use_client_cert(c2, id.cert, id.key), WF_SSL_OK);
    wf_ssl_ctx_free(c2);

    WF_CHECK_EQ(wf_ssl_ctx_set_min_proto(NULL, 13), WF_SSL_FATAL);
    int versions[] = { 13, 12, 0, 11, 14, -1, 1 << 30 };
    for (size_t i = 0; i < sizeof versions / sizeof versions[0]; i++) {
        WF_CHECK_EQ(wf_ssl_ctx_set_min_proto(c, versions[i]), WF_SSL_OK);
        WF_CHECK_EQ(SSL_CTX_get_min_proto_version(c),
                    versions[i] == 13 ? TLS1_3_VERSION : TLS1_2_VERSION);
    }

    WF_CHECK_EQ(wf_ssl_ctx_set_cipher_suites(NULL, "HIGH"), WF_SSL_FATAL);
    WF_CHECK_EQ(wf_ssl_ctx_set_cipher_suites(c, NULL), WF_SSL_FATAL);
    WF_CHECK_EQ(wf_ssl_ctx_set_cipher_suites(c, "NOT-A-CIPHER-AT-ALL"), WF_SSL_FATAL);
    WF_CHECK_EQ(wf_ssl_ctx_set_cipher_suites(c, "HIGH:!aNULL:!MD5"), WF_SSL_OK);
    ERR_clear_error();

    WF_CHECK_EQ(wf_ssl_ctx_set_alpn(c, (const unsigned char *)"\x02h2\x08http/1.1", 12), 0);
    WF_CHECK(wf_ssl_ctx_set_alpn(c, (const unsigned char *)"\x05h2", 3) != 0);
    WF_CHECK(wf_ssl_ctx_set_alpn(c, (const unsigned char *)"\x00h2", 3) != 0);
    WF_CHECK_EQ(wf_ssl_ctx_set_alpn(c, NULL, 0), 0);
    ERR_clear_error();

    /* Setting, replacing and clearing the server list is safe and frees
     * the previous copy (LeakSanitizer on Linux). */
    wf_ssl_ctx_set_alpn_select_server(c, (const unsigned char *)"\x02h2\0");
    wf_ssl_ctx_set_alpn_select_server(c, (const unsigned char *)"\x08http/1.1\0");
    wf_ssl_ctx_set_alpn_select_server(c, NULL);
    wf_ssl_ctx_set_alpn_select_server(c, (const unsigned char *)"\0");
    wf_ssl_ctx_free(c);
}

static void last_error_buffers(void)
{
    ERR_clear_error();
    char buf[16];
    WF_CHECK_EQ(wf_ssl_last_error(NULL, 10), 0);
    WF_CHECK_EQ(wf_ssl_last_error(buf, 0), 0);
    memset(buf, 'x', sizeof buf);
    WF_CHECK_EQ(wf_ssl_last_error(buf, sizeof buf), 0);
    WF_CHECK_EQ(buf[0], 0);

    for (size_t cap = 1; cap <= 300; cap += (cap < 40 ? 1 : 37)) {
        ERR_raise(ERR_LIB_SSL, SSL_R_UNEXPECTED_EOF_WHILE_READING);
        char *b = (char *)wf_alloc(cap);
        size_t n = wf_ssl_last_error(b, cap);
        WF_CHECK(n < cap);
        WF_CHECK_EQ(n, strlen(b));
        if (cap >= 120) WF_CHECK(strstr(b, "unexpected eof") != NULL);
        WF_CHECK_EQ(ERR_peek_error(), 0); /* drained */
        wf_free(b, cap);
    }
}

/* ------------------------------------------------------------------ */
/* Handshakes                                                           */
/* ------------------------------------------------------------------ */

static void alpn_negotiation(void)
{
    struct {
        const char *client;
        unsigned clen;
        const char *server; /* NUL-terminated list, or NULL for none */
        const char *want;   /* NULL: no protocol negotiated */
    } cases[] = {
        { "\x02h2\x08http/1.1", 12, "\x08http/1.1\x02h2", "http/1.1" },
        { "\x08http/1.1\x02h2", 12, "\x02h2\x08http/1.1", "h2" },
        { "\x02h2", 3, "\x08http/1.1\x02h2", "h2" },
        { "\x03h2c", 4, "\x02h2", NULL },          /* no prefix matching */
        { "\x02h2", 3, "\x03h2c", NULL },
        { "\x02h2", 3, "\x03h2c\x02h2", "h2" },
        { "\x03" "foo", 4, "\x02h2", NULL },
        { NULL, 0, "\x02h2", NULL },
        { "\x02h2", 3, NULL, NULL },
        { "\x02h2", 3, "", NULL },                  /* empty server list */
    };
    for (int tls = 0; tls <= 12; tls += 12) {
        for (size_t i = 0; i < sizeof cases / sizeof cases[0]; i++) {
            conn k;
            conn_ctxs(&k, tls);
            if (cases[i].client)
                WF_CHECK_EQ(wf_ssl_ctx_set_alpn(k.cctx, (const unsigned char *)cases[i].client,
                                                cases[i].clen), 0);
            if (cases[i].server)
                wf_ssl_ctx_set_alpn_select_server(k.sctx, (const unsigned char *)cases[i].server);
            conn_ssls(&k);
            WF_CHECK_EQ(tls_handshake(k.c, k.s, NULL, NULL), 0);
            WF_CHECK_EQ(SSL_version(k.c), tls == 12 ? TLS1_2_VERSION : TLS1_3_VERSION);
            for (int side = 0; side < 2; side++) {
                SSL *ssl = side ? k.s : k.c;
                const unsigned char *p = NULL;
                unsigned plen = 0;
                int rc = wf_ssl_get_alpn(ssl, &p, &plen);
                if (cases[i].want) {
                    WF_CHECK_EQ(rc, 0);
                    WF_CHECK_EQ(plen, strlen(cases[i].want));
                    if (p) WF_CHECK_MEM(p, cases[i].want, plen);
                    WF_CHECK_EQ(wf_ssl_get_alpn(ssl, NULL, NULL), 0);
                } else {
                    WF_CHECK_EQ(rc, -1);
                    WF_CHECK(p == NULL);
                }
            }
            conn_close(&k);
        }
    }
}

/* The server list is copied into the SSL_CTX: the caller's buffer may be
 * freed straight after the call (Haskell's withPinnedBs buffer is
 * GC-managed and is not kept alive). Before the fix the callback read the
 * freed buffer during the handshake: ASan heap-use-after-free. */
static void alpn_server_list_is_copied(void)
{
    conn k;
    conn_ctxs(&k, 0);
    WF_CHECK_EQ(wf_ssl_ctx_set_alpn(k.cctx, (const unsigned char *)"\x02h2", 3), 0);
    size_t n = 4;
    unsigned char *list = malloc(n);
    memcpy(list, "\x02h2\0", n);
    wf_ssl_ctx_set_alpn_select_server(k.sctx, list);
    memset(list, 0x02, n); /* would still parse as a list, wrongly */
    free(list);
    conn_ssls(&k);
    WF_CHECK_EQ(tls_handshake(k.c, k.s, NULL, NULL), 0);
    const unsigned char *p = NULL;
    unsigned plen = 0;
    WF_CHECK_EQ(wf_ssl_get_alpn(k.c, &p, &plen), 0);
    WF_CHECK_EQ(plen, 2);
    if (p) WF_CHECK_MEM(p, "h2", 2);
    conn_close(&k);
}

static void sni_reaches_server(void)
{
    conn k;
    conn_ctxs(&k, 0);
    conn_ssls(&k);
    WF_CHECK_EQ(wf_ssl_set_sni(k.c, "service.example.test"), 0);
    WF_CHECK_EQ(tls_handshake(k.c, k.s, NULL, NULL), 0);
    const char *name = SSL_get_servername(k.s, TLSEXT_NAMETYPE_host_name);
    WF_CHECK(name && strcmp(name, "service.example.test") == 0);
    conn_close(&k);

    /* An over-long name is refused (RFC 6066: 255 bytes max). */
    conn_ctxs(&k, 0);
    conn_ssls(&k);
    char longname[300];
    memset(longname, 'a', sizeof longname - 1);
    longname[sizeof longname - 1] = 0;
    WF_CHECK_EQ(wf_ssl_set_sni(k.c, longname), WF_SSL_FATAL);
    conn_close(&k);
}

static void verified_connections(void)
{
    struct {
        int load_ca;
        const char *host; /* NULL: no hostname pin */
        int ok;
    } cases[] = {
        { 1, "localhost", 1 },
        { 1, "127.0.0.1", 1 },   /* IP SAN; set1_host matches it as a name */
        { 1, NULL, 1 },
        { 1, "wrong.example", 0 },
        { 1, "sub.localhost", 0 },
        { 0, "localhost", 0 },   /* no trust anchor for the self-signed cert */
    };
    for (size_t i = 0; i < sizeof cases / sizeof cases[0]; i++) {
        conn k;
        memset(&k, 0, sizeof k);
        k.cctx = wf_ssl_ctx_new_client(1);
        k.sctx = wf_ssl_ctx_new_server(id.cert, id.key);
        if (cases[i].load_ca) WF_CHECK_EQ(wf_ssl_ctx_load_ca_bundle(k.cctx, id.cert), WF_SSL_OK);
        conn_ssls(&k);
        if (cases[i].host) WF_CHECK_EQ(wf_ssl_set_verify_hostname(k.c, cases[i].host), WF_SSL_OK);
        int crc = 0, src = 0;
        int rc = tls_handshake(k.c, k.s, &crc, &src);
        if (cases[i].ok) {
            WF_CHECK_EQ(rc, 0);
            WF_CHECK_EQ(SSL_get_verify_result(k.c), X509_V_OK);
        } else {
            WF_CHECK_EQ(crc, WF_SSL_FATAL);
            char buf[512];
            WF_CHECK(wf_ssl_last_error(buf, sizeof buf) > 0);
        }
        conn_close(&k);
    }
}

/* Mutual TLS: the server demands a client certificate signed by our CA. */
static void mutual_tls(void)
{
    for (int present = 0; present <= 1; present++) {
        conn k;
        conn_ctxs(&k, 0);
        WF_CHECK_EQ(wf_ssl_ctx_load_ca_bundle(k.sctx, id.cert), WF_SSL_OK);
        SSL_CTX_set_verify(k.sctx, SSL_VERIFY_PEER | SSL_VERIFY_FAIL_IF_NO_PEER_CERT, NULL);
        if (present) WF_CHECK_EQ(wf_ssl_ctx_use_client_cert(k.cctx, id.cert, id.key), WF_SSL_OK);
        conn_ssls(&k);
        int crc = 0, src = 0;
        int rc = tls_handshake(k.c, k.s, &crc, &src);
        if (present) {
            WF_CHECK_EQ(rc, 0);
            X509 *peer = SSL_get1_peer_certificate(k.s);
            WF_CHECK(peer != NULL);
            X509_free(peer);
        } else {
            WF_CHECK_EQ(src, WF_SSL_FATAL);
        }
        conn_close(&k);
    }
}

/* TLS 1.3-only server against a client that tops out at TLS 1.2. */
static void protocol_mismatch_is_fatal(void)
{
    conn k;
    conn_ctxs(&k, 0);
    WF_CHECK_EQ(wf_ssl_ctx_set_min_proto(k.sctx, 13), WF_SSL_OK);
    SSL_CTX_set_max_proto_version(k.cctx, TLS1_2_VERSION);
    conn_ssls(&k);
    int crc = 0, src = 0;
    tls_handshake(k.c, k.s, &crc, &src);
    WF_CHECK_EQ(src, WF_SSL_FATAL);
    /* The client sees the server's alert on its next step. */
    for (int i = 0; i < 10 && wf_ssl_want(crc); i++) crc = wf_ssl_connect(k.c);
    WF_CHECK(crc == WF_SSL_FATAL || crc == WF_SSL_EOF);
    conn_close(&k);
}

/* A peer that hangs up during the handshake. */
static void handshake_peer_gone(void)
{
    conn k;
    conn_ctxs(&k, 0);
    conn_ssls(&k);
    WF_CHECK_EQ(wf_ssl_connect(k.c), WF_SSL_WANT_RETRY); /* ClientHello sent */
    close(k.fds[0]);
    k.fds[0] = -1;
    int rs = wf_ssl_accept(k.s);
    /* The ClientHello is still queued, so the server may get as far as
     * sending its flight before it sees the hangup. */
    for (int i = 0; i < 10 && wf_ssl_want(rs); i++) rs = wf_ssl_accept(k.s);
    WF_CHECK(rs == WF_SSL_FATAL || rs == WF_SSL_EOF);
    conn_close(&k);

    conn_ctxs(&k, 0);
    conn_ssls(&k);
    close(k.fds[1]);
    k.fds[1] = -1;
    int rc = wf_ssl_connect(k.c);
    WF_CHECK(rc == WF_SSL_FATAL || rc == WF_SSL_EOF);
    conn_close(&k);

    /* Garbage instead of a ServerHello. */
    conn_ctxs(&k, 0);
    conn_ssls(&k);
    WF_CHECK_EQ(wf_ssl_connect(k.c), WF_SSL_WANT_RETRY);
    WF_CHECK(write(k.fds[1], "HTTP/1.1 400 Bad Request\r\n\r\n", 28) == 28);
    WF_CHECK_EQ(wf_ssl_connect(k.c), WF_SSL_FATAL);
    conn_close(&k);
}

/* ------------------------------------------------------------------ */
/* Data path                                                            */
/* ------------------------------------------------------------------ */

static void roundtrip_sizes(void)
{
    wf_rng rng = wf_rng_from_env();
    size_t sizes[] = { 1, 2, 15, 16, 17, 255, 4096, 16383, 16384, 16385, 65536, 200003 };
    size_t chunks[] = { 1, 7, 4096, 16384, 70000 };
    for (int tls = 0; tls <= 12; tls += 12) {
        conn k;
        WF_CHECK_EQ(conn_open(&k, tls), 0);
        for (size_t i = 0; i < sizeof sizes / sizeof sizes[0]; i++) {
            size_t n = sizes[i];
            size_t chunk = chunks[(i + (size_t)tls) % (sizeof chunks / sizeof chunks[0])];
            if (n > 20000 && chunk < 16) chunk = 4096; /* keep it quick */
            uint8_t *src = wf_alloc(n);
            uint8_t *dst = malloc(n);
            wf_rng_fill(&rng, src, n);
            transfer(k.c, k.s, src, n, dst, chunk);
            transfer(k.s, k.c, src, n, dst, chunk);
            free(dst);
            wf_free(src, n);
        }
        conn_close(&k);
    }
}

static void zero_length_and_null_out(void)
{
    conn k;
    WF_CHECK_EQ(conn_open(&k, 0), 0);
    size_t out = 99;
    uint8_t one = 0x5A;
    WF_CHECK_EQ(wf_ssl_read_into(k.s, &one, 0, &out), WF_SSL_OK);
    WF_CHECK_EQ(out, 0);
    out = 99;
    WF_CHECK_EQ(wf_ssl_write_from(k.c, &one, 0, &out), WF_SSL_OK);
    WF_CHECK_EQ(out, 0);
    WF_CHECK_EQ(wf_ssl_read_into(k.s, &one, 0, NULL), WF_SSL_OK);
    WF_CHECK_EQ(wf_ssl_write_from(k.c, &one, 1, NULL), WF_SSL_OK);
    uint8_t got = 0;
    int rc = WF_SSL_WANT_RETRY;
    for (int i = 0; i < 100 && wf_ssl_want(rc); i++)
        rc = wf_ssl_read_into(k.s, &got, 1, NULL);
    WF_CHECK_EQ(rc, WF_SSL_OK);
    WF_CHECK_EQ(got, 0x5A);
    conn_close(&k);
}

static void idle_read_wants_retry(void)
{
    for (int tls = 0; tls <= 12; tls += 12) {
        conn k;
        WF_CHECK_EQ(conn_open(&k, tls), 0);
        uint8_t buf[32];
        size_t out = 5;
        WF_CHECK_EQ(wf_ssl_read_into(k.s, buf, sizeof buf, &out), WF_SSL_WANT_RETRY);
        WF_CHECK_EQ(out, 0);
        WF_CHECK_EQ(wf_ssl_read_into(k.c, buf, sizeof buf, &out), WF_SSL_WANT_RETRY);
        conn_close(&k);
    }
}

static void eof_after_close_notify(void)
{
    for (int tls = 0; tls <= 12; tls += 12) {
        conn k;
        WF_CHECK_EQ(conn_open(&k, tls), 0);
        static const uint8_t msg[] = "last words";
        size_t out = 0;
        WF_CHECK_EQ(wf_ssl_write_from(k.c, msg, sizeof msg, &out), WF_SSL_OK);
        wf_ssl_shutdown(k.c);
        uint8_t buf[64];
        WF_CHECK_EQ(read_all(k.s, buf, sizeof msg, sizeof buf), sizeof msg);
        WF_CHECK_MEM(buf, msg, sizeof msg);
        WF_CHECK_EQ(wf_ssl_read_into(k.s, buf, sizeof buf, &out), WF_SSL_EOF);
        WF_CHECK_EQ(out, 0);
        WF_CHECK_EQ(wf_ssl_read_into(k.s, buf, sizeof buf, &out), WF_SSL_EOF);
        wf_ssl_shutdown(k.s);
        WF_CHECK_EQ(wf_ssl_read_into(k.c, buf, sizeof buf, &out), WF_SSL_EOF);
        wf_ssl_shutdown(k.s); /* repeated shutdown is harmless */
        WF_CHECK_EQ(ERR_peek_error(), 0);
        conn_close(&k);
    }
}

/* Peer closes the TCP stream without close_notify: OpenSSL 3 reports
 * "unexpected eof while reading", which the kernel maps to EOF. */
static void eof_without_close_notify(void)
{
    for (int tls = 0; tls <= 12; tls += 12) {
        conn k;
        WF_CHECK_EQ(conn_open(&k, tls), 0);
        close(k.fds[0]);
        k.fds[0] = -1;
        uint8_t buf[64];
        size_t out = 3;
        WF_CHECK_EQ(wf_ssl_read_into(k.s, buf, sizeof buf, &out), WF_SSL_EOF);
        WF_CHECK_EQ(out, 0);
        WF_CHECK_EQ(ERR_peek_error(), 0);
        conn_close(&k);
    }
}

/* Bytes that are not a valid record for this session are fatal and
 * reported through wf_ssl_last_error. */
static void corrupt_records_are_fatal(void)
{
    static const uint8_t rec[] = { 0x17, 0x03, 0x03, 0x00, 0x20,
        1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
        17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32 };
    static const uint8_t junk[] = "GET / HTTP/1.1\r\nHost: x\r\n\r\n";
    const uint8_t *inputs[] = { rec, junk };
    size_t lens[] = { sizeof rec, sizeof junk - 1 };
    for (int tls = 0; tls <= 12; tls += 12) {
        for (int j = 0; j < 2; j++) {
            conn k;
            WF_CHECK_EQ(conn_open(&k, tls), 0);
            WF_CHECK(write(k.fds[0], inputs[j], lens[j]) == (ssize_t)lens[j]);
            uint8_t buf[64];
            size_t out = 3;
            WF_CHECK_EQ(wf_ssl_read_into(k.s, buf, sizeof buf, &out), WF_SSL_FATAL);
            WF_CHECK_EQ(out, 0);
            char msg[512];
            WF_CHECK(wf_ssl_last_error(msg, sizeof msg) > 0);
            /* The session is dead: further writes fail too. */
            WF_CHECK_EQ(wf_ssl_write_from(k.s, buf, 8, &out), WF_SSL_FATAL);
            conn_close(&k);
        }
    }
}

/* A legitimate record with one ciphertext bit flipped in transit. */
static void bit_flip_in_transit_is_fatal(void)
{
    conn k;
    WF_CHECK_EQ(conn_open(&k, 0), 0);
    static const uint8_t msg[100] = "payload";
    size_t out = 0;
    WF_CHECK_EQ(wf_ssl_write_from(k.c, msg, sizeof msg, &out), WF_SSL_OK);
    uint8_t wire[512];
    ssize_t n = read(k.fds[1], wire, sizeof wire);
    WF_CHECK(n > 20);
    if (n > 20) {
        wire[n - 3] ^= 0x10;
        WF_CHECK(write(k.fds[0], wire, (size_t)n) == n);
    }
    uint8_t buf[128];
    WF_CHECK_EQ(wf_ssl_read_into(k.s, buf, sizeof buf, &out), WF_SSL_FATAL);
    conn_close(&k);
}

static void write_to_closed_peer_is_fatal(void)
{
    conn k;
    WF_CHECK_EQ(conn_open(&k, 0), 0);
    close(k.fds[1]);
    k.fds[1] = -1;
    static const uint8_t msg[64] = "x";
    size_t out = 9;
    WF_CHECK_EQ(wf_ssl_write_from(k.c, msg, sizeof msg, &out), WF_SSL_FATAL);
    WF_CHECK_EQ(out, 0);
    conn_close(&k);
}

/* Fill the socket until the write would block, then drain and retry the
 * same buffer: OpenSSL requires the identical (buf, len) on retry. */
static void write_backpressure(void)
{
    conn k;
    WF_CHECK_EQ(conn_open(&k, 0), 0);
    size_t n = 1 << 20;
    uint8_t *src = malloc(n);
    uint8_t *dst = malloc(n);
    wf_rng rng = wf_rng_from_env();
    wf_rng_fill(&rng, src, n);
    size_t out = 0;
    int rc = wf_ssl_write_from(k.c, src, n, &out);
    WF_CHECK_EQ(rc, WF_SSL_WANT_WRITE); /* 1 MiB cannot fit a socketpair */
    WF_CHECK_EQ(out, 0);
    transfer(k.c, k.s, src, n, dst, 16384);
    free(src);
    free(dst);
    conn_close(&k);
}

/* ------------------------------------------------------------------ */
/* Error-queue and errno hygiene (regressions)                          */
/* ------------------------------------------------------------------ */

/* GHC runs many connections on a few OS threads, which share OpenSSL's
 * per-thread error queue. An error left by an unrelated call (another
 * connection's failure, a failed cert load) made SSL_get_error() report
 * SSL_ERROR_SSL for a mere WANT_READ, so an idle read returned FATAL and
 * tore down a healthy connection; a stale ERR_LIB_SYS entry turned it
 * into a fake EOF. */
static void stale_error_queue_is_ignored(void)
{
    struct { int lib, reason; } stale[] = {
        { ERR_LIB_X509, X509_R_CERT_ALREADY_IN_HASH_TABLE },
        { ERR_LIB_SSL, SSL_R_UNEXPECTED_EOF_WHILE_READING },
        { ERR_LIB_SYS, ENOENT },
        { ERR_LIB_SYS, ECONNRESET },
    };
    for (size_t i = 0; i < sizeof stale / sizeof stale[0]; i++) {
        conn k;
        WF_CHECK_EQ(conn_open(&k, 0), 0);
        uint8_t buf[32];
        size_t out = 0;
        ERR_raise(stale[i].lib, stale[i].reason);
        WF_CHECK_EQ(wf_ssl_read_into(k.s, buf, sizeof buf, &out), WF_SSL_WANT_RETRY);

        /* Same for a write that would block. */
        size_t n = 1 << 20;
        uint8_t *big = calloc(1, n);
        ERR_raise(stale[i].lib, stale[i].reason);
        WF_CHECK_EQ(wf_ssl_write_from(k.c, big, n, &out), WF_SSL_WANT_WRITE);
        uint8_t *sink = malloc(n);
        transfer(k.c, k.s, big, n, sink, 16384);
        free(sink);
        free(big);
        conn_close(&k);

        /* And for a handshake step that has to wait for the peer. */
        conn_ctxs(&k, 0);
        conn_ssls(&k);
        ERR_raise(stale[i].lib, stale[i].reason);
        WF_CHECK_EQ(wf_ssl_connect(k.c), WF_SSL_WANT_RETRY);
        ERR_raise(stale[i].lib, stale[i].reason);
        WF_CHECK_EQ(wf_ssl_accept(k.s), WF_SSL_WANT_RETRY);
        ERR_raise(stale[i].lib, stale[i].reason);
        /* TLS 1.3: the server's first flight completes the client. */
        WF_CHECK_EQ(wf_ssl_connect(k.c), WF_SSL_OK);
        WF_CHECK_EQ(tls_handshake(k.c, k.s, NULL, NULL), 0);
        conn_close(&k);
    }
}

/* wf_ssl_shutdown discards what SSL_shutdown pushes (e.g. "shutdown while
 * in init"), so it cannot poison the next call on this OS thread. */
static void shutdown_leaves_queue_empty(void)
{
    wf_ssl_shutdown(NULL);
    wf_ssl_free(NULL);
    conn k;
    conn_ctxs(&k, 0);
    conn_ssls(&k);
    wf_ssl_shutdown(k.c); /* before any handshake */
    WF_CHECK_EQ(ERR_peek_error(), 0);
    conn_close(&k);
}

/* Return-code mapping for BIO failures a real socket rarely produces. */
static void bio_failure_mapping(void)
{
    conn k;

    /* Would-block from the BIO: retry. */
    conn_fault_rbio(&k);
    WF_CHECK_EQ(read_with(&k, FAULT_RETRY, 0, 0, 0), WF_SSL_WANT_RETRY);
    conn_close(&k);

    /* A read that must write first (TLS 1.3 key update) reports the
     * write direction even though the caller asked to read, so the
     * Haskell loop parks on writability; likewise a blocked send. */
    conn_fault_rbio(&k);
    WF_CHECK_EQ(read_with(&k, FAULT_RETRY_WRITE, 0, 0, 0), WF_SSL_WANT_WRITE);
    conn_close(&k);
    conn_fault_wbio(&k);
    WF_CHECK_EQ(write_with(&k, FAULT_RETRY_WRITE, 0, 0, 0), WF_SSL_WANT_WRITE);
    conn_close(&k);

    /* Orderly EOF from the BIO with no close_notify: EOF. */
    conn_fault_rbio(&k);
    WF_CHECK_EQ(read_with(&k, FAULT_EOF, 0, 0, 0), WF_SSL_EOF);
    conn_close(&k);

    /* A syscall-level errno from the BIO. */
    int retry_errnos[] = { EAGAIN, EWOULDBLOCK, EINTR };
    for (size_t i = 0; i < 3; i++) {
        conn_fault_rbio(&k);
        WF_CHECK_EQ(read_with(&k, FAULT_ERRNO, 0, retry_errnos[i], 0), WF_SSL_WANT_RETRY);
        conn_close(&k);
        conn_fault_wbio(&k);
        WF_CHECK_EQ(write_with(&k, FAULT_ERRNO, 0, retry_errnos[i], 0), WF_SSL_WANT_WRITE);
        conn_close(&k);
    }
    conn_fault_rbio(&k);
    WF_CHECK_EQ(read_with(&k, FAULT_ERRNO, 0, ECONNRESET, 0), WF_SSL_EOF);
    conn_close(&k);
    conn_fault_wbio(&k);
    WF_CHECK_EQ(write_with(&k, FAULT_ERRNO, 0, EPIPE, 0), WF_SSL_FATAL);
    conn_close(&k);

    /* errno left at EAGAIN by an earlier, unrelated call must not turn a
     * BIO failure that sets no errno into an endless WANT_RETRY: the
     * Haskell loop would park on readability (true at EOF) and spin.
     * OpenSSL 3's record layer clears errno itself before BIO I/O; the
     * kernel also does, so this holds on OpenSSL 1.1.1 too. */
    conn_fault_rbio(&k);
    WF_CHECK_EQ(read_with(&k, FAULT_PLAIN, 0, 0, EAGAIN), WF_SSL_EOF);
    conn_close(&k);
    conn_fault_wbio(&k);
    WF_CHECK_EQ(write_with(&k, FAULT_PLAIN, 0, 0, EAGAIN), WF_SSL_FATAL);
    conn_close(&k);

    /* System errors reported through the queue (ERR_LIB_SYS). */
    conn_fault_rbio(&k);
    WF_CHECK_EQ(read_with(&k, FAULT_PUSH, ERR_LIB_SYS, EAGAIN, 0), WF_SSL_WANT_RETRY);
    conn_close(&k);
    conn_fault_rbio(&k);
    WF_CHECK_EQ(read_with(&k, FAULT_PUSH, ERR_LIB_SYS, ECONNRESET, 0), WF_SSL_EOF);
    conn_close(&k);
    conn_fault_wbio(&k);
    WF_CHECK_EQ(write_with(&k, FAULT_PUSH, ERR_LIB_SYS, EINTR, 0), WF_SSL_WANT_WRITE);
    conn_close(&k);
    conn_fault_wbio(&k);
    WF_CHECK_EQ(write_with(&k, FAULT_PUSH, ERR_LIB_SYS, EPIPE, 0), WF_SSL_FATAL);
    conn_close(&k);

    /* The EOF reason codes count only from the SSL library: the same
     * number from another library is a real failure. */
    conn_fault_rbio(&k);
    WF_CHECK_EQ(read_with(&k, FAULT_PUSH, ERR_LIB_SSL, SSL_R_UNEXPECTED_EOF_WHILE_READING, 0),
                WF_SSL_EOF);
    conn_close(&k);
    conn_fault_rbio(&k);
    WF_CHECK_EQ(read_with(&k, FAULT_PUSH, ERR_LIB_SSL, SSL_R_SHUTDOWN_WHILE_IN_INIT, 0),
                WF_SSL_EOF);
    conn_close(&k);
    conn_fault_rbio(&k);
    WF_CHECK_EQ(read_with(&k, FAULT_PUSH, ERR_LIB_EVP, SSL_R_UNEXPECTED_EOF_WHILE_READING, 0),
                WF_SSL_FATAL);
    conn_close(&k);
    conn_fault_rbio(&k);
    WF_CHECK_EQ(read_with(&k, FAULT_PUSH, ERR_LIB_SSL, SSL_R_DECRYPTION_FAILED_OR_BAD_RECORD_MAC, 0),
                WF_SSL_FATAL);
    conn_close(&k);
    conn_fault_wbio(&k);
    WF_CHECK_EQ(write_with(&k, FAULT_PUSH, ERR_LIB_SSL, SSL_R_BAD_LENGTH, 0), WF_SSL_FATAL);
    conn_close(&k);
}

/* The same mapping during a handshake: a would-block surfaced as a
 * system error is a retry, any other system error is fatal. */
static void handshake_bio_failure_mapping(void)
{
    /* want = expectation for a failing read step, want_w = for a failing
     * write step: a BIO retry flag set during the client's ClientHello
     * write is a definite WANT_WRITE, an errno-only failure leaves the
     * direction unknown and the handshake waits on readability. */
    struct { enum fault_mode mode; int lib, arg, want, want_w; } cases[] = {
        { FAULT_RETRY, 0, 0, WF_SSL_WANT_RETRY, WF_SSL_WANT_WRITE },
        { FAULT_RETRY_WRITE, 0, 0, WF_SSL_WANT_WRITE, WF_SSL_WANT_WRITE },
        { FAULT_ERRNO, 0, EAGAIN, WF_SSL_WANT_RETRY, WF_SSL_WANT_RETRY },
        { FAULT_ERRNO, 0, EINTR, WF_SSL_WANT_RETRY, WF_SSL_WANT_RETRY },
        { FAULT_PUSH, ERR_LIB_SYS, EWOULDBLOCK, WF_SSL_WANT_RETRY, WF_SSL_WANT_RETRY },
        { FAULT_ERRNO, 0, ECONNRESET, WF_SSL_FATAL, WF_SSL_FATAL },
        { FAULT_PLAIN, 0, 0, WF_SSL_FATAL, WF_SSL_FATAL },
        { FAULT_EOF, 0, 0, WF_SSL_FATAL, WF_SSL_FATAL },
        /* not a system error */
        { FAULT_PUSH, ERR_LIB_EVP, EAGAIN, WF_SSL_FATAL, WF_SSL_FATAL },
    };
    for (size_t i = 0; i < sizeof cases / sizeof cases[0]; i++) {
        for (int side = 0; side < 2; side++) {
            conn k;
            conn_ctxs(&k, 0);
            conn_ssls(&k);
            fault_mode = cases[i].mode;
            fault_lib = cases[i].lib;
            fault_arg = cases[i].arg;
            int rc;
            if (side == 0) {
                /* Server: its read of the ClientHello fails. */
                WF_CHECK_EQ(wf_ssl_connect(k.c), WF_SSL_WANT_RETRY);
                SSL_set0_rbio(k.s, BIO_new(fault_method()));
                rc = wf_ssl_accept(k.s);
            } else {
                /* Client: its write of the ClientHello fails. */
                SSL_set0_wbio(k.c, BIO_new(fault_method()));
                rc = wf_ssl_connect(k.c);
            }
            WF_CHECK_EQ(rc, side == 0 ? cases[i].want : cases[i].want_w);
            conn_close(&k);
        }
    }
}

/* TLS 1.3 KeyUpdate with update_requested: the reader must answer with
 * its own KeyUpdate, so a read can need to WRITE. With the write side
 * blocked the read reports WF_SSL_WANT_WRITE, so the Haskell receive
 * path parks on writability; waiting for readability there would wait on
 * a peer that is waiting for this record (the stall this pins down).
 * Once the socket drains, data flows again under the new keys. */
static void key_update_read_wants_write(void)
{
    conn k;
    WF_CHECK_EQ(conn_open(&k, 0), 0);
    WF_CHECK_EQ(SSL_key_update(k.c, SSL_KEY_UPDATE_REQUESTED), 1);
    static const uint8_t msg[] = "after key update";
    size_t out = 0;
    WF_CHECK_EQ(wf_ssl_write_from(k.c, msg, sizeof msg, &out), WF_SSL_OK);
    BIO *real = SSL_get_wbio(k.s);
    BIO_up_ref(real);
    fault_mode = FAULT_RETRY;
    SSL_set0_wbio(k.s, BIO_new(fault_method()));
    uint8_t buf[64];
    int rc = wf_ssl_read_into(k.s, buf, sizeof buf, &out);
    /* OpenSSL may hand back the application data first and answer the
     * update later, so either outcome is valid here; when the read does
     * need to send, the code must say WRITE (bio_failure_mapping pins
     * that direction exactly). */
    WF_CHECK(rc == WF_SSL_OK || rc == WF_SSL_WANT_WRITE);
    SSL_set0_wbio(k.s, real);
    size_t got = rc == WF_SSL_OK ? out : 0;
    got += read_all(k.s, buf + got, sizeof msg - got, sizeof buf - got);
    WF_CHECK_EQ(got, sizeof msg);
    WF_CHECK_MEM(buf, msg, sizeof msg);
    /* The server's answering KeyUpdate reaches the client intact. */
    WF_CHECK_EQ(wf_ssl_write_from(k.s, msg, sizeof msg, &out), WF_SSL_OK);
    WF_CHECK_EQ(read_all(k.c, buf, sizeof msg, sizeof buf), sizeof msg);
    WF_CHECK_MEM(buf, msg, sizeof msg);
    conn_close(&k);
}

int main(void)
{
    tls_identity_make(&id);
    WF_RUN(init_is_idempotent);
    WF_RUN(client_ctx);
    WF_RUN(server_ctx);
    WF_RUN(ctx_knobs);
    WF_RUN(last_error_buffers);
    WF_RUN(alpn_negotiation);
    WF_RUN(alpn_server_list_is_copied);
    WF_RUN(sni_reaches_server);
    WF_RUN(verified_connections);
    WF_RUN(mutual_tls);
    WF_RUN(protocol_mismatch_is_fatal);
    WF_RUN(handshake_peer_gone);
    WF_RUN(roundtrip_sizes);
    WF_RUN(zero_length_and_null_out);
    WF_RUN(idle_read_wants_retry);
    WF_RUN(eof_after_close_notify);
    WF_RUN(eof_without_close_notify);
    WF_RUN(corrupt_records_are_fatal);
    WF_RUN(bit_flip_in_transit_is_fatal);
    WF_RUN(write_to_closed_peer_is_fatal);
    WF_RUN(write_backpressure);
    WF_RUN(stale_error_queue_is_ignored);
    WF_RUN(shutdown_leaves_queue_empty);
    WF_RUN(bio_failure_mapping);
    WF_RUN(handshake_bio_failure_mapping);
    WF_RUN(key_update_read_wants_write);
    tls_identity_remove(&id);
    return wf_finish("test_openssl");
}
