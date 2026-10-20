/*
 * wireform-network/cbits/wireform_openssl.c, as
 * Wireform.Network.TLS.OpenSSL imports it (all `ccall`; connect, accept,
 * read_into and write_from are `safe`, the rest `unsafe`). The prototypes
 * come from wireform_openssl.h, which the Haskell imports name.
 *
 * Caller contract, from the Haskell call sites:
 *
 * - Every SSL_CTX* / SSL* argument is a live non-NULL pointer the Haskell
 *   side got from wf_ssl_ctx_new_* / wf_ssl_new_for_fd (checked against
 *   nullPtr before use). wf_ssl_ctx_free / wf_ssl_free accept NULL.
 * - Path and hostname arguments are NUL-terminated C strings
 *   (withCString, or `bs <> "\0"` + unsafeUseAsCString, so a hostname
 *   ByteString with an embedded NUL is silently truncated there).
 * - wf_ssl_ctx_set_alpn: (encodeAlpn protos) pointer + length; OpenSSL
 *   validates the list and rejects a malformed one with nonzero.
 * - wf_ssl_ctx_set_alpn_select_server: encodeAlpn protos ++ "\0". Each
 *   protocol must be 1..255 bytes (a 0-length entry ends the list early; a
 *   longer one is mis-encoded by encodeAlpn's length byte and breaks the
 *   walk). The list is copied into the SSL_CTX, so the Haskell buffer
 *   (whose lifetime withPinnedBs does not actually extend) may die at once.
 * - wf_ssl_read_into(ssl, dst, want, &n): dst has room for `want` bytes
 *   (a magic-ring window); on WF_SSL_OK, n <= want bytes were written.
 *   `want` may be 0 (returns OK, n = 0).
 * - wf_ssl_write_from(ssl, src, len, &n): on WANT_RETRY the Haskell loop
 *   retries with the same (src, len), as OpenSSL requires. On OK n == len.
 * - Calls on one SSL* are serialised by an MVar; different connections
 *   run concurrently on arbitrary OS threads, which share OpenSSL's
 *   per-thread error queue: every I/O entry point must start from an
 *   empty queue and a clean errno or SSL_get_error() misreports.
 * - wf_ssl_last_error(buf, 512) right after a failed call.
 *
 * Return codes: WF_SSL_OK 0, WF_SSL_EOF -1, WF_SSL_WANT_RETRY -2,
 * WF_SSL_FATAL -3. (wf_ssl_get_alpn returns -1 for "none"; set_alpn
 * returns OpenSSL's 0 / nonzero.)
 */
#ifndef WF_NETWORK_OPENSSL_API_H
#define WF_NETWORK_OPENSSL_API_H

#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

#include <openssl/err.h>
#include <openssl/evp.h>
#include <openssl/pem.h>
#include <openssl/ssl.h>
#include <openssl/x509.h>
#include <openssl/x509v3.h>

#include "wireform_openssl.h"

static inline int tls_rc_valid(int rc)
{
    return rc == WF_SSL_OK || rc == WF_SSL_EOF || rc == WF_SSL_WANT_RETRY
           || rc == WF_SSL_FATAL;
}

/* ------------------------------------------------------------------ */
/* Identity: self-signed EC P-256 certificate written to temp files     */
/* ------------------------------------------------------------------ */

typedef struct {
    char cert[256];   /* PEM certificate (also usable as the CA bundle) */
    char key[256];    /* PEM private key matching cert */
    char other[256];  /* PEM private key NOT matching cert */
    char junk[256];   /* not PEM at all */
} tls_identity;

static inline EVP_PKEY *tls_ec_key(void)
{
    EVP_PKEY *k = EVP_EC_gen("P-256");
    if (!k) { ERR_print_errors_fp(stderr); abort(); }
    return k;
}

static inline void tls_tmpfile(char *path, size_t cap, const char *tag)
{
    const char *dir = getenv("TMPDIR");
    snprintf(path, cap, "%s/wf-tls-%s-XXXXXX", dir && *dir ? dir : "/tmp", tag);
    int fd = mkstemp(path);
    if (fd < 0) { perror("mkstemp"); abort(); }
    close(fd);
}

static inline void tls_write_key(const char *path, EVP_PKEY *k)
{
    FILE *f = fopen(path, "w");
    if (!f || !PEM_write_PrivateKey(f, k, NULL, NULL, 0, NULL, NULL)) abort();
    fclose(f);
}

/* CN=localhost, SAN DNS:localhost + IP:127.0.0.1, CA:TRUE so the cert is
 * its own trust anchor. */
static inline void tls_identity_make(tls_identity *id)
{
    EVP_PKEY *k = tls_ec_key();
    EVP_PKEY *k2 = tls_ec_key();
    X509 *x = X509_new();
    if (!x) abort();
    X509_set_version(x, 2);
    ASN1_INTEGER_set(X509_get_serialNumber(x), 1);
    X509_gmtime_adj(X509_getm_notBefore(x), -86400);
    X509_gmtime_adj(X509_getm_notAfter(x), 86400L * 365);
    X509_set_pubkey(x, k);
    X509_NAME *n = X509_get_subject_name(x);
    X509_NAME_add_entry_by_txt(n, "CN", MBSTRING_ASC,
                               (const unsigned char *)"localhost", -1, -1, 0);
    X509_set_issuer_name(x, n);
    X509V3_CTX v3;
    X509V3_set_ctx_nodb(&v3);
    X509V3_set_ctx(&v3, x, x, NULL, NULL, 0);
    const char *exts[][2] = {
        { "subjectAltName", "DNS:localhost,IP:127.0.0.1" },
        { "basicConstraints", "critical,CA:TRUE" },
        { "keyUsage", "critical,digitalSignature,keyCertSign" },
    };
    for (size_t i = 0; i < sizeof exts / sizeof exts[0]; i++) {
        X509_EXTENSION *e = X509V3_EXT_conf(NULL, &v3, exts[i][0], exts[i][1]);
        if (!e) abort();
        X509_add_ext(x, e, -1);
        X509_EXTENSION_free(e);
    }
    if (!X509_sign(x, k, EVP_sha256())) { ERR_print_errors_fp(stderr); abort(); }

    tls_tmpfile(id->cert, sizeof id->cert, "cert");
    tls_tmpfile(id->key, sizeof id->key, "key");
    tls_tmpfile(id->other, sizeof id->other, "other");
    tls_tmpfile(id->junk, sizeof id->junk, "junk");
    FILE *f = fopen(id->cert, "w");
    if (!f || !PEM_write_X509(f, x)) abort();
    fclose(f);
    tls_write_key(id->key, k);
    tls_write_key(id->other, k2);
    f = fopen(id->junk, "w");
    if (!f) abort();
    fputs("-----BEGIN CERTIFICATE-----\nnot base64 at all\n", f);
    fclose(f);
    X509_free(x);
    EVP_PKEY_free(k);
    EVP_PKEY_free(k2);
    ERR_clear_error();
}

static inline void tls_identity_remove(tls_identity *id)
{
    unlink(id->cert);
    unlink(id->key);
    unlink(id->other);
    unlink(id->junk);
}

/* ------------------------------------------------------------------ */
/* Connected non-blocking socket pairs                                  */
/* ------------------------------------------------------------------ */

static inline void tls_nonblock(int fd)
{
    int fl = fcntl(fd, F_GETFL);
    fcntl(fd, F_SETFL, fl | O_NONBLOCK);
#if defined(SO_NOSIGPIPE)
    int one = 1;
    setsockopt(fd, SOL_SOCKET, SO_NOSIGPIPE, &one, sizeof one);
#endif
}

/* fds[0] = client end, fds[1] = server end. */
static inline void tls_socketpair(int fds[2])
{
    signal(SIGPIPE, SIG_IGN);
    if (socketpair(AF_UNIX, SOCK_STREAM, 0, fds) != 0) {
        perror("socketpair");
        abort();
    }
    tls_nonblock(fds[0]);
    tls_nonblock(fds[1]);
}

/* Drive connect and accept alternately until both finish. Returns 0 when
 * both sides report OK; otherwise the first non-retry code seen, stored
 * per side in *crc / *src (either may be NULL). */
static inline int tls_handshake(SSL *c, SSL *s, int *crc, int *src)
{
    int rc = WF_SSL_WANT_RETRY, rs = WF_SSL_WANT_RETRY;
    for (int it = 0; it < 1000; it++) {
        if (rc == WF_SSL_WANT_RETRY) rc = wf_ssl_connect(c);
        if (rs == WF_SSL_WANT_RETRY) rs = wf_ssl_accept(s);
        if (rc != WF_SSL_WANT_RETRY && rs != WF_SSL_WANT_RETRY) break;
        if ((rc != WF_SSL_WANT_RETRY && rc != WF_SSL_OK)
            || (rs != WF_SSL_WANT_RETRY && rs != WF_SSL_OK))
            break;
    }
    if (crc) *crc = rc;
    if (src) *src = rs;
    if (rc == WF_SSL_OK && rs == WF_SSL_OK) return 0;
    return rc != WF_SSL_OK ? rc : rs;
}

/* Read everything currently queued on a non-blocking fd and drop it. */
static inline void tls_drain(int fd)
{
    char buf[4096];
    while (read(fd, buf, sizeof buf) > 0) {
    }
}

/* Bigger socket buffers so a fuzz input fits in flight. */
static inline void tls_bigbufs(int fd)
{
    int sz = 256 * 1024;
    setsockopt(fd, SOL_SOCKET, SO_SNDBUF, &sz, sizeof sz);
    setsockopt(fd, SOL_SOCKET, SO_RCVBUF, &sz, sizeof sz);
}

/* Write as much of [p, p + n) as the socket takes now; returns bytes. */
static inline size_t tls_push(int fd, const uint8_t *p, size_t n)
{
    size_t done = 0;
    while (done < n) {
        ssize_t w = write(fd, p + done, n - done);
        if (w <= 0) break;
        done += (size_t)w;
    }
    return done;
}

/* Process-lifetime contexts for the fuzz targets: built on first use
 * from a fresh identity whose files are removed straight after loading.
 * [0] negotiates TLS 1.3, [1] is capped at TLS 1.2. The client verifies
 * the server against the identity as CA and offers http/1.1, h2; the
 * server prefers h2, http/1.1. */
#define TLS_FUZZ_SERVER_ALPN ((const unsigned char *)"\x02h2\x08http/1.1")

static SSL_CTX *tls_fuzz_sctx[2], *tls_fuzz_cctx[2];

static inline void tls_fuzz_init(void)
{
    if (tls_fuzz_sctx[0]) return;
    tls_identity id;
    tls_identity_make(&id);
    for (int v = 0; v < 2; v++) {
        SSL_CTX *s = wf_ssl_ctx_new_server(id.cert, id.key);
        SSL_CTX *c = wf_ssl_ctx_new_client(1);
        if (!s || !c) abort();
        wf_ssl_ctx_set_alpn_select_server(s, TLS_FUZZ_SERVER_ALPN);
        if (wf_ssl_ctx_load_ca_bundle(c, id.cert) != WF_SSL_OK) abort();
        if (wf_ssl_ctx_set_alpn(c, (const unsigned char *)"\x08http/1.1\x02h2", 12)) abort();
        if (v) {
            SSL_CTX_set_max_proto_version(s, TLS1_2_VERSION);
            SSL_CTX_set_max_proto_version(c, TLS1_2_VERSION);
        }
        tls_fuzz_sctx[v] = s;
        tls_fuzz_cctx[v] = c;
    }
    tls_identity_remove(&id);
}

static inline SSL_CTX *tls_fuzz_server_ctx(int tls12)
{
    tls_fuzz_init();
    return tls_fuzz_sctx[tls12 ? 1 : 0];
}

static inline SSL_CTX *tls_fuzz_client_ctx(int tls12)
{
    tls_fuzz_init();
    return tls_fuzz_cctx[tls12 ? 1 : 0];
}

/* The negotiated ALPN, if any, is one both sides offered. */
static inline int tls_alpn_ok(SSL *ssl)
{
    const unsigned char *p = NULL;
    unsigned n = 0;
    int rc = wf_ssl_get_alpn(ssl, &p, &n);
    if (rc == -1) return p == NULL;
    if (rc != 0 || !p) return 0;
    return (n == 2 && memcmp(p, "h2", 2) == 0) || (n == 8 && memcmp(p, "http/1.1", 8) == 0);
}

/* wf_ssl_last_error into an exact-size buffer: NUL-terminated, length
 * consistent, queue drained. */
static inline int tls_last_error_ok(size_t cap)
{
    char *b = malloc(cap ? cap : 1);
    if (!b) abort();
    size_t n = wf_ssl_last_error(b, cap);
    int ok = cap == 0 ? n == 0 : (n < cap && n == strlen(b));
    ok = ok && (cap == 0 || ERR_peek_error() == 0);
    free(b);
    return ok;
}

#endif
