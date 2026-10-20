/*
 * wireform_openssl.c
 *
 * Direct OpenSSL FFI for the wireform magic-ring transport.
 *
 * The motivating asymmetry: every compression codec we use exposes
 * a `void* dst, size_t cap` C API so the magic ring can host the
 * destination buffer.  The Haskell `tls` package does NOT (it only
 * yields decrypted plaintext as freshly-allocated `ByteString`s
 * via `recvData`).  OpenSSL's `SSL_read_ex` *does* — it decrypts
 * straight into a caller-supplied buffer.  That's what this
 * wrapper exposes, so a wireform `Wireform.Transport.Transport`
 * can plumb an OpenSSL TLS connection through `recvBuf`-style
 * direct-into-ring writes, the same way it plumbs a raw TCP socket.
 *
 * Surface kept deliberately small:
 *
 *   - process init  (wf_ssl_init)
 *   - client + server SSL_CTX construction with PEM cert / key load,
 *     ALPN selection, and verify mode toggling
 *   - per-connection: bind to an fd, do the handshake, query the
 *     negotiated ALPN protocol, read plaintext into a Ptr, write
 *     plaintext from a Ptr, shutdown + free.
 *
 * Each call returns 0 on success, a small negative sentinel on
 * "transport is closed" or "openssl error".  Error details aren't
 * marshalled across the FFI boundary; the Haskell side fetches a
 * formatted message via `wf_ssl_last_error` after a failed call.
 */

#include <openssl/ssl.h>
#include <openssl/err.h>
#include <openssl/sslerr.h>
#include <openssl/x509.h>
#include <openssl/x509v3.h>
#include <openssl/x509_vfy.h>

#include <stddef.h>
#include <stdint.h>
#include <errno.h>
#include <string.h>

#include "wireform_openssl.h"

/* ---------------------------------------------------------------- *
 *  Error-state hygiene                                              *
 * ---------------------------------------------------------------- *
 *
 * SSL_get_error() consults the calling thread's OpenSSL error queue
 * first and errno for SSL_ERROR_SYSCALL. GHC runs many connections
 * on a handful of OS threads, so both routinely hold leftovers from
 * an unrelated call (another connection's failure, a failed cert
 * load, an earlier EAGAIN). OpenSSL requires an empty queue before
 * each I/O call; without it an idle read on a healthy connection
 * came back FATAL (stale SSL error) or EOF (stale system error),
 * and a BIO failure that sets no errno looked like EAGAIN forever.
 * Every I/O entry point therefore starts from a clean slate, as
 * nginx and curl do before SSL_read / SSL_write. */
static void clear_error_state(void) {
    ERR_clear_error();
    errno = 0;
}

/* errno-style cause of a system-level failure: the queued system
 * error if the newest entry is one, else errno. */
static int sys_cause(void) {
    unsigned long e = ERR_peek_last_error();
    if (e != 0 && ERR_GET_LIB(e) == ERR_LIB_SYS) return ERR_GET_REASON(e);
    return errno;
}

static int is_transient(int e) {
    return e == EAGAIN || e == EWOULDBLOCK || e == EINTR;
}

/* A failure is a would-block retry when it is SSL_ERROR_SYSCALL with a
 * transient cause, or SSL_ERROR_SSL whose newest entry is a transient
 * system error (errno alone is not trusted for SSL_ERROR_SSL). */
static int is_transient_failure(int err) {
    if (err == SSL_ERROR_SYSCALL) return is_transient(sys_cause());
    if (err == SSL_ERROR_SSL) {
        unsigned long e = ERR_peek_last_error();
        return ERR_GET_LIB(e) == ERR_LIB_SYS && is_transient(ERR_GET_REASON(e));
    }
    return 0;
}

/* Shared by connect and accept: WANT_* and a would-block surfaced as
 * a system error are retries, everything else is fatal. */
static int map_handshake(SSL *ssl, int rc) {
    int err = SSL_get_error(ssl, rc);
    /* WANT_WRITE is kept distinct: a read that has to send first (a TLS
     * 1.3 key update) must park on writability, or it waits for a peer
     * that is waiting for it. */
    if (err == SSL_ERROR_WANT_WRITE) {
        return WF_SSL_WANT_WRITE;
    }
    if (err == SSL_ERROR_WANT_READ) {
        return WF_SSL_WANT_RETRY;
    }
    if (is_transient_failure(err)) {
        ERR_clear_error();
        return WF_SSL_WANT_RETRY;
    }
    return WF_SSL_FATAL;
}

/* ---------------------------------------------------------------- *
 *  One-time process init                                            *
 * ---------------------------------------------------------------- */

void wf_ssl_init(void) {
    /* OpenSSL 1.1+ auto-initialises on first use; SSL_load_error_strings
     * / OpenSSL_add_all_algorithms became no-ops.  We still call the
     * modern init explicitly so the load happens here rather than on
     * the first SSL_CTX_new call (deterministic startup cost). */
    OPENSSL_init_ssl(OPENSSL_INIT_LOAD_SSL_STRINGS
                       | OPENSSL_INIT_LOAD_CRYPTO_STRINGS,
                     NULL);
}

/* ---------------------------------------------------------------- *
 *  SSL_CTX construction                                             *
 * ---------------------------------------------------------------- */

SSL_CTX* wf_ssl_ctx_new_client(int verify_peer) {
    SSL_CTX *ctx = SSL_CTX_new(TLS_client_method());
    if (!ctx) return NULL;
    SSL_CTX_set_min_proto_version(ctx, TLS1_2_VERSION);
    if (verify_peer) {
        SSL_CTX_set_verify(ctx, SSL_VERIFY_PEER, NULL);
        SSL_CTX_set_default_verify_paths(ctx);
    } else {
        SSL_CTX_set_verify(ctx, SSL_VERIFY_NONE, NULL);
    }
    return ctx;
}

SSL_CTX* wf_ssl_ctx_new_server(const char *cert_path, const char *key_path) {
    SSL_CTX *ctx = SSL_CTX_new(TLS_server_method());
    if (!ctx) return NULL;
    SSL_CTX_set_min_proto_version(ctx, TLS1_2_VERSION);
    if (SSL_CTX_use_certificate_chain_file(ctx, cert_path) != 1) {
        SSL_CTX_free(ctx);
        return NULL;
    }
    if (SSL_CTX_use_PrivateKey_file(ctx, key_path, SSL_FILETYPE_PEM) != 1) {
        SSL_CTX_free(ctx);
        return NULL;
    }
    if (SSL_CTX_check_private_key(ctx) != 1) {
        SSL_CTX_free(ctx);
        return NULL;
    }
    return ctx;
}

void wf_ssl_ctx_free(SSL_CTX *ctx) {
    if (ctx) SSL_CTX_free(ctx);
}

/* Optional: load an explicit CA bundle (PEM) on top of the default
 * system trust store the client ctx already uses.  Callers that want
 * to /replace/ the system store should construct the ctx with
 * verify_peer=0 and then opt back into verification themselves. */
int wf_ssl_ctx_load_ca_bundle(SSL_CTX *ctx, const char *ca_path) {
    if (!ctx || !ca_path) return WF_SSL_FATAL;
    if (SSL_CTX_load_verify_locations(ctx, ca_path, NULL) != 1) {
        return WF_SSL_FATAL;
    }
    return WF_SSL_OK;
}

/* mTLS client identity.  Loads the cert chain + private key and
 * checks they match. */
int wf_ssl_ctx_use_client_cert(SSL_CTX *ctx,
                                const char *cert_path,
                                const char *key_path) {
    if (!ctx || !cert_path || !key_path) return WF_SSL_FATAL;
    if (SSL_CTX_use_certificate_chain_file(ctx, cert_path) != 1) return WF_SSL_FATAL;
    if (SSL_CTX_use_PrivateKey_file(ctx, key_path, SSL_FILETYPE_PEM) != 1) return WF_SSL_FATAL;
    if (SSL_CTX_check_private_key(ctx) != 1) return WF_SSL_FATAL;
    return WF_SSL_OK;
}

/* Override the minimum protocol version.  @version is 12 (TLS 1.2)
 * or 13 (TLS 1.3); anything else falls back to TLS 1.2. */
int wf_ssl_ctx_set_min_proto(SSL_CTX *ctx, int version) {
    if (!ctx) return WF_SSL_FATAL;
    int v = (version == 13) ? TLS1_3_VERSION : TLS1_2_VERSION;
    if (SSL_CTX_set_min_proto_version(ctx, v) != 1) return WF_SSL_FATAL;
    return WF_SSL_OK;
}

/* Override the cipher list (TLS 1.2 and earlier).  TLS 1.3 cipher
 * suites are configured separately via SSL_CTX_set_ciphersuites;
 * for now we leave those at OpenSSL defaults. */
int wf_ssl_ctx_set_cipher_suites(SSL_CTX *ctx, const char *cipher_list) {
    if (!ctx || !cipher_list) return WF_SSL_FATAL;
    if (SSL_CTX_set_cipher_list(ctx, cipher_list) != 1) return WF_SSL_FATAL;
    return WF_SSL_OK;
}

/* ALPN protocol list: pre-encoded as a series of <len byte><proto bytes>
 * tuples (e.g. \x02h2\x08http/1.1).  Returns 0 on success per the
 * OpenSSL convention. */
int wf_ssl_ctx_set_alpn(SSL_CTX *ctx,
                       const unsigned char *protos,
                       unsigned int protos_len) {
    return SSL_CTX_set_alpn_protos(ctx, protos, protos_len);
}

/* Server-side ALPN selection callback.  We pick the first protocol
 * in the server's preference list (@arg, the SSL_CTX-owned copy) that
 * the client also advertised in @in. */
static int wf_alpn_select_cb(SSL *ssl,
                              const unsigned char **out,
                              unsigned char *outlen,
                              const unsigned char *in,
                              unsigned int inlen,
                              void *arg) {
    (void) ssl;
    const unsigned char *cb_protos = (const unsigned char *)arg;
    if (!cb_protos) return SSL_TLSEXT_ERR_NOACK;
    /* cb_protos is a length-prefixed list ended by a zero length byte;
     * iterate it against the client's advertised list (length-prefixed,
     * @inlen bytes; OpenSSL has validated it, but an entry running past
     * @inlen ends the walk rather than read beyond it). */
    for (const unsigned char *p = cb_protos; *p != 0;) {
        unsigned char proto_len = *p;
        const unsigned char *proto = p + 1;
        for (unsigned int i = 0; i < inlen;) {
            unsigned char client_len = in[i];
            if (client_len > inlen - i - 1) break;
            if (client_len == proto_len
                && memcmp(in + i + 1, proto, proto_len) == 0) {
                *out = in + i + 1;
                *outlen = client_len;
                return SSL_TLSEXT_ERR_OK;
            }
            i += 1 + client_len;
        }
        p += 1 + proto_len;
    }
    return SSL_TLSEXT_ERR_NOACK;
}

/* The server list lives in SSL_CTX ex_data, freed with the context. */
static CRYPTO_ONCE alpn_index_once = CRYPTO_ONCE_STATIC_INIT;
static int alpn_index = -1;

static void alpn_list_free(void *parent, void *ptr, CRYPTO_EX_DATA *ad,
                           int idx, long argl, void *argp) {
    (void) parent; (void) ad; (void) idx; (void) argl; (void) argp;
    OPENSSL_free(ptr);
}

static void alpn_index_init(void) {
    alpn_index = SSL_CTX_get_ex_new_index(0, NULL, NULL, NULL, alpn_list_free);
}

void wf_ssl_ctx_set_alpn_select_server(SSL_CTX *ctx,
                                       const unsigned char *protos) {
    /* @protos is a length-prefixed list ended by a zero length byte
     * (each entry 1..255 bytes). It is copied: the Haskell caller's
     * buffer is garbage-collected and may be gone before the first
     * handshake reads it. One allocation per SSL_CTX, none per
     * connection. On allocation failure the previous list stays. */
    if (!ctx) return;
    if (!CRYPTO_THREAD_run_once(&alpn_index_once, alpn_index_init)
        || alpn_index < 0)
        return;
    unsigned char *copy = NULL;
    if (protos) {
        size_t len = 0;
        while (protos[len] != 0) len += 1 + (size_t)protos[len];
        copy = OPENSSL_memdup(protos, len + 1);
        if (!copy) return;
    }
    void *old = SSL_CTX_get_ex_data(ctx, alpn_index);
    if (!SSL_CTX_set_ex_data(ctx, alpn_index, copy)) {
        OPENSSL_free(copy);
        return;
    }
    SSL_CTX_set_alpn_select_cb(ctx, wf_alpn_select_cb, copy);
    OPENSSL_free(old);
}

/* ---------------------------------------------------------------- *
 *  Per-connection: bind socket fd, handshake                        *
 * ---------------------------------------------------------------- */

SSL* wf_ssl_new_for_fd(SSL_CTX *ctx, int fd) {
    SSL *ssl = SSL_new(ctx);
    if (!ssl) return NULL;
    if (SSL_set_fd(ssl, fd) != 1) {
        SSL_free(ssl);
        return NULL;
    }
    return ssl;
}

int wf_ssl_set_sni(SSL *ssl, const char *hostname) {
    /* SSL_set_tlsext_host_name returns 1 on success. */
    return SSL_set_tlsext_host_name(ssl, hostname) == 1 ? 0 : WF_SSL_FATAL;
}

int wf_ssl_set_verify_hostname(SSL *ssl, const char *hostname) {
    /* Configure the SSL_get_verify_result() check to also enforce
     * that the cert matches @hostname: its DNS SAN / CN, or, for an
     * IP literal, its IP SAN (set1_host alone never matches an IP
     * address, so https://127.0.0.1 always failed verification).
     * Called before connect for client mode. */
    X509_VERIFY_PARAM *param = SSL_get0_param(ssl);
    if (!param) return WF_SSL_FATAL;
    X509_VERIFY_PARAM_set_hostflags(param, X509_CHECK_FLAG_NO_PARTIAL_WILDCARDS);
    if (hostname && X509_VERIFY_PARAM_set1_ip_asc(param, hostname) == 1) {
        if (X509_VERIFY_PARAM_set1_host(param, NULL, 0) != 1) return WF_SSL_FATAL;
    } else {
        ERR_clear_error(); /* not an IP literal */
        if (X509_VERIFY_PARAM_set1_host(param, hostname, 0) != 1) {
            return WF_SSL_FATAL;
        }
    }
    SSL_set_verify(ssl, SSL_VERIFY_PEER, NULL);
    return WF_SSL_OK;
}

int wf_ssl_connect(SSL *ssl) {
    clear_error_state();
    int rc = SSL_connect(ssl);
    if (rc == 1) return WF_SSL_OK;
    return map_handshake(ssl, rc);
}

int wf_ssl_accept(SSL *ssl) {
    clear_error_state();
    int rc = SSL_accept(ssl);
    if (rc == 1) return WF_SSL_OK;
    return map_handshake(ssl, rc);
}

/* Get the negotiated ALPN protocol after handshake.  Returns 0 if
 * a protocol was negotiated and writes (proto, proto_len) into the
 * caller's out pointers.  Returns -1 if no ALPN was negotiated. */
int wf_ssl_get_alpn(SSL *ssl,
                    const unsigned char **proto_out,
                    unsigned int *proto_len_out) {
    const unsigned char *proto = NULL;
    unsigned int proto_len = 0;
    SSL_get0_alpn_selected(ssl, &proto, &proto_len);
    if (proto_len == 0) return -1;
    if (proto_out) *proto_out = proto;
    if (proto_len_out) *proto_len_out = proto_len;
    return WF_SSL_OK;
}

/* ---------------------------------------------------------------- *
 *  Read / write (the magic-ring direct path)                        *
 * ---------------------------------------------------------------- */

/* Decrypt up to @buf_len bytes of plaintext directly into @buf.
 * On success returns 0 and writes the actual byte count into *out.
 * On clean EOF returns WF_SSL_EOF.  WANT_READ / WANT_WRITE are
 * collapsed into WF_SSL_WANT_RETRY so the Haskell side can park on
 * the IO manager between attempts.  Anything else is WF_SSL_FATAL. */
int wf_ssl_read_into(SSL *ssl, void *buf, size_t buf_len, size_t *out) {
    if (out) *out = 0;
    if (buf_len == 0) return WF_SSL_OK;
    size_t n = 0;
    clear_error_state();
    int rc = SSL_read_ex(ssl, buf, buf_len, &n);
    if (rc == 1) {
        if (out) *out = n;
        return WF_SSL_OK;
    }
    int err = SSL_get_error(ssl, rc);
    if (err == SSL_ERROR_ZERO_RETURN) return WF_SSL_EOF;
    /* WANT_WRITE is kept distinct: a read that has to send first (a TLS
     * 1.3 key update) must park on writability, or it waits for a peer
     * that is waiting for it. */
    if (err == SSL_ERROR_WANT_WRITE) {
        return WF_SSL_WANT_WRITE;
    }
    if (err == SSL_ERROR_WANT_READ) {
        return WF_SSL_WANT_RETRY;
    }
    /* SSL_ERROR_SYSCALL, or SSL_ERROR_SSL whose newest entry is a
     * system error: the underlying recv() failed.  Distinguish a
     * transient "would block" (EAGAIN/EWOULDBLOCK/EINTR), which OpenSSL
     * on some platforms (notably macOS under load) surfaces here instead
     * of SSL_ERROR_WANT_READ, from a real transport close.  Mapping a
     * transient EAGAIN to EOF would tear down a perfectly live
     * connection (every in-flight stream then fails), so retry it; any
     * other cause (errno 0 = peer FIN, EPIPE, ECONNRESET...) is EOF. */
    unsigned long last = ERR_peek_last_error();
    if (err == SSL_ERROR_SYSCALL
        || (err == SSL_ERROR_SSL && ERR_GET_LIB(last) == ERR_LIB_SYS)) {
        int transient = is_transient(sys_cause());
        ERR_clear_error();
        return transient ? WF_SSL_WANT_RETRY : WF_SSL_EOF;
    }
    /* SSL_ERROR_SSL: the OpenSSL-3.x "unexpected eof while reading" (peer
     * closed TCP with no close_notify) and a "shutdown while in init"
     * teardown race are both EOF.  Any other reason (e.g. a bad record
     * MAC from corruption, or the same reason number from another
     * library) stays WF_SSL_FATAL so real failures surface. */
    if (err == SSL_ERROR_SSL && ERR_GET_LIB(last) == ERR_LIB_SSL) {
        int reason = ERR_GET_REASON(last);
        if (reason == SSL_R_UNEXPECTED_EOF_WHILE_READING
            || reason == SSL_R_SHUTDOWN_WHILE_IN_INIT) {
            ERR_clear_error();
            return WF_SSL_EOF;
        }
    }
    return WF_SSL_FATAL;
}

/* Encrypt @buf_len plaintext bytes and write them to the wire.
 * @out gets the number of bytes consumed from the caller's buffer
 * (always == @buf_len on a clean WF_SSL_OK return).  After
 * WF_SSL_WANT_RETRY the caller must retry with the same buffer and
 * length. */
int wf_ssl_write_from(SSL *ssl, const void *buf, size_t buf_len, size_t *out) {
    if (out) *out = 0;
    if (buf_len == 0) return WF_SSL_OK;
    size_t n = 0;
    clear_error_state();
    int rc = SSL_write_ex(ssl, buf, buf_len, &n);
    if (rc == 1) {
        if (out) *out = n;
        return WF_SSL_OK;
    }
    int err = SSL_get_error(ssl, rc);
    /* WANT_WRITE is kept distinct: a read that has to send first (a TLS
     * 1.3 key update) must park on writability, or it waits for a peer
     * that is waiting for it. */
    if (err == SSL_ERROR_WANT_WRITE) {
        return WF_SSL_WANT_WRITE;
    }
    if (err == SSL_ERROR_WANT_READ) {
        return WF_SSL_WANT_RETRY;
    }
    /* As on the read side, a non-blocking send that would block can be
     * surfaced as a system error with EAGAIN/EWOULDBLOCK/EINTR rather
     * than SSL_ERROR_WANT_WRITE.  Retry those (park on writability)
     * instead of treating them as a fatal connection error; otherwise a
     * burst of concurrent stream writes spuriously tears the connection
     * down. */
    if (is_transient_failure(err)) {
        ERR_clear_error();
        return WF_SSL_WANT_WRITE;
    }
    return WF_SSL_FATAL;
}

/* ---------------------------------------------------------------- *
 *  Shutdown / free                                                  *
 * ---------------------------------------------------------------- */

void wf_ssl_shutdown(SSL *ssl) {
    if (!ssl) return;
    /* Best-effort bidirectional close.  SSL_shutdown can return 0
     * meaning "we sent close_notify, peer hasn't replied"; that's
     * fine, the kernel close() takes care of the rest.  Whatever it
     * pushes (e.g. "shutdown while in init") is discarded so it cannot
     * leak into the next call made on this OS thread. */
    clear_error_state();
    SSL_shutdown(ssl);
    ERR_clear_error();
}

void wf_ssl_free(SSL *ssl) {
    if (ssl) SSL_free(ssl);
}

/* ---------------------------------------------------------------- *
 *  Error reporting                                                  *
 * ---------------------------------------------------------------- */

/* Drain the OpenSSL error queue into @buf (NUL-terminated).
 * Returns the number of bytes written (excluding the NUL).  Empty
 * queue → "" + 0. */
size_t wf_ssl_last_error(char *buf, size_t buf_len) {
    if (!buf || buf_len == 0) return 0;
    buf[0] = 0;
    unsigned long e = ERR_peek_last_error();
    if (e == 0) return 0;
    ERR_error_string_n(e, buf, buf_len);
    ERR_clear_error();
    return strlen(buf);
}
