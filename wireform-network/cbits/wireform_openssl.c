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

/* ---------------------------------------------------------------- *
 *  Return codes                                                    *
 * ---------------------------------------------------------------- */

#define WF_SSL_OK            0
#define WF_SSL_EOF          -1   /* peer closed cleanly             */
#define WF_SSL_WANT_RETRY   -2   /* WANT_READ / WANT_WRITE          */
#define WF_SSL_FATAL        -3   /* anything else                   */

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
 * the server-side cb_protos list matches against the client's
 * advertised list. */
static int wf_alpn_select_cb(SSL *ssl,
                              const unsigned char **out,
                              unsigned char *outlen,
                              const unsigned char *in,
                              unsigned int inlen,
                              void *arg) {
    (void) ssl;
    const unsigned char *cb_protos = (const unsigned char *)arg;
    if (!cb_protos) return SSL_TLSEXT_ERR_NOACK;
    /* cb_protos is a length-prefixed list; iterate it against the
     * client's advertised list (also length-prefixed). */
    for (const unsigned char *p = cb_protos; *p != 0;) {
        unsigned char proto_len = *p;
        const unsigned char *proto = p + 1;
        for (unsigned int i = 0; i < inlen;) {
            unsigned char client_len = in[i];
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

void wf_ssl_ctx_set_alpn_select_server(SSL_CTX *ctx,
                                       const unsigned char *protos) {
    /* @protos is a null-terminated length-prefixed list owned by the
     * caller; it must outlive the SSL_CTX.  Saves one allocation per
     * connection vs. dup-ing into the cb's arg slot. */
    SSL_CTX_set_alpn_select_cb(ctx, wf_alpn_select_cb,
                                (void *)protos);
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
     * that the cert's CN / SAN matches @hostname.  Called before
     * connect for client mode. */
    X509_VERIFY_PARAM *param = SSL_get0_param(ssl);
    if (!param) return WF_SSL_FATAL;
    X509_VERIFY_PARAM_set_hostflags(param, X509_CHECK_FLAG_NO_PARTIAL_WILDCARDS);
    if (X509_VERIFY_PARAM_set1_host(param, hostname, 0) != 1) {
        return WF_SSL_FATAL;
    }
    SSL_set_verify(ssl, SSL_VERIFY_PEER, NULL);
    return WF_SSL_OK;
}

int wf_ssl_connect(SSL *ssl) {
    int rc = SSL_connect(ssl);
    if (rc == 1) return WF_SSL_OK;
    int err = SSL_get_error(ssl, rc);
    if (err == SSL_ERROR_WANT_READ || err == SSL_ERROR_WANT_WRITE) {
        return WF_SSL_WANT_RETRY;
    }
    return WF_SSL_FATAL;
}

int wf_ssl_accept(SSL *ssl) {
    int rc = SSL_accept(ssl);
    if (rc == 1) return WF_SSL_OK;
    int err = SSL_get_error(ssl, rc);
    if (err == SSL_ERROR_WANT_READ || err == SSL_ERROR_WANT_WRITE) {
        return WF_SSL_WANT_RETRY;
    }
    return WF_SSL_FATAL;
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
    int rc = SSL_read_ex(ssl, buf, buf_len, &n);
    if (rc == 1) {
        if (out) *out = n;
        return WF_SSL_OK;
    }
    int err = SSL_get_error(ssl, rc);
    if (err == SSL_ERROR_ZERO_RETURN) return WF_SSL_EOF;
    if (err == SSL_ERROR_WANT_READ || err == SSL_ERROR_WANT_WRITE) {
        return WF_SSL_WANT_RETRY;
    }
    /* SSL_ERROR_SYSCALL: the underlying recv() failed.  Distinguish a
     * transient "would block" (EAGAIN/EWOULDBLOCK/EINTR) — which OpenSSL
     * on some platforms (notably macOS under load) surfaces here instead
     * of SSL_ERROR_WANT_READ — from a real transport close.  Mapping a
     * transient EAGAIN to EOF would tear down a perfectly live
     * connection (every in-flight stream then fails), so retry it; only
     * errno==0 (peer FIN) / EPIPE / ECONNRESET etc. are genuine EOF. */
    if (err == SSL_ERROR_SYSCALL) {
        int e = errno;
        if (e == EAGAIN || e == EWOULDBLOCK || e == EINTR) {
            return WF_SSL_WANT_RETRY;
        }
        ERR_clear_error();
        return WF_SSL_EOF;
    }
    /* SSL_ERROR_SSL: a system-library (errno) error is again EAGAIN-vs-gone;
     * the OpenSSL-3.x "unexpected eof while reading" (peer closed TCP with
     * no close_notify) and a "shutdown while in init" teardown race are
     * both EOF.  Any other reason (e.g. a bad record MAC from corruption)
     * stays WF_SSL_FATAL so real failures surface. */
    if (err == SSL_ERROR_SSL) {
        unsigned long e = ERR_peek_last_error();
        int reason = ERR_GET_REASON(e);
        if (ERR_GET_LIB(e) == ERR_LIB_SYS) {
            if (reason == EAGAIN || reason == EWOULDBLOCK || reason == EINTR) {
                ERR_clear_error();
                return WF_SSL_WANT_RETRY;
            }
            ERR_clear_error();
            return WF_SSL_EOF;
        }
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
 * (always == @buf_len on a clean WF_SSL_OK return). */
int wf_ssl_write_from(SSL *ssl, const void *buf, size_t buf_len, size_t *out) {
    if (out) *out = 0;
    if (buf_len == 0) return WF_SSL_OK;
    size_t n = 0;
    int rc = SSL_write_ex(ssl, buf, buf_len, &n);
    if (rc == 1) {
        if (out) *out = n;
        return WF_SSL_OK;
    }
    int err = SSL_get_error(ssl, rc);
    if (err == SSL_ERROR_WANT_READ || err == SSL_ERROR_WANT_WRITE) {
        return WF_SSL_WANT_RETRY;
    }
    /* As on the read side, a non-blocking send that would block can be
     * surfaced as SSL_ERROR_SYSCALL with EAGAIN/EWOULDBLOCK/EINTR rather
     * than SSL_ERROR_WANT_WRITE.  Retry those (park on writability)
     * instead of treating them as a fatal connection error — otherwise a
     * burst of concurrent stream writes spuriously tears the connection
     * down. */
    if (err == SSL_ERROR_SYSCALL) {
        int e = errno;
        if (e == EAGAIN || e == EWOULDBLOCK || e == EINTR) {
            return WF_SSL_WANT_RETRY;
        }
        return WF_SSL_FATAL;
    }
    if (err == SSL_ERROR_SSL) {
        unsigned long e = ERR_peek_last_error();
        if (ERR_GET_LIB(e) == ERR_LIB_SYS) {
            int reason = ERR_GET_REASON(e);
            if (reason == EAGAIN || reason == EWOULDBLOCK || reason == EINTR) {
                ERR_clear_error();
                return WF_SSL_WANT_RETRY;
            }
        }
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
     * fine, the kernel close() takes care of the rest. */
    SSL_shutdown(ssl);
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
