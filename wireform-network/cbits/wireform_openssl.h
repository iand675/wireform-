/*
 * wireform_openssl.h — Public surface for the direct-OpenSSL
 * TLS-on-ring path.  See the .c file for design notes.
 */

#ifndef WIREFORM_OPENSSL_H
#define WIREFORM_OPENSSL_H

#include <stddef.h>
#include <openssl/ssl.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Return codes shared by every entry point. */
#define WF_SSL_OK          0
#define WF_SSL_EOF        -1
#define WF_SSL_WANT_RETRY -2  /* retry, waiting for readability       */
#define WF_SSL_FATAL      -3
#define WF_SSL_WANT_WRITE -4  /* retry, waiting for writability       */

/* One-time process init.  Idempotent; safe to call from FFI on
 * every connection setup (OpenSSL 1.1+ guards itself). */
void wf_ssl_init(void);

/* SSL_CTX construction.  Return NULL on failure. */
SSL_CTX* wf_ssl_ctx_new_client(int verify_peer);
SSL_CTX* wf_ssl_ctx_new_server(const char *cert_path, const char *key_path);
void     wf_ssl_ctx_free(SSL_CTX *ctx);

/* Optional knobs.  Each returns 0 on success / WF_SSL_FATAL on failure. */
int wf_ssl_ctx_load_ca_bundle(SSL_CTX *ctx, const char *ca_path);
int wf_ssl_ctx_use_client_cert(SSL_CTX *ctx,
                                const char *cert_path,
                                const char *key_path);
int wf_ssl_ctx_set_min_proto(SSL_CTX *ctx, int version);   /* 12 or 13 */
int wf_ssl_ctx_set_cipher_suites(SSL_CTX *ctx, const char *cipher_list);

/* ALPN.  protos / protos_len: length-prefixed list,
 * e.g. "\x02h2\x08http/1.1"; OpenSSL rejects a malformed one (nonzero).
 * The server list is the same encoding ended by a zero length byte
 * (each protocol 1..255 bytes); it is copied into the SSL_CTX, so the
 * caller's buffer need not outlive the call.  NULL clears it. */
int  wf_ssl_ctx_set_alpn(SSL_CTX *ctx,
                          const unsigned char *protos,
                          unsigned int protos_len);
void wf_ssl_ctx_set_alpn_select_server(SSL_CTX *ctx,
                                       const unsigned char *protos);

/* Per-connection. */
SSL* wf_ssl_new_for_fd(SSL_CTX *ctx, int fd);
int  wf_ssl_set_sni(SSL *ssl, const char *hostname);
int  wf_ssl_set_verify_hostname(SSL *ssl, const char *hostname);
int  wf_ssl_connect(SSL *ssl);
int  wf_ssl_accept(SSL *ssl);
int  wf_ssl_get_alpn(SSL *ssl,
                     const unsigned char **proto_out,
                     unsigned int *proto_len_out);

/* The magic-ring direct path. */
int  wf_ssl_read_into(SSL *ssl, void *buf, size_t buf_len, size_t *out);
int  wf_ssl_write_from(SSL *ssl, const void *buf, size_t buf_len, size_t *out);

void wf_ssl_shutdown(SSL *ssl);
void wf_ssl_free(SSL *ssl);

size_t wf_ssl_last_error(char *buf, size_t buf_len);

#ifdef __cplusplus
}
#endif

#endif /* WIREFORM_OPENSSL_H */
