/*
 * failfns.h - library failure injection for the direct-OpenSSL shim's
 * "fail" build variant.
 *
 * The variant renames these entry points (see module.mk) so
 * wireform_openssl.c calls the fakes below instead of OpenSSL. A fake
 * fails when its failpoint fires (common/wf_fail.h) and otherwise
 * passes through to the real function, so a fail-variant binary can
 * run ordinary tests too. Naming follows the renamed entry point, so
 * e.g. setenv("WF_FAIL_SSL_new", "1", 1) makes the next SSL_new call
 * fail.
 *
 * OpenSSL 3.6 declares these with direct prototypes, so the renames
 * also rewrite their declarations in the included headers; the
 * explicit declarations below match those.
 *
 * Some fakes also simulate library behaviour a real OpenSSL does not
 * produce, to drive the kernel's defensive guards (the same idea as
 * kafka-compress's "LZ4F_decompress_full"):
 *
 *   - "key_file_fake_ok": SSL_CTX_use_PrivateKey_file accepts a key
 *     file without loading it. Real OpenSSL rejects a key that does
 *     not match the already-loaded certificate here, so the kernel's
 *     SSL_CTX_check_private_key is only reachable against an OpenSSL
 *     that loads keys without cross-checking (as older ones did).
 *   - "read_fail_*" / "write_want_read": SSL_read_ex / SSL_write_ex
 *     fail and the next SSL_get_error reports the chosen code,
 *     decoupled from the error queue, so the kernel's transient-error
 *     classification can be driven for every (code, queue) pairing.
 *   - "write_err_multi": a failed write whose record layer queued an
 *     older library error and a newer transient system error, the
 *     multi-entry queue the kernel's classification comments about
 *     (SSL_get_error decides from the oldest entry, the kernel's
 *     sys_cause from the newest).
 */
#ifndef NETWORK_OPENSSL_FAILFNS_H
#define NETWORK_OPENSSL_FAILFNS_H

#include <openssl/ssl.h>
#include <openssl/err.h>
#include <openssl/x509_vfy.h>
#include <openssl/crypto.h>

SSL_CTX *wf_fail_SSL_CTX_new(const SSL_METHOD *meth);
long wf_fail_SSL_CTX_ctrl(SSL_CTX *ctx, int cmd, long larg, void *parg);
int wf_fail_SSL_CTX_use_PrivateKey_file(SSL_CTX *ctx, const char *file, int type);
int wf_fail_SSL_CTX_set_ex_data(SSL_CTX *ssl, int idx, void *data);
SSL *wf_fail_SSL_new(SSL_CTX *ctx);
int wf_fail_SSL_set_fd(SSL *s, int fd);
X509_VERIFY_PARAM *wf_fail_SSL_get0_param(SSL *ssl);
int wf_fail_X509_VERIFY_PARAM_set1_host(X509_VERIFY_PARAM *param,
                                        const char *name, size_t namelen);
void *wf_fail_CRYPTO_memdup(const void *str, size_t siz,
                            const char *file, int line);
int wf_fail_CRYPTO_THREAD_run_once(CRYPTO_ONCE *once, void (*init)(void));
int wf_fail_SSL_read_ex(SSL *ssl, void *buf, size_t num, size_t *readbytes);
int wf_fail_SSL_write_ex(SSL *s, const void *buf, size_t num, size_t *written);
int wf_fail_SSL_get_error(const SSL *s, int ret_code);

#endif /* NETWORK_OPENSSL_FAILFNS_H */
