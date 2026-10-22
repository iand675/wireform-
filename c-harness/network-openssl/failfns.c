/*
 * failfns.c - see failfns.h. One translation unit per program (via
 * network-openssl_EXTRA) defines the fakes. The command-line renames
 * are undefined first, so the pass-through calls reach the real
 * OpenSSL functions under their own names, while every other
 * translation unit (the kernel, the tests) keeps the renamed calls.
 */
#undef SSL_CTX_new
#undef SSL_CTX_ctrl
#undef SSL_CTX_use_PrivateKey_file
#undef SSL_CTX_set_ex_data
#undef SSL_new
#undef SSL_set_fd
#undef SSL_get0_param
#undef X509_VERIFY_PARAM_set1_host
#undef CRYPTO_memdup
#undef CRYPTO_THREAD_run_once
#undef SSL_read_ex
#undef SSL_write_ex
#undef SSL_get_error

#include "failfns.h"
#include "wf_fail.h"

#include <errno.h>

/* The force-included headers above saw the command-line renames, so
 * they declared the fakes' names instead of the real library entry
 * points. Declare the real ones under their own names for the
 * pass-through calls. */
extern SSL_CTX *SSL_CTX_new(const SSL_METHOD *meth);
extern long SSL_CTX_ctrl(SSL_CTX *ctx, int cmd, long larg, void *parg);
extern int SSL_CTX_use_PrivateKey_file(SSL_CTX *ctx, const char *file, int type);
extern int SSL_CTX_set_ex_data(SSL_CTX *ssl, int idx, void *data);
extern SSL *SSL_new(SSL_CTX *ctx);
extern int SSL_set_fd(SSL *s, int fd);
extern X509_VERIFY_PARAM *SSL_get0_param(SSL *ssl);
extern int X509_VERIFY_PARAM_set1_host(X509_VERIFY_PARAM *param,
                                       const char *name, size_t namelen);
extern void *CRYPTO_memdup(const void *str, size_t siz,
                           const char *file, int line);
extern int CRYPTO_THREAD_run_once(CRYPTO_ONCE *once, void (*init)(void));
extern int SSL_read_ex(SSL *ssl, void *buf, size_t num, size_t *readbytes);
extern int SSL_write_ex(SSL *s, const void *buf, size_t num, size_t *written);
extern int SSL_get_error(const SSL *s, int ret_code);

/* -1: no forced classification. Otherwise the value the next
 * SSL_get_error reports (SSL_ERROR_NONE is 0, so the sentinel must be
 * negative). Set by the read/write fakes, consumed immediately after
 * by the kernel's classification call. */
static int forced_get_error = -1;

SSL_CTX *wf_fail_SSL_CTX_new(const SSL_METHOD *meth)
{
    if (wf_failpoint("SSL_CTX_new")) return NULL;
    return SSL_CTX_new(meth);
}

long wf_fail_SSL_CTX_ctrl(SSL_CTX *ctx, int cmd, long larg, void *parg)
{
    /* Only the min-protocol knob is a documented failure point. */
    if (cmd == SSL_CTRL_SET_MIN_PROTO_VERSION
        && wf_failpoint("SSL_CTX_ctrl_set_min_proto"))
        return 0;
    return SSL_CTX_ctrl(ctx, cmd, larg, parg);
}

int wf_fail_SSL_CTX_use_PrivateKey_file(SSL_CTX *ctx, const char *file, int type)
{
    /* See failfns.h: accept without loading, so the caller's
     * SSL_CTX_check_private_key is what rejects the key. */
    if (wf_failpoint("key_file_fake_ok")) return 1;
    return SSL_CTX_use_PrivateKey_file(ctx, file, type);
}

int wf_fail_SSL_CTX_set_ex_data(SSL_CTX *ssl, int idx, void *data)
{
    if (wf_failpoint("SSL_CTX_set_ex_data")) return 0;
    return SSL_CTX_set_ex_data(ssl, idx, data);
}

SSL *wf_fail_SSL_new(SSL_CTX *ctx)
{
    if (wf_failpoint("SSL_new")) return NULL;
    return SSL_new(ctx);
}

int wf_fail_SSL_set_fd(SSL *s, int fd)
{
    if (wf_failpoint("SSL_set_fd")) return 0;
    return SSL_set_fd(s, fd);
}

X509_VERIFY_PARAM *wf_fail_SSL_get0_param(SSL *ssl)
{
    if (wf_failpoint("SSL_get0_param")) return NULL;
    return SSL_get0_param(ssl);
}

int wf_fail_X509_VERIFY_PARAM_set1_host(X509_VERIFY_PARAM *param,
                                        const char *name, size_t namelen)
{
    if (wf_failpoint("X509_VERIFY_PARAM_set1_host")) return 0;
    return X509_VERIFY_PARAM_set1_host(param, name, namelen);
}

void *wf_fail_CRYPTO_memdup(const void *str, size_t siz,
                            const char *file, int line)
{
    if (wf_failpoint("CRYPTO_memdup")) return NULL;
    return CRYPTO_memdup(str, siz, file, line);
}

int wf_fail_CRYPTO_THREAD_run_once(CRYPTO_ONCE *once, void (*init)(void))
{
    /* "run_once_fail": report failure without running init (the once
     * stays pending for a later real call). "run_once_skip": report
     * success without running init, so state the initializer would
     * have set is still missing. */
    if (wf_failpoint("run_once_fail")) return 0;
    if (wf_failpoint("run_once_skip")) return 1;
    return CRYPTO_THREAD_run_once(once, init);
}

int wf_fail_SSL_read_ex(SSL *ssl, void *buf, size_t num, size_t *readbytes)
{
    if (wf_failpoint("read_fail_none")) {
        forced_get_error = SSL_ERROR_NONE;
        return 0;
    }
    if (wf_failpoint("read_fail_sysev")) {
        /* A non-system error in the queue under a SYSCALL verdict. */
        ERR_raise(ERR_LIB_EVP, 1);
        forced_get_error = SSL_ERROR_SYSCALL;
        return 0;
    }
    if (wf_failpoint("read_fail_sysev_sys")) {
        ERR_raise(ERR_LIB_SYS, EAGAIN);
        forced_get_error = SSL_ERROR_SYSCALL;
        return 0;
    }
    return SSL_read_ex(ssl, buf, num, readbytes);
}

int wf_fail_SSL_write_ex(SSL *s, const void *buf, size_t num, size_t *written)
{
    if (wf_failpoint("write_want_read")) {
        forced_get_error = SSL_ERROR_WANT_READ;
        return 0;
    }
    if (wf_failpoint("write_err_multi")) {
        /* A failed write whose record layer queued two entries: an
         * older SSL error and a newer system EAGAIN. SSL_get_error
         * peeks the oldest entry and reports SSL_ERROR_SSL, while the
         * kernel's transient classification peeks the newest and sees
         * a transient system error: the "SSL_ERROR_SSL whose newest
         * entry is a transient system error" case from the source. */
        ERR_raise(ERR_LIB_SSL, SSL_R_BAD_LENGTH);
        ERR_raise(ERR_LIB_SYS, EAGAIN);
        return 0;
    }
    return SSL_write_ex(s, buf, num, written);
}

int wf_fail_SSL_get_error(const SSL *s, int ret_code)
{
    if (forced_get_error >= 0) {
        int forced = forced_get_error;
        forced_get_error = -1;
        return forced;
    }
    return SSL_get_error(s, ret_code);
}
