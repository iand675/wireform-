/* A verifying TLS client (wf_ssl_new_for_fd, wf_ssl_set_sni,
 * wf_ssl_set_verify_hostname, wf_ssl_connect) fed arbitrary server bytes
 * over a socketpair after it has sent its ClientHello.
 *
 * Input: [flags u8][server bytes...]. flags bit 0 caps the client at TLS
 * 1.2; bits 1-2 split the server bytes into 1, 2 or 4 deliveries with a
 * connect step between each; bit 3 leaves the connection open after the
 * last delivery; bit 4 pins the hostname as an IP literal (127.0.0.1)
 * instead of "localhost"; bits 5-7 pick a wf_ssl_last_error buffer size.
 *
 * Invariants: every return is one of the four codes; the client never
 * reports OK (no input can forge the server's signature over this
 * session's transcript with the identity's key); a closed peer ends the
 * handshake (EOF/FATAL, not WANT_RETRY); wf_ssl_last_error is well
 * formed. Seeds are real server flights captured from wf_ssl_accept.
 */
#include "harness.h"
#include "api.h"

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
    if (size < 1 || size > 65536) return 0;
    uint8_t flags = data[0];
    const uint8_t *in = data + 1;
    size_t n = size - 1;
    int parts = 1 << ((flags >> 1) & 3);
    if (parts == 8) parts = 4;
    int keep_open = (flags >> 3) & 1;
    const char *host = (flags >> 4) & 1 ? "127.0.0.1" : "localhost";
    size_t errcap = (size_t)((flags >> 5) * 71);

    int fds[2];
    tls_socketpair(fds);
    tls_bigbufs(fds[0]);
    tls_bigbufs(fds[1]);
    SSL *c = wf_ssl_new_for_fd(tls_fuzz_client_ctx(flags & 1), fds[0]);
    WF_FUZZ_ASSERT(c != NULL);
    WF_FUZZ_ASSERT(wf_ssl_set_sni(c, "localhost") == WF_SSL_OK);
    WF_FUZZ_ASSERT(wf_ssl_set_verify_hostname(c, host) == WF_SSL_OK);

    int rc = wf_ssl_connect(c); /* sends the ClientHello */
    WF_FUZZ_ASSERT(rc == WF_SSL_WANT_RETRY);
    tls_drain(fds[1]);

    size_t sent = 0;
    for (int p = 1; p <= parts && rc == WF_SSL_WANT_RETRY; p++) {
        size_t upto = n * (size_t)p / (size_t)parts;
        sent += tls_push(fds[1], in + sent, upto - sent);
        if (p == parts && !keep_open) shutdown(fds[1], SHUT_WR);
        for (int step = 0; step < 4 && rc == WF_SSL_WANT_RETRY; step++) {
            rc = wf_ssl_connect(c);
            WF_FUZZ_ASSERT(tls_rc_valid(rc));
            tls_drain(fds[1]);
        }
    }
    WF_FUZZ_ASSERT(rc != WF_SSL_OK);
    if (!keep_open && rc == WF_SSL_WANT_RETRY) {
        rc = wf_ssl_connect(c);
        WF_FUZZ_ASSERT(rc != WF_SSL_WANT_RETRY || sent < n);
    }
    WF_FUZZ_ASSERT(tls_last_error_ok(errcap));
    wf_ssl_shutdown(c);
    WF_FUZZ_ASSERT(ERR_peek_error() == 0);
    wf_ssl_free(c);
    close(fds[0]);
    close(fds[1]);
    ERR_clear_error();
    return 0;
}
