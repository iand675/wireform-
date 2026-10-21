/* Kernel API of wireform-http1/cbits/http1_send.c as Network.HTTP1.SendFile
 * imports it, the caller contracts, and socket / file helpers for the
 * tests and the fuzz target.
 *
 *   foreign import ccall safe "hs_http1_sendfile"
 *     c_http1_sendfile :: CInt -> CInt -> Int64 -> CSize -> IO CSsize
 *   foreign import ccall safe "hs_http1_send_more"
 *     c_send_more :: CInt -> Ptr () -> CSize -> IO CSsize
 *
 * hs_http1_send_all has no Haskell import; it is tested to the contract
 * its comment documents.
 *
 * Contracts (from SendFile.sendFileFd / sendMore):
 *   - sock is a connected stream socket; the network package makes it
 *     non-blocking, so a full send buffer yields -EAGAIN / -EWOULDBLOCK
 *     and the Haskell loop waits for writability (threadWaitWrite) before
 *     retrying. Blocking sockets block instead.
 *   - send_more(sock, buf, len): buf readable for len bytes; returns bytes
 *     sent in [0, len] or -errno; EINTR is retried.
 *   - send_all: returns len, a short count after partial progress followed
 *     by a failure, or -errno when the first send fails.
 *   - sendfile(sock, fd, offset, count): sendFileFd passes count in
 *     1 .. 1 MiB and offset = Word64 narrowed to Int64 (so offsets of 2^63
 *     and up arrive negative and must fail cleanly with -EINVAL). A count
 *     above the platform's per-call transfer limit (Linux MAX_RW_COUNT,
 *     Darwin INT64_MAX) is clamped, so a "send the rest" count of SIZE_MAX
 *     transfers rather than failing with EINVAL. Returns bytes sent in
 *     1..count, exactly 0 only at or past end of file (or for
 *     count == 0), else -errno. At EOF with a full non-blocking buffer
 *     Darwin may report -EAGAIN before 0 (the retry after writability then
 *     returns 0). EINTR before progress is retried; bytes
 *     sent before an interruption are reported, never re-sent.
 *   - Darwin has no MSG_NOSIGNAL: SIGPIPE must be ignored by the process
 *     (the GHC RTS does this); the tests ignore it the same way.
 */
#ifndef HTTP1_SEND_API_H
#define HTTP1_SEND_API_H

#include <errno.h>
#include <fcntl.h>
#include <pthread.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

typedef ssize_t hs_ssize_t;

hs_ssize_t hs_http1_send_more(int sock, const void *buf, size_t len);
hs_ssize_t hs_http1_send_all(int sock, const void *buf, size_t len);
hs_ssize_t hs_http1_sendfile(int sock, int file_fd, int64_t offset, size_t count);

static inline int hs_is_eagain(hs_ssize_t r)
{
    return r == -EAGAIN || r == -EWOULDBLOCK;
}

/* ------------------------------------------------------------------ */
/* fd helpers                                                          */
/* ------------------------------------------------------------------ */

static inline void sx_die(const char *what)
{
    perror(what);
    abort();
}

static inline void sx_pair(int sv[2])
{
    if (socketpair(AF_UNIX, SOCK_STREAM, 0, sv) != 0) sx_die("socketpair");
}

static inline void sx_nonblock(int fd, int on)
{
    int fl = fcntl(fd, F_GETFL);
    if (fl < 0) sx_die("fcntl(F_GETFL)");
    fl = on ? (fl | O_NONBLOCK) : (fl & ~O_NONBLOCK);
    if (fcntl(fd, F_SETFL, fl) != 0) sx_die("fcntl(F_SETFL)");
}

static inline void sx_sndbuf(int fd, int bytes)
{
    (void)setsockopt(fd, SOL_SOCKET, SO_SNDBUF, &bytes, sizeof bytes);
    (void)setsockopt(fd, SOL_SOCKET, SO_RCVBUF, &bytes, sizeof bytes);
}

/* Anonymous temp file holding n bytes, offset 0. */
static inline int sx_tempfile(const void *data, size_t n)
{
    const char *dir = getenv("TMPDIR");
    char path[512];
    snprintf(path, sizeof path, "%s/wf-http1-send-XXXXXX", dir && *dir ? dir : "/tmp");
    int fd = mkstemp(path);
    if (fd < 0) sx_die("mkstemp");
    unlink(path);
    size_t done = 0;
    while (done < n) {
        ssize_t w = write(fd, (const char *)data + done, n - done);
        if (w < 0) {
            if (errno == EINTR) continue;
            sx_die("write(tempfile)");
        }
        done += (size_t)w;
    }
    return fd;
}

/* Read whatever is available on a non-blocking fd. Bytes land in
 * out[*have .. cap); *have counts every byte read, also those beyond cap
 * (discarded), so an over-long stream is still detected. Returns the bytes
 * read by this call. */
static inline size_t sx_drain(int fd, uint8_t *out, size_t *have, size_t cap)
{
    size_t got = 0;
    uint8_t scratch[4096];
    for (;;) {
        int fits = *have < cap;
        size_t room = fits ? cap - *have : sizeof scratch;
        if (room > sizeof scratch) room = sizeof scratch;
        ssize_t r = read(fd, fits ? out + *have : scratch, room);
        if (r > 0) {
            *have += (size_t)r;
            got += (size_t)r;
            continue;
        }
        if (r < 0 && errno == EINTR) continue;
        return got;
    }
}

/* Fill a non-blocking socket's send buffer until send() fails. */
static inline size_t sx_fill(int fd)
{
    static const uint8_t junk[4096];
    size_t total = 0;
    for (;;) {
        ssize_t r = send(fd, junk, sizeof junk, 0);
        if (r > 0) {
            total += (size_t)r;
            continue;
        }
        if (r < 0 && errno == EINTR) continue;
        return total;
    }
}

static inline void sx_msleep(long ms)
{
    struct timespec ts = { ms / 1000, (ms % 1000) * 1000000L };
    while (nanosleep(&ts, &ts) != 0 && errno == EINTR) {
    }
}

#endif
