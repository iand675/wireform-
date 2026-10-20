/* Portable socket-send helpers for the HTTP/1.x server.
 *
 * Two concerns live here:
 *
 *  1. hs_http1_send_more / hs_http1_send_all — plain socket writes,
 *     with MSG_MORE coalescing where the platform supports it.
 *
 *  2. hs_http1_sendfile — a single zero-copy file->socket transfer,
 *     wrapping the platform's native primitive:
 *
 *       Linux            sendfile(2)        (out_fd, in_fd, &off, count)
 *       macOS / Darwin   sendfile(2)        (fd, s, off, &len, hdtr, flags)
 *       FreeBSD / Dfly   sendfile(2)        (fd, s, off, n, hdtr, &sbytes, flags)
 *       Windows          TransmitFile()     (mswsock; offset via OVERLAPPED)
 *       anything else    pread(2)+send(2)   userspace fallback (not zero-copy,
 *                                            but correct everywhere POSIX)
 *
 *     The argument order and semantics differ wildly across these — the
 *     Linux binding is *not* ABI-compatible with the BSD/macOS one, which
 *     is exactly why a single raw FFI import to "sendfile" silently fails
 *     on non-Linux. This shim normalises all of them to one signature:
 *
 *       hs_ssize_t hs_http1_sendfile(sock, file_fd, offset, count)
 *         -> bytes sent (>= 0), 0 on premature EOF, or -errno on failure.
 *
 *     EINTR is retried internally so the Haskell loop never sees it.
 *
 * Windows note: this file is winsock-clean and would compile on Windows,
 * but wireform-http1 currently depends on `unix`, so the package as a
 * whole is Unix-only until that dependency is lifted. The _WIN32 paths
 * are provided so the C side is ready when it is.
 */

#if defined(_WIN32)
#  include <winsock2.h>
#  include <mswsock.h>
#  include <windows.h>
#  include <io.h>
#  include <errno.h>
#  include <limits.h>
#  include <stdint.h>
typedef SSIZE_T hs_ssize_t;
#  ifndef MSG_NOSIGNAL
#    define MSG_NOSIGNAL 0
#  endif
#  ifndef MSG_MORE
#    define MSG_MORE 0
#  endif
#else
#  include <stddef.h>
#  include <stdint.h>
#  include <sys/types.h>
#  include <sys/socket.h>
#  include <errno.h>
#  include <unistd.h>
#  if defined(__linux__)
#    include <sys/sendfile.h>
#  endif
typedef ssize_t hs_ssize_t;
#  ifndef MSG_MORE
#    define MSG_MORE 0
#  endif
#  ifndef MSG_NOSIGNAL
#    define MSG_NOSIGNAL 0
#  endif
#endif

/* Returns the number of bytes sent (>= 0), or -errno on failure
 * (-WSAGetLastError() on Windows; the codes are not POSIX errno there,
 * but the Haskell binding for this path is Unix-only, see header note).
 * EINTR is retried. On a non-blocking socket with a full send buffer the
 * result is -EAGAIN (or -EWOULDBLOCK); the caller waits for writability.
 *
 * SIGPIPE: suppressed per call with MSG_NOSIGNAL where it exists (Linux,
 * the BSDs). Darwin has no MSG_NOSIGNAL, so there a write to a closed peer
 * raises SIGPIPE unless it is ignored (the GHC RTS ignores it) or the
 * socket has SO_NOSIGPIPE; with SIGPIPE ignored the result is -EPIPE.
 */
hs_ssize_t hs_http1_send_more(int sock, const void *buf, size_t len) {
#if defined(_WIN32)
  int n = send((SOCKET)sock, (const char *)buf, len > INT_MAX ? INT_MAX : (int)len, 0);
  if (n == SOCKET_ERROR) return -(hs_ssize_t)WSAGetLastError();
  return n;
#else
  ssize_t n;
  do {
    n = send(sock, buf, len, MSG_MORE | MSG_NOSIGNAL);
  } while (n < 0 && errno == EINTR);
  if (n < 0) return -errno;
  return n;
#endif
}

/* Plain send() that loops until everything is written, looping on EINTR
 * and suppressing SIGPIPE where the platform can (see hs_http1_send_more).
 * Returns the total bytes sent. A failure before any byte was sent
 * returns -errno; a failure after partial progress returns the (short)
 * count so the caller knows how much went out and sees the error on its
 * next call. On a non-blocking socket a short count therefore means the
 * send buffer filled up.
 */
hs_ssize_t hs_http1_send_all(int sock, const void *buf, size_t len) {
  const char *p = (const char *)buf;
  size_t remaining = len;
  while (remaining > 0) {
#if defined(_WIN32)
    int n = send((SOCKET)sock, p, remaining > INT_MAX ? INT_MAX : (int)remaining, 0);
    if (n == SOCKET_ERROR) {
      if (remaining < len) break;
      return -(hs_ssize_t)WSAGetLastError();
    }
#else
    ssize_t n;
    do {
      n = send(sock, p, remaining, MSG_NOSIGNAL);
    } while (n < 0 && errno == EINTR);
    if (n < 0) {
      if (remaining < len) break;
      return -errno;
    }
#endif
    if (n == 0) break; /* peer closed */
    p += n;
    remaining -= (size_t)n;
  }
  return (hs_ssize_t)(len - remaining);
}

/* One zero-copy (where available) file->socket transfer of up to `count`
 * bytes starting at `offset`. See the header comment for the per-platform
 * primitive used.
 *
 * Returns bytes sent (> 0), 0 when offset is at or past the end of the
 * file or count == 0, or -errno on failure. EINTR is retried internally
 * when nothing was sent yet; once some bytes went out they are reported
 * (never re-sent). A non-blocking socket whose send buffer is full gives
 * -EAGAIN, never 0, so 0 always means end of file (at end of file Darwin
 * may still report -EAGAIN first while the buffer is full; the retry after
 * writability returns 0). offset < 0 is -EINVAL.
 *
 * Building with -DHS_HTTP1_SENDFILE_PREAD selects the portable pread+send
 * fallback on every POSIX platform (used to test that path).
 */
hs_ssize_t hs_http1_sendfile(int sock, int file_fd, int64_t offset, size_t count) {
  /* Darwin, FreeBSD and TransmitFile all read "0 bytes" as "to EOF". */
  if (count == 0) return 0;
#if defined(_WIN32)
  HANDLE hFile = (HANDLE)_get_osfhandle(file_fd);
  if (hFile == INVALID_HANDLE_VALUE) return -(hs_ssize_t)EBADF;
  if (offset < 0) return -(hs_ssize_t)EINVAL;
  /* TransmitFile sends nothing and still succeeds at EOF, and reports no
   * byte count on a synchronous call; clamp to the file size so the
   * returned count is exact and EOF is 0. */
  LARGE_INTEGER size;
  if (!GetFileSizeEx(hFile, &size)) return -(hs_ssize_t)GetLastError();
  if (offset >= size.QuadPart) return 0;
  uint64_t avail = (uint64_t)(size.QuadPart - offset);
  if ((uint64_t)count < avail) avail = (uint64_t)count;
  DWORD toSend = avail > 0x7FFFFFFEu ? 0x7FFFFFFEu : (DWORD)avail;
  /* lpOverlapped == NULL: TransmitFile starts at the file pointer and
   * completes before returning even on an overlapped socket. (Passing an
   * OVERLAPPED would make it asynchronous on overlapped sockets, the
   * default for socket(), and return WSA_IO_PENDING.) */
  LARGE_INTEGER pos;
  pos.QuadPart = offset;
  if (!SetFilePointerEx(hFile, pos, NULL, FILE_BEGIN)) return -(hs_ssize_t)GetLastError();
  if (!TransmitFile((SOCKET)sock, hFile, toSend, 0, NULL, NULL, 0))
    return -(hs_ssize_t)WSAGetLastError();
  return (hs_ssize_t)toSend;

#elif defined(__linux__) && !defined(HS_HTTP1_SENDFILE_PREAD)
  /* ssize_t sendfile(int out_fd, int in_fd, off_t *offset, size_t count);
   * Linux reports partial progress as a short count, and fails with EINTR
   * / EAGAIN only when nothing was transferred. */
  off_t off = (off_t)offset;
  ssize_t n;
  do {
    n = sendfile(sock, file_fd, &off, count);
  } while (n < 0 && errno == EINTR);
  if (n < 0) return -errno;
  return n;

#elif defined(__APPLE__) && !defined(HS_HTTP1_SENDFILE_PREAD)
  /* int sendfile(int fd, int s, off_t offset, off_t *len,
   *              struct sf_hdtr *hdtr, int flags);
   * fd = source file, s = socket (note: order is the reverse of Linux).
   * *len is in/out: requested in, actually-sent out. Unlike Linux, a call
   * interrupted by a signal (EINTR) or stopped by a full non-blocking
   * buffer (EAGAIN) after partial progress fails AND reports the bytes
   * sent in *len: retrying from `offset` would send them twice, and
   * returning *len == 0 on EAGAIN would read as EOF. */
  if (count > (size_t)INT64_MAX) count = (size_t)INT64_MAX;
  for (;;) {
    off_t len = (off_t)count;
    int r = sendfile(file_fd, sock, (off_t)offset, &len, NULL, 0);
    if (r == 0 || len > 0) return (hs_ssize_t)len;
    if (errno != EINTR) return -errno;
  }

#elif (defined(__FreeBSD__) || defined(__DragonFly__)) && !defined(HS_HTTP1_SENDFILE_PREAD)
  /* int sendfile(int fd, int s, off_t offset, size_t nbytes,
   *              struct sf_hdtr *hdtr, off_t *sbytes, int flags);
   * Same partial-progress-on-error contract as Darwin. */
  for (;;) {
    off_t sbytes = 0;
    int r = sendfile(file_fd, sock, (off_t)offset, count, NULL, &sbytes, 0);
    if (r == 0 || sbytes > 0) return (hs_ssize_t)sbytes;
    if (errno != EINTR) return -errno;
  }

#else
  /* Portable fallback: one pread + send loop. Correct on any POSIX
   * platform without a native sendfile (OpenBSD, NetBSD, Solaris, ...). */
  char buf[65536];
  size_t want = count < sizeof(buf) ? count : sizeof(buf);
  ssize_t r;
  do {
    r = pread(file_fd, buf, want, (off_t)offset);
  } while (r < 0 && errno == EINTR);
  if (r < 0) return -errno;
  if (r == 0) return 0;
  size_t total = 0;
  while (total < (size_t)r) {
    ssize_t n;
    do {
      n = send(sock, buf + total, (size_t)r - total, MSG_NOSIGNAL);
    } while (n < 0 && errno == EINTR);
    if (n < 0) return total > 0 ? (hs_ssize_t)total : -errno;
    if (n == 0) break;
    total += (size_t)n;
  }
  return (hs_ssize_t)total;
#endif
}
