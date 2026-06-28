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
 * but the Haskell binding for this path is Unix-only — see header note).
 */
hs_ssize_t hs_http1_send_more(int sock, const void *buf, size_t len) {
#if defined(_WIN32)
  int n = send((SOCKET)sock, (const char *)buf, (int)len, 0);
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
 * and suppressing SIGPIPE (MSG_NOSIGNAL) so a closed peer doesn't kill
 * the process. Returns total bytes sent, or -errno on the first failure.
 */
hs_ssize_t hs_http1_send_all(int sock, const void *buf, size_t len) {
  const char *p = (const char *)buf;
  size_t remaining = len;
  while (remaining > 0) {
#if defined(_WIN32)
    int n = send((SOCKET)sock, p, (int)remaining, 0);
    if (n == SOCKET_ERROR) return -(hs_ssize_t)WSAGetLastError();
#else
    ssize_t n;
    do {
      n = send(sock, p, remaining, MSG_NOSIGNAL);
    } while (n < 0 && errno == EINTR);
    if (n < 0) return -errno;
#endif
    if (n == 0) break; /* peer closed */
    p += n;
    remaining -= (size_t)n;
  }
  return (hs_ssize_t)(len - remaining);
}

/* One zero-copy (where available) file->socket transfer of up to `count`
 * bytes starting at `offset`. See the header comment for the per-platform
 * primitive used. Returns bytes sent (>= 0), 0 on premature EOF, or
 * -errno on failure. EINTR is retried internally.
 */
hs_ssize_t hs_http1_sendfile(int sock, int file_fd, int64_t offset, size_t count) {
#if defined(_WIN32)
  HANDLE hFile = (HANDLE)_get_osfhandle(file_fd);
  if (hFile == INVALID_HANDLE_VALUE) return -1;
  /* TransmitFile reads from the file position given by the OVERLAPPED
   * Offset fields; on a blocking socket the call completes synchronously. */
  OVERLAPPED ov;
  ZeroMemory(&ov, sizeof(ov));
  ov.Offset = (DWORD)((uint64_t)offset & 0xFFFFFFFFu);
  ov.OffsetHigh = (DWORD)(((uint64_t)offset >> 32) & 0xFFFFFFFFu);
  DWORD toSend = count > 0x7FFFFFFFu ? 0x7FFFFFFFu : (DWORD)count;
  BOOL ok = TransmitFile((SOCKET)sock, hFile, toSend, 0, &ov, NULL, 0);
  if (!ok) return -(hs_ssize_t)WSAGetLastError();
  return (hs_ssize_t)toSend;

#elif defined(__linux__)
  /* ssize_t sendfile(int out_fd, int in_fd, off_t *offset, size_t count); */
  off_t off = (off_t)offset;
  ssize_t n;
  do {
    n = sendfile(sock, file_fd, &off, count);
  } while (n < 0 && errno == EINTR);
  if (n < 0) return -errno;
  return n;

#elif defined(__APPLE__)
  /* int sendfile(int fd, int s, off_t offset, off_t *len,
   *              struct sf_hdtr *hdtr, int flags);
   * fd = source file, s = socket (note: order is the reverse of Linux).
   * *len is in/out: requested in, actually-sent out. */
  off_t len;
  int r;
  do {
    len = (off_t)count;
    r = sendfile(file_fd, sock, (off_t)offset, &len, NULL, 0);
  } while (r < 0 && errno == EINTR);
  /* On EAGAIN macOS still reports the partial byte count in *len. */
  if (r < 0 && errno != EAGAIN) return -errno;
  return (hs_ssize_t)len;

#elif defined(__FreeBSD__) || defined(__DragonFly__)
  /* int sendfile(int fd, int s, off_t offset, size_t nbytes,
   *              struct sf_hdtr *hdtr, off_t *sbytes, int flags); */
  off_t sbytes;
  int r;
  do {
    sbytes = 0;
    r = sendfile(file_fd, sock, (off_t)offset, count, NULL, &sbytes, 0);
  } while (r < 0 && errno == EINTR);
  if (r < 0 && errno != EAGAIN) return -errno;
  return (hs_ssize_t)sbytes;

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
