/* wireform_iouring.h
 *
 * Tiny self-contained io_uring wrapper used by the Haskell-side
 * `Kafka.Network.IoUring` module. We intentionally do /not/
 * depend on liburing: io_uring's userspace ABI is stable since
 * Linux 5.1 and the syscalls / mmap layout are all documented
 * in `linux/io_uring.h`, so we issue them directly via the
 * `syscall(2)` wrapper. That keeps the dependency surface small
 * (libc + kernel headers) and avoids a runtime link against
 * `-luring` which isn't installed on every distribution.
 *
 * On non-Linux platforms the implementation in
 * `wireform_iouring_stub.c` is compiled instead, exposing the
 * same symbol names but always returning "not supported" so the
 * Haskell side can probe at runtime and degrade gracefully.
 */

#ifndef WIREFORM_IOURING_H
#define WIREFORM_IOURING_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct wf_iouring_ring wf_iouring_ring;

/* Returns 1 if the current kernel supports io_uring (the
 * io_uring_setup syscall returns a valid fd for a 1-entry
 * ring), 0 otherwise. Safe to call without ever opening a ring.
 *
 * Possible "not supported" causes:
 *   * Linux kernel < 5.1.
 *   * io_uring disabled via `kernel.io_uring_disabled` sysctl
 *     (Linux >= 6.6) — common on hardened distros.
 *   * seccomp / AppArmor / SELinux denying io_uring_setup.
 *   * Non-Linux host (stub returns 0). */
int wf_iouring_is_supported(void);

/* Open a ring with @entries submission slots. Returns NULL on
 * failure (kernel rejected the setup, mmap failed, OOM, ...).
 * Sets errno on the failure path so the caller can surface a
 * useful error from the Haskell side. */
wf_iouring_ring* wf_iouring_open(unsigned int entries);

/* Close a ring opened with wf_iouring_open(). Safe to call with
 * a NULL pointer. */
void wf_iouring_close(wf_iouring_ring* r);

/* Submit a single SEND op for @fd over @buf[0..len) and wait
 * for completion. Returns:
 *   * >= 0 : number of bytes sent (== len on success);
 *   *  < 0 : -errno from the io_uring CQE (or -1 on the stub /
 *           if the io_uring_enter syscall itself failed before
 *           submission).
 *
 * Blocks the calling OS thread until the CQE arrives. Callers
 * must hold an external mutex if the ring is shared between
 * threads (the Haskell side serialises via 'MVar'). */
int64_t wf_iouring_send(wf_iouring_ring* r, int fd,
                        const void* buf, size_t len);

/* Submit a single RECV op for @fd into @buf[0..len) and wait
 * for completion. Returns:
 *   * >  0 : bytes received;
 *   * == 0 : EOF (peer half-closed);
 *   *  < 0 : -errno from the io_uring CQE.
 *
 * Same threading caveats as wf_iouring_send. */
int64_t wf_iouring_recv(wf_iouring_ring* r, int fd,
                        void* buf, size_t len);

#ifdef __cplusplus
}
#endif

#endif /* WIREFORM_IOURING_H */
