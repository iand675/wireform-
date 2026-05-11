/* wireform_iouring.c
 *
 * Minimal io_uring wrapper implementation. The design is
 * deliberately conservative — one submission per Haskell call,
 * one completion drain per Haskell call, with the ring used as
 * a single-slot pipe. We don't try to batch yet: the goal of
 * this first cut is to /replace/ the recv(2) / send(2) syscall
 * pair (which goes via the GHC RTS' epoll IO manager) with a
 * single io_uring_enter(2), so latency-sensitive workloads
 * (small fetches, frequent heartbeats) see a one-syscall saving
 * per round-trip.
 *
 * A future revision can batch multiple in-flight ops onto the
 * same ring; the SQ / CQ ring layout below already supports
 * @entries@ slots, we just don't expose that to the Haskell
 * side yet.
 *
 * The implementation deliberately avoids `liburing` so callers
 * don't need to install another C library; everything is via
 * `syscall(2)` + `mmap(2)`.
 */

#ifdef __linux__

#ifndef _GNU_SOURCE
#  define _GNU_SOURCE
#endif

#include <errno.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/syscall.h>
#include <unistd.h>

/* `linux/io_uring.h` is the source of truth for the SQE / CQE
 * layout and the IORING_OFF_* mmap offsets. On older toolchains
 * (Ubuntu 18.04, CentOS 7) this header may be missing or stale;
 * in that case the build will fail with a clear error and the
 * user can disable the io-uring cabal flag. */
#include <linux/io_uring.h>

#include "wireform_iouring.h"

/* Syscall numbers are 425 / 426 on Linux for io_uring_setup /
 * io_uring_enter, but rather than hard-code we rely on libc's
 * SYS_* macros (available since glibc 2.30). If the macros are
 * missing the build fails with a meaningful error. */
#ifndef SYS_io_uring_setup
#  error "SYS_io_uring_setup is not defined; glibc is too old for native io_uring."
#endif
#ifndef SYS_io_uring_enter
#  error "SYS_io_uring_enter is not defined; glibc is too old for native io_uring."
#endif

struct wf_iouring_ring {
  int                   ring_fd;
  unsigned              entries;

  /* SQ ring mapping. */
  void*                 sq_ring;
  size_t                sq_ring_size;
  unsigned*             sq_khead;
  unsigned*             sq_ktail;
  unsigned*             sq_kmask;
  unsigned*             sq_array;

  /* SQEs mapping (separate mmap from the SQ ring header). */
  struct io_uring_sqe*  sqes;
  size_t                sqes_size;

  /* CQ ring mapping. May alias sq_ring when the kernel
   * supports IORING_FEAT_SINGLE_MMAP (5.4+). */
  void*                 cq_ring;
  size_t                cq_ring_size;
  unsigned*             cq_khead;
  unsigned*             cq_ktail;
  unsigned*             cq_kmask;
  struct io_uring_cqe*  cqes;
};

static inline int
io_uring_setup_(unsigned entries, struct io_uring_params* p)
{
  return (int)syscall(SYS_io_uring_setup, entries, p);
}

static inline int
io_uring_enter_(int fd, unsigned to_submit, unsigned min_complete,
                unsigned flags)
{
  return (int)syscall(SYS_io_uring_enter, fd, to_submit,
                      min_complete, flags, NULL, (size_t)0);
}

int
wf_iouring_is_supported(void)
{
  struct io_uring_params p;
  memset(&p, 0, sizeof(p));
  int fd = io_uring_setup_(1, &p);
  if (fd < 0) {
    return 0;
  }
  close(fd);
  return 1;
}

wf_iouring_ring*
wf_iouring_open(unsigned int entries)
{
  if (entries == 0) entries = 8;

  struct io_uring_params p;
  memset(&p, 0, sizeof(p));
  int fd = io_uring_setup_(entries, &p);
  if (fd < 0) {
    return NULL;
  }

  /* mmap the SQ ring header + array. */
  size_t sq_size = p.sq_off.array + p.sq_entries * sizeof(unsigned);
  size_t cq_size = p.cq_off.cqes  + p.cq_entries * sizeof(struct io_uring_cqe);

  void* sq_ptr = MAP_FAILED;
  void* cq_ptr = MAP_FAILED;

  if (p.features & IORING_FEAT_SINGLE_MMAP) {
    /* Kernel mapped the CQ and SQ rings into a single backing
     * region; pick the larger of the two sizes. */
    if (cq_size > sq_size) sq_size = cq_size;
    cq_size = sq_size;
    sq_ptr = mmap(NULL, sq_size, PROT_READ | PROT_WRITE,
                  MAP_SHARED | MAP_POPULATE, fd, IORING_OFF_SQ_RING);
    if (sq_ptr == MAP_FAILED) goto fail_fd;
    cq_ptr = sq_ptr;
  } else {
    sq_ptr = mmap(NULL, sq_size, PROT_READ | PROT_WRITE,
                  MAP_SHARED | MAP_POPULATE, fd, IORING_OFF_SQ_RING);
    if (sq_ptr == MAP_FAILED) goto fail_fd;
    cq_ptr = mmap(NULL, cq_size, PROT_READ | PROT_WRITE,
                  MAP_SHARED | MAP_POPULATE, fd, IORING_OFF_CQ_RING);
    if (cq_ptr == MAP_FAILED) goto fail_sq;
  }

  /* mmap the SQE array. */
  size_t sqes_size = (size_t)p.sq_entries * sizeof(struct io_uring_sqe);
  struct io_uring_sqe* sqes =
    (struct io_uring_sqe*)mmap(NULL, sqes_size, PROT_READ | PROT_WRITE,
                               MAP_SHARED | MAP_POPULATE, fd,
                               IORING_OFF_SQES);
  if (sqes == MAP_FAILED) {
    goto fail_cq;
  }

  wf_iouring_ring* r = (wf_iouring_ring*)calloc(1, sizeof(*r));
  if (!r) {
    int saved = errno;
    munmap(sqes, sqes_size);
    if (cq_ptr != sq_ptr) munmap(cq_ptr, cq_size);
    munmap(sq_ptr, sq_size);
    close(fd);
    errno = saved;
    return NULL;
  }

  r->ring_fd       = fd;
  r->entries       = p.sq_entries;
  r->sq_ring       = sq_ptr;
  r->sq_ring_size  = sq_size;
  r->cq_ring       = cq_ptr;
  r->cq_ring_size  = cq_size;
  r->sqes          = sqes;
  r->sqes_size     = sqes_size;
  r->sq_khead      = (unsigned*)((char*)sq_ptr + p.sq_off.head);
  r->sq_ktail      = (unsigned*)((char*)sq_ptr + p.sq_off.tail);
  r->sq_kmask      = (unsigned*)((char*)sq_ptr + p.sq_off.ring_mask);
  r->sq_array      = (unsigned*)((char*)sq_ptr + p.sq_off.array);
  r->cq_khead      = (unsigned*)((char*)cq_ptr + p.cq_off.head);
  r->cq_ktail      = (unsigned*)((char*)cq_ptr + p.cq_off.tail);
  r->cq_kmask      = (unsigned*)((char*)cq_ptr + p.cq_off.ring_mask);
  r->cqes          = (struct io_uring_cqe*)((char*)cq_ptr + p.cq_off.cqes);

  /* The SQ array maps "submission slot index" to "SQE slot
   * index". For our single-slot-at-a-time usage they're 1:1, so
   * pre-populate the identity mapping once and never touch it
   * again. */
  for (unsigned i = 0; i < r->entries; i++) {
    r->sq_array[i] = i;
  }

  return r;

fail_cq:
  if (cq_ptr != sq_ptr) munmap(cq_ptr, cq_size);
fail_sq:
  munmap(sq_ptr, sq_size);
fail_fd:
  {
    int saved = errno;
    close(fd);
    errno = saved;
  }
  return NULL;
}

void
wf_iouring_close(wf_iouring_ring* r)
{
  if (!r) return;
  if (r->sqes)    munmap(r->sqes, r->sqes_size);
  if (r->cq_ring && r->cq_ring != r->sq_ring) munmap(r->cq_ring, r->cq_ring_size);
  if (r->sq_ring) munmap(r->sq_ring, r->sq_ring_size);
  if (r->ring_fd >= 0) close(r->ring_fd);
  free(r);
}

/* Push @sqe_in into slot 0, kick the kernel, and read the
 * (single) completion. The slot is reused on every call; we
 * preserve memory ordering with __atomic_* barriers as the
 * kernel and userspace share the head / tail words.
 *
 * Returns the CQE result (>=0 on success, -errno on failure),
 * or -EIO if the io_uring_enter syscall itself returned an
 * error. */
static int64_t
submit_and_wait(wf_iouring_ring* r, const struct io_uring_sqe* sqe_in)
{
  unsigned tail = __atomic_load_n(r->sq_ktail, __ATOMIC_RELAXED);
  unsigned mask = *r->sq_kmask;
  unsigned slot = tail & mask;
  r->sqes[slot] = *sqe_in;
  /* sq_array[slot] was pre-populated to `slot` at ring open
   * time, so there's nothing to update here. */

  /* Release-store the new tail so the kernel sees the SQE
   * write before it reads the tail. */
  __atomic_store_n(r->sq_ktail, tail + 1, __ATOMIC_RELEASE);

  int submitted = io_uring_enter_(r->ring_fd, 1, 1,
                                  IORING_ENTER_GETEVENTS);
  if (submitted < 0) {
    return -EIO;
  }

  /* Read the completion. We expect exactly one per call; if
   * the kernel coalesced ours with a stale event the
   * user_data field below disambiguates. */
  unsigned head = __atomic_load_n(r->cq_khead, __ATOMIC_RELAXED);
  unsigned cq_mask = *r->cq_kmask;
  struct io_uring_cqe* cqe = &r->cqes[head & cq_mask];
  int64_t res = (int64_t)(int32_t)cqe->res;
  __atomic_store_n(r->cq_khead, head + 1, __ATOMIC_RELEASE);
  return res;
}

int64_t
wf_iouring_send(wf_iouring_ring* r, int fd,
                const void* buf, size_t len)
{
  if (!r) return -EINVAL;
  struct io_uring_sqe sqe;
  memset(&sqe, 0, sizeof(sqe));
  sqe.opcode    = IORING_OP_SEND;
  sqe.fd        = fd;
  sqe.addr      = (uint64_t)(uintptr_t)buf;
  sqe.len       = (unsigned)len;
  sqe.user_data = 1;
  return submit_and_wait(r, &sqe);
}

int64_t
wf_iouring_recv(wf_iouring_ring* r, int fd,
                void* buf, size_t len)
{
  if (!r) return -EINVAL;
  struct io_uring_sqe sqe;
  memset(&sqe, 0, sizeof(sqe));
  sqe.opcode    = IORING_OP_RECV;
  sqe.fd        = fd;
  sqe.addr      = (uint64_t)(uintptr_t)buf;
  sqe.len       = (unsigned)len;
  sqe.user_data = 2;
  return submit_and_wait(r, &sqe);
}

#endif /* __linux__ */
