/*
 * Syscall fault injection for the `faults` variant of core-magic-ring.
 *
 * module.mk force-includes this header (-include) into every translation
 * unit of the variant, the kernel source included. It pulls in the real
 * declarations first, then redirects the calls the kernel makes
 * (shm_open, memfd_create via syscall, ftruncate, mmap, munmap) to
 * wrappers defined in fault_inject_impl.h. A wrapper passes through to
 * the real call unless armed with wf_fi_arm, in which case the chosen
 * calls fail with the chosen errno. The test and fuzz programs include
 * fault_inject_impl.h once to provide the definitions.
 */
#ifndef WF_FAULT_INJECT_H
#define WF_FAULT_INJECT_H

#if defined(__linux__) && !defined(_GNU_SOURCE)
#  define _GNU_SOURCE
#endif

#include <sys/types.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>
#include <stddef.h>

enum wf_fi_op {
    WF_FI_OPEN,      /* shm_open (posix) / memfd_create (linux) */
    WF_FI_FTRUNCATE,
    WF_FI_MMAP,
    WF_FI_NOPS
};

/* Make calls nth .. nth+count-1 (1-based, counted from now) of `op` fail
 * with errno `err`. Resets every op's call counter. */
void wf_fi_arm(enum wf_fi_op op, int nth, int count, int err);
/* Disarm everything and reset counters. */
void wf_fi_reset(void);
/* Calls of `op` since the last arm/reset. */
int wf_fi_calls(enum wf_fi_op op);

/* Every successful PROT_NONE reservation and every munmap is recorded, so
 * a test can check an error path released what it reserved. */
extern void *wf_fi_last_reserve;
extern size_t wf_fi_last_reserve_len;
extern void *wf_fi_last_unmap;
extern size_t wf_fi_last_unmap_len;

int wf_fi_shm_open(const char *name, int oflag, ...);
long wf_fi_syscall(long nr, ...);
int wf_fi_ftruncate(int fd, off_t len);
void *wf_fi_mmap(void *addr, size_t len, int prot, int flags, int fd, off_t off);
int wf_fi_munmap(void *addr, size_t len);

#define shm_open(...) wf_fi_shm_open(__VA_ARGS__)
#define syscall(...) wf_fi_syscall(__VA_ARGS__)
#define ftruncate(...) wf_fi_ftruncate(__VA_ARGS__)
#define mmap(...) wf_fi_mmap(__VA_ARGS__)
#define munmap(...) wf_fi_munmap(__VA_ARGS__)

#endif
