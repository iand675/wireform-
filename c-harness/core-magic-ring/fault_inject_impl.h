/*
 * Definitions for fault_inject.h. Included exactly once per program (by
 * the test or fuzz source) when WF_FI is defined. Real calls are written
 * as `(name)(...)` so the function-like redirect macros do not apply.
 */
#ifndef WF_FAULT_INJECT_IMPL_H
#define WF_FAULT_INJECT_IMPL_H

#include <errno.h>
#include <stdarg.h>
#include <stdlib.h>
#if defined(__linux__)
#  include <sys/syscall.h>
#endif

void *wf_fi_last_reserve;
size_t wf_fi_last_reserve_len;
void *wf_fi_last_unmap;
size_t wf_fi_last_unmap_len;

static struct {
    int nth, count, err, calls;
} wf_fi__ops[WF_FI_NOPS];

void wf_fi_reset(void)
{
    memset(wf_fi__ops, 0, sizeof wf_fi__ops);
    wf_fi_last_reserve = wf_fi_last_unmap = NULL;
    wf_fi_last_reserve_len = wf_fi_last_unmap_len = 0;
}

void wf_fi_arm(enum wf_fi_op op, int nth, int count, int err)
{
    wf_fi_reset();
    wf_fi__ops[op].nth = nth;
    wf_fi__ops[op].count = count;
    wf_fi__ops[op].err = err;
}

int wf_fi_calls(enum wf_fi_op op)
{
    return wf_fi__ops[op].calls;
}

/* Count the call; return nonzero (with errno set) if it must fail. */
static int wf_fi__fail(enum wf_fi_op op)
{
    int k = ++wf_fi__ops[op].calls;
    if (wf_fi__ops[op].nth && k >= wf_fi__ops[op].nth
        && k < wf_fi__ops[op].nth + wf_fi__ops[op].count) {
        errno = wf_fi__ops[op].err;
        return 1;
    }
    return 0;
}

int wf_fi_shm_open(const char *name, int oflag, ...)
{
    va_list ap;
    va_start(ap, oflag);
    int mode = (oflag & O_CREAT) ? va_arg(ap, int) : 0;
    va_end(ap);
    if (wf_fi__fail(WF_FI_OPEN)) return -1;
    return (shm_open)(name, oflag, (mode_t)mode);
}

long wf_fi_syscall(long nr, ...)
{
#if defined(__linux__)
    va_list ap;
    va_start(ap, nr);
    if (nr != SYS_memfd_create) {
        va_end(ap);
        fprintf(stderr, "wf_fi_syscall: unexpected syscall %ld\n", nr);
        abort();
    }
    const char *name = va_arg(ap, const char *);
    unsigned flags = va_arg(ap, unsigned);
    va_end(ap);
    if (wf_fi__fail(WF_FI_OPEN)) return -1;
    return (syscall)(nr, name, flags);
#else
    fprintf(stderr, "wf_fi_syscall: unexpected syscall %ld\n", nr);
    abort();
#endif
}

int wf_fi_ftruncate(int fd, off_t len)
{
    if (wf_fi__fail(WF_FI_FTRUNCATE)) return -1;
    return (ftruncate)(fd, len);
}

void *wf_fi_mmap(void *addr, size_t len, int prot, int flags, int fd, off_t off)
{
    if (wf_fi__fail(WF_FI_MMAP)) return MAP_FAILED;
    void *p = (mmap)(addr, len, prot, flags, fd, off);
    if (p != MAP_FAILED && prot == PROT_NONE) {
        wf_fi_last_reserve = p;
        wf_fi_last_reserve_len = len;
    }
    return p;
}

int wf_fi_munmap(void *addr, size_t len)
{
    wf_fi_last_unmap = addr;
    wf_fi_last_unmap_len = len;
    return (munmap)(addr, len);
}

#endif
