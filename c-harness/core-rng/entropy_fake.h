/* Interposed entropy source for the core-rng harness.
 *
 * fast_rng.c seeds from syscall(SYS_getrandom, ...) on Linux and
 * arc4random_buf on Darwin. This header defines those symbols in the test
 * executable, which the static link binds fast_rng.o to. By default
 * (fe.mode == 0) they forward to the real libc function, so the rest of the
 * process is unaffected; with fe.mode == 1 they serve fe.bytes (cyclically
 * from fe.pos) so the kernel's seed is known and its stream can be checked
 * against the reference. The Linux fake can also inject EINTR, failures,
 * zero / over-long returns and short reads. All of it is per thread.
 *
 * Include exactly once per program, after defining _GNU_SOURCE (RTLD_NEXT)
 * before any system header. */
#ifndef CORE_RNG_ENTROPY_FAKE_H
#define CORE_RNG_ENTROPY_FAKE_H

#include <dlfcn.h>
#include <errno.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#if defined(__linux__)
#include <sys/syscall.h>
#endif
#if defined(__has_feature)
#  if __has_feature(memory_sanitizer)
#    include <sanitizer/msan_interface.h>
#    define FE_MSAN 1
#  endif
#endif

typedef struct {
    int mode;          /* 0: forward to the real source; 1: fake */
    uint8_t bytes[32]; /* fake seed bytes */
    size_t pos;        /* next byte served (mod 32) */
    int calls;         /* entropy requests seen on this thread, either mode */
    /* Linux-only fault injection, consumed in this order: */
    int eintr;         /* fail with EINTR this many times */
    int fail_errno;    /* nonzero: fail with this errno (sticky) */
    int ret_zero;      /* nonzero: return 0 (sticky) */
    int overreport;    /* nonzero: claim one byte more than asked (sticky) */
    size_t chunk;      /* nonzero: serve at most this many bytes per call */
} fake_entropy;

static __thread fake_entropy fe;

static void fe_serve(void *buf, size_t n)
{
    uint8_t *b = buf;
    for (size_t k = 0; k < n; k++) b[k] = fe.bytes[(fe.pos + k) % 32];
    fe.pos += n;
}

/* Fake mode with a fresh seed and no injected faults. */
static void fe_set_seed(const uint64_t s[4])
{
    int calls = fe.calls;
    memset(&fe, 0, sizeof fe);
    fe.calls = calls;
    memcpy(fe.bytes, s, 32);
    fe.mode = 1;
}

#if defined(__linux__)
typedef long (*fe_syscall_fn)(long, ...);

long syscall(long nr, ...)
{
    static fe_syscall_fn real;
    long a[6];
    va_list ap;
    va_start(ap, nr);
    for (int i = 0; i < 6; i++) a[i] = va_arg(ap, long);
    va_end(ap);
#ifdef FE_MSAN
    __msan_unpoison(a, sizeof a);
#endif
    if (nr == SYS_getrandom) fe.calls++;
    if (nr != SYS_getrandom || !fe.mode) {
        fe_syscall_fn r = __atomic_load_n(&real, __ATOMIC_ACQUIRE);
        if (!r) {
            r = (fe_syscall_fn)dlsym(RTLD_NEXT, "syscall");
            if (!r) abort();
            __atomic_store_n(&real, r, __ATOMIC_RELEASE);
        }
        if (nr == SYS_getrandom) return r(nr, a[0], a[1], a[2]);
        return r(nr, a[0], a[1], a[2], a[3], a[4], a[5]);
    }
    if (fe.eintr > 0) {
        fe.eintr--;
        errno = EINTR;
        return -1;
    }
    if (fe.fail_errno) {
        errno = fe.fail_errno;
        return -1;
    }
    if (fe.ret_zero) return 0;
    size_t n = (size_t)a[1];
    if (fe.chunk && n > fe.chunk) n = fe.chunk;
    fe_serve((void *)a[0], n);
    return (long)n + (fe.overreport ? 1 : 0);
}
#define FE_FAULTS 1
#elif defined(__APPLE__)
typedef void (*fe_arc4_fn)(void *, size_t);

void arc4random_buf(void *buf, size_t n)
{
    static fe_arc4_fn real;
    fe.calls++;
    if (!fe.mode) {
        fe_arc4_fn r = __atomic_load_n(&real, __ATOMIC_ACQUIRE);
        if (!r) {
            r = (fe_arc4_fn)dlsym(RTLD_NEXT, "arc4random_buf");
            if (!r) abort();
            __atomic_store_n(&real, r, __ATOMIC_RELEASE);
        }
        r(buf, n);
        return;
    }
    fe_serve(buf, n);
}
#define FE_FAULTS 0
#else
#error "core-rng harness: no entropy interposer for this platform"
#endif

#endif
