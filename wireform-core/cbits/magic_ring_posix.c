#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdatomic.h>
#include "magic_ring.h"

/* Filled lazily. Several capabilities can call in at once, so the cache is
 * an atomic: every racer stores the same value, relaxed order suffices. */
static _Atomic long cached_page_size = 0;

long hs_page_size(void) {
    long ps = atomic_load_explicit(&cached_page_size, memory_order_relaxed);
    if (ps == 0) {
        ps = sysconf(_SC_PAGESIZE);
        atomic_store_explicit(&cached_page_size, ps, memory_order_relaxed);
    }
    return ps;
}

/* Round up to the next power of two >= val, for 1 <= val <= RING_MAX_SIZE. */
static size_t next_pow2(size_t val) {
    val--;
    val |= val >> 1;
    val |= val >> 2;
    val |= val >> 4;
    val |= val >> 8;
    val |= val >> 16;
#if SIZE_MAX > 0xFFFFFFFFu
    val |= val >> 32;
#endif
    val++;
    return val;
}

/* Largest ring: a power of two whose 2N reservation still fits in size_t
 * (and in off_t and the Haskell Int that receives the size). Larger
 * requests would wrap the rounding arithmetic. */
#define RING_MAX_SIZE ((size_t)1 << (sizeof(size_t) * 8 - 2))

/* Close fd on an error path without clobbering the errno being reported. */
static int fail_close(int fd) {
    int saved_errno = errno;
    close(fd);
    errno = saved_errno;
    return -1;
}

#if !defined(__FreeBSD__) || !defined(SHM_ANON)
static _Atomic long shm_counter = 0;

/* A name left behind by a process that died between shm_open and
 * shm_unlink (and whose pid has been reused) makes O_EXCL fail with
 * EEXIST; move on to the next counter value rather than fail. */
#define SHM_OPEN_ATTEMPTS 16
#endif

int hs_ring_create(size_t requested, struct hs_ring *out) {
    long ps = hs_page_size();
    size_t size;
    int fd;
    void *base, *m1, *m2;
    int saved_errno;

    if (requested > RING_MAX_SIZE) {
        errno = ENOMEM;
        return -1;
    }
    if (requested < (size_t)ps)
        requested = (size_t)ps;

    size = (requested + (size_t)ps - 1) & ~((size_t)ps - 1);
    size = next_pow2(size);

#if defined(__FreeBSD__) && defined(SHM_ANON)
    fd = shm_open(SHM_ANON, O_RDWR, 0600);
#else
    for (int attempt = 0;; attempt++) {
        char name[64];
        snprintf(name, sizeof(name), "/wfring.%d.%ld",
                 getpid(), atomic_fetch_add(&shm_counter, 1));
        fd = shm_open(name, O_RDWR | O_CREAT | O_EXCL, 0600);
        if (fd >= 0) {
            shm_unlink(name);
            break;
        }
        if (errno != EEXIST || attempt + 1 >= SHM_OPEN_ATTEMPTS)
            break;
    }
#endif

    if (fd < 0)
        return -1;

    if (ftruncate(fd, (off_t)size) != 0)
        return fail_close(fd);

    base = mmap(NULL, size * 2, PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (base == MAP_FAILED)
        return fail_close(fd);

    m1 = mmap(base, size, PROT_READ | PROT_WRITE,
              MAP_SHARED | MAP_FIXED, fd, 0);
    if (m1 == MAP_FAILED) {
        saved_errno = errno;
        munmap(base, size * 2);
        errno = saved_errno;
        return fail_close(fd);
    }

    m2 = mmap((char *)base + size, size, PROT_READ | PROT_WRITE,
              MAP_SHARED | MAP_FIXED, fd, 0);
    if (m2 == MAP_FAILED) {
        saved_errno = errno;
        munmap(base, size * 2);
        errno = saved_errno;
        return fail_close(fd);
    }

    close(fd);
    memset(base, 0, size);

    out->base = base;
    out->size = size;
    return 0;
}

/* Idempotent for one struct: clears base/size, so a second call on the
 * same struct is a no-op. A copy of the struct is NOT protected. */
void hs_ring_destroy(struct hs_ring *ring) {
    if (ring && ring->base) {
        munmap(ring->base, ring->size * 2);
        ring->base = NULL;
        ring->size = 0;
    }
}
