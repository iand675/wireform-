/*
 * wireform_affinity.c
 *
 * CPU-affinity helper backing the `foreign import ccall
 * "sched_setaffinity_single"` in Wireform.Network.Transport.Profile.
 *
 * That foreign import had no C definition, so any build that resolves
 * symbols eagerly (e.g. a dynamic / shared-object build, or the GHC
 * interpreter dlopen'ing the package during a Template Haskell splice)
 * failed with `undefined symbol: sched_setaffinity_single`. Static
 * builds happened to get away with it because archive members are only
 * pulled in on demand and `c_pin_thread` is rarely referenced.
 *
 * The helper pins the *calling thread* (pid 0) to a single core. It is
 * Linux-only: the cabal file compiles this translation unit only on
 * Linux, matching the `#if defined(linux_HOST_OS)` guard around the
 * Haskell foreign import.
 */

#define _GNU_SOURCE
#include <errno.h>
#include <sched.h>

#include "HsFFI.h"

/* Largest core index accepted: the kernel's CPU mask limit is far below. */
#define AFFINITY_MAX_CORE (1 << 20)

/* Returns 0 on success, or -1 with errno set on failure: EINVAL for a
 * negative or absurd core index (the argument is an arbitrary Haskell
 * Int, and truncating it to int used to pin core 2^32 + k to core k),
 * otherwise whatever sched_setaffinity reports (EINVAL for a core that
 * does not exist or is outside the allowed set). Cores past the static
 * cpu_set_t (CPU_SETSIZE, 1024) use a dynamically sized set. HsInt is
 * used for both the argument and the result so the C ABI matches the
 * Haskell `Int -> IO Int` foreign import exactly. */
HsInt sched_setaffinity_single(HsInt core)
{
    if (core < 0 || core >= AFFINITY_MAX_CORE) {
        errno = EINVAL;
        return -1;
    }
    int cpu = (int) core;
    if (cpu < CPU_SETSIZE) {
        cpu_set_t set;
        CPU_ZERO(&set);
        CPU_SET(cpu, &set);
        return (HsInt) sched_setaffinity(0, sizeof(set), &set);
    }
    cpu_set_t *set = CPU_ALLOC(cpu + 1);
    if (!set) {
        errno = ENOMEM;
        return -1;
    }
    size_t size = CPU_ALLOC_SIZE(cpu + 1);
    CPU_ZERO_S(size, set);
    CPU_SET_S(cpu, size, set);
    int rc = sched_setaffinity(0, size, set);
    int saved_errno = errno;
    CPU_FREE(set);
    errno = saved_errno;
    return (HsInt) rc;
}
