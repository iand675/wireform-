/* Tests for sched_setaffinity_single (Linux). */
#define _GNU_SOURCE
#include "harness.h"
#include "api.h"

#include <errno.h>
#include <pthread.h>
#include <sched.h>

static cpu_set_t original;

static int allowed_cpu(int k)
{
    int seen = 0;
    for (int c = 0; c < CPU_SETSIZE; c++) {
        if (CPU_ISSET(c, &original) && seen++ == k) return c;
    }
    return -1;
}

static void restore(void)
{
    if (sched_setaffinity(0, sizeof original, &original) != 0) {
        perror("restore affinity");
        abort();
    }
}

static int mask_is_only(int cpu)
{
    cpu_set_t now;
    if (sched_getaffinity(0, sizeof now, &now) != 0) return 0;
    return CPU_COUNT(&now) == 1 && CPU_ISSET(cpu, &now);
}

static int mask_is_original(void)
{
    cpu_set_t now;
    if (sched_getaffinity(0, sizeof now, &now) != 0) return 0;
    return CPU_EQUAL(&now, &original);
}

/* Every allowed core (first 16) can be pinned, and the mask is then
 * exactly that core. */
static void pins_allowed_cores(void)
{
    for (int k = 0; k < 16; k++) {
        int cpu = allowed_cpu(k);
        if (cpu < 0) break;
        errno = 0;
        WF_CHECK_EQ(sched_setaffinity_single(cpu), 0);
        WF_CHECK(mask_is_only(cpu));
        restore();
    }
}

/* Out-of-range indices fail with EINVAL and leave the mask alone. Before
 * the fix, (HsInt)2^32 + k was truncated to int and pinned core k. */
static void rejects_bad_cores(void)
{
    int c0 = allowed_cpu(0);
    HsInt bad[] = {
        -1, -2, -1024, INTPTR_MIN, INTPTR_MIN + 1,
        (HsInt)1 << 20, ((HsInt)1 << 20) + 1, INTPTR_MAX,
#if INTPTR_MAX > 0x7FFFFFFF
        ((HsInt)1 << 32) + c0, ((HsInt)1 << 32) + c0 + ((HsInt)1 << 40),
        (HsInt)0xFFFFFFFF, ((HsInt)1 << 31) + c0,
#endif
    };
    for (size_t i = 0; i < sizeof bad / sizeof bad[0]; i++) {
        errno = 0;
        WF_CHECK_EQ(sched_setaffinity_single(bad[i]), -1);
        WF_CHECK_EQ(errno, EINVAL);
        WF_CHECK(mask_is_original());
        restore();
    }
}

/* In-range indices the kernel refuses (no such CPU, or not allowed),
 * including ones past the static cpu_set_t that need a dynamic set. */
static void kernel_refusals(void)
{
    long nconf = sysconf(_SC_NPROCESSORS_CONF);
    HsInt cores[] = { CPU_SETSIZE - 1, CPU_SETSIZE, CPU_SETSIZE + 1, 4095, 65535,
                      ((HsInt)1 << 20) - 1 };
    for (size_t i = 0; i < sizeof cores / sizeof cores[0]; i++) {
        if (cores[i] < nconf) continue; /* might exist */
        errno = 0;
        WF_CHECK_EQ(sched_setaffinity_single(cores[i]), -1);
        WF_CHECK_EQ(errno, EINVAL);
        WF_CHECK(mask_is_original());
        restore();
    }
    /* A core below CPU_SETSIZE that exists but is not in our allowed set,
     * if there is one, is refused by the kernel. */
    for (int c = 0; c < CPU_SETSIZE && c < nconf; c++) {
        if (!CPU_ISSET(c, &original)) {
            int rc = (int)sched_setaffinity_single(c);
            if (rc == 0) {
                /* Allowed after all (cgroup changed under us); fine. */
                restore();
            } else {
                WF_CHECK_EQ(errno, EINVAL);
            }
            break;
        }
    }
}

/* Only the calling thread is pinned. */
static void *pin_in_thread(void *arg)
{
    int cpu = *(int *)arg;
    long ok = sched_setaffinity_single(cpu) == 0 && mask_is_only(cpu);
    return (void *)ok;
}

static void pins_calling_thread_only(void)
{
    int cpu = allowed_cpu(CPU_COUNT(&original) - 1);
    if (cpu < 0) return;
    pthread_t t;
    void *res = NULL;
    WF_CHECK_EQ(pthread_create(&t, NULL, pin_in_thread, &cpu), 0);
    pthread_join(t, &res);
    WF_CHECK(res == (void *)1);
    WF_CHECK(mask_is_original());
}

int main(void)
{
    if (sched_getaffinity(0, sizeof original, &original) != 0) {
        perror("sched_getaffinity");
        return 1;
    }
    WF_RUN(pins_allowed_cores);
    WF_RUN(rejects_bad_cores);
    WF_RUN(kernel_refusals);
    WF_RUN(pins_calling_thread_only);
    restore();
    return wf_finish("test_affinity");
}
