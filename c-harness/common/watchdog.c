/*
 * Memory ceiling for harness binaries.
 *
 * Linked into every test and standalone replay binary (libFuzzer builds
 * have their own -rss_limit_mb). A background thread samples the
 * process's private memory every 50 ms and aborts with a message once it
 * passes WF_MEM_LIMIT_MB (default 4096; 0 disables), so a kernel or test
 * that loops while growing its buffers fails that one binary instead of
 * exhausting the machine.
 *
 * Private memory is what the process alone pins: phys_footprint on
 * Darwin, RssAnon + RssShmem on Linux. File-backed pages are excluded,
 * so tests that map one small file over gigabytes of address space
 * (core-hash, xml-fast) are not counted for the whole range. Sanitizer
 * shadow is included; the busiest ASan test sits around 600 MiB.
 */
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#if defined(__APPLE__)
#include <mach/mach.h>
#endif

static size_t wf_private_bytes(void)
{
#if defined(__APPLE__)
    task_vm_info_data_t info;
    mach_msg_type_number_t count = TASK_VM_INFO_COUNT;
    if (task_info(mach_task_self(), TASK_VM_INFO, (task_info_t)&info, &count) != KERN_SUCCESS)
        return 0;
    return (size_t)info.phys_footprint;
#elif defined(__linux__)
    FILE *f = fopen("/proc/self/status", "r");
    if (!f) return 0;
    char line[256];
    size_t kb = 0;
    while (fgets(line, sizeof line, f)) {
        unsigned long v;
        if (sscanf(line, "RssAnon: %lu kB", &v) == 1 || sscanf(line, "RssShmem: %lu kB", &v) == 1)
            kb += v;
    }
    fclose(f);
    return kb * 1024;
#else
    return 0;
#endif
}

static size_t wf_limit_bytes;

static void *wf_watchdog(void *arg)
{
    (void)arg;
    const struct timespec tick = { 0, 50 * 1000 * 1000 };
    for (;;) {
        size_t used = wf_private_bytes();
        if (used > wf_limit_bytes) {
            fprintf(stderr,
                    "wf watchdog: private memory %zu MiB exceeds WF_MEM_LIMIT_MB=%zu; aborting\n",
                    used >> 20, wf_limit_bytes >> 20);
            abort();
        }
        nanosleep(&tick, NULL);
    }
    return NULL;
}

__attribute__((constructor)) static void wf_watchdog_start(void)
{
    const char *s = getenv("WF_MEM_LIMIT_MB");
    unsigned long long mb = s ? strtoull(s, NULL, 10) : 4096;
    if (mb == 0) return;
    wf_limit_bytes = (size_t)(mb << 20);
    pthread_t t;
    if (pthread_create(&t, NULL, wf_watchdog, NULL) == 0) pthread_detach(t);
}
