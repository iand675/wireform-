#define _GNU_SOURCE
#include <sched.h>

int sched_setaffinity_single(int core) {
    cpu_set_t cpuset;
    CPU_ZERO(&cpuset);
    CPU_SET(core, &cpuset);
    return sched_setaffinity(0, sizeof(cpuset), &cpuset);
}
