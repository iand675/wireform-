/*
 * wf_fail.c - see wf_fail.h. Compiled into every harness program so a
 * fail variant's renamed entry points always resolve; unarmed fakes
 * pass through, so non-fail variants are unaffected (nothing calls
 * them there).
 */
#include "wf_fail.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int wf_failpoint(const char *name)
{
    char var[96];
    int n = snprintf(var, sizeof(var), "WF_FAIL_%s", name);
    if (n <= 0 || (size_t)n >= sizeof(var)) return 0;
    const char *v = getenv(var);
    if (v == NULL) return 0;
    long left = atol(v);
    if (left <= 0) return 0;
    char buf[24];
    snprintf(buf, sizeof(buf), "%ld", left - 1);
    setenv(var, buf, 1);
    return left == 1;
}

/* posix_memalign is never renamed, so the pass-through cannot recurse
 * into the fake. n == 0 is widened to 1 so the result is always a
 * unique, free()-able pointer or NULL. */
static void *wf_real_alloc(size_t n)
{
    void *p = NULL;
    if (n == 0) n = 1;
    if (posix_memalign(&p, 16, n) != 0) return NULL;
    return p;
}

void *wf_fail_malloc(size_t n)
{
    if (wf_failpoint("malloc")) return NULL;
    if (wf_failpoint("malloc_stub")) {
        /* Pretend a huge request succeeded with a small real block.
         * Only usable together with faked callees that never write the
         * claimed size (they must not touch the buffer at all). */
        return wf_real_alloc(16);
    }
    return wf_real_alloc(n);
}

void *wf_fail_calloc(size_t n, size_t m)
{
    if (wf_failpoint("calloc")) return NULL;
    if (m != 0 && n > (size_t)-1 / m) return NULL;
    void *p = wf_real_alloc(n * m);
    if (p != NULL) memset(p, 0, n * m);
    return p;
}
