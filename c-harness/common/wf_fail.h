/*
 * wf_fail.h - failure injection for the harness's "fail" build variants.
 *
 * A fail variant compiles the kernel sources with selected libc and
 * library entry points renamed to the fakes declared here (and, for
 * third-party constructors, in the module's own failfns.h), so tests
 * can make those calls fail on demand and drive the error paths that
 * are otherwise reachable only under out-of-memory conditions.
 *
 * Failpoints are armed through the environment: set WF_FAIL_<name> to
 * a positive countdown N and the Nth call to failpoint <name> fails
 * (each call counts the environment down, so the call after that sees
 * zero and passes). For example
 *
 *     setenv("WF_FAIL_malloc", "1", 1);   -- next malloc() fails
 *     setenv("WF_FAIL_inflateInit2", "2", 1);  -- second call fails
 *
 * The fakes pass through to the real implementation when unarmed, so
 * a fail variant can also run ordinary tests. Allocation pass-through
 * uses posix_memalign (never renamed), so the fakes do not recurse.
 *
 * Armed through the environment rather than shared globals so one
 * rename-instrumented translation unit arms another's fakes without
 * link-order games; everything is plain libc.
 */
#ifndef WF_FAIL_H
#define WF_FAIL_H

#include <stddef.h>

/* Returns 1 when the named failpoint is armed and its countdown has
 * reached this call (which disarms it), 0 otherwise. */
int wf_failpoint(const char *name);

/* malloc/calloc lookalikes: fail when the "malloc" / "calloc"
 * failpoint fires, otherwise return real, free()-able memory. */
void *wf_fail_malloc(size_t n);
void *wf_fail_calloc(size_t n, size_t m);

#endif /* WF_FAIL_H */
