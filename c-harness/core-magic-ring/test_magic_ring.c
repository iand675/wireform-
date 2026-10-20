/* Unit, property, failure-path and concurrency tests for the magic ring
 * (magic_ring_linux.c on Linux, magic_ring_posix.c on Darwin/BSD). */
#include "harness.h"
#include "api.h"

#include <pthread.h>
#include <signal.h>
#include <sys/resource.h>
#include <sys/wait.h>

#if defined(WF_FI)
#  include "fault_inject_impl.h"
#endif

#if defined(__has_feature)
#  if __has_feature(thread_sanitizer)
#    define WF_TSAN 1
#  endif
#endif

static size_t page(void) { return (size_t)sysconf(_SC_PAGESIZE); }

/* Check the double mapping of a live ring: zeroed, every byte i and
 * i + size alias in both directions, and a window across the midpoint is
 * contiguous. Leaves the ring zeroed. */
static void check_ring(struct hs_ring *r, wf_rng *rng)
{
    ring_byte *b = r->base;
    size_t n = r->size;
    WF_CHECK(b != NULL);
    WF_CHECK_EQ((uintptr_t)b % page(), 0);
    WF_CHECK(ref_is_mapped(r->base, 2 * n));
    for (size_t i = 0; i < 2 * n; i++) {
        if (b[i]) { WF_CHECK_EQ(b[i], 0); break; }
    }
    /* Every page boundary and a sample of random offsets, both copies. */
    for (size_t i = 0; i < n; i += page()) {
        size_t idx[3] = { i, i + page() - 1, (size_t)wf_rng_below(rng, n) };
        for (int k = 0; k < 3; k++) {
            size_t j = idx[k];
            b[j] = (uint8_t)(j * 7 + 1);
            WF_CHECK_EQ(b[j + n], (uint8_t)(j * 7 + 1));
            b[j + n] = (uint8_t)(j * 13 + 5);
            WF_CHECK_EQ(b[j], (uint8_t)(j * 13 + 5));
            b[j] = 0;
        }
    }
    /* A full-size window starting anywhere in the first copy. */
    size_t start = (size_t)wf_rng_below(rng, n);
    uint8_t *pat = malloc(n);
    wf_rng_fill(rng, pat, n);
    ring_copy_in(b + start, pat, n);
    WF_CHECK_EQ(ring_cmp(b + start, pat, n), n);
    for (size_t i = 0; i < n; i++) {
        size_t phys = (start + i) % n;
        if (b[phys] != pat[i] || b[phys + n] != pat[i]) {
            WF_CHECK_EQ(b[phys], pat[i]);
            break;
        }
    }
    ring_fill(b, 0, n);
    free(pat);
}

static void page_size_matches_sysconf(void)
{
    long ps = hs_page_size();
    WF_CHECK_EQ(ps, sysconf(_SC_PAGESIZE));
    WF_CHECK(ps > 0 && (ps & (ps - 1)) == 0);
    WF_CHECK_EQ(hs_page_size(), ps);
}

/* Requested sizes around page and power-of-two boundaries. */
static void sizes_round_up(void)
{
    wf_rng rng = wf_rng_from_env();
    size_t ps = page();
    size_t reqs[] = {
        0, 1, 2, ps / 2, ps - 1, ps, ps + 1, 2 * ps - 1, 2 * ps, 2 * ps + 1,
        3 * ps, 3 * ps + 1, 4 * ps, 5 * ps, 7 * ps + 3, 8 * ps,
        65535, 65536, 65537, (1u << 20) - 1, 1u << 20, (4u << 20) + 1,
    };
    for (size_t k = 0; k < sizeof reqs / sizeof reqs[0]; k++) {
        struct hs_ring r = { NULL, 0 };
        int fds = ref_open_fds();
        WF_CHECK_EQ(hs_ring_create(reqs[k], &r), 0);
        WF_CHECK_EQ(r.size, ref_ring_size(reqs[k], ps));
        WF_CHECK_EQ(r.size & (r.size - 1), 0);
        WF_CHECK_EQ(ref_open_fds(), fds);
        if (r.base) check_ring(&r, &rng);
        void *base = r.base;
        size_t sz = r.size;
        hs_ring_destroy(&r);
        WF_CHECK(r.base == NULL);
        WF_CHECK_EQ(r.size, 0);
        if (base) WF_CHECK(!ref_is_mapped(base, 2 * sz));
    }
}

/* Every byte of a one-page and a two-page ring aliases its mirror. */
static void mirror_exhaustive(void)
{
    for (size_t pages = 1; pages <= 2; pages++) {
        struct hs_ring r = { NULL, 0 };
        WF_CHECK_EQ(hs_ring_create(pages * page(), &r), 0);
        if (!r.base) continue;
        ring_byte *b = r.base;
        size_t n = r.size;
        for (size_t i = 0; i < n; i++) b[i] = (uint8_t)(i ^ (i >> 8));
        for (size_t i = 0; i < n; i++) {
            if (b[i + n] != (uint8_t)(i ^ (i >> 8))) {
                WF_CHECK_EQ(b[i + n], (uint8_t)(i ^ (i >> 8)));
                break;
            }
        }
        for (size_t i = 0; i < n; i++) b[n + i] = (uint8_t)~i;
        for (size_t i = 0; i < n; i++) {
            if (b[i] != (uint8_t)~i) { WF_CHECK_EQ(b[i], (uint8_t)~i); break; }
        }
        /* Writes straddling the midpoint at every split point near it. */
        for (size_t k = 0; k <= 64; k++) {
            uint8_t chunk[64];
            for (int j = 0; j < 64; j++) chunk[j] = (uint8_t)(k + (size_t)j * 3);
            ring_copy_in(b + n - k, chunk, 64);
            for (size_t j = 0; j < 64; j++) {
                size_t phys = (n - k + j) % n;
                if (b[phys] != chunk[j]) { WF_CHECK_EQ(b[phys], chunk[j]); break; }
            }
        }
        hs_ring_destroy(&r);
    }
}

/* Requests too big to map, including what negative Ints convert to, fail
 * cleanly: -1, errno set, *out untouched, nothing leaked. */
static void huge_requests_fail_cleanly(void)
{
    size_t reqs[] = {
        SIZE_MAX, SIZE_MAX - 1, SIZE_MAX - page() + 1, SIZE_MAX - page(),
        (size_t)(intptr_t)-1, (size_t)(intptr_t)-4096, (size_t)INTPTR_MIN,
        (SIZE_MAX >> 1) + 1, (SIZE_MAX >> 1) + 2, SIZE_MAX >> 1,
        REF_RING_MAX + 1, REF_RING_MAX + page(), REF_RING_MAX * 3,
    };
    for (size_t k = 0; k < sizeof reqs / sizeof reqs[0]; k++) {
        struct hs_ring r = { (void *)0x1234, 77 };
        int fds = ref_open_fds();
        errno = 0;
        WF_CHECK_EQ(hs_ring_create(reqs[k], &r), -1);
        WF_CHECK_EQ(errno, ENOMEM);
        WF_CHECK(r.base == (void *)0x1234);
        WF_CHECK_EQ(r.size, 77);
        WF_CHECK_EQ(ref_open_fds(), fds);
    }
    /* At the cap the kernel tries for real; no machine maps 2 * 2^62
     * bytes, so it fails from ftruncate or mmap with some errno. */
    struct hs_ring r = { (void *)0x1234, 77 };
    int fds = ref_open_fds();
    errno = 0;
    WF_CHECK_EQ(hs_ring_create(REF_RING_MAX, &r), -1);
    WF_CHECK(errno != 0);
    WF_CHECK(r.base == (void *)0x1234);
    WF_CHECK_EQ(ref_open_fds(), fds);
}

/* Many create/destroy cycles: no descriptor or mapping leak, and live
 * rings never alias each other. */
static void create_destroy_cycles(void)
{
    wf_rng rng = wf_rng_from_env();
    int fds = ref_open_fds();
    long iters = wf_iters(1000);
    for (long it = 0; it < iters; it++) {
        struct hs_ring r = { NULL, 0 };
        size_t req = (size_t)wf_rng_below(&rng, 16 * page() + 1);
        if (hs_ring_create(req, &r) != 0) { WF_CHECK(0); break; }
        ring_byte *b = r.base;
        size_t idx = (size_t)wf_rng_below(&rng, r.size);
        WF_CHECK_EQ(b[idx], 0);
        WF_CHECK_EQ(b[idx + r.size], 0);
        b[idx + r.size] = 0xA5;
        WF_CHECK_EQ(b[idx], 0xA5);
        size_t sz = r.size;
        hs_ring_destroy(&r);
        WF_CHECK(!ref_is_mapped((const void *)b, 2 * sz));
    }
    WF_CHECK_EQ(ref_open_fds(), fds);

    enum { LIVE = 64 };
    struct hs_ring rings[LIVE];
    for (int i = 0; i < LIVE; i++) {
        rings[i].base = NULL;
        WF_CHECK_EQ(hs_ring_create((size_t)(i + 1) * 1000, &rings[i]), 0);
        if (rings[i].base) ring_fill(rings[i].base, (uint8_t)(i + 1), rings[i].size);
    }
    WF_CHECK_EQ(ref_open_fds(), fds);
    for (int i = 0; i < LIVE; i++) {
        ring_byte *b = rings[i].base;
        if (!b) continue;
        for (size_t j = 0; j < 2 * rings[i].size; j += 509) {
            if (b[j] != i + 1) { WF_CHECK_EQ(b[j], i + 1); break; }
        }
        hs_ring_destroy(&rings[i]);
    }
    WF_CHECK_EQ(ref_open_fds(), fds);
}

static void destroy_is_idempotent(void)
{
    hs_ring_destroy(NULL);
    struct hs_ring zero = { NULL, 0 };
    hs_ring_destroy(&zero);
    WF_CHECK(zero.base == NULL);

    struct hs_ring r = { NULL, 0 };
    WF_CHECK_EQ(hs_ring_create(page(), &r), 0);
    void *base = r.base;
    hs_ring_destroy(&r);
    WF_CHECK(r.base == NULL && r.size == 0);
    hs_ring_destroy(&r); /* same struct: no-op */
    WF_CHECK(r.base == NULL && r.size == 0);
    if (base) WF_CHECK(!ref_is_mapped(base, 2 * page()));

    /* A failed create leaves the poked {NULL, 0}; destroying it is a no-op. */
    struct hs_ring f = { NULL, 0 };
    WF_CHECK_EQ(hs_ring_create(SIZE_MAX, &f), -1);
    hs_ring_destroy(&f);
    WF_CHECK(f.base == NULL);
}

#if defined(__APPLE__) || (!defined(__linux__) && !defined(__FreeBSD__))
/* The shm name is unlinked immediately: no /wfring.<pid>.<k> survives. */
static void shm_names_unlinked(void)
{
    struct hs_ring r = { NULL, 0 };
    WF_CHECK_EQ(hs_ring_create(page(), &r), 0);
    hs_ring_destroy(&r);
    for (long k = 0; k < 20000; k++) {
        char name[64];
        snprintf(name, sizeof name, "/wfring.%d.%ld", getpid(), k);
        int fd = shm_open(name, O_RDONLY, 0);
        if (fd >= 0) {
            close(fd);
            shm_unlink(name);
            WF_CHECK(!"leaked shm name");
            break;
        }
    }
}
#endif

/* Real resource exhaustion in a forked child: no descriptor left
 * (RLIMIT_NOFILE) and, in unsanitized Linux builds, no address space
 * (RLIMIT_AS; the sanitizers reserve terabytes of shadow, so not there). */
static int child_create_fails(int which)
{
    struct rlimit rl;
    if (which == 0) {
        int maxfd = 0;
        for (int fd = 0; fd < 1024; fd++) {
            if (fcntl(fd, F_GETFD) != -1) maxfd = fd;
        }
        rl.rlim_cur = rl.rlim_max = (rlim_t)maxfd + 1;
        for (int fd = 0; fd <= maxfd; fd++) {
            if (fcntl(fd, F_GETFD) == -1) {
                if (dup2(0, fd) < 0) return 10;
            }
        }
        if (setrlimit(RLIMIT_NOFILE, &rl) != 0) return 11;
    } else {
#if defined(RLIMIT_AS) && defined(__linux__)
        /* Current address space plus 64 MiB: a 1 GiB ring cannot fit. */
        unsigned long pages = 0;
        FILE *f = fopen("/proc/self/statm", "r");
        if (!f || fscanf(f, "%lu", &pages) != 1) return 13;
        fclose(f);
        rl.rlim_cur = rl.rlim_max = (rlim_t)pages * (rlim_t)page() + ((rlim_t)64 << 20);
        if (setrlimit(RLIMIT_AS, &rl) != 0) return 12;
#endif
    }
    struct hs_ring r = { (void *)0x1234, 77 };
    errno = 0;
    int rc = hs_ring_create(which == 0 ? 4096 : (size_t)1 << 30, &r);
    if (rc != -1) return 1;
    if (errno == 0) return 2;
    if (r.base != (void *)0x1234 || r.size != 77) return 3;
    return 0;
}

static void resource_exhaustion_in_child(void)
{
    int cases = 1;
#if defined(__linux__) && !WF_SANITIZED_HEAP && !defined(WF_TSAN)
    /* Not under valgrind either: the limit would cap valgrind's own maps. */
    const char *pre = getenv("LD_PRELOAD");
    if (!pre || !strstr(pre, "vgpreload")) cases = 2;
#endif
    for (int which = 0; which < cases; which++) {
        fflush(NULL);
        pid_t pid = fork();
        if (pid == 0) _exit(child_create_fails(which));
        int st = 0;
        WF_CHECK(waitpid(pid, &st, 0) == pid);
        WF_CHECK(WIFEXITED(st));
        WF_CHECK_EQ(WEXITSTATUS(st), 0);
    }
}

#if defined(WF_FI)
/* Force each syscall of hs_ring_create to fail in turn: -1 with exactly
 * that errno, *out untouched, the fd closed, and any reservation already
 * made released in full. */
static void injected_failures(void)
{
    struct {
        enum wf_fi_op op;
        int nth, err;
        int reserved; /* the 2N reservation exists when it fails */
    } cases[] = {
        { WF_FI_OPEN, 1, EMFILE, 0 },
        { WF_FI_FTRUNCATE, 1, ENOSPC, 0 },
        { WF_FI_MMAP, 1, ENOMEM, 0 },
        { WF_FI_MMAP, 2, EACCES, 1 },
        { WF_FI_MMAP, 3, EINVAL, 1 },
    };
    for (size_t k = 0; k < sizeof cases / sizeof cases[0]; k++) {
        for (size_t pages = 1; pages <= 4; pages *= 4) {
            int fds = ref_open_fds();
            struct hs_ring r = { (void *)0x1234, 77 };
            wf_fi_arm(cases[k].op, cases[k].nth, 1, cases[k].err);
            errno = 0;
            int rc = hs_ring_create(pages * page(), &r);
            WF_CHECK_EQ(rc, -1);
            WF_CHECK_EQ(errno, cases[k].err);
            WF_CHECK(r.base == (void *)0x1234);
            WF_CHECK_EQ(r.size, 77);
            WF_CHECK_EQ(ref_open_fds(), fds);
            if (cases[k].reserved) {
                WF_CHECK(wf_fi_last_reserve != NULL);
                WF_CHECK(wf_fi_last_unmap == wf_fi_last_reserve);
                WF_CHECK_EQ(wf_fi_last_unmap_len, 2 * pages * page());
                WF_CHECK_EQ(wf_fi_last_reserve_len, 2 * pages * page());
                WF_CHECK(!ref_is_mapped(wf_fi_last_reserve, wf_fi_last_reserve_len));
            } else {
                WF_CHECK(wf_fi_last_reserve == NULL);
            }
            wf_fi_reset();
            /* And the next create works. */
            struct hs_ring ok = { NULL, 0 };
            WF_CHECK_EQ(hs_ring_create(pages * page(), &ok), 0);
            hs_ring_destroy(&ok);
        }
    }
}

#if !defined(__linux__) && !(defined(__FreeBSD__) && defined(SHM_ANON))
/* A stale /wfring name (EEXIST) is skipped over, a bounded number of times. */
static void shm_open_eexist_retry(void)
{
    struct hs_ring r = { NULL, 0 };
    wf_fi_arm(WF_FI_OPEN, 1, 3, EEXIST);
    WF_CHECK_EQ(hs_ring_create(page(), &r), 0);
    WF_CHECK_EQ(wf_fi_calls(WF_FI_OPEN), 4);
    hs_ring_destroy(&r);

    int fds = ref_open_fds();
    struct hs_ring f = { (void *)0x1234, 77 };
    wf_fi_arm(WF_FI_OPEN, 1, 1000, EEXIST);
    errno = 0;
    WF_CHECK_EQ(hs_ring_create(page(), &f), -1);
    WF_CHECK_EQ(errno, EEXIST);
    WF_CHECK_EQ(wf_fi_calls(WF_FI_OPEN), 16);
    WF_CHECK(f.base == (void *)0x1234);
    WF_CHECK_EQ(ref_open_fds(), fds);

    /* Any other shm_open error is not retried. */
    wf_fi_arm(WF_FI_OPEN, 1, 1000, EACCES);
    WF_CHECK_EQ(hs_ring_create(page(), &f), -1);
    WF_CHECK_EQ(errno, EACCES);
    WF_CHECK_EQ(wf_fi_calls(WF_FI_OPEN), 1);
    wf_fi_reset();
}
#endif
#endif /* WF_FI */

#if !defined(WF_FI)
/* GHC calls these from several capabilities at once. Run first, before
 * anything has filled the page-size cache, so the threads race to fill it
 * (a data race on a plain `long` before the fix; TSan reports it). */
static void *ring_worker(void *arg)
{
    uintptr_t id = (uintptr_t)arg;
    for (int it = 0; it < 100; it++) {
        long ps = hs_page_size();
        if (ps != sysconf(_SC_PAGESIZE)) return (void *)1;
        struct hs_ring r = { NULL, 0 };
        if (hs_ring_create((size_t)ps * (1 + (id + (uintptr_t)it) % 4), &r) != 0)
            return (void *)2;
        ring_byte *b = r.base;
        size_t off = (id * 131 + (size_t)it) % r.size;
        b[off + r.size] = (uint8_t)id;
        if (b[off] != (uint8_t)id) return (void *)3;
        hs_ring_destroy(&r);
    }
    return NULL;
}

static void concurrent_create_destroy(void)
{
    enum { THREADS = 8 };
    pthread_t t[THREADS];
    for (uintptr_t i = 0; i < THREADS; i++) {
        WF_CHECK_EQ(pthread_create(&t[i], NULL, ring_worker, (void *)i), 0);
    }
    for (int i = 0; i < THREADS; i++) {
        void *res = (void *)1;
        pthread_join(t[i], &res);
        WF_CHECK(res == NULL);
    }
}
#endif

int main(void)
{
#if !defined(WF_FI)
    WF_RUN(concurrent_create_destroy);
#endif
    WF_RUN(page_size_matches_sysconf);
    WF_RUN(sizes_round_up);
    WF_RUN(mirror_exhaustive);
    WF_RUN(huge_requests_fail_cleanly);
    WF_RUN(create_destroy_cycles);
    WF_RUN(destroy_is_idempotent);
#if defined(__APPLE__) || (!defined(__linux__) && !defined(__FreeBSD__))
    WF_RUN(shm_names_unlinked);
#endif
    WF_RUN(resource_exhaustion_in_child);
#if defined(WF_FI)
    WF_RUN(injected_failures);
#if !defined(__linux__) && !(defined(__FreeBSD__) && defined(SHM_ANON))
    WF_RUN(shm_open_eexist_retry);
#endif
#endif
    return wf_finish("test_magic_ring");
}
